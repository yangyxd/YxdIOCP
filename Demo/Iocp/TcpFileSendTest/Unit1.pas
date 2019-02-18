unit Unit1;

interface

uses
  iocp, iocp.Sockets, iocp.Utils.Hash,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TExecThread = class(TThread)
  private
    FOnExecute: TNotifyEvent;
  protected
    procedure Execute; override;
  end;

type
  PConnData = ^TConnData;
  TConnData = record
    FDataSize: Int64;
    FRecvCount: Int64;
    FFirstTime: Int64;
  end;

type
  TForm1 = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Button2: TButton;
    pbSendFile: TProgressBar;
    Label5: TLabel;
    mmoLog: TMemo;
    Label6: TLabel;
    Edit3: TEdit;
    Label7: TLabel;
    Timer1: TTimer;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure btnPostWSACloseClick(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
    FTcpServer: TIocpTcpServer;
    FClient: TIocpTcpClient;
    FConn: TIocpRemoteContext;
    
    FSendStream: TStream;
    FFileSize: Int64;
    FLastSendBytes: Int64;

    FTryDecode: Boolean;

    FStartTime: Int64;
    FMap: TIntHash;
  public
    { Public declarations }
    procedure Msg(const s: string);
    procedure RefreshState;
    function GetResponseData: string;
    procedure OnContextConnected(const pvContext: TIocpContext);
    procedure OnContextDisconnected(const pvContext: TIocpContext);
    procedure OnContextError(const Context: TIocpCustomContext; ErrorCode: Integer);
    procedure OnSendRequestResponse(Context: TIocpCustomContext; Request: TIocpSendRequest);

    procedure OnAccept(pvSocket: THandle; const pvAddr: string; pvPort: Word; var
        vAllowAccept: Boolean);

    procedure OnDataRecvived(const Context: TIocpContext; buf: Pointer;
      len: Cardinal; ErrorCode: Integer);
    function OnDecode(Connection: TIocpConnection; const Stream: TIocpStream;
      var Request: TObject): Boolean;

    procedure DoSendFile(Sender: TObject);

    procedure DecodeData(buf: Pointer; len: Cardinal);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnDisconectAllClick(Sender: TObject);
begin
  FTcpServer.DisConnectAll();
  RefreshState;
end;

procedure TForm1.btnGetWorkerStateClick(Sender: TObject);
begin
  ShowMessage(FTcpServer.Engine.getWorkerStateInfo(0));
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  if btnOpen.Tag = 0 then begin
    FTcpServer.ListenPort := StrToInt(edtPort.Text);
    FTcpServer.OnContextConnected := OnContextConnected;
    FTcpServer.OnContextDisconnected := OnContextDisconnected;
    FTcpServer.OnDataReceived := OnDataRecvived;
    FTcpServer.Active := true;
    // 接收缓存要大一点，但越大，每个连接占用的内存就越多, 能同时支撑的连接数
    // 就越少，所以要终合考量
    FTcpServer.RecvBufferSize := 1024 * 1024 * 8; 
  end else begin
    FTcpServer.Stop;
  end;
  RefreshState;
end;

procedure TForm1.btnPostWSACloseClick(Sender: TObject);
var
  lvList:TList;
  i:Integer;
begin
  lvList := TList.Create;
  try
    FTcpServer.getOnlineContextList(lvList);
    for i:=0 to lvList.Count -1 do
      TIocpContext(lvList[i]).Close();
  finally
    lvList.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not Assigned(FClient) then begin
    FClient := TIocpTcpClient.Create(nil);
    FClient.OnContextError := OnContextError;
    FClient.OnContextDisconnected := OnContextDisconnected;
    FClient.OnSendRequestResponse := OnSendRequestResponse;
    FClient.SendBufferSize := 1024 * 256;  // 缓存要大一点，传输速度才会更快，但太大了也没意义。
  end else begin
    if FClient.Active then
      FClient.Close;
  end;

  FConn := FClient.Connect(Edit3.Text, StrToIntDef(Edit1.text, 9983), True, False);  
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  T: TExecThread;
begin
  if Edit2.Text = '' then Exit;
  if not FileExists(Edit2.Text) then begin
    Msg('要发送的文件不存在.');
    Exit;
  end;
  if Assigned(FSendStream) then begin
    Msg('正在发送中...');
    Exit;
  end;
  if not Assigned(FClient) then Exit;
  if not FClient.Active then Exit;
  if not Assigned(FConn) then Exit;

  FSendStream := TFileOnlyStream.Create(Edit2.Text);
  FSendStream.Position := 0;
  FFileSize := FSendStream.Size;
  FStartTime := GetTimestamp;
  
  T := TExecThread.Create(True);
  T.FreeOnTerminate := True;
  T.FOnExecute := DoSendFile;
  T.Resume;
end;

procedure TForm1.Button8Click(Sender: TObject);
var
  TCP: TIocpTcpSocket;
  T: Int64;
begin
  TCP := TIocpTcpSocket.Create(Self);
  try
    TCP.RemoteHost := Edit1.Text;
    TCP.RemotePort := StrToIntDef(edtPort.Text, 9983);
    T := GetTimestamp;
    TCP.Active := True;
    while TCP.Connected do
      Sleep(20);
    T := GetTimestamp - T;
  finally
    TCP.Free;
  end;
  ShowMessage(Format('连接超时: %dms.', [T]));
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  FTryDecode := CheckBox1.Checked;
end;

procedure TForm1.DecodeData(buf: Pointer; len: Cardinal);
var
  pv, p: PAnsiChar;
begin
  p := buf;
  pv := p + len;
  while p < pv do begin
    Inc(P);
  end;
end;

procedure TForm1.DoSendFile(Sender: TObject);
begin
  if not Assigned(FSendStream) then
    Exit;
  FConn.SendStream(FSendStream);  
  FreeAndNil(FSendStream);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FClient) then
    FClient.Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMap := TIntHash.Create();
  
  FTcpServer := TIocpTcpCodecServer.Create(Self);
  FTcpServer.Name := 'iocpSVR';
  FTcpServer.OnDataReceived := OnDataRecvived;
  FTcpServer.OnContextAccept := OnAccept;
  FTcpServer.createDataMonitor;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClient);
  Application.ProcessMessages;
  FreeAndNil(FSendStream);
  FreeAndNil(FTcpServer);
  FreeAndNil(FMap);
end;

function TForm1.GetResponseData: string;
begin
  Result := '';
end;

procedure TForm1.Msg(const s: string);
begin
  mmoLog.Lines.Add(s);
  OutputDebugString(PChar(s));
end;

procedure TForm1.OnAccept(pvSocket: THandle; const pvAddr: string; pvPort: Word;
  var vAllowAccept: Boolean);
begin
  //  if pvAddr = '127.0.0.1' then
  //    vAllowAccept := false;
end;

procedure TForm1.OnContextConnected(const pvContext: TIocpContext);
begin
  Msg('Context Connected.');
end;

procedure TForm1.OnContextDisconnected(const pvContext: TIocpContext);
begin
  Msg('Context Disconnected.');
  FreeAndNil(FSendStream);
end;

procedure TForm1.OnContextError(const Context: TIocpCustomContext;
  ErrorCode: Integer);
begin
  //FreeAndNil(FSendStream);
end;

function TForm1.OnDecode(Connection: TIocpConnection; const Stream: TIocpStream; var Request: TObject): Boolean;
begin
  Msg('OnDecode.');
end;

procedure TForm1.OnSendRequestResponse(Context: TIocpCustomContext;
  Request: TIocpSendRequest);
begin
  if not Assigned(FSendStream) then begin  
    Context.Close;
  end;
end;

procedure TForm1.OnDataRecvived(const Context: TIocpContext; buf: Pointer;
  len: Cardinal; ErrorCode: Integer);
begin
  if FTryDecode then 
    DecodeData(buf, len);
end;

procedure TForm1.RefreshState;
begin
  if FTcpServer.Active then
  begin
    btnOpen.Caption := '停止服务';
    btnOpen.Tag := 1;
  end else
  begin
    btnOpen.Caption := '开启服务';
    btnOpen.Tag := 0;
  end;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
var
  V: Int64;
begin
  V := 0;
  if Assigned(FSendStream) then     
    V := FSendStream.Position;
  if V > 0 then begin
    pbSendFile.Max := 10000;
    self.pbSendFile.Position := Round(V / FFileSize * 10000);
    if FLastSendBytes > 0 then begin
      self.Label5.Caption := Format('用时: %ds, 发送速度: %.2f M/s, 平均速度: %.2f M/s',
        [
          (GetTimestamp - FStartTime) div 1000,
          (V - FLastSendBytes) /1048576,
          V / ((GetTimestamp - FStartTime) / 1000) / 1048576
        ]);
    end;
    FLastSendBytes := V;     
  end else
    self.pbSendFile.Position := 0;
end;

{ TExecThread }

procedure TExecThread.Execute;
begin
  try
    if Assigned(FOnExecute) then
      FOnExecute(Self);
  except
  end;
end;

end.
