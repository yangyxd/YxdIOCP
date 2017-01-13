unit Unit1;

interface

uses
  iocp,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    edtPort: TEdit;
    btnOpen: TButton;
    btnDisconectAll: TButton;
    btnGetWorkerState: TButton;
    btnFindContext: TButton;
    btnPostWSAClose: TButton;
    btnReOpenTest: TButton;
    pgcMain: TPageControl;
    TabSheet1: TTabSheet;
    pnlMonitor: TPanel;
    tmrReader: TTimer;
    tmrTest: TTimer;
    Button3: TButton;
    Button2: TButton;
    Button1: TButton;
    Edit1: TEdit;
    Timer1: TTimer;
    Label1: TLabel;
    Button5: TButton;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnDisconectAllClick(Sender: TObject);
    procedure btnGetWorkerStateClick(Sender: TObject);
    procedure btnFindContextClick(Sender: TObject);
    procedure btnPostWSACloseClick(Sender: TObject);
    procedure btnReOpenTestClick(Sender: TObject);
    procedure tmrTestTimer(Sender: TObject);
    procedure tmrReaderTimer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    iCounter: Integer;
    FTcpServer: TIocpTcpServer;
    FStickRef: Integer;

    FClient: TIocpTcpClient;
    FSendStr: string;

  public
    { Public declarations }
    procedure RefreshState;
    function GetResponseData: string;
    procedure OnContextConnected(const pvContext: TIocpContext);

    procedure OnClientRecvBuffer(const pvClientContext: TIocpContext; buf: Pointer;
        len:cardinal; errCode:Integer);
    procedure OnRecvBuffer(const pvClientContext: TIocpContext; buf: Pointer;
        len:cardinal; errCode:Integer);
    procedure OnAccept(pvSocket: THandle; const pvAddr: string; pvPort: Word; var
        vAllowAccept: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uFMMonitor;


procedure TForm1.btnDisconectAllClick(Sender: TObject);
begin
  FTcpServer.DisConnectAll();
  RefreshState;
end;

procedure TForm1.btnFindContextClick(Sender: TObject);
var
  lvList:TList;
  i:Integer;
begin
  lvList := TList.Create;
  try
    FTcpServer.getOnlineContextList(lvList);
    for i:=0 to lvList.Count -1 do
    begin
      FTcpServer.findContext(TIocpContext(lvList[i]).SocketHandle);
    end;
  finally
    lvList.Free;
  end;
end;

procedure TForm1.btnGetWorkerStateClick(Sender: TObject);
begin
  ShowMessage(FTcpServer.Engine.getWorkerStateInfo(0));
end;

procedure TForm1.btnOpenClick(Sender: TObject);
begin
  if btnOpen.Tag = 0 then begin
    FTcpServer.ListenPort := StrToInt(edtPort.Text);
    FTcpServer.OnDataReceived := OnRecvBuffer;
    FTcpServer.Active := true;
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

procedure TForm1.btnReOpenTestClick(Sender: TObject);
begin
  tmrTest.Enabled := not tmrTest.Enabled;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  Host: string;
begin
  if not Assigned(FClient) then begin
    FClient := TIocpTcpClient.Create(Self);
    FClient.OnContextConnected := OnContextConnected;
    FClient.OnDataReceived := OnClientRecvBuffer;
  end;
  Host := Trim(Edit1.Text);
  for I := 0 to 1000 - 1 do
    FClient.Connect(AnsiString(Host), StrToIntDef(edtPort.Text, 9983), True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not Assigned(FClient) then begin
    FClient := TIocpTcpClient.Create(Self);
    FClient.OnContextConnected := OnContextConnected;
    FClient.OnDataReceived := OnClientRecvBuffer;
  end;
  FClient.Connect(AnsiString(Edit1.Text), StrToIntDef(edtPort.Text, 9983), True);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if not Assigned(FClient) then
    Exit;
  FClient.Delete(0);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if not Assigned(FClient) then
    Exit;
  FClient.RemoveAll();
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FSendStr := Memo1.Lines.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FStickRef := 0;
  
  iCounter := 0;

  FTcpServer := TIocpTcpServer.Create(Self);
  FTcpServer.Name := 'iocpSVR';
  FTcpServer.OnDataReceived := OnRecvBuffer;
  //FTcpServer.OnContextAccept := OnAccept;
  FTcpServer.createDataMonitor;

  TFMMonitor.createAsChild(pnlMonitor, FTcpServer);
  FSendStr := Memo1.Lines.Text;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTcpServer);
  FreeAndNil(FClient);
end;

function TForm1.GetResponseData: string;
begin
  Result := '';
end;

procedure TForm1.OnAccept(pvSocket: THandle; const pvAddr: string; pvPort: Word;
  var vAllowAccept: Boolean);
begin
  //  if pvAddr = '127.0.0.1' then
  //    vAllowAccept := false;
end;

procedure TForm1.OnClientRecvBuffer(const pvClientContext: TIocpContext;
  buf: Pointer; len: cardinal; errCode: Integer);
begin
  pvClientContext.Send(FSendStr);
  Sleep(1);
end;

procedure TForm1.OnContextConnected(const pvContext: TIocpContext);
begin
  pvContext.Send(FSendStr);
end;

procedure TForm1.OnRecvBuffer(const pvClientContext: TIocpContext; buf: Pointer;
  len: cardinal; errCode: Integer);
//var
  //j, i:Integer;
  //s:AnsiString;
begin
  if len <> 10 then begin
    InterlockedIncrement(FStickRef);
  end;
  if errCode = 0 then
  begin
    // 如果客户端发送的为字符串，可以用下面代码进行显示
    //    SetLength(s, len);
    //    Move(buf^, s[1], len);
    //    sfLogger.logMessage(s);
    pvClientContext.Send(buf, len);
  end else
    pvClientContext.Disconnect;
end;

procedure TForm1.RefreshState;
begin
  if FTcpServer.Active then
  begin
    btnOpen.Caption := '关闭';
    btnOpen.Tag := 1;
  end else
  begin
    btnOpen.Caption := '开启';
    btnOpen.Tag := 0;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label1.Caption := Format('CPU: %d%%, Worker: %d',
    [GetCPUUsage, GetTaskWorkerCount()]);  
end;

procedure TForm1.tmrReaderTimer(Sender: TObject);
  function GetClientCount: Integer;
  begin
    if Assigned(FClient) then
      Result := FClient.Count
    else
      Result := 0;
  end;
begin
  Caption := Format('Stick: %d, ClientCount: %d', [FStickRef, GetClientCount]);
end;

procedure TForm1.tmrTestTimer(Sender: TObject);
begin
  btnOpenClick(Sender);
  FTcpServer.IocpAcceptorMgr.MinRequest := 1000;
  FTcpServer.IocpAcceptorMgr.MaxRequest := 2000;
  Application.ProcessMessages;
  btnOpenClick(Sender);
end;

end.
