unit Unit1;

interface

uses
  iocp, iocp.Sockets, PsAPI,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, XPMan;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    lbSvrInfo: TLabel;
    Timer1: TTimer;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    XPManifest1: TXPManifest;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FUdpSvr: TIocpUdpServer;
  public
    { Public declarations }
    procedure OnUdpDataRecv(Request: TIocpUdpRequest; buf: Pointer; len: Cardinal);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function RollupSize(ASize: Int64): String;
const
  Units: array [0 .. 3] of String = ('GB', 'MB', 'KB', 'B');
var
  AIdx: Integer;
  R1, S1: Int64;
  AIsNeg: Boolean;
begin
  AIdx := 3;
  R1 := 0;
  AIsNeg := (ASize < 0);
  if AIsNeg then
    ASize := -ASize;
  SetLength(Result, 0);
  while (AIdx >= 0) do begin
    S1 := ASize mod 1024;
    ASize := ASize shr 10;
    if (ASize = 0) or (AIdx = 0) then begin
      R1 := R1 * 100 div 1024;
      if R1 > 0 then begin
        if R1 >= 10 then
          Result := IntToStr(S1) + '.' + IntToStr(R1) + Units[AIdx]
        else
          Result := IntToStr(S1) + '.' + '0' + IntToStr(R1) + Units[AIdx];
      end else
        Result := IntToStr(S1) + Units[AIdx];
      break;
    end;
    R1 := S1;
    Dec(AIdx);
  end;
  if AIsNeg then
    Result := '-' + Result;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Enabled := False;
  FUdpSvr.ListenPort := StrToIntDef(Edit1.Text, 0);
  FUdpSvr.OnDataReceived := OnUdpDataRecv;
  FUdpSvr.Open;
  Button1.Enabled := not FUdpSvr.Active;
  Button2.Enabled := FUdpSvr.Active;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FUdpSvr.Close;
  Button1.Enabled := not FUdpSvr.Active;
  Button2.Enabled := FUdpSvr.Active;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Timer1.Enabled := False;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FUdpSvr := TIocpUdpServer.Create(Self);
  FUdpSvr.CreateDataMonitor;
  FUdpSvr.BindAddr := '0.0.0.0';
  Button1.Enabled := True;
  Button2.Enabled := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FUdpSvr);
end;

procedure TForm1.OnUdpDataRecv(Request: TIocpUdpRequest; buf: Pointer; len: Cardinal);
var
  s: AnsiString;
begin
  SetString(s, PAnsiChar(buf), len);
  Request.Send(AnsiString(Format('%s (Addr: %s:%d)',
    [s, Request.RemoteAddr, Request.PeerPort])));
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  lbSvrInfo.Caption := Format('运行信息'#9'CPU: %d%%, 工作内存: %s, 句柄数: %d, 工作引擎线程: %d, 任务处理线程: %d',
    [
      GetCPUUsage,
      RollupSize(GetProcessMemUse(GetCurrentProcessId)),
      iocp.GetProcessHandleCount(GetCurrentProcessId),
      FUdpSvr.WorkerCount,
      GetTaskWorkerCount
    ]);
  if not Assigned(FUdpSvr) then Exit;
  Label3.Caption := Format('在线信息'#9'%d',
    [
      0 //FUdpSvr.ClientCount
    ]);
  Label4.Caption := Format('接收信息'#9'投递:%d, 回应:%d, 剩余:%d',
    [
      FUdpSvr.Moniter.PostWSARecvCounter,
      FUdpSvr.Moniter.ResponseWSARecvCounter,
      FUdpSvr.Moniter.PostWSARecvCounter - FUdpSvr.Moniter.ResponseWSARecvCounter
    ]);
  Label7.Caption := TransByteSize(FUdpSvr.Moniter.RecvSize);
  Label5.Caption := Format('发送信息'#9'投递:%d, 回应:%d, 剩余:%d',
    [
      FUdpSvr.Moniter.PostWSASendCounter,
      FUdpSvr.Moniter.ResponseWSASendCounter,
      FUdpSvr.Moniter.PostWSASendCounter - FUdpSvr.Moniter.ResponseWSASendCounter
    ]);
  Label8.Caption := TransByteSize(FUdpSvr.Moniter.SentSize);
  Label6.Caption := Format('套接字句柄'#9'创建:%d, 销毁:%d',
    [
      FUdpSvr.Moniter.HandleCreateCounter,
      FUdpSvr.Moniter.HandleDestroyCounter
    ]);
  Label9.Caption := Format('AcceptEx信息'#9'创建:%d, 投递:%d, 回应:%d',
    [
      FUdpSvr.Moniter.AcceptExObjectCounter,
      FUdpSvr.Moniter.PostWSAAcceptExCounter,
      FUdpSvr.Moniter.ResponseWSAAcceptExCounter
    ]);
  Label10.Caption := Format('连接信息'#9'创建:%d, 借出:%d, 还回:%d',
    [
      FUdpSvr.Moniter.ContextCreateCounter,
      FUdpSvr.Moniter.ContextOutCounter,
      FUdpSvr.Moniter.ContextReturnCounter
    ]);
  Label11.Caption := Format('发送请求'#9'创建:%d, 借出:%d, 还回:%d',
    [
      FUdpSvr.Moniter.SendRequestCreateCounter,
      FUdpSvr.Moniter.SendRequestOutCounter,
      FUdpSvr.Moniter.SendRequestReturnCounter
    ]);
  Label12.Caption := Format('运行时间'#9'%s', [GetRunTimeInfo]);
end;

end.
