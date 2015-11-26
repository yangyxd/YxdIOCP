unit Unit1;

interface

uses
  iocp, iocp.http, PsAPI, utils.buffer, 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

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
    Button3: TButton;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
    FHttpSvr: TIocpHttpServer;
  public
    { Public declarations }
    procedure OnHttpExecute(Sender: TIocpHttpServer;
      Request: TIocpHttpRequest; Response: TIocpHttpResponse);
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
  FHttpSvr.ListenPort := StrToIntDef(Edit1.Text, 0);
  FHttpSvr.OnHttpRequest := OnHttpExecute;
  FHttpSvr.Start;
  Button1.Enabled := not FHttpSvr.Active;
  Button2.Enabled := FHttpSvr.Active;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FHttpSvr.Stop;
  Button1.Enabled := not FHttpSvr.Active;
  Button2.Enabled := FHttpSvr.Active;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FHttpSvr.DisconnectAll;
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  s: string;
  n: TDateTime;
  i: Integer;
  t: Int64;
begin
  n := Now;
  s := DateTimeToGMTRFC822(n);

  t := GetTimestamp;
  for I := 0 to 1000000 - 1 do begin
    s := DateTimeToGMTRFC822(n);
    GMTRFC822ToDateTime(s);
  end;
  t := GetTimestamp - t;

  ShowMessage(s + ', ' + IntToStr(t) + 'ms');
  ShowMessage(DateTimeToStr(GMTRFC822ToDateTime(s)));
end;

function MyTrunc(const V: Double): Integer; inline;
var
  D: Double;
begin
  if V > 0 then
    D := V - 0.499999999999 + $18000000000000
  else
    D := V + 0.499999999999 + $18000000000000;
  Result := PInteger(@D)^;
end;

function MyRound(const V: Double): Integer; inline;
var
  D: Double;
begin
  D := V + $18000000000000;
  Result := PInteger(@D)^;
end;

procedure TForm1.Button6Click(Sender: TObject);
const
  C = 10000000;
var
  u, u1, u2: Extended;
  I: Integer;
  v, v1, v2: Integer;
  a, a1, a2: Integer;
  b, b1, b2: Integer;
  t, t1, t2: Int64;
begin
  u := 99115.586;
  u1 := 1.23E15;
  u2 := -1.53;

  t := GetTimestamp;
  for I := 0 to C - 1 do begin
    v := Trunc(u);
    a := Trunc(u1);
    b := Trunc(u2);
  end;
  t := GetTimestamp - t;

  t1 := GetTimestamp;
  for I := 0 to C - 1 do begin
    v1 := Round(u);
    a1 := Round(u1);
    b1 := Round(u2);
  end;
  t1 := GetTimestamp - t1;

  t2 := GetTimestamp;
  for I := 0 to C - 1 do begin
    v2 := MyTrunc(u);
    a2 := MyTrunc(u1);
    b2 := MyTrunc(u2);
  end;
  t2 := GetTimestamp - t2;

  ShowMessage(Format('Trunc: %dms, Round: %dms, MyTrunc: %dms'#13+
    '%f, %f, %f'#13'%d, %d, %d'#13'%d, %d, %d'#13'%d, %d, %d',
    [t, t1, t2, u, u1, u2, v, v1, v2, a, a1, a2, b, b1, b2]));
end;

function RollupSize2(const ASize: Int64): string;
begin
  if ASize >= $40000000 then begin
    if ASize < 0 then
      Result := Format('-%.2fGB', [(-ASize) / $40000000])
    else
      Result := Format('%.2fGB', [ASize / $40000000])
  end;
end;

procedure TForm1.Button7Click(Sender: TObject);
const
  C = 1000000;
var
  u: Int64;
  I: Integer;
  t, t1, t2: Int64;
  v, v1: string;
begin
  u := $41222000;

  t := GetTimestamp;
  for I := 0 to C - 1 do
    v := RollupSize(u);
  t := GetTimestamp - t;

  t := GetTimestamp;
  for I := 0 to C - 1 do
    v := RollupSize(u);
  t := GetTimestamp - t;

  t1 := GetTimestamp;
  for I := 0 to C - 1 do
    v1 := RollupSize2(u);
  t1 := GetTimestamp - t1;

  ShowMessage(Format('RollupSize: %dms, RollupSize2: %dms'#13'%s, %s',
    [t, t1, v, v1]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FHttpSvr := TIocpHttpServer.Create(Self);
  FHttpSvr.KickOutInterval := 10000;
  FHttpSvr.CreateDataMonitor;
  FHttpSvr.BindAddr := '0.0.0.0';
  Button1.Enabled := True;
  Button2.Enabled := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FHttpSvr);
end;

procedure TForm1.OnHttpExecute(Sender: TIocpHttpServer;
  Request: TIocpHttpRequest; Response: TIocpHttpResponse);
var
  Params: AnsiString;
  I: Integer;
  S: TStream;
begin
  if Request.GetParam('downfile') <> '' then begin
    //ShowMessage(Request.Header);
    Response.SendFile(Request.GetParam('downfile'));
    Exit;
  end;

  if Request.IsFormData then begin
    //Response.Send('表单提交模式. ' + IntToStr(Request.InstanceSize));
    with Request.FormData['F1'] do begin
      S := GetContentStream;
      if Assigned(S) then begin
        S.Position := 0;
        try
          //Response.Send(Content);
          Response.SendFileStream(S, FileName, ContentType);
        finally
          FreeAndNil(S);
        end;
      end else
        Response.Send('无效请求数据');
    end;
    Exit;    
  end;
  //Response.SendFile('F:\back\MaxSoft\loadrunner-11.iso');
  //Exit;
  //Response.RedirectURL('http://www.baidu.com');
  {
  if Request.ContextLength > 0 then
    Response.Send(Request.Params[0] + #13#10 +
      Request.DataString)
  else
    Response.Send(Request.Params[0]);
  }
  //Response.GZip := False;
  Params := Request.Params[0];    

  //Response.SendHeader(Length(Params));
  //Response.SendContent(Params);

  if Request.IsPost then
    Request.ParsePostParams;

  Response.SendChunkHeader(False);
  Response.SendChunk(AnsiString(Format('远程地址: %s:%d'#13#10'<br>', [Request.Connection.RemoteAddr, Request.Connection.RemotePort])));
  Response.SendChunk('HTTP 版本: ' + Request.RequestVersionStr + #13#10'<br>');
  Response.SendChunk('URL: ' + Request.URL + #13#10'<br>');
  Response.SendChunk('URI: ' + Request.URI + #13#10'<br>');
  Response.SendChunk(AnsiString('请求头部长度: ' + IntToStr(Request.HeaderLength) + #13#10'<br>'));
  Response.SendChunk(AnsiString('请求参数: ' + IntToStr(Request.ParamsCount) + #13#10'<br>'));
  Params := '';
  for I := 0 to Request.ParamsCount - 1 do
    Params := Params + '参数' + IntToStr(I+1) + ': ' + Request.Params[i] + #13#10'<br>';
  Response.SendChunk(Params + #13#10'<br>');
  Response.SendChunk(AnsiString('请求内容长度: ' + IntToStr(Request.ContextLength) + 'Bytes'#13#10'<br>'));
  Response.SendChunk(AnsiString('请求内容: '#13#10'<br>' + Request.DataString + #13#10'<br>'));
  Response.SendChunkEnd;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  lbSvrInfo.Caption := Format('运行信息'#9'CPU: %d%%, 工作内存: %s, 句柄数: %d, 工作引擎线程: %d, 任务处理线程: %d',
    [
      GetCPUUsage,
      RollupSize(GetProcessMemUse(GetCurrentProcessId)),
      iocp.GetProcessHandleCount(GetCurrentProcessId),
      FHttpSvr.WorkerCount,
      GetTaskWorkerCount
    ]);
  if not Assigned(FHttpSvr) then Exit;
  Label3.Caption := Format('在线信息'#9'%d',
    [
      FHttpSvr.ClientCount
    ]);
  Label4.Caption := Format('接收信息'#9'投递:%d, 回应:%d, 剩余:%d',
    [
      FHttpSvr.Moniter.PostWSARecvCounter,
      FHttpSvr.Moniter.ResponseWSARecvCounter,
      FHttpSvr.Moniter.PostWSARecvCounter - FHttpSvr.Moniter.ResponseWSARecvCounter
    ]);
  Label7.Caption := TransByteSize(FHttpSvr.Moniter.RecvSize);
  Label5.Caption := Format('发送信息'#9'投递:%d, 回应:%d, 剩余:%d',
    [
      FHttpSvr.Moniter.PostWSASendCounter,
      FHttpSvr.Moniter.ResponseWSASendCounter,
      FHttpSvr.Moniter.PostWSASendCounter - FHttpSvr.Moniter.ResponseWSASendCounter
    ]);
  Label8.Caption := TransByteSize(FHttpSvr.Moniter.SentSize);
  Label6.Caption := Format('套接字句柄'#9'创建:%d, 销毁:%d',
    [
      FHttpSvr.Moniter.HandleCreateCounter,
      FHttpSvr.Moniter.HandleDestroyCounter
    ]);
  Label9.Caption := Format('AcceptEx信息'#9'创建:%d, 投递:%d, 回应:%d',
    [
      FHttpSvr.Moniter.AcceptExObjectCounter,
      FHttpSvr.Moniter.PostWSAAcceptExCounter,
      FHttpSvr.Moniter.ResponseWSAAcceptExCounter
    ]);
  Label10.Caption := Format('连接信息'#9'创建:%d, 借出:%d, 还回:%d',
    [
      FHttpSvr.Moniter.ContextCreateCounter,
      FHttpSvr.Moniter.ContextOutCounter,
      FHttpSvr.Moniter.ContextReturnCounter
    ]);
  Label11.Caption := Format('发送请求'#9'创建:%d, 借出:%d, 还回:%d',
    [
      FHttpSvr.Moniter.SendRequestCreateCounter,
      FHttpSvr.Moniter.SendRequestOutCounter,
      FHttpSvr.Moniter.SendRequestReturnCounter
    ]);
  Label12.Caption := Format('运行时间'#9'%s', [GetRunTimeInfo]);
end;

end.
