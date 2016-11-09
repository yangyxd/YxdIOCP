unit uServer;

interface

uses
  Iocp, iocp.Utils.Hash, iocp.Http, iocp.Http.Websocket,
  Windows, SysUtils, Classes, SyncObjs;
  
type
  /// <summary>
  /// 日志信息类型
  /// </summary>
  TXLogType = (log_Debug, {调试信息} log_Info {信息}, log_Warning {警告},
    log_Error {错误});

type
  /// <summary>
  /// 日志写入处理
  /// </summary>
  TOnWriteLog = procedure (Sender: TObject; AType: TXLogType;
    const Log: string) of object;

  /// <summary>
  /// 任务处理请求
  /// </summary>
  TOnProcRequest = procedure (Request: TIocpHttpRequest; Response: TIocpHttpResponse) of object;

  /// <summary>
  /// HTTP服务系统
  /// </summary>
  TPtService = class(TObject)
  private
    //FPtWebService: TIocpHttpServer;
    FPtWebService: TIocpWebSocketServer;
    FOnWriteLog: TOnWriteLog;
    HttpReqRef: Integer;
    FProcList: TStringHash;
    FHtmlFileExts: TStringHash;
  protected
    function IsDestroying: Boolean;
    procedure Log(Sender: TObject; AType: TXLogType; const Msg: string);
    procedure LogD(Sender: TObject; const Msg: string);
    procedure LogE(Sender: TObject; const Title: string; E: Exception);
    procedure DoWriteLog(Sender: TObject; AType: TXLogType; const Msg: string);
  protected
    procedure DoRequest(Sender: TIocpHttpServer; Request: TIocpHttpRequest; Response: TIocpHttpResponse);
    procedure DoWebSocketRequest(Sender: TIocpWebSocketServer; Request: TIocpWebSocketRequest; Response: TIocpWebSocketResponse);
    procedure DoRegProc(); virtual; abstract;
    procedure DoFreeProcItem(Item: PHashItem);
  public
    constructor Create(Port: Word); reintroduce;
    destructor Destroy; override;

    procedure RegProc(const URI: string; const Proc: TOnProcRequest);

    procedure Start;
    procedure Stop;
  end;

type
  TPtHttpService = class(TPtService)
  protected
    procedure DoRegProc(); override;
    procedure RequestDemo03(Request: TIocpHttpRequest; Response: TIocpHttpResponse);
  end;

implementation

type
  PMethod = ^TMethod;

var
  SoftPath: string;

{ TPtService }

constructor TPtService.Create(Port: Word);
begin
  FOnWriteLog := DoWriteLog;
  //FPtWebService := TIocpHttpServer.Create(nil);
  FPtWebService := TIocpWebSocketServer.Create(nil);
  FPtWebService.ListenPort := Port;
  FPtWebService.UploadMaxDataSize := 1024 * 1024;
  FPtWebService.MaxTaskWorker := 64;
  FPtWebService.MaxContextPoolSize := 1;
  FPtWebService.OnHttpRequest := DoRequest;
  FPtWebService.OnWebSocketRequest := DoWebSocketRequest;

  FProcList := TStringHash.Create();
  FProcList.OnFreeItem := DoFreeProcItem;

  FHtmlFileExts := TStringHash.Create();
  FHtmlFileExts.Add('.html', 1);
  FHtmlFileExts.Add('.htm', 1);
  FHtmlFileExts.Add('.xml', 1);
  FHtmlFileExts.Add('.xmls', 1);
  FHtmlFileExts.Add('.json', 1);

  DoRegProc(); 
end;

destructor TPtService.Destroy;
begin
  try
    Stop;
    FreeAndNil(FPtWebService);
    FreeAndNil(FProcList);
    FreeAndNil(FHtmlFileExts);
  except
    LogE(Self, 'DoDestroy', Exception(ExceptObject));  
  end;
  inherited Destroy;
end;

procedure TPtService.DoFreeProcItem(Item: PHashItem);
begin
  if Item <> nil then
    Dispose(Pointer(Item.Value));
end;

procedure TPtService.DoRequest(Sender: TIocpHttpServer;
  Request: TIocpHttpRequest; Response: TIocpHttpResponse);
var
  Path: string;
  V: Number;
begin
  InterlockedIncrement(HttpReqRef);
  Path := StringReplace(string(Request.URI), '/', '\', [rfReplaceAll]);
  if (Length(Path) > 0) and (Path[1] = '\') then
    Delete(Path, 1, 1);
  Path := SoftPath + Path;
  // 如果是一个文件
  if FileExists(Path) then begin
    // 如果是网页，就不使用下载方式
    if FHtmlFileExts.Exists(LowerCase(ExtractFileExt(Path))) then
      Response.SendFile(Path, '', False, True)
    else
      Response.SendFile(Path, '', True, True);
  end else begin
    V := FProcList.ValueOf(LowerCase(string(Request.URI)));
    if V <> -1 then begin
      TOnProcRequest(PMethod(Pointer(V))^)(Request, Response);
    end else
      Response.ErrorRequest(404);
  end;
end;

procedure TPtService.DoWebSocketRequest(Sender: TIocpWebSocketServer;
  Request: TIocpWebSocketRequest; Response: TIocpWebSocketResponse);
var
  S: TMemoryStream;
  Data: string;
begin
  //OutputDebugString(PChar(Request.DataString()));
  S := TMemoryStream.Create;
  try
    Data := Request.DataString(hct_UTF8);
    S.Write(Data[1], Length(Data) {$IFDEF UNICODE} shl 1{$ENDIF});
    S.Position := 0;
    Response.Send(S, wso_Text);
  finally
    S.Free;
  end;
  Response.Send(Request.DataString());
end;

procedure TPtService.DoWriteLog(Sender: TObject; AType: TXLogType;
  const Msg: string);
begin
end;

function TPtService.IsDestroying: Boolean;
begin
  Result := (not Assigned(Self));
end;

procedure TPtService.Log(Sender: TObject; AType: TXLogType; const Msg: string);
begin
  if Assigned(FOnWriteLog) and (not IsDestroying) then
    FOnWriteLog(Sender, AType, Msg);  
end;

procedure TPtService.LogD(Sender: TObject; const Msg: string);
begin
  if Assigned(FOnWriteLog) and (not IsDestroying) then
    FOnWriteLog(Sender, log_Debug, Msg);
end;

procedure TPtService.LogE(Sender: TObject; const Title: string; E: Exception);
begin
  if Assigned(FOnWriteLog) and (not IsDestroying) then begin
    if E = nil then
      FOnWriteLog(Sender, log_Error, Format('[%s] %s', [Sender.ClassName, Title]))
    else
      FOnWriteLog(Sender, log_Error, Format('[%s] %s Error: %s',
        [Sender.ClassName, Title, E.Message]))
  end;
end;

procedure TPtService.RegProc(const URI: string; const Proc: TOnProcRequest);
var
  P: PMethod;
begin
  if Length(URI) = 0 then Exit;
  if Assigned(Proc) then begin
    New(P);
    P^ := TMethod(Proc);
    FProcList.Add(LowerCase(URI), Integer(P));
  end;
end;

procedure TPtService.Start;
begin
  FPtWebService.Open;
end;

procedure TPtService.Stop;
begin
  FPtWebService.Close;
end;  

{ TPtHttpService }

procedure TPtHttpService.DoRegProc;
begin
  RegProc('/RequestDemo03.o', RequestDemo03);
end;

procedure TPtHttpService.RequestDemo03(Request: TIocpHttpRequest;
  Response: TIocpHttpResponse);
var
  O: TIocpHttpWriter;
begin
  O := Response.GetOutWriter();
  O.Charset := hct_GB2312;
  O.Write('Data: ').Write(Request.GetDataString(Request.CharSet)).Write('<br>');
  O.Write('编号: ').Write(Request.GetParam('userid')).Write('<br>');
  O.Write('用户名: ').Write(Request.GetParam('username')).Write('<br>');
  O.Write('密码: ').Write(Request.GetParam('userpass')).Write('<br>');
  O.Write('性别: ').Write(Request.GetParam('sex')).Write('<br>');
  O.Write('部门: ').Write(Request.GetParam('dept')).Write('<br>');
  O.Write('兴趣: ').Write(Request.GetParamValues('inst')).Write('<br>');
  O.Write('说明: ').Write(Request.GetParam('note')).Write('<br>');
  O.Write('隐藏内容: ').Write(Request.GetParam('hiddenField')).Write('<br>');
  O.Flush;
end;

initialization
  SoftPath := ExtractFilePath(ParamStr(0));

finalization

end.