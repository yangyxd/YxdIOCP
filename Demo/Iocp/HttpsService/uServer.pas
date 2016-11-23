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
    //FPtWebService: TIocpWebSocketServer;
    FPtWebService: TIocpHttpsServer;
    FOnWriteLog: TOnWriteLog;
    HttpReqRef: Integer;
    FProcList: TStringHash;
    FHtmlFileExts: TStringHash;
    function GetWebService: TIocpHttpServer;
  protected
    function IsDestroying: Boolean;
    procedure Log(Sender: TObject; AType: TXLogType; const Msg: string);
    procedure LogD(Sender: TObject; const Msg: string);
    procedure LogE(Sender: TObject; const Title: string; E: Exception);
    procedure DoWriteLog(Sender: TObject; AType: TXLogType; const Msg: string);
    procedure DoStateInfo(Sender: TObject; MsgType: TIocpStateMsgType;
      const Msg: string);
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

    property WebService: TIocpHttpServer read GetWebService;
  end;

type
  TPtHttpService = class(TPtService)
  protected
    procedure DoRegProc(); override;
    procedure RequestDemo03(Request: TIocpHttpRequest; Response: TIocpHttpResponse);
    procedure RequestHello(Request: TIocpHttpRequest; Response: TIocpHttpResponse);
    procedure RequestUpfile(Request: TIocpHttpRequest; Response: TIocpHttpResponse);
  end;

implementation

type
  PMethod = ^TMethod;

{ TPtService }

constructor TPtService.Create(Port: Word);
begin
  FOnWriteLog := DoWriteLog;
  //FPtWebService := TIocpHttpServer.Create(nil);
  //FPtWebService := TIocpWebSocketServer.Create(nil);
  FPtWebService := TIocpHttpsServer.Create(nil);
  FPtWebService.ListenPort := Port;
  FPtWebService.UploadMaxDataSize := 1024 * 1024;
  FPtWebService.MaxTaskWorker := 64;
  FPtWebService.MaxContextPoolSize := 1;
  FPtWebService.OnHttpRequest := DoRequest;
  FPtWebService.OnStateInfo := DoStateInfo;
  //FPtWebService.OnWebSocketRequest := DoWebSocketRequest;

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
  V: Number;
begin
  InterlockedIncrement(HttpReqRef);
  V := FProcList.ValueOf(LowerCase(string(Request.URI)));
  if V <> -1 then begin
    TOnProcRequest(PMethod(Pointer(V))^)(Request, Response);
  end else
    Response.SendFileByURI(string(Request.URI), '', False, True);
end;

procedure TPtService.DoStateInfo(Sender: TObject; MsgType: TIocpStateMsgType;
  const Msg: string);
begin
  DoWriteLog(Sender, TXLogType(Ord(MsgType)), Msg);
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
  {$IFDEF DEBUG}
  OutputDebugString(PChar(Format('[%s] %s', [Sender.ClassName, Msg])));
  {$ENDIF}
end;

function TPtService.GetWebService: TIocpHttpServer;
begin
  Result := FPtWebService;
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
  RegProc('/Hello', RequestHello);
  RegProc('/upfile', RequestUpfile);
end;

procedure TPtHttpService.RequestDemo03(Request: TIocpHttpRequest;
  Response: TIocpHttpResponse);
var
  O: TIocpHttpWriter;
begin
  O := Response.GetOutWriter();
  O.Charset := hct_GB2312;
  O.Write('Data: ').Write(Request.GetDataString(string(Request.CharSet))).Write('<br>');
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

procedure TPtHttpService.RequestHello(Request: TIocpHttpRequest;
  Response: TIocpHttpResponse);
begin
  Response.Send('Hello');
end;

// 上传文件处理
procedure TPtHttpService.RequestUpfile(Request: TIocpHttpRequest;
  Response: TIocpHttpResponse);
const
  UpFileDir = 'files';
var
  S: TStream;
  F: TFileStream;
  FName, Path: string;
begin
  if Request.IsPost and Request.IsFormData then begin // 判断是否为表单数据
    F := nil;
    with Request.FormData['fname'] do begin // 表单中的文件数据字段
      S := GetContentStream;  // 内容流
      if Assigned(S) then begin
        S.Position := 0;
        try
          // 得到文件保存位置
          Path := Request.Owner.WebPath + UpFileDir + '\';
          if not DirectoryExists(Path) then
            ForceDirectories(Path);
          // 生成一个文件名
          FName := FileName;
          while FileExists(Path + FName) do begin
            FName := FormatDateTime('HHMMSSZZZ', Now) + FileName;
          end;
          // 写入文件
          F := TFileStream.Create(Path + FName, fmCreate);
          F.CopyFrom(S, S.Size);
        finally
          FreeAndNil(F);
          S.Free;
        end;
        // 返回状态给浏览器
        Response.Send(
          Format('{"result":"success.","ObjectFileName":"%s","SourceFileName":"%s"}',
            [UpFileDir + '/' + FName, FileName]));
      end else
        Response.Send('无效请求数据');
    end;
  end else
    Response.ErrorRequest();
end;

initialization

finalization

end.