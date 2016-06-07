unit uServer;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, Iocp, iocp.Utils.Hash;
  
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
  /// 服务系统
  /// </summary>
  TPtService = class(TObject)
  private
    FPtWebService: TIocpHttpServer;
    FOnWriteLog: TOnWriteLog;
    HttpReqRef: Integer;
  protected
    function IsDestroying: Boolean;
    procedure Log(Sender: TObject; AType: TXLogType; const Msg: string);
    procedure LogD(Sender: TObject; const Msg: string);
    procedure LogE(Sender: TObject; const Title: string; E: Exception);
    procedure DoWriteLog(Sender: TObject; AType: TXLogType; const Msg: string);
  protected
    procedure DoRequest(Sender: TIocpHttpServer; Request: TIocpHttpRequest; Response: TIocpHttpResponse);
  public
    constructor Create(Port: Word); reintroduce;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
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
  FPtWebService := TIocpHttpServer.Create(nil);
  FPtWebService.ListenPort := Port;
  FPtWebService.UploadMaxDataSize := 1024 * 1024;
  FPtWebService.OnHttpRequest := DoRequest;
end;

destructor TPtService.Destroy;
begin
  try
    Stop;
    FreeAndNil(FPtWebService);
  except
    LogE(Self, 'DoDestroy', Exception(ExceptObject));  
  end;
  inherited Destroy;
end;

procedure TPtService.DoRequest(Sender: TIocpHttpServer;
  Request: TIocpHttpRequest; Response: TIocpHttpResponse);
var
  Path: string;
begin
  InterlockedIncrement(HttpReqRef);
  Path := StringReplace(Request.URI, '/', '\', [rfReplaceAll]);
  if (Length(Path) > 0) and (Path[1] = '\') then
    Delete(Path, 1, 1);
  Path := SoftPath + Path;
  if FileExists(Path) then begin
    Response.SendFile(Path, '', False, True);
  end else
    Response.ErrorRequest(404);
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

procedure TPtService.Start;
begin
  FPtWebService.Open;
end;

procedure TPtService.Stop;
begin
  FPtWebService.Close;
end;  

initialization
  SoftPath := ExtractFilePath(ParamStr(0));

finalization

end.