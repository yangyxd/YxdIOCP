program HttpService;

{

 这是一个命令行的 Http 服务
 启动方式
 httpservice 端口 WEB目录
 httpservice 8080 ./dist/
 httpservice 80 D:\web\
}

{$APPTYPE CONSOLE}

{$R *.res}

uses
  YxdServer,
  iocp, iocp.Http, ShlwApi,
  Winapi.Windows,
  System.SysUtils;

type
  TAppSvr = class(TServiceApplication)
  private
    FSvr: TIocpHttpServer;
  protected
    procedure ServiceInitialize; override;
    procedure ServiceExecute; override;
    procedure OnHttpExecute(Sender: TIocpHttpServer;
      Request: TIocpHttpRequest; Response: TIocpHttpResponse);
  public
    destructor Destroy; override;
  end;


// 取绝对路径的函数。需要引用 ShlwApi.pas
// BasePath: 是参考路径。RelativePath: 是指相对路径
function GetAbsolutePathEx(BasePath, RelativePath:string):string;
var
  Dest:array [0..MAX_PATH] of char;
begin
  FillChar(Dest,MAX_PATH+1,0);
  PathCombine(Dest,PChar(BasePath), PChar(RelativePath));
  Result:=string(Dest);
end;

{ TAppSvr }

destructor TAppSvr.Destroy;
begin
  FreeAndNil(FSvr);
  inherited;
end;

procedure TAppSvr.OnHttpExecute(Sender: TIocpHttpServer;
  Request: TIocpHttpRequest; Response: TIocpHttpResponse);
begin
  Response.SendFile(Request, '', False, True);
end;

procedure TAppSvr.ServiceExecute;
begin
  Writeln('Http Service Execute...');
  FSvr.Start;
  Writeln(Format('Port: %d', [Fsvr.ListenPort]));
  Writeln(Format('Root Path: %s', [Fsvr.WebPath]));
  inherited ServiceExecute;
  Writeln('Http Service stop...');
  FSvr.Stop;
end;

procedure TAppSvr.ServiceInitialize;
var
  Port: Integer;
  RootPath: string;
begin
  inherited;
  Port := 8080;
  RootPath := '';
  if (ParamCount > 0) then begin
    Port := StrToIntDef(Trim(ParamStr(1)), Port);
    RootPath := GetAbsolutePathEx(GetCurrentDir, StringReplace(Trim(ParamStr(2)), '/', '\', [rfReplaceAll]));
  end;
  FSvr := TIocpHttpServer.Create(nil);
  FSvr.ListenPort := Port;
  if RootPath <> '' then
    FSvr.WebPath := RootPath;
  FSvr.OnHttpRequest := OnHttpExecute;
  Writeln('Http Service loading...');
end;

var
  FSvr: TAppSvr;
begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    FSvr := TAppSvr.Create;
    FSvr.Start;
    FreeAndNil(FSvr);
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
