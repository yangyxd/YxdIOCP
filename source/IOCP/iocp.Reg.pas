unit iocp.Reg;

interface

procedure Register;

implementation

{$DEFINE UseHttpServer}
{$IFDEF UseHttpServer}
{$DEFINE UseWebSocketServer}
  {$IF (RTLVersion>=26)}
  {$DEFINE UseWebMvcServer}
  {$IFEND}
{$ENDIF}

uses
  iocp,
  {$IFDEF UseHttpServer}iocp.Http, {$ENDIF}
  {$IFDEF UseWebSocketServer}iocp.Http.WebSocket, {$ENDIF}
  {$IFDEF UseWebMvcServer}iocp.Http.MVC, {$ENDIF}
  iocp.Http.Client,
  Classes;

const
  ComPageName = 'YxdIOCP';

procedure Register;
begin
  RegisterComponents(ComPageName, [TIocpUdpSocket]);
  RegisterComponents(ComPageName, [TIocpUdpServer]);
  RegisterComponents(ComPageName, [TIocpTcpSocket]);
  RegisterComponents(ComPageName, [TIocpTcpClient]);
  RegisterComponents(ComPageName, [TIocpTcpServer]);
  RegisterComponents(ComPageName, [TIocpTcpCodecServer]);
  {$IFDEF UseHttpServer}
  RegisterComponents(ComPageName, [TIocpHttpServer]);
  RegisterComponents(ComPageName, [TIocpHttpsServer]);
  {$ENDIF}
  RegisterComponents(ComPageName, [TIocpHttpClient]);
  {$IFDEF UseWebSocketServer}
  RegisterComponents(ComPageName, [TIocpWebSocketServer]);
  {$ENDIF}
  {$IFDEF UseWebMvcServer}
  RegisterComponents(ComPageName, [TIocpHttpMvcServer]);
  {$ENDIF}
end;

end.
