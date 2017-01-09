program HttpsServer;

{$I 'CMOV.inc'}

uses
  Forms,
  SysUtils,
  uServer in 'uServer.pas',
  uFrmMain in 'uFrmMain.pas' {HttpService},
  iocp.Http.WebSocket in '..\..\..\source\IOCP\iocp.Http.WebSocket.pas',
  OpenSSL in '..\..\..\source\IOCP\OpenSSL.pas',
  iocp.Http in '..\..\..\source\IOCP\iocp.Http.pas';

{ TServer }


begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;  
  Application.MainFormOnTaskbar := True;
  Application.Title := 'HTTP·þÎñ';
  Application.CreateForm(THttpService, HttpService);
  Application.Run;
  
end.


