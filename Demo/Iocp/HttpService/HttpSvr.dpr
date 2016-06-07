program HttpSvr;

{$I 'CMOV.inc'}

uses
  Forms,
  SysUtils,
  iocp,
  iocp.Utils.Queues,
  uServer in 'uServer.pas',
  iocp.Http.Client in 'Include\IOCP\iocp.Http.Client.pas',
  iocp.Http in 'Include\Iocp\iocp.Http.pas',
  iocp.RawSockets in 'Include\IOCP\iocp.RawSockets.pas',
  iocp.Sockets in 'Include\IOCP\iocp.Sockets.pas',
  iocp.Sockets.Utils in 'Include\IOCP\iocp.Sockets.Utils.pas',
  iocp.Utils.Hash in 'Include\Iocp\iocp.Utils.Hash.pas',
  iocp.Utils.Dictionary in 'Include\IOCP\iocp.Utils.Dictionary.pas',
  uFrmMain in 'uFrmMain.pas' {HttpService};

{ TServer }


begin
  Application.Initialize;
  ReportMemoryLeaksOnShutdown := True;  
  Application.MainFormOnTaskbar := True;
  Application.Title := 'HTTP·þÎñ';
  Application.CreateForm(THttpService, HttpService);
  Application.Run;
  
end.
