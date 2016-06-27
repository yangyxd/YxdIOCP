program HttpSvr;

{$I 'CMOV.inc'}

uses
  Forms,
  SysUtils,
  iocp,
  iocp.Utils.Queues,
  uServer in 'uServer.pas',
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
