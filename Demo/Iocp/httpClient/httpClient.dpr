program httpClient;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  iocp.Utils.Hash in '..\..\..\source\IOCP\iocp.Utils.Hash.pas',
  iocp.Utils.LinkedList in '..\..\..\source\IOCP\iocp.Utils.LinkedList.pas',
  iocp.Http.Client in '..\..\..\source\IOCP\iocp.Http.Client.pas',
  iocp.Http in '..\..\..\source\IOCP\iocp.Http.pas',
  iocp.Utils.Str in '..\..\..\source\IOCP\iocp.Utils.Str.pas',
  iocp.Sockets.Utils in '..\..\..\source\IOCP\iocp.Sockets.Utils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
