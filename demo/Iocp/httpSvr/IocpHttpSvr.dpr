program IocpHttpSvr;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  utils.buffer in 'utils.buffer.pas',
  QSimplePool in 'QSimplePool.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
