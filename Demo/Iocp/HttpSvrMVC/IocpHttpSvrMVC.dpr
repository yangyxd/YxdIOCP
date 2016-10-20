program IocpHttpSvrMVC;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  MvcDemo in 'MvcDemo.pas',
  YxdJson in 'Json\YxdJson.pas',
  YxdRtti in 'Json\YxdRtti.pas',
  YxdStr in 'Json\YxdStr.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
