unit uFrmMain;

interface

uses
  uServer,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, XPMan, Buttons, ExtCtrls;

type
  THttpService = class(TForm)
    Edit1: TEdit;
    XPManifest1: TXPManifest;
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Timer1: TTimer;
    Label5: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FSvr: TPtService;
  public
    { Public declarations }
  end;

var
  HttpService: THttpService;

implementation

{$R *.dfm}

uses iocp;

procedure THttpService.BitBtn1Click(Sender: TObject);
begin
  if Assigned(FSvr) then begin
    FreeAndNil(FSvr);
    BitBtn1.Caption := '启动服务';
  end else begin
    BitBtn1.Enabled := False;
    FSvr := TPtHttpService.Create(StrToIntDef(Edit1.Text, 8080));
    FSvr.Start;
    BitBtn1.Caption := '停止服务';
    BitBtn1.Enabled := True;
  end
end;

procedure THttpService.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  Edit1.DoubleBuffered := True;
  BitBtn1.DoubleBuffered := True;
end;

procedure THttpService.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FSvr);
end;

procedure THttpService.Timer1Timer(Sender: TObject);

  function GetLinkCount(): Integer;
  begin
    if Assigned(FSvr) then
      Result := FSvr.WebService.ClientCount
    else
      Result := 0;
  end;

begin
  Label4.Caption := Format('CPU: %d%%, 内存: %s, 线程: %d',
    [
      iocp.GetCPUUsage,
      RollupSize(GetProcessMemUse(GetCurrentProcessId())),
      GetThreadCount(GetCurrentProcessId)]);
  Label5.Caption := Format('连接数: %d, 工作线程: %d/%d, 运行: %s',
    [
      GetLinkCount(),
      GetTaskWorkerCount(),
      GetTaskWorkerMaxCount(),
      GetRunTimeInfo]);
end;

end.
