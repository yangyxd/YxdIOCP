unit Unit1;

interface

uses
  uMsgBase,
  YxdWorker, YxdCommon, YxdHash,
  iocp, 
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    edtPort: TEdit;
    pgcMain: TPageControl;
    TabSheet1: TTabSheet;
    tsMonitor: TPanel;
    tsLog: TTabSheet;
    mmoLog: TMemo;
    Button3: TButton;
    Button2: TButton;
    Button1: TButton;
    Edit1: TEdit;
    Timer1: TTimer;
    Button5: TButton;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    iCounter: Integer;
    FClient: TIocpTcpClient;
    FSendStr: string;

    procedure Msg(const s: string);
  public
    { Public declarations }
    procedure OnContextConnected(const pvContext: TIocpContext);
    procedure OnClientRecvBuffer(const pvClientContext: TIocpContext; buf: Pointer;
        len:cardinal; errCode:Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  uFMMonitor, utils.buffer;

procedure TForm1.Button1Click(Sender: TObject);
var
  I: Integer;
  Host: string;
begin
  Host := Trim(Edit1.Text);
  for I := 0 to 1000 - 1 do
    FClient.Connect(Host, StrToIntDef(edtPort.Text, 9983), True);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FClient.Connect(Edit1.Text, StrToIntDef(edtPort.Text, 9983), True);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if not Assigned(FClient) then
    Exit;
  FClient.Delete(0);
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  if not Assigned(FClient) then
    Exit;
  FClient.RemoveAll();
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  FSendStr := Memo1.Lines.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin 
  iCounter := 0;

  FClient := TIocpTcpClient.Create(Self);
  FClient.createDataMonitor;
  FClient.OnContextConnected := OnContextConnected;
  FClient.OnDataReceived := OnClientRecvBuffer;
  TFMMonitor.createAsChild(tsMonitor, FClient);
  FSendStr := Memo1.Lines.Text;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FClient);
end;

procedure TForm1.Msg(const s: string);
begin
  mmoLog.Lines.Add(s);
  OutputDebugString(PChar(s));
end;

procedure TForm1.OnClientRecvBuffer(const pvClientContext: TIocpContext;
  buf: Pointer; len: cardinal; errCode: Integer);
begin
end;

procedure TForm1.OnContextConnected(const pvContext: TIocpContext);
begin
  pvContext.Send(FSendStr);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if not Assigned(FClient) then Exit;  
  Caption := Format('CPU: %d%%, Worker: %d, Client Count: %d',
    [GetCPUUsage, GetTaskWorkerCount(), FClient.Count]);
end;

end.
