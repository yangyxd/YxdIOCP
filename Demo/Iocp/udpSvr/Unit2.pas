unit Unit2;

interface

uses
  YxdWorker, iocp,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls, ExtCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    CheckBox1: TCheckBox;
    Edit3: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit19: TEdit;
    Button32: TButton;
    ServerInfo: TListBox;
    PopupMenu2: TPopupMenu;
    MenuItem1: TMenuItem;
    N2: TMenuItem;
    MenuItem3: TMenuItem;
    muScroll: TMenuItem;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure MenuItem1Click(Sender: TObject);
    procedure Button32Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    RunRef: Integer;
    procedure DoPostFuncTest(AJob: PJob);
    procedure DoFuncTest(AJob: PJob);
    procedure DoFuncTestDone(AJob: PJob);
  public
    { Public declarations }
    procedure CopyToClipbrd(const Value: string);
    procedure SingleSend();
    procedure FuncTest(const AData: string; TestCount, ThdCount: Integer;
      const Addr: string; port: Word);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses
  Clipbrd;
  
type
  PFuncTestItem = ^TFuncTestItem;
  TFuncTestItem = packed record
    SendData: string;
    RecvData: string;
    Count: Integer;
    Runs:Integer;
    ErrRef: Integer;
    ThdCount: Integer;
    PostCount: Integer;
    StartTime: Int64;
    Addr: string;
    Port: Word;
  end;

procedure TForm2.Button32Click(Sender: TObject);
begin
  if Length(Edit19.Text) = 0 then Exit;
  if CheckBox1.Checked then
    FuncTest(Edit19.Text, StrToIntDef(Edit3.Text, 1), StrToIntDef(Edit4.Text, 1),
      Edit1.Text, StrToIntDef(Edit2.Text, 9983))
  else
    SingleSend();
end;

procedure TForm2.CopyToClipbrd(const Value: string);
var
  S: TClipboard;
begin
  S := TClipboard.Create;
  try
    S.Clear;
    S.SetTextBuf(PChar(Value)); 
  finally
    FreeAndNil(S);
  end;
end;

procedure TForm2.DoFuncTest(AJob: PJob);
var
  FUDPSend: TIocpUdpSocket;
  Item: PFuncTestItem;
  I: Integer;
  R: string;
begin
  if AJob.IsTerminated then Exit;  
  Item := AJOb.Data;
  I := InterlockedIncrement(Item.Runs);
  if I > Item.Count then Exit;  
  InterlockedIncrement(RunRef);
  FUDPSend := TIocpUdpSocket.Create(nil);
  try
    // 发送数据包
    FUDPSend.Connect(Item.Addr, Item.Port);
    FUDPSend.Send(Item.SendData);
    FUDPSend.ReadTimeOut := 5000;
    R := FUDPSend.Recv();
    if Length(R) < 1 then
      InterlockedIncrement(Item.ErrRef);
  finally
    FUDPSend.Disconnect;
    FreeAndNil(FUDPSend);
    if (I = Item.Count) and (Assigned(Workers)) then begin
      Item.RecvData := R;
      Item.StartTime := GetTimeStamp - Item.StartTime;
      Workers.Post(DoFuncTestDone, Item, True); 
    end;
  end;
end;

procedure TForm2.DoFuncTestDone(AJob: PJob);
var
  Item: PFuncTestItem;
begin
  Item := AJOb.Data;
  ProgressBar1.Position := Item.Runs;
  ServerInfo.Items.Add(Format('Time Used=%d, Runs=%d, Speed=%d, ErrRef: %d, Recv: %s',
          [Item.StartTime, Item.Runs, Int64(Item.Runs)*1000 div Item.StartTime,
          Item.ErrRef, Item.RecvData]));
  if muScroll.Checked then
    ServerInfo.TopIndex := ServerInfo.Count - 1;
  Button32.Enabled := True;
  ProgressBar1.Visible := False;
  Dispose(Item);
end;

procedure TForm2.DoPostFuncTest(AJob: PJob);
var
  I: Integer;
  Item: PFuncTestItem;
begin
  Item := AJOb.Data;
  while not AJob.IsTerminated do begin
    I := InterlockedIncrement(Item.PostCount);
    if i > Item.Count then
      Break
    else
      Workers.Post(DoFuncTest, Item);   
  end; 
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ServerInfo.DoubleBuffered := True;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  if Assigned(Workers) then
    Workers.Clear(Self);
end;

procedure TForm2.FuncTest(const AData: string; TestCount, ThdCount: Integer;
  const Addr: string; port: Word);
var
  Item: PFuncTestItem;
  I: Integer;
begin
  Button32.Enabled := False;

  if not Assigned(Workers) then
    Workers := TYXDWorkers.Create();
  if Workers.MaxWorkers < ThdCount then
    Workers.MaxWorkers := ThdCount;

  ProgressBar1.Min := 0;
  ProgressBar1.Max := TestCount;
  ProgressBar1.Position := 0;
  ProgressBar1.Visible := True;
  RunRef := 0;
    
  New(Item);
  FillChar(Item^, SizeOf(TFuncTestItem), 0);
  Item.SendData := AData;
  Item.Count := TestCount;
  Item.Runs := 0;
  Item.PostCount := 0;
  Item.Addr := Addr;
  Item.Port := port;
  Item.ThdCount := ThdCount;
  Item.StartTime := GetTimestamp;

  for I := 0 to ThdCount - 1 do
    Workers.Post(DoPostFuncTest, Item); 
end;

procedure TForm2.MenuItem1Click(Sender: TObject);
begin
  if ServerInfo.ItemIndex <> -1 then
    CopyToClipbrd(ServerInfo.items[ServerInfo.ItemIndex]);
end;

procedure TForm2.MenuItem3Click(Sender: TObject);
begin
  ServerInfo.Clear;
end;

procedure TForm2.SingleSend;
var
  FUDPSend: TIocpUdpSocket;
  Buf: TBytes;
  i: Integer;
begin
  FUDPSend := TIocpUdpSocket.Create(nil);
  FUDPSend.ReadTimeOut := 5000;
  try
    try
      FUDPSend.ReadTimeOut := 5000;
      FUDPSend.Send(Edit19.Text, Edit1.Text, StrToIntDef(Edit2.Text, 9983));
      SetLength(Buf, 10240);
      I := FUDPSend.Recv(@Buf[0], 10240);
      if i > 0 then begin
        SetLength(Buf, i);
        ServerInfo.Items.Add(string(Buf));
        if muScroll.Checked then
          ServerInfo.TopIndex := ServerInfo.Count - 1;
      end;
    finally
      FreeAndNil(FUDPSend);
    end;
  except
    ServerInfo.Items.Add(Exception(ExceptObject).Message);
  end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  if ProgressBar1.Visible then
    ProgressBar1.Position := RunRef;
end;

end.
