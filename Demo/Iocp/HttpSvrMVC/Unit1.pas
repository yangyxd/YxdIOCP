unit Unit1;

interface

uses
  MvcDemo,
  iocp, iocp.http, iocp.Http.MVC, iocp.Utils.GMTTime,
  Rtti, YxdRtti, YxdJson,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, iocp.Sockets, iocp.Http.WebSocket;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Button3: TButton;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DoWebSocketConnected(
      Sender: TIocpWebSocketServer; Connection: TIocpWebSocketConnection);
  private
    function DoSerializeData(Sender: TObject; const Value: TValue): string;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ResetRunTime;
  Button1.Enabled := False;
  HttpMvc.UseWebSocket := CheckBox1.Checked;
  HttpMvc.ListenPort := StrToIntDef(Edit1.Text, HttpMvc.ListenPort);
  if HttpMvc.UseWebSocket then
    HttpMvc.WebSocketServer.OnWebSocketConnected := DoWebSocketConnected;
  HttpMvc.Active := True;
  Button1.Enabled := not HttpMvc.Active;
  Button2.Enabled := HttpMvc.Active;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  HttpMvc.Active := False;
  Button1.Enabled := not HttpMvc.Active;
  Button2.Enabled := HttpMvc.Active;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  HttpMvc.Server.DisconnectAll;
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  Mvc: TMvcDemo;
begin
  Mvc := TMvcDemo.Create;
  if Mvc.ExistAttribute('service') then
    ShowMessage(Mvc.ClassName);
  Mvc.Free;
end;

function TForm1.DoSerializeData(Sender: TObject; const Value: TValue): string;
var
  Json: JSONObject;
begin
  Json := JSONObject.Create;
  try
    TYxdSerialize.WriteValue(Json, '', Value);
  finally
    Result := Json.ToString();
    Json.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitHttpMvcServer;
  HttpMvc.OnSerializeData := DoSerializeData;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Edit1.Text := IntToStr(HttpMvc.ListenPort);
  CheckBox1.Checked := HttpMvc.UseWebSocket;
  Button1.Enabled := not HttpMvc.Active;
  Button2.Enabled := HttpMvc.Active;
end;

procedure TForm1.DoWebSocketConnected(
  Sender: TIocpWebSocketServer; Connection: TIocpWebSocketConnection);
begin
  Connection.Response.Send('连接成功，欢迎加入。');
end;

end.
