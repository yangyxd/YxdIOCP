unit Unit1;

interface

uses
  MvcDemo,
  iocp, iocp.http, iocp.Http.MVC, iocp.Utils.GMTTime, iocp.Http.Client,
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
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DoWebSocketConnected(
      Sender: TIocpWebSocketServer; Connection: TIocpWebSocketConnection);
  private
    function DoSerializeData(Sender: TObject; const Value: TValue): string;
    function DoDeSerializeData(Sender: TObject; const Value: string;
      const Dest: TValue; IsGet: Boolean): Boolean;
    procedure DoReadedItem(const Name, Value: string; Data: Pointer);
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

// 反序列化处理
function TForm1.DoDeSerializeData(Sender: TObject; const Value: string;
  const Dest: TValue; IsGet: Boolean): Boolean;
var
  Json: JSONObject;
  S: AnsiString;
begin
  if not IsGet then begin
    Json := JSONObject.ParseObject(Value, False);
    try
      if Assigned(Json) then
        TYxdSerialize.ReadValue(Json, Dest);
    finally
      FreeAndNil(Json);
    end;
  end else begin
    // Get 请求单独处理
    Json := JSONObject.Create;
    try
      S := AnsiString(Value);
      HttpMvc.Server.DecodeParam(PAnsiChar(S), Length(S), DoReadedItem, Json);
      TYxdSerialize.ReadValue(Json, Dest);
    finally
      FreeAndNil(Json);
    end;
  end;
  Result := True;
end;

procedure TForm1.DoReadedItem(const Name, Value: string; Data: Pointer);
begin
  JSONObject(Data).Put(Name, Value);
end;

// 序列化处理
function TForm1.DoSerializeData(Sender: TObject; const Value: TValue): string;
begin
  Result := JSONObject.Serialize(Value);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitHttpMvcServer;
  JSONObject.SetJsonCaseSensitive(False);
  HttpMvc.OnSerializeData := DoSerializeData;
  HttpMvc.OnDeSerializeData := DoDeSerializeData;
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
