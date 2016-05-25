unit Unit1;

interface

uses
  iocp.Http.Client,
  iocp.Utils.Dictionary, iocp,  iocp.Utils.Str,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    IocpHttpClient1: TIocpHttpClient;
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Log(const S: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button10Click(Sender: TObject);
var
  Resp: THttpResult;
begin
  Resp := LocalClient.Get(Edit1.Text, nil, nil);
  if Assigned(Resp.Response) then
    Memo1.Text := Resp.Response.Header + sLineBreak + Resp.Response.ContentString
  else
    Memo1.Text := '';
  ShowMessage(Resp.Request.Headers);
end;

procedure TForm1.Button11Click(Sender: TObject);
var
  O: THttpResponse;
begin
  O := THttpResponse.Create(nil, nil);
  try
    O.Header := Memo1.Text;
    ShowMessage(O.HeaderValue[S_ContentLength]);
  finally
    O.Free;
  end;
end;

procedure TForm1.Button12Click(Sender: TObject);
var
  A, B: TURI;
begin
  FillChar(A, SizeOf(A), 0);
  FillChar(B, SizeOf(B), 0);
  A.URL := 'http://localhost/myApp/cool/bar.do';
  ShowMessage(B.PathRelativeToAbs('foo/stuff.do', A));
  ShowMessage(B.PathRelativeToAbs('/foo/stuff.do', A));
  ShowMessage(B.PathRelativeToAbs('http://niye.name', A));
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  T: Int64;
  I, V: Integer;
  map: Dictionary;
begin
  map := Dictionary.Create();
  try
    Map.Add('dsafssds', 123456);
    Map.Add('dsafssdsaaa', 999);
    Map.Add('test', 58778);
    Map.Add('test1', 58778);
    Map.Add('test2', 58778);
    Map.Add('test3', 58778);
    Map.Add('test4', 58778);
    Map.Add('test5', 58778);
    Map.Add('test6', 58778);
    Map.Add('test7', 58778);
    Map.Add('test8', 58778);
    Map.Add('test9', 58778);
    Map.Add('test10', 58778);
    Map.Add('dasfdewqxd', 58778);
    Map.Add('String', 100);

    Map.Add('StringKey', 'abc');
    Map.Add(12, 'abc');
    Map.Add(55, 99.556);
    Map.Add(525, 456);
    //Map.Add(15.14235646, 546879798);
    Map.Add('String', 100);
    Map['Key'] := 123;
    Map['Key'] := 'Value';
    Map[10] := 123;
    Map[99.999] := 1234578;

    T := GetTickCount;
    for I := 0 to 10000000 do begin
      //V := Map.Values['dasfdewqxd'].AsInteger + 1;
      V := Map.GetInt('dasfdewqxd') + 1;
      //Map.Add('test88', 58778);
      //Map.Remove('test88');
      Map.Values['dasfdewqxd'].AsInteger := V;
    end;
    T := GetTickCount - T;
    ShowMessage(InttoStr(T));
    ShowMessage(Map['dasfdewqxd'].ToString);
  finally
    map.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Text := HttpGet(Edit1.Text);
end;

procedure TForm1.Button4Click(Sender: TObject); begin
//var
//  Obj: TAuthentication;
//begin
//  Obj := TAuthentication.Create;
//  try
//    Obj.Username := 'aaa';
//    Obj.Password := '123';
//    ShowMessage(Obj.Authentication);
//  finally
//    Obj.Free;
//  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
//var
//  Obj: TEntityRanges;
begin
//  Obj := TEntityRanges.Create;
//  try
//    Obj.Add.StartPos := 0;
//    Obj.Add.EndPos := 0;
//  finally
//    Obj.Free;
//  end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  ShowMessage(IntToStr(THttpClient.InstanceSize));
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  Memo1.Text := HttpPost(Edit1.Text, '123456');
end;

procedure TForm1.Button9Click(Sender: TObject);
var
  O: THttpHeaders;
begin
  O := THttpHeaders.Create();
  O.Values['AAA'] := 'test';
  O.Params['AAA', 'BBB'] := 'bbbbbb';
  O.Params['AAA', 'ccc'] := '123';
  O.Params['AAA', 'eee'] := '123da';
  O.Params['AAA', 'fff'] := 'fdasfdea';
  ShowMessage(O.Params['AAA', 'ccc']);
  ShowMessage(o.Text);
  o.Text := Memo1.Text;
  ShowMessage(o.Values['Host']);
  ShowMessage(o.Text);
  FreeAndNil(O);
end;

procedure TForm1.Log(const S: string);
begin
  OutputDebugString(PChar(S));
end;

end.

