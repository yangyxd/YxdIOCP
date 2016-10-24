unit MvcDemo;

interface

{$R+}

uses
  Iocp.Http, Iocp.Http.WebSocket, iocp.Http.MVC, DB,
  SysUtils, Classes;

type
  TUserData = record
    UID: Integer;
    Age: Integer;
    Name: string;
    Nick: string;
  end;

  TPerson = record
    UID: Integer;
    Name: string;
    Status: Integer;
  end;

  TRegState = record
    Status: Integer;
    Msg: string;
  end;

type
  [Service]
  [RequestMapping('/demo')]
  TMvcDemo = class(TObject)
  protected
    [Autowired]
    FServer: TIocpHttpServer;
  public
    class procedure Hello;

    // 直接处理请求，并主动使用 Response 返回数据
    // 处理 URL: /demo/view
    [RequestMapping('/view', http_GET)]
    procedure ViewTest(Request: TIocpHttpRequest; Response: TIocpHttpResponse);

    // 直接处理请求，并主动使用 Response 返回数据
    [RequestMapping('/view/{uid}/{uname}', http_GET)]
    procedure ViewTest1(
      [PathVariable('uid')] UID: Integer;
      [PathVariable('uname')] const UName: string;
      Response: TIocpHttpWriter);

    // 返回一个网页名称
    // 处理 URL: /demo/view2
    [RequestMapping('/view2', http_GET)]
    function ViewTest2(Request: TIocpHttpRequest): string;

    // 返回一个网页名称
    // 处理 URL: /demo/view3/123456
    [RequestMapping('/view3/{uid}', http_GET)]
    function ViewTest3([PathVariable('uid')] UID: Integer): string;

    // 返回一个网页名称, 以下载方式
    // 处理 URL: /demo/view4?uid=123456
    [Download]
    [RequestMapping('/view4', http_GET)]
    function ViewTest4([RequestParam('uid')] UID: Integer): string;

    // 返回一个记录，会自动序列化
    // 处理 URL: /demo/view5
    [RequestMapping('/view5', http_GET)]
    [ResponseBody]
    function ViewTest5(): TUserData;

    // 返回一个对象，会自动序列化。返回的对象会自动释放
    // 处理 URL: /demo/view6
    [RequestMapping('/view6', http_GET)]
    [ResponseBody]
    function ViewTest6(): TStrings;

    // 根据UID查询用户信息。
    // 处理 URL: /demo/person/profile/123456
    [RequestMapping('/person/profile/{id}', http_GET)]
    [ResponseBody]
    function Porfile([PathVariable('id')] UID: Integer): TPerson;

    // 提交用户数据
    // 处理 URL: /demo/person/profile/reguser
    [RequestMapping('/person/profile/reguser')]
    [ResponseBody]
    function RegUser([RequestBody] Data: TUserData): TRegState;

    // WebSocket 请求处理，直接返回字符串内容
    [WebSocket]
    function HelloWebSocket(): string;

    // WebSocket 请求处理, 只有接收到文本信息且内容是 'hello' 时才响应
    [WebSocket('hello')]
    procedure HelloWebSocket2(Response: TIocpWebSocketResponse);

  end;

implementation

{ TMvcDemo }

class procedure TMvcDemo.Hello;
begin
end;

function TMvcDemo.HelloWebSocket: string;
begin
  Result := '123456789';
end;

procedure TMvcDemo.HelloWebSocket2(Response: TIocpWebSocketResponse);
begin
  Response.Send('你好。');
end;

function TMvcDemo.Porfile(UID: Integer): TPerson;
begin
  Result.UID := UID;
  Result.Name := 'Admin';
  Result.Status := 100;
end;

function TMvcDemo.RegUser(Data: TUserData): TRegState;
begin
  Result.Status := 0;
  Result.Msg := '注册成功，用户名是: ' + Data.Name;
end;

procedure TMvcDemo.ViewTest(Request: TIocpHttpRequest;
  Response: TIocpHttpResponse);
begin
  Response.Send(FServer.WebPath);
end;

procedure TMvcDemo.ViewTest1(UID: Integer; const UName: string;
  Response: TIocpHttpWriter);
begin
  Response.Charset := hct_UTF8;
  Response.Write(Format('{"uid":%d, "uname":"%s"}', [UID, UName]));
  Response.Flush;
end;

function TMvcDemo.ViewTest2(Request: TIocpHttpRequest): string;
begin
  Result := 'http_mvc_setting.xml';
end;

function TMvcDemo.ViewTest3(UID: Integer): string;
begin
  Result := 'httpPostTest.html';
end;

function TMvcDemo.ViewTest4(UID: Integer): string;
begin
  Result := 'http_mvc_setting.xml';
end;

function TMvcDemo.ViewTest5: TUserData;
begin
  Result.UID := 666;
  Result.Age := 30;
  Result.Name := 'yangyxd';
  Result.Nick := '喵喵';
end;

function TMvcDemo.ViewTest6: TStrings;
begin
  Result := TStringList.Create;
  Result.Add('aaa');
  Result.Add('bbb');
  Result.Add('cccc');
end;

initialization
  // 因为 TMvcDemo 在其它任何地方都没有用到，所以要
  // 防止编译优化，写这一段无用代码
  // 也可以直接调用 RegMvcClass 进行注册
  TMvcDemo.Hello;

end.
