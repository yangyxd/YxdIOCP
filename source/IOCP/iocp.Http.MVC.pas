{*******************************************************}
{                                                       }
{       Http 服务 MVC 核心单元                          }
{                                                       }
{       版权所有 (C) 2016 yangyxd                       }
{                                                       }
{*******************************************************}

unit iocp.Http.MVC;

interface

{$I 'iocp.inc'}

{$IF (RTLVersion>=26)}
{$DEFINE UseMvc}
{$ENDIF}

{$IFNDEF UseMvc}
{$MESSAGE WARN 'iocp.Http.MVC 单元检测到当前IDE版本过低，可能无法正常使用。'}
{$ENDIF}

uses
  iocp.Http, iocp.Http.WebSocket, iocp.Sockets,
  iocp.Utils.Str,
  
  XML,
  
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  Generics.Collections, Rtti, System.Generics.Defaults,
  SysUtils, Classes, Variants, TypInfo;

type
  TCustomAttributeClass = class of TCustomAttribute;
  TCompareAttributeItem = reference to function (const Item: TCustomAttribute; 
    const Data: Pointer): Boolean;  

  /// <summary>
  /// TObject Helper 增加 RTTI 处理功能
  /// </summary>
  TObjectHelper = class helper for TObject
  private
    function GetRealAttrName(const AttrName: string): string;
  public
    class procedure RegToMVC();
    function CheckAttribute(ARttiType: TRttiType; ACompare: TCompareAttributeItem;
      const Data: Pointer = nil): Boolean; overload;
    function CheckAttribute(const Attributes: TArray<TCustomAttribute>;
      ACompare: TCompareAttributeItem;
      const Data: Pointer = nil): Boolean; overload;
    /// <summary>
    /// 检测当前对象的各项属性。
    /// </summary>
    function CheckAttribute(ACompare: TCompareAttributeItem; const Data: Pointer = nil): Boolean; overload;
    /// <summary>
    /// 检测是否存在指定类型的属性
    /// </summary>
    function ExistAttribute(const AttrType: TCustomAttributeClass): Boolean; overload;
    function ExistAttribute(const Attributes: TArray<TCustomAttribute>;
      const AttrType: TCustomAttributeClass): Boolean; overload;
    /// <summary>
    /// 检测是否存在指定名称的属性 (不要包含属性类名的"Attribute"部分)
    /// </summary>
    function ExistAttribute(const AttrName: string): Boolean; overload;
    function ExistAttribute(const Attributes: TArray<TCustomAttribute>;
      const AttrName: string): Boolean; overload;
    /// <summary>
    /// 获取指定属性中指定字段名的值
    /// </summary>
    function GetAttribute<T>(const AttrName, FiledName: string): T; overload;
    function GetAttribute<T>(const AttrName, FiledName: string; const DefaultValue: T): T; overload; 

    function GetRttiValue<T>(const Name: string): T; 
    procedure SetRttiValue<T>(const Name: string; const Value: T);

    function CreateObject(const ARttiType: TRttiType; const Args: array of TValue): TObject; overload;
    function CreateObject(const AClassName: string; const Args: array of TValue): TObject; overload;

    procedure Log(const Msg: string);
  end;

type
  /// <summary>
  /// MVC 扫描器 - 将标注了指定标识的类加入Map中
  /// </summary>
  TIocpMvcScanner = class(TObject)
  private
    FScannerOK: Boolean;
    FClassMap: TDictionary<string, TObject>;
    FRttiContext: TRttiContext;
  protected
    procedure DoValueNotify(Sender: TObject; const Item: TObject; 
      Action: System.Generics.Collections.TCollectionNotification);
    function GetAllTypes: TArray<TRttiType>;
  public
    constructor Create;
    destructor Destroy; override;
    /// <summary>
    /// 自动扫描系统中的类，初始化标注了 [Service, Controller] 的类
    /// </summary>
    procedure StartScan;
    procedure Clear;

    procedure RegClass(AClass: TClass); overload;
    procedure RegClass(AClass: TRttiType); overload;
  end;

type
  /// <summary>
  /// URI 映射信息
  /// </summary>
  TUriMapData = record
  private
    function GetParamName: string;
    function GetParamValue: string;
    function GetHeaderName: string;
    function GetHeaderValue: string;
  public
    URI: string;
    DownMode: Boolean;
    ResponseBody: Boolean;
    Method: TIocpHttpMethod;
    Consumes, Produces, Params, Headers: string;
    // 控制器实例对象
    Controller: TObject;
    // 方法索引号
    MethodIndex: Integer;
    procedure Clear;
    property ParamName: string read GetParamName;
    property ParamValue: string read GetParamValue;
    property HeaderName: string read GetHeaderName;
    property HeaderValue: string read GetHeaderValue;
  end;

type
  /// <summary>
  /// Web Socket 映射信息
  /// </summary>
  TWebSocketMapData = record
    // 必须是文本消息，且与Data相符时才触发命令. 是否大小写敏感由 MvcServer 控制。
    Data: string;
    ResponseBody: Boolean;
    // 控制器实例对象
    Controller: TObject;
    // 方法索引号
    MethodIndex: Integer;  
    class function Create(AController: TObject; AMethodIndex: Integer): TWebSocketMapData; static; 
  end;

type
  /// <summary>
  /// 序列化请求 - 用于在事件中序列化对象
  /// </summary>
  TOnSerializeRequest = function (Sender: TObject; const Value: TValue): string of object;
  /// <summary>
  /// 反序列化请求 - 用于在事件中反序列化对象
  /// </summary>
  TOnDeSerializeRequest = function (Sender: TObject; const Value: string;
    const Dest: TValue; IsGet: Boolean): Boolean of object;

type
  /// <summary>
  /// 支持 MVC 功能的 Http 服务
  /// </summary>
  TIocpHttpMvcServer = class(TComponent)
  private
    FServer: TIocpHttpServer;
    FContext: TRttiContext;
    FPrefix, FSuffix: string;
    FUseWebSocket: Boolean;
    FNeedSaveConfig: Boolean;
    FUriCaseSensitive: Boolean;
    FUriMap: TDictionary<string, TUriMapData>;
    FWebSocketMap: TList<TWebSocketMapData>;

    FOnSerializeData: TOnSerializeRequest;
    FOnDeSerializeData: TOnDeSerializeRequest;
    
    procedure SetUseWebSocket(const Value: Boolean);
    function GetAccessControlAllow: TIocpHttpAccessControlAllow;
    function GetAutoDecodePostParams: Boolean;
    function GetCharset: StringA;
    function GetContentLanguage: StringA;
    function GetGzipFileTypes: string;
    function GetListenPort: Integer;
    function GetUploadMaxDataSize: NativeUInt;
    function GetWebBasePath: string;
    procedure SetAccessControlAllow(const Value: TIocpHttpAccessControlAllow);
    procedure SetActive(const Value: Boolean);
    procedure SetAutoDecodePostParams(const Value: Boolean);
    procedure SetCharset(const Value: StringA);
    procedure SetContentLanguage(const Value: StringA);
    procedure SetGzipFileTypes(const Value: string);
    procedure SetListenPort(const Value: Integer);
    procedure SetUploadMaxDataSize(const Value: NativeUInt);
    procedure SetWebBasePath(const Value: string);
    function GetActive: Boolean;
    function GetBindAddr: StringA;
    procedure SetBindAddr(const Value: StringA);
    function GetServer: TIocpWebSocketServer;
  protected
    procedure InitServer();
    function DefaultConfigName: string;
  protected
    procedure DoHttpRequest(Sender: TIocpHttpServer;
      Request: TIocpHttpRequest; Response: TIocpHttpResponse);
    procedure DoHttpWebSocketRequest(Sender: TIocpWebSocketServer;
      Request: TIocpWebSocketRequest; Response: TIocpWebSocketResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    
    /// <summary>
    /// 序列化对象
    /// </summary>
    function SerializeData(const Value: TValue): string;

    /// <summary>
    /// 反序列化对象
    /// </summary>
    function DeSerializeData(const SrcData: string; const DestValue: TValue; IsGet: Boolean): Boolean;
    
    /// <summary>
    /// 加载配置
    /// </summary>
    procedure LoadConfig(const AFileName: string = '');
    /// <summary>
    /// 保存配置
    /// </summary>
    procedure SaveConfig(const AFileName: string = '');
    /// <summary>
    /// 是否启用服务
    /// </summary>
    property Active: Boolean read GetActive write SetActive;
    /// <summary>
    /// 网络服务器
    /// </summary>
    property Server: TIocpHttpServer read FServer;
    /// <summary>
    /// WebSocket网络服务器
    /// </summary>
    property WebSocketServer: TIocpWebSocketServer read GetServer;
  published

    /// <summary>
    /// 返回视图的前缀名称，默认为空
    /// </summary>
    property Prefix: string read FPrefix write FPrefix;
    /// <summary>
    /// 返回视图的后缀名称，默认为空
    /// </summary>
    property Suffix: string read FSuffix write FSuffix;

    /// <summary>
    /// URI 是否大小写敏感
    /// </summary>
    property UriCaseSensitive: Boolean read FUriCaseSensitive write FUriCaseSensitive;

    /// <summary>
    /// 是否启用 WebSocket 功能
    /// </summary>
    property UseWebSocket: Boolean read FUseWebSocket write SetUseWebSocket default False;
    
    
    property ListenPort: Integer read GetListenPort write SetListenPort default 8080;

    /// <summary>
    /// 跨域控制选项, 默认不启用
    /// </summary>
    property AccessControlAllow: TIocpHttpAccessControlAllow read GetAccessControlAllow
      write SetAccessControlAllow;

    /// <summary>
    /// 默认字符集选项，会在响应客户端请求时加入 Content-Type 中。
    /// </summary>
    property Charset: StringA read GetCharset write SetCharset;

    /// <summary>
    /// 服务器端口绑定地址
    /// </summary>
    property BindAddr: StringA read GetBindAddr write SetBindAddr;

    /// <summary>
    /// 默认响应内容语言。会在响应客户端请求时加入 Content-Language 中。
    /// </summary>
    property ContentLanguage: StringA read GetContentLanguage write SetContentLanguage;

    /// <summary>
    /// WEB文件夹的根目录。默认为程序所在目录下Web文件夹
    /// </summary>
    property WebPath: string read GetWebBasePath write SetWebBasePath;

    /// <summary>
    /// 下载文件时，自动使用GZip进行压缩的文件类型 (以";"进行分隔)
    /// 如：'.htm;.html;.css;.js;'
    /// </summary>
    property GzipFileTypes: string read GetGzipFileTypes write SetGzipFileTypes;

    /// <summary>
    /// 是否自动解析POST参数
    /// </summary>
    property AutoDecodePostParams: Boolean read GetAutoDecodePostParams write SetAutoDecodePostParams;

    /// 客户端上传数据大小的上限（默认为2M）
    property UploadMaxDataSize: NativeUInt read GetUploadMaxDataSize write SetUploadMaxDataSize;

    /// <summary>
    /// 序列化请求事件
    /// </summary>
    property OnSerializeData: TOnSerializeRequest read FOnSerializeData write FOnSerializeData;
    /// <summary>
    /// 反序列化请求事件
    /// </summary>
    property OnDeSerializeData: TOnDeSerializeRequest read FOnDeSerializeData write FOnDeSerializeData;
  end;

type
  /// <summary>
  /// 业务层标注，增加此标注后，指定类才具备处理业务请求的能力
  /// </summary>
  ServiceAttribute = class(TCustomAttribute);

  /// <summary>
  /// 控制层标注。增加此标注后，指定类才具备处理业务请求的能力。
  /// </summary>
  ControllerAttribute = class(TCustomAttribute);

  /// <summary>
  /// 数据访问组件标注。增加此标注后，指定类可以访问数据库。
  /// </summary>
  RepositoryAttribute = class(TCustomAttribute); 

   
type
  /// <summary>
  /// 需要自动装配的标注
  /// </summary>
  AutowiredAttribute = class(TCustomAttribute);

type
  /// <summary>
  /// 标识请求返回页面采用下载方式的标注
  /// </summary>
  DownloadAttribute = class(TCustomAttribute);

type
  /// <summary>
  /// WebSocket 请求处理标注
  /// </summary>
  WebSocketAttribute = class(TCustomAttribute)
  private
    FData: string;
  public
    /// <summary>
    /// <param name="Data">指定仅当Data为指定内容的文本消息时才响应</param>
    /// </summary>
    constructor Create(const Data: string = '');
    
    property Data: string read FData;
  end;

type
  /// <summary>
  /// 请求地址映射标注
  /// </summary>
  RequestMappingAttribute = class(TCustomAttribute)
  private
    FValue: string;
    FMethod: TIocpHttpMethod;
    FConsumes, FProduces, FParams, FHeaders: string;
  public
    /// <summary>
    /// <param name="Value">指定请求的实际地址</param>
    /// </summary>
    constructor Create(const Value: string); overload;
    /// <summary>
    /// <param name="Method">指定请求的method类型， GET、POST、PUT、DELETE等</param>
    /// </summary>
    constructor Create(Method: TIocpHttpMethod); overload;
    /// <summary>
    /// <param name="Value">指定请求的实际地址</param>
    /// <param name="Method">指定请求的method类型， GET、POST、PUT、DELETE等</param>
    /// <param name="Consumes">指定处理请求的提交内容类型（Content-Type），例如application/json, text/html</param>
    /// <param name="Produces">指定返回的内容类型，仅当request请求头中的(Accept)类型中包含该指定类型才返回</param>
    /// <param name="Params">指定request中必须包含某些参数值是，才让该方法处理。</param>
    /// <param name="Headers">指定request中必须包含某些指定的header值，才能让该方法处理请求。</param>
    /// </summary>
    constructor Create(const Value: string; Method: TIocpHttpMethod; const Consumes, Produces, Params: string; const Headers: string = ''); overload;
    /// <summary>
    /// <param name="Value">指定请求的实际地址</param>
    /// <param name="Method">指定请求的method类型， GET、POST、PUT、DELETE等</param>
    /// <param name="Params">指定request中必须包含某些参数值是，才让该方法处理。</param>
    /// </summary>
    constructor Create(const Value: string; Method: TIocpHttpMethod; const Params: string = ''); overload;
    /// <summary>
    /// <param name="Value">指定请求的实际地址</param>
    /// <param name="Params">指定request中必须包含某些参数值是，才让该方法处理。</param>
    /// <param name="Headers">指定request中必须包含某些指定的header值，才能让该方法处理请求。</param>
    /// </summary>
    constructor Create(const Value: string; const Params: string; const Headers: string = ''); overload;

    property Value: string read FValue;
    property Method: TIocpHttpMethod read FMethod;
    property Consumes: string read FConsumes;
    property Produces: string read FProduces;
    property Params: string read FParams;
    property Headers: string read FHeaders;
  end;

type
  /// <summary>
  /// 用来获得请求url中的动态参数。
  /// </summary>
  PathVariableAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    /// <summary>
    /// <param name="Value">当方法参数名称和需要绑定的uri template中变量名称不一致时，
    /// 用于指定uri template的名称</param>
    /// </summary>
    constructor Create(const Name: string); overload;

    property Name: string read FName;
  end;

type
  /// <summary>
  /// 用于将请求参数区数据映射到功能处理方法的参数上
  /// </summary>
  RequestParamAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(const Name: string); overload;
    // 参数名称
    property Name: string read FName;
  end;

type
  /// <summary>
  /// 1. 读取Request请求的body部分数据，使用系统默认配置的Converter进行解析，
  ///    然后把相应的数据绑定到要返回的对象上；
  /// 2. 再把Converter返回的对象数据绑定到controller中方法的参数上。
  /// </summary>
  RequestBodyAttribute = class(TCustomAttribute);

type
  /// <summary>
  /// 将Controller的方法返回的对象，通过适当的Converter的Adapter转换对象,
  /// 将内容转换为指定格式后，写入到Response对象的body数据区。
  /// </summary>
  ResponseBodyAttribute = class(TCustomAttribute);

var
  HttpMvc: TIocpHttpMvcServer = nil;

/// <summary>
/// 初始化 MVC 服务器 - 如果已经在窗口上放置了 Mvc 组件，则不需要调用本方法
/// 使用本方法会自动加载配置文件：http_mvc_setting.xml
/// 此配置文件应放在 AppPath 目录中。
/// </summary>
procedure InitHttpMvcServer;

/// <summary>
/// 注册
/// </summary>
procedure RegMvcClass(AClass: TClass); overload;
procedure RegMvcClass(AClass: TRttiType); overload;


implementation

resourcestring
  S_BadRequestBody = 'Request Body Parse Failed.';

var
  MvcScanner: TIocpMvcScanner = nil;
  HttpMvcAllowFree: Boolean = False;

procedure InitHttpMvcServer;
begin
  if not Assigned(HttpMvc) then
    MvcScanner.StartScan;
end;

procedure RegMvcClass(AClass: TClass);
begin
  if Assigned(MvcScanner) then
    MvcScanner.RegClass(AClass);
end;

procedure RegMvcClass(AClass: TRttiType);
begin
  if Assigned(MvcScanner) then
    MvcScanner.RegClass(AClass);
end;

// 获取 Name=Value 字符串中的 Name 或 Value
function GetNameOrValue(const Value: string; GetName: Boolean = True): string;
var
  I: Integer;
begin
  if Value = '' then
    Result := ''
  else begin
    I := Pos('=', Value);
    if I > 0 then begin
      if GetName then
        Result := Value.Substring(0, I - 1)
      else
        Result := Value.Substring(I);
    end else
      Result := '';
  end;
end;

{ TObjectHelper }

function TObjectHelper.ExistAttribute(
  const AttrType: TCustomAttributeClass): Boolean;
begin
  Result := CheckAttribute(
    function(const Item: TCustomAttribute; const Data: Pointer): Boolean 
    begin
      Result := Item.ClassType = AttrType;
    end, nil);
end;

function TObjectHelper.ExistAttribute(
  const Attributes: TArray<TCustomAttribute>;
  const AttrType: TCustomAttributeClass): Boolean;
begin
  Result := CheckAttribute(Attributes,
    function(const Item: TCustomAttribute; const Data: Pointer): Boolean
    begin
      Result := Item.ClassType = AttrType;
    end, nil);
end;

function TObjectHelper.ExistAttribute(
  const Attributes: TArray<TCustomAttribute>; const AttrName: string): Boolean;
var
  ARealAttrName: string;
begin
  ARealAttrName := GetRealAttrName(AttrName);
  Result := CheckAttribute(Attributes,
    function(const Item: TCustomAttribute; const Data: Pointer): Boolean
    begin
      Result := Item.ClassNameIs(ARealAttrName);
    end, nil);
end;

function TObjectHelper.CheckAttribute(ACompare: TCompareAttributeItem; 
  const Data: Pointer): Boolean;
var
  AContext: TRttiContext;
  ARttiType: TRttiType;
  AFieldAttrItem: TCustomAttribute;
begin
  Result := False;
  AContext := TRttiContext.Create;
  ARttiType := AContext.GetType(Self.ClassType);
  if ARttiType.GetAttributes <> nil then begin
    for AFieldAttrItem in ARttiType.GetAttributes do
      if ACompare(AFieldAttrItem, Data) then begin
        Result := True;
        Break;
      end;
  end;
end;

function TObjectHelper.CheckAttribute(
  const Attributes: TArray<TCustomAttribute>; ACompare: TCompareAttributeItem;
  const Data: Pointer): Boolean;
var
  AFieldAttrItem: TCustomAttribute;
begin
  Result := False;
  for AFieldAttrItem in Attributes do
    if ACompare(AFieldAttrItem, Data) then begin
      Result := True;
      Break;
    end;
end;

function TObjectHelper.CreateObject(const AClassName: string;
  const Args: array of TValue): TObject;
var
  Context: TRttiContext;
  RttiType: TRttiType;
begin
  RttiType := Context.FindType(AClassName);
  if Assigned(RttiType) then   
    Result := CreateObject(RttiType, Args)
  else
    Exit(nil);
end;

function TObjectHelper.CreateObject(const ARttiType: TRttiType;
  const Args: array of TValue): TObject;
var
  RttiMethod: TRttiMethod;
  AClass: TClass;
begin
  for RttiMethod in ARttiType.GetMethods do begin
    if RttiMethod.IsConstructor and (Length(RttiMethod.GetParameters) = Length(Args)) then begin
      AClass := ARttiType.AsInstance.MetaclassType;
      Exit(RttiMethod.Invoke(AClass, Args).AsObject);
    end;
  end;
  Exit(nil);
end;

function TObjectHelper.CheckAttribute(ARttiType: TRttiType;
  ACompare: TCompareAttributeItem; const Data: Pointer): Boolean;
var
  AFieldAttrItem: TCustomAttribute;
begin
  Result := False;
  if Assigned(ARttiType) and (ARttiType.GetAttributes <> nil) then begin
    for AFieldAttrItem in ARttiType.GetAttributes do
      if ACompare(AFieldAttrItem, Data) then begin
        Result := True;
        Break;
      end;
  end;    
end;

function TObjectHelper.ExistAttribute(const AttrName: string): Boolean;
var
  ARealAttrName: string;
begin
  ARealAttrName := GetRealAttrName(AttrName);
  Result := CheckAttribute(
    function(const Item: TCustomAttribute; const Data: Pointer): Boolean 
    begin
      Result := Item.ClassNameIs(ARealAttrName);
    end, nil);
end;

function TObjectHelper.GetAttribute<T>(const AttrName, FiledName: string): T;
begin
  Result := GetAttribute<T>(AttrName, FiledName, T(nil));  
end;

function TObjectHelper.GetAttribute<T>(const AttrName, FiledName: string;
  const DefaultValue: T): T;
var
  AContext: TRttiContext;
  ARttiType: TRttiType;
  AFieldAttrItem: TCustomAttribute;
  AFiled: TRttiField;
  ARealAttrName: string;
begin
  AContext := TRttiContext.Create;
  ARttiType := AContext.GetType(Self.ClassType);
  if (ARttiType.GetAttributes <> nil) and (FiledName <> '') then begin
    ARealAttrName := GetRealAttrName(AttrName);
    for AFieldAttrItem in ARttiType.GetAttributes do
      if AFieldAttrItem.ClassNameIs(ARealAttrName) then begin
        if FiledName <> '' then begin        
          ARttiType := AContext.GetType(AFieldAttrItem.ClassType);
          if (LowerCase(FiledName[1]) <> 'f') then
            AFiled := ARttiType.GetField('f' + FiledName)
          else
            AFiled := nil;
          if not Assigned(AFiled) then
            AFiled := ARttiType.GetField(FiledName);
          if Assigned(AFiled) then begin   
            Result := AFiled.GetValue(AFieldAttrItem).AsType<T>();
            Exit;
          end else 
            Break;
        end else
          Break;
      end;
  end;    
  Result := DefaultValue;
end;  

function TObjectHelper.GetRealAttrName(const AttrName: string): string;
const
  S_Attribute = 'attribute';
  S_AttributeLen = Length(S_Attribute) - 1;
  SP_Attribute: PChar = S_Attribute;
  SP_AttributeLen = Length(S_Attribute);
var
  Len: Integer;
  P: PChar;
begin
  Len := Length(AttrName);
  if (Len > S_AttributeLen) then begin
    P := PChar(AttrName) + Len - SP_AttributeLen;
    if (StrLIComp(P, SP_Attribute, SP_AttributeLen) = 0) then begin
      Result := AttrName;
      Exit;
    end;
  end;
  Result := AttrName + S_Attribute;
end;

function TObjectHelper.GetRttiValue<T>(const Name: string): T;
var
  FType: TRttiType;
  FFiled: TRttiField;
  FContext: TRttiContext;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Self.ClassType);
    FFiled := FType.GetField(Name);
    if not Assigned(FFiled) then
      Result := T(nil)
    else
      Result := FFiled.GetValue(Self).AsType<T>();
  finally
    FContext.Free;
  end; 
end;

procedure TObjectHelper.Log(const Msg: string);
begin
  {$IFDEF MSWINDOWS}
  OutputDebugString(PChar(Msg));
  {$ENDIF}
end;

class procedure TObjectHelper.RegToMVC;
begin
end;

procedure TObjectHelper.SetRttiValue<T>(const Name: string; const Value: T);
var
  FType: TRttiType;
  FFiled: TRttiField;
  FContext: TRttiContext;
begin
  FContext := TRttiContext.Create;
  try
    FType := FContext.GetType(Self.ClassType);
    FFiled := FType.GetField(Name);
    if not Assigned(FFiled) then Exit;
    if FFiled.FieldType.TypeKind <> PTypeInfo(TypeInfo(T)).Kind then
      Exit;
    FFiled.SetValue(Self, TValue.From(Value));
  finally
    FContext.Free;
  end;
end;

{ TIocpMvcScanner }

procedure TIocpMvcScanner.Clear;
begin
  FClassMap.Clear;
end;

constructor TIocpMvcScanner.Create;
begin
  FScannerOK := False;
  FClassMap := TDictionary<string, TObject>.Create(63);
  FClassMap.OnValueNotify := DoValueNotify;
  FRttiContext := TRttiContext.Create;
end;

destructor TIocpMvcScanner.Destroy;
begin
  Clear;
  FreeAndNil(FClassMap);
  inherited;
end;

procedure TIocpMvcScanner.DoValueNotify(Sender: TObject; const Item: TObject; 
  Action: System.Generics.Collections.TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

function TIocpMvcScanner.GetAllTypes: TArray<TRttiType>;
begin
  Result := FRttiContext.GetTypes;
end;

procedure TIocpMvcScanner.RegClass(AClass: TRttiType);
var
  Obj: TObject;
begin
  if not FClassMap.ContainsKey(AClass.Name) then begin
    Obj := CreateObject(AClass, []);
    FClassMap.Add(AClass.Name, Obj);
    Log('Initial success: ' + AClass.QualifiedName);
  end;
end;

procedure TIocpMvcScanner.RegClass(AClass: TClass);
var
  AType: TRttiType;
begin
  AType := FRttiContext.GetType(AClass);
  RegClass(AType);
end;

procedure TIocpMvcScanner.StartScan;
var
  tl: TArray<TRttiType>;
  Item: TRttiType;
  J: Integer;
begin
  if FScannerOK then
    Exit;
  FClassMap.Clear;
  tl := GetAllTypes;
  for J := 0 to High(tl) do begin  
    Item := tl[J];    
    if CheckAttribute(Item,
      function(const Item: TCustomAttribute; const Data: Pointer): Boolean
      begin
        Result := (Item.ClassType = ServiceAttribute) or
           (Item.ClassType = ControllerAttribute);
      end, nil)
    then begin 
      Log('Scanner: ' + Item.QualifiedName);
      // 初始化
      RegClass(Item);
    end;      
  end;
  
  // 初始化服务
  if (HttpMvc = nil) then begin
    HttpMvcAllowFree := True;
    HttpMvc := TIocpHttpMvcServer.Create(nil);
    HttpMvc.LoadConfig;
  end;

  FScannerOK := True;
end;

{ WebSocketAttribute }

constructor WebSocketAttribute.Create(const Data: string);
begin
  FData := Data;
end;

{ RequestMappingAttribute }

constructor RequestMappingAttribute.Create(const Value: string);
begin
  FValue := Value;
end;

constructor RequestMappingAttribute.Create(Method: TIocpHttpMethod);
begin
  FMethod := Method;
end;

constructor RequestMappingAttribute.Create(const Value: string;
  Method: TIocpHttpMethod; const Consumes, Produces, Params, Headers: string);
begin
  FValue := Value;
  FMethod := Method;
  FConsumes := Consumes;
  FProduces := Produces;
  FParams := Params;
  FHeaders := Headers;
end;

constructor RequestMappingAttribute.Create(const Value: string;
  Method: TIocpHttpMethod; const Params: string);
begin
  FValue := Value;
  FMethod := Method;
  FParams := Params;
end;

constructor RequestMappingAttribute.Create(const Value, Params,
  Headers: string);
begin
  FValue := Value;
  FParams := Params;
  FHeaders := Headers;
end;

{ PathVariableAttribute }

constructor PathVariableAttribute.Create(const Name: string);
begin
  FName := Name;
end;

{ RequestParamAttribute }

constructor RequestParamAttribute.Create(const Name: string);
begin
  FName := Name;
end;

{ TIocpHttpMvcServer }

constructor TIocpHttpMvcServer.Create(AOwner: TComponent);
begin
  if Assigned(HttpMvc) then begin
    FreeAndNil(HttpMvc);
    HttpMvcAllowFree := False;
  end;
  HttpMvc := Self;
  FUseWebSocket := False;
  FNeedSaveConfig := False;
  FContext := TRttiContext.Create;
  FUriMap := TDictionary<string, TUriMapData>.Create(9973);
  FWebSocketMap := TList<TWebSocketMapData>.Create;

  if Assigned(MvcScanner) then
    MvcScanner.StartScan;

  InitServer();
  inherited;   
end;

function TIocpHttpMvcServer.DefaultConfigName: string;
begin
  Result := AppPath + 'http_mvc_setting.xml';
end;

function TIocpHttpMvcServer.DeSerializeData(const SrcData: string;
  const DestValue: TValue; IsGet: Boolean): Boolean;
begin
  if Assigned(FOnDeSerializeData) then begin
    Result := FOnDeSerializeData(Self, SrcData, DestValue, IsGet);
  end else
    Result := False;
end;

destructor TIocpHttpMvcServer.Destroy;
begin
  FreeAndNil(FServer);
  FreeAndNil(FUriMap);
  FreeAndNil(FWebSocketMap);
  FContext.Free;
  if HttpMvc = Self then
    HttpMvc := nil;
  inherited;
end;

function GetArgsValue(AParam: TRttiParameter; const Value: string): TValue;
begin
  Result := TValue.Empty;
  if Value = '' then Exit;  
  case AParam.ParamType.TypeKind of
    tkInteger, tkInt64:
      Result := StrToInt(Value);
    tkFloat:
      Result := StrToFloat(Value);
    tkChar, tkWChar:
      begin
        if Value <> '' then
          Result := Value[1];
      end;
    tkString, tkLString, tkWString, tkUString:
      Result := Value;
    tkEnumeration:
      begin
        if StrToIntDef(Value, 0) <> 0 then
          Result := StrToInt(Value)
        else
          Result := GetEnumValue(AParam.ParamType.Handle, Value);
      end;
  end;
end;

procedure TIocpHttpMvcServer.DoHttpRequest(Sender: TIocpHttpServer;
  Request: TIocpHttpRequest; Response: TIocpHttpResponse);

  // 检测 Path 变量
  procedure CheckPathVariable(const URI: string; var Item: TUriMapData;
    var APathVariable: TDictionary<string, string>);
  var
    P, P1: PChar;
    Key: string;
  begin
    P := PChar(URI);
    P1 := P + Length(URI);
    while P1 > P do begin
      if P1^ = '/' then begin
        Key := PCharToString(P, P1 - P + 1);
        if FUriMap.ContainsKey(Key) then begin
          Item := FUriMap[Key];
          APathVariable := TDictionary<string, string>.Create(7);
          Break;
        end;
      end;
      Dec(P1);
    end;
  end;

  // 初始化 Path 变量
  procedure InitPathVariable(const URI: string; const Item: TUriMapData;
    APathVariable: TDictionary<string, string>);
  var
    P1, P2, P3, P4, P5: PChar;
    Key: string;
  begin
    P1 := PChar(URI);
    P3 := PChar(Item.Uri);
    P4 := P3 + Length(Item.URI);
    P2 := P1 + Length(URI);
    while P3 < P4 do begin
      if P3^ = '{' then begin
        Inc(P3);
        P5 := P3;
        while (P3 < P4) and (P3^ <> '}') do
          Inc(P3);
        Key := PCharToString(P5, P3 - P5);
        P5 := P1;
        while (P1 < P2) and (P1^ <> '/') do
          Inc(P1);
        APathVariable.Add(Key, PCharToString(P5, P1 - P5));
      end;
      Inc(P1);
      Inc(P3);
    end;
  end;

const
  CS_Mltipart_FormData: StringA = 'multipart/form-data';
var
  Key, URI: string;
  Item: TUriMapData;
  AClass: TClass;
  AMethod: TRttiMethod;
  ARttiType: TRttiType;
  AParams: TArray<TRttiParameter>;
  APathVariable: TDictionary<string, string>;
  AParamObjs: TObject;
  Args: array of TValue;
  ResultValue: TValue;
  IsOK: Boolean;
  I: Integer;
begin
  try
    if FUriCaseSensitive then
      URI := string(Request.URI)
    else
      URI := LowerCase(string(Request.URI));
    FillChar(Item, SizeOf(Item), 0);
    APathVariable := nil;
    AParamObjs := nil;
    try
      // 检测 URI 映射
      if not FUriMap.ContainsKey(URI) then begin
        // 不存在时，从最后一节开始，向上截断，判断是否存在 Path 变量的 URI
        CheckPathVariable(URI, Item, APathVariable);
      end else
        Item := FUriMap[URI];

      // 没有找到页面
      if (not Assigned(Item.Controller)) then begin
        Response.SendFileByURI(URI, '', False, True);
        //Response.ErrorRequest(404);
        Exit;
      end;

      // 不允许访问
      if // 方法不匹配
        ((Item.Method <> http_Unknown) and (Item.Method <> Request.Method)) or
        // 不包含指定的参数
        ((Item.Params <> '') and (Request.GetParam(StringA(Item.ParamName)) <> Item.ParamValue)) or
        // 不存在指定处理请求的提交内容类型
        ((Item.Consumes <> '') and (not Request.ExistContentType(StringA(Item.Consumes)))) or
        // 不允许指定返回的内容类型
        ((Item.Produces <> '') and (not Request.AllowAccept(StringA(Item.Produces)))) or
        // 未包含有指定的header值
        ((Item.Headers <> '') and (Request.GetHeader(StringA(Item.HeaderName)) <> StringA(Item.HeaderValue)))
      then begin
        Response.ErrorRequest(405);
        Exit;
      end;

      // 初始化 PathVariable 列表
      if Assigned(APathVariable) then
        InitPathVariable(URI, Item, APathVariable);

      // 参数注入
      ARttiType := FContext.GetType(Item.Controller.ClassType);
      AMethod := ARttiType.GetMethods[Item.MethodIndex];
      AParams := AMethod.GetParameters;
      SetLength(Args, Length(AParams));
      for I := 0 to High(AParams) do begin
        // 检测属性绑定
        if CheckAttribute(AParams[I].GetAttributes,
          function(const Item: TCustomAttribute; const Data: Pointer): Boolean
          begin
            if Assigned(APathVariable) and (Item.ClassType = PathVariableAttribute) then begin
              // PathVariable 标注的字段
              Result := True;
              Key := PathVariableAttribute(Item).Name;
              if not APathVariable.ContainsKey(Key) then
                Exit;
              Args[I] := GetArgsValue(AParams[I], APathVariable[Key]);
            end else if Item.ClassType = RequestParamAttribute then begin
              // RequestParam 标注的字段
              Result := True;
              Args[I] := GetArgsValue(AParams[I], Request.GetParam(StringA(PathVariableAttribute(Item).FName)));
            end else if (Item.ClassType = RequestBodyAttribute) and (AParamObjs = nil) then begin
              // RequestBody 绑定请求参数到对象中
              Result := True;
              case AParams[I].ParamType.TypeKind of
                tkString, tkLString, tkWString, tkUString:
                  begin
                    // 字符串，直接赋值
                    if Request.Method <> http_GET then
                      Args[I] := Request.GetDataString()
                    else
                      Args[I] := string(Request.ParamData);
                  end;
                tkClass:
                  begin
                    // 类，实例化后通过序列化事件赋值
                    Args[I] := nil;
                    AClass := AParams[I].ParamType.AsInstance.MetaclassType;
                    try
                      if AClass.InheritsFrom(TComponent) then
                        Args[I] := CreateObject(AClass.ClassName, [Owner])
                      else
                        Args[I] := CreateObject(AClass.ClassName, []);
                      // 如果不是二进制数据
                      if not Request.ExistContentType(CS_Mltipart_FormData) then begin
                        if Request.Method <> http_GET then
                          IsOK := DeSerializeData(Request.GetDataString(), Args[I], False)
                        else
                          IsOK := DeSerializeData(string(Request.ParamData), Args[I], True);
                        if not IsOK then
                          raise Exception.Create(S_BadRequestBody);
                      end;
                    finally
                      if (not Args[I].IsEmpty) and (Args[I].IsObject) then
                        AParamObjs := Args[I].AsObject; // 将对象类加入到列表，待使用完后释放
                    end;
                  end;
                tkRecord:
                  begin
                    // 记录，实例化后通过序列化事件赋值
                    TValue.Make(nil, AParams[I].ParamType.Handle, Args[I]);
                    // 如果不是二进制数据
                    if not Request.ExistContentType(CS_Mltipart_FormData) then begin
                      if Request.Method <> http_GET then
                        IsOK := DeSerializeData(Request.GetDataString(), Args[I], False)
                      else
                        IsOK := DeSerializeData(string(Request.ParamData), Args[I], True);
                      if not IsOK then
                        raise Exception.Create(S_BadRequestBody);
                    end;
                  end;
              end;
            end else
              Result := False;
          end
        ) then
          Continue;

        // 检测字段绑定，自动注入参数
        case AParams[I].ParamType.TypeKind of
          tkClass:
            begin
              AClass := AParams[I].ParamType.AsInstance.MetaclassType;
              if AClass = TIocpHttpRequest then
                Args[I] := Request
              else if AClass = TIocpHttpResponse then
                Args[I] := Response
              else if (AClass = TIocpHttpServer) or (AClass = FServer.ClassType) then
                Args[I] := FServer
              else if AClass = TIocpHttpWriter then
                Args[I] := Response.GetOutWriter()
              else if (AClass = TIocpHttpConnection) or (AClass = TIocpClientContext) or (AClass = TIocpCustomContext) then
                Args[I] := Request.Connection
              else if AClass.InheritsFrom(TStream) then
                Args[I] := Request.Data
              else
                Args[I] := nil;
            end;
          tkString, tkLString, tkWString, tkUString:
            begin
              // 如果参数是一个字符串，且名称为 RequestData ，则直接注入为字符串的 Data
              if LowerCase(AParams[I].Name) = 'requestdata' then
                Args[I] := string(Request.DataString);
            end;
        end;
      end;

      // 执行
      if Assigned(AMethod.ReturnType) then begin
        ResultValue := AMethod.Invoke(Item.Controller, Args);
        if not ResultValue.IsEmpty then begin 
          case ResultValue.TypeInfo.Kind of
            // 返回类型为字符串时，认为是视图文件名
            tkString, tkLString, tkWString, tkUString:
              begin
                Key := Prefix + ResultValue.AsString + Suffix;
                Response.SendFile(GetAbsolutePathEx(WebPath, Key), Item.Produces, Item.DownMode, True);
              end;
            // 返回类型为数字时，认为是错误代码
            tkInteger, tkInt64:
              begin
                Response.ErrorRequest(ResultValue.AsInt64);
              end;
            tkClass:
              begin
                try
                  if Item.ResponseBody then
                    Response.Send(SerializeData(ResultValue))
                  else
                    Response.ResponeCode(200);
                finally
                  ResultValue.AsObject.Free;
                end;
              end;
            tkRecord:
              begin
                if Item.ResponseBody then
                  Response.Send(SerializeData(ResultValue))
                else
                  Response.ResponeCode(200);
              end;
          else
            Response.ResponeCode(200);
          end;
        end else
          // 返回内容为空时，直接返回一个Http 200状态
          Response.ResponeCode(200);
      end else begin
        // 无返回值调用
        AMethod.Invoke(Item.Controller, Args);
        Response.ResponeCode(200);
      end;
    finally
      FreeAndNil(APathVariable);
      FreeAndNil(AParamObjs);
    end;
  except
    Log(Exception(ExceptObject).Message);
    if Assigned(Response) and (Response.Active) then
      Response.ServerError(StringA(Exception(ExceptObject).Message));
  end;
end;

procedure TIocpHttpMvcServer.DoHttpWebSocketRequest(
  Sender: TIocpWebSocketServer; Request: TIocpWebSocketRequest;
  Response: TIocpWebSocketResponse);
var
  I, J: Integer;
  AClass: TClass;
  AMethod: TRttiMethod;
  ARttiType: TRttiType;
  AParams: TArray<TRttiParameter>;
  Args: array of TValue;
  ResultValue: TValue;
  Data: string;
begin
  try
    if Request.DataOpcode = wso_Text then begin
      if FUriCaseSensitive then
        Data := LowerCase(Request.DataString())
      else
        Data := Request.DataString();
    end else
      Data := '';

    // 一个 WebSocket 请求允许多个控制器进行处理
    for J := 0 to FWebSocketMap.Count - 1 do begin
      // 检测是否需要触发
      if (FWebSocketMap[J].Data <> '') then begin
        if (FUriCaseSensitive and (LowerCase(FWebSocketMap[J].Data) <> Data)) or 
          (FWebSocketMap[J].Data <> Data) 
        then begin
          Continue;
        end;
      end;
      // 参数注入
      ARttiType := FContext.GetType(FWebSocketMap[J].Controller.ClassType);
      AMethod := ARttiType.GetMethods[FWebSocketMap[J].MethodIndex];
      AParams := AMethod.GetParameters;
      SetLength(Args, Length(AParams));
      for I := 0 to High(AParams) do begin
        // 检测字段绑定，自动注入参数
        case AParams[I].ParamType.TypeKind of
          tkClass:
            begin
              AClass := AParams[I].ParamType.AsInstance.MetaclassType;
              if AClass = TIocpWebSocketRequest then
                Args[I] := Request
              else if AClass = TIocpWebSocketResponse then
                Args[I] := Response
              else if (AClass = TIocpWebSocketServer) or (AClass = Sender.ClassType) then
                Args[I] := Sender
              else if (AClass = TIocpWebSocketConnection) or
                (AClass = TIocpHttpConnection) or
                (AClass = TIocpClientContext) or 
                (AClass = TIocpCustomContext) then
                Args[I] := Response.Connection
              else if (AClass = TBytesCatHelper) then
                Args[I] := Request.Data 
              else
                Args[I] := nil;
            end;
          tkString, tkLString, tkWString, tkUString:
            begin
              // 如果参数是一个字符串，且名称为 RequestData ，则直接注入为字符串的 Data
              if LowerCase(AParams[I].Name) = 'requestdata' then
                Args[I] := Data;
            end;
        end;
      end;   

      // 执行
      if Assigned(AMethod.ReturnType) then begin
        ResultValue := AMethod.Invoke(FWebSocketMap[J].Controller, Args);
        if not ResultValue.IsEmpty then begin        
          case ResultValue.TypeInfo.Kind of
            // 返回类型为字符串时，认为是视图文件名
            tkString, tkLString, tkWString, tkUString:
              begin
                Response.Send(ResultValue.AsString);
              end;
            // 返回类型为数字时，转为字符串
            tkInteger, tkInt64:
              begin
                Response.Send(IntToStr(ResultValue.AsInt64));
              end;
            tkClass:
              begin
                try
                  if FWebSocketMap[J].ResponseBody then
                    Response.Send(SerializeData(ResultValue))
                finally
                  ResultValue.AsObject.Free;
                end;
              end;
            tkRecord:
              begin
                if FWebSocketMap[J].ResponseBody then
                  Response.Send(SerializeData(ResultValue))
              end;
          end;
        end;
      end else
        // 无返回值调用
        AMethod.Invoke(FWebSocketMap[J].Controller, Args);     
    end;
  except
    Log(Exception(ExceptObject).Message);
  end;
end;

function TIocpHttpMvcServer.GetAccessControlAllow: TIocpHttpAccessControlAllow;
begin
  Result := FServer.AccessControlAllow;
end;

function TIocpHttpMvcServer.GetActive: Boolean;
begin
  Result := FServer.Active;
end;

function TIocpHttpMvcServer.GetAutoDecodePostParams: Boolean;
begin
  Result := FServer.AutoDecodePostParams;
end;

function TIocpHttpMvcServer.GetBindAddr: StringA;
begin
  Result := FServer.BindAddr;
end;

function TIocpHttpMvcServer.GetCharset: StringA;
begin
  Result := FServer.Charset;
end;

function TIocpHttpMvcServer.GetContentLanguage: StringA;
begin
  Result := FServer.ContentLanguage;
end;

function TIocpHttpMvcServer.GetGzipFileTypes: string;
begin
  Result := FServer.GzipFileTypes;
end;

function TIocpHttpMvcServer.GetListenPort: Integer;
begin
  Result := FServer.ListenPort;
end;

function TIocpHttpMvcServer.GetServer: TIocpWebSocketServer;
begin
  if FServer is TIocpWebSocketServer then
    Result := FServer as TIocpWebSocketServer
  else
    Result := nil;
end;

function TIocpHttpMvcServer.GetUploadMaxDataSize: NativeUInt;
begin
  Result := FServer.UploadMaxDataSize;
end;

function TIocpHttpMvcServer.GetWebBasePath: string;
begin
  Result := FServer.WebPath;
end;

procedure TIocpHttpMvcServer.InitServer;
var
  Svr: TIocpHttpServer;
  WebSocketDataItem: TWebSocketMapData;
  DataItem: TUriMapData;
  BaseDataItem: TUriMapData;
  BaseUri, ChildUri: string;
  ClassItem: TPair<string, TObject>;
  AMethods: TArray<TRttiMethod>;
  AFileds: TArray<TRttiField>;
  AParams: TArray<TRttiParameter>;
  AContext: TRttiContext;
  ARttiType: TRttiType;
  AClass: TClass;
  I, J, L: Integer;
begin
  // 初始化服务
  if FUseWebSocket then        
    Svr := TIocpWebSocketServer.Create(Owner)
  else
    Svr := TIocpHttpServer.Create(Owner);

  if Assigned(FServer) then begin      
    Svr.ListenPort := ListenPort;
    Svr.AccessControlAllow := AccessControlAllow;
    Svr.Charset := Charset;
    Svr.ContentLanguage := ContentLanguage;
    Svr.WebPath := WebPath;
    Svr.GzipFileTypes := GzipFileTypes;
    Svr.AutoDecodePostParams := AutoDecodePostParams;
    Svr.UploadMaxDataSize := UploadMaxDataSize;
    FreeAndNil(FServer);
    
  end;

  FServer := Svr;
  FServer.OnHttpRequest := DoHttpRequest;
  if Svr is TIocpWebSocketServer then
    (TIocpWebSocketServer(Svr)).OnWebSocketRequest := DoHttpWebSocketRequest;

  // 设计状态不处理
  if (csDesigning in ComponentState) then
    Exit;

  // 初始化映射表
  if not Assigned(MvcScanner) then
    Exit;
  FUriMap.Clear;
  FWebSocketMap.Clear;
  AContext := TRttiContext.Create;
  for ClassItem in MvcScanner.FClassMap do begin
    if not Assigned(ClassItem.Value) then
      Continue;

    // 注入所有标注为 Autowired 的字段
    ARttiType := AContext.GetType(ClassItem.Value.ClassType);
    AFileds := ARttiType.GetFields;
    for I := 0 to High(AFileds) do begin
      if ExistAttribute(AFileds[i].GetAttributes, AutowiredAttribute) then begin
        case AFileds[i].FieldType.TypeKind of
          tkClass:
            begin
              AClass := AFileds[i].FieldType.AsInstance.MetaclassType;
              if AClass = Self.ClassType then // 自己
                AFileds[i].SetValue(ClassItem.Value, TValue.From(Self))
              else if (AClass = FServer.ClassType) then  // FServer
                AFileds[i].SetValue(ClassItem.Value, TValue.From(FServer))
              else if AClass = TIocpHttpServer then  // FServer
                AFileds[i].SetValue(ClassItem.Value, TValue.From(FServer))
            end;
        end;        
      end;
    end;

    // 从类中获取 Value ，作为请求的基础 URI
    // 类中设置的 RequestMapping 标注，仅 Value 有效
    BaseDataItem.Clear;
    if not ClassItem.Value.CheckAttribute(
      function(const Item: TCustomAttribute; const Data: Pointer): Boolean
      begin
        Result := Item.ClassType = RequestMappingAttribute;
        if Result then begin
          BaseUri := RequestMappingAttribute(Item).FValue;
          BaseDataItem.Method := RequestMappingAttribute(Item).FMethod;
          BaseDataItem.Consumes := RequestMappingAttribute(Item).FConsumes;
          BaseDataItem.Produces := RequestMappingAttribute(Item).FProduces;
          BaseDataItem.Headers := RequestMappingAttribute(Item).FHeaders;
        end;
      end)
    then 
      BaseUri := '';
    BaseDataItem.DownMode := ClassItem.Value.ExistAttribute(DownloadAttribute);

    // 遍厉所有方法，找出需要映射的Uri
    ARttiType := AContext.GetType(ClassItem.Value.ClassType);
    AMethods := ARttiType.GetMethods;
    for I := 0 to High(AMethods) do begin
      if AMethods[I].IsConstructor or AMethods[I].IsDestructor or
        AMethods[I].IsClassMethod
      then
        Continue;

      // RequestMapping
      if CheckAttribute(AMethods[I].GetAttributes,
        function(const Item: TCustomAttribute; const Data: Pointer): Boolean
        begin
          Result := Item.ClassType = RequestMappingAttribute;
          if Result then begin
            ChildUri := RequestMappingAttribute(Item).FValue;
            DataItem.Method := RequestMappingAttribute(Item).FMethod;
            DataItem.Consumes := RequestMappingAttribute(Item).FConsumes;
            DataItem.Produces := RequestMappingAttribute(Item).FProduces;
            DataItem.Params := RequestMappingAttribute(Item).FParams;
            DataItem.Headers := RequestMappingAttribute(Item).FHeaders;
          end;
        end)
      then begin
        // 继承类中的设置
        if DataItem.Method = http_Unknown then
          DataItem.Method := BaseDataItem.Method;
        if DataItem.Consumes = '' then
          DataItem.Consumes := BaseDataItem.Consumes;
        if DataItem.Produces = '' then
          DataItem.Produces := BaseDataItem.Produces;
        if DataItem.Headers = '' then
          DataItem.Headers := BaseDataItem.Headers;

        Log(Format('映射URI: %s, 处理方法: %s', [BaseUri + ChildUri, AMethods[I].Name]));
        DataItem.Controller := ClassItem.Value;
        DataItem.MethodIndex := I;
        DataItem.URI := BaseUri + ChildUri;
        DataItem.DownMode := BaseDataItem.DownMode or
          ExistAttribute(AMethods[I].GetAttributes, DownloadAttribute);
        DataItem.ResponseBody := ExistAttribute(AMethods[I].GetAttributes, ResponseBodyAttribute);

        // 判断 Uri 中是否有 PathVariable 字段
        L := Pos('{', ChildUri);
        if L > 0 then begin
          // 检测参数中是否有 PathVariable
          AParams := AMethods[I].GetParameters;
          for J := 0 to High(AParams) do begin
            if ExistAttribute(AParams[J].GetAttributes, PathVariableAttribute) then begin
              ChildUri := ChildUri.Substring(0, L - 1);
              Break;
            end;
          end;
        end;
        // 加入映射表中
        if FUriCaseSensitive then
          FUriMap.Add(BaseUri + ChildUri, DataItem)
        else
          FUriMap.Add(LowerCase(BaseUri + ChildUri), DataItem);
        Continue;
      end;

      // WebSocketAttribute
      if CheckAttribute(AMethods[I].GetAttributes,
        function(const Item: TCustomAttribute; const Data: Pointer): Boolean
        begin
          Result := Item.ClassType = WebSocketAttribute;
          if Result then
            WebSocketDataItem.Data := (WebSocketAttribute(Item)).Data; 
        end)
      then begin 
        WebSocketDataItem.Controller := ClassItem.Value;
        WebSocketDataItem.MethodIndex := I;
        WebSocketDataItem.ResponseBody := ExistAttribute(AMethods[I].GetAttributes, ResponseBodyAttribute);
        FWebSocketMap.Add(WebSocketDataItem);
      end;
    end;
  end;
end;

procedure TIocpHttpMvcServer.LoadConfig(const AFileName: string);
var
  LFileName: string;  
  XML: TXMLDocument;
  LNode: PXMLNode;

  function GetInt(const Name: string; DefaultValue: Integer = 0): Integer;
  var
    ANode: PXMLNode;
  begin
    ANode := LNode.NodeByName(Name);
    if Assigned(ANode) then      
      Result := StrToIntDef(ANode.Text, DefaultValue)
    else
      Result := DefaultValue;
  end;
  
  function GetString(const Name: string): string;
  var
    ANode: PXMLNode;
  begin
    ANode := LNode.NodeByName(Name);
    if Assigned(ANode) then      
      Result := Trim(ANode.Text)
    else Result := '';
  end;
  
  function GetBoolean(const Name: string): Boolean;
  var
    ANode: PXMLNode;
  begin
    ANode := LNode.NodeByName(Name);
    if Assigned(ANode) then
      Result := ANode.AsBoolean
    else
      Result := False;
  end;
  
var
  LActive: Boolean;
begin
  FNeedSaveConfig := True;
  if (AFileName = '') or (not FileExists(AFileName)) then begin
    LFileName := DefaultConfigName;
    if not FileExists(LFileName) then 
      Exit;
  end else
    LFileName := AFileName;
  LActive := Active;
  XML := TXMLDocument.Create();
  try
    XML.LoadFromFile(LFileName);
    LNode := @XML.Root;
    LActive := GetBoolean('Active');
    ListenPort := GetInt('ListenPort', 8080);
    Charset := StringA(GetString('Charset'));
    UseWebSocket := GetBoolean('UseWebSocket');
    BindAddr := StringA(GetString('BindAddr'));
    Prefix := GetString('Prefix');
    Suffix := GetString('Suffix');
    UriCaseSensitive := GetBoolean('UriCaseSensitive');
    ContentLanguage := StringA(GetString('ContentLanguage')); 
    WebPath := GetAbsolutePathEx(AppPath, GetString('WebPath'));
    GzipFileTypes := GetString('GzipFileTypes');
    AutoDecodePostParams := GetBoolean('AutoDecodePostParams');
    UploadMaxDataSize := GetInt('UploadMaxDataSize');
    
    LNode := XML.Root.NodeByName('AccessControlAllow');
    if Assigned(LNode) then begin
      AccessControlAllow.Enabled := GetBoolean('Enabled');
      AccessControlAllow.Origin := GetString('Origin');
      AccessControlAllow.Methods := GetString('Methods');
      AccessControlAllow.Headers := GetString('Headers');
    end;

    FNeedSaveConfig := False;
  finally
    Active := LActive;
    FreeAndNil(XML);
  end;
end;

procedure TIocpHttpMvcServer.SaveConfig(const AFileName: string);
var
  LFileName: string;  
  XML: TXMLDocument;
  LNode: PXMLNode;

  procedure SetInt(const Name: string; const Value: Integer);
  begin
    LNode.AddOrUpdate(Name, Value);
  end;

  procedure SetString(const Name, Value: string);
  begin
    LNode.AddOrUpdate(Name, Value);
  end;
  
  procedure SetBoolean(const Name: string; Value: Boolean);
  begin
    LNode.AddOrUpdate(Name, Value);
  end;
  
begin
  if (AFileName = '') then
    LFileName := DefaultConfigName
  else
    LFileName := AFileName;
  XML := TXMLDocument.Create();
  try
    if FileExists(LFileName) then
      XML.LoadFromFile(LFileName);
    LNode := @XML.Root;
    SetInt('ListenPort', ListenPort);
    SetBoolean('Active', Active);
    SetString('Charset', string(Charset));
    SetString('BindAddr', string(BindAddr));
    SetBoolean('UseWebSocket', UseWebSocket);
    SetString('Prefix', Prefix);
    SetString('Suffix', Suffix);
    SetBoolean('UriCaseSensitive', FUriCaseSensitive);
    SetString('ContentLanguage', string(ContentLanguage));
    SetString('WebPath', GetRelativePath(AppPath, WebPath));
    SetString('GzipFileTypes', GzipFileTypes);
    SetBoolean('AutoDecodePostParams', AutoDecodePostParams);
    SetInt('UploadMaxDataSize', UploadMaxDataSize);
    
    LNode := XML.Node^['AccessControlAllow'];
    if not Assigned(LNode) then
      LNode := XML.AddChild('AccessControlAllow');
    SetBoolean('Enabled', AccessControlAllow.Enabled);
    SetString('Origin', AccessControlAllow.Origin);
    SetString('Methods', AccessControlAllow.Methods);
    SetString('Headers', AccessControlAllow.Headers);
  finally
    XML.SaveToFile(LFileName);
    FreeAndNil(XML);
  end;  
end;

function TIocpHttpMvcServer.SerializeData(const Value: TValue): string;
begin
  if Assigned(FOnSerializeData) then
    Result := FOnSerializeData(Self, Value)
  else
    Result := Value.ToString;
end;

procedure TIocpHttpMvcServer.SetAccessControlAllow(
  const Value: TIocpHttpAccessControlAllow);
begin
  FServer.AccessControlAllow := Value;
end;

procedure TIocpHttpMvcServer.SetActive(const Value: Boolean);
begin
  FServer.Active := Value;
end;

procedure TIocpHttpMvcServer.SetAutoDecodePostParams(const Value: Boolean);
begin
  FServer.AutoDecodePostParams := Value;
end;

procedure TIocpHttpMvcServer.SetBindAddr(const Value: StringA);
begin
  FServer.BindAddr := Value;
end;

procedure TIocpHttpMvcServer.SetCharset(const Value: StringA);
begin
  FServer.Charset := Value;
end;

procedure TIocpHttpMvcServer.SetContentLanguage(const Value: StringA);
begin
  FServer.ContentLanguage := Value;
end;

procedure TIocpHttpMvcServer.SetGzipFileTypes(const Value: string);
begin
  FServer.GzipFileTypes := Value;
end;

procedure TIocpHttpMvcServer.SetListenPort(const Value: Integer);
begin
  FServer.ListenPort := Value;
end;

procedure TIocpHttpMvcServer.SetUploadMaxDataSize(const Value: NativeUInt);
begin
  FServer.UploadMaxDataSize := Value;
end;

procedure TIocpHttpMvcServer.SetUseWebSocket(const Value: Boolean);
begin
  if FUseWebSocket <> Value then begin  
    FUseWebSocket := Value;
    if not (csLoading in ComponentState) then
      InitServer;
  end;
end;

procedure TIocpHttpMvcServer.SetWebBasePath(const Value: string);
begin
  FServer.WebPath := Value;
end;

{ TUriMapData }

procedure TUriMapData.Clear;
begin
  Method := http_Unknown;
  Consumes := '';
  Produces := '';
  Params := '';
  Headers := '';
  Controller := nil;
  MethodIndex := -1;
  DownMode := False;
end;

function TUriMapData.GetHeaderName: string;
begin
  Result := GetNameOrValue(Headers, True);
end;

function TUriMapData.GetHeaderValue: string;
begin
  Result := GetNameOrValue(Headers, False);
end;

function TUriMapData.GetParamName: string;
begin
  Result := GetNameOrValue(Params, True);
end;

function TUriMapData.GetParamValue: string;
begin
  Result := GetNameOrValue(Params, False);
end;

{ TWebSocketMapData }

class function TWebSocketMapData.Create(AController: TObject;
  AMethodIndex: Integer): TWebSocketMapData;
begin
  Result.Controller := AController;
  Result.MethodIndex := AMethodIndex;
end;

initialization
  MvcScanner := TIocpMvcScanner.Create();

finalization
  FreeAndNil(MvcScanner);
  if HttpMvcAllowFree and Assigned(HttpMvc) then begin
    if HttpMvc.FNeedSaveConfig then
      HttpMvc.SaveConfig();
    FreeAndNil(HttpMvc);
  end;

end.
