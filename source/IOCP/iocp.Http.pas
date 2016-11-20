{*******************************************************}
{                                                       }
{       IOCP HTTP 服务组件单元                          }
{                                                       }
{       版权所有 (C) 2015 YangYxd                       }
{                                                       }
{*******************************************************}
{
  IOCP HTTP 服务
  主要功能：
  1. 支持标准Http访问服务
  2. 支持局部传输
  3. 支持文件下载，断点续传
  4. 支持表单提交
  5. 支持GZip压缩传输
  6. 支持文件上传（不能超过上限）
  7. 支持Pipe管道（一次多个请求）
  8. 支持Session
  9. 支持Cookies
  10. 支持ajax跨域设置
}

unit iocp.Http;

{$I 'iocp.inc'}
{$DEFINE UseMemPool_IocpLink}
{$DEFINE UseGZip}

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
{$DEFINE ANSISTRINGS}
{$IFEND}

interface

uses
  iocp.Utils.Hash, iocp.Utils.Str, {$IFDEF UseGZip}ZLibExGZ, {$ENDIF}
  iocp.Sockets, iocp.Task, iocp.core.Engine, iocp.Utils.GMTTime,
  iocp.Sockets.Utils, iocp.Res, iocp.Utils.Queues, iocp.Utils.MemPool,
  {$IFDEF ANSISTRINGS}AnsiStrings, {$ENDIF}
  SyncObjs, Windows, ShlwApi, Classes, SysUtils, DateUtils;

const
  HTTPLineBreak: StringA = #13#10;
  HTTPHeaderEnd: StringA = #13#10#13#10;
  HTTPCTTypeStream: StringA = 'application/octet-stream'; // （ 二进制流，不知道下载文件类型）
  HTTPMethodLen: array [0..8] of Word = (0, 3, 4, 3, 4, 7, 6, 5, 7);
  HTTPSESSIONID: StringA = 'diocp_sid';

type
  /// <summary>
  /// HTTP 1.1 支持8种请求类型
  // 请求方法（所有方法全为大写）有多种，各个方法的解释如下：
  // GET     请求获取Request-URI所标识的资源
  // POST    在Request-URI所标识的资源后附加新的数据，常用于提交表单。
  // HEAD    请求获取由Request-URI所标识的资源的响应消息报头
  // PUT     请求服务器存储一个资源，并用Request-URI作为其标识
  // DELETE  请求服务器删除Request-URI所标识的资源
  // TRACE   请求服务器回送收到的请求信息，主要用于测试或诊断
  // CONNECT 保留将来使用
  // OPTIONS 请求查询服务器的性能，或者查询与资源相关的选项和需求
  /// </summary>
  TIocpHttpMethod = (http_Unknown, http_GET, http_POST, http_PUT, http_HEAD,
    http_OPTIONS, http_DELETE, http_TRACE, http_CONNECT);

type
  /// <summary>
  /// Http 请求版本, 分为 HTTP 1.0, HTTP 1.1
  /// </summary>
  TIocpHttpReqVer = (hv_Unknown, hv_V1, hv_V2);

  /// <summary>
  /// 支持的 Http 编译字符集
  /// </summary>
  TIocpHttpCharset = (hct_Unknown, hct_8859_1 {英文网站}, hct_GB2312 {简体中文},
    hct_UTF8, hct_UTF16);

const
  CS_ACAH = 'X-Requested-With, Content-Type';
  CS_DefaultMethods = 'POST, GET, OPTIONS';

type
  TIocpHttpServer = class;
  TIocpHttpRequest = class;
  TIocpHttpResponse = class;
  TIocpHttpRequestCls = class of TIocpHttpRequest;
  TIocpHttpResponseCls = class of TIocpHttpResponse;
  TIocpArrayString = array of String;

  TOnHttpFilter = procedure (Request: TIocpHttpRequest; var CancelRequest: Boolean) of object;
  TOnHttpRequest = procedure (Sender: TIocpHttpServer;
    Request: TIocpHttpRequest; Response: TIocpHttpResponse) of object;
  /// <summary>
  /// 获取新 Session 事件，返回值将作为当前SessionID的值
  /// </summary>
  TOnHttpGetSession = function (Sender: TIocpHttpServer; const SessionID: string): Pointer of object;
  /// <summary>
  /// 释放(删除) Session 事件，Data 需要外部程序来释放
  /// </summary>
  TOnHttpFreeSession = procedure (Sender: TIocpHttpServer;
    const SessionID: string; var Data: Pointer) of object;

  TIocpPointerStr = record
    P: PAnsiChar;
    Len: Word;
    function IsEmpty: Boolean;
    function ToString(): StringA;
  end;

  /// <summary>
  /// Http跨域控制
  /// </summary>
  TIocpHttpAccessControlAllow = class(TPersistent)
  public
    FEnabled: Boolean;
    FOrigin: string;  // *
    FMethods: string; // GET, POST,PUT,DELETE,OPTIONS
    FHeaders: string; // x-requested-with,content-type
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure MakerHeader(const Data: TStringCatHelperA);
  published
    // 是否启用跨域控制
    property Enabled: Boolean read FEnabled write FEnabled default False;
    // 允许哪些url可以跨域请求到本域
    property Origin: string read FOrigin write FOrigin;
    // 允许的请求方法
    property Methods: string read FMethods write FMethods;
    // 允许哪些请求头可以跨域
    property Headers: string read FHeaders write FHeaders;
  end;

  /// <summary>
  /// Http 表单数据项
  /// </summary>
  TIocpHttpFromDataItem = packed record
  private
    P, FC: PAnsiChar;
    Len: Integer;
    function GetContentType: StringA;
    function GetDisposition: StringA;
    function GetFileName: StringA;
    function GetName: StringA;
    function GetContent: StringA;
    function GetHeaderSize: Integer;
    function GetIsFile: Boolean;
  public
    // 保存表单数据到指定流中
    procedure SaveContentToStream(const Stream: TStream);
    // 获取表单数据的输入流，返回非nil时，在使用完后需手动释放
    function GetContentStream: TStream;
    // 是否为文件数据
    property IsFile: Boolean read GetIsFile;

    property Data: PAnsiChar read P;
    property DataSize: Integer read Len;
    property Name: StringA read GetName;
    property ContentDisposition: StringA read GetDisposition;
    property ContentType: StringA read GetContentType;
    property FileName: StringA read GetFileName;
    property Content: StringA read GetContent;
    property HeaderSize: Integer read GetHeaderSize;
  end;

  TIocpHttpState = (hsCompleted, hsRequest { 接收请求 } , hsRecvingPost { 接收数据 } );

  TIocpHttpConnection = class(TIocpClientContext)
  private
    FRequest: TIocpHttpRequest;
    FHttpState: TIocpHttpState;
  protected
    procedure DoCleanUp; override;
    procedure DoJob(AJob: PIocpJob);
    procedure DoRequest(ARequest: TIocpHttpRequest);
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrorCode: Integer); override;
  public
  end;

  /// <summary>
  /// 客户端Cookie设置类
  /// </summary>
  TIocpHttpCookie = class(TObject)
  private
    FExpires: TDateTime;
    FMaxAge: Cardinal;
    FName: StringA;
    FPath: StringA;
    FValue: StringA;
    FDoMain: StringA;
  public
    constructor Create;
    /// <summary>
    /// 编码成一个String
    /// </summary>
    {$IFDEF UNICODE}
    function ToString: string; override;
    {$ELSE}
    function ToString: StringA;
    {$ENDIF}
    // 指定了coolie的生存期
    property Expires: TDateTime read FExpires write FExpires;
    property Name: StringA read FName write FName;
    property Value: StringA read FValue write FValue;
    // 指定与cookie关联在一起的网页
    property Path: StringA read FPath write FPath;
    // 使多个web服务器共享cookie
    property DoMain: StringA read FDoMain write FDoMain;
    // 用秒来设置cookie的生存期
    property MaxAge: Cardinal read FMaxAge write FMaxAge;
  end;

  TDoReadedParam = procedure (const Name, Value: string; Data: Pointer) of object;

  /// <summary>
  /// HTTP 服务
  /// </summary>
  {$IFDEF SupportEx}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TIocpHttpServer = class(TIocpCustomTcpServer)
  private
    FSessionList: TStringHash;
    FHttpRequestPool: TBaseQueue;
    FHeaderBuildPool: TBaseQueue;
    FUploadMaxDataSize: NativeUInt;
    FMaxHeaderBuildPoolSize: Integer;
    FAutoDecodePostParams: Boolean;
    FWebBasePath: string;
    FGzipFileTypes: string;
    FCharset, FContentLanguage: StringA;
    FAccessControlAllow: TIocpHttpAccessControlAllow;

    FOnHttpRequest: TOnHttpRequest;
    FOnHttpFilter: TOnHttpFilter;
    FOnHttpGetSession: TOnHttpGetSession;
    FOnHttpFreeSession: TOnHttpFreeSession;
    procedure SetAccessControlAllow(const Value: TIocpHttpAccessControlAllow);
    procedure SetGzipFileTypes(const Value: string);
    function GetWebBasePath: string;
    procedure SetWebBasePath(const Value: string);  protected
    procedure FreeSessionList;
    procedure DoFreeHashItem(Item: PHashItem);
  protected
    FHttpRequestClass: TIocpHttpRequestCls;
    FHttpResponseClass: TIocpHttpResponseCls;
    procedure DoRequest(ARequest: TIocpHttpRequest); virtual;
    function GetHttpRequest: TIocpHttpRequest; virtual;
    procedure FreeHttpRequest(V: TIocpHttpRequest);
    function GetHeaderBuilder: TStringCatHelperA;
    procedure FreeHeaderBuilder(V: TStringCatHelperA);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    /// <summary>
    /// 解析请求参数
    /// </summary>
    procedure DecodeParam(P: PAnsiChar; Len: Cardinal;
      DoCallBack: TDoReadedParam; DoCallBackData: Pointer = nil; DecodeURL: Boolean = False);
    /// <summary>
    /// 获取指定ID的Session数据。Session数据由外部程序维护，Http服务只是由
    /// HashMap记录SessionID与Data的关联。
    /// </summary>
    function GetSession(const SID: string): Pointer;
    /// <summary>
    /// 最大Http响应头部构造器内存池大小
    /// </summary>
    property MaxHeaderBuildPoolSize: Integer read FMaxHeaderBuildPoolSize write FMaxHeaderBuildPoolSize;
  published
    property ListenPort default 8080;
    /// <summary>
    /// 跨域控制选项, 默认不启用
    /// </summary>
    property AccessControlAllow: TIocpHttpAccessControlAllow read FAccessControlAllow
      write SetAccessControlAllow;
    /// <summary>
    /// 默认字符集选项，会在响应客户端请求时加入 Content-Type 中。
    /// </summary>
    property Charset: StringA read FCharset write FCharset;
    /// <summary>
    /// 默认响应内容语言。会在响应客户端请求时加入 Content-Language 中。
    /// </summary>
    property ContentLanguage: StringA read FContentLanguage write FContentLanguage;
    /// <summary>
    /// WEB文件夹的根目录。默认为程序所在目录下Web文件夹
    /// </summary>
    property WebPath: string read GetWebBasePath write SetWebBasePath;
    /// <summary>
    /// 下载文件时，自动使用GZip进行压缩的文件类型 (以";"进行分隔)
    /// 如：'.htm;.html;.css;.js;'
    /// </summary>
    property GzipFileTypes: string read FGzipFileTypes write SetGzipFileTypes;
    /// <summary>
    /// 是否自动解析POST参数
    /// </summary>
    property AutoDecodePostParams: Boolean read FAutoDecodePostParams write FAutoDecodePostParams;

    /// 客户端上传数据大小的上限（默认为2M）
    property UploadMaxDataSize: NativeUInt read FUploadMaxDataSize write FUploadMaxDataSize;
    /// <summary>
    /// Http请求过滤事件, 这里返回False将中断当前请求，断开连接
    /// </summary>
    property OnHttpFilter: TOnHttpFilter read FOnHttpFilter write FOnHttpFilter;
    /// <summary>
    /// Http请求事务响应处理
    /// </summary>
    property OnHttpRequest: TOnHttpRequest read FOnHttpRequest write FOnHttpRequest;
    /// <summary>
    /// 获取 Session 数据事件
    /// </summary>
    property OnGetSession: TOnHttpGetSession read FOnHttpGetSession write FOnHttpGetSession;
    /// <summary>
    /// 释放 Session 项通知事件
    /// </summary>
    property OnFreeSession: TOnHttpFreeSession read FOnHttpFreeSession write FOnHttpFreeSession;
  end;

  /// <summary>
  /// Http 请求数据
  /// </summary>
  TIocpHttpRequest = class(TObject)
  private
    FOwner: TIocpHttpServer;
    FConnHandle: Cardinal;
    FConn: TIocpHttpConnection;
    FResponse: TIocpHttpResponse;
    FMethod: TIocpHttpMethod;
    FRequestVersion: TIocpHttpReqVer;
    FDataSize: NativeUInt;
    FHeaderSize: Integer;
    FRequestData: TMemoryStream;
    FKeepAlive: Boolean;
    FAcceptGZip: Boolean;
    FRange: Boolean;
    FIsFormData: Byte;
    FRangeStart, FRangeEnd: Int64;
    FParamHash: TStringHash;
    FParams: TStrings;
    FRawURL: TIocpPointerStr;
    FURI: TIocpPointerStr;
    FURL: StringA;
    FFormDataBoundary: TIocpPointerStr;
    FCookies: TIocpPointerStr;
    FSessionID : StringA;
    FTag: Integer;
    function GetAccept: StringA;
    function GetAcceptEncoding: StringA;
    function GetAcceptLanguage: StringA;
    function GetCookies: StringA;
    function GetHost: StringA;
    function GetParamItem(Index: Integer): StringA;
    function GetReferer: StringA;
    function GetParamsCount: Integer;
    function GetRequestVersionStr: StringA;
    procedure DecodeParams();
    function GetDataStringA: StringA;
    function GetHeaderStr: StringA;
    function GetRawURL: StringA;
    function GetParamIndex(Index: Integer): StringA;
    function GetURI: StringA;
    function GetIsPost: Boolean;
    function GetIsGet: Boolean;
    function GetIsPut: Boolean;
    function GetIsRange: Boolean;
    function GetIsFormData: Boolean;
    function GetFormDataItem(const Key: StringA): TIocpHttpFromDataItem;
    class function InnerGetHeader(const Key: StringA;
      const Data: Pointer; DataSize: Integer): TIocpPointerStr;
    procedure InnerGetCookie;
    procedure CheckCookieSession;
    function GetCookieItem(const Name: StringA): StringA;
    function GetSessionID: StringA;
    function GetCharSet: StringA;
    function GetContentType: StringA;
    function GetGetParamData: StringA;
  protected
    function DecodeHttpRequestMethod(): TIocpHttpMethod;
    function DecodeHttpHeader(): Boolean;
    function DecodeHttpHeaderRange(): Boolean;
    function GetWaitRecvSize: Int64;
    procedure Clear;
    procedure WriteBuffer(P: Pointer; Len: Cardinal);        
    procedure DoReadParamItem(const Name, Value: string; Data: Pointer);

    class function DecodeStr(const S: StringA): StringA;
  public
    constructor Create(AOwner: TIocpHttpServer);
    destructor Destroy; override;

    /// <summary>
    /// 获取当前连接Session
    /// </summary>
    function GetSession: Pointer;

    /// <summary>
    /// 关闭当前请求
    /// </summary>
    procedure Close;

    /// <summary>
    /// 关闭连接, 异步模式，保证正在发送的数据可以发送完成
    /// </summary>
    procedure CloseConnection;

    /// <summary>
    /// 解析Post参数 , 调用本方法后， 就可以使用 Params[Key] 来读取Post的参数了
    /// </summary>
    procedure ParsePostParams();


    /// <summary>
    /// 检测 ContentType 中是否存在指定的值
    /// </summary>
    function ExistContentType(const Text: StringA): Boolean;
    /// <summary>
    /// 判断客户端是否允许接收指定的数据类型
    /// </summary>
    function AllowAccept(const Text: StringA): Boolean;
    /// <summary>
    /// 判断参数是否存在
    /// </summary>
    function ExistParam(const Name: StringA): Boolean;
    /// <summary>
    /// 读取请求参数
    /// </summary>
    function GetParam(const Name: StringA): string;
    /// <summary>
    /// 读取请求参数数组
    /// </summary>
    function GetParamValues(const Name: StringA): TIocpArrayString;
    /// <summary>
    /// 读取请求头
    /// </summary>
    function GetHeader(const Name: StringA): StringA;
    /// <summary>
    /// 读取请求头中指定字段的值
    /// </summary>
    function GetHeaderParam(const Name, ParamName: StringA): StringA;
    /// <summary>
    /// 获取指定字符集字符串形式的请求参数
    /// </summary>
    function GetDataString(const ACharset: string): string; overload;
    function GetDataString(): string; overload;

    property Owner: TIocpHttpServer read FOwner;
    property Connection: TIocpHttpConnection read FConn;
    
    // 是否是Post请求
    property IsPost: Boolean read GetIsPost;
    // 是否是Put请求
    property IsPut: Boolean read GetIsPut;
    // 是否是Get请求
    property IsGet: Boolean read GetIsGet;
    // 是否是表单提交请求 (同时肯定是Post请求)
    property IsFormData: Boolean read GetIsFormData;
    // 是否使用范围传输 (即断点续传)
    property IsRange: Boolean read GetIsRange;
    
    // 请求类型
    property Method: TIocpHttpMethod read FMethod;
    // 原始请求数据长度，一般为0，Post请求时一般大于0
    property ContextLength: NativeUInt read FDataSize;
    // 原始请求数据，一般为nil，Post请求一般会有数据
    property Data: TMemoryStream read FRequestData;
    // 从头信息解码器出来的Url,包含参数
    property URL: StringA read FURL;
    // 不带URL参数
    property URI: StringA read GetURI;
    // 原始URL，带参数
    property RawURL: StringA read GetRawURL;
    // 原始请求头信息
    property Header: StringA read GetHeaderStr;
    // HTTP 请求协议版本
    property RequestVersion: TIocpHttpReqVer read FRequestVersion;
    // HTTP 请求协议版本字符串
    property RequestVersionStr: StringA read GetRequestVersionStr;
    // 字符串形式的请求参数
    property DataString: StringA read GetDataStringA;
    // 请求头部长度
    property HeaderLength: Integer read FHeaderSize;
    // 区域传送开始位置
    property RangeStart: Int64 read FRangeStart;
    // 区域传送结束位置
    property RangeEnd: Int64 read FRangeEnd;
    // POST请求参数字符集
    property CharSet: StringA read GetCharSet;
    
    // ------- 以下属性是在读的时候才从Header中解析 ---------
    property Accept: StringA read GetAccept;
    property AcceptEncoding: StringA read GetAcceptEncoding;
    property AcceptLanguage: StringA read GetAcceptLanguage;
    property AcceptGzip: Boolean read FAcceptGZip;
    property Host: StringA read GetHost;
    property Referer: StringA read GetReferer;
    property ContentType: StringA read GetContentType;
    property Session: Pointer read GetSession;
    property SessionID: StringA read GetSessionID;
    property Cookies: StringA read GetCookies;
    property Cookie[const Name: StringA]: StringA read GetCookieItem;
    property ParamsCount: Integer read GetParamsCount;
    property Params[Index: Integer]: StringA read GetParamIndex;
    property ParamData: StringA read GetGetParamData;
    // 注意，本方法是实时解析，使用时注意效率
    property FormData[const Key: StringA]: TIocpHttpFromDataItem read GetFormDataItem;
    property Tag: Integer read FTag write FTag;
    property Response: TIocpHttpResponse read FResponse;
  end;

  TIocpPointerStream = class(TCustomMemoryStream)
  public
    constructor Create; overload;
    constructor Create(const Ptr: Pointer; ASize: Integer); overload;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  /// <summary>
  /// 响应数据写入器
  /// </summary>
  TIocpHttpWriter = class(TObject)
  private
    FOwner: TIocpHttpResponse;
    FData: TStringCatHelper;
    FCharset: TIocpHttpCharset;
    function GetIsEmpty: Boolean;
    function GetPosition: Int64;
  public
    constructor Create(const BufferSize: Cardinal = 1024 * 8);
    destructor Destroy; override;
    function SetCharset(V: TIocpHttpCharset): TIocpHttpWriter;
    function Write(const Data: string): TIocpHttpWriter; overload;
    function Write(const Data: TIocpArrayString): TIocpHttpWriter; overload;
    function Write(const Data: Integer): TIocpHttpWriter; overload;
    function Write(const Data: Int64): TIocpHttpWriter; overload;
    function Write(const Data: Cardinal): TIocpHttpWriter; overload;
    function Write(const Data: Double): TIocpHttpWriter; overload;
    function ToString: string; {$IFDEF UNICODE} override; {$ENDIF}
    procedure Clear;
    procedure Flush;
    property Charset: TIocpHttpCharset read FCharset write FCharset;
    property IsEmpty: Boolean read GetIsEmpty;
    property Response: TIocpHttpResponse read FOwner write FOwner;
    property Position: Int64 read GetPosition;
  end;

  /// <summary>
  /// Http 连接
  /// </summary>
  TIocpHttpResponse = class(TObject)
  private
    FRequest: TIocpHttpRequest;
    FCookies: TList;
    FCacheTime: Cardinal;
    FGZip: Boolean;
    FBlockSendBuffer: TMemoryStream;
    FOutWriter: TIocpHttpWriter;
    FContentType: StringA;
    FContentLanguage: StringA;
    FCharset: StringA;
    function GetConnection: TIocpHttpConnection;
    function GetActive: Boolean;
    function GetContentType: StringA;
    function GetCharsetType: TIocpHttpCharset;
    procedure SetCharsetType(const Value: TIocpHttpCharset);
  protected
    procedure MakeHeaderEx(const StatusCode: Integer;
      const Data: TStringCatHelperA); virtual;
    function MakeHeader(Data: TStringCatHelperA;
      const ContentLength: Int64; const StatusCode: Integer = 0;
      FileDown: Boolean = False; const FileName: StringA = '';
      const LastModified: TDateTime = 0): Boolean; overload; virtual;
    function MakeHeader(const ContentLength: Int64; const StatusCode: Integer = 0;
      FileDown: Boolean = False; const FileName: StringA = '';
      const LastModified: TDateTime = 0; IsFixHeader: Boolean = False): StringA; overload;
    function MakeFixHeader(const ContentLength: Int64; const StatusCode: Integer = 0;
      FileDown: Boolean = False; const FileName: StringA = '';
      const LastModified: TDateTime = 0): StringA; overload; inline;
    function GetBlockHeader: StringA;
    procedure SendStream(Stream: TStream; IsDownloadFile: Boolean = False;
      const AFileName: StringA = ''; const AContentType: StringA = '';
      const LastModified: TDateTime = 0);
    function CheckFileUpdate(const Last: TDateTime): Boolean;
  public
    constructor Create();
    destructor Destroy; override;

    procedure Clear;

    /// <summary>
    /// 添加Cookie
    /// </summary>
    function AddCookie: TIocpHttpCookie; overload;
    function AddCookie(const Name, Value: StringA): TIocpHttpCookie; overload;
    function AddCookie(const Name, Value: StringA; MaxAge: Cardinal): TIocpHttpCookie; overload;

    /// <summary>
    /// URL重定向, 调用经方法后会发客户端发送重定向包, 重定向包发送后会关闭连接
    /// </summary>
    procedure RedirectURL(const pvURL: StringA);
    /// <summary>
    /// 返回代表请求错误的响应包，并断开连接
    /// </summary>
    procedure ErrorRequest(ErrorCode: Word = 400; const Msg: StringA = '');
    /// <summary>
    /// 返回指定响应代码
    /// </summary>
    procedure ResponeCode(Code: Word; const Data: StringA = '');
    /// <summary>
    /// 返回服务器错误信息，并断开连接
    /// </summary>
    procedure ServerError(const Msg: StringA);

    {$IFDEF UseGZip}
    // GZ 压缩数据
    function GZCompress(const Data: StringA): StringA; overload;
    function GZCompress(const Data: StringW): StringA; overload;
    function GZCompress(Buf: Pointer; Len: Cardinal): StringA; overload;
    function GZCompress(inStream, outStream: TStream): Boolean; overload;
    {$ENDIF}


    /// <summary>
    /// 获取文件修改时间
    /// </summary>
    class function GetFileLastModified(const AFileName: string): TDateTime;

    /// <summary>
    /// 获取文件 Mime 类型
    /// </summary>
    class function GetFileMimeType(const AFileName: string): string;

    /// <summary>
    /// 发送文件, FileName 需要包含文件路径
    /// <param name="FileName">文件名（包含完整路径）</param>
    /// <param name="AContentType">内容类型（非空时不自动解析）</param>
    /// <param name="IsDownFile">是否使用文件下载的方式</param>
    /// <param name="ParserContentType">是否自动解析内容类型（根据文件名称）</param>
    /// </summary>
    procedure SendFile(const FileName: string; const AContentType: string = '';
      IsDownFile: Boolean = True; ParserContentType: Boolean = False); overload; 

    /// <summary>
    /// 发送文件
    /// <param name="Request">下载文件的请求</param>
    /// <param name="AContentType">内容类型（非空时不自动解析）</param>
    /// <param name="IsDownFile">是否使用文件下载的方式</param>
    /// <param name="ParserContentType">是否自动解析内容类型（根据文件名称）</param>
    /// </summary>
    procedure SendFile(Request: TIocpHttpRequest; const AContentType: string = '';
      IsDownFile: Boolean = True; ParserContentType: Boolean = False); overload; inline;

    /// <summary>
    /// 发送文件
    /// <param name="URI">下载文件的请求URI</param>
    /// <param name="AContentType">内容类型（非空时不自动解析）</param>
    /// <param name="IsDownFile">是否使用文件下载的方式</param>
    /// <param name="ParserContentType">是否自动解析内容类型（根据文件名称）</param>
    /// </summary>
    procedure SendFileByURI(const URI: string; const AContentType: string = '';
      IsDownFile: Boolean = True; ParserContentType: Boolean = False);
      
    /// <summary>
    /// 发送文件, FileName用于指定文件名称，可以不是完整路径
    ///  * 文件服务器建议将文件加载到内存，然后使用本函数来发送数据，直接用
    ///  * SendFile会因为磁盘IO问题产生异常
    /// </summary>
    procedure SendFileStream(AFileStream: TStream; const FileName: StringA = '';
      const AContentType: StringA = ''; const LastModified: TDateTime = 0;
      AGZip: Boolean = False);
    procedure SendFileData(const Data: StringA; const FileName: StringA = '';
      const AContentType: StringA = ''; const LastModified: TDateTime = 0;
      AGZip: Boolean = False); overload;
    procedure SendFileData(const Data: StringW; const FileName: StringA = '';
      const AContentType: StringA = ''; const LastModified: TDateTime = 0;
      AGZip: Boolean = False); overload;
    procedure SendFileData(const Data: Pointer; DataLen: Cardinal;
      const FileName: StringA = '';  const AContentType: StringA = '';
      const LastModified: TDateTime = 0; AGZip: Boolean = False); overload;

    /// <summary>
    /// 发送数据，全自动添加 Http 响应头 (异步)
    /// </summary>
    procedure Send(buf: Pointer; len: Cardinal; AGZip: Boolean = False); overload;
    procedure Send(const Data: TBytes; AGZip: Boolean = False); overload;
    procedure Send(const Data: string; AGZip: Boolean = False); overload;
    procedure Send(Stream: TStream; AGZip: Boolean = False); overload;
    // 使用 Writer 来返回数据，可以防止乱码
    procedure Send(var Writer: TIocpHttpWriter; AGZip: Boolean = False; AFreeWriter: Boolean = True); overload;

    procedure SendString(const Data: StringA; AGZip: Boolean = False); overload;
    procedure SendString(const Data: StringW; AGZip: Boolean = False); overload;

    /// <summary>
    /// 发送HTTP响应数据头部 (异步)
    /// </summary>
    procedure SendHeader(DataSize: NativeUInt; AGZip: Boolean = False);
    /// <summary>
    /// 发送数据内容 (异步)(*注意，本函数内部不会再使用GZip压缩数据)
    /// * 要使用GZip，建议的做法是先将数据压缩，再SendHeader，再调用本方法
    /// * 也可以使用分块发送的方式。
    /// </summary>
    procedure SendContent(const Data: StringA); overload;
    procedure SendContent(const Data: StringW); overload;

    /// <summary>
    /// 分块发送HTTP响应数据头部 (异步)
    /// </summary>
    procedure SendChunkHeader(AGZip: Boolean = False); overload;
    /// <summary>
    /// 分块发送数据内容 (异步) (*注意，本函数内部不会再使用GZip压缩数据)
    /// * 每块数据不能独自Gzip，否则浏览器解码时会出错，当然要是自己处理
    /// * 可以这样做。
    /// </summary>
    procedure SendChunk(const Data: StringA); overload;
    procedure SendChunk(const Data: StringW); overload;
    procedure SendChunk(buf: Pointer; len: Cardinal); overload;
    procedure SendChunk(Stream: TStream); overload;
    /// <summary>
    /// 分块发送数据结束 (异步)
    /// </summary>
    procedure SendChunkEnd();

    /// <summary>
    /// 获取一个输出对象, 使用这个对象输出数据，然后调用 Send 方法发送数据。
    /// </summary>
    function GetOutWriter(BufferSize: Cardinal = 1024 * 8): TIocpHttpWriter;

    property Request: TIocpHttpRequest read FRequest;
    property Connection: TIocpHttpConnection read GetConnection;
    property Active: Boolean read GetActive;

    // 缓存控制（浏览器缓存时间：ms)
    property CacheTime: Cardinal read FCacheTime write FCacheTime;
    // 返回内容类型, 默认text/html
    property ContentType: StringA read GetContentType write FContentType;
    // 返回内容语言
    property ContentLanguage: StringA read FContentLanguage write FContentLanguage;
    // 返回内容字符集(默认使用所属服务设置的字符集)
    property Charset: StringA read FCharset write FCharset;
    // 返回内容字符集
    property CharsetType: TIocpHttpCharset read GetCharsetType write SetCharsetType;
  end;

type
  /// <summary>
  /// 只读文件流 
  /// </summary>
  TFileOnlyStream = class(THandleStream)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    property FileName: string read FFileName;
  end;

var
  /// <summary>
  /// 当前执行程序路径
  /// </summary>
  AppPath: string;

function NewSessionID(): string;
function GetResponseCodeNote(V: Word): StringA;
/// <summary>
/// 取绝对路径的函数。需要引用 ShlwApi.pas
/// </summary>
function GetAbsolutePathEx(const BasePath, RelativePath: string): string;
/// <summary>
/// 取相对路径的函数
/// </summary>
function GetRelativePath(const Path, AFile: string): string;

implementation

const
  S_GB2312 = 'gb2312';
  S_UTF_8 = 'utf-8';
  S_UTF_16 = 'utf-16';

var
  Workers: TIocpTask;
  MimeMap: TStringHash;

{$IFDEF ANSISTRINGS}
function StrLIComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer; inline;
begin
  Result := AnsiStrings.StrLIComp(Str1, Str2, MaxLen);
end;
function StrScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar; inline;
begin
  Result := AnsiStrings.StrScan(Str, Chr);
end;
{$ENDIF}

procedure FixHeader(const Header: TStringCatHelperA); overload;
begin
  if Header.RightStr(4) <> HTTPHeaderEnd then begin
    if Header.RightStr(2) = HTTPLineBreak then
      Header.Cat(HTTPLineBreak)
    else
      Header.Cat(HTTPHeaderEnd);
  end;
end;

function FixHeader(const Header: StringA): StringA; overload;
begin
  if (iocp.Utils.Str.RightStr(Header, 4) <> HTTPHeaderEnd) then begin
    if (iocp.Utils.Str.RightStr(Header, 2) = HTTPLineBreak) then
      Result := Header + HTTPLineBreak
    else
      Result := Header + HTTPHeaderEnd;
  end else
    Result := Header;
end;

/// <summary>
/// 取绝对路径的函数。需要引用 ShlwApi.pas
/// </summary>
function GetAbsolutePathEx(const BasePath, RelativePath: string): string;
var
  Dest:array [0..MAX_PATH] of char;
begin
  FillChar(Dest,MAX_PATH+1,0);
  PathCombine(Dest,PChar(BasePath), PChar(RelativePath));
  Result:=string(Dest);
end;

/// <summary>
/// 取相对路径的函数
/// </summary>
function GetRelativePath(const Path, AFile: string): string;
  function GetAttr(IsDir: Boolean): DWORD;
  begin
    if IsDir then
      Result := FILE_ATTRIBUTE_DIRECTORY
    else
      Result := FILE_ATTRIBUTE_NORMAL;
  end;
var
   p: array[0..MAX_PATH] of Char;
begin
  PathRelativePathTo(p, PChar(Path), GetAttr(False), PChar(AFile), GetAttr(True));
  Result := StrPas(p);
end;

function ReplaceSlashFromURI(const Value: string; const NewChar: Char = '\'): string;
var
  P, P1, P2, PMax: PChar;
begin
  P := PChar(Value);
  PMax := P + Length(Value);
  SetLength(Result, PMax - P);
  P1 := PChar(Result);
  P2 := P1;
  while P < PMax do begin
    if (P^ <> '/') then begin
      P1^ := P^;
      Inc(P);
      Inc(P1);
    end else begin
      if P1 > P2 then begin
        P1^ := NewChar;
        Inc(P1);
      end;
      while (P < PMax) and (P^ = '/') do Inc(P);            
    end;
  end;
  SetLength(Result, P1 - P2);
end;

type
  TMimeValue = record
    Key: string;
    Value: string;
  end;

{$REGION 'MIME CONST'}
const
  MimeTypes: array[0..987] of TMimeValue = (
  (Key: 'ez'; Value: 'application/andrew-inset'), // do not localize
  (Key: 'aw'; Value: 'application/applixware'), // do not localize
  (Key: 'atom'; Value: 'application/atom+xml'), // do not localize
  (Key: 'atomcat'; Value: 'application/atomcat+xml'), // do not localize
  (Key: 'atomsvc'; Value: 'application/atomsvc+xml'), // do not localize
  (Key: 'ccxml'; Value: 'application/ccxml+xml'), // do not localize
  (Key: 'cdmia'; Value: 'application/cdmi-capability'), // do not localize
  (Key: 'cdmic'; Value: 'application/cdmi-container'), // do not localize
  (Key: 'cdmid'; Value: 'application/cdmi-domain'), // do not localize
  (Key: 'cdmio'; Value: 'application/cdmi-object'), // do not localize
  (Key: 'cdmiq'; Value: 'application/cdmi-queue'), // do not localize
  (Key: 'cu'; Value: 'application/cu-seeme'), // do not localize
  (Key: 'davmount'; Value: 'application/davmount+xml'), // do not localize
  (Key: 'dbk'; Value: 'application/docbook+xml'), // do not localize
  (Key: 'dssc'; Value: 'application/dssc+der'), // do not localize
  (Key: 'xdssc'; Value: 'application/dssc+xml'), // do not localize
  (Key: 'ecma'; Value: 'application/ecmascript'), // do not localize
  (Key: 'emma'; Value: 'application/emma+xml'), // do not localize
  (Key: 'epub'; Value: 'application/epub+zip'), // do not localize
  (Key: 'exi'; Value: 'application/exi'), // do not localize
  (Key: 'pfr'; Value: 'application/font-tdpfr'), // do not localize
  (Key: 'gml'; Value: 'application/gml+xml'), // do not localize
  (Key: 'gpx'; Value: 'application/gpx+xml'), // do not localize
  (Key: 'gxf'; Value: 'application/gxf'), // do not localize
  (Key: 'stk'; Value: 'application/hyperstudio'), // do not localize
  (Key: 'ink'; Value: 'application/inkml+xml'), // do not localize
  (Key: 'inkml'; Value: 'application/inkml+xml'), // do not localize
  (Key: 'ipfix'; Value: 'application/ipfix'), // do not localize
  (Key: 'jar'; Value: 'application/java-archive'), // do not localize
  (Key: 'ser'; Value: 'application/java-serialized-object'), // do not localize
  (Key: 'class'; Value: 'application/java-vm'), // do not localize
  (Key: 'js'; Value: 'application/javascript'), // do not localize
  (Key: 'json'; Value: 'application/json'), // do not localize
  (Key: 'jsonml'; Value: 'application/jsonml+json'), // do not localize
  (Key: 'lostxml'; Value: 'application/lost+xml'), // do not localize
  (Key: 'hqx'; Value: 'application/mac-binhex40'), // do not localize
  (Key: 'cpt'; Value: 'application/mac-compactpro'), // do not localize
  (Key: 'mads'; Value: 'application/mads+xml'), // do not localize
  (Key: 'mrc'; Value: 'application/marc'), // do not localize
  (Key: 'mrcx'; Value: 'application/marcxml+xml'), // do not localize
  (Key: 'ma'; Value: 'application/mathematica'), // do not localize
  (Key: 'nb'; Value: 'application/mathematica'), // do not localize
  (Key: 'mb'; Value: 'application/mathematica'), // do not localize
  (Key: 'mathml'; Value: 'application/mathml+xml'), // do not localize
  (Key: 'mbox'; Value: 'application/mbox'), // do not localize
  (Key: 'mscml'; Value: 'application/mediaservercontrol+xml'), // do not localize
  (Key: 'metalink'; Value: 'application/metalink+xml'), // do not localize
  (Key: 'meta4'; Value: 'application/metalink4+xml'), // do not localize
  (Key: 'mets'; Value: 'application/mets+xml'), // do not localize
  (Key: 'mods'; Value: 'application/mods+xml'), // do not localize
  (Key: 'm21'; Value: 'application/mp21'), // do not localize
  (Key: 'mp21'; Value: 'application/mp21'), // do not localize
  (Key: 'mp4s'; Value: 'application/mp4'), // do not localize
  (Key: 'doc'; Value: 'application/msword'), // do not localize
  (Key: 'dot'; Value: 'application/msword'), // do not localize
  (Key: 'mxf'; Value: 'application/mxf'), // do not localize
  (Key: 'bin'; Value: 'application/octet-stream'), // do not localize
  (Key: 'bpk'; Value: 'application/octet-stream'), // do not localize
  (Key: 'class'; Value: 'application/octet-stream'), // do not localize
  (Key: 'deploy'; Value: 'application/octet-stream'), // do not localize
  (Key: 'dist'; Value: 'application/octet-stream'), // do not localize
  (Key: 'distz'; Value: 'application/octet-stream'), // do not localize
  (Key: 'dmg'; Value: 'application/octet-stream'), // do not localize
  (Key: 'dms'; Value: 'application/octet-stream'), // do not localize
  (Key: 'dump'; Value: 'application/octet-stream'), // do not localize
  (Key: 'elc'; Value: 'application/octet-stream'), // do not localize
  (Key: 'iso'; Value: 'application/octet-stream'), // do not localize
  (Key: 'lha'; Value: 'application/octet-stream'), // do not localize
  (Key: 'lrf'; Value: 'application/octet-stream'), // do not localize
  (Key: 'lzh'; Value: 'application/octet-stream'), // do not localize
  (Key: 'mar'; Value: 'application/octet-stream'), // do not localize
  (Key: 'pkg'; Value: 'application/octet-stream'), // do not localize
  (Key: 'so'; Value: 'application/octet-stream'), // do not localize
  (Key: 'oda'; Value: 'application/oda'), // do not localize
  (Key: 'opf'; Value: 'application/oebps-package+xml'), // do not localize
  (Key: 'ogx'; Value: 'application/ogg'), // do not localize
  (Key: 'omdoc'; Value: 'application/omdoc+xml'), // do not localize
  (Key: 'onetoc'; Value: 'application/onenote'), // do not localize
  (Key: 'onetoc2'; Value: 'application/onenote'), // do not localize
  (Key: 'onetmp'; Value: 'application/onenote'), // do not localize
  (Key: 'onepkg'; Value: 'application/onenote'), // do not localize
  (Key: 'oxps'; Value: 'application/oxps'), // do not localize
  (Key: 'xer'; Value: 'application/patch-ops-error+xml'), // do not localize
  (Key: 'pdf'; Value: 'application/pdf'), // do not localize
  (Key: 'pgp'; Value: 'application/pgp-encrypted'), // do not localize
  (Key: 'asc'; Value: 'application/pgp-signature'), // do not localize
  (Key: 'sig'; Value: 'application/pgp-signature'), // do not localize
  (Key: 'prf'; Value: 'application/pics-rules'), // do not localize
  (Key: 'p10'; Value: 'application/pkcs10'), // do not localize
  (Key: 'p7m'; Value: 'application/pkcs7-mime'), // do not localize
  (Key: 'p7c'; Value: 'application/pkcs7-mime'), // do not localize
  (Key: 'p7s'; Value: 'application/pkcs7-signature'), // do not localize
  (Key: 'p8'; Value: 'application/pkcs8'), // do not localize
  (Key: 'ac'; Value: 'application/pkix-attr-cert'), // do not localize
  (Key: 'cer'; Value: 'application/pkix-cert'), // do not localize
  (Key: 'crl'; Value: 'application/pkix-crl'), // do not localize
  (Key: 'pkipath'; Value: 'application/pkix-pkipath'), // do not localize
  (Key: 'pki'; Value: 'application/pkixcmp'), // do not localize
  (Key: 'pls'; Value: 'application/pls+xml'), // do not localize
  (Key: 'ai'; Value: 'application/postscript'), // do not localize
  (Key: 'eps'; Value: 'application/postscript'), // do not localize
  (Key: 'ps'; Value: 'application/postscript'), // do not localize
  (Key: 'cww'; Value: 'application/prs.cww'), // do not localize
  (Key: 'pskcxml'; Value: 'application/pskc+xml'), // do not localize
  (Key: 'rdf'; Value: 'application/rdf+xml'), // do not localize
  (Key: 'rif'; Value: 'application/reginfo+xml'), // do not localize
  (Key: 'rnc'; Value: 'application/relax-ng-compact-syntax'), // do not localize
  (Key: 'rl'; Value: 'application/resource-lists+xml'), // do not localize
  (Key: 'rld'; Value: 'application/resource-lists-diff+xml'), // do not localize
  (Key: 'rs'; Value: 'application/rls-services+xml'), // do not localize
  (Key: 'gbr'; Value: 'application/rpki-ghostbusters'), // do not localize
  (Key: 'mft'; Value: 'application/rpki-manifest'), // do not localize
  (Key: 'roa'; Value: 'application/rpki-roa'), // do not localize
  (Key: 'rsd'; Value: 'application/rsd+xml'), // do not localize
  (Key: 'rss'; Value: 'application/rss+xml'), // do not localize
  (Key: 'rtf'; Value: 'application/rtf'), // do not localize
  (Key: 'sbml'; Value: 'application/sbml+xml'), // do not localize
  (Key: 'scq'; Value: 'application/scvp-cv-request'), // do not localize
  (Key: 'scs'; Value: 'application/scvp-cv-response'), // do not localize
  (Key: 'spq'; Value: 'application/scvp-vp-request'), // do not localize
  (Key: 'spp'; Value: 'application/scvp-vp-response'), // do not localize
  (Key: 'sdp'; Value: 'application/sdp'), // do not localize
  (Key: 'setpay'; Value: 'application/set-payment-initiation'), // do not localize
  (Key: 'setreg'; Value: 'application/set-registration-initiation'), // do not localize
  (Key: 'shf'; Value: 'application/shf+xml'), // do not localize
  (Key: 'smi'; Value: 'application/smil+xml'), // do not localize
  (Key: 'smil'; Value: 'application/smil+xml'), // do not localize
  (Key: 'rq'; Value: 'application/sparql-query'), // do not localize
  (Key: 'srx'; Value: 'application/sparql-results+xml'), // do not localize
  (Key: 'gram'; Value: 'application/srgs'), // do not localize
  (Key: 'grxml'; Value: 'application/srgs+xml'), // do not localize
  (Key: 'sru'; Value: 'application/sru+xml'), // do not localize
  (Key: 'ssdl'; Value: 'application/ssdl+xml'), // do not localize
  (Key: 'ssml'; Value: 'application/ssml+xml'), // do not localize
  (Key: 'tei'; Value: 'application/tei+xml'), // do not localize
  (Key: 'teicorpus'; Value: 'application/tei+xml'), // do not localize
  (Key: 'tfi'; Value: 'application/thraud+xml'), // do not localize
  (Key: 'tsd'; Value: 'application/timestamped-data'), // do not localize
  (Key: 'plb'; Value: 'application/vnd.3gpp.pic-bw-large'), // do not localize
  (Key: 'psb'; Value: 'application/vnd.3gpp.pic-bw-small'), // do not localize
  (Key: 'pvb'; Value: 'application/vnd.3gpp.pic-bw-var'), // do not localize
  (Key: 'tcap'; Value: 'application/vnd.3gpp2.tcap'), // do not localize
  (Key: 'pwn'; Value: 'application/vnd.3m.post-it-notes'), // do not localize
  (Key: 'aso'; Value: 'application/vnd.accpac.simply.aso'), // do not localize
  (Key: 'imp'; Value: 'application/vnd.accpac.simply.imp'), // do not localize
  (Key: 'acu'; Value: 'application/vnd.acucobol'), // do not localize
  (Key: 'atc'; Value: 'application/vnd.acucorp'), // do not localize
  (Key: 'acutc'; Value: 'application/vnd.acucorp'), // do not localize
  (Key: 'air'; Value: 'application/vnd.adobe.air-application-installer-package+zip'), // do not localize
  (Key: 'fcdt'; Value: 'application/vnd.adobe.formscentral.fcdt'), // do not localize
  (Key: 'fxp'; Value: 'application/vnd.adobe.fxp'), // do not localize
  (Key: 'fxpl'; Value: 'application/vnd.adobe.fxp'), // do not localize
  (Key: 'xdp'; Value: 'application/vnd.adobe.xdp+xml'), // do not localize
  (Key: 'xfdf'; Value: 'application/vnd.adobe.xfdf'), // do not localize
  (Key: 'ahead'; Value: 'application/vnd.ahead.space'), // do not localize
  (Key: 'azf'; Value: 'application/vnd.airzip.filesecure.azf'), // do not localize
  (Key: 'azs'; Value: 'application/vnd.airzip.filesecure.azs'), // do not localize
  (Key: 'azw'; Value: 'application/vnd.amazon.ebook'), // do not localize
  (Key: 'acc'; Value: 'application/vnd.americandynamics.acc'), // do not localize
  (Key: 'ami'; Value: 'application/vnd.amiga.ami'), // do not localize
  (Key: 'apk'; Value: 'application/vnd.android.package-archive'), // do not localize
  (Key: 'cii'; Value: 'application/vnd.anser-web-certificate-issue-initiation'), // do not localize
  (Key: 'fti'; Value: 'application/vnd.anser-web-funds-transfer-initiation'), // do not localize
  (Key: 'atx'; Value: 'application/vnd.antix.game-component'), // do not localize
  (Key: 'mpkg'; Value: 'application/vnd.apple.installer+xml'), // do not localize
  (Key: 'm3u8'; Value: 'application/vnd.apple.mpegurl'), // do not localize
  (Key: 'swi'; Value: 'application/vnd.aristanetworks.swi'), // do not localize
  (Key: 'iota'; Value: 'application/vnd.astraea-software.iota'), // do not localize
  (Key: 'aep'; Value: 'application/vnd.audiograph'), // do not localize
  (Key: 'mpm'; Value: 'application/vnd.blueice.multipass'), // do not localize
  (Key: 'bmi'; Value: 'application/vnd.bmi'), // do not localize
  (Key: 'rep'; Value: 'application/vnd.businessobjects'), // do not localize
  (Key: 'cdxml'; Value: 'application/vnd.chemdraw+xml'), // do not localize
  (Key: 'mmd'; Value: 'application/vnd.chipnuts.karaoke-mmd'), // do not localize
  (Key: 'cdy'; Value: 'application/vnd.cinderella'), // do not localize
  (Key: 'cla'; Value: 'application/vnd.claymore'), // do not localize
  (Key: 'rp9'; Value: 'application/vnd.cloanto.rp9'), // do not localize
  (Key: 'c4g'; Value: 'application/vnd.clonk.c4group'), // do not localize
  (Key: 'c4d'; Value: 'application/vnd.clonk.c4group'), // do not localize
  (Key: 'c4f'; Value: 'application/vnd.clonk.c4group'), // do not localize
  (Key: 'c4p'; Value: 'application/vnd.clonk.c4group'), // do not localize
  (Key: 'c4u'; Value: 'application/vnd.clonk.c4group'), // do not localize
  (Key: 'c11amc'; Value: 'application/vnd.cluetrust.cartomobile-config'), // do not localize
  (Key: 'c11amz'; Value: 'application/vnd.cluetrust.cartomobile-config-pkg'), // do not localize
  (Key: 'csp'; Value: 'application/vnd.commonspace'), // do not localize
  (Key: 'cdbcmsg'; Value: 'application/vnd.contact.cmsg'), // do not localize
  (Key: 'cmc'; Value: 'application/vnd.cosmocaller'), // do not localize
  (Key: 'clkx'; Value: 'application/vnd.crick.clicker'), // do not localize
  (Key: 'clkk'; Value: 'application/vnd.crick.clicker.keyboard'), // do not localize
  (Key: 'clkp'; Value: 'application/vnd.crick.clicker.palette'), // do not localize
  (Key: 'clkt'; Value: 'application/vnd.crick.clicker.template'), // do not localize
  (Key: 'clkw'; Value: 'application/vnd.crick.clicker.wordbank'), // do not localize
  (Key: 'wbs'; Value: 'application/vnd.criticaltools.wbs+xml'), // do not localize
  (Key: 'pml'; Value: 'application/vnd.ctc-posml'), // do not localize
  (Key: 'ppd'; Value: 'application/vnd.cups-ppd'), // do not localize
  (Key: 'car'; Value: 'application/vnd.curl.car'), // do not localize
  (Key: 'pcurl'; Value: 'application/vnd.curl.pcurl'), // do not localize
  (Key: 'dart'; Value: 'application/vnd.dart'), // do not localize
  (Key: 'rdz'; Value: 'application/vnd.data-vision.rdz'), // do not localize
  (Key: 'uvf'; Value: 'application/vnd.dece.data'), // do not localize
  (Key: 'uvvf'; Value: 'application/vnd.dece.data'), // do not localize
  (Key: 'uvd'; Value: 'application/vnd.dece.data'), // do not localize
  (Key: 'uvvd'; Value: 'application/vnd.dece.data'), // do not localize
  (Key: 'uvt'; Value: 'application/vnd.dece.ttml+xml'), // do not localize
  (Key: 'uvvt'; Value: 'application/vnd.dece.ttml+xml'), // do not localize
  (Key: 'uvx'; Value: 'application/vnd.dece.unspecified'), // do not localize
  (Key: 'uvvx'; Value: 'application/vnd.dece.unspecified'), // do not localize
  (Key: 'uvz'; Value: 'application/vnd.dece.zip'), // do not localize
  (Key: 'uvvz'; Value: 'application/vnd.dece.zip'), // do not localize
  (Key: 'fe_launch'; Value: 'application/vnd.denovo.fcselayout-link'), // do not localize
  (Key: 'dna'; Value: 'application/vnd.dna'), // do not localize
  (Key: 'mlp'; Value: 'application/vnd.dolby.mlp'), // do not localize
  (Key: 'dpg'; Value: 'application/vnd.dpgraph'), // do not localize
  (Key: 'dfac'; Value: 'application/vnd.dreamfactory'), // do not localize
  (Key: 'kpxx'; Value: 'application/vnd.ds-keypoint'), // do not localize
  (Key: 'ait'; Value: 'application/vnd.dvb.ait'), // do not localize
  (Key: 'svc'; Value: 'application/vnd.dvb.service'), // do not localize
  (Key: 'geo'; Value: 'application/vnd.dynageo'), // do not localize
  (Key: 'mag'; Value: 'application/vnd.ecowin.chart'), // do not localize
  (Key: 'nml'; Value: 'application/vnd.enliven'), // do not localize
  (Key: 'esf'; Value: 'application/vnd.epson.esf'), // do not localize
  (Key: 'msf'; Value: 'application/vnd.epson.msf'), // do not localize
  (Key: 'qam'; Value: 'application/vnd.epson.quickanime'), // do not localize
  (Key: 'slt'; Value: 'application/vnd.epson.salt'), // do not localize
  (Key: 'ssf'; Value: 'application/vnd.epson.ssf'), // do not localize
  (Key: 'es3'; Value: 'application/vnd.eszigno3+xml'), // do not localize
  (Key: 'et3'; Value: 'application/vnd.eszigno3+xml'), // do not localize
  (Key: 'ez2'; Value: 'application/vnd.ezpix-album'), // do not localize
  (Key: 'ez3'; Value: 'application/vnd.ezpix-package'), // do not localize
  (Key: 'fdf'; Value: 'application/vnd.fdf'), // do not localize
  (Key: 'mseed'; Value: 'application/vnd.fdsn.mseed'), // do not localize
  (Key: 'seed'; Value: 'application/vnd.fdsn.seed'), // do not localize
  (Key: 'dataless'; Value: 'application/vnd.fdsn.seed'), // do not localize
  (Key: 'gph'; Value: 'application/vnd.flographit'), // do not localize
  (Key: 'ftc'; Value: 'application/vnd.fluxtime.clip'), // do not localize
  (Key: 'fm'; Value: 'application/vnd.framemaker'), // do not localize
  (Key: 'frame'; Value: 'application/vnd.framemaker'), // do not localize
  (Key: 'maker'; Value: 'application/vnd.framemaker'), // do not localize
  (Key: 'book'; Value: 'application/vnd.framemaker'), // do not localize
  (Key: 'fnc'; Value: 'application/vnd.frogans.fnc'), // do not localize
  (Key: 'ltf'; Value: 'application/vnd.frogans.ltf'), // do not localize
  (Key: 'fsc'; Value: 'application/vnd.fsc.weblaunch'), // do not localize
  (Key: 'oas'; Value: 'application/vnd.fujitsu.oasys'), // do not localize
  (Key: 'oa2'; Value: 'application/vnd.fujitsu.oasys2'), // do not localize
  (Key: 'oa3'; Value: 'application/vnd.fujitsu.oasys3'), // do not localize
  (Key: 'fg5'; Value: 'application/vnd.fujitsu.oasysgp'), // do not localize
  (Key: 'bh2'; Value: 'application/vnd.fujitsu.oasysprs'), // do not localize
  (Key: 'ddd'; Value: 'application/vnd.fujixerox.ddd'), // do not localize
  (Key: 'xdw'; Value: 'application/vnd.fujixerox.docuworks'), // do not localize
  (Key: 'xbd'; Value: 'application/vnd.fujixerox.docuworks.binder'), // do not localize
  (Key: 'fzs'; Value: 'application/vnd.fuzzysheet'), // do not localize
  (Key: 'txd'; Value: 'application/vnd.genomatix.tuxedo'), // do not localize
  (Key: 'ggb'; Value: 'application/vnd.geogebra.file'), // do not localize
  (Key: 'ggt'; Value: 'application/vnd.geogebra.tool'), // do not localize
  (Key: 'gex'; Value: 'application/vnd.geometry-explorer'), // do not localize
  (Key: 'gre'; Value: 'application/vnd.geometry-explorer'), // do not localize
  (Key: 'gxt'; Value: 'application/vnd.geonext'), // do not localize
  (Key: 'g2w'; Value: 'application/vnd.geoplan'), // do not localize
  (Key: 'g3w'; Value: 'application/vnd.geospace'), // do not localize
  (Key: 'gmx'; Value: 'application/vnd.gmx'), // do not localize
  (Key: 'kml'; Value: 'application/vnd.google-earth.kml+xml'), // do not localize
  (Key: 'kmz'; Value: 'application/vnd.google-earth.kmz'), // do not localize
  (Key: 'gqf'; Value: 'application/vnd.grafeq'), // do not localize
  (Key: 'gqs'; Value: 'application/vnd.grafeq'), // do not localize
  (Key: 'gac'; Value: 'application/vnd.groove-account'), // do not localize
  (Key: 'ghf'; Value: 'application/vnd.groove-help'), // do not localize
  (Key: 'gim'; Value: 'application/vnd.groove-identity-message'), // do not localize
  (Key: 'grv'; Value: 'application/vnd.groove-injector'), // do not localize
  (Key: 'gtm'; Value: 'application/vnd.groove-tool-message'), // do not localize
  (Key: 'tpl'; Value: 'application/vnd.groove-tool-template'), // do not localize
  (Key: 'vcg'; Value: 'application/vnd.groove-vcard'), // do not localize
  (Key: 'hal'; Value: 'application/vnd.hal+xml'), // do not localize
  (Key: 'zmm'; Value: 'application/vnd.handheld-entertainment+xml'), // do not localize
  (Key: 'hbci'; Value: 'application/vnd.hbci'), // do not localize
  (Key: 'les'; Value: 'application/vnd.hhe.lesson-player'), // do not localize
  (Key: 'hpgl'; Value: 'application/vnd.hp-hpgl'), // do not localize
  (Key: 'hpid'; Value: 'application/vnd.hp-hpid'), // do not localize
  (Key: 'hps'; Value: 'application/vnd.hp-hps'), // do not localize
  (Key: 'jlt'; Value: 'application/vnd.hp-jlyt'), // do not localize
  (Key: 'pcl'; Value: 'application/vnd.hp-pcl'), // do not localize
  (Key: 'pclxl'; Value: 'application/vnd.hp-pclxl'), // do not localize
  (Key: 'sfd-hdstx'; Value: 'application/vnd.hydrostatix.sof-data'), // do not localize
  (Key: 'mpy'; Value: 'application/vnd.ibm.minipay'), // do not localize
  (Key: 'afp'; Value: 'application/vnd.ibm.modcap'), // do not localize
  (Key: 'listafp'; Value: 'application/vnd.ibm.modcap'), // do not localize
  (Key: 'list3820'; Value: 'application/vnd.ibm.modcap'), // do not localize
  (Key: 'irm'; Value: 'application/vnd.ibm.rights-management'), // do not localize
  (Key: 'sc'; Value: 'application/vnd.ibm.secure-container'), // do not localize
  (Key: 'icc'; Value: 'application/vnd.iccprofile'), // do not localize
  (Key: 'icm'; Value: 'application/vnd.iccprofile'), // do not localize
  (Key: 'igl'; Value: 'application/vnd.igloader'), // do not localize
  (Key: 'ivp'; Value: 'application/vnd.immervision-ivp'), // do not localize
  (Key: 'ivu'; Value: 'application/vnd.immervision-ivu'), // do not localize
  (Key: 'igm'; Value: 'application/vnd.insors.igm'), // do not localize
  (Key: 'xpw'; Value: 'application/vnd.intercon.formnet'), // do not localize
  (Key: 'xpx'; Value: 'application/vnd.intercon.formnet'), // do not localize
  (Key: 'i2g'; Value: 'application/vnd.intergeo'), // do not localize
  (Key: 'qbo'; Value: 'application/vnd.intu.qbo'), // do not localize
  (Key: 'qfx'; Value: 'application/vnd.intu.qfx'), // do not localize
  (Key: 'rcprofile'; Value: 'application/vnd.ipunplugged.rcprofile'), // do not localize
  (Key: 'irp'; Value: 'application/vnd.irepository.package+xml'), // do not localize
  (Key: 'xpr'; Value: 'application/vnd.is-xpr'), // do not localize
  (Key: 'fcs'; Value: 'application/vnd.isac.fcs'), // do not localize
  (Key: 'jam'; Value: 'application/vnd.jam'), // do not localize
  (Key: 'rms'; Value: 'application/vnd.jcp.javame.midlet-rms'), // do not localize
  (Key: 'jisp'; Value: 'application/vnd.jisp'), // do not localize
  (Key: 'joda'; Value: 'application/vnd.joost.joda-archive'), // do not localize
  (Key: 'ktz'; Value: 'application/vnd.kahootz'), // do not localize
  (Key: 'ktr'; Value: 'application/vnd.kahootz'), // do not localize
  (Key: 'karbon'; Value: 'application/vnd.kde.karbon'), // do not localize
  (Key: 'chrt'; Value: 'application/vnd.kde.kchart'), // do not localize
  (Key: 'kfo'; Value: 'application/vnd.kde.kformula'), // do not localize
  (Key: 'flw'; Value: 'application/vnd.kde.kivio'), // do not localize
  (Key: 'kon'; Value: 'application/vnd.kde.kontour'), // do not localize
  (Key: 'kpr'; Value: 'application/vnd.kde.kpresenter'), // do not localize
  (Key: 'kpt'; Value: 'application/vnd.kde.kpresenter'), // do not localize
  (Key: 'ksp'; Value: 'application/vnd.kde.kspread'), // do not localize
  (Key: 'kwd'; Value: 'application/vnd.kde.kword'), // do not localize
  (Key: 'kwt'; Value: 'application/vnd.kde.kword'), // do not localize
  (Key: 'htke'; Value: 'application/vnd.kenameaapp'), // do not localize
  (Key: 'kia'; Value: 'application/vnd.kidspiration'), // do not localize
  (Key: 'kne'; Value: 'application/vnd.kinar'), // do not localize
  (Key: 'knp'; Value: 'application/vnd.kinar'), // do not localize
  (Key: 'skp'; Value: 'application/vnd.koan'), // do not localize
  (Key: 'skd'; Value: 'application/vnd.koan'), // do not localize
  (Key: 'skt'; Value: 'application/vnd.koan'), // do not localize
  (Key: 'skm'; Value: 'application/vnd.koan'), // do not localize
  (Key: 'sse'; Value: 'application/vnd.kodak-descriptor'), // do not localize
  (Key: 'lasxml'; Value: 'application/vnd.las.las+xml'), // do not localize
  (Key: 'lbd'; Value: 'application/vnd.llamagraphics.life-balance.desktop'), // do not localize
  (Key: 'lbe'; Value: 'application/vnd.llamagraphics.life-balance.exchange+xml'), // do not localize
  (Key: '123'; Value: 'application/vnd.lotus-1-2-3'), // do not localize
  (Key: 'apr'; Value: 'application/vnd.lotus-approach'), // do not localize
  (Key: 'pre'; Value: 'application/vnd.lotus-freelance'), // do not localize
  (Key: 'nsf'; Value: 'application/vnd.lotus-notes'), // do not localize
  (Key: 'org'; Value: 'application/vnd.lotus-organizer'), // do not localize
  (Key: 'scm'; Value: 'application/vnd.lotus-screencam'), // do not localize
  (Key: 'lwp'; Value: 'application/vnd.lotus-wordpro'), // do not localize
  (Key: 'portpkg'; Value: 'application/vnd.macports.portpkg'), // do not localize
  (Key: 'mcd'; Value: 'application/vnd.mcd'), // do not localize
  (Key: 'mc1'; Value: 'application/vnd.medcalcdata'), // do not localize
  (Key: 'cdkey'; Value: 'application/vnd.mediastation.cdkey'), // do not localize
  (Key: 'mwf'; Value: 'application/vnd.mfer'), // do not localize
  (Key: 'mfm'; Value: 'application/vnd.mfmp'), // do not localize
  (Key: 'flo'; Value: 'application/vnd.micrografx.flo'), // do not localize
  (Key: 'igx'; Value: 'application/vnd.micrografx.igx'), // do not localize
  (Key: 'mif'; Value: 'application/vnd.mif'), // do not localize
  (Key: 'daf'; Value: 'application/vnd.mobius.daf'), // do not localize
  (Key: 'dis'; Value: 'application/vnd.mobius.dis'), // do not localize
  (Key: 'mbk'; Value: 'application/vnd.mobius.mbk'), // do not localize
  (Key: 'mqy'; Value: 'application/vnd.mobius.mqy'), // do not localize
  (Key: 'msl'; Value: 'application/vnd.mobius.msl'), // do not localize
  (Key: 'plc'; Value: 'application/vnd.mobius.plc'), // do not localize
  (Key: 'txf'; Value: 'application/vnd.mobius.txf'), // do not localize
  (Key: 'mpn'; Value: 'application/vnd.mophun.application'), // do not localize
  (Key: 'mpc'; Value: 'application/vnd.mophun.certificate'), // do not localize
  (Key: 'xul'; Value: 'application/vnd.mozilla.xul+xml'), // do not localize
  (Key: 'cil'; Value: 'application/vnd.ms-artgalry'), // do not localize
  (Key: 'cab'; Value: 'application/vnd.ms-cab-compressed'), // do not localize
  (Key: 'xls'; Value: 'application/vnd.ms-excel'), // do not localize
  (Key: 'xlm'; Value: 'application/vnd.ms-excel'), // do not localize
  (Key: 'xla'; Value: 'application/vnd.ms-excel'), // do not localize
  (Key: 'xlc'; Value: 'application/vnd.ms-excel'), // do not localize
  (Key: 'xlt'; Value: 'application/vnd.ms-excel'), // do not localize
  (Key: 'xlw'; Value: 'application/vnd.ms-excel'), // do not localize
  (Key: 'xlam'; Value: 'application/vnd.ms-excel.addin.macroenabled.12'), // do not localize
  (Key: 'xlsb'; Value: 'application/vnd.ms-excel.sheet.binary.macroenabled.12'), // do not localize
  (Key: 'xlsm'; Value: 'application/vnd.ms-excel.sheet.macroenabled.12'), // do not localize
  (Key: 'xltm'; Value: 'application/vnd.ms-excel.template.macroenabled.12'), // do not localize
  (Key: 'eot'; Value: 'application/vnd.ms-fontobject'), // do not localize
  (Key: 'chm'; Value: 'application/vnd.ms-htmlhelp'), // do not localize
  (Key: 'ims'; Value: 'application/vnd.ms-ims'), // do not localize
  (Key: 'lrm'; Value: 'application/vnd.ms-lrm'), // do not localize
  (Key: 'thmx'; Value: 'application/vnd.ms-officetheme'), // do not localize
  (Key: 'cat'; Value: 'application/vnd.ms-pki.seccat'), // do not localize
  (Key: 'stl'; Value: 'application/vnd.ms-pki.stl'), // do not localize
  (Key: 'ppt'; Value: 'application/vnd.ms-powerpoint'), // do not localize
  (Key: 'pps'; Value: 'application/vnd.ms-powerpoint'), // do not localize
  (Key: 'pot'; Value: 'application/vnd.ms-powerpoint'), // do not localize
  (Key: 'ppam'; Value: 'application/vnd.ms-powerpoint.addin.macroenabled.12'), // do not localize
  (Key: 'pptm'; Value: 'application/vnd.ms-powerpoint.presentation.macroenabled.12'), // do not localize
  (Key: 'sldm'; Value: 'application/vnd.ms-powerpoint.slide.macroenabled.12'), // do not localize
  (Key: 'ppsm'; Value: 'application/vnd.ms-powerpoint.slideshow.macroenabled.12'), // do not localize
  (Key: 'potm'; Value: 'application/vnd.ms-powerpoint.template.macroenabled.12'), // do not localize
  (Key: 'mpp'; Value: 'application/vnd.ms-project'), // do not localize
  (Key: 'mpt'; Value: 'application/vnd.ms-project'), // do not localize
  (Key: 'docm'; Value: 'application/vnd.ms-word.document.macroenabled.12'), // do not localize
  (Key: 'dotm'; Value: 'application/vnd.ms-word.template.macroenabled.12'), // do not localize
  (Key: 'wps'; Value: 'application/vnd.ms-works'), // do not localize
  (Key: 'wks'; Value: 'application/vnd.ms-works'), // do not localize
  (Key: 'wcm'; Value: 'application/vnd.ms-works'), // do not localize
  (Key: 'wdb'; Value: 'application/vnd.ms-works'), // do not localize
  (Key: 'wpl'; Value: 'application/vnd.ms-wpl'), // do not localize
  (Key: 'xps'; Value: 'application/vnd.ms-xpsdocument'), // do not localize
  (Key: 'mseq'; Value: 'application/vnd.mseq'), // do not localize
  (Key: 'mus'; Value: 'application/vnd.musician'), // do not localize
  (Key: 'msty'; Value: 'application/vnd.muvee.style'), // do not localize
  (Key: 'taglet'; Value: 'application/vnd.mynfc'), // do not localize
  (Key: 'nlu'; Value: 'application/vnd.neurolanguage.nlu'), // do not localize
  (Key: 'ntf'; Value: 'application/vnd.nitf'), // do not localize
  (Key: 'nitf'; Value: 'application/vnd.nitf'), // do not localize
  (Key: 'nnd'; Value: 'application/vnd.noblenet-directory'), // do not localize
  (Key: 'nns'; Value: 'application/vnd.noblenet-sealer'), // do not localize
  (Key: 'nnw'; Value: 'application/vnd.noblenet-web'), // do not localize
  (Key: 'ngdat'; Value: 'application/vnd.nokia.n-gage.data'), // do not localize
  (Key: 'n-gage'; Value: 'application/vnd.nokia.n-gage.symbian.install'), // do not localize
  (Key: 'rpst'; Value: 'application/vnd.nokia.radio-preset'), // do not localize
  (Key: 'rpss'; Value: 'application/vnd.nokia.radio-presets'), // do not localize
  (Key: 'edm'; Value: 'application/vnd.novadigm.edm'), // do not localize
  (Key: 'edx'; Value: 'application/vnd.novadigm.edx'), // do not localize
  (Key: 'ext'; Value: 'application/vnd.novadigm.ext'), // do not localize
  (Key: 'odc'; Value: 'application/vnd.oasis.opendocument.chart'), // do not localize
  (Key: 'otc'; Value: 'application/vnd.oasis.opendocument.chart-template'), // do not localize
  (Key: 'odb'; Value: 'application/vnd.oasis.opendocument.database'), // do not localize
  (Key: 'odf'; Value: 'application/vnd.oasis.opendocument.formula'), // do not localize
  (Key: 'odft'; Value: 'application/vnd.oasis.opendocument.formula-template'), // do not localize
  (Key: 'odg'; Value: 'application/vnd.oasis.opendocument.graphics'), // do not localize
  (Key: 'otg'; Value: 'application/vnd.oasis.opendocument.graphics-template'), // do not localize
  (Key: 'odi'; Value: 'application/vnd.oasis.opendocument.image'), // do not localize
  (Key: 'oti'; Value: 'application/vnd.oasis.opendocument.image-template'), // do not localize
  (Key: 'odp'; Value: 'application/vnd.oasis.opendocument.presentation'), // do not localize
  (Key: 'otp'; Value: 'application/vnd.oasis.opendocument.presentation-template'), // do not localize
  (Key: 'ods'; Value: 'application/vnd.oasis.opendocument.spreadsheet'), // do not localize
  (Key: 'ots'; Value: 'application/vnd.oasis.opendocument.spreadsheet-template'), // do not localize
  (Key: 'odt'; Value: 'application/vnd.oasis.opendocument.text'), // do not localize
  (Key: 'odm'; Value: 'application/vnd.oasis.opendocument.text-master'), // do not localize
  (Key: 'ott'; Value: 'application/vnd.oasis.opendocument.text-template'), // do not localize
  (Key: 'oth'; Value: 'application/vnd.oasis.opendocument.text-web'), // do not localize
  (Key: 'xo'; Value: 'application/vnd.olpc-sugar'), // do not localize
  (Key: 'dd2'; Value: 'application/vnd.oma.dd2+xml'), // do not localize
  (Key: 'oxt'; Value: 'application/vnd.openofficeorg.extension'), // do not localize
  (Key: 'pptx'; Value: 'application/vnd.openxmlformats-officedocument.presentationml.presentation'), // do not localize
  (Key: 'sldx'; Value: 'application/vnd.openxmlformats-officedocument.presentationml.slide'), // do not localize
  (Key: 'ppsx'; Value: 'application/vnd.openxmlformats-officedocument.presentationml.slideshow'), // do not localize
  (Key: 'potx'; Value: 'application/vnd.openxmlformats-officedocument.presentationml.template'), // do not localize
  (Key: 'xlsx'; Value: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'), // do not localize
  (Key: 'xltx'; Value: 'application/vnd.openxmlformats-officedocument.spreadsheetml.template'), // do not localize
  (Key: 'docx'; Value: 'application/vnd.openxmlformats-officedocument.wordprocessingml.document'), // do not localize
  (Key: 'dotx'; Value: 'application/vnd.openxmlformats-officedocument.wordprocessingml.template'), // do not localize
  (Key: 'mgp'; Value: 'application/vnd.osgeo.mapguide.package'), // do not localize
  (Key: 'dp'; Value: 'application/vnd.osgi.dp'), // do not localize
  (Key: 'esa'; Value: 'application/vnd.osgi.subsystem'), // do not localize
  (Key: 'pdb'; Value: 'application/vnd.palm'), // do not localize
  (Key: 'pqa'; Value: 'application/vnd.palm'), // do not localize
  (Key: 'oprc'; Value: 'application/vnd.palm'), // do not localize
  (Key: 'paw'; Value: 'application/vnd.pawaafile'), // do not localize
  (Key: 'str'; Value: 'application/vnd.pg.format'), // do not localize
  (Key: 'ei6'; Value: 'application/vnd.pg.osasli'), // do not localize
  (Key: 'efif'; Value: 'application/vnd.picsel'), // do not localize
  (Key: 'wg'; Value: 'application/vnd.pmi.widget'), // do not localize
  (Key: 'plf'; Value: 'application/vnd.pocketlearn'), // do not localize
  (Key: 'pbd'; Value: 'application/vnd.powerbuilder6'), // do not localize
  (Key: 'box'; Value: 'application/vnd.previewsystems.box'), // do not localize
  (Key: 'mgz'; Value: 'application/vnd.proteus.magazine'), // do not localize
  (Key: 'qps'; Value: 'application/vnd.publishare-delta-tree'), // do not localize
  (Key: 'ptid'; Value: 'application/vnd.pvi.ptid1'), // do not localize
  (Key: 'qxd'; Value: 'application/vnd.quark.quarkxpress'), // do not localize
  (Key: 'qxt'; Value: 'application/vnd.quark.quarkxpress'), // do not localize
  (Key: 'qwd'; Value: 'application/vnd.quark.quarkxpress'), // do not localize
  (Key: 'qwt'; Value: 'application/vnd.quark.quarkxpress'), // do not localize
  (Key: 'qxl'; Value: 'application/vnd.quark.quarkxpress'), // do not localize
  (Key: 'qxb'; Value: 'application/vnd.quark.quarkxpress'), // do not localize
  (Key: 'bed'; Value: 'application/vnd.realvnc.bed'), // do not localize
  (Key: 'mxl'; Value: 'application/vnd.recordare.musicxml'), // do not localize
  (Key: 'musicxml'; Value: 'application/vnd.recordare.musicxml+xml'), // do not localize
  (Key: 'cryptonote'; Value: 'application/vnd.rig.cryptonote'), // do not localize
  (Key: 'cod'; Value: 'application/vnd.rim.cod'), // do not localize
  (Key: 'rm'; Value: 'application/vnd.rn-realmedia'), // do not localize
  (Key: 'rmvb'; Value: 'application/vnd.rn-realmedia-vbr'), // do not localize
  (Key: 'link66'; Value: 'application/vnd.route66.link66+xml'), // do not localize
  (Key: 'st'; Value: 'application/vnd.sailingtracker.track'), // do not localize
  (Key: 'see'; Value: 'application/vnd.seemail'), // do not localize
  (Key: 'sema'; Value: 'application/vnd.sema'), // do not localize
  (Key: 'semd'; Value: 'application/vnd.semd'), // do not localize
  (Key: 'semf'; Value: 'application/vnd.semf'), // do not localize
  (Key: 'ifm'; Value: 'application/vnd.shana.informed.formdata'), // do not localize
  (Key: 'itp'; Value: 'application/vnd.shana.informed.formtemplate'), // do not localize
  (Key: 'iif'; Value: 'application/vnd.shana.informed.interchange'), // do not localize
  (Key: 'ipk'; Value: 'application/vnd.shana.informed.package'), // do not localize
  (Key: 'twd'; Value: 'application/vnd.simtech-mindmapper'), // do not localize
  (Key: 'twds'; Value: 'application/vnd.simtech-mindmapper'), // do not localize
  (Key: 'mmf'; Value: 'application/vnd.smaf'), // do not localize
  (Key: 'teacher'; Value: 'application/vnd.smart.teacher'), // do not localize
  (Key: 'sdkm'; Value: 'application/vnd.solent.sdkm+xml'), // do not localize
  (Key: 'sdkd'; Value: 'application/vnd.solent.sdkm+xml'), // do not localize
  (Key: 'dxp'; Value: 'application/vnd.spotfire.dxp'), // do not localize
  (Key: 'sfs'; Value: 'application/vnd.spotfire.sfs'), // do not localize
  (Key: 'sdc'; Value: 'application/vnd.stardivision.calc'), // do not localize
  (Key: 'sda'; Value: 'application/vnd.stardivision.draw'), // do not localize
  (Key: 'sdd'; Value: 'application/vnd.stardivision.impress'), // do not localize
  (Key: 'smf'; Value: 'application/vnd.stardivision.math'), // do not localize
  (Key: 'sdw'; Value: 'application/vnd.stardivision.writer'), // do not localize
  (Key: 'vor'; Value: 'application/vnd.stardivision.writer'), // do not localize
  (Key: 'sgl'; Value: 'application/vnd.stardivision.writer-global'), // do not localize
  (Key: 'smzip'; Value: 'application/vnd.stepmania.package'), // do not localize
  (Key: 'sm'; Value: 'application/vnd.stepmania.stepchart'), // do not localize
  (Key: 'sxc'; Value: 'application/vnd.sun.xml.calc'), // do not localize
  (Key: 'stc'; Value: 'application/vnd.sun.xml.calc.template'), // do not localize
  (Key: 'sxd'; Value: 'application/vnd.sun.xml.draw'), // do not localize
  (Key: 'std'; Value: 'application/vnd.sun.xml.draw.template'), // do not localize
  (Key: 'sxi'; Value: 'application/vnd.sun.xml.impress'), // do not localize
  (Key: 'sti'; Value: 'application/vnd.sun.xml.impress.template'), // do not localize
  (Key: 'sxm'; Value: 'application/vnd.sun.xml.math'), // do not localize
  (Key: 'sxw'; Value: 'application/vnd.sun.xml.writer'), // do not localize
  (Key: 'sxg'; Value: 'application/vnd.sun.xml.writer.global'), // do not localize
  (Key: 'stw'; Value: 'application/vnd.sun.xml.writer.template'), // do not localize
  (Key: 'sus'; Value: 'application/vnd.sus-calendar'), // do not localize
  (Key: 'susp'; Value: 'application/vnd.sus-calendar'), // do not localize
  (Key: 'svd'; Value: 'application/vnd.svd'), // do not localize
  (Key: 'sis'; Value: 'application/vnd.symbian.install'), // do not localize
  (Key: 'sisx'; Value: 'application/vnd.symbian.install'), // do not localize
  (Key: 'xsm'; Value: 'application/vnd.syncml+xml'), // do not localize
  (Key: 'bdm'; Value: 'application/vnd.syncml.dm+wbxml'), // do not localize
  (Key: 'xdm'; Value: 'application/vnd.syncml.dm+xml'), // do not localize
  (Key: 'tao'; Value: 'application/vnd.tao.intent-module-archive'), // do not localize
  (Key: 'pcap'; Value: 'application/vnd.tcpdump.pcap'), // do not localize
  (Key: 'cap'; Value: 'application/vnd.tcpdump.pcap'), // do not localize
  (Key: 'dmp'; Value: 'application/vnd.tcpdump.pcap'), // do not localize
  (Key: 'tmo'; Value: 'application/vnd.tmobile-livetv'), // do not localize
  (Key: 'tpt'; Value: 'application/vnd.trid.tpt'), // do not localize
  (Key: 'mxs'; Value: 'application/vnd.triscape.mxs'), // do not localize
  (Key: 'tra'; Value: 'application/vnd.trueapp'), // do not localize
  (Key: 'ufd'; Value: 'application/vnd.ufdl'), // do not localize
  (Key: 'ufdl'; Value: 'application/vnd.ufdl'), // do not localize
  (Key: 'utz'; Value: 'application/vnd.uiq.theme'), // do not localize
  (Key: 'umj'; Value: 'application/vnd.umajin'), // do not localize
  (Key: 'unityweb'; Value: 'application/vnd.unity'), // do not localize
  (Key: 'uoml'; Value: 'application/vnd.uoml+xml'), // do not localize
  (Key: 'vcx'; Value: 'application/vnd.vcx'), // do not localize
  (Key: 'vsd'; Value: 'application/vnd.visio'), // do not localize
  (Key: 'vst'; Value: 'application/vnd.visio'), // do not localize
  (Key: 'vss'; Value: 'application/vnd.visio'), // do not localize
  (Key: 'vsw'; Value: 'application/vnd.visio'), // do not localize
  (Key: 'vis'; Value: 'application/vnd.visionary'), // do not localize
  (Key: 'vsf'; Value: 'application/vnd.vsf'), // do not localize
  (Key: 'wbxml'; Value: 'application/vnd.wap.wbxml'), // do not localize
  (Key: 'wmlc'; Value: 'application/vnd.wap.wmlc'), // do not localize
  (Key: 'wmlsc'; Value: 'application/vnd.wap.wmlscriptc'), // do not localize
  (Key: 'wtb'; Value: 'application/vnd.webturbo'), // do not localize
  (Key: 'nbp'; Value: 'application/vnd.wolfram.player'), // do not localize
  (Key: 'wpd'; Value: 'application/vnd.wordperfect'), // do not localize
  (Key: 'wqd'; Value: 'application/vnd.wqd'), // do not localize
  (Key: 'stf'; Value: 'application/vnd.wt.stf'), // do not localize
  (Key: 'xar'; Value: 'application/vnd.xara'), // do not localize
  (Key: 'xfdl'; Value: 'application/vnd.xfdl'), // do not localize
  (Key: 'hvd'; Value: 'application/vnd.yamaha.hv-dic'), // do not localize
  (Key: 'hvs'; Value: 'application/vnd.yamaha.hv-script'), // do not localize
  (Key: 'hvp'; Value: 'application/vnd.yamaha.hv-voice'), // do not localize
  (Key: 'osf'; Value: 'application/vnd.yamaha.openscoreformat'), // do not localize
  (Key: 'osfpvg'; Value: 'application/vnd.yamaha.openscoreformat.osfpvg+xml'), // do not localize
  (Key: 'saf'; Value: 'application/vnd.yamaha.smaf-audio'), // do not localize
  (Key: 'spf'; Value: 'application/vnd.yamaha.smaf-phrase'), // do not localize
  (Key: 'cmp'; Value: 'application/vnd.yellowriver-custom-menu'), // do not localize
  (Key: 'zir'; Value: 'application/vnd.zul'), // do not localize
  (Key: 'zirz'; Value: 'application/vnd.zul'), // do not localize
  (Key: 'zaz'; Value: 'application/vnd.zzazz.deck+xml'), // do not localize
  (Key: 'vxml'; Value: 'application/voicexml+xml'), // do not localize
  (Key: 'wgt'; Value: 'application/widget'), // do not localize
  (Key: 'hlp'; Value: 'application/winhlp'), // do not localize
  (Key: 'wsdl'; Value: 'application/wsdl+xml'), // do not localize
  (Key: 'wspolicy'; Value: 'application/wspolicy+xml'), // do not localize
  (Key: '7z'; Value: 'application/x-7z-compressed'), // do not localize
  (Key: 'abw'; Value: 'application/x-abiword'), // do not localize
  (Key: 'ace'; Value: 'application/x-ace-compressed'), // do not localize
  (Key: 'dmg'; Value: 'application/x-apple-diskimage'), // do not localize
  (Key: 'aab'; Value: 'application/x-authorware-bin'), // do not localize
  (Key: 'x32'; Value: 'application/x-authorware-bin'), // do not localize
  (Key: 'u32'; Value: 'application/x-authorware-bin'), // do not localize
  (Key: 'vox'; Value: 'application/x-authorware-bin'), // do not localize
  (Key: 'aam'; Value: 'application/x-authorware-map'), // do not localize
  (Key: 'aas'; Value: 'application/x-authorware-seg'), // do not localize
  (Key: 'bcpio'; Value: 'application/x-bcpio'), // do not localize
  (Key: 'torrent'; Value: 'application/x-bittorrent'), // do not localize
  (Key: 'blb'; Value: 'application/x-blorb'), // do not localize
  (Key: 'blorb'; Value: 'application/x-blorb'), // do not localize
  (Key: 'bz'; Value: 'application/x-bzip'), // do not localize
  (Key: 'bz2'; Value: 'application/x-bzip2'), // do not localize
  (Key: 'boz'; Value: 'application/x-bzip2'), // do not localize
  (Key: 'cbr'; Value: 'application/x-cbr'), // do not localize
  (Key: 'cba'; Value: 'application/x-cbr'), // do not localize
  (Key: 'cbt'; Value: 'application/x-cbr'), // do not localize
  (Key: 'cbz'; Value: 'application/x-cbr'), // do not localize
  (Key: 'cb7'; Value: 'application/x-cbr'), // do not localize
  (Key: 'vcd'; Value: 'application/x-cdlink'), // do not localize
  (Key: 'cfs'; Value: 'application/x-cfs-compressed'), // do not localize
  (Key: 'chat'; Value: 'application/x-chat'), // do not localize
  (Key: 'pgn'; Value: 'application/x-chess-pgn'), // do not localize
  (Key: 'nsc'; Value: 'application/x-conference'), // do not localize
  (Key: 'cpio'; Value: 'application/x-cpio'), // do not localize
  (Key: 'csh'; Value: 'application/x-csh'), // do not localize
  (Key: 'deb'; Value: 'application/x-debian-package'), // do not localize
  (Key: 'udeb'; Value: 'application/x-debian-package'), // do not localize
  (Key: 'dgc'; Value: 'application/x-dgc-compressed'), // do not localize
  (Key: 'dir'; Value: 'application/x-director'), // do not localize
  (Key: 'dcr'; Value: 'application/x-director'), // do not localize
  (Key: 'dxr'; Value: 'application/x-director'), // do not localize
  (Key: 'cst'; Value: 'application/x-director'), // do not localize
  (Key: 'cct'; Value: 'application/x-director'), // do not localize
  (Key: 'cxt'; Value: 'application/x-director'), // do not localize
  (Key: 'w3d'; Value: 'application/x-director'), // do not localize
  (Key: 'fgd'; Value: 'application/x-director'), // do not localize
  (Key: 'swa'; Value: 'application/x-director'), // do not localize
  (Key: 'wad'; Value: 'application/x-doom'), // do not localize
  (Key: 'ncx'; Value: 'application/x-dtbncx+xml'), // do not localize
  (Key: 'dtb'; Value: 'application/x-dtbook+xml'), // do not localize
  (Key: 'res'; Value: 'application/x-dtbresource+xml'), // do not localize
  (Key: 'dvi'; Value: 'application/x-dvi'), // do not localize
  (Key: 'evy'; Value: 'application/x-envoy'), // do not localize
  (Key: 'eva'; Value: 'application/x-eva'), // do not localize
  (Key: 'bdf'; Value: 'application/x-font-bdf'), // do not localize
  (Key: 'gsf'; Value: 'application/x-font-ghostscript'), // do not localize
  (Key: 'psf'; Value: 'application/x-font-linux-psf'), // do not localize
  (Key: 'otf'; Value: 'application/x-font-otf'), // do not localize
  (Key: 'pcf'; Value: 'application/x-font-pcf'), // do not localize
  (Key: 'snf'; Value: 'application/x-font-snf'), // do not localize
  (Key: 'ttf'; Value: 'application/x-font-ttf'), // do not localize
  (Key: 'ttc'; Value: 'application/x-font-ttf'), // do not localize
  (Key: 'pfa'; Value: 'application/x-font-type1'), // do not localize
  (Key: 'pfb'; Value: 'application/x-font-type1'), // do not localize
  (Key: 'pfm'; Value: 'application/x-font-type1'), // do not localize
  (Key: 'afm'; Value: 'application/x-font-type1'), // do not localize
  (Key: 'woff'; Value: 'application/x-font-woff'), // do not localize
  (Key: 'arc'; Value: 'application/x-freearc'), // do not localize
  (Key: 'spl'; Value: 'application/x-futuresplash'), // do not localize
  (Key: 'gca'; Value: 'application/x-gca-compressed'), // do not localize
  (Key: 'ulx'; Value: 'application/x-glulx'), // do not localize
  (Key: 'gnumeric'; Value: 'application/x-gnumeric'), // do not localize
  (Key: 'gramps'; Value: 'application/x-gramps-xml'), // do not localize
  (Key: 'gtar'; Value: 'application/x-gtar'), // do not localize
  (Key: 'hdf'; Value: 'application/x-hdf'), // do not localize
  (Key: 'install'; Value: 'application/x-install-instructions'), // do not localize
  (Key: 'iso'; Value: 'application/x-iso9660-image'), // do not localize
  (Key: 'jnlp'; Value: 'application/x-java-jnlp-file'), // do not localize
  (Key: 'latex'; Value: 'application/x-latex'), // do not localize
  (Key: 'lzh'; Value: 'application/x-lzh-compressed'), // do not localize
  (Key: 'lha'; Value: 'application/x-lzh-compressed'), // do not localize
  (Key: 'mie'; Value: 'application/x-mie'), // do not localize
  (Key: 'prc'; Value: 'application/x-mobipocket-ebook'), // do not localize
  (Key: 'mobi'; Value: 'application/x-mobipocket-ebook'), // do not localize
  (Key: 'application'; Value: 'application/x-ms-application'), // do not localize
  (Key: 'lnk'; Value: 'application/x-ms-shortcut'), // do not localize
  (Key: 'wmd'; Value: 'application/x-ms-wmd'), // do not localize
  (Key: 'wmz'; Value: 'application/x-ms-wmz'), // do not localize
  (Key: 'xbap'; Value: 'application/x-ms-xbap'), // do not localize
  (Key: 'mdb'; Value: 'application/x-msaccess'), // do not localize
  (Key: 'obd'; Value: 'application/x-msbinder'), // do not localize
  (Key: 'crd'; Value: 'application/x-mscardfile'), // do not localize
  (Key: 'clp'; Value: 'application/x-msclip'), // do not localize
  (Key: 'exe'; Value: 'application/x-msdownload'), // do not localize
  (Key: 'dll'; Value: 'application/x-msdownload'), // do not localize
  (Key: 'com'; Value: 'application/x-msdownload'), // do not localize
  (Key: 'bat'; Value: 'application/x-msdownload'), // do not localize
  (Key: 'msi'; Value: 'application/x-msdownload'), // do not localize
  (Key: 'mvb'; Value: 'application/x-msmediaview'), // do not localize
  (Key: 'm13'; Value: 'application/x-msmediaview'), // do not localize
  (Key: 'm14'; Value: 'application/x-msmediaview'), // do not localize
  (Key: 'wmf'; Value: 'application/x-msmetafile'), // do not localize
  (Key: 'wmz'; Value: 'application/x-msmetafile'), // do not localize
  (Key: 'emf'; Value: 'application/x-msmetafile'), // do not localize
  (Key: 'emz'; Value: 'application/x-msmetafile'), // do not localize
  (Key: 'mny'; Value: 'application/x-msmoney'), // do not localize
  (Key: 'pub'; Value: 'application/x-mspublisher'), // do not localize
  (Key: 'scd'; Value: 'application/x-msschedule'), // do not localize
  (Key: 'trm'; Value: 'application/x-msterminal'), // do not localize
  (Key: 'wri'; Value: 'application/x-mswrite'), // do not localize
  (Key: 'nc'; Value: 'application/x-netcdf'), // do not localize
  (Key: 'cdf'; Value: 'application/x-netcdf'), // do not localize
  (Key: 'nzb'; Value: 'application/x-nzb'), // do not localize
  (Key: 'p12'; Value: 'application/x-pkcs12'), // do not localize
  (Key: 'pfx'; Value: 'application/x-pkcs12'), // do not localize
  (Key: 'p7b'; Value: 'application/x-pkcs7-certificates'), // do not localize
  (Key: 'spc'; Value: 'application/x-pkcs7-certificates'), // do not localize
  (Key: 'p7r'; Value: 'application/x-pkcs7-certreqresp'), // do not localize
  (Key: 'rar'; Value: 'application/x-rar-compressed'), // do not localize
  (Key: 'ris'; Value: 'application/x-research-info-systems'), // do not localize
  (Key: 'sh'; Value: 'application/x-sh'), // do not localize
  (Key: 'shar'; Value: 'application/x-shar'), // do not localize
  (Key: 'swf'; Value: 'application/x-shockwave-flash'), // do not localize
  (Key: 'xap'; Value: 'application/x-silverlight-app'), // do not localize
  (Key: 'sql'; Value: 'application/x-sql'), // do not localize
  (Key: 'sit'; Value: 'application/x-stuffit'), // do not localize
  (Key: 'sitx'; Value: 'application/x-stuffitx'), // do not localize
  (Key: 'srt'; Value: 'application/x-subrip'), // do not localize
  (Key: 'sv4cpio'; Value: 'application/x-sv4cpio'), // do not localize
  (Key: 'sv4crc'; Value: 'application/x-sv4crc'), // do not localize
  (Key: 't3'; Value: 'application/x-t3vm-image'), // do not localize
  (Key: 'gam'; Value: 'application/x-tads'), // do not localize
  (Key: 'tar'; Value: 'application/x-tar'), // do not localize
  (Key: 'tcl'; Value: 'application/x-tcl'), // do not localize
  (Key: 'tex'; Value: 'application/x-tex'), // do not localize
  (Key: 'tfm'; Value: 'application/x-tex-tfm'), // do not localize
  (Key: 'texinfo'; Value: 'application/x-texinfo'), // do not localize
  (Key: 'texi'; Value: 'application/x-texinfo'), // do not localize
  (Key: 'obj'; Value: 'application/x-tgif'), // do not localize
  (Key: 'ustar'; Value: 'application/x-ustar'), // do not localize
  (Key: 'src'; Value: 'application/x-wais-source'), // do not localize
  (Key: 'der'; Value: 'application/x-x509-ca-cert'), // do not localize
  (Key: 'crt'; Value: 'application/x-x509-ca-cert'), // do not localize
  (Key: 'fig'; Value: 'application/x-xfig'), // do not localize
  (Key: 'xlf'; Value: 'application/x-xliff+xml'), // do not localize
  (Key: 'xpi'; Value: 'application/x-xpinstall'), // do not localize
  (Key: 'xz'; Value: 'application/x-xz'), // do not localize
  (Key: 'z1'; Value: 'application/x-zmachine'), // do not localize
  (Key: 'z2'; Value: 'application/x-zmachine'), // do not localize
  (Key: 'z3'; Value: 'application/x-zmachine'), // do not localize
  (Key: 'z4'; Value: 'application/x-zmachine'), // do not localize
  (Key: 'z5'; Value: 'application/x-zmachine'), // do not localize
  (Key: 'z6'; Value: 'application/x-zmachine'), // do not localize
  (Key: 'z7'; Value: 'application/x-zmachine'), // do not localize
  (Key: 'z8'; Value: 'application/x-zmachine'), // do not localize
  (Key: 'xaml'; Value: 'application/xaml+xml'), // do not localize
  (Key: 'xdf'; Value: 'application/xcap-diff+xml'), // do not localize
  (Key: 'xenc'; Value: 'application/xenc+xml'), // do not localize
  (Key: 'xhtml'; Value: 'application/xhtml+xml'), // do not localize
  (Key: 'xht'; Value: 'application/xhtml+xml'), // do not localize
  (Key: 'xml'; Value: 'application/xml'), // do not localize
  (Key: 'xsl'; Value: 'application/xml'), // do not localize
  (Key: 'dtd'; Value: 'application/xml-dtd'), // do not localize
  (Key: 'xop'; Value: 'application/xop+xml'), // do not localize
  (Key: 'xpl'; Value: 'application/xproc+xml'), // do not localize
  (Key: 'xslt'; Value: 'application/xslt+xml'), // do not localize
  (Key: 'xspf'; Value: 'application/xspf+xml'), // do not localize
  (Key: 'mxml'; Value: 'application/xv+xml'), // do not localize
  (Key: 'xhvml'; Value: 'application/xv+xml'), // do not localize
  (Key: 'xvml'; Value: 'application/xv+xml'), // do not localize
  (Key: 'xvm'; Value: 'application/xv+xml'), // do not localize
  (Key: 'yang'; Value: 'application/yang'), // do not localize
  (Key: 'yin'; Value: 'application/yin+xml'), // do not localize
  (Key: 'zip'; Value: 'application/zip'), // do not localize
  (Key: 'adp'; Value: 'audio/adpcm'), // do not localize
  (Key: 'au'; Value: 'audio/basic'), // do not localize
  (Key: 'snd'; Value: 'audio/basic'), // do not localize
  (Key: 'mid'; Value: 'audio/midi'), // do not localize
  (Key: 'midi'; Value: 'audio/midi'), // do not localize
  (Key: 'kar'; Value: 'audio/midi'), // do not localize
  (Key: 'rmi'; Value: 'audio/midi'), // do not localize
  (Key: 'mp4a'; Value: 'audio/mp4'), // do not localize
  (Key: 'mpga'; Value: 'audio/mpeg'), // do not localize
  (Key: 'mp2'; Value: 'audio/mpeg'), // do not localize
  (Key: 'mp2a'; Value: 'audio/mpeg'), // do not localize
  (Key: 'mp3'; Value: 'audio/mpeg'), // do not localize
  (Key: 'm2a'; Value: 'audio/mpeg'), // do not localize
  (Key: 'm3a'; Value: 'audio/mpeg'), // do not localize
  (Key: 'oga'; Value: 'audio/ogg'), // do not localize
  (Key: 'ogg'; Value: 'audio/ogg'), // do not localize
  (Key: 'spx'; Value: 'audio/ogg'), // do not localize
  (Key: 's3m'; Value: 'audio/s3m'), // do not localize
  (Key: 'sil'; Value: 'audio/silk'), // do not localize
  (Key: 'uva'; Value: 'audio/vnd.dece.audio'), // do not localize
  (Key: 'uvva'; Value: 'audio/vnd.dece.audio'), // do not localize
  (Key: 'eol'; Value: 'audio/vnd.digital-winds'), // do not localize
  (Key: 'dra'; Value: 'audio/vnd.dra'), // do not localize
  (Key: 'dts'; Value: 'audio/vnd.dts'), // do not localize
  (Key: 'dtshd'; Value: 'audio/vnd.dts.hd'), // do not localize
  (Key: 'lvp'; Value: 'audio/vnd.lucent.voice'), // do not localize
  (Key: 'pya'; Value: 'audio/vnd.ms-playready.media.pya'), // do not localize
  (Key: 'ecelp4800'; Value: 'audio/vnd.nuera.ecelp4800'), // do not localize
  (Key: 'ecelp7470'; Value: 'audio/vnd.nuera.ecelp7470'), // do not localize
  (Key: 'ecelp9600'; Value: 'audio/vnd.nuera.ecelp9600'), // do not localize
  (Key: 'rip'; Value: 'audio/vnd.rip'), // do not localize
  (Key: 'weba'; Value: 'audio/webm'), // do not localize
  (Key: 'aac'; Value: 'audio/x-aac'), // do not localize
  (Key: 'aif'; Value: 'audio/x-aiff'), // do not localize
  (Key: 'aiff'; Value: 'audio/x-aiff'), // do not localize
  (Key: 'aifc'; Value: 'audio/x-aiff'), // do not localize
  (Key: 'caf'; Value: 'audio/x-caf'), // do not localize
  (Key: 'flac'; Value: 'audio/x-flac'), // do not localize
  (Key: 'mka'; Value: 'audio/x-matroska'), // do not localize
  (Key: 'm3u'; Value: 'audio/x-mpegurl'), // do not localize
  (Key: 'wax'; Value: 'audio/x-ms-wax'), // do not localize
  (Key: 'wma'; Value: 'audio/x-ms-wma'), // do not localize
  (Key: 'ram'; Value: 'audio/x-pn-realaudio'), // do not localize
  (Key: 'ra'; Value: 'audio/x-pn-realaudio'), // do not localize
  (Key: 'rmp'; Value: 'audio/x-pn-realaudio-plugin'), // do not localize
  (Key: 'wav'; Value: 'audio/x-wav'), // do not localize
  (Key: 'xm'; Value: 'audio/xm'), // do not localize
  (Key: 'cdx'; Value: 'chemical/x-cdx'), // do not localize
  (Key: 'cif'; Value: 'chemical/x-cif'), // do not localize
  (Key: 'cmdf'; Value: 'chemical/x-cmdf'), // do not localize
  (Key: 'cml'; Value: 'chemical/x-cml'), // do not localize
  (Key: 'csml'; Value: 'chemical/x-csml'), // do not localize
  (Key: 'xyz'; Value: 'chemical/x-xyz'), // do not localize
  (Key: 'bmp'; Value: 'image/bmp'), // do not localize
  (Key: 'cgm'; Value: 'image/cgm'), // do not localize
  (Key: 'g3'; Value: 'image/g3fax'), // do not localize
  (Key: 'gif'; Value: 'image/gif'), // do not localize
  (Key: 'ief'; Value: 'image/ief'), // do not localize
  (Key: 'jpeg'; Value: 'image/jpeg'), // do not localize
  (Key: 'jpg'; Value: 'image/jpeg'), // do not localize
  (Key: 'jpe'; Value: 'image/jpeg'), // do not localize
  (Key: 'ktx'; Value: 'image/ktx'), // do not localize
  (Key: 'png'; Value: 'image/png'), // do not localize
  (Key: 'btif'; Value: 'image/prs.btif'), // do not localize
  (Key: 'sgi'; Value: 'image/sgi'), // do not localize
  (Key: 'svg'; Value: 'image/svg+xml'), // do not localize
  (Key: 'svgz'; Value: 'image/svg+xml'), // do not localize
  (Key: 'tiff'; Value: 'image/tiff'), // do not localize
  (Key: 'tif'; Value: 'image/tiff'), // do not localize
  (Key: 'psd'; Value: 'image/vnd.adobe.photoshop'), // do not localize
  (Key: 'uvi'; Value: 'image/vnd.dece.graphic'), // do not localize
  (Key: 'uvvi'; Value: 'image/vnd.dece.graphic'), // do not localize
  (Key: 'uvg'; Value: 'image/vnd.dece.graphic'), // do not localize
  (Key: 'uvvg'; Value: 'image/vnd.dece.graphic'), // do not localize
  (Key: 'sub'; Value: 'image/vnd.dvb.subtitle'), // do not localize
  (Key: 'djvu'; Value: 'image/vnd.djvu'), // do not localize
  (Key: 'djv'; Value: 'image/vnd.djvu'), // do not localize
  (Key: 'dwg'; Value: 'image/vnd.dwg'), // do not localize
  (Key: 'dxf'; Value: 'image/vnd.dxf'), // do not localize
  (Key: 'fbs'; Value: 'image/vnd.fastbidsheet'), // do not localize
  (Key: 'fpx'; Value: 'image/vnd.fpx'), // do not localize
  (Key: 'fst'; Value: 'image/vnd.fst'), // do not localize
  (Key: 'mmr'; Value: 'image/vnd.fujixerox.edmics-mmr'), // do not localize
  (Key: 'rlc'; Value: 'image/vnd.fujixerox.edmics-rlc'), // do not localize
  (Key: 'mdi'; Value: 'image/vnd.ms-modi'), // do not localize
  (Key: 'wdp'; Value: 'image/vnd.ms-photo'), // do not localize
  (Key: 'npx'; Value: 'image/vnd.net-fpx'), // do not localize
  (Key: 'wbmp'; Value: 'image/vnd.wap.wbmp'), // do not localize
  (Key: 'xif'; Value: 'image/vnd.xiff'), // do not localize
  (Key: 'webp'; Value: 'image/webp'), // do not localize
  (Key: '3ds'; Value: 'image/x-3ds'), // do not localize
  (Key: 'ras'; Value: 'image/x-cmu-raster'), // do not localize
  (Key: 'cmx'; Value: 'image/x-cmx'), // do not localize
  (Key: 'fh'; Value: 'image/x-freehand'), // do not localize
  (Key: 'fhc'; Value: 'image/x-freehand'), // do not localize
  (Key: 'fh4'; Value: 'image/x-freehand'), // do not localize
  (Key: 'fh5'; Value: 'image/x-freehand'), // do not localize
  (Key: 'fh7'; Value: 'image/x-freehand'), // do not localize
  (Key: 'ico'; Value: 'image/x-icon'), // do not localize
  (Key: 'sid'; Value: 'image/x-mrsid-image'), // do not localize
  (Key: 'pcx'; Value: 'image/x-pcx'), // do not localize
  (Key: 'pic'; Value: 'image/x-pict'), // do not localize
  (Key: 'pct'; Value: 'image/x-pict'), // do not localize
  (Key: 'pnm'; Value: 'image/x-portable-anymap'), // do not localize
  (Key: 'pbm'; Value: 'image/x-portable-bitmap'), // do not localize
  (Key: 'pgm'; Value: 'image/x-portable-graymap'), // do not localize
  (Key: 'ppm'; Value: 'image/x-portable-pixmap'), // do not localize
  (Key: 'rgb'; Value: 'image/x-rgb'), // do not localize
  (Key: 'tga'; Value: 'image/x-tga'), // do not localize
  (Key: 'xbm'; Value: 'image/x-xbitmap'), // do not localize
  (Key: 'xpm'; Value: 'image/x-xpixmap'), // do not localize
  (Key: 'xwd'; Value: 'image/x-xwindowdump'), // do not localize
  (Key: 'eml'; Value: 'message/rfc822'), // do not localize
  (Key: 'mime'; Value: 'message/rfc822'), // do not localize
  (Key: 'igs'; Value: 'model/iges'), // do not localize
  (Key: 'iges'; Value: 'model/iges'), // do not localize
  (Key: 'msh'; Value: 'model/mesh'), // do not localize
  (Key: 'mesh'; Value: 'model/mesh'), // do not localize
  (Key: 'silo'; Value: 'model/mesh'), // do not localize
  (Key: 'dae'; Value: 'model/vnd.collada+xml'), // do not localize
  (Key: 'dwf'; Value: 'model/vnd.dwf'), // do not localize
  (Key: 'gdl'; Value: 'model/vnd.gdl'), // do not localize
  (Key: 'gtw'; Value: 'model/vnd.gtw'), // do not localize
  (Key: 'mts'; Value: 'model/vnd.mts'), // do not localize
  (Key: 'vtu'; Value: 'model/vnd.vtu'), // do not localize
  (Key: 'wrl'; Value: 'model/vrml'), // do not localize
  (Key: 'vrml'; Value: 'model/vrml'), // do not localize
  (Key: 'x3db'; Value: 'model/x3d+binary'), // do not localize
  (Key: 'x3dbz'; Value: 'model/x3d+binary'), // do not localize
  (Key: 'x3dv'; Value: 'model/x3d+vrml'), // do not localize
  (Key: 'x3dvz'; Value: 'model/x3d+vrml'), // do not localize
  (Key: 'x3d'; Value: 'model/x3d+xml'), // do not localize
  (Key: 'x3dz'; Value: 'model/x3d+xml'), // do not localize
  (Key: 'appcache'; Value: 'text/cache-manifest'), // do not localize
  (Key: 'ics'; Value: 'text/calendar'), // do not localize
  (Key: 'ifb'; Value: 'text/calendar'), // do not localize
  (Key: 'css'; Value: 'text/css'), // do not localize
  (Key: 'csv'; Value: 'text/csv'), // do not localize
  (Key: 'html'; Value: 'text/html'), // do not localize
  (Key: 'htm'; Value: 'text/html'), // do not localize
  (Key: 'n3'; Value: 'text/n3'), // do not localize
  (Key: 'txt'; Value: 'text/plain'), // do not localize
  (Key: 'text'; Value: 'text/plain'), // do not localize
  (Key: 'conf'; Value: 'text/plain'), // do not localize
  (Key: 'def'; Value: 'text/plain'), // do not localize
  (Key: 'list'; Value: 'text/plain'), // do not localize
  (Key: 'log'; Value: 'text/plain'), // do not localize
  (Key: 'in'; Value: 'text/plain'), // do not localize
  (Key: 'dsc'; Value: 'text/prs.lines.tag'), // do not localize
  (Key: 'rtx'; Value: 'text/richtext'), // do not localize
  (Key: 'sgml'; Value: 'text/sgml'), // do not localize
  (Key: 'sgm'; Value: 'text/sgml'), // do not localize
  (Key: 'tsv'; Value: 'text/tab-separated-values'), // do not localize
  (Key: 't'; Value: 'text/troff'), // do not localize
  (Key: 'tr'; Value: 'text/troff'), // do not localize
  (Key: 'roff'; Value: 'text/troff'), // do not localize
  (Key: 'man'; Value: 'text/troff'), // do not localize
  (Key: 'me'; Value: 'text/troff'), // do not localize
  (Key: 'ms'; Value: 'text/troff'), // do not localize
  (Key: 'ttl'; Value: 'text/turtle'), // do not localize
  (Key: 'uri'; Value: 'text/uri-list'), // do not localize
  (Key: 'uris'; Value: 'text/uri-list'), // do not localize
  (Key: 'urls'; Value: 'text/uri-list'), // do not localize
  (Key: 'vcard'; Value: 'text/vcard'), // do not localize
  (Key: 'curl'; Value: 'text/vnd.curl'), // do not localize
  (Key: 'dcurl'; Value: 'text/vnd.curl.dcurl'), // do not localize
  (Key: 'scurl'; Value: 'text/vnd.curl.scurl'), // do not localize
  (Key: 'mcurl'; Value: 'text/vnd.curl.mcurl'), // do not localize
  (Key: 'sub'; Value: 'text/vnd.dvb.subtitle'), // do not localize
  (Key: 'fly'; Value: 'text/vnd.fly'), // do not localize
  (Key: 'flx'; Value: 'text/vnd.fmi.flexstor'), // do not localize
  (Key: 'gv'; Value: 'text/vnd.graphviz'), // do not localize
  (Key: '3dml'; Value: 'text/vnd.in3d.3dml'), // do not localize
  (Key: 'spot'; Value: 'text/vnd.in3d.spot'), // do not localize
  (Key: 'jad'; Value: 'text/vnd.sun.j2me.app-descriptor'), // do not localize
  (Key: 'wml'; Value: 'text/vnd.wap.wml'), // do not localize
  (Key: 'wmls'; Value: 'text/vnd.wap.wmlscript'), // do not localize
  (Key: 's'; Value: 'text/x-asm'), // do not localize
  (Key: 'asm'; Value: 'text/x-asm'), // do not localize
  (Key: 'c'; Value: 'text/x-c'), // do not localize
  (Key: 'cc'; Value: 'text/x-c'), // do not localize
  (Key: 'cxx'; Value: 'text/x-c'), // do not localize
  (Key: 'cpp'; Value: 'text/x-c'), // do not localize
  (Key: 'h'; Value: 'text/x-c'), // do not localize
  (Key: 'hh'; Value: 'text/x-c'), // do not localize
  (Key: 'dic'; Value: 'text/x-c'), // do not localize
  (Key: 'f'; Value: 'text/x-fortran'), // do not localize
  (Key: 'for'; Value: 'text/x-fortran'), // do not localize
  (Key: 'f77'; Value: 'text/x-fortran'), // do not localize
  (Key: 'f90'; Value: 'text/x-fortran'), // do not localize
  (Key: 'java'; Value: 'text/x-java-source'), // do not localize
  (Key: 'opml'; Value: 'text/x-opml'), // do not localize
  (Key: 'p'; Value: 'text/x-pascal'), // do not localize
  (Key: 'pas'; Value: 'text/x-pascal'), // do not localize
  (Key: 'nfo'; Value: 'text/x-nfo'), // do not localize
  (Key: 'etx'; Value: 'text/x-setext'), // do not localize
  (Key: 'sfv'; Value: 'text/x-sfv'), // do not localize
  (Key: 'uu'; Value: 'text/x-uuencode'), // do not localize
  (Key: 'vcs'; Value: 'text/x-vcalendar'), // do not localize
  (Key: 'vcf'; Value: 'text/x-vcard'), // do not localize
  (Key: '3gp'; Value: 'video/3gpp'), // do not localize
  (Key: '3g2'; Value: 'video/3gpp2'), // do not localize
  (Key: 'h261'; Value: 'video/h261'), // do not localize
  (Key: 'h263'; Value: 'video/h263'), // do not localize
  (Key: 'h264'; Value: 'video/h264'), // do not localize
  (Key: 'jpgv'; Value: 'video/jpeg'), // do not localize
  (Key: 'jpm'; Value: 'video/jpm'), // do not localize
  (Key: 'jpgm'; Value: 'video/jpm'), // do not localize
  (Key: 'mj2'; Value: 'video/mj2'), // do not localize
  (Key: 'mjp2'; Value: 'video/mj2'), // do not localize
  (Key: 'mp4'; Value: 'video/mp4'), // do not localize
  (Key: 'mp4v'; Value: 'video/mp4'), // do not localize
  (Key: 'mpg4'; Value: 'video/mp4'), // do not localize
  (Key: 'mpeg'; Value: 'video/mpeg'), // do not localize
  (Key: 'mpg'; Value: 'video/mpeg'), // do not localize
  (Key: 'mpe'; Value: 'video/mpeg'), // do not localize
  (Key: 'm1v'; Value: 'video/mpeg'), // do not localize
  (Key: 'm2v'; Value: 'video/mpeg'), // do not localize
  (Key: 'ogv'; Value: 'video/ogg'), // do not localize
  (Key: 'qt'; Value: 'video/quicktime'), // do not localize
  (Key: 'mov'; Value: 'video/quicktime'), // do not localize
  (Key: 'uvh'; Value: 'video/vnd.dece.hd'), // do not localize
  (Key: 'uvvh'; Value: 'video/vnd.dece.hd'), // do not localize
  (Key: 'uvm'; Value: 'video/vnd.dece.mobile'), // do not localize
  (Key: 'uvvm'; Value: 'video/vnd.dece.mobile'), // do not localize
  (Key: 'uvp'; Value: 'video/vnd.dece.pd'), // do not localize
  (Key: 'uvvp'; Value: 'video/vnd.dece.pd'), // do not localize
  (Key: 'uvs'; Value: 'video/vnd.dece.sd'), // do not localize
  (Key: 'uvvs'; Value: 'video/vnd.dece.sd'), // do not localize
  (Key: 'uvv'; Value: 'video/vnd.dece.video'), // do not localize
  (Key: 'uvvv'; Value: 'video/vnd.dece.video'), // do not localize
  (Key: 'dvb'; Value: 'video/vnd.dvb.file'), // do not localize
  (Key: 'fvt'; Value: 'video/vnd.fvt'), // do not localize
  (Key: 'mxu'; Value: 'video/vnd.mpegurl'), // do not localize
  (Key: 'm4u'; Value: 'video/vnd.mpegurl'), // do not localize
  (Key: 'pyv'; Value: 'video/vnd.ms-playready.media.pyv'), // do not localize
  (Key: 'uvu'; Value: 'video/vnd.uvvu.mp4'), // do not localize
  (Key: 'uvvu'; Value: 'video/vnd.uvvu.mp4'), // do not localize
  (Key: 'viv'; Value: 'video/vnd.vivo'), // do not localize
  (Key: 'webm'; Value: 'video/webm'), // do not localize
  (Key: 'f4v'; Value: 'video/x-f4v'), // do not localize
  (Key: 'fli'; Value: 'video/x-fli'), // do not localize
  (Key: 'flv'; Value: 'video/x-flv'), // do not localize
  (Key: 'm4v'; Value: 'video/x-m4v'), // do not localize
  (Key: 'mkv'; Value: 'video/x-matroska'), // do not localize
  (Key: 'mk3d'; Value: 'video/x-matroska'), // do not localize
  (Key: 'mks'; Value: 'video/x-matroska'), // do not localize
  (Key: 'mng'; Value: 'video/x-mng'), // do not localize
  (Key: 'asf'; Value: 'video/x-ms-asf'), // do not localize
  (Key: 'asx'; Value: 'video/x-ms-asf'), // do not localize
  (Key: 'vob'; Value: 'video/x-ms-vob'), // do not localize
  (Key: 'wm'; Value: 'video/x-ms-wm'), // do not localize
  (Key: 'wmv'; Value: 'video/x-ms-wmv'), // do not localize
  (Key: 'wmx'; Value: 'video/x-ms-wmx'), // do not localize
  (Key: 'wvx'; Value: 'video/x-ms-wvx'), // do not localize
  (Key: 'avi'; Value: 'video/x-msvideo'), // do not localize
  (Key: 'movie'; Value: 'video/x-sgi-movie'), // do not localize
  (Key: 'smv'; Value: 'video/x-smv'), // do not localize
  (Key: 'ice'; Value: 'x-conference/x-cooltalk') );  // do not localize
{$ENDREGION}

procedure InitMimeMap();
var
  I: Integer;
begin
  MimeMap := TStringHash.Create(2099);
  for I := 0 to High(MimeTypes) do
    MimeMap.Add('.' + MimeTypes[I].Key, I);
end;

function PosHeaderSubItem(const AHeaderLine, ASubItem: StringA;
  var PStart, PEnd: PAnsiChar): Boolean;
var
  P, PMax, Ps, P1, P2: PAnsiChar;
  SL: Integer;
begin
  Result := False;
  P := PAnsiChar(AHeaderLine);
  PMax := P + Length(AHeaderLine);
  SL := Length(ASubItem);
  if (SL = 0) or (PMax = P) or (P = nil) then Exit;  
  Ps := PAnsiChar(ASubItem);
  while P < PMax do begin
    P1 := P;
    while (P < PMax) do begin
      if (P^ = ';') or (P^ = ',') or (P^ = '|') then begin
        Inc(P);
        Break;
      end else if P^ = '=' then begin
        P := P1;
        Break;
      end;
      Inc(P);
    end;
    while (P < PMax) and (P^ = ' ') do Inc(P);
    P1 := P;
    while (P1 < PMax) and (P1^ <> '=') do Inc(P1);    
    if (P1 = nil) or (P1^ = #0) then Exit;
    if P1 - P = SL then begin
      P2 := Ps;
      Result := True;
      while (P < P1) do begin
        if P^ <> P2^ then begin
          P := P1 + 1;
          Result := False;
          Break;
        end else begin
          Inc(P);
          Inc(P2);
        end;
      end;
      if Result then begin
        Inc(P);
        while (P < PMax) and (P^ = ' ') do Inc(P);
        while (P1 < PMax) do begin
          if (P1^ = ';') or (P1^ = ',') or (P1^ = '|') then Break;
          Inc(P1);
        end;
        PStart := P;
        PEnd := P1;
        Exit;
      end;
    end else begin
      P := P1 + 1;
    end;
  end;
end;

function ExtractHeaderSubItem(const AHeaderLine, ASubItem: StringA): StringA;
var
  P, P1: PAnsiChar;
begin
  if PosHeaderSubItem(AHeaderLine, ASubItem, P, P1) then
    SetString(Result, P, P1 - P)
  else
    Result := '';
end;

{ TIocpHttpRequest }

function NewSessionID(): string;
var
  V: TGUID;
begin
  CreateGUID(V);
  SetLength(Result, 32);
  StrLFmt(PChar(Result), 32, '%.8x%.4x%.4x%.2x%.2x%.2x%.2x%.2x%.2x%.2x%.2x',
    [V.D1, V.D2, V.D3, V.D4[0], V.D4[1], V.D4[2], V.D4[3],
    V.D4[4], V.D4[5], V.D4[6], V.D4[7]]);
end;

procedure TIocpHttpRequest.CheckCookieSession;
begin
  FSessionID := GetCookieItem(HTTPSESSIONID);
  if (FSessionID = '') and (Assigned(FResponse)) then begin
    FSessionID := StringA(NewSessionID());
    FResponse.AddCookie(HTTPSESSIONID, FSessionID);
  end;
end;

procedure TIocpHttpRequest.Clear;
begin
  FreeAndNil(FParams);
  FreeAndNil(FParamHash);
  FRequestData.Clear;
  FMethod := http_Unknown;
  FDataSize := 0;
  FIsFormData := 0;
  FConn := nil;
  if Assigned(FResponse) then begin
    FResponse.Clear;
    FResponse.FRequest := Self;
    if FOwner <> nil then     
      FResponse.FCharset := FOwner.FCharset;
  end;
  FURL := '';
  FURI.Len := 0;
  FURI.P := nil;
  FFormDataBoundary.Len := 0;
  FFormDataBoundary.P := nil;
  FCookies.P := nil;
  FCookies.Len := 0;
  FSessionID := '';
end;

procedure TIocpHttpRequest.Close;
begin
  if Assigned(Self) and Assigned(FOwner) then
    FOwner.FreeHttpRequest(Self);
end;

procedure TIocpHttpRequest.CloseConnection;
begin
  if Assigned(FConn) and (FConn.Active) then
    FConn.PostWSACloseRequest;
end;

constructor TIocpHttpRequest.Create(AOwner: TIocpHttpServer);
begin
  FOwner := AOwner;
  FResponse := AOwner.FHttpResponseClass.Create;
  FResponse.FRequest := Self; 
  FRequestData := TMemoryStream.Create;
  FParamHash := nil;
end;

procedure TIocpHttpRequest.DecodeParams;
var
  P: PAnsiChar;
begin
  FParamHash := TStringHash.Create();
  // GET 等通过URL传递的参数
  if FURI.Len < Length(FURL) then begin
    P := Pointer(FURL);
    Inc(P, FURI.Len);
    Inc(P);
    FOwner.DecodeParam(P, Length(FURL) - FURI.Len, DoReadParamItem);
  end;
  if FOwner.AutoDecodePostParams then
    ParsePostParams;
end;

procedure TIocpHttpRequest.ParsePostParams;
begin
  // POST 参数
  if (FMethod = http_POST) and (FDataSize > 0) then begin
    if not Assigned(FParamHash) then
      DecodeParams;
    FOwner.DecodeParam(PAnsiChar(FRequestData.Memory) + FHeaderSize, FDataSize, DoReadParamItem, nil, True);
  end;
end;

procedure TIocpHttpRequest.WriteBuffer(P: Pointer; Len: Cardinal);
begin
  FRequestData.Write(P^, Len);
end;

class function TIocpHttpRequest.DecodeStr(const S: StringA): StringA;
var
  tmp: StringA;
  AStr: StringA;
begin
  if Pos(StringA('%'), S) > 0 then begin
    try
      AStr := URLDecode(S, False);
      tmp := StringA(Utf8Decode(PCharA(AStr), Length(AStr)));
      if Length(tmp) > 0 then
        Result := tmp
      else
        Result := AStr;
    except
      Result := S;
    end;
  end else
    Result := S;
end;

destructor TIocpHttpRequest.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FParamHash);
  FreeAndNil(FResponse);
  FreeAndNil(FRequestData);
  inherited Destroy;
end;

procedure TIocpHttpRequest.DoReadParamItem(const Name, Value: string; Data: Pointer);
begin
  if not Assigned(FParams) then
    FParams := TStringList.Create;
  FParamHash.Add(LowerCase(Name), FParams.Count);
  FParams.Add(Value);
end;

function TIocpHttpRequest.AllowAccept(const Text: StringA): Boolean;
const
  S_XX: StringA = '*/*';
var
  AValue: StringA;
begin
  AValue := LowerCase(Accept);
  Result := (AValue = '') or (Pos(S_XX, AValue) > 0) or
    (Pos(LowerCase(Text), AValue) > 0);
end;

function TIocpHttpRequest.ExistContentType(const Text: StringA): Boolean;
var
  AValue: StringA;
begin
  AValue := LowerCase(GetContentType);
  Result := Pos(LowerCase(Text), AValue) > 0;
end;

function TIocpHttpRequest.ExistParam(const Name: StringA): Boolean;
begin
  if not Assigned(FParamHash) then
    DecodeParams;
  Result := FParamHash.Exists(string(Name));
end;

function TIocpHttpRequest.GetAccept: StringA;
begin
  Result := GetHeader('Accept');
end;

function TIocpHttpRequest.GetAcceptEncoding: StringA;
begin
  Result := GetHeader('Accept-Encoding');
end;

function TIocpHttpRequest.GetAcceptLanguage: StringA;
begin
  Result := GetHeader('Accept-Language');
end;

function TIocpHttpRequest.GetCharSet: StringA;
begin
  Result := GetHeaderParam('Content-Type', 'charset');
end;

function TIocpHttpRequest.GetContentType: StringA;
begin
  Result := GetHeader('Content-Type');
end;

function TIocpHttpRequest.GetCookieItem(const Name: StringA): StringA;
var
  P, PMax: PAnsiChar;
  Sub: PAnsiChar;
  SubLen: Integer;
  I: Integer;
begin
  InnerGetCookie();
  Result := '';
  SubLen := Length(Name);
  if (SubLen = 0) or FCookies.IsEmpty then
    Exit;
  P := FCookies.P;
  PMax := P + FCookies.Len;
  Sub := Pointer(Name);
  while P < PMax do begin
    I := PosStr(Sub, SubLen, P, PMax - P, 0);
    if I < 0 then
      Break;
    Inc(P, I + SubLen);
    if P^ = '=' then begin
      Inc(P);
      I := PosStr(';', 1, P, PMax - P, 0);
      if I < 0 then
        SetString(Result, P, PMax - P)
      else
        SetString(Result, P, I);
      Break;
    end;
  end;
end;

function TIocpHttpRequest.GetCookies: StringA;
begin
  InnerGetCookie();
  Result := FCookies.ToString;
end;

function TIocpHttpRequest.GetDataString(const ACharset: string): string;
var
  P: PChar;
  Data: Pointer;
  DataLen: Integer;
begin
  if (FDataSize = 0) or (not Assigned(FRequestData)) then begin
    Result := '';
    Exit;
  end;
  P := PChar(ACharset);
  Data := PAnsiChar(FRequestData.Memory) + FHeaderSize;
  DataLen := FRequestData.Size - FHeaderSize;
  // 根据字符集，解码成Ansi字符串
  if SysUtils.StrLIComp(P, S_UTF_8, 5) = 0 then
    Result := UTF8Decode(Data, DataLen)
  else if SysUtils.StrLIComp(P, S_UTF_16, 6) = 0 then begin
    {$IFDEF UNICODE}
    Result := PChar(Data);
    {$ELSE}
    Result := PCharWToString(Data, DataLen);
    {$ENDIF}
  end else // 其它的直接返回 (比如 GB2312, GBK, ISO-8859 等)
    {$IFDEF UNICODE}
    Result := PCharAToStringW(Data, DataLen);
    {$ELSE}
    Result := PCharToString(Data, DataLen);
    {$ENDIF}
end;

function TIocpHttpRequest.GetDataString: string;
var
  LCharSet: StringA;
begin
  LCharSet := CharSet;
  if LCharSet = '' then
    LCharSet := Owner.FCharset;
  Result := GetDataString(string(LCharSet));
end;

function TIocpHttpRequest.GetDataStringA: StringA;
var
  P, Data: PAnsiChar;
  LCharSet: StringA;
  DataLen: Integer;
begin
  if (FDataSize = 0) or (not Assigned(FRequestData)) then begin
    Result := '';
    Exit;
  end;
  LCharSet := CharSet;
  P := PAnsiChar(LCharSet);
  Data := PAnsiChar(FRequestData.Memory) + FHeaderSize;
  DataLen := FRequestData.Size - FHeaderSize;
  // 根据字符集，解码成Ansi字符串
  if StrLIComp(P, PAnsiChar(StringA(S_UTF_8)), 5) = 0 then
    Result := StringA(UTF8Decode(Data, DataLen))
  else if StrLIComp(P, PAnsiChar(StringA(S_UTF_16)), 6) = 0 then
    Result := PCharWToString(Data, DataLen)
  else // 其它的直接返回 (比如 GB2312, GBK, ISO-8859 等)
    SetString(Result, Data, DataLen);
end;

// 这一块功能为实时查询，你要是有需要也可以弄成将解析结果保存起来，以加快读取速度
function TIocpHttpRequest.GetFormDataItem(
  const Key: StringA): TIocpHttpFromDataItem;
var
  P, P1, P2, P3: PAnsiChar;
  B: Boolean;
begin
  Result.P := nil;
  Result.FC := nil;
  Result.Len := 0;
  if (not IsFormData) or (FRequestData.Size < FFormDataBoundary.Len) then
    Exit;
  P1 := FFormDataBoundary.P;
  P := FRequestData.Memory;
  P2 := P + FRequestData.Size;
  Inc(P, FHeaderSize);
  if PWORD(P)^ = PWORD(PAnsiChar(#13#10))^ then
    Inc(P, 2)
  else if (P^ = #13) or (P^ = #10) then
    Inc(P);
  while (P < P2 - 1) and (PWORD(P)^ = PWORD(PAnsiChar('--'))^) do begin
    Inc(P, 2);
    if StrLIComp(P, P1, FFormDataBoundary.Len) = 0 then begin
      Inc(P, FFormDataBoundary.Len + 2);
      Result.P := P;
      P3 := P2 - 3;
      B := False;
      while (P < P3) do begin
        if PDWORD(P)^ = PDWORD(PAnsiChar(#13#10#13#10))^ then begin
          B := True;
          Break;
        end;
        Inc(P);
      end;
      if not B then Break;

      Inc(P, 4);
      Result.FC := P;
      B := False; 
      while (P < P3) do begin
        if (PDWORD(P)^ = PDWORD(PAnsiChar(#13#10'--'))^) then begin
          Inc(P, 4);
          if StrLIComp(P, P1, FFormDataBoundary.Len) = 0 then begin
            B := True;
            Dec(P, 2);
          end;
          Break;
        end;
        Inc(P);
      end;

      if B then begin
        // 成功取出一段数据
        Result.Len := P - Result.P - 2;
        if Result.Name = Key then
          Exit;     
      end;
    end else
      Break;
  end;
  Result.P := nil;
  Result.FC := nil;
  Result.Len := 0;
end;

function TIocpHttpRequest.GetGetParamData: StringA;
var
  P: PAnsiChar;
  Len: Integer;
begin
  if FURI.Len < Length(FURL) then begin
    P := Pointer(FURL);
    Inc(P, FURI.Len + 1);
    Len := Length(FURL) - FURI.Len - 1;
    SetLength(Result, Len);
    if Len > 0 then
      Move(P^, Result[1], Len);
  end else
    Result := '';
end;

function TIocpHttpRequest.GetHeaderStr: StringA;
begin
  SetString(Result, PAnsiChar(FRequestData.Memory), FHeaderSize);
end;

function TIocpHttpRequest.GetHeader(const Name: StringA): StringA;
begin
  Result := InnerGetHeader(Name, FRequestData.Memory, FHeaderSize).ToString;
end;

function TIocpHttpRequest.GetHeaderParam(const Name,
  ParamName: StringA): StringA;
begin
  Result := ExtractHeaderSubItem(GetHeader(Name), ParamName);
end;

function TIocpHttpRequest.GetHost: StringA;
begin
  Result := GetHeader('Host');
end;

function TIocpHttpRequest.GetIsFormData: Boolean;
var
  S: StringA;
  I: Integer;
begin
  if FIsFormData = 0 then begin
    if FMethod <> http_POST then begin
      FIsFormData := 1;
      Result := False;
    end else begin
      FIsFormData := 1;
      FFormDataBoundary := InnerGetHeader('Content-Type', FRequestData.Memory, FHeaderSize);
      S := FFormDataBoundary.ToString;      
      I := Pos(StringA('multipart/form-data;'), LowerCase(S));
      Result := I > 0;
      if Result then begin
        I := Pos(StringA('boundary='), LowerCase(S));
        if I > 0 then begin
          FFormDataBoundary.P := FFormDataBoundary.P + I + 8;
          FFormDataBoundary.Len := FFormDataBoundary.Len - (I + 8);
          Result := FFormDataBoundary.Len > 0;
          if Result then
            FIsFormData := 2;
        end else
          Result := False;
      end;
    end;
  end else
    Result := FIsFormData = 2;
end;

function TIocpHttpRequest.GetIsGet: Boolean;
begin
  Result := FMethod = http_GET;
end;

function TIocpHttpRequest.GetIsPost: Boolean;
begin
  Result := FMethod = http_POST
end;

function TIocpHttpRequest.GetIsPut: Boolean;
begin
  Result := FMethod = http_PUT;
end;

function TIocpHttpRequest.GetIsRange: Boolean;
begin
  Result := (FRequestVersion = hv_V2) and (FRange);
end;

function TIocpHttpRequest.GetParam(const Name: StringA): string;
begin
  if not Assigned(FParamHash) then
    DecodeParams;
  Result := string(GetParamItem(FParamHash.ValueOf(LowerCase(string(Name)))));
end;

function TIocpHttpRequest.GetParamValues(const Name: StringA): TIocpArrayString;
var
  P, P1: PHashItem;
  I, J: Integer;
begin
  if not Assigned(FParamHash) then
    DecodeParams;
  FParamHash.Lock;
  try
    P := FParamHash.Find(LowerCase(string(Name)))^;
    I := 0;
    P1 := P;
    while P1 <> nil do begin
      Inc(I);
      P1 := P1.Next;
      if I > 100000 then
        Exit;
    end;
    SetLength(Result, I);
    J := 0;
    while (J < I) do begin
      Result[J] := string(GetParamItem(P.Value));
      Inc(J);
      P := P.Next;
    end;
  finally
    FParamHash.UnLock;
  end;
end;

function TIocpHttpRequest.GetParamIndex(Index: Integer): StringA;
begin
  if not Assigned(FParamHash) then
    DecodeParams;
  Result := GetParamItem(Index);
end;

function TIocpHttpRequest.GetParamItem(Index: Integer): StringA;
begin
  if (not Assigned(FParams)) or (Index < 0) or (Index >= FParams.Count) then
    Result := ''
  else
    Result := StringA(FParams[index]);
end;

function TIocpHttpRequest.GetParamsCount: Integer;
begin
  if Assigned(FParams) then
    Result := FParams.Count
  else
    Result := 0;
end;

function TIocpHttpRequest.GetRawURL: StringA;
begin
  Result := FRawURL.ToString;
end;

function TIocpHttpRequest.GetReferer: StringA;
begin
  Result := GetHeader('Referer');
end;

const
  CSHTTP1: StringA = 'HTTP/1.0';
  CSHTTP2: StringA = 'HTTP/1.1';

function TIocpHttpRequest.GetRequestVersionStr: StringA;
begin
  case FRequestVersion of
    hv_V1: Result := CSHTTP1;
    hv_V2: Result := CSHTTP2;
  else
    Result := CSHTTP2; // 默认1.1
  end;
end;

function TIocpHttpRequest.GetSession: Pointer;
begin
  if FSessionID = '' then
    CheckCookieSession;
  Result := FOwner.GetSession(string(FSessionID));
end;

function TIocpHttpRequest.GetSessionID: StringA;
begin
  if Length(FSessionID) = 0 then
    CheckCookieSession;
  Result := FSessionID;
end;

function TIocpHttpRequest.GetURI: StringA;
begin
  Result := FURI.ToString;
end;

function TIocpHttpRequest.GetWaitRecvSize: Int64;
begin
  Result := FDataSize - (FRequestData.Size - FHeaderSize);
end;

procedure TIocpHttpRequest.InnerGetCookie;
begin
  if FCookies.IsEmpty and Assigned(FRequestData) then
    FCookies := InnerGetHeader('Cookie', FRequestData.Memory, FHeaderSize);
end;

class function TIocpHttpRequest.InnerGetHeader(const Key: StringA;
  const Data: Pointer; DataSize: Integer): TIocpPointerStr;
var
  I, KeyLen: Integer;
  P, P1, P2, PMax: PAnsiChar;
begin
  P := Data;
  PMax := P + DataSize;
  Result.Len := 0;
  if P = PMax then Exit;  
  KeyLen := Length(Key);
  while P < PMax do begin
    I := PosStr(PAnsiChar(Key), KeyLen, P, PMax - P, 0);
    if I > -1 then begin
      Inc(I, KeyLen);
      Inc(P, I);
      while (P < PMax) and (P^ <> ':') and (P^ <> #13) do
        Inc(P);
      if P^ = ':' then begin
        Inc(P);
        P2 := P;
        while (P2 < PMax) and (P2^ = ' ') do Inc(P2);
        while (P < PMax) and (P^ <> #13) do Inc(P);
        P1 := P - 1;
        while (P1 > P2) and (P1^ = ' ') do begin
          Dec(P);
          Dec(P1);
        end;
        Result.P := P2;
        Result.Len := P - P2;
        Break;
      end;
    end else
      Break;
  end;
end;

function TIocpHttpRequest.DecodeHttpHeader(): Boolean;
const
  CSKeepAlive: StringA = 'keep-alive';
  CSContentLength: StringA = 'Content-Length';
  CSConnection: StringA = 'Connection';
  CSGzip: StringA = 'gzip';
var
  P, P1, PMax: PAnsiChar;
  J: Integer;
begin
  Result := False;
  
  FHeaderSize := FRequestData.Size;
  P := FRequestData.Memory;
  PMax := P + FHeaderSize - 4;

  // 请求参数及路径
  Inc(P, HTTPMethodLen[Integer(FMethod)]);
  if P^ <> ' ' then Exit;
  Inc(P);
  while (P < PMax) and (P^ = ' ') do Inc(P);
  P1 := P;
  while (P1 < PMax) and ((P1^ <> ' ') and (P1^ <> #13)) do Inc(P1);
  if P1^ <> ' ' then Exit;
  FRawURL.P := P;
  FRawURL.Len := P1 - P;
  FURL := StringA(DecodeStr(FRawURL.ToString));
  if Length(FURL) = 0 then Exit;  
  J := Integer(Pointer(FURL));
  P := StrScan(PAnsiChar(J), '?');
  if (P = nil) or (P^ = #0) then begin
    FURI.P := Pointer(J); //无参数和url一致
    FURI.Len := Length(FURL);
  end else begin
    FURI.P := Pointer(J);
    FURI.Len := P - PAnsiChar(J);
  end;

  // 请求版本号
  Inc(P1);
  if PInt64(P1)^ = PInt64(PAnsiChar(CSHTTP2))^ then begin
    FRequestVersion := hv_V2;
    FKeepAlive := True;
  end else if PInt64(P1)^ = PInt64(PAnsiChar(CSHTTP1))^ then begin
    FRequestVersion := hv_V1;
    FKeepAlive := False;
  end else
    Exit;
    
  // 读取内容长度
  if (FMethod = http_POST) or (FMethod = http_PUT) then begin   
    FDataSize := StrToIntDef(string(GetHeader(CSContentLength)), 0);
    if FDataSize > FOwner.FUploadMaxDataSize then
      Exit;
  end else
    FDataSize := 0;    

  // Keep-Alive
  if LowerCase(GetHeader(CSConnection)) = CSKeepAlive then
    FKeepAlive := True
  else
    FKeepAlive := False;

  // Accept GZip
  FAcceptGZip := Pos(CSGzip, GetAcceptEncoding()) > 0;
  if Assigned(FResponse) then
    FResponse.FGZip := FAcceptGZip;

  Result := True;
end;

function TIocpHttpRequest.DecodeHttpHeaderRange: Boolean;
const
  CSRange: StringA = 'Range';
  CSByte: StringA = 'byte';
var
  S: StringA;
  P, P1: PAnsiChar;
begin
  Result := False;
  FRangeStart := 0;
  FRangeEnd := 0;
  FRange := False;
  if FRequestVersion = hv_V2 then begin
    S := GetHeader(CSRange);
    if Length(S) > 6 then begin
      P := Pointer(S);
      if (PDWORD(P)^ = PDWORD(PAnsiChar(CSByte))^) and (S[5] = 's') and (S[6] = '=') then begin
        Inc(P, 6);
        P1 := StrScan(P, '-');
        if P1 = nil then begin
          FRangeStart := PCharToInt64Def(P, Length(S) - 6);
        end else begin
          FRangeStart := PCharToInt64Def(P, P1 - P);
          Inc(P1);
          P := P1;
          if P^ <> #0 then
            FRangeEnd := PCharToInt64Def(P, Length(P), 0);
        end;
        if (FRangeEnd > 0) and (FRangeEnd < FRangeStart) then
          Exit;
        FRange := (FRangeEnd > 0) or (FRangeStart > 0);
      end else
        Exit;
    end else if Length(S) > 0 then
      Exit;
  end;
  Result := True;
end;

function TIocpHttpRequest.DecodeHttpRequestMethod: TIocpHttpMethod;
var
  P: PAnsiChar;
begin
  Result := FMethod;
  if (Result <> http_Unknown) or (FRequestData.Size < 8) then
    Exit;

  P := FRequestData.Memory;
  
  if StrLIComp(P, 'GET', 3) = 0 then begin
    Result := http_GET;
  end else if StrLIComp(P, 'POST', 4) = 0 then begin
    Result := http_POST;
  end else if StrLIComp(P, 'PUT', 3) = 0 then begin
    Result := http_PUT;
  end else if StrLIComp(P, 'HEAD', 4) = 0 then begin
    Result := http_HEAD;
  end else if StrLIComp(P, 'OPTIONS', 7) = 0 then begin
    Result := http_OPTIONS;
  end else if StrLIComp(P, 'DELETE', 6) = 0 then begin
    Result := http_DELETE;
  end else if StrLIComp(P, 'TRACE', 5) = 0 then begin
    Result := http_TRACE;
  end else if StrLIComp(P, 'CONNECT', 7) = 0 then begin
    Result := http_CONNECT;
  end else
    Result := http_Unknown; 
    
  FMethod := Result;
end;

{ TIocpPointerStr }

function TIocpPointerStr.IsEmpty: Boolean;
begin
  Result := (Len = 0) or (P = nil);
end;

function TIocpPointerStr.ToString: StringA;
begin
  if (Len = 0) then
    Result := ''
  else
    SetString(Result, P, Len);
end;

{ TIocpHttpConnection }

const
  DEBUGINFO = 'HTTP逻辑处理...';
  
procedure TIocpHttpConnection.DoJob(AJob: PIocpJob);
var
  Obj: TIocpHttpRequest;
begin
  Obj := TIocpHttpRequest(AJob.Data);
  try     
    // 连接已经断开, 放弃处理逻辑
    if (Self = nil) or (Owner = nil) or (not Self.Active) then
      Exit;
      
    // 已经不是当时请求的连接，放弃处理逻辑
    if Obj.FConnHandle <> Self.Handle then begin
      OWner.DoStateMsgE(Self, '不是当前请求的连接，放弃处理.');
      Exit;
    end;

    try
      TIocpHttpServer(Owner).DoRequest(Obj);
    except
      Obj.FResponse.ServerError(StringA(Exception(ExceptObject).Message));
    end;

    // 更新激活时间
    LastActivity := GetTimestamp;  
  finally
    if Assigned(Owner.Moniter) then
      Owner.Moniter.incHttpRequestExecCounter;
    UnLockContext(Self, DEBUGINFO);
    Obj.Close;
  end;
end;

procedure TIocpHttpConnection.DoRequest(ARequest: TIocpHttpRequest);
begin
  if LockContext(Self, DEBUGINFO) then begin
    if Assigned(Owner.Moniter) then
      Owner.Moniter.incHttpRequestCreateCounter;
    Workers.Post(DoJob, ARequest);
  end else
    ARequest.Close;
end;

procedure TIocpHttpConnection.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrorCode: Integer);
var
  L, R, I: Cardinal;
  P, P1: PAnsiChar;
  lvTempRequest: TIocpHttpRequest;
  B: Boolean;
begin
  P := Buf;
  P1 := P;
  L := Len;
  R := 0;

  while L > 0 do begin

    if (FHttpState = hsCompleted) then begin
      FRequest := TIocpHttpServer(Owner).GetHttpRequest;
      FRequest.FConn := Self;
      FRequest.FConnHandle := Self.Handle;
      FHttpState := hsRequest;
    end;

    if (FHttpState = hsRequest) then begin

      case P^ of
        #13: Inc(R);
        #10:
          if (R = 1) or (R = 3) then
            Inc(R)
          else
            R := 0;
      else
        R := 0;
      end;

      Dec(L);

      if (R > 1) or (L = 0) then begin
        FRequest.WriteBuffer(P1, P - P1 + 1);         
        P1 := P + 1;
      end else begin
        Inc(P);
        Continue;
      end;      

      if FRequest.DecodeHttpRequestMethod = http_Unknown then begin
        FRequest.Close;
        FRequest := nil;
        CloseConnection;  // 无效的Http请求
        Break;       
      end;

      if R = 4 then begin
        Inc(P);  // Inc(P), 因为后面的Inc(P)不会被执行到了，所以这里先加上。
      
        if not FRequest.DecodeHttpHeader then begin
          FRequest.FResponse.ErrorRequest(400);  // 错误的请求
          Break;
        end else begin
          if not FRequest.DecodeHttpHeaderRange() then begin
            FRequest.FResponse.ErrorRequest(416);  // 无效的请求范围
            Break;
          end;
          if Assigned(TIocpHttpServer(Owner).FOnHttpFilter) then begin
            B := False;
            TIocpHttpServer(Owner).FOnHttpFilter(FRequest, B);
            if B then begin
              FRequest.FResponse.ErrorRequest(403);  // 禁止访问
              Break;
            end;
          end;
        end;

        if (FRequest.FMethod = http_POST) or (FRequest.FMethod = http_PUT) then begin
          // 改变Http状态, 进入接受数据状态
          FHttpState := hsRecvingPost;
        end else begin
          FHttpState := hsCompleted;
          lvTempRequest := FRequest;
          // 避免断开后还回对象池，造成重复还回
          FRequest := nil;
          // 触发事件
          DoRequest(lvTempRequest);  
          // 如果剩余的数据小于一个值，就不处理了
          if L < 12 then
            Break
          else
            Continue;

        end;
        
      end;
            
    end;

    if FHttpState = hsRecvingPost then begin
      // 接收Post数据中
      I := FRequest.GetWaitRecvSize;
      if I <= L then begin
        FRequest.WriteBuffer(P1, I);
        Dec(L, I);

        FHttpState := hsCompleted;
        lvTempRequest := FRequest;
        // 避免断开后还回对象池，造成重复还回
        FRequest := nil;
        // 触发事件
        DoRequest(lvTempRequest);
        // 如果剩余的数据小于一个值，就不处理了
        if L < 12 then
          Break
        else begin
          //Inc(P, I);
          Owner.DoStateMsgD(Self, 'Next Request. Ignore.');
          Break;
        end;
      end else begin
        if L > 0 then
          FRequest.WriteBuffer(P, L);
        Break;
      end;                                  
    end; 
      
    Inc(P);
  end;
end;

procedure TIocpHttpConnection.DoCleanUp;
begin
  inherited DoCleanUp;
  FHttpState := hsCompleted;
  if FRequest <> nil then begin
    FRequest.Close;
    FRequest := nil;
  end;
end;

{ TIocpHttpServer }

constructor TIocpHttpServer.Create(AOwner: TComponent);
begin
  inherited;
  ListenPort := 8080;
  FHttpRequestClass := TIocpHttpRequest;
  FHttpResponseClass := TIocpHttpResponse;
  FHttpRequestPool := TBaseQueue.Create;
  FHeaderBuildPool := TBaseQueue.Create;
  FMaxHeaderBuildPoolSize := 3072;
  FAutoDecodePostParams := True;
  FUploadMaxDataSize := 1024 * 1024 * 2;  // 2M
  FContextClass := TIocpHttpConnection;
  FSessionList := TStringHash.Create(99991);
  FSessionList.OnFreeItem := DoFreeHashItem;
  FAccessControlAllow := TIocpHttpAccessControlAllow.Create;
  {$IFDEF UNICODE}
  FCharset := S_UTF_16;  // delphi的 UnicodeString 实际上 UTF-16
  {$ELSE}
  FCharset := S_GB2312;
  {$ENDIF}
  FWebBasePath := ExtractFilePath(ParamStr(0)) + 'Web\';
  GzipFileTypes := '.htm;.html;.css;.js;.txt;.xml;.csv;.ics;.sgml;.c;.h;.pas;.cpp;.java;';
end;

procedure TIocpHttpServer.DecodeParam(P: PAnsiChar; Len: Cardinal;
  DoCallBack: TDoReadedParam; DoCallBackData: Pointer; DecodeURL: Boolean);
var
  P1: PAnsiChar;
  Key, Value: string;
  ReadValue: Boolean;
begin
  if (Len = 0) or (not Assigned(DoCallBack)) then Exit;
  while (Len > 0) and ((P^ = #13) or (P^ = #10) or (P^ = #32)) do begin
    Inc(P);
    Dec(Len);
  end;
  P1 := P;
  ReadValue := False;
  while (P <> nil) do begin
    if (P^ = '=') and (not ReadValue) then begin
      SetString(Key, P1, P - P1);
      P1 := P + 1;
      ReadValue := True;
    end else if (P^ = '&') or (P^ = #0) or (Len = 0) then begin
      if Length(Key) > 0 then begin
        SetString(Value, P1, P - P1);
        if Value <> '' then begin
          if DecodeURL then
            DoCallBack(LowerCase(Key), string(TIocpHttpRequest.DecodeStr(StringA(Value))),
              DoCallBackData)
          else
            DoCallBack(LowerCase(Key), Value, DoCallBackData);
        end;
        if (P^ = #0) or (Len = 0) then
          Break;
        Key := '';
        P1 := P + 1;
      end else if P^ = #0 then
        Break;
      ReadValue := False;
    end;
    Dec(Len);
    Inc(P);
  end;
end;

destructor TIocpHttpServer.Destroy;
begin
  inherited Destroy;
  try
    FHttpRequestPool.FreeDataObject;
    FHeaderBuildPool.FreeDataObject;
  finally
    FreeAndNil(FHttpRequestPool);
    FreeAndNil(FHeaderBuildPool);
    FreeSessionList;
    FAccessControlAllow.Free;
  end;
end;

procedure TIocpHttpServer.DoFreeHashItem(Item: PHashItem);
var
  P: Pointer;
begin
  if Assigned(FOnHttpFreeSession) then begin
    P := Pointer(Item.Value);
    FOnHttpFreeSession(Self, Item.Key, P);
    Item.Value := Integer(P);
  end;
end;

procedure TIocpHttpServer.DoRequest(ARequest: TIocpHttpRequest);
begin
  if Assigned(ARequest) then begin
    if Assigned(FOnHttpRequest) then begin
      FOnHttpRequest(Self, ARequest, ARequest.FResponse);
    end else begin
      // 如果没有事件处理，则返回 404 错误
      ARequest.FResponse.ErrorRequest(404);
      Exit;
    end;
    if not ARequest.FKeepAlive then
      ARequest.CloseConnection;
  end;
end;

procedure TIocpHttpServer.FreeHeaderBuilder(V: TStringCatHelperA);
begin
  if Assigned(V) then begin
    if FHeaderBuildPool.Size > FMaxHeaderBuildPoolSize then
      V.Free
    else
      FHeaderBuildPool.EnQueue(V);
  end;
end;

procedure TIocpHttpServer.FreeHttpRequest(V: TIocpHttpRequest);
begin
  if not Assigned(V) then Exit;
  V.Clear;
  FHttpRequestPool.EnQueue(V);
end;

procedure TIocpHttpServer.FreeSessionList;
begin
  FreeAndNil(FSessionList);
end;

function TIocpHttpServer.GetHeaderBuilder: TStringCatHelperA;
begin
  Result := TStringCatHelperA(FHeaderBuildPool.DeQueue);
  if Result = nil then
    Result := TStringCatHelperA.Create(512);
  Result.Reset;
end;

function TIocpHttpServer.GetHttpRequest: TIocpHttpRequest;
begin
  Result := TIocpHttpRequest(FHttpRequestPool.DeQueue);
  if Result = nil then
    Result := FHttpRequestClass.Create(Self);
  Result.Clear;
  Result.FOwner := Self;
  Result.FResponse.FRequest := Result;
end;

function TIocpHttpServer.GetSession(const SID: string): Pointer;
begin
  Result := Pointer(FSessionList.ValueOf(SID));
  if (Result = nil) and Assigned(FOnHttpGetSession) then begin
    Result := FOnHttpGetSession(Self, SID);
    FSessionList.AddOrUpdate(SID, Integer(Result));
  end;
end;

function TIocpHttpServer.GetWebBasePath: string;
begin
  if csDesigning in ComponentState then
    Result := GetRelativePath(AppPath, FWebBasePath)
  else
    Result := FWebBasePath;  
end;

procedure TIocpHttpServer.SetAccessControlAllow(
  const Value: TIocpHttpAccessControlAllow);
begin
  if Value = nil then
    FAccessControlAllow.Enabled := False
  else
    FAccessControlAllow.Assign(Value);
end;

procedure TIocpHttpServer.SetGzipFileTypes(const Value: string);
begin
  FGzipFileTypes := LowerCase(Value);
end;

procedure TIocpHttpServer.SetWebBasePath(const Value: string);
begin
  if csDesigning in ComponentState then
    FWebBasePath := GetAbsolutePathEx(AppPath, Value)
  else
    FWebBasePath := Value;
end;

{ TIocpHttpResponse }

const
  ContextLength0 = 'Content-Length: 0'#13#10;

function TIocpHttpResponse.AddCookie: TIocpHttpCookie;
begin
  Result := TIocpHttpCookie.Create;
  if not Assigned(FCookies) then
    FCookies := TList.Create;
  FCookies.Add(Result);
end;

function TIocpHttpResponse.AddCookie(const Name, Value: StringA): TIocpHttpCookie;
begin
  Result := AddCookie;
  Result.Name := Name;
  Result.Value := Value;
end;

function TIocpHttpResponse.AddCookie(const Name, Value: StringA;
  MaxAge: Cardinal): TIocpHttpCookie;
begin
  Result := AddCookie;
  Result.Name := Name;
  Result.Value := Value;
  Result.MaxAge := MaxAge;
end;

function TIocpHttpResponse.CheckFileUpdate(const Last: TDateTime): Boolean;
var
  T: TDateTime;
begin
  if (Last > 0) and (not Request.IsRange) then begin
    // 下载文件时，判断客户端请求的最后修改时间，如果没有变化就返回 304
    T := GMTRFC822ToDateTime(Request.GetHeader('If-Modified-Since'));
    if (T > 0) and (SecondsBetween(T, Last) = 0) then begin
      Result := False;
      ResponeCode(304);
      Exit;
    end;
  end;
  Result := True;
end;

procedure TIocpHttpResponse.Clear;
var
  I: Integer;
begin
  FContentType := '';
  FContentLanguage := '';
  FCacheTime := 0;
  {$IFDEF UseGZip}
  FGZip := True;
  {$ENDIF}
  if Assigned(FBlockSendBuffer) then
    FBlockSendBuffer.Clear;
  FreeAndNil(FOutWriter);
  if Assigned(FCookies) then begin
    for I := 0 to FCookies.Count - 1 do
      TObject(FCookies[I]).Free;
    FCookies.Clear;
  end;
end;

constructor TIocpHttpResponse.Create;
begin
  FGZip := False;
end;

destructor TIocpHttpResponse.Destroy;
begin
  FreeAndNil(FOutWriter);
  FreeAndNil(FBlockSendBuffer);
  FreeAndNil(FCookies);
  inherited;
end;

function GetResponseCodeNote(V: Word): StringA;
begin
  case V of
    100: Result := 'Continue';
    101: Result := 'Switching Protocols';
    102: Result := 'Processing';
    200: Result := 'OK';
    201: Result := 'Created';
    202: Result := 'Accepted';
    203: Result := 'Non-Authoriative Information';
    204: Result := 'No Content';
    205: Result := 'Reset Content';
    206: Result := 'Partial Content';
    207: Result := 'Multi-Status';
    300: Result := 'Multiple Choices';
    301: Result := 'Moved Permanently';
    302: Result := 'Temporarily Moved';
    303: Result := 'See Other';
    304: Result := 'Not Modified';
    305: Result := 'Use Proxy';
    306: Result := '(Unused)';
    307: Result := 'Temporary Redirect';
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'Not Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Timeout';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Precondition Failed';
    413: Result := 'Request Entity Too Large';
    414: Result := 'Request URI Too Long';
    415: Result := 'An Unsupported Media Type';
    416: Result := 'Requested Range Not Satisfiable';
    417: Result := 'On Failure';
    422: Result := 'Unprocessable Entity';
    423: Result := 'Locked';
    424: Result := 'Failed Dependency';
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Timeout';
    505: Result := 'Version Not Supported';
    507: Result := 'Insufficient Storage';
  else
    Result := 'Unknown';
  end;
end;

procedure TIocpHttpResponse.ServerError(const Msg: StringA);
begin
  if (not Active) then Exit;
  ErrorRequest(500, StringA(Format('<html><head><meta http-equiv="Content-Type" content="text/html; '+
      'charset=gb2312"></head>'#13'<body><font color="red"><b>%s</b></font><br>'+
      '<br>%s<br>'#13'</body></html>', [GetResponseCodeNote(500), Msg])));
end;

procedure TIocpHttpResponse.SetCharsetType(const Value: TIocpHttpCharset);
begin
  case Value of
    hct_8859_1: FCharset := '';
    hct_GB2312: FCharset := S_GB2312;
    hct_UTF8: FCharset := S_UTF_8;
    hct_UTF16: FCharset := S_UTF_16;
  end;
end;

procedure TIocpHttpResponse.ErrorRequest(ErrorCode: Word; const Msg: StringA);
var
  Data: TStringCatHelperA;
begin
  if (not Active) or (ErrorCode < 400) then Exit;
  Data := FRequest.FOwner.GetHeaderBuilder;
  try
    FCharSet := S_GB2312;
    FGZip := False;
    MakeHeader(Data, Length(Msg), ErrorCode);
    FixHeader(Data);
    if Msg <> '' then
      Data.Cat(Msg);
    FRequest.FConn.Send(Data.Memory, Data.Position, True);
    FRequest.FConn.CloseConnection;
  finally
    FRequest.FOwner.FreeHeaderBuilder(Data);
  end;
end;

function TIocpHttpResponse.GetActive: Boolean;
begin
  if Assigned(FRequest) and Assigned(FRequest.FConn) then
    Result := FRequest.FConn.Active
  else
    Result := False;
end;

function TIocpHttpResponse.GetBlockHeader: StringA;
const
  HStr: StringA = 'Transfer-Encoding: chunked'#13#10;
begin
  Result := FixHeader(MakeHeader(0) + HStr);
end;

function TIocpHttpResponse.GetCharsetType: TIocpHttpCharset;
var
  S: string;
begin
  S := LowerCase(string(FCharset));
  if S = '' then
    Result := hct_8859_1
  else if S = S_UTF_8 then
    Result := hct_UTF8
  else if S = S_GB2312 then
    Result := hct_GB2312
  else if S = S_UTF_16 then
    Result := hct_UTF16
  else
    Result := hct_Unknown;
end;

function TIocpHttpResponse.GetConnection: TIocpHttpConnection;
begin
  if Assigned(FRequest) then
    Result := FRequest.FConn
  else
    Result := nil;
end;

function TIocpHttpResponse.GetContentType: StringA;
begin
  if Length(FContentType) > 0 then   
    Result := FContentType
  else
    Result := 'text/html';
end;

class function TIocpHttpResponse.GetFileLastModified(
  const AFileName: string): TDateTime;
begin
  Result := GetFileLastWriteTime(StringA(AFileName));
end;

class function TIocpHttpResponse.GetFileMimeType(const AFileName: string): string;
var
  I: Integer;
begin
  I := MimeMap.ValueOf(LowerCase(ExtractFileExt(AFileName)));
  if I < 0 then
    Result := string(HTTPCTTypeStream)
  else
    Result := MimeTypes[I].Value;
end;

function TIocpHttpResponse.GetOutWriter(BufferSize: Cardinal): TIocpHttpWriter;
begin
  if not Assigned(FOutWriter) then begin
    FOutWriter := TIocpHttpWriter.Create(BufferSize);
    FOutWriter.FOwner := Self;
    if FCharset = '' then begin
      {$IFDEF UNICODE}
      FOutWriter.Charset := hct_UTF16;
      {$ELSE}
      FOutWriter.Charset := hct_GB2312;
      {$ENDIF}
    end else
      FOutWriter.Charset := CharsetType;
  end;
  Result := FOutWriter;
end;

{$IFDEF UseGZip}
function TIocpHttpResponse.GZCompress(inStream, outStream: TStream): Boolean;
begin
  GZCompressStream(inStream, outStream);
  Result := True;
end;

function TIocpHttpResponse.GZCompress(Buf: Pointer; Len: Cardinal): StringA;
var
  S: StringA;
begin
  if Len = 0 then
    Result := ''
  else begin
    SetLength(S, Len);
    Move(Buf^, S[1], Len);
    Result := GZCompressStr(S);
  end;
end;

function TIocpHttpResponse.GZCompress(const Data: StringW): StringA;
var
  S: StringA;
begin
  if Length(Data) = 0 then
    Result := ''
  else begin
    SetLength(S, Length(Data) shl 1);
    Move(Data[1], S[1], Length(S));
    Result := GZCompressStr(S);
  end;
end;
{$ENDIF}

{$IFDEF UseGZip}
function TIocpHttpResponse.GZCompress(const Data: StringA): StringA;
begin
  Result := GZCompressStr(Data);
end;
{$ENDIF}

function TIocpHttpResponse.MakeFixHeader(const ContentLength: Int64;
  const StatusCode: Integer; FileDown: Boolean; const FileName: StringA;
  const LastModified: TDateTime): StringA;
begin
  Result := MakeHeader(ContentLength, StatusCode, FileDown, FileName,
    LastModified, True);
end;

function TIocpHttpResponse.MakeHeader(const ContentLength: Int64;
  const StatusCode: Integer; FileDown: Boolean; const FileName: StringA;
  const LastModified: TDateTime; IsFixHeader: Boolean): StringA;
var
  Data: TStringCatHelperA;
begin
  Data := FRequest.FOwner.GetHeaderBuilder;
  MakeHeader(Data, ContentLength, StatusCode, FileDown, FileName, LastModified);
  if IsFixHeader then
    FixHeader(Data);
  Result := Data.Value;
  FRequest.FOwner.FreeHeaderBuilder(Data);
end;

function TIocpHttpResponse.MakeHeader(Data: TStringCatHelperA;
  const ContentLength: Int64; const StatusCode: Integer; FileDown: Boolean;
  const FileName: StringA; const LastModified: TDateTime): Boolean;
const
  CSStatusOK: StringA = ' 200 OK';
  CSVRNAME: StringA = #13#10'Server: IOCP-YXD/1.1'#13#10;
  CSDFILE: StringA = 'Accept-Ranges: bytes'#13#10 +
    'Content-Disposition: attachment;filename="%s"'#13#10'Last-Modified: %s'#13#10;
  CSCHARSET: StringA = 'charset=';
  CSContentLanguage: StringA = 'Content-Language: ';
  CSSetCookie: StringA = 'Set-Cookie:';
  CSDate: StringA = 'Date: ';
  CSContentType: StringA = 'Content-Type: ';
  CSContentLength: StringA = 'Content-Length: ';
  CSContentEncodeGzip: StringA = 'Content-Encoding: gzip'#13#10;
  CSCacheControlMaxAge: StringA = 'Cache-Control: max-age=';
var
  I: Integer;
begin
  Data.Cat(FRequest.RequestVersionStr);
  if (StatusCode <= 0) then begin
    // 处理区域传送 (用于断点传输)
    if FileDown and FRequest.IsRange then begin
      Data.Cat(' 206 Partial Content');
    end else
      Data.Cat(CSStatusOK);
  end else
    Data.Space(1).Cat(IntToStr(StatusCode)).Cat(' ').Cat(GetResponseCodeNote(StatusCode));
  
  Data.Cat(CSVRNAME);
  Data.Cat(CSDate).Cat(GetNowGMTRFC822()).Cat(HTTPLineBreak);

  // 如果不是错误的响应或者内容长度大于0才写入这些Header
  if (StatusCode < 400) or (ContentLength > 0) then begin
    // Content-Language
    if Length(FContentLanguage) > 0 then
      Data.Cat(CSContentLanguage).Cat(FContentLanguage).Cat(HTTPLineBreak)
    else if Length(FRequest.FOwner.FContentLanguage) > 0 then
      Data.Cat(CSContentLanguage).Cat(FRequest.FOwner.FContentLanguage).Cat(HTTPLineBreak);

    // Content-Type
    Data.Cat(CSContentType).Cat(GetContentType);
    if Length(FCharset) > 0 then begin
      if PosStr(PAnsiChar(CSCHARSET), Length(CSCHARSET), Data.Memory, Data.Position, 0) < 1 then
        Data.Cat('; ').Cat(CSCHARSET).Cat(FCharset);
    end;
    Data.Cat(HTTPLineBreak);

    // Content-Length
    Data.Cat(CSContentLength).Cat(ContentLength).Cat(HTTPLineBreak);

    // Content-Encode
    {$IFDEF UseGZip}
    if FGZip then
      Data.Cat(CSContentEncodeGzip);
    {$ENDIF}
  end;

  // 跨域控制
  FRequest.FOwner.AccessControlAllow.MakerHeader(Data);

  if FCacheTime > 0 then
    Data.Cat(CSCacheControlMaxAge).Cat(FCacheTime).Cat(HTTPLineBreak);
    
  if FileDown then begin
    if LastModified > 0 then
      Data.Cat(Format(CSDFILE, [FileName, DateTimeToGMTRFC822(LastModified)]))
    else
      Data.Cat('Accept-Ranges: bytes'#13#10'Content-Disposition: attachment;filename="')
        .Cat(FileName).Cat('"'#13#10);
  end;

  if Assigned(FCookies) then begin  
    for I := 0 to FCookies.Count - 1 do
      Data.Cat(CSSetCookie).Cat(TIocpHttpCookie(FCookies[i]).ToString()).Cat(HTTPLineBreak);
  end;
  
  MakeHeaderEx(StatusCode, Data);

  Result := True;
end;

procedure TIocpHttpResponse.MakeHeaderEx(const StatusCode: Integer;
  const Data: TStringCatHelperA);
const
  CSConnectionKeepAlive: StringA = 'Connection: Keep-Alive'#13#10;
  CSConnectionClose: StringA = 'Connection: close'#13#10;
begin
  if Request.FKeepAlive and (StatusCode < 400) then
    Data.Cat(CSConnectionKeepAlive)
  else       
    Data.Cat(CSConnectionClose);
end;

procedure TIocpHttpResponse.RedirectURL(const pvURL: StringA);
begin
  if (not Active) or (Length(pvURL) = 0) then Exit;
  FRequest.FConn.Send(
    FixHeader(MakeHeader(0, 302) + 'Location: ' + pvURL));
  FRequest.FConn.CloseConnection;
end;

procedure TIocpHttpResponse.ResponeCode(Code: Word; const Data: StringA);
begin
  if (not Active) or (Code < 100) then Exit;
  FRequest.FConn.Send(
    MakeFixHeader(Length(Data), Code) + Data);
end;

const
  MaxHttpOSS = $40000;  // Http数据超过此值时，先发送协议头再发送内容

procedure TIocpHttpResponse.Send(buf: Pointer; len: Cardinal; AGZip: Boolean);
var
  s: StringA;
begin
  if (not Active) then Exit;
  if (len = 0) or (buf = nil) then begin
    Send('');
    Exit;
  end;
  {$IFDEF UseGZip}
  FGZip := AGZip;
  if AGZip then begin
    s := GZCompress(buf, len);
    if Length(s) > MaxHttpOSS then begin
      FRequest.FConn.Send(MakeFixHeader(Length(s)));
      FRequest.FConn.Send(s);
    end else
      FRequest.FConn.Send(MakeFixHeader(Length(s)) + s);
  end else
  {$ENDIF}
  begin  
    if len > MaxHttpOSS then begin
      FRequest.FConn.Send(MakeFixHeader(len));
      FRequest.FConn.Send(buf, len);
    end else begin
      SetString(s, PAnsiChar(buf), len);
      CopyMemory(@s[1], buf, len);
      FRequest.FConn.Send(MakeFixHeader(len) + s);
    end;
  end;
end;

procedure TIocpHttpResponse.Send(const Data: TBytes; AGZip: Boolean);
begin
  if Length(Data) = 0 then begin
    if (not Active) then Exit;
    Send('');
  end else
    Send(@Data[0], Length(Data), AGZip);
end;

procedure WriteStringToStream(Stream: TStream; const V: StringA); overload;
begin
  Stream.Write(V[1], Length(V));
end;

procedure WriteStringToStream(Stream: TStream; const V: StringW); overload;
begin
  Stream.Write(Pointer(V)^, Length(V) shl 1);
end;

procedure TIocpHttpResponse.Send(Stream: TStream; AGZip: Boolean);
{$IFDEF UseGZip}
var
  S: TMemoryStream;
  L: NativeUInt;
{$ENDIF}
begin
  if not Active then Exit;
  if not Assigned(Stream) then begin
    Send('');
    Exit;
  end;
  {$IFDEF UseGZip}
  FGZip := AGZip;
  L := Stream.Size - Stream.Position;
  if L < 1024 then
    FGZip := False;
  if FGZip and (L > 0) then begin
    S := TMemoryStream.Create;
    try
      GZCompressStream(Stream, S);
      S.Position := 0;
      SendStream(S, False, '', FContentType);
    finally
      S.Free;
    end;
  end else
  {$ENDIF}
    SendStream(Stream, False, '', FContentType);
end;

procedure TIocpHttpResponse.SendChunk(const Data: StringA);
begin
  if (not Active) or (not Assigned(FBlockSendBuffer)) then Exit;
  if Length(Data) = 0 then Exit;
  WriteStringToStream(FBlockSendBuffer, StringA(IntToHex(Length(Data), 2)) + HTTPLineBreak);
  WriteStringToStream(FBlockSendBuffer, Data);
  WriteStringToStream(FBlockSendBuffer, StringA(HTTPLineBreak));
end;

procedure TIocpHttpResponse.SendChunk(const Data: StringW);
begin
  if (not Active) or (not Assigned(FBlockSendBuffer)) then Exit;
  if Length(Data) = 0 then Exit;
  WriteStringToStream(FBlockSendBuffer, StringA(IntToHex(Length(Data) shl 1, 2)) + HTTPLineBreak);
  WriteStringToStream(FBlockSendBuffer, Data);
  WriteStringToStream(FBlockSendBuffer, StringA(HTTPLineBreak));
end;

procedure TIocpHttpResponse.SendChunk(buf: Pointer; len: Cardinal);
begin
  if (not Active) or (not Assigned(FBlockSendBuffer)) then Exit;
  if len = 0 then Exit;
  WriteStringToStream(FBlockSendBuffer, StringA(IntToHex(len, 2)) + HTTPLineBreak);
  FBlockSendBuffer.Write(buf^, len);
  WriteStringToStream(FBlockSendBuffer, StringA(HTTPLineBreak));
end;

procedure TIocpHttpResponse.SendChunk(Stream: TStream);
var
  I, J: Integer;
  Buf: array [0..4095] of Byte;
  P: Pointer;
begin
  if (not Active) or (not Assigned(FBlockSendBuffer)) then Exit;
  I := Stream.Size - Stream.Position;
  if I = 0 then Exit;
  WriteStringToStream(FBlockSendBuffer, StringA(IntToHex(I, 2)) + HTTPLineBreak);
  if (Stream is TMemoryStream) then begin
    P := Pointer(IntPtr(TMemoryStream(Stream).Memory) + Stream.Position);
    FBlockSendBuffer.Write(P^, I);
  end else begin
    while I > 0 do begin
      if I > SizeOf(Buf) then
        J := Stream.Read(Buf, SizeOf(Buf))
      else
        J := Stream.Read(Buf, I);
      if J > 0 then begin
        FBlockSendBuffer.Write(Buf, J);
        Dec(I, J);
      end;
    end;
  end;
  WriteStringToStream(FBlockSendBuffer, StringA(HTTPLineBreak));
end;

procedure TIocpHttpResponse.SendChunkEnd;
begin
  if not Assigned(FBlockSendBuffer) then Exit;
  WriteStringToStream(FBlockSendBuffer, StringA(CharA('0') + HTTPHeaderEnd));
  FBlockSendBuffer.Position := 0;
  FRequest.FConn.SendStream(FBlockSendBuffer);
  FBlockSendBuffer.Clear;
end;

procedure TIocpHttpResponse.SendChunkHeader(AGZip: Boolean);
begin
  {$IFDEF UseGZip}
  FGZip := AGZip;
  {$ENDIF}
  if not Assigned(FBlockSendBuffer) then
    FBlockSendBuffer := TMemoryStream.Create;
  WriteStringToStream(FBlockSendBuffer, GetBlockHeader);
end;

procedure TIocpHttpResponse.SendContent(const Data: StringW);
begin
  if (not Active) then Exit;
  FRequest.FConn.Send(Data);
end;

procedure TIocpHttpResponse.SendFile(Request: TIocpHttpRequest;
  const AContentType: string; IsDownFile, ParserContentType: Boolean);
begin
  SendFileByURI(string(Request.URI), AContentType, IsDownFile, ParserContentType);
end;

procedure TIocpHttpResponse.SendFileByURI(const URI, AContentType: string;
  IsDownFile, ParserContentType: Boolean);
var
  Path: string;
begin
  Path := ReplaceSlashFromURI(URI);
  SendFile(Request.FOwner.FWebBasePath + Path, AContentType, IsDownFile, ParserContentType);
end;

procedure TIocpHttpResponse.SendFile(const FileName, AContentType: string;
  IsDownFile, ParserContentType: Boolean);

  procedure SendData(S: TStream; const FileExt: string; const Last: TDateTime);
  var
    I: Integer;
  begin
    FCharset := '';
    if Length(AContentType) = 0 then begin
      I := -1;
      if ParserContentType then
        I := MimeMap.ValueOf(FileExt);
      if I < 0 then
        SendStream(S, IsDownFile, StringA(FileName), HTTPCTTypeStream, Last)
      else
        SendStream(S, IsDownFile, StringA(FileName), StringA(MimeTypes[I].Value), Last);
    end else
      SendStream(S, IsDownFile, StringA(FileName), StringA(AContentType), Last);
  end;

var
  S: TFileOnlyStream;
  MS: TMemoryStream;
  Last: TDateTime;
  FileExt: string;
begin
  if (not Active) then Exit;
  if not FileExists(FileName) then begin
    ErrorRequest(404);
    Exit;
  end;
  Last := GetFileLastWriteTime(StringA(FileName));
  // 下载文件时，判断客户端请求的最后修改时间，如果没有变化就返回 304
  if not CheckFileUpdate(Last) then
    Exit;
  FileExt := LowerCase(ExtractFileExt(FileName));
  // 如果未设置Gzip，客户端支持Gzip    
  if Request.AcceptGzip then 
    FGZip := Pos(FileExt + ';', Request.FOwner.FGzipFileTypes) > 0
  else
    FGZip := False;
  MS := nil;
  S := TFileOnlyStream.Create(FileName);
  try
    S.Position := 0;
    if FGZip then begin
      MS := TMemoryStream.Create;
      if GZCompress(S, MS) then begin
        FreeAndNil(S);
        MS.Position := 0;
        SendData(MS, FileExt, Last)
      end else begin
        S.Position := 0;
        SendData(S, FileExt, Last);
      end;
    end else 
      SendData(S, FileExt, Last);    
  finally
    FreeAndNil(MS);
    FreeAndNil(S);
  end;
end;

procedure TIocpHttpResponse.SendFileData(const Data: StringA; const FileName,
  AContentType: StringA; const LastModified: TDateTime; AGZip: Boolean);
var
  S: TIocpPointerStream;
begin
  if (not Active) then Exit;
  S := TIocpPointerStream.Create;
  try
    S.SetPointer(Pointer(Data), Length(Data));
    SendFileStream(S, FileName, AContentType, LastModified, AGZip);
  finally
    S.Free;
  end;
end;

procedure TIocpHttpResponse.SendFileData(const Data: StringW; const FileName,
  AContentType: StringA; const LastModified: TDateTime; AGZip: Boolean);
var
  S: TIocpPointerStream;
begin
  if (not Active) then Exit;
  S := TIocpPointerStream.Create;
  try
    S.SetPointer(Pointer(Data), Length(Data) shl 1);
    SendFileStream(S, FileName, AContentType, LastModified, AGZip);
  finally
    S.Free;
  end;
end;

procedure TIocpHttpResponse.SendFileData(const Data: Pointer; DataLen: Cardinal;
  const FileName, AContentType: StringA; const LastModified: TDateTime;
  AGZip: Boolean);
var
  S: TIocpPointerStream;
begin
  if (not Active) or (DataLen = 0) or (Data = nil) then Exit;
  S := TIocpPointerStream.Create;
  try
    S.SetPointer(Data, DataLen);
    SendFileStream(S, FileName, AContentType, LastModified, AGZip);
  finally
    S.Free;
  end;
end;

procedure TIocpHttpResponse.SendFileStream(AFileStream: TStream;
  const FileName, AContentType: StringA; const LastModified: TDateTime;
  AGZip: Boolean);
var
  S: TMemoryStream;
begin
  if (not Active) then Exit;
  FCharset := '';
  {$IFDEF UseGZip}
  FGZip := AGZip;
  if AGZip then begin
    FGZip := True;
    S := TMemoryStream.Create;
    try
      GZCompress(AFileStream, S);
      S.Position := 0;
      if Length(AContentType) = 0 then begin
        SendStream(S, True, FileName, HTTPCTTypeStream, LastModified)
      end else
        SendStream(S, True, FileName, AContentType, LastModified);
    finally
      S.Free;
    end;
  end else {$ENDIF} begin
    if Length(AContentType) = 0 then begin
      SendStream(AFileStream, True, FileName, HTTPCTTypeStream, LastModified)
    end else
      SendStream(AFileStream, True, FileName, AContentType, LastModified);
  end;
end;

procedure TIocpHttpResponse.SendContent(const Data: StringA);
begin
  if (not Active) then Exit;
  FRequest.FConn.Send(Data);
end;

procedure TIocpHttpResponse.SendHeader(DataSize: NativeUInt; AGZip: Boolean);
begin
  if (not Active) then Exit;
  if DataSize < 1 then
    Send('')
  else begin
    {$IFDEF UseGZip}
    FGZip := AGZip;
    {$ENDIF}
    FRequest.FConn.Send(MakeFixHeader(DataSize));
  end;
end;

function SpanOfNowAndThen(const ANow, AThen: TDateTime): TDateTime; inline;
begin
  if ANow < AThen then
    Result := AThen - ANow
  else
    Result := ANow - AThen;
end;

function SecondsBetween(const ANow, AThen: TDateTime): Int64; 
begin
  Result := Round(SecsPerDay * SpanOfNowAndThen(ANow, AThen));
end;

procedure TIocpHttpResponse.SendStream(Stream: TStream; IsDownloadFile: Boolean;
  const AFileName, AContentType: StringA; const LastModified: TDateTime);
var
  L: Int64;
  I: Integer;
  Buf: array [0..4095] of Byte;
  Header: StringA;
  IsRange: Boolean;
  T: TDateTime;
begin
  L := Stream.Size - Stream.Position;
  FContentType := AContentType;

  if IsDownloadFile then begin
    // 文件下载模式，支持区域下载/断点续传
    IsRange := FRequest.IsRange;

    if (LastModified > 0) and (not IsRange) then begin
      // 下载文件时，判断客户端请求的最后修改时间，如果没有变化就返回 304
      T := GMTRFC822ToDateTime(Request.GetHeader('If-Modified-Since'));
      if (T > 0) and (SecondsBetween(T, LastModified) = 0) then begin
        ResponeCode(304);
        Exit;
      end;
    end else if (IsRange) and (LastModified > 0) then begin
      // 判断文件是否已经修改，如果已经修改，则不允许分块下载
      T := GMTRFC822ToDateTime(Request.GetHeader('If-Range'));
      if (T > 0) and (SecondsBetween(T, LastModified) > 0) then
        // 已经修改，返回全部数据
        IsRange := False
      else begin
        T := GMTRFC822ToDateTime(Request.GetHeader('If-Unmodified-Since'));
        if (T > 0) and (SecondsBetween(T, LastModified) > 0) then begin
          // 已经修改，不返回数据
          IsRange := False;
          L := 0;
        end;
      end;
    end;

    if (IsRange) then begin
      if FRequest.FRangeStart >= L then
        Exit;
      if FRequest.FRangeEnd = 0 then
        FRequest.FRangeEnd := L - 1;
      Header := StringA(Format('Content-Range: bytes %d-%d/%d', [
          FRequest.FRangeStart, FRequest.FRangeEnd, L]));
      L := FRequest.FRangeEnd - FRequest.FRangeStart + 1;
      Header := FixHeader(MakeHeader(L, 0, IsDownloadFile, ExtractFileName(AFileName), LastModified) + Header);
      if FRequest.FRangeStart > 0 then
        Stream.Position := Stream.Position + FRequest.FRangeStart;
    end else begin
      Header := MakeHeader(L, 0, IsDownloadFile,
        ExtractFileName(AFileName), LastModified, True);
    end;

  end else
    Header := MakeFixHeader(L);

  if L > MaxHttpOSS then begin
    FRequest.FConn.Send(Header);
    FRequest.FConn.SendStream(Stream, L);
  end else begin
    if not Assigned(FBlockSendBuffer) then
      FBlockSendBuffer := TMemoryStream.Create
    else
      FBlockSendBuffer.Clear;
    try
      WriteStringToStream(FBlockSendBuffer, Header);
      while L > 0 do begin
        I := Stream.Read(Buf, SizeOf(Buf));
        if I > 0 then begin
          FBlockSendBuffer.Write(Buf, I);
          Dec(L, I);
        end else
          Break;
      end;
      FBlockSendBuffer.Position := 0;
      FRequest.FConn.SendStream(FBlockSendBuffer, FBlockSendBuffer.Size);
    finally
      FBlockSendBuffer.Clear;
    end;
  end;
end;

procedure TIocpHttpResponse.SendString(const Data: StringA; AGZip: Boolean);
{$IFDEF UseGZip}
var
  s: StringA;
{$ENDIF}
begin
  if (not Active) then Exit;
  if Length(Data) = 0 then begin
    // 0长度的返回内容，写入一个#0，否则有些浏览器会等待
    FRequest.FConn.Send(MakeFixHeader(0) + #0);
    Exit;
  end;
  {$IFDEF UseGZip}
  FGZip := AGZip;
  if FGZip then begin
    s := GZCompressStr(Data);
    if (Length(s)) > MaxHttpOSS then begin
      FRequest.FConn.Send(MakeFixHeader(Length(s)));
      FRequest.FConn.Send(s);
    end else
      FRequest.FConn.Send(MakeFixHeader(Length(s)) + s);
  end else
  {$ENDIF}
  begin
    if (Length(Data)) > MaxHttpOSS then begin
      FRequest.FConn.Send(MakeFixHeader(Length(Data)));
      FRequest.FConn.Send(Data);
    end else
      FRequest.FConn.Send(MakeFixHeader(Length(Data)) + Data);
  end;
end;

procedure TIocpHttpResponse.SendString(const Data: StringW; AGZip: Boolean);
var
  Len: Int64;
{$IFDEF UseGZip}
  s: StringA;
{$ENDIF}
  Buffer: TMemoryStream;
begin
  if (not Active) then Exit;
  if Length(Data) = 0 then begin
    // 0长度的返回内容，写入一个#0，否则有些浏览器会等待
    FRequest.FConn.Send(MakeFixHeader(0) + #0);
    Exit;
  end;
  {$IFDEF UseGZip}
  FGZip := AGZip;
  if FGZip then begin
    s := GZCompress(Data);
    if (Length(s)) > MaxHttpOSS then begin
      FRequest.FConn.Send(MakeFixHeader(Length(s)));
      FRequest.FConn.Send(s);
    end else begin
      FRequest.FConn.Send(MakeFixHeader(Length(s)) + s);
    end;
  end else
  {$ENDIF}
  begin
    Len := Length(Data) shl 1;
    if Len > MaxHttpOSS then begin
      FRequest.FConn.Send(MakeFixHeader(Len));
      FRequest.FConn.Send(Data);
    end else begin
      Buffer := TMemoryStream.Create;
      try
        WriteStringToStream(Buffer, MakeFixHeader(Len));
        WriteStringToStream(Buffer, Data);
        FRequest.FConn.Send(Buffer.Memory, Buffer.Position);
      finally
        Buffer.Free;
      end;
    end;
  end;
end;

procedure TIocpHttpResponse.Send(var Writer: TIocpHttpWriter; AGZip, AFreeWriter: Boolean);
begin
  try
    if Assigned(Writer) and (not Writer.IsEmpty) then begin
      case Writer.FCharset of
        hct_8859_1:
          begin
            FCharset := '';
            SendString(StringA(Writer.ToString), AGZip)
          end;
        hct_GB2312:
          begin
            FCharset := S_GB2312;
            {$IFDEF UNICODE}
            SendString(StringA(Writer.ToString), AGZip)
            {$ELSE}
            SendString(Writer.ToString, AGZip)
            {$ENDIF}
          end;
        hct_UTF8:
          begin
            FCharset := S_UTF_8;
            {$IFDEF UNICODE}
            SendString(UTF8Encode(Writer.FData.Start, Writer.FData.Position), AGzip)
            {$ELSE}
            SendString(UTF8Encode(Writer.ToString()), AGzip)
            {$ENDIF}
          end;
        hct_UTF16:
          begin
            FCharset := S_UTF_16;
            {$IFDEF UNICODE}
            SendString(Writer.ToString, AGZip)
            {$ELSE}
            SendString(StringW(Writer.ToString), AGZip)
            {$ENDIF}
          end;
      end;
    end else begin
      if (not Active) then Exit;
      FRequest.FConn.Send(MakeFixHeader(0) + #0);
    end;
  finally
    if AFreeWriter then
      FreeAndNil(Writer);
  end;
end;

procedure TIocpHttpResponse.Send(const Data: string; AGZip: Boolean);
begin
  if (not Active) then Exit;
  case CharsetType of
    hct_8859_1:
      begin
        SendString(StringA(Data), AGZip)
      end;
    hct_GB2312:
      begin
        {$IFDEF UNICODE}
        SendString(StringA(Data), AGZip)
        {$ELSE}
        SendString(Data, AGZip)
        {$ENDIF}
      end;
    hct_UTF8:
      begin
        {$IFDEF UNICODE}
        SendString(StringA(iocp.Utils.Str.UTF8Encode(Data)), AGzip);
        {$ELSE}
        SendString(UTF8Encode(Data), AGzip)
        {$ENDIF}
      end;
    hct_UTF16:
      begin
        {$IFDEF UNICODE}
        SendString(Data, AGZip)
        {$ELSE}
        SendString(StringW(Data), AGZip)
        {$ENDIF}
      end;
    else
      SendString(Data, AGzip);
  end;
end;

{ TIocpPointerStream }

constructor TIocpPointerStream.Create;
begin
  inherited Create;
end;

constructor TIocpPointerStream.Create(const Ptr: Pointer; ASize: Integer);
begin
  inherited Create;
  SetPointer(Ptr, ASize);
end;

function TIocpPointerStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create('PointerStream Ban Written.');
end;

{ TIocpHttpFromDataItem }

function TIocpHttpFromDataItem.GetContentStream: TStream;
begin
  if (Len = 0) or (P = nil) or (FC < P) then
    Result := nil
  else begin
    Result := TIocpPointerStream.Create;
    TIocpPointerStream(Result).SetPointer(FC, Len - HeaderSize);
  end;
end;

function TIocpHttpFromDataItem.GetContentType: StringA;
begin
  if Len = 0 then
    Result := ''
  else
    Result := TIocpHttpRequest.InnerGetHeader('Content-Type', P, FC - P).ToString;
end;

function TIocpHttpFromDataItem.GetDisposition: StringA;
begin
  if Len = 0 then
    Result := ''
  else
    Result := TIocpHttpRequest.InnerGetHeader('Content-Disposition', P, FC - P).ToString;
end;

function TIocpHttpFromDataItem.GetFileName: StringA;
var
  I: Integer;
  P, P1: PAnsiChar;
begin
  Result := GetDisposition;
  if Length(Result) > 0 then begin
    I := Pos(StringA('filename="'), Result);
    if I > 0 then begin
      P := PAnsiChar(Result) + I + 9;
      P1 := StrScan(P, '"');
      if P1 <> nil then
        SetString(Result, P, P1 - P);
    end;
  end;
end;

function TIocpHttpFromDataItem.GetHeaderSize: Integer;
begin
  if (Len = 0) or (P = nil) or (FC = nil) then
    Result := 0
  else
    Result := FC - P;
end;

function TIocpHttpFromDataItem.GetIsFile: Boolean;
begin
  Result := Pos(StringA('filename="'), GetDisposition) > 0;
end;

function TIocpHttpFromDataItem.GetName: StringA;
var
  I: Integer;
  P, P1: PAnsiChar;
begin
  Result := GetDisposition;
  if Length(Result) > 0 then begin
    I := Pos(StringA('name="'), Result);
    if I > 0 then begin
      P := PAnsiChar(Result) + I + 5;
      P1 := StrScan(P, '"');
      if P1 <> nil then
        SetString(Result, P, P1 - P);
    end;
  end;
end;

procedure TIocpHttpFromDataItem.SaveContentToStream(const Stream: TStream);
begin
  if Assigned(Stream) and (Len > 0) and (P <> nil) or (FC > P) then
    Stream.Write(FC^, Len - HeaderSize);
end;

function TIocpHttpFromDataItem.GetContent: StringA;
begin
  if (Len > 0) and (P <> nil) or (FC > P) then
    SetString(Result, FC, Len - HeaderSize)
  else
    Result := '';
end;

{ TFileOnlyStream }

constructor TFileOnlyStream.Create(const AFileName: string);
begin
  inherited Create(_lopen(PAnsiChar(StringA(AFileName)), OF_READ));
end;

destructor TFileOnlyStream.Destroy;
begin
  if FHandle > 0 then _lclose(FHandle);
  inherited Destroy;
end;

{ TIocpHttpCookie }

constructor TIocpHttpCookie.Create;
begin
  FMaxAge := 604800; // 7 天
  FPath := '/';
end;

function TIocpHttpCookie.ToString: {$IFDEF UNICODE}string{$ELSE}StringA{$ENDIF};
begin
  Result := Format('%s=%s; path=%s', [FName, FValue, FPath]);
  if FExpires > 0 then   
    Result := Result + '; expires=' + DateTimeToGMTRFC822(FExpires);
  if FMaxAge > 0 then
    Result := Result + '; max-age=' + IntToStr(FMaxAge);
  if Length(FDoMain) > 0 then
    Result := Result + '; domain=' + string(FDoMain);
end;

{ TIocpHttpAccessControlAllow }

procedure TIocpHttpAccessControlAllow.Assign(Source: TPersistent);
begin
  if Source is TIocpHttpAccessControlAllow then begin
    Self.FEnabled := TIocpHttpAccessControlAllow(Source).FEnabled;
    Self.FOrigin := TIocpHttpAccessControlAllow(Source).FOrigin;
    Self.FMethods := TIocpHttpAccessControlAllow(Source).FMethods;
    Self.FHeaders := TIocpHttpAccessControlAllow(Source).FHeaders;
  end else
    inherited;
end;

constructor TIocpHttpAccessControlAllow.Create;
begin
  FOrigin := '*';
  FMethods := CS_DefaultMethods;
  FHeaders := CS_ACAH;
end;

procedure TIocpHttpAccessControlAllow.MakerHeader(
  const Data: TStringCatHelperA);
const
  CS_AccessControlAllowOrigin = 'Access-Control-Allow-Origin: ';
  CS_AccessControlAllowMethods = 'Access-Control-Allow-Methods: ';
  CS_AccessControlAllowHeaders = 'Access-Control-Allow-Headers: ';
begin
  if Enabled then begin
    Data.Cat(CS_AccessControlAllowOrigin);
    if FOrigin = '' then
      Data.Cat('*')
    else
      Data.Cat(FOrigin);
    Data.Cat(HTTPLineBreak);
    
    Data.Cat(CS_AccessControlAllowMethods);
    if FMethods = '' then
      Data.Cat(CS_DefaultMethods)
    else
      Data.Cat(FMethods);
    Data.Cat(HTTPLineBreak);

    Data.Cat(CS_AccessControlAllowHeaders);
    if FHeaders = '' then
      Data.Cat(CS_ACAH)
    else
      Data.Cat(FHeaders);
    Data.Cat(HTTPLineBreak);
  end;
end;

{ TIocpHttpWriter }

procedure TIocpHttpWriter.Clear;
begin
  FData.Reset;
end;

constructor TIocpHttpWriter.Create(const BufferSize: Cardinal);
begin
  FData := TStringCatHelper.Create(BufferSize);
  FCharset := hct_Unknown;
  {$IFDEF UNICODE}
  FCharset := hct_UTF16;
  {$ELSE}
  FCharset := hct_GB2312;
  {$ENDIF}
end;

destructor TIocpHttpWriter.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

procedure TIocpHttpWriter.Flush;
begin
  if Assigned(FOwner) then begin
    if FOwner.FOutWriter = Self then
      FOwner.FOutWriter := nil;
    FOwner.Send(Self, FOwner.FRequest.AcceptGzip and (FData.Position > 32), True)
  end else
    Exception.Create(strConnectNonExist);
end;

function TIocpHttpWriter.GetIsEmpty: Boolean;
begin
  Result := FData.Position = 0;
end;

function TIocpHttpWriter.GetPosition: Int64;
begin
  Result := FData.Position;
end;

function TIocpHttpWriter.SetCharset(V: TIocpHttpCharset): TIocpHttpWriter;
begin
  FCharset := V;
  Result := Self;
end;

function TIocpHttpWriter.ToString: string;
begin
  Result := FData.Value;
end;

function TIocpHttpWriter.Write(const Data: string): TIocpHttpWriter;
begin
  Result := Self;
  FData.Cat(Data);
end;

function TIocpHttpWriter.Write(const Data: Int64): TIocpHttpWriter;
begin
  Result := Self;
  FData.Cat(Data);
end;

function TIocpHttpWriter.Write(const Data: Double): TIocpHttpWriter;
begin
  Result := Self;
  FData.Cat(Data);
end;

function TIocpHttpWriter.Write(const Data: TIocpArrayString): TIocpHttpWriter;
var
  I: Integer;
begin
  Result := Self;
  FData.Cat('[');
  for I := 0 to High(Data) do begin
    if I > 0 then
      FData.Cat(',');
    FData.Cat(Data[I]);
  end;
  FData.Cat(']');
end;

function TIocpHttpWriter.Write(const Data: Cardinal): TIocpHttpWriter;
begin
  Result := Self;
  FData.Cat(Data);
end;

function TIocpHttpWriter.Write(const Data: Integer): TIocpHttpWriter;
begin
  Result := Self;
  FData.Cat(Data);
end;

initialization
  AppPath := ExtractFilePath(Paramstr(0));
  Workers := TIocpTask.GetInstance;
  InitMimeMap();

finalization
  Workers := nil;
  FreeAndNil(MimeMap);

end.


