{*******************************************************}
{                                                       }
{       IOCP HTTP 客户端组件单元                        }
{                                                       }
{       版权所有 (C) 2015 YangYxd                       }
{                                                       }
{*******************************************************}
{
  HTTP Client
  备注: 本源码参考了 Delphi XE10 System.Net.HttpClient
}

unit iocp.Http.Client;

{$I 'iocp.inc'}

{$DEFINE USE_COOKIES}  // 全局开关：是否使用 Cookies

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
{$DEFINE ANSISTRINGS}
{$DEFINE WEAK}
{$IFEND}

interface

uses
  {$IFDEF ANSISTRINGS}AnsiStrings, {$ENDIF}
  ZLibExGZ, iocp.Utils.Str, iocp.WinHttp, iocp.Utils.GMTTime,
  SyncObjs, SysUtils, Classes, Windows;

const
  DefaultAccept = 'text/html';
  DefaultAcceptEncoding = 'gzip, deflate';
  DefaultAcceptLanguage = 'zh-cn,zh;q=0.8,en-us;q=0.5,en;q=0.3';
  DefaultUserAgent = 'Mozilla/5.0 (Windows NT 5.1; rv:12.0) Gecko/20100101 Firefox/12.0';

const
  DefaultMaxRedirects = 5;         // 默认重定向最大次数
  
const
  S_CacheControl            = 'Cache-control';
  S_CharSet                 = 'charset';
  S_Connection              = 'Connection';
  S_ContentDisposition      = 'Content-Disposition';
  S_ContentEncoding         = 'Content-Encoding';
  S_ContentLanguage         = 'Content-Language';
  S_ContentRange            = 'Content-Range';
  S_ContentType             = 'Content-Type';
  S_ContentVersion          = 'Content-Version';
  S_ContentLength           = 'Content-Length';

  S_ETag                    = 'ETag';
  S_Pragma                  = 'Pragma';
  S_TransferEncoding        = 'Transfer-Encoding';

  S_Accept                  = 'Accept';
  S_AcceptCharSet           = 'Accept-Charset';
  S_AcceptEncoding          = 'Accept-Encoding';
  S_AcceptLanguage          = 'Accept-Language';
  S_AcceptPatch             = 'Accept-Patch';
  S_AcceptRanges            = 'Accept-Ranges';

  S_Host                    = 'Host';
  S_From                    = 'From';
  S_Referer                 = 'Referer';
  S_UserAgent               = 'User-Agent';
  S_Range                   = 'Range';
  S_MethodOverride          = 'X-HTTP-Method-Override';
  S_Username                = 'Username';
  S_Password                = 'Password';
  S_ProxyConnection         = 'Proxy-Connection';
  S_ProxyAuthorization      = 'Proxy-Authorization';
  S_ProxyAuthenticate       = 'Proxy-Authenticate';

  S_LastModified            = 'Last-Modified';
  S_IfModifiedSince         = 'If-Modified-Since';
  S_Authorization           = 'Authorization';
  S_Expires                 = 'Expires';
  S_Date                    = 'Date';
  S_Location                = 'Location';
  S_Server                  = 'Server';
  S_WWWAuthenticate         = 'WWW-Authenticate';
  {$IFDEF USE_COOKIES}
  S_Cookie                  = 'Cookie';
  S_SetCookie               = 'Set-Cookie';
  {$ENDIF}
  
  S_Bytes                   = 'bytes';

  S_GET                     = 'GET';
  S_PUT                     = 'PUT';
  S_POST                    = 'POST';
  S_HEAD                    = 'HEAD';
  S_DELETE                  = 'DELETE';
  S_OPTIONS                 = 'OPTIONS';
  S_TRACE                   = 'TRACE';

  S_KEEPALIVE               = 'KEEP-ALIVE';
  S_KEEPALIVE_L             = 'keep-alive';
  S_CLOSE                   = 'CLOSE';
  S_CLOSE_L                 = 'close';
  S_HTTP                    = 'http';
  S_HTTPS                   = 'https';

type
  EHTTPException = class(Exception);
  StringA = AnsiString;
  {$IFDEF UNICODE}
  StringW = UnicodeString;
  TIntArray = TArray<Integer>;
  {$ELSE}
  StringW = WideString;
  TIntArray = array of Integer;
  {$ENDIF}
  CharA = AnsiChar;
  CharW = WideChar;
  PCharA = PAnsiChar;
  PCharW = PWideChar;
  {$if CompilerVersion < 23}
  NativeUInt = Cardinal;
  IntPtr = NativeInt;
  {$ifend}
  CharWS = array of CharW;
  CharAS = array of CharA;

type
  /// <summary>
  /// URI
  /// </summary>
  TURI = record
  private
    FChangeURL: Boolean;
    FPort: Word;
    FUrl: string;
    FHost: string;
    FDocument: string;
    FParams: string;
    FBookmark: string;
    FPath: string;
    FScheme: string;
    FUserName: string;
    FPassword: string;
    function GetHost: string;
    function GetPort: Word;
    procedure SetURL(const Value: string);
    procedure Analytic;
    procedure Change;
    function GetParams: string;
    function GetBookmark: string;
    function GetPath: string;
    function GetDocumnet: string;
    function GetUrl: string;
    function GetScheme: string;
    procedure SetBookmark(const Value: string);
    procedure SetHost(const Value: string);
    procedure SetParams(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetPort(const Value: Word);
    procedure SetScheme(const Value: string);
    function GetPassword: string;
    function GetUserName: string;
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
  public
    constructor Create(const AURIStr: string);
    class function PathRelativeToAbs(const RelPath: string; const Base: TURI): string; static;
    function ToString(): string;
    function GetDocumentWithParams: string;
    function GetNewURL(haveAuthInfo: Boolean): string;
    function GetURLBase(): string;
    property Document: string read GetDocumnet;
    property URL: string read GetUrl write SetURL;
    property Port: Word read GetPort write SetPort;
    property Host: string read GetHost write SetHost;
    property Params: string read GetParams write SetParams;
    property Bookmark: string read GetBookmark write SetBookmark;
    property Path: string read GetPath write SetPath;
    property Scheme: string read GetScheme write SetScheme;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
  end;

type
  /// <summary>
  /// 证书列表
  /// </summary>
  PCertificate = ^TCertificate; 
  TCertificate = record
    /// <summary> Expiry date of the certificate</summary>
    Expiry: TDateTime;
    /// <summary> Start date of the certificate</summary>
    Start: TDateTime;
    /// <summary> Subject of the certificate</summary>
    Subject: string;
    /// <summary> Issuer of the certificate</summary>
    Issuer: string;
    /// <summary> ProtocolName of the certificate</summary>
    ProtocolName: string;
    /// <summary> Algorithm Signature of the certificate</summary>
    AlgSignature: string;
    /// <summary> Algorithm Encryption of the certificate</summary>
    AlgEncryption: string;
    /// <summary> Encryption's KeySize of the certificate</summary>
    KeySize: Integer;
  end;
  /// <summary> List of Certificates.</summary>
  TCertificateArray = array of TCertificate;
  TCertificateList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure AddItem(const Value: TCertificate); overload;
    procedure AddItem(const Value: PCertificate); overload;
  end;

  TPCCERTCONTEXTList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure AddItem(const Value: PCCERT_CONTEXT);
  end;

type
  THttpProtocolVersion = (hpv_UNKNOWN_HTTP, hpv_HTTP_1_0, hpv_HTTP_1_1, hpv_HTTP_2_0);
  THttpCharsetType = (hct_GB2312, hct_UTF8, hct_GBK, hct_ISO8859_1, hct_UTF16, hct_BIG5);
  
type
  TAuthTargetType = (att_Proxy, att_Server);
  TAuthPersistenceType = (apt_Request, apt_Client);
  TAuthSchemeType = (ast_Basic, ast_Digest, ast_NTLM, ast_Negotiate);

type
  TCredential = record
    /// <summary>凭据的目标类型</summary>
    AuthTarget: TAuthTargetType;
    /// <summary>可以使用此凭据的域</summary>
    Realm: string;
    /// <summary>URL定义凭证的范围</summary>
    URL: string; // Scope? Comment it...
    /// <summary>用户名</summary>
    UserName: string;
    /// <summary>密码</summary>
    Password: string;
    /// <summary>初始化</summary>
    constructor Create(AnAuthTarget: TAuthTargetType; const ARealm, AURL, AUserName, APassword: string);
  end;
  PCredential = ^TCredential;
  TCredentialArray = array of TCredential;

  /// <summary>
  /// Callback to request user credentials when server asks for them
  /// </summary>
  TCredentialAuthCallback = procedure(const Sender: TObject; AnAuthTarget: TAuthTargetType;
    const ARealm, AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean;
    var Persistence: TAuthPersistenceType);
  /// <summary>
  /// Callback to request user credentials when server asks for them
  /// </summary>
  TCredentialAuthEvent = procedure(const Sender: TObject; AnAuthTarget: TAuthTargetType;
    const ARealm, AURL: string; var AUserName, APassword: string; var AbortAuth: Boolean;
    var Persistence: TAuthPersistenceType) of object;

  /// <summary>
  /// 凭据存储类
  /// </summary>
  TCredentialsStorage = class(TList)
  private
    FLocker: TCriticalSection;
    function GetItem(Index: Integer): TCredential;
    procedure SetItem(Index: Integer; const Value: TCredential);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
    /// <summary>添加一个凭证到存储中</summary>
    function Add(const ACredential: TCredential): Boolean;
    /// <summary>删除符合条件的凭证</summary>
    function Remove(AnAuthTargetType: TAuthTargetType;
      const ARealm: string; const AURL: string = ''; const AUser: string = ''): Boolean;
    /// <summary>根据条件查找凭证，返回所有符合过滤条件的凭证</summary>
    function FindCredentials(AnAuthTargetType: TAuthTargetType; const ARealm: string; const AURL: string = '';
      const AUser: string = ''): TCredentialArray;
    /// <summary>检测是否存在指定的凭证</summary>
    function ExistCredentials(AnAuthTargetType: TAuthTargetType; const ARealm: string; const AURL: string = '';
      const AUser: string = ''): Boolean;
    /// <summary>根据条件查找凭证</summary>
    function FindAccurateCredential(AnAuthTargetType: TAuthTargetType; const ARealm: string; const AURL: string = '';
      const AUser: string = ''): TCredential;
    /// <summary>已存在的凭证数组</summary>
    property Credentials[Index: Integer]: TCredential read GetItem write SetItem;
    /// <summary>排序一个凭证数组</summary>
    class function SortCredentials(const ACredentials: TCredentialArray): TCredentialArray;

    procedure Sort(); reintroduce;
  end;

type
  PEntityRange = ^TEntityRange;
  TEntityRange = record
  private
    function GetText: String;
    procedure SetText(const AValue: String);
  public
    StartPos: Int64;
    EndPos: Int64;
    SuffixLength: Int64;
    property Text: String read GetText write SetText;
  end;

  /// <summary>
  /// 用于请求头中，指定第一个字节的位置和最后一个字节的位置，一般格式：
  //  Range:(unit=first byte pos)-[last byte pos]
  /// </summary>
  TEntityRanges = class(TList)
  protected
    FUnits: String;
    function GetRange(Index: Integer): PEntityRange;
    procedure SetRange(Index: Integer; AValue: PEntityRange);
    function GetText: String;
    procedure SetText(const AValue: String);
    procedure SetUnits(const AValue: String);
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    constructor Create(); reintroduce;
    function Add: PEntityRange; reintroduce;
    property Ranges[Index: Integer]: PEntityRange read GetRange write SetRange; default;
    property Text: String read GetText write SetText;
    property Units: String read FUnits write SetUnits;
  end;

type
  /// <summary>
  /// Name - Value 记录
  /// </summary>
  THeaderItem = record
    Name: string;
    Value: string;
    constructor Create(const AName, AValue: string);
  end;
  PHeaderItem = ^THeaderItem;
  THeaderArray = array of PHeaderItem;

  /// <summary>
  /// Http头信息列表
  /// </summary>
  THeadersList = class(TList)
  private
    function GetItem(Index: Integer): PHeaderItem;
    procedure SetItem(Index: Integer; const Value: PHeaderItem);
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure AddItem(const Name, Value: string; allowRepeat: Boolean = False);
    function Exist(const Name: string): Boolean; inline;
    function FindName(const Name: string): PHeaderItem;
    function GetString(const Name: string): string; inline;
    property Values[const Name: string]: string read GetValue write SetValue; default;
    property Items[Index: Integer]: PHeaderItem read GetItem write SetItem;
  end;

type
  /// <summary>
  /// Http 头部信息列表
  /// </summary>
  THttpHeaders = class(TObject)
  private
    FData: THeadersList;
    FLineBreak: string;
    function GetHaderItem(const Name: string): string;
    function GetLineBreak: string;
    function GetParam(const AName, AParam: string): string;
    function GetTextStr: string;
    procedure SetHaderItem(const Name, Value: string);
    procedure SetParam(const AName, AParam, AValue: string);
    procedure SetTextStr(const Value: string);
    function GetCount: Integer;
    function GetItem(Index: Integer): PHeaderItem;
    procedure SetItem(Index: Integer; const Value: PHeaderItem);
  public
    constructor Create(); overload;
    constructor Create(const Name, Value: string); overload;
    destructor Destroy; override;
    procedure Clear; virtual;
    function Add(const Name, Value: string): THttpHeaders;
    property Values[const Name: string]: string read GetHaderItem write SetHaderItem; default;
    property Params[const Name, Param: string]: string read GetParam write SetParam;
    property Text: string read GetTextStr write SetTextStr;
    property LineBreak: string read GetLineBreak write FLineBreak;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: PHeaderItem read GetItem write SetItem;
  end;

type
  /// <summary>
  /// 代理设置
  /// </summary>
  TProxySettings = record
  private
    FHost: string;
    FPort: Integer;
    FUserName: string;
    FPassword: string;
    FScheme: string;
    function GetCredential: TCredential;
  public
    /// <summary>Creates the proxy settings from the given arguments.</summary>
    constructor Create(const AURL: string); overload;
    constructor Create(const AHost: string; APort: Integer; const AUserName: string = ''; const APassword: string = ''; const AScheme: string = ''); overload;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property UserName: string read FUserName write FUserName;
    property Password: string read FPassword write FPassword;
    property Scheme: string read FScheme write FScheme;
    property Credential: TCredential read GetCredential;
  end;

type
  /// <summary>
  /// 执行请求返回状态
  /// </summary>
  TExecutionResult = (
    resp_Success,                  // 成功
    resp_UnknownError,             // 未知错误
    resp_ServerCertificateInvalid, // 服务器证书错误
    resp_ClientCertificateNeeded   // 客户端需要证书
  );
  // 内部状态
  InternalState = (is_Other, is_ProxyAuthRequired, is_ServerAuthRequired);

{$IFDEF USE_COOKIES}
type
  PCookie = ^TCookie;
  TCookie = record
  public
    Name: string;
    Value: string;
    Expires: TDateTime;
    Domain: string;
    Path: string;
    Secure: Boolean;
    HttpOnly: Boolean;
    function ToString: string;
  end;
  TCookiesArray = array of TCookie;
  
  /// <summary>
  /// Cookies 列表
  /// </summary>
  TCookies = class(TList)
  private
    function GetItem(Index: Integer): TCookie;
    procedure SetItem(Index: Integer; const Value: TCookie);
    function GetText: string;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure Add(const Value: TCookie);
    property Items[Index: Integer]: TCookie read GetItem write SetItem;
    property Text: string read GetText;
  end;

  TCookieManager = class
  private
    FMS: TStringCatHelper;
    FCS: TCriticalSection;
    FCookies: TStringList;
    function IncludeSubDomin(const Domain,SubDomain: string):Boolean;
    function GetSubDomain(const Domain: string):string;
    procedure AppendDataSet(PF: PCookie);
  public
    constructor Create();
    destructor Destroy();override;
    procedure Clear();
  public
    procedure AddCookies(const Value: string; const URL: TURI);
    function GetCookies(const URL: TURI): string; overload;
    function GetCookies(const URL: TURI; List: TCookies): Integer; overload;
  public
    property Cookies: TStringList read FCookies;
  end;
{$ENDIF}

type
  IHttp = interface end;
  THttpClient = class;
  THttpRequest = class;
  THttpResponse = class;

  THttpResult = record
  private
    FRequestInf: IHttp;
    FResponseInf: IHttp;
    function GetStatusCode: Integer;
  public
    Request: THttpRequest;
    Response: THttpResponse;
    property StatusCode: Integer read GetStatusCode;
  end;  

  TReceiveDataCallback = procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
  TReceiveDataEvent = procedure(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean) of object;
  TRequestErrorEvent = procedure(const Sender: TObject; const AError: string) of object;
  TRequestCompletedEvent = procedure(const Sender: TObject; const AResult: THttpResult) of object;
  TRequestRedirectEvent = procedure(const Sender: TObject; StatusCode: Integer; const NewURL: string) of object;

  /// <summary>
  /// HTTP状态
  /// </summary>
  THTTPState = record
    NeedProxyCredential: Boolean;
    ProxyCredential: TCredential;
    ProxyCredentials: TCredentialArray;
    ProxyIterator: Integer;
    NeedServerCredential: Boolean;
    ServerCredential: TCredential;
    ServerCredentials: TCredentialArray;
    ServerIterator: Integer;
    Status: InternalState;
    Redirections: Integer;
  end;

  THttpRequest = class(TInterfacedObject, IHttp)
  private
    FClient: THttpClient;
    FSourceStream: TStream;
    FHeaders: THttpHeaders;
    FLocalCredential: TCredential;

    FWConnect: HINTERNET;
    FWRequest: HINTERNET;
    FLastProxyAuthScheme: DWORD;
    FLastServerAuthScheme: DWORD;

    procedure SetURL(const Value: TURI);
    procedure SetWinProxySettings;

    procedure CreateHandles(const AURI: TURI);
    procedure UpdateRequest(const AURI: TURI);
    function GetSourceString: string;
  protected
    FURL: TURI;
    FMethodString: string;

    procedure DoPrepare; virtual;
    procedure DoCreate;
    function Execute(const AHeaders: THttpHeaders = nil; const AResponseContent: TStream = nil): THttpResponse;
    function GetHeaders: string;
    procedure DoReceiveDataProgress(AStatusCode: Integer; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
  public
    constructor Create(AClient: THttpClient; const ARequestMethod: string;
      const AURL: string); overload;
    constructor Create(AClient: THttpClient; const ARequestMethod: string;
      const AURI: TURI); overload;
    destructor Destroy; override;


    function GetHeaderValue(const AName: string): string; virtual;
    procedure SetHeaderValue(const AName, AValue: string); virtual;
    
    procedure AddHeader(const AName, AValue: string); virtual;
    function RemoveHeader(const AName: string): Boolean; virtual;

    property URL: TURI read FURL write SetURL;
    property SourceStream: TStream read FSourceStream write FSourceStream;
    property SourceString: string read GetSourceString;
    property Headers: string read GetHeaders;
  end;

  THttpResponse = class(TInterfacedObject, IHttp)
  private
    FHeader: string;
    FRequest: THttPRequest;
    FContentStream: TMemoryStream;
    FStream: TStream;
    FLastStreamPos: Int64;
    FRawDataSize: Int64;
    {$IFDEF USE_COOKIES}
    FCookies: TCookies;
    {$ENDIF}
    FStatusCode: Integer;
    FIsGZip: Boolean;
    FCharSet: THttpCharsetType;
    FCharSetText: string;
    function InnerGetHeaderValue(var P, PMax: PChar; const Name: string): string;
    function GetHeaderValue(const Name: string): string;
    function GetContentLength: Int64;
    function GetStatusCode: Integer;
    function GetVersion: THttpProtocolVersion;
    function GetContentEncoding: string;
    function GetContentLanguage: string;
    function GetLastModified: string;
    function GetDate: string;
    function GetMimeType: string;
    function GetContentType: string;
    function GetContentStringStream(var M: TMemoryStream): Boolean;
    function GetContentString: string;
    {$IFNDEF UNICODE}
    function GetContentStringW: StringW;
    {$ENDIF}
    function GetHeaderSize: Cardinal;
    {$IFDEF USE_COOKIES}
    function GetCookies: TCookies;
    {$ENDIF}
  protected
    function GetHeaders: string;
    /// <summary>Returns realm attribute from Server/Proxy Authentication response</summary>
    /// <remarks>Internal use only</remarks>
    function InternalGetAuthRealm: string;
    procedure DecodeHeader; virtual;
    procedure DoReadData(const AStream: TStream); virtual;
  public
    constructor Create(ARequest: THttPRequest; const AResponseStream: TStream);
    destructor Destroy; override;
    
    property Header: string read GetHeaders write FHeader;
    property HeaderSize: Cardinal read GetHeaderSize;
    property HeaderValue[const Name: string]: string read GetHeaderValue;

    property StatusCode: Integer read FStatusCode;
    property Version: THttpProtocolVersion read GetVersion;

    property RawDataSize: Int64 read FRawDataSize;
    property LastModified: string read GetLastModified;
    property Date: string read GetDate;
    property ContentLength: Int64 read GetContentLength;
    property ContentCharSet: string read FCharSetText;
    property ContentEncoding: string read GetContentEncoding;
    property ContentLanguage: string read GetContentLanguage;
    property ContentType: string read GetContentType;
    property MimeType: string read GetMimeType;

    property IsGZip: Boolean read FIsGZip;
    property CharSet: THttpCharsetType read FCharSet;

    property ContentStream: TMemoryStream read FContentStream;
    property ContentString: string read GetContentString;
    {$IFNDEF UNICODE}
    property ContentStringW: StringW read GetContentStringW;
    {$ENDIF}
    {$IFDEF USE_COOKIES}
    property Cookies: TCookies read GetCookies;
    {$ENDIF}
  end;

  /// <summary>
  /// Http 客户端
  /// </summary>
  THttpClient = class(TComponent)
  private
    FAllowCookies: Boolean;
    FHandleRedirects: Boolean;
    FAutoDecodeStr: Boolean;
    FConnectionTimeOut: Integer;
    FRecvTimeOut: Integer;

    FReceiveDataCallback: TReceiveDataCallback;
    FOnReceiveData: TReceiveDataEvent;
    FOnRequestError: TRequestErrorEvent;
    FOnRequestCompleted: TRequestCompletedEvent;
    FOnRequestRedirect: TRequestRedirectEvent;
    
    function GetAccept: string;
    function GetAcceptCharSet: string;
    function GetAcceptEncoding: string;
    function GetAcceptLanguage: string;
    function GetContentType: string;
    function GetUserAgent: string;
    procedure SetAccept(const Value: string);
    procedure SetAcceptCharSet(const Value: string);
    procedure SetAcceptEncoding(const Value: string);
    procedure SetAcceptLanguage(const Value: string);
    procedure SetContentType(const Value: string);
    procedure SetProxySettings(const Value: TProxySettings);
    procedure SetUserAgent(const Value: string);

    function ChooseAuthScheme(SupportedSchemes: DWORD): DWORD;
    {$IFDEF USE_COOKIES}
    procedure UpdateCookiesFromResponse(const AResponse: THttpResponse);
    {$ENDIF}
  protected
    FMaxRedirects: Integer;
    FCustomHeaders: THttpHeaders;
    FCredentialsStorage: TCredentialsStorage;
    FProxySettings: TProxySettings;
    {$IFDEF USE_COOKIES}
    FCookieMgr: TCookieManager;
    {$ENDIF}
    FDefautlCharset: THttpCharsetType;
    
    FWSession: HINTERNET;
    FCertificateList: TCertificateList;
    FWinCertList: TPCCERTCONTEXTList;
    
    procedure DoOnReceiveData(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure DoOnRequestError(const Sender: TObject; const AError: string);
    procedure DoOnRequestCompleted(const Sender: TObject; const AResult: THttpResult);

    function DoSetCredential(AnAuthTargetType: TAuthTargetType; const ARequest: THttpRequest;
      const ACredential: TCredential): Boolean; 

    function DoExecuteRequest(const ARequest: THTTPRequest; var AResponse: THTTPResponse;
      const AResponseContent: TStream): TExecutionResult; virtual;
    function DoProcessStatus(const ARequest: THTTPRequest; const  AResponse: THTTPResponse): Boolean;

    /// <summary>
    /// 获取指定URL的证书
    /// </summary>
    function GetCredentials(AuthTarget: TAuthTargetType;
      const ARealm, URL: string): TCredentialArray; virtual;

    function DoExecute(const AURL, AMethod: string; const AHeaders: THttpHeaders;
      const ASource: TStream = nil; const AResponseContent: TStream = nil): THttpResult;

    function SetServerCredential(const ARequest: THTTPRequest;
      const AResponse: THTTPResponse; var State: THTTPState): Boolean;
    function SetProxyCredential(const ARequest: THTTPRequest;
      const AResponse: THTTPResponse; var State: THTTPState): Boolean;

    function DoPost(const AMethod: string; const AURL: string; const ASource: string; ACharset: THttpCharsetType = hct_UTF8;
      const AResponseContent: TStream = nil; const AHeaders: THttpHeaders = nil): THttpResult; overload;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ExecuteHTTP(const ARequest: THttpRequest; const AResponseContent: TStream): THttpResponse;

    // ------------------------------------------- //
    // Standard HTTP Methods  (标准HTTP方法)
    // ------------------------------------------- //
    /// <summary>Send 'DELETE' command to url</summary>
    function Delete(const AURL: string; const AResponseContent: TStream = nil; const AHeaders: THttpHeaders = nil): THttpResult;

    /// <summary>Send 'OPTIONS' command to url</summary>
    function Options(const AURL: string; const AResponseContent: TStream = nil; const AHeaders: THttpHeaders = nil): THttpResult;

    /// <summary>Send 'GET' command to url</summary>
    function Get(const AURL: string; const AResponseContent: TStream = nil;
      const AHeaders: THttpHeaders = nil): THttpResult;

    /// <summary>Check if the server has download resume feature</summary>
    function CheckDownloadResume(const AURL: string): Boolean;

    /// <summary>Send 'GET' command to url adding Range header</summary>
    /// <remarks>It's used for resume downloads</remarks>
    function GetRange(const AURL: string; AStart: Int64; AnEnd: Int64 = -1; const AResponseContent: TStream = nil;
      const AHeaders: THttpHeaders = nil): THttpResult;

    /// <summary>Send 'TRACE' command to url</summary>
    function Trace(const AURL: string; const AResponseContent: TStream = nil; const AHeaders: THttpHeaders = nil): THttpResult;

    /// <summary>Send 'HEAD' command to url</summary>
    function Head(const AURL: string; const AHeaders: THttpHeaders = nil): THttpResult;

    /// <summary>Post TStrings values adding multipart info</summary>
    function Post(const AURL: string; const ASource: string; ACharset: THttpCharsetType = hct_UTF8;
      const AResponseContent: TStream = nil; const AHeaders: THttpHeaders = nil): THttpResult; overload; inline;
    /// <summary>Post a stream without multipart info</summary>
    function Post(const AURL: string; const ASource: TStream; const AResponseContent: TStream = nil;
      const AHeaders: THttpHeaders = nil): THttpResult; overload;
    /// <summary>Post a raw file without multipart info</summary>
    function PostFile(const AURL: string; const ASourceFile: string; const AResponseContent: TStream = nil;
      const AHeaders: THttpHeaders = nil): THttpResult;

    /// <summary>Send 'PUT' command to url</summary>
    function Put(const AURL: string; const ASource: TStream = nil; const AResponseContent: TStream = nil;
      const AHeaders: THttpHeaders = nil): THttpResult; overload;
    /// <summary>Send 'PUT' command to url</summary>
    function Put(const AURL: string; const ASource: string; ACharset: THttpCharsetType = hct_UTF8;
      const AResponseContent: TStream = nil; const AHeaders: THttpHeaders = nil): THttpResult; overload; inline;

    // 是否允许响应内容自动字符串转码
    property AutoDecodeStr: Boolean read FAutoDecodeStr write FAutoDecodeStr;
    // 是否允许记录Cookies
    property AllowCookies: Boolean read FAllowCookies write FAllowCookies;
    // 是否支持重定向
    property HandleRedirects: Boolean read FHandleRedirects write FHandleRedirects;
    // 代理设置
    property ProxySettings: TProxySettings read FProxySettings write SetProxySettings;

    property Accept: string read GetAccept write SetAccept;
    property AcceptCharSet: string read GetAcceptCharSet write SetAcceptCharSet;
    property AcceptEncoding: string read GetAcceptEncoding write SetAcceptEncoding;
    property AcceptLanguage: string read GetAcceptLanguage write SetAcceptLanguage;
    property ContentType: string read GetContentType write SetContentType;
    property UserAgent: string read GetUserAgent write SetUserAgent;
    property MaxRedirects: Integer read FMaxRedirects write FMaxRedirects;
    property DefautlCharset: THttpCharsetType read FDefautlCharset write FDefautlCharset;

    /// <summary>
    /// 连接超时：单位（毫秒），连接远程主机时的超时设置。默认30秒
    /// </summary>
    property ConnectionTimeOut: Integer read FConnectionTimeOut write FConnectionTimeOut;
    /// <summary>
    /// 接收数据超时：单位（毫秒），默认没有超时
    /// </summary>
    property RecvTimeOut: Integer read FRecvTimeOut write FRecvTimeOut;

    property ReceiveDataCallBack: TReceiveDataCallback read FReceiveDataCallback write FReceiveDataCallback;
  published
    property OnReceiveData: TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
    property OnRequestError: TRequestErrorEvent read FOnRequestError write FOnRequestError;
    property OnRequestCompleted: TRequestCompletedEvent read FOnRequestCompleted write FOnRequestCompleted;
    property OnRequestRedirect: TRequestRedirectEvent read FOnRequestRedirect write FOnRequestRedirect;
  end;

  THttpLocalClient = class(THttpClient)
  protected
    procedure DoError(const Sender: TObject; const AError: string);
  end;

  TIocpHttpClient = class(THttpClient)
  published
    property AllowCookies;
    property HandleRedirects;

    property Accept;
    property AcceptCharSet;
    property AcceptEncoding;
    property AcceptLanguage;
    property ContentType;
    property UserAgent;

    property RecvTimeOut;
    property ConnectionTimeOut;

    property MaxRedirects;
    property DefautlCharset;

    property OnReceiveData;
    property OnRequestError;
    property OnRequestCompleted;
  end;


// ------------------------------------------- //
// 简化使用接口
// ------------------------------------------- //

// 本地Http客户端 (以下几个Http命令函数会使用此客户端)
function LocalClient: THttpClient;

function HttpGet(const AURL: string; AHeaders: THttpHeaders = nil): string; overload;
function HttpGet(const AURL: string; OutStream: TStream; AHeaders: THttpHeaders = nil): Boolean; overload;

function HttpPost(const AURL, AData: string; ACharset: THttpCharsetType = hct_GB2312; AHeaders: THttpHeaders = nil): string; overload;
function HttpPost(const AURL: string; AData: TStream; OutStream: TStream; AHeaders: THttpHeaders = nil): Boolean; overload;
function HttpPostFile(const AURL, AFileName: string; AHeaders: THttpHeaders = nil): string; overload;

function HttpPut(const AURL, AData: string; ACharset: THttpCharsetType = hct_GB2312; AHeaders: THttpHeaders = nil): string; overload;
function HttpPut(const AURL: string; AData: TStream; OutStream: TStream; AHeaders: THttpHeaders = nil): Boolean; overload;

function HttpGetRange(const AURL: string; AStart: Int64; AnEnd: Int64 = -1; AHeaders: THttpHeaders = nil): string; overload;
function HttpGetRange(const AURL: string; OutStream: TStream; AStart: Int64; AnEnd: Int64 = -1; AHeaders: THttpHeaders = nil): Boolean; overload;

procedure Register;

implementation

resourcestring
  sRSURINoHost = 'Host field is empty.';
  SNetUriInvalid = 'Invalid URL: "%s"';
  SNetHttpClientHandleError = 'Error obtaining session handle';
  SNetHttpRequestConnectError = 'Error connecting to server: %s';
  SNetHttpRequestOpenError = 'Error opening request: (%d) %s';
  SNetHttpHeadersError = 'Error querying headers: (%d) %s';
  SNetHttpRequestReadDataError = 'Error reading data: (%d) %s';
  SCredentialInvalidUserPassword = 'Credential without user or password';
  SNetUriURLAlreadyAssigned = 'URL already assigned';
  SNetHttpRequestAddHeaderError = 'Error adding header: (%d) %s';
  SNetHttpRequestRemoveHeaderError = 'Error removing header: (%d) %s';
  SNetHttpClientSendError = 'Error sending data: (%d) %s';
  SNetHttpMaxRedirections = 'Maximum number of redirections (%d) exceeded';
  SNetHttpClientUnknownError = 'Execution of request terminated with unknown error';
  SNetHttpInvalidServerCertificate = 'Server Certificate Invalid or not present';
  SNetHttpUnspecifiedCertificate = 'Unspecified certificate from client';

type
  TPointerArray = array of Pointer;
  UInt16 = Word;
  UInt32 = Cardinal;
  PtrInt = Integer;
  
var
  FWinHttpHandle: THandle;
  FLocalClient: THttpLocalClient = nil;
  FLocalLoker: TCriticalSection;

const
  BUFFERSIZE = 64 * 1024;  // Usual TCP Window Size

const
  ComPageName = 'YxdIOCP';

procedure Register;
begin
  RegisterComponents(ComPageName, [TIocpHttpClient]);
end;

function LocalClient: THttpClient;
begin
  if Assigned(FLocalClient) then
    Result := FLocalClient
  else begin
    FLocalLoker.Enter;
    if not Assigned(FLocalClient) then begin
      FLocalClient := THttpLocalClient.Create(nil);
      FLocalClient.OnRequestError := FLocalClient.DoError;
    end;
    FLocalLoker.Leave;
    Result := FLocalClient
  end;
end;

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

function HttpGet(const AURL: string; AHeaders: THttpHeaders): string;
var
  Resp: THttpResult;
begin
  Resp := LocalClient.Get(AURL, nil, AHeaders);
  if Assigned(Resp.Response) then
    Result := Resp.Response.ContentString
  else
    Result := '';
end;

function HttpGet(const AURL: string; OutStream: TStream; AHeaders: THttpHeaders): Boolean;
var
  Resp: THttpResult;
begin
  Resp := LocalClient.Get(AURL, OutStream, AHeaders);
  Result := Resp.StatusCode = 200;
end;

function HttpGetRange(const AURL: string; AStart: Int64; AnEnd: Int64 = -1; AHeaders: THttpHeaders = nil): string;
var
  Resp: THttpResult;
begin
  Resp := LocalClient.GetRange(AURL, AStart, AnEnd, nil, AHeaders);
  if Assigned(Resp.Response) then
    Result := Resp.Response.ContentString
  else
    Result := '';
end;

function HttpGetRange(const AURL: string; OutStream: TStream; AStart: Int64; AnEnd: Int64 = -1; AHeaders: THttpHeaders = nil): Boolean;
var
  Resp: THttpResult;
begin
  Resp := LocalClient.GetRange(AURL, AStart, AnEnd, OutStream, AHeaders);
  Result := Resp.StatusCode = 200;
end;

function HttpPost(const AURL, AData: string; ACharset: THttpCharsetType; AHeaders: THttpHeaders): string;
var
  Resp: THttpResult;
begin
  Resp := LocalClient.Post(AURL, AData, ACharset, nil, AHeaders);
  if Assigned(Resp.Response) then
    Result := Resp.Response.ContentString
  else
    Result := '';
end;

function HttpPost(const AURL: string; AData: TStream; OutStream: TStream; AHeaders: THttpHeaders = nil): Boolean;
var
  Resp: THttpResult;
begin
  Resp := LocalClient.Post(AURL, AData, OutStream, AHeaders);
  Result := Resp.StatusCode = 200;
end;

function HttpPostFile(const AURL, AFileName: string; AHeaders: THttpHeaders = nil): string; overload;
var
  Resp: THttpResult;
begin
  Resp := LocalClient.PostFile(AURL, AFileName, nil, AHeaders);
  if Assigned(Resp.Response) then
    Result := Resp.Response.ContentString
  else
    Result := '';
end;

function HttpPut(const AURL, AData: string; ACharset: THttpCharsetType; AHeaders: THttpHeaders): string;
var
  Resp: THttpResult;
begin
  Resp := LocalClient.Put(AURL, AData, ACharset, nil, AHeaders);
  if Assigned(Resp.Response) then
    Result := Resp.Response.ContentString
  else
    Result := '';
end;

function HttpPut(const AURL: string; AData: TStream; OutStream: TStream; AHeaders: THttpHeaders = nil): Boolean;
var
  Resp: THttpResult;
begin
  Resp := LocalClient.Put(AURL, AData, OutStream, AHeaders);
  Result := Resp.StatusCode = 200;
end;

// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------
// ---------------------------------------------------------------------------

function SysErrorMessage(ErrorCode: Cardinal; AModuleHandle: THandle = 0): string;
var
  Buffer: PChar;
  Len: Integer;
  Flags: DWORD;
begin
  Flags := FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_IGNORE_INSERTS or
    FORMAT_MESSAGE_ARGUMENT_ARRAY or
    FORMAT_MESSAGE_ALLOCATE_BUFFER;

  if AModuleHandle <> 0 then
    Flags := Flags or FORMAT_MESSAGE_FROM_HMODULE;
  Len := FormatMessage(Flags, Pointer(AModuleHandle), ErrorCode, 0, @Buffer, 0, nil);
  try
    while (Len > 0) and ((Buffer[Len - 1] <= #32) or (Buffer[Len - 1] = '.')) do
      Dec(Len);
    SetString(Result, Buffer, Len);
  finally
    LocalFree(HLOCAL(Buffer));
  end;
end;

function PosHeaderSubItem(const AHeaderLine, ASubItem: String;
  var PStart, PEnd: PChar): Boolean;
var
  P, PMax, Ps, P1, P2: PChar;
  SL: Integer;
begin
  Result := False;
  P := PChar(AHeaderLine);
  PMax := P + Length(AHeaderLine);
  SL := Length(ASubItem);
  if (SL = 0) or (PMax = P) or (P = nil) then Exit;  
  Ps := PChar(ASubItem);
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

function ExtractHeaderSubItem(const AHeaderLine, ASubItem: String): String;
var
  P, P1: PChar;
begin
  if PosHeaderSubItem(AHeaderLine, ASubItem, P, P1) then
    SetString(Result, P, P1 - P)
  else
    Result := '';
end;

function ReplaceHeaderSubItem(const AHeaderLine, ASubItem, AValue: string): string;
var
  PA, PMax, P, P1: PChar;
begin
  if Length(AHeaderLine) = 0 then
    Result := ''
  else begin
    if PosHeaderSubItem(AHeaderLine, ASubItem, P, P1) then begin
      PA := PChar(AHeaderLine);
      PMax := P + Length(AHeaderLine);
      Result := PCharToString(PA, P - PA) + AValue + PCharToString(P1, PMax - P1);
    end else
      Result := AHeaderLine + '; ' + ASubItem + '=' + AValue;
  end;
end;

function StartsWith(const Src, Value: string; IgnoreCase: Boolean = False): Boolean; inline;
begin
  if Value = '' then
    Result := False
  else
    if not IgnoreCase then
      Result := StrLComp(PChar(Src), PChar(Value), Length(Value)) = 0
    else
      Result := StrLIComp(PChar(Src), PChar(Value), Length(Value)) = 0;
end;

function StrLICompH(const Src, Value: string): Boolean; inline;
begin
  Result := StrLIComp(PChar(Src), PChar(Value), Length(Value)) = 0;
end;

procedure QuickSort(var Values: TPointerArray; const Comparer: TListSortCompare; L, R: Integer); overload;
var
  I, J: Integer;
  pivot, temp: Pointer;
begin
  if (Length(Values) = 0) or ((R - L) <= 0) then
    Exit;
  repeat
    I := L;
    J := R;
    pivot := Values[L + (R - L) shr 1];
    repeat
      while Comparer(Values[I], pivot) < 0 do Inc(I);
      while Comparer(Values[J], pivot) > 0 do Dec(J);
      if I <= J then begin
        if I <> J then begin
          temp := Values[I];
          Values[I] := Values[J];
          Values[J] := temp;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(Values, Comparer, L, J);
    L := I;
  until I >= R;
end;

procedure QuickSort(var Values: TPointerArray; const Comparer: TListSortCompare); overload; inline;
begin
  QuickSort(Values, Comparer, Low(Values), High(Values));
end;

function PCharWToString(P: Pointer; Size: Integer): StringA;
const
  CodePage = 936;
var
  Len: Integer;
begin
  Len := WideCharToMultiByte(CodePage,
    WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,  
    P, -1, nil, 0, nil, nil);  
  SetLength(Result, Len - 1);
  if Len > 1 then
    WideCharToMultiByte(CodePage,  
      WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR,  
      P, -1, @Result[1], Len - 1, nil, nil);
end;

function ReadHeader(ARequest: HINTERNET; AHeaderFlag: DWORD; const AHeaderName: string = ''): string;
var
  LSize: Cardinal;
  Buff: TBytes;
  LFlags: DWORD;
  LHeaderName: PWideChar;
begin
  LFLags := AHeaderFlag;
  if AHeaderName <> '' then
  begin
    LFLags := LFLags or WINHTTP_QUERY_CUSTOM;
    LHeaderName := PWideChar(StringW(AHeaderName));
  end
  else
    LHeaderName := WINHTTP_HEADER_NAME_BY_INDEX;

  LSize := 0;
  WinHttpQueryHeaders(ARequest, LFLags, LHeaderName, nil, LSize, WINHTTP_NO_HEADER_INDEX);

  if GetLastError = ERROR_WINHTTP_HEADER_NOT_FOUND then
    Result := ''
  else
  begin
    if GetLastError <> ERROR_INSUFFICIENT_BUFFER then
      raise EHTTPException.CreateResFmt(@SNetHttpHeadersError, [GetLastError, SysErrorMessage(GetLastError, FWinHttpHandle)]);

    SetLength(Buff, LSize);
    if WinHttpQueryHeaders(ARequest, LFLags, LHeaderName, @Buff[0], LSize, WINHTTP_NO_HEADER_INDEX) = False then
      raise EHTTPException.CreateResFmt(@SNetHttpHeadersError, [GetLastError, SysErrorMessage(GetLastError, FWinHttpHandle)]);

    {$IFDEF UNICODE}
    Result := TEncoding.Unicode.GetString(Buff, 0, LSize);
    {$ELSE}
    Result := PCharWToString(@Buff[0], LSize);
    {$ENDIF}
  end;
end;   

function DecompressGZip(inStream, outStream: TStream): Boolean;
begin
  Result := False;
  if (inStream.Size <= 0) or (not Assigned(outStream)) then Exit;
  try
    inStream.Seek(0, 0);
    outStream.Position := 0;
    GZDecompressStream(inStream, outStream);
    Result := True;
  except
    on E: Exception do
      OutputDebugString(PChar(E.Message));
  end;
end;

procedure SplitDelimitedString(const AData: string; AStrings: TStrings; ATrim: Boolean;
  const ADelim: string = ' ');
begin

end;

{ TURI }

procedure TURI.Analytic;
var
  i, j: Integer;
  LBuffer, LURI: string;
begin
  FHost := '';
  FPort := 0;
  FDocument := '';
  FParams := '';
  FBookmark := '';
  FScheme := '';
  FUserName := '';
  FPassword := '';
  LURI := FUrl;
  i := Pos('://', LURI);
  if i > 0 then begin
    FScheme := Copy(LURI, 1, i  - 1);
    Delete(LURI, 1, i + 2);
  end else
    FScheme := S_HTTP;
  LBuffer := Fetch(LURI, '/', True);
  i := Pos('@', LBuffer);
  FPassword := Copy(LBuffer, 1, i - 1);
  if i > 0 then Delete(LBuffer, 1, i);
  FUserName := Fetch(FPassword, ':', True);
  if Length(FUserName) = 0 then
    FPassword := '';
  i := Pos('[', LBuffer);
  if i > 0 then j := Pos(']', LBuffer) else j := 0;
  if (j > i) then begin
    FHost := Fetch(LBuffer,']');
    Fetch(FHost,'[');
    Fetch(LBuffer,':');
  end else
    FHost := Fetch(LBuffer, ':', True);
  if (LowerCase(FScheme) = S_HTTP) then
    FPort := StrToIntDef(LBuffer, 80)
  else if (LowerCase(FScheme) = S_HTTPS) then
    FPort := StrToIntDef(LBuffer, 443);

  i := Pos('?', LURI);
  if i > 0 then
    j := RPosStr('/', LURI, i)
  else begin
    i := Pos('=', LURI);    {Do not Localize}
    if i > 0 then
      j := RPosStr('/', LURI, i)
    else
      j := RPosStr('/', LURI) - 1;    {Do not Localize}
  end;

  FPath := '/' + Copy(LURI, 1, j);
  if i > 0 then begin
    FDocument := Copy(LURI, 1, i - 1);
    Delete(LURI, 1, i - 1);
    FParams := LURI;
  end else
    FDocument := LURI;
  Delete(FDocument, 1, j);
  FBookmark := FDocument;
  FDocument := Fetch(FBookmark, '#');
  if FPort < 1 then FPort := 80;  
end;

procedure TURI.Change;
begin
  if FPort = 0 then Analytic;
  FChangeURL := True;
end;

constructor TURI.Create(const AURIStr: string);
begin
  FUrl := AURIStr;
  FPort := 0;
  FChangeURL := True;
end;

function TURI.GetBookmark: string;
begin
  if FPort = 0 then Analytic;
  Result := FBookmark;
end;

function TURI.GetDocumentWithParams: string;
begin
  if FPort = 0 then Analytic;
  Result := FPath + FDocument + FParams;
end;

function TURI.GetDocumnet: string;
begin
  if FPort = 0 then Analytic;
  Result := FDocument;
end;

function TURI.GetHost: string;
begin
  if FPort = 0 then Analytic;
  Result := FHost;
end;

function TURI.GetNewURL(haveAuthInfo: Boolean): string;
begin
  if Scheme <> '' then
    Result := Scheme + '://'
  else
    Result := '';
  if haveAuthInfo and (FUserName <> '') then begin
    Result := Result + FUserName;
    if FPassword <> '' then
      Result := Result + ':' + FPassword;
    Result := Result + '@';
  end;
  if FHost = '' then
    raise EHTTPException.Create(sRSURINoHost);
  Result := Result + FHost;
  if (LowerCase(FScheme) = S_HTTP) then begin
    if (FPort <> 80) then
      Result := Result + ':' + IntToStr(FPort);
  end else if (LowerCase(FScheme) = S_HTTPS) then begin
    if (FPort <> 443) then
      Result := Result + ':' + IntToStr(FPort);
  end;
  Result := Result + GetDocumentWithParams;
  if haveAuthInfo and (FBookmark <> '') then
    Result := Result + '#' + FBookmark;
end;

function TURI.GetParams: string;
begin
  if FPort = 0 then Analytic;
  Result := FParams;
end;

function TURI.GetPassword: string;
begin
  if FPort = 0 then Analytic;
  Result := FPassword;
end;

function TURI.GetPath: string;
begin
  if FPort = 0 then Analytic;
  Result := FPath;
end;

function TURI.GetPort: Word;
begin
  if FPort = 0 then Analytic;
  Result := FPort;
end;

function TURI.GetScheme: string;
begin
  if FPort = 0 then Analytic;
  Result := FScheme;
end;

function TURI.GetUrl: string;
begin
  if FChangeURL then begin
    FUrl := GetNewURL(True);
    FChangeURL := False;
  end;
  Result := FUrl;
end;

function TURI.GetURLBase: string;
begin
  if ((LowerCase(Scheme) = S_HTTP) and (Port = 80)) or
    ((LowerCase(Scheme) = S_HTTPS) and (Port = 443)) then
    Result := Scheme + '://' + Host
  else
    Result := Scheme + '://' + Host + ':' + IntToStr(Port);
end;

function TURI.GetUserName: string;
begin
  if FPort = 0 then Analytic;
  Result := FUserName;
end;

class function TURI.PathRelativeToAbs(const RelPath: string;
  const Base: TURI): string;
var
  P, PMax, P1, P2: PChar;
begin
  Result := '';
  P := PChar(RelPath);
  PMax := P + Length(RelPath);
  if P = PMax then Exit;
  P1 := P;
  P2 := P;
  while (P < PMax) and (P^ <> ':') do Inc(P);
  while (P2 < PMax) and (P2^ <> '/') do Inc(P2);
  if (P = PMax) or (P2 <= P) then begin
    // 相对路径
    if P2 = P1 then // 以 '/' 开头
      Result := Base.GetURLBase
    else begin
      Result := Base.GetURLBase + Base.Path;
      P2 := PChar(Result);
      P := P2 + Length(Result);
      while (P > P2) and (P^ <> '/') do Dec(P);
      Delete(Result, P - P2 + 1, Length(Result) - (P - P2) - 1);      
    end;  
    Result := Result + RelPath;
  end else
    Result := RelPath;  // 绝对路径
end;

procedure TURI.SetBookmark(const Value: string);
begin
  Change;
  FBookmark := Value;
end;

procedure TURI.SetHost(const Value: string);
begin
  Change;
  FHost := Value;
end;

procedure TURI.SetParams(const Value: string);
begin
  Change;
  FParams := Value;
end;

procedure TURI.SetPassword(const Value: string);
begin
  Change;
  FPassword := Value;
end;

procedure TURI.SetPath(const Value: string);
begin
  Change;
  FPath := Value;
end;

procedure TURI.SetPort(const Value: Word);
begin
  if Value < 1 then Exit;
  Change;
  FPort := Value;
end;

procedure TURI.SetScheme(const Value: string);
begin
  Change;
  FScheme := Value;
end;

procedure TURI.SetURL(const Value: string);
begin
  FUrl := Value;
  FPort := 0;
  FChangeURL := False;
end;

procedure TURI.SetUserName(const Value: string);
begin
  Change;
  FUserName := Value;
end;

function TURI.ToString: string;
begin
  Result := GetNewURL(True);
end;

{ TEntityRange }

function TEntityRange.GetText: string;
begin
  if (StartPos >= 0) or (EndPos >= 0) then begin
    if EndPos >= 0 then
      Result := IntToStr(StartPos) + '-' + IntToStr(EndPos)  {do not localize}
    else
      Result := IntToStr(StartPos) + '-' {do not localize}
  end else if SuffixLength >= 0 then
    Result := '-' + IntToStr(SuffixLength)
  else
    Result := '';
end;

procedure TEntityRange.SetText(const AValue: String);
var
  LValue, S: String;
begin
  LValue := Trim(AValue);
  if LValue <> '' then begin
    S := Fetch(LValue, '-'); {do not localize}
    if S <> '' then begin
      StartPos := StrToInt64Def(S, -1);
      EndPos := StrToInt64Def(Fetch(LValue), -1);
      SuffixLength := -1;
    end else begin
      StartPos := -1;
      EndPos := -1;
      SuffixLength := StrToInt64Def(Fetch(LValue), -1);
    end;
  end else begin
    StartPos := -1;
    EndPos := -1;
    SuffixLength := -1;
  end;
end;

{ TEntityRanges }

function TEntityRanges.Add: PEntityRange;
begin
  New(Result);
  inherited Add(Result);
end;

constructor TEntityRanges.Create();
begin
  inherited Create();
  FUnits := S_Bytes;
end;

function TEntityRanges.GetRange(Index: Integer): PEntityRange;
begin
  Result := inherited Items[Index];
end;

function TEntityRanges.GetText: String;
var
  I: Integer;
  S: string;
begin
  Result := '';
  for I := 0 to Count-1 do begin
    S := Ranges[I].Text;
    if S <> '' then begin
      if Result <> '' then
        Result := Result + ','; {do not localize}
      Result := Result + S;
    end;
  end;
  if Result <> '' then
    Result := FUnits + '=' + Result; {do not localize}
end;

procedure TEntityRanges.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(Ptr);
end;

procedure TEntityRanges.SetRange(Index: Integer; AValue: PEntityRange);
begin
  inherited Items[Index] := AValue;
end;

procedure TEntityRanges.SetText(const AValue: String);
var
  LUnits, LTmp: String;
  LRanges: TStringList;
  I: Integer;
  LRange: PEntityRange;
begin
  LTmp := Trim(AValue);
  Clear;
  if Pos('=', LTmp) > 0 then  {do not localize}
    LUnits := Fetch(LTmp, '=');
  SetUnits(LUnits);
  LRanges := TStringList.Create;
  try
    SplitDelimitedString(LTmp, LRanges, True, ','); {do not localize}
    for I := 0 to LRanges.Count-1 do begin
      LTmp := Trim(LRanges[I]);
      if LTmp <> '' then begin
        LRange := Add;
        try
          LRange.Text := LTmp;
        except
          Dispose(LRange);
          raise;
        end;
      end;
    end;
  finally
    LRanges.Free;
  end;
end;

procedure TEntityRanges.SetUnits(const AValue: String);
var
  LUnits: String;
begin
  LUnits := Trim(AValue);
  if LUnits <> '' then
    FUnits := LUnits
  else
    FUnits := S_Bytes;
end;

{ THeaderItem }

constructor THeaderItem.Create(const AName, AValue: string);
begin
  Name := AName;
  Value := AValue;
end;

{ THeadersList }

procedure THeadersList.AddItem(const Name, Value: string; allowRepeat: Boolean);
var
  Item: PHeaderItem;
begin
  if not allowRepeat then begin
    Item := FindName(Name);
    if Item <> nil then begin
      Item.Value := Value;
      Exit;
    end;
  end;
  New(Item);
  Item.Create(Name, Value);
  inherited Add(Item);
end;

function THeadersList.Exist(const Name: string): Boolean;
begin
  Result := FindName(Name) <> nil;
end;

function THeadersList.FindName(const Name: string): PHeaderItem;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do begin
    if TextIsSame(Name, PHeaderItem(inherited Items[I]).Name) then begin
      Result := inherited Items[I];
      Exit;
    end;
  end;
  Result := nil;
end;

function THeadersList.GetItem(Index: Integer): PHeaderItem;
begin
  Result := inherited Items[Index];
end;

function THeadersList.GetString(const Name: string): string;
begin
  Result := GetValue(Name);
end;

function THeadersList.GetValue(const Name: string): string;
var
  Item: PHeaderItem;
begin
  Item := FindName(Name);
  if Item <> nil then
    Result := Item.Value
  else
    Result := '';
end;

procedure THeadersList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and (Ptr <> nil) then begin
    PHeaderItem(Ptr).Name := '';
    PHeaderItem(Ptr).Value := '';
    Dispose(Ptr);
  end;
end;

procedure THeadersList.SetItem(Index: Integer; const Value: PHeaderItem);
begin
  PHeaderItem(inherited Items[Index])^ := Value^;
end;

procedure THeadersList.SetValue(const Name, Value: string);
var
  Item: PHeaderItem;
begin
  Item := FindName(Name);
  if Item <> nil then
    Item.Value := Value
  else if Length(Value) > 0 then begin
    New(Item);
    Item.Create(Name, Value);
    inherited Add(Item);
  end;
end;

{ THttpHeaders }

function THttpHeaders.Add(const Name, Value: string): THttpHeaders;
begin
  if Length(Name) > 0 then
    FData[Name] := Value;
  Result := Self;
end;

procedure THttpHeaders.Clear;
begin
  FData.Clear;
end;

constructor THttpHeaders.Create();
begin
  FData := THeadersList.Create();
  Clear();
end;

constructor THttpHeaders.Create(const Name, Value: string);
begin
  FData := THeadersList.Create();
  Clear();
  if Length(Name) > 0 then
    FData[Name] := Value;
end;

destructor THttpHeaders.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

function THttpHeaders.GetCount: Integer;
begin
  Result := FData.Count;
end;

function THttpHeaders.GetHaderItem(const Name: string): string;
begin
  Result := FData.GetString(Name);
end;

function THttpHeaders.GetItem(Index: Integer): PHeaderItem;
begin
  Result := FData.Items[index];
end;

function THttpHeaders.GetLineBreak: string;
begin
  if Length(FLineBreak) = 0 then
    Result := sLineBreak
  else
    Result := FLineBreak;
end;

function THttpHeaders.GetParam(const AName, AParam: string): string;
var
  s: string;
begin
  s := Values[AName];
  if s <> '' then
    Result := ExtractHeaderSubItem(s, AParam)
  else
    Result := '';
end;

function THttpHeaders.GetTextStr: string;
var
  LB: string;
  SB: TStringCatHelper;
  Item: PHeaderItem;
begin
  LB := LineBreak;
  SB := TStringCatHelper.Create;
  for Item in FData do begin
    SB.Cat(Item.Name).Cat(': ');
    SB.Cat(Item.Value);
    SB.Cat(LB);
  end;
  Result := SB.Value;
  SB.Free;
end;

procedure THttpHeaders.SetHaderItem(const Name, Value: string);
begin
  FData[Name] := Value;
end;

procedure THttpHeaders.SetItem(Index: Integer; const Value: PHeaderItem);
begin
  FData.Items[Index] := Value;
end;

procedure THttpHeaders.SetParam(const AName, AParam, AValue: string);
begin
  Values[AName] := ReplaceHeaderSubItem(Values[AName], AParam, AValue);
end;

procedure THttpHeaders.SetTextStr(const Value: string);
var
  P, P1, PN, PV, PMax: PChar;
begin
  Clear;
  P := PChar(Value);
  PMax := P + Length(Value);
  if PMax = P then Exit;
  P1 := P;
  while P < PMax do begin
    while (P < PMax) and (P^ <> #13) and (P^ <> #10) do Inc(P);  // 查找换行符
    PV := P1;
    while (PV < P) and (PV^ <> ':') do Inc(PV); // 查找 ":"
    PN := PV;
    Inc(PV);
    while (PV < PMax) and (PV^ = ' ') do Inc(PV); // 跳过空格
    FData.AddItem(PCharToString(P1, PN - P1), PCharToString(PV, P - PV), True); // 添加项
    Inc(P);
    while (P < PMax) and ((P^ = ' ') or (P^ = #13) or (P^ = #10)) do Inc(P); // 跳过换行符和行首空格
    P1 := P;
  end;
end;

{ TCredential }

constructor TCredential.Create(AnAuthTarget: TAuthTargetType; const ARealm,
  AURL, AUserName, APassword: string);
begin
  AuthTarget := AnAuthTarget;
  Realm := ARealm;
  URL := AURL;
  UserName := AUserName;
  Password := APassword;
end;

{ TCredentialsStorage }

function CompareCredential(Left, Right: Pointer): Integer;
begin
  Result := 0;
  if (PCredential(Left).Realm <> '') then begin
    if (PCredential(Right).Realm = '') then
      Result := -1
    else
      Result := CompareText(PCredential(Left).Realm, PCredential(Right).Realm);
  end else if (PCredential(Right).Realm <> '') then
    Result := 1;

  if Result = 0 then
    Result := - CompareText(PCredential(Left).URL, PCredential(Right).URL);
  if Result = 0 then
    Result := CompareText(PCredential(Left).UserName, PCredential(Right).UserName);
end;

function TCredentialsStorage.Add(const ACredential: TCredential): Boolean;
var
  Item: PCredential;
begin
  if (ACredential.AuthTarget = att_Server) and
     ((ACredential.UserName = '') or (ACredential.Password = ''))  then
    raise EHTTPException.CreateRes(@SCredentialInvalidUserPassword);
  if not ExistCredentials(ACredential.AuthTarget, ACredential.Realm, ACredential.URL, ACredential.UserName) then
  begin
    New(Item);
    Item^ := ACredential;
    FLocker.Enter;
    try
      inherited Add(Item);
      Result := True;
    finally
      FLocker.Leave;
    end;
  end else
    Result := False;
end;

procedure TCredentialsStorage.Clear;
begin
  FLocker.Enter;
  try
    inherited Clear;
  finally
    FLocker.Leave;
  end;
end;

constructor TCredentialsStorage.Create;
begin
  inherited Create;
  FLocker := TCriticalSection.Create;
end;

destructor TCredentialsStorage.Destroy;
begin
  inherited;
  FreeAndNil(FLocker);
end;

function TCredentialsStorage.ExistCredentials(AnAuthTargetType: TAuthTargetType;
  const ARealm, AURL, AUser: string): Boolean;
var
  LCredential: PCredential;
  LValid: Boolean;
  LHost: string;
  LURI: TURI;
  I: Integer;
begin
  Result := False;
  FLocker.Enter();
  try
    LHost := TURI.Create(AURL).Host;
    for I := 0 to Count - 1 do begin
      LCredential := inherited Items[I];
      LURI := TURI.Create(LCredential.URL);
      LValid := AnAuthTargetType = LCredential.AuthTarget;
      LValid := LValid and ((ARealm = '') or ((ARealm <> '') and ((LCredential.Realm = '') or
                    ((LCredential.Realm <> '') and
                     (CompareText(LCredential.Realm, ARealm) = 0)
                     )
                    )));
      LValid := LValid and ((AURL   = '') or ((AURL   <> '') and ((LURI.Host = '') or
                    ((LURI.Host <> '') and (StartsWith(LHost, LURI.Host, True)))
                    )));
      LValid := LValid and ((AUser  = '') or (CompareText(LCredential.UserName, AUser) = 0));

      if LValid then begin
        Result := True;
        Break;
      end;
    end;
  finally
    FLocker.Leave;
  end;
end;

function TCredentialsStorage.FindAccurateCredential(
  AnAuthTargetType: TAuthTargetType; const ARealm, AURL,
  AUser: string): TCredential;
var
  LCredential, LResult: PCredential;
  LValid: Boolean;
  I: Integer;
begin
  LResult := nil;
  FLocker.Enter;
  try
    for I := 0 to Count - 1 do begin
      LCredential := inherited Items[I];
      LValid := AnAuthTargetType = LCredential.AuthTarget;
      LValid := LValid and ((ARealm = '') or (CompareText(LCredential.Realm, ARealm) = 0));
      LValid := LValid and ((AURL   = '') or StartsWith(AURL, LCredential.URL));
      LValid := LValid and ((AUser  = '') or (CompareText(LCredential.UserName, AUser) = 0));

      if LValid and ((LResult = nil) or (Length(LResult.URL) < Length(LCredential.URL))) then
        LResult := LCredential;
    end;
  finally
    if LResult <> nil then     
      Result := LResult^
    else
      FillChar(Result, SizeOf(Result), 0);
    FLocker.Leave;
  end;
end;

function TCredentialsStorage.FindCredentials(AnAuthTargetType: TAuthTargetType;
  const ARealm, AURL, AUser: string): TCredentialArray;
var
  LCredential: PCredential;
  LValid: Boolean;
  LHost: string;
  LURI: TURI;
  I: Integer;
  List: TList;
begin
  FLocker.Enter();
  List := TList.Create;
  try
    LHost := TURI.Create(AURL).Host;
    for I := 0 to Count - 1 do begin
      LCredential := inherited Items[I];
      LURI := TURI.Create(LCredential.URL);
      LValid := AnAuthTargetType = LCredential.AuthTarget;
      LValid := LValid and ((ARealm = '') or ((ARealm <> '') and ((LCredential.Realm = '') or
                    ((LCredential.Realm <> '') and
                     (CompareText(LCredential.Realm, ARealm) = 0)
                     )
                    )));
      LValid := LValid and ((AURL   = '') or ((AURL   <> '') and ((LURI.Host = '') or
                    ((LURI.Host <> '') and (StartsWith(LHost, LURI.Host, True)))
                    )));
      LValid := LValid and ((AUser  = '') or (CompareText(LCredential.UserName, AUser) = 0));

      if LValid then
        List.Add(LCredential);
    end;
  finally
    SetLength(Result, List.Count);
    for I := 0 to List.Count - 1 do
      Result[I] := PCredential(List.Items[I])^;
    FLocker.Leave;
    List.Free;
  end;
end;

function TCredentialsStorage.GetItem(Index: Integer): TCredential;
begin
  FLocker.Enter;
  try
    Result := PCredential(inherited Items[Index])^;
  finally
    FLocker.Leave;
  end;
end;

procedure TCredentialsStorage.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and (Ptr <> nil) then begin
    PCredential(Ptr).Realm := '';
    PCredential(Ptr).URL := '';
    PCredential(Ptr).UserName := '';
    PCredential(Ptr).Password := '';
    Dispose(Ptr);
  end;
end;

function TCredentialsStorage.Remove(AnAuthTargetType: TAuthTargetType;
  const ARealm, AURL, AUser: string): Boolean;
var
  I: Integer;
  LEqual: Boolean;
  LItem: PCredential;
begin
  Result := False;
  FLocker.Enter();
  try
    for I := Count - 1 downto 0 do begin
      LItem := inherited Items[I];
      LEqual := AnAuthTargetType = LItem.AuthTarget;
      LEqual := LEqual and (CompareText(LItem.Realm, ARealm) = 0);
      LEqual := LEqual and (CompareText(LItem.URL, AURL) = 0);
      LEqual := LEqual and (CompareText(LItem.UserName, AUser) = 0);
      if LEqual then begin
        Delete(I);
        Result := True;
      end;
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TCredentialsStorage.SetItem(Index: Integer;
  const Value: TCredential);
begin
  FLocker.Enter;
  try
    PCredential(inherited Items[Index])^ := Value;
  finally
    FLocker.Leave;
  end;
end;

procedure TCredentialsStorage.Sort;
begin
  inherited Sort(CompareCredential);
end;

class function TCredentialsStorage.SortCredentials(
  const ACredentials: TCredentialArray): TCredentialArray;
var
  List: TPointerArray;
  I: Integer;
begin
  if Length(ACredentials) > 1 then begin
    for I := 0 to High(ACredentials) do
      List[I] := @ACredentials[I];
    QuickSort(List, CompareCredential);
    SetLength(Result, Length(ACredentials));
    for I := 0 to High(List) do
      Result[I] := PCredential(List[I])^;
  end else
    Result := ACredentials;
end;

{ TProxySettings }

constructor TProxySettings.Create(const AURL: string);
var
  LURL: TURI;
begin
  LURL := TURI.Create(AURL);
  Scheme := LURL.Scheme;
  Host := LURL.Host;
  Port := LURL.Port;
  UserName := LURL.Username;
  Password := LURL.Password;
  //Credential := TCredential.Create(att_Proxy, '', '', UserName, Password);
end;

constructor TProxySettings.Create(const AHost: string; APort: Integer;
  const AUserName, APassword, AScheme: string);
begin
  Host := AHost;
  Port := APort;
  UserName := AUsername;
  Password := APassword;
  Scheme := AScheme;
  //Credential := TCredential.Create(att_Proxy, '', '', UserName, Password);
end;

function TProxySettings.GetCredential: TCredential;
begin
  Result := TCredential.Create(att_Proxy, '',  '', UserName, Password);
end;

{ TCookie }

{$IFDEF USE_COOKIES}
function TCookie.ToString: string;
begin
  Result := Name + '=' + Value;
end;
{$ENDIF}

{ TCookies }

{$IFDEF USE_COOKIES}
procedure TCookies.Add(const Value: TCookie);
var
  Item: PCookie;
begin
  New(Item);
  Item^ := Value;
  inherited Add(Item);
end;

function TCookies.GetItem(Index: Integer): TCookie;
begin
  Result := PCookie(inherited Items[Index])^;
end;

function TCookies.GetText: string;
var
  I: Integer;
  SB: TStringCatHelper;
  Item: PCookie;
begin
  SB := TStringCatHelper.Create;
  try
    for I := 0 to Count - 1 do begin
      Item := inherited Items[I];
      SB.Cat(Item.ToString);
      SB.Cat(sLineBreak);
    end;
  finally
    Result := SB.Value;
    SB.Free;
  end;
end;

procedure TCookies.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and (Ptr <> nil) then begin
    PCookie(Ptr).Name := '';
    PCookie(Ptr).Value := '';
    PCookie(Ptr).Domain := '';
    PCookie(Ptr).Path := '';
    Dispose(Ptr);
  end;
end;
{$ENDIF}

const
   winhttp = 'winhttp.dll';

{ THttpClient }

function THttpClient.CheckDownloadResume(const AURL: string): Boolean;
var
  LResponse: THttpResult;
  LValue: string;
begin
  LResponse := Head(AURL, THttpHeaders.Create(S_Range, 'bytes=0-1')); // do not translate
  if LResponse.StatusCode = 206 then
    Result := True
  else begin
    LValue := LResponse.Response.HeaderValue[S_AcceptRanges];
    if (LValue = '') or SameText(LValue, 'none') then  // do not translate
      Result := False
    else
      Result := True;
  end;
end;

function THttpClient.ChooseAuthScheme(SupportedSchemes: DWORD): DWORD;
begin
  if (SupportedSchemes and WINHTTP_AUTH_SCHEME_NEGOTIATE) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_NEGOTIATE
  else if (SupportedSchemes and WINHTTP_AUTH_SCHEME_NTLM) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_NTLM
  else if (SupportedSchemes and WINHTTP_AUTH_SCHEME_PASSPORT) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_PASSPORT
  else if (SupportedSchemes and WINHTTP_AUTH_SCHEME_DIGEST) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_DIGEST
  else if (SupportedSchemes and WINHTTP_AUTH_SCHEME_BASIC) <> 0 then
    Result := WINHTTP_AUTH_SCHEME_BASIC
  else
    Result := 0;
end;

constructor THttpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCustomHeaders := THttpHeaders.Create;
  FCustomHeaders[S_Accept] := DefaultAccept;
  FCustomHeaders[S_AcceptEncoding] := DefaultAcceptEncoding;
  FCustomHeaders[S_AcceptLanguage] := DefaultAcceptLanguage;
  FCustomHeaders[S_UserAgent] := DefaultUserAgent;

  FCredentialsStorage := TCredentialsStorage.Create;

  FConnectionTimeOut := 30000;    // 默认连接超时 30 秒
  FRecvTimeOut := 0;              // 默认无接收数据超时
  FDefautlCharset := hct_GB2312;  // 默认使用GB2312
  
  FMaxRedirects := DefaultMaxRedirects;
  FHandleRedirects := True;
  {$IFDEF USE_COOKIES}
  FCookieMgr := TCookieManager.Create;
  FAllowCookies := True;
  {$ELSE}
  FAllowCookies := False;
  {$ENDIF}
  FAutoDecodeStr := True;

  FCertificateList := TCertificateList.Create;
  FWinCertList := TPCCERTCONTEXTList.Create;

  FWSession := WinHttpOpen('', WINHTTP_ACCESS_TYPE_NO_PROXY, WINHTTP_NO_PROXY_NAME, WINHTTP_NO_PROXY_BYPASS, 0);
  if FWSession = nil then
    raise EHTTPException.CreateRes(@SNetHttpClientHandleError);
end;

function THttpClient.Delete(const AURL: string; const AResponseContent: TStream;
  const AHeaders: THttpHeaders): THttpResult;
begin
  Result := DoExecute(AURL, S_DELETE, AHeaders, nil, AResponseContent);
end;

destructor THttpClient.Destroy;
begin
  {$IFDEF USE_COOKIES}
  FreeAndNil(FCookieMgr);
  {$ENDIF}
  FreeAndNil(FCredentialsStorage);
  FreeAndNil(FCustomHeaders);
  FreeAndNil(FCertificateList);
  FreeAndNil(FWinCertList);
  if FWSession <> nil then
    WinHttpCloseHandle(FWSession);
  inherited;
end;

function THttpClient.DoExecute(const AURL, AMethod: string;
  const AHeaders: THttpHeaders; const ASource, AResponseContent: TStream): THttpResult;
begin
  FillChar(Result, SizeOf(Result), 0);
  try
    Result.Request := THttpRequest.Create(Self, AMethod, AURL);
    Result.Request.FSourceStream := ASource;
    Result.FRequestInf := Result.Request;
    Result.Response := Result.Request.Execute(AHeaders, AResponseContent);
    Result.FResponseInf := Result.Response;
  except
    DoOnRequestError(Self, Exception(ExceptObject).Message);
  end;
  DoOnRequestCompleted(Self, Result);
end;

function THttpClient.DoExecuteRequest(const ARequest: THTTPRequest;
  var AResponse: THTTPResponse; const AResponseContent: TStream): TExecutionResult;
var
  DataLength: Cardinal;
  OptionValue: DWORD;
  Res: Boolean;
  LastError: Cardinal;
  Buffer: TBytes;
  ToRead, LastSrcPosition: Int64;
  BytesWritten: Cardinal;
begin
  Result := resp_Success;

  if ARequest.FSourceStream <> nil then
    DataLength := ARequest.FSourceStream.Size - ARequest.FSourceStream.Position
  else
    DataLength := 0;

  // Disable automatic redirects
  OptionValue := WINHTTP_DISABLE_REDIRECTS;
  WinHttpSetOption(ARequest.FWRequest, WINHTTP_OPTION_DISABLE_FEATURE, @OptionValue, sizeof(OptionValue));
  // Disable automatic addition of cookie headers to requests, it's done by framework
  OptionValue := WINHTTP_DISABLE_COOKIES;
  WinHttpSetOption(ARequest.FWRequest, WINHTTP_OPTION_DISABLE_FEATURE, @OptionValue, sizeof(OptionValue));
  // 超时设置
  if (FConnectionTimeOut > 0) or (FRecvTimeOut > 0) then begin
    if FConnectionTimeOut > 0 then begin
      OptionValue := WINHTTP_OPTION_CONNECT_TIMEOUT;
      WinHttpSetOption(ARequest.FWRequest, WINHTTP_OPTION_DISABLE_FEATURE, @OptionValue, sizeof(OptionValue));
    end;
    if FRecvTimeOut > 0 then begin    
      OptionValue := WINHTTP_OPTION_RECEIVE_TIMEOUT;
      WinHttpSetOption(ARequest.FWRequest, WINHTTP_OPTION_DISABLE_FEATURE, @OptionValue, sizeof(OptionValue));
    end;
    WinHttpSetTimeouts(ARequest.FWRequest, 0, FConnectionTimeOut, 0, FRecvTimeOut);
  end;

  Res := WinHttpSendRequest(ARequest.FWRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0,
    WINHTTP_NO_REQUEST_DATA, 0, DataLength, 0);
  if not Res then
  begin
    LastError := GetLastError;
    case LastError of
      ERROR_WINHTTP_SECURE_FAILURE:
        Result := resp_ServerCertificateInvalid;
      ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED:
        Result := resp_ClientCertificateNeeded;
      else
        Result := resp_UnknownError;
    end;
    Exit;
  end;

  // Send data
  if DataLength > 0 then begin
    LastSrcPosition := ARequest.FSourceStream.Position;
    SetLength(Buffer, BUFFERSIZE);
    while ARequest.FSourceStream.Position < ARequest.FSourceStream.Size do begin
      ToRead := ARequest.FSourceStream.Size - ARequest.FSourceStream.Position;
      if ToRead > BUFFERSIZE then
        ToRead := BUFFERSIZE;
      ARequest.FSourceStream.ReadBuffer(Buffer[0], ToRead);
      // Write data to the server.
      if not WinHttpWriteData(ARequest.FWRequest, Buffer[0], ToRead, @BytesWritten) then
        raise EHTTPException.CreateResFmt(@SNetHttpClientSendError, [GetLastError, SysErrorMessage(GetLastError, FWinHttpHandle)]);
      if BytesWritten < ToRead then
        ARequest.FSourceStream.Position := ARequest.FSourceStream.Position - (ToRead - BytesWritten);
    end;
    ARequest.FSourceStream.Position := LastSrcPosition;
  end;

  // Wait to receive response
  Res := WinHttpReceiveResponse(ARequest.FWRequest, nil);
  if not Res then
  begin
    LastError := GetLastError;
    case LastError of
      ERROR_WINHTTP_SECURE_FAILURE:
          Result := resp_ServerCertificateInvalid;
      ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED:
          Result := resp_ClientCertificateNeeded;
      else
        Result := resp_UnknownError;
    end;
    Exit;
  end;

  if AResponse = nil then
    AResponse := THttpResponse.Create(ARequest, AResponseContent);
  AResponse.FHeader := '';  // Reset response headers
end;

procedure THttpClient.DoOnReceiveData(const Sender: TObject; AContentLength,
  AReadCount: Int64; var Abort: Boolean);
begin
  if Assigned(FOnReceiveData) then
    FOnReceiveData(Sender, AContentLength, AReadCount, Abort);
end;

procedure THttpClient.DoOnRequestCompleted(const Sender: TObject; const AResult: THttpResult);
begin
  if Assigned(FOnRequestCompleted) then begin
    try
      FOnRequestCompleted(Sender, AResult);
    except
      DoOnRequestError(Sender, Exception(ExceptObject).Message);
    end;
  end;
end;

procedure THttpClient.DoOnRequestError(const Sender: TObject;
  const AError: string);
begin
  if Assigned(FOnRequestError) then
    FOnRequestError(Sender, AError)
  else
    raise EHTTPException.Create(AError);
end;

function THttpClient.DoPost(const AMethod, AURL, ASource: string;
  ACharset: THttpCharsetType; const AResponseContent: TStream;
  const AHeaders: THttpHeaders): THttpResult;
var
  LSourceStream: TMemoryStream;
  SA: StringA;
  {$IFNDEF UNICODE}
  SW: StringW;
  {$ENDIF}
begin
  if Length(ASource) > 0 then begin
    LSourceStream := TMemoryStream.Create();
    try
      case ACharset of
        hct_GB2312, hct_GBK, hct_ISO8859_1:
          begin
            {$IFDEF UNICODE}
            LSourceStream.Write(ASource[1], Length(ASource));
            {$ELSE}
            SA := StringA(ASource);
            LSourceStream.Write(SA[1], Length(SA));
            {$ENDIF}
          end;
        hct_UTF8:
          begin
            SA := Utf8Encode(WideString(ASource));
            LSourceStream.Write(SA[1], Length(SA));
          end;
        hct_UTF16:
          begin
            {$IFDEF UNICODE}
            LSourceStream.Write(ASource[1], Length(ASource) shl 1);
            {$ELSE}
            SW := StringW(ASource);
            LSourceStream.Write(SW[1], Length(SW) shl 1);
            {$ENDIF}
          end;
        hct_BIG5:
          begin
            LSourceStream.Write(ASource[1], Length(ASource){$IFDEF UNICODE} shl 1{$ENDIF});
          end;
      end;
      LSourceStream.Position := 0;
      Result := DoExecute(AURL, AMethod, AHeaders, LSourceStream, AResponseContent);
    finally
      LSourceStream.Free;
    end;
  end else
    Result := DoExecute(AURL, AMethod, AHeaders, nil, AResponseContent);
end;

function THttpClient.DoProcessStatus(const ARequest: THTTPRequest;
  const AResponse: THTTPResponse): Boolean;
var
  LURI: TURI;
begin
  Result := True;
  if (AResponse.StatusCode >= 300) and (AResponse.StatusCode < 400) then // Redirect
  begin
    LURI.Create(TURI.PathRelativeToAbs(AResponse.HeaderValue[S_Location], ARequest.FURL));
    if Assigned(FOnRequestRedirect) then
      FOnRequestRedirect(ARequest, AResponse.StatusCode, LURI.URL);
    ARequest.UpdateRequest(LURI);
    AResponse.FStatusCode := 0;
    Result := False;
  end;
end;

function THttpClient.DoSetCredential(AnAuthTargetType: TAuthTargetType;
  const ARequest: THttpRequest; const ACredential: TCredential): Boolean;
var
  AuthTarget: DWORD;
  Target: DWORD;
  FirstScheme: DWORD;
  SupportedSchemes: DWORD;
  AuthScheme: DWORD;
begin
  if AnAuthTargetType = att_Server then begin
    AuthTarget := WINHTTP_AUTH_TARGET_SERVER;
    AuthScheme := ARequest.FLastServerAuthScheme;
  end else begin
    AuthTarget := WINHTTP_AUTH_TARGET_PROXY;
    AuthScheme := ARequest.FLastProxyAuthScheme;
  end;

  if AuthScheme = 0 then begin // we haven't a previous scheme
    if WinHttpQueryAuthSchemes(ARequest.FWRequest, SupportedSchemes, FirstScheme, Target) then
    begin
      AuthScheme := ChooseAuthScheme(SupportedSchemes);
      if AnAuthTargetType = att_Server then
        ARequest.FLastServerAuthScheme := AuthScheme
      else
        ARequest.FLastProxyAuthScheme := AuthScheme;
    end
  end;
  Result := WinHttpSetCredentials(ARequest.FWRequest, AuthTarget, AuthScheme,
    PWideChar(StringW(ACredential.UserName)),
    PWideChar(StringW(ACredential.Password)), nil);
end;

function THttpClient.ExecuteHTTP(const ARequest: THttpRequest;
  const AResponseContent: TStream): THttpResponse;
var
  LResponse: THTTPResponse;
  LExecResult: TExecutionResult;

  OrigSourceStreamPosition: Int64;

  State: THTTPState;
  LClientCertificateList: TCertificateList;
  Status: Integer;
  {$IFDEF USE_COOKIES}
  LCookieHeader: string;
  {$ENDIF}
begin
  LResponse := nil;
  Result := nil;
  
  OrigSourceStreamPosition := 0;
  if ARequest.FSourceStream <> nil then
    OrigSourceStreamPosition := ARequest.FSourceStream.Position;

  FillChar(State, SizeOf(State), 0);
  LClientCertificateList := TCertificateList.Create;
  try
    while True do begin
      ARequest.DoPrepare; 

      // Add Cookies
      {$IFDEF USE_COOKIES}
      if AllowCookies and (FCookieMgr <> nil) then begin
        LCookieHeader := FCookieMgr.GetCookies(ARequest.FURL);
        if Length(LCookieHeader) > 0 then
          ARequest.AddHeader(S_Cookie, LCookieHeader);  // do not localize
      end;
      {$ENDIF}
      
      if not SetServerCredential(ARequest, Result, State) then
        Break;
      if not SetProxyCredential(ARequest, Result, State) then
        Break;

      if ARequest.FSourceStream <> nil then
        ARequest.FSourceStream.Position := OrigSourceStreamPosition;

      if LResponse <> nil then begin
        LResponse.FContentStream.Position := 0;
        LResponse.FContentStream.Size := 0;
      end;

      LExecResult := DoExecuteRequest(ARequest, LResponse, AResponseContent);
      case LExecResult of
        resp_Success:
          begin
            if not SameText(ARequest.FMethodString, S_HEAD) then
              LResponse.DoReadData(LResponse.FContentStream);
            LResponse.DecodeHeader;
            Status := LResponse.GetStatusCode;
            case Status of
              200: Break;
              401: State.Status := is_ServerAuthRequired;
              407: State.Status := is_ProxyAuthRequired;
              else
                begin
                  case Status of
                    301..304, 307:
                      if FHandleRedirects and (ARequest.FMethodString <> S_HEAD) then
                      begin
                        Inc(State.Redirections);
                        if State.Redirections > FMaxRedirects then
                          raise EHTTPException.CreateResFmt(@SNetHttpMaxRedirections, [FMaxRedirects]);
                      end
                      else
                        Break;
                  else
                  end;
                  State.Status := is_Other;
                  if DoProcessStatus(ARequest, LResponse) then
                    Break;
                end;
            end;
          end;
        resp_ServerCertificateInvalid:
          begin
            raise EHTTPException.Create(SNetHttpInvalidServerCertificate);
          end;
        resp_ClientCertificateNeeded:
          begin
            raise EHTTPException.CreateRes(@SNetHttpUnspecifiedCertificate);
          end
        else
          raise EHTTPException.CreateRes(@SNetHttpClientUnknownError);
      end;  
      
    end;

    if ARequest.FSourceStream <> nil then
      ARequest.FSourceStream.Seek(0, soEnd);
    {$IFDEF USE_COOKIES}
    if AllowCookies then
      UpdateCookiesFromResponse(LResponse);
    {$ENDIF}
  finally
    LClientCertificateList.Free;
    Result := LResponse;
    if ARequest.FSourceStream <> nil then
      ARequest.FSourceStream.Position := OrigSourceStreamPosition;
  end;
end;

function THttpClient.Get(const AURL: string; const AResponseContent: TStream;
  const AHeaders: THttpHeaders): THttpResult;
begin
  Result := DoExecute(AURL, S_GET, AHeaders, nil, AResponseContent);
end;

function THttpClient.GetAccept: string;
begin
  Result := FCustomHeaders[S_Accept];
end;

function THttpClient.GetAcceptCharSet: string;
begin
  Result := FCustomHeaders[S_AcceptCharSet];
end;

function THttpClient.GetAcceptEncoding: string;
begin
  Result := FCustomHeaders[S_AcceptEncoding];
end;

function THttpClient.GetAcceptLanguage: string;
begin
  Result := FCustomHeaders[S_AcceptLanguage];
end;

function THttpClient.GetContentType: string;
begin
  Result := FCustomHeaders[S_ContentType];
end;

function THttpClient.GetCredentials(AuthTarget: TAuthTargetType; const ARealm,
  URL: string): TCredentialArray;
begin
  Result := TCredentialsStorage.SortCredentials(
    FCredentialsStorage.FindCredentials(AuthTarget, ARealm, URL));
end;

function THttpClient.GetRange(const AURL: string; AStart, AnEnd: Int64;
  const AResponseContent: TStream; const AHeaders: THttpHeaders): THttpResult;
var
  LHeaders: THttpHeaders;
  LRange: string;
  AllowFree: Boolean;
begin
  LRange := 'bytes=';
  if AStart > -1 then
    LRange := LRange + IntToStr(AStart);
  LRange := LRange + '-';
  if AnEnd > -1 then
    LRange := LRange + IntToStr(AnEnd);
  LHeaders := AHeaders;
  AllowFree := False;
  if not Assigned(LHeaders) then begin
    LHeaders := THttpHeaders.Create(S_Range, LRange);
    AllowFree := True;
  end else
    LHeaders[S_Range] := LRange;
  try
    Result := DoExecute(AURL, S_GET, LHeaders, nil, AResponseContent);
  finally
    if AllowFree then
      LHeaders.Free;
  end;
end;

function THttpClient.GetUserAgent: string;
begin
  Result := FCustomHeaders[S_UserAgent];
end;

function THttpClient.Head(const AURL: string;
  const AHeaders: THttpHeaders): THttpResult;
begin
  Result := DoExecute(AURL, S_HEAD, AHeaders, nil, nil);
end;

function THttpClient.Options(const AURL: string;
  const AResponseContent: TStream; const AHeaders: THttpHeaders): THttpResult;
begin
  Result := DoExecute(AURL, S_OPTIONS, AHeaders, nil, AResponseContent);
end;

function THttpClient.Post(const AURL, ASource: string; ACharset: THttpCharsetType;
  const AResponseContent: TStream; const AHeaders: THttpHeaders): THttpResult;
begin
  Result := DoPost(S_POST, AURL, ASource, ACharset, AResponseContent, AHeaders);
end;

function THttpClient.PostFile(const AURL, ASourceFile: string;
  const AResponseContent: TStream; const AHeaders: THttpHeaders): THttpResult;
var
  LSourceStream: TStream;
begin
  LSourceStream := TFileStream.Create(ASourceFile, fmOpenRead);
  try
    Result := DoExecute(AURL, S_POST, AHeaders, LSourceStream, AResponseContent);
  finally
    LSourceStream.Free;
  end;
end;

function THttpClient.Put(const AURL, ASource: string;
  ACharset: THttpCharsetType; const AResponseContent: TStream;
  const AHeaders: THttpHeaders): THttpResult;
begin
  Result := DoPost(S_PUT, AURL, ASource, ACharset, AResponseContent, AHeaders);
end;

function THttpClient.Put(const AURL: string; const ASource,
  AResponseContent: TStream; const AHeaders: THttpHeaders): THttpResult;
begin
  Result := DoExecute(AURL, S_PUT, AHeaders, nil, AResponseContent);
end;

function THttpClient.Post(const AURL: string; const ASource,
  AResponseContent: TStream; const AHeaders: THttpHeaders): THttpResult;
begin
  Result := DoExecute(AURL, S_POST, AHeaders, AResponseContent, AResponseContent);
end;

procedure THttpClient.SetAccept(const Value: string);
begin
  FCustomHeaders[S_Accept] := Value;
end;

procedure THttpClient.SetAcceptCharSet(const Value: string);
begin
  FCustomHeaders[S_AcceptCharSet] := Value;
end;

procedure THttpClient.SetAcceptEncoding(const Value: string);
begin
  FCustomHeaders[S_AcceptEncoding] := Value;
end;

procedure THttpClient.SetAcceptLanguage(const Value: string);
begin
  FCustomHeaders[S_AcceptLanguage] := Value;
end;

procedure THttpClient.SetContentType(const Value: string);
begin
  FCustomHeaders[S_ContentType] := Value;
end;

function THttpClient.SetProxyCredential(const ARequest: THTTPRequest;
  const AResponse: THTTPResponse; var State: THTTPState): Boolean;
var
  I: Integer;
  LAbortAuth: Boolean;
  LPersistence: TAuthPersistenceType;
  OldPass: string;
  OldUser: string;
  Credentials: TCredentialArray;
begin
  Result := True;
  LPersistence := apt_Client;

  if State.Status = is_ProxyAuthRequired then begin
    if State.ProxyCredential.UserName = '' then
    // It's the first Proxy auth request
    begin
      Credentials := GetCredentials(att_Proxy, AResponse.InternalGetAuthRealm, '');
      if ProxySettings.Host <> '' then begin
        SetLength(State.ProxyCredentials, Length(Credentials) + 1);
        State.ProxyCredentials[0] := ProxySettings.Credential;
        for I := 0 to High(Credentials) do
          State.ProxyCredentials[I+1] := Credentials[I];
      end else
        State.ProxyCredentials := Credentials;
    end;
  end;
  if State.NeedProxyCredential then begin
    if State.Status = is_ProxyAuthRequired then
    begin
      if State.ProxyIterator < Length(State.ProxyCredentials) then
      begin   // Get the next credential from the storage
        State.ProxyCredential.AuthTarget := att_Proxy;
        State.ProxyCredential.UserName := State.ProxyCredentials[State.ProxyIterator].UserName;
        State.ProxyCredential.Password := State.ProxyCredentials[State.ProxyIterator].Password;
        Inc(State.ProxyIterator);
      end
      else
      begin
        //Can't get a valid proxy credential from the storage so ask to the user
        LAbortAuth := False;
        State.ProxyCredential.AuthTarget := att_Proxy;
        OldUser := State.ProxyCredential.UserName;
        OldPass := State.ProxyCredential.Password;
        State.ProxyCredential.UserName := '';
        State.ProxyCredential.Password := '';
        if (State.ProxyCredential.Password = '') or LAbortAuth then
          State.ProxyCredential.UserName := '';
        if State.ProxyCredential.UserName <> '' then
        begin
          // If it is the same user and password than the previous one we empty the given and abort the operation to avoid infinite loops.
          if (State.ProxyCredential.UserName = OldUser) and (State.ProxyCredential.Password = OldPass) then
            State.ProxyCredential.UserName := ''
          else
            if LPersistence = apt_Client then
              FCredentialsStorage.Add(State.ProxyCredential);
        end;
      end;
    end;
    if State.ProxyCredential.UserName <> '' then
      Result := DoSetCredential(att_Proxy, ARequest, State.ProxyCredential)
    else
      Result := False;  // We need a Credential but we haven't found a good one, so exit
  end;
end;

procedure THttpClient.SetProxySettings(const Value: TProxySettings);
var
  LCredential: TCredential;
  LURI: TURI;
begin
  FProxySettings := Value;

  //如果有用户信息,我们创建所需的凭据进行身份验证,并将其添加到存储。
  if Value.Username <> '' then
  begin
    LURI.FHost := Value.Host;
    LURI.FPort := Value.Port;
    LURI.FScheme := Value.Scheme;
    LCredential := TCredential.Create(att_Proxy, '', LURI.ToString, Value.UserName,
      Value.Password);
    FCredentialsStorage.Add(LCredential);
  end;
end;

function THttpClient.SetServerCredential(const ARequest: THTTPRequest;
  const AResponse: THTTPResponse; var State: THTTPState): Boolean;
var
  LAbortAuth: Boolean;
  LPersistence: TAuthPersistenceType;
  OldPass: string;
  OldUser: string;
begin
  Result := True;
  LPersistence := apt_Client;
  // Set Server Credentials
  if State.Status = is_ServerAuthRequired then
  begin
    if State.ServerCredential.UserName = '' then
    // It's the first Server auth request
    begin
      State.ServerCredentials := GetCredentials(att_Server, AResponse.InternalGetAuthRealm,
        ARequest.FURL.URL);
      if ARequest.FLocalCredential.UserName <> '' then begin
        SetLength(State.ServerCredentials, Length(State.ServerCredentials) + 1);
        State.ServerCredentials[High(State.ServerCredentials)] := ARequest.FLocalCredential;
      end;
      State.NeedServerCredential := True;
    end;
  end;
  if State.NeedServerCredential then
  begin
    if State.Status = is_ServerAuthRequired then
    begin
      if State.ServerIterator < Length(State.ServerCredentials) then
      begin  // Get the next credential from the storage
        State.ServerCredential.AuthTarget := att_Server;
        State.ServerCredential.UserName := State.ServerCredentials[State.ServerIterator].UserName;
        State.ServerCredential.Password := State.ServerCredentials[State.ServerIterator].Password;
        State.ServerCredential.Realm := State.ServerCredentials[State.ServerIterator].Realm;
        Inc(State.ServerIterator);
      end
      else
      begin
        //Can't get a valid server credential from the storage so ask to the user
        LAbortAuth := False;
        State.ServerCredential.AuthTarget := att_Server;
        OldUser := State.ServerCredential.UserName;
        OldPass := State.ServerCredential.Password;
        State.ServerCredential.UserName := '';
        State.ServerCredential.Password := '';
        State.ServerCredential.Realm := AResponse.InternalGetAuthRealm;
        if (State.ServerCredential.Password = '') or LAbortAuth then
          State.ServerCredential.UserName := '';
        if State.ServerCredential.UserName <> '' then begin
          // If it is the same user and password than the previous one we empty the given and abort the operation to avoid infinite loops.
          if (State.ServerCredential.UserName = OldUser) and (State.ServerCredential.Password = OldPass) then
            State.ServerCredential.UserName := ''
          else
            if LPersistence = apt_Client then
              FCredentialsStorage.Add(State.ServerCredential);
        end;
      end;
    end;
    if State.ServerCredential.UserName <> '' then
      Result := DoSetCredential(att_Server, ARequest, State.ServerCredential)
    else
      Result := False;  // We need a Credential but we haven't found a good one, so exit
  end;
end;

procedure THttpClient.SetUserAgent(const Value: string);
begin
  FCustomHeaders[S_UserAgent] := Value;
end;

function THttpClient.Trace(const AURL: string; const AResponseContent: TStream;
  const AHeaders: THttpHeaders): THttpResult;
begin
  Result := DoExecute(AURL, S_TRACE, AHeaders, nil, AResponseContent);
end;

{$IFDEF USE_COOKIES}
procedure THttpClient.UpdateCookiesFromResponse(const AResponse: THttpResponse);
var
  P, PMax: PChar;
  V: string;
begin
  P := PChar(AResponse.Header);
  PMax := P + Length(AResponse.Header);
  while P < PMax do begin
    V := AResponse.InnerGetHeaderValue(P, PMax, S_SetCookie);
    if V <> '' then
      FCookieMgr.AddCookies(V, AResponse.FRequest.URL)
    else
      Break;
  end;
end;
{$ENDIF}

{ TCertificateList }

procedure TCertificateList.AddItem(const Value: TCertificate);
var
  Item: PCertificate;
begin
  New(Item);
  Item^ := Value;
  Add(Item);
end;

procedure TCertificateList.AddItem(const Value: PCertificate);
begin
  if Value <> nil then Add(Value);
end;

procedure TCertificateList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and (Ptr <> nil) then begin
    PCertificate(Ptr).Subject := '';
    PCertificate(Ptr).Issuer := '';
    PCertificate(Ptr).ProtocolName := '';
    PCertificate(Ptr).AlgSignature := '';
    PCertificate(Ptr).AlgEncryption := '';
    Dispose(Ptr);
  end;
end;

{ TPCCERTCONTEXTList }

procedure TPCCERTCONTEXTList.AddItem(const Value: PCCERT_CONTEXT);
begin
  if Value <> nil then Add(Value);
end;

procedure TPCCERTCONTEXTList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if (Action = lnDeleted) and (Ptr <> nil) then 
    CertFreeCertificateContext(Ptr);
end;

{ THttpRequest }

procedure THttpRequest.AddHeader(const AName, AValue: string);
begin
  if WinHttpAddRequestHeaders(FWRequest, PWideChar(StringW(AName+': '+ AValue)), $ffffffff,
    WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA) = False then
    raise EHTTPException.CreateResFmt(@SNetHttpRequestAddHeaderError, [GetLastError, SysErrorMessage(GetLastError), FWinHttpHandle]);
end;

constructor THttpRequest.Create(AClient: THttpClient; const ARequestMethod: string;
  const AURI: TURI);
begin
  inherited Create();
  FClient := AClient;
  FURL := AURI;
  FMethodString := ARequestMethod;
  DoCreate();
end;

constructor THttpRequest.Create(AClient: THttpClient; const ARequestMethod,
  AURL: string);
begin
  inherited Create();
  FClient := AClient;
  FURL.URL := AURL;
  FMethodString := ARequestMethod;
  DoCreate();
end;

procedure THttpRequest.CreateHandles(const AURI: TURI);
var
  LFlags: DWORD;
begin
  if Length(AURI.Host) = 0 then
    raise EHTTPException.CreateResFmt(@SNetUriInvalid, [AURI.FUrl]); 
  FWConnect := WinHttpConnect(FClient.FWSession, PWideChar(StringW(AURI.Host)), AURI.Port, 0);
  if FWConnect = nil then
    raise EHTTPException.CreateResFmt(@SNetHttpRequestConnectError, [AURI.Host]);

  LFlags := 0;
  if LowerCase(AURI.Scheme) = S_HTTPS then
    LFlags := LFlags or WINHTTP_FLAG_SECURE;

  FWRequest := WinHttpOpenRequest(FWConnect, PWideChar(StringW(FMethodString)),
    PWideChar(StringW(AURI.GetDocumentWithParams)), nil,
    WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, LFlags);
  if FWRequest = nil then
    raise EHTTPException.CreateResFmt(@SNetHttpRequestOpenError, [GetLastError, SysErrorMessage(GetLastError, FWinHttpHandle)]);
end;

destructor THttpRequest.Destroy;
begin
  if FWRequest <> nil then
    WinHttpCloseHandle(FWRequest);
  if FWConnect <> nil then
    WinHttpCloseHandle(FWConnect);
  FreeAndNil(FHeaders);
  inherited;
end;

procedure THttpRequest.DoCreate;
begin
  FHeaders := THttpHeaders.Create;
  FLocalCredential := TCredential.Create(att_Server, '', FURL.Host,
    FURL.Username, FURL.Password);
  CreateHandles(FURL);
end;

procedure THttpRequest.DoPrepare;
var
  I: Integer;
begin
  // 加入自定义头
  if FHeaders <> nil then begin
    for I := 0 to FHeaders.Count - 1 do
      SetHeaderValue(FHeaders.Items[I].Name, FHeaders.Items[I].Value);
  end; 
  SetWinProxySettings;
end;

procedure THttpRequest.DoReceiveDataProgress(AStatusCode: Integer;
  AContentLength, AReadCount: Int64; var Abort: Boolean);
begin
  if AStatusCode < 300 then
    if Assigned(FClient.FReceiveDataCallback) then
      FClient.FReceiveDataCallback(Self, AContentLength, AReadCount, Abort)
    else if Assigned(FClient.FOnReceiveData) then
      FClient.FOnReceiveData(Self, AContentLength, AReadCount, Abort);
end;

function THttpRequest.Execute(const AHeaders: THttpHeaders; const AResponseContent: TStream): THttpResponse;
var
  I: Integer;
  Item: PHeaderItem;
begin
  // 加入自定义头
  if AHeaders <> nil then begin
    for I := 0 to AHeaders.Count - 1 do
      FHeaders[AHeaders.Items[I].Name] := AHeaders.Items[I].Value;
  end; 
  for I := 0 to FClient.FCustomHeaders.Count - 1 do begin
    Item := FClient.FCustomHeaders.Items[I];
    if FHeaders.FData.Exist(Item.Name) then Continue;
    FHeaders[Item.Name] := Item.Value;
  end;
  if Assigned(FSourceStream) then    
    FHeaders[S_ContentLength] := IntToStr(FSourceStream.Size - FSourceStream.Position);
  Result := FClient.ExecuteHTTP(Self, AResponseContent);
end;

function THttpRequest.GetHeaders: string;
begin
  Result := ReadHeader(FWRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF or WINHTTP_QUERY_FLAG_REQUEST_HEADERS);
end;

function THttpRequest.GetHeaderValue(const AName: string): string;
begin
  Result := ReadHeader(FWRequest, WINHTTP_QUERY_FLAG_REQUEST_HEADERS, AName);
end;

function THttpRequest.GetSourceString: string;
var
  Buf: TBytes;
  A, Len: Int64;
  M: TMemoryStream;
  AllowFree: Boolean;
  LastSrcPosition: Int64;
begin
  Result := '';
  M := nil;
  AllowFree := False;
  try
    if Assigned(FSourceStream) then begin
      if FSourceStream is TCustomMemoryStream then
        M := TMemoryStream(FSourceStream)
      else begin
        AllowFree := True;
        M := TMemoryStream.Create;
        Setlength(Buf, BUFFERSIZE);
        LastSrcPosition := FSourceStream.Position;
        Len := FSourceStream.Size - LastSrcPosition;
        try
          while Len > 0 do begin
            if Len > BUFFERSIZE then
              A := BUFFERSIZE
            else
              A := Len;
            A := FSourceStream.Read(Buf[0], A);
            M.Write(Buf[0], A);
            Dec(Len, A);
          end;
        finally
          FSourceStream.Position := LastSrcPosition;
          M.Position := 0;
        end;
      end;
    end else
      Exit;
    Result := PCharToString(PAnsiChar(M.Memory) + M.Position, M.Size - M.Position);
  finally
    if AllowFree then     
      FreeAndNil(M);
  end;
end;

function THttpRequest.RemoveHeader(const AName: string): Boolean;
begin
  Result := True;
  if GetHeaderValue(AName) = '' then
    Result := False
  else if WinHttpAddRequestHeaders(FWRequest, PWideChar(StringW(AName+':')), $ffffffff, WINHTTP_ADDREQ_FLAG_REPLACE) = False then
    raise EHTTPException.CreateResFmt(@SNetHttpRequestRemoveHeaderError, [GetLastError, SysErrorMessage(GetLastError, FWinHttpHandle)]);
end;

procedure THttpRequest.SetHeaderValue(const AName, AValue: string);
begin
  if WinHttpAddRequestHeaders(FWRequest, PWideChar(StringW(AName+': '+ AValue)), $ffffffff, WINHTTP_ADDREQ_FLAG_REPLACE or WINHTTP_ADDREQ_FLAG_ADD) = False then
    raise EHTTPException.CreateResFmt(@SNetHttpRequestAddHeaderError, [GetLastError, SysErrorMessage(GetLastError, FWinHttpHandle)]);
end;

procedure THttpRequest.SetURL(const Value: TURI);
begin
  if FURL.Host <> '' then
    raise  EHTTPException.CreateRes(@SNetUriURLAlreadyAssigned);
  FURL := Value;
end;

procedure THttpRequest.SetWinProxySettings;

  function GetProxyInfo(const AURL: string; var AProxy, AProxyBypass: string): Boolean;
  var
    LAutoDetectProxy: Boolean;
    LProxyConfig: TWinHTTPCurrentUserIEProxyConfig;
    LWinHttpProxyInfo: TWinHTTPProxyInfo;
    LAutoProxyOptions: TWinHTTPAutoProxyOptions;
  begin
    Result := True;
    AProxy := '';
    AProxyBypass := '';
    FillChar(LAutoProxyOptions, SizeOf(LAutoProxyOptions), 0);

    if WinHttpGetIEProxyConfigForCurrentUser(LProxyConfig) then
    begin
      if LProxyConfig.fAutoDetect then
      begin
        LAutoProxyOptions.dwFlags := WINHTTP_AUTOPROXY_AUTO_DETECT;
        LAutoProxyOptions.dwAutoDetectFlags := WINHTTP_AUTO_DETECT_TYPE_DHCP or WINHTTP_AUTO_DETECT_TYPE_DNS_A;
      end;

      if LProxyConfig.lpszAutoConfigURL <> '' then
      begin
        LAutoProxyOptions.dwFlags := WINHTTP_AUTOPROXY_CONFIG_URL;
        LAutoProxyOptions.lpszAutoConfigUrl := LProxyConfig.lpszAutoConfigUrl;
        LAutoDetectProxy := True;
      end
      else
      begin
        AProxy := LProxyConfig.lpszProxy;
        AProxyBypass := LProxyConfig.lpszProxyBypass;
        LAutoDetectProxy := False;
      end;
    end
    else
    begin
      // if the proxy configuration is not found then try to autodetect it (If the Internet Explorer settings are not configured for system accounts)
      LAutoProxyOptions.dwFlags := WINHTTP_AUTOPROXY_AUTO_DETECT;
      LAutoProxyOptions.dwAutoDetectFlags := WINHTTP_AUTO_DETECT_TYPE_DHCP or WINHTTP_AUTO_DETECT_TYPE_DNS_A;
      LAutoDetectProxy := True;
    end;

    if (AProxy = '') and LAutoDetectProxy then
    begin
      // From https://msdn.microsoft.com/en-us/library/aa383153%28VS.85%29.aspx
      // Try with fAutoLogonIfChallenged parameter set to false, if ERROR_WINHTTP_LOGIN_FAILURE then try
      // with fAutoLogonIfChallenged parameter set to true.
      LAutoProxyOptions.fAutoLogonIfChallenged := False;
      if WinHttpGetProxyForUrl(FClient.FWSession, LPCWSTR(StringW(AURL)), LAutoProxyOptions, LWinHttpProxyInfo) then
      begin
        AProxy := LWinHttpProxyInfo.lpszProxy;
        AProxyBypass := LWinHttpProxyInfo.lpszProxyBypass;
      end
      else
      begin
        if GetLastError = ERROR_WINHTTP_LOGIN_FAILURE then
        begin
          LAutoProxyOptions.fAutoLogonIfChallenged := True;
          if WinHttpGetProxyForUrl(FClient.FWSession, LPCWSTR(StringW(AURL)), LAutoProxyOptions, LWinHttpProxyInfo) then
          begin
            AProxy := LWinHttpProxyInfo.lpszProxy;
            AProxyBypass := LWinHttpProxyInfo.lpszProxyBypass;
          end
          else
            Result := False;
        end
        else
          Result := False;
      end;
    end;
    GlobalFree(HGLOBAL(LProxyConfig.lpszAutoConfigUrl));
    GlobalFree(HGLOBAL(LProxyConfig.lpszProxy));
    GlobalFree(HGLOBAL(LProxyConfig.lpszProxyBypass));
    if AProxy = '' then
      Result := False;
  end;

  function GetProxyString: string;
  begin
    Result := '';
    if FClient.ProxySettings.Scheme <> '' then
      Result := Result + FClient.ProxySettings.Scheme + '://';
    Result := Result + FClient.ProxySettings.Host + ':' + IntToStr(FClient.ProxySettings.Port);
  end;

var
//  LRequest: TWinHTTPRequest;
  LProxyInfo: TWinHttpProxyInfo;
  OptionValue: DWORD;
  LProxyString: string;
  LProxy, LProxyBypass: string;
begin
//  LRequest := TWinHTTPRequest(ARequest);

  if FClient.ProxySettings.Host <> '' then
  begin
    LProxyString := GetProxyString;
    if not SameText('http://direct:80', LProxyString) then
    begin
      LProxyInfo.dwAccessType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
      LProxyInfo.lpszProxy := PWideChar(StringW(LProxyString));
      LProxyInfo.lpszProxyBypass := PWideChar(StringW(''));
      OptionValue := SizeOf(LProxyInfo);
      WinHttpSetOption(FWRequest, WINHTTP_OPTION_PROXY, @LProxyInfo, OptionValue);
      if FClient.ProxySettings.UserName <> '' then // we try to use the usual auth scheme to try to avoid a round trip
        WinHttpSetCredentials(FWRequest, WINHTTP_AUTH_TARGET_PROXY,
          WINHTTP_AUTH_SCHEME_BASIC,
          PWideChar(StringW(FClient.ProxySettings.UserName)),
          PWideChar(StringW(FClient.ProxySettings.Password)), nil);
    end;
  end
  else
  begin
    if GetProxyInfo(FURL.ToString, LProxy, LProxyBypass) then
    begin
      LProxyInfo.dwAccessType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
      LProxyInfo.lpszProxy := PWideChar(StringW(LProxy));
      LProxyInfo.lpszProxyBypass := PWideChar(StringW(LProxyBypass));
      OptionValue := SizeOf(LProxyInfo);
      WinHttpSetOption(FWRequest, WINHTTP_OPTION_PROXY, @LProxyInfo, OptionValue);
    end;
  end;
end;

procedure THttpRequest.UpdateRequest(const AURI: TURI);
begin
  if FWRequest <> nil then
    WinHttpCloseHandle(FWRequest);
  if FWConnect <> nil then
    WinHttpCloseHandle(FWConnect);
  CreateHandles(AURI);
end;

{ THttpResponse }


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

constructor THttpResponse.Create(ARequest: THttPRequest; const AResponseStream: TStream);
begin
  inherited Create();
  FRequest := ARequest;
  FContentStream := TMemoryStream.Create;
  FStream := AResponseStream;
  if Assigned(FStream) then
    FLastStreamPos := FStream.Position
  else
    FLastStreamPos := 0;
  FRawDataSize := 0;
end;

procedure THttpResponse.DecodeHeader;

  procedure CmpStr(P: PAnsiChar);
  begin
    if StrLIComp(P, PCharA(StringA('UTF-8')), 5) = 0 then
      FCharSet := hct_UTF8
    else if StrLIComp(P, PCharA(StringA('utf-16')), 6) = 0 then
      FCharSet := hct_UTF16
    else if StrLIComp(P, PCharA(StringA('gb2312')), 6) = 0 then
      FCharSet := hct_GB2312
    else if StrLIComp(P, PCharA(StringA('gbk')), 3) = 0 then
      FCharSet := hct_GBK
    else if StrLIComp(P, PCharA(StringA('big')), 3) = 0 then
      FCharSet := hct_BIG5
    else if StrLIComp(P, PCharA(StringA('ISO-8859')), 8) = 0 then
      FCharSet := hct_ISO8859_1
  end;

  procedure DecodeCharSet(P: PAnsiChar; Len: Integer);
  const
    PS: PAnsiChar = 'charset=';
    PSA: PAnsiChar = 'CHARSET=';
  var
    PMax, P2, PSS, PSAS: PAnsiChar;
  begin
    FCharSet := FRequest.FClient.FDefautlCharset;
    FCharSetText := ExtractHeaderSubItem(MimeType, S_CharSet);
    if Length(FCharSetText) = 0 then begin
      // 在内容中查找charset
      PMax := P + Len;
      while (P < PMax) do begin
        if (P^ = PS^) or (P^ = PSA^) then begin
          P2 := P + 8;
          Inc(P);
          PSS := PS + 1;
          PSAS := PSA + 1;
          if P2 < PMax then begin
            while (P < P2) and ((P^ = PSS^) or (P^ = PSAS^)) do begin
              Inc(P);
              Inc(PSS);
              Inc(PSAS);
            end;
            if P = P2 then begin
              while (P < PMax) and ((P^ = '"') or (P^ = '''') or (P^ = ' ')) do
                Inc(P);            
              CmpStr(P);
              Exit;
            end;
          end else
            Break; 
        end;
        Inc(P);
      end;
    end else
      CmpStr(PAnsiChar(StringA(FCharSetText)));
  end;

var
  M: TMemoryStream;
begin
  FContentStream.Position := 0;
  FRawDataSize := FContentStream.Size;
  FIsGZip := Pos('gzip', LowerCase(ContentEncoding)) > 0;
  if FIsGZip then begin
    M := TMemoryStream.Create;
    try
      DecompressGZip(FContentStream, M);
      FContentStream.Position := 0;
      FContentStream.Write(M.Memory^, M.Size);
    finally
      M.Free;
    end;
  end;
  DecodeCharSet(FContentStream.Memory, FContentStream.Size);
  FContentStream.Position := 0;
  if Assigned(FStream) then begin
    FStream.Write(FContentStream.Memory^, FContentStream.Size);
    FContentStream.Clear;
  end;
end;

destructor THttpResponse.Destroy;
begin
  FStream := nil;
  FreeAndNil(FContentStream);
  {$IFDEF USE_COOKIES}
  FreeAndNil(FCookies);
  {$ENDIF}
  inherited;
end; 

procedure THttpResponse.DoReadData(const AStream: TStream);
var
  LSize: Cardinal;
  LDownloaded: Cardinal;
  LBuffer: TBytes;
  LExpected, LReaded: Int64;
  LStatusCode: Integer;
  Abort: Boolean;
begin
  LReaded := 0;
  LExpected := GetContentLength;
  if LExpected = 0 then
    LExpected := -1;
  LStatusCode := GetStatusCode;
  Abort := False;
  FRequest.DoReceiveDataProgress(LStatusCode, LExpected, LReaded, Abort);
  if not Abort then
    repeat
      if not WinHttpQueryDataAvailable(FRequest.FWRequest, @LSize) then // Get the size of readed data in LSize
        Break;

      if LSize = 0 then
        Break;

      SetLength(LBuffer, LSize + 1);

      if not WinHttpReadData(FRequest.FWRequest, LBuffer[0], LSize, @LDownloaded) then
        raise EHTTPException.CreateResFmt(@SNetHttpRequestReadDataError, [GetLastError, SysErrorMessage(GetLastError, FWinHttpHandle)]);

      // This condition should never be reached since WinHttpQueryDataAvailable
      // reported that there are bits to read.
      if LDownloaded = 0 then
        Break;

      AStream.WriteBuffer(LBuffer[0], LDownloaded);
      LReaded := LReaded + LDownloaded;
      FRequest.DoReceiveDataProgress(LStatusCode, LExpected, LReaded, Abort);
    until (LSize = 0) or Abort;
end;

function THttpResponse.GetContentEncoding: string;
begin
  Result := GetHeaderValue(S_ContentEncoding);
end;

function THttpResponse.GetContentLanguage: string;
begin
  Result := GetHeaderValue(S_ContentLanguage);
end;

function THttpResponse.GetContentLength: Int64;
begin
  Result := StrToInt64Def(GetHeaderValue(S_ContentLength), 0);
end;

function THttpResponse.GetContentString: string;
var
  M: TMemoryStream;
  AllowFree: Boolean;
begin
  M := nil;
  AllowFree := False;
  try
    AllowFree := GetContentStringStream(M);
    if FRequest.FClient.FAutoDecodeStr then begin
      case FCharSet of
        hct_GB2312, hct_GBK, hct_ISO8859_1:
          Result := PCharToString(M.Memory, M.Size);
        hct_UTF8:
          Result := Utf8Decode(M.Memory, M.Size);
        hct_UTF16:
          {$IFDEF UNICODE}
          Result := PChar(M.Memory);
          {$ELSE}
          Result := PCharWToString(M.Memory, M.Size);
          {$ENDIF}
        hct_BIG5: // 这个暂时不处理
          Result := PCharToString(M.Memory, M.Size);
      end;
    end else
      Result := PCharToString(M.Memory, M.Size);
  finally
    if AllowFree then
      FreeAndNil(M);
  end;
end;

function THttpResponse.GetContentStringStream(var M: TMemoryStream): Boolean;
var
  Buf: TBytes;
  A, Len: Int64;
begin
  M := nil;
  Result := False;
  if Assigned(FStream) then begin
    if FStream is TCustomMemoryStream then
      M := TMemoryStream(FStream)
    else begin
      Result := True;
      M := TMemoryStream.Create;
      Setlength(Buf, BUFFERSIZE);
      FStream.Position := FLastStreamPos;
      Len := FStream.Size - FLastStreamPos;
      while Len > 0 do begin
        if Len > BUFFERSIZE then
          A := BUFFERSIZE
        else
          A := Len;
        A := FStream.Read(Buf[0], A);
        M.Write(Buf[0], A);
        Dec(Len, A);
      end;
    end;
  end else
    M := FContentStream;
end;

{$IFNDEF UNICODE}
function THttpResponse.GetContentStringW: StringW;
var
  M: TMemoryStream;
  AllowFree: Boolean;
begin
  AllowFree := False;
  try
    AllowFree := GetContentStringStream(M);
    if FRequest.FClient.FAutoDecodeStr then begin
      case FCharSet of
        hct_GB2312, hct_GBK, hct_ISO8859_1:
          Result := PCharToString(M.Memory, M.Size);
        hct_UTF8:
          Result := Utf8Decode(M.Memory, M.Size);
        hct_UTF16:
          Result := PCharWToString(M.Memory, M.Size);
        hct_BIG5: // 这个暂时不处理
          Result := PCharToString(M.Memory, M.Size);
      end;
    end else
      Result := PCharToString(M.Memory, M.Size);
  finally
    if AllowFree then
      FreeAndNil(M);
  end;
end;
{$ENDIF}

function THttpResponse.GetContentType: string;
begin
  Result := GetHeaderValue(S_ContentType);  
end;

{$IFDEF USE_COOKIES}
function THttpResponse.GetCookies: TCookies;
begin
  if not Assigned(FCookies) then begin
    FCookies := TCookies.Create;
    if Assigned(FRequest) and Assigned(FRequest.FClient) then
      FRequest.FClient.FCookieMgr.GetCookies(FRequest.URL, FCookies);
  end;
  Result := FCookies;
end;
{$ENDIF}

function THttpResponse.GetDate: string;
begin
  Result := GetHeaderValue(S_Date);
end;

function THttpResponse.GetHeaders: string;
begin
  if FHeader = '' then
    FHeader := ReadHeader(FRequest.FWRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF);
  Result := FHeader;
end;

function THttpResponse.GetHeaderSize: Cardinal;
begin
  Result := Length(Header);
end;

function THttpResponse.GetHeaderValue(const Name: string): string;
var
  P, PMax: PChar;
begin
  P := PChar(Header);
  PMax := P + Length(FHeader);
  Result := InnerGetHeaderValue(P, PMax, Name);
end;

function THttpResponse.GetLastModified: string;
begin
  Result := GetHeaderValue(S_LastModified);
end;

function THttpResponse.GetMimeType: string;
begin
  Result := GetHeaderValue(S_ContentType);
end;

function THttpResponse.GetStatusCode: Integer;
begin
  if FStatusCode = 0 then
    FStatusCode := StrToIntDef(ReadHeader(FRequest.FWRequest, WINHTTP_QUERY_STATUS_CODE), 0);
  Result := FStatusCode;
end;

function THttpResponse.GetVersion: THttpProtocolVersion;
var
  Version: string;
begin
  Version := ReadHeader(FRequest.FWRequest, WINHTTP_QUERY_VERSION);
  if CompareText(Version, 'HTTP/1.0') = 0 then
    Result := hpv_HTTP_1_0
  else if CompareText(Version, 'HTTP/1.1') = 0 then
    Result := hpv_HTTP_1_1
  else if CompareText(Version, 'HTTP/2.0') = 0 then
    Result := hpv_HTTP_2_0
  else
    Result := hpv_UNKNOWN_HTTP;
end;

function THttpResponse.InnerGetHeaderValue(var P, PMax: PChar; const Name: string): string;
var
  P1, P2: PChar;
  PKey: PChar;
  KL: Integer;
begin
  Result := '';
  if (Length(Name) = 0) or (P = nil) or (P >= PMax) then Exit;   
  PKey := PChar(Name);
  KL := Length(Name);
  
  while P < PMax do begin
    if P^ = PKey^ then begin
      P1 := PKey;
      P2 := P + KL;
      if P2 < PMax then begin
        while (P < P2) and (P^ = P1^) do begin
          Inc(P);
          Inc(P1);
        end; 
        if P = P2 then begin
          Inc(P);
          while (P < PMax) and (P^ = ' ') do Inc(P);
          P1 := P;
          while (P < PMax) and (P^ <> #13) and (P^ <> #10) do Inc(P);
          while (P > P1) and (P^ = ' ') do Dec(P);
          Result := PCharToString(P1, P - P1); 
          Exit;
        end;
      end else
        Exit;
    end;
    Inc(P);
  end;   
end;

function THttpResponse.InternalGetAuthRealm: string;
const
  CSTR_REALM = 'realm="';
var
  LValue: string;
  APos: Integer;
  LLower: string;
begin
  LValue := GetHeaderValue(S_WWWAuthenticate);
  if LValue = '' then
    LValue := GetHeaderValue(S_ProxyAuthenticate);   
  if LValue = '' then
    Result := ''
  else begin
    LLower := LowerCase(LValue);
    APos := Pos(CSTR_REALM, LLower) + Length(CSTR_REALM); // Do not translate
    Result := Copy(LValue, APos, PosStr(LLower, '"', APos + 1) - APos);         
  end;
end;

{$IFDEF USE_COOKIES}
procedure TCookies.SetItem(Index: Integer; const Value: TCookie);
begin
  PCookie(inherited Items[Index])^ := Value;
end;
{$ENDIF}

{ THttpResult }

function THttpResult.GetStatusCode: Integer;
begin
  if Assigned(Response) then
    Result := Response.StatusCode
  else
    Result := 0;
end;

{ THttpLocalClient }

procedure THttpLocalClient.DoError(const Sender: TObject; const AError: string);
begin
  OutputDebugString(PChar(AError));
end;

{ TCookieManager }

{$IFDEF USE_COOKIES}
procedure TCookieManager.AddCookies(const Value: string; const URL: TURI);
var
  Item: TCookie;
  Items: TStrings;
  V: string;
  I, J: Integer;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.Path := '/';
  Items := TStringList.Create;
  Items.StrictDelimiter := True;
  Items.Delimiter := ';';
  Items.DelimitedText := Value;
  try
    for I := 0 to Items.Count - 1 do begin
      V := Trim(Items[I]);
      if I = 0 then begin
        J := Pos('=', V);
        if J > 0 then begin
          Item.Name := Copy(V, 1, J - 1);
          Delete(V, 1, J);
          Item.Value := V;
        end else
          Item.Value := V;
      end else begin
        if StrLICompH(V, 'path=') then begin
          Delete(V, 1, 5);
          Item.Path := V;
          for J := 1 to Length(Item.Path) do
            if Item.Path[J] = '\' then Item.Path[J] := '/';
        end else if StrLICompH(V, 'Domain=') then begin
          Delete(V, 1, 7);
          Item.Domain := V;
        end else if StrLICompH(V, 'Expires=') then begin
          Delete(V, 1, 8);
          Item.Expires := GMTRFC822ToDateTime(StringA(V));
        end;
        
      end;
    end;
    if Length(Item.Domain) = 0 then
      Item.Domain := URL.Host;
    AppendDataSet(@Item);
  finally
    Items.Free();
  end;
end;

procedure TCookieManager.AppendDataSet(PF: PCookie);
var
  FKey: string;
  LvPF: PCookie;
  Index: Integer;
begin
  FKey := PF^.Domain + '----' + PF^.Path + '----' + PF^.Name;
  if FKey = '' then
    Exit;
  FCS.Enter;
  try
    Index := FCookies.IndexOf(FKey);
    if Index < 0 then begin
      New(LvPF);
      LvPF^ := PF^;
      FCookies.AddObject(FKey,TObject(LvPF));
    end else begin
      LvPF := PCookie(FCookies.Objects[Index]);
      LvPF^ := PF^;
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TCookieManager.Clear;
var
  I: Integer;
  Item: PCookie;
begin
  FCS.Enter;
  for I := 0 to FCookies.Count - 1 do begin
    Item := PCookie(FCookies.Objects[I]);
    if Item <> nil then
      Dispose(Item);
  end;
  FCookies.Clear();
  FCS.Leave;
end;

constructor TCookieManager.Create;
begin
  FCS := TCriticalSection.Create;
  FMS := TStringCatHelper.Create(4096);
  FCookies := TStringList.Create;
end;

destructor TCookieManager.Destroy;
begin
  Clear;
  FreeAndNil(FCS);
  FMS.Free;
  FCookies.Free;
  inherited;
end;

function TCookieManager.GetCookies(const URL: TURI): string;
var
  Domain, SubDomain, Path: string;
  _Domain, _Path: string;
  Index: Integer;
  PF: PCookie;
begin
  FCS.Enter;
  try
    FMS.Reset;
    DoMain    := URL.Host;
    Path      := URL.Path;
    if Path = '' then Path := '/';

    for Index := 0 to FCookies.Count - 1 do begin
      PF := PCookie(FCookies.Objects[Index]);
      _Domain := PF.Domain;
      _Path   := PF.Path;
      if StrLICompH(_Domain, DoMain) then begin
        if Pos(_Path, Path) = 1 then
          FMS.Cat(PF.ToString).Cat('; ');
      end else begin
        //判读2级域名
        SubDoMain := GetSubDomain(DoMain);
        if IncludeSubDomin(_Domain, SubDoMain) and (Pos(_Path, Path) = 1) then
          FMS.Cat(PF.ToString).Cat('; ');
      end;
    end;
    Result := FMS.Value;
  finally
    FCS.Leave;
  end;
end;

function TCookieManager.GetCookies(const URL: TURI; List: TCookies): Integer;
var
  Domain, SubDomain, Path: string;
  _Domain, _Path: string;
  Index: Integer;
  PF: PCookie;
begin
  FCS.Enter;
  try
    DoMain    := URL.Host;
    Path      := URL.Path;
    if Path = '' then Path := '/';

    for Index := 0 to FCookies.Count - 1 do begin
      PF := PCookie(FCookies.Objects[Index]);
      _Domain := PF.Domain;
      _Path   := PF.Path;
      if StrLICompH(_Domain, DoMain) then begin
        if Pos(_Path, Path) = 1 then
          List.Add(PF^);
      end else begin
        //判读2级域名
        SubDoMain := GetSubDomain(DoMain);
        if IncludeSubDomin(_Domain, SubDoMain) and (Pos(_Path, Path) = 1) then
          List.Add(PF^);
      end;
    end;
  finally
    FCS.Leave;
  end;
  Result := List.Count;
end;

function TCookieManager.GetSubDomain(const Domain: string): string;
var
  Index:Integer;
begin
  Index := Pos('.', Domain);
  if Index > 0 then
    Result := Copy(Domain, Index, System.MaxInt)
  else
    Result := '';
end;

function TCookieManager.IncludeSubDomin(const Domain,
  SubDomain: string): Boolean;
var
  S1,S2: string;
  Index,L1,L2: Integer;
begin
  Result := False;
  S1 := LowerCase(Domain);
  S2 := LowerCase(SubDomain);
  L1 := Length(S1);
  L2 := Length(S2);
  if L2 > L1 then
    Exit;
  for Index := L2 downto 1 do begin
    if S2[Index] <> S1[L1] then Exit;
    Dec(L1);
  end;
  Result := True;
end;
{$ENDIF}

initialization
  FWinHttpHandle := GetModuleHandle(winhttp);
  FLocalLoker := TCriticalSection.Create;

finalization
  FreeAndNil(FLocalClient);
  FreeAndNil(FLocalLoker);

end.
