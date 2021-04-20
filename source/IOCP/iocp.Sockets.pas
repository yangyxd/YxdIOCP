{*******************************************************}
{                                                       }
{       IOCP Sockets ������Ԫ   (����DIOCP�޸İ汾)     }
{                                                       }
{       ��Ȩ���� (C) 2015 YangYxd                       }
{                                                       }
{*******************************************************}
{
  ����Ԫ����DIOCP�޸ģ��󲿷ִ��붼����ͬ�ġ��Ƴ�����־
  ϵͳ������ͳһ��StateMsg�¼����ϲ������Ϣ��

  ��лԭDIOCP���ߣ�����ң�����Դ����ԭ���߽��ƣ��������
  �⸴�ơ��޸ġ�ʹ�ã��緢��BUG�뱨������ǡ�
  Ϊ�˳�����Դ�����һ���޸İ汾������ϣ��Ҳ�ܿ�Դ��

  ʹ��ע�⣺ ����int64���ͱ�ʾ��ʱ��㣬����ͨ��
  TimestampToDatetime ��������ת��Ϊ TDateTime.
}

unit iocp.Sockets;

{$I 'iocp.inc'}
// �����־��¼���뿪��
{$DEFINE WRITE_LOG}

// �Ƿ��������ĵ�����Ϣ�������󽫽��Ͳ�������

{.$DEFINE DEBUGINFO}

// ��������ʱ��ʹ�� Iocp �����SendBuffer�ڴ��

{$DEFINE UseSendMemPool}

interface

uses
  iocp.Utils.Hash, iocp.Task, iocp.Utils.MemPool,
  iocp.Sockets.Utils, iocp.Core.Engine, iocp.Res, iocp.RawSockets,
  iocp.Utils.Queues, iocp.Utils.ObjectPool,
  WinSock, iocp.Winapi.WinSock,
  {$IFDEF UNICODE} {System.} Types, Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  {$IF CompilerVersion>22}VCL.ExtCtrls, {$ELSE}ExtCtrls, {$IFEND}
  SyncObjs, Windows, Classes, SysUtils;

const
  LF = #10;
  CR = #13;
  EOL = #13#10;

type
  /// <summary>
  /// �����ͷŷ�ʽ
  /// </summary>
  TDataReleaseType = (
    dtNone {���Զ��ͷ�},
    dtFreeMem {����FreeMem�ͷ��ڴ�},
    dtDispose {����Dispose�ͷ����ݣ�������New���������},
    dtMemPool {ʹ���ڴ��});

type
  TIocpCustom = class;
  TIocpRecvRequest = class;
  TIocpSendRequest = class;
  TIocpCustomContext = class;
  TIocpDataMonitor = class;
  TIocpAcceptorMgr = class;
  TIocpAcceptExRequest = class;
  TIocpBlockSocketStream = class;
  TIocpDisconnectExRequest = class;

  TIocpRecvRequestClass = class of TIocpRecvRequest;
  TIocpSendRequestClass = class of TIocpSendRequest;
  TIocpContextClass = class of TIocpCustomContext;

  TIocpStateMsgType = (
    iocp_mt_Info {��Ϣ},
    iocp_mt_Debug {������Ϣ},
    iocp_mt_Warning {����},
    iocp_mt_Error {����});

  TNotifyContextEvent = procedure(const Context: TIocpCustomContext) of object;
  TOnStateMsgEvent = procedure(Sender: TObject; MsgType: TIocpStateMsgType;
    const Msg: string) of object;
  TOnContextError = procedure(const Context: TIocpCustomContext; ErrorCode: Integer) of object;
  TOnBufferReceived = procedure(const Context: TIocpCustomContext; buf: Pointer;
    len: Cardinal; ErrorCode: Integer) of object;
  TOnDataRequestCompleted = procedure(const ClientContext: TIocpCustomContext;
    Request: TIocpRequest) of object;
  TOnSendRequestResponse = procedure(Context: TIocpCustomContext;
    Request: TIocpSendRequest) of object;

  TIocpActionRequest = class(TIocpRequest)
  public
    procedure HandleResponse; override;
  end;

  /// <summary>
  /// �ͻ�(Client)����
  /// </summary>
  TIocpCustomContext = class(TObject)
  private
    FPrev: TIocpCustomContext;
    FNext: TIocpCustomContext;
    FOwner: TIocpCustom;
    FActive: Boolean;
    FSending: Boolean;
    FSendStreaming: Integer;

    FRawSocket: TRawSocket;
    FSocketHandle: TSocket;
    FRecvRequest: TIocpRecvRequest;
    FSendRequest: TIocpSendRequest;
    FSendRequestList: TIocpRequestLinkList;

    FData: Pointer;
    FOnConnectedEvent: TNotifyContextEvent;
    FOnSocketStateChanged: TNotifyEvent;

    FLastActive: Int64;
    FHandle: Cardinal;
    FSocketState: TSocketState; 
    FLastErrorCode: Integer;

    FRefCount: Integer;
    // ��󽻻���ʱ��
    FLastActivity: Int64;

    function GetSocketHandle: TSocket;

    // �ڲ���������
    function InnerSendData(buf: Pointer; len: Cardinal;
      pvBufReleaseType: TDataReleaseType; pvTag: Integer = 0;
      pvTagData: Pointer = nil): Boolean;
  private
    // �ͷſͻ�����
    procedure ReleaseClientContext(); virtual;
    /// <example>
    /// �ͷŴ����Ͷ����еķ�������(TSendRequest)
    /// </example>
    procedure CheckReleaseRes;
    /// <summary>
    /// ��Ӧ RecvRequest
    /// </summary>
    procedure DoReceiveData;
    /// <summary>
    /// ��鲢Ͷ����һ����������
    /// </summary>
    function CheckNextSendRequest: Boolean;
    /// <summary>
    /// �Ͽ���������
    /// </summary>
    procedure RequestDisconnect(pvObj: TObject = nil; const pvDebugInfo: string = '');
    function GetIsDisconnect: Boolean;
    procedure SetOwner(const Value: TIocpCustom);
  protected
    FAlive: Boolean;  // �Ƿ�����ʹ��
    FRequestDisconnect: Integer; // ����Ͽ�����
    FContextLocker: TIocpLocker;
    FCloseRequest: TIocpActionRequest;

    procedure PostCloseRequest;
    procedure OnCloseRequestResponse(Sender: TObject);
    
    function IncReferenceCounter(pvObj: TObject; const pvDebugInfo: string = ''): Boolean;
    function DecReferenceCounter(pvObj: TObject; const pvDebugInfo: string = ''): Integer;

    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrorCode: Integer); virtual;
    procedure OnDisconnected; virtual;
    procedure OnConnected; virtual;
    procedure DoCleanUp; virtual;
    procedure DoConnected;
    procedure DoDisconnect;
    procedure DoError(ErrorCode: Integer);
    procedure CreateSocket(IsOverlapped: Boolean); virtual;
    function GetSendRequest: TIocpSendRequest; virtual;

    /// <summary>
    /// Ͷ�ݵķ���������Ӧʱִ�У�һ��Ӧ������ִ�У�Errcode <> 0Ҳ����Ӧ
    /// </summary>
    procedure DoSendRequestRespnonse(pvRequest: TIocpSendRequest); virtual;
    /// <summary>
    /// ��Ӧ SendRequest
    /// </summary>
    procedure DoSendRequestCompleted(pvRequest: TIocpSendRequest); virtual;

    /// <summary>
    /// �����������
    /// </summary>
    procedure PostWSARecvRequest(); virtual;
    procedure PostNextSendRequest; virtual;
    procedure PostWSACloseRequest(); virtual;
    procedure InnerCloseContext;

    /// <summary>
    /// 1. Ͷ�ݷ������󵽶�����, ������Ͷ����������� False
    /// 2. ��� sending ��־, ��� sending �� False �ſ�ʼ
    /// </summary>
    function InnerPostSendRequestAndCheckStart(pvSendRequest: TIocpSendRequest): Boolean;

    procedure SetSocketState(pvState: TSocketState); virtual;
    function LockContext(pvObj: TObject; const pvDebugInfo: string): Boolean;
    procedure UnLockContext(pvObj: TObject; const pvDebugInfo: string);
  public
    constructor Create(AOwner: TIocpCustom); virtual;
    destructor Destroy; override;

    procedure Lock;
    procedure UnLock;

    /// <summary>
    /// �Ͽ�����
    /// </summary>
    procedure Disconnect; virtual;
    procedure Close; virtual;

    /// <summary>
    /// �ر�����, �첽ģʽ����֤���ڷ��͵����ݿ��Է������
    /// </summary>
    procedure CloseConnection;

    /// <summary>
    /// �������� (�첽) , �ɹ����� True.
    /// </summary>
    function Send(buf: Pointer; len: Cardinal; CopyBuf: Boolean = True): Boolean; overload;
    function Send(buf: Pointer; len: Cardinal; BufReleaseType: TDataReleaseType): Boolean; overload;
    function Send(const Data: AnsiString): Boolean; overload;
    function Send(const Data: WideString): Boolean; overload;
    {$IFDEF UNICODE}
    function Send(const Data: UnicodeString): Boolean; overload;
    {$ENDIF}
    function SendStream(Stream: TStream): Boolean; overload;
    function SendStream(Stream: TStream; ASize: Int64): Boolean; overload;

    /// <summary>
    /// Ͷ��һ����������Iocp������, �ɹ����� True.
    /// ����������, ������ DoSendRequestCompleted ����
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal;
      pvCopyBuf: Boolean = True): Boolean; overload; 
    /// <summary>
    /// Ͷ��һ����������Iocp������, �ɹ����� True.
    /// ����������, ������ DoSendRequestCompleted ����
    /// </summary>
    function PostWSASendRequest(buf: Pointer; len: Cardinal;
      pvBufReleaseType: TDataReleaseType; pvTag: Integer = 0;
      pvTagData: Pointer = nil): Boolean; overload; virtual;
    /// <summary>
    /// ������
    /// </summary>
    function PostWSASendStream(Stream: TStream; ASize: Int64): Boolean; virtual;

    /// <summary>
    /// ���÷��Ͷ������޴�С
    /// </summary>
    procedure SetMaxSendingQueueSize(pvSize: Integer);

    property Active: Boolean read FActive;
    property Owner: TIocpCustom read FOwner write SetOwner;
    property Socket: TRawSocket read FRawSocket;
    property SocketHandle: TSocket read GetSocketHandle;
    property SocketState: TSocketState read FSocketState;
    property IsDisconnecting: Boolean read GetIsDisconnect;

    property RecvRequest: TIocpRecvRequest read FRecvRequest;
    property SendRequest: TIocpSendRequest read FSendRequest;

    /// <summary>
    /// ��󽻻����ݵ�ʱ��
    /// </summary>
    property LastActive: Int64 read FLastActive;
    // ��������
    property Data: Pointer read FData write FData;

    property Prev: TIocpCustomContext read FPrev;
    property Next: TIocpCustomContext read FNext;
    // ��󽻻�ʱ��
    property LastActivity: Int64 read FLastActivity write FLastActivity;
    // ���Ӿ���������첽����ʱʶ������
    property Handle: Cardinal read FHandle;
    /// <summary>
    /// ������ɴ������¼�
    /// </summary>
    property OnConnectedEvent: TNotifyContextEvent read FOnConnectedEvent write FOnConnectedEvent;
    /// <summary>
    /// Socket״̬�ı䴥�����¼�
    /// </summary>
    property OnSocketStateChanged: TNotifyEvent read FOnSocketStateChanged write FOnSocketStateChanged;
  end;

  TIocpBase = class(TComponent)
  private
    FActive: Boolean;
    FIocpEngine: TIocpEngine;
    FLocker: TIocpLocker;
    FMemPool: TIocpMemPool;
    FIsDestroying: Boolean;
    FWSARecvBufferSize: Cardinal;
    FWSASendBufferSize: Cardinal;
    FBindAddr: AnsiString;

    FOnSendRequestResponse: TOnSendRequestResponse;
    FOnStateMsg: TOnStateMsgEvent;

    function GetWorkerCount: Integer;
    function GetMaxWorkerCount: Integer;
    procedure SetBindAddr(const Value: AnsiString);
    procedure SetMaxWorkerCount(const Value: Integer);
    procedure SetWorkerCount(const Value: Integer);
    procedure SetWSARecvBufferSize(const Value: Cardinal);
    procedure SetWSASendBufferSize(const Value: Cardinal);
  protected
    FDataMoniter: TIocpDataMonitor;
    FContextClass: TIocpContextClass;
    FSendRequestClass: TIocpSendRequestClass;
    FRecvRequestClass: TIocpRecvRequestClass;
    procedure SetName(const NewName: TComponentName); override;
    procedure SetActive(const Value: Boolean);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function PopMem: Pointer;
    procedure PushMem(const V: Pointer);

    /// <summary>
    /// �������ݼ������ʵ��
    /// </summary>
    procedure CreateDataMonitor;

    /// <summary>
    /// ֹͣIOCP�̣߳��ȴ������߳��ͷ�
    /// </summary>
    procedure Close; virtual; abstract;

    /// <summary>
    /// ��ʼ����
    /// </summary>
    procedure Open; virtual; abstract;

    /// <summary>
    /// �Ƿ������ͷ�
    /// </summary>
    function IsDestroying: Boolean;

    // ״̬��Ϣ
    procedure DoStateMsg(Sender: TObject; MsgType: TIocpStateMsgType;
      const Msg: string);
    // ״̬��Ϣ
    procedure DoStateMsgD(Sender: TObject; const Msg: string); overload;
    // ״̬��Ϣ
    procedure DoStateMsgD(Sender: TObject; const MsgFormat: string; const Params: array of const); overload;
    // ״̬��Ϣ
    procedure DoStateMsgE(Sender: TObject; const Msg: string); overload;
    // ״̬��Ϣ
    procedure DoStateMsgE(Sender: TObject; E: Exception); overload;
    // ״̬��Ϣ
    procedure DoStateMsgE(Sender: TObject; const MsgFormat: string; E: Exception); overload;
    // ״̬��Ϣ
    procedure DoStateMsgE(Sender: TObject; const MsgFormat: string; const Params: array of const); overload;

    property Active: Boolean read FActive write SetActive;
    /// <summary>
    /// ��������������С(Ĭ��4k)
    /// </summary>
    property RecvBufferSize: Cardinal read FWSARecvBufferSize write SetWSARecvBufferSize;
    /// <summary>
    /// ���η�����������ֽ��� (Ĭ��8k)
    /// </summary>
    property SendBufferSize: Cardinal read FWSASendBufferSize write SetWSASendBufferSize;
    /// <summary>
    /// �����߳�����
    /// </summary>
    property WorkerCount: Integer read GetWorkerCount write SetWorkerCount;
    /// <summary>
    /// ������߳�����
    /// </summary>
    property MaxWorkerCount: Integer read GetMaxWorkerCount write SetMaxWorkerCount;

    property Locker: TIocpLocker read FLocker;
    /// <summary>
    /// ״̬������
    /// </summary>
    property Moniter: TIocpDataMonitor read FDataMoniter;
    /// <summary>
    /// IOCP����
    /// </summary>
    property Engine: TIocpEngine read FIocpEngine;
    /// <summary>
    /// �󶨵�ַ
    /// </summary>
    property BindAddr: AnsiString read FBindAddr write SetBindAddr;
    /// <summary>
    /// ��һ���첽����������Ӧʱ����
    /// </summary>
    property OnSendRequestResponse: TOnSendRequestResponse read FOnSendRequestResponse write FOnSendRequestResponse;
    /// <summary>
    /// ״̬��Ϣ����ӿ�
    /// </summary>
    property OnStateInfo: TOnStateMsgEvent read FOnStateMsg write FOnStateMsg;
  end;

  TIocpCustom = class(TIocpBase)
  private
    FOnlineContextList: TIntHash;
    FSendRequestPool: TBaseQueue;

    FContextCounter: Integer;

    FOnReceivedBuffer: TOnBufferReceived;
    FOnContextConnected: TNotifyContextEvent;
    FOnContextDisconnected: TNotifyContextEvent;
    FOnContextError: TOnContextError;

    function GetOnlineContextCount: Integer;
  protected
    function RequestContextHandle: Integer;

    /// <summary>
    /// ��������
    /// </summary>
    procedure DoReceiveData(const pvContext: TIocpCustomContext;
      pvRequest: TIocpRecvRequest);

    /// <summary>
    /// ���ӷ�������
    /// </summary>
    procedure DoClientContextError(const pvClientContext: TIocpCustomContext;
      pvErrorCode: Integer);

    /// <summary>
    ///  ��ӵ������б���
    /// </summary>
    procedure AddToOnlineList(const pvObject: TIocpCustomContext);
    /// <summary>
    /// �������б����Ƴ�
    /// </summary>
    procedure RemoveFromOnlineList(const pvObject: TIocpCustomContext); virtual;

    procedure DoAcceptExResponse(pvRequest: TIocpAcceptExRequest); virtual;

    /// <summary>
    /// ��ȡһ��SendRequest����ʵ�����Ǵӳ��е���
    /// </summary>
    function GetSendRequest: TIocpSendRequest;
    /// <summary>
    /// ��ȡһ��RecvRequest����
    /// </summary>
    function GetRecvRequest: TIocpRecvRequest;
    /// <summary>
    /// �ͷ�SendRequest�����س���
    /// </summary>
    function ReleaseSendRequest(pvObject: TIocpSendRequest): Boolean;
    /// <summary>
    /// �ȴ��������ӹر�
    /// </summary>
    function WaitForContext(pvTimeOut: Cardinal = 30000): Boolean;

    /// <summary>
    /// ����һ������ʵ��
    /// ͨ��ע���ContextClass���д���ʵ��
    /// </summary>
    function CreateContext: TIocpCustomContext; virtual;
    procedure OnCreateContext(const Context: TIocpCustomContext); virtual;
    /// <summary>
    /// �ͷ����Ӷ��󣬹黹�������
    /// </summary>
    function ReleaseClientContext(const pvObject: TIocpCustomContext): Boolean; virtual;

    procedure DoOpen(); virtual;
    procedure DoClose(); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// <summary>
    /// ֹͣIOCP�̣߳��ȴ������߳��ͷ�
    /// </summary>
    procedure Close; override;

    /// <summary>
    /// ����Ͽ��������ӣ������̷��ء�
    /// </summary>
    procedure DisconnectAll;

    /// <summary>
    /// ��ʼ����
    /// </summary>
    procedure Open; override;

    /// <summary>
    /// ��ȡһ�����Ӷ�������������û�У���ᴴ��һ���µ�ʵ��
    /// </summary>
    function GetClientContext: TIocpCustomContext; virtual;

    /// <summary>
    /// ��ȡ���߿ͻ��б�
    /// </summary>
    procedure GetOnlineContextList(pvList: TList);

    /// <summary>
    /// ���ͻ����Ӷ����Ƿ���Ч
    /// </summary>
    function CheckClientContextValid(const ClientContext: TIocpCustomContext): Boolean;

    /// <summary>
    /// ��ǰ������
    /// </summary>
    property OnlineContextCount: Integer read GetOnlineContextCount;
    /// <summary>
    /// ������Ͷ�������Iocp�����߳��еĴ����¼�
    /// </summary>
    property OnContextError: TOnContextError read FOnContextError write FOnContextError;
    /// <summary>
    /// �ͻ����Ӻ���յ�����ʱ�������¼�����Iocp�����̴߳���
    /// </summary>
    property OnDataReceived: TOnBufferReceived read FOnReceivedBuffer write FOnReceivedBuffer;
    /// <summary>
    /// �����ӽ����ɹ�ʱ�����¼�
    /// </summary>
    property OnContextConnected: TNotifyContextEvent read FOnContextConnected write FOnContextConnected;
    /// <summary>
    /// �����ӶϿ�ʱ�����¼�
    /// </summary>
    property OnContextDisconnected: TNotifyContextEvent read FOnContextDisconnected write FOnContextDisconnected;
  end;

  TIocpRequestEx = class(TIocpRequest)
  private
    FContext: TIocpCustomContext;
  public
    property Context: TIocpCustomContext read FContext;
  end;

  /// <summary>
  /// ���ݽ�������
  /// </summary>
  TIocpRecvRequest = class(TIocpRequestEx)
  private
    FOwner: TIocpCustom;
  protected
    FRecvBuffer: TWsaBuf;
    FInnerBuffer: TWsaBuf;
    FRecvdFlag: Cardinal;
    function PostRequest: Boolean; overload;
    function PostRequest(pvBuffer: PAnsiChar; len: Cardinal): Boolean; overload; virtual;
    procedure HandleResponse; override;
    procedure ResponseDone; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear;
    property Owner: TIocpCustom read FOwner;
    property RecvBuffer: TWsaBuf read FRecvBuffer;
  end;

  /// <summary>
  /// ������������
  /// </summary>
  TIocpSendRequest = class(TIocpRequestEx)
  private
    FOwner: TIocpCustom;
    //FNext: TIocpSendRequest;
    FIsBusying: Boolean;
    FAlive: Boolean;
    FBuf: Pointer;
    FLen: Cardinal;
    FSendBufferReleaseType: TDataReleaseType;
    FSendBuf: TWsaBuf;
    FBytesSize: Cardinal;
    FOnDataRequestCompleted: TOnDataRequestCompleted;
    procedure CheckClearSendBuffer;
  protected
    // post send a block
    function ExecuteSend: Boolean; virtual;
    procedure UnBindingSendBuffer;
    function InnerPostRequest(buf: Pointer; len: Cardinal): Boolean; virtual;

    procedure HandleResponse; override;
    procedure ResponseDone; override;

    procedure DoCleanUp; virtual;
    procedure SetBuffer(const buf: Pointer; const len: Cardinal; pvCopyBuf: Boolean = True);overload;
    procedure SetBuffer(const buf: Pointer; const len: Cardinal; pvBufReleaseType: TDataReleaseType); overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetStateInfo: string; override;

    property IsBusying: Boolean read FIsBusying;
    property Owner: TIocpCustom read FOwner;
    property OnDataRequestCompleted: TOnDataRequestCompleted read FOnDataRequestCompleted write FOnDataRequestCompleted;
  end;

  /// <summary>
  /// IO��������
  /// </summary>
  TIocpConnectExRequest = class(TIocpRequestEx)
  private
    FBytesSent: Cardinal;
  public
    constructor Create(const AContext: TIocpCustomContext); reintroduce;
    destructor Destroy; override;
    function PostRequest(const Host: AnsiString; Port: Word): Boolean;
  end;

  /// <summary>
  /// �Ͽ���������
  /// </summary>
  TIocpDisconnectExRequest = class(TIocpRequestEx)
  private
    FOwner: TIocpCustom;
  protected
    function PostRequest: Boolean;
    /// <summary>
    /// directly post request,
    /// </summary>
    function DirectlyPost: Boolean;
  end;

  /// <summary>
  /// ������������
  /// </summary>
  TIocpAcceptExRequest = class(TIocpRequestEx)
  private
    FOwner: TIocpCustom;
    FAcceptorMgr: TIocpAcceptorMgr;
    /// <summary>
    ///   acceptEx lpOutBuffer[in]
    ///     A pointer to a buffer that receives the first block of data sent on a new connection,
    ///       the local address of the server, and the remote address of the client.
    ///       The receive data is written to the first part of the buffer starting at offset zero,
    ///       while the addresses are written to the latter part of the buffer.
    ///       This parameter must be specified.
    /// </summary>
    FAcceptBuffer: array [0.. (SizeOf(TSockAddrIn) + 16) * 2 - 1] of byte;
    FOnAcceptedEx: TNotifyEvent;
    // get socket peer info on acceptEx reponse
    procedure getPeerInfo;
    procedure SetOnAcceptedEx(const Value: TNotifyEvent);
  protected
    procedure HandleResponse; override;
    procedure ResponseDone; override;
    function PostRequest: Boolean;
  public
    constructor Create(AOwner: TIocpCustom); reintroduce;
    property OnAcceptedEx: TNotifyEvent read FOnAcceptedEx write SetOnAcceptedEx;
  end;

  /// <summary>
  /// ����������ܹ�����
  /// </summary>
  TIocpAcceptorMgr = class(TObject)
  private
    FOwner: TIocpCustom;
    FListenSocket: TRawSocket;
    FLocker: TIocpLocker;
    FMaxRequest: Integer;
    FMinRequest: Integer;
    FCount: Integer;
    FAcceptExRequestPool: TBaseQueue;
  protected
    /// <summary>
    /// ����Ƿ���ҪͶ��AcceptEx
    /// </summary>
    procedure PostAcceptExRequest; overload;
  public
    constructor Create(AOwner: TIocpCustom; AListenSocket: TRawSocket);
    destructor Destroy; override;
    procedure Release(Request: TIocpAcceptExRequest);

    /// <summary>
    /// ����Ƿ���ҪͶ��AcceptEx
    /// </summary>
    procedure CheckAcceptExRequest();

    procedure ReleaseRequestObject(pvRequest: TIocpAcceptExRequest);
    function GetRequestObject: TIocpAcceptExRequest;
    /// <summary>
    /// �ȴ��������ӹر�
    /// </summary>
    function WaitForCancel(pvTimeOut: Cardinal): Boolean;
    property MaxRequest: Integer read FMaxRequest write FMaxRequest;
    property MinRequest: Integer read FMinRequest write FMinRequest;
  end;

  /// <summary>
  /// ����ʽTCP�ͻ���
  /// </summary>
  TIocpCustomBlockTcpSocket = class(TComponent)
  private
    FActive: Boolean;
    FRawSocket: TRawSocket;
    FReadTimeOut: Integer;
    FConnectTimeOut: Integer;
    FErrorCode: Integer;
    FHost: AnsiString;
    FPort: Word;
    FStream: TIocpBlockSocketStream;
    FBuffer: TMemoryStream;
    FRecvBufferSize: Integer;
    procedure SetActive(const Value: Boolean);
    procedure CheckSocketResult(pvSocketResult:Integer);
    function IsConnected: Boolean;
    function GetErrorCode: Integer;
    function GetRecvStream: TStream;
    procedure SetReadTimeOut(const Value: Integer);
    function GetRecvBufferIsEmpty: Boolean;
    function GetRecvBufferSize: Cardinal;
  protected
    procedure CreateSocket; virtual;
    procedure RaiseLastOSError(RaiseErr: Boolean = True);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Connect(RaiseError: Boolean = True): Boolean; overload; inline;
    function Connect(ATimeOut: Integer; RaiseError: Boolean = True): Boolean; overload;
    function Connect(const RemoteHost: AnsiString; RemotePort: Word; ATimeOut: Integer = 0): Boolean; overload;
    procedure Disconnect;
    procedure Open;
    procedure Close;
    function SetKeepAlive(pvKeepAliveTime: Integer = 5000): Boolean;
    class function IsIP(v: PAnsiChar): Longint;
    function DomainNameToAddr(const host: AnsiString): AnsiString;
    function Recv(Len: Integer = -1): AnsiString; overload;
    function Recv(buf: Pointer; len: Cardinal): Cardinal; overload;
    function Seek(Len: Integer = -1): Boolean;
    function Send(buf: Pointer; len: Cardinal): Cardinal; overload; inline;
    function Send(const Data: WideString): Cardinal; overload;
    function Send(const Data: AnsiString): Cardinal; overload;
    {$IFDEF UNICODE}
    function Send(const Data: UnicodeString): Cardinal; overload;
    {$ENDIF}
    procedure Send(Stream: TStream; SendSize: Integer = -1); overload;
    function ReadChar: Char;
    function ReadInteger: Integer;
    function ReadWord: Word;
    function ReadDouble: Double;
    function ReadInt64: Int64;
    function ReadSmallInt: SmallInt;
    function ReadString(const ABytes: Integer = -1): AnsiString;
    /// <summary>
    /// �������ݵ�����
    /// <param name="WaitRecvLen">�ȴ�����ָ�����ȵ�����ֱ�����ӶϿ�(Len����1ʱ��Ч��</param>
    /// </summary>
    function ReadStream(OutStream: TStream; Len: Integer; WaitRecvLen: Boolean = False): Integer;
    function ReadBytes(var Buffer: TBytes; AByteCount: Integer; AAppend: Boolean = True): Integer;
    function RecvBuffer(buf: Pointer; len: cardinal): Integer;
    function SendBuffer(buf: Pointer; len: cardinal): Integer;
    property Active: Boolean read FActive write SetActive;
    property Socket: TRawSocket read FRawSocket;
    property Connected: Boolean read IsConnected;
    property ErrorCode: Integer read GetErrorCode;
    property RecvStream: TStream read GetRecvStream;
    property RecvBufferSize: Cardinal read GetRecvBufferSize;
    property RecvBufferIsEmpty: Boolean read GetRecvBufferIsEmpty;
  published
    property RemoteHost: AnsiString read FHost write FHost;
    property RemotePort: Word read FPort write FPort;
    property ReadTimeOut: Integer read FReadTimeOut write SetReadTimeOut;
    property ConnectTimeOut: Integer read FConnectTimeOut write FConnectTimeOut default -1;
  end;

  /// <summary>
  /// ����ʽTCP�ͻ���
  /// </summary>
  TIocpCustomBlockUdpSocket = class(TIocpCustomBlockTcpSocket)
  protected
    procedure CreateSocket; override;
  public
    function Send(buf: Pointer; len: Cardinal; const Addr: AnsiString; Port: Word): Cardinal; overload;
    {$IFDEF UNICODE}
    function Send(const Data: UnicodeString; const Addr: AnsiString; Port: Word): Cardinal; overload;
    {$ENDIF}
    function Send(const Data: WideString; const Addr: AnsiString; Port: Word): Cardinal; overload;
    function Send(const Data: AnsiString; const Addr: AnsiString; Port: Word): Cardinal; overload;
  end;
  
  /// <summary>
  /// ����ʽ�ͻ���Socket��
  /// </summary>
  TIocpBlockSocketStream = class(TStream)
  protected
    FSocket: TIocpCustomBlockTcpSocket;
    FReadTimeOut: Integer;
    procedure SetSize(NewSize: Longint); override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(ASocket: TIocpCustomBlockTcpSocket);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  /// <summary>
  /// IOCP ���ݼ�����
  /// </summary>
  TIocpDataMonitor = class(TObject)
  private
    FLocker: TCriticalSection;
    FSentSize:Int64;
    FRecvSize:Int64;
    FPostWSASendSize: Int64;

    FHandleCreateCounter:Integer;
    FHandleDestroyCounter:Integer;

    FContextCreateCounter: Integer;
    FContextOutCounter:Integer;
    FContextReturnCounter:Integer;
    FAcceptExObjectCounter: Integer;

    FPushSendQueueCounter: Integer;
    FResponseSendObjectCounter:Integer;

    FPostWSASendCounter:Integer;
    FResponseWSASendCounter:Integer;

    FPostWSARecvCounter:Integer;
    FResponseWSARecvCounter:Integer;

    FPostWSAAcceptExCounter:Integer;
    FResponseWSAAcceptExCounter:Integer;

    FPostSendObjectCounter: Integer;
    FSendRequestAbortCounter: Integer;
    FSendRequestCreateCounter: Integer;
    FSendRequestOutCounter: Integer;
    FSendRequestReturnCounter: Integer;

    FHttpRequestCreateCounter: Integer;
    FHttpRequestExecCounter: Integer;
    FHttpRequestPopCounter: Integer;
    FHttpRequestPushCounter: Integer;
  protected
    procedure incHttpRequestPopCounter();
    procedure incHttpRequestPushCounter();
  public
    constructor Create;
    destructor Destroy; override;

    procedure incRecvdSize(pvSize:Cardinal);
    procedure incSentSize(pvSize:Cardinal);
    procedure incPostWSASendSize(pvSize:Cardinal);

    procedure incPostWSASendCounter();
    procedure incResponseWSASendCounter;

    procedure incPostWSARecvCounter();
    procedure incResponseWSARecvCounter;

    procedure IncAcceptExObjectCounter;
    procedure incPushSendQueueCounter;
    procedure incPostSendObjectCounter();
    procedure incResponseSendObjectCounter();
    procedure incHandleCreateCounter;
    procedure incHandleDestroyCounter;


    procedure incHttpRequestCreateCounter();
    procedure incHttpRequestExecCounter();

    procedure Clear;

    property ContextCreateCounter: Integer read FContextCreateCounter;
    property ContextOutCounter: Integer read FContextOutCounter;
    property ContextReturnCounter: Integer read FContextReturnCounter;

    property PushSendQueueCounter: Integer read FPushSendQueueCounter;
    property PostSendObjectCounter: Integer read FPostSendObjectCounter;
    property ResponseSendObjectCounter: Integer read FResponseSendObjectCounter;

    property PostWSAAcceptExCounter: Integer read FPostWSAAcceptExCounter;
    property PostWSARecvCounter: Integer read FPostWSARecvCounter;
    property PostWSASendCounter: Integer read FPostWSASendCounter;


    property PostWSASendSize: Int64 read FPostWSASendSize;
    property RecvSize: Int64 read FRecvSize;

    property AcceptExObjectCounter: Integer read FAcceptExObjectCounter;
    property HandleCreateCounter: Integer read FHandleCreateCounter;
    property HandleDestroyCounter: Integer read FHandleDestroyCounter;
    property ResponseWSAAcceptExCounter: Integer read FResponseWSAAcceptExCounter;
    property ResponseWSARecvCounter: Integer read FResponseWSARecvCounter;
    property ResponseWSASendCounter: Integer read FResponseWSASendCounter;
    property SendRequestAbortCounter: Integer read FSendRequestAbortCounter;
    property SendRequestCreateCounter: Integer read FSendRequestCreateCounter;
    property SendRequestOutCounter: Integer read FSendRequestOutCounter;
    property SendRequestReturnCounter: Integer read FSendRequestReturnCounter;
    property SentSize: Int64 read FSentSize;

    property HttpRequestCreateCounter: Integer read FHttpRequestCreateCounter;
    property HttpRequestExecCounter: Integer read FHttpRequestExecCounter;
    property HttpRequestPopCounter: Integer read FHttpRequestPopCounter;
    property HttpRequestPushCounter: Integer read FHttpRequestPushCounter;
  end;

type
  TIocpClientContext = class;

  TOnContextAcceptEvent = procedure(Socket: THandle; const Addr: string; Port: Word;
      var AllowAccept: Boolean) of object;

  /// <summary>
  /// Զ��������, ��Ӧ�ͻ��˵�һ������
  /// </summary>
  TIocpClientContext = class(TIocpCustomContext)
  private
    FRemotePort: Word;
    FRemoteAddr: AnsiString;
    function GetPeerAddr: Cardinal;
    function GetBindIP: string;
    function GetBindPort: Word;
    function GetRemoteAddr: string;
  protected
    procedure ReleaseClientContext(); override;
  public
    constructor Create(AOwner: TIocpCustom); override;
    destructor Destroy; override;

    procedure Disconnect; override;  

    property RemoteAddr: string read GetRemoteAddr;
    property RemotePort: Word read FRemotePort;
    property PeerPort: Word read FRemotePort;
    property PeerAddr: Cardinal read GetPeerAddr;
    property BindIP: string read GetBindIP;
    property BindPort: Word read GetBindPort;
  end;

  /// <summary>
  /// IOCP �����
  /// </summary>
  TIocpCustomTcpServer = class(TIocpCustom)
  private
    FListenSocket: TRawSocket;
    FKeepAlive: Boolean;
    FPort: Word;
    FMaxSendingQueueSize: Integer;
    FKickOutInterval: Integer;
    FIocpAcceptorMgr: TIocpAcceptorMgr;
    FContextPool: TBaseQueue;
    FMaxContextPoolSize: Integer;
    FTimeOutKickOut: TTimer;

    FOnContextAccept: TOnContextAcceptEvent;
    function GetClientCount: Integer;
    function GetMaxTaskWorker: Integer;
    procedure SetMaxTaskWorker(const Value: Integer);
  protected
    procedure CreateSocket; virtual;
    /// <summary>
    /// ��ʱ����ʱ����
    /// </summary>
    procedure DoKickOutTimer(Sender: TObject);
    /// <summary>
    /// ��Ͷ�ݵ�AcceptEx������Ӧʱ�е���
    /// </summary>
    procedure DoAcceptExResponse(pvRequest: TIocpAcceptExRequest); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;
    function GetStateInfo: string;

    function CreateContext: TIocpCustomContext; override;

    function GetClientContext: TIocpCustomContext; override;
    function ReleaseClientContext(const pvObject: TIocpCustomContext): Boolean; override;

    /// <summary>
    /// ��������ÿ��������������Ͷ��У������������ٽ���Ͷ��
    /// </summary>
    procedure SetMaxSendingQueueSize(pvSize: Integer);

    /// <summary>
    /// ����ContextHandle�������б��в��Ҷ�Ӧ��Contextʵ��
    /// </summary>
    function FindContext(ContextHandle: Cardinal): TIocpClientContext;

    /// <summary>
    /// ��ʱ���, �������Timeoutָ����ʱ�仹û���κ����ݽ������ݼ�¼��
    /// �ͽ��йر�����, ʹ��ѭ�����
    /// </summary>
    procedure KickOut(pvTimeOut:Cardinal = 60000);

    procedure RegisterContextClass(pvContextClass: TIocpContextClass);
    procedure RegisterSendRequestClass(pvClass: TIocpSendRequestClass);

    procedure Start;
    procedure Stop;

    // ��ǰ�ͻ�������
    property ClientCount: Integer read GetClientCount;
    // �������ӹ�����
    property IocpAcceptorMgr: TIocpAcceptorMgr read FIocpAcceptorMgr;
    // ÿ����������Ͷ���
    property MaxSendingQueueSize: Integer read FMaxSendingQueueSize;
  published
    property Active default False;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    property BindAddr;

    property RecvBufferSize;
    property SendBufferSize;

    /// <summary>
    /// Ĭ�������Ķ˿�
    /// </summary>
    property ListenPort: Word read FPort write FPort default 9000;
    /// <summary>
    /// ���������ҵ�߳�����
    /// </summary>
    property MaxTaskWorker: Integer read GetMaxTaskWorker write SetMaxTaskWorker;
    /// <summary>
    /// ���ӳ�����С (С��1ʱ�����޴�С)
    /// </summary>
    property MaxContextPoolSize: Integer read FMaxContextPoolSize write FMaxContextPoolSize;
    /// <summary>
    /// �����Զ���ʱ�߳����ʱ�� (ע�⣬Ϊ�˲�Ӱ�����ܣ�ÿ10��ִ��һ���Զ��߳��������������ֵֻ�Ǹ��ο�)
    /// </summary>
    property KickOutInterval: Integer read FKickOutInterval write FKickOutInterval default 30000;
    /// <summary>
    /// ����������ʱ�����¼� (����ִ��)
    /// </summary>
    property OnContextAccept: TOnContextAcceptEvent read FOnContextAccept write FOnContextAccept;
    /// <summary>
    /// ���ӽ������ʱ�����¼� (����ִ��)
    /// </summary>
    property OnContextConnected;
    /// <summary>
    /// ���ӶϿ��󴥷��¼�  (����ִ��)
    /// </summary>
    property OnContextDisconnected;
    /// <summary>
    /// ��������ʱ�����¼� (����ִ��)
    /// </summary>
    property OnContextError;
    /// <summary>
    /// ���������¼� (����ִ��)
    /// </summary>
    property OnDataReceived;
    /// <summary>
    /// �ڲ�״̬��Ϣ����ӿ� (����ִ��)
    /// </summary>
    property OnStateInfo;
  end;

type
  TIocpUdpServer = class;
  TIocpUdpRecvRequest = class;
  TIocpUdpRequest = TIocpUdpRecvRequest;
  TIocpUdpSendRequest = class;
  TIocpUdpSendRequestClass = class of TIocpUdpSendRequest;

  TOnUdpBufferReceived = procedure(Request: TIocpUdpRequest; buf: Pointer; len: Cardinal) of object;

  /// <summary>
  /// UDP ���ݽ�������
  /// </summary>
  TIocpUdpRecvRequest = class(TIocpRequest)
  private
    FOwner: TIocpUdpServer;
    FRecvBuffer: TWsaBuf;
    FRecvdFlag: Cardinal;
    FFrom: TSockAddrIn;
    FFromLen: Integer;
    function GetPeerAddr: Cardinal;
    function GetRemoteAddr: AnsiString;
    function GetRemotePort: Word;
  protected
    procedure HandleResponse; override;
    function PostRequest: Boolean;
    procedure DoRecvData; virtual;
  public
    constructor Create(AOwner: TIocpUdpServer); reintroduce;
    destructor Destroy; override;
    procedure Clear;

    procedure Send(buf: Pointer; len: Cardinal); overload;
    {$IFDEF UNICODE}
    procedure Send(const Data: UnicodeString); overload;
    {$ENDIF}
    procedure Send(const Data: WideString); overload;
    procedure Send(const Data: AnsiString); overload;

    property Owner: TIocpUdpServer read FOwner;
    property RemoteAddr: AnsiString read GetRemoteAddr;
    property RemotePort: Word read GetRemotePort;
    property PeerPort: Word read GetRemotePort;
    property PeerAddr: Cardinal read GetPeerAddr;
  end;

  /// <summary>
  /// UDP Send ����
  /// </summary>
  TIocpUdpSendRequest = class(TIocpRequest)
  private
    FOwner: TIocpUdpServer;
    FBuf: Pointer;
    FLen: Cardinal;
    FSendBuf: TWsaBuf;
    FAddr: TSockAddrIn;
    FSendBufferReleaseType: TDataReleaseType;
    FIsBusying: Boolean;
    FAlive: Boolean;
    procedure CheckClearSendBuffer;
  protected
    function ExecuteSend: Boolean; virtual;
    procedure UnBindingSendBuffer;
    function InnerPostRequest(buf: Pointer; len: Cardinal): Boolean;

    procedure HandleResponse; override;
    procedure ResponseDone; override;

    procedure DoCleanUp; virtual;
    procedure SetBuffer(buf: Pointer; len: Cardinal; pvCopyBuf: Boolean = True); overload;
    procedure SetBuffer(buf: Pointer; len: Cardinal; pvBufReleaseType: TDataReleaseType); overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Owner: TIocpUdpServer read FOwner;
    property IsBusying: Boolean read FIsBusying;
  end;

  /// <summary>
  /// IOCP UDP �����
  /// </summary>
  {$IFDEF SupportEx}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TIocpUdpServer = class(TIocpBase)
  private
    FListenSocket: TRawSocket;
    FPort: Word;
    FOnReceivedBuffer: TOnUdpBufferReceived;
    FSendRef: Integer;
    FRecvItems: array of TObject;
    FSendRequestPool: TBaseQueue;
    FSendRequestClass: TIocpUdpSendRequestClass;
    FSendRequestList: TIocpRequestLinkList;
    function GetSocketHandle: TSocket;
    function GetMaxSendingQueueSize: Integer;
  protected
    procedure CreateSocket;
    procedure ClearRecvObjs;
    procedure DoReceiveData(Sender: TIocpUdpRequest); virtual;
    function WaitFor(pvTimeOut: Cardinal = 30000): Boolean;
    /// <summary>
    /// ��鲢Ͷ����һ����������
    /// </summary>
    function CheckNextSendRequest: Boolean;
    /// <summary>
    /// ��ȡһ��SendRequest����ʵ�����Ǵӳ��е���
    /// </summary>
    function GetSendRequest: TIocpUdpSendRequest;
    /// <summary>
    /// �ͷ�SendRequest�����س���
    /// </summary>
    function ReleaseSendRequest(pvObject: TIocpUdpSendRequest): Boolean;
    /// <summary>
    /// 1. Ͷ�ݷ������󵽶�����, ������Ͷ����������� False
    /// 2. ��� sending ��־, ��� sending �� False �ſ�ʼ
    /// </summary>
    function InnerPostSendRequestAndCheckStart(pvSendRequest: TIocpUdpSendRequest): Boolean;
    function InnerSendData(const Dest: TSockAddrin; buf: Pointer; len: Cardinal;
      pvBufReleaseType: TDataReleaseType; pvTag: Integer = 0; pvTagData: Pointer = nil): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open; override;
    procedure Close; override;

    procedure RegisterSendRequestClass(pvClass: TIocpUdpSendRequestClass);
    /// <summary>
    /// ���÷��Ͷ������޴�С
    /// </summary>
    procedure SetMaxSendingQueueSize(pvSize: Integer);

    procedure PostNextSendRequest; virtual;

    function Send(const Dest: TSockAddrin; buf: Pointer; len: Cardinal; CopyBuf: Boolean = True): Boolean; overload;
    function Send(const Dest: TSockAddrin; buf: Pointer; len: Cardinal; BufReleaseType: TDataReleaseType): Boolean; overload;

    property SocketHandle: TSocket read GetSocketHandle;
    property MaxSendingQueueSize: Integer read GetMaxSendingQueueSize;
  published
    property Active;
    /// <summary>
    /// Ĭ�������Ķ˿�
    /// </summary>
    property ListenPort: Word read FPort write FPort default 9000;
    /// <summary>
    /// ���ݽ����¼�
    /// </summary>
    property OnDataReceived: TOnUdpBufferReceived read FOnReceivedBuffer write FOnReceivedBuffer;
    property OnStateInfo;
  end;

type
  /// <summary>
  /// Զ�������࣬��Ӧ���������ÿ������
  /// </summary>
  TIocpRemoteContext = class(TIocpCustomContext)
  private
    FIsConnecting: Boolean;
    FAutoReConnect: Boolean;
    FConnectExRequest: TIocpConnectExRequest;
    FLastDisconnectTime: Int64;
    FBindAddr: AnsiString;
    FHost: AnsiString;
    FPort: Word;
    function CanAutoReConnect: Boolean;
    procedure ReCreateSocket;
    procedure PostConnectRequest;
    function GetBindAddr: AnsiString;
    procedure SetBindAddr(const Value: AnsiString);
  protected
    procedure OnConnecteExResponse(pvObject: TObject);
    procedure OnDisconnected; override;
    procedure OnConnected; override;
    procedure SetSocketState(pvState:TSocketState); override;
    procedure ReleaseClientContext(); override;
  public
    constructor Create(AOwner: TIocpCustom); override;
    destructor Destroy; override;
    /// <summary>
    /// �������ӣ�Async �Ƿ�ʹ���첽��
    /// <param name="ASync">�Ƿ�ʹ���첽</param>
    /// <param name="pvTimeOut">��ʱֵ����AsyncΪFalseʱ��> 0 ʱ��ʱ��Ч��Ĭ��Ϊ -1���޳�ʱ���á�</param>
    /// </summary>
    procedure Connect(ASync: Boolean = False; pvTimeOut: Integer = -1); overload;
    /// <summary>
    /// �������ӣ�Async �Ƿ�ʹ���첽��
    /// <param name="ASync">�Ƿ�ʹ���첽</param>
    /// <param name="pvTimeOut">��ʱֵ����AsyncΪFalseʱ��> 0 ʱ��ʱ��Ч��Ĭ��Ϊ -1���޳�ʱ���á�</param>
    /// </summary>
    procedure Connect(const AHost: AnsiString; APort: Word; ASync: Boolean = False;
      pvTimeOut: Integer = -1); overload;

    /// <summary>
    /// ���ø����Ӷ�����Զ���������
    /// </summary>
    property AutoReConnect: Boolean read FAutoReConnect write FAutoReConnect;

    property BindAddr: AnsiString read GetBindAddr write SetBindAddr;
    property Host: AnsiString read FHost write FHost;
    property Port: Word read FPort write FPort;
  end;

  /// <summary>
  /// IOCP TCP �ͻ���
  /// </summary>
  TIocpCustomTcpClient = class(TIocpCustom)
  private
    FDisableAutoConnect: Boolean;
    FConnectTimeOut: Integer;
    FReconnectRequestPool: TObjectPool;
    function GetCount: Integer;
    function GetItems(Index: Integer): TIocpRemoteContext;
    function CreateReconnectRequest: TObject;
  protected
    {$IFDEF UNICODE}
    FList: TObjectList<TIocpRemoteContext>;
    {$ELSE}
    FList: TObjectList;
    {$ENDIF}
    /// <summary>
    /// ��Ӧ��ɣ��黹������󵽳�
    /// </summary>
    procedure OnReconnectRequestResponseDone(pvObject: TObject);
    /// <summary>
    /// ��Ӧ��������Request
    /// </summary>
    procedure OnReconnectRequestResponse(pvObject: TObject);
    /// <summary>
    /// Ͷ�����������¼�
    /// </summary>
    procedure PostReconnectRequestEvent(const pvContext: TIocpRemoteContext);

    function ReleaseClientContext(const pvObject: TIocpCustomContext): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateContext: TIocpCustomContext; override;

    /// <summary>
    /// ע��һ���Զ����������
    /// </summary>
    procedure RegisterContextClass(pvContextClass: TIocpContextClass);

    /// <summary>
    /// ���һ�����Ӷ���
    /// </summary>
    function Add: TIocpRemoteContext; virtual;

    
    /// <summary>
    /// ����һ��������
    /// </summary>
    function Connect(const Host: AnsiString; Port: Word;
      AutoReConnect: Boolean = False; ASync: Boolean = True): TIocpRemoteContext; overload;
    
    /// <summary>
    /// ����һ��������
    /// </summary>
    function Connect(const Host: AnsiString; Port: Word; const BindAddr: AnsiString;
      AutoReConnect: Boolean = False; ASync: Boolean = True): TIocpRemoteContext; overload;

    /// <summary>
    /// ɾ��һ������
    /// </summary>
    procedure Remove(const Value: TIocpRemoteContext);

    /// <summary>
    /// ɾ��ָ����������
    /// </summary>
    procedure Delete(Index: Integer); overload;

    /// <summary>
    /// ֱ��ɾ��һ�����ӣ������Ƿ�Ͽ����ӣ�ʹ��ʱע�� (�־��ܿ�  ~)
    /// </summary>
    function Delete(const pvContext: TIocpCustomContext): Integer; overload;

    /// <summary>
    /// ɾ��ȫ������
    /// </summary>
    procedure RemoveAll();


    /// <summary>
    /// �ܵ����Ӷ�������
    /// </summary>
    property Count: Integer read GetCount;
    /// <summary>
    /// ͨ��λ��������ȡ���е�һ������
    /// </summary>
    property Items[Index: Integer]: TIocpRemoteContext read GetItems; default;
  published
    property Active;
    /// <summary>
    /// ��ֹ�������Ӷ����Զ�����
    /// </summary>
    property DisableAutoConnect: Boolean read FDisableAutoConnect write FDisableAutoConnect;

    /// <summary>
    /// ���ӳ�ʱ����, < 1 ʱ��ʹ���첽����ʱ����Ч (����ms)
    /// </summary>
    property ConnectTimeOut: Integer read FConnectTimeOut write FConnectTimeOut default -1;

    property RecvBufferSize;
    property SendBufferSize;

    property OnContextError;
    property OnContextConnected;
    property OnContextDisconnected;
    property OnDataReceived;
    property OnSendRequestResponse;
    property OnStateInfo;
  end;

function TransByteSize(const pvByte: Int64): string;
function BytesToString(const ABytes: TBytes; const AStartIndex: Integer = 0;
  AMaxCount: Integer = MaxInt): string;
function GetRunTimeInfo: string;
procedure ResetRunTime;

implementation

var
  Workers: TIocpTask;
  StartRunTime: Int64 = 0;

const
  RECONNECT_INTERVAL = 1000; // ����������������ӹ��죬����OnDisconnected��û�д������, 1��
const
  ADDRESS_LENGTH_EX = 18;

procedure ResetRunTime();
begin
  StartRunTime := GetTimestamp;
end;

function TransByteSize(const pvByte: Int64): string;
var
  lvTB, lvGB, lvMB, lvKB: Word;
  lvRemain: Int64;
begin
  lvRemain := pvByte;
  lvTB := Trunc(lvRemain / BytePerGB / 1024);
  lvGB := Trunc(lvRemain / BytePerGB);
  lvGB := lvGB mod 1024;      // trunc TB
  lvRemain := lvRemain mod BytePerGB;
  lvMB := Trunc(lvRemain/BytePerMB);
  lvRemain := lvRemain mod BytePerMB;
  lvKB := Trunc(lvRemain/BytePerKB);
  lvRemain := lvRemain mod BytePerKB;
  Result := Format('%d TB, %d GB, %d MB, %d KB, %d B', [lvTB, lvGB, lvMB, lvKB, lvRemain]);
end;

function GetRunTimeInfo: string;
var
  lvMSec, lvRemain: Int64;
  lvDay, lvHour, lvMin, lvSec: Integer;
begin
  lvMSec := GetTimestamp - StartRunTime;
  lvDay := Trunc(lvMSec / MSecsPerDay);
  lvRemain := lvMSec mod MSecsPerDay;

  lvHour := Trunc(lvRemain / (MSecsPerSec * 60 * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60 * 60);

  lvMin := Trunc(lvRemain / (MSecsPerSec * 60));
  lvRemain := lvRemain mod (MSecsPerSec * 60);

  lvSec := Trunc(lvRemain / (MSecsPerSec));

  if lvDay > 0 then
    Result := Result + IntToStr(lvDay) + ' d ';
  if lvHour > 0 then
    Result := Result + IntToStr(lvHour) + ' h ';
  if lvMin > 0 then
    Result := Result + IntToStr(lvMin) + ' m ';
  if lvSec > 0 then
    Result := Result + IntToStr(lvSec) + ' s ';
end;

{ TIocpCustomContext }

function TIocpCustomContext.CheckNextSendRequest: Boolean;
var
  lvRequest: TIocpSendRequest;
begin
  Result := False;
  Assert(FOwner <> nil);
  FContextLocker.Enter();
  try
    lvRequest := TIocpSendRequest(FSendRequestList.Pop);
    if lvRequest = nil then begin
      FSending := False;
      Exit;
    end;
  finally
    FContextLocker.Leave;
  end;

  if lvRequest <> nil then begin
    FSendRequest := lvRequest;
    if lvRequest.ExecuteSend then begin
      Result := True;
      if (FOwner.FDataMoniter <> nil) then
        FOwner.FDataMoniter.IncPostSendObjectCounter;
    end else begin
      FSendRequest := nil;

      /// cancel request
      lvRequest.CancelRequest;

      {$IFDEF DEBUG_ON}
      FOwner.DoStateMsgD(Self, '[0x%.4x] CheckNextSendRequest.ExecuteSend Return False',
         [SocketHandle]);
      {$ENDIF}
      // ����Ͽ����� kick out the clientContext
      RequestDisconnect(lvRequest);
      FOwner.ReleaseSendRequest(lvRequest);
    end;
  end;
end;

procedure TIocpCustomContext.CheckReleaseRes;
var
  lvRequest: TIocpSendRequest;
begin
  if not Assigned(FOwner) then Exit;
  while True do begin
    lvRequest := TIocpSendRequest(FSendRequestList.Pop);
    if lvRequest <> nil then begin
      if (FOwner.FDataMoniter <> nil) then
        InterlockedIncrement(FOwner.FDataMoniter.FSendRequestAbortCounter);
      lvRequest.CancelRequest;
      FOwner.ReleaseSendRequest(lvRequest)
    end else
      Break;
  end;
end;

procedure TIocpCustomContext.Close;
begin
  RequestDisconnect();
end;

procedure TIocpCustomContext.CloseConnection;
begin
  PostWSACloseRequest();
end;

constructor TIocpCustomContext.Create(AOwner: TIocpCustom);
begin
  FOwner := AOwner;
  FRefCount := 0;
  FActive := False;
  FAlive := False;
  FContextLocker := TIocpLocker.Create();
  //FContextLocker.Name := 'ContextLocker';
  FRawSocket := TRawSocket.Create();
  FSocketHandle := FRawSocket.SocketHandle;
  FSendRequestList := TIocpRequestLinkList.Create(64);
  FRecvRequest := AOwner.GetRecvRequest();
  FRecvRequest.FOwner := AOwner;
  FRecvRequest.FContext := Self;
  FCloseRequest := TIocpActionRequest.Create;
  FCloseRequest.OnResponse := OnCloseRequestResponse;
end;

procedure TIocpCustomContext.CreateSocket(IsOverlapped: Boolean);
begin
  FRawSocket.CreateTcpSocket(IsOverlapped);
  FSocketHandle := FRawSocket.SocketHandle;
end;

function TIocpCustomContext.DecReferenceCounter(pvObj: TObject; const pvDebugInfo: string): Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    PostCloseRequest();
  if IsDebugMode and Assigned(FOwner) then
    FOwner.DoStateMsgD(Self, strConn_CounterDec, [Result, IntPtr(pvObj), pvDebugInfo]);
end;

destructor TIocpCustomContext.Destroy;
begin
  if IsDebugMode then begin
    if FRefCount <> 0 then
      Assert(FRefCount = 0);
  end;

  if Assigned(FRecvRequest) then begin
    if FRecvRequest.Responding then begin
      FRecvRequest.FContext := nil;
      FRecvRequest.FDestroyOnResponseEnd := True;
    end else
      FreeAndNil(FRecvRequest);
  end;

  if Assigned(FSendRequestList) then   
    Assert(FSendRequestList.Count = 0);
  FreeAndNil(FSendRequestList);
  FreeAndNil(FContextLocker);
  FreeAndNil(FRawSocket);
  FreeAndNil(FCloseRequest);

  inherited Destroy;
end;

procedure TIocpCustomContext.Disconnect;
begin
  RequestDisconnect();
end;

procedure TIocpCustomContext.DoCleanUp;
begin
  FLastActive := 0;
  FRequestDisconnect := 0;
  FSending := False;
  if IsDebugMode then begin
    //if Assigned(FOwner) then
    //  FOwner.DoStateMsgD(Self, '-(%d):0x%.4x, %s', [FRefCount, IntPtr(Self), 'DoCleanUp']);
    if FRefCount <> 0 then
      Assert(FRefCount = 0);
    Assert(not FActive);
  end;
  if Assigned(FRecvRequest) then
    FRecvRequest.Clear;
end;

procedure TIocpCustomContext.DoConnected;
begin
  FRequestDisconnect := 0;
  FSendStreaming := 0;
  FLastActive := GetTimestamp;
  FLastActivity := FLastActive;
  Assert(FOwner <> nil);

  if IncReferenceCounter(nil, 'OnConnected') then begin
    try
      if FActive then begin
        // �Ѿ���������ӣ������κδ���
        if IsDebugMode then
          Assert(not FActive);
        {$IFDEF DEBUG_ON}
        FOwner.DoStateMsgD(Self, strSocket_ConnActived);
        {$ENDIF}
      end else begin
        FHandle := FOwner.RequestContextHandle; //����OwnerΪ�Լ�����һ��Handle
        FActive := True;
        FOwner.AddToOnlineList(Self);
        
        if Assigned(FOwner.FOnContextConnected) then
          FOwner.FOnContextConnected(Self);

        try
          OnConnected();
        except
          {$IFDEF DEBUG_ON}
          FOwner.DoStateMsgE(Self, strSocket_ConnError, Exception(ExceptObject));
          {$ENDIF}
        end;
        if Assigned(FOnConnectedEvent) then
          FOnConnectedEvent(Self);
        // ����Ϊ���ӳɹ�״̬���������������
        SetSocketState(ssConnected);
        if FActive then         
          PostWSARecvRequest
        else
          RequestDisconnect(nil, '����ֹͣ, �������');
      end;
    finally
      DecReferenceCounter(nil, 'OnConnected');  
    end;
  end;
end;

procedure TIocpCustomContext.DoDisconnect;
begin
  RequestDisconnect;
end;

procedure TIocpCustomContext.DoError(ErrorCode: Integer);
begin
  FLastErrorCode := ErrorCode;
  FOwner.DoClientContextError(Self, ErrorCode);
end;

procedure TIocpCustomContext.DoReceiveData;
begin
  OnRecvBuffer(FRecvRequest.FRecvBuffer.buf,
    FRecvRequest.FBytesTransferred,
    FRecvRequest.ErrorCode);
  if Assigned(FOwner) and (not GetIsDisconnect) then
    FOwner.DoReceiveData(Self, FRecvRequest);
end;

procedure TIocpCustomContext.DoSendRequestCompleted(pvRequest: TIocpSendRequest);
begin
end;

procedure TIocpCustomContext.DoSendRequestRespnonse(
  pvRequest: TIocpSendRequest);
begin
  FLastActive := GetTimestamp;
  if Assigned(FOwner.FOnSendRequestResponse) then
    FOwner.FOnSendRequestResponse(Self, pvRequest);
end;

function TIocpCustomContext.GetIsDisconnect: Boolean;
begin
  Result := (not Assigned(Self)) or (FRequestDisconnect = 1);
end;

function TIocpCustomContext.GetSendRequest: TIocpSendRequest;
begin
  Result := FOwner.GetSendRequest;
  Assert(Result <> nil);
  Result.FContext := Self;
end;

function TIocpCustomContext.GetSocketHandle: TSocket;
begin
  Result := FSocketHandle;
end;

function TIocpCustomContext.IncReferenceCounter(pvObj: TObject; const pvDebugInfo: string): Boolean;
var
  lvRefCount: Integer;
begin
  Result := False;
  Assert(Self <> nil);
  if AtomicAdd(FRefCount, 0) < 0 then
    Exit;
  if FRequestDisconnect = 1 then
    Exit;
  lvRefCount := AtomicIncrement(FRefCount);
  if lvRefCount > 0 then
    Result := True
  else
    AtomicDecrement(FRefCount); // һ�㲻�ᵽ����
  if Assigned(FOwner) and IsDebugMode then
    FOwner.DoStateMsgD(Self, strConn_CounterInc, [lvRefCount, IntPtr(pvObj), pvDebugInfo]);
end;

procedure TIocpCustomContext.InnerCloseContext;
begin
  Assert(FOwner <> nil);
  {$IFDEF DEBUG_ON}
  if FRefCount <> 0 then
    FOwner.DoStateMsgD(Self, 'InnerCloseContext ContextCounter: %d', [FRefCount]);
  if not FActive then begin
    FOwner.DoStateMsgD(Self, 'InnerCloseContext Active is False');
    Exit;
  end;
  {$ELSE}
  if not FActive then Exit;
  {$ENDIF}
  try
    FActive := False;
    FRawSocket.Close;
    CheckReleaseRes;
    try
      if FOwner.Active then begin
        if Assigned(FOwner.FOnContextDisconnected) then
          FOwner.FOnContextDisconnected(Self);
        OnDisconnected;
      end;
      // ����Socket״̬
      SetSocketState(ssDisconnected);
    except
      {$IFDEF DEBUG_ON}
      FOwner.DoStateMsgE(Self, Exception(ExceptObject));
      {$ENDIF}
    end;
  finally
    FOwner.RemoveFromOnlineList(Self);
    ReleaseClientContext;
  end;
end;

function TIocpCustomContext.InnerPostSendRequestAndCheckStart(
  pvSendRequest: TIocpSendRequest): Boolean;
var
  lvStart: Boolean;
begin
  lvStart := False;
  FContextLocker.Enter();
  //try
    Result := FSendRequestList.Push(pvSendRequest);
    if Result and (not FSending) then begin
      FSending := true;
      lvStart := true;  // start send work
    end;
  //finally
    FContextLocker.Leave;
  //end;

  {$IFDEF DEBUG_ON}
  if (not Result) and Assigned(FOwner) then
    FOwner.DoStateMsgE(Self, strSend_PushFail, [SocketHandle, FSendRequestList.Count, FSendRequestList.MaxSize]);
  {$ENDIF}

  if lvStart then begin  // start send work
    if (Assigned(FOwner)) and (FOwner.FDataMoniter <> nil) then
      FOwner.FDataMoniter.incPushSendQueueCounter;
    CheckNextSendRequest;
  end;
end;

function TIocpCustomContext.InnerSendData(buf: Pointer; len: Cardinal;
  pvBufReleaseType: TDataReleaseType; pvTag: Integer;
  pvTagData: Pointer): Boolean;
var
  lvRequest: TIocpSendRequest;
begin
  Result := False;
  if Active then begin
    if IncReferenceCounter(Self, 'InnerSendData') then begin
      try
        lvRequest := GetSendRequest;
        lvRequest.SetBuffer(buf, len, pvBufReleaseType);
        lvRequest.Tag := pvTag;
        lvRequest.Data := pvTagData;
        Result := InnerPostSendRequestAndCheckStart(lvRequest);
        if not Result then begin
          /// Push Fail unbinding buf
          lvRequest.UnBindingSendBuffer;

          {$IFDEF DEBUG_ON}
          Self.RequestDisconnect(lvRequest, Format(strSend_PushFail, [SocketHandle,
            FSendRequestList.Count, FSendRequestList.MaxSize]));
          {$ELSE}
          Self.RequestDisconnect(lvRequest);
          {$ENDIF}

          lvRequest.CancelRequest;
          FOwner.ReleaseSendRequest(lvRequest);
        end;
      finally
        decReferenceCounter(self, 'InnerSendData');
      end;
    end;
  end;
end;

procedure TIocpCustomContext.Lock;
begin
  FContextLocker.Enter();
end;

function TIocpCustomContext.LockContext(pvObj: TObject; const pvDebugInfo: string): Boolean;
begin
  Result := Assigned(Self) and IncReferenceCounter(pvObj, pvDebugInfo)
end;

procedure TIocpCustomContext.OnCloseRequestResponse(Sender: TObject);
begin
  if Assigned(Self) then
    InnerCloseContext();
end;

procedure TIocpCustomContext.OnConnected;
begin
end;

procedure TIocpCustomContext.OnDisconnected;
begin
end;

procedure TIocpCustomContext.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrorCode: Integer);
begin
end;

procedure TIocpCustomContext.PostCloseRequest;
begin
  if Assigned(Self) and Assigned(FOwner) then
    FOwner.FIocpEngine.PostRequest(FCloseRequest);
end;

procedure TIocpCustomContext.PostNextSendRequest;
begin
  FContextLocker.Enter();
  try
    if not CheckNextSendRequest then FSending := false;
  finally
    FContextLocker.Leave;
  end;
end;

procedure TIocpCustomContext.PostWSACloseRequest;
begin
  PostWSASendRequest(nil, 0, dtNone, -1);
end;

procedure TIocpCustomContext.PostWSARecvRequest;
var
  lvSuccFlag: Boolean;
begin
  lvSuccFlag := False;
  if IncReferenceCounter(nil) then
  try
    if Assigned(FRecvRequest) then  
      lvSuccFlag := FRecvRequest.PostRequest;
  finally
    if not lvSuccFlag then
      DecReferenceCounter(nil);
  end;
end;

function TIocpCustomContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
  pvCopyBuf: Boolean): Boolean;
var
  lvBuf: PAnsiChar;
begin
  if (not Assigned(Self)) or (buf = nil) then begin
    Result := False;
    Exit;
  end;
  if pvCopyBuf and (len <= Owner.SendBufferSize) then begin
    if len = 0 then begin
      Result := False;
      Exit;
    end;

    {$IFDEF UseSendMemPool}
    lvBuf := FOwner.PopMem;
    Move(buf^, lvBuf^, len);
    Result := PostWSASendRequest(lvBuf, len, dtMemPool);
    if not Result then //post fail
      FOwner.PushMem(lvBuf);
    {$ELSE}
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    Result := PostWSASendRequest(lvBuf, len, dtFreeMem);
    if not Result then //post fail
      FreeMem(lvBuf);
    {$ENDIF}
  end else
    Result := PostWSASendRequest(buf, len, dtNone);
end;

function TIocpCustomContext.PostWSASendRequest(buf: Pointer; len: Cardinal;
  pvBufReleaseType: TDataReleaseType; pvTag: Integer; pvTagData: Pointer): Boolean;

  // �ֿ�Ͷ��
  function BlockSend(lvSrc: PAnsiChar; lvSrcLen: Cardinal; pvTag: Integer; pvTagData: Pointer): Boolean;
  var
    lvBuf: PAnsiChar;
    lvLen, lvBuffSize: Cardinal;
    lvFreeType: TDataReleaseType;
    lvRequest: TIocpSendRequest;
  begin
    lvBuffSize := Owner.SendBufferSize;
    Result := False;

    while lvSrcLen > 0 do begin
      if lvSrcLen > lvBuffSize then
        lvLen := lvBuffSize
      else
        lvLen := lvSrcLen;
      {$IFDEF UseSendMemPool}
      lvBuf := FOwner.PopMem;
      Move(lvSrc^, lvBuf^, lvLen);
      lvFreeType := dtMemPool;
      {$ELSE}
      GetMem(lvBuf, lvLen);
      Move(lvSrc^, lvBuf^, lvLen);
      lvFreeType := dtFreeMem;
      {$ENDIF}

      Result := False;

      lvRequest := GetSendRequest;
      lvRequest.SetBuffer(lvBuf, lvLen, lvFreeType);
      lvRequest.Tag := pvTag;
      lvRequest.Data := pvTagData;
      repeat
        if IncReferenceCounter(Self, 'BlockSend') then begin
          try
            Result := InnerPostSendRequestAndCheckStart(lvRequest);
            if not Result then begin
              {$IFDEF MSWINDOWS}
              SwitchToThread;
              {$ELSE}
              TThread.Yield;
              {$ENDIF}
              Sleep(50);
            end;
          finally
            decReferenceCounter(self, 'BlockSend');
          end;
        end else
          Break;
      until (Result);

      if not Result then begin
        lvRequest.UnBindingSendBuffer;
        lvRequest.CancelRequest;
        FOwner.ReleaseSendRequest(lvRequest);

        {$IFDEF UseSendMemPool}
        FOwner.PushMem(lvBuf);
        {$ELSE}
        FreeMem(lvBuf);
        {$ENDIF}
        Break;
      end;

      Inc(lvSrc, lvLen);
      Dec(lvSrcLen, lvLen);
    end;
  end;

begin
  Result := False;
  if (not Assigned(Self)) then Exit;
  if Active then begin
    if len <= Owner.SendBufferSize then
      Result := InnerSendData(buf, len, pvBufReleaseType, pvTag, pvTagData)
    else begin
      // ���ڷ��ͻ�����ʱ���ֿ鷢��
      try
        Result := BlockSend(buf, len, pvTag, pvTagData);
      finally
        case pvBufReleaseType of
          dtDispose: Dispose(buf);
          dtFreeMem: FreeMem(buf);
          dtMemPool: FOwner.PushMem(buf);
        end;
      end;
    end;
  end;
end;

function TIocpCustomContext.PostWSASendStream(Stream: TStream;
  ASize: Int64): Boolean;
var
  lvFreeType: TDataReleaseType;
  lvRequest: TIocpSendRequest;
  lvBuffSize, lvL: Cardinal;
  lvLen: Int64;
  lvBuf: PAnsiChar;
begin
  Result := False;
  if Assigned(Stream) then begin
    lvLen := Stream.Size - Stream.Position;
    if lvLen < ASize then
      Exit
    else
      lvLen := ASize;

    try
      AtomicIncrement(FSendStreaming);   // ��������������1,��ֹ�Զ�����
      lvBuffSize := Owner.SendBufferSize;
      while lvLen > 0 do begin
        if lvLen > lvBuffSize then
          lvL := lvBuffSize
        else
          lvL := lvLen;
        {$IFDEF UseSendMemPool}
        lvBuf := FOwner.PopMem;
        lvFreeType := dtMemPool;
        {$ELSE}
        GetMem(lvBuf, lvL);
        lvFreeType := dtFreeMem;
        {$ENDIF}

        lvL := Stream.Read(lvBuf^, lvL);
        if (lvL = 0) then
          Break;

        Result := False;
        lvRequest := GetSendRequest;
        lvRequest.SetBuffer(lvBuf, lvL, lvFreeType);
        lvRequest.Tag := 0;
        lvRequest.Data := nil;
        repeat
          if IncReferenceCounter(Self, 'SendStream') then begin
            try
              Result := InnerPostSendRequestAndCheckStart(lvRequest);
              if not Result then begin
                {$IFDEF MSWINDOWS}
                SwitchToThread;
                {$ELSE}
                TThread.Yield;
                {$ENDIF}
                Sleep(50);
              end;
            finally
              decReferenceCounter(self, 'SendStream');
            end;
          end else
            Break;
        until (Result);

        if not Result then begin
          lvRequest.UnBindingSendBuffer;
          lvRequest.CancelRequest;
          FOwner.ReleaseSendRequest(lvRequest);

          {$IFDEF UseSendMemPool}
          FOwner.PushMem(lvBuf);
          {$ELSE}
          FreeMem(lvBuf);
          {$ENDIF}
          Break;
        end;

        Dec(lvLen, lvL);
      end;
    finally
      AtomicDecrement(FSendStreaming);  // ��������������1
    end;
  end;
end;

procedure TIocpCustomContext.ReleaseClientContext;
begin
end;

procedure TIocpCustomContext.RequestDisconnect(pvObj: TObject; const pvDebugInfo: string);
begin
  if AtomicCmpExchange(FRequestDisconnect, 1, 0) = 0 then
    FRawSocket.Close;
end;

function TIocpCustomContext.Send(buf: Pointer; len: Cardinal;
  CopyBuf: Boolean): Boolean;
begin
  Result := PostWSASendRequest(buf, len, CopyBuf);
end;

function TIocpCustomContext.Send(buf: Pointer; len: Cardinal;
  BufReleaseType: TDataReleaseType): Boolean;
begin
  Result := PostWSASendRequest(buf, len, BufReleaseType);
end;

function TIocpCustomContext.Send(const Data: AnsiString): Boolean;
begin
  Result := PostWSASendRequest(PAnsiChar(Data), Length(Data), True);
end;

function TIocpCustomContext.Send(const Data: WideString): Boolean;
begin
  Result := PostWSASendRequest(Pointer(Data), Length(Data) shl 1, True);
end;

{$IFDEF UNICODE}
function TIocpCustomContext.Send(const Data: UnicodeString): Boolean;
begin
  Result := PostWSASendRequest(Pointer(Data), Length(Data) shl 1, True);
end;
{$ENDIF}

function TIocpCustomContext.SendStream(Stream: TStream; ASize: Int64): Boolean;
var
  P: PAnsiChar;
  lvLen: Int64;
begin
  Result := False;
  if Assigned(Stream) then begin
    if (Stream is TMemoryStream) then begin
      lvLen := Stream.Size - Stream.Position;
      if lvLen < ASize then
        Exit
      else
        lvLen := ASize;
      P := PAnsiChar(TMemoryStream(Stream).Memory) + Stream.Position;
      Result := PostWSASendRequest(P, lvLen);
      Stream.Position := Stream.Position + lvLen;
    end else
      Result := PostWSASendStream(Stream, ASize);
  end;
end;

function TIocpCustomContext.SendStream(Stream: TStream): Boolean;
begin
  if Assigned(Stream) then begin
    Result := SendStream(Stream, Stream.Size - Stream.Position);
  end else
    Result := False;
end;

procedure TIocpCustomContext.SetMaxSendingQueueSize(pvSize: Integer);
begin
  FSendRequestList.MaxSize := pvSize;
end;

procedure TIocpCustomContext.SetOwner(const Value: TIocpCustom);
begin
  FOwner := Value;
  FRecvRequest.FOwner := FOwner;
end;

procedure TIocpCustomContext.SetSocketState(pvState: TSocketState);
begin
  FSocketState := pvState;
  if Assigned(FOnSocketStateChanged) then
    FOnSocketStateChanged(Self);
end;

procedure TIocpCustomContext.UnLock;
begin
  FContextLocker.Leave;
end;

procedure TIocpCustomContext.UnLockContext(pvObj: TObject; const pvDebugInfo: string);
begin
  if Assigned(Self) then
    DecReferenceCounter(pvObj, pvDebugInfo);
end;

{ TIocpBase }

constructor TIocpBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBindAddr := '0.0.0.0';
  FLocker := TIocpLocker.Create();
  //FLocker.Name := Self.ClassName;
  FIocpEngine := TIocpEngine.Create();
  // post wsaRecv block size
  FWSARecvBufferSize := 1024 shl 2;
  FWSASendBufferSize := 1024 shl 3;
  FMemPool := TIocpMemPool.Create(FWSASendBufferSize, 4096);
end;

procedure TIocpBase.CreateDataMonitor;
begin
  if not Assigned(FDataMoniter) then
    FDataMoniter := TIocpDataMonitor.Create;
end;

destructor TIocpBase.Destroy;
begin
  FIsDestroying := True;
  Close;
  FreeAndNil(FDataMoniter);
  FreeAndNil(FIocpEngine);
  inherited Destroy;
  FreeAndNil(FMemPool);
  FreeAndNil(FLocker);
end;

procedure TIocpBase.DoStateMsg(Sender: TObject;
  MsgType: TIocpStateMsgType; const Msg: string);
begin
  if Assigned(FOnStateMsg) then
    FOnStateMsg(Sender, MsgType, Msg);
end;

procedure TIocpBase.DoStateMsgD(Sender: TObject;
  const MsgFormat: string; const Params: array of const);
begin
  if Assigned(FOnStateMsg) then
    FOnStateMsg(Sender, iocp_mt_Debug, Format(MsgFormat, Params));
end;

procedure TIocpBase.DoStateMsgE(Sender: TObject; E: Exception);
begin
  if Assigned(FOnStateMsg) then
    FOnStateMsg(Sender, iocp_mt_Error, E.Message);
end;

procedure TIocpBase.DoStateMsgE(Sender: TObject;
  const MsgFormat: string; E: Exception);
begin
  if Assigned(FOnStateMsg) then
    FOnStateMsg(Sender, iocp_mt_Error, Format(MsgFormat, [E.Message]));
end;

procedure TIocpBase.DoStateMsgE(Sender: TObject;
  const MsgFormat: string; const Params: array of const);
begin
  if Assigned(FOnStateMsg) then
    FOnStateMsg(Sender, iocp_mt_Error, Format(MsgFormat, Params));
end;

procedure TIocpBase.DoStateMsgD(Sender: TObject; const Msg: string);
begin
  if Assigned(FOnStateMsg) then
    FOnStateMsg(Sender, iocp_mt_Debug, Msg);
end;

procedure TIocpBase.DoStateMsgE(Sender: TObject; const Msg: string);
begin
  if Assigned(FOnStateMsg) then
    FOnStateMsg(Sender, iocp_mt_Error, Msg);
end;

function TIocpBase.GetMaxWorkerCount: Integer;
begin
  if Assigned(FIocpEngine) then
    Result := FIocpEngine.MaxWorkerCount
  else
    Result := 0;
end;

function TIocpBase.GetWorkerCount: Integer;
begin
  if Assigned(FIocpEngine) then
    Result := FIocpEngine.WorkerCount
  else
    Result := 0;
end;

function TIocpBase.IsDestroying: Boolean;
begin
  Result := (not Assigned(Self)) or FIsDestroying or (csDestroying in Self.ComponentState);
end;

procedure TIocpBase.Loaded;
begin
  inherited;
  if FActive and (not (csDesigning in ComponentState)) then begin
    FActive := False;
    Active := True;
  end;
end;

function TIocpBase.PopMem: Pointer;
begin
  Result := FMemPool.Pop;
end;

procedure TIocpBase.PushMem(const V: Pointer);
begin
  FMemPool.Push(V);
end;

procedure TIocpBase.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;
  if Value then
    Open
  else
    Close;
end;

procedure TIocpBase.SetBindAddr(const Value: AnsiString);
begin
  if FBindAddr <> Value then
    FBindAddr := Value;
end;

procedure TIocpBase.SetMaxWorkerCount(const Value: Integer);
begin
  FIocpEngine.SetMaxWorkerCount(Value);
end;

procedure TIocpBase.SetName(const NewName: TComponentName);
begin
  inherited;
end;

procedure TIocpBase.SetWorkerCount(const Value: Integer);
begin
  FIocpEngine.SetWorkerCount(Value);
end;

procedure TIocpBase.SetWSARecvBufferSize(const Value: Cardinal);
begin
  FWSARecvBufferSize := Value;
  if FWSARecvBufferSize = 0 then
    FWSARecvBufferSize := 1024 shl 2;
end;

procedure TIocpBase.SetWSASendBufferSize(const Value: Cardinal);
begin
  if FWSASendBufferSize <> Value then begin
    FWSASendBufferSize := Value;
    if FWSASendBufferSize = 0 then
      FWSASendBufferSize := 1024 shl 3;
    FreeAndNil(FMemPool);
    FMemPool := TIocpMemPool.Create(FWSASendBufferSize, 4096);
  end;
end;

{ TIocpCustom }

procedure TIocpCustom.AddToOnlineList(const pvObject: TIocpCustomContext);
begin
  FOnlineContextList.Add(pvObject.FHandle, Integer(pvObject));
end;

function TIocpCustom.GetClientContext: TIocpCustomContext;
begin
  Result := CreateContext;
end;

function TIocpCustom.GetOnlineContextCount: Integer;
begin
  Result := FOnlineContextList.Count;
end;

procedure TIocpCustom.GetOnlineContextList(pvList: TList);
begin
  FOnlineContextList.GetItems(pvList);
end;

function TIocpCustom.GetRecvRequest: TIocpRecvRequest;
begin
  if Assigned(FRecvRequestClass) then
    Result := FRecvRequestClass.Create
  else
    Result := TIocpRecvRequest.Create;
end;

function TIocpCustom.GetSendRequest: TIocpSendRequest;
begin
  Result := TIocpSendRequest(FSendRequestPool.DeQueue);
  if Result = nil then begin
    if FSendRequestClass <> nil then
      Result := FSendRequestClass.Create
    else
      Result := TIocpSendRequest.Create;
    Result.DoCleanup;
  end;
  Result.FAlive := True;
  Result.FOwner := Self;
end;

function TIocpCustom.CheckClientContextValid(const ClientContext: TIocpCustomContext): Boolean;
begin
  Result := (ClientContext.FOwner = Self);
end;

procedure TIocpCustom.Close;
begin
  if not FActive then Exit;
  FActive := False;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  DisconnectAll;
  WaitForContext(30000);
  DoClose;
  // engine Stop
  FIocpEngine.Stop;
end;

constructor TIocpCustom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnlineContextList := TIntHash.Create(199999);
  // send requestPool
  FSendRequestPool := TBaseQueue.Create;
end;

function TIocpCustom.CreateContext: TIocpCustomContext;
begin
  if FContextClass <> nil then
    Result := FContextClass.Create(Self)
  else
    Result := TIocpCustomContext.Create(Self);
  OnCreateContext(Result);
end;

destructor TIocpCustom.Destroy;
begin
  inherited Destroy;
  FSendRequestPool.FreeDataObject;
  FreeAndNil(FOnlineContextList);
  FreeAndNil(FSendRequestPool);
end;

procedure TIocpCustom.DisconnectAll;
var
  P: PIntHashItem;
  I: Integer;
  lvClientContext: TIocpCustomContext;
begin
  FOnlineContextList.Lock;
  try
    for I := 0 to FOnlineContextList.BucketsCount - 1 do begin
      P := FOnlineContextList.Buckets[I];
      while P <> nil do begin
        lvClientContext := Pointer(P.Value);
        if lvClientContext <> nil then
          lvClientContext.RequestDisconnect(Self);
        P := P.Next;
      end;
    end;
  finally
    FOnlineContextList.UnLock;
  end;
end;

procedure TIocpCustom.DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);
begin
  pvRequest.FAcceptorMgr.ReleaseRequestObject(pvRequest);
end;

procedure TIocpCustom.DoClientContextError(
  const pvClientContext: TIocpCustomContext; pvErrorCode: Integer);
begin
  if Assigned(FOnContextError) then
    FOnContextError(pvClientContext, pvErrorCode);
end;

procedure TIocpCustom.DoClose;
begin
end;

procedure TIocpCustom.DoOpen;
begin
end;

procedure TIocpCustom.DoReceiveData(const pvContext: TIocpCustomContext;
  pvRequest: TIocpRecvRequest);
begin
  pvContext.FLastActivity := GetTimestamp;
  if Assigned(FOnReceivedBuffer) then
    FOnReceivedBuffer(pvContext, pvRequest.FRecvBuffer.buf,
      pvRequest.FBytesTransferred, pvRequest.ErrorCode)
end;

procedure TIocpCustom.OnCreateContext(const Context: TIocpCustomContext);
begin
end;

procedure TIocpCustom.Open;
begin
  if FActive then Exit;
  FActive := True;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  try
    DoOpen;
    if Assigned(FDataMoniter) then
      FDataMoniter.Clear;
    FIocpEngine.Start;
  except
    FActive := False;
    raise;
  end;
end;

function TIocpCustom.ReleaseClientContext(const pvObject: TIocpCustomContext): Boolean;
begin
  if Assigned(pvObject) then
    pvObject.Free;
  Result := False;
end;

function TIocpCustom.ReleaseSendRequest(pvObject: TIocpSendRequest): Boolean;
begin
  Result := False;
  if (not Assigned(Self)) or (not Assigned(FSendRequestPool)) then
    Assert(False);
  if IsDebugMode then
    Assert(pvObject.FAlive);
  if lock_cmp_exchange(True, False, pvObject.FAlive) = True then begin
    if Assigned(FDataMoniter) then
      InterlockedIncrement(FDataMoniter.FSendRequestReturnCounter);
    pvObject.DoCleanUp;
    pvObject.FOwner := nil;
    FSendRequestPool.EnQueue(pvObject);
    Result := True;
  end else begin
    if IsDebugMode then
      Assert(False);
  end;
end;

procedure TIocpCustom.RemoveFromOnlineList(const pvObject: TIocpCustomContext);
{$IFDEF DEBUG_ON}
var
  lvSucc:Boolean;
{$ENDIF}
begin
  {$IFDEF DEBUG_ON}
  lvSucc := FOnlineContextList.Remove(pvObject.FHandle);
  Assert(lvSucc);
  {$ELSE}
  FOnlineContextList.Remove(pvObject.FHandle);
  {$ENDIF}
end;

function TIocpCustom.RequestContextHandle: Integer;
begin
  Result := InterlockedIncrement(FContextCounter);
end;

function TIocpCustom.WaitForContext(pvTimeOut: Cardinal): Boolean;
var
  l, t: Cardinal;
  c: Integer;
begin
  l := GetTickCount;
  c := FOnlineContextList.Count;
  while (c > 0) do begin
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}
    t := GetTickCount - l;
    if t > pvTimeOut then begin
      {$IFDEF DEBUG_ON}
      DoStateMsgD(Self, 'WaitForContext End Current num: %d', [c]);
      {$ENDIF}
      Break;
    end else if (t > 10000) and (c < 100) then begin
      {$IFDEF DEBUG_ON}
      DoStateMsgD(Self, 'WaitForContext End Current num: %d', [c]);
      {$ENDIF}
      Break;
    end;
    c := FOnlineContextList.Count;
  end;
  Result := FOnlineContextList.Count = 0;
end;

{ TIocpRecvRequest }

procedure TIocpRecvRequest.Clear;
begin
  if FInnerBuffer.len > 0 then begin
    FreeMem(FInnerBuffer.buf, FInnerBuffer.len);
    FInnerBuffer.len := 0;
  end;
end;

constructor TIocpRecvRequest.Create;
begin
  inherited Create;
end;

destructor TIocpRecvRequest.Destroy;
begin
  if FOwner = nil then
    Assert(FOwner = nil, 'error');
  FOwner := nil;
  Clear;
  if FContext <> nil then   
    FContext.FRecvRequest := nil;
  inherited Destroy;
end;

procedure TIocpRecvRequest.HandleResponse;
var
  lvFlag: Integer;
begin
  {$IFDEF DEBUG_ON}
  InterlockedDecrement(FOverlapped.refCount);
  if FOverlapped.refCount <> 0 then
    Assert(FOverlapped.refCount <> 0);
  if FOwner = nil then
    Assert(FOwner <> nil);
  if FContext = nil then
    Exit;
  {$ENDIF}
  lvFlag := 0;
  try     
    if (Assigned(FOwner.FDataMoniter)) then begin
      FOwner.FDataMoniter.incResponseWSARecvCounter;
      FOwner.FDataMoniter.incRecvdSize(FBytesTransferred);
    end;

    if not FOwner.Active then begin
      {$IFDEF DEBUG_ON}
      FOwner.DoStateMsgD(Self, strRecv_EngineOff, [FContext.SocketHandle]);
      {$ENDIF}
      // avoid postWSARecv
      FContext.RequestDisconnect(Self{$IFDEF DEBUGINFO}, Format(strRecv_EngineOff, [FContext.SocketHandle]){$ENDIF});
    end else if ErrorCode <> 0 then begin
      FContext.DoError(ErrorCode);
      if ErrorCode <> ERROR_OPERATION_ABORTED then begin  // �첽����ȴ�ʱ�����˹ر��׽���
        FOwner.DoStateMsgE(Self, strRecv_Error, [FContext.SocketHandle, ErrorCode]);
      end;
      if ErrorCode <> WSAESHUTDOWN then begin
        FContext.RequestDisconnect(Self{$IFDEF DEBUGINFO}, Format(strRecv_Error, [FContext.SocketHandle, ErrorCode]){$ENDIF});
      end else
        FOwner.DoStateMsgE(Self, strRecv_Error, [FContext.SocketHandle, ErrorCode]);
    end else if (FBytesTransferred = 0) then begin
      // no data recvd, socket is break
      {$IFDEF DEBUG_ON}
      //FOwner.DoStateMsgE(Self, strRecv_Zero, [FContext.SocketHandle]);
      {$ENDIF}
      FContext.RequestDisconnect(Self{$IFDEF DEBUGINFO}, Format(strRecv_Zero, [FContext.SocketHandle]){$ENDIF});
    end else begin
      lvFlag := 1;
      FContext.DoReceiveData;
    end;
  finally
    if Assigned(FContext) then begin
      if (lvFlag = 1) then
        FContext.PostWSARecvRequest;
//      FContext.DecReferenceCounter(Self{$IFDEF DEBUGINFO},
//        Format('Refcount: %d, TIocpRecvRequest.WSARecvRequest.HandleResponse',
//          [FOverlapped.refCount]){$ENDIF});
    end;
  end;
end;

function TIocpRecvRequest.PostRequest: Boolean;
begin
  if FInnerBuffer.len <> FOwner.FWSARecvBufferSize then begin
    if FInnerBuffer.len > 0 then
      FreeMem(FInnerBuffer.buf);
    GetMem(FInnerBuffer.buf, FOwner.FWSARecvBufferSize);
    FInnerBuffer.len := FOwner.FWSARecvBufferSize;
  end;
  Result := PostRequest(FInnerBuffer.buf, FInnerBuffer.len);
end;

function TIocpRecvRequest.PostRequest(pvBuffer: PAnsiChar;
  len: Cardinal): Boolean;
var
  lvRet: Integer;
  lpNumberOfBytesRecvd: Cardinal;
begin
  Result := False;
  if not Assigned(Self) then Exit;
  if not Assigned(FContext) then Exit;
  lpNumberOfBytesRecvd := 0;
  FRecvdFlag := 0;
  FRecvBuffer.buf := pvBuffer;
  FRecvBuffer.len := len;

  {$IFDEF DEBUG_ON}
  InterlockedIncrement(FOverlapped.refCount);
  {$ENDIF}
  lvRet := iocp.winapi.winsock.WSARecv(FContext.FRawSocket.SocketHandle,
       @FRecvBuffer, 1, lpNumberOfBytesRecvd, FRecvdFlag,
       LPWSAOVERLAPPED(@FOverlapped),   // d7 need to cast
       nil
     );
  if lvRet = SOCKET_ERROR then begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then begin
      {$IFDEF DEBUG_ON}
      FOwner.DoStateMsgE(Self, strRecv_PostError, [FContext.SocketHandle, lvRet]);
      InterlockedDecrement(FOverlapped.refCount);
      {$ENDIF}
      // trigger error event
      FOwner.DoClientContextError(FContext, lvRet);
    end else begin
      if (FOwner <> nil) and (Assigned(FOwner.FDataMoniter)) then
        FOwner.FDataMoniter.incPostWSARecvCounter;
    end;
  end else begin
    Result := True;
    if (FOwner <> nil) and (Assigned(FOwner.FDataMoniter)) then
      FOwner.FDataMoniter.incPostWSARecvCounter;
  end;
end;

procedure TIocpRecvRequest.ResponseDone;
begin
  inherited;
  if FOwner = nil then begin
    if IsDebugMode then
      Assert(FOwner <> nil);
  end else
    FContext.DecReferenceCounter(nil);
end;

{ TIocpSendRequest }

procedure TIocpSendRequest.CheckClearSendBuffer;
begin
  if FLen > 0 then begin
    case FSendBufferReleaseType of
      dtDispose: Dispose(FBuf);
      dtFreeMem: FreeMem(FBuf);
      dtMemPool: FOwner.PushMem(FBuf);
    end;
  end;
  FSendBufferReleaseType := dtNone;
  FLen := 0;
end;

constructor TIocpSendRequest.Create;
begin
  inherited Create;
end;

destructor TIocpSendRequest.Destroy;
begin
  CheckClearSendBuffer;
  inherited;
end;

procedure TIocpSendRequest.DoCleanUp;
begin
  CheckClearSendBuffer;
  FBytesSize := 0;
  //FNext := nil;
  FOwner := nil;
  FContext := nil;
  FBuf := nil;
  FLen := 0;
end;

function TIocpSendRequest.ExecuteSend: Boolean;
begin
  if (FBuf = nil) or (FLen = 0) then begin
    {$IFDEF DEBUG_ON}
    FOwner.DoStateMsgD(Self, strSend_Zero, [FContext.SocketHandle]);
    {$ENDIF}
    Result := False;
  end else
    Result := InnerPostRequest(FBuf, FLen);
end;

function TIocpSendRequest.GetStateInfo: string;
begin
  Result := Format('%s %s', [Self.ClassName, Remark]);
  if Responding then
    Result := Result + sLineBreak + Format('start: %s, datalen: %d',
      [TimeToStr(FRespondStartTime), FSendBuf.len])
  else
    Result := Result + sLineBreak + Format('start: %s, end: %s, datalen: %d',
      [TimeToStr(FRespondStartTime), TimeToStr(FRespondEndTime), FSendBuf.len]);
end;

procedure TIocpSendRequest.HandleResponse;
var
  lvContext: TIocpCustomContext;
begin
  lvContext := FContext;
  if lvContext = nil then Exit;
  FIsBusying := False;
  if FOwner = nil then Exit;   
  try
    if Assigned(FOwner.FDataMoniter) then begin
      FOwner.FDataMoniter.incSentSize(FBytesTransferred);
      FOwner.FDataMoniter.incResponseWSASendCounter;
    end;

    lvContext.DoSendRequestRespnonse(Self);

    if not FOwner.Active then begin
      {$IFDEF DEBUG_ON}
      FOwner.DoStateMsgD(Self, strSend_EngineOff, [lvContext.SocketHandle]);
      {$ENDIF}
      // avoid postWSARecv
      lvContext.RequestDisconnect(Self,
        Format(strSend_EngineOff, [lvContext.SocketHandle]));
    end else if ErrorCode <> 0 then begin
      FOwner.DoClientContextError(lvContext, ErrorCode);
      FOwner.DoStateMsgD(Self, strSend_Err, [lvContext.SocketHandle, ErrorCode]);
      lvContext.RequestDisconnect(Self,
         Format(strSend_Err, [lvContext.SocketHandle, ErrorCode]));
    end else begin
      // �ɹ�
      lvContext.FLastActive := GetTimestamp;
      if Assigned(FOwner.FDataMoniter) then
        FOwner.FDataMoniter.incResponseSendObjectCounter;
      if Assigned(FOnDataRequestCompleted) then
        FOnDataRequestCompleted(lvContext, Self);
      lvContext.DoSendRequestCompleted(Self);
      lvContext.PostNextSendRequest;
    end;
  finally
    lvContext.DecReferenceCounter(Self, 'TIocpSendRequest.HandleResponse');
  end;
end;

function TIocpSendRequest.InnerPostRequest(buf: Pointer;
  len: Cardinal): Boolean;
var
  lvErrorCode, lvRet: Integer;
  dwFlag, lpNumberOfBytesSent: Cardinal;
  lvContext: TIocpCustomContext;
  lvOwner: TIocpCustom;
begin
  Result := False;
  FIsBusying := True;
  FBytesSize := len;
  FSendBuf.buf := buf;
  FSendBuf.len := len;
  dwFlag := 0;
  lvErrorCode := 0;
  lpNumberOfBytesSent := 0;

  // maybe on HandleResonse and release self
  lvOwner := FOwner;
  lvContext := FContext;
  if lvContext.IncReferenceCounter(Self, 'InnerPostRequest::WSASend_Start') then
  try
    lvRet := WSASend(lvContext.FRawSocket.SocketHandle,
                  @FSendBuf, 1, lpNumberOfBytesSent, dwFlag,
                  LPWSAOVERLAPPED(@FOverlapped),   // d7 need to cast
                  nil);
    if lvRet = SOCKET_ERROR then begin
      // Ͷ��ʧ��
      lvErrorCode := WSAGetLastError;
      Result := lvErrorCode = WSA_IO_PENDING;
      if not Result then begin
        //���ʹ����ͷŸ�SOCKET��Ӧ��������Դ
        FIsBusying := False;
        lvOwner.DoStateMsgE(Self, strSend_PostError, [lvContext.SocketHandle, lvErrorCode]);
        lvContext.RequestDisconnect(Self);
      end else begin
        // ���ͳɹ�����TCP/IP�㻺������������TCP/IP�㻺�����п���ĵط�������
        // �������ǵĳ��򻺳�������ʱ�Ż���ɿ�����Ȼ�󽫸�IOCPһ�������Ϣ
        if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then begin
          lvOwner.FDataMoniter.incPostWSASendSize(len);
          lvOwner.FDataMoniter.incPostWSASendCounter;
        end;
      end;
    end else begin
      // ���ͳɹ����Ѿ������ݷ���TCP/IP�㻺����
      Result := True;
      if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then begin
        lvOwner.FDataMoniter.incPostWSASendSize(len);
        lvOwner.FDataMoniter.incPostWSASendCounter;
      end;
    end;
  finally
    if not Result then begin
      // Ͷ��ʧ��ֱ�ӽ���������1�����������Ϊ0ʱ����û���κ����󣬽��ر�socket
      if IsDebugMode then
        Assert(lvContext = FContext);
      lvContext.DecReferenceCounter(Self,
        Format('InnerPostRequest::WSASend_Fail, ErrorCode:%d', [lvErrorCode]));
    end;
    // �������True�����ܻ��� HandleResponse �� dispose �򷵻ص����С�
  end;
end;

procedure TIocpSendRequest.ResponseDone;
begin
  if FOwner = nil then begin
    if IsDebugMode then begin
      Assert(FOwner <> nil);
      Assert(Self.FAlive);
    end;
  end else
    FOwner.ReleaseSendRequest(Self);
end;

procedure TIocpSendRequest.SetBuffer(const buf: Pointer; const len: Cardinal;
  pvCopyBuf: Boolean);
var
  lvBuf: PAnsiChar;
begin
  if pvCopyBuf then begin
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    SetBuffer(lvBuf, len, dtFreeMem);
  end else
    SetBuffer(buf, len, dtNone);
end;

procedure TIocpSendRequest.SetBuffer(const buf: Pointer; const len: Cardinal;
  pvBufReleaseType: TDataReleaseType);
begin
  CheckClearSendBuffer;
  FBuf := buf;
  FLen := len;
  FSendBufferReleaseType := pvBufReleaseType;
end;

procedure TIocpSendRequest.UnBindingSendBuffer;
begin
  FBuf := nil;
  FLen := 0;
  FSendBufferReleaseType := dtNone;
end;

{ TIocpDataMonitor }

procedure TIocpDataMonitor.Clear;
var
  L: TCriticalSection;
begin
  L := FLocker;
  FLocker.Enter;
  try
    FillChar(Pointer(IntPtr(Pointer(@Self)^)+4)^, Self.InstanceSize, 0);
    FLocker := L;
  finally
    FLocker.Leave;
  end;
end;

constructor TIocpDataMonitor.Create;
begin
  FLocker := TCriticalSection.Create;
end;

destructor TIocpDataMonitor.Destroy;
begin
  FreeAndNil(FLocker);
  inherited;
end;

procedure TIocpDataMonitor.IncAcceptExObjectCounter;
begin
  InterlockedIncrement(FAcceptExObjectCounter);
end;

procedure TIocpDataMonitor.incHandleCreateCounter;
begin
  InterlockedIncrement(FHandleCreateCounter);
end;

procedure TIocpDataMonitor.incHandleDestroyCounter;
begin
  InterlockedIncrement(FHandleDestroyCounter);
end;

procedure TIocpDataMonitor.incHttpRequestCreateCounter;
begin
  InterlockedIncrement(FHttpRequestCreateCounter);
end;

procedure TIocpDataMonitor.incHttpRequestExecCounter;
begin
  InterlockedIncrement(FHttpRequestExecCounter);
end;

procedure TIocpDataMonitor.incHttpRequestPopCounter;
begin
  InterlockedIncrement(FHttpRequestPopCounter);
end;

procedure TIocpDataMonitor.incHttpRequestPushCounter;
begin
  InterlockedIncrement(FHttpRequestPushCounter);
end;

procedure TIocpDataMonitor.incPostSendObjectCounter;
begin
  InterlockedIncrement(FPostSendObjectCounter);
end;

procedure TIocpDataMonitor.incPostWSARecvCounter;
begin
  InterlockedIncrement(FPostWSARecvCounter);
end;

procedure TIocpDataMonitor.incPostWSASendCounter;
begin
  InterlockedIncrement(FPostWSASendCounter);
end;

procedure TIocpDataMonitor.incPostWSASendSize(pvSize: Cardinal);
begin
  FLocker.Enter;
  FPostWSASendSize := FPostWSASendSize + pvSize;
  FLocker.Leave;
end;

procedure TIocpDataMonitor.incPushSendQueueCounter;
begin
  InterlockedIncrement(FPushSendQueueCounter);
end;

procedure TIocpDataMonitor.incRecvdSize(pvSize: Cardinal);
begin
  FLocker.Enter;
  FRecvSize := FRecvSize + pvSize;
  FLocker.Leave;
end;

procedure TIocpDataMonitor.incResponseSendObjectCounter;
begin
  InterlockedIncrement(FResponseSendObjectCounter);
end;

procedure TIocpDataMonitor.incResponseWSARecvCounter;
begin
  InterlockedIncrement(FResponseWSARecvCounter);
end;

procedure TIocpDataMonitor.incResponseWSASendCounter;
begin
  InterlockedIncrement(FResponseWSASendCounter);
end;

procedure TIocpDataMonitor.incSentSize(pvSize: Cardinal);
begin
  FLocker.Enter;
  FSentSize := FSentSize + pvSize;
  FLocker.Leave;
end;

{ TIocpConnectExRequest }

constructor TIocpConnectExRequest.Create(const AContext: TIocpCustomContext);
begin
  inherited Create;
  FContext := AContext;
end;

destructor TIocpConnectExRequest.Destroy;
begin
  inherited Destroy;
end;

function TIocpConnectExRequest.PostRequest(const Host: AnsiString; Port: Word): Boolean;
var
  lvSockAddrIn: TSockAddrIn;
  lvRet: BOOL;
  lvErrCode: Integer;
  lp: Pointer;
  lvRemoteIP: AnsiString;
begin
  {$IFDEF DEBUG_ON}
  Remark := Format(strConn_Request, [Host, Port]);
  {$ENDIF}
  try
    lvRemoteIP := FContext.Socket.DomainNameToAddr(Host);
  except
    lvRemoteIP := AnsiString(Host);
  end;
  FContext.SetSocketState(ssConnecting);
  lvSockAddrIn := GetSocketAddr(lvRemoteIP, Port);
  FContext.Socket.bind(TIocpRemoteContext(FContext).BindAddr, 0); // '0.0.0.0'

  lp := @FOverlapped;
  lvRet := IocpConnectEx(FContext.Socket.SocketHandle, @lvSockAddrIn,
    sizeOf(lvSockAddrIn), nil, 0, FBytesSent, lp);
  if not lvRet then begin
    lvErrCode := WSAGetLastError;
    Result := lvErrCode = WSA_IO_PENDING;
    if not Result then begin
      FContext.DoError(lvErrCode);
      FContext.RequestDisconnect(Self, 'TIocpConnectExRequest.PostRequest');
    end;
  end else
    Result := True;
end;

{ TIocpAcceptExRequest }

constructor TIocpAcceptExRequest.Create(AOwner: TIocpCustom);
begin
  FOwner := AOwner;
  inherited Create();
  FOnAcceptedEx := nil;
end;

procedure TIocpAcceptExRequest.getPeerInfo;
const
  ADDRSIZE = SizeOf(TSockAddr) + ADDRESS_LENGTH_EX;
var
  localAddr: PSockAddr;
  remoteAddr: PSockAddr;
  localAddrSize: Integer;
begin
  localAddrSize := ADDRSIZE;
  IocpGetAcceptExSockaddrs(@FAcceptBuffer[0], 0,
    SizeOf(localAddr^) + ADDRESS_LENGTH_EX, SizeOf(remoteAddr^) + ADDRESS_LENGTH_EX,
    localAddr, localAddrSize, remoteAddr, localAddrSize);
  TIocpClientContext(FContext).FRemoteAddr := inet_ntoa(TSockAddrIn(remoteAddr^).sin_addr);
  TIocpClientContext(FContext).FRemotePort := ntohs(TSockAddrIn(remoteAddr^).sin_port);
end;

procedure TIocpAcceptExRequest.HandleResponse;
begin
  if (FOwner<> nil) and (FOwner.FDataMoniter <> nil) then
    InterlockedIncrement(FOwner.FDataMoniter.FResponseWSAAcceptExCounter);
  try
    if ErrorCode = 0 then begin
      // msdn
      // The socket sAcceptSocket does not inherit the properties of the socket
      //  associated with sListenSocket parameter until SO_UPDATE_ACCEPT_CONTEXT
      //  is set on the socket.
      FAcceptorMgr.FListenSocket.UpdateAcceptContext(FContext.SocketHandle);
      getPeerInfo();
    end;
    if Assigned(FOnAcceptedEx) and (TMethod(FOnAcceptedEx).Data <> nil) then
      FOnAcceptedEx(Self);
  finally
    FOwner.DoAcceptExResponse(Self);
  end;
end;

function TIocpAcceptExRequest.PostRequest: Boolean;
var
  dwBytes: Cardinal;
  lvRet: BOOL;
  lvErrCode: Integer;
begin
  FContext.FRawSocket.IPVersion := FAcceptorMgr.FListenSocket.IPVersion;
  FContext.CreateSocket(True);

  dwBytes := 0;
  if FContext.FRawSocket.IPVersion = IP_V6 then
    lvRet := IocpAcceptEx(FAcceptorMgr.FListenSocket.SocketHandle
                , FContext.FRawSocket.SocketHandle
                , @FAcceptBuffer[0]
                , 0
                , SizeOf(TSockAddrIn6) + ADDRESS_LENGTH_EX
                , SizeOf(TSockAddrIn6) + ADDRESS_LENGTH_EX
                , dwBytes
                , @FOverlapped)
  else
    lvRet := IocpAcceptEx(FAcceptorMgr.FListenSocket.SocketHandle
                , FContext.FRawSocket.SocketHandle
                , @FAcceptBuffer[0]
                , 0
                , SizeOf(TSockAddrIn) + ADDRESS_LENGTH_EX
                , SizeOf(TSockAddrIn) + ADDRESS_LENGTH_EX
                , dwBytes
                , @FOverlapped);

  if not lvRet then begin
    lvErrCode := WSAGetLastError;
    Result := lvErrCode = WSA_IO_PENDING;
    if not Result then begin
      {$IFDEF DEBUG_ON}
      FOwner.DoStateMsgE(Self, strBindingIocpError,
        [FContext.FRawSocket.SocketHandle, lvErrCode, 'PostRequest']);
      {$ENDIF}
      FOwner.DoClientContextError(FContext, lvErrCode);
      // destroy socket
      FContext.FRawSocket.Close;
    end;
  end else
    Result := True;
end;

procedure TIocpAcceptExRequest.ResponseDone;
begin
  FAcceptorMgr.ReleaseRequestObject(Self)
end;

procedure TIocpAcceptExRequest.SetOnAcceptedEx(const Value: TNotifyEvent);
begin
  FOnAcceptedEx := Value;
end;

{ TIocpAcceptorMgr }

procedure TIocpAcceptorMgr.CheckAcceptExRequest;
var
  I, M: Integer;
begin
  if FCount < FMinRequest then begin
    Assert(FOwner <> nil);
    if not FOwner.Active then
      Exit;
    I := 0;
    FLocker.Enter();
    try
      M := FMaxRequest - FCount + FMaxRequest shr 1;
      while FCount < FMaxRequest do begin
        PostAcceptExRequest();
        Inc(I);
        if I > M then
          Break;
      end;
    finally
      FLocker.Leave;
    end;
  end;
end;

constructor TIocpAcceptorMgr.Create(AOwner: TIocpCustom; AListenSocket: TRawSocket);
begin
  inherited Create;
  FOwner := AOwner;
  FListenSocket := AListenSocket;
  FLocker := TIocpLocker.Create();
  //FLocker.Name := 'AcceptorLocker';
  FMaxRequest := 256;
  FMinRequest := 16;
  FCount := 0;
  FAcceptExRequestPool := TBaseQueue.Create;
end;

destructor TIocpAcceptorMgr.Destroy;
begin
  FMaxRequest := 0;
  FAcceptExRequestPool.FreeDataObject;
  FreeAndNil(FAcceptExRequestPool);
  FLocker.Free;
  inherited Destroy;
end;

function TIocpAcceptorMgr.GetRequestObject: TIocpAcceptExRequest;
begin
  Result := TIocpAcceptExRequest(FAcceptExRequestPool.DeQueue);
  if Result = nil then begin
    Result := TIocpAcceptExRequest.Create(FOwner);
    if (FOwner.FDataMoniter <> nil) then
      FOwner.Moniter.IncAcceptExObjectCounter;
  end;
  InterlockedIncrement(FCount);
end;

procedure TIocpAcceptorMgr.PostAcceptExRequest;
var
  lvRequest: TIocpAcceptExRequest;
  j: Integer;
begin
  Assert(FOwner <> nil);
  j := 0;
  lvRequest := nil;
  try
    lvRequest := GetRequestObject;

    j := 1;
    lvRequest.FContext := FOwner.GetClientContext;
    lvRequest.FAcceptorMgr := self;

    j := 2;
    if lvRequest.PostRequest then begin
      j := 3;
      if (FOwner.FDataMoniter <> nil) then
        InterlockedIncrement(FOwner.FDataMoniter.FPostWSAAcceptExCounter);
      j := 4;
    end else begin
      // post fail
      j := 100;
      try
        // �����쳣��ֱ���ͷ�Context
        lvRequest.FContext.Socket.Close;
        lvRequest.FContext.FAlive := False;
        lvRequest.FContext.Free;
        lvRequest.FContext := nil;
      except
        j := 101;
        OutputDebugString(PChar(Exception(ExceptObject).Message));
      end;

      // �黹�������
      j := 110;
      ReleaseRequestObject(lvRequest);

      // ��ʾ��־
      j := 111;
      FOwner.DoStateMsgE(Self, 'PostAcceptExRequest Failed.');
    end;
  except
    on E:Exception do begin
      FOwner.DoStateMsgE(Self, 'PostAcceptExRequest. ErrMsg: (%d)%s', [J, e.Message]);
      if (J > 0) and (J < 110) and Assigned(lvRequest) then
        ReleaseRequestObject(lvRequest);
    end;
  end;
end;

procedure TIocpAcceptorMgr.Release(Request: TIocpAcceptExRequest);
begin
  Request.Free;
end;

procedure TIocpAcceptorMgr.ReleaseRequestObject(
  pvRequest: TIocpAcceptExRequest);
begin
  pvRequest.FAcceptorMgr := nil;
  pvRequest.FContext := nil;
  pvRequest.FOnAcceptedEx := nil;
  FAcceptExRequestPool.EnQueue(pvRequest);
  // �˴��ٴμ���Ƿ���ҪͶ�ݽ������󣬼�����ϵͳ������ɵ����Ӳ���Ӧ�������
  if InterlockedDecrement(FCount) < FMinRequest - 1 then
    CheckAcceptExRequest;
end;

function TIocpAcceptorMgr.WaitForCancel(pvTimeOut: Cardinal): Boolean;
var
  l: Cardinal;
begin
  l := GetTickCount;
  while (FCount > 0) do begin
    {$IFDEF MSWINDOWS}
    SwitchToThread;
    {$ELSE}
    TThread.Yield;
    {$ENDIF}
    if GetTickCount - l > pvTimeOut then begin
      {$IFDEF WRITE_LOG}
      FOwner.DoStateMsgD(Self, 'WaitForCancel End Current AccepEx num:%d', [FCount]);
      {$ENDIF}
      Break;
    end;
  end;
  Result := FCount = 0;
end;

{ TIocpCustomBlockTcpSocket }

const
  RECVBUFSize = 1024 * 16;

function Min(const AValueOne, AValueTwo: Int64): Int64;
begin
  If AValueOne > AValueTwo then
    Result := AValueTwo
  else
    Result := AValueOne;
end;

function BytesToString(const ABytes: TBytes; const AStartIndex: Integer;
  AMaxCount: Integer): string;
begin
  if (AStartIndex > High(ABytes)) or (AStartIndex < 0) then
    raise Exception.Create(Format('Index (%d) out of bounds. (%d)', [AStartIndex, Length(ABytes) - 1]));
  AMaxCount := Min(Length(ABytes) - AStartIndex, AMaxCount);
  {$IFDEF DotNet}
  Result := System.Text.Encoding.ASCII.GetString(ABytes, AStartIndex, AMaxCount);
  {$ELSE}
  SetLength(Result, AMaxCount);
  if AMaxCount > 0 then
    Move(ABytes[AStartIndex], Result[1], AMaxCount);
  {$ENDIF}
end;

procedure TIocpCustomBlockTcpSocket.CheckSocketResult(pvSocketResult: Integer);
begin
  {$IFDEF POSIX}
  if (pvSocketResult = -1) or (pvSocketResult = 0) then
    RaiseLastOSError;
  {$ELSE}
  if (pvSocketResult = -1) then
    RaiseLastOSError;
  {$ENDIF}
end;

function TIocpCustomBlockTcpSocket.Connect(ATimeOut: Integer; RaiseError: Boolean): Boolean;
var
  lvIpAddr: AnsiString;
begin
  Result := FActive;
  if Result then Exit;

  if Length(FHost) = 0 then Exit;

  CreateSocket;

  lvIpAddr := FRawSocket.DomainNameToAddr(FHost);
  FActive := FRawSocket.Connect(lvIpAddr, FPort, ATimeOut);
  Result := FActive;
  if not Result then begin
    if ATimeOut > 0 then begin
      if RaiseError then       
        raise Exception.CreateFmt(strConn_TimeOut, [FHost, FPort])
    end else
      RaiseLastOSError(RaiseError);
  end
end;

function TIocpCustomBlockTcpSocket.Connect(RaiseError: Boolean): Boolean;
begin
  Result := Connect(FConnectTimeOut, RaiseError);
end;

procedure TIocpCustomBlockTcpSocket.Close;
begin
  Disconnect;
end;

function TIocpCustomBlockTcpSocket.Connect(const RemoteHost: AnsiString;
  RemotePort: Word; ATimeOut: Integer): Boolean;
var
  lvIpAddr: AnsiString;
begin
  Result := FActive;
  if Result then begin
    if (FHost = RemoteHost) and (FPort = RemotePort) then
      Exit;
    Active := False;
    Result := False;
  end;

  if Length(RemoteHost) = 0 then Exit;

  CreateSocket;

  lvIpAddr := FRawSocket.DomainNameToAddr(RemoteHost);
  FActive := FRawSocket.Connect(lvIpAddr, RemotePort, ATimeOut);
  Result := FActive;
  if not Result then begin
    if ATimeOut > 0 then
      raise Exception.CreateFmt(strConn_TimeOut, [RemoteHost, RemotePort])
    else
      RaiseLastOSError;
  end else begin
    FHost := RemoteHost;
    FPort := RemotePort;
  end;
end;

constructor TIocpCustomBlockTcpSocket.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRawSocket := TRawSocket.Create;
  FReadTimeOut := 30000;
  FConnectTimeOut := -1;
  FRecvBufferSize := -1;
end;

procedure TIocpCustomBlockTcpSocket.CreateSocket;
begin
  FRecvBufferSize := -1;
  FRawSocket.CreateTcpSocket;
  FRawSocket.SetReadTimeOut(FReadTimeOut);
end;

destructor TIocpCustomBlockTcpSocket.Destroy;
begin
  FreeAndNil(FRawSocket);
  FreeAndNil(FStream);
  FreeAndNil(FBuffer);
  inherited;
end;

procedure TIocpCustomBlockTcpSocket.Disconnect;
begin
  if not FActive then Exit;
  FRecvBufferSize := -1;
  FRawSocket.Close;
  FActive := False;
end;

function TIocpCustomBlockTcpSocket.DomainNameToAddr(const host: AnsiString): AnsiString;
begin
  Result := FRawSocket.DomainNameToAddr(host);
end;

function TIocpCustomBlockTcpSocket.GetErrorCode: Integer;
begin
  Result := FErrorCode;
  FErrorCode := 0;
end;

function TIocpCustomBlockTcpSocket.GetRecvBufferIsEmpty: Boolean;
begin
  if FRecvBufferSize < 0 then begin
    FRecvBufferSize := GetRecvBufferSize();
    if FRecvBufferSize = 0 then
      FRecvBufferSize := -1;
  end;
  Result := FRecvBufferSize < 1;
end;

function TIocpCustomBlockTcpSocket.GetRecvBufferSize: Cardinal;
begin
  if Assigned(FRawSocket) and (FRawSocket.SocketHandle <> INVALID_SOCKET) then
    ioctlsocket(FRawSocket.SocketHandle, FIONREAD, Result)
  else
    Result := 0;
end;

function TIocpCustomBlockTcpSocket.GetRecvStream: TStream;
begin
  if not Assigned(FStream) then
    FStream := TIocpBlockSocketStream.Create(Self)
  else if FStream.FSocket <> Self then
    FStream.FSocket := Self;
  Result := FStream;
  TIocpBlockSocketStream(Result).FReadTimeOut := ReadTimeOut;
end;

function TIocpCustomBlockTcpSocket.IsConnected: Boolean;
begin
  Result := FActive and Assigned(FRawSocket) and (FRawSocket.Connected);
  if not Result then
    FActive := False;
end;

class function TIocpCustomBlockTcpSocket.IsIP(v: PAnsiChar): Longint;
begin
  Result := inet_addr(v);
end;

procedure TIocpCustomBlockTcpSocket.Open;
begin
  SetActive(True);
end;

procedure TIocpCustomBlockTcpSocket.RaiseLastOSError(RaiseErr: Boolean);
begin
  FRecvBufferSize := -1;
  FErrorCode := GetLastError;
  if (FErrorCode = WSAENOTSOCK)         // �����ֲ���һ���׽ӿڡ�
    or (FErrorCode = WSAECONNABORTED)   // ���ڳ�ʱ������ԭ�����·ʧЧ��
    or (FErrorCode = WSAECONNRESET)     // Զ��ǿ����ֹ�����·��
    or (FErrorCode = WSAESHUTDOWN)      // �׽ӿ��ѱ��ر�
    or (FErrorCode = WSAEINVAL)         // �׽ӿ�δ��bind()��������
    or (FErrorCode = WSAENOTCONN)       // �׽ӿ�δ���ӡ�
    or (FErrorCode = WSAETIMEDOUT)      // ���ӳ�ʱ 2016.08.01
  then begin
    Disconnect; // ���ӶϿ�
    if RaiseErr then
      SysUtils.RaiseLastOSError(FErrorCode);
  end else
    SysUtils.RaiseLastOSError(FErrorCode);
end;

function TIocpCustomBlockTcpSocket.ReadBytes(var Buffer: TBytes;
  AByteCount: Integer; AAppend: Boolean): Integer;
var
  J: Integer;
  P: Pointer;
begin
  if not Assigned(FBuffer) then
    FBuffer := TMemoryStream.Create;
  P := Pointer(NativeUInt(FBuffer.Memory) + FBuffer.Position); // Use NativeUint
  Result := ReadStream(FBuffer, AByteCount);
  if Result > 0 then begin
    if p = nil then P := FBuffer.Memory; // jinghuai  2016-8-30
    if AAppend then begin
      J := Length(Buffer);
      SetLength(Buffer, J + Result);
      Move(P^, Buffer[J], Result);
    end else begin
      SetLength(Buffer, Result);
      Move(P^, Buffer[0], Result);
    end;
    FBuffer.Clear;
  end;
end;

function TIocpCustomBlockTcpSocket.ReadChar: Char;
begin
  Recv(@Result, SizeOf(Result));
end;

function TIocpCustomBlockTcpSocket.ReadDouble: Double;
begin
  Result := 0;
  Recv(@Result, SizeOf(Result));
end;

function TIocpCustomBlockTcpSocket.ReadInt64: Int64;
begin
  Result := 0;
  Recv(@Result, SizeOf(Result));
end;

function TIocpCustomBlockTcpSocket.ReadInteger: Integer;
begin
  Result := 0;
  Recv(@Result, SizeOf(Result));
end;

function TIocpCustomBlockTcpSocket.ReadSmallInt: SmallInt;
begin
  Result := 0;
  Recv(@Result, SizeOf(Result));
end;

function TIocpCustomBlockTcpSocket.ReadStream(OutStream: TStream;
  Len: Integer; WaitRecvLen: Boolean): Integer;
const
  BUFSize = 4096;
var
  lvTempL: Integer;
  Buf: array [0..BUFSize - 1] of Byte;
begin
  Result := 0;
  FRecvBufferSize := -1;
  if Len > 0 then begin
    while True do begin
      if Len > BUFSize then
        lvTempL := FRawSocket.RecvBuf(Buf, BUFSize)
      else
        lvTempL := FRawSocket.RecvBuf(Buf, Len);
      if lvTempL > 0 then begin
        OutStream.WriteBuffer(Buf, lvTempL);
        Dec(Len, lvTempL);
        Inc(Result, lvTempL);
        if (Len = 0) or ((lvTempL < BUFSize) and (not WaitRecvLen)) then Break;
      end else begin
        if (lvTempL < 0){$IFDEF POSIX} or (lvTempL = 0){$ENDIF} then
          RaiseLastOSError(False);
        Break;
      end;
    end;
  end else begin
    while True do begin
      lvTempL := FRawSocket.RecvBuf(Buf, BUFSize);
      if lvTempL > 0 then begin
        OutStream.WriteBuffer(Buf, lvTempL);
        Inc(Result, lvTempL);
        if lvTempL < BUFSize then Break;        
      end else begin
        if (lvTempL < 0){$IFDEF POSIX} or (lvTempL = 0){$ENDIF} then
          RaiseLastOSError(False);
        Break;
      end;
    end;
  end;
end;

function TIocpCustomBlockTcpSocket.ReadString(const ABytes: Integer): AnsiString;
const
  MaxRecvSize = 1024 * 1024 * 8;
var
  I: Integer;
  Last: Int64;
begin
  if not Assigned(FBuffer) then begin
    FBuffer := TMemoryStream.Create;
    Last := 0;
  end else
    Last := FBuffer.Position;
  if ABytes < 1 then
    I := ReadStream(FBuffer, MaxRecvSize)
  else
    I := ReadStream(FBuffer, ABytes);
  if I > 0 then begin
    SetString(Result, PAnsiChar(NativeUInt(FBuffer.Memory) + Last), I);
    FBuffer.Clear;
  end else
    Result := '';
end;

function TIocpCustomBlockTcpSocket.ReadWord: Word;
begin
  Result := 0;
  Recv(@Result, SizeOf(Result));
end;

function TIocpCustomBlockTcpSocket.Recv(buf: Pointer; len: Cardinal): Cardinal;
var
  lvTempL :Integer;
  lvPBuf: Pointer;
begin
  Result := 0;
  FRecvBufferSize := -1;
  lvPBuf := buf;
  while Result < len do begin
    if len - Result > RECVBUFSize then
      lvTempL := RECVBUFSize
    else
      lvTempL := len - Result;
    lvTempL := FRawSocket.RecvBuf(lvPBuf^, lvTempL);
    CheckSocketResult(lvTempL);
    lvPBuf := Pointer(Cardinal(lvPBuf) + Cardinal(lvTempL));
    Result := Result + Cardinal(lvTempL);
    if lvTempL < RECVBUFSize then
      Break;
  end;
end;

function TIocpCustomBlockTcpSocket.Recv(Len: Integer): AnsiString;
begin
  Result := ReadString(Len);
end;

function TIocpCustomBlockTcpSocket.RecvBuffer(buf: Pointer;
  len: cardinal): Integer;
begin
  FRecvBufferSize := -1;
  Result := FRawSocket.RecvBuf(buf^, len);
  CheckSocketResult(Result);
end;

function TIocpCustomBlockTcpSocket.Send(buf: Pointer; len: Cardinal): Cardinal;
begin
  Result := SendBuffer(buf, len);
end;

function TIocpCustomBlockTcpSocket.Send(const Data: WideString): Cardinal;
begin
  if Length(Data) = 0 then
    Result := 0
  else
    Result := SendBuffer(@Data[1], Length(Data) shl 1);
end;

function TIocpCustomBlockTcpSocket.Send(const Data: AnsiString): Cardinal;
begin
  if Length(Data) = 0 then
    Result := 0
  else
    Result := SendBuffer(@Data[1], Length(Data));
end;

procedure TIocpCustomBlockTcpSocket.Send(Stream: TStream; SendSize: Integer);
var
  buf: array [0..4095] of Byte;
  BytesRead, SendRef: Integer;
begin
  if (SendSize < 0) or (SendSize > stream.Size - stream.Position) then
    SendSize := stream.Size - stream.Position;
  if SendSize = 0 then Exit;
  SendRef := 0;
  while SendRef < SendSize do begin
    if SendSize - SendRef < SizeOf(buf) then
      BytesRead := SendSize - SendRef
    else
      BytesRead := SizeOf(buf);
    BytesRead := stream.Read(buf, bytesRead);
    SendBuffer(@buf[0], BytesRead);
    Inc(SendRef, BytesRead);
  end;
end;

{$IFDEF UNICODE}
function TIocpCustomBlockTcpSocket.Send(const Data: UnicodeString): Cardinal;
begin
  if Length(Data) = 0 then
    Result := 0
  else
    Result := SendBuffer(@Data[1], Length(Data) shl 1);
end;
{$ENDIF}

function TIocpCustomBlockTcpSocket.SendBuffer(buf: Pointer;
  len: cardinal): Integer;
begin
  Result := FRawSocket.SendBuf(buf^, len);
  CheckSocketResult(Result);
end;

procedure TIocpCustomBlockTcpSocket.SetActive(const Value: Boolean);
begin
  if (FActive = Value) then Exit;
  if Value then
    Connect
  else
    Disconnect;
end;

function TIocpCustomBlockTcpSocket.SetKeepAlive(
  pvKeepAliveTime: Integer): Boolean;
begin
  Result := Assigned(FRawSocket) and (FRawSocket.SetKeepAliveOption(pvKeepAliveTime));
end;

procedure TIocpCustomBlockTcpSocket.SetReadTimeOut(const Value: Integer);
begin
  if FReadTimeOut <> Value then begin
    FReadTimeOut := Value;
    if Assigned(FRawSocket) then
      FRawSocket.SetReadTimeOut(Value);
  end;
end;

function TIocpCustomBlockTcpSocket.Seek(Len: Integer): Boolean;
const
  BUFSize = 4096;
var
  lvTempL: Integer;
  Buf: array [0..BUFSize - 1] of Byte;
begin
  if Active then begin
    lvTempL := BUFSize;
    if Len > 0 then begin
      while lvTempL = BUFSize do begin
        if Len > BUFSize then
          lvTempL := FRawSocket.RecvBuf(Buf, BUFSize)
        else
          lvTempL := FRawSocket.RecvBuf(Buf, Len);
        CheckSocketResult(lvTempL);
        Dec(Len, lvTempL);
      end;
    end else begin
      while lvTempL = BUFSize do begin
        lvTempL := FRawSocket.RecvBuf(Buf, BUFSize);
        CheckSocketResult(lvTempL);
      end;
    end;
    Result := True;
  end else
    Result := False;
end;

{ TIocpCustomBlockUdpSocket }

procedure TIocpCustomBlockUdpSocket.CreateSocket;
begin
  FRawSocket.CreateUdpSocket;
  FRawSocket.SetReadTimeOut(FReadTimeOut);
end;

function TIocpCustomBlockUdpSocket.Send(buf: Pointer; len: Cardinal;
  const Addr: AnsiString; Port: Word): Cardinal;
begin
  if Connect(Addr, Port) then begin
    Result := SendBuffer(buf, len);
  end else
    Result := 0;
end;

function TIocpCustomBlockUdpSocket.Send(const Data: WideString;
  const Addr: AnsiString; Port: Word): Cardinal;
begin
  if Connect(Addr, Port) then
    Result := Send(Data)
  else
    Result := 0;
end;

function TIocpCustomBlockUdpSocket.Send(const Data: AnsiString;
  const Addr: AnsiString; Port: Word): Cardinal;
begin
  if Connect(Addr, Port) then
    Result := Send(Data)
  else
    Result := 0;
end;

{$IFDEF UNICODE}
function TIocpCustomBlockUdpSocket.Send(const Data: UnicodeString;
  const Addr: AnsiString; Port: Word): Cardinal;
begin
  if Connect(Addr, Port) then
    Result := Send(Data)
  else
    Result := 0;
end;
{$ENDIF}

{ TIocpCustomTcpServer }

procedure TIocpCustomTcpServer.Close;

  procedure DoCloseTimeOut();
  begin
    DoStateMsg(Self, iocp_mt_Warning, Self.Name +
        'CloseTimeOut. '#13#10'EngineWorkerInfo: ' + sLineBreak +
        FIocpEngine.GetStateInfo + sLineBreak +
        '================================================'#1310 +
        'TcpServerInfo:'#1310 + GetStateInfo);
  end;

begin
  if not FActive then Exit;
  FActive := False;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  DoStateMsgD(Self, 'Server Closeing...');
  if Assigned(FListenSocket) then
    FListenSocket.Close;
  FTimeOutKickOut.Enabled := False;

  try
    DisconnectAll;
    // �ȴ����е�Ͷ�ݵ�AcceptEx����ع�
    FIocpAcceptorMgr.WaitForCancel(12000);
    if not WaitForContext(120000) then begin
      Sleep(10);
      if not FIocpEngine.StopWorkers(10000) then begin
        // record info
        DoCloseTimeOut();
      end else begin
        if not FIocpEngine.StopWorkers(120000) then begin
          // record info
          DoCloseTimeOut();
        end;
      end;
    end;
  except
  end;
  
  FIocpAcceptorMgr.FAcceptExRequestPool.FreeDataObject;
  FIocpAcceptorMgr.FAcceptExRequestPool.Clear;

  FContextPool.FreeDataObject;
  FContextPool.Clear;

  FSendRequestPool.FreeDataObject;
  FSendRequestPool.Clear;

  DoClose;
  // engine Stop
  FIocpEngine.Stop;
  DoStateMsgD(Self, 'Server Closed.');
end;

constructor TIocpCustomTcpServer.Create(AOwner: TComponent);
begin
  DoStateMsgD(Self, 'Server Create...');
  inherited Create(AOwner);
  // Ĭ�ϲ���������ѡ��
  FKeepAlive := False;
  FPort := 9000;
  FMaxSendingQueueSize := 128;
  FMaxContextPoolSize := 8192;
  FKickOutInterval := 30000;
  FListenSocket := TRawSocket.Create;
  FIocpAcceptorMgr := TIocpAcceptorMgr.Create(Self, FListenSocket);
  FIocpAcceptorMgr.FMaxRequest := 256;
  FIocpAcceptorMgr.FMinRequest := 8;
  FContextPool := TBaseQueue.Create;

  FTimeOutKickOut := TTimer.Create(nil);
  FTimeOutKickOut.Interval := FKickOutInterval;
  FTimeOutKickOut.Enabled := False;
  FTimeOutKickOut.OnTimer := DoKickOutTimer;
  
  DoStateMsgD(Self, 'Server Created.');
end;

function TIocpCustomTcpServer.CreateContext: TIocpCustomContext;
begin
  if FContextClass <> nil then
    Result := FContextClass.Create(Self)
  else
    Result := TIocpClientContext.Create(Self);
  OnCreateContext(Result);
end;

procedure TIocpCustomTcpServer.CreateSocket;
begin
  FListenSocket.CreateTcpSocket(True);
  // �������˿�
  if not FListenSocket.Bind(FBindAddr, FPort) then
    RaiseLastOSError;
  // ��������
  if not FListenSocket.listen() then
    RaiseLastOSError;
  // �������׽��ְ󶨵�IOCP���
  FIocpEngine.IocpCore.Bind(FListenSocket.SocketHandle, 0);
  // post AcceptEx request
  FIocpAcceptorMgr.CheckAcceptExRequest;
end;

destructor TIocpCustomTcpServer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTimeOutKickOut);
  FreeAndNil(FListenSocket);
  FreeAndNil(FIocpAcceptorMgr);
  FreeAndNil(FContextPool);
end;

procedure TIocpCustomTcpServer.DoAcceptExResponse(pvRequest: TIocpAcceptExRequest);
var
  lvRet:Integer;
  lvErrCode:Integer;

  function DoAfterAcceptEx():Boolean;
  begin
    Result := true;
    if Assigned(FOnContextAccept) then begin
      FOnContextAccept(pvRequest.FContext.SocketHandle,
         TIocpClientContext(pvRequest.FContext).RemoteAddr,
         TIocpClientContext(pvRequest.FContext).RemotePort, Result);

      {$IFDEF WRITE_LOG}
      if not Result then
        DoStateMsgD(Self, 'OnAcceptEvent AllowAccept is False.');
      {$ENDIF}
    end;
    if Result then begin
      if FKeepAlive then begin
        Result := SetKeepAlive(pvRequest.FContext.SocketHandle, 10000);
        if not Result then begin
          lvErrCode := GetLastError;
          {$IFDEF WRITE_LOG}
          DoStateMsgE(Self, 'AcceptEx Response AfterAccept. Socket.SetKeepAlive Error: %d', [lvErrCode]);
          {$ENDIF}
        end;
      end;
    end;

  end;
begin
  if pvRequest.ErrorCode = 0 then begin
    if DoAfterAcceptEx then begin
      lvRet := FIocpEngine.IocpCore.Bind(pvRequest.FContext.SocketHandle, 0);
      if lvRet = 0 then begin
        // binding error
        lvErrCode := GetLastError;
        {$IFDEF WRITE_LOG}
        DoStateMsgE(Self, 'Bind IOCPHandle(%d) in Context DoAcceptExResponse occur Error: %d',
            [pvRequest.FContext.SocketHandle, lvErrCode]);
        {$ENDIF}

        DoClientContextError(pvRequest.FContext, lvErrCode);
        pvRequest.FContext.FRawSocket.Close;

        // relase client context object
        ReleaseClientContext(pvRequest.FContext);
        pvRequest.FContext := nil;
      end else if pvRequest.FContext <> nil then               
        pvRequest.FContext.DoConnected;

    end else begin
      pvRequest.FContext.FRawSocket.Close;

      // return to pool
      ReleaseClientContext(pvRequest.FContext);
      pvRequest.FContext := nil;
    end;

  end else begin
    pvRequest.FContext.FRawSocket.Close;
    // �黹�����������ĳ�
    ReleaseClientContext(pvRequest.FContext);
    pvRequest.FContext := nil;
  end;

  // ��������ţ�������Ͷ��һ����������
  if FActive then
    FIocpAcceptorMgr.CheckAcceptExRequest;
end;

procedure TIocpCustomTcpServer.DoKickOutTimer(Sender: TObject);
begin
  if IsDestroying or (FKickOutInterval = High(Integer)) then Exit;
  try
    if FKickOutInterval < 1000 then
      KickOut(1000)
    else
      KickOut(FKickOutInterval);
  except
    DoStateMsgE(Self, Exception(ExceptObject));
  end;  
end;

function TIocpCustomTcpServer.FindContext(ContextHandle: Cardinal): TIocpClientContext;
var
  V: Number;
begin
  V := FOnlineContextList.ValueOf(ContextHandle, 0);
  if V <> 0 then
    Result := TIocpClientContext(Pointer(V))
  else
    Result := nil;
end;

function TIocpCustomTcpServer.GetClientContext: TIocpCustomContext;
begin
  Result := TIocpClientContext(FContextPool.DeQueue);
  if Result = nil then begin
    Result := CreateContext;
    if (FDataMoniter <> nil) then
      InterlockedIncrement(FDataMoniter.FContextCreateCounter);
    Result.FSendRequestList.MaxSize := FMaxSendingQueueSize;
    Result.DoCleanUp;
  end;
  if (FDataMoniter <> nil) then
    InterlockedIncrement(FDataMoniter.FContextOutCounter);
  Result.FAlive := True;
  Result.FOwner := Self;
end;

function TIocpCustomTcpServer.GetClientCount: Integer;
begin
  Result := FOnlineContextList.Count;
end;

function TIocpCustomTcpServer.GetMaxTaskWorker: Integer;
begin
  if Assigned(Workers) then
    Result := Workers.MaxWorkers
  else
    Result := 0;
end;

function TIocpCustomTcpServer.GetStateInfo: string;
var
  lvStrings:TStrings;
begin
  Result := '';
  if not Assigned(FDataMoniter) then exit;
  lvStrings := TStringList.Create;
  try
    if Active then
      lvStrings.Add(strState_Active)
    else
      lvStrings.Add(strState_Off);

    lvStrings.Add(Format(strRecv_PostInfo,
         [
           Moniter.PostWSARecvCounter,
           Moniter.ResponseWSARecvCounter,
           Moniter.PostWSARecvCounter -
           Moniter.ResponseWSARecvCounter
         ]
        ));


    lvStrings.Add(Format(strRecv_SizeInfo, [TransByteSize(Moniter.RecvSize)]));

    //  Format('post:%d, response:%d, recvd:%d',
    //     [
    //       FIocpTcpServer.DataMoniter.PostWSARecvCounter,
    //       FIocpTcpServer.DataMoniter.ResponseWSARecvCounter,
    //       FIocpTcpServer.DataMoniter.RecvSize
    //     ]
    //    );

    lvStrings.Add(Format(strSend_Info,
       [
         Moniter.PostWSASendCounter,
         Moniter.ResponseWSASendCounter,
         Moniter.PostWSASendCounter -
         Moniter.ResponseWSASendCounter
       ]
      ));

    lvStrings.Add(Format(strSendRequest_Info,
       [
         Moniter.SendRequestCreateCounter,
         Moniter.SendRequestOutCounter,
         Moniter.SendRequestReturnCounter
       ]
      ));

    lvStrings.Add(Format(strSendQueue_Info,
       [
         Moniter.PushSendQueueCounter,
         Moniter.PostSendObjectCounter,
         Moniter.ResponseSendObjectCounter,
         Moniter.SendRequestAbortCounter
       ]
      ));

    lvStrings.Add(Format(strSend_SizeInfo, [TransByteSize(Moniter.SentSize)]));

    lvStrings.Add(Format(strAcceptEx_Info,
       [
         Moniter.PostWSAAcceptExCounter,
         Moniter.ResponseWSAAcceptExCounter
       ]
      ));

    lvStrings.Add(Format(strSocketHandle_Info,
       [
         Moniter.HandleCreateCounter,
         Moniter.HandleDestroyCounter
       ]
      ));

    lvStrings.Add(Format(strContext_Info,
       [
         Moniter.ContextCreateCounter,
         Moniter.ContextOutCounter,
         Moniter.ContextReturnCounter
       ]
      ));

    lvStrings.Add(Format(strOnline_Info, [ClientCount]));
    lvStrings.Add(Format(strWorkers_Info, [WorkerCount]));
    lvStrings.Add(Format(strRunTime_Info, [GetRunTimeInfo]));

    Result := lvStrings.Text;
  finally
    lvStrings.Free;
  end;
end;

procedure TIocpCustomTcpServer.KickOut(pvTimeOut: Cardinal);
var
  lvNowTickCount: Int64;
  lvContext: TIocpCustomContext;
  P: PIntHashItem;
  I: Integer;
  List: TList;
begin
  List := TList.Create;
  lvNowTickCount := GetTimestamp;
  FOnlineContextList.Lock;
  try
    for I := 0 to FOnlineContextList.BucketsCount - 1 do begin
      P := FOnlineContextList.Buckets[I];
      while P <> nil do begin
        if Pointer(P.Value) <> nil then begin
          lvContext := Pointer(P.Value);
          if Assigned(lvContext) and
            (not lvContext.IsDisconnecting) and
            (lvContext.FAlive) and
            (lvContext.FLastActivity <> 0) then
          begin
            if (lvNowTickCount - lvContext.FLastActivity > pvTimeOut) and (AtomicIncrement(lvContext.FSendStreaming, 0) <= 0) then
              List.Add(lvContext);
          end;
        end;
        P := P.Next;
      end;
    end;
    for I := 0 to List.Count - 1 do begin
      lvContext := List.Items[I];
      if Assigned(lvContext) and (not lvContext.IsDisconnecting) then
        lvContext.CloseConnection;
    end;
  finally
    FOnlineContextList.UnLock;
    List.Free;
  end;
end;

procedure TIocpCustomTcpServer.Open;
begin
  if FActive then Exit;
  FActive := True;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  try
    if Assigned(FDataMoniter) then
      FDataMoniter.Clear;
    DoOpen;
    // ����IOCP����
    FIocpEngine.Start;
    // �����������׽���
    CreateSocket;
    // ���ó�ʱ������
    FTimeOutKickOut.Interval := FKickOutInterval;
    FTimeOutKickOut.Enabled := True;
  except
    FActive := False;
    FListenSocket.Close;
    raise;
  end;
end;

procedure TIocpCustomTcpServer.RegisterContextClass(
  pvContextClass: TIocpContextClass);
begin
  FContextClass := pvContextClass;
end;

procedure TIocpCustomTcpServer.RegisterSendRequestClass(
  pvClass: TIocpSendRequestClass);
begin
  FSendRequestClass := pvClass;
end;

function TIocpCustomTcpServer.ReleaseClientContext(
  const pvObject: TIocpCustomContext): Boolean;
begin
  if lock_cmp_exchange(True, False, pvObject.FAlive) = True then begin
    // ��������ش�Сʱ����ֱ���ͷ�
    if (FMaxContextPoolSize > 0) and (FContextPool.Size >= FMaxContextPoolSize) then begin
      pvObject.DoCleanUp;
      pvObject.Free;
    end else begin
      pvObject.DoCleanUp;
      FContextPool.EnQueue(pvObject);
    end;
    if (FDataMoniter <> nil) then
      InterlockedIncrement(FDataMoniter.FContextReturnCounter);
    Result := True;
  end else
    Result := False
end;

procedure TIocpCustomTcpServer.SetMaxSendingQueueSize(pvSize: Integer);
begin
  if pvSize <= 0 then
    FMaxSendingQueueSize := 10
  else
    FMaxSendingQueueSize := pvSize;
end;

procedure TIocpCustomTcpServer.SetMaxTaskWorker(const Value: Integer);
begin
  if Assigned(Workers) then
    Workers.MaxWorkers := Value;
end;

procedure TIocpCustomTcpServer.Start;
begin
  Open;
end;

procedure TIocpCustomTcpServer.Stop;
begin
  Close;
end;

{ TIocpClientContext }

constructor TIocpClientContext.Create(AOwner: TIocpCustom);
begin
  inherited Create(AOwner);
end;

destructor TIocpClientContext.Destroy;
begin
  inherited Destroy;
end;

procedure TIocpClientContext.Disconnect;
begin
  PostWSACloseRequest;
end;

function TIocpClientContext.GetBindIP: string;
var
  name: sockaddr;
  l: Integer;
begin
  if FSocketHandle <> INVALID_SOCKET then begin
    l := SizeOf(name);
    if getsockname(FSocketHandle, name, l) = S_OK then
      Result := string(inet_ntoa(TSockAddrIn(name).sin_addr))
    else
      Result := '';
  end else
    Result := '';
end;

function TIocpClientContext.GetBindPort: Word;
begin
  if Assigned(FOwner) then
    Result := TIocpCustomTcpServer(FOwner).ListenPort
  else
    Result := 0;
end;

function TIocpClientContext.GetPeerAddr: Cardinal;
begin
  if (Length(FRemoteAddr) > 0) then
    Result := ipToInt(AnsiString(FRemoteAddr))
  else
    Result := 0;
end;

function TIocpClientContext.GetRemoteAddr: string;
begin
  Result := string(FRemoteAddr);
end;

procedure TIocpClientContext.ReleaseClientContext;
begin
  FOwner.ReleaseClientContext(Self);
end;

{ TIocpRemoteContext }

function TIocpRemoteContext.CanAutoReConnect: Boolean;
begin
  Result := FAutoReConnect and (Owner.Active) and
    (not TIocpCustomTcpClient(Owner).DisableAutoConnect);
end;

procedure TIocpRemoteContext.Connect(ASync: Boolean; pvTimeOut: Integer);
var
  lvRemoteIP: AnsiString;
begin
  if Length(FHost) = 0 then
    Exit;
  if SocketState <> ssDisconnected then
    raise Exception.Create(strCannotConnect);

  ReCreateSocket;
  if ASync then
    PostConnectRequest
  else begin
    try
      lvRemoteIP := Socket.DomainNameToAddr(FHost);
    except
      lvRemoteIP := FHost;
    end;
    if not Socket.Connect(lvRemoteIP, FPort, pvTimeOut) then
      RaiseLastOSError;
    DoConnected;
  end;
end;

procedure TIocpRemoteContext.Connect(const AHost: AnsiString; APort: Word;
  ASync: Boolean; pvTimeOut: Integer);
begin
  FHost := AHost;
  FPort := APort;
  Connect(ASync, pvTimeOut);
end;

constructor TIocpRemoteContext.Create(AOwner: TIocpCustom);
begin
  inherited Create(AOwner);
  FBindAddr := '';
  FAutoReConnect := False;
  FIsConnecting := False;
  FConnectExRequest := TIocpConnectExRequest.Create(Self);
  FConnectExRequest.OnResponse := OnConnecteExResponse;
end;

destructor TIocpRemoteContext.Destroy;
begin
  FreeAndNil(FConnectExRequest);
  inherited Destroy;
end;

function TIocpRemoteContext.GetBindAddr: AnsiString;
begin
  if FBindAddr = '' then
    Result := '0.0.0.0'
  else
    Result := FBindAddr;
end;

procedure TIocpRemoteContext.OnConnected;
begin
  inherited OnConnected;
  FLastDisconnectTime := 0;  // ���öϿ�ʱ��
end;

procedure TIocpRemoteContext.OnConnecteExResponse(pvObject: TObject);
begin
  FIsConnecting := False;
  if TIocpConnectExRequest(pvObject).ErrorCode = 0 then
    DoConnected
  else begin
    {$IFDEF DEBUG_ON}
    if Assigned(Owner) then
      Owner.DoStateMsgE(Self, strConnectError, [TIocpConnectExRequest(pvObject).ErrorCode]);
    {$ENDIF}

    DoError(TIocpConnectExRequest(pvObject).ErrorCode);

    if (CanAutoReConnect) then begin
      Sleep(100);
      PostConnectRequest;
    end else
      SetSocketState(ssDisconnected);
  end;
end;

procedure TIocpRemoteContext.OnDisconnected;
begin
  inherited OnDisconnected;
end;

procedure TIocpRemoteContext.PostConnectRequest;
begin
  if lock_cmp_exchange(False, True, FIsConnecting) = False then begin
    if Socket = nil then begin
      FIsConnecting := False;
      Exit;
    end;

    if (Socket.SocketHandle = INVALID_SOCKET) then
      ReCreateSocket;

    if not FConnectExRequest.PostRequest(FHost, FPort) then begin
      FIsConnecting := False;
      Sleep(RECONNECT_INTERVAL);
      if CanAutoReConnect then
        PostConnectRequest;
    end;
  end;
end;

procedure TIocpRemoteContext.ReCreateSocket;
begin
  Socket.CreateTcpSocket(True);
  FSocketHandle := Socket.SocketHandle;
  if not Socket.bind(BindAddr, 0) then  // '0.0.0.0'
    RaiseLastOSError;
  Owner.Engine.IocpCore.Bind(FSocketHandle, 0);
end;

procedure TIocpRemoteContext.ReleaseClientContext;
begin
  if not FAutoReConnect then
    FOwner.ReleaseClientContext(Self)
  else
    inherited;
end;

procedure TIocpRemoteContext.SetBindAddr(const Value: AnsiString);
begin
  if Value = '0.0.0.0' then
    FBindAddr := ''
  else
    FBindAddr := Value;
end;

procedure TIocpRemoteContext.SetSocketState(pvState: TSocketState);
begin
  inherited SetSocketState(pvState);
  if pvState = ssDisconnected then begin
    // ��¼���Ͽ�ʱ��
    FLastDisconnectTime := GetTimestamp;
    if CanAutoReConnect then
      TIocpCustomTcpClient(Owner).PostReconnectRequestEvent(Self);
  end;
end;

{ TIocpCustomTcpClient }

function TIocpCustomTcpClient.Add: TIocpRemoteContext;
begin
  Result := TIocpRemoteContext(CreateContext());
  FList.Add(Result);
end;

function TIocpCustomTcpClient.Connect(const Host: AnsiString; Port: Word;
  AutoReConnect, ASync: Boolean): TIocpRemoteContext;
begin
  Result := nil;
  if Length(Host) = 0 then Exit;
  Open;
  Result := Add();
  if not Assigned(Result) then Exit;
  Result.Host := Host;
  Result.Port := Port;
  Result.AutoReConnect := AutoReConnect;
  Result.Connect(ASync, FConnectTimeOut);
end;

function TIocpCustomTcpClient.Connect(const Host: AnsiString; Port: Word;
  const BindAddr: AnsiString; AutoReConnect,
  ASync: Boolean): TIocpRemoteContext;
begin
  Result := nil;
  if Length(Host) = 0 then Exit;
  Open;
  Result := Add();
  if not Assigned(Result) then Exit;
  Result.BindAddr := BindAddr;
  Result.Host := Host;
  Result.Port := Port;
  Result.AutoReConnect := AutoReConnect;
  Result.Connect(ASync, FConnectTimeOut);
end;

constructor TIocpCustomTcpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF UNICODE}
  FList := TObjectList<TIocpRemoteContext>.Create();
  {$ELSE}
  FList := TObjectList.Create();
  {$ENDIF}
  FDisableAutoConnect := False;
  FConnectTimeOut := -1;
  FReconnectRequestPool := TObjectPool.Create(CreateReconnectRequest);
end;

function TIocpCustomTcpClient.CreateContext: TIocpCustomContext;
begin
  if FContextClass = nil then
    Result := TIocpRemoteContext.Create(Self)
  else
    Result := TIocpRemoteContext(FContextClass.Create(Self));
  OnCreateContext(Result);
end;

function TIocpCustomTcpClient.CreateReconnectRequest: TObject;
begin
  Result := TIocpASyncRequest.Create;
end;

procedure TIocpCustomTcpClient.Delete(Index: Integer);
var
  Context: TIocpRemoteContext;
begin
  Context := Items[index];
  Context.FAutoReConnect := False;
  Context.Disconnect;
end;

function TIocpCustomTcpClient.Delete(
  const pvContext: TIocpCustomContext): Integer;
begin
  Result := FList.Remove(TIocpRemoteContext(pvContext));
  //if Assigned(pvContext) then begin
  //  Result := FList.Remove(TIocpRemoteContext(pvContext));
  //end else
  //  Result := -1;
end;

destructor TIocpCustomTcpClient.Destroy;
begin
  Close;
  FReconnectRequestPool.WaitFor(5000);
  FList.Clear;
  FList.Free;
  FReconnectRequestPool.Free;
  inherited Destroy;
end;

function TIocpCustomTcpClient.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TIocpCustomTcpClient.GetItems(Index: Integer): TIocpRemoteContext;
begin
  {$IFDEF UNICODE}
  Result := FList[Index];
  {$ELSE}
  Result := TIocpRemoteContext(FList[Index]);
  {$ENDIF}
end;

procedure TIocpCustomTcpClient.OnReconnectRequestResponse(pvObject: TObject);
var
  lvContext: TIocpRemoteContext;
  lvRequest: TIocpASyncRequest;
begin
  // �˳�
  if (not Self.Active) then Exit;

  lvRequest := TIocpASyncRequest(pvObject);
  lvContext := TIocpRemoteContext(lvRequest.Data);

  if GetTimestamp - lvContext.FLastDisconnectTime >= RECONNECT_INTERVAL then begin
    // Ͷ����������������
    lvContext.PostConnectRequest();
  end else begin
    Sleep(100);
    // �ٴ�Ͷ����������
    PostReconnectRequestEvent(lvContext);
  end;
end;

procedure TIocpCustomTcpClient.OnReconnectRequestResponseDone(
  pvObject: TObject);
begin
  FReconnectRequestPool.ReleaseObject(pvObject);
end;

procedure TIocpCustomTcpClient.PostReconnectRequestEvent(
  const pvContext: TIocpRemoteContext);
var
  lvRequest: TIocpASyncRequest;
begin
  if not Active then Exit;
  lvRequest := TIocpASyncRequest(FReconnectRequestPool.GetObject);
  lvRequest.DoCleanUp;
  lvRequest.OnResponseDone := OnReconnectRequestResponseDone;
  lvRequest.OnResponse := OnReconnectRequestResponse;
  lvRequest.Data := pvContext;
  Engine.PostRequest(lvRequest);
end;

procedure TIocpCustomTcpClient.RegisterContextClass(
  pvContextClass: TIocpContextClass);
begin
  FContextClass := pvContextClass;
end;

function TIocpCustomTcpClient.ReleaseClientContext(
  const pvObject: TIocpCustomContext): Boolean;
begin
  if Assigned(pvObject) then
    FList.Remove(TIocpRemoteContext(pvObject));
  Result := False;
end;

procedure TIocpCustomTcpClient.Remove(const Value: TIocpRemoteContext);
begin
  if not Assigned(Value) then Exit;
  Value.FAutoReConnect := False;
  Value.Disconnect;
end;

procedure TIocpCustomTcpClient.RemoveAll;
var
  B: Boolean;
begin
  B := Active;
  Close;
  FReconnectRequestPool.WaitFor(5000);
  FList.Clear;
  Active := B;
end;

{ TIocpUdpServer }

function TIocpUdpServer.CheckNextSendRequest: Boolean;
var
  lvRequest: TIocpUdpSendRequest;
begin
  Result := False;
  FLocker.Enter();
  try
    lvRequest := TIocpUdpSendRequest(FSendRequestList.Pop);
  finally
    FLocker.Leave;
  end;
  if lvRequest <> nil then begin
    if lvRequest.ExecuteSend then begin
      Result := True;
      if (FDataMoniter <> nil) then
        FDataMoniter.IncPostSendObjectCounter;
    end else begin
      /// cancel request
      lvRequest.CancelRequest;
      {$IFDEF DEBUG_ON}
      DoStateMsgD(Self, '[0x%.4x] CheckNextSendRequest.ExecuteSend Return False',
         [SocketHandle]);
      {$ENDIF}
      ReleaseSendRequest(lvRequest);
    end;
    AtomicDecrement(FSendRef);
  end;
end;

procedure TIocpUdpServer.ClearRecvObjs;
var
  I: Integer;
begin
  for I := 0 to High(FRecvItems) do
    FreeAndNil(FRecvItems[i]);
  SetLength(FRecvItems, 0);
end;

procedure TIocpUdpServer.Close;
begin
  if not FActive then Exit;
  FActive := False;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  DoStateMsgD(Self, 'Server Closeing...');
  if Assigned(FListenSocket) then
    FListenSocket.Close;
  // engine Stop
  FIocpEngine.Stop;
  WaitFor(30000);
  if Assigned(FSendRequestPool) then begin
    FSendRequestPool.FreeDataObject;
    FSendRequestPool.Clear;
  end;
  ClearRecvObjs;
  DoStateMsgD(Self, 'Server Closed.');
end;

constructor TIocpUdpServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListenSocket := TRawSocket.Create;
  FSendRequestPool := TBaseQueue.Create;
  FSendRequestList := TIocpRequestLinkList.Create(256);
end;

procedure TIocpUdpServer.CreateSocket();
var
  ARequest: TIocpUdpRecvRequest;
  AAddr: sockaddr_in;
  I: Integer;
begin
  FListenSocket.CreateUdpSocket(True);
  // �������˿�
  AAddr := GetSocketAddr(AnsiString(FBindAddr), FPort);
  if not FListenSocket.Bind(TSockAddr(AAddr)) then
    RaiseLastOSError;
  // �������׽��ְ󶨵�IOCP���
  FIocpEngine.IocpCore.Bind(FListenSocket.SocketHandle, DWORD(FListenSocket.SocketHandle));
  // ��ʼ��������
  SetLength(FRecvItems, FIocpEngine.MaxWorkerCount);
  //SetLength(FRecvItems, GetCPUCount shl 1);
  for I := 0 to High(FRecvItems) do begin
    ARequest := TIocpUdpRecvRequest.Create(Self);
    if not ARequest.PostRequest then begin
      FreeAndNil(ARequest);
      Break;
    end else
      FRecvItems[i] := ARequest;
  end;
end;

destructor TIocpUdpServer.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FListenSocket);
  ClearRecvObjs;
  FSendRequestPool.FreeDataObject;
  FreeAndNil(FSendRequestPool);
  Assert(FSendRequestList.Count = 0);
  FreeAndNil(FSendRequestList);
end;

procedure TIocpUdpServer.DoReceiveData(Sender: TIocpUdpRequest);
begin
  if Assigned(FOnReceivedBuffer) then
    FOnReceivedBuffer(Sender, Sender.FRecvBuffer.buf, Sender.FBytesTransferred);
end;

function TIocpUdpServer.GetMaxSendingQueueSize: Integer;
begin
  if Assigned(FSendRequestList) then
    Result := FSendRequestList.MaxSize
  else
    Result := 0;
end;

function TIocpUdpServer.GetSendRequest: TIocpUdpSendRequest;
begin
  Result := TIocpUdpSendRequest(FSendRequestPool.DeQueue);
  if Result = nil then begin
    if FSendRequestClass <> nil then
      Result := FSendRequestClass.Create
    else
      Result := TIocpUdpSendRequest.Create;
  end;
  Result.DoCleanup;
  Result.FAlive := True;
  Result.FOwner := Self;
end;

function TIocpUdpServer.GetSocketHandle: TSocket;
begin
  Result := FListenSocket.SocketHandle;
end;

function TIocpUdpServer.InnerPostSendRequestAndCheckStart(
  pvSendRequest: TIocpUdpSendRequest): Boolean;
var
  lvStart: Boolean;
begin
  lvStart := False;
  FLocker.Enter();
  try
    Result := FSendRequestList.Push(pvSendRequest);
  finally
    FLocker.Leave;
  end;
  if Result and (AtomicIncrement(FSendRef) <= 1) then
    lvStart := true;  // start send work
  {$IFDEF DEBUG_ON}
  if (not Result) then
    DoStateMsgE(Self, strSend_PushFail, [SocketHandle, FSendRequestList.Count, FSendRequestList.MaxSize]);
  {$ENDIF}
  if lvStart then begin  // start send work
    if (FDataMoniter <> nil) then
      FDataMoniter.incPushSendQueueCounter;
    CheckNextSendRequest;
  end;
end;

function TIocpUdpServer.InnerSendData(const Dest: TSockAddrin; buf: Pointer; len: Cardinal;
  pvBufReleaseType: TDataReleaseType; pvTag: Integer;
  pvTagData: Pointer): Boolean;
var
  lvRequest: TIocpUdpSendRequest;
begin
  if Active then begin
    lvRequest := GetSendRequest;
    lvRequest.FAddr := Dest;
    lvRequest.SetBuffer(buf, len, pvBufReleaseType);
    lvRequest.Tag := pvTag;
    lvRequest.Data := pvTagData;
    Result := InnerPostSendRequestAndCheckStart(lvRequest);
    if not Result then begin
      /// Push Fail unbinding buf
      lvRequest.UnBindingSendBuffer;
      lvRequest.CancelRequest;
      ReleaseSendRequest(lvRequest);
    end;
  end else
    Result := False;
end;

procedure TIocpUdpServer.Open;
begin
  if FActive then Exit;
  FActive := True;
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  try
    if Assigned(FDataMoniter) then
      FDataMoniter.Clear;
    // ����IOCP����
    FIocpEngine.Start;
    // �����������׽���
    CreateSocket;
  except
    FActive := False;
    FListenSocket.Close;
    raise;
  end;
end;

procedure TIocpUdpServer.PostNextSendRequest;
begin
  CheckNextSendRequest;
end;

procedure TIocpUdpServer.RegisterSendRequestClass(
  pvClass: TIocpUdpSendRequestClass);
begin
  FSendRequestClass := pvClass;
end;

function TIocpUdpServer.ReleaseSendRequest(pvObject: TIocpUdpSendRequest): Boolean;
begin
  AtomicDecrement(FSendRef);
  Result := False;
  if (not Assigned(Self)) or (not Assigned(FSendRequestPool)) then
    Assert(False);
  if lock_cmp_exchange(True, False, pvObject.FAlive) = True then begin
    if Assigned(FDataMoniter) then
      InterlockedIncrement(FDataMoniter.FSendRequestReturnCounter);
    pvObject.DoCleanUp;
    pvObject.FOwner := nil;
    FSendRequestPool.EnQueue(pvObject);
    Result := True;
  end else begin
    if IsDebugMode then
      Assert(False);
  end;
end;

function TIocpUdpServer.Send(const Dest: TSockAddrin; buf: Pointer; len: Cardinal;
  CopyBuf: Boolean): Boolean;
var
  Data: Pointer;
begin
  Result := False;
  if (buf = nil) or (Len = 0) then Exit;
  if CopyBuf and (Len <= FWSASendBufferSize) then begin
    {$IFDEF UseSendMemPool}
    Data := PopMem;
    Move(buf^, Data^, len);
    Result := InnerSendData(Dest, Data, len, dtMemPool);
    {$ELSE}
    GetMem(Data, len);
    Move(buf^, Data^, len);
    Result := InnerSendData(Dest, Data, len, dtFreeMem);
    {$ENDIF}
  end else
    Result := InnerSendData(Dest, buf, len, dtNone);
end;

function TIocpUdpServer.Send(const Dest: TSockAddrin; buf: Pointer; len: Cardinal;
  BufReleaseType: TDataReleaseType): Boolean;
begin
  if (buf = nil) or (Len = 0) then
    Result := True
  else
    Result := InnerSendData(Dest, buf, len, BufReleaseType);
end;

procedure TIocpUdpServer.SetMaxSendingQueueSize(pvSize: Integer);
begin
  FSendRequestList.MaxSize := pvSize;
end;

function TIocpUdpServer.WaitFor(pvTimeOut: Cardinal): Boolean;
var
  T: Int64;
begin
  T := GetTimestamp;
  while True do begin
    if InterlockedIncrement(FSendRef) > 1 then begin
      InterlockedDecrement(FSendRef);
      Sleep(50);
      if (pvTimeOut > 0) and (GetTimestamp - T > pvTimeOut) then
        Break;
    end else begin
      InterlockedDecrement(FSendRef);
      Break;
    end;
  end;
  Result := FSendRef < 1;
end;

{ TIocpUdpRecvRequest }

procedure TIocpUdpRecvRequest.Clear;
begin
  if FRecvBuffer.len > 0 then begin
    FreeMem(FRecvBuffer.buf, FRecvBuffer.len);
    FRecvBuffer.len := 0;
  end;
end;

constructor TIocpUdpRecvRequest.Create(AOwner: TIocpUdpServer);
begin
  FOwner := AOwner;
  inherited Create();
end;

destructor TIocpUdpRecvRequest.Destroy;
begin
  if FRecvBuffer.len > 0 then
    FreeMem(FRecvBuffer.buf, FRecvBuffer.len);
  inherited Destroy;
end;

procedure TIocpUdpRecvRequest.DoRecvData;
begin
  FOwner.DoReceiveData(Self);
end;

function TIocpUdpRecvRequest.GetPeerAddr: Cardinal;
begin
  Result := inet_addr(inet_ntoa(FFrom.sin_addr));
end;

function TIocpUdpRecvRequest.GetRemoteAddr: AnsiString;
begin
  Result := inet_ntoa(FFrom.sin_addr);
end;

function TIocpUdpRecvRequest.GetRemotePort: Word;
begin
  Result := ntohs(FFrom.sin_port);
end;

procedure TIocpUdpRecvRequest.HandleResponse;
var
  IsDisconnect: Boolean;
begin
  {$IFDEF DEBUG_ON}
  if FOwner = nil then
    Assert(FOwner <> nil);
  {$ENDIF}
  if not FOwner.Active then begin
    {$IFDEF DEBUG_ON}
    FOwner.DoStateMsgD(Self, strRecv_EngineOff, [FOwner.SocketHandle]);
    {$ENDIF}
    Exit;
  end;
  IsDisconnect := True;
  try
    if (Assigned(FOwner.FDataMoniter)) then begin
      FOwner.FDataMoniter.incResponseWSARecvCounter;
      FOwner.FDataMoniter.incRecvdSize(FBytesTransferred);
    end;
    if ErrorCode <> 0 then begin
      if ErrorCode <> ERROR_OPERATION_ABORTED then  // �첽����ȴ�ʱ�����˹ر��׽���
        FOwner.DoStateMsgE(Self, strRecv_Error, [FOwner.SocketHandle, ErrorCode]);
      IsDisconnect := False;
    end else begin
      IsDisconnect := False;
      DoRecvData;
    end;
  finally
    if not IsDisconnect then
      PostRequest;
  end;
end;

function TIocpUdpRecvRequest.PostRequest: Boolean;
var
  lvRet: Integer;
begin
  if (FOwner = nil) or (not FOwner.Active) then begin
    Result := False;
    Exit;
  end;
  lvRet := FOwner.FWSARecvBufferSize;
  if Integer(FRecvBuffer.len) <> lvRet then begin
    if FRecvBuffer.len > 0 then
      FreeMem(FRecvBuffer.buf);
    FRecvBuffer.len := lvRet;
    GetMem(FRecvBuffer.buf, lvRet);
  end;
  FRecvdFlag := 0;
  FBytesTransferred := 0;
  FFromLen := SizeOf(FFrom);
  if (Assigned(FOwner.FDataMoniter)) then
    FOwner.FDataMoniter.incPostWSARecvCounter;
  lvRet := WSARecvFrom(FOwner.FListenSocket.SocketHandle,
    @FRecvBuffer,
    1,
    @FBytesTransferred,
    FRecvdFlag,
    @FFrom,
    @FFromLen,
    LPWSAOVERLAPPED(@FOverlapped),
    nil);
  if lvRet = SOCKET_ERROR then begin
    lvRet := WSAGetLastError;
    Result := lvRet = WSA_IO_PENDING;
    if not Result then begin
      {$IFDEF DEBUG_ON}
      FOwner.DoStateMsgE(Self, strRecv_PostError, [FOwner.FListenSocket.SocketHandle, lvRet]);
      {$ENDIF}
      if lvRet = 10054 then begin
        Sleep(100);
        PostRequest;
      end;
    end;
  end else
    Result := True;
end;

procedure TIocpUdpRecvRequest.Send(const Data: WideString);
begin
  if Length(Data) = 0 then Exit;
  FOwner.Send(FFrom, @Data[1], Length(Data) shl 1, True);
end;

procedure TIocpUdpRecvRequest.Send(const Data: AnsiString);
begin
  if Length(Data) = 0 then Exit;
  FOwner.Send(FFrom, @Data[1], Length(Data), True);
end;

{$IFDEF UNICODE}
procedure TIocpUdpRecvRequest.Send(const Data: UnicodeString);
begin
  if Length(Data) = 0 then Exit;
  FOwner.Send(FFrom, @Data[1], Length(Data) shl 1, True);
end;
{$ENDIF}

procedure TIocpUdpRecvRequest.Send(buf: Pointer; len: Cardinal);
begin
  if len > 0 then
    FOwner.Send(FFrom, buf, len);
end;

{ TIocpUdpSendRequest }

procedure TIocpUdpSendRequest.CheckClearSendBuffer;
begin
  if FLen > 0 then begin
    case FSendBufferReleaseType of
      dtDispose: Dispose(FBuf);
      dtFreeMem: FreeMem(FBuf);
      dtMemPool: FOwner.PushMem(FBuf);
    end;
  end;
  FSendBufferReleaseType := dtNone;
  FLen := 0;
end;

constructor TIocpUdpSendRequest.Create;
begin
  inherited Create;
end;

destructor TIocpUdpSendRequest.Destroy;
begin
  CheckClearSendBuffer;
  inherited Destroy;
end;

procedure TIocpUdpSendRequest.DoCleanUp;
begin
  CheckClearSendBuffer;
  FOwner := nil;
  FBuf := nil;
  FLen := 0;
end;

function TIocpUdpSendRequest.ExecuteSend: Boolean;
begin
  if (FBuf = nil) or (FLen = 0) then begin
    {$IFDEF DEBUG_ON}
    FOwner.DoStateMsgD(Self, strSend_Zero, [FOwner.SocketHandle]);
    {$ENDIF}
    Result := False;
  end else
    Result := InnerPostRequest(FBuf, FLen);
end;

procedure TIocpUdpSendRequest.HandleResponse;
begin
  FIsBusying := False;
  if FOwner = nil then Exit;
  if Assigned(FOwner.FDataMoniter) then begin
    FOwner.FDataMoniter.incSentSize(FBytesTransferred);
    FOwner.FDataMoniter.incResponseWSASendCounter;
  end;
  if not FOwner.Active then begin
    {$IFDEF DEBUG_ON}
    FOwner.DoStateMsgD(Self, strSend_EngineOff, [FOwner.SocketHandle]);
    {$ENDIF}
  end else if ErrorCode <> 0 then begin
    FOwner.DoStateMsgE(Self, strSend_Err, [FOwner.SocketHandle, ErrorCode]);
    FOwner.CheckNextSendRequest;
  end else begin
    if Assigned(FOwner.FDataMoniter) then
      FOwner.FDataMoniter.incResponseSendObjectCounter;
    FOwner.CheckNextSendRequest;
  end;
end;

function TIocpUdpSendRequest.InnerPostRequest(buf: Pointer;
  len: Cardinal): Boolean;
var
  lvErrorCode, lvRet: Integer;
  dwFlag, lpNumberOfBytesSent: Cardinal;
  lvOwner: TIocpUdpServer;
begin
  Result := False;
  if not Assigned(FOwner) then Exit;
  FIsBusying := True;
  FSendBuf.buf := buf;
  FSendBuf.len := len;
  dwFlag := 0;
  lpNumberOfBytesSent := 0;

  // maybe on HandleResonse and release self
  lvOwner := FOwner;
  Result := lvOwner.Active;
  if not Result then Exit;
  lvRet := WSASendTo(lvOwner.SocketHandle,
                @FSendBuf, 1, @lpNumberOfBytesSent, dwFlag,
                TSockAddr(FAddr), SizeOf(FAddr),
                LPWSAOVERLAPPED(@FOverlapped),   // d7 need to cast
                nil);
  if lvRet = SOCKET_ERROR then begin
    // Ͷ��ʧ��
    lvErrorCode := WSAGetLastError;
    Result := lvErrorCode = WSA_IO_PENDING;
    if not Result then begin
      //���ʹ����ͷŸ�SOCKET��Ӧ��������Դ
      FIsBusying := False;
      lvOwner.DoStateMsgE(Self, strSend_PostError, [lvOwner.SocketHandle, lvErrorCode]);
    end else begin
      // ���ͳɹ�
      if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then begin
        lvOwner.FDataMoniter.incPostWSASendSize(len);
        lvOwner.FDataMoniter.incPostWSASendCounter;
      end;
    end;
  end else begin
    // ���ͳɹ�
    Result := True;
    if (lvOwner <> nil) and (lvOwner.FDataMoniter <> nil) then begin
      lvOwner.FDataMoniter.incPostWSASendSize(len);
      lvOwner.FDataMoniter.incPostWSASendCounter;
    end;
  end;
end;

procedure TIocpUdpSendRequest.ResponseDone;
begin
  if FOwner = nil then begin
    if IsDebugMode then
      Assert(FOwner <> nil);
  end else
    FOwner.ReleaseSendRequest(Self);
end;

procedure TIocpUdpSendRequest.SetBuffer(buf: Pointer; len: Cardinal;
  pvCopyBuf: Boolean);
var
  lvBuf: PAnsiChar;
begin
  if pvCopyBuf then begin
    GetMem(lvBuf, len);
    Move(buf^, lvBuf^, len);
    SetBuffer(lvBuf, len, dtFreeMem);
  end else
    SetBuffer(buf, len, dtNone);
end;

procedure TIocpUdpSendRequest.SetBuffer(buf: Pointer; len: Cardinal;
  pvBufReleaseType: TDataReleaseType);
begin
  CheckClearSendBuffer;
  FBuf := buf;
  FLen := len;
  FSendBufferReleaseType := pvBufReleaseType;
end;

procedure TIocpUdpSendRequest.UnBindingSendBuffer;
begin
  FBuf := nil;
  FLen := 0;
  FSendBufferReleaseType := dtNone;
end;

{ TIocpBlockSocketStream }

constructor TIocpBlockSocketStream.Create(ASocket: TIocpCustomBlockTcpSocket);
begin
  FSocket := ASocket;
  FReadTimeOut := 30000;
end;

function TIocpBlockSocketStream.Read(var Buffer; Count: Integer): Longint;
var
  P: PAnsiChar;
  ALen, I: Integer;
  T: Cardinal;
begin
  ALen := Count;
  P := @Buffer;
  Result := 0;
  T := GetTickCount;
  while ALen > 0 do begin
    if ALen > 4096 then
      I := FSocket.FRawSocket.RecvBuf(P^, 4096)
    else
      I := FSocket.FRawSocket.RecvBuf(P^, ALen);
    if I > 0 then begin
      Inc(P, I);
      Inc(Result, I);
      Dec(ALen, I);
    end else begin
      if (I = -1){$IFDEF POSIX} or (I = 0){$ENDIF} then
        FSocket.RaiseLastOSError(False);
      if (not FSocket.Active) or (FReadTimeOut < 1) or
        ((FReadTimeOut > 0) and (GetTickCount - T > Cardinal(FReadTimeOut))) then
      begin
        if FSocket.Active then          
          FSocket.Disconnect;
        Break;
      end;
      Sleep(50);
      {$IFDEF MSWINDOWS}
      SwitchToThread;  
      {$ELSE}
      TThread.Yield;
      {$ENDIF}
    end;
  end;
end;

procedure TIocpBlockSocketStream.SetSize(NewSize: Integer);
begin
  raise Exception.Create(strSocket_RSNotSup);
end;

function TIocpBlockSocketStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  raise Exception.Create(strSocket_RSNotSup);
end;

procedure TIocpBlockSocketStream.SetSize(const NewSize: Int64);
begin
  raise Exception.Create(strSocket_RSNotSup);
end;

function TIocpBlockSocketStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FSocket.FRawSocket.SendBuf(Buffer, Count);
end;

{ TIocpDisconnectExRequest }

function TIocpDisconnectExRequest.DirectlyPost: Boolean;
var
  lvErrorCode: Integer;
begin
  Result := IocpDisconnectEx(FContext.FRawSocket.SocketHandle, @FOverlapped, TF_REUSE_SOCKET, 0);
  if not Result then begin
    lvErrorCode := WSAGetLastError;
    if lvErrorCode <> ERROR_IO_PENDING then begin
      // do normal close;
      FContext.FRawSocket.close;
      {$IFDEF WRITE_LOG}
      if Assigned(FOwner) then
        FOwner.DoStateMsgE(Self, 'DirectlyPost Error: %d', [lvErrorCode]);
      {$ENDIF}
      // context may return to pool
      FContext.decReferenceCounter(Self, Format('DirectlyPost Error: %d', [lvErrorCode]));
      Result := false;
    end else
      Result := true;
  end;
end;

function TIocpDisconnectExRequest.PostRequest: Boolean;
var
  lvErrorCode: Integer;
begin
  Result := False;
  if FContext.IncReferenceCounter(Self, 'PostRequest') then  begin
    Result := IocpDisconnectEx(FContext.FRawSocket.SocketHandle, @FOverlapped, TF_REUSE_SOCKET, 0);
    if not Result then begin
      lvErrorCode := WSAGetLastError;
      if lvErrorCode <> ERROR_IO_PENDING then begin
        // do normal close;
        FContext.FRawSocket.close;
        {$IFDEF WRITE_LOG}
        if Assigned(FOwner) then
          FOwner.DoStateMsgE(Self, 'PostRequest Error: %d', [lvErrorCode]);
        {$ENDIF}
        // context may return to pool
        FContext.decReferenceCounter(Self, Format('DirectlyPost Error: %d', [lvErrorCode]));
        Result := false;
      end else
        Result := true;
    end;
  end;
end;

{ TIocpActionRequest }

procedure TIocpActionRequest.HandleResponse;
begin
  inherited;  
end;

initialization
  Workers := TIocpTask.GetInstance;

finalization
  Workers := nil;

end.


