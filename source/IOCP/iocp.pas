{*******************************************************}
{                                                       }
{       IOCP 单元  (本单元已经封装所有对外类)           }
{                                                       }
{       版权所有 (C) 2015 YangYxd (基于DIOCP修改版本)   }
{                                                       }
{*******************************************************}
{
  说明： 本单元集成本项目中所有外部可直接使用的模块和类，
  在使用时，你只需要引用本单元就可以满足常规使用。

  感谢 DIOCP 项目作者天地弦，本项目核心来自于它的DIOCP，
  并且核心部分修改很少，原版已经非常稳定。主要是简化的使
  用方式，对代码作了整理清理，并完善了HTTP服务。

  本源码受原作者节制，你可以任意复制、修改、使用，如发现
  BUG请报告给我们。为了倡导开源，你的一切修改版本，我们
  希望也能开源。

  警告：使用本项目源码产生的一切后果由使用者自行负责。

  DIOCP官方QQ群：320641073
  QDAC官方QQ群：250530692
  新版本获取地址： http://pan.baidu.com/s/1kTKgHoj
  提取密码：ma5w
}

unit iocp;

{$I 'iocp.inc'}
// 是否启用 TIocpHttpServer
{$DEFINE UseHttpServer}

interface

uses
  iocp.Utils.Hash, 
  {$IFDEF UseHttpServer}iocp.Http, {$ENDIF}
  iocp.Sockets, iocp.Task, iocp.Winapi.TlHelp32, iocp.Utils.MemPool,
  iocp.Sockets.Utils, iocp.Core.Engine, iocp.Res, iocp.RawSockets,
  iocp.Utils.Queues, iocp.Utils.ObjectPool, WinSock,
  SyncObjs, Windows, Classes, SysUtils;

type
  TIocpStateMsgType = iocp.Sockets.TIocpStateMsgType;
  
type
  TIocpContext = iocp.Sockets.TIocpCustomContext;

type
  TMemPool = iocp.Utils.MemPool.TYXDMemPool;
  TIocpMemPool = iocp.Utils.MemPool.TIocpMemPool;
  TIocpStream = iocp.Utils.MemPool.TIocpStream;
  TIocpSocketStream = iocp.Sockets.TIocpBlockSocketStream;

type
  /// <summary>
  /// 阻塞型 UDP Scoket
  /// </summary>
  TIocpUdpSocket = iocp.Sockets.TIocpCustomBlockUdpSocket;

type
  /// <summary>
  /// 阻塞型 TCP Scoket
  /// </summary>
  TIocpTcpSocket = class(TIocpCustomBlockTcpSocket)
  end;

type
  /// <summary>
  /// 高性能Iocp服务端 (基础功能)
  /// </summary>
  TIocpTcpServer = class(TIocpCustomTcpServer)
  end;

type
  /// <summary>
  /// 高性能多连接Iocp客户端 (异步通讯)
  /// </summary>
  TIocpTcpClient = class(TIocpCustomTcpClient)
  end;

type
  /// <summary>
  /// Iocp UDP 服务端 （基础功能）
  /// </summary>
  TIocpUdpServer = iocp.Sockets.TIocpUdpServer;
  TIocpUdpRequest = iocp.Sockets.TIocpUdpRequest;

type
  TIocpTcpCodecServer = class;
  TIocpConnection = class;
  
  TOnRecvExecute = procedure (Sender: TIocpTcpCodecServer;
    AConnection: TIocpConnection; var RequestData: TObject) of object;
  TOnDecodeData = function (Connection: TIocpConnection; const Stream: TIocpStream; var Request: TObject): Boolean of object;

  PIocpAsyncExecute = ^TIocpAsyncExecute;
  TIocpAsyncExecute = packed record
    Conn: TIocpConnection;
    Request: TObject;
  end;

  /// <summary>
  /// Iocp 连接
  /// </summary>
  TIocpConnection = class(TIocpClientContext)
  private
    FStream: TIocpStream;
    FProcessRequesting: Boolean;
    FRequestQueue: TSimpleQueue;
  protected
    procedure DoCleanUp; override;
    function PopMem: Pointer; inline;
    procedure PushMem(const V: Pointer); inline;

    procedure ClearRequestTaskObject();
    procedure DoRecvExecute(const RequestObj: TObject);
    procedure DoJob(AJob: PIocpJob);
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrorCode: Integer); override;
  public
    constructor Create(AOwner: TIocpCustom); override;
    destructor Destroy; override;  

    /// <summary>
    /// 数据缓冲流
    /// </summary>
    property BufferStream: TIocpStream read FStream;
  end;

  /// <summary>
  /// TCP 服务端，支持通过 OnDecodeData 事件解码数据缓冲区
  /// </summary>
  TIocpTcpCodecServer = class(TIocpTcpServer)
  private
    FMemPool: TIocpMemPool;
    FIocpStreamPool: TBaseQueue;
    FOnRecvExecute: TOnRecvExecute;
    FOnDecodeData: TOnDecodeData;
  protected
    function GetStream: TIocpStream;
    procedure FreeStream(V: TIocpStream);
    function DoDecodeData(Connection: TIocpConnection; Stream: TIocpStream;
      var Request: TObject): Boolean; 
    procedure DoRecvExecute(const AConnection: TIocpConnection;
      var RequestObj: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PopMem: Pointer;
    procedure PushMem(const V: Pointer);
  published
    /// <summary>
    /// 每次成功解码后执行
    /// </summary>
    property OnRecvExecute: TOnRecvExecute read FOnRecvExecute write FOnRecvExecute;
    /// <summary>
    /// 解码回调事件，在接收到数据时触发
    /// </summary>
    property OnDecodeData: TOnDecodeData read FOnDecodeData write FOnDecodeData;
  end;

{$IFDEF UseHttpServer}
type
  TIocpHttpMethod = iocp.Http.TIocpHttpMethod;
  TIocpHttpReqVer = iocp.Http.TIocpHttpReqVer;

  TIocpHttpServer = iocp.Http.TIocpHttpServer;
  TIocpHttpRequest = iocp.Http.TIocpHttpRequest;
  TIocpHttpResponse = iocp.Http.TIocpHttpResponse;

  TOnHttpFilter = iocp.Http.TOnHttpFilter;
  TOnHttpRequest = iocp.Http.TOnHttpRequest;
  TOnHttpGetSession = iocp.Http.TOnHttpGetSession;
  TOnHttpFreeSession = iocp.Http.TOnHttpFreeSession;

  TIocpHttpConnection = iocp.Http.TIocpHttpConnection;
  TIocpHttpFromDataItem = iocp.Http.TIocpHttpFromDataItem;
  TFileOnlyStream = iocp.Http.TFileOnlyStream;
  TIocpPointerStream = iocp.Http.TIocpPointerStream;
{$ENDIF}

type
  /// <summary>
  /// 阻塞型 TCP Scoket 池
  /// </summary>
  TIocpTcpSocketPool = class(TObject)
  private
    FList: TStringHash;
    FMax: Integer;
    FDestroying: Boolean;
    FLocker: TCriticalSection;
    function GetKey(const RemoteAddr: string; RemotePort: Word): string;
  protected
    function GetQueues(const Key: string): TBaseQueue; overload;
    function GetQueues(const RemoteAddr: string; RemotePort: Word): TBaseQueue; overload;
    procedure DoFreeItem(Item: PHashItem);
  public
    constructor Create(const AMax: Integer = 8);
    destructor Destroy; override;
    procedure Clear;
    /// <summary>
    /// 弹出一个已经Socket，已经设定好远程地址和端口，弹出的Socket可能已经连接
    /// </summary>
    function Pop(const RemoteAddr: string; RemotePort: Word): TIocpTcpSocket;
    /// <summary>
    /// 压入使用完的Socket。 Push不会去管连接是否断开
    /// </summary>
    procedure Push(const Socket: TIocpTcpSocket);
    // 每个主机+端口最多保持的连接数 (默认8), 设为<1时则不限制连接数
    property Max: Integer read FMax write FMax default 8;
  end;

type
  /// <summary>
  /// TCP 客户端代理服务
  /// </summary>
  TIocpTcpClientProxy = class;

  TIocpTcpProxyCallBack = procedure (Socket: TIocpTcpSocket; Tag: Integer) of Object;

  PIocpTcpProxyData = ^TIocpTcpProxyData;
  TIocpTcpProxyData = record
    Socket: TIocpTcpSocket;
    StartTime: Int64;
    Tag: Integer;
    IsTimeOut: Boolean;
    CallBack: TIocpTcpProxyCallBack;
    Prev: PIocpTcpProxyData;
    Next: PIocpTcpProxyData;
  end;

  TIocpTcpProxyWorkerThread = class(TThread)
  private
    FOwner: TIocpTcpClientProxy;
  protected
    procedure Execute; override;
  end;

  // Proxy 内部使用一个双向链表来管理待处理的连接， 在断线、
  // 超时、接收到数据时会触发CallBack, CallBack 用来接收和处理数据，可以不
  // 设置 CallBack， 那样在收到数据时，Proxy会清空接收缓冲区，并将这个连接
  // 从链表中删除。
  // Proxy 内部会创建一个工作线程，不断的扫描各个连接是否有数据到达，
  // 或者出现断线、超时等情况，适时触发CallBack或清理连接接收缓冲区。
  TIocpTcpClientProxy = class(TObject)
  private
    FPool: TIocpTcpSocketPool;
    FMemPool: TMemPool;
    FHostSelect: TStringHash;
    FList: PIocpTcpProxyData;
    FLast: PIocpTcpProxyData;
    FWorker: array of TIocpTcpProxyWorkerThread;
    FOnStateMsg: TOnStateMsgEvent;
    FIsDestroying: Boolean;
    FTimeOut: Integer;
    function GetIsDestroying: Boolean;
  protected
    FLocker: TCriticalSection;
    procedure DoWorker();
    procedure DoStateMsg(Sender: TObject; MsgType: TIocpStateMsgType; const Msg: string);
    function CheckHost(const Host: string; const Port: Word): Boolean;
    procedure AddItem(const Socket: TIocpTcpSocket;
      const CallBack: TIocpTcpProxyCallBack; const Tag: Integer);
    function PopItem(): PIocpTcpProxyData;
    procedure PushItem(const V: PIocpTcpProxyData);
    function ItemEmpty: Boolean;
    function DoCallBack(Socket: TIocpTcpSocket;
      CallBack: TIocpTcpProxyCallBack; const Tag: Integer): Boolean;

  public
    constructor Create(AMaxWorker: Integer = 1);
    destructor Destroy; override;
    procedure Clear; virtual;

    // 根据一个以","分隔的IP字符串列表，自动更换IP地址功能

    // 根据一个以","分隔的IP字符串列表，返回当前使用的IP地址
    function HostSelect(const Host: string): string; virtual;
    // 根据一个以","分隔的IP字符串列表，切换当前使用的IP地址，切换成功返回True
    // 切换失败可能是变更速度太快，或者Host为空
    function HostChange(const Host: string): Boolean; virtual;
    // HostChange到可用IP时，调用此方法来中止其它线程的切换操作
    procedure HostUpdate(const Host: string); overload;
    // HostChange到可用IP时，调用此方法来中止其它线程的切换操作
    procedure HostUpdate(const Host, IPAddr: string); overload;

    function GetSocket(const Host: string; const Port: Word): TIocpTcpSocket;
    procedure ReleaseSocket(V: TIocpTcpSocket); inline;
    
    function Pop(const RemoteAddr: string; RemotePort: Word): TIocpTcpSocket;
    procedure Push(const Socket: TIocpTcpSocket);

    /// <summary>
    /// 发送数据到指定远程主机端口。
    /// <param name="CallBack">回调函数，返回True时一定会被执行到</param>
    /// <param name="Sync">是否使用同步发送，为True时直接调用CallBack</param>
    /// </summary>
    function Send(const Host: string; Port: Word; S: TStream;
      CallBack: TIocpTcpProxyCallBack = nil; Sync: Boolean = False; Tag: Integer = 0): Boolean; overload;
    function Send(const Host: string; Port: Word; Data: PAnsiChar;
      Len: Cardinal; CallBack: TIocpTcpProxyCallBack = nil; Sync: Boolean = False; Tag: Integer = 0): Boolean; overload;
    function Send(const Host: string; Port: Word; const Data: AnsiString;
      CallBack: TIocpTcpProxyCallBack = nil; Sync: Boolean = False; Tag: Integer = 0): Boolean; overload;
    function Send(const Host: string; Port: Word; const Data: WideString;
      CallBack: TIocpTcpProxyCallBack = nil; Sync: Boolean = False; Tag: Integer = 0): Boolean; overload;

    property OnStateInfo: TOnStateMsgEvent read FOnStateMsg write FOnStateMsg;
    // 接收超时设置， 小于1时不判断超时，默认20秒
    property TimeOut: Integer read FTimeOut write FTimeOut;
    // 是否正在释放
    property IsDestroying: Boolean read GetIsDestroying;
  end;

{
 一些常用函数
}

// 获取CPU使用率
function GetCPUUsage: Integer;
// 获取任务工作者数量
function GetTaskWorkerCount: Integer;
// 获取指定进程的内存使用情况
function GetProcessMemUse(PID: Cardinal): Cardinal;
// 获取指定进程句柄数量
function GetProcessHandleCount(PID: Cardinal): Cardinal;

// 获取当前时间戳 （高精度计时）
function GetTimestamp: Int64;
// 获取文件最后写入时间（既修改时间）
function GetFileLastWriteTime(const AFileName: AnsiString): TDateTime;

// 将一个代表文件大小的数据转换成可读的字符中
function TransByteSize(const pvByte: Int64): string;
// 获取当前运行信息
function GetRunTimeInfo: string;

procedure Register;

implementation

const
  ComPageName = 'YxdIOCP';

procedure Register;
begin
  RegisterComponents(ComPageName, [TIocpUdpSocket]);
  RegisterComponents(ComPageName, [TIocpUdpServer]);
  RegisterComponents(ComPageName, [TIocpTcpSocket]);
  RegisterComponents(ComPageName, [TIocpTcpClient]);
  RegisterComponents(ComPageName, [TIocpTcpServer]);
  RegisterComponents(ComPageName, [TIocpTcpCodecServer]);
  {$IFDEF UseHttpServer}
  RegisterComponents(ComPageName, [TIocpHttpServer]);
  {$ENDIF}
end;

var
  Workers: TIocpTask;
  SMemPool: TIocpMemPool;

function GetTimestamp: Int64;
begin
  Result := iocp.Core.Engine.GetTimestamp;
end;

function GetFileLastWriteTime(const AFileName: AnsiString): TDateTime;
begin
  Result := iocp.Sockets.Utils.GetFileLastWriteTime(AFileName);
end;

function GetCPUUsage: Integer;
begin
  Result := TIocpTask.GetCPUUsage;
end;

function GetTaskWorkerCount: Integer;
begin
  Result := Workers.WorkerCount;
end;

function GetProcessMemUse(PID: Cardinal): Cardinal;
begin
  Result := iocp.Winapi.TlHelp32.GetProcessMemUse(PID);
end;  

function GetProcessHandleCount(PID: Cardinal): Cardinal;
begin
  Result := iocp.Winapi.TlHelp32.GetProcessHandleCount(PID);
end;

function TransByteSize(const pvByte: Int64): string;
begin
  Result := iocp.Sockets.TransByteSize(pvByte);
end;

function GetRunTimeInfo: string;
begin
  Result := iocp.Sockets.GetRunTimeInfo;
end;

{ TIocpConnection }

procedure TIocpConnection.ClearRequestTaskObject;
var
  P: PIocpAsyncExecute;
begin
  Lock;
  try
    if not FProcessRequesting then Exit;    
    while True do begin
      P := FRequestQueue.DeQueue;
      if P = nil then
        Break;
      try
        if Assigned(P.Request) then begin
          try
            FreeAndNil(P.Request);
          except
            if Assigned(Owner) then
              Owner.DoStateMsgE(Self, Exception(ExceptObject));
          end;
        end;
      finally
        SMemPool.Push(P);
      end;
    end;
  finally
    FProcessRequesting := False;
    UnLock;
  end;
end;

constructor TIocpConnection.Create(AOwner: TIocpCustom);
begin
  inherited Create(AOwner);
  FRequestQueue := TSimpleQueue.Create();
end;

destructor TIocpConnection.Destroy;
begin
  // 清理待处理请求队列
  ClearRequestTaskObject;
  if Assigned(Workers) and FProcessRequesting then
    Workers.Clear(Self);
  if Assigned(Owner) and (Assigned(FStream)) then
    TIocpTcpCodecServer(Owner).FreeStream(FStream);
  FStream := nil;
  FRequestQueue.Free;
  inherited Destroy;
end;

procedure TIocpConnection.DoCleanUp;
begin
  inherited DoCleanUp;
  FProcessRequesting := False;
  // 清理待处理请求队列
  ClearRequestTaskObject; 
  TIocpTcpCodecServer(Owner).FreeStream(FStream);
  FStream := nil;
end;

procedure TIocpConnection.DoJob(AJob: PIocpJob);
const
  DEBUGINFO = '业务逻辑处理...';
var
  Data: TIocpAsyncExecute;
  P: Pointer;
begin
  while Assigned(Self) and (Self.Active) do begin
    Lock;
    try
      P := FRequestQueue.DeQueue;
      if P = nil then begin
        FProcessRequesting := False;
        Exit;
      end;
    finally
      UnLock;
    end;
    try
      Data := PIocpAsyncExecute(P)^;
      SMemPool.Push(P);
      try
        // 连接已经断开, 放弃处理逻辑
        if (Self = nil) then Exit;
        // 连接已经断开, 放弃处理逻辑
        if (Owner = nil) then Exit;
        // 已经不是当时请求的连接，放弃处理逻辑
        if (Data.Conn = nil) or (Data.Conn.Handle <> Self.Handle) then Exit;
        Self.LockContext(Self, DEBUGINFO);
        try
          TIocpTcpCodecServer(Owner).DoRecvExecute(Data.Conn, Data.Request);
          LastActivity := GetTimestamp;
        finally
          Self.UnLockContext(Self, DEBUGINFO);
        end;
      finally
        FreeAndNil(Data.Request);
      end;
    except
      Owner.DoStateMsgE(Self, Exception(ExceptObject));
    end;
  end;
end;

procedure TIocpConnection.DoRecvExecute(const RequestObj: TObject);
var
  Data: PIocpAsyncExecute;
begin
  if Assigned(Workers) and Active then begin
    Data := SMemPool.Pop;
    Data.Conn := Self;
    Data.Request := RequestObj;
    Lock;
    try
      FRequestQueue.EnQueue(Data);
      if not FProcessRequesting then begin
        FProcessRequesting := True;
        Workers.Post(DoJob, FRequestQueue);
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TIocpConnection.OnRecvBuffer(buf: Pointer; len: Cardinal;
  ErrorCode: Integer);
var
  Last: Int64;
  FRequest: TObject;
begin
  // 没有解码器时关闭连接
  if (not Assigned(Owner)) or (not Assigned(TIocpTcpCodecServer(Owner).FOnDecodeData)) then begin
    CloseConnection;
    Exit;
  end;

  // 获取一个缓冲流
  if not Assigned(FStream) then begin
    FStream := TIocpTcpCodecServer(Owner).GetStream;
    FStream.Handle := Self.Handle;
    FStream.SetCunkSize(TIocpTcpCodecServer(Owner).RecvBufferSize);
    FStream.OnPopMem := TIocpTcpCodecServer(Owner).PopMem;
    FStream.OnPushMem := TIocpTcpCodecServer(Owner).PushMem;
  end;

  // 将新接收到的数据解入缓冲流
  FStream.Write(Buf^, Len);
  
  // 尝试解码
  while Assigned(FStream) do begin
    FStream.WaitRecv := False;
    Last := FStream.GetPosition;

    if not TIocpTcpCodecServer(Owner).FOnDecodeData(Self, FStream, FRequest) then begin
      CloseConnection;
      Exit;
    end;

    // 等待接收数据
    if FStream.WaitRecv then
      Exit;

    if FStream.GetPosition() = Last then
      Break;

    // 解码成功
    if Assigned(FRequest) then
      DoRecvExecute(FRequest);
    
    // 继续解码
    if Assigned(FStream) and (FStream.GetPosition < FStream.Size) then begin
      FStream.ClearRead;
      Continue;
    end else
      Break;
  end;
  
  if Assigned(FStream) then   
    FStream.Clear;
end;

function TIocpConnection.PopMem: Pointer;
begin
  Result := TIocpTcpCodecServer(Owner).PopMem;
end;

procedure TIocpConnection.PushMem(const V: Pointer);
begin
  TIocpTcpCodecServer(Owner).PushMem(V);
end;

{ TIocpTcpCodecServer }

constructor TIocpTcpCodecServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIocpStreamPool := TBaseQueue.Create;
  FContextClass := TIocpConnection;
  FMemPool := TIocpMemPool.Create(RecvBufferSize, 4096);
end;

destructor TIocpTcpCodecServer.Destroy;
begin
  try
    FOnDecodeData := nil;
    inherited Destroy;
  except end;
  if Assigned(FIocpStreamPool) then begin
    FIocpStreamPool.FreeDataObject;
    FIocpStreamPool.Clear;
    FIocpStreamPool.Free;
  end;
  FreeAndNil(FMemPool);
end;

function TIocpTcpCodecServer.DoDecodeData(Connection: TIocpConnection; Stream: TIocpStream; var Request: TObject): Boolean;
begin
  if Assigned(FOnDecodeData) then
    Result := FOnDecodeData(Connection, Stream, Request)
  else
    Result := False;
end;

procedure TIocpTcpCodecServer.DoRecvExecute(const AConnection: TIocpConnection; var RequestObj: TObject);
begin
  if Assigned(FOnRecvExecute) and Assigned(RequestObj) then begin
    try
      {$IFDEF DEBUG_ON}
      DoStateMsgD(Self, 'DoRecvExecute: ' + RequestObj.ClassName);
      {$ENDIF}
      FOnRecvExecute(Self, AConnection, RequestObj);
    except
      DoStateMsgE(Self, 'TcpServerEx.DoRecvExecute: %s', Exception(ExceptObject));
    end;
  end;
  FreeAndNil(RequestObj);
end;

procedure TIocpTcpCodecServer.FreeStream(V: TIocpStream);
begin
  if not Assigned(V) then Exit;
  V.Clear;
  FIocpStreamPool.EnQueue(V);
end;

function TIocpTcpCodecServer.GetStream: TIocpStream;
begin
  Result := TIocpStream(FIocpStreamPool.DeQueue);
  if not Assigned(Result) then
    Result := TIocpStream.Create()
end;

function TIocpTcpCodecServer.PopMem: Pointer;
begin
  Result := FMemPool.Pop;
end;

procedure TIocpTcpCodecServer.PushMem(const V: Pointer);
begin
  FMemPool.Push(V);
end;

{ TIocpTcpSocketPool }

procedure TIocpTcpSocketPool.Clear;
begin
  FLocker.Enter;
  FList.Clear;
  FLocker.Leave;
end;

constructor TIocpTcpSocketPool.Create(const AMax: Integer);
begin
  FList := TStringHash.Create();
  FList.OnFreeItem := DoFreeItem;
  FLocker := TCriticalSection.Create;
  FMax := AMax;
end;

destructor TIocpTcpSocketPool.Destroy;
begin
  FDestroying := True;
  FreeAndNil(FList);
  FreeAndNil(FLocker);
  inherited;
end;

procedure TIocpTcpSocketPool.DoFreeItem(Item: PHashItem);
var
  Que: TBaseQueue;
begin
  Que := TBaseQueue(Pointer(Item.Value)); 
  if Assigned(Que) then begin
    Que.FreeDataObject;
    FreeAndNil(Que);
    Item.Value := 0;  
  end;
end;

function TIocpTcpSocketPool.GetKey(const RemoteAddr: string; RemotePort: Word): string;
begin
  Result := Format('%.4x.%s', [RemotePort, RemoteAddr])
end;

function TIocpTcpSocketPool.GetQueues(const RemoteAddr: string; RemotePort: Word): TBaseQueue;
begin
  if (Length(RemoteAddr) > 0) and (RemotePort > 0) then
    Result := GetQueues(GetKey(RemoteAddr, RemotePort))
  else
    Result := nil;
end;

function TIocpTcpSocketPool.GetQueues(const Key: string): TBaseQueue;
var
  V: Integer;
begin
  Result := nil;
  if Length(Key) > 0 then begin
    FLocker.Enter;
    V := FList.ValueOf(Key);
    if V > 1 then     
      Result := TBaseQueue(V);
    if not Assigned(Result) then begin
      Result := TBaseQueue.Create;
      FList.Add(Key, Integer(Result));     
    end;
    FLocker.Leave;
  end;
end;

function TIocpTcpSocketPool.Pop(const RemoteAddr: string; RemotePort: Word): TIocpTcpSocket;
var
  Que: TBaseQueue;
begin
  Que := GetQueues(RemoteAddr, RemotePort);
  if Assigned(Que) then begin
    Result := TIocpTcpSocket(Que.DeQueue);
    if not Assigned(Result) then begin
      Result := TIocpTcpSocket.Create(nil);
      Result.RemoteHost := RemoteAddr;
      Result.RemotePort := RemotePort;
      Result.ConnectTimeOut := 3000;
    end;
  end else 
    Result := TIocpTcpSocket.Create(nil);
end;

procedure TIocpTcpSocketPool.Push(const Socket: TIocpTcpSocket);
var
  Que: TBaseQueue;
begin
  if (not Assigned(Socket)) or (not Assigned(Self)) then Exit;
  Que := GetQueues(Socket.RemoteHost, Socket.RemotePort);
  if Assigned(Que) and ((FMax < 1) or (Que.Size < FMax)) then begin
    Que.EnQueue(Socket);
  end else
    Socket.Free;
end;

{ TIocpTcpClientProxy }

const
  IPIndexHeader = 'ipi_';
  IPLastHeader = 'ipl_';
  IPLastTimeHeader = 'ipt_';
  IPChangeOK = 9999999;

procedure TIocpTcpClientProxy.AddItem(const Socket: TIocpTcpSocket;
  const CallBack: TIocpTcpProxyCallBack; const Tag: Integer);
var
  Item: PIocpTcpProxyData;
  Time: Int64;
begin
  Time := GetTimestamp;
  FLocker.Enter;
  Item := FMemPool.Pop;
  Item.Socket := Socket;
  Item.Tag := Tag;
  Item.StartTime := Time;
  Item.CallBack := CallBack;
  Item.Next := nil;
  if FLast = nil then begin
    FList := Item;
    Item.Prev := nil;
  end else begin
    Item.Prev := FLast;
    FLast.Next := Item;
  end;
  FLast := Item;
  FLocker.Leave; 
end;

function TIocpTcpClientProxy.HostChange(const Host: string): Boolean;
var
  I: Integer;
  T: Cardinal;
  List: TStrings;
begin
  Result := False;
  List := TStringList.Create;
  try
    List.Delimiter := ',';
    List.DelimitedText := Host;
    if List.Count < 2 then Exit;
    T := FHostSelect.ValueOf(IPLastTimeHeader + Host);
    if Abs(GetTickCount - T) > 300000 then begin   // 30秒内没有更改过地址，则从第一个开始
      I := 0;
    end else begin
      I := FHostSelect.ValueOf(IPLastHeader + Host);
      if I < 0 then I := 0;
    end;

    // 在其它线程已经切换成功了
    if I = IPChangeOK then begin
      Result := True;
      Exit;
    end;
    
    if I >= List.Count then Exit;
    Inc(I);
    FHostSelect.Add(IPLastTimeHeader + Host, GetTickCount);
    FHostSelect.Add(IPLastHeader + Host, I);

    I := FHostSelect.ValueOf(IPIndexHeader + Host) + 1;
    if (I < 0) or (I >= List.Count) then
      I := 0;
    FHostSelect.Add(Host, ipToInt(List[I]));
    FHostSelect.Add(IPIndexHeader + Host, I);
    Result := True;
  finally
    List.Free;
  end;
end;

function TIocpTcpClientProxy.HostSelect(const Host: string): string;
var
  I: Cardinal;
  L: Integer;
begin
  // 从多个IP中选中一个可用的
  if Length(Host) = 0 then
    Result := ''
  else begin
    I := FHostSelect.ValueOf(Host);
    if I <> 0 then
      Result := IPToStr(I)
    else begin
      L := Pos(',', Host);
      if L = 0 then
        Result := Host
      else
        Result := Copy(Host, 1, L - 1);
      FHostSelect.Add(Host, ipToInt(Result));
    end;
  end;
end;

procedure TIocpTcpClientProxy.HostUpdate(const Host: string);
begin
  FHostSelect.Add(IPLastHeader + Host, IPChangeOK);
end;

procedure TIocpTcpClientProxy.HostUpdate(const Host, IPAddr: string);
begin
  FHostSelect.Add(Host, ipToInt(IPAddr));
  HostUpdate(Host);
end;

function TIocpTcpClientProxy.CheckHost(const Host: string;
  const Port: Word): Boolean;
begin
  Result := (Port > 0) and (Length(Host) > 0);
end;

procedure TIocpTcpClientProxy.Clear;
var
  P, P1: PIocpTcpProxyData;
begin
  FLocker.Enter;
  P := FList;
  while P <> nil do begin
    P1 := P;
    P := P.Next;
    if Assigned(P1.CallBack) then begin
      try
        P1.CallBack(P1.Socket, P1.Tag);
      except
        DoStateMsg(Self, iocp_mt_Error, Exception(ExceptObject).Message);
      end;
    end;
    ReleaseSocket(P1.Socket);
    FMemPool.Push(P1);
  end;
  FLocker.Leave;
end;

constructor TIocpTcpClientProxy.Create(AMaxWorker: Integer);
var
  I: Integer;
begin
  FTimeOut := 30000;
  FHostSelect := TStringHash.Create();
  FPool := TIocpTcpSocketPool.Create(64);
  FMemPool := TMemPool.Create(SizeOf(TIocpTcpProxyData));
  FLocker := TCriticalSection.Create;
  if AMaxWorker < 1 then AMaxWorker := 1;
  if AMaxWorker > 64 then AMaxWorker := 64;  
  SetLength(FWorker, AMaxWorker);
  for I := 0 to AMaxWorker - 1 do begin
    FWorker[I] := TIocpTcpProxyWorkerThread.Create(True);
    FWorker[I].FOwner := Self;
    {$IFDEF UNICODE}
    FWorker[I].Start;
    {$ELSE}
    FWorker[I].Resume;
    {$ENDIF}
  end;
end;

destructor TIocpTcpClientProxy.Destroy;
var
  I: Integer;
begin
  FIsDestroying := True;
  try
    Clear;
  finally
    FreeAndNil(FPool);
    FreeAndNil(FMemPool);
    FreeAndNil(FLocker);
    FreeAndNil(FHostSelect);
    for I := 0 to High(FWorker) do
      FreeAndNil(FWorker[I]);
    inherited Destroy;
  end;
end;

function TIocpTcpClientProxy.DoCallBack(Socket: TIocpTcpSocket;
  CallBack: TIocpTcpProxyCallBack; const Tag: Integer): Boolean;
begin
  try
    if Assigned(CallBack) then begin
      CallBack(Socket, Tag);
    end else
      Socket.Disconnect;
    Result := True;
  except
    Result := False;
    DoStateMsg(Self, iocp_mt_Error, Exception(ExceptObject).Message);
  end;
end;

procedure TIocpTcpClientProxy.DoStateMsg(Sender: TObject;
  MsgType: TIocpStateMsgType; const Msg: string);
begin
  if Assigned(FOnStateMsg) then
    FOnStateMsg(Sender, MsgType, Msg);
end;

procedure TIocpTcpClientProxy.DoWorker();
var
  CallBack: TIocpTcpProxyCallBack;
  Item: PIocpTcpProxyData;
  IsFree: Boolean;
  IsBreak: Boolean;
  Time: Int64;
  List: TList;
  I: Integer;
begin
  if ItemEmpty then Exit;  
  List := TList.Create;
  try
    while not IsDestroying do begin
      Time := GetTimestamp;
      Item := PopItem;
      CallBack := nil;
      if Item <> nil then begin
        IsBreak := Item.Next = nil;
        IsFree := False;
        if Assigned(Item.Socket) then begin
          if not Item.Socket.RecvBufferIsEmpty then begin // 完成数据接收
            CallBack := Item.CallBack;
            Item.CallBack := nil;
            DoCallBack(Item.Socket, CallBack, Item.Tag);
            IsFree := True;
          end else if not Item.Socket.Active then  // 连接断开
            IsFree := True
          else if (FTimeOut > 0) and (Time - Item.StartTime > FTimeOut) then begin // 超时
            IsFree := True;
            Item.IsTimeOut := True;
            Item.Socket.Disconnect;
          end;
        end else
          IsFree := True;
        if IsFree then begin  // 超时、断开、完成时释放连接
          try
            if Assigned(Item.CallBack) then begin
              try  // 如果还未调用过CallBack则触发
                Item.CallBack(Item.Socket, Item.Tag);
              except
                DoStateMsg(Self, iocp_mt_Error, Exception(ExceptObject).Message);
              end;
            end;
          finally
            ReleaseSocket(Item.Socket);
            Item.Socket := nil;
            Item.CallBack := nil;
            FLocker.Enter;
            FMemPool.Push(Item);
            FLocker.Leave;
          end;
        end else
          List.Add(Item);
        if IsBreak then
          Break;
      end else
        Break;
      ThreadYield;
    end;
  finally
    ThreadYield;
    for I := 0 to List.Count - 1 do
      PushItem(List[I]);
    FreeAndNil(List);
  end;
end;

function TIocpTcpClientProxy.GetIsDestroying: Boolean;
begin
  Result := (not Assigned(Self)) or FIsDestroying;
end;

function TIocpTcpClientProxy.GetSocket(const Host: string;
  const Port: Word): TIocpTcpSocket;
begin
  if CheckHost(Host, Port) then begin
    Result := FPool.Pop(HostSelect(Host), Port);
    if Assigned(Result) then begin
      if not Result.Connect(False) then begin
        // 连接失败，自动切换IP线路
        while HostChange(Host) do begin
          Result := FPool.Pop(HostSelect(Host), Port);
          if Assigned(Result) then begin
            if Result.Connect(False) then begin
              HostUpdate(Host);
              DoStateMsg(Self, iocp_mt_Debug, Format('切换服务器：%s, %s',
                [Host, Result.RemoteHost]));
              Break;
            end else begin
              FPool.Push(Result);
              Result := nil;
            end;
          end else
            Break;
        end;
      end;
    end;
  end else
    Result := nil;
end;

function TIocpTcpClientProxy.ItemEmpty: Boolean;
begin
  FLocker.Enter;
  Result := FList = nil;
  FLocker.Leave;
end;

function TIocpTcpClientProxy.Pop(const RemoteAddr: string;
  RemotePort: Word): TIocpTcpSocket;
begin
  Result := FPool.Pop(RemoteAddr, RemotePort);
end;

function TIocpTcpClientProxy.PopItem: PIocpTcpProxyData;
begin
  FLocker.Enter;
  Result := FList;
  if Result <> nil then begin
    FList := FList.Next;
    if FList <> nil then    
      FList.Prev := nil;
    if FLast = Result then
      FLast := nil;
  end else if FLast <> nil then
    FLast := nil;
  FLocker.Leave;
end;

procedure TIocpTcpClientProxy.Push(const Socket: TIocpTcpSocket);
begin
  FPool.Push(Socket);
end;

procedure TIocpTcpClientProxy.PushItem(const V: PIocpTcpProxyData);
begin
  if V = nil then Exit;
  FLocker.Enter;
  V.Next := nil;
  if FLast = nil then begin
    FList := V;
    V.Prev := nil;
  end else begin
    V.Prev := FLast;
    FLast.Next := V;
  end;
  FLast := V;
  FLocker.Leave;
end;

procedure TIocpTcpClientProxy.ReleaseSocket(V: TIocpTcpSocket);
begin
  FPool.Push(V);
end;

function TIocpTcpClientProxy.Send(const Host: string; Port: Word; S: TStream;
  CallBack: TIocpTcpProxyCallBack; Sync: Boolean; Tag: Integer): Boolean;
var
  Socket: TIocpTcpSocket;
begin
  Result := False;
  if (not Assigned(S)) or (S.Size - S.Position = 0) then Exit;  
  Socket := GetSocket(Host, Port);
  if Assigned(Socket) then begin
    try
      try
        Socket.Send(S);
      except
        if not Socket.Active then begin
          Socket.Active := True;
          Socket.Send(S);
        end else
          Exit;
      end;
      if Sync then begin
        DoCallBack(Socket, CallBack, Tag);
      end else begin
        AddItem(Socket, CallBack, Tag);
      end;
      Result := True;
    finally
      if (not Result) or Sync then
        ReleaseSocket(Socket);
    end;
  end;
end;

function TIocpTcpClientProxy.Send(const Host: string; Port: Word;
  const Data: WideString; CallBack: TIocpTcpProxyCallBack;
  Sync: Boolean; Tag: Integer): Boolean;
var
  Socket: TIocpTcpSocket;
begin
  Result := False;
  if Length(Data) = 0 then Exit;
  Socket := GetSocket(Host, Port);
  if Assigned(Socket) then begin
    try
      try
        Socket.Send(Data);
      except
        if not Socket.Active then begin
          Socket.Active := True;
          Socket.Send(Data);
        end else
          Exit;
      end;
      if Sync then begin
        DoCallBack(Socket, CallBack, Tag);
      end else begin
        AddItem(Socket, CallBack, Tag);
      end;
      Result := True;
    finally
      if (not Result) or Sync then
        ReleaseSocket(Socket);
    end;
  end;  
end;

function TIocpTcpClientProxy.Send(const Host: string; Port: Word;
  const Data: AnsiString; CallBack: TIocpTcpProxyCallBack;
  Sync: Boolean; Tag: Integer): Boolean;
var
  Socket: TIocpTcpSocket;
begin
  Result := False;
  if Length(Data) = 0 then Exit;  
  Socket := GetSocket(Host, Port);
  if Assigned(Socket) then begin
    try
      try
        Socket.Send(Data);
      except
        if not Socket.Active then begin
          Socket.Active := True;
          Socket.Send(Data);
        end else begin
          Socket.Disconnect;
          Exit;
        end;
      end;
      if Sync then begin
        DoCallBack(Socket, CallBack, Tag);
      end else begin
        AddItem(Socket, CallBack, Tag);
      end;
      Result := True;
    finally
      if (not Result) or Sync then
        ReleaseSocket(Socket);
    end;
  end;
end;

function TIocpTcpClientProxy.Send(const Host: string; Port: Word;
  Data: PAnsiChar; Len: Cardinal; CallBack: TIocpTcpProxyCallBack;
  Sync: Boolean; Tag: Integer): Boolean;
var
  Socket: TIocpTcpSocket;
begin
  Result := False;
  if (Data = nil) or (Len = 0) then Exit;
  Socket := GetSocket(Host, Port);
  if Assigned(Socket) then begin
    try
      try
        Socket.Send(Data, Len);
      except
        if not Socket.Active then begin
          Socket.Active := True;
          Socket.Send(Data, Len);
        end else
          Exit;
      end;
      if Sync then begin
        DoCallBack(Socket, CallBack, Tag);
      end else begin
        AddItem(Socket, CallBack, Tag);
      end;
      Result := True;
    finally
      if (not Result) or Sync then   
        ReleaseSocket(Socket);
    end;
  end;
end;

{ TIocpTcpProxyWorkerThread }

procedure TIocpTcpProxyWorkerThread.Execute;
begin
  while not Terminated do begin
    if Assigned(FOwner) and (not FOwner.IsDestroying) then begin
      try
        FOwner.DoWorker();
      except
        FOwner.DoStateMsg(Self, iocp_mt_Error, Exception(ExceptObject).Message);
      end;
      Sleep(100);
    end;
  end;
end;

initialization
  Workers := TIocpTask.GetInstance;
  SMemPool := TIocpMemPool.Create(8, 2048);

finalization
  Workers := nil;
  FreeAndNil(SMemPool);

end.


