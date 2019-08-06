{*******************************************************}
{                                                       }
{       IOCP 基础Socket （基于DIOCP修改）               }
{                                                       }
{       版权所有 (C) 2015 YangYxd                       }
{                                                       }
{*******************************************************}

unit iocp.RawSockets;

{$I 'iocp.inc'}

interface

uses
  iocp.Res, WinSock, iocp.Winapi.WinSock, iocp.Sockets.Utils,
  Windows, SysUtils;

const
  SIO_KEEPALIVE_VALS = IOC_IN or IOC_VENDOR or 4;

  { Other NT-specific options. }

  {$EXTERNALSYM SO_MAXDG}
  SO_MAXDG        = $7009;
  {$EXTERNALSYM SO_MAXPATHDG}
  SO_MAXPATHDG    = $700A;
  {$EXTERNALSYM SO_UPDATE_ACCEPT_CONTEXT}
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  {$EXTERNALSYM SO_CONNECT_TIME}
  SO_CONNECT_TIME = $700C;

  IP_V4 = 0;
  IP_V6 = 1;

const
  IP_HDRINCL = 2;

const
  {$IFNDEF DOTNET}
  {$IFDEF USE_VCL_POSIX}
  DIOCP_PF_INET4 = AF_INET;
  DIOCP_PF_INET6 = AF_INET6;
  {$ELSE}
  DIOCP_PF_INET4 = PF_INET;
  DIOCP_PF_INET6 = PF_INET6;
  {$ENDIF}
  {$ELSE}
  DIOCP_PF_INET4 = ProtocolFamily.InterNetwork;
  DIOCP_PF_INET6 = ProtocolFamily.InterNetworkV6;
  {$ENDIF}

  IPV6_SOCKADDR_SIZE = SizeOf(TSockAddrIn6);

type
  TKeepAlive = record
    OnOff: Integer;
    KeepAliveTime: Integer;
    KeepAliveInterval: Integer;
  end;
  TTCP_KEEPALIVE = TKeepAlive;
  PTCP_KEEPALIVE = ^TKeepAlive;

type
  /// <summary>
  /// raw socket object.
  /// From: 天地弦, Thansk: ryan
  /// Amend: YangYxd
  /// </summary>
  TRawSocket = class(TObject)
  private
    FSocketHandle: TSocket;
    FProtocol: Integer;
    FSockType: Integer;
    FIPVersion: Integer;
    function IsConnected: Boolean;
    function GetActive: Boolean;
    function GetIsRaw: Boolean;
    function GetIsTCP: Boolean; 
    function GetIsUDP: Boolean;
    function GetIsInvalid: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    procedure CreateSocket(ASockType: Integer; Protocol: Integer; IsOverlapped: Boolean);
    procedure CheckDestroyHandle;

    procedure CreateRawSocket(IsOverlapped: Boolean = False); inline;
    procedure CreateTcpSocket(IsOverlapped: Boolean = False); inline;
    procedure CreateUdpSocket(IsOverlapped: Boolean = False); inline;

    function Bind(const pvAddr: AnsiString; pvPort: Word): Boolean; overload;
    function Bind(var pvAddr: TSockAddr): Boolean; overload;
    /// <summary>
    /// 开始监听，backlog 为后备日志数量，默认为5
    /// </summary>
    function Listen(const backlog: Integer = SOMAXCONN): Boolean;

    /// <summary>
    /// 域名解析到IP地址
    /// </summary>
    class function DomainNameToAddr(const host: AnsiString): AnsiString;

    function RecvBuf(var data; const len: Integer): Integer;
    function SendBuf(const data; const len: Integer): Integer;

    function Connect(const pvAddr: AnsiString; pvPort: Word; pvTimeOut: Integer = -1): Boolean; overload;
    function Connect(sockaddr: TSockAddrIn; pvTimeOut: Integer = -1): Boolean; overload;

    /// <summary>
    /// 没有状况发生返回0, 有错误返回 SOCKET_ERROR, 有事做了返回大于0的数
    /// </summary>
    function SelectSocket(vReadReady, vWriteReady, vExceptFlag: PBoolean;
        pvTimeOut: Integer = 0): Integer;

    /// <summary>
    /// 设置收数据超时值
    /// </summary>
    function SetReadTimeOut(const pvTimeOut: Cardinal): Integer;

    /// <summary>
    /// 设置发数据超时值
    /// </summary>
    function SetSendTimeOut(const pvTimeOut: Cardinal): Integer;

    /// <summary>
    /// 取消当前 IO 操作
    /// </summary>
    function Cancel: Boolean;

    /// <summary>
    /// 禁止Socket上的接收或发送操作，默认同时禁止发送和接收 
    /// </summary>
    function ShutDown(pvHow: Integer = SD_BOTH): Integer;

    /// <summary>
    /// 默认 5000 ms 检查一次存活状态
    /// </summary>
    function SetKeepAliveOption(pvKeepAliveTime: Integer = 5000): Boolean;

    /// <summary>
    /// call in Listen RawSocket instance
    /// </summary>
    function UpdateAcceptContext(pvSocket: TSocket): Boolean;

    function SetNoDelayOption(pvOption:Boolean): Boolean;
    property SocketHandle: TSocket read FSocketHandle;
    property Connected: Boolean read IsConnected;
    property Active: Boolean read GetActive;
    property IsRAW: Boolean read GetIsRaw;
    property IsTCP: Boolean read GetIsTCP;
    property IsUDP: Boolean read GetIsUDP;
    property IsInvalid: Boolean read GetIsInvalid;
    property Protocol: Integer read FProtocol;
    property SockType: Integer read FSockType;
    property IPVersion: Integer read FIPVersion write FIPVersion;
  end;

var
  _DebugWSACreateCounter: Integer;
  _DebugWSACloseCounter: Integer;

implementation

{ TRawSocket }

function TRawSocket.Bind(const pvAddr: AnsiString; pvPort: Word): Boolean;
var
  sockaddr: array[0..127] of byte;
  lvSize: Integer;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);
  if FIPVersion = IP_V4 then begin
    PSockAddrIn(@sockaddr[0])^ := GetSocketAddr(AnsiString(pvAddr), pvPort);
    lvSize := SizeOf(TSockAddrIn);
  end else begin
    PSockAddrIn6(@sockaddr[0])^.sin6_family := DIOCP_PF_INET6;
    PSockAddrIn6(@sockaddr[0])^.sin6_port := htons(pvPort);
    lvSize := SizeOf(TSockAddrIn6);
  end;
  Result := iocp.Winapi.WinSock.Bind(FSocketHandle, PSockAddr(@sockaddr[0]), lvSize) = 0;
end;

function TRawSocket.Bind(var pvAddr: TSockAddr): Boolean;
begin
  Result := iocp.Winapi.WinSock.Bind(FSocketHandle, @pvAddr, SizeOf(pvAddr)) = 0;
end;

function TRawSocket.Cancel: Boolean;
begin
  Result := Windows.CancelIo(FSocketHandle);
end;

procedure TRawSocket.CheckDestroyHandle;
var
  lvTempSocket: TSocket;
begin
  lvTempSocket := FSocketHandle;
  if (lvTempSocket <> 0) and (lvTempSocket <> INVALID_SOCKET) then begin
    FSocketHandle := INVALID_SOCKET;
    Closesocket(lvTempSocket);
    InterlockedIncrement(_DebugWSACloseCounter);
  end;
end;

procedure TRawSocket.Close;
var
  lvTempSocket: TSocket;
begin
  if not Assigned(Self) then Exit;  
  lvTempSocket := FSocketHandle;
  if lvTempSocket <> INVALID_SOCKET then begin
    FSocketHandle := INVALID_SOCKET;
    iocp.Winapi.WinSock.shutdown(lvTempSocket, SD_BOTH);
    closesocket(lvTempSocket);
    InterlockedIncrement(_DebugWSACloseCounter);
  end;
end;

function TRawSocket.Connect(sockaddr: TSockAddrIn; pvTimeOut: Integer): Boolean;
var
  lvFlags: Cardinal;
  lvRet: Integer;
  fs: TFDset;
  tv: timeval;
  Timeptr: PTimeval;
begin
  if pvTimeOut <= 0 then begin
    Result := iocp.Winapi.WinSock.Connect(FSocketHandle, TSockAddr(sockaddr),
      sizeof(sockaddr)) = 0;

  end else begin
    lvFlags := 1;  // 非阻塞模式
    ioctlsocket(FSocketHandle, Integer(FIONBIO), lvFlags);
    lvRet := iocp.Winapi.WinSock.Connect(FSocketHandle, TSockAddr(sockaddr), sizeof(TSockAddrIn));
    if lvRet = 0 then begin // 连接成功
      lvFlags := 0;  // 非阻塞模式
      ioctlsocket(FSocketHandle, Integer(FIONBIO), lvFlags);
      Result := True;
    end else begin
      FD_ZERO(fs);
      _FD_SET(FSocketHandle, fs);

      tv.tv_sec := pvTimeOut div 1000;
      tv.tv_usec :=  1000 * (pvTimeOut mod 1000);
      Timeptr := @tv;

      lvRet := iocp.Winapi.WinSock.select(FSocketHandle, nil, @fs, nil, Timeptr);

      if lvRet <= 0 then begin
        Result := False;  //连接超时
        //lvErr := WSAGetLastError;
        CloseSocket(FSocketHandle);
        FSocketHandle := INVALID_SOCKET;
        //Result := lvErr = 0;
      end else begin
        lvFlags := 0;  // 非阻塞模式
        ioctlsocket(FSocketHandle, Integer(FIONBIO), lvFlags);
        Result := True;
      end;
    end;
  end;
end;

function TRawSocket.Connect(const pvAddr: AnsiString; pvPort: Word; pvTimeOut: Integer): Boolean;
begin
  Result := Connect(GetSocketAddr(AnsiString(pvAddr), pvPort), pvTimeOut);
end;

constructor TRawSocket.Create;
begin
  FSocketHandle := INVALID_SOCKET;
  FIPVersion := IP_V4;
end;

procedure TRawSocket.CreateRawSocket(IsOverlapped: Boolean);
begin
  CreateSocket(SOCK_RAW, IPPROTO_RAW, IsOverlapped);
end;

procedure TRawSocket.CreateSocket(ASockType, Protocol: Integer;
  IsOverlapped: Boolean);
begin
  CheckDestroyHandle();
  if IsOverlapped then begin
    if FIPVersion = IP_V6 then begin
      {$IFDEF UNICODE}
      FSocketHandle := WSASocketW(AF_INET6, ASockType, Protocol, Nil, 0, WSA_FLAG_OVERLAPPED);
      {$ELSE}
      FSocketHandle := WSASocketA(AF_INET6, ASockType, Protocol, Nil, 0, WSA_FLAG_OVERLAPPED);
      {$ENDIF}
    end else
      FSocketHandle := WSASocket(AF_INET, ASockType, Protocol, nil, 0, WSA_FLAG_OVERLAPPED)
  end else
    FSocketHandle := socket(AF_INET, ASockType, Protocol);
  if (FSocketHandle = 0) or (FSocketHandle = INVALID_SOCKET) then
    RaiseLastOSError;
  FProtocol := Protocol;
  FSockType := ASockType;
  InterlockedIncrement(_DebugWSACreateCounter);
end;

procedure TRawSocket.CreateTcpSocket(IsOverlapped: Boolean);
begin
  CreateSocket(SOCK_STREAM, IPPROTO_TCP, IsOverlapped);
end;

procedure TRawSocket.CreateUdpSocket(IsOverlapped: Boolean);
begin
  CreateSocket(SOCK_DGRAM, IPPROTO_IP, IsOverlapped);
end;

destructor TRawSocket.Destroy;
begin
  Close();
  inherited;
end;

class function TRawSocket.DomainNameToAddr(const host: AnsiString): AnsiString;
var
  lvhostInfo: PHostEnt;
begin
  lvhostInfo := gethostbyname(PAnsiChar(host));
  if lvhostInfo = nil then
    RaiseLastOSError;
  Result := inet_ntoa(PInAddr(lvhostInfo^.h_addr_list^)^);
end;

function TRawSocket.GetActive: Boolean;
begin
  Result := FSocketHandle <> INVALID_SOCKET;
end;

function TRawSocket.GetIsInvalid: Boolean;
begin
  Result := (FSocketHandle = INVALID_SOCKET) or (FSocketHandle = 0);
end;

function TRawSocket.GetIsRaw: Boolean;
begin
  Result := (FSockType = SOCK_RAW) and (FProtocol = IPPROTO_RAW);
end;

function TRawSocket.GetIsTCP: Boolean;
begin
  Result := (FSockType = SOCK_STREAM) and (FProtocol = IPPROTO_TCP);
end;

function TRawSocket.GetIsUDP: Boolean;
begin
  Result := (FSockType = SOCK_DGRAM) and (FProtocol = IPPROTO_IP);
end;

function TRawSocket.IsConnected: Boolean;
var
  Buf: array [0..3] of Byte;
begin
  if FSocketHandle <> INVALID_SOCKET then begin
    if IsTCP then begin
      if Recv(FSocketHandle, Buf, 1, MSG_PEEK) <= 0 then
        Result := False
      else
        Result := WSAECONNRESET <> WSAGetLastError();
    end else
      Result := True;
  end else
    Result := False;
end;

function TRawSocket.Listen(const backlog: Integer): Boolean;
begin
  Result := iocp.Winapi.WinSock.Listen(FSocketHandle, backlog) = 0;
end;

function TRawSocket.RecvBuf(var data; const len: Integer): Integer;
begin
  Result := iocp.Winapi.WinSock.recv(FSocketHandle, data, len, 0);
end; 

function TRawSocket.SelectSocket(vReadReady, vWriteReady, vExceptFlag: PBoolean;
  pvTimeOut: Integer): Integer;
var
  ReadFds: TFDset;
  ReadFdsptr: PFDset;
  WriteFds: TFDset;
  WriteFdsptr: PFDset;
  ExceptFds: TFDset;
  ExceptFdsptr: PFDset;
  tv: timeval;
  Timeptr: PTimeval;
begin
  if Assigned(vReadReady) then begin
    ReadFdsptr := @ReadFds;
    FD_ZERO(ReadFds);
    _FD_SET(FSocketHandle, ReadFds);
  end else
    ReadFdsptr := nil;
  if Assigned(vWriteReady) then begin
    WriteFdsptr := @WriteFds;
    FD_ZERO(WriteFds);
    _FD_SET(FSocketHandle, WriteFds);
  end else
    WriteFdsptr := nil;
  if Assigned(vExceptFlag) then begin
    ExceptFdsptr := @ExceptFds;
    FD_ZERO(ExceptFds);
    _FD_SET(FSocketHandle, ExceptFds);
  end else
    ExceptFdsptr := nil;
  if pvTimeOut >= 0 then begin
    tv.tv_sec := pvTimeOut div 1000;
    tv.tv_usec :=  1000 * (pvTimeOut mod 1000);
    Timeptr := @tv;
  end else
    Timeptr := nil;

  //The select function determines the status of one or more sockets, waiting if necessary,
  //to perform synchronous I/O.
  //  The select function returns the total number of socket handles that are ready
  //  and contained in the fd_set structures,
  //  zero if the time limit expired, or SOCKET_ERROR if an error occurred.
  //  If the return value is SOCKET_ERROR,
  //  WSAGetLastError can be used to retrieve a specific error code.

  // nfds：是一个整数值，是指集合中所有文件描述符的范围，即所有文件描述符的
  // 最大值加1，不能错！在Windows中这个参数的值无所谓，可以设置不正确。
  Result := iocp.Winapi.WinSock.select(0, ReadFdsptr, WriteFdsptr, ExceptFdsptr, Timeptr);

  if Assigned(vReadReady) then
    vReadReady^ := FD_ISSET(FSocketHandle, ReadFds);
  if Assigned(vWriteReady) then
    vWriteReady^ := FD_ISSET(FSocketHandle, WriteFds);
  if Assigned(vExceptFlag) then
    vExceptFlag^ := FD_ISSET(FSocketHandle, ExceptFds);
end;

function TRawSocket.SendBuf(const data; const len: Integer): Integer;
begin
  Result := iocp.winapi.winsock.Send(FSocketHandle, data, len, 0);
end;

function TRawSocket.SetKeepAliveOption(pvKeepAliveTime: Integer): Boolean;
var
  Opt: integer;
  outByte: DWORD;
  inKeepAlive, outKeepAlive: TTCP_KEEPALIVE;
begin
  Result := false;
  Opt := 1;
  if SetSockopt(FSocketHandle, SOL_SOCKET, SO_KEEPALIVE, @Opt, sizeof(Opt)) = SOCKET_ERROR then
    exit;

  inKeepAlive.OnOff := 1;
  inKeepAlive.KeepAliveTime := pvKeepAliveTime;
  inKeepAlive.KeepAliveInterval := 1;

  if WSAIoctl(FSocketHandle,
     SIO_KEEPALIVE_VALS,
     @inKeepAlive, sizeof(TTCP_KEEPALIVE),
     @outKeepAlive,
    sizeof(TTCP_KEEPALIVE), outByte, nil, nil) <> SOCKET_ERROR then
  begin
    Result := true;
  end;
end; 

function TRawSocket.SetNoDelayOption(pvOption: Boolean): Boolean;
var
  bNoDelay: BOOL;
begin
  bNoDelay := pvOption;
  Result := setsockopt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, @bNoDelay, SizeOf(bNoDelay)) <> SOCKET_ERROR;
end;

function TRawSocket.SetReadTimeOut(const pvTimeOut: Cardinal): Integer;
begin
  Result := setsockopt(FSocketHandle, SOL_SOCKET, SO_RCVTIMEO,
    PAnsiChar(@pvTimeOut), SizeOf(Cardinal));
end;

function TRawSocket.SetSendTimeOut(const pvTimeOut: Cardinal): Integer;
begin
  Result := setsockopt(FSocketHandle, SOL_SOCKET, SO_SNDTIMEO,
    PAnsiChar(@pvTimeOut), SizeOf(Cardinal));
end;

function TRawSocket.ShutDown(pvHow: Integer): Integer;
begin
  Result := iocp.winapi.winsock.shutdown(FSocketHandle, pvHow);
end;

function TRawSocket.UpdateAcceptContext(pvSocket: TSocket): Boolean;
begin
  Result := setsockopt(pvSocket, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT,
    PAnsiChar(@FSocketHandle), SizeOf(TSocket)) <> SOCKET_ERROR;
end;

end.


