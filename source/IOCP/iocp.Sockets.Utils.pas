{*******************************************************}
{                                                       }
{       IOCP Sockets 公用函数等                         }
{                                                       }
{       版权所有 (C) 2015 YangYxd                       }
{                                                       }
{*******************************************************}

unit iocp.Sockets.Utils;

interface

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
{$DEFINE ANSISTRINGS}
{$IFEND}

uses
  {$IFDEF ANSISTRINGS}AnsiStrings, {$ENDIF}
  Windows, SysUtils, iocp.winapi.WinSock;


{$if CompilerVersion < 23}
type
   NativeUInt = Cardinal;
   IntPtr = Cardinal;
{$ifend}

// before delphi 2007
{$if CompilerVersion < 18}
type
     ULONG_PTR = Cardinal;
{$ifend}

type
  TSocketState = (ssDisconnected, ssConnected, ssConnecting, ssListening, ssAccepting);

  TIocpAcceptEx = function(sListenSocket, sAcceptSocket: TSocket; lpOutputBuffer:
      Pointer; dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength:
      DWORD; var lpdwBytesReceived: DWORD; lpOverlapped: POverlapped): BOOL; stdcall;

  TIocpConnectEx = function(const s : TSocket; const name: PSOCKADDR; const
      namelen: Integer; lpSendBuffer : Pointer; dwSendDataLength : DWORD; var
      lpdwBytesSent : DWORD; lpOverlapped : LPWSAOVERLAPPED): BOOL; stdcall;


  //  Extention function "GetAcceptExSockAddrs"
  TIocpGetAcceptExSockAddrs = procedure(lpOutputBuffer: Pointer;
      dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD;
      var LocalSockaddr: PSockAddr; var LocalSockaddrLength: Integer;
      var RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: Integer); stdcall;

  TIocpDisconnectEx = function(const hSocket : TSocket; lpOverlapped: LPWSAOVERLAPPED;
     const dwFlags : DWORD; const dwReserved : DWORD) : BOOL; stdcall;


const
  IOCP_RESULT_OK = 0;
  IOCP_RESULT_QUIT = 1;


function CreateIoCompletionPort(FileHandle, ExistingCompletionPort: THandle;
  CompletionKey:ULONG_PTR; NumberOfConcurrentThreads: DWORD): THandle; stdcall;
{$EXTERNALSYM CreateIoCompletionPort}

function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred, lpCompletionKey: ULONG_PTR;
  var lpOverlapped: POverlapped; dwMilliseconds: DWORD): BOOL; stdcall;
{$EXTERNALSYM GetQueuedCompletionStatus}

  
var
  IocpAcceptEx:TIocpAcceptEx;
  IocpConnectEx:TIocpConnectEx;
  IocpDisconnectEx:TIocpDisconnectEx;
  IocpGetAcceptExSockaddrs: TIocpGetAcceptExSockAddrs;

function GetSocketAddr(const pvAddr: AnsiString; pvPort: Integer): TSockAddrIn;
function socketBind(s: TSocket; const pvAddr: string; pvPort: Integer): Boolean;


/// <summary>
/// 设置KeepAlive 检测选项
/// </summary>
/// <returns> Boolean
/// </returns>
/// <param name="pvSocket"> (TSocket) </param>
/// <param name="pvKeepAliveTime"> (Integer) </param>
function SetKeepAlive(pvSocket: TSocket; pvKeepAliveTime: Integer = 5000):
    Boolean;

function CreateTcpOverlappedSocket: THandle;

/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean):
    Boolean;
function ipToInt(const strIP : AnsiString): Cardinal;
function IPToStr(const Addr: Cardinal): string;


function PCharToInt64Def(const S: PAnsichar; Len: Integer; def: int64 = 0): int64;
function PCharToIntDef(const S: PAnsichar; Len: Integer; def: Integer = 0): Integer; overload;

function GetFileLastWriteTime(const fName: AnsiString): TDateTime;


implementation

const
  SIO_KEEPALIVE_VALS = IOC_IN or IOC_VENDOR or 4;

const
  Convert: array[0..255] of Integer =
    (
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
     );

{ Other NT-specific options. }

  {$EXTERNALSYM SO_MAXDG}
  SO_MAXDG        = $7009;
  {$EXTERNALSYM SO_MAXPATHDG}
  SO_MAXPATHDG    = $700A;
  {$EXTERNALSYM SO_UPDATE_ACCEPT_CONTEXT}
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  {$EXTERNALSYM SO_CONNECT_TIME}
  SO_CONNECT_TIME = $700C;

type
  TKeepAlive = record
    OnOff: Integer;
    KeepAliveTime: Integer;
    KeepAliveInterval: Integer;
  end;
  TTCP_KEEPALIVE = TKeepAlive;
  PTCP_KEEPALIVE = ^TKeepAlive;


const
  WINSOCK_LIB_VERSION : Word = $0202;

const
  WSAID_GETACCEPTEXSOCKADDRS: TGuid = (D1:$b5367df2;D2:$cbac;D3:$11cf;D4:($95,$ca,$00,$80,$5f,$48,$a1,$92));
  WSAID_ACCEPTEX: TGuid = (D1:$b5367df1;D2:$cbac;D3:$11cf;D4:($95,$ca,$00,$80,$5f,$48,$a1,$92));
  WSAID_CONNECTEX: TGuid = (D1:$25a207b9;D2:$ddf3;D3:$4660;D4:($8e,$e9,$76,$e5,$8c,$74,$06,$3e));
  {$EXTERNALSYM WSAID_DISCONNECTEX}
  WSAID_DISCONNECTEX: TGuid = (D1:$7fda2e11;D2:$8630;D3:$436f;D4:($a0,$31,$f5,$36,$a6,$ee,$c1,$57));

{$IFDEF ANSISTRINGS}
function StrScan(const Str: PAnsiChar; Chr: AnsiChar): PAnsiChar; inline;
begin
  Result := AnsiStrings.StrScan(Str, Chr);
end;
{$ENDIF}

function GetFileLastWriteTime(const fName: AnsiString): TDateTime;
var
  hFile: THandle;
  mCreationTime: TFileTime;
  mLastAccessTime: TFileTime;
  mLastWriteTime: TFileTime;
  dft:DWord;  
begin
  hFile := _lopen(PAnsiChar(fName), OF_READ);
  GetFileTime(hFile, @mCreationTime, @mLastAccessTime, @mLastWriteTime);
  _lclose(hFile);
  FileTimeToLocalFileTime(mLastWriteTime, mCreationTime);
  FileTimeToDosDateTime(mCreationTime, LongRec(dft).Hi,LongRec(dft).Lo);
  Result:=FileDateToDateTime(dft);
end;

function PCharToInt64Def(const S: PAnsichar; Len: Integer; def: int64 = 0): int64;
var
  I: Integer;
  v: Integer;
begin
  if Len = 0 then
    Result := def
  else begin
    Result := 0;
    for I := 0 to len-1 do begin
      V := Convert[ord(s[i])];
      if V<0 then begin
        Result := def;
        Exit;
      end;
      result := (result * 10) + V;
    end;
  end;
end;

function PCharToIntDef(const S: PAnsichar; Len: Integer; def: Integer = 0): Integer;
var
  I: Integer;
  v: Integer;
begin
  if Len = 0 then
    Result := Def
  else begin
    Result := 0;
    for I := 0 to len-1 do begin
      V := Convert[ord(s[i])];
      if V<0 then begin
        Result := def;
        Exit;
      end;
      result := (result * 10) + V;
    end;
  end;
end;

function ipToInt(const strIP : AnsiString): Cardinal;
const
  DD: AnsiChar = '.';
var
  i: Cardinal;
  p, p1: PAnsichar;
begin
  result := 0;
  p := PAnsichar(strIP);
  p1 := StrScan(p, DD);
  if p1 = nil then Exit;
  i := PCharToInt64Def(p, p1-p) shl 24;
  Inc(p1);
  p := StrScan(p1, DD);
  if (p = nil) then Exit;
  i := i + PCharToInt64Def(p1, p-p1) shl 16;
  Inc(p);
  p1 := StrScan(p, DD);
  if (p1 = nil) then Exit;
  Result := i + PCharToInt64Def(p, p1-p) shl 8;
  Inc(p1);
  Result := Result + PCharToInt64Def(p1, Length(p1));
end;

function IPToStr(const Addr: Cardinal): string;
begin
  Result := Format('%d.%d.%d.%d', [
    (Addr shr $18) and $FF,
    (Addr shr $10) and $FF,
    (Addr shr $8) and $FF,
    Addr and $FF]);
end;

/// compare target, cmp_val same set target = new_val
/// return old value
function lock_cmp_exchange(cmp_val, new_val: Boolean; var target: Boolean):
    Boolean;
asm
{$ifdef win32}
  lock cmpxchg [ecx], dl
{$else}
.noframe
  mov rax, rcx
  lock cmpxchg [r8], dl
{$endif}
end;

function GetQueuedCompletionStatus; external kernel32 name 'GetQueuedCompletionStatus';
function CreateIoCompletionPort; external kernel32 name 'CreateIoCompletionPort';

function creatTcpSocketHandle:THandle;
begin
  Result := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_IP, nil, 0, WSA_FLAG_OVERLAPPED);
  if (Result = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadAcceptEx(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s,
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        @WSAID_ACCEPTEX,
        SizeOf(WSAID_ACCEPTEX),
        @@IocpAcceptEx,
        SizeOf(Pointer),
        bytesReturned,
        nil,
        nil);

  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadDisconnectEx(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s,
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        @WSAID_DISCONNECTEX,
        SizeOf(WSAID_DISCONNECTEX),
        @@IocpDisconnectEx,
        SizeOf(Pointer),
        bytesReturned,
        nil,
        nil);

  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadAcceptExSockaddrs(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s,
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        @WSAID_GETACCEPTEXSOCKADDRS,
        SizeOf(WSAID_GETACCEPTEXSOCKADDRS),
        @@IocpGetAcceptExSockaddrs,
        SizeOf(Pointer),
        bytesReturned,
        nil,
        nil);



  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure LoadConnecteEx(const s: TSocket);
var
  rtnCode: Integer;
  bytesReturned: Cardinal;
begin
  rtnCode := WSAIoctl(s,
        SIO_GET_EXTENSION_FUNCTION_POINTER,
        @WSAID_CONNECTEX,
        SizeOf(WSAID_CONNECTEX),
        @@IocpConnectEx,
        SizeOf(Pointer),
        bytesReturned,
        nil,
        nil);
  if rtnCode <> 0 then
  begin
    RaiseLastOSError;
  end;
end;

procedure WSAStart;
var
  lvRET: Integer;
  WSData: TWSAData;
begin
  lvRET := WSAStartup($0202, WSData);
  if lvRET <> 0 then RaiseLastOSError;
end;

procedure loadExFunctions;
var
  skt:TSocket;
begin
  skt := creatTcpSocketHandle;
  LoadAcceptEx(skt);
  LoadConnecteEx(skt);
  LoadAcceptExSockaddrs(skt);
  LoadDisconnectEx(skt);
  closesocket(skt);
end;

function GetSocketAddr(const pvAddr: AnsiString; pvPort: Integer): TSockAddrIn;
begin
  Result.sin_family := AF_INET;
  if Length(pvAddr) = 0 then
    Result.sin_addr.S_addr:= inet_addr(PAnsiChar(AnsiString('0.0.0.0')))
  else
    Result.sin_addr.S_addr:= inet_addr(PAnsiChar(AnsiString(pvAddr)));
  Result.sin_port := htons(pvPort);
end;

function socketBind(s: TSocket; const pvAddr: string; pvPort: Integer): Boolean;
var
  sockaddr: TSockAddrIn;
begin
  FillChar(sockaddr, SizeOf(sockaddr), 0);
  with sockaddr do
  begin
    sin_family := AF_INET;
    sin_addr.S_addr := inet_addr(PAnsichar(AnsiString(pvAddr)));
    sin_port :=  htons(pvPort);
  end;
  Result := iocp.winapi.winsock.bind(s, @TSockAddr(sockaddr), SizeOf(sockaddr)) = 0;
end;

function SetKeepAlive(pvSocket: TSocket; pvKeepAliveTime: Integer = 5000):
    Boolean;
var
  Opt, insize, outsize: integer;
  outByte: DWORD;
  inKeepAlive, outKeepAlive: TTCP_KEEPALIVE;
begin
  Result := false;
  Opt := 1;
  if SetSockopt(pvSocket, SOL_SOCKET, SO_KEEPALIVE,
     @Opt, sizeof(Opt)) = SOCKET_ERROR then exit;

  inKeepAlive.OnOff := 1;
  
  inKeepAlive.KeepAliveTime := pvKeepAliveTime;

  inKeepAlive.KeepAliveInterval := 1;
  insize := sizeof(TTCP_KEEPALIVE);
  outsize := sizeof(TTCP_KEEPALIVE);

  if WSAIoctl(pvSocket,
     SIO_KEEPALIVE_VALS,
     @inKeepAlive, insize,
     @outKeepAlive,
    outsize, outByte, nil, nil) <> SOCKET_ERROR then
  begin
    Result := true;
  end;
end;

function CreateTcpOverlappedSocket: THandle;
begin
  Result := WSASocket(AF_INET,SOCK_STREAM, IPPROTO_TCP, Nil, 0, WSA_FLAG_OVERLAPPED);
  if (Result = 0) or (Result = INVALID_SOCKET) then
  begin
    RaiseLastOSError;
  end;
end;

initialization
  WSAStart;
  loadExFunctions;

end.
