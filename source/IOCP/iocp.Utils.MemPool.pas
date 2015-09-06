{*******************************************************}
{                                                       }
{       iocp.Utils.MemPool    内存池                    }
{                                                       }
{       版权所有 (C) 2013  YangYxd                      }
{                                                       }
{*******************************************************}
{
  说明： TIocpMemPool 经过多次修改，后来还是换成了 QDAC
  作者的方式，更加简洁明了。感谢 QDAC作者 swish。
}

unit iocp.Utils.MemPool;

interface

{$DEFINE UseMemPool_IocpLink}
{$IF defined(FPC) or defined(VER170) or defined(VER180) or defined(VER190) or defined(VER200) or defined(VER210)}
  {$DEFINE HAVE_INLINE}
{$IFEND}

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  SysUtils, Classes, SyncObjs;

{$if CompilerVersion < 23}
type
   NativeUInt = Cardinal;
   IntPtr = Cardinal;
{$ifend}

const
  MemoryDelta = $2000; { Must be a power of 2 }
  MaxListSize = MaxInt div 16;

type
  MAddrList = array of Pointer;
  PMAddrList = ^MAddrList;
  Number = Cardinal;

type
  /// <summary>
  /// 内存池  (非线程安全)
  /// </summary>
  TYXDMemPool = class(TObject)
  private
    FMemory: Pointer;
    FDataBuf: MAddrList;
    FDataBufSize: Integer;
    FBufSize, FPosition: Cardinal;
    FBlockSize: Cardinal;
    FDebrisList: MAddrList;
    FDebrisCapacity: Cardinal;
    function GetBufferPageCount: Cardinal;
    procedure SetDebrisCapacity(NewCapacity: Cardinal);
    function GetBufferSize: Cardinal;
  protected
    FDebrisCount: Cardinal;
    procedure ClearDebris();
    procedure GrowDebris; virtual;
  public
    constructor Create(BlockSize: Cardinal; PageSize: Cardinal = MemoryDelta); virtual;
    destructor Destroy; override;
    procedure Clear;
    function Pop: Pointer;
    procedure Push(const V: Pointer);
    property Size: Cardinal read GetBufferSize;
    property BlockSize: Cardinal read FBlockSize;
    property PageCount: Cardinal read GetBufferPageCount;
  end;

type
  TIocpMemPoolNotify = procedure(Sender: TObject; const AData: Pointer) of object;
  TIocpMemPoolNew = procedure(Sender: TObject; var AData: Pointer) of object;

type
  /// <summary>
  /// 固定大小内存池， 线程安全
  ///  注意事项：Push 时并不会检查压入的数据是否已经Push过。
  ///  重复Push必将产生AV等异常和严重后果
  /// </summary>
  TIocpMemPool = class(TObject)
  private
    FPool: MAddrList;
    FCount: Integer;
    FMaxSize: Integer;
    FBlockSize: Cardinal;
    FLocker: TCriticalSection;
    FOnFree: TIocpMemPoolNotify;
    FOnNew: TIocpMemPoolNew;
    FOnReset: TIocpMemPoolNotify;
  protected
    procedure DoFree(const AData: Pointer); inline;
    procedure DoReset(const AData: Pointer); inline;
    procedure DoNew(var AData: Pointer); inline;
  public
    constructor Create(BlockSize: Cardinal; MaxSize: Integer = 64);
    destructor Destroy; override;
    procedure Clear;
    procedure Lock; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
    procedure Unlock; {$IFDEF HAVE_INLINE} inline;{$ENDIF}
    function Pop(): Pointer;
    procedure Push(const V: Pointer);
    property BlockSize: Cardinal read FBlockSize;
    property MaxSize: Integer read FMaxSize;
    property Count: Integer read FCount;
    property OnFree: TIocpMemPoolNotify read FOnFree write FOnFree;
    property OnNew: TIocpMemPoolNew read FOnNew write FOnNew;
    property OnReset: TIocpMemPoolNotify read FOnReset write FOnReset;
  end;

type
  PIocpLink = ^TIocpLink;
  TIocpLink = packed record
    Next: PIocpLink;
    Data: PAnsiChar;
  end;

type
  TOnPopMem = function (): Pointer of object;
  TOnPushMem = procedure (const V: Pointer) of object;

type
  /// <summary>
  /// IOCP 缓冲流 （非线程安全）
  /// </summary>
  TIocpStream = class(TStream)
  private
    FHandle: Cardinal;
    FWritePos: Pointer;
    FReadPos: PAnsiChar;
    FFirst: PIocpLink;
    FLast: PIocpLink;
    FCur: PIocpLink;
    FSize: Integer;
    FOffset: Integer;
    FPosition: Int64;
    FCunkSize: Integer;
    FWaitRecv: Boolean;
    FOnGetMem: TOnPopMem;
    FOnPushMem: TOnPushMem;
    function ReadData(const Offset: Integer): Boolean;
    function GetUnReadSize: Cardinal;
  protected
    function GetSize: Int64; override;
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
    function SetPosition(const Position: Int64): NativeUInt;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear; overload;
    procedure Clear(ASize: Integer); overload;
    procedure ClearRead;
    procedure SetCunkSize(const Value: Integer);
    function GetPosition: Int64; inline;
    function Skip(ASize: Longint): Longint;
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(ASize: Integer): string; 
    function ReadBuffer(var Buffer; Count: Longint): Boolean; inline;
    // 写入数据，用户不要在外部调用
    function Write(const Buffer; Count: Longint): Longint; override;
    function WriteBuffer(Buf: PAnsiChar; Count: Longint): Longint; 
    function Seek(const Offset: Int64; Origin: TSeekOrigin = soCurrent): Int64; override;
    // 是否还需要接收更多的数据？
    property WaitRecv: Boolean read FWaitRecv write FWaitRecv;
    // 未读数据大小
    property UnReadSize: Cardinal read GetUnReadSize;
    // 自定义句柄
    property Handle: Cardinal read FHandle write FHandle; 
    property OnPopMem: TOnPopMem read FOnGetMem write FOnGetMem;
    property OnPushMem: TOnPushMem read FOnPushMem write FOnPushMem;
  end;

implementation

resourcestring
  strStreamError = '不支持的接口';

{$IFDEF UseMemPool_IocpLink}
var
  SMemPool: TIocpMemPool;
{$ENDIF}
  
{ TYxdMemPool }

procedure TYxdMemPool.Clear;
var
  I: Integer;
begin
  for i := 0 to FDataBufSize do
    FreeMem(FDataBuf[i], FBufSize);
  SetLength(FDataBuf, 0);
  FMemory := nil;
  FDataBufSize := -1;
  FPosition := FBufSize;
  ClearDebris();
end;

procedure TYxdMemPool.ClearDebris;
begin
  SetDebrisCapacity(0);
  FDebrisCount := 0;
end;

constructor TYxdMemPool.Create(BlockSize, PageSize: Cardinal);
begin
  FBlockSize := BlockSize;
  if FBlockSize < 1 then
    FBlockSize := 4;
  FBufSize := (PageSize div FBlockSize);
  if (FBufSize < 1) or (PageSize mod FBlockSize > 0) then
    Inc(FBufSize);
  FBufSize := FBufSize * FBlockSize;

  FMemory := nil;
  FDataBufSize := -1;
  FPosition := FBufSize;

  FDebrisCount := 0;
  FDebrisCapacity := 0;
  SetLength(FDebrisList, 0);
end;

destructor TYxdMemPool.Destroy;
begin
  Clear;
  SetLength(FDebrisList, 0);
  inherited Destroy;
end;

function TYxdMemPool.GetBufferPageCount: Cardinal;
begin
  Result := Length(FDataBuf);
end;

function TYxdMemPool.GetBufferSize: Cardinal;
begin
  if High(FDataBuf) < 0 then
    Result := 0
  else Result := DWORD(GetBufferPageCount) * FBufSize;
end;

procedure TYxdMemPool.GrowDebris;
var
  Delta: Cardinal;
begin
  if FDebrisCapacity > 64 then begin
    Delta := FDebrisCapacity shr 2;
    if Delta > 1024 then
      Delta := 1024;
  end else
    if FDebrisCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetDebrisCapacity(FDebrisCapacity + Delta);
end;

function TYxdMemPool.Pop: Pointer;
begin
  if FDebrisCount > 0 then begin
    Dec(FDebrisCount);
    Result := FDebrisList[FDebrisCount]; // 取末尾的，不用移动内存
    FDebrisList[FDebrisCount] := nil;
  end else begin
    if (FBufSize - FPosition < FBlockSize) then begin   // 预分配内存已经用完，增加内存
      Inc(FDataBufSize);
      SetLength(FDataBuf, FDataBufSize + 1);
      GetMem(FMemory, FBufSize);
      FDataBuf[FDataBufSize] := FMemory;
      Result := FMemory;
      FPosition := FBlockSize;
    end else begin
      Result := Pointer(DWORD(FMemory) + FPosition);
      Inc(FPosition, FBlockSize);
    end;
  end;
end;

procedure TYxdMemPool.Push(const V: Pointer);
begin
  if V <> nil then begin
    if FDebrisCount = FDebrisCapacity then
      GrowDebris;
    FDebrisList[FDebrisCount] := V;
    Inc(FDebrisCount);
  end;
end;

procedure TYxdMemPool.SetDebrisCapacity(NewCapacity: Cardinal);
begin
  if (NewCapacity < FDebrisCount) or (NewCapacity > MaxListSize) then
    Exit;
  if NewCapacity <> FDebrisCapacity then begin
    SetLength(FDebrisList, NewCapacity);
    FDebrisCapacity := NewCapacity;
  end;
end;

{ TIocpMemPool }

procedure TIocpMemPool.Clear;
var
  I: Integer;
begin
  FLocker.Enter;
  try
    I := 0;
    while I < FCount do begin
      DoFree(FPool[I]);
      Inc(I);
    end;
  finally
    FLocker.Leave;
  end;
end;

constructor TIocpMemPool.Create(BlockSize: Cardinal; MaxSize: Integer);
begin
  FLocker := TCriticalSection.Create;
  FCount := 0;
  if MaxSize < 4 then
    FMaxSize := 4
  else
    FMaxSize := MaxSize;
  SetLength(FPool, FMaxSize);
  FBlockSize := BlockSize;
  if BlockSize <= 8 then
    FBlockSize := 8
  else if BlockSize <= 16 then
    FBlockSize := 16
  else if BlockSize <= 32 then
    FBlockSize := 32
  else begin
    // 块大小以64字节对齐，这样的执行效率最高
    if (BlockSize mod 64 = 0) then
      FBlockSize := BlockSize
    else
      FBlockSize := (BlockSize div 64) * 64 + 64;
  end; 
end;

destructor TIocpMemPool.Destroy;
begin
  try
    Clear;
  finally
    FreeAndNil(FLocker);  
    inherited;
  end;
end;

procedure TIocpMemPool.DoFree(const AData: Pointer);
begin
  if Assigned(FOnFree) then
    FOnFree(Self, AData)
  else
    FreeMem(AData);
end;

procedure TIocpMemPool.DoNew(var AData: Pointer);
begin
  if Assigned(FOnNew) then
    FOnNew(Self, AData)
  else
    GetMem(AData, FBlockSize);
end;

procedure TIocpMemPool.DoReset(const AData: Pointer);
begin
  if Assigned(FOnReset) then
    FOnReset(Self, AData);
end;

procedure TIocpMemPool.Lock;
begin
  FLocker.Enter;
end;

function TIocpMemPool.Pop: Pointer;
begin
  Result := nil;
  FLocker.Enter;
  if FCount > 0 then begin
    Dec(FCount);
    Result := FPool[FCount];
  end;
  FLocker.Leave;
  if Result = nil then
    DoNew(Result);
  if Result <> nil then
    DoReset(Result);
end;

procedure TIocpMemPool.Push(const V: Pointer);
var
  ADoFree: Boolean;
begin
  if V = nil then Exit;
  ADoFree := True;
  FLocker.Enter;
  if FCount < FMaxSize then begin
    FPool[FCount] := V;
    Inc(FCount);
    ADoFree := False;
  end;
  FLocker.Leave;
  if ADoFree then 
    DoFree(V);
end;

procedure TIocpMemPool.Unlock;
begin
  FLocker.Leave;
end;

{ TIocpStream }

procedure TIocpStream.Clear;
var
  Last: Pointer;
begin
  while FFirst <> nil do begin
    if Assigned(FOnPushMem) then
      FOnPushMem(FFirst.Data)
    else
      FreeMemory(FFirst.Data);
    Last := FFirst;
    FFirst := FFirst.Next;
    {$IFDEF UseMemPool_IocpLink}
    SMemPool.Push(Last);
    {$ELSE}
    Dispose(Last);
    {$ENDIF}
  end;
  FSize := 0;
  FPosition := 0;
  FOffset := 0;
  FWritePos := nil;
  FLast := nil;
  FReadPos := nil;
  FCur := nil;
  FHandle := 0;
end;

procedure TIocpStream.Clear(ASize: Integer);
var
  Last: Pointer;
begin
  if ASize < 1 then Exit;
  // 清除从开始算起指定大小的数据
  if ASize >= FSize then begin
    Clear();
    Exit;
  end;
  Inc(ASize, FOffset);
  while (FFirst <> nil) and (ASize >= FCunkSize) do begin
    if Assigned(FOnPushMem) then
      FOnPushMem(FFirst.Data)
    else
      FreeMem(FFirst.Data);
    Last := FFirst;
    FFirst := FFirst.Next;
    {$IFDEF UseMemPool_IocpLink}
    SMemPool.Push(Last);
    {$ELSE}
    Dispose(Last);
    {$ENDIF}
    Dec(ASize, FCunkSize);
  end;
  FCur := FFirst;
  FPosition := 0;
  FReadPos := FCur.Data + ASize;
  FOffset := ASize;
end;

procedure TIocpStream.ClearRead;
begin
  Clear(FPosition);
end;

constructor TIocpStream.Create();
begin
  FCunkSize := 1024 shl 2;
  FSize := 0;
  FPosition := 0;
  FWritePos := nil;
  FLast := nil;
  FReadPos := nil;
  FCur := nil;
  FFirst := nil;
end;

destructor TIocpStream.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TIocpStream.GetPosition: Int64;
begin
  Result := FPosition;
end;

function TIocpStream.GetSize: Int64;
begin
  Result := FSize - FOffset;
end;

function TIocpStream.GetUnReadSize: Cardinal;
begin
  Result := FSize - FOffset - FPosition;
end;

function TIocpStream.Read(var Buffer; Count: Integer): Longint;
var
  P, P1, Buf: PAnsiChar;
  I: Integer;
begin
  Result := 0;
  if (Count < 1) or (not ReadData(Count)) then 
    Exit;
  Buf := Pointer(@Buffer);
  P := FCur.Data;
  P1 := FReadPos;
  while Count > 0 do begin
    I := FCunkSize - (P1 - P);
    if I < Count then begin
      Move(P1^, Buf^, I);
      Dec(Count, I);
      Inc(Buf, I);
      FCur := FCur.Next;
      P := FCur.Data;
      P1 := P;
      Inc(Result, I);
    end else begin
      Move(P1^, Buf^, Count);
      Inc(Result, Count);
      Inc(P1, Count);
      Break;
    end;
  end;
  Inc(FPosition, Result);
  FReadPos := P1;
end;

function TIocpStream.ReadBuffer(var Buffer; Count: Integer): Boolean;
begin
  Result := Read(Buffer, Count) = Count;
end;

function TIocpStream.ReadData(const Offset: Integer): Boolean;
begin
  if (FSize - FOffset - FPosition) < OffSet then begin
    Result := False;
    FWaitRecv := True;
  end else
    Result := True;
end;

function TIocpStream.ReadString(ASize: Integer): string;
begin
  if (ASize < 1) or (not ReadData(ASize)) then begin
    Result := '';
    Exit;
  end;
  SetLength(Result, ASize);
  Read(Result[1], ASize);
end;

function TIocpStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Origin = soBeginning then begin
    Result := SetPosition(Offset);
  end else if Origin = soEnd then
    Result := SetPosition(FSize)
  else
    Result := SetPosition(FPosition + Offset);
end;

procedure TIocpStream.SetSize(NewSize: Integer);
begin
  raise Exception.Create(strStreamError);
end;

procedure TIocpStream.SetCunkSize(const Value: Integer);
begin
  if Value < 8 then Exit;  
  FCunkSize := Value;
end;

function TIocpStream.SetPosition(const Position: Int64): NativeUInt;
var
  I: Int64;
begin
  I := Position;
  Result := FPosition;
  if Result = I then Exit;
  if I <= 0 then begin
    Result := 0;
    FCur := FFirst;
    if FCur <> nil then
      FReadPos := FCur.Data + FOffset;
  end else if I >= FSize then begin
    Result := GetSize;
    FCur := FLast;
    FReadPos := FWritePos;
  end else begin
    FCur := FFirst;
    Result := 0;
    Inc(I, FOffset);
    while (FCur <> nil) and (Result < I) do begin
      if Result + Cardinal(FCunkSize) <= I then begin
        FCur := FCur.Next;
        Inc(Result, FCunkSize);
      end else begin
        FReadPos := FCur.Data + (I - Result);
        Result := Position;
        Break;
      end;
    end;
  end;
  FPosition := Result;
end;

procedure TIocpStream.SetSize(const NewSize: Int64);
begin
  raise Exception.Create(strStreamError);
end;

function TIocpStream.Skip(ASize: Integer): Longint;
begin
  Result := SetPosition(FPosition + ASize);
end;

function TIocpStream.Write(const Buffer; Count: LongInt): Longint;
begin
  Result := WriteBuffer(Pointer(@Buffer), Count);
end;

function TIocpStream.WriteBuffer(Buf: PAnsiChar; Count: Integer): Longint;
var
  P: PAnsiChar;
  I: Integer;
begin
  Result := 0;
  if Count < 1 then
    Exit;
  if FFirst = nil then begin
    {$IFDEF UseMemPool_IocpLink}
    FFirst := SMemPool.Pop;
    {$ELSE}
    New(FFirst);
    {$ENDIF}
    FFirst.Next := nil;
    if Assigned(FOnGetMem) then
      FFirst.Data := FOnGetMem()
    else
      GetMem(FFirst.Data, FCunkSize);
    FLast := FFirst;
    FCur := FFirst;
    FWritePos := FFirst.Data;
    FReadPos := FWritePos;
  end;
  P := FLast.Data;
  while Count > 0 do begin
    I := FCunkSize - (FWritePos - P);
    if Count > I then begin
      Move(Buf^, FWritePos^, I);
      Dec(Count, I);
      Inc(Buf, I);
      {$IFDEF UseMemPool_IocpLink}
      FLast.Next := SMemPool.Pop;
      {$ELSE}
      New(FLast.Next);
      {$ENDIF}
      FLast := FLast.Next;
      FLast.Next := nil;
      if Assigned(FOnGetMem) then
        FLast.Data := FOnGetMem()
      else
        GetMem(FLast.Data, FCunkSize);
      FWritePos := FLast.Data;
      P := FWritePos;
      Inc(Result, I);
    end else begin
      Move(Buf^, FWritePos^, Count);
      FWritePos := Pointer(IntPtr(FWritePos) + NativeInt(Count));
      Inc(Result, Count);
      Break;
    end;
  end;
  Inc(FSize, Result);
end;

initialization
  {$IFDEF UseMemPool_IocpLink}
  SMemPool := TIocpMemPool.Create(8, 2048);
  {$ENDIF}

finalization
  {$IFDEF UseMemPool_IocpLink}
  FreeAndNil(SMemPool);
  {$ENDIF}

end.




