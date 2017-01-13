{*******************************************************}
{                                                       }
{       iocp.Utils.Hash    哈希表，哈希函数             }
{                                                       }
{       版权所有 (C) 2013 YangYxd                       }
{                                                       }
{*******************************************************}
{
 --------------------------------------------------------------------
  说明
 --------------------------------------------------------------------
  iocp.Utils.Hash 基于YxdHash，版本归YangYxd所有，保留一切权利
  Hash函数由QDAC作者swish编写，版权归swish(QQ:109867294)所有
  QDAC官方群：250530692

 2015.05.20 ver 1.0.0
 --------------------------------------------------------------------
  - 此单元做为Iocp的Hash操作专用库.
 --------------------------------------------------------------------
}

unit iocp.Utils.Hash;

interface

{$DEFINE USE_MEMPOOL}         // 是否使用内存池
{$DEFINE USE_ATOMIC}          // 是否启用原子操作函数
{.$DEFINE AUTORESIE}           // 哈希表是否自动调整桶大小

uses
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  iocp.Utils.MemPool,
  SysUtils, Classes, Types, SyncObjs;

type
  {$if CompilerVersion < 23}
  NativeUInt = Cardinal;
  NativeInt = Integer;
  {$ifend}
  Number = NativeInt;
  PNumber = ^Number;
  NumberU = NativeUInt;
  PNumberU = ^NumberU;

type
  /// 桶内元素的哈希值列表
  THashType = NumberU;
  PPHashList = ^PHashList;
  PHashList = ^THashList;
  THashList = packed record
    Next: PHashList;  // 下一元素
    Data: Pointer;    // 附加数据成员
    Hash: THashType;  // 当前元素哈希值，记录以便重新分配桶时不需要再次外部计算
    procedure Reset; inline;
  end;
  THashArray = array of PHashList;

type
  PHashValue = ^THashValue;
  THashValue = packed record
    Size: Cardinal;       // 数据大小
    Data: Pointer;        // 数据指针
    function AsString: string;
    procedure Clear;
  end;

type
  PHashMapValue = ^THashMapValue;
  THashMapValue = packed record
    Value: THashValue;      // 数据
    IsStrKey: WordBool;     // 是否是字符串 Key
    Key: string;            // 字符串 Key
    function GetNumKey: Number; inline;
    procedure SetNumKey(const Value: Number); inline;
  end;

type
  PHashMapList = ^THashMapList;
  THashMapList = packed record
    Next: PHashList;      // 下一元素
    Data: PHashMapValue;  // 附加数据成员
    Hash: THashType;      // 当前元素哈希值，记录以便重新分配桶时不需要再次外部计算
  end;

type
  PHashMapLinkItem = ^THashMapLinkItem;
  THashMapLinkItem = packed record
    Next: PHashMapLinkItem;
    Prev: PHashMapLinkItem;
    Value: PHashMapValue;
  end;

type
  /// <summary>比较函数</summary>
  /// <param name='P1'>第一个要比较的参数</param>
  /// <param name='P2'>第二个要比较的参数</param>
  /// <returns> 如果P1<P2，返回小于0的值，如果P1>P2返回大于0的值，如果相等，返回0</returns>
  TYXDCompare = function (P1, P2:Pointer): Integer of object;

type
  /// <summary>删除哈希表一个元素的通知</summary>
  /// <param name="ATable">哈希表对象</param>
  /// <param name="AHash">要删除的对象的哈希值</param>
  /// <param name="AData">要删除的对象数据指针</param>
  TYXDHashDeleteNotify = procedure (ATable: TObject; AHash: THashType; AData: Pointer) of object;

type
  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next: PHashItem;
    Key: string;
    Value: Number;
  end;

type
  /// <summary>删除哈希表一个元素的通知</summary>
  /// <param name="ATable">哈希表对象</param>
  /// <param name="AHash">要删除的对象的哈希值</param>
  /// <param name="AData">要删除的对象数据指针</param>
  TYXDStrHashItemFreeNotify = procedure (Item: PHashItem) of object;

  TStringHash = class
  private
    FCount: Integer;
    FOnFreeItem: TYXDStrHashItemFreeNotify;
    function GetBucketsCount: Integer;
  public
    Buckets: array of PHashItem;
    FLocker: TCriticalSection;
    constructor Create(Size: Cardinal = 331);
    destructor Destroy; override;
    function Find(const Key: string): PPHashItem;
    procedure Add(const Key: string; Value: Number);
    procedure AddOrUpdate(const Key: string; Value: Number);
    procedure Clear;
    procedure GetItems(AList: TList);
    procedure Lock;
    procedure UnLock;
    procedure Remove(const Key: string);
    function Modify(const Key: string; Value: Number): Boolean;
    function ValueOf(const Key: string; const DefaultValue: Number = -1): Number;
    function Exists(const Key: string): Boolean;  
    property Count: Integer read FCount;
    property BucketsCount: Integer read GetBucketsCount;
    property OnFreeItem: TYXDStrHashItemFreeNotify read FOnFreeItem write FOnFreeItem;
  end;

type
  PPIntHashItem = ^PIntHashItem;
  PIntHashItem = ^TIntHashItem;
  TIntHashItem = record
    Next: PIntHashItem;
    Key: THashType;
    Value: Number;
  end;

  /// <summary>删除哈希表一个元素的通知</summary>
  /// <param name="ATable">哈希表对象</param>
  /// <param name="AHash">要删除的对象的哈希值</param>
  /// <param name="AData">要删除的对象数据指针</param>
  TYXDIntHashItemFreeNotify = procedure (Item: PIntHashItem) of object;

  TIntHash = class
  private
    FCount: Integer;
    FOnFreeItem: TYXDIntHashItemFreeNotify;
    function GetBucketsCount: Integer;
  protected
  public
    Buckets: array of PIntHashItem;
    FLocker: TCriticalSection;
    constructor Create(Size: Cardinal = 331);
    destructor Destroy; override;
    function Find(const Key: THashType): PPIntHashItem;
    procedure Add(const Key: THashType; Value: Number);
    procedure AddOrUpdate(const Key: THashType; Value: Number);
    procedure Clear;
    procedure GetItems(AList: TList);
    procedure Lock;
    procedure UnLock;
    function Remove(const Key: THashType): Boolean;
    function Modify(const Key: THashType; Value: Number): Boolean;
    function ValueOf(const Key: THashType; const DefaultValue: Number = -1): Number;
    function Exists(const Key: THashType): Boolean;
    property Count: Integer read FCount;
    property BucketsCount: Integer read GetBucketsCount;
    property OnFreeItem: TYXDIntHashItemFreeNotify read FOnFreeItem write FOnFreeItem;
  end;

type
  /// <summary>
  /// 哈希表, 用于存贮一些用于查询的散列数据
  /// </summary>
  TYXDHashTable = class(TObject)
  private
    FPool: TYXDMemPool;
    procedure SetAutoSize(const Value: Boolean);
    procedure FreeBucket(var ABucket: PHashList); virtual;
    function GetMemSize: Int64; virtual;
  protected
    FCount: Integer;
    FBuckets: THashArray;
    FOnDelete: TYXDHashDeleteNotify;
    FOnCompare: TYXDCompare;
    FAutoSize : Boolean; 
    procedure DoDelete(AHash: THashType; AData:Pointer); virtual;
    function GetBuckets(AIndex: Integer): PHashList; inline;
    function GetBucketCount: Integer; inline;
    function Compare(Data1, Data2: Pointer; var AResult: Integer): Boolean; inline;
  public
    ///构造函数，以桶数量为参数，后期可以调用Resize调整
    constructor Create(ASize: Integer); overload; virtual;
    ///构造函数
    constructor Create; overload;
    destructor Destroy;override;
    procedure Clear; virtual;
    procedure ReSize(ASize: Cardinal);
    procedure Add(AData: Pointer; AHash: THashType);
    // 找出哈希值为AHash的所有HashList，需要自己释放返回的HashList
    function Find(AHash: THashType): PHashList; overload;
    function Find(AData: Pointer; AHash: THashType): Pointer; overload;
    function FindFirstData(AHash: THashType): Pointer;
    function FindFirst(AHash: THashType): PHashList; inline;
    function FindNext(AList: PHashList): PHashList; inline;
    procedure FreeHashList(AList: PHashList);
    function Exists(AData: Pointer; AHash: THashType):Boolean;
    procedure Delete(AData: Pointer; AHash: THashType);
    procedure Update(AData: Pointer; AOldHash, ANewHash: THashType);
    // 元素个数
    property Count: Integer read FCount;
    // 桶数量
    property BucketCount: Integer read GetBucketCount;
    // 桶列表
    property Buckets[AIndex:Integer]: PHashList read GetBuckets;default;
    // 比较函数
    property OnCompare:TYXDCompare read FOnCompare write FOnCompare;
    // 删除事件通知
    property OnDelete: TYXDHashDeleteNotify read FOnDelete write FOnDelete;
    // 是否自动调整桶大小
    property AutoSize: Boolean read FAutoSize write SetAutoSize;
    // 内存占用大小
    property MemSize: Int64 read GetMemSize;
  end;

type
  /// <summary>
  /// 以字符串为 Key 的Hash表
  /// 特点：
  ///   1. 以字符串作为Key
  ///   2. 可快速删除数据
  ///   3. 可快速添加数据
  ///   4. 无索引，只能通过桶来遍列每一个数据
  /// </summary>
  TYXDHashMapTable = class(TYXDHashTable)
  private
    FListPool: TYxdMemPool;
    procedure FreeBucket(var ABucket: PHashList); override;
    function GetMemSize: Int64; override;
  protected
    procedure DoAdd(ABucket: PHashMapList); virtual;
  public
    constructor Create(ASize: Integer); override;
    destructor Destroy; override;
    procedure Add(const Key: string; AData: PHashValue); overload;
    procedure Add(const Key: Number; AData: PHashValue); overload;
    procedure Add(const Key: string; AData: NativeInt); overload;
    procedure Add(const Key: Number; AData: NativeInt); overload;
    procedure Clear; override;
    function Exists(const Key: string): Boolean; overload; inline;
    function Exists(const Key: Number): Boolean; overload; inline;
    function Find(const Key: string): PHashMapValue; overload;
    function Find(const Key: Number): PHashMapValue; overload;
    function FindList(const Key: string): PPHashList; overload;
    function FindList(const Key: Number): PPHashList; overload;
    function Update(const Key: string; Value: PHashValue): Boolean; overload;
    function Update(const Key: Number; Value: PHashValue): Boolean; overload;
    function Remove(const Key: string): Boolean; overload;
    function Remove(const Key: Number): Boolean; overload;
    function Remove(const P: PHashMapValue): Boolean; overload;
    function ValueOf(const Key: string): PHashValue; overload;
    function ValueOf(const Key: Number): PHashValue; overload;
  end;

type
  TYXDHashMapListBase = class(TYXDHashMapTable)
  private
    function GetItem(Index: Integer): PHashMapValue; virtual; abstract;
  public
    property Items[Index: Integer]: PHashMapValue read GetItem;
  end;

type
  /// <summary>
  /// 以字符串为Key，带索引的 Hash 列表
  /// 特点：
  ///   1. 以字符串作为Key
  ///   2. 可快速使用 Index 访问遍列数据
  ///   3. 可通过Index删除数据。删除速度较慢
  ///   4. 可快速添加数据
  /// </summary>
  TYXDHashMapList = class(TYXDHashMapListBase)
  private
    FList: TList;
    function GetItem(Index: Integer): PHashMapValue; override;
  protected
    procedure DoAdd(ABucket: PHashMapList); override;
    procedure DoDelete(AHash: THashType; AData:Pointer); override;
  public
    constructor Create(ASize: Integer); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
  end;

type
  /// <summary>
  /// 以字符串为Key，带双向链表索引的 Hash 链表
  /// 特点：
  ///   1. 以字符串作为Key
  ///   2. 可使用 Index 访问每一个数据（速度慢，建议使用链表方式遍列）
  ///   3. 可快速删除数据
  ///   4. 可快速添加数据
  /// </summary>
  TYXDHashMapLinkTable = class;

  TYXDHashMapLinkTableEnumerator = class
  private
    FItem: PHashMapLinkItem;
  public
    constructor Create(AList: TYXDHashMapLinkTable);
    function GetCurrent: PHashMapLinkItem; inline;
    function MoveNext: Boolean;
    property Current: PHashMapLinkItem read GetCurrent;
  end;

  TYXDHashMapLinkTable = class(TYXDHashMapListBase)
  private
    FFirst: PHashMapLinkItem;
    FLast: PHashMapLinkItem;
    ListBuckets: THashArray;
    FLinkHashPool: TYxdMemPool;
    function GetItem(Index: Integer): PHashMapValue; override;
    function GetMemSize: Int64; override;
    function FindLinkItem(AData: Pointer; isDelete: Boolean): PHashMapLinkItem;
    procedure FreeLinkList;
    function GetLast: PHashMapValue;
  protected
    procedure DoAdd(ABucket: PHashMapList); override;
    procedure DoDelete(AHash: THashType; AData:Pointer); override;
  public
    constructor Create(ASize: Integer); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer);
    function GetEnumerator: TYXDHashMapLinkTableEnumerator;   
    property First: PHashMapLinkItem read FFirst;
    property Last: PHashMapLinkItem read FLast;
    property LastValue: PHashMapValue read GetLast;
  end;

// --------------------------------------------------------------------------
//  HASH 处理函数
// --------------------------------------------------------------------------

// HASH 函数
function HashOf(const Key: Pointer; KeyLen: Cardinal): THashType; overload;
function HashOf(const Key: string): THashType; inline; overload;
// 根据一个参考客户值，返回适当的哈希表大小
function CalcBucketSize(dataSize: Cardinal): THashType;

// --------------------------------------------------------------------------
//  原子操作 函数
// --------------------------------------------------------------------------

{$IFDEF USE_ATOMIC}
{$IF RTLVersion<26}
// 为与D2007兼容, 原子操作函数
function AtomicCmpExchange(var Target: Integer; Value, Comparand: Integer): Integer; inline;
function AtomicExchange(var Target: Integer; Value: Integer): Integer; inline;
function AtomicIncrement(var Target: Integer): Integer; overload; inline;
function AtomicIncrement(var Target: Integer; const Value: Integer): Integer; overload; inline;
function AtomicDecrement(var Target: Integer): Integer; inline;
{$IFEND}
// 原子操作函数
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer; inline;
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer; inline;
function AtomicAdd(var Dest: Integer; const AValue: Integer): Integer; inline;
{$ENDIF}

implementation

const
  BucketSizes: array[0..47] of Cardinal = (
    17,37,79,163,331,673,1361,2729,5471,10949,21911,43853,87719,175447,350899,
    701819,1403641,2807303,5614657,8999993,11229331,22458671,30009979,44917381,
    50009969,60009997,70009987,80009851,89834777,100009979,110009987,120009979,
    130009903, 140009983,150009983,165009937,179669557,200009959,359339171,
    400009999, 450009883,550009997,718678369,850009997,1050009979,1437356741,
    1850009969, 2147483647
  );

function HashOf(const Key: Pointer; KeyLen: Cardinal): THashType; overload;
var
  ps: PCardinal;
  lr: Cardinal;
begin
  Result := 0;
  if KeyLen > 0 then begin
    ps := Key;
    lr := (KeyLen and $03);//检查长度是否为4的整数倍
    KeyLen := (KeyLen and $FFFFFFFC);//整数长度
    while KeyLen > 0 do begin
      Result := ((Result shl 5) or (Result shr 27)) xor ps^;
      Inc(ps);
      Dec(KeyLen, 4);
    end;
    if lr <> 0 then begin
      case lr of
        1: KeyLen := PByte(ps)^;
        2: KeyLen := PWORD(ps)^;
        3: KeyLen := PWORD(ps)^ or (PByte(Cardinal(ps) + 2)^ shl 16);
      end;
      Result := ((Result shl 5) or (Result shr 27)) xor KeyLen;
    end;
  end;
end;

function HashOf(const Key: string): THashType; inline; overload;
begin
  Result := HashOf(PChar(Key), Length(Key){$IFDEF UNICODE} shl 1{$ENDIF});
end;

function CalcBucketSize(dataSize: Cardinal): THashType;
var
  i: Integer;
begin
  for i := 0 to High(BucketSizes) do
    if BucketSizes[i] > dataSize then begin
      Result := BucketSizes[i];
      Exit;
    end;
  Result := BucketSizes[High(BucketSizes)];
end;


{$IFDEF USE_ATOMIC}
{$IF RTLVersion<26}
function AtomicCmpExchange(var Target: Integer; Value: Integer; Comparand: Integer): Integer; inline;
begin
  Result := InterlockedCompareExchange(Target, Value, Comparand);
end;

function AtomicIncrement(var Target: Integer): Integer; inline;
begin
  Result := InterlockedIncrement(Target);
end;

function AtomicIncrement(var Target: Integer; const Value: Integer): Integer; inline;
begin
  {$IFDEF MSWINDOWS}
  if Value = 1 then
    Result := InterlockedIncrement(Target)
  else if Value = -1 then
    Result := InterlockedDecrement(Target)
  else
    Result := InterlockedExchangeAdd(Target, Value);
  {$ELSE}
  if Value = 1 then
    Result := TInterlocked.Increment(Target)
  else if Value = -1 then
    Result := TInterlocked.Decrement(Target)
  else
    Result := TInterlocked.Add(Target, Value);
  {$ENDIF}
end;

function AtomicDecrement(var Target: Integer): Integer; inline;
begin
  Result := InterlockedDecrement(Target);
end;

function AtomicExchange(var Target: Integer; Value: Integer): Integer;
begin
  Result := InterlockedExchange(Target, Value);
end;
{$IFEND <XE5}

// 位与，返回原值
function AtomicAnd(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  i: Integer;
begin
  repeat
    Result := Dest;
    i := Result and AMask;
  until AtomicCmpExchange(Dest, i, Result) = Result;
end;

// 位或，返回原值
function AtomicOr(var Dest: Integer; const AMask: Integer): Integer; inline;
var
  i: Integer;
begin
  repeat
    Result := Dest;
    i := Result or AMask;
  until AtomicCmpExchange(Dest, i, Result) = Result;
end;

// 原子加法，返回原值
function AtomicAdd(var Dest: Integer; const AValue: Integer): Integer; inline;
var
  i: Integer;
begin
  repeat
    Result := Dest;
    i := Result + AValue;
  until AtomicCmpExchange(Dest, i, Result) = Result;
end;
{$ENDIF}

{ THashValue }

function THashValue.AsString: string;
begin
  SetLength(Result, Size);
  if Size > 0 then
    Move(Data^, Result[1], Size);
end;

procedure THashValue.Clear;
begin
  Size := 0;
  Data := nil;
end;

{ TStringHash }

procedure TStringHash.Add(const Key: string; Value: Number);
var
  Hash: THashType;
  Bucket: PHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  FLocker.Enter;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  Inc(FCount);
  FLocker.Leave;
end;

procedure TStringHash.AddOrUpdate(const Key: string; Value: Number);
begin
  if not Modify(Key, Value) then
    Add(Key, Value);
end;

procedure TStringHash.Clear;
var
  I: Integer;
  P, N: PHashItem;
begin
  FLocker.Enter;
  for I := 0 to Length(Buckets) - 1 do begin
    P := Buckets[I];
    while P <> nil do begin
      N := P^.Next;
      if Assigned(FOnFreeItem) then
        FOnFreeItem(P);
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
  FCount := 0;
  FLocker.Leave;
end;

constructor TStringHash.Create(Size: Cardinal);
begin
  inherited Create;
  FCount := 0;
  FLocker := TCriticalSection.Create;
  SetLength(Buckets, Size);
end;

destructor TStringHash.Destroy;
begin
  FLocker.Enter;
  try
    Clear;
    inherited Destroy;
  finally
    FLocker.Free;
  end;
end;

function TStringHash.Exists(const Key: string): Boolean;
begin
  FLocker.Enter;
  Result := Find(Key)^ <> nil;
  FLocker.Leave;
end;

function TStringHash.Find(const Key: string): PPHashItem;
var
  Hash: Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TStringHash.GetBucketsCount: Integer;
begin
  Result := Length(Buckets);
end;

procedure TStringHash.GetItems(AList: TList);
var
  P: PHashItem;
  I: Integer;
begin
  if not Assigned(AList) then
    Exit;
  FLocker.Enter;
  try
    for I := 0 to High(Buckets) do begin
      P := Buckets[I];
      while P <> nil do begin
        if Pointer(P.Value) <> nil then
          AList.Add(Pointer(P.Value));
        P := P.Next;
      end;
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TStringHash.Lock;
begin
  FLocker.Enter;
end;

function TStringHash.Modify(const Key: string; Value: Number): Boolean;
var
  P: PHashItem;
begin
  FLocker.Enter;
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    if Assigned(FOnFreeItem) then
      FOnFreeItem(P);
    P^.Value := Value;
  end
  else
    Result := False;
  FLocker.Leave;
end;

procedure TStringHash.Remove(const Key: string);
var
  P: PHashItem;
  Prev: PPHashItem;
begin
  FLocker.Enter;
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Dec(FCount);
    Prev^ := P^.Next;
    if Assigned(FOnFreeItem) then
      FOnFreeItem(P);
    Dispose(P);
  end;
  FLocker.Leave;
end;

procedure TStringHash.UnLock;
begin
  FLocker.Leave;
end;

function TStringHash.ValueOf(const Key: string; const DefaultValue: Number): Number;
var
  P: PHashItem;
begin
  FLocker.Enter;
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := DefaultValue;
  FLocker.Leave;
end;

{ TIntHash }

procedure TIntHash.Add(const Key: THashType; Value: Number);
var
  Hash: THashType;
  Bucket: PIntHashItem;
begin
  Hash := Key mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  FLocker.Enter;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  Inc(FCount);
  FLocker.Leave;
end;

procedure TIntHash.AddOrUpdate(const Key: THashType; Value: Number);
begin
  if not Modify(Key, Value) then
    Add(Key, Value);
end;

procedure TIntHash.Clear;
var
  I: Integer;
  P, N: PIntHashItem;
begin
  FLocker.Enter;
  for I := 0 to Length(Buckets) - 1 do begin
    P := Buckets[I];
    while P <> nil do begin
      N := P^.Next;
      if Assigned(FOnFreeItem) then
        FOnFreeItem(P);
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
  FCount := 0;
  FLocker.Leave;
end;

constructor TIntHash.Create(Size: Cardinal);
begin
  inherited Create;
  FLocker := TCriticalSection.Create;
  SetLength(Buckets, Size);
  FCount := 0;
end;

destructor TIntHash.Destroy;
begin
  FLocker.Enter;
  try
    Clear;
    inherited Destroy;
  finally
    FLocker.Free;
  end;
end;

function TIntHash.Exists(const Key: THashType): Boolean;
begin
  FLocker.Enter;
  Result := Find(Key)^ <> nil;
  FLocker.Leave;
end;

function TIntHash.Find(const Key: THashType): PPIntHashItem;
begin
  Result := @Buckets[Key mod Cardinal(Length(Buckets))];
  while Result^ <> nil do begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TIntHash.GetBucketsCount: Integer;
begin
  Result := Length(Buckets);
end;

procedure TIntHash.GetItems(AList: TList);
var
  P: PIntHashItem;
  I: Integer;
begin
  if not Assigned(AList) then
    Exit;
  FLocker.Enter;
  try
    for I := 0 to High(Buckets) do begin
      P := Buckets[I];
      while P <> nil do begin
        if Pointer(P.Value) <> nil then
          AList.Add(Pointer(P.Value));
        P := P.Next;
      end;
    end;
  finally
    FLocker.Leave;
  end;
end;

procedure TIntHash.Lock;
begin
  FLocker.Enter;
end;

function TIntHash.Modify(const Key: THashType; Value: Number): Boolean;
var
  P: PIntHashItem;
begin
  FLocker.Enter;
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
  FLocker.Leave;
end;

function TIntHash.Remove(const Key: THashType): Boolean;
var
  P: PIntHashItem;
  Prev: PPIntHashItem;
begin
  Result := False;
  FLocker.Enter;
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then begin
    Prev^ := P^.Next;
    Dispose(P);
    Result := True;
    Dec(FCount);
  end;
  FLocker.Leave;
end;

procedure TIntHash.UnLock;
begin
  FLocker.Leave;
end;

function TIntHash.ValueOf(const Key: THashType; const DefaultValue: Number): Number;
var
  P: PIntHashItem;
begin
  FLocker.Enter;
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := DefaultValue;
  FLocker.Leave;
end;

{ THashList }

procedure THashList.Reset;
begin
  Next := nil;
  Data := nil;
end;  

{ THashMapValue }

function THashMapValue.GetNumKey: Number;
begin
  Result := PNumber(@Key)^;
end;

procedure THashMapValue.SetNumKey(const Value: Number);
begin
  PNumber(@Key)^ := THashType(Value);
end;

{ TYXDHashTable }

procedure TYXDHashTable.Add(AData: Pointer; AHash: THashType);
var
  AIndex: Integer;
  ABucket: PHashList;
begin
  ABucket := FPool.Pop;
  ABucket.Hash := AHash;
  ABucket.Data := AData;
  AIndex := AHash mod Cardinal(Length(FBuckets));
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := ABucket;
  Inc(FCount);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
end;

procedure TYXDHashTable.Clear;
var
  I,H: Integer;
  ABucket: PHashList;
begin
  H := High(FBuckets);
  for I := 0 to H do begin
    ABucket := FBuckets[I];
    while ABucket <> nil do begin
      FBuckets[I] := ABucket.Next;
      DoDelete(ABucket.Hash, ABucket.Data);
      FreeBucket(ABucket);
      ABucket := FBuckets[I];
    end;
  end;
  FPool.Clear;
  FCount := 0;
end;

function TYXDHashTable.Compare(Data1, Data2: Pointer;
  var AResult: Integer): Boolean;
begin
  if Assigned(FOnCompare) then begin
    AResult := FOnCompare(Data1, Data2);
    Result := True;
  end else
    Result := False;
end;

constructor TYXDHashTable.Create(ASize: Integer);
begin
  //FPool := THashListPool.Create(8192, SizeOf(THashList));
  FPool := TYxdMemPool.Create(SizeOf(THashList), SizeOf(THashList) shl $D);
  if ASize = 0 then ASize := 17;
  Resize(ASize);
end;

constructor TYXDHashTable.Create;
begin
  Resize(0);
end;

procedure TYXDHashTable.Delete(AData: Pointer; AHash: THashType);
var
  AIndex, ACompare: Integer;
  AHashList, APrior: PHashList;
begin
  AIndex := AHash mod Cardinal(Length(FBuckets));
  AHashList := FBuckets[AIndex];
  APrior := nil;
  while Assigned(AHashList) do begin
    // 同一数据，哈希值我们只能认为是相同，如果不同，找上帝去吧
    if (AHashList.Data=AData) or ((Compare(AHashList.Data,AData,ACompare) and (ACompare=0))) then
    begin
      DoDelete(AHashList.Hash,AHashList.Data);
      if Assigned(APrior) then
        APrior.Next := AHashList.Next
      else
        FBuckets[AIndex] := AHashList.Next; // yangyxd 2014.10.8
      FreeBucket(AHashList);
      Dec(FCount);
      Break;
    end else begin
      APrior := AHashList;
      AHashList := APrior.Next;
    end;
  end;
end;

destructor TYXDHashTable.Destroy;
begin
  Clear;
  FreeAndNil(FPool);
end;

procedure TYXDHashTable.DoDelete(AHash: THashType; AData: Pointer);
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, AHash, AData);
end;

function TYXDHashTable.Exists(AData: Pointer; AHash: THashType): Boolean;
var
  AList: PHashList;
  AResult: Integer;
begin
  AList := FindFirst(AHash);
  Result := False;
  while AList <> nil do begin
    if (AList.Data = AData) or (Compare(AList.Data,AData,AResult) and (AResult=0)) then begin
      Result:=True;
      Break;
    end;
    AList := FindNext(AList);
  end;
end;

function TYXDHashTable.Find(AHash: THashType): PHashList;
var
  AIndex: Integer;
  AList, AItem: PHashList;
begin
  AIndex := AHash mod Cardinal(Length(FBuckets));
  Result := nil;
  AList := FBuckets[AIndex];
  while AList <> nil do begin
    if AList.Hash = AHash then begin
      New(AItem);
      AItem.Data := AList.Data;
      AItem.Next := Result;
      AItem.Hash := AHash;
      Result := AItem;
    end;
    AList := AList.Next;
  end;
end;

function TYXDHashTable.Find(AData: Pointer; AHash: THashType): Pointer;
var
  ACmpResult: Integer;
  AList: PHashList;
begin
  Result := nil;
  AList := FindFirst(AHash);
  while AList<>nil do begin
    if (AList.Data = AData) or (Compare(AData, AList.Data, ACmpResult) and (ACmpResult=0)) then begin
      Result := AList.Data;
      Break;
    end;
    AList := AList.Next;
  end;
end;

function TYXDHashTable.FindFirst(AHash: THashType): PHashList;
var
  AIndex: Integer;
  AList: PHashList;
begin
  AIndex := AHash mod Cardinal(Length(FBuckets));
  Result := nil;
  AList := FBuckets[AIndex];
  while AList <> nil do begin
    if AList.Hash = AHash then begin
      Result := AList;
      Break;
    end;
    AList := AList.Next;
  end;
end;

function TYXDHashTable.FindFirstData(AHash: THashType): Pointer;
begin
  Result := FindFirst(AHash);
  if Result <> nil then
    Result := PHashList(Result).Data;
end;

function TYXDHashTable.FindNext(AList: PHashList): PHashList;
begin
  Result := nil;
  if Assigned(AList) then begin
    Result := AList.Next;
    while Result<>nil do begin
      if Result.Hash=AList.Hash then
        Break
      else
        Result := Result.Next;
    end;
  end;
end;

procedure TYXDHashTable.FreeBucket(var ABucket: PHashList);
begin
  FPool.Push(ABucket);
end;

procedure TYXDHashTable.FreeHashList(AList: PHashList);
var
  ANext: PHashList;
begin
  while AList<>nil do begin
    ANext := AList.Next;
    FreeBucket(AList);
    AList := ANext;
  end;
end;

function TYXDHashTable.GetBucketCount: Integer;
begin
  Result := Length(FBuckets);
end;

function TYXDHashTable.GetBuckets(AIndex: Integer): PHashList;
begin
  Result := FBuckets[AIndex];
end;

function TYXDHashTable.GetMemSize: Int64;
begin
  Result := FPool.Size;
  Inc(Result, Length(FBuckets) shl 2);
end;

procedure TYXDHashTable.Resize(ASize: Cardinal);
var
  I, AIndex: Integer;
  AHash: Cardinal;
  ALastBuckets: THashArray;
  AList, ANext: PHashList;
begin
  if ASize = 0 then begin
    ASize := CalcBucketSize(FCount);
    if ASize = Cardinal(Length(FBuckets)) then
      Exit;
  end;

  //桶尺寸变更后，重新分配元素所在的哈希桶，如果是自动调用的话，理想的结果就是一个桶有一个元素
  if ASize <> Cardinal(Length(FBuckets)) then begin
    ALastBuckets := FBuckets;
    SetLength(FBuckets, ASize);
    for I := 0 to ASize-1 do
      FBuckets[I] := nil;
    for I := 0 to High(ALastBuckets) do begin
      AList := ALastBuckets[I];
      while AList<>nil do begin
        AHash := AList.Hash;
        AIndex := AHash mod ASize;
        ANext := AList.Next;
        AList.Next := FBuckets[AIndex];
        FBuckets[AIndex] := AList;
        AList := ANext;
      end;
    end;
  end;
end;

procedure TYXDHashTable.SetAutoSize(const Value: Boolean);
begin
  if FAutoSize <> Value then begin
    FAutoSize := Value;
    if AutoSize then begin
      if (FCount div Length(FBuckets)) > 3 then
        Resize(0);
    end;
  end;
end;

procedure TYXDHashTable.Update(AData: Pointer; AOldHash, ANewHash: THashType);
var
  AList, APrior: PHashList;
  ACmpResult: Integer;
  AIndex: Integer;
  AChanged: Boolean;
begin
  AChanged := False;
  AIndex := AOldHash mod Cardinal(Length(FBuckets));
  AList := FBuckets[AIndex];
  APrior := nil;
  while AList <> nil do begin
    if (AList.Hash = AOldHash) then begin
      if (AList.Data=AData) or (Compare(AData, AList.Data, ACmpResult) and (ACmpResult=0)) then begin
        if Assigned(APrior) then
          APrior.Next := AList.Next
        else
          FBuckets[AIndex] := AList.Next;
        AList.Hash := ANewHash;
        AIndex := ANewHash mod Cardinal(Length(FBuckets));
        AList.Next := FBuckets[AIndex];
        FBuckets[AIndex] := AList;
        AChanged := True;
        Break;
      end;
    end;
    APrior := AList;
    AList := AList.Next;
  end;
  if not AChanged then
    Add(AData, ANewHash);
end;

{ TYXDHashMapTable }
  
procedure TYXDHashMapTable.Add(const Key: string; AData: PHashValue);
var
  AIndex: THashType;
  ABucket: PHashMapList;
begin
  AIndex := HashOf(Key);
  ABucket := Pointer(FListPool.Pop);
  ABucket.Hash := AIndex;
  AIndex := AIndex mod Cardinal(Length(FBuckets));
  ABucket.Data := Pointer(NativeUInt(ABucket) + SizeOf(THashMapList));
  Initialize(ABucket.Data.Key);
  if AData <> nil then   
    ABucket.Data.Value := AData^
  else
    ABucket.Data.Value.Clear;
  ABucket.Data.IsStrKey := True;
  ABucket.Data.Key := Key;
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := Pointer(ABucket);
  Inc(FCount);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
  DoAdd(ABucket);
end;

procedure TYXDHashMapTable.Add(const Key: Number; AData: PHashValue);
var
  AIndex: THashType;
  ABucket: PHashMapList;
begin
  ABucket := Pointer(FListPool.Pop);
  ABucket.Hash := THashType(Key);
  AIndex := THashType(Key) mod Cardinal(Length(FBuckets));
  ABucket.Data := Pointer(NativeUInt(ABucket) + SizeOf(THashMapList));
  if AData <> nil then   
    ABucket.Data.Value := AData^
  else
    ABucket.Data.Value.Clear;
  ABucket.Data.IsStrKey := False;
  PNumber(@ABucket.Data.Key)^ := THashType(Key);
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := Pointer(ABucket);
  Inc(FCount);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
  DoAdd(ABucket);
end;

procedure TYXDHashMapTable.Add(const Key: string; AData: NativeInt);
var
  AIndex: THashType;
  ABucket: PHashMapList;
begin
  AIndex := HashOf(Key);
  ABucket := Pointer(FListPool.Pop);
  ABucket.Hash := AIndex;
  AIndex := AIndex mod Cardinal(Length(FBuckets));
  ABucket.Data := Pointer(NativeUInt(ABucket) + SizeOf(THashMapList));
  Initialize(ABucket.Data.Key);
  ABucket.Data.Value.Data := Pointer(AData);
  ABucket.Data.Value.Size := 0;
  ABucket.Data.IsStrKey := True;
  ABucket.Data.Key := Key;
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := Pointer(ABucket);
  Inc(FCount);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
  DoAdd(ABucket);
end;

procedure TYXDHashMapTable.Add(const Key: Number; AData: NativeInt);
var
  AIndex: THashType;
  ABucket: PHashMapList;
begin
  ABucket := Pointer(FListPool.Pop);
  ABucket.Hash := THashType(Key);
  AIndex := THashType(Key) mod Cardinal(Length(FBuckets));
  ABucket.Data := Pointer(NativeUInt(ABucket) + SizeOf(THashMapList));
  ABucket.Data.Value.Data := Pointer(AData);
  ABucket.Data.Value.Size := 0;
  ABucket.Data.IsStrKey := False;
  PDWORD(@ABucket.Data.Key)^ := THashType(Key);
  ABucket.Next := FBuckets[AIndex];
  FBuckets[AIndex] := Pointer(ABucket);
  Inc(FCount);
  {$IFDEF AUTORESIE}
  if (FCount div Length(FBuckets)) > 3 then
    Resize(0);
  {$ENDIF}
  DoAdd(ABucket);
end;

procedure TYXDHashMapTable.Clear;
var
  I: Integer;
  P, N: PHashList;
begin
  for I := 0 to High(FBuckets) do begin
    P := FBuckets[I];
    FBuckets[I] := nil;
    while P <> nil do begin
      N := P^.Next;
      DoDelete(P.Hash, P.Data);
      FreeBucket(P);
      P := N;
    end;
  end;
  FCount := 0;
  FListPool.Clear;
  FPool.Clear;
end;

constructor TYXDHashMapTable.Create(ASize: Integer);
var
  HASHITEMSize: Integer;
begin
  inherited;
  HASHITEMSize := SizeOf(THashMapList) + SizeOf(THashMapValue);
  FListPool := TYxdMemPool.Create(HASHITEMSize, HASHITEMSize shl $D);
end;

destructor TYXDHashMapTable.Destroy;
begin
  inherited;
  FreeAndNil(FListPool);
end;

procedure TYXDHashMapTable.DoAdd(ABucket: PHashMapList);
begin
end;

function TYXDHashMapTable.Exists(const Key: Number): Boolean;
begin
  Result := Find(Key) <> nil;
end;

function TYXDHashMapTable.Exists(const Key: string): Boolean;
begin
  Result := Find(Key) <> nil;
end;

function TYXDHashMapTable.Find(const Key: string): PHashMapValue;
var
  AList: PHashList;
  AHash: Cardinal;
begin
  AHash := HashOf(Key);
  AList := FBuckets[AHash mod Cardinal(Length(FBuckets))];
  while AList <> nil do begin
    if (AList.Hash = AHash) and (PHashMapValue(AList.Data).IsStrKey) and
      (PHashMapValue(AList.Data).Key = Key) then begin
      Result := AList.Data;
      Exit;
    end;
    AList := AList.Next;
  end;
  Result := nil;
end;

function TYXDHashMapTable.Find(const Key: Number): PHashMapValue;
var
  AList: PHashList;
  AHash: THashType;
begin
  AHash := THashType(Key);
  AList := FBuckets[AHash mod Cardinal(Length(FBuckets))];
  while AList <> nil do begin
    if (AList.Hash = AHash) and (not PHashMapValue(AList.Data).IsStrKey) then begin
      Result := AList.Data;
      Exit;
    end;
    AList := AList.Next;
  end;
  Result := nil;
end;

function TYXDHashMapTable.FindList(const Key: Number): PPHashList;
begin
  Result := @FBuckets[THashType(Key) mod Cardinal(Length(FBuckets))];
  while Result^ <> nil do begin
    if (Result^.Hash = THashType(Key)) and (not PHashMapValue(Result^.Data).IsStrKey) then
      Break;
    Result := @Result^.Next;
  end;
end;

function TYXDHashMapTable.FindList(const Key: string): PPHashList;
var
  AHash: Cardinal;
begin
  AHash := HashOf(Key);
  Result := @FBuckets[AHash mod Cardinal(Length(FBuckets))];
  while Result^ <> nil do begin
    if (Result^.Hash = AHash) and (PHashMapValue(Result^.Data).IsStrKey) and
      (PHashMapValue(Result^.Data).Key = Key) then begin
      Break;
    end;
    Result := @Result^.Next;
  end;
end;

procedure TYXDHashMapTable.FreeBucket(var ABucket: PHashList);
begin
  if PHashMapList(ABucket).Data.IsStrKey then  
    Finalize(PHashMapList(ABucket).Data.Key);
  FListPool.Push(ABucket);
end;

function TYXDHashMapTable.GetMemSize: Int64;
begin
  Result := inherited GetMemSize;
  Inc(Result, FListPool.Size);
end;

function TYXDHashMapTable.Remove(const Key: Number): Boolean;
var
  AIndex: Integer;
  AHash: THashType;
  AHashList, APrior: PHashList;
begin
  Result := False;
  AHash := THashType(Key);
  AIndex := AHash mod Cardinal(Length(FBuckets));
  AHashList := FBuckets[AIndex];
  APrior := nil;
  while Assigned(AHashList) do begin
    if (AHashList.Hash = AHash) and (not PHashMapValue(AHashList.Data).IsStrKey) then begin
      DoDelete(AHashList.Hash, AHashList.Data);
      if Assigned(APrior) then
        APrior.Next := AHashList.Next
      else
        FBuckets[AIndex] := AHashList.Next;
      FreeBucket(AHashList);
      Dec(FCount);
      Result := True;
      Break;
    end else begin
      APrior := AHashList;
      AHashList := APrior.Next;
    end;
  end;
end;

function TYXDHashMapTable.Update(const Key: string;
  Value: PHashValue): Boolean;
var
  P: PHashMapValue;
begin
  P := Find(Key);
  if P <> nil then begin
    if Value <> nil then
      P.Value := Value^
    else
      P.Value.Clear;
    Result := True;
  end else
    Result := False;
end;

function TYXDHashMapTable.Remove(const Key: string): Boolean;
var
  AIndex: Integer;
  AHash: Cardinal;
  AHashList, APrior: PHashList;
begin
  Result := False;
  AHash := HashOf(Key);
  AIndex := AHash mod Cardinal(Length(FBuckets));
  AHashList := FBuckets[AIndex];
  APrior := nil;
  while Assigned(AHashList) do begin
    if (AHashList.Hash = AHash) and (PHashMapValue(AHashList.Data).Key = Key) then begin
      DoDelete(AHashList.Hash, AHashList.Data);
      if Assigned(APrior) then
        APrior.Next := AHashList.Next
      else
        FBuckets[AIndex] := AHashList.Next;
      FreeBucket(AHashList);
      Dec(FCount);
      Result := True;
      Break;
    end else begin
      APrior := AHashList;
      AHashList := APrior.Next;
    end;
  end;
end;

function TYXDHashMapTable.ValueOf(const Key: string): PHashValue;
var
  P: PHashMapValue;
begin
  P := Find(Key);
  if (P <> nil) then // and (P.Value.Size > 0) then
    Result := @P.Value
  else
    Result := nil;
end;

function TYXDHashMapTable.ValueOf(const Key: Number): PHashValue;
var
  P: PHashMapValue;
begin
  P := Find(Key);
  if (P <> nil) then // and (P.Value.Size > 0) then
    Result := @P.Value
  else
    Result := nil;
end;

function TYXDHashMapTable.Update(const Key: Number; Value: PHashValue): Boolean;
var
  P: PHashMapValue;
begin
  P := Find(Key);
  if P <> nil then begin
    if Value <> nil then
      P.Value := Value^
    else
      P.Value.Clear;
    Result := True;
  end else
    Result := False;  
end;

function TYXDHashMapTable.Remove(const P: PHashMapValue): Boolean;
begin
  if P <> nil then begin
    if P.IsStrKey then
      Result := Remove(P.Key)
    else
      Result := Remove(P.GetNumKey)
  end else
    Result := False;
end;

{ TYXDHashMapList }

procedure TYXDHashMapList.Clear;
begin
  FList.Clear;
  inherited; 
end;

constructor TYXDHashMapList.Create(ASize: Integer);
begin
  inherited;
  FList := TList.Create;
end;

procedure TYXDHashMapList.Delete(Index: Integer);
begin
  if (index >= 0) and (Index < FCount) then
    Remove(Items[index].Key);
end;

destructor TYXDHashMapList.Destroy;
begin
  inherited;
  FreeAndNil(FList);
end;

procedure TYXDHashMapList.DoAdd(ABucket: PHashMapList);
begin
  FList.Add(ABucket.Data);  
end;

procedure TYXDHashMapList.DoDelete(AHash: THashType; AData: Pointer);
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, AHash, AData);
  if FList.Count > 0 then   
    FList.Remove(AData);
end;

function TYXDHashMapList.GetItem(Index: Integer): PHashMapValue;
begin
  Result := FList.Items[index];
end; 

{ TYXDHashMapLinkTable }

procedure TYXDHashMapLinkTable.Clear;
begin
  FreeLinkList;
  inherited Clear;
end;

constructor TYXDHashMapLinkTable.Create(ASize: Integer);
begin
  inherited;
  FFirst := nil;
  FLast := nil;
  FLinkHashPool := TYxdMemPool.Create(SizeOf(THashList), SizeOf(THashList) shl $D);
  SetLength(ListBuckets, ASize);
end;

procedure TYXDHashMapLinkTable.Delete(Index: Integer);
var
  P: PHashMapValue;
begin
  P := GetItem(Index);
  if P <> nil then
    Remove(P.Key);
end;

destructor TYXDHashMapLinkTable.Destroy;
begin
  inherited;
  FreeAndNil(FLinkHashPool);
end;

procedure TYXDHashMapLinkTable.DoAdd(ABucket: PHashMapList);
var
  AIndex: Integer;
  AItem: PHashList;
  P: PHashMapLinkItem;
begin
  P := Pointer(FPool.Pop);
  P.Value := ABucket.Data;
  if FFirst = nil then begin
    P.Prev := nil;
    FFirst := P;
    FLast := FFirst;
  end else begin
    P.Prev := FLast;
    FLast.Next := P;
    FLast := FLast.Next;
  end;
  FLast.Next := nil;
  
  // 添加到Hash表中
  AIndex := NativeUInt(ABucket.Data) mod Cardinal(Length(ListBuckets));
  AItem := ListBuckets[AIndex];
  while AItem <> nil do begin
    if AItem.Hash = NativeUInt(ABucket.Data) then begin
      AItem.Data := FLast;
      Exit
    end else
      AItem := AItem.Next;
  end;
  AItem := FLinkHashPool.Pop;
  AItem^.Hash := THashType(ABucket.Data);
  AItem^.Data := FLast;
  AItem^.Next := ListBuckets[AIndex];
  ListBuckets[AIndex] := AItem;
end;

procedure TYXDHashMapLinkTable.DoDelete(AHash: THashType; AData: Pointer);
var
  P: PHashMapLinkItem;
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, AHash, AData);
  P := FindLinkItem(AData, True);
  if P = nil then Exit;
  if P = FFirst then begin
    FFirst := FFirst.Next;
    if FFirst = nil then
      FLast := nil
    else
      FFirst.Prev := nil;
  end else if P = FLast then begin
    FLast := P.Prev;
    if FLast = nil then
      FFirst := nil
    else
      FLast.Next := nil;
  end else begin
    P.Prev.Next := P.Next;
    P.Next.Prev := P.Prev;
  end;
  FPool.Push(Pointer(P));
end;

function TYXDHashMapLinkTable.FindLinkItem(AData: Pointer;
  isDelete: Boolean): PHashMapLinkItem;
var
  P, P1: PHashList;
  Prev: PPHashList;
begin
  Prev := @ListBuckets[NativeUInt(AData) mod Cardinal(Length(ListBuckets))];
  P := Prev^;
  P1 := nil;
  Result := nil;
  while P <> nil do begin
    if PHashMapLinkItem(P.Data).Value = AData then begin
      Result := P.Data;
      if isDelete then begin
        if P1 = nil then
          Prev^ := nil
        else
          P1.Next := P.Next;
        FLinkHashPool.Push(P);
      end;
      Break;
    end else begin
      P1 := P;
      P := P.Next;
    end;
  end;
end;

procedure TYXDHashMapLinkTable.FreeLinkList;
var
  P, N: PHashMapLinkItem;
begin
  P := FFirst;
  while P <> nil do begin
    N := P.Next;
    FPool.Push(Pointer(P));
    P := N;
  end;

  if Length(ListBuckets) > 0 then
    FillChar(ListBuckets[0], Length(ListBuckets) * SizeOf(PHashList), 0);

  FLinkHashPool.Clear;
  FFirst := nil;
  FLast := nil;
end;

function TYXDHashMapLinkTable.GetEnumerator: TYXDHashMapLinkTableEnumerator;
begin
  Result := TYXDHashMapLinkTableEnumerator.Create(Self);
end;

function TYXDHashMapLinkTable.GetItem(Index: Integer): PHashMapValue;
var
  P: PHashMapLinkItem;
  I: Integer;
begin
  if Index > (FCount shr 1) then begin
    if Index < FCount then begin
      P := FLast;
      if P <> nil then begin
        for I := FCount - Index - 1 downto 1 do
          P := P.Prev;
        Result := P.Value;
        Exit;
      end;
    end;
  end else if Index > -1 then begin
    P := FFirst;
    if P <> nil then begin
      for I := 0 to Index - 1 do
        P := P.Next;
      Result := P.Value;
      Exit;
    end;
  end;
  Result := nil;
end; 

function TYXDHashMapLinkTable.GetLast: PHashMapValue;
begin
  if FLast <> nil then
    Result := FLast.Value
  else
    Result := nil;
end;

function TYXDHashMapLinkTable.GetMemSize: Int64;
begin
  Result := inherited GetMemSize;
  Inc(Result, Length(ListBuckets) shl 2);
  Inc(Result, FLinkHashPool.Size);
end;

{ TYXDHashMapLinkTableEnumerator }

constructor TYXDHashMapLinkTableEnumerator.Create(AList: TYXDHashMapLinkTable);
begin
  FItem := AList.FFirst;
end;

function TYXDHashMapLinkTableEnumerator.GetCurrent: PHashMapLinkItem;
begin
  Result := FItem;
  FItem := FItem.Next;
end;

function TYXDHashMapLinkTableEnumerator.MoveNext: Boolean;
begin
  Result := FItem <> nil;
end;

end.


