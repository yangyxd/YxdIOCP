{*******************************************************}
{                                                       }
{       iocp.Utils.Dictionary    KeyValue表             }
{                                                       }
{       版权所有 (C) 2013 YangYxd                       }
{                                                       }
{*******************************************************}
{
 --------------------------------------------------------------------
  说明
 --------------------------------------------------------------------
  版本归YangYxd所有，保留一切权利

 --------------------------------------------------------------------
  Dictionary 示例
 --------------------------------------------------------------------
  Map: Dictionary;
  // 增加数据,
  // Key 可以是字符串、整数、浮点数
  // Value 可以常规类型：字符串、数字、浮点数、布尔值、字节数组
  Map.Add('StringKey', 'abc');
  Map.Add(12, 'abc');
  Map.Add(55, 99.556);
  Map.Add(12.145, 666);
  Map.Add('String', 100);
  Map['Key'] := 123;
  Map['Key'] := 'Value';
  Map[10] := 123;
  // 更新
  Map['Key'] = Value
  Map[123] = Value
  Map.Values['Key'].AsString := 'Value';
  // 读取
  V = Map[Key].AsInteger;
  V = Map[Key].AsString;
  V = Map.GetInt(Key);
  V = Map.GetString(Key);
  V = Map.Items[Index].AsString;
  V = Map.Values[Key].AsFloat;
  // 检测
  Boolean = Map.Exist(Key);
  Boolean = Map.ContainsKey(Key);
  Map[Key] = nil;
  Map[Key] <> nil;
  // 删除
  Map.Remove(Key);
  Map.Delete(Index);
  ...
  
 2016.05.17 ver 1.0.0
 --------------------------------------------------------------------
  - 使用 KeyValue 形式存放数据.
  - Key 可以是字符串，可以是整数
 --------------------------------------------------------------------
}

unit iocp.Utils.Dictionary;

interface

uses
  iocp.Utils.Hash, iocp.Utils.Str,
  Windows, SysUtils, Classes, Types, SyncObjs, Math, Variants;

type
  MString = string;
  PMapChar = PChar;
  MapChar = Char;

type
  /// <summary>
  /// Key类型  (暂时只支持这两种)
  /// </summary>
  MapKeyType = (mktString {字符串型Key}, mktInteger {数值型Key}, mktFloat {浮点数型});

type
  /// <summary>
  /// 数据类型
  /// </summary>
  MapDataType = (mdtNull, mdtString, mdtInteger, mdtDWORD, mdtFloat,
    mdtBoolean, mdtDateTime);

type
  /// <summary>
  /// Key 值
  /// </summary>
  PMapKey = ^MapKey;
  MapKey = packed record
  private
    FStrValue: MString;
    FIntValue: Int64;
    function GetAsInteger: Number;
    function GetAsString: MString;
    procedure SetAsInteger(const Value: Number);
    procedure SetAsString(const Value: MString);
    function GetAsFloat: Double;
    procedure SetAsFloat(const Value: Double);
    procedure InitHashCode();
  public
    KeyType: MapKeyType;

    constructor Create(const V: MString); overload;
    constructor Create(const V: Number); overload;
    constructor Create(const V: Double); overload;

    procedure Clear();

    function ToString(): MString;
    function GetHashCode: THashType;
    function IsEmpty: Boolean; inline;

    {重载操作符}
    // 重载 :=
    class operator Implicit(const Value: MString): MapKey;
    class operator Implicit(const Value: Number): MapKey;
    class operator Implicit(const Value: Double): MapKey;

    // 重载 =
    class operator Equal(const A: MapKey; const B: MapKey): Boolean;
    class operator Equal(const A: MString; const B: MapKey): Boolean;
    class operator Equal(const A: Number; const B: MapKey): Boolean;
    class operator Equal(const A: Double; const B: MapKey): Boolean;
    
    // 重载 !=
    class operator NotEqual(const a: MapKey; const b: MapKey): Boolean;

    property AsInteger: Number read GetAsInteger write SetAsInteger;
    property AsString: MString read GetAsString write SetAsString;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
  end;

type
  /// <summary>
  /// Value 值
  /// </summary>
  PMapValue = ^MapValue;
  MapValue = packed record
  private
    FDataType: MapDataType;
    Value: TBytes;
    function ValueAsDateTime(const DateFormat, TimeFormat, DateTimeFormat: MString): MString;
    function GetAsBoolean: Boolean;
    function GetAsByte: Byte;
    function GetAsDouble: Double;
    function GetAsFloat: Single;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsString: MString;
    function GetAsVariant: Variant;
    function GetAsWord: Word;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsByte(const Value: Byte);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsFloat(const Value: Single);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: MString);
    procedure SetAsVariant(const Value: Variant);
    procedure SetAsWord(const Value: Word);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsDWORD(const Value: Cardinal);
    function GetSize: Cardinal; inline;
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const Value: TBytes);
    function GetAsInt32: Cardinal;
    function GetIsNull: Boolean;
    function GetAsNativeInt: NativeInt;
    procedure SetAsNativeInt(const Value: NativeInt);
  public
    function ToString: MString;
    function GetString: string;
    procedure CopyValue(ASource: PMapValue); inline;

    procedure Clear();

    // 是否为空
    property IsNull: Boolean read GetIsNull; 
    
    {重载操作符}
    // 重载 :=
    class operator Implicit(const Value: MString): MapValue;
    class operator Implicit(const Value: Boolean): MapValue;
    class operator Implicit(const Value: Byte): MapValue;
    class operator Implicit(const Value: TBytes): MapValue;
    class operator Implicit(const Value: Word): MapValue;
    class operator Implicit(const Value: Integer): MapValue;
    class operator Implicit(const Value: Int64): MapValue;
    class operator Implicit(const Value: Cardinal): MapValue;
    class operator Implicit(const Value: Single): MapValue;
    class operator Implicit(const Value: Double): MapValue;

    class operator Implicit(const Value: MapValue): PMapValue;
    class operator Implicit(const Value: MapValue): MString;
    class operator Implicit(const Value: MapValue): Int64;
    class operator Implicit(const Value: MapValue): Double;

    // +
    class operator Add(const A: MapValue; const B: Integer): Integer;
    class operator Add(const A: MapValue; const B: Cardinal): Cardinal;
    class operator Add(const A: MapValue; const B: Int64): Int64;
    class operator Add(const A: MapValue; const B: Double): Double;
    class operator Add(const A: MapValue; const B: MString): MString;

    class operator Add(const A: MString; const B: MapValue): MString;
    class operator Add(const A: Double; const B: MapValue): Double;
    class operator Add(const A: Integer; const B: MapValue): Integer;
    class operator Add(const A: Cardinal; const B: MapValue): Cardinal;
    class operator Add(const A: Int64; const B: MapValue): Int64;

    // -
    class operator Subtract(const A: MapValue; const B: Integer): Integer;
    class operator Subtract(const A: MapValue; const B: Int64): Int64;
    class operator Subtract(const A: MapValue; const B: Cardinal): Cardinal;
    class operator Subtract(const A: MapValue; const B: Double): Double;

    class operator Subtract(const A: Integer; const B: MapValue): Integer;
    class operator Subtract(const A: Int64; const B: MapValue): Int64;
    class operator Subtract(const A: Cardinal; const B: MapValue): Cardinal;
    class operator Subtract(const A: Double; const B: MapValue): Double;

    // *
    class operator Multiply(const A: MapValue; const B: Integer): Integer;
    class operator Multiply(const A: MapValue; const B: Int64): Int64;
    class operator Multiply(const A: MapValue; const B: Cardinal): Cardinal;
    class operator Multiply(const A: MapValue; const B: Double): Double;

    class operator Multiply(const A: Integer; const B: MapValue): Integer;
    class operator Multiply(const A: Int64; const B: MapValue): Int64;
    class operator Multiply(const A: Cardinal; const B: MapValue): Cardinal;
    class operator Multiply(const A: Double; const B: MapValue): Double;

    // /
    class operator Divide(const A: MapValue; const B: Integer): Double;
    class operator Divide(const A: MapValue; const B: Int64): Double;
    class operator Divide(const A: MapValue; const B: Cardinal): Double;
    class operator Divide(const A: MapValue; const B: Double): Double;

    class operator Divide(const A: Integer; const B: MapValue): Double;
    class operator Divide(const A: Int64; const B: MapValue): Double;
    class operator Divide(const A: Cardinal; const B: MapValue): Double;
    class operator Divide(const A: Double; const B: MapValue): Double;

    // =
    class operator Equal(const A: MapValue; const B: Integer): Boolean;
    class operator Equal(const A: MapValue; const B: Int64): Boolean;
    class operator Equal(const A: MapValue; const B: Cardinal): Boolean;
    class operator Equal(const A: MapValue; const B: Double): Boolean;
    class operator Equal(const A: MapValue; const B: Pointer): Boolean;

    class operator Equal(const A: Integer; const B: MapValue): Boolean;
    class operator Equal(const A: Int64; const B: MapValue): Boolean;
    class operator Equal(const A: Cardinal; const B: MapValue): Boolean;
    class operator Equal(const A: Double; const B: MapValue): Boolean;
    class operator Equal(const A: Pointer; const B: MapValue): Boolean;

    // <>
    class operator NotEqual(const A: MapValue; const B: Integer): Boolean;
    class operator NotEqual(const A: MapValue; const B: Int64): Boolean;
    class operator NotEqual(const A: MapValue; const B: Cardinal): Boolean;
    class operator NotEqual(const A: MapValue; const B: Double): Boolean;
    class operator NotEqual(const A: MapValue; const B: Pointer): Boolean;

    class operator NotEqual(const A: Integer; const B: MapValue): Boolean;
    class operator NotEqual(const A: Int64; const B: MapValue): Boolean;
    class operator NotEqual(const A: Cardinal; const B: MapValue): Boolean;
    class operator NotEqual(const A: Double; const B: MapValue): Boolean;
    class operator NotEqual(const A: Pointer; const B: MapValue): Boolean;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsByte: Byte read GetAsByte write SetAsByte;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    property AsWord: Word read GetAsWord write SetAsWord;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsDWORD: Cardinal read GetAsInt32 write SetAsDWORD;
    property AsFloat: Single read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsString: MString read GetAsString write SetAsString;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
    property AsNativeInt: NativeInt read GetAsNativeInt write SetAsNativeInt;

    property Size: Cardinal read GetSize;
    property DataType: MapDataType read FDataType;
  end;

type
  PMapEntry = ^MapEntry;
  MapEntry = record
    Key: MapKey;
    Value: MapValue;   
    function ToString(const ADelimiter: MString = ':'): MString;
  end;
  MapEntryList = array of PMapEntry;

type
  PPMapHashItem = ^PMapHashItem;
  PMapHashItem = ^TMapHashItem;
  TMapHashItem = record
    Next: PMapHashItem;
    Value: PMapEntry;
  end;

type
  MapList = class(TObject)
  private
    Buckets: array of PMapHashItem;
    FCount: Integer;
    function GetBucketsIndex(const Index: Integer): PMapHashItem;
    function GetItem(const Index: Integer): PMapEntry; inline;
    procedure SetItem(const Index: Integer; const Value: PMapEntry); inline;
    procedure DoOutOfBoundsError(const Index: Integer);
  protected
    function Find(const Key: PMapKey): PPMapHashItem; overload;
    function Find(const Key: MString): PPMapHashItem; overload;
    function Find(const Key: Number): PPMapHashItem; overload;
    function Find(const Key: Double): PPMapHashItem; overload;
    function RemoveItem(const Value: PPMapHashItem): Boolean; inline;
  public
    constructor Create(Size: Cardinal = 331);
    destructor Destroy; override;
    procedure Clear; 

    procedure Add(const Value: PMapEntry); overload;
    
    function Force(const Value: PMapEntry): PMapEntry; overload;
    function Force(const Value: MapKey): PMapEntry; overload;
    function Force(const Value: MString): PMapEntry; overload;
    function Force(const Value: Number): PMapEntry; overload;
    function Force(const Value: Double): PMapEntry; overload;

    function Delete(Index: Integer): Boolean; overload;

    function Remove(const Key: PMapKey): Boolean; overload;
    function Remove(const Key: MString): Boolean; overload;
    function Remove(const Key: Number): Boolean; overload;
    function Remove(const Key: Double): Boolean; overload;

    function Exists(const Key: PMapKey): Boolean; overload;
    function Exists(const Key: MString): Boolean; overload;
    function Exists(const Key: Number): Boolean; overload;
    function Exists(const Key: Double): Boolean; overload;

    function ValueOf(const Key: PMapKey): PMapEntry; overload;
    function ValueOf(const Key: MString): PMapEntry; overload;
    function ValueOf(const Key: Number): PMapEntry; overload;
    function ValueOf(const Key: Double): PMapEntry; overload;

    property Items[const Index: Integer]: PMapEntry read GetItem write SetItem; default;
    property Count: Integer read FCount;
  end;

type
  DictionaryEnumerator = class;

  MapBase = class(TObject)
  protected
    procedure WriteByte(avOut: TStream; avData: Byte);
    function ReadByte(avIn: TStream): Byte; 
    procedure WriteCardinal(avOut: TStream; avData: Cardinal);
    function ReadCardinal(avIn: TStream): Cardinal;
    procedure WriteInt64(avOut: TStream; avData: Int64);
    function ReadInt64(avIn: TStream): Int64;
    procedure WriteNumber(avOut: TStream; avData: Number);
    function ReadNumber(avIn: TStream): Number;
    procedure WriteString(avOut: TStream; const avData: MString);
    function ReadString(avIn: TStream): MString;

    procedure WriteMapEntry(avOut: TStream; V: PMapEntry);
    function ReadMapEntry(avIn: TStream; var avOut: MapEntry): Boolean;
  public
    procedure SaveToFile(const AFileName: MString);
    procedure LoadFromFile(const AFileName: MString);
    procedure SaveToStream(AStream: TStream); virtual; abstract;
    procedure LoadFromStream(AStream: TStream); virtual; abstract;
  end;
  
  /// <summary>
  /// 字典
  /// </summary>
  Dictionary = class(MapBase)
  private
    FItems: MapList;
    procedure SetItem(Index: Integer; const Value: PMapEntry);
    procedure _SetValue(const Key: MapKey; const Value: MapValue);
    function _GetValue(const Key: MapKey): MapValue; overload;
  protected
    function AddItem(const Key: MapKey): PMapEntry; overload; inline;
    function AddItem(const Key: MString): PMapEntry; overload; inline;
    function AddItem(const Key: Number): PMapEntry; overload; inline;
    function AddItem(const Key: Double): PMapEntry; overload; inline;

    function GetValueItem(const Key: MapKey): PMapValue; overload;
    function GetValueItem(const Key: MString): PMapValue; overload;
    function GetValueItem(const Key: Number): PMapValue; overload;
    function GetValueItem(const Key: Double): PMapValue; overload;

    property DefaultValues[const Key: MapKey]: MapValue read _GetValue write _SetValue; default;
  public
    constructor Create(Size: Cardinal = 331); virtual;
    destructor Destroy; override;

    procedure SaveToStream(AStream: TStream); override;
    procedure LoadFromStream(AStream: TStream); override;

    // 设置Key是否大小写敏感
    class procedure SetMapCaseSensitive(v: Boolean);

    /// <summary>
    /// 清除所有的映射关系
    /// </summary>
    procedure Clear(); virtual;

    /// <summary>
    /// 返回全部条目数组
    /// </summary>
    function GetEntryList(): MapEntryList;
    
    /// <summary>
    /// 检测是否包含指定键的映射关系
    /// </summary>
    function Exist(const Key: MapKey): Boolean; overload;
    function Exist(const Key: Number): Boolean; overload;
    function Exist(const Key: MString): Boolean; overload;
    function Exist(const Key: Double): Boolean; overload;

    /// <summary>
    /// 检测是否包含指定键的映射关系
    /// </summary>
    function ContainsKey(const Key: MapKey): Boolean; virtual;

    /// <summary>获取for..in需要的GetEnumerator支持</summary>
    function GetEnumerator: DictionaryEnumerator;

    /// <summary>
    /// 获取一个条目的数据
    /// </summary>
    function GetItem(Index: Integer): PMapEntry; overload;
    
    /// <summary>
    /// 获取一个条目的数据
    /// </summary>
    function GetValue(const Key: MapKey): PMapValue; overload;
    function GetValue(const Key: Number): PMapValue; overload;
    function GetValue(const Key: Double): PMapValue; overload;
    function GetValue(const Key: MString): PMapValue; overload;

    /// <summary>
    /// 添加一个条目, 如果已经存在，则替换其 Value
    /// </summary>
    function Add(const Key: MapKey; const Value: MapValue): PMapEntry; overload;   

    // 以下重载只是为了优化性能
    procedure Add(const Key, Value: MString); overload;
    procedure Add(const Key: MString; Value: Integer); overload;
    procedure Add(const Key: MString; Value: Double); overload;
    
    procedure Add(const Key: Number; const Value: MString); overload;
    procedure Add(const Key: Number; const Value: Integer); overload;
    procedure Add(const Key: Number; const Value: Double); overload;
    
    procedure Add(const Key: Double; const Value: MString); overload;
    procedure Add(const Key: Double; const Value: Integer); overload;
    procedure Add(const Key: Double; const Value: Double); overload;
    
    procedure Add(const Key: MapKey; Value: Integer); overload;
    procedure Add(const Key: MapKey; Value: Cardinal); overload;
    procedure Add(const Key: MapKey; Value: Byte); overload;
    procedure Add(const Key: MapKey; const Value: MString); overload;
    procedure Add(const Key: MapKey; const Value: Int64); overload;
    procedure Add(const Key: MapKey; const Value: Double); overload;
    procedure Add(const Key: MapKey; const Value: Variant); overload;
    procedure AddDateTime(const Key: MapKey; const Value: TDateTime); overload;

    function GetByte(const Key: MapKey): Byte; overload;
    function GetBoolean(const Key: MapKey): Boolean; overload;
    function GetInt(const Key: MapKey): Integer; overload;
    function GetInt64(const Key: MapKey): Int64; overload;
    function GetWord(const Key: MapKey): Word; overload;
    function GetDWORD(const Key: MapKey): Cardinal; overload;
    function GetFloat(const Key: MapKey): Single; overload;
    function GetDouble(const Key: MapKey): Double; overload;
    function GetString(const Key: MapKey): MString; overload;
    function GetDateTime(const Key: MapKey): TDateTime; overload;
    function GetVariant(const Key: MapKey): Variant; overload;

    function GetString(const Key: MString): MString; overload;
    function GetInt(const Key: MString): Integer; overload;
    function GetInt64(const Key: MString): Int64; overload;
    function GetDouble(const Key: MString): Double; overload;

    function GetString(const Key: Number): MString; overload;
    function GetInt(const Key: Number): Integer; overload;
    function GetInt64(const Key: Number): Int64; overload;
    function GetDouble(const Key: Number): Double; overload;

    /// <summary>
    /// 删除指定索引号的条目
    /// </summary>
    procedure Delete(const Index: Integer);
    /// <summary>
    /// 删除一个条目
    /// </summary>
    procedure Remove(const Key: MapKey); overload;
    procedure Remove(const Key: MString); overload;
    procedure Remove(const Key: Number); overload;
    procedure Remove(const Key: Double); overload;

    /// <summary>
    /// 是否为空
    /// </summary>
    function IsEmpty(): Boolean;
    /// <summary>
    /// 条目总数
    /// </summary>
    function GetSize(): Integer;

    property Count: Integer read GetSize;
    property Items[Index: Integer]: PMapEntry read GetItem write SetItem;
    property Values[const Key: MapKey]: PMapValue read GetValue;
  end;

  DictionaryEnumerator = class
  private
    FList: Dictionary;
    FIndex, FCount: Integer;
    FItems: MapEntryList;
  public
    constructor Create(AList: Dictionary);
    function GetCurrent: PMapEntry; inline;
    function MoveNext: Boolean;
    property Current: PMapEntry read GetCurrent;
  end;


implementation

resourcestring
  SBadConvert = '%s 不是一个有效的 %s 类型的值。';
  SBadStream  = '无效的数据格式';
  SNotExist  = '键值不存在';
  SBadIndex  = '索引值超出边界(%d)';
  SNameNotFound = '项目名称未找到.';

const
  MapTypeName: array [0 .. 5] of MString = ('Null', 'String', 'Integer',
    'Float', 'Boolean', 'DateTime');

const
  FileFlag = 'MapData'#0;  // 文件标识
  FileVer: Cardinal = 1;   // 文件版本

const
  MapDateFormat: MString = 'yyyy-mm-dd';
  MapTimeFormat: MString = 'hh:nn:ss.zzz';
  MapDateTimeFormat: MString = 'yyyy-mm-dd hh:nn:ss.zzz';
  //浮点数精度
  MapFloatDigits: Integer = 6;

var
  //Key是否区分大小写
  MapCaseSensitive: Boolean = True;

  NULLMapValue: MapValue;

function BoolToStr(const v: Boolean): MString; inline;
begin
  if v then Result := 'true' else Result := 'false';
end;

function FloatToStr(const value: Extended): string; inline;
var
  Buffer: array[0..63] of Char;
  P: PChar;
  I: Integer;
begin
  I := FloatToText(Buffer, Value, fvExtended, ffGeneral, 15, 0);
  P := StrScan(@Buffer[0], '.');
  if (P <> nil) then begin
    if I - (P - @Buffer[0] + 1) > MapFloatDigits then begin
      I := P - @Buffer[0] + MapFloatDigits;
      while Buffer[i] = '0' do Dec(I);
      SetString(Result, Buffer, I + 1)
    end else
      SetString(Result, Buffer, I);
  end else
    SetString(Result, Buffer, I);
end;

function GetHashValue(const V: MString): THashType; inline;
begin
  Result := HashOf(PMapChar(V), Length(V){$IFDEF UNICODE} shl 1{$ENDIF})
end;

{ MapKey }

function MapKey.GetAsFloat: Double;
begin
  if KeyType = mktFloat then
    Result := PDouble(@FIntValue)^
  else
    Result := 0;
end;

function MapKey.GetAsInteger: Number;
begin
  if KeyType = mktInteger then
    Result := FIntValue
  else
    Result := 0;
end;

function MapKey.GetAsString: MString;
begin
  if KeyType = mktString then
    Result := FStrValue
  else
    Result := '';
end;

function MapKey.GetHashCode: THashType;
begin
  case KeyType of
    mktString:
      begin
        if FIntValue = 0 then
          FIntValue := GetHashValue(FStrValue);
        Result := FIntValue;
      end;
    mktInteger, mktFloat:
      begin
        REsult := FIntValue;
      end;
  else
    Result := 0;
  end;
end;

procedure MapKey.SetAsFloat(const Value: Double);
begin
  FStrValue := '';
  PDouble(@FIntValue)^ := Value;
  KeyType := mktFloat;
end;

procedure MapKey.SetAsInteger(const Value: Number);
begin
  FStrValue := '';
  FIntValue := Value;
  KeyType := mktInteger;
end;

procedure MapKey.SetAsString(const Value: MString);
begin
  FIntValue := 0;
  FStrValue := Value;
  KeyType := mktString;
end;

function MapKey.ToString: MString;
begin
  case KeyType of
    mktString: Result := FStrValue;
    mktInteger: Result := IntToStr(FIntValue);
    mktFloat: Result := FloatToStr(AsFloat);
  else
    Result := '';
  end;
end;

class operator MapKey.Implicit(const Value: Double): MapKey;
begin
  Result.AsFloat := Value;
end;

procedure MapKey.InitHashCode;
begin
  if (KeyType = mktString) and (FIntValue = 0) then
    FIntValue := GetHashValue(FStrValue);
end;

function MapKey.IsEmpty: Boolean;
begin
  Result := (KeyType = mktString) and (Length(FStrValue) = 0);
end;

class operator MapKey.Implicit(const Value: MString): MapKey;
begin
  Result.AsString := Value;
end;

class operator MapKey.Implicit(const Value: Number): MapKey;
begin
  Result.AsInteger := Value;
end;

constructor MapKey.Create(const V: MString);
begin
  AsString := V;
end;

constructor MapKey.Create(const V: Number);
begin
  AsInteger := V;
end;

procedure MapKey.Clear;
begin
  FStrValue := '';
  FIntValue := 0;
end;

constructor MapKey.Create(const V: Double);
begin
  AsFloat := V;
end;

class operator MapKey.Equal(const A: MString; const B: MapKey): Boolean;
begin
  if (B.KeyType = mktString) and (Length(A) = Length(B.FStrValue)) then begin
    if MapCaseSensitive then begin
      Result := A = B.FStrValue;
    end else
      Result := StartWithIgnoreCase(PMapChar(A), PMapChar(B.FStrValue));  
  end else
    Result := False;
end;

class operator MapKey.Equal(const A: Number; const B: MapKey): Boolean;
begin
  Result := (B.KeyType = mktInteger) and (A = B.FIntValue);
end;

class operator MapKey.Equal(const A: Double; const B: MapKey): Boolean;
begin
  Result := (B.KeyType = mktFloat) and (A = PDouble(@B.FIntValue)^);
end;

class operator MapKey.Equal(const a: MapKey; const B: MapKey): Boolean;
var
  I: Integer;
begin
  if A.KeyType = B.KeyType then begin
    if A.KeyType = mktString then begin
      I := Length(A.FStrValue);
      if (I > 0) and (I = Length(B.FStrValue)) then begin
        if MapCaseSensitive then begin
          if B.FIntValue = 0 then
            B.InitHashCode();
          Result := (A.FIntValue = B.FIntValue) and (A.FStrValue = B.FStrValue);
        end else
          Result := StartWithIgnoreCase(PMapChar(A.FStrValue), PMapChar(B.FStrValue));
      end else
        Result := False;
    end else
      Result := A.FIntValue = B.FintValue;
  end else
    Result := False;
end;

class operator MapKey.NotEqual(const a: MapKey; const b: MapKey): Boolean;
begin
  Result := not (A = B);
end; 

{ MapValue }

class operator MapValue.Add(const A: MapValue; const B: Integer): Integer;
begin
  Result := A.AsInteger + B;
end;

class operator MapValue.Add(const A: MapValue; const B: Double): Double;
begin
  Result := A.AsDouble + B;
end;

class operator MapValue.Add(const A: MapValue; const B: MString): MString;
begin
  Result := A.AsString + B;
end;

class operator MapValue.Add(const A: MString; const B: MapValue): MString;
begin
  Result := A + B.AsString;
end;

class operator MapValue.Add(const A: Double; const B: MapValue): Double;
begin
  Result := B.AsDouble + A;
end;

class operator MapValue.Add(const A: Integer; const B: MapValue): Integer;
begin
  Result := B.AsInteger + A;
end;

class operator MapValue.Add(const A: MapValue; const B: Int64): Int64;
begin
  Result := A.AsInt64 + B;
end;

class operator MapValue.Add(const A: MapValue; const B: Cardinal): Cardinal;
begin
  Result := A.AsDWORD + B;
end;

class operator MapValue.Add(const A: Int64; const B: MapValue): Int64;
begin
  Result := B.AsInt64 + A;
end;

class operator MapValue.Add(const A: Cardinal; const B: MapValue): Cardinal;
begin
  Result := B.AsDWORD + A;
end;

procedure MapValue.Clear;
begin
  FDataType := mdtNull;
  SetLength(Value, 0);
end;

procedure MapValue.CopyValue(ASource: PMapValue);
var
  L: Integer;
begin
  L := Length(ASource.Value);
  FDataType := ASource.FDataType;
  SetLength(Value, L);
  if L > 0 then
    Move(ASource.Value[0], Value[0], L);
end;

class operator MapValue.Divide(const A: MapValue; const B: Cardinal): Double;
begin
  Result := A.AsDWORD / B;
end;

class operator MapValue.Divide(const A: MapValue; const B: Double): Double;
begin
  Result := A.AsDouble / B;
end;

class operator MapValue.Divide(const A: MapValue; const B: Integer): Double;
begin
  Result := A.AsInteger / B;
end;

class operator MapValue.Divide(const A: MapValue; const B: Int64): Double;
begin
  Result := A.AsInt64 / B;
end;

class operator MapValue.Divide(const A: Cardinal; const B: MapValue): Double;
begin
  Result := A / B.AsDWORD;
end;

class operator MapValue.Divide(const A: Double; const B: MapValue): Double;
begin
  Result := A / B.AsDouble;
end;

class operator MapValue.Divide(const A: Integer; const B: MapValue): Double;
begin
  Result := A / B.AsInteger;
end;

class operator MapValue.Divide(const A: Int64; const B: MapValue): Double;
begin
  Result := A / B.AsInt64;
end;

class operator MapValue.Equal(const A: MapValue; const B: Cardinal): Boolean;
begin
  Result := A.AsDWORD = B;
end;

class operator MapValue.Equal(const A: MapValue; const B: Double): Boolean;
begin
  Result := A.AsDouble = B;
end;

class operator MapValue.Equal(const A: MapValue; const B: Integer): Boolean;
begin
  Result := A.AsInteger = B;
end;

class operator MapValue.Equal(const A: MapValue; const B: Int64): Boolean;
begin
  Result := A.AsInt64 = B;
end;

class operator MapValue.Equal(const A: Cardinal; const B: MapValue): Boolean;
begin
  Result := B.AsDWORD = A;
end;

class operator MapValue.Equal(const A: Double; const B: MapValue): Boolean;
begin
  Result := B.AsDouble = A;
end;

class operator MapValue.Equal(const A: Integer; const B: MapValue): Boolean;
begin
  Result := B.AsInteger = A;
end;

class operator MapValue.Equal(const A: Int64; const B: MapValue): Boolean;
begin
  Result := B.AsInt64 = A;
end;

function MapValue.GetAsBoolean: Boolean;
begin
  if High(Value) > -1 then
    Result := PBoolean(@Value[0])^
  else Result := False;
end;

function MapValue.GetAsByte: Byte;
begin
  Result := GetAsInt64();
end;

function MapValue.GetAsBytes: TBytes;
begin
  Result := Value;
end;

function MapValue.GetAsDateTime: TDateTime;
begin
  if (FDataType = mdtFloat) or (FDataType = mdtDateTime) then
    Result := GetAsFloat
  else if (FDataType = mdtInteger) or (FDataType = mdtDWORD) then
    Result := AsInt64
  else
    raise Exception.Create(Format(SBadConvert, [MapTypeName[Integer(FDataType)], MapTypeName[5]]));
end;

function MapValue.GetAsDouble: Double;
begin
  case FDataType of
    mdtFloat, mdtDateTime:
      begin
        if Length(Value) = 8 then
          Result := PDouble(@Value[0])^
        else if Length(Value) >= SizeOf(Extended) then
          Result := PExtended(@Value[0])^
        else
          Result := 0;
      end;
    mdtString:
      Result := StrToFloatDef(GetString(), 0);
    mdtInteger, mdtDWORD:
      begin
        case High(Value) of
          3: Result := PInteger(@Value[0])^;
          7: Result := PInt64(@Value[0])^;
          0: Result := PShortInt(@Value[0])^;
          1: Result := PSmallInt(@Value[0])^;
        else
          Result := 0;
        end;
      end;
    mdtBoolean:
      Result := Integer(AsBoolean);
  else
    Result := 0;
  end;
end;

function MapValue.GetAsFloat: Single;
begin
  Result := AsDouble;
end;

function MapValue.GetAsInt32: Cardinal;
begin
  Result := AsInt64;
end;

function MapValue.GetAsInt64: Int64;
begin
  case FDataType of
    mdtInteger:
      begin
        case High(Value) of
          3: Result := PInteger(@Value[0])^;
          7: Result := PInt64(@Value[0])^;
          0: Result := PShortInt(@Value[0])^;
          1: Result := PSmallInt(@Value[0])^;
        else
          Result := 0;
        end;
      end;
    mdtDWORD:
      begin
        case High(Value) of
          3: Result := PDWORD(@Value[0])^;
          7: Result := PInt64(@Value[0])^;
          0: Result := PByte(@Value[0])^;
          1: Result := PWORD(@Value[0])^;
        else
          Result := 0;
        end;
      end;
    mdtString:
      Result := StrToIntDef(GetString(), 0);
    mdtFloat:
      begin
        if Length(Value) = 8 then
          Result := Round(PDouble(@Value[0])^)
        else if Length(Value) >= SizeOf(Extended) then
          Result := Round(PExtended(@Value[0])^)
        else
          Result := 0;
      end;
    mdtDateTime:
      Result := Round(AsDateTime);
    mdtBoolean:
      Result := Integer(AsBoolean);
  else
    Result := 0;
  end;
end;

function MapValue.GetAsInteger: Integer;
begin
  Result := GetAsInt64;
end;

function MapValue.GetAsNativeInt: NativeInt;
begin
  Result := GetAsInt64;
end;

function MapValue.GetAsString: MString;
begin
  Result := ToString();
end;

function MapValue.GetAsVariant: Variant;
begin
  case FDataType of
    mdtString: Result := AsString;
    mdtInteger: Result := AsInt64;
    mdtFloat: Result := AsFloat;
    mdtBoolean: Result := AsBoolean;
    mdtDateTime: Result := AsFloat;
  else
    Result := varEmpty;
  end;
end;

function MapValue.GetAsWord: Word;
begin
  Result := GetAsInt64;
end;

function MapValue.GetIsNull: Boolean;
begin
  Result := FDataType = mdtNull;
end;

function MapValue.GetSize: Cardinal;
begin
  Result := Length(Value);
end;

function MapValue.GetString: string;
begin
  {$IFDEF UNICODE}
  SetString(Result, PMapChar(Value), System.Length(Value) shr 1);
  {$ELSE}
  Result := MString(Value);
  SetLength(Result, System.Length(Value));
  {$ENDIF}
end;

class operator MapValue.Implicit(const Value: TBytes): MapValue;
begin
  Result.AsBytes := Value;
end;

class operator MapValue.Implicit(const Value: Word): MapValue;
begin
  Result.AsWord := Value;
end;

class operator MapValue.Implicit(const Value: Byte): MapValue;
begin
  Result.AsByte := Value;
end;

class operator MapValue.Implicit(const Value: MString): MapValue;
begin
  Result.AsString := Value;
end;

class operator MapValue.Implicit(const Value: Boolean): MapValue;
begin
  Result.AsBoolean := Value;
end;

class operator MapValue.Implicit(const Value: Integer): MapValue;
begin
  Result.AsInteger := Value;
end;

class operator MapValue.Implicit(const Value: Double): MapValue;
begin
  Result.AsDouble := Value;
end;

class operator MapValue.Implicit(const Value: Single): MapValue;
begin
  Result.AsFloat := Value;
end;

class operator MapValue.Implicit(const Value: Int64): MapValue;
begin
  Result.AsInt64 := Value;
end;

class operator MapValue.Implicit(const Value: Cardinal): MapValue;
begin
  Result.AsDWORD := Value;
end;

procedure MapValue.SetAsBoolean(const Value: Boolean);
begin
  SetLength(Self.Value, SizeOf(Value));
  PBoolean(@Self.Value[0])^ := Value;
  FDataType := mdtBoolean;
end;

procedure MapValue.SetAsByte(const Value: Byte);
begin
  SetLength(Self.Value, SizeOf(Value));
  Self.Value[0] := Value;
  FDataType := mdtDWORD;
end;

procedure MapValue.SetAsBytes(const Value: TBytes);
begin
  Self.Value := Value;
end;

procedure MapValue.SetAsDateTime(const Value: TDateTime);
begin
  SetLength(Self.Value, SizeOf(Value));
  PDouble(@Self.Value[0])^ := Value;
  FDataType := mdtDateTime;
end;

procedure MapValue.SetAsDouble(const Value: Double);
begin
  SetLength(Self.Value, SizeOf(Value));
  PDouble(@Self.Value[0])^ := Value;
  FDataType := mdtFloat;
end;

procedure MapValue.SetAsDWORD(const Value: Cardinal);
begin
  SetLength(Self.Value, SizeOf(Value));
  PCardinal(@Self.Value[0])^ := Value;
  FDataType := mdtDWORD;
end;

procedure MapValue.SetAsFloat(const Value: Single);
begin
  SetLength(Self.Value, SizeOf(Value));
  PSingle(@Self.Value[0])^ := Value;
  FDataType := mdtFloat;
end;

procedure MapValue.SetAsInt64(const Value: Int64);
begin
  SetLength(Self.Value, SizeOf(Value));
  PInt64(@Self.Value[0])^ := Value;
  FDataType := mdtInteger;
end;

procedure MapValue.SetAsInteger(const Value: Integer);
begin
  SetLength(Self.Value, SizeOf(Value));
  PInteger(@Self.Value[0])^ := Value;
  FDataType := mdtInteger;
end;

procedure MapValue.SetAsNativeInt(const Value: NativeInt);
begin

end;

procedure MapValue.SetAsString(const Value: MString);
begin
  if Length(Value) > 0 then begin
    {$IFDEF UNICODE}
    SetLength(Self.Value, (Length(Value) shl 1));
    Move(PMapChar(Value)^, Self.Value[0], Length(Value) shl 1);
    {$ELSE}
    SetLength(Self.Value, Length(Value));
    Move(Value[1], Self.Value[0], Length(Value));
    {$ENDIF}
  end else
    SetLength(Self.Value, 0);
  FDataType := mdtString;  
end;

procedure MapValue.SetAsVariant(const Value: Variant);
begin
  case FindVarData(Value)^.VType of
    varBoolean: SetAsBoolean(Value);
    varByte: SetAsByte(Value);
    varWord: SetAsWord(Value);
    varSmallint: SetAsInteger(Value);
    varInteger,
    varShortInt: SetAsInteger(Value);
    varLongWord: SetAsDWORD(Value);
    varInt64: SetAsInt64(Value);
    varSingle: SetAsDouble(Value);
    varDouble: SetAsDouble(Value);
    varDate: SetAsDateTime(Value);
    varCurrency: SetAsFloat(Value);
    varOleStr, varString: SetAsString(VarToStrDef(Value, ''));
    else begin
      SetLength(Self.Value, 0);
      FDataType := mdtNull;
    end;
  end;
end;

procedure MapValue.SetAsWord(const Value: Word);
begin
  SetLength(Self.Value, SizeOf(Value));
  PWord(@Self.Value[0])^ := Value;
  FDataType := mdtDWORD;
end;

class operator MapValue.Subtract(const A: MapValue;
  const B: Cardinal): Cardinal;
begin
  Result := A.AsDWORD - B;
end;

class operator MapValue.Subtract(const A: MapValue; const B: Double): Double;
begin
  Result := A.AsDouble - B;
end;

class operator MapValue.Subtract(const A: MapValue; const B: Integer): Integer;
begin
  Result := A.AsInteger - B;
end;

class operator MapValue.Subtract(const A: MapValue; const B: Int64): Int64;
begin
  Result := A.AsInt64 - B;
end;

class operator MapValue.Subtract(const A: Cardinal;
  const B: MapValue): Cardinal;
begin
  Result := A - B.AsDWORD;
end;

class operator MapValue.Subtract(const A: Double; const B: MapValue): Double;
begin
  Result := A - B.AsDouble;
end;

class operator MapValue.Subtract(const A: Integer; const B: MapValue): Integer;
begin
  Result := A - B.AsInteger;
end;

class operator MapValue.Subtract(const A: Int64; const B: MapValue): Int64;
begin
  Result := A - B.AsInt64;
end;

function MapValue.ToString: MString;
begin
  case FDataType of
    mdtString:
      Result := GetString();
    mdtDWORD:
      Result := IntToStr(AsDWORD);
    mdtInteger:
      Result := IntToStr(AsInteger);
    mdtFloat:
      Result := FloatToStr(AsFloat);
    mdtBoolean:
      Result := BoolToStr(AsBoolean);
    mdtDateTime:
      Result := ValueAsDateTime(MapDateFormat, MapTimeFormat, MapDateTimeFormat);
    mdtNull:
      Result := '';
  end;
end;

function MapValue.ValueAsDateTime(const DateFormat, TimeFormat,
  DateTimeFormat: MString): MString;
var
  ADate: Integer;
  AValue: Double;
begin
  AValue := AsDateTime;
  ADate := Trunc(AValue);
  if SameValue(ADate, 0) then begin //Date为0，是时间
    if SameValue(AValue, 0) then
      Result := FormatDateTime(DateFormat, AValue)
    else
      Result := FormatDateTime(TimeFormat, AValue);
  end else begin
    if SameValue(AValue-ADate, 0) then
      Result := FormatDateTime(DateFormat, AValue)
    else
      Result := FormatDateTime(DateTimeFormat, AValue);
  end;    
end;  

class operator MapValue.Implicit(const Value: MapValue): PMapValue;
begin
  Result := @Value; 
end;

class operator MapValue.Implicit(const Value: MapValue): MString;
begin
  Result := Value.AsString;
end;

class operator MapValue.Multiply(const A: MapValue;
  const B: Cardinal): Cardinal;
begin
  Result := A.AsDWORD * B;
end;

class operator MapValue.Multiply(const A: MapValue; const B: Double): Double;
begin
  Result := A.AsDouble * B;
end;

class operator MapValue.Multiply(const A: MapValue; const B: Integer): Integer;
begin
  Result := A.AsInteger * B;
end;

class operator MapValue.Multiply(const A: MapValue; const B: Int64): Int64;
begin
  Result := A.AsInt64 * B;
end;

class operator MapValue.Multiply(const A: Cardinal;
  const B: MapValue): Cardinal;
begin
  Result := B.AsDWORD * A;
end;

class operator MapValue.Multiply(const A: Double; const B: MapValue): Double;
begin
  Result := B.AsDouble * A;
end;

class operator MapValue.Multiply(const A: Integer; const B: MapValue): Integer;
begin
  Result := B.AsInteger * A;
end;

class operator MapValue.Multiply(const A: Int64; const B: MapValue): Int64;
begin
  Result := B.AsInt64 * A;
end;

class operator MapValue.NotEqual(const A: MapValue; const B: Cardinal): Boolean;
begin
  Result := A.AsDWORD <> B;
end;

class operator MapValue.NotEqual(const A: MapValue; const B: Double): Boolean;
begin
  Result := A.AsDouble <> B;
end;

class operator MapValue.NotEqual(const A: MapValue; const B: Integer): Boolean;
begin
  Result := A.AsInteger <> B;
end;

class operator MapValue.NotEqual(const A: MapValue; const B: Int64): Boolean;
begin
  Result := A.AsInt64 <> B;
end;

class operator MapValue.NotEqual(const A: Cardinal; const B: MapValue): Boolean;
begin
  Result := A <> B.AsDWORD;
end;

class operator MapValue.NotEqual(const A: Double; const B: MapValue): Boolean;
begin
  Result := A <> B.AsDouble;
end;

class operator MapValue.NotEqual(const A: Integer; const B: MapValue): Boolean;
begin
  Result := A <> B.AsInteger;
end;

class operator MapValue.NotEqual(const A: Int64; const B: MapValue): Boolean;
begin
  Result := A <> B.AsInt64;
end;

class operator MapValue.Equal(const A: MapValue; const B: Pointer): Boolean;
begin
  Result := Pointer(A.AsNativeInt) = B;
end;

class operator MapValue.Equal(const A: Pointer; const B: MapValue): Boolean;
begin
  Result := A = Pointer(B.AsNativeInt);
end;

class operator MapValue.NotEqual(const A: MapValue; const B: Pointer): Boolean;
begin
  Result := Pointer(A.AsNativeInt) <> B;
end;

class operator MapValue.NotEqual(const A: Pointer; const B: MapValue): Boolean;
begin
  Result := A <> Pointer(B.AsNativeInt);
end;

class operator MapValue.Implicit(const Value: MapValue): Int64;
begin
  Result := Value.AsInt64;
end;

class operator MapValue.Implicit(const Value: MapValue): Double;
begin
  Result := Value.AsDouble;
end;

{ MapEntry }

function MapEntry.ToString(const ADelimiter: MString): MString;
begin
  Result := Key.ToString + ADelimiter + Value.ToString; 
end;

{ DictionaryEnumerator }

constructor DictionaryEnumerator.Create(AList: Dictionary);
begin
  FList := AList;
  FIndex := -1;
  FItems := FList.GetEntryList;
  FCount := Length(FItems);
end;

function DictionaryEnumerator.GetCurrent: PMapEntry;
begin
  Result := FItems[FIndex];
end;

function DictionaryEnumerator.MoveNext: Boolean;
begin
  if FIndex < FCount - 1 then begin
    Inc(FIndex);
    Result := True;
  end else
    Result := False;
end;

{ MapBase }

procedure MapBase.LoadFromFile(const AFileName: MString);
var
  AStream: TFileStream;
begin
  if not FileExists(AFileName) then Exit;
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

function MapBase.ReadByte(avIn: TStream): Byte;
begin
  avIn.ReadBuffer(Result, SizeOf(Result));
end;

function MapBase.ReadCardinal(avIn: TStream): Cardinal;
begin
  avIn.ReadBuffer(Result, SizeOf(Result));
end;

function MapBase.ReadInt64(avIn: TStream): Int64;
begin
  avIn.ReadBuffer(Result, SizeOf(Result));
end;

function MapBase.ReadMapEntry(avIn: TStream; var avOut: MapEntry): Boolean;
begin
  Result := False;
  avOut.Key.KeyType := MapKeyType(ReadByte(avIn));
  case avOut.Key.KeyType of
    mktString:
      avOut.Key := ReadString(avIn);
    mktInteger, mktFloat:
      begin
        avOut.Key.FIntValue := ReadInt64(avIn);
        avOut.Key.FStrValue := '';
      end;
  else
    Exit;
  end;
  avOut.Value.FDataType := MapDataType(ReadByte(avIn));
  SetLength(avOut.Value.Value, ReadCardinal(avIn));
  if Length(avOut.Value.Value) > 0 then
    avIn.ReadBuffer(avOut.Value.Value[0], Length(avOut.Value.Value));
  Result := True;
end;

function MapBase.ReadNumber(avIn: TStream): Number;
begin
  avIn.ReadBuffer(Result, SizeOf(Result));
end;

function MapBase.ReadString(avIn: TStream): MString;
var
  Len: Cardinal;
begin
  avIn.ReadBuffer(Len, SizeOf(Len));
  if (Len > 0) and (Len < $FFFF) then begin
    SetLength(Result, Len);
    {$IFDEF UNICODE}
    avIn.ReadBuffer(Result[1], Len shl 1);
    {$ELSE}
    avIn.ReadBuffer(Result[1], Len);
    {$ENDIF}
  end else
    Result := '';
end;

procedure MapBase.SaveToFile(const AFileName: MString);
var
  AStream: TMemoryStream;
begin
  AStream := TMemoryStream.Create;
  try
    SaveToStream(AStream);
    AStream.SaveToFile(AFileName);
  finally
    AStream.Free;
  end;
end;

procedure MapBase.WriteByte(avOut: TStream; avData: Byte);
begin
  avOut.WriteBuffer(avData, SizeOf(avData));
end;

procedure MapBase.WriteCardinal(avOut: TStream; avData: Cardinal);
begin
  avOut.WriteBuffer(avData, SizeOf(avData));
end;

procedure MapBase.WriteInt64(avOut: TStream; avData: Int64);
begin
  avOut.WriteBuffer(avData, SizeOf(avData));
end;

procedure MapBase.WriteMapEntry(avOut: TStream; V: PMapEntry);
begin
  WriteByte(avOut, Ord(V.Key.KeyType));
  if V.Key.KeyType = mktString then
    WriteString(avOut, V.Key.FStrValue)
  else
    WriteInt64(avOut, V.Key.FIntValue);

  WriteByte(avOut, Ord(V.Value.FDataType));
  WriteCardinal(avOut, V.Value.Size);
  if V.Value.Size > 0 then
    avOut.WriteBuffer(V.Value.Value[0], V.Value.Size);
end;

procedure MapBase.WriteNumber(avOut: TStream; avData: Number);
begin
  avOut.WriteBuffer(avData, SizeOf(avData));
end;

procedure MapBase.WriteString(avOut: TStream; const avData: MString);
var
  Len: Cardinal;
begin
  Len := Length(avData);
  avOut.WriteBuffer(Len, SizeOf(Len));
  {$IFDEF UNICODE}
  if Len > 0 then
    avOut.WriteBuffer(avData[1], Len shl 1);
  {$ELSE}
  if Len > 0 then
    avOut.WriteBuffer(avData[1], Len);
  {$ENDIF}
end;

{ Dictionary }

function Dictionary.AddItem(const Key: MapKey): PMapEntry;
begin
  Result := FItems.Force(Key);
end;

function Dictionary.AddItem(const Key: MString): PMapEntry;
begin
  Result := FItems.Force(Key);
end;

function Dictionary.AddItem(const Key: Number): PMapEntry;
begin
  Result := FItems.Force(Key);
end;

function Dictionary.AddItem(const Key: Double): PMapEntry;
begin
  Result := FItems.Force(Key);
end;

procedure Dictionary.Clear;
var
  I: Integer;
  Item: PMapEntry;
begin
  if FItems.Count > 0 then begin
    for I := 0 to FItems.Count - 1 do begin
      Item := FItems.Items[i];
      if (Item <> nil) then
        Dispose(Item);
    end;
    FItems.Clear;
  end;
end;

function Dictionary.ContainsKey(const Key: MapKey): Boolean;
begin
  Result := FItems.Exists(@Key);
end;

constructor Dictionary.Create(Size: Cardinal = 331);
begin
  inherited Create;
  FItems := MapList.Create(Size);
end;

procedure Dictionary.Remove(const Key: MapKey);
begin
  FItems.Remove(@Key);
end;

procedure Dictionary.Remove(const Key: MString);
begin
  FItems.Remove(Key);
end;

procedure Dictionary.Remove(const Key: Number);
begin
  FItems.Remove(Key);
end;

procedure Dictionary.Remove(const Key: Double);
begin
  FItems.Remove(Key);
end;

destructor Dictionary.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function Dictionary.Exist(const Key: Number): Boolean;
begin
  Result := FItems.Exists(Key);
end;

function Dictionary.Exist(const Key: MString): Boolean;
begin
  Result := FItems.Exists(Key);
end;

function Dictionary.Exist(const Key: Double): Boolean;
begin
  Result := FItems.Exists(Key);
end;

function Dictionary.Exist(const Key: MapKey): Boolean;
begin
  Result := FItems.Exists(@Key);
end;

function Dictionary.GetInt(const Key: MapKey): Integer;
begin
  Result := GetValueItem(Key).AsInteger;
end;

function Dictionary.GetInt64(const Key: MapKey): Int64;
begin
  Result := GetValueItem(Key).AsInt64;
end;

function Dictionary.GetItem(Index: Integer): PMapEntry;
begin
  Result := FItems.Items[index];
end;

function Dictionary.GetBoolean(const Key: MapKey): Boolean;
begin
  Result := GetValueItem(Key).AsBoolean;
end;

function Dictionary.GetByte(const Key: MapKey): Byte;
begin
  Result := GetValueItem(Key).AsByte;
end;

function Dictionary.GetDateTime(const Key: MapKey): TDateTime;
begin
  Result := GetValueItem(Key).AsDateTime;
end;

function Dictionary.GetDouble(const Key: MString): Double;
begin
  Result := GetValueItem(Key).AsDouble;
end;

function Dictionary.GetDouble(const Key: Number): Double;
begin
  Result := GetValueItem(Key).AsDouble;
end;

function Dictionary.GetDouble(const Key: MapKey): Double;
begin
  Result := GetValueItem(Key).AsDouble;
end;

function Dictionary.GetDWORD(const Key: MapKey): Cardinal;
begin
  Result := GetValueItem(Key).AsDWORD;
end;

function Dictionary.GetEntryList: MapEntryList;
var
  I, J: Integer;
  P: PMapHashItem;
begin
  SetLength(Result, FItems.Count);
  if FItems.Count > 0 then begin
    J := 0;
    for I := 0 to Length(FItems.Buckets) - 1 do begin
      P := FItems.Buckets[I];
      while P <> nil do begin
        Result[J] := P.Value;
        P := P^.Next;
        Inc(J);
      end;
    end;
  end;
end;

function Dictionary.GetEnumerator: DictionaryEnumerator;
begin
  Result := DictionaryEnumerator.Create(Self);
end;

function Dictionary.GetFloat(const Key: MapKey): Single;
begin
  Result := GetValueItem(Key).AsFloat;
end;

function Dictionary.GetSize: Integer;
begin
  Result := FItems.Count;
end;

function Dictionary.GetString(const Key: Number): MString;
begin
  Result := GetValueItem(Key).AsString;
end;

function Dictionary.GetString(const Key: MString): MString;
begin
  Result := GetValueItem(Key).AsString;
end;

function Dictionary.GetString(const Key: MapKey): MString;
begin
  Result := GetValueItem(Key).AsString;
end;

function Dictionary.GetValue(const Key: MapKey): PMapValue;
var
  Item: PMapEntry;
begin
  Item := FItems.ValueOf(@Key);
  if (Item = nil) then
    Result := nil
  else
    Result := @Item.Value;
end;

function Dictionary.GetValueItem(const Key: MapKey): PMapValue;
var
  Item: PMapEntry;
begin
  Item := FItems.ValueOf(@Key);
  if (Item = nil) then
    Result := @NULLMapValue
  else
    Result := @Item.Value; 
end;

function Dictionary.GetValueItem(const Key: MString): PMapValue;
var
  Item: PMapEntry;
begin
  Item := FItems.ValueOf(Key);
  if (Item = nil) then
    Result := @NULLMapValue
  else
    Result := @Item.Value; 
end;

function Dictionary.GetValueItem(const Key: Number): PMapValue;
var
  Item: PMapEntry;
begin
  Item := FItems.ValueOf(Key);
  if (Item = nil) then
    Result := @NULLMapValue
  else
    Result := @Item.Value; 
end;

function Dictionary.GetValue(const Key: Number): PMapValue;
var
  Item: PMapEntry;
begin
  Item := FItems.ValueOf(Key);
  if (Item = nil) then
    Result := nil
  else
    Result := @Item.Value;
end;

function Dictionary.GetValue(const Key: Double): PMapValue;
var
  Item: PMapEntry;
begin
  Item := FItems.ValueOf(Key);
  if (Item = nil) then
    Result := nil
  else
    Result := @Item.Value;
end;

function Dictionary.GetValue(const Key: MString): PMapValue;
var
  Item: PMapEntry;
begin
  Item := FItems.ValueOf(Key);
  if (Item = nil) then
    Result := nil
  else
    Result := @Item.Value;
end;

function Dictionary.GetValueItem(const Key: Double): PMapValue;
var
  Item: PMapEntry;
begin
  Item := FItems.ValueOf(Key);
  if (Item = nil) then
    Result := @NULLMapValue
  else
    Result := @Item.Value; 
end;

function Dictionary.GetVariant(const Key: MapKey): Variant;
begin
  Result := GetValueItem(Key).AsVariant;
end;

function Dictionary.GetWord(const Key: MapKey): Word;
begin
  Result := GetValueItem(Key).AsWord;
end;

function Dictionary.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure Dictionary.LoadFromStream(AStream: TStream);
var
  I: Integer;
  ACount: Integer;
  Item: MapEntry;
  Ver: Cardinal;
begin
  if ReadInt64(AStream) <> PInt64(@FileFlag[1])^ then
    raise Exception.Create(SBadStream);

  Clear;

  // 读出文件版本号
  Ver := ReadCardinal(AStream);

  if Ver = 1 then begin
    ACount := ReadCardinal(AStream);
    if ACount = 0 then Exit;
    for I := 0 to ACount - 1 do begin
      Item.Key.FStrValue := '';
      if ReadMapEntry(AStream, Item) then
        Add(Item.Key, Item.Value)
      else
        raise Exception.Create(SBadStream);
    end;
  end;
end;

function Dictionary.Add(const Key: MapKey; const Value: MapValue): PMapEntry;
begin
  if Key.IsEmpty then 
    Result := nil
  else begin 
    Result := FItems.ValueOf(@Key); 
    if Result = nil then begin        
      New(Result);
      Result.Key := Key;
      FItems.Add(Result);
    end;
    Result.Value := Value;
  end;
end;

procedure Dictionary.Delete(const Index: Integer);
begin
  FItems.Delete(Index);
end; 

procedure Dictionary.SaveToStream(AStream: TStream);
var
  I: Integer;
  List: MapEntryList;
begin
  List := GetEntryList;
  WriteInt64(AStream, PInt64(@FileFlag[1])^);
  WriteCardinal(AStream, FileVer);
  WriteCardinal(AStream, Length(List));
  for I := 0 to High(List) do
    WriteMapEntry(AStream, List[I]);
end;

procedure Dictionary.SetItem(Index: Integer; const Value: PMapEntry);
var
  Item: PMapEntry;
begin
  Item := FItems.Items[Index];
  if Item = Value then Exit;
  if Item <> nil then
    Dispose(Item);
  FItems.Items[Index] := Value;
end;

class procedure Dictionary.SetMapCaseSensitive(v: Boolean);
begin
  MapCaseSensitive := v;
end;

function Dictionary._GetValue(const Key: MapKey): MapValue;
var
  Item: PMapEntry;
begin
  Item := FItems.ValueOf(@Key);
  if (Item = nil) then begin
    Result := NULLMapValue
  end else
    Result := Item.Value;
end;

procedure Dictionary._SetValue(const Key: MapKey; const Value: MapValue);
begin
  FItems.Force(Key).Value := Value;
end;

procedure Dictionary.Add(const Key: Number; const Value: Integer);
begin
  AddItem(Key).Value.AsInteger := Value;
end;

procedure Dictionary.Add(const Key: Number; const Value: Double);
begin
  AddItem(Key).Value.AsDouble := Value;
end;

procedure Dictionary.Add(const Key: MapKey; Value: Integer);
begin
  if not Key.IsEmpty then   
    AddItem(Key).Value.AsInteger := Value;
end;

procedure Dictionary.Add(const Key: Number; const Value: MString);
begin
  AddItem(Key).Value.AsString := Value;
end;

procedure Dictionary.Add(const Key, Value: MString);
begin
  AddItem(Key).Value.AsString := Value;
end;

procedure Dictionary.Add(const Key: MString; Value: Integer);
begin
  AddItem(Key).Value.AsInteger := Value;
end;

procedure Dictionary.Add(const Key: MString; Value: Double);
begin
  AddItem(Key).Value.AsDouble := Value;
end;

procedure Dictionary.Add(const Key: MapKey; const Value: Int64);
begin
  if not Key.IsEmpty then   
    AddItem(Key).Value.AsInt64 := Value;
end;

procedure Dictionary.Add(const Key: MapKey; const Value: Double);
begin
  if not Key.IsEmpty then   
    AddItem(Key).Value.AsDouble := Value;
end;

procedure Dictionary.Add(const Key: MapKey; const Value: Variant);
begin
  if not Key.IsEmpty then   
    AddItem(Key).Value.AsVariant := Value;
end;

procedure Dictionary.Add(const Key: Double; const Value: MString);
begin
  AddItem(Key).Value.AsString := Value;
end;

procedure Dictionary.Add(const Key: Double; const Value: Integer);
begin
  AddItem(Key).Value.AsInteger := Value;
end;

procedure Dictionary.Add(const Key, Value: Double);
begin
  AddItem(Key).Value.AsDouble := Value;
end;

procedure Dictionary.Add(const Key: MapKey; Value: Cardinal);
begin
  if not Key.IsEmpty then   
    AddItem(Key).Value.AsDWORD := Value;
end;

procedure Dictionary.Add(const Key: MapKey; Value: Byte);
begin
  if not Key.IsEmpty then   
    AddItem(Key).Value.AsByte := Value;
end;

procedure Dictionary.Add(const Key: MapKey; const Value: MString);
begin
  if not Key.IsEmpty then   
    AddItem(Key).Value.AsString := Value;
end;

procedure Dictionary.AddDateTime(const Key: MapKey; const Value: TDateTime);
begin
  if not Key.IsEmpty then   
    AddItem(Key).Value.AsDateTime := Value;
end;

function Dictionary.GetInt(const Key: Number): Integer;
begin
  Result := GetValueItem(Key).AsInteger;
end;

function Dictionary.GetInt(const Key: MString): Integer;
begin
  Result := GetValueItem(Key).AsInteger;
end;

function Dictionary.GetInt64(const Key: MString): Int64;
begin
  Result := GetValueItem(Key).AsInt64;
end;

function Dictionary.GetInt64(const Key: Number): Int64;
begin
  Result := GetValueItem(Key).AsInt64;
end;

{ MapList }

procedure MapList.Add(const Value: PMapEntry);
var
  Hash: THashType;
  Bucket: PMapHashItem;
begin
  Hash := Value.Key.GetHashCode mod THashType(Length(Buckets));
  New(Bucket);
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
  Inc(FCount);
end;

procedure MapList.Clear;
var
  I: Integer;
  P, N: PMapHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do begin
    P := Buckets[I];
    while P <> nil do begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
  FCount := 0;
end;

constructor MapList.Create(Size: Cardinal);
begin
  inherited Create();
  SetLength(Buckets, Size);
  FCount := 0;
end;

destructor MapList.Destroy;
begin
  Clear;
  inherited;
end;

procedure MapList.DoOutOfBoundsError(const Index: Integer);
begin
  raise Exception.Create(Format('OutOfBounds. Index: %d, Size: %d.', [Index, FCount]))
end;

function MapList.Exists(const Key: MString): Boolean;
begin
  Result := Find(Key)^ <> nil;
end;

function MapList.Exists(const Key: Number): Boolean;
begin
  Result := Find(Key)^ <> nil;
end;

function MapList.Exists(const Key: Double): Boolean;
begin
  Result := Find(Key)^ <> nil;
end;

function MapList.Exists(const Key: PMapKey): Boolean;
begin
  Result := Find(Key)^ <> nil;
end;

function MapList.Find(const Key: MString): PPMapHashItem;
var
  Hash: THashType;
begin
  Hash := GetHashValue(Key);
  Result := @Buckets[Hash mod Cardinal(Length(Buckets))];
  while Result^ <> nil do begin
    if (Key = Result^.Value.Key) then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function MapList.Find(const Key: Number): PPMapHashItem;
begin
  Result := @Buckets[Key mod Cardinal(Length(Buckets))];
  while Result^ <> nil do begin
    if (Key = Result^.Value.Key) then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function MapList.Find(const Key: PMapKey): PPMapHashItem;
begin
  Result := @Buckets[Key.GetHashCode mod Cardinal(Length(Buckets))];
  while Result^ <> nil do begin
    if Key^ = Result^.Value.Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function MapList.Find(const Key: Double): PPMapHashItem;
var
  Hash: THashType;
begin
  Hash := PInt64(@Key)^;
  Result := @Buckets[Hash mod Cardinal(Length(Buckets))];
  while Result^ <> nil do begin
    if (Key = Result^.Value.Key) then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function MapList.Force(const Value: MapKey): PMapEntry;
begin
  Value.InitHashCode;
  Result := ValueOf(@Value);
  if Result = nil then begin
    New(Result);
    Result.Key := Value;
    Add(Result);
  end;
end;

function MapList.Force(const Value: MString): PMapEntry;
var
  Hash: THashType;
  PH: PMapHashItem;
begin
  Hash := GetHashValue(Value);
  PH := Buckets[Hash mod Cardinal(Length(Buckets))];
  while PH <> nil do begin
    if (Value = PH.Value.Key) then begin
      Result := PH.Value;
      Exit;
    end else
      PH := PH.Next;
  end;
  New(Result);
  Result.Key.KeyType := mktString;
  Result.Key.FIntValue := Hash;
  Result.Key.FStrValue := Value;
  Add(Result);
end;

function MapList.Force(const Value: PMapEntry): PMapEntry;
begin
  Value.Key.InitHashCode;
  Result := ValueOf(@Value.Key);
  if Result = nil then begin
    New(Result);
    Result^ := Value^;
    Add(Result);
  end;
end;

function MapList.Force(const Value: Double): PMapEntry;
begin
  Result := ValueOf(Value);
  if Result = nil then begin
    New(Result);
    Result.Key := Value;
    Add(Result);
  end;
end;

function MapList.Force(const Value: Number): PMapEntry;
begin
  Result := ValueOf(Value);
  if Result = nil then begin
    New(Result);
    Result.Key.KeyType := mktInteger;
    Result.Key.FIntValue := Value;
    Result.Key.FStrValue := '';
    Add(Result);
  end;
end;

function MapList.GetBucketsIndex(const Index: Integer): PMapHashItem;
var
  I, M: Integer;
  P: PMapHashItem;
begin
  Result := nil;
  if (Index < 0) or (Index >= FCount) then Exit;  
  M := 0;
  for I := 0 to High(Buckets) do begin
    P := Buckets[I];
    while P <> nil do begin
      if M = Index then begin
        Result := P;
        Exit;
      end else
        P := P^.Next;
      Inc(M);
    end;
  end;
end;

function MapList.GetItem(const Index: Integer): PMapEntry;
var
  P: PMapHashItem;
begin
  P := GetBucketsIndex(Index);
  if P <> nil then
    Result := P.Value
  else begin
    DoOutOfBoundsError(Index);
    Result := nil;
  end;
end;

function MapList.Delete(Index: Integer): Boolean;
var
  Item: PMapEntry;
begin
  Item := GetItem(Index);
  if Item <> nil then
    Result := Remove(@Item.Key)
  else
    Result := False;
end;

function MapList.Remove(const Key: MString): Boolean;
begin
  Result := RemoveItem(Find(Key));
end;

function MapList.Remove(const Key: Number): Boolean;
begin
  Result := RemoveItem(Find(Key));
end;

function MapList.Remove(const Key: Double): Boolean;
begin
  Result := RemoveItem(Find(Key));
end;

function MapList.RemoveItem(const Value: PPMapHashItem): Boolean;
var
  P: PMapHashItem;
begin
  P := Value^;
  if P <> nil then begin
    Value^ := P^.Next;
    Dispose(P.Value);
    Dispose(P);
    Dec(FCount);
    Result := True;
  end else
    Result := False;
end;

function MapList.Remove(const Key: PMapKey): Boolean;
begin
  Result := RemoveItem(Find(Key));
end;

procedure MapList.SetItem(const Index: Integer; const Value: PMapEntry);
var
  P: PMapHashItem;
begin
  P := GetBucketsIndex(Index);
  if P <> nil then
    P.Value := Value
  else
    DoOutOfBoundsError(Index);
end;

function MapList.ValueOf(const Key: Number): PMapEntry;
var
  P: PMapHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := nil;
end;

function MapList.ValueOf(const Key: Double): PMapEntry;
var
  P: PMapHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := nil;
end;

function MapList.ValueOf(const Key: MString): PMapEntry;
var
  P: PMapHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := nil;
end;

function MapList.ValueOf(const Key: PMapKey): PMapEntry;
var
  P: PMapHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := nil;
end;

initialization
  FillChar(NULLMapValue, SizeOf(NULLMapValue), 0);
  
end.
