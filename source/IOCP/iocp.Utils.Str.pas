{*******************************************************}
{                                                       }
{       字符串公用基础函数                              }
{                                                       }
{       版权所有 (C) 2013      YangYxd                  }
{                                                       }
{*******************************************************}
{
  本单元基于 YxdStr 精简而来， 为 IOCP 专用
  部分函数来自QDAC项目中的QString, 版权归原作者所有
  QDAC官方群：250530692
}

unit iocp.Utils.Str;

interface

// 是否使用URL函数
{$DEFINE USE_URLFUNC}
// 是否使用字符串编码转换函数
{$DEFINE USE_STRENCODEFUNC}

//Delphi XE
{$IF (RTLVersion>=26)}
{$DEFINE USE_UNICODE}
{$IFEND}

//是否使用Inline
{$DEFINE INLINE}

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
{$DEFINE ANSISTRINGS}
{$IFEND}

uses
  {$IFNDEF UNICODE}Windows, {$ELSE} {$IFDEF MSWINDOWS}Windows, {$ENDIF}{$ENDIF}
  {$IFDEF ANSISTRINGS}AnsiStrings, {$ENDIF}
  {$IFDEF POSIX}Posix.String_, {$ENDIF}
  {$IFDEF USE_URLFUNC}StrUtils, Math, {$ENDIF}
  SysUtils, SysConst, Classes, Variants, DateUtils, SyncObjs;

type
  {$IFDEF NEXTGEN}
  AnsiChar = Byte;
  PAnsiChar = ^AnsiChar;
  WideString = UnicodeString;
  AnsiString = record
  private
    FValue:TBytes;
    function GetChars(AIndex: Integer): AnsiChar;
    procedure SetChars(AIndex: Integer; const Value: AnsiChar);
    function GetLength:Integer;
    procedure SetLength(const Value: Integer);
    function GetIsUtf8: Boolean;
  public
    class operator Implicit(const S:WideString):AnsiString;
    class operator Implicit(const S:AnsiString):PAnsiChar;
    class operator Implicit(const S:AnsiString):TBytes;
    class operator Implicit(const ABytes:TBytes):AnsiString;
    class operator Implicit(const S:AnsiString):WideString;
    //class operator Implicit(const S:PAnsiChar):AnsiString;
    //字符串比较
    procedure From(p:PAnsiChar;AOffset,ALen:Integer);
    property Chars[AIndex:Integer]:AnsiChar read GetChars write SetChars;default;
    property Length:Integer read GetLength write SetLength;
    property IsUtf8:Boolean read GetIsUtf8;
  end;
  {$ENDIF}

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
  TBytesCatHelper = class
  private
    FValue: TBytes;
    FStart, FDest: PByte;
    FBlockSize: Integer;
    FSize: Integer;
    function GetValue: TBytes;
    function GetPosition: Integer;
    function GetBytes(AIndex:Integer): Byte;
    procedure SetPosition(const Value: Integer);
    procedure NeedSize(ASize: Integer);
    procedure SetCapacity(const Value: Integer);
  public
    constructor Create; overload;
    constructor Create(ASize: Integer); overload;
    destructor Destroy; override;
    function Cat(V: Byte): TBytesCatHelper; overload;
    function Cat(V: CharA): TBytesCatHelper; overload;
    function Cat(p: Pointer; len: Integer): TBytesCatHelper; overload;
    function Cat(const V: Shortint): TBytesCatHelper; overload;
    function Cat(const V: Word): TBytesCatHelper; overload;
    function Cat(const V: Smallint): TBytesCatHelper; overload;
    function Cat(const V: Cardinal): TBytesCatHelper; overload;
    function Cat(const V: Integer): TBytesCatHelper; overload;
    function Cat(const V: Int64): TBytesCatHelper; overload;
    function Cat(const V: Currency): TBytesCatHelper;overload;
    function Cat(const V: Double): TBytesCatHelper; overload;
    function Cat(const s: StringA): TBytesCatHelper; overload;
    function Cat(const s: StringW): TBytesCatHelper; overload;
    function Cat(const V:Boolean): TBytesCatHelper;overload;
    function Skip(Count: Integer): TBytesCatHelper;
    function Replicate(const ABytes: TBytes; ACount: Integer): TBytesCatHelper;
    function Back(ALen: Integer): TBytesCatHelper;
    procedure Reset;
    property Value: TBytes read GetValue;
    property Bytes[Index: Integer]: Byte read GetBytes;
    property Start: PByte read FStart;
    property Current: PByte read FDest;
    property Position: Integer read GetPosition write SetPosition;
    property Capacity: Integer read FSize write SetCapacity;
  end;

type
  TStringCatHelperA = class
  private
    FValue: array of CharA;
    FStart, FDest: PCharA;
    FBlockSize: Integer;
    FSize: Integer;
    function GetValue: StringA;
    function GetPosition: Integer;
    function GetChars(AIndex:Integer): CharA;
    procedure SetPosition(const Value: Integer);
    procedure NeedSize(ASize: Integer);
    function GetMemory: Pointer;
  public
    constructor Create; overload;
    constructor Create(ASize: Integer); overload;
    destructor Destroy; override;
    function Cat(p: PCharA; len: Integer): TStringCatHelperA; overload;
    function Cat(const s: StringA): TStringCatHelperA; overload;
    function Cat(const s: StringW): TStringCatHelperA; overload;
    function Cat(c: CharA): TStringCatHelperA; overload;
    function Cat(const V:Int64): TStringCatHelperA;overload;
    function Cat(const V:Double): TStringCatHelperA;overload;
    function Cat(const V:Boolean): TStringCatHelperA;overload;
    function Cat(const V:Currency): TStringCatHelperA;overload;
    function Space(count: Integer): TStringCatHelperA;
    function Back(ALen: Integer): TStringCatHelperA;
    procedure Reset;
    property Value: StringA read GetValue;
    property Chars[Index: Integer]: CharA read GetChars;
    property Start: PCharA read FStart;
    property Current: PCharA read FDest;
    property Position: Integer read GetPosition write SetPosition;
    property Memory: Pointer read GetMemory;
  end;

type
  TStringCatHelperW = class
  private
    FValue: array of CharW;
    FStart, FDest, FLast: PCharW;
    FBlockSize: Integer;
    FSize: Integer;
    function GetValue: StringW;
    function GetPosition: Integer;
    function GetChars(AIndex:Integer): CharW;
    procedure SetPosition(const Value: Integer);
    procedure NeedSize(ASize: Integer);
    function GetIsEmpty: Boolean;
    procedure SetDest(const Value: PCharW);
  public
    constructor Create; overload;
    constructor Create(ASize: Integer); overload;
    destructor Destroy; override;
    function Cat(p: PCharW; len: Integer): TStringCatHelperW; overload;
    function Cat(const s: StringW): TStringCatHelperW; overload;
    function Cat(c: CharW): TStringCatHelperW; overload;
    function Cat(const V: Int64): TStringCatHelperW;overload;
    function Cat(const V: Double): TStringCatHelperW;overload;
    function Cat(const V: Boolean): TStringCatHelperW;overload;
    function Cat(const V: Currency): TStringCatHelperW; overload;
    function Cat(const V: Variant): TStringCatHelperW; overload;
    function Replicate(const S: StringW; count: Integer): TStringCatHelperW;
    function Space(count: Integer): TStringCatHelperW;
    function Back(ALen: Integer): TStringCatHelperW;
    procedure IncSize(ADelta: Integer);
    procedure TrimRight;
    procedure Reset;
    property Value: StringW read GetValue;
    property Chars[Index: Integer]: CharW read GetChars;
    property Start: PCharW read FStart;
    property Current: PCharW read FDest write SetDest;
    property Last: PCharW read FLast;
    property Position: Integer read GetPosition write SetPosition;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

type
  TStringCatHelper = {$IFDEF UNICODE} TStringCatHelperW; {$ELSE}  TStringCatHelperA; {$ENDIF}

// --------------------------------------------------------------------------
//  基本字符串处理函数，类型转换
// --------------------------------------------------------------------------
function StrDupX(const s: PChar; ACount:Integer): String;
function StrDupXA(const s: PCharA; ACount:Integer): StringA;
function StrDupXW(const s: PCharW; ACount:Integer): StringW;
function StrDup(const S: PChar; AOffset: Integer; const ACount: Integer): String;
procedure ExchangeByteOrder(p: PCharA; l: Integer); overload;
function ExchangeByteOrder(V: Smallint): Smallint; overload; inline;
function ExchangeByteOrder(V: Word): Word; overload; inline;
function ExchangeByteOrder(V: Integer): Integer; overload; inline;
function ExchangeByteOrder(V: Cardinal): Cardinal; overload; inline;
function ExchangeByteOrder(V: Int64): Int64; overload; inline;
function ExchangeByteOrder(V: Single): Single; overload; inline;
function ExchangeByteOrder(V: Double): Double; overload; inline;
// 转换成大写字符
function CharUpper(c: Char): Char; inline;
function CharUpperA(c: AnsiChar): AnsiChar;
function CharUpperW(c: WideChar): WideChar;
// 内存扫描
function MemScan(S: Pointer; len_s: Integer; sub: Pointer; len_sub: Integer): Pointer;
// 字节位比较
function BinaryCmp(const p1, p2: Pointer; len: Integer): Integer;
// 字符串查找，从左到右  （UTF8直接使用Ansi版本即可）
function StrScanW(const Str: PCharW; Chr: CharW): PCharW;
function StrStr(src, sub: string): Integer; overload; inline;
function StrStr(src, sub: PChar): PChar; overload; inline;
function StrStrA(src, sub: PAnsiChar): PAnsiChar;
function StrStrW(src, sub: PWideChar): PWideChar;
function StrIStr(src, sub: string): Integer; overload;
function StrIStr(src, sub: PChar): PChar; overload;
function StrIStrA(src, sub: PAnsiChar): PAnsiChar;
function StrIStrW(src, sub: PWideChar): PWideChar; 
function PosStr(sub, src: AnsiString; Offset: Integer = 0): Integer; overload; inline;
function PosStr(sub: PAnsiChar; src: PAnsiChar; Offset: Integer = 0): Integer; overload; inline;
function PosStr(sub: PAnsiChar; subLen: Integer; src: PAnsiChar; Offset: Integer): Integer; overload; inline;
function PosStr(sub: PAnsiChar; subLen: Integer; src: PAnsiChar; srcLen: Integer; Offset: Integer): Integer; overload;
function PosStr(const sub, src: StringW; Offset: Integer = 0): Integer; overload; inline;
function PosStr(sub: PCharW; subLen: Integer; src: PCharW; srcLen: Integer; Offset: Integer): Integer; overload;
// 查找字符串，从右到左
function RPosStr(sub, src: AnsiString; Offset: Integer = 0): Integer; overload; inline;
function RPosStr(sub: PAnsiChar; src: PAnsiChar; Offset: Integer = 0): Integer; overload; inline;
function RPosStr(sub: PAnsiChar; subLen: Integer; src: PAnsiChar; srcLen: Integer; Offset: Integer): Integer; overload;
function RPosStr(sub, src: StringW; Offset: Integer = 0): Integer; overload; inline;
function RPosStr(sub: PCharW; src: PCharW; Offset: Integer = 0): Integer; overload; inline;
function RPosStr(sub: PCharW; subLen: Integer; src: PCharW; srcLen: Integer; Offset: Integer): Integer; overload;
// 计算一个以 #0 为结束标志的Wide字符串长度
function WideStrLen(S: PWideChar): Integer; inline;
// 计算一个以 #0 为结束标志的Ansi字符串长度
function AnsiStrLen(s: PAnsiChar): Integer; inline;
// 从一个字符串取出以ADelim为结束标志的字子符串，并且在ADelete为True时，删除源字符串的内容（包括Delim)
function Fetch(var AInput: StringA; const ADelim: StringA = ' ';
  const ADelete: Boolean = True): StringA; overload; inline;
function Fetch(var AInput: StringW; const ADelim: StringW = ' ';
  const ADelete: Boolean = True; const ACaseSensitive: Boolean = True): StringW; overload; inline;
// 计算一个整数转为16进制字符串的长度
function LengthAsDWordToHex(const Value: Cardinal): Integer;
// 比较字符串
function TextIsSame(const A1, A2: string): Boolean; inline;
{$IFDEF USE_URLFUNC}
// URL编码
function UrlEncode(const AUrl: StringA): StringA; overload;
function UrlEncode(const AUrl: StringW): StringW; overload;
function UrlEncodeA(const AStr: PCharA; OutBuf: PCharA): Integer; overload;
function UrlEncodeW(const AStr: PCharW; OutBuf: PCharW): Integer; overload;
// URL解码
function UrlDecode(const Src: PCharA; OutBuf: PCharA; RaiseError: Boolean = True): Integer; overload;
function UrlDecode(const AStr: StringA; RaiseError: Boolean = True): StringA; overload;
function UTFStrToUnicode(UTFStr: StringA): StringW;
{$ENDIF}
// 字符串截取
function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload; inline;
function LeftStr(const AText: WideString; const ACount: Integer): WideString; overload; inline;
function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString; overload; inline;
function RightStr(const AText: WideString; const ACount: Integer): WideString; overload; inline;
function MidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString; overload; inline;
function MidStr(const AText: WideString; const AStart, ACount: Integer): WideString; overload; inline;
function PCharToString(const P: PChar; Len: Integer): string;
//编码转换
function IsHexChar(c: Char): Boolean; inline;
function HexValue(c: Char): Integer;
function HexChar(v: Byte): Char;
//检查字符是否在指定的列表中
function CharIn(const c, list: PChar; ACharLen:PInteger = nil): Boolean; inline;
{$IFNDEF NEXTGEN}
function CharInA(c, list: PAnsiChar; ACharLen: PInteger = nil): Boolean;
function CharInU(c, list: PAnsiChar; ACharLen: PInteger = nil): Boolean;
{$ENDIF}
function CharInW(c, list: PWideChar; ACharLen: PInteger = nil): Boolean;
//计算字符长度
function CharSizeA(c: PAnsiChar): Integer;
function CharSizeU(c: PAnsiChar): Integer;
function CharSizeW(c: PWideChar): Integer;
{$IFDEF USE_STRENCODEFUNC}
function AnsiEncode(p:PWideChar; l:Integer): AnsiString; overload;
function AnsiEncode(const p: StringW): AnsiString; overload;
{$IFNDEF MSWINDOWS}
function AnsiDecode(const S: AnsiString): StringW; overload;
{$ENDIF}
function AnsiDecode(p: PAnsiChar; l:Integer): StringW; overload;
function Utf8Encode(const p: StringW): AnsiString; overload;
function Utf8Encode(p: PWideChar; l: Integer): AnsiString; overload;
{$IFNDEF MSWINDOWS}
function Utf8Decode(const S: AnsiString): StringW; overload;
{$ENDIF}
function Utf8Decode(p: PAnsiChar; l: Integer): StringW; overload;
{$ENDIF}
function BinToHex(p: Pointer; l: Integer): string; overload;
function BinToHex(const ABytes:TBytes): string; overload;
procedure HexToBin(p: Pointer; l: Integer; var AResult: TBytes); overload;
function HexToBin(const S: String): TBytes; overload;
procedure HexToBin(const S: String; var AResult: TBytes); overload;

//查找字符所在行列号，返回行的起始地址
function StrPosA(start, current: PAnsiChar; var ACol, ARow:Integer): PAnsiChar;
function StrPosU(start, current: PAnsiChar; var ACol, ARow:Integer): PAnsiChar;
function StrPosW(start, current: PWideChar; var ACol, ARow:Integer): PWideChar;
//获取一行
function DecodeLineA(var p:PAnsiChar; ASkipEmpty:Boolean=True): StringA;
function DecodeLineW(var p:PWideChar; ASkipEmpty:Boolean=True): StringW;
//跳过空白字符，对于 Ansi编码，跳过的是#9#10#13#161#161，对于UCS编码，跳过的是#9#10#13#$3000
function SkipSpaceA(var p: PAnsiChar): Integer;
function SkipSpaceU(var p: PAnsiChar): Integer;
function SkipSpaceW(var p: PWideChar): Integer;
//跳过一行,以#10为行结尾
function SkipLineA(var p: PAnsiChar): Integer;
function SkipLineU(var p: PAnsiChar): Integer;
function SkipLineW(var p: PWideChar): Integer;
//检查是否是空白字符
function IsSpaceA(const c:PAnsiChar; ASpaceSize:PInteger=nil): Boolean;
function IsSpaceU(const c:PAnsiChar; ASpaceSize:PInteger=nil): Boolean;
function IsSpaceW(const c:PWideChar; ASpaceSize:PInteger=nil): Boolean;
//跳过直接遇到指定的字符
{$IFNDEF NEXTGEN}
function SkipUntilA(var p: PAnsiChar; AExpects: PAnsiChar; AQuoter: AnsiChar = {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF}): Integer;
function SkipUntilU(var p: PAnsiChar; AExpects: PAnsiChar; AQuoter: AnsiChar = {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF}): Integer;
{$ENDIF}
function SkipUntilW(var p: PWideChar; AExpects: PWideChar; AQuoter: WideChar = #0): Integer;
//判断是否是以指定的字符串开始
function StartWith(s, startby: PChar; AIgnoreCase: Boolean): Boolean;
function StartWithIgnoreCase(s, startby: PChar): Boolean;
//字符串转数字
function PCharToFloat(const S: PChar; Len: Integer): Double;

var
  // 系统 ACP
  SysACP: Integer;
{$IFDEF USE_STRENCODEFUNC}
  // 按照Java格式编码，将#$0字符编码为#$C080
  JavaFormatUtf8: Boolean = True;
{$ENDIF}

implementation

resourcestring
  SOutOfIndex = '索引越界，值 %d 不在[%d..%d]的范围内。';
  SBadUnicodeChar = '无效的Unicode字符:%d';
{$IFDEF USE_URLFUNC}
  sErrorDecodingURLText = 'Error decoding URL style (%%XX) encoded string at position %d';
  sInvalidURLEncodedChar = 'Invalid URL encoded character (%s) at position %d';
{$ENDIF}

{$IFDEF MSWINDOWS}
type
  TMSVCStrStr = function(s1, s2: PAnsiChar): PAnsiChar; cdecl;
  TMSVCStrStrW = function(s1, s2: PWideChar): PWideChar; cdecl;
  TMSVCMemCmp = function(s1, s2: Pointer; len: Integer): Integer; cdecl;
var
  hMsvcrtl: HMODULE;
  VCStrStr: TMSVCStrStr;
  VCStrStrW: TMSVCStrStrW;
  VCMemCmp: TMSVCMemCmp;
{$ENDIF}

const
  ConvertInt: array[0..255] of Integer =
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
      V := ConvertInt[ord(s[i])];
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
      V := ConvertInt[ord(s[i])];
      if V<0 then begin
        Result := def;
        Exit;
      end;
      result := (result * 10) + V;
    end;
  end;
end;

function WideStrLen(S: PWideChar): Integer; inline;
begin
  Result := 0;
  if S <> nil then
    while S^ <> #0 do begin
      Inc(Result);
      Inc(S);
    end;
end;

function AnsiStrLen(s: PAnsiChar): Integer; inline;
begin
  Result := 0;
  if s <> nil then
    {$IFDEF POSIX}
    while S^ <> 0 do begin
    {$ELSE}
    while S^ <> #0 do begin
    {$ENDIF}
      Inc(Result);
      Inc(s);
    end;
end;

function Fetch(var AInput: StringA; const ADelim: StringA = ' ';
  const ADelete: Boolean = True): StringA;
var
  LPos: Integer;
begin
  if ADelim = #0 then
    LPos := Pos(ADelim, AInput)
  else
    LPos := Pos(ADelim, AInput);
  if LPos = 0 then begin
    Result := AInput;
    if ADelete then AInput := '';
  end else begin
    Result := Copy(AInput, 1, LPos - 1);
    if ADelete then
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
  end;
end;

function Fetch(var AInput: StringW; const ADelim: StringW;
  const ADelete, ACaseSensitive: Boolean): StringW;
var
  LPos: Integer;
begin
  if ACaseSensitive then begin
    if ADelim = #0 then
      LPos := Pos(ADelim, AInput)
    else
      LPos := Pos(ADelim, AInput);
  end else begin
    if ADelim = #0 then
      LPos := Pos(LowerCase(ADelim), LowerCase(AInput))
    else
      LPos := Pos(LowerCase(ADelim), LowerCase(AInput));
  end;
  if LPos = 0 then begin
    Result := AInput;
    if ADelete then
      AInput := '';    {Do not Localize}
  end else begin
    Result := Copy(AInput, 1, LPos - 1);
    if ADelete then
      AInput := Copy(AInput, LPos + Length(ADelim), MaxInt);
  end;
end;

procedure ExchangeByteOrder(p: PCharA; l: Integer);
var
  pe: PCharA;
  c: CharA;
begin
  pe := p;
  Inc(pe, l);
  while IntPtr(p) < IntPtr(pe) do begin
    c := p^;
    p^ := PCharA(IntPtr(p) + 1)^;
    PCharA(IntPtr(p) + 1)^ := c;
    Inc(p, 2);
  end;
end;

function ExchangeByteOrder(V: Smallint): Smallint;
var
  pv: array [0 .. 1] of Byte absolute V;
  pd: array [0 .. 1] of Byte absolute Result;
begin
  pd[0] := pv[1];
  pd[1] := pv[0];
end;

function ExchangeByteOrder(V: Word): Word;
var
  pv: array [0 .. 1] of Byte absolute V;
  pd: array [0 .. 1] of Byte absolute Result;
begin
  pd[0] := pv[1];
  pd[1] := pv[0];
end;

function ExchangeByteOrder(V: Integer): Integer;
var
  pv: array [0 .. 3] of Byte absolute V;
  pd: array [0 .. 3] of Byte absolute Result;
begin
  pd[0] := pv[3];
  pd[1] := pv[2];
  pd[2] := pv[1];
  pd[3] := pv[0];
end;

function ExchangeByteOrder(V: Cardinal): Cardinal;
var
  pv: array [0 .. 3] of Byte absolute V;
  pd: array [0 .. 3] of Byte absolute Result;
begin
  pd[0] := pv[3];
  pd[1] := pv[2];
  pd[2] := pv[1];
  pd[3] := pv[0];
end;

function ExchangeByteOrder(V: Int64): Int64;
var
  pv: array [0 .. 7] of Byte absolute V;
  pd: array [0 .. 7] of Byte absolute Result;
begin
  pd[0] := pv[7];
  pd[1] := pv[6];
  pd[2] := pv[5];
  pd[3] := pv[4];
  pd[4] := pv[3];
  pd[5] := pv[2];
  pd[6] := pv[1];
  pd[7] := pv[0];
end;

function ExchangeByteOrder(V: Single): Single;
var
  pv: array [0 .. 3] of Byte absolute V;
  pd: array [0 .. 3] of Byte absolute Result;
begin
  pd[0] := pv[3];
  pd[1] := pv[2];
  pd[2] := pv[1];
  pd[3] := pv[0];
end;

function ExchangeByteOrder(V: Double): Double;
var
  pv: array [0 .. 7] of Byte absolute V;
  pd: array [0 .. 7] of Byte absolute Result;
begin
  pd[0] := pv[7];
  pd[1] := pv[6];
  pd[2] := pv[5];
  pd[3] := pv[4];
  pd[4] := pv[3];
  pd[5] := pv[2];
  pd[6] := pv[1];
  pd[7] := pv[0];
end;

function StrDupX(const s: PChar; ACount:Integer): String;
begin
  SetLength(Result, ACount);
  Move(s^, PChar(Result)^, ACount{$IFDEF UNICODE} shl 1{$ENDIF});
end;

function StrDupXA(const s: PCharA; ACount:Integer): StringA;
begin
  {$IFDEF NEXTGEN}
  Result.From(s, 0, ACount);
  {$ELSE}
  SetLength(Result, ACount);
  Move(s^, PCharA(Result)^, ACount);
  {$ENDIF}
end;

function StrDupXW(const s: PCharW; ACount:Integer): StringW;
begin
  SetLength(Result, ACount);
  Move(s^, PCharW(Result)^, ACount shl 1);
end;

function StrDup(const S: PChar; AOffset: Integer; const ACount: Integer): String;
var
  C, ACharSize: Integer;
  p, pds, pd: PChar;
begin
  C := 0;
  p := S + AOffset;
  SetLength(Result, 4096);
  pd := PChar(Result);
  pds := pd;
  while (p^ <> #0) and (C < ACount) do begin
    ACharSize := {$IFDEF UNICODE} CharSizeW(p); {$ELSE} CharSizeA(p); {$ENDIF}
    AOffset := pd - pds;
    if AOffset + ACharSize = Length(Result) then begin
      SetLength(Result, Length(Result){$IFDEF UNICODE} shl 1{$ENDIF});
      pds := PChar(Result);
      pd := pds + AOffset;
    end;
    Inc(C);
    pd^ := p^;
    if ACharSize = 2 then
      pd[1] := p[1];
    Inc(pd, ACharSize);
    Inc(p, ACharSize);
  end;
  SetLength(Result, pd-pds);
end;

function CharUpper(c: Char): Char;
begin
  {$IFDEF UNICODE}
  Result := CharUpperW(c);
  {$ELSE}
  Result := CharUpperA(c);
  {$ENDIF};
end;

function CharUpperA(c: AnsiChar): AnsiChar;
begin
  {$IFNDEF NEXTGEN}
  if (c>=#$61) and (c<=#$7A) then
  {$ELSE}
  if (c>=$61) and (c<=$7A) then
  {$ENDIF}
    Result := AnsiChar(Ord(c)-$20)
  else
    Result := c;
end;

function CharUpperW(c: WideChar): WideChar;
begin
  if (c>=#$61) and (c<=#$7A) then
    Result := WideChar(PWord(@c)^-$20)
  else
    Result := c;
end;

function StrScanW(const Str: PCharW; Chr: CharW): PCharW; 
begin
  Result := Str;
  while Result^ <> Chr do begin
    if Result^ = #0 then begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function MemScan(S: Pointer; len_s: Integer; sub: Pointer; len_sub: Integer): Pointer;
var
  pb_s, pb_sub, pc_sub, pc_s: PByte;
  remain: Integer;
begin
  if len_s > len_sub then begin
    pb_s := S;
    pb_sub := sub;
    Result := nil;
    while len_s >= len_sub do begin
      if pb_s^ = pb_sub^ then begin
        remain := len_sub - 1;
        pc_sub := pb_sub;
        pc_s := pb_s;
        Inc(pc_s);
        Inc(pc_sub);
        if BinaryCmp(pc_s, pc_sub, remain) = 0 then begin
          Result := pb_s;
          Break;
        end;
      end;
      Inc(pb_s);
    end;
  end else if len_s = len_sub then begin
    if CompareMem(S, sub, len_s) then
      Result := S
    else
      Result := nil;
  end else
    Result := nil;
end;

function BinaryCmp(const p1, p2: Pointer; len: Integer): Integer;
  function CompareByByte: Integer;
  var
    b1, b2: PByte;
  begin
    if (len <= 0) or (p1 = p2) then
      Result := 0
    else begin
      b1 := p1;
      b2 := p2;
      Result := 0;
      while len > 0 do begin
        if b1^ <> b2^ then begin
          Result := b1^ - b2^;
          Exit;
        end;
        Inc(b1);
        Inc(b2);
      end;
    end;
  end;
begin
  {$IFDEF MSWINDOWS}
  if Assigned(VCMemCmp) then
    Result := VCMemCmp(p1, p2, len)
  else
    Result := CompareByByte;
  {$ELSE}
  Result := memcmp(p1, p2, len);
  {$ENDIF}
end;

function StrStr(src, sub: string): Integer;
var
  p1, p2: PChar;
begin
  p1 := PChar(src);
  p2 := PChar(sub);
  {$IFDEF UNICODE}
  p2 := StrStrW(p1, p2);
  {$ELSE}
  p2 := StrStrA(p1, p2);
  {$ENDIF};
  if p2 <> nil then
    Result := p2 - p1
  else
    Result := -1;
end;

function StrIStr(src, sub: string): Integer;
var
  p1, p2: PChar;
begin
  p1 := PChar(src);
  p2 := PChar(sub);
  {$IFDEF UNICODE}
  p2 := StrIStrW(p1, p2);
  {$ELSE}
  p2 := StrIStrA(p1, p2);
  {$ENDIF};
  if p2 <> nil then
    Result := p2 - p1
  else
    Result := -1;
end;

function StrStr(src, sub: PChar): PChar;
begin
  {$IFDEF UNICODE}
  Result := StrStrW(src, sub);
  {$ELSE}
  Result := StrStrA(src, sub);
  {$ENDIF};
end;

function DoStrStrASearch(s1, ps2: PAnsiChar): PAnsiChar; inline;
var
  ps1: PAnsiChar;
begin
  ps1 := s1;
  Inc(ps1);
  Inc(ps2);
  while ps2^ <> {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
    if ps1^ = ps2^ then begin
      Inc(ps1);
      Inc(ps2);
    end else
      Break;
  end;
  if ps2^ = {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} then
    Result := s1
  else
    Result := nil;
end;

function StrStrA(src, sub: PAnsiChar): PAnsiChar;
begin
  {$IFDEF MSWINDOWS}
  if Assigned(VCStrStr) then begin
    Result := VCStrStr(src, sub);
    Exit;
  end;
  {$ENDIF}
  Result := nil;
  if (src <> nil) and (sub <> nil) then begin
    while src^ <> {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
      if src^ = sub^ then begin
        Result := DoStrStrASearch(src, sub);
        if Result <> nil then
          Exit;
      end;
      Inc(src);
    end;
  end;
end;

function StrStrW(src, sub: PWideChar): PWideChar;
var
  I: Integer;
begin
  {$IFDEF MSWINDOWS}
  if Assigned(VCStrStrW) then begin
    Result := VCStrStrW(src, sub);
    Exit;
  end;
  {$ENDIF}
  if (sub = nil) or (sub^ = #0) then
    Result := src
  else begin
    Result := nil;
    while src^ <> #0 do begin
      if src^ = sub^ then begin
        I := 1;
        while sub[I] <> #0 do begin
          if src[I] = sub[I] then
            Inc(I)
          else
            Break;
        end;
        if sub[I] = #0 then begin
          Result := src;
          Break;
        end;
      end;
      Inc(src);
    end;
  end;
end;

function StrIStr(src, sub: PChar): PChar;
begin
  {$IFDEF UNICODE}
  Result := StrIStrW(src, sub);
  {$ELSE}
  Result := StrIStrA(src, sub);
  {$ENDIF};
end;

function DoStrStrAISearch(s1, ps2: PAnsiChar): PAnsiChar; inline;
var
  ps1: PAnsiChar;
begin
  ps1 := s1;
  Inc(ps1);
  Inc(ps2);
  while ps2^ <> {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
    if CharUpperA(ps1^) = CharUpperA(ps2^) then begin
      Inc(ps1);
      Inc(ps2);
    end else
      Break;
  end;
  if ps2^ = {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} then
    Result := s1
  else
    Result := nil;
end;

function StrIStrA(src, sub: PAnsiChar): PAnsiChar;
begin
  Result := nil;
  if (src <> nil) and (sub <> nil) then begin
    while src^ <> {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
      if CharUpperA(src^) = CharUpperA(sub^) then begin
        Result := DoStrStrAISearch(src, sub);
        if Result <> nil then
          Exit;
      end;
      Inc(src);
    end;
  end;
end;

function StrIStrW(src, sub: PWideChar): PWideChar;
var
  I: Integer;
  ws2: StringW;
begin
  Result := nil;
  if (src = nil) or (sub = nil) then
    Exit;
  ws2 := UpperCase(sub);
  sub := PWideChar(ws2);
  while src^ <> #0 do begin
    if CharUpperW(src^) = sub^ then begin
      I := 1;
      while sub[I] <> #0 do begin
        if CharUpperW(src[I]) = sub[I] then
          Inc(I)
        else
          Break;
      end;
      if sub[I] = #0 then begin
        Result := src;
        Break;
      end;
    end;
    Inc(src);
  end;
end;

function PosStr(sub, src: AnsiString; Offset: Integer = 0): Integer;
begin
  Result := PosStr(PAnsiChar(sub), Length(sub), PAnsiChar(src), Length(src), Offset);
  if Result <> -1 then
    Inc(Result);
end;

function PosStr(sub: PAnsiChar; src: PAnsiChar; Offset: Integer): Integer;
begin
  Result := PosStr(sub, AnsiStrLen(sub), src, AnsiStrLen(src), Offset);
end;

function PosStr(sub: PAnsiChar; subLen: Integer; src: PAnsiChar; Offset: Integer): Integer;
begin
  Result := PosStr(sub, subLen, src, AnsiStrLen(src), Offset);
end;

function PosStr(sub: PAnsiChar; subLen: Integer; src: PAnsiChar; srcLen: Integer; Offset: Integer): Integer;
var
  p: PAnsiChar;
  j: Integer;
begin
  Result := -1;
  if (sub = nil) or (src = nil) then
    Exit;
  if (Offset > 0) then Dec(srcLen, Offset);
  if (subLen <= srcLen) and (subLen > 0) then begin
    p := src;
    Inc(p, Offset);
    Dec(subLen);
    Dec(srcLen, subLen);
    while srcLen > 0 do begin
      if PByte(p)^ = PByte(sub)^ then begin
        if subLen > 0 then begin
          for j := 1 to subLen do
            {$IFDEF NEXTGEN}
            if PAnsiChar(IntPtr(p)+j) <> PAnsiChar(IntPtr(sub)+j) then Break;
            {$ELSE}
            if p[j] <> sub[j] then Break;
            {$ENDIF}
        end else
          j := 1;
        if j > subLen then begin
          {$IFDEF NEXTGEN}
          Result := IntPtr(p) - IntPtr(src);
          {$ELSE}
          Result := p - src;
          {$ENDIF}
          Exit;
        end;
      end;
      Inc(p);
      Dec(srcLen);
    end;
  end;
end;

function PosStr(const sub, src: StringW; Offset: Integer): Integer;
begin
  Result := PosStr(PCharW(sub), Length(sub), PCharW(src), Length(src), Offset);
  if Result <> -1 then
    Inc(Result);
end;

function PosStr(sub: PCharW; subLen: Integer; src: PCharW; srcLen: Integer; Offset: Integer): Integer;
var
  p: PCharW;
  j: Integer;
begin
  Result := -1;
  if (sub = nil) or (src = nil) then
    Exit;
  if (Offset > 0) then Dec(srcLen, Offset);
  if (subLen <= srcLen) and (subLen > 0) then begin
    p := src;
    Inc(p, Offset);
    Dec(subLen);
    Dec(srcLen, subLen);
    while srcLen > 0 do begin
      if p^ = sub^ then begin
        if subLen > 0 then begin
          for j := 1 to subLen do
            if p[j] <> sub[j] then Break;
        end else
          j := 1;
        if j > subLen then begin
          Result := p - src;
          Exit;
        end;
      end;
      Inc(p);
      Dec(srcLen);
    end;
  end;
end;

function RPosStr(sub, src: AnsiString; Offset: Integer = 0): Integer;
begin
  Result := RPosStr(PAnsiChar(sub), Length(sub), PAnsiChar(src), Length(src), Offset);
  if Result <> -1 then
    Inc(Result);
end;

function RPosStr(sub: PAnsiChar; src: PAnsiChar; Offset: Integer = 0): Integer;
begin
  Result := RPosStr(sub, AnsiStrLen(sub), src, AnsiStrLen(src), Offset);
end;

function RPosStr(sub: PAnsiChar; subLen: Integer; src: PAnsiChar; srcLen: Integer; Offset: Integer): Integer;
var
  p: PAnsiChar;
  j: Integer;
begin
  Result := -1;
  if (sub = nil) or (src = nil) then
    Exit;
  p := src;
  Inc(p, srcLen);
  if (Offset > 0) then begin
    Dec(p, Offset + 1);
    Dec(srcLen, Offset);
  end else
    Dec(p);
  if (subLen <= srcLen) and (subLen > 0) then begin  
    while srcLen > 0 do begin
      if PByte(p)^ = PByte(sub)^ then begin
        if (subLen > 1) then begin
          for j := 1 to subLen do
            {$IFDEF NEXTGEN}
            if PAnsiChar(IntPtr(p)+j) <> PAnsiChar(IntPtr(sub)+j) then Break;
            {$ELSE}
            if p[j] <> sub[j] then Break;
            {$ENDIF}
        end else
          j := 1;
        if j = subLen then begin
          {$IFDEF NEXTGEN}
          Result := IntPtr(p) - IntPtr(src) + 1;
          {$ELSE}
          Result := p - src + 1;
          {$ENDIF}
          Exit;
        end;
      end;
      Dec(p);
      Dec(srcLen);
    end;
  end;
end;

function RPosStr(sub, src: StringW; Offset: Integer = 0): Integer;
begin
  Result := RPosStr(PCharW(sub), Length(sub), PCharW(src), Length(src), Offset);
  if Result <> -1 then
    Inc(Result);
end;

function RPosStr(sub: PCharW; src: PCharW; Offset: Integer = 0): Integer;
begin
  Result := RPosStr(sub, WideStrLen(sub), src, WideStrLen(src), Offset);
end;

function RPosStr(sub: PCharW; subLen: Integer; src: PCharW; srcLen: Integer; Offset: Integer): Integer;
var
  p: PCharW;
  j: Integer;
begin
  Result := -1;
  if (sub = nil) or (src = nil) then
    Exit;
  p := src;
  Inc(p, srcLen);
  if (Offset > 0) then begin
    Dec(p, Offset + 1);
    Dec(srcLen, Offset);
  end else
    Dec(p);
  if (subLen <= srcLen) and (subLen > 0) then begin
    while srcLen > 0 do begin
      if p^ = sub^ then begin
        if (subLen > 1) then begin
          for j := 1 to subLen do
            if p[j] <> sub[j] then Break;
        end else
          j := 1;
        if j = subLen then begin
          Result := p - src + 1;
          Exit;
        end;
      end;
      Dec(p);
      Dec(srcLen);
    end;
  end;
end;

function LengthAsDWordToHex(const Value: Cardinal): Integer;
begin
  if Value < $10 then
    Result := 1
  else if Value < $100 then
    Result := 2
  else if Value < $1000 then
    Result := 3
  else if Value < $10000 then
    Result := 4
  else if Value < $100000 then
    Result := 5
  else if Value < $1000000 then
    Result := 6
  else if Value < $10000000 then
    Result := 7
  else
    Result := 8;
end;

{$IFDEF USE_URLFUNC}
function UrlDecode(const Src: PCharA; OutBuf: PCharA; RaiseError: Boolean): Integer;
const
  H_BYTE:array[0..255] of Smallint =
  (
    -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,$00,$10,$20,$30,$40,$50,$60,$70,$80,$90,-1,-1,-1,-1,-1,-1
    ,-1,$A0,$B0,$C0,$D0,$E0,$F0,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
    ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ); 
  L_BYTE: array[0..255] of Smallint =
  (
   -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,$00,$01,$02,$03,$04,$05,$06,$07,$08,$09,-1,-1,-1,-1,-1,-1
  ,-1,$0A,$0B,$0C,$0D,$0E,$0F,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  ,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
  );
var
  Sp, Rp: PAnsichar;
  HB, LB: SmallInt;
begin
  Result := -1;
  if (Src = nil) or (OutBuf = nil) then
    Exit;
  Sp := Src;
  Rp := OutBuf;
  LB := -1;
  while Sp^ <> #0 do begin
    case Sp^ of
      '+': Rp^ := #32;
      '%': begin
             // Look for an escaped % (%%) or %<hex> encoded character
             Inc(Sp);
             if Sp^ = '%' then
               Rp^ := '%'
             else begin
               HB := H_BYTE[Byte(Sp^)];
               if HB <> 0 then               
                LB := L_BYTE[Byte((Sp+1)^)];
               if (HB <> -1) and (LB <> -1) then begin
                 Rp^ := AnsiChar(HB + LB);
                 Inc(Sp);
               end else begin
                 if RaiseError then
                   raise Exception.Create(Format(sErrorDecodingURLText, [Sp - Src]))
                 else
                   Exit;
               end;
             end;
           end;
    else
      Rp^ := Sp^;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  Result := Rp - OutBuf;
end;

function UrlDecode(const AStr: StringA; RaiseError: Boolean): StringA;
var
  I: Integer;
begin
  if Length(AStr) > 0 then begin
    SetLength(Result, Length(AStr));
    I := UrlDecode(Pointer(AStr), Pointer(Result), RaiseError);
    if I = -1 then
      Result := ''
    else if I <> Length(Result) then
      SetLength(Result, I);   
  end else
    Result := '';
end;

function UrlEncodeA(const AStr: PCharA; OutBuf: PCharA): Integer;
const
  HTTP_CONVERT: array[0..255] of PCharA = (
    ' %00#','%01#','%02#','%03#','%04#','%05#','%06#','%07#'
    ,'%08#','%09#','%0A#','%0B#','%0C#','%0D#','%0E#','%0F#'
    ,'%10#','%11#','%12#','%13#','%14#','%15#','%16#','%17#'
    ,'%18#','%19#','%1A#','%1B#','%1C#','%1D#','%1E#','%1F#'
    ,'%20#',''    ,'%22#','%23#',''    ,'%25#','%26#',''
    ,''    ,''    ,''    ,'%2B#','%2C#',''    ,''    ,'%2F#'
    ,''    ,''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''    ,''    ,'%3A#','%3B#','%3C#','%3D#','%3E#','%3F#'
    ,''    ,''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''    ,''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''    ,''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''    ,''    ,''    ,'%5B#','%5C#','%5D#','%5E#',''
    ,'%60#',''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''     ,''   ,''    ,''    ,''    ,''    ,''    ,''
    ,''     ,''   ,''    ,''    ,''    ,''    ,''    ,''
    ,''     ,''   ,''     ,'%7B#','%7C#','%7D#','%7E#','%7F#'
    ,'%80#','%81#','%82#','%83#','%84#','%85#','%86#','%87#'
    ,'%88#','%89#','%8A#','%8B#','%8C#','%8D#','%8E#','%8F#'
    ,'%90#','%91#','%92#','%93#','%94#','%95#','%96#','%97#'
    ,'%98#','%99#','%9A#','%9B#','%9C#','%9D#','%9E#','%9F#'
    ,'%A0#','%A1#','%A2#','%A3#','%A4#','%A5#','%A6#','%A7#'
    ,'%A8#','%A9#','%AA#','%AB#','%AC#','%AD#','%AE#','%AF#'
    ,'%B0#','%B1#','%B2#','%B3#','%B4#','%B5#','%B6#','%B7#'
    ,'%B8#','%B9#','%BA#','%BB#','%BC#','%BD#','%BE#','%BF#'
    ,'%C0#','%C1#','%C2#','%C3#','%C4#','%C5#','%C6#','%C7#'
    ,'%C8#','%C9#','%CA#','%CB#','%CC#','%CD#','%CE#','%CF#'
    ,'%D0#','%D1#','%D2#','%D3#','%D4#','%D5#','%D6#','%D7#'
    ,'%D8#','%D9#','%DA#','%DB#','%DC#','%DD#','%DE#','%DF#'
    ,'%E0#','%E1#','%E2#','%E3#','%E4#','%E5#','%E6#','%E7#'
    ,'%E8#','%E9#','%EA#','%EB#','%EC#','%ED#','%EE#','%EF#'
    ,'%F0#','%F1#','%F2#','%F3#','%F4#','%F5#','%F6#','%F7#'
    ,'%F8#','%F9#','%FA#','%FB#','%FC#','%FD#','%FE#','%FF#'
  );
var
  Sp, Rp, P: PCharA;
begin
  Sp := AStr;
  Rp := OutBuf;
  while Sp^ <> #0 do begin
    if Sp^ = ' ' then
      Rp^ := '+'
    else begin
      P := HTTP_CONVERT[Ord(Sp^)];
      if P^ = #0 then
        Rp^ := Sp^
      else begin
        PInteger(Rp)^ := PInteger(P)^;
        Inc(Rp, 2);
      end;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  Result := Rp - OutBuf;
end;

function UrlEncodeW(const AStr: PCharW; OutBuf: PCharW): Integer;
const
  HTTP_CONVERT: array[0..255] of PCharW = (
    ' %00#','%01#','%02#','%03#','%04#','%05#','%06#','%07#'
    ,'%08#','%09#','%0A#','%0B#','%0C#','%0D#','%0E#','%0F#'
    ,'%10#','%11#','%12#','%13#','%14#','%15#','%16#','%17#'
    ,'%18#','%19#','%1A#','%1B#','%1C#','%1D#','%1E#','%1F#'
    ,'%20#',''    ,'%22#','%23#',''    ,'%25#','%26#',''
    ,''    ,''    ,''    ,'%2B#','%2C#',''    ,''    ,'%2F#'
    ,''    ,''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''    ,''    ,'%3A#','%3B#','%3C#','%3D#','%3E#','%3F#'
    ,''    ,''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''    ,''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''    ,''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''    ,''    ,''    ,'%5B#','%5C#','%5D#','%5E#',''
    ,'%60#',''    ,''    ,''    ,''    ,''    ,''    ,''
    ,''     ,''   ,''    ,''    ,''    ,''    ,''    ,''
    ,''     ,''   ,''    ,''    ,''    ,''    ,''    ,''
    ,''     ,''   ,''     ,'%7B#','%7C#','%7D#','%7E#','%7F#'
    ,'%80#','%81#','%82#','%83#','%84#','%85#','%86#','%87#'
    ,'%88#','%89#','%8A#','%8B#','%8C#','%8D#','%8E#','%8F#'
    ,'%90#','%91#','%92#','%93#','%94#','%95#','%96#','%97#'
    ,'%98#','%99#','%9A#','%9B#','%9C#','%9D#','%9E#','%9F#'
    ,'%A0#','%A1#','%A2#','%A3#','%A4#','%A5#','%A6#','%A7#'
    ,'%A8#','%A9#','%AA#','%AB#','%AC#','%AD#','%AE#','%AF#'
    ,'%B0#','%B1#','%B2#','%B3#','%B4#','%B5#','%B6#','%B7#'
    ,'%B8#','%B9#','%BA#','%BB#','%BC#','%BD#','%BE#','%BF#'
    ,'%C0#','%C1#','%C2#','%C3#','%C4#','%C5#','%C6#','%C7#'
    ,'%C8#','%C9#','%CA#','%CB#','%CC#','%CD#','%CE#','%CF#'
    ,'%D0#','%D1#','%D2#','%D3#','%D4#','%D5#','%D6#','%D7#'
    ,'%D8#','%D9#','%DA#','%DB#','%DC#','%DD#','%DE#','%DF#'
    ,'%E0#','%E1#','%E2#','%E3#','%E4#','%E5#','%E6#','%E7#'
    ,'%E8#','%E9#','%EA#','%EB#','%EC#','%ED#','%EE#','%EF#'
    ,'%F0#','%F1#','%F2#','%F3#','%F4#','%F5#','%F6#','%F7#'
    ,'%F8#','%F9#','%FA#','%FB#','%FC#','%FD#','%FE#','%FF#'
  );
var
  Sp, Rp, P: PCharW;
  Buf: array [0..4] of Byte;
  I, J: Integer;
begin
  Sp := AStr;
  Rp := OutBuf;
  while Sp^ <> #0 do begin
    if Sp^ = ' ' then
      Rp^ := '+'
    else begin
      if Ord(Sp^) > $FF then begin
        I := WideCharToMultiByte(CP_ACP, 0, Sp, 1, @Buf[0], 4, nil, nil);
        for J := 0 to I - 1 do begin
          P := HTTP_CONVERT[Buf[J]];
          PInt64(Rp)^ := PInt64(P)^;
          Inc(Rp, 3);
        end;
        Inc(Sp);
        Continue;
      end else begin
        P := HTTP_CONVERT[Ord(Sp^)];
        if P^ = #0 then
          Rp^ := Sp^
        else begin
          PInt64(Rp)^ := PInt64(P)^;
          Inc(Rp, 2);
        end;
      end;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  Result := Rp - OutBuf;
end;

function UrlEncode(const AUrl: StringA): StringA;
var
  I: Integer;
begin
  if Length(AUrl) > 0 then begin
    SetLength(Result, Length(AUrl) * 3);
    I := UrlEncodeA(PAnsiChar(AUrl), @Result[1]);
    if Length(Result) <> I then
      SetLength(Result, I);
  end else
    Result := '';
end;

function UrlEncode(const AUrl: StringW): StringW;
var
  I: Integer;
begin
  if Length(AUrl) > 0 then begin
    SetLength(Result, Length(AUrl) * 3);
    I := UrlEncodeW(PWideChar(AUrl), @Result[1]);
    if Length(Result) <> I then
      SetLength(Result, I);
  end else
    Result := '';
end;
{$ENDIF}

{$IFDEF USE_URLFUNC}
function XDigit(Ch : AnsiChar) : Integer;
begin
  {$IFDEF NEXTGEN}
  if (Ch >= Ord('0')) and (Ch <= Ord('9')) then
  {$ELSE}
  if (Ch >= '0') and (Ch <= '9') then
  {$ENDIF}
      Result := Ord(Ch) - Ord('0')
  else
      Result := (Ord(Ch) and 15) + 9;
end;

function UTFStrToUnicode(UTFStr: StringA): StringW;
var
  I:Integer;
  Index:Integer;
  HexStr:String;
  LowerCaseUTFStr:String;
  WChar:WideChar;
  WCharWord:Word;
  AChar:AnsiChar;
begin
  ////\u60a8\u7684\u9a8c\u8bc1\u7801\u9519\u8bef
  Result:='';
  LowerCaseUTFStr := LowerCase(string(UTFStr));
  Index:=PosEx('\u',LowerCaseUTFStr,1);
  while Index>0 do
  begin
    HexStr:=Copy(LowerCaseUTFStr,Index+2,4);
    WCharWord:=0;
    //HexStr=60a8
    for I := 1 to Length(HexStr) do
    begin
      AChar:=AnsiChar(HexStr[I]);
      WCharWord:=WCharWord+XDigit(AChar)*Ceil(Power(16,4-I));
    end;
    WChar:=WideChar(WCharWord);
    //WChar=您
    Result:=Result+WChar;
    Index:=PosEx('\u',LowerCaseUTFStr,Index+6);
  end;
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
procedure CalcCharLengthA(var Lens: TIntArray; list: PAnsiChar);
var
  i, l: Integer;
begin
  i := 0;
  System.SetLength(Lens, Length(List));
  while i< Length(List) do begin
    l := CharSizeA(@list[i]);
    lens[i] := l;
    Inc(i, l);
  end;
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
procedure CalcCharLengthU(var Lens: TIntArray; list: PAnsiChar);
var
  i, l: Integer;
begin
  i := 0;
  System.SetLength(Lens, Length(List));
  while i< Length(List) do begin
    l := CharSizeU(@list[i]);
    lens[i] := l;
    Inc(i, l);
  end;
end;
{$ENDIF}

// 检查字符是否在指定的列表中
function CharIn(const c, list: PChar; ACharLen:PInteger = nil): Boolean;
begin
{$IFDEF UNICODE}
  Result := CharInW(c, list, ACharLen);
{$ELSE}
  Result := CharInA(c, list, ACharLen);
{$ENDIF}
end;

{$IFNDEF NEXTGEN}
function CharInA(c, list: PAnsiChar; ACharLen: PInteger = nil): Boolean;
var
  i: Integer;
  lens: TIntArray;
begin
  Result := False;
  CalcCharLengthA(lens, list);
  i := 0;
  while i < Length(list) do begin
    if CompareMem(c, @list[i], lens[i]) then begin
      if ACharLen <> nil then
        ACharLen^:=lens[i];
      Result := True;
      Break;
    end else
      Inc(i, lens[i]);
  end;
end;
{$ENDIF}

function CharInW(c, list: PWideChar; ACharLen: PInteger = nil): Boolean;
var
  p: PWideChar;
begin
  Result:=False;
  p := list;
  while p^ <> #0 do begin
    if p^ = c^ then begin
      if (p[0]>=#$DB00) and (p[0]<=#$DBFF) then begin
        if p[1]=c[1] then begin
          Result := True;
          if ACharLen <> nil then
            ACharLen^ := 2;
          Break;
        end;
      end else begin
        Result := True;
        if ACharLen <> nil then
          ACharLen^ := 1;
        Break;
      end;
    end;
    Inc(p);
  end;
end;

{$IFNDEF NEXTGEN}
function CharInU(c, list: PAnsiChar; ACharLen: PInteger = nil): Boolean;
var
  i: Integer;
  lens: TIntArray;
begin
  Result := False;
  CalcCharLengthU(lens, list);
  i := 0;
  while i < Length(list) do begin
    if CompareMem(c, @list[i], lens[i]) then begin
      if ACharLen <> nil then
        ACharLen^ := lens[i];
      Result := True;
      Break;
    end else
      Inc(i, lens[i]);
  end;
end;
{$ENDIF}

//计算当前字符的长度
// GB18030,兼容GBK和GB2312
// 单字节，其值从0到0x7F。
// 双字节，第一个字节的值从0x81到0xFE，第二个字节的值从0x40到0xFE（不包括0x7F）。
// 四字节，第一个字节的值从0x81到0xFE，第二个字节的值从0x30到0x39，第三个字节从0x81到0xFE，第四个字节从0x30到0x39。
function CharSizeA(c: PAnsiChar): Integer;
begin
  if SysACP = 936 then begin
    Result:=1;
    {$IFDEF NEXTGEN}
    if (c^>=$81) and (c^<=$FE) then begin
      Inc(c);
      if (c^>=$40) and (c^<=$FE) and (c^<>$7F) then
        Result:=2
      else if (c^>=$30) and (c^<=$39) then begin
        Inc(c);
        if (c^>=$81) and (c^<=$FE) then begin
          Inc(c);
          if (c^>=$30) and (c^<=$39) then
            Result:=4;
        end;
      end;
    end;
    {$ELSE}
    if (c^>=#$81) and (c^<=#$FE) then begin
      Inc(c);
      if (c^>=#$40) and (c^<=#$FE) and (c^<>#$7F) then
        Result:=2
      else if (c^>=#$30) and (c^<=#$39) then begin
        Inc(c);
        if (c^>=#$81) and (c^<=#$FE) then begin
          Inc(c);
          if (c^>=#$30) and (c^<=#$39) then
            Result:=4;
        end;
      end;
    end;
    {$ENDIF}
  end else
    {$IFDEF UNICODE}
    {$IFDEF NEXTGEN}
    if TEncoding.ANSI.CodePage = CP_UTF8 then
      Result := CharSizeU(c)
    else if (c^<128) or (TEncoding.ANSI.CodePage=437) then
      Result:=1
    else
      Result:=2;
    {$ELSE}
    {$IF RTLVersion>26}
    Result := AnsiStrings.StrCharLength(PAnsiChar(c));
    {$ELSE}
    Result := Sysutils.StrCharLength(PAnsiChar(c));
    {$IFEND}
    {$ENDIF}
    {$ELSE}
    Result := StrCharLength(PAnsiChar(c));
    {$ENDIF}
end;

function CharSizeU(c: PAnsiChar): Integer;
begin
  if (Ord(c^) and $80) = 0 then
    Result := 1
  else begin
    if (Ord(c^) and $FC) = $FC then //4000000+
      Result := 6
    else if (Ord(c^) and $F8)=$F8 then//200000-3FFFFFF
      Result := 5
    else if (Ord(c^) and $F0)=$F0 then//10000-1FFFFF
      Result := 4
    else if (Ord(c^) and $E0)=$E0 then//800-FFFF
      Result := 3
    else if (Ord(c^) and $C0)=$C0 then//80-7FF
      Result := 2
    else
      Result := 1;
  end
end;

function CharSizeW(c: PWideChar): Integer;
begin
  if (c[0]>=#$DB00) and (c[0]<=#$DBFF) and (c[1] >= #$DC00) and (c[1] <= #$DFFF) then
    Result := 2
  else
    Result := 1;
end;

{$IFDEF USE_STRENCODEFUNC}
function AnsiEncode(p:PWideChar; l:Integer): AnsiString;
var
  ps: PWideChar;
  {$IFDEF MSWINDOWS}
  len: Integer;
  {$ENDIF}
begin
  if l<=0 then begin
    ps:=p;
    while ps^<>#0 do Inc(ps);
    l:=ps-p;
  end;
  if l>0 then  begin
    {$IFDEF MSWINDOWS}
    len := WideCharToMultiByte(CP_ACP,0,p,l,nil,0,nil,nil);
    SetLength(Result, len);
    WideCharToMultiByte(CP_ACP,0,p,l,PAnsiChar(Result), len, nil, nil);
    {$ELSE}
    Result.Length:=l shl 1;
    Result.FValue[0]:=0;
    Move(p^,PAnsiChar(Result)^,l shl 1);
    Result:=TEncoding.Convert(TEncoding.Unicode,TEncoding.ANSI,Result.FValue,1,l shl 1);
    {$ENDIF}
  end else
    Result := '';
end;
{$ENDIF}

function LeftStr(const AText: AnsiString; const ACount: Integer): AnsiString; 
begin
  Result := Copy(AText, 1, ACount);
end;

function LeftStr(const AText: WideString; const ACount: Integer): WideString;
begin
  Result := Copy(AText, 1, ACount);
end;

function RightStr(const AText: AnsiString; const ACount: Integer): AnsiString;
begin
  Result := Copy(AText, Length(AText) + 1 - ACount, ACount);
end;

function RightStr(const AText: WideString; const ACount: Integer): WideString;
begin
  Result := Copy(AText, Length(AText) + 1 - ACount, ACount);
end;

function MidStr(const AText: AnsiString; const AStart, ACount: Integer): AnsiString;
begin
  Result := Copy(AText, AStart, ACount);
end;

function MidStr(const AText: WideString; const AStart, ACount: Integer): WideString;
begin
  Result := Copy(AText, AStart, ACount);
end;

function PCharToString(const P: PChar; Len: Integer): string;
begin
  if Len > 0 then   
    SetString(Result, P, Len)
  else
    Result := '';
end;

{$IFDEF USE_STRENCODEFUNC}
function AnsiEncode(const p: StringW):AnsiString;
begin
  Result := AnsiEncode(PWideChar(p), Length(p));
end;
{$ENDIF}

{$IFDEF USE_STRENCODEFUNC} {$IFNDEF MSWINDOWS}
function AnsiDecode(const S: AnsiString): StringW;
begin
  if S.IsUtf8 then
    Result := Utf8Decode(S)
  else
    Result := TEncoding.ANSI.GetString(S.FValue, 1, S.Length);
end;
{$ENDIF} {$ENDIF}

{$IFDEF USE_STRENCODEFUNC}
function AnsiDecode(p: PAnsiChar; l:Integer): StringW;
var
  ps: PAnsiChar;
{$IFNDEF MSWINDOWS}
  ABytes:TBytes;
{$ENDIF}
begin
  if l<=0 then begin
    ps := p;
    while ps^<>{$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do Inc(ps);
    l:=IntPtr(ps)-IntPtr(p);
  end;
  if l>0 then begin
    {$IFDEF MSWINDOWS}
    System.SetLength(Result, MultiByteToWideChar(CP_ACP,0,PAnsiChar(p),l,nil,0));
    MultiByteToWideChar(CP_ACP, 0, PAnsiChar(p),l,PWideChar(Result),Length(Result));
    {$ELSE}
    System.SetLength(ABytes, l);
    Move(p^, PByte(@ABytes[0])^, l);
    Result := TEncoding.ANSI.GetString(ABytes);
    {$ENDIF}
  end else
    System.SetLength(Result,0);
end;
{$ENDIF}

{$IFDEF USE_STRENCODEFUNC}
function Utf8Encode(const p: StringW): AnsiString;
begin
  Result := Utf8Encode(PWideChar(p), Length(p));
end;

function Utf8Encode(p:PWideChar; l:Integer): AnsiString;
var
  ps:PWideChar;
  pd,pds:PAnsiChar;
  c:Cardinal;
begin
  if p=nil then
    Result := ''
  else begin
    if l<=0 then begin
      ps:=p;
      while ps^<>#0 do
        Inc(ps);
      l:=ps-p;
      end;
    {$IFDEF NEXTGEN}
    Result.Length:=l*6;
    {$ELSE}
    SetLength(Result, l*6);//UTF8每个字符最多6字节长,一次性分配足够的空间
    {$ENDIF}
    if l>0 then begin
      Result[1] := {$IFDEF NEXTGEN}1{$ELSE}#1{$ENDIF};
      ps:=p;
      pd:=PAnsiChar(Result);
      pds:=pd;
      while l>0 do begin
        c:=Cardinal(ps^);
        Inc(ps);
        if (c>=$D800) and (c<=$DFFF) then begin//Unicode 扩展区字符
          c:=(c-$D800);
          if (ps^>=#$DC00) and (ps^<=#$DFFF) then begin
            c:=$10000+((c shl 10) + (Cardinal(ps^)-$DC00));
            Inc(ps);
            Dec(l);
          end else
            raise Exception.Create(Format(SBadUnicodeChar,[IntPtr(ps^)]));
        end;
        Dec(l);
        if c=$0 then begin
          if JavaFormatUtf8 then begin//按照Java格式编码，将#$0字符编码为#$C080
            pd^:={$IFDEF NEXTGEN}$C0{$ELSE}#$C0{$ENDIF};
            Inc(pd);
            pd^:={$IFDEF NEXTGEN}$80{$ELSE}#$80{$ENDIF};
            Inc(pd);
          end else begin
            pd^:=AnsiChar(c);
            Inc(pd);
          end;
        end else if c<=$7F then begin //1B
          pd^:=AnsiChar(c);
          Inc(pd);
        end else if c<=$7FF then begin//$80-$7FF,2B
          pd^:=AnsiChar($C0 or (c shr 6));
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));
          Inc(pd);
        end else if c<=$FFFF then begin //$8000 - $FFFF,3B
          pd^:=AnsiChar($E0 or (c shr 12));
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 6) and $3F));
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));
          Inc(pd);
        end else if c<=$1FFFFF then begin //$01 0000-$1F FFFF,4B
          pd^:=AnsiChar($F0 or (c shr 18));//1111 0xxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 12) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 6) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));//10 xxxxxx
          Inc(pd);
        end else if c<=$3FFFFFF then begin//$20 0000 - $3FF FFFF,5B
          pd^:=AnsiChar($F8 or (c shr 24));//1111 10xx
          Inc(pd);
          pd^:=AnsiChar($F0 or ((c shr 18) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 12) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 6) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));//10 xxxxxx
          Inc(pd);
        end else if c<=$7FFFFFFF then begin //$0400 0000-$7FFF FFFF,6B
          pd^:=AnsiChar($FC or (c shr 30));//1111 11xx
          Inc(pd);
          pd^:=AnsiChar($F8 or ((c shr 24) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($F0 or ((c shr 18) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 12) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or ((c shr 6) and $3F));//10 xxxxxx
          Inc(pd);
          pd^:=AnsiChar($80 or (c and $3F));//10 xxxxxx
          Inc(pd);
        end;
      end;
      pd^:={$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF};
      {$IFDEF NEXTGEN}
      Result.Length := IntPtr(pd)-IntPtr(pds);
      {$ELSE}
      SetLength(Result, IntPtr(pd)-IntPtr(pds));
      {$ENDIF}
    end;
  end;
end;
{$ENDIF}

{$IFDEF USE_STRENCODEFUNC} {$IFNDEF MSWINDOWS}
function Utf8Decode(const S: AnsiString): StringW; overload;
begin
  if S.IsUtf8 then
    Result := Utf8Decode(PAnsiChar(S), S.Length)
  else
    Result := AnsiDecode(S);
end;
{$ENDIF} {$ENDIF}

{$IFDEF USE_STRENCODEFUNC}
function Utf8Decode(p: PAnsiChar; l: Integer): StringW;
var
  ps,pe: PByte;
  pd,pds: PWord;
  c: Cardinal;
begin
  if l<=0 then begin
    ps:=PByte(p);
    while ps^<>0 do Inc(ps);
    l := Integer(ps) - Integer(p);
  end;
  ps := PByte(p);
  pe := ps;
  Inc(pe, l);
  System.SetLength(Result, l);
  pd := PWord(PWideChar(Result));
  pds := pd;
  while Integer(ps)<Integer(pe) do begin
    if (ps^ and $80)<>0 then begin
      if (ps^ and $FC)=$FC then begin //4000000+
        c:=(ps^ and $03) shl 30;
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 24);
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 18);
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 12);
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 6);
        Inc(ps);
        c:=c or (ps^ and $3F);
        Inc(ps);
        c:=c-$10000;
        pd^:=$D800+((c shr 10) and $3FF);
        Inc(pd);
        pd^:=$DC00+(c and $3FF);
        Inc(pd);
      end else if (ps^ and $F8)=$F8 then begin //200000-3FFFFFF
        c:=(ps^ and $07) shl 24;
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 18);
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 12);
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 6);
        Inc(ps);
        c:=c or (ps^ and $3F);
        Inc(ps);
        c:=c-$10000;
        pd^:=$D800+((c shr 10) and $3FF);
        Inc(pd);
        pd^:=$DC00+(c and $3FF);
        Inc(pd);
      end else if (ps^ and $F0)=$F0 then begin //10000-1FFFFF
        c:=(ps^ and $0F) shr 18;
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 12);
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 6);
        Inc(ps);
        c:=c or (ps^ and $3F);
        Inc(ps);
        c:=c-$10000;
        pd^:=$D800+((c shr 10) and $3FF);
        Inc(pd);
        pd^:=$DC00+(c and $3FF);
        Inc(pd);
      end else if (ps^ and $E0)=$E0 then begin //800-FFFF
        c:=(ps^ and $1F) shl 12;
        Inc(ps);
        c:=c or ((ps^ and $3F) shl 6);
        Inc(ps);
        c:=c or (ps^ and $3F);
        Inc(ps);
        pd^:=c;
        Inc(pd);
      end else if (ps^ and $C0)=$C0 then begin //80-7FF
        pd^:=(ps^ and $3F) shl 6;
        Inc(ps);
        pd^:=pd^ or (ps^ and $3F);
        Inc(pd);
        Inc(ps);
      end else
        raise Exception.Create(Format('无效的UTF8字符:%d',[Integer(ps^)]));
    end else begin
      pd^ := ps^;
      Inc(ps);
      Inc(pd);
    end;
  end;
  System.SetLength(Result, (Integer(pd)-Integer(pds)) shr 1);
end;
{$ENDIF}

{$IFDEF NEXTGEN}
{ AnsiString }
procedure AnsiString.From(p: PAnsiChar; AOffset, ALen: Integer);
begin
  SetLength(ALen);
  Inc(P, AOffset);
  Move(P^, PAnsiChar(@FValue[1])^,ALen);
end;

function AnsiString.GetChars(AIndex: Integer): AnsiChar;
begin
  if (AIndex<0) or (AIndex >= Length) then
    raise Exception.CreateFmt(SOutOfIndex, [AIndex, 0, Length - 1]);
  Result:=FValue[AIndex+1];
end;

class operator AnsiString.Implicit(const S: WideString): AnsiString;
begin
  Result := AnsiEncode(S);
end;

class operator AnsiString.Implicit(const S: AnsiString): PAnsiChar;
begin
  Result:=PansiChar(@S.FValue[1]);
end;

function AnsiString.GetIsUtf8: Boolean;
begin
  if System.Length(FValue)>0 then
    Result:=(FValue[0]=1)
  else
    Result:=False;
end;

function AnsiString.GetLength: Integer;
begin
  //FValue[0]存贮编码类型，0-ANSI,1-UTF8，末尾存贮字符串的\0结束符
  Result := System.Length(FValue);
  if Result>=2 then
    Dec(Result,2)
  else
    Result:=0;
end;

class operator AnsiString.Implicit(const S: AnsiString): TBytes;
var
  L:Integer;
begin
  L:=System.Length(S.FValue)-1;
  System.SetLength(Result,L);
  if L>0 then
    Move(S.FValue[1],Result[0],L);
end;

procedure AnsiString.SetChars(AIndex: Integer; const Value: AnsiChar);
begin
  if (AIndex<0) or (AIndex>=Length) then
    raise Exception.CreateFmt(SOutOfIndex,[AIndex,0,Length-1]);
  FValue[AIndex+1]:=Value;
end;

procedure AnsiString.SetLength(const Value: Integer);
begin
  if Value<0 then begin
    if System.Length(FValue)>0 then
      System.SetLength(FValue,1)
    else begin
      System.SetLength(FValue,1);
      FValue[0]:=0;//ANSI
    end;
  end else begin
    System.SetLength(FValue,Value+2);
    FValue[Value+1]:=0;
  end;
end;

class operator AnsiString.Implicit(const ABytes: TBytes): AnsiString;
var
  L:Integer;
begin
  L:=System.Length(ABytes);
  Result.Length:=L;
  if L>0 then
    Move(ABytes[0],Result.FValue[1],L);
end;

class operator AnsiString.Implicit(const S: AnsiString): WideString;
begin
  Result := AnsiDecode(S);
end;
{$ENDIF}

const
  Convert: array[0..127] of Integer =
    (
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
      0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1,
     -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
     );
     
function PCharToFloat(const S: PChar; Len: Integer): Double;
var
  I, K, V, M: Integer;
begin
  Result := 0;
  K := 0;
  M := 10;
  for i := 0 to len - 1 do begin
    V := Convert[Ord(s[i])];
    if (s[i] = '.') and (k = 0) then Inc(k);
    if (V < 0) then begin
      if (k > 1) then begin
        Result := 0;
        Exit;
      end;
    end else begin
      if k = 0 then
        Result := (result * 10) + V
      else begin
        Result := Result + V / M;
        M := M * 10;
      end;
    end;
  end;
end;

function IsHexChar(c: Char): Boolean; inline;
begin
  Result:=((c>='0') and (c<='9')) or
    ((c>='a') and (c<='f')) or
    ((c>='A') and (c<='F'));
end;

function HexValue(c: Char): Integer;
begin
  if (c>='0') and (c<='9') then
    Result := Ord(c) - Ord('0')
  else if (c>='a') and (c<='f') then
    Result := 10+ Ord(c)-Ord('a')
  else
    Result := 10+ Ord(c)-Ord('A');
end;

function HexChar(v: Byte): Char;
begin
  if v<10 then
    Result := Char(v + Ord('0'))
  else
    Result := Char(v-10 + Ord('A'));
end;

function BinToHex(p:Pointer;l:Integer): String;
const
  B2HConvert: array[0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  pd: PChar;
  pb: PByte;
begin
  SetLength(Result, l shl 1);
  pd := PChar(Result);
  pb := p;
  while l>0 do begin
    pd^ := B2HConvert[pb^ shr 4];
    Inc(pd);
    pd^ := B2HConvert[pb^ and $0F];
    Inc(pd);
    Inc(pb);
    Dec(l);
  end;
end;

function BinToHex(const ABytes:TBytes): String;
begin
  Result:=BinToHex(@ABytes[0], Length(ABytes));
end;

procedure HexToBin(p: pointer; l: Integer; var AResult: TBytes);
var
  ps: PChar;
  pd: PByte;
begin
  SetLength(AResult, l shr 1);
  ps := p;
  pd := @AResult[0];
  while ps - p < l do begin
    if IsHexChar(ps[0]) and IsHexChar(ps[1]) then begin
      pd^:=(HexValue(ps[0]) shl 4) + HexValue(ps[1]);
      Inc(pd);
      Inc(ps, 2);
    end else begin
      SetLength(AResult, 0);
      Exit;
    end;
  end;
end;

function HexToBin(const S: String): TBytes;
begin
  HexToBin(PChar(S), System.Length(S), Result);
end;

procedure HexToBin(const S: String; var AResult: TBytes);
begin
  HexToBin(PChar(S), System.Length(S), AResult);
end;

function StrPosA(start, current: PAnsiChar; var ACol, ARow:Integer): PAnsiChar;
begin
  ACol := 1;
  ARow := 1;
  Result := start;
  while IntPtr(start) < IntPtr(current) do begin
    if start^={$IFDEF NEXTGEN}10{$ELSE}#10{$ENDIF} then begin
      Inc(ARow);
      ACol := 1;
      Inc(start);
      Result := start;
    end else begin
      Inc(start, CharSizeA(start));
      Inc(ACol);
    end;
  end;
end;

function StrPosU(start, current: PAnsiChar; var ACol, ARow:Integer): PAnsiChar;
begin
  ACol := 1;
  ARow := 1;
  Result := start;
  while IntPtr(start)<IntPtr(current) do begin
    if start^={$IFDEF NEXTGEN}10{$ELSE}#10{$ENDIF} then begin
      Inc(ARow);
      ACol := 1;
      Inc(start);
      Result := start;
    end else begin
      Inc(start, CharSizeU(start));
      Inc(ACol);
    end;
  end;
end;

function StrPosW(start, current: PWideChar; var ACol, ARow:Integer): PWideChar;
begin
  ACol := 1;
  ARow := 1;
  Result := start;
  while start < current do begin
    if start^=#10 then begin
      Inc(ARow);
      ACol := 1;
      Inc(start);
      Result := start;
    end else begin
      Inc(start, CharSizeW(start));
      Inc(ACol);
    end;
  end;
end;

function DecodeLineA(var p: PAnsiChar; ASkipEmpty: Boolean): StringA;
var
  ps: PAnsiChar;
  i: Integer;
begin
  ps := p;
  while p^<>{$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
    if (PWORD(p)^ = $0D0A) or (PWORD(p)^ = $0A0D) then
      i := 2
    else if (p^ = {$IFDEF NEXTGEN}13{$ELSE}#13{$ENDIF}) then
      i := 1
    else
      i := 0;
    if i > 0 then begin
      if ps = p then begin
        if ASkipEmpty then begin
          Inc(p, i);
          ps := p;
        end else begin
          Result := '';
          Exit;
        end;
      end else begin
        {$IFDEF NEXTGEN}
        Result.Length := IntPtr(p)-IntPtr(ps);
        {$ELSE}
        SetLength(Result, p-ps);
        {$ENDIF}
        Move(ps^, PAnsiChar(Result)^, IntPtr(p)-IntPtr(ps));
        Inc(p, i);
        Exit;
      end;
    end else
      Inc(p);
  end;
  if ps = p then
    Result := ''
  else begin
    {$IFDEF NEXTGEN}
    Result.Length := IntPtr(p)-IntPtr(ps);
    {$ELSE}
    SetLength(Result, p-ps);
    {$ENDIF}
    Move(ps^, PAnsiChar(Result)^, IntPtr(p)-IntPtr(ps));
  end;
end;

function DecodeLineW(var p: PWideChar; ASkipEmpty: Boolean): StringW;
var
  ps: PWideChar;
  i: Integer;
begin
  ps := p;
  while p^<>#0 do begin
    if (PCardinal(p)^ = $000D000A) or (PCardinal(p)^ = $000A000D) then
      i := 2
    else if (p^ = #13) then
      i := 1
    else
      i := 0;
    if i > 0 then begin
      if ps = p then begin
        if ASkipEmpty then begin
          Inc(p, i);
          ps := p;
        end else begin
          Result := '';
          Exit;
        end;
      end else begin
        SetLength(Result, p-ps);
        Move(ps^, PWideChar(Result)^, p-ps);
        Inc(p, i);
        Exit;
      end;
    end else
      Inc(p);
  end;
  if ps = p then
    Result := ''
  else begin
    SetLength(Result, p-ps);
    Move(ps^, PWideChar(Result)^, p-ps);
  end;
end;

function IsSpaceA(const c: PAnsiChar; ASpaceSize: PInteger): Boolean;
begin
  {$IFDEF NEXTGEN}
  if c^ in [9, 10, 13, 32] then begin
  {$ELSE}
  if c^ in [#9, #10, #13, #32] then begin
  {$ENDIF}
    Result := True;
    if ASpaceSize <> nil then
      ASpaceSize^ := 1;
  end else if PWORD(c)^ = $A1A1 then begin
    Result := True;
    if ASpaceSize <> nil then
      ASpaceSize^ := 2;
  end else
    Result:=False;
end;

function IsSpaceW(const c: PWideChar; ASpaceSize: PInteger): Boolean;
begin
  Result := (c^=#9) or (c^=#10) or (c^=#13) or (c^=#32) or (c^=#$3000);
  if Result and (ASpaceSize <> nil) then
    ASpaceSize^ := 1;
end;

//全角空格$3000的UTF-8编码是227,128,128
function IsSpaceU(const c: PAnsiChar; ASpaceSize: PInteger): Boolean;
begin
  {$IFDEF NEXTGEN}
  if c^ in [9, 10, 13, 32] then begin
  {$ELSE}
  if c^ in [#9, #10, #13, #32] then begin
  {$ENDIF}
    Result := True;
    if (ASpaceSize <> nil) then
      ASpaceSize^ := 1;
  end else if (c^={$IFDEF NEXTGEN}227{$ELSE}#227{$ENDIF}) and (PWORD(IntPtr(c)+1)^ = $8080) then begin
    Result := True;
    if (ASpaceSize <> nil) then
      ASpaceSize^ := 3;
  end else
    Result:=False;
end;

function SkipSpaceA(var p: PAnsiChar): Integer;
var
  ps: PAnsiChar;
  L: Integer;
begin
  ps := p;
  while p^<>{$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
    if IsSpaceA(p, @L) then
      Inc(p, L)
    else
      Break;
  end;
  Result:= IntPtr(p) - IntPtr(ps);
end;

function SkipSpaceU(var p: PAnsiChar): Integer;
var
  ps: PAnsiChar;
  L: Integer;
begin
  ps := p;
  while p^<>{$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
    if IsSpaceU(p, @L) then
      Inc(p, L)
    else
      Break;
  end;
  Result:= IntPtr(p) - IntPtr(ps);
end;

function SkipSpaceW(var p: PWideChar): Integer;
var
  ps: PWideChar;
  L:Integer;
begin
  ps := p;
  while p^<>#0 do begin
    if IsSpaceW(p, @L) then
      Inc(p, L)
    else
      Break;
  end;
  Result := p - ps;
end;

function SkipLineA(var p: PAnsiChar): Integer;
var
  ps: PAnsiChar;
begin
  ps := p;
  while p^ <> {$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
    if (PWORD(p)^ = $0D0A) or (PWORD(p)^ = $0A0D) then begin
      Inc(p, 2);
      Break;
    end else if  (p^ = {$IFDEF NEXTGEN}13{$ELSE}#13{$ENDIF}) then begin
      Inc(p);
      Break;
    end else
      Inc(p);
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;

function SkipLineU(var p: PAnsiChar): Integer;
begin
  Result := SkipLineA(p);
end;

function SkipLineW(var p: PWideChar): Integer;
var
  ps: PWideChar;
begin
  ps := p;
  while p^ <> #0 do begin
    if (PCardinal(p)^ = $000D000A) or (PCardinal(p)^ = $000A000D) then begin
      Inc(p, 2);
      Break;
    end else if  (p^ = #13) then begin
      Inc(p);
      Break;
    end else
      Inc(p);
  end;
  Result := p - ps;
end;

{$IFNDEF NEXTGEN}
function SkipUntilA(var p: PAnsiChar; AExpects: PAnsiChar; AQuoter: AnsiChar): Integer;
var
  ps: PAnsiChar;
begin
  ps := p;
  while p^<>{$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
    if (p^ = AQuoter) then begin
      Inc(p);
      while p^<>{$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} do begin
        if p^ = {$IFDEF NEXTGEN}$5C{$ELSE}#$5C{$ENDIF} then begin
          Inc(p);
          if p^<>{$IFDEF NEXTGEN}0{$ELSE}#0{$ENDIF} then
            Inc(p);
        end else if p^ = AQuoter then begin
          Inc(p);
          if p^ = AQuoter then
            Inc(p)
          else
            Break;
        end else
          Inc(p);
      end;
    end else if CharInA(p, AExpects) then
      Break
    else
      Inc(p, CharSizeA(p));
  end;
  Result := IntPtr(p) - IntPtr(ps);
end;
{$ENDIF}
{$IFNDEF NEXTGEN}
function SkipUntilU(var p: PAnsiChar; AExpects: PAnsiChar; AQuoter: AnsiChar): Integer;
var
  ps: PAnsiChar;
begin
  ps := p;
  while p^<>#0 do begin
    if (p^ = AQuoter) then begin
      Inc(p);
      while p^<>#0 do begin
        if p^=#$5C then begin
          Inc(p);
          if p^<>#0 then
            Inc(p);
        end else if p^=AQuoter then begin
          Inc(p);
          if p^=AQuoter then
            Inc(p)
          else
            Break;
        end else
          Inc(p);
      end;
    end else if CharInU(p, AExpects) then
      Break
    else
      Inc(p, CharSizeU(p));
  end;
  Result := p - ps;
end;
{$ENDIF}

function SkipUntilW(var p: PWideChar; AExpects: PWideChar; AQuoter: WideChar): Integer;
var
  ps: PWideChar;
begin
  ps := p;
  while p^<>#0 do begin
    if (p^=AQuoter) then begin
      Inc(p);
      while p^<>#0 do begin
        if p^=#$5C then begin
          Inc(p);
          if p^<>#0 then
            Inc(p);
        end else if p^=AQuoter then begin
          Inc(p);
          if p^=AQuoter then
            Inc(p)
          else
            Break;
        end else
          Inc(p);
      end;
    end else if CharInW(p, AExpects) then
      Break
    else
      Inc(p, CharSizeW(p));
  end;
  Result := p - ps;
end;

function StartWith(s, startby: PChar; AIgnoreCase: Boolean): Boolean;
begin
  while (s^<>#0) and (startby^<>#0) do begin
    if AIgnoreCase then begin
      {$IFDEF UNICODE}
      if CharUpperW(s^) <> CharUpperW(startby^) then
      {$ELSE}
      if CharUpperA(s^) <> CharUpperA(startby^) then
      {$ENDIF}
        Break;
    end else if s^ <> startby^ then
      Break;
    Inc(s);
    Inc(startby);
  end;
  Result := startby^ = #0;
end;

function StartWithIgnoreCase(s, startby: PChar): Boolean;
begin
  while (s^<>#0) and (startby^<>#0) do begin
    {$IFDEF UNICODE}
    if CharUpperW(s^) <> CharUpperW(startby^) then
    {$ELSE}
    if CharUpperA(s^) <> CharUpperA(startby^) then
    {$ENDIF}
      Break;
    Inc(s);
    Inc(startby);
  end;
  Result := startby^ = #0;
end;

function TextIsSame(const A1, A2: string): Boolean; inline;
begin
  {$IFDEF DOTNET}
  Result := System.String.Compare(A1, A2, True) = 0;
  {$ELSE}
  Result := AnsiCompareText(A1, A2) = 0;
  {$ENDIF}
end;

{ TStringCatHelperA }

function TStringCatHelperA.Back(ALen: Integer): TStringCatHelperA;
begin
  Result := Self;
  Dec(FDest, ALen);
  if FDest < PCharA(FValue) then
    FDest := PCharA(FValue);
end;

function TStringCatHelperA.Cat(const V: Int64): TStringCatHelperA;
begin
  Result := Cat(IntToStr(V));
end;

function TStringCatHelperA.Cat(const V: Double): TStringCatHelperA;
begin
  Result := Cat(FloatToStr(V));
end;

function TStringCatHelperA.Cat(const V: Boolean): TStringCatHelperA;
begin
  Result := Cat(BoolToStr(V));
end;

function TStringCatHelperA.Cat(p: PCharA; len: Integer): TStringCatHelperA;
begin
  Result := Self;
  if len < 0 then begin
    while p^ <> #0 do begin
      if FDest-FStart >= FSize then
        NeedSize(FSize + FBlockSize);
      FDest^ := p^;
      Inc(p);
      Inc(FDest);
    end;
  end else begin
    NeedSize(-len);
    Move(p^, FDest^, len);
    Inc(FDest, len);
  end;
end;

function TStringCatHelperA.Cat(const s: StringA): TStringCatHelperA;
begin
  Result := Cat(PCharA(s), Length(s));
end;

function TStringCatHelperA.Cat(c: CharA): TStringCatHelperA;
begin
  if (FDest-FStart)=FSize then
    NeedSize(-1);
  FDest^ := c;
  Inc(FDest);
  Result := Self;
end;

function TStringCatHelperA.Cat(const V: Currency): TStringCatHelperA;
begin
  Result := Cat(CurrToStr(V));
end;

function TStringCatHelperA.Cat(const s: StringW): TStringCatHelperA;
var
  V: StringA;
begin
  V := StringA(S);
  Result := Cat(PCharA(V), Length(V));
end;

constructor TStringCatHelperA.Create;
begin
  inherited Create;
  FBlockSize := 4096;
  NeedSize(FBlockSize);
end;

constructor TStringCatHelperA.Create(ASize: Integer);
begin
  inherited Create;
  FBlockSize := ASize;
  NeedSize(FBlockSize);
end;

destructor TStringCatHelperA.Destroy;
begin
  SetLength(FValue, 0);
  inherited;
end;

function TStringCatHelperA.GetChars(AIndex: Integer): CharA;
begin
  Result := FStart[AIndex];
end;

function TStringCatHelperA.GetMemory: Pointer;
begin
  Result := FStart;
end;

function TStringCatHelperA.GetPosition: Integer;
begin
  Result := FDest - PCharA(FValue);
end;

function TStringCatHelperA.GetValue: StringA;
var
  L: Integer;
begin
  L := FDest - PCharA(FValue);
  SetLength(Result, L);
  Move(FStart^, PCharA(Result)^, L);
end;

procedure TStringCatHelperA.NeedSize(ASize: Integer);
var
  offset:Integer;
begin
  offset := FDest-FStart;
  if ASize < 0 then
    ASize := offset - ASize;
  if ASize > FSize then begin
    FSize := ((ASize + FBlockSize) div FBlockSize) * FBlockSize;
    SetLength(FValue, FSize);
    FStart := PCharA(@FValue[0]);
    FDest := FStart + offset;
  end;
end;

procedure TStringCatHelperA.Reset;
begin
  FDest := FStart;
end;

procedure TStringCatHelperA.SetPosition(const Value: Integer);
begin
  if Value <= 0 then
    FDest := PCharA(FValue)
  else if Value>Length(FValue) then begin
    NeedSize(Value);
    FDest := PCharA(FValue) + Value;
  end else
    FDest := PCharA(FValue) + Value;
end;

function TStringCatHelperA.Space(count: Integer): TStringCatHelperA;
begin
  Result := Self;
  if Count > 0 then begin
    while Count>0 do begin
      Cat(#32);
      Dec(Count);
    end;
  end;
end;

{ TBytesCatHelper }

function TBytesCatHelper.Back(ALen: Integer): TBytesCatHelper;
begin
  Result := Self;
  Dec(FDest, ALen);
  if IntPtr(FDest) < IntPtr(FStart) then
    FDest := FStart;
end;

function TBytesCatHelper.Cat(const V: Cardinal): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(const V: Smallint): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(const V: Integer): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(const V: Currency): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(const V: Int64): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(V: CharA): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(V: Byte): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(p: Pointer; len: Integer): TBytesCatHelper;
begin
  Result := Self;
  NeedSize(-len);
  Move(p^, FDest^, len);
  Inc(FDest, len);
end;

function TBytesCatHelper.Cat(const V: Word): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(const V: Shortint): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(const V: Double): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(const V: Boolean): TBytesCatHelper;
begin
  Result := Cat(@V, SizeOf(V));
end;

function TBytesCatHelper.Cat(const s: StringW): TBytesCatHelper;
begin
  Result := Cat(PCharW(S), System.Length(S) shl 1);
end;

function TBytesCatHelper.Cat(const s: StringA): TBytesCatHelper;
begin
  Result := Cat(PCharA(s), Length(s));
end;

constructor TBytesCatHelper.Create;
begin
  inherited Create;
  FBlockSize := 8192;
  NeedSize(FBlockSize);
end;

constructor TBytesCatHelper.Create(ASize: Integer);
begin
  inherited Create;
  FBlockSize := ASize;
  NeedSize(FBlockSize);
end;

destructor TBytesCatHelper.Destroy;
begin
  SetLength(FValue, 0);
  inherited;
end;

function TBytesCatHelper.GetBytes(AIndex: Integer): Byte;
begin
  Result := FValue[AIndex];
end;

function TBytesCatHelper.GetPosition: Integer;
begin
  Result := IntPtr(FDest) - IntPtr(FStart);
end;

function TBytesCatHelper.GetValue: TBytes;
var
  ALen: Integer;
begin
  ALen := Position;
  SetLength(Result, ALen);
  if ALen > 0 then
    Move(FValue[0], Result[0], ALen);
end;

procedure TBytesCatHelper.NeedSize(ASize: Integer);
var
  Offset: Integer;
begin
  Offset := IntPtr(FDest) - IntPtr(FStart);
  if ASize < 0 then
    ASize := Offset - ASize;
  if ASize > FSize then begin
    FSize := ((ASize + FBlockSize) div FBlockSize) * FBlockSize;
    SetLength(FValue, FSize);
    FStart := @FValue[0];
    FDest := PByte(IntPtr(FStart) + Offset);
  end;
end;

function TBytesCatHelper.Replicate(const ABytes: TBytes;
  ACount: Integer): TBytesCatHelper;
var
  l: Integer;
begin
  Result := Self;
  l := Length(ABytes);
  if l > 0 then begin
    NeedSize(-l * ACount);
    while ACount > 0 do begin
      Move(ABytes[0], FDest^, l);
      Inc(FDest, l);
      Dec(ACount);
    end;
  end;
end;

procedure TBytesCatHelper.Reset;
begin
  FDest := FStart;
end;

procedure TBytesCatHelper.SetCapacity(const Value: Integer);
begin
  if FSize <> Value then
    NeedSize(Value);
end;

procedure TBytesCatHelper.SetPosition(const Value: Integer);
begin
  if Value <= 0 then
    FDest := FStart
  else if Value > Length(FValue) then begin
    NeedSize(Value);
    FDest := Pointer(IntPtr(FStart) + Value);
  end else
    FDest := Pointer(IntPtr(FStart) + Value);
end;

function TBytesCatHelper.Skip(Count: Integer): TBytesCatHelper;
begin
  Result := Self;
  if Count > 0 then begin
    while Count>0 do begin
      Cat(Byte(0));
      Dec(Count);
    end;
  end;
end;

{ TStringCatHelperW }

function TStringCatHelperW.Back(ALen: Integer): TStringCatHelperW;
begin
  Result := Self;
  Dec(FDest, ALen);
  if FDest < FStart then
    FDest := FStart;
end;

function TStringCatHelperW.Cat(const V: Double): TStringCatHelperW;
begin
  Result := Cat(FloatToStr(V));
end;

function TStringCatHelperW.Cat(const V: Int64): TStringCatHelperW;
begin
  Result := Cat(IntToStr(V));
end;

function TStringCatHelperW.Cat(const V: Currency): TStringCatHelperW;
begin
  Result := Cat(CurrToStr(V));
end;

function TStringCatHelperW.Cat(const V: Boolean): TStringCatHelperW;
begin
   Result := Cat(BoolToStr(V, True));
end;

function TStringCatHelperW.Cat(p: PCharW; len: Integer): TStringCatHelperW;
begin
  Result := Self;
  if len < 0 then begin
    while p^ <> #0 do begin
      if Position >= FSize then
        NeedSize(FSize + FBlockSize);
      FDest^ := p^;
      Inc(p);
      Inc(FDest);
    end;
  end else begin
    NeedSize(-len);
    Move(p^, FDest^, len shl 1);
    Inc(FDest, len);
  end;
end;

function TStringCatHelperW.Cat(c: CharW): TStringCatHelperW;
begin
  if Position >= FSize then
    NeedSize(-1);
  FDest^ := c;
  Inc(FDest);
  Result := Self;
end;

function TStringCatHelperW.Cat(const s: StringW): TStringCatHelperW;
begin
  Result := Cat(PCharW(S), Length(S));
end;

function TStringCatHelperW.Cat(const V: Variant): TStringCatHelperW;
begin
  Result := Cat(VarToStr(V));
end;

constructor TStringCatHelperW.Create;
begin
  inherited Create;
  FBlockSize := 8192;
  NeedSize(FBlockSize);
end;

constructor TStringCatHelperW.Create(ASize: Integer);
begin
  inherited Create;
  if ASize < 8192 then
    ASize := 8192
  else if (ASize and $3FF) <> 0 then
    ASize := ((ASize shr 10) + 1) shr 1;
  FBlockSize := ASize;
  NeedSize(FBlockSize);
end;

destructor TStringCatHelperW.Destroy;
begin
  SetLength(FValue, 0);
  inherited;
end;

function TStringCatHelperW.GetChars(AIndex: Integer): CharW;
begin
  Result := FStart[AIndex];
end;

function TStringCatHelperW.GetIsEmpty: Boolean;
begin
  Result := FDest <> FStart;
end;

function TStringCatHelperW.GetPosition: Integer;
begin
  Result := FDest - FStart;
end;

function TStringCatHelperW.GetValue: StringW;
var
  l: Integer;
begin
  l := Position;
  SetLength(Result, l);
  Move(FStart^, PCharW(Result)^, l shl 1);
end;

procedure TStringCatHelperW.IncSize(ADelta: Integer);
begin
  NeedSize(-ADelta);
end;

procedure TStringCatHelperW.NeedSize(ASize: Integer);
var
  Offset: Integer;
begin
  Offset := FDest - FStart;
  if ASize < 0 then
    ASize := Offset - ASize;
  if ASize > FSize then begin
    FSize := ((ASize + FBlockSize) div FBlockSize) * FBlockSize;
    SetLength(FValue, FSize);
    FStart := PCharW(@FValue[0]);
    FDest := FStart + Offset;
    FLast := FStart + FSize;
  end;
end;

function TStringCatHelperW.Replicate(const S: StringW;
  count: Integer): TStringCatHelperW;
var
  ps: PCharW;
  l: Integer;
begin
  Result := Self;
  if count > 0 then begin
    ps := PCharW(S);
    l := Length(S);
    while count > 0 do begin
      Cat(ps, l);
      Dec(count);
    end;
  end;
end;

procedure TStringCatHelperW.Reset;
begin
  FDest := FStart;
end;

procedure TStringCatHelperW.SetDest(const Value: PCharW);
begin
  if (Value >= FStart) and (Value < FLast) then
    FDest := Value;
end;

procedure TStringCatHelperW.SetPosition(const Value: Integer);
begin
  if Value <= 0 then
    FDest := FStart
  else if Value > Length(FValue) then begin
    NeedSize(Value);
    FDest := FStart + Value;
  end else
    FDest := FStart + Value;
end;

function TStringCatHelperW.Space(count: Integer): TStringCatHelperW;
begin
  Result := Self;
  if Count > 0 then begin
    while Count>0 do begin
      Cat(#32);
      Dec(Count);
    end;
  end;
end;

procedure TStringCatHelperW.TrimRight;
var
  pd: PCharW;
begin
  pd := FDest;
  Dec(pd);
  while FStart < pd do begin
    if IsSpaceW(pd) then
      Dec(pd)
    else
      Break;
  end;
  Inc(pd);
  FDest := pd;
end;

initialization
 {$IFDEF MSWINDOWS}
  hMsvcrtl := LoadLibrary('msvcrt.dll');
  if hMsvcrtl <> 0 then begin
    VCStrStr := TMSVCStrStr(GetProcAddress(hMsvcrtl, 'strstr'));
    VCStrStrW := TMSVCStrStrW(GetProcAddress(hMsvcrtl, 'wcsstr'));
    VCMemCmp := TMSVCMemCmp(GetProcAddress(hMsvcrtl, 'memcmp'));
  end else begin
    VCStrStr := nil;
    VCStrStrW := nil;
    VCMemCmp := nil;
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  SysACP := GetACP();
  {$ELSE}
  SysACP := TEncoding.ANSI.CodePage
  {$ENDIF}

finalization
  {$IFDEF MSWINDOWS}
  if hMsvcrtl <> 0 then
    FreeLibrary(hMsvcrtl);
  {$ENDIF}

end.
