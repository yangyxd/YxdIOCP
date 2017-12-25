unit XML;

interface

uses
  {$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}AnsiStrings, {$IFEND}
  Windows, SysUtils, Classes, DateUtils, Math;

type
  StringA = AnsiString;
  {$IFDEF UNICODE}
  StringW = UnicodeString;
  {$ELSE}
  StringW = WideString;
  {$ENDIF}
  PXMLNode = ^TXMLNode;
  TXMLStringListItem = record
    Text: string;
  	Value: string;
	  Data: PXMLNode;
  end;
  PXMLStringListItem = ^TXMLStringListItem;
  PPXMLStringListItem = ^PXMLStringListItem;

  PXMLStringList = ^TXMLStringList;
  TXMLStringList = packed record
  private
		FCount: Integer;
    FItems: PPXMLStringListItem;
		FCapacity: integer;
    procedure Grow;
    function GetCount: Integer;
    function GetData(index: Integer): Pointer;
    function GetTexts(index: Integer): string;
    function GetItem(index: Integer): PPXMLStringListItem;
    procedure SetCapacity(Value: Integer);
    procedure SetData(index: Integer; const Value: Pointer);
    procedure SetTexts(index: Integer; const Value: string);
    function GetItems(index: Integer): PXMLStringListItem;
  public
    class function NewXMLStringList: TXMLStringList; static;
    procedure Free();

    function Add(const s: string; AData: Pointer = nil): Integer; overload;
    function Add(const AName, AValue: string; AData: Pointer = nil): integer; overload;
    function Get(AIndex: Integer; out AName, AValue: string): Boolean;

    procedure Assign(ASource: TXMLStringList);
    procedure Delete(AIdx: Integer);
    procedure Clear;

    property Count: Integer read GetCount;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Items[index: Integer]: PXMLStringListItem read GetItems;
    property ItemTexts[index: Integer]: string read GetTexts write SetTexts; default;
    property Data[index: Integer]: Pointer read GetData write SetData;
  end;

  TXMLNode = packed record
  private
    function GetChild(Index: Integer): PXMLNode;
    function GetChildCount: Integer;
    function GetAttr(Index: Integer): PXMLStringListItem;
    function GetAttrCount(Index: Integer): Integer;
    function GetAttrByName(const Name: string): PXMLStringListItem;
    function GetNodeItem(const Name: string): PXMLNode;
    function GetAsBoolean: Boolean;
    function GetAsByte: Byte;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsFloat: Extended;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsString: string;
    function GetAsWord: Word;
    procedure SetAsBoolean(const Value: Boolean);
    procedure SetAsByte(const Value: Byte);
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsFloat(const Value: Extended);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsInteger(const Value: Integer);
    procedure SetAsString(const Value: string);
    procedure SetAsWord(const Value: Word);
  public
    Index: Integer;
	  Name: string;
	  Text: string;
    CData: string;
	  Parent: PXMLNode;
	  Attrs: TXMLStringList;
	  Children: TXMLStringList;
    procedure Clear();
    
    procedure AddOrUpdate(const NodeName, Value: string); overload;
    procedure AddOrUpdate(const NodeName: string; const Value: Integer); overload;
    procedure AddOrUpdate(const NodeName: string; const Value: Int64); overload;
    procedure AddOrUpdate(const NodeName: string; const Value: Double); overload;
    procedure AddOrUpdate(const NodeName: string; const Value: Boolean); overload;

    function ExistNode(const NodeName: string): Boolean;
    function AddChild(const NodeName: string): PXMLNode;
    function IndexOf(Value: PXMLNode): Integer;
    function NextSibling: PXMLNode;
    function NodeByName(const NodeName: string): PXMLNode;
    function NodeByPath(const Path: string): PXMLNode;
    
    function GetNodeCData(const NodeName: string): string;
    function GetNodeText(const NodeName: string): string;

    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsByte: Byte read GetAsByte write SetAsByte;
    property AsWord: Word read GetAsWord write SetAsWord;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Extended read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsString: string read GetAsString write SetAsString;
    
    property Child[Index: Integer]: PXMLNode read GetChild;
    property ChildCount: Integer read GetChildCount;
    property ParentNode: PXMLNode read Parent write Parent;
    property Attr[Index: Integer]: PXMLStringListItem read GetAttr;
    property AttrByName[const Name: string]: PXMLStringListItem read GetAttrByName;
    property AttrCount[Index: Integer]: Integer read GetAttrCount;
    property Nodes[const Name: string]: PXMLNode read GetNodeItem; default;
    property NodeText[const NodeName: string]: string read GetNodeText;
    property NodeCData[const NodeName: string]: string read GetNodeCData;
  end;

type
  TXMLDocumentBase = class(TObject)
  private
    FRoot: TXMLNode;
    FErrorCode: Integer;
    FIndentFactor: Integer;
    FSource: PAnsiChar;
    FCharset: string;
    function GetCharset(src: PAnsiChar): AnsiString;
    function GetNode: PXMLNode;
  protected
    procedure InternalClear(AParent: PXMLNode);
    procedure Decode(p: PAnsiChar; L: Integer; var DecodStr: string);overload;
    procedure Decode(Stream: TStream; StartPs, L: Integer; var DecodeStr: string);overload;
    function Encode(s: string): AnsiString; overload;
    function CharByCode(var AVal: array of AnsiChar; var p: PAnsiChar): Integer;
    function CharIn(c: AnsiChar; s: PAnsiChar): Boolean;
    procedure SkipSpace(const FromStream: TStream=nil);
    procedure SkipDelimiters(const FromStream: TStream=nil);
    procedure ParseAttr(var AName, AValue: string);
    procedure ParseAttrFromStream(var AName, AValue: string; Stream: TStream);
    function EndLength(ANode: TXMLNode): Integer;
    procedure SkipComment(const FromStream: TStream=nil);
    procedure SkipProcessor(const FromStream: TStream=nil);
    procedure SkipCData(const FromStream: TStream=nil);
    function ParseCData(var AValue:string):Boolean;
    procedure InternalParse(ANode: PXMLNode);
    procedure InternalEncode(ANode: PXMLNode; var ToXmlstr: string);overload;
    procedure InternalEncode(ANode: PXMLNode; ToStream: TStream);overload;
    procedure InternalEncode(ANode: PXMLNode; ToStream: TStream;
      indentFactor, indent: integer);overload;

    function CharsetEncode(const charset: string; var Value: AnsiString): Boolean;
    function CharsetDecode(const charset: string; var Value: string): Boolean; overload;
    function CharsetDecode(const charset: string; Value: PAnsiChar; len: Integer; out Data: string): Boolean; overload;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    function LoadFromStream(Stream: TStream): Boolean; overload;
    function LoadFromStream(Stream: TStream; const charset: string): Boolean; overload;
    function LoadFromFile(const FileName: string): Boolean; overload;
    function LoadFromFile(const FileName: string; const charset: string): Boolean; overload;
    function LoadFromFileByMap(const FileName: string): Boolean;
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);

    function Parse(s: AnsiString): Boolean; overload;
    function Parse(s: PAnsiChar; var Len: Integer): Boolean; overload;
    function Parse(s: PAnsiChar; var Len: Integer; const charset: string): Boolean; overload;

    function ToString: string; overload; {$IFDEF UNICODE} override; {$ENDIF}
    function ToString(indentFactor, indent: integer): string; {$IFDEF UNICODE}reintroduce; {$ENDIF} overload;
    function ToString(ANode: PXMLNode): string; {$IFDEF UNICODE}reintroduce; {$ENDIF} overload;
    function ToString(ANode: PXMLNode; indentFactor, indent: integer): string; {$IFDEF UNICODE}reintroduce; {$ENDIF} overload;

    function Add(AParent: PXMLNode; const AName: string): PXMLNode;
    function AddChild(const AName: string): PXMLNode;
    property Root: TXMLNode read FRoot;
    property Node: PXMLNode read GetNode;
    property ErrorCode: Integer read FErrorCode;
    property IndentFactor: Integer read FIndentFactor write FIndentFactor;
    property Encoding: string read FCharset write FCharset;
  end;

type
  TXMLDocument = class(TXMLDocumentBase)
  public
    function NodeByPath(const Path: string): PXMLNode;
  end; 

function PCharToStr(p: PAnsiChar; len: Integer): StringA; overload; inline;
function PCharToStr(p: PWideChar; len: Integer): StringW; overload; inline;
function UTF8Decode(S:PAnsiChar; L:Integer):WideString; overload;
function UTF8Decode(S:AnsiString):WideString; overload;
function UTF8Encode(S:WideString):AnsiString; overload;
function UTF8Encode(s: PWideChar; len: Integer):AnsiString; overload;
function UTF8Encode(s: PAnsiChar; len: Integer):AnsiString; overload;

implementation

resourcestring
  SBadConvert = '%s 不是一个有效的 %s 类型的值。';

const
  E_BAD_FORMAT = 1;
  SGB2312 = 'GB2312';
  SUTF8 = 'UTF-8';

var
  dwBlockBytes: DWORD;

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
function StrLComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
  Result := AnsiStrings.StrLComp(Str1, Str2, MaxLen);
end;
{$IFEND}

function SpaceStr(ALen: Integer): string; inline;
{$IFDEF UNICODE}
var
  I: Integer;
{$ENDIF}
begin
  if ALen > 0 then begin
    SetLength(Result, ALen);
    {$IFDEF UNICODE}
    for I := 1 to ALen do
      Result[I] := ' ';
    {$ELSE}
    FillChar(Result[1], ALen, ' ');
    {$ENDIF}
  end else Result := '';
end;

procedure WriteCharToStream(s: TStream; const c: AnsiChar); inline; 
begin
  s.WriteBuffer(c, 1);
end;

procedure WriteStringToStream(s: TStream; const text: AnsiString); inline; overload;
begin
  s.WriteBuffer(Pointer(text)^, Length(text));
end;

procedure WriteStringToStream(s: TStream; const text: StringW); inline; overload;
var
  V: StringA;
begin
  V := StringA(text);
  s.WriteBuffer(Pointer(V)^, Length(V));
end;

procedure WriteStringToStream(s: TStream; const text: AnsiString; len: Integer); inline; overload;
begin
  s.WriteBuffer(Pointer(text)^, len);
end;

function ReadCharFromStream(s: TStream): AnsiChar; inline;
begin
  s.ReadBuffer(Result, 1);
end;

function ReadStringFromStream(s: TStream; len: Integer): AnsiString; inline;
begin
  SetLength(Result, len);
  s.ReadBuffer(Result[1], len);
end;

function PCharToStr(p: PAnsiChar; len: Integer): StringA; inline; overload;
begin
  if (len > 0) and (p <> nil) then begin
    SetLength(Result, len);
    CopyMemory(@Result[1], p, len)
  end else
    Result := '';
end;

function PCharToStr(p: PWideChar; len: Integer): StringW; overload;
begin
  if (len > 0) and (p <> nil) then begin
    SetLength(Result, len);
    CopyMemory(@Result[1], p, len shl 1)
  end else
    Result := '';
end;

function UTF8Decode(S:PAnsiChar; L:Integer):WideString; overload;
var
  ALen:Integer;
begin
  if L <= 0 then begin
    L := 0;
    while S[L] <> #0 do
      Inc(L);
  end;
  if L > 0 then begin
    ALen := MultiByteToWideChar(CP_UTF8,0,S,L,nil,0);
    SetLength(Result, ALen);
    MultiByteToWideChar(CP_UTF8,0,S,L,PWideChar(Result),ALen);
  end else Result := '';
end;

function UTF8Decode(S:AnsiString): WideString; overload;
begin
  Result := Utf8Decode(PAnsiChar(S),Length(S));
end;

function UTF8Encode(S:WideString):AnsiString; overload;
var
  ALen,L:Integer;
begin
  SetLength(Result,0);
  L:=Length(S);
  if L > 0 then begin
    ALen:=WideCharToMultiByte(CP_UTF8,0,PWideChar(S),L,nil,0,nil,nil);
    SetLength(Result,ALen);
    WideCharToMultiByte(CP_UTF8,0,PWideChar(S),L,PAnsiChar(Result),ALen,nil,nil);
  end
end;

function UTF8Encode(s: PAnsiChar; len: Integer):AnsiString; overload;
var
  ALen:Integer;
  wStr: WideString;
begin
  SetLength(Result, 0);
  if Len > 0 then begin
    wStr := WideString(PCharToStr(s, len));
    ALen := WideCharToMultiByte(CP_UTF8,0,PWideChar(wStr),Len,nil,0,nil,nil);
    SetLength(Result, ALen);
    WideCharToMultiByte(CP_UTF8,0,PWideChar(wStr),Len,PAnsiChar(Result),ALen,nil,nil);
  end
end;

function UTF8Encode(s: PWideChar; len: Integer):AnsiString; overload;
var
  ALen:Integer;
begin
  SetLength(Result, 0);
  if Len > 0 then begin
    ALen := WideCharToMultiByte(CP_UTF8,0,s,Len,nil,0,nil,nil);
    SetLength(Result, ALen);
    WideCharToMultiByte(CP_UTF8,0,s,Len,PAnsiChar(Result),ALen,nil,nil);
  end
end;

function BoolToStr(const v: Boolean): string; inline;
begin
  if v then Result := 'true' else Result := 'false';
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

function IsSpace(const c: PChar; ASpaceSize: PInteger): Boolean;
begin
  {$IFDEF UNICODE}
  Result := (c^=#9) or (c^=#10) or (c^=#13) or (c^=#32) or (c^=#$3000);
  if Result and (ASpaceSize <> nil) then
    ASpaceSize^ := 1;
  {$ELSE}
  if c^ in [#9, #10, #13, #32] then begin
    Result := True;
    if ASpaceSize <> nil then
      ASpaceSize^ := 1;
  end else if PWORD(c)^ = $A1A1 then begin
    Result := True;
    if ASpaceSize <> nil then
      ASpaceSize^ := 2;
  end else
    Result:=False;
  {$ENDIF}
end;

function SkipSpace(var p: PChar): Integer;
var
  ps: PChar;
  L:Integer;
begin
  ps := p;
  while p^<>#0 do begin
    if IsSpace(p, @L) then
      Inc(p, L)
    else
      Break;
  end;
  Result := p - ps;
end;

function ParseDateTime(s: PChar; var AResult:TDateTime):Boolean;
var
  Y,M,D,H,N,Sec,MS: Word;
  AQuoter: Char;
  ADate: TDateTime;

  function ParseNum(var n:Word):Boolean;
  var
    neg: Boolean;
    ps: PChar;
  begin
    n := 0; ps := s;
    if s^ = '-' then begin
      neg := true;
      Inc(s);
    end else
      neg:=false;
    while s^<>#0 do begin
      if (s^>='0') and (s^<='9') then begin
        n:=n*10+Ord(s^)-48;
        Inc(s);
      end else
        Break;
    end;
    if neg then
      n := -n;
    Result := ps <> s;
  end;

begin
  if (s^='"') or (s^='''') then begin
    AQuoter := s^;
    Inc(s);
  end else
    AQuoter:=#0;
  Result := ParseNum(Y);
  if not Result then
    Exit;
  if s^='-' then begin
    Inc(s);
    Result:=ParseNum(M);
    if (not Result) or (s^<>'-') then
      Exit;
    Inc(s);
    Result:=ParseNum(D);
    if (not Result) or ((s^<>'T') and (s^<>' ') and (s^<>#0)) then
      Exit;
    if s^<>#0 then Inc(s);
    Result := TryEncodeDate(Y,M,D,ADate);
    if not Result then
      Exit;
    SkipSpace(s);
    if s^<>#0 then begin
      if not ParseNum(H) then begin //没跟时间值
        AResult:=ADate;
        Exit;
      end;
      if s^<>':' then begin
        if H in [0..23] then
          AResult := ADate + EncodeTime(H,0,0,0)
        else
          Result:=False;
        Exit;
      end;
      Inc(s);
    end else begin
      AResult:=ADate;
      Exit;
    end;
  end else if s^=':' then begin
    ADate:=0;
    H:=Y;
    Inc(s);
  end else begin
    Result:=False;
    Exit;
  end;
  if H>23 then begin
    Result:=False;
    Exit;
  end;
  if not ParseNum(N) then begin
    if AQuoter<>#0 then begin
      if s^=AQuoter then
        AResult:=ADate+EncodeTime(H,0,0,0)
      else
        Result:=False;
    end else
      AResult:=ADate+EncodeTime(H,0,0,0);
    Exit;
  end else if N>59 then begin
    Result:=False;
    Exit;
  end;
  Sec:=0;
  MS:=0;
  if s^=':' then begin
    Inc(s);
    if not ParseNum(Sec) then begin
      if AQuoter<>#0 then begin
        if s^=AQuoter then
          AResult:=ADate+EncodeTime(H,N,0,0)
        else
          Result:=False;
      end else
        AResult:=ADate+EncodeTime(H,N,0,0);
      Exit;
    end else if Sec>59 then begin
      Result:=False;
      Exit;
    end;
    if s^='.' then begin
      Inc(s);
      if not ParseNum(MS) then begin
        if AQuoter<>#0 then begin
          if AQuoter=s^ then
            AResult:=ADate+EncodeTime(H,N,Sec,0)
          else
            Result:=False;
        end else
          AResult:=ADate+EncodeTime(H,N,Sec,0);
        Exit;
      end else if MS>=1000 then begin//超过1000是以微秒为单位计时的，转换为毫秒
        while MS>=1000 do
          MS:=MS div 10;
      end;
      if AQuoter<>#0 then begin
        if AQuoter=s^ then
          AResult:=ADate+EncodeTime(H,N,Sec,MS)
        else
          Result:=False;
        Exit;
      end else
        AResult:=ADate+EncodeTime(H,N,Sec,MS);
    end else begin
      if AQuoter<>#0 then begin
        if AQuoter=s^ then
          AResult:=ADate+EncodeTime(H,N,Sec,0)
        else
          Result:=False;
      end else
        AResult:=ADate+EncodeTime(H,N,Sec,0)
    end;
  end else begin
    if AQuoter<>#0 then begin
      if AQuoter=s^ then
        AResult:=ADate+EncodeTime(H,N,0,0)
      else
        Result:=False;
    end else
      AResult:=ADate+EncodeTime(H,N,0,0);
  end;
end;

{ TXMLStringList }

function TXMLStringList.Add(const AName, AValue: string;
  AData: Pointer): integer;
var
  AItem: PXMLStringListItem;
  p: PPXMLStringListItem;
begin
  if FCount = FCapacity then
    Grow();
  New(AItem);
  AItem^.Text := AName;
  AItem^.Value := AValue;
  AItem^.Data := AData;
  p := FItems;
  Inc(p, FCount);
  p^ := AItem;
  Result := FCount;
  Inc(FCount);
end;

function TXMLStringList.Add(const s: string; AData: Pointer): Integer;
begin
  Result := Add(S, '', AData);
end;

procedure TXMLStringList.Assign(ASource: TXMLStringList);
var
  AItem: PXMLStringListItem;
  p: PPXMLStringListItem;
  I: Integer;
begin
  Clear();
  Capacity := ASource.Count;
  for I := 0 to ASource.Count - 1 do begin
    p := ASource.FItems;
    Inc(p, I);
    AItem := p^;
    Add(AItem.Text, AItem.Value, AItem.Data);
  end;
end;

procedure TXMLStringList.Clear;
var
  i: Integer;
  p: PPXMLStringListItem;
begin
  p := FItems;
  for i := 0 to FCount - 1 do begin
  	Dispose(p^);
    Inc(p);
  end;
  FCount := 0
end;

procedure TXMLStringList.Delete(AIdx: Integer);
var
  p, AStart: PPXMLStringListItem;
begin
  p := GetItem(AIdx);
  if (p <> nil) then begin
    Dispose(p^);
    AStart := p;
    Inc(p);
    Move(p^, AStart^, (FCount-AIdx-1) * sizeof(PXMLStringListItem));
  end;
end;

procedure TXMLStringList.Free;
begin
  Clear();
  if FCapacity <> 0 then
    FreeMem(FItems, FCapacity * Sizeof(PXMLStringListItem));
end;

function TXMLStringList.Get(AIndex: Integer; out AName,
  AValue: string): Boolean;
var
  p: PPXMLStringListItem;
begin
  p := GetItem(AIndex);
  Result := p <> nil;
  if Result then begin
    AName := p^.Text;
	  AValue := p^.Value;
  end;
end;

function TXMLStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TXMLStringList.GetData(index: Integer): Pointer;
var
  p: PPXMLStringListItem;
begin
  p := GetItem(index);
  if (p <> nil) then Result := p^.Data else Result := nil;
end;

function TXMLStringList.GetItem(index: Integer): PPXMLStringListItem;
begin
  if (index >= 0) and (index < FCount) then begin
    Result := FItems;
    Inc(Result, Index);
  end else Result := nil;
end;

function TXMLStringList.GetItems(index: Integer): PXMLStringListItem;
var
  p: PPXMLStringListItem;
begin
  p := GetItem(index);
  if (p <> nil) then Result := p^ else Result := nil;
end;

function TXMLStringList.GetTexts(index: Integer): string;
var
  p: PPXMLStringListItem;
begin
  p := GetItem(index);
  if (p <> nil) then Result := p^.Text else Result := '';
end;

procedure TXMLStringList.Grow;
var
  ANewSize: Integer;
begin
  ANewSize := FCapacity shl 1;
  if ANewSize = 0 then
  	ANewSize := 8;
  FItems := ReallocMemory(FItems, ANewSize*sizeof(PXMLStringListItem));
  FCapacity := ANewSize;
end;

class function TXMLStringList.NewXMLStringList: TXMLStringList;
begin
  Result.FItems := nil;
  Result.FCount := 0;
  Result.FCapacity := 0;
end;

procedure TXMLStringList.SetCapacity(Value: Integer);
begin
  FCapacity := Value;
  if Value < FCount then
	  Value := FCount;
  FItems := ReallocMemory(FItems, Value*sizeof(PXMLStringListItem));
  Assert(FItems <> nil);
end;

procedure TXMLStringList.SetData(index: Integer; const Value: Pointer);
var
  p: PPXMLStringListItem;
begin
  p := GetItem(index);
  if (p <> nil) then
  	p^.Data := Value;
end;

procedure TXMLStringList.SetTexts(index: Integer; const Value: string);
var
  p: PPXMLStringListItem;
begin
  p := GetItem(index);
  if (p <> nil) then
  	p^.Text := Value;
end;

{ TXMLDocumentBase }

function TXMLDocumentBase.Add(AParent: PXMLNode; const AName: string): PXMLNode;
begin
  Result := AParent.AddChild(AName);
end;

function TXMLDocumentBase.AddChild(const AName: string): PXMLNode;
begin
  Result := FRoot.AddChild(AName);
end;

function TXMLDocumentBase.CharByCode(var AVal: array of AnsiChar;
  var p: PAnsiChar): Integer;
var
  Buf: array[0..2] of WideChar;
  v: Integer;
  e: PAnsiChar;
  l: Integer;
begin
  Result := 0;
  FillChar(Buf, SizeOf(Buf), 0);
  Inc(p, 2);
  v := 0;
  e := p;
  FErrorCode := 0;
  while (e^ <> '') and (e^ <> #0) do begin
    if e^ = ';' then
      Break
    else Inc(e);
  end;
  if p^ = 'x' then begin
    Inc(p);
    while p < e do begin
      if(p[0]>='0') and (p[0]<='9') then
        v := (v shl 4)+ord(p[0])-ord('0')
      else if (p[0]>='a') and (p[0]<='f') then
        v := (v shl 4)+10+ord(p[0])-ord('a')
      else if (p[0]>='A') and (p[0]<='F') then
        v := (v shl 4)+10+ord(p[0])-ord('A')
      else begin
        FErrorCode := E_BAD_FORMAT;
        Break;
      end;
      Inc(p);
    end;
  end else begin
    while p < e do begin
      if (p[0]>='0') and (p[0]<='9') then
        v := v*10+ord(p[0])-ord('0')
      else begin
        FErrorCode := E_BAD_FORMAT;
        Break;
      end;
      inc(p);
    end;
  end;
  if FErrorCode = 0 then begin
    l := 0;
    while v <> 0 do begin
      Buf[l] := WideChar(v and $ffff);
		  Inc(l);
		  v := v shr 16;
    end;
    v := WideCharToMultiByte(CP_UTF8, 0, Buf, l, nil, 0, nil, nil);
    WideCharToMultiByte(CP_UTF8, 0, Buf, l, @AVal[0], v, nil, nil);
    Result := v;
  end;
end;

function TXMLDocumentBase.CharIn(c: AnsiChar; s: PAnsiChar): Boolean;
begin
  Result := False;
  while (s^ <> '') and (s^ <> #0) do begin
    if s^ = c then begin
      Result := True;
      Break;
    end;
    Inc(s) 
  end;
end;

function TXMLDocumentBase.CharsetDecode(const charset: string;
  var Value: string): Boolean;
begin
  Result := CharsetDecode(charset, PAnsiChar(AnsiString(Value)), Length(Value), Value);
end;

function TXMLDocumentBase.CharsetDecode(const charset: string; Value: PAnsiChar;
  len: Integer; out Data: string): Boolean;
begin
  Result := True;
  if UpperCase(charset) = SUTF8 then
    Data := UTF8Decode(Value, len)
  else
    Result := False;
end;

function TXMLDocumentBase.CharsetEncode(const charset: string;
  var Value: AnsiString): Boolean;
begin
  Result := True;
  if (UpperCase(FCharset) = SUTF8) then begin
    Value := UTF8Encode(Value);
  end else
    Result := False;
end;

procedure TXMLDocumentBase.Clear;
begin
  InternalClear(@FRoot);
end;

constructor TXMLDocumentBase.Create;
begin
  FCharset := SUTF8;
  FRoot.Name := 'xml';
  FRoot.Text := '';
  FRoot.Attrs := TXMLStringList.NewXMLStringList;
  FRoot.Children := TXMLStringList.NewXMLStringList;
  FRoot.CData:= '';
  FRoot.Parent := nil;
  FErrorCode := 0;
  FIndentFactor := 4;
end;

procedure TXMLDocumentBase.Decode(Stream: TStream; StartPs, L: Integer;
  var DecodeStr: string);
var
  ACodeVal: array[0..5] of AnsiChar;
  ARealLen: Integer;
  pr: PAnsiChar;
  m: AnsiChar;
begin
  ARealLen := 0;
  Stream.Position := StartPs;
  Stream.ReadBuffer(m, 1);
  if (m = '''') or (m='"') then begin
    Inc(StartPs);
    Dec(L, 2)
  end else
    Stream.Position := Stream.Position - 1;
  if L = 0 then begin
    DecodeStr := '';
    Exit;
  end;
  SetLength(DecodeStr, L);
  pr := PAnsiChar(StringA(DecodeStr));
  FErrorCode := 0;
  while (Stream.Position < StartPs + L) and (FErrorCode = 0) do begin
    Stream.ReadBuffer(ACodeVal[0],1);
    if ACodeVal[0] = '&' then begin
      Stream.ReadBuffer(ACodeVal[1],5);
      if StrLComp(@ACodeVal[0],'&lt;',4) = 0 then begin
        pr[ARealLen] := '<';
        inc(ARealLen);
        Stream.Position := Stream.Position - 2;
      end;
      if StrLComp(ACodeVal,'&gt;',4) = 0 then begin
        pr[ARealLen] := '>';
        inc(ARealLen);
        Stream.Position := Stream.Position - 2;
      end else if StrLComp(ACodeVal,'&amp;',5) = 0 then begin
        pr[ARealLen] := '&';
	    	inc(ARealLen);
        Stream.Position := Stream.Position - 1;
      end else if StrLComp(ACodeVal,'&apos;',6) = 0 then begin
        pr[ARealLen] := '''';
	    	inc(ARealLen);
      end else if StrLComp(ACodeVal,'&quot;',6) = 0 then begin
        pr[ARealLen] := '''';
	    	inc(ARealLen);
      end else if ACodeVal[1] ='#' then begin
      
      end else begin
        pr[ARealLen] := ACodeVal[0];
        inc(ARealLen);
        Stream.Position := Stream.Position - 5;
      end;
    end else begin
     pr[ARealLen] := ACodeVal[0];
     inc(ARealLen);
    end;
  end;
  SetLength(DecodeStr, ARealLen);
end;

procedure TXMLDocumentBase.Decode(p: PAnsiChar; L: Integer; var DecodStr: string);
var
  i, o, ARealLen: Integer;
  ps, pr: PAnsiChar;
  ACodeVal: array[0..5] of AnsiChar;
  {$IFDEF UNICODE}
  ABuf: AnsiString;
  {$ENDIF}
begin
  if L = 0 then begin
    DecodStr := '';
    Exit;
  end;
  if (p[0] = '''') or (p[0]= '"') then begin
    Inc(p);
    Dec(l,2);
  end;
  ps := p;
  ARealLen := 0;
  {$IFDEF UNICODE}
  SetLength(ABuf, L);
  pr := PAnsiChar(ABuf);
  {$ELSE}
  SetLength(DecodStr, L);
  pr := PAnsiChar(DecodStr);
  {$ENDIF}
  FErrorCode := 0;
  while(p - ps < L) and (FErrorCode = 0) do begin
    if p[0] = '&' then begin
      if StrLComp(p, '&lt;', 4) = 0 then begin
        pr[ARealLen] := '<';
		    Inc(p,4);
	    	inc(ARealLen);
      end else if StrLComp(p,'&gt;',4) = 0 then begin
        pr[ARealLen] := '>';
		    Inc(p,4);
	    	inc(ARealLen);
      end else if StrLComp(p,'&amp;',5) = 0 then begin
        pr[ARealLen] := '&';
		    Inc(p,5);
	    	inc(ARealLen);
      end else if StrLComp(p,'&apos;',6) = 0 then begin
        pr[ARealLen] := '''';
		    Inc(p,6);
	    	inc(ARealLen);
      end else if StrLComp(p,'&quot;',6) = 0 then begin
        pr[ARealLen] := '''';
		    Inc(p,6);
	    	inc(ARealLen);
      end else if p[1] ='#' then begin
        o := CharByCode(ACodeVal,p);
        for i := 0 to o - 1 do begin
          pr[ARealLen] := ACodeVal[i];
    			inc(ARealLen);
        end;
      end else begin
        pr[ARealLen] := p^;
        inc(ARealLen);
        inc(p);
      end;
    end else begin
      pr[ARealLen] := p^;
      inc(ARealLen);
      inc(p);
    end;
  end;
  {$IFDEF UNICODE}
  if ARealLen <> L then
    SetLength(ABuf, ARealLen);
  DecodStr := string(ABuf);
  {$ELSE}
  if ARealLen <> L then
    SetLength(DecodStr, ARealLen);
  {$ENDIF}
end;

destructor TXMLDocumentBase.Destroy;
begin
  Clear;
  FRoot.Name := '';
  FRoot.Text := '';
  FRoot.Attrs.Free;
  FRoot.Children.Free;
  FRoot.CData := '';
  inherited;
end;

function TXMLDocumentBase.Encode(s: string): AnsiString;
var
  p, pd: PAnsiChar;
  st: AnsiString;
  j: Integer;
begin
  if Length(s) > 0 then begin
    SetLength(Result, Length(s));
    p := PAnsiChar(StringA(s));
    pd := @result[1];
    while p^ <> '' do begin
      case p^ of
        '<': begin st := '&lt;'; j := 4; end;
        '>': begin st := '&gt;'; j := 4; end;
        '&': begin St := '&amp;'; j := 5; end;
        '''': begin st := '&apos;'; j := 6; end;
        '"': begin st := '&quot;'; j := 6; end
      else j := 0;
      end;
      if j > 0 then begin
        Move(pointer(st)^, pd^, j);
        Inc(pd, j);
      end else begin
        pd^ := p^;
        Inc(pd);
      end;
      Inc(p);
    end;
  end else Result := '';
end;

procedure TXMLDocumentBase.SaveToFile(const FileName: string);
var
  F: TMemoryStream;
begin
  F := TMemoryStream.Create;;
  SaveToStream(F);
  F.SaveToFile(FileName);
  F.Free;
end;

procedure TXMLDocumentBase.SaveToStream(Stream: TStream);
var
  st: AnsiString;
begin
  st :='<?xml version="1.0" encoding="' + UpperCase(StringA(FCharset)) + '"?>'#13#10;
  Stream.WriteBuffer(Pointer(st)^,Length(st));
  InternalEncode(@FRoot, Stream);
  if Stream is TMemoryStream then begin
    st := PCharToStr(PAnsiChar(TMemoryStream(Stream).Memory), Stream.Size);
  end else if Stream is TStringStream then begin
    st := StringA(TStringStream(Stream).DataString);
  end else begin
    SetLength(st, Stream.Size);
    Stream.Position := 0;
    Stream.ReadBuffer(st[1], Stream.Size);
  end;
  if CharsetEncode(FCharset, st) then begin
    Stream.Size := Length(st);
    Stream.Position := 0;
    Stream.WriteBuffer(st[1], Stream.Size);
  end;
end;

function TXMLDocumentBase.EndLength(ANode: TXMLNode): Integer;
begin
  if(FSource[1] = '>') then // />
    Result := 2
  else if strLComp(FSource+1, PAnsiChar(StringA(ANode.Name)), Length(ANode.Name)) = 0 then
    Result := Length(ANode.Name)+1
  else Result := 0;
end;

// 获取XML文件头中的Encoding信息
function TXMLDocumentBase.GetCharset(src: PAnsiChar): AnsiString;
const
  SENCODINGL: AnsiString = 'encoding';
  SENCODINGU: AnsiString = 'ENCODING';
var
  i, j: Integer;
  p: PAnsiChar;
begin
  j := 0;
  while (src^ <> '') and (j < 60) do begin
    if src^ = '<' then begin
      Inc(src);
      if src^ = '?' then
        Inc(src)
      else
        Break;
    end else if (src^ = SENCODINGL[1]) or (src^ = SENCODINGU[1]) then begin
      for i := 1 to 7 do
        if not ((src[i] = SENCODINGL[i+1]) or (src[i] = SENCODINGU[i+1])) then Break;
      if i = 8 then begin
        Inc(src, 8);
        if src^ <> '=' then
          while src^ in [' ',#9] do Inc(src);
        if src^ = '=' then begin
          p := nil;
          Inc(src);
          while src^ <> '' do begin
            if src^ in ['"', ''''] then begin
              if p = nil then
                p := src
              else begin
                Inc(p);
                SetLength(Result, src-p);
                CopyMemory(@Result[1], p, src-p);
                Exit;
              end;
            end;
            Inc(src);
          end;
        end;
        Break;
      end else Inc(src);
    end else Inc(src);
    Inc(j);
  end;
  Result := AnsiString(FCharset);
end;

function TXMLDocumentBase.GetNode: PXMLNode;
begin
  Result := @FRoot;
end;

procedure TXMLDocumentBase.InternalClear(AParent: PXMLNode);
var
  ANode: PXMLNode;
  i: Integer;
begin
  for i := 0 to AParent.Children.Count - 1 do begin
    ANode := AParent.Children.Data[i];
    InternalClear(ANode);
    ANode.Name := '';
    ANode.Text := '';
    ANode.CData := '';
    ANode.Attrs.Free;
    ANode.Children.Free;
    Dispose(ANode);
  end;
  AParent.Children.Clear();
  AParent.Attrs.Clear();
end;

procedure TXMLDocumentBase.InternalEncode(ANode: PXMLNode; ToStream: TStream;
  indentFactor, indent: integer);
var
  AName, AValue: string;
  sb: AnsiString;
  i: Integer;
begin
  sb := StringA(SpaceStr(indent));
  WriteStringToStream(ToStream, sb, indent);
  WriteCharToStream(ToStream, '<');
  WriteStringToStream(ToStream, StringA(ANode^.Name));

  //写入属性
  if ANode^.Attrs.Count > 0 then begin
    WriteCharToStream(ToStream, ' ');
    for i := 0 to ANode^.Attrs.Count - 1 do
      if ANode^.Attrs.Get(i, AName, AValue) then begin
        WriteStringToStream(ToStream, StringA(AName));
        WriteStringToStream(ToStream, '="', 2);
        WriteStringToStream(ToStream, Encode(AValue));
        WriteStringToStream(ToStream, '" ', 2);
      end;
  end;

  if (Length(ANode^.CData)=0) and (ANode^.Children.Count=0) then begin
    if Length(ANode^.Text) <> 0 then begin
      WriteCharToStream(ToStream, '>');
      WriteStringToStream(ToStream, ANode^.Text);
      WriteStringToStream(ToStream, '</', 2);
      WriteStringToStream(ToStream, ANode^.Name);
      WriteStringToStream(ToStream, '>'#13#10, 3);
    end else
      WriteStringToStream(ToStream, '/>'#13#10, 4);
  end else begin
    WriteCharToStream(ToStream, '>');
    if Length(ANode^.CData) > 0 then begin
      WriteStringToStream(ToStream, '<![CDATA[', 9);
      WriteStringToStream(ToStream, ANode.CData);
      WriteStringToStream(ToStream, ']]>', 3);
    end;
    if ANode^.Children.Count <> 0 then begin
      if Length(ANode^.Text) <> 0 then begin
        WriteCharToStream(ToStream, '>');
        WriteStringToStream(ToStream, Encode(ANode^.Text));
      end;
      WriteStringToStream(ToStream, #13#10, 2);
      for i := 0 to ANode^.Children.Count - 1 do
        InternalEncode(ANode^.Children.Data[i], ToStream, indentFactor, indent + indentFactor);
    end;
    WriteStringToStream(ToStream, sb, indent);
    WriteStringToStream(ToStream, '</' ,2);
    WriteStringToStream(ToStream, ANode^.Name);
    WriteStringToStream(ToStream, '>'#13#10 ,3);
  end;
end;

procedure TXMLDocumentBase.InternalEncode(ANode: PXMLNode; var ToXmlstr: string);
var
  AName, AValue: string;
  i: Integer;
begin
  ToXmlstr := '<' + ANode^.Name + ' '; //<name ...
  for i := 0 to ANode^.Attrs.Count - 1 do begin
    if ANode^.Attrs.Get(i, AName, AValue) then
      ToXmlstr := ToXmlstr + AName + '=''' + string(Encode(AValue)) + ''' ';
  end;
  if ANode^.Children.Count = 0 then begin
    if Length(ANode^.Text) <> 0 then
      ToXmlstr := ToXmlstr + '>' + string(Encode(ANode^.Text)) + '</' + ANode^.Name + '>'#13#10
    else ToXmlstr := ToXmlstr + '/>'#13#10;
  end else begin
    ToXmlstr := ToXmlstr + '>';
    if Length(ANode^.Text) <> 0 then
      ToXmlstr := ToXmlstr + '>' + string(Encode(ANode^.Text));
    ToXmlstr := ToXmlstr + #13#10;
    for i := 0 to ANode^.Children.Count - 1 do begin
      InternalEncode(ANode^.Children.Data[i], AName);
      ToXmlstr := ToXmlstr + AName;
    end;
    ToXmlstr := ToXmlstr + '</' + ANode^.Name + '>'#13#10;
  end;
end;

procedure TXMLDocumentBase.InternalEncode(ANode: PXMLNode; ToStream: TStream);
begin
  InternalEncode(ANode, ToStream, FIndentFactor, 0);
end;

procedure TXMLDocumentBase.InternalParse(ANode: PXMLNode);
var
  AName, AValue: string;
  AStep, L: Integer;
  AChild: PXMLNode;
  ps: PAnsiChar;
  isNotData: Boolean;
begin
  AStep := 0;
  ps := FSource;
  isNotData := True;
  while (FSource^ <> '') and (FErrorCode = 0) do begin
    case FSource^ of
      '<'://一个标签的开始，可能是注释、预处理、CDATA或者正常的小节
        begin
          isNotData := False;
          if (AStep = 2) and (FSource <> ps) then
            ANode^.Text := ANode^.Text + string(PCharToStr(ps, FSource-ps));
          case FSource[1] of
            '?'://处理指令
              SkipProcessor();
            '!':
              if ParseCData(AValue) then
                ANode.CData := AValue;
            '/':
              begin
                Inc(FSource);
                L := EndLength(ANode^);
                if L <> 0 then  begin
                  Inc(FSource,L);
                  SkipSpace();
                end else
                  FErrorCode := E_BAD_FORMAT;
                Exit;
              end;
            else begin
              if AStep = 0 then begin
                Inc(FSource);
                ParseAttr(AName, AValue);
                if Length(AName) <> 0 then begin
                  ANode^.Name := AName;
                  if ANode^.Parent <> nil then
                    ANode^.Parent^.Children.ItemTexts[ANode^.Index] := AName;
                  AStep := 1;//后续的是小节属性
                end
                else FErrorCode := E_BAD_FORMAT;
              end else begin
                AChild := Add(ANode, '');
                InternalParse(AChild);
                ps := FSource;
              end;
            end;
          end;
        end;
      '/'://结束标记
        begin
          L := EndLength(ANode^);
          if L = 0 then
            Inc(FSource, 1)
          else begin
            Inc(FSource,L);
            SkipSpace();
            Exit;
          end;
        end;
      '>'://小节内容开始，直到遇到下一个<
        begin
          AStep := 2;
          Inc(FSource);
          SkipSpace();
          ps := FSource;
        end;
      else begin
        if AStep = 1 then begin
          while FSource^ <> '' do begin
            if (FSource^ = '/') or (FSource^ = '>') then
              Break
            else begin
              ParseAttr(AName,AValue);
              ANode^.Attrs.Add(AName, AValue);
            end;
          end;
        end else
          Inc(FSource);
      end;
    end;
  end;
  if isNotData then
    FErrorCode := E_BAD_FORMAT;
end;

function TXMLDocumentBase.LoadFromFile(const FileName: string): Boolean;
var
  F: TMemoryStream;
begin
  F := TMemoryStream.Create;
  F.LoadFromFile(FileName);
  Result := LoadFromStream(F);
  F.Free;
end;

function TXMLDocumentBase.LoadFromFile(const FileName,
  charset: string): Boolean;
var
  F: TMemoryStream;
begin
  F := TMemoryStream.Create;
  F.LoadFromFile(FileName);
  Result := LoadFromStream(F, charset);
  F.Free;
end;

function TXMLDocumentBase.LoadFromFileByMap(const FileName: string): Boolean;
var
  FileSizeHigh:LongWord;
  MapSize,L: Integer;
  FSize: int64;
  FFileHandle,FMappingHandle: THandle;
  FMemory: Pointer;
begin
  FFileHandle := FileOpen(FileName, fmOpenRead);
  if FFileHandle=INVALID_HANDLE_VALUE then
    raise exception.Create('打开文件失败');
  FSize := GetFileSize(FFileHandle,@FileSizeHigh);
  FSize := FSize or (FileSizeHigh shl 32);//获得文件大小
  if FSize=INVALID_FILE_SIZE then
    raise exception.Create('获得文件大小失败');
  FMappingHandle :=  CreateFileMapping(FFileHandle,nil,PAGE_READONLY,Int64Rec(FSize).Hi,Int64Rec(FSize).Lo,nil);
  if FMappingHandle=0 then
    raise exception.Create('创建映射失败');
  if FSize < dwBlockBytes then
    MapSize := FSize
  else
    MapSize := dwBlockBytes;
  FMemory := MapViewOfFile(FMappingHandle,FILE_MAP_READ,0, 0, MapSize);
  if FMemory=nil then
    raise exception.Create('映射文件失败');
  L := 0;
  result := Parse(FMemory,L);
  UnmapViewOfFile(FMemory);
  CloseHandle(FMappingHandle);
  CloseHandle(FFileHandle);
end;

function TXMLDocumentBase.LoadFromStream(Stream: TStream;
  const charset: string): Boolean;
var
  L: Integer;
  tmpStream: TMemoryStream;
begin
  if Stream.InheritsFrom(TMemoryStream) then begin
    L := Stream.Size;
    result := Parse(TMemoryStream(Stream).Memory, L, charset)
  end else begin
    tmpStream := TMemoryStream.Create;
    Stream.Position := 0;
    tmpStream.LoadFromStream(Stream);
    L := tmpStream.Size;
    result := Parse(tmpStream.Memory, L, charset);
    tmpStream.Free;
  end;
end;

function TXMLDocumentBase.LoadFromStream(Stream: TStream): Boolean;
var
  p: PAnsiChar;
begin
  if Stream.Size > 15 then begin
    if Stream.InheritsFrom(TMemoryStream) then
      Result := LoadFromStream(Stream, string(GetCharset(TMemoryStream(Stream).Memory)))
    else begin
      Stream.Position := 0;
      p := PAnsiChar(ReadStringFromStream(Stream, 50));
      Result := LoadFromStream(Stream, string(GetCharset(p)));
    end;
  end else
    Result := LoadFromStream(Stream, FCharset);
end;

function TXMLDocumentBase.Parse(s: PAnsiChar; var Len: Integer): Boolean;
begin
  Result := Parse(s, Len, FCharset);
end;

function TXMLDocumentBase.Parse(s: AnsiString): Boolean;
begin
  FErrorCode := 0;
  Clear;
  FSource := PAnsiChar(s);
  InternalParse(@FRoot);
  Result := FErrorCode = 0;
end;

function TXMLDocumentBase.Parse(s: PAnsiChar; var Len: Integer;
  const charset: string): Boolean;
var
  ps: PAnsiChar;
  AText: string;
begin
  FErrorCode := 0;
  Clear;   
  if CharsetDecode(charset, s, Len, AText) then
    FSource := PAnsiChar(StringA(AText))
  else
    FSource := s;
  ps := FSource;
  InternalParse(@FRoot);
  Len := FSource - ps;
  Result := FErrorCode = 0;
end;

procedure TXMLDocumentBase.ParseAttr(var AName, AValue: string);
var
  ps: PAnsiChar;
  AQuoter: AnsiChar;
  AFlags: Integer;
begin
  SkipSpace();
  AName := '';
  AValue := '';
  ps := FSource;
  AFlags := 0;
  while (FSource^ <> '/') do begin
    if (FSource^ ='''') or (FSource^ = '"') then begin
      AQuoter := FSource^;
      inc(FSource);
      while FSource^ <> '' do begin
        if FSource^ = AQuoter then
        begin
          if FSource[1] = AQuoter then
            Inc(FSource)
          else
          begin
            Inc(FSource);
            Break;
          end;
        end
        else inc(FSource);
      end;
    end else if (FSource^ = '=') then begin
      Decode(ps, FSource-ps, AName);
      AFlags := 1;
      Inc(FSource);
      SkipSpace();
      ps := FSource;
    end else if FSource^ in [' ',#9,#13,#10,'>'] then
      Break
    else
      Inc(FSource);
  end;
  if AFlags <> 0 then
    Decode(ps, FSource-ps, AValue)
  else
    Decode(ps, FSource-ps, AName);
  SkipSpace();
end;

procedure TXMLDocumentBase.ParseAttrFromStream(var AName, AValue: string;
  Stream: TStream);
var
  ps,tmp: Int64;
  AQuoter: AnsiChar;
  AFlags: Integer;
  m: array[0..2] of AnsiChar;
begin
  SkipSpace(Stream);
  AName := '';
  AValue := '';
  ps := Stream.Position;
  AFlags := 0;
  Stream.ReadBuffer(m,3);
  Stream.Position := Stream.Position - 3;
  while (m[0] <> '/') do begin
    if (m[0] ='''') or (m[0] = '"') then begin
      AQuoter := m[0];
      Stream.Position := Stream.Position + 1;
      Stream.ReadBuffer(m,2);
      Stream.Position := Stream.Position - 2;
      while Stream.Position <> Stream.Size do begin
        if m[0] = AQuoter then begin
          if m[1] = AQuoter then
            Stream.Position := Stream.Position + 1
          else begin
            Stream.Position := stream.Position + 1;
            Break;
          end;
        end else
          Stream.Position := Stream.Position + 1;
        Stream.ReadBuffer(m,2);
        Stream.Position := Stream.Position - 2;
      end;
    end else if m[0] in [' ',#9,#13,#10,'>'] then
      Break
    else if (m[0] = '=') then begin
      tmp := Stream.Position;
      Decode(Stream,ps,Stream.Position - PS,AName);
      AFlags := 1;
      Stream.Position := tmp + 1;
      SkipSpace(Stream);
      ps := Stream.Position;
    end else
      Stream.Position := Stream.Position + 1;
    Stream.ReadBuffer(m,3);
    Stream.Position := Stream.Position - 3;
  end;
  tmp := Stream.Position;
  if AFlags <> 0 then
    Decode(Stream,ps,Stream.Position - PS,AValue)
  else
    Decode(Stream,ps,Stream.Position - PS,AName);
  Stream.Position := tmp;
  SkipSpace(stream);
end;

function TXMLDocumentBase.ParseCData(var AValue: string): Boolean;
var
  pStart: PAnsiChar;
begin
  //<![CDATA[....]]>
  Result:=False;
  if strLComp(FSource,'<![CDATA[',9)= 0 then begin
    Inc(FSource,9);
    pStart:=FSource;
    while(FSource^ <> '') do begin
      if (FSource[0] = ']') and (FSource[1] = ']') and (FSource[2] = '>') then begin
        if (FSource - pStart > 0) then begin
          SetLength(AValue, FSource - pStart);
          CopyMemory(@AValue[1], pStart, Length(AValue));
        end;
        Inc(FSource, 3);
        SkipSpace();
        Result:=True;
        break;
      end;
      Inc(FSource);
    end;
  end else if (StrLComp(FSource,'<!DOCTYPE ', 10)= 0) or
    (StrLComp(FSource,'<!ELEMENT ', 10)= 0) or
    (StrLComp(FSource,'<!ATTLIST ', 10)= 0) then
  begin
    // DTD
    Inc(FSource, 10);
    APairCount := 1;
    while FSource^ <> #0 do begin
      if FSource^ = '<' then
        Inc(APairCount)
      else if FSource^ = '>' then begin
        Dec(APairCount);
        if APairCount = 0 then begin
          Inc(FSource);
          SkipSpace();
          Break;
        end;
      end else
        Inc(FSource);
    end;
    AValue := '';
    Result := True;
  end else begin
    //可能是注释，跳过注释
    Inc(FSource,1);
    SkipComment;
    AValue := '';
    Result:=True;
  end;
end;

procedure TXMLDocumentBase.SkipCData(const FromStream: TStream);
var
  st: array[0..8] of AnsiChar;
begin
  //<![CDATA[....]]>
  if FromStream = nil then begin
    if strLComp(FSource,'<![CDATA[',9)= 0 then begin
      Inc(FSource,9);
      while(FSource^ <> '') do begin
        if (FSource[0] = ']') and (FSource[1] = ']') and (FSource[2] = '>') then begin
          Inc(FSource,3);
          SkipSpace();
          break;
        end;
        Inc(FSource);
      end;
    end else
      FErrorCode := E_BAD_FORMAT;
  end else begin
    FromStream.ReadBuffer(st,9);
    if StrLComp(st,'<![CDATA[',9)= 0 then begin
      while FromStream.Position <> FromStream.Size do begin
        FromStream.ReadBuffer(st, 3);
        FromStream.Position := FromStream.Position - 3;
        if (st[0] = ']') and (st[1] = ']') and (st[2] = '>') then begin
          FromStream.Position := FromStream.Position + 3;
          SkipSpace(FromStream);
          Break;
        end;
        FromStream.Position := FromStream.Position + 1;
      end;
    end else begin
      FErrorCode := E_BAD_FORMAT;
      FromStream.Position := FromStream.Position - 9;
    end;
  end;
end;

procedure TXMLDocumentBase.SkipComment(const FromStream: TStream);
var
  m: array[0..2] of AnsiChar;
begin
  if FromStream = nil then begin
    if (FSource[1]='-') and (FSource[2]='-') then begin
      Inc(FSource,3);
      while FSource^ <> '' do begin
        if (FSource[0] = '-') and (FSource[1] = '-') and (FSource[2]='>') then begin
          Inc(FSource,3);
          SkipSpace();
          break;
        end else
          Inc(FSource);
      end;
    end else
      FErrorCode := E_BAD_FORMAT;

  end else begin
    FromStream.ReadBuffer(m,3);
    if (m[1] = '-') and (m[2] = '-') then begin
       FromStream.ReadBuffer(m,3);
       FromStream.Position := FromStream.Position - 3;
       while FromStream.Position <> FromStream.Size do begin
         if (m[0] = '-') and (m[1] = '-') and (m[2]='>') then begin
           FromStream.Position := FromStream.Position + 3;
           SkipSpace(FromStream);
           Break;
         end else
          FromStream.Position := FromStream.Position + 1;
         FromStream.ReadBuffer(m,3);
         FromStream.Position := FromStream.Position - 3;
       end;
    end else begin
      FErrorCode := E_BAD_FORMAT;
      FromStream.Position := FromStream.Position - 3;
    end;
  end;
end;

procedure TXMLDocumentBase.SkipDelimiters(const FromStream: TStream);
const
  CANSIS = [' ',#9,#13,#10,'>'];
var
  m: AnsiChar;
begin
  if FromStream = nil then begin
    while FSource^ in CANSIS do
      Inc(FSource);
  end else begin
    if FromStream.Position <> FromStream.Size then begin
      FromStream.ReadBuffer(m,1);
      while (m in CANSIS) and (FromStream.Position < FromStream.Size) do
        FromStream.ReadBuffer(m,1);
      FromStream.Position := FromStream.Position - 1;
    end;
  end;
end;

procedure TXMLDocumentBase.SkipProcessor(const FromStream: TStream);
var
  m: array[0..1] of AnsiChar;
begin
  if FromStream = nil then begin
    if FSource[1]='?' then begin
      Inc(FSource,2);
      while FSource^ <> '' do begin
        if (FSource[0] = '?') and (FSource[1] = '>') then begin
          Inc(FSource,2);
          SkipSpace();
          Break;
        end else
          Inc(FSource);
      end;
    end else
      FErrorCode := E_BAD_FORMAT;
  end else begin
    FromStream.ReadBuffer(m,2);
    if m[1] = '?' then begin
      FromStream.ReadBuffer(m,2);
      FromStream.Position := FromStream.Position - 2;
      while FromStream.Position <> FromStream.Size do begin
        if (m[0] = '?') and (m[1]='>') then begin
          FromStream.Position := FromStream.Position + 2;
          SkipSpace(FromStream);
          Break;
        end else
          FromStream.Position := FromStream.Position + 1;
        FromStream.ReadBuffer(m,2);
        FromStream.Position := FromStream.Position - 2;
      end;
    end else begin
      FromStream.Position := FromStream.Position - 2;
      FErrorCode := E_BAD_FORMAT;
    end;
  end;
end;

procedure TXMLDocumentBase.SkipSpace(const FromStream: TStream);
const
  CANSIS = [' ',#9,#13,#10];
var
  m: AnsiChar;
begin
  if FromStream = nil then begin
    while FSource^ in CANSIS do
      Inc(FSource);
  end else begin
    if FromStream.Position <> FromStream.Size then begin
      FromStream.ReadBuffer(m,1);
      while m in CANSIS do begin
        FromStream.ReadBuffer(m,1);
        if FromStream.Position = FromStream.Size then
          Break;
      end;
      FromStream.Position := FromStream.Position - 1;
    end;
  end;
end;

function TXMLDocumentBase.toString(indentFactor, indent: integer): string;
begin
  Result := toString(@FRoot, indentFactor, indent);
end;

function TXMLDocumentBase.toString(ANode: PXMLNode; indentFactor,
  indent: integer): string;
var
  ret: TMemoryStream;
begin
  ret := TMemoryStream.Create();
  InternalEncode(ANode, ret, indentFactor, indent);
  Result := string(PCharToStr(PAnsiChar(ret.Memory), ret.Size));
  ret.Free;
end;

function TXMLDocumentBase.toString: string;
begin
  Result := toString(@FRoot, FIndentFactor, 0);
end;

function TXMLDocumentBase.toString(ANode: PXMLNode): string;
begin
  Result := toString(ANode, FIndentFactor, 0);
end;

function GetSysAllocBlockSize:LongInt; // 得到系统分配粒度
var
  SystemInfoGet:TSystemInfo;
begin
  GetSystemInfo(SystemInfoGet);
  Result:=SystemInfoGet.dwAllocationGranularity;
end;

{ TXMLNode }

function TXMLNode.AddChild(const NodeName: string): PXMLNode;
begin
  New(Result);
  Result^.Parent := @Self;
  Result^.Text := '';
  Result^.Name := NodeName;
  Result^.Attrs := TXMLStringList.NewXMLStringList;
  Result^.Children := TXMLStringList.NewXMLStringList;
  Result^.CData := '';
  Result^.Index := Children.Add(NodeName, Result);
end;

procedure TXMLNode.AddOrUpdate(const NodeName, Value: string);
var
  ANode: PXMLNode;
begin
  if NodeName = '' then Exit;
  ANode := NodeByName(NodeName);
  if (ANode = nil) then
    ANode := AddChild(NodeName);
  ANode.Text := Value;
end;

procedure TXMLNode.AddOrUpdate(const NodeName: string; const Value: Int64);
begin
  AddOrUpdate(NodeName, IntToStr(Value));
end;

procedure TXMLNode.AddOrUpdate(const NodeName: string; const Value: Integer);
begin
  AddOrUpdate(NodeName, IntToStr(Value));
end;

procedure TXMLNode.AddOrUpdate(const NodeName: string; const Value: Double);
begin
  AddOrUpdate(NodeName, FloatToStr(Value));
end;

procedure TXMLNode.AddOrUpdate(const NodeName: string; const Value: Boolean);
begin
  AddOrUpdate(NodeName, BoolToStr(Value));
end;

procedure TXMLNode.Clear;
var
  tmpNode: PXMLNode;
  i: Integer;
begin
  Attrs.Clear;
  for i := 0 to Children.Count - 1 do begin
    tmpNode := Children.Data[i];
    tmpNode.Clear;
    tmpNode.Name := '';
    tmpNode.Text := '';
    tmpNode.CData := '';
    tmpNode.Attrs.Free;
    tmpNode.Children.Free;
    Dispose(tmpNode);
  end;
  Children.Clear;
end;

function TXMLNode.ExistNode(const NodeName: string): Boolean;
begin
  Result := NodeByName(NodeName) <> nil;
end;

function TXMLNode.NodeByName(const NodeName: string): PXMLNode;
var
  I: Integer;
  p: PPXMLStringListItem;
begin
  for i := 0 to Children.Count - 1 do begin
    p := Children.GetItem(i);
    if (p = nil) or (p^.Data = nil) then Continue;
    if LowerCase(p^.Data^.Name) = LowerCase(NodeName) then begin
      Result := p^.Data;
      Exit;
    end;
  end;
  Result := nil;
end;

function TXMLNode.GetAsBoolean: Boolean;
begin
  Result := (Text = 'true') or (StrToIntDef(Text, 0) <> 0);  
end;

function TXMLNode.GetAsByte: Byte;
begin
  Result := StrToIntDef(Text, 0);
end;

function TXMLNode.GetAsDateTime: TDateTime;
begin  
  if not(ParseDateTime(PChar(Text), Result)) then begin
    Result := StrToFloatDef(Text, 0);
    if Result = 0 then        
      raise Exception.Create(Format(SBadConvert, ['String', 'DateTime']));
  end;
end;

function TXMLNode.GetAsDouble: Double;
begin
  Result := StrToFloatDef(Text, 0);
end;

function TXMLNode.GetAsFloat: Extended;
begin
  Result := StrToFloatDef(Text, 0);
end;

function TXMLNode.GetAsInt64: Int64;
begin
  Result := StrToIntDef(Text, 0);
end;

function TXMLNode.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Text, 0);
end;

function TXMLNode.GetAsString: string;
begin
  Result := Text;
end;

function TXMLNode.GetAsWord: Word;
begin
  Result := StrToIntDef(Text, 0);
end;

function TXMLNode.GetAttr(Index: Integer): PXMLStringListItem;
begin
  Result := Attrs.Items[index];
end;

function TXMLNode.GetAttrByName(const Name: string): PXMLStringListItem;
var
  I: Integer;
  p: PPXMLStringListItem;
begin
  for i := 0 to Attrs.Count - 1 do begin
    p := Attrs.GetItem(i);
    if (p = nil) then Continue;
    if LowerCase(p^.Text) = LowerCase(Name) then begin
      Result := p^;
      Exit;
    end;
  end;
  Result := nil;
end;

function TXMLNode.GetAttrCount(Index: Integer): Integer;
begin
  Result := Attrs.Count;
end;

function TXMLNode.GetChild(Index: Integer): PXMLNode;
var
  p: PXMLStringListItem;
begin
  p := Children.Items[index];
  if p <> nil then Result := P^.Data else Result := nil;  
end;

function TXMLNode.GetChildCount: Integer;
begin
  Result := Children.Count;
end;

function TXMLNode.GetNodeCData(const NodeName: string): string;
var
  N: PXMLNode;
begin
  N := NodeByName(NodeName);
  if N = nil then
    Result := ''
  else
    Result := N.CData;
end;

function TXMLNode.GetNodeItem(const Name: string): PXMLNode;
begin
  Result := NodeByName(Name);
end;

function TXMLNode.GetNodeText(const NodeName: string): string;
var
  N: PXMLNode;
begin
  N := NodeByName(NodeName);
  if N = nil then
    Result := ''
  else
    Result := N.Text;
end;

function TXMLNode.NodeByPath(const Path: string): PXMLNode;
var
  p, p1: PChar;
begin
  p := PChar(Path);
  p1 := p;
  Result := @Self;
  while (p^ <> '') and (p^ <> #0) do begin
    if p1^ = '\' then begin
      Result := Result.NodeByName(PCharToStr(p, p1-p));
      if Result = nil then
        Break;
      p := p1 + 1;
    end;
    Inc(p1);
  end;
  if (Result <> nil) and (p <> p1) then
    Result := Result.NodeByName(PCharToStr(p, p1-p));
end;

procedure TXMLNode.SetAsBoolean(const Value: Boolean);
begin
  Text := BoolToStr(Value);
end;

procedure TXMLNode.SetAsByte(const Value: Byte);
begin
  Text := IntToStr(Value);
end;

procedure TXMLNode.SetAsDateTime(const Value: TDateTime);
const
  DateFormat = 'yyyy-mm-dd';
  TimeFormat = 'hh:nn:ss';
  DateTimeFormat = 'yyyy-mm-dd hh:nn:ss';
var
  ADate: Integer;
begin
  ADate := Trunc(Value);
  if SameValue(ADate, 0) then begin //Date为0，是时间
    if SameValue(Value, 0) then
      Text := FormatDateTime(DateFormat, Value)
    else
      Text := FormatDateTime(TimeFormat, Value);
  end else begin
    if SameValue(Value-ADate, 0) then
      Text := FormatDateTime(DateFormat, Value)
    else
      Text := FormatDateTime(DateTimeFormat, Value);
  end;
end;

procedure TXMLNode.SetAsDouble(const Value: Double);
begin
  Text := FloatToStr(Value);
end;

procedure TXMLNode.SetAsFloat(const Value: Extended);
begin
  Text := FloatToStr(Value);
end;

procedure TXMLNode.SetAsInt64(const Value: Int64);
begin
  Text := IntToStr(Value);
end;

procedure TXMLNode.SetAsInteger(const Value: Integer);
begin
  Text := IntToStr(Value);
end;

procedure TXMLNode.SetAsString(const Value: string);
begin
  Text := Value;
end;

procedure TXMLNode.SetAsWord(const Value: Word);
begin
  Text := IntToStr(Value);
end;

function TXMLNode.IndexOf(Value: PXMLNode): Integer;
var
  I: Integer;
  p: PPXMLStringListItem;
begin
  Result := -1;
  for i := 0 to Children.Count - 1 do begin
    p := Children.GetItem(i);
    if p = nil then Continue;
    if p^.Data = Value then begin
      Result := i;
      Break;
    end;
  end;
end;

function TXMLNode.NextSibling: PXMLNode;
var
  i: Integer;
  p: PPXMLStringListItem;
begin
  Result := nil;
  if (Parent <> nil) then begin
    i := Parent.IndexOf(@Self);
    if (i > -1) and (i < Parent.Children.Count - 1) then begin
      p := Parent.Children.GetItem(i + 1);
      if p <> nil then
        Result := p^.Data;
    end;
  end;
end;

{ TXMLDocument }

function TXMLDocument.NodeByPath(const Path: string): PXMLNode;
begin
  Result := FRoot.NodeByPath(Path);
end;

initialization
  dwBlockBytes := 1000 * GetSysAllocBlockSize; // 块大小

end.
