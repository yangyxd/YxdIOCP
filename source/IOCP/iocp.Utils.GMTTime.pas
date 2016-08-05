{*******************************************************}
{                                                       }
{       GMT Time 处理函数                               }
{                                                       }
{       版权所有 (C) 2013      YangYxd                  }
{                                                       }
{*******************************************************}

unit iocp.Utils.GMTTime;

interface

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
{$DEFINE ANSISTRINGS}
{$IFEND}

uses
  {$IFNDEF UNICODE}Windows, {$ELSE} {$IFDEF MSWINDOWS}Windows, {$ENDIF}{$ENDIF}
  {$IFDEF ANSISTRINGS}AnsiStrings, {$ENDIF}
  {$IFDEF POSIX}Posix.String_, {$ENDIF}
  SysUtils, SysConst, Classes, DateUtils, SyncObjs;

//GMT时间处理
function DateTimeToGMTRFC822(Const DateTime: TDateTime): string;
function GMTRFC822ToDateTime(const pSour: AnsiString): TDateTime;
function GetNowGMTRFC822: AnsiString;

//字符串转数字
function PCharToInt64Def(const S: PAnsichar; Len: Integer; def: int64 = 0): int64;
function PCharToIntDef(const S: PAnsichar; Len: Integer; def: Integer = 0): Integer; overload;

implementation

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

function LocalTimeZoneBias: Integer;
{$IFDEF LINUX}
var
  TV: TTimeval;
  TZ: TTimezone;
begin
  gettimeofday(TV, TZ);
  Result := TZ.tz_minuteswest;
end;
{$ELSE}
var
  TimeZoneInformation: TTimeZoneInformation;
  Bias: Longint;
begin
  case GetTimeZoneInformation(TimeZoneInformation) of
    TIME_ZONE_ID_STANDARD: Bias := TimeZoneInformation.Bias + TimeZoneInformation.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: Bias := TimeZoneInformation.Bias + ((TimeZoneInformation.DaylightBias div 60) * -100);
  else
    Bias := TimeZoneInformation.Bias;
  end;
  Result := Bias;
end;
{$ENDIF}

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

var
  DLocalTimeZoneBias: Double = 0;
  CURCentury: Integer = 0;

function DateTimeToGMT(const DT: TDateTime): TDateTime; inline;
begin
  Result := DT + DLocalTimeZoneBias;
end;

function GMTToDateTime(const DT: TDateTime): TDateTime; inline;
begin
  Result := DT - DLocalTimeZoneBias;
end;

function DateTimeToGMTRFC822(const DateTime: TDateTime): string;
const
  WEEK: array[1..7] of string = ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  STR_ENGLISH_M: array[1..12] of string = ('Jan', 'Feb', 'Mar', 'Apr', 'May',
    'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  wWeek, wYear, wMonth, wDay, wHour, wMin, wSec, wMilliSec: Word;
begin
  DecodeDateTime(DateTimeToGMT(DateTime), wYear, wMonth, wDay, wHour, wMin, wSec, wMilliSec);
  wWeek := DayOfWeek(DateTimeToGMT(DateTime));
  Result := Format('%s, %.2d %s %.4d %.2d:%.2d:%.2d GMT',
    [WEEK[wWeek], wDay, STR_ENGLISH_M[wMonth], wYear, wHour, wMin, wSec]);
end;

// Wed, 25 May 2016 07:39:05 GMT
// Thu, 31-Dec-37 23:55:55 GMT
function GMTRFC822ToDateTime(const pSour: AnsiString): TDateTime;
  function GetMonthDig(const Value: PAnsiChar): Integer;
  const
    STR_ENGLISH_M: array[1..12] of PAnsiChar = ('Jan', 'Feb', 'Mar', 'Apr', 'May',
      'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  begin
    for Result := Low(STR_ENGLISH_M) to High(STR_ENGLISH_M) do begin
      if StrLIComp(Value, STR_ENGLISH_M[Result], 3) = 0 then
        Exit;
    end;
    Result := 0;
  end;
var
  P1, P2, PMax: PAnsiChar;
  wDay, wMonth, wYear, wHour, wMinute, wSec: SmallInt;
begin
  Result := 0;
  if Length(pSour) < 25 then Exit;
  P1 := Pointer(pSour);
  P2 := P1;
  PMax := P1 + Length(pSour);
  while (P1 < PMax) and (P1^ <> ',') do Inc(P1); Inc(P1);
  if (P1^ <> #32) and (P1 - P2 < 4) then Exit;
  Inc(P1); P2 := P1;
  while (P1 < PMax) and (P1^ <> #32) and (P1^ <> '-') do Inc(P1);
  if (P1^ <> #32) and (P1^ <> '-') then Exit;
  wDay := PCharToIntDef(P2, P1 - P2);
  if wDay = 0 then Exit;
  Inc(P1); P2 := P1;

  while (P1 < PMax) and (P1^ <> #32) and (P1^ <> '-') do Inc(P1);
  if (P1^ <> #32) and (P1 - P2 < 3) then Exit;
  wMonth := GetMonthDig(P2);
  Inc(P1); P2 := P1;

  while (P1 < PMax) and (P1^ <> #32) and (P1^ <> '-') do Inc(P1);
  if (P1^ <> #32) and (P1^ <> '-') then Exit;
  wYear := PCharToIntDef(P2, P1 - P2);
  if wYear = 0 then Exit;
  Inc(P1); P2 := P1;

  while (P1 < PMax) and (P1^ <> ':') do Inc(P1);
  if (P1^ <> ':') then Exit;
  wHour := PCharToIntDef(P2, P1 - P2);
  if wHour = 0 then Exit;
  Inc(P1); P2 := P1;

  while (P1 < PMax) and (P1^ <> ':') do Inc(P1);
  if (P1^ <> ':') then Exit;
  wMinute := PCharToIntDef(P2, P1 - P2);
  if wMinute = 0 then Exit;
  Inc(P1); P2 := P1;

  while (P1 < PMax) and (P1^ <> #32) do Inc(P1);
  if (P1^ <> #32) then Exit;
  wSec := PCharToIntDef(P2, P1 - P2);
  if wSec = 0 then Exit;

  if wYear < 100 then Inc(wYear, CURCentury); // 不带世纪，加上当前的世纪
  Result := GMTToDateTime(EnCodeDateTime(wYear, wMonth, wDay, wHour, wMinute, wSec, 0));
end;

var
  LastUpdate: Cardinal = 0;
  LastGMTTime: AnsiString = '';
  FGMTLocker: TCriticalSection;

function GetNowGMTRFC822: AnsiString;
var
  T: Cardinal;
begin
  T := GetTickCount;
  if T - LastUpdate > 100 then begin
    FGMTLocker.Enter;
    LastGMTTime := AnsiString(DateTimeToGMTRFC822(Now));
    LastUpdate := T;
    FGMTLocker.Leave;
  end;
  Result := LastGMTTime;
end;

initialization
  DLocalTimeZoneBias := LocalTimeZoneBias / 1440;
  CURCentury := Trunc(DateUtils.YearOf(Now) / 100) * 100;
  FGMTLocker := TCriticalSection.Create;

finalization
  FreeAndNil(FGMTLocker);

end.
