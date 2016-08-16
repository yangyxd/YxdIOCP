{*******************************************************}
{                                                       }
{       YxdInclude Base64加解密模块                     }
{                                                       }
{       版权所有 (C) 2013 YangYxd                       }
{                                                       }
{*******************************************************}

unit Base64;

interface

uses SysUtils, Classes;

type
{$IFDEF UNICODE}
  Base64String = AnsiString;
{$ELSE}
  Base64String = string;
{$ENDIF}

// 按源长度SourceSize返回Base64编码所需缓冲区字节数
function Base64EncodeBufSize(SourceSize: Integer): Integer;
// 获取Sourec的Base64编码，Base64Buf必须有足够长度。返回实际编码字节数
function Base64Encode(const Source; SourceSize: Integer; var Base64Buf): Integer; overload;
// 将Source编码为Base64字符串返回
function Base64Encode(const Source; SourceSize: Integer): Base64String; overload;
// 将Source从StartPos开始的Size长度的内容源编码为Base64，写入流Dest。
// Size=0 表示一直编码到文件尾
procedure Base64Encode(Source, Dest: TStream; StartPos: Int64 = 0; Size: Int64 = 0); overload;
// 把字符串Str编码为Base64字符串返回
{$IFDEF UNICODE}
function StrToBase64(const Str: AnsiString): Base64String; overload;
function StrToBase64(const Str: string): Base64String; overload;
{$ELSE}
function StrToBase64(const Str: string): Base64String;
{$ENDIF}

// 按给定的编码源Source和长度SourceSize计算并返回解码缓冲区所需字节数
function Base64DecodeBufSize(const Base64Source; SourceSize: Integer): Integer;
// 将Base64编码源Base64Source解码，Buf必须有足够长度。返回实际解码字节数
function Base64Decode(const Base64Source; SourceSize: Integer; var Buf): Integer; overload;
// 将Source从StartPos开始的Size长度的Base64编码内容解码，写入流Dest。
// Size=0 表示一直解码到文件尾
procedure Base64Decode(Source, Dest: TStream; StartPos: Int64 = 0; Size: Int64 = 0); overload;
// 将Base64编码源Base64Source解码为字符串返回
function Base64Decode(const Base64Source; SourceSize: Integer): string; overload;
// 把Base64字符串Base64Str解码为字符串返回
function Base64ToStr(const Base64Str: Base64String): string;
// 把字符串转为Unicode再编码成Base64字符串
function StrToUnicodeBase64(const Value: string): string;
function UnicodeBase64ToStr(const Value: string): string;

function StrBase64ToUNICODE(const Value: string): string;

implementation

const
  Base64_Chars: array[0..63] of AnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Base64_Bytes: array[0..79] of Byte =
  (
    62, 0, 0, 0, 63, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 0,
    0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
    10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    0, 0, 0, 0, 0, 0, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35,
    36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51
  );
  
type
  Base64Proc = function(const Source; SourceSize: Integer; var Buf): Integer;


function StrToUnicodeBase64(const Value: string): string;
var
  cSize: Integer;
  tmp: Byte;
  i: Integer;
  ppszW: array of Byte;
  ww: WideString;
begin
  ww := Value;
  cSize := length(ww) * 2;
  SetLength(ppszW, cSize);
  try
    Move(ww[1], ppszW[0], cSize);
    i := 0;
    while i < cSize do begin
      tmp := ppszw[i];
      ppszw[i] := ppszw[i + 1];
      ppszw[i + 1] := tmp;
      inc(i, 2);
    end;
    Result := string(Base64Encode(ppszW[0], High(ppszW)+1));
  finally
    SetLength(ppszW, 0);
  end;
end;
      
function StrBase64ToUNICODE(const Value: string): string;
begin
  Result := UnicodeBase64ToStr(Value);
end;

function UnicodeBase64ToStr(const Value: string): string;
var
  cSize: Integer;
  tmp: Byte;
  i: Integer;
  ppszW: array of Byte;
  ww: WideString;
begin
  ww := Value;
  cSize := length(ww);
  SetLength(ppszW, cSize);
  Base64Decode(Value[1], Length(ww), ppszW[0]);
  try
    i := 0;
    while i < High(ppszW) do begin
      tmp := ppszw[i];
      ppszw[i] := ppszw[i + 1];
      ppszw[i + 1] := tmp;
      inc(i, 2);
    end;
    Result := Trim(WideString(ppszW));
  finally
    SetLength(ppszW, 0);
  end;
end;

procedure Base64Stream(Source, Dest: TStream; Proc: Base64Proc;
  StartPos, Size: Int64; RBufSize, WBufSize: Integer);
var
  RBuf: array of Byte;
  WBuf: array of Byte;
  RSize, WSize: Integer;
begin
  if (StartPos < 0) or (StartPos >= Source.Size) then Exit;
  Source.Position := StartPos;
  if (Size <= 0) or (Size > Source.Size - Source.Position) then
    Size := Source.Size
  else
    Size := Size + Source.Position;
  SetLength(RBuf, RBufSize);
  SetLength(WBuf, WBufSize);
  while Size <> Source.Position do
  begin
    if RBufSize > Size - Source.Position then
       RBufSize := Size - Source.Position;
    RSize := Source.Read(RBuf[0], RBufSize);
    WSize := Proc(RBuf[0],  RSize, WBuf[0]);
    Dest.Write(WBuf[0], WSize);
  end;
end;

function Base64EncodeBufSize(SourceSize: Integer): Integer;
begin
  Result := ((SourceSize + 2) div 3) shl 2;
end;

(****************************************************************************
*                                                                           *
* BASE64 Encode hint:                                                       *
*                                                                           *
* addr:            (high) 4 byte     3 byte     2 byte     1 byte (low)     *
* sourec ASCII(3 bytes):            33333333   22222222   11111111          *
* bswap:                 11111111   22222222   33333333   00000000          *
* b4 = Base64_Chars[(source >> 8) & 63]:      [00333333]->44444444          *
* b3 = Base64_Chars[(source >> 14) & 63]:     [00222233]->33333333          *
* b2 = Base64_Chars[(source >> 20) & 63]:     [00112222]->22222222          *
* b1 = Base64_Chars[source >> 26]:            [00111111]->11111111          *
*                        b4 << 24   b3 << 16   b2 << 8    b1                *
* dest BASE64(4 bytes)   44444444   33333333   22222222   11111111          *
*                                                                           *
****************************************************************************)

function Base64Encode(const Source; SourceSize: Integer; var Base64Buf): Integer;
asm
    push    ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax         // esi = Source
    mov     edi, ecx         // edi = Buf
    mov     eax, edx
    cdq
    mov     ecx, 3
    div     ecx              // edx = SourceSize % 3
    mov     ecx, eax         // ecx = SourceSize / 3
    test    edx, edx         
    jz      @@1
    inc     eax              // eax = (SourceSize + 2) / 3
  @@1:
    push    eax
    push    edx
    lea     ebp, Base64_Chars
    jecxz   @Last
    cld
  @EncodeLoop:               //  while (ecx > 0){
    mov     edx, [esi]       //   edx = 00000000 33333333 22222222 11111111
    bswap   edx              //   edx = 11111111 22222222 33333333 00000000
    push    edx
    push    edx
    push    edx
    pop     ebx              //   ebx = edx
    shr     edx, 20
    shr     ebx, 26          //   ebx = 00111111
    and     edx, 63          //   edx = 00112222
    mov     ah, [ebp + edx]  //   *(word*)edi = (Base64_Chars[edx] << 8) |
    mov     al, [ebp + ebx]  //     Base64_Chars[ebx]
    stosw                    //   edi += 2
    pop     edx              //   edx = 11111111 22222222 33333333 00000000
    pop     ebx              //   ebx = edx
    shr     edx, 8
    shr     ebx, 14
    and     edx, 63          //   edx = 00333333
    and     ebx, 63          //   ebx = 00222233
    mov     ah, [ebp + edx]  //   *(word*)edi = (Base64_Chars[edx] << 8) |
    mov     al, [ebp + ebx]  //     Base64_Chars[ebx]
    stosw                    //   edi += 2
    add     esi, 3           //   esi += 3
    loop    @EncodeLoop      // }
  @Last:
    pop     ecx              // ecx = SourceSize % 3
    jecxz   @end             // if (ecx == 0) return
    mov     eax, 3d3d0000h   // preset 2 bytes '='
    mov     [edi], eax
    test    ecx, 2
    jnz     @@3
    mov     al, [esi]        // if (ecx == 1)
    shl     eax, 4           //   eax = *esi << 4
    jmp     @@4
  @@3:
    mov     ax, [esi]        // else
    xchg    al, ah           //   eax = ((*esi << 8) or *(esi + 1)) << 2
    shl     eax, 2
  @@4:
    add     edi, ecx         // edi += ecx
    inc     ecx              // ecx = last encode bytes
  @LastLoop:
    mov     edx, eax         // for (; cex > 0; ecx --, edi --)
    and     edx, 63          // {
    mov     dl, [ebp + edx]  //   edx = eax & 63
    mov     [edi], dl        //   *edi = Base64_Chars[edx]
    shr     eax, 6           //   eax >>= 6
    dec     edi              // }
    loop    @LastLoop
  @end:
    pop     eax
    shl     eax, 2           // return  encode bytes
    pop     ebx
    pop     edi
    pop     esi
    pop     ebp
end;

function Base64Encode(const Source; SourceSize: Integer): Base64String;
begin
  SetLength(Result, Base64EncodeBufSize(SourceSize));
  Base64Encode(Source, SourceSize, Result[1]);
end;

procedure Base64Encode(Source, Dest: TStream; StartPos: Int64; Size: Int64);
begin
  Base64Stream(Source, Dest, Base64Encode, StartPos, Size, 6144, 8192);
end;

{$IFDEF UNICODE}
function StrToBase64(const Str: AnsiString): Base64String;
begin
  Result := Base64Encode(Str[1], Length(Str));
end;

function StrToBase64(const Str: string): Base64String;
begin
  Result := StrToBase64(AnsiString(Str));
end;
{$ELSE}
function StrToBase64(const Str: string): Base64String;
begin
  Result := Base64Encode(Str[1], Length(Str));
end;
{$ENDIF}

function Base64DecodeBufSize(const Base64Source; SourceSize: Integer): Integer;
asm
    mov     ecx, eax    // ecx = Source + Size
    add     ecx, edx
    mov     eax, edx    // eax = Size / 4 * 3
    shr     edx, 2
    shr     eax, 1
    add     eax, edx
    mov     edx, eax
    jz      @@2
  @@1:
    dec     ecx
    cmp     byte ptr [ecx], 61
    jne     @@2         // if (*--ecx == '=')
    dec     eax         //   eax --
    jmp     @@1
  @@2:                  // return eax: BufSize;  edx: Size / 4 * 3
end;

function Base64Decode(const Base64Source; SourceSize: Integer; var Buf): Integer;
asm
    push    ebp
    push    esi
    push    edi
    push    ebx
    mov     esi, eax       // esi = Source
    mov     edi, ecx       // edi = Buf
    mov     ebx, edx
    call    Base64DecodeBufSize
    push    eax            // eax = Base64DecodeBufSize(Source, SourceSize)
    sub     edx, eax       // edx -= eax  // edx: '=' count
    lea     ebp, Base64_Bytes
    shr     ebx, 2         // ebx = SourceSize / 4
    test    ebx, ebx
    jz      @end
    push    edx
    cld
  @DecodeLoop:             // for (; ebx > 0; ebx --; edi += 3)
    mov     ecx, 4         // {
    xor     eax, eax
  @xchgLoop:               //   for (ecx = 4, eax = 0; ecx > 0; ecx --)
    movzx   edx, [esi]     //   {
    sub     edx, 43        //      edx = *(int*)esi - 43
    shl     eax, 6         //      eax <<= 6
    or      al, [ebp + edx]//      al |= Base64_Bytes[edx]
    inc     esi            //      esi ++
    loop    @xchgLoop      //   }
    bswap   eax            //   bswap(eax)
    dec     ebx            //   if (ebx == 1) break
    jz      @Last
    shr     eax, 8         //   eax >>= 8
    stosw                  //   *edi = ax; edi += 2
    shr     eax, 16        //   eax >>= 16
    stosb                  //   *edi++ = al
    jmp     @DecodeLoop    // }
  @Last:
    pop     ecx            
    xor     ecx, 3         // ecx = last bytes
  @LastLoop:               // for (; ecx > 0; ecx --)
    shr     eax, 8         // {
    stosb                  //   eax >>= 8; *edi ++ = al
    loop    @LastLoop      // }
  @end:
    pop     eax            // return eax
    pop     ebx
    pop     edi
    pop     esi
    pop     ebp
end;

procedure Base64Decode(Source, Dest: TStream; StartPos: Int64; Size: Int64);
begin
  Base64Stream(Source, Dest, Base64Decode, StartPos, Size, 8192, 6144);
end;

{$IFDEF UNICODE}
function Base64Decode(const Base64Source; SourceSize: Integer): string;
var
  s: AnsiString;
begin
  SetLength(s, Base64DecodeBufSize(Base64Source, SourceSize));
  Base64Decode(Base64Source, SourceSize, s[1]);
  Result := string(s);
end;
{$ELSE}
function Base64Decode(const Base64Source; SourceSize: Integer): string;
begin
  SetLength(Result, Base64DecodeBufSize(Base64Source, SourceSize));
  Base64Decode(Base64Source, SourceSize, Result[1]);
end;
{$ENDIF}

function Base64ToStr(const Base64Str: Base64String): string;
begin
  Result := Base64Decode(Base64Str[1], Length(Base64Str));
end;


end.
