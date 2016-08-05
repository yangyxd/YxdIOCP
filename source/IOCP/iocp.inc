{$DEFINE DIOCP}

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

// Socket 重用
{.$DEFINE SOCKET_REUSE}
// 使用HashTable
{$DEFINE USE_HASHTABLE}

{$IF (RTLVersion>=26) and (not Defined(NEXTGEN))}
{$DEFINE ANSISTRINGS}
{$IFEND}

{$IF defined(FPC) or defined(VER170) or defined(VER180) or defined(VER190) or defined(VER200) or defined(VER210)}
  {$DEFINE HAVE_INLINE}
{$IFEND}

{$IFDEF CIL}
  // Platform specific conditional. Used for platform specific code.
  {$DEFINE DotNet}
  // Extra include used in D7 for testing. Remove later when all comps are
  // ported. Used to selectively exclude non ported parts. Allowed in places
  // IFDEFs are otherwise not permitted.
  {$DEFINE DotNetExclude}
{$ENDIF}

{$UNDEF STRING_IS_UNICODE}
{$UNDEF STRING_IS_ANSI}
{$UNDEF STRING_UNICODE_MISMATCH}

{$IFDEF CIL}
  {$DEFINE DOTNET}
  {$DEFINE STRING_IS_UNICODE}
  {$DEFINE STRING_IS_IMMUTABLE}
{$ENDIF}
{$IFDEF UNICODE}
  {$IFNDEF DOTNET}
    {$DEFINE STRING_IS_UNICODE}
  {$ENDIF}
{$ENDIF}
{$IFDEF FPC}
  {$DEFINE USE_INLINE}
  {$IFDEF FPC_UNICODESTRINGS}
    {$DEFINE STRING_IS_UNICODE}
  {$ENDIF}
{$ENDIF}

{$IFDEF STRING_IS_UNICODE}
  {$IFNDEF UNICODE}
    {$DEFINE STRING_UNICODE_MISMATCH}
  {$ENDIF}
{$ELSE}
  {$DEFINE STRING_IS_ANSI}
  {$IFDEF UNICODE}
    {$DEFINE STRING_UNICODE_MISMATCH}
  {$ENDIF}
{$ENDIF}  

{$IFDEF AUTOREFCOUNT}
  {$DEFINE USE_OBJECT_ARC}
{$ENDIF}
{$IFDEF DCC_NEXTGEN}
  {$DEFINE NO_ANSI_TYPES} // 没有 Ansi 字符串
{$ENDIF}
