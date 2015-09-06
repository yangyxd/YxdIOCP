{$DEFINE DIOCP}

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

{.$DEFINE SOCKET_REUSE}

{$DEFINE USE_HASHTABLE}

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
