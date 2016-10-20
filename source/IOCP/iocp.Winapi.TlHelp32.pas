{*******************************************************}
{                                                       }
{      Windows 部分内核功能和API                        }
{                                                       }
{      版权所有 (C) 2015 YangYxd                        }
{                                                       }
{*******************************************************}

unit iocp.Winapi.TlHelp32;

interface

uses
  Windows, PsAPI, SyncObjs;

const
  MAX_LENGTH  = $20000;

type
  PVOID = Pointer;
  USHORT = Word;

  _CLIENT_ID = packed record
    UniqueProcess: PVOID;
    UniqueThread: PVOID;
  end;
  CLIENT_ID = _CLIENT_ID;
  PCLIENT_ID = ^CLIENT_ID;

  PSystemInformationClass = ^TSystemInformationClass;
  _SYSTEM_INFORMATION_CLASS = (
    SystemBasicInformation,
    SystemProcessorInformation,
    SystemPerformanceInformation,
    SystemTimeOfDayInformation,
    SystemPathInformation,
    SystemProcessInformation,
    SystemCallCountInformation,
    SystemConfigurationInformation,
    SystemProcessorPerformanceInformation,
    SystemGlobalFlag,
    SystemCallTimeInformation,
    SystemModuleInformation,
    SystemLockInformation,
    SystemStackTraceInformation,
    SystemPagedPoolInformation,
    SystemNonPagedPoolInformation,
    SystemHandleInformation,
    SystemObjectInformation,
    SystemPageFileInformation,
    SystemVdmInstemulInformation,
    SystemVdmBopInformation,
    SystemFileCacheInformation,
    SystemPoolTagInformation,
    SystemInterruptInformation,
    SystemDpcBehaviorInformation,
    SystemFullMemoryInformation, 
    SystemLoadGdiDriverInformation,
    SystemUnloadGdiDriverInformation,
    SystemTimeAdjustmentInformation,
    SystemSummaryMemoryInformation,
    SystemNextEventIdInformation,
    SystemEventIdsInformation, 
    SystemCrashDumpInformation,
    SystemExceptionInformation,
    SystemCrashDumpStateInformation,
    SystemKernelDebuggerInformation,
    SystemContextSwitchInformation,
    SystemRegistryQuotaInformation,
    SystemExtendServiceTableInformation,
    SystemPrioritySeperation, 
    SystemPlugPlayBusInformation, 
    SystemDockInformation, 
    SystemPowerInformation, 
    SystemProcessorSpeedInformation, 
    SystemCurrentTimeZoneInformation, 
    SystemLookasideInformation,
    SystemSetTimeSlipEvent,
    SystemCreateSession, // set mode only
    SystemDeleteSession, // set mode only
    SystemInvalidInfoClass1, // invalid info class
    SystemRangeStartInformation, // 0x0004 (fails if size != 4)
    SystemVerifierInformation,
    SystemAddVerifier,
    SystemSessionProcessesInformation, // checked build only
    MaxSystemInfoClass
  );
  TSystemInformationClass = _SYSTEM_INFORMATION_CLASS;

  _PROCESS_MEMORY_COUNTERS = PsAPI.TProcessMemoryCounters;

  PSYSTEM_THREAD_INFORMATION = ^TSystemThreadInfo;
  _SYSTEM_THREAD_INFORMATION = packed record
    KernelTime: LARGE_INTEGER; // 100 nsec units //$000
    UserTime: LARGE_INTEGER; // 100 nsec units //$008
    CreateTime: LARGE_INTEGER; // relative to 01-01-1601 //$010
    WaitTime: DWORD; //$018
    pStartAddress: PVOID; //$01C
    Cid: CLIENT_ID; // process/thread ids //$020
    Priority: DWORD; //$028
    BasePriority: DWORD; //$02C
    ContextSwitches: DWORD; //$030
    ThreadState: DWORD; // 2=running, 5=waiting //$034
    WaitReason: DWORD; //KWAIT_REASON; //$038
    uReserved01: DWORD; //$03C
  end; //$040
  TSystemThreadInfo = _SYSTEM_THREAD_INFORMATION;
  SYSTEM_THREAD_INFORMATION = _SYSTEM_THREAD_INFORMATION;

  TNtAnsiString = packed record
    Length        : Word;
    MaximumLength : Word;
    Buffer        : PChar;
  end;
  PNtAnsiString = ^TNtAnsiString;
  ANSI_STRING = TNtAnsiString;
  PANSI_STRING = ^ANSI_STRING;

  _LSA_UNICODE_STRING = packed record
    Len: USHORT;  // 字符的长度，单位是字节。如果是N个字符，那么Length等于N的2倍
    MaximumLen: USHORT;  //整个字符串缓冲区的最大长度，单位是字节
    Buffer: PWideChar;  //缓冲区的指针  
  end;
  LSA_UNICODE_STRING = _LSA_UNICODE_STRING;
  UNICODE_STRING = LSA_UNICODE_STRING;
  PUNICODE_STRING = ^UNICODE_STRING;
  TNtUnicodeString = UNICODE_STRING;

  PVM_COUNTERS = ^TVmCounters;
  _VM_COUNTERS = packed record
    uPeakVirtualSize: ULONG;
    uVirtualSize: ULONG;
    uPageFaultCount: ULONG;
    uPeakWorkingSetSize: ULONG;
    uWorkingSetSize: ULONG;
    uQuotaPeakPagedPoolUsage: ULONG;
    uQuotaPagedPoolUsage: ULONG;
    uQuotaPeakNonPagedPoolUsage: ULONG;
    uQuotaNonPagedPoolUsage: ULONG;
    uPagefileUsage: ULONG;
    uPeakPagefileUsage: ULONG;
  end;
  TVmCounters = _VM_COUNTERS;
  VM_COUNTERS = _VM_COUNTERS;

  PSYSTEM_PROCESS = ^TSystemProcess;
  _SYSTEM_PROCESS = packed record // common members
  uNext: DWORD; // relative offset //$000
    ThreadCount: DWORD; //$004
    Reserved01: LARGE_INTEGER; //$008
    Reserved02: LARGE_INTEGER; //$010
    Reserved03: LARGE_INTEGER; //$018
    CreateTime: LARGE_INTEGER; // relative to 01-01-1601 //$020
    UserTime: LARGE_INTEGER; // 100 nsec units //$028
    KernelTime: LARGE_INTEGER; // 100 nsec units //$030
    usName: UNICODE_STRING; //$038
    BasePriority: DWORD; //KPRIORITY; //$040
    UniqueProcessId: DWORD; //$044
    InheritedFromUniqueProcessId: DWORD; //$048
    HandleCount: DWORD; //$04C
    SessionId: DWORD; //$050 W2K Only
    Reserved08: DWORD; //$054
    VmCounters: VM_COUNTERS; // see ntddk.h //$058
    CommitCharge: DWORD; // bytes //$084
  end; //$088
  TSystemProcess = _SYSTEM_PROCESS;
  SYSTEM_PROCESS = _SYSTEM_PROCESS;

  SYSTEM_HANDLE_INFORMATION = packed record
    ProcessId: ULONG;
    ObjectTypeNumber: UCHAR;
    Flags: UCHAR;  // 0x01 = PROTECT_FROM_CLOSE, 0x02 = INHERIT
    Handle: USHORT;
    Object_: Pointer;
    GrantedAccess: ACCESS_MASK;
  end;
  TSystemHandleInformation = SYSTEM_HANDLE_INFORMATION;
  PSystemHandleInformation = ^TSystemHandleInformation;

  PIO_COUNTERSEX = ^TIoCountersex;
  _IO_COUNTERSEX = packed record
    ReadOperationCount: LARGE_INTEGER;
    WriteOperationCount: LARGE_INTEGER;
    OtherOperationCount: LARGE_INTEGER;
    ReadTransferCount: LARGE_INTEGER;
    WriteTransferCount: LARGE_INTEGER;
    OtherTransferCount: LARGE_INTEGER;
  end;
  TIoCountersex = _IO_COUNTERSEX;
  IO_COUNTERSEX = _IO_COUNTERSEX;

  PSYSTEM_PROCESS_INFORMATION = ^TSystemProcessInformation;
  _SYSTEM_PROCESS_INFORMATION = packed record
    Process: SYSTEM_PROCESS; // common members //$000
    IoCounters: IO_COUNTERSEX; // see ntddk.h //$088
    aThreads: SYSTEM_THREAD_INFORMATION; // thread array //$0B8
  end; //$0B8
  TSystemProcessInformation = _SYSTEM_PROCESS_INFORMATION;
  SYSTEM_PROCESS_INFORMATION = _SYSTEM_PROCESS_INFORMATION;

  PROCESS_PARAMETERS = packed record
    AllocationSize: ULONG;
    ActualSize: ULONG;
    Flags: ULONG;
    Unknown1: ULONG;
    Unknown2: UNICODE_STRING;
    InpuCardinal: Cardinal;
    OutpuCardinal: Cardinal;
    ErrorHandle: Cardinal;
    CurrentDirectory: UNICODE_STRING;
    CurrentDirectoryHandle: Cardinal;
    SearchPaths: UNICODE_STRING;
    ApplicationName: UNICODE_STRING;
    CommandLine: UNICODE_STRING;
    EnvironmentBlock: Pointer;
    Unknown: array[0..9 - 1] of ULONG;
    Unknown3: UNICODE_STRING;
    Unknown4: UNICODE_STRING;
    Unknown5: UNICODE_STRING;
    Unknown6: UNICODE_STRING;
  end;
  PPROCESS_PARAMETERS = ^PROCESS_PARAMETERS;

  PEB = packed record
    AllocationSize: ULONG;
    Unknown1: ULONG;
    ProcessHinstance: Longword;
    ListDlls: Pointer;
    ProcessParameters: PPROCESS_PARAMETERS;
    Unknown2: ULONG;
    Heap: Cardinal;
  end;
  PPEB = ^PEB;

  _PROCESS_BASIC_INFORMATION = packed record
    Reserved1: Pointer;
    PebBaseAddress: PPEB;
    Reserved2: array[0..1] of Pointer;
    UniqueProcessId: PULONG;
    Reserved3: Pointer;
  end;
  PROCESS_BASIC_INFORMATION = _PROCESS_BASIC_INFORMATION;
  PPROCESS_BASIC_INFORMATION = ^PROCESS_BASIC_INFORMATION;

  PROCESSINFOCLASS = (
    ProcessBasicInformation = 0,
    ProcessWow64Information = 26
  );
  NTSTATUS = DWORD;

function NtQuerySystemInformation(infoClass: DWORD; buffer: Pointer;
  bufSize: DWORD; var returnSize: Dword): DWORD;
function NtQueryInformationProcess(ProcessHandle: Cardinal;
    ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: Pointer;
    ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS;


function GetProcessHandleCount(PID: Cardinal): Cardinal;
function GetProcessMemUse(PID: Cardinal): NativeUInt;
function GetProcessMemoryInfo(Process: THandle;
  ppsmemCounters: PPROCESS_MEMORY_COUNTERS; cb: Cardinal): BOOL;

implementation

type
  TNtQuerySystemInformation = function(infoClass: DWORD; buffer: Pointer; bufSize: DWORD;
    var returnSize: Dword): DWORD; stdcall;
  TNtQueryInformationProcess = function (ProcessHandle: Cardinal;
    ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: Pointer;
    ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS; stdcall;

var
  _NtQuerySystemInformation: TNtQuerySystemInformation;
  _NtQueryInformationProcess: TNtQueryInformationProcess;

function NtQuerySystemInformation(infoClass: DWORD; buffer: Pointer; bufSize: DWORD;
  var returnSize: Dword): DWORD;
begin
  if not Assigned(_NtQuerySystemInformation) then
    Result := 0
  else
    Result := _NtQuerySystemInformation(infoClass, buffer, bufSize, returnSize);
end;

function NtQueryInformationProcess(ProcessHandle: Cardinal;
    ProcessInformationClass: PROCESSINFOCLASS; ProcessInformation: Pointer;
    ProcessInformationLength: ULONG; ReturnLength: PULONG): NTSTATUS;
begin
  if not Assigned(_NtQueryInformationProcess) then
    Result := 0
  else
    Result := _NtQueryInformationProcess(ProcessHandle, ProcessInformationClass,
      ProcessInformation, ProcessInformationLength, ReturnLength);
end;

function GetProcessMemoryInfo(Process: THandle;
  ppsmemCounters: PPROCESS_MEMORY_COUNTERS; cb: Cardinal): BOOL;
begin
  Result := PsAPI.GetProcessMemoryInfo(Process, ppsmemCounters, cb);
end;

type
  TSystemHandleInformationsBuffer = packed record
    NumberOfHandles: LongWord;
    SystemHandleInformations: array[0..MAX_LENGTH-1] of TSystemHandleInformation;
  end;

var
  FLocker: TCriticalSection;
  HandleInfoBuffer: Pointer;
  
function GetProcessHandleCount(PID: Cardinal): Cardinal;
var
  returnSize: Cardinal;
  p, p1: PAnsiChar;
begin
  P := HandleInfoBuffer;
  FLocker.Enter;
  Result := NtQuerySystemInformation(Cardinal(SystemHandleInformation),
    P, SizeOf(TSystemHandleInformationsBuffer), returnSize);
  if Result = 0 then begin
    returnSize := PLongWord(P)^;
    Inc(P, 4);
    p1 := p + returnSize * SizeOf(TSystemHandleInformation);
    while(p < p1) do begin
      if (PSystemHandleInformation(P)^.ProcessId = PID) then
        Inc(Result);
      Inc(P, SizeOf(TSystemHandleInformation));
    end;
  end else
    Result := 0;   
  FLocker.Leave;
end;

function GetProcessMemUse(PID: Cardinal): NativeUInt;
var
  pmc: _PROCESS_MEMORY_COUNTERS; //uses psApi
  ProcHandle: HWND;
  iSize: DWORD;
begin
  Result := 0;
  iSize := SizeOf(_PROCESS_MEMORY_COUNTERS);
  pmc.cb := iSize;
  ProcHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID); //由PID取得进程对象的句柄
  if GetProcessMemoryInfo(ProcHandle, @pmc, iSize) then
    Result := pmc.WorkingSetSize;
  CloseHandle(ProcHandle);  
end;  

initialization
  FLocker := TCriticalSection.Create;
  HandleInfoBuffer := GetMemory(SizeOf(TSystemHandleInformationsBuffer));
  _NtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQuerySystemInformation');
  _NtQueryInformationProcess := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQueryInformationProcess');

finalization
  FLocker.Free;
  FreeMemory(HandleInfoBuffer);

end.

