{*******************************************************}
{                                                       }
{       服务器程序                                      }
{                                                       }
{       版权所有 (C) 2015 YangYxd                       }
{                                                       }
{*******************************************************}
{
  系统服务程序基础库，一个EXE只支持一个服务（与Delphi自带
  的不同）。
  调试时可以启用 $APPTYPE CONSOLE 编译指令 ，以便以命令
  行方式运行服务进行调试。
  请在派生类中重载 ServiceInitialize 来初始化业务，
  重载 ServiceExecute 来执行服务代码。
}

unit YxdServer;

{$J+,H+,X+}
{$WARNINGS OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, WinSvc;

type
  { TEventLogger }
  TEventLogger = class(TObject)
  private
    FName: String;
    FEventLog: Integer;
  public
    constructor Create(Name: String);
    destructor Destroy; override;
    procedure LogMessage(const Message: String; EventType: DWord = 1;
      Category: Word = 0; ID: DWord = 0);
  end;

type
  TServiceType = (stWin32, stDevice, stFileSystem);
  TStartType = (stBoot, stSystem, stAuto, stManual, stDisabled);
  TErrorSeverity = (esIgnore, esNormal, esSevere, esCritical);
  TCurrentStatus = (csStopped, csStartPending, csStopPending, csRunning,
    csContinuePending, csPausePending, csPaused);
  TServiceController = procedure(CtrlCode: DWord); stdcall;

const
  CM_SERVICE_CONTROL_CODE = WM_USER + 1;

type
  TService = class;
  TServiceEvent = procedure(Sender: TService) of object;
  TContinueEvent = procedure(Sender: TService; var Continued: Boolean) of object;
  TPauseEvent = procedure(Sender: TService; var Paused: Boolean) of object;
  TStartEvent = procedure(Sender: TService; var Started: Boolean) of object;
  TStopEvent = procedure(Sender: TService; var Stopped: Boolean) of object;
  
  TServiceThread = class(TThread)
  private
    FService: TService;
  protected
    procedure Execute; override;
  public
    constructor Create(Service: TService);
    procedure ProcessRequests(WaitForMessage: Boolean);
  end;

  TService = class(TObject)
  private
    FDesigning: Boolean;
    FErrorSeverity: TErrorSeverity;
    FServiceType: TServiceType;
    FStartType: TStartType;
    FStatus: TCurrentStatus;
    FInteractive: Boolean;
    FTagID: Integer;
    FName: string;
    FDisplayName: string;
    FAllowStop: Boolean;
    FAllowPause: Boolean;
    FWin32ErrorCode: DWord;
    FErrCode: DWord;
    FStatusHandle: THandle;
    FEventLogger: TEventLogger;
    FServiceThread: TServiceThread;
    FOnContinue: TContinueEvent;
    FOnExecute: TServiceEvent;
    FOnShutdown: TServiceEvent;
    FOnPause: TPauseEvent;
    FOnStop: TStopEvent;
    FOnStart: TStartEvent;
    function GetDisplayName: string;
    procedure SetName(const NewName: string);
    function GetNTServiceType: Integer;
    function GetNTStartType: Integer;
    function GetNTErrorSeverity: Integer;
    function GetNTControlsAccepted: Integer;
    procedure SetInteractive(const Value: Boolean);
    procedure SetStatus(const Value: TCurrentStatus);
    procedure SetOnContinue(const Value: TContinueEvent);
    procedure SetOnPause(const Value: TPauseEvent);
    procedure SetOnStop(const Value: TStopEvent);
  protected
    function DoStop: Boolean; virtual;
    function DoPause: Boolean; virtual;
    function DoContinue: Boolean; virtual;
    procedure DoStart; virtual;
    procedure DoInterrogate; virtual;
    procedure DoShutdown; virtual;

    procedure ReportStatus;
    procedure Main(Argc: DWord; Argv: PLPSTR);
    procedure Controller(CtrlCode: DWord);
  public
    constructor Create();
    destructor Destroy; override;
    procedure LogMessage(Message: String; EventType: DWord = 1;
      Category: Integer = 0; ID: Integer = 0);    
    property ServiceThread: TServiceThread read FServiceThread;
    property AllowStop: Boolean read FAllowStop write FAllowStop default True;
    property AllowPause: Boolean read FAllowPause write FAllowPause default True;
    property Name: string read FName write SetName stored False;
    property DisplayName: String read GetDisplayName write FDisplayName;
    property Status: TCurrentStatus read FStatus write SetStatus;
    property ErrCode: DWord read FErrCode write FErrCode;
    property Win32ErrCode: DWord read FWin32ErrorCode write FWin32ErrorCode;
    property Interactive: Boolean read FInteractive write SetInteractive default False;
    property ErrorSeverity: TErrorSeverity read FErrorSeverity write FErrorSeverity default esNormal;
    property ServiceType: TServiceType read FServiceType write FServiceType default stWin32;
    property StartType: TStartType read FStartType write FStartType default stAuto;
    property OnContinue: TContinueEvent read FOnContinue write SetOnContinue;
    property OnExecute: TServiceEvent read FOnExecute write FOnExecute;
    property OnPause: TPauseEvent read FOnPause write SetOnPause;
    property OnShutdown: TServiceEvent read FOnShutdown write FOnShutdown;
    property OnStart: TStartEvent read FOnStart write FOnStart;
    property OnStop: TStopEvent read FOnStop write SetOnStop;
  end;

type
  { TServiceApplication }
  TServiceApplication = class(TObject)
  private
    FHandle: THandle;
    FDialogHandle: HWnd;
    FService: TService;
    FDesigning: Boolean;
    FIsRunMode: Boolean;
    FEventLogger: TEventLogger;
    FObjectInstance: Pointer;
    FTerminate: Boolean;
    procedure SetServiceDisplayName(const Value: string);
    function GetServiceDisplayName: string;
    procedure SetServiceName(const Value: string);
    function GetServiceName: string;
    procedure Run;
  protected
    procedure DispatchServiceMain(Argc: DWord; Argv: PLPSTR);
    procedure DoHandleException(E: Exception); dynamic;
    procedure RegisterServices(Install, Silent: Boolean);
    function IsDlgMsg(var Msg: TMsg): Boolean;
    procedure Idle(const Msg: TMsg);
    procedure WndProc(var Message: TMessage);
    procedure ServiceInitialize; virtual;
    procedure ServiceExecute; virtual;
    procedure ServiceCreate;
    procedure ServiceDestroy;
    procedure Terminate;
    procedure Initialize;
  public
    constructor Create(); virtual;
    destructor Destroy; override;
    procedure Start; //inline;
    procedure Stop; //inline;
    procedure HandleMessage;
    class function Installing: Boolean;
    function ProcessMessage(var Msg: TMsg): Boolean;
    class function ServiceGetStatus(const Machine, ServerName: string ): Cardinal;
    class function ServiceUninstalled(const Machine, ServerName: string): Boolean;
    // 是否是标准运行模式 (服务未安装时、安装模式、卸载模式等都返回False)
    property IsRunMode: Boolean read FIsRunMode;
    property Handle: THandle read FHandle;
    property Terminated: Boolean read FTerminate;
    property Service: TService read FService;
    property ServiceName: string read GetServiceName write SetServiceName;
    property ServiceDisplayName: string read GetServiceDisplayName write SetServiceDisplayName;
  end;

function IsDebuggerPresent: Boolean; stdcall; external kernel32;
function IsService: Boolean;
function IsRunMode: Boolean;

implementation

resourcestring
  SInvalidName = '''''%s'''' is not a valid server name';
  SWindowClass = 'Error creating window class';
  SServiceInstallOK = '服务安装成功';
  SServiceInstallFailed = '服务 "%s" 安装时发生错误: "%s"';
  SServiceUninstallOK = '服务卸载成功';
  SServiceUninstallFailed = '服务 "%s" 卸载时发生错误: "%s"';
  SServiceFailed = '服务失败 %s: %s';
  SCustomError = '服务失败的自定义消息(%d): %s';
  SExecute = '执行';
  SStart = '开始';
  SStop = '停止';
  SPause = '暂停';
  SContinue = '继续';
  SInterrogate = '询问';
  SShutdown = '关闭';

const
  CM_BASE                   = $B000;
  CN_BASE                   = $BC00;
  CM_ACTIVATE               = CM_BASE + 0;
  CM_DEACTIVATE             = CM_BASE + 1;
  CM_DIALOGHANDLE           = CM_BASE + 49;
  
var
  Application: TServiceApplication = nil;

function IsService: Boolean;
var
  AWnd: HWND;
  APId: Cardinal;
begin
  AWnd := GetForegroundWindow;
  if AWnd <> 0 then begin
    GetWindowThreadProcessId(AWnd, APId); 
    Result := not (APId = GetCurrentProcessId);
  end else
    Result := True;
end;

function IsRunMode: Boolean;
begin
  if IsConsole then
    Result := not TServiceApplication.Installing()
  else
    Result := True;
end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  Application.FService.Controller(CtrlCode);
end;

type
  TServiceClass = class of TService;

procedure ServiceMain(Argc: DWord; Argv: PLPSTR); stdcall;
begin
  Application.DispatchServiceMain(Argc, Argv);
end;

procedure DoneServiceApplication;
begin
  if Assigned(Application) then   
    with Application do begin
      FDesigning := True;
      FreeAndNil(FService);
    end;
end;

function FindSwitch(const Switch: string): Boolean;
begin
  Result := FindCmdLineSwitch(Switch, ['-', '/'], True);
end;

{ TEventLogger }

constructor TEventLogger.Create(Name: String);
begin
  FName := Name;
  FEventLog := 0;
end;

destructor TEventLogger.Destroy;
begin
  if FEventLog <> 0 then
    DeregisterEventSource(FEventLog);
  inherited;
end;

procedure TEventLogger.LogMessage(const Message: String; EventType: DWord;
  Category: Word; ID: DWord);
var
  P: Pointer;
begin
  P := PChar(Message);
  if FEventLog = 0 then
    FEventLog := RegisterEventSource(nil, PChar(FName));
  ReportEvent(FEventLog, EventType, Category, ID, nil, 1, 0, @P, nil);
end;

{ TServiceApplication }

var
  WindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @DefWindowProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'ServiceApp');

constructor TServiceApplication.Create();
var
  TempClass: TWndClass;
begin
  Application := Self;
  FIsRunMode := not Installing();
  FDesigning := False;
  FEventLogger := TEventLogger.Create(ExtractFileName(ParamStr(0)));
  FService := TService.Create();
  FService.FEventLogger := FEventLogger;
  if not IsConsole then begin 
    FObjectInstance := Classes.MakeObjectInstance(WndProc);
    WindowClass.lpfnWndProc := @DefWindowProc;
    if not GetClassInfo(HInstance, WindowClass.lpszClassName, TempClass) then
    begin
      WindowClass.hInstance := HInstance;
      if Windows.RegisterClass(WindowClass) = 0 then
        raise EOutOfResources.Create(SWindowClass);
    end;
    FHandle := CreateWindowEx(0, WindowClass.lpszClassName, PChar(FService.Name),
      WS_POPUP or WS_CAPTION or WS_CLIPSIBLINGS or WS_SYSMENU
      or WS_MINIMIZEBOX,
      GetSystemMetrics(SM_CXSCREEN) div 2,
      GetSystemMetrics(SM_CYSCREEN) div 2,
      0, 0, 0, 0, HInstance, nil);
    SetWindowLong(FHandle, GWL_WNDPROC, Longint(FObjectInstance));
  end;
end;

destructor TServiceApplication.Destroy;
begin
  FDesigning := True;
  FEventLogger.Free;
  FreeAndNil(FService);
  if (FHandle <> 0) then
    DestroyWindow(FHandle);
  if FObjectInstance <> nil then
    Classes.FreeObjectInstance(FObjectInstance);
  inherited Destroy;
  Application := nil;
end;

procedure TServiceApplication.DispatchServiceMain(Argc: DWord; Argv: PLPSTR);
begin
  if Assigned(FService) then    
    FService.Main(Argc, Argv);
end;

procedure TServiceApplication.ServiceInitialize;
begin
end;

procedure TServiceApplication.DoHandleException(E: Exception);
begin
  FEventLogger.LogMessage(E.Message);
end;

function TServiceApplication.GetServiceDisplayName: string;
begin
  Result := FService.DisplayName;
end;

function TServiceApplication.GetServiceName: string;
begin
  Result := FService.Name;
end;

procedure TServiceApplication.HandleMessage;
var
  Msg: TMsg;
begin
  if not ProcessMessage(Msg) then Idle(Msg);
end;

procedure TServiceApplication.Idle(const Msg: TMsg);
var
  Done: Boolean;
begin
  Done := True;
  if (GetCurrentThreadID = MainThreadID) and CheckSynchronize then
    Done := False;
  if Done then WaitMessage;
end;

procedure TServiceApplication.Initialize;
begin
  if InitProc <> nil then TProcedure(InitProc);
end;

class function TServiceApplication.Installing: Boolean;
begin
  Result := FindSwitch('INSTALL') or FindSwitch('UNINSTALL');
end;

function TServiceApplication.IsDlgMsg(var Msg: TMsg): Boolean;
begin
  Result := False;
  if FDialogHandle <> 0 then
    if IsWindowUnicode(Msg.hwnd) then
      Result := IsDialogMessageW(FDialogHandle, Msg)
    else
      Result := IsDialogMessageA(FDialogHandle, Msg);
end;

function TServiceApplication.ProcessMessage(var Msg: TMsg): Boolean;
var
  Unicode: Boolean;
  Handled: Boolean;
  MsgExists: Boolean;
begin
  Result := False;
  if PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE) then
  begin
    Unicode := (Msg.hwnd <> 0) and IsWindowUnicode(Msg.hwnd);
    if Unicode then
      MsgExists := PeekMessageW(Msg, 0, 0, 0, PM_REMOVE)
    else
      MsgExists := PeekMessage(Msg, 0, 0, 0, PM_REMOVE);
    if not MsgExists then Exit;
    Result := True;
    if Msg.Message <> WM_QUIT then
    begin
      Handled := False;
      if not Handled and not IsDlgMsg(Msg) then
      begin
        TranslateMessage(Msg);
        if Unicode then
          DispatchMessageW(Msg)
        else
          DispatchMessage(Msg);
      end;
    end
    else
      FTerminate := True;
  end;
end;

procedure TServiceApplication.RegisterServices(Install, Silent: Boolean);

  procedure InstallService(Service: TService; SvcMgr: Integer);
  var
    TmpTagID, Svc: Integer;
    PTag, PSSN: Pointer;
    Path: string;
  begin
    Path := ParamStr(0);
    with Service do begin
      TmpTagID := FTagID;
      if TmpTagID > 0 then PTag := @TmpTagID else PTag := nil;
      PSSN := nil;
      Svc := CreateService(SvcMgr, PChar(Name), PChar(DisplayName),
        SERVICE_ALL_ACCESS, GetNTServiceType, GetNTStartType, GetNTErrorSeverity,
        PChar(Path), '', PTag, '', PSSN, '');
      FTagID := TmpTagID;
      if Svc = 0 then
        RaiseLastOSError;
      CloseServiceHandle(Svc);
    end;
  end;

  procedure UninstallService(Service: TService; SvcMgr: Integer);
  var
    Svc: Integer;
  begin
    with Service do
    begin
      Svc := OpenService(SvcMgr, PChar(Name), SERVICE_ALL_ACCESS);
      if Svc = 0 then RaiseLastOSError;
      try
        if not DeleteService(Svc) then RaiseLastOSError;
      finally
        CloseServiceHandle(Svc);
      end;
    end;
  end;


var
  SvcMgr: Integer;
  Success: Boolean;
  Msg: string;
begin
  Success := True;
  SvcMgr := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SvcMgr = 0 then RaiseLastOSError;
  try
    try
      if Install then
        InstallService(FService, SvcMgr) else
        UninstallService(FService, SvcMgr)
    except
      on E: Exception do
      begin
        Success := False;
        if Install then
          Msg := SServiceInstallFailed else
          Msg := SServiceUninstallFailed;
        MessageBox(Handle, PChar(Format(Msg, [FService.DisplayName, E.Message])),
          PChar(FService.Name), 32);
      end;
    end;
    if Success and not Silent then
      if Install then
        MessageBox(Handle, PChar(SServiceInstallOK), PChar(FService.Name), 0)
      else
        MessageBox(Handle, PChar(SServiceUninstallOK), PChar(FService.Name), 0);
  finally
    CloseServiceHandle(SvcMgr);
  end;
end;

type
  TServiceTableEntryArray = array of TServiceTableEntry;

  TServiceStartThread = class(TThread)
  private
    FServiceStartTable: TServiceTableEntryArray;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(Services: TServiceTableEntryArray);
  end;

constructor TServiceStartThread.Create(Services: TServiceTableEntryArray);
begin
  FreeOnTerminate := False;
  ReturnValue := 0;
  FServiceStartTable := Services;
  inherited Create(False);
end;

procedure TServiceStartThread.DoTerminate;
begin
  inherited DoTerminate;
  PostMessage(Application.Handle, WM_QUIT, 0, 0);
end;

procedure TServiceStartThread.Execute;
begin
  if StartServiceCtrlDispatcher(FServiceStartTable[0]) then
    ReturnValue := 0
  else
    ReturnValue := GetLastError;
end;

procedure TServiceApplication.Run;
var
  ServiceStartTable: TServiceTableEntryArray;
  StartThread: TServiceStartThread;
begin
  AddExitProc(DoneServiceApplication);
  if FindSwitch('INSTALL') then
    RegisterServices(True, FindSwitch('SILENT'))
  else if FindSwitch('UNINSTALL') then
    RegisterServices(False, FindSwitch('SILENT'))
  else begin
    SetLength(ServiceStartTable, 1);
    FillChar(ServiceStartTable[0], SizeOf(TServiceTableEntry) * (1 + 1), 0);
    ServiceStartTable[0].lpServiceName := PChar(FService.Name);
    ServiceStartTable[0].lpServiceProc := @ServiceMain;
    StartThread := TServiceStartThread.Create(ServiceStartTable);
    try
      while not Terminated do
      try
        HandleMessage;
      except
        on E: Exception do
          DoHandleException(E);
      end;
      if StartThread.ReturnValue <> 0 then begin
        FEventLogger.LogMessage(SysErrorMessage(StartThread.ReturnValue));
        if not IsConsole then
          MessageBox(Handle, PChar(SysErrorMessage(StartThread.ReturnValue)), PChar(FService.Name), 48);
      end;
    finally
      Terminate;
      StartThread.Free;
    end;
  end;
end;

procedure TServiceApplication.ServiceCreate;
begin
  if FIsRunMode then
    FIsRunMode := (not ServiceUninstalled('', FService.Name)) or IsConsole;
  Initialize;
  ServiceInitialize();
end;

procedure TServiceApplication.ServiceDestroy;
begin
  Terminate;
end;

procedure TServiceApplication.ServiceExecute;
var
  s: Char;
begin
  if (not IsService) or (IsConsole) then begin  
    while not Self.Terminated do begin
      Sleep(500);
      Read(s);
      if s =#13 then
        Break;
    end;
  end;
end;

class function TServiceApplication.ServiceGetStatus(const Machine,
  ServerName: string): Cardinal;
var
  schm, schs: SC_Handle;
  ss: TServiceStatus;
begin
  Result := 0;
  schm := OpenSCManager(PChar(Machine), nil, SC_MANAGER_CONNECT);
  if (schm > 0)then begin
    schs := OpenService(schm, PChar(ServerName), SERVICE_QUERY_STATUS);
    if(schs > 0)then begin
      if (QueryServiceStatus(schs, ss)) then
        Result := ss.dwCurrentState;
      CloseServiceHandle(schs);
    end;
    CloseServiceHandle(schm);
  end;
end;

class function TServiceApplication.ServiceUninstalled(const Machine,
  ServerName: string): Boolean;
begin
  Result := ServiceGetStatus(Machine, ServerName) = 0;
end;

procedure TServiceApplication.SetServiceDisplayName(const Value: string);
begin
  FService.DisplayName := Value;
end;

procedure TServiceApplication.SetServiceName(const Value: string);
begin
  FService.Name := Value;
end;

procedure TServiceApplication.Start;
begin
  if not IsService or IsConsole then begin
    try
      ServiceCreate;
      ServiceExecute;
    finally
      ServiceDestroy;
    end;
  end else begin
    ServiceCreate;
    ServiceExecute;
    Run;
  end;
end;

procedure TServiceApplication.Stop;
begin
  ServiceDestroy;
end;

procedure TServiceApplication.Terminate;
begin
  if CallTerminateProcs then PostQuitMessage(0);
end;

procedure TServiceApplication.WndProc(var Message: TMessage);
  procedure Default;
  begin
    with Message do
      Result := DefWindowProc(FHandle, Msg, WParam, LParam);
  end;

begin
  Message.Result := 0;
  with Message do begin
    case Message.Msg of
      WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
        Result := SendMessage(LParam, CN_BASE + Msg, WParam, LParam);
      WM_ENDSESSION:
        if TWMEndSession(Message).EndSession then
        begin
          CallTerminateProcs;
          Halt;
        end;
      WM_QUERYENDSESSION:
        Result := 1;
      WM_ACTIVATEAPP:
        begin
          Default;
          if TWMActivateApp(Message).Active then begin
            PostMessage(FHandle, CM_ACTIVATE, 0, 0);
            HandleMessage;
          end else begin
            PostMessage(FHandle, CM_DEACTIVATE, 0, 0);
          end;
        end;
      CM_DIALOGHANDLE:
        if wParam = 1 then
          Result := FDialogHandle
        else
          FDialogHandle := lParam;
    else
      Default;
    end;
  end;
end;

{ TService }

procedure TService.Controller(CtrlCode: DWord);
begin
  PostThreadMessage(ServiceThread.ThreadID, CM_SERVICE_CONTROL_CODE, CtrlCode, 0);
  if ServiceThread.Suspended then
    {$IFDEF UNICODE}
    ServiceThread.Start;
    {$ELSE}
    ServiceThread.Resume;
    {$ENDIF}
end;

constructor TService.Create;
begin
  FDesigning := False;
  FInteractive := False;
  FServiceType := stWin32;
  FErrorSeverity := esNormal;
  FStartType := stAuto;
  FAllowStop := True;
  FAllowPause := True;
  FTagID := 0;
end;

destructor TService.Destroy;
begin
  FDesigning := True;
  inherited Destroy;
end;

function TService.DoContinue: Boolean;
begin
  Result := True;
  Status := csContinuePending;
  if Assigned(FOnContinue) then FOnContinue(Self, Result);
  if Result then
    Status := csRunning;
end;

procedure TService.DoInterrogate;
begin
  ReportStatus;
end;

function TService.DoPause: Boolean;
begin
  Result := True;
  Status := csPausePending;
  if Assigned(FOnPause) then FOnPause(Self, Result);
  if Result then
  begin
    Status := csPaused;
    ServiceThread.Suspend;
  end;
end;

procedure TService.DoShutdown;
begin
  Status := csStopPending;
  try
    if Assigned(FOnShutdown) then FOnShutdown(Self);
  finally
    { Shutdown cannot abort, it must stop regardless of any exception }
    ServiceThread.Terminate;
  end;
end;

procedure TService.DoStart;
begin
  try
    Status := csStartPending;
    try
      FServiceThread := TServiceThread.Create(Self);
      {$IFDEF UNICODE}
      FServiceThread.Start;
      {$ELSE}
      FServiceThread.Resume;
      {$ENDIF}
      FServiceThread.WaitFor;
      FreeAndNil(FServiceThread);
    finally
      Status := csStopped;
    end;
  except
    on E: Exception do
      LogMessage(Format(SServiceFailed,[SExecute, E.Message]));
  end;
end;

function TService.DoStop: Boolean;
begin
  Result := True;
  Status := csStopPending;
  if Assigned(FOnStop) then FOnStop(Self, Result);
  if Result then ServiceThread.Terminate;
end;

function TService.GetDisplayName: String;
begin
  if FDisplayName <> '' then
    Result := FDisplayName
  else
    Result := Name;
end;

function TService.GetNTControlsAccepted: Integer;
begin
  Result := SERVICE_ACCEPT_SHUTDOWN;
  if AllowStop then Result := Result or SERVICE_ACCEPT_STOP;
  if AllowPause then Result := Result or SERVICE_ACCEPT_PAUSE_CONTINUE;
end;

function TService.GetNTErrorSeverity: Integer;
const
  NTErrorSeverity: array[TErrorSeverity] of Integer = (SERVICE_ERROR_IGNORE,
    SERVICE_ERROR_NORMAL, SERVICE_ERROR_SEVERE, SERVICE_ERROR_CRITICAL);
begin
  Result := NTErrorSeverity[FErrorSeverity];
end;

function TService.GetNTServiceType: Integer;
const
  NTServiceType: array[TServiceType] of Integer = ( SERVICE_WIN32_OWN_PROCESS,
    SERVICE_KERNEL_DRIVER, SERVICE_FILE_SYSTEM_DRIVER);
begin
  Result := NTServiceType[FServiceType];
  if (FServiceType = stWin32) and Interactive then
    Result := Result or SERVICE_INTERACTIVE_PROCESS;
end;

function TService.GetNTStartType: Integer;
const
  NTStartType: array[TStartType] of Integer = (SERVICE_BOOT_START,
    SERVICE_SYSTEM_START, SERVICE_AUTO_START, SERVICE_DEMAND_START,
    SERVICE_DISABLED);
begin
  Result := NTStartType[FStartType];
  if (FStartType in [stBoot, stSystem]) and (FServiceType <> stDevice) then
    Result := SERVICE_AUTO_START;
end;

procedure TService.LogMessage(Message: String; EventType: DWord; Category,
  ID: Integer);
begin
  if Assigned(FEventLogger) then
    FEventLogger.LogMessage(Message, EventType, Category, ID);
end;

procedure TService.Main(Argc: DWord; Argv: PLPSTR);
type
  PPCharArray = ^TPCharArray;
  TPCharArray = array [0..1024] of PChar;
begin
  FStatusHandle := RegisterServiceCtrlHandler(PChar(Name), @ServiceController);
  if (FStatusHandle = 0) then
    LogMessage(SysErrorMessage(GetLastError))
  else
    DoStart;    
end;

procedure TService.ReportStatus;
const
  LastStatus: TCurrentStatus = csStartPending;
  NTServiceStatus: array[TCurrentStatus] of Integer = (SERVICE_STOPPED,
    SERVICE_START_PENDING, SERVICE_STOP_PENDING, SERVICE_RUNNING,
    SERVICE_CONTINUE_PENDING, SERVICE_PAUSE_PENDING, SERVICE_PAUSED);
  PendingStatus: set of TCurrentStatus = [csStartPending, csStopPending,
    csContinuePending, csPausePending];
var
  ServiceStatus: TServiceStatus;
begin
  with ServiceStatus do
  begin
    dwWaitHint := 5000;
    dwServiceType := GetNTServiceType;
    if FStatus = csStartPending then
      dwControlsAccepted := 0 else
      dwControlsAccepted := GetNTControlsAccepted;
    if (FStatus in PendingStatus) and (FStatus = LastStatus) then
      Inc(dwCheckPoint) else
      dwCheckPoint := 0;
    LastStatus := FStatus;
    dwCurrentState := NTServiceStatus[FStatus];
    dwWin32ExitCode := Win32ErrCode;
    dwServiceSpecificExitCode := ErrCode;
    if ErrCode <> 0 then
      dwWin32ExitCode := ERROR_SERVICE_SPECIFIC_ERROR;
    if not SetServiceStatus(FStatusHandle, ServiceStatus) then
      LogMessage(SysErrorMessage(GetLastError));
  end;
end;

procedure TService.SetInteractive(const Value: Boolean);
begin
  if Value = FInteractive then Exit;
  FInteractive := Value;
end;

procedure TService.SetName(const NewName: string);
begin
  if FName <> NewName then begin
    if (NewName <> '') and not IsValidIdent(NewName) then
      raise EComponentError.CreateResFmt(@SInvalidName, [NewName]);
    FName := NewName;
  end;
end;

procedure TService.SetOnContinue(const Value: TContinueEvent);
begin
  FOnContinue := Value;
  AllowPause := True;
end;

procedure TService.SetOnPause(const Value: TPauseEvent);
begin
  FOnPause := Value;
  AllowPause := True;
end;

procedure TService.SetOnStop(const Value: TStopEvent);
begin
  FOnStop := Value;
  AllowStop := True;
end;

procedure TService.SetStatus(const Value: TCurrentStatus);
begin
  FStatus := Value;
  if not (FDesigning) then
    ReportStatus;
end;

{ TServiceThread }

constructor TServiceThread.Create(Service: TService);
begin
  FService := Service;
  inherited Create(True);
end;

procedure TServiceThread.Execute;
var
  msg: TMsg;
  Started: Boolean;
begin
  PeekMessage(msg, 0, WM_USER, WM_USER, PM_NOREMOVE); { Create message queue }
  try
    // Allow initialization of the Application object after
    // StartServiceCtrlDispatcher to prevent conflicts under
    // Windows 2003 Server when registering a class object with OLE.
    FService.Status := csStartPending;
    Started := True;
    if Assigned(FService.OnStart) then FService.OnStart(FService, Started);
    if not Started then Exit;
    try
      FService.Status := csRunning;
      if Assigned(FService.OnExecute) then
        FService.OnExecute(FService)
      else
        ProcessRequests(True);
      ProcessRequests(False);
    except
      on E: Exception do 
        FService.LogMessage(Format(SServiceFailed,[SExecute, E.Message]));
    end;
  except
    on E: Exception do
      FService.LogMessage(Format(SServiceFailed,[SStart, E.Message]));
  end;
end;

procedure TServiceThread.ProcessRequests(WaitForMessage: Boolean);
const
  ActionStr: array[1..5] of String = (SStop, SPause, SContinue, SInterrogate,
    SShutdown);
var
  msg: TMsg;
  OldStatus: TCurrentStatus;
  ErrorMsg: String;
  ActionOK, Rslt: Boolean;
begin
  while True do
  begin
    if Terminated and WaitForMessage then break;
    if WaitForMessage then
      Rslt := GetMessage(msg, 0, 0, 0)
    else
      Rslt := PeekMessage(msg, 0, 0, 0, PM_REMOVE);
    if not Rslt then break;
    if msg.hwnd = 0 then { Thread message }
    begin
      if msg.message = CM_SERVICE_CONTROL_CODE then
      begin
        OldStatus := FService.Status;
        try
          ActionOK := True;
          case msg.wParam of
            SERVICE_CONTROL_STOP: ActionOK := FService.DoStop;
            SERVICE_CONTROL_PAUSE: ActionOK := FService.DoPause;
            SERVICE_CONTROL_CONTINUE: ActionOK := FService.DoContinue;
            SERVICE_CONTROL_SHUTDOWN: FService.DoShutDown;
            SERVICE_CONTROL_INTERROGATE: FService.DoInterrogate;
          else
            ActionOK := True;
          end;
          if not ActionOK then
            FService.Status := OldStatus;
        except
          on E: Exception do
          begin
            if msg.wParam <> SERVICE_CONTROL_SHUTDOWN then
              FService.Status := OldStatus;
            if msg.wParam in [1..5] then
              ErrorMsg := Format(SServiceFailed, [ActionStr[msg.wParam], E.Message])
            else
              ErrorMsg := Format(SCustomError,[msg.wParam, E.Message]);
            FService.LogMessage(ErrorMsg);
          end;
        end;
      end else
        DispatchMessage(msg);
    end else
      DispatchMessage(msg);
  end;
end;

end.
