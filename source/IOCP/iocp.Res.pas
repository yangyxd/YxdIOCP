unit iocp.Res;

interface

const
  BytePerKB = 1024;
  BytePerMB = BytePerKB * 1024;
  BytePerGB = BytePerMB * 1024;

const
  IOCP_CORE_LOG_FILE = 'Iocp_Core_Exception';
  IOCP_CORE_DEBUG_FILE = 'Iocp_Core_Debug'; 

resourcestring
  strEngine_DebugInfo   = 'State: %s, Workers: %d';
  strEngine_WorkerTitle = '------------------- Worker Thread (%d) -------------------';
  strWorker_Done        = 'Work done!!!';
  strWorker_Info        = 'ThreadID: %d, ResponeCount: %d';
  strWorker_StateInfo   = 'Working: %s, Waiting: %s, Reserved: %s';
  strRequest_Title      = 'Request State Info:';
  strRequest_State      = 'Finished: %s, Time(ms): %d';

  strSocket_ConnActived = 'On DoConnected event is already actived.';
  strSocket_ConnError   = 'On DoConnected error: %s.';
  strSocket_RSError     = 'Read-only data flow';
  strSocket_RSNotSup    = 'This operation is not supported';

  strConn_CounterInc    = 'ContextCounter: +(%d):0x%.8x, %s';
  strConn_CounterDec    = 'ContextCounter: -(%d):0x%.8x, %s';
  strConn_CounterView   = 'ContextCounter: *(%d):0x%.8x, %s';

  strSend_PushFail  = '[0x%.4x]投递发送请求数据包超出队列允许的最大长度[%d/%d]。';
  strSend_ReqKick   = '(0x%.4x)断开请求信息: %s';
  strRecv_EngineOff = '[0x%.4x]响应接收请求时发现IOCP服务关闭';
  strRecv_Error     = '[0x%.4x]响应接收请求时出现了错误。错误代码:%d!';
  strRecv_Zero      = '[0x%.4x]接收到0字节的数据,该连接将断开!';
  strRecv_PostError = '[0x%.4x]投递接收请求时出现了错误。错误代码:%d!';
  strSend_Zero      = '[0x%.4x]投递发送请求数据时遇到0长度数据。进行关闭处理';
  strSend_EngineOff = '[0x%.4x]响应发送数据请求时发现IOCP服务关闭';
  strSend_Err       = '[0x%.4x]响应发送数据请求时出现了错误。错误代码:%d!';
  strSend_PostError = '[0x%.4x]投递发送数据请求时出现了错误。错误代码:%d';
  strConn_Request   = '正在连接%s(%d)';
  strConn_TimeOut   = '建立连接超时(%s:%d)';
  strBindingIocpError = '[%d]绑定到IOCP句柄时出现了异常, 错误代码:%d, (%s)';

  STooFewWorkers = '指定的最小工作者数量太少(必需大于等于1)。';   
  SBadWaitDoneParam = '未知的等待正在执行作业完成方式:%d';

  /// =========== iocpTcpServer 状态信息============
  strState_Active      = '服务状态: 开启';
  strState_MonitorNull = '没有创建监控器';
  strState_ObjectNull  = '没有监控对象';    //'iocp server is null'
  strState_Off         = '服务状态: 关闭';
  strRecv_SizeInfo     = '接收数据: %s';
  strSend_SizeInfo     = '发送数据: %s';
  strRecv_PostInfo     = '接收信息: 投递:%d, 回应:%d, 剩余:%d';  //post:%d, response:%d, remain:%d
  strSend_Info         = '发送信息: 投递:%d, 回应:%d, 剩余:%d';  //post:%d, response:%d, remain:%d
  strSendQueue_Info    = '发送队列: 压入/弹出/完成/终止:%d, %d, %d, %d';//push/pop/complted/abort:%d, %d, %d, %d
  strSendRequest_Info  = '发送对象: 创建:%d, 借出:%d, 还回:%d';  //'create:%d, out:%d, return:%d'
  strAcceptEx_Info     = 'AcceptEx: 投递:%d, 回应:%d';      //'post:%d, response:%d'
  strSocketHandle_Info = '套接字句柄: 创建:%d, 销毁:%d';  //'create:%d, destroy:%d'
  strContext_Info      = '连接对象: 创建:%d, 借出:%d, 还回:%d';  //'create:%d, out:%d, return:%d'
  strOnline_Info       = '在线信息: %d';
  strWorkers_Info      = '工作线程: %d';
  strRunTime_Info      = '运行信息: %s';
  /// =========== 服务端状态信息============

  strCannotConnect     = '当前状态下不能进行连接...';
  strConnectError      = '建立连接失败, 错误代码: %d';
  strConnectNonExist   = '连接不存在';
  strStreamReadTimeOut = '流读取错误';

  
implementation

end.
