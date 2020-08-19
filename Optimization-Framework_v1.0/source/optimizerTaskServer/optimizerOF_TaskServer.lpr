program optimizerOF_TaskServer;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

uses
  cthreads, SysUtils, StrUtils, Classes, uhttp_server,

  uClient, uVirtualThread, uClientBase, uClientTask, uTaskManager, uDatabase,
  uHTTP_Utils, uTaskManagerTable;

type

  TSettings = record
    taskManagerTableSettings : TTaskManagerTableSettings;
    httpPort : Word;
  end;

  { THTTPHandlerClass }

  THTTPHandlerClass = class
  protected
    _taskManagerTable : TTaskManagerTable;
  public
    constructor Create(taskManagerTable : TTaskManagerTable);

    // handles a request, fetches the appropriate task manager and performs
    // the requested action on it returning the necessary information
    procedure handleRequest(const filePath, fileName: string; _GET, _POST,
      _SERVER: TStringList; var ContentType, ContentText: string;
      var ContentStream: TMemoryStream; var httpResult: Integer);
  end;

{ THTTPHandlerClass }

constructor THTTPHandlerClass.Create(taskManagerTable: TTaskManagerTable);
begin
  _taskManagerTable := taskManagerTable;
end;

procedure THTTPHandlerClass.handleRequest(const filePath, fileName: string;
  _GET, _POST, _SERVER: TStringList; var ContentType, ContentText: string;
  var ContentStream: TMemoryStream; var httpResult: Integer);
var
  clientInfo : TClientInfo;
  clientTask : TClientTask;
  taskManager : TTaskManager;
  taskManagerList : TTaskManagerList;
  taskManagerIdentifier : TTaskManagerIdentifier;
  doNotMarkAsActive, forceAcquisition : boolean;
  x : integer;
  //temp : TStringList;
begin
  //Writeln('HTTP-Request (1)');
  clientInfo.host := _SERVER.Values['REMOTE_HOST'];
  clientInfo.IP   := _SERVER.Values['REMOTE_ADDR'];
  if (fileName = 'submitResult') or (fileName = 'showTaskState') then
  begin
    _taskManagerTable.updateClientInfoByTaskID(_GET.Values['taskID'], clientInfo);

    taskManagerIdentifier := createIdentifier(clientInfo.userName, clientInfo.taskName);
  end
  else
  begin
    //Writeln('HTTP-Request (2)');
    taskManagerIdentifier := createIdentifier(_GET.Values['userName'], _GET.Values['taskName']);
    //Writeln('HTTP-Request (3)');
  end;

  //Writeln('HTTP-Request (4)');
  taskManager := _taskManagerTable.getTaskManager(taskManagerIdentifier);
  //Writeln('HTTP-Request (5)');


  // status page
  if (fileName = '') or (fileName = 'showState') then
  begin
    //Writeln('showState');
    ContentType := 'text/html';
    ContentText := _taskManagerTable.getState(taskManager);
  end

  // clear tasks
  else if fileName = 'clearTasks' then
  begin
    if taskManager <> nil then
    begin
      taskManager.clearTasks;
    end;

    ContentType := 'text/html';
    ContentText := _taskManagerTable.getState(taskManager);
  end

  else if fileName = 'ping' then
  begin
    ContentText := '0';

    if taskManager <> nil then
    begin
      clientInfo.instanceID   := _GET.Values['clientID'];
      clientInfo.userName     := _GET.Values['userName'];
      clientInfo.taskName      := _GET.Values['taskName'];

      taskManager.ping(clientInfo);

      ContentText := 'ok!';
    end;
  end

  // get a task
  else if fileName = 'getTask' then
  begin
    clientInfo.instanceID   := _GET.Values['clientID'];
    clientInfo.userName     := _GET.Values['userName'];
    clientInfo.taskName      := _GET.Values['taskName'];
    clientInfo.nMaxThreads  := StrToIntDef(_GET.Values['numMaxThreads'], -1);
    clientInfo.nFastThreads := StrToIntDef(_GET.Values['numFastThreads'], -1);

    doNotMarkAsActive := StrToIntDef(_GET.Values['doNotMarkAsActive'], 0) <> 0;
    forceAcquisition  := StrToIntDef(_GET.Values['force'], 0) <> 0;

    // TO DO: only temporarily
    //forceAcquisition := true;

    if taskManager <> nil then
    begin
      {if forceAcquisition then
        Writeln('forceAcquisition!');}

      try
        clientTask := taskManager.getTask(clientInfo, doNotMarkAsActive, forceAcquisition);
      except on e: exception do
        WriteLn(e.Message);
      end;

      ContentText := #13#10;
      if clientTask.ID <> '' then
      begin
        ContentText := ContentTExt + clientTask.ID + #13#10;
        ContentText := ContentText + clientTask.cmdLine + #13#10;
        ContentText := ContentText + clientTask.errorName + #13#10;
        ContentText := ContentText + clientTask.workingDirectory + #13#10;
        ContentText := ContentText + FloatToStr(clientTask.deadline) + #13#10;
        ContentText := ContentText + clientTask.evaluationBinary;
      end
      else
      begin
        ContentText := IntToStr(taskManager.getSleepTime());
      end;
    end;
  end

  // submit results for a task
  else if fileName = 'submitResult' then
  begin
    clientTask.ID         := _GET.Values['taskID'];
    clientTask.success    := StrToIntDef(_GET.Values['successful'], 0) <> 0;
    clientTask.errorValue := _GET.Values['errorValue'];
    clientTask.output     := _POST.Values['output'];
    for x := 0 to high(clientTask.errorNames) do
    begin
      clientTask.errorNames[x]  := _GET.Values['errorName' + IntToStr(x)];
      clientTask.errorValues[x] := _GET.Values['errorValue' + IntToStr(x)];
    end;

    {for x := 0 to _GET.Count-1 do
      Writeln(_GET.Names[x], ': ', _GET.Values[_GET.Names[x]]);
    for x := 0 to _POST.Count-1 do
      Writeln(_POST.Names[x], '(Size: ', Length(_POST.Values[_POST.Names[x]]),')');}

    if taskManager <> nil then
    begin
      try
        ContentText := taskManager.submitResult(clientTask, clientInfo);
      except on e: exception do
        WriteLn(e.Message);
      end;
    end;
  end
  else if fileName = 'ignoreClient' then
  begin
    clientInfo.instanceID   := _GET.Values['clientID'];
    clientInfo.IP           := _GET.Values['clientIP'];

    taskManagerList := _taskManagerTable.getTaskManagerList();
    for x := 0 to high(taskManagerList) do
    begin
      if taskManagerList[x].ignoreClient(clientInfo, trim(_GET.Values['value']) = '1') then
        break;
    end;

    ContentType := 'text/html';
    ContentText := _taskManagerTable.getState(taskManager);
  end

  else if fileName = 'showTaskState' then
  begin
    clientTask.ID         := _GET.Values['taskID'];

    if taskManager <> nil then
    begin
      ContentType := 'text/html';
      ContentText := taskManager.showTaskState(clientTask, clientInfo);
    end;
  end;

end;


function getTaskServerSettings() : TSettings;
var
  x : integer;
  value : string;

  function isParam(const Name: string; parampos: integer;
    out Value: string): boolean;
  var
    param: string;
  begin
    param := ParamStr(parampos);
    Result := StrUtils.AnsiStartsStr(Name, param)
              and (Length(param) > Length(name))
              and (param[Length(name)+1] = '=');
    if Result then
    begin
      Value := Copy(param, Length(Name + '=') + 1, Length(param));
      Value := StringReplace(Value, '''', '"', [rfReplaceAll]);
    end
  end;
begin

  // set standard parameters
  result.httpPort := 8083;
  with result do
  begin
    taskManagerTableSettings.maxInactivity := EncodeTime(2,0,0,0);
    with taskManagerTableSettings do
    begin

      taskManagerSettings.sqlTable          := 'tasks';
      taskManagerSettings.useDBLocks        := false;
      taskManagerSettings.maxDeadline       := EncodeTime(0,5,0,0);
      taskManagerSettings.initialAvgRunTime := EncodeTime(0,0,20,0);
      taskManagerSettings.fetchTasksInterval := 2;
      taskManagerSettings.maxFailureCounter := 2;
      taskManagerSettings.restrictedAssignments := true;
      with taskManagerSettings do
      begin
        clientSettings.maxLoadFactor     := 1.8;
        clientSettings.averagingWeight   := 1/6;
        clientSettings.epsilonFinishTime := EncodeTime(0,0,1,0);
        clientSettings.silenceTimeThresholdFactor := 1;
      end;
    end;
  end;

  for x := 1 to ParamCount do
  begin
    if isParam('--httpPort', x, value) then
      result.httpPort := StrToIntDef(value, result.httpPort)
    else if isParam('--unrestrictedTaskAssignments', x, value) then
      result.taskManagerTableSettings.taskManagerSettings.restrictedAssignments := (trim(value) <> '1')
    else if isParam('--minUpdateInterval', x, value) then
      result.taskManagerTableSettings.taskManagerSettings.fetchTasksInterval := StrToIntDef(value, result.taskManagerTableSettings.taskManagerSettings.fetchTasksInterval)
    else if isParam('--useDBLocks', x, value) then
      result.taskManagerTableSettings.taskManagerSettings.useDBLocks := trim(value) = '1';
  end;
end;

var
  dbSettings : TDBSettings;
  dbConnection : TDBConnection;
  settings : TSettings;
  TaskManagerTable : TTaskManagerTable;
  HTTPHandler : THTTPHandlerClass;

begin
  // read settings for the database
  dbSettings := readDBSettings('optimizerOF_Distributed.xml');

  // get settings
  settings := getTaskServerSettings();

  // instantiate database connection
  dbConnection := TDBConnection.Create(dbSettings);
  try

    // instantiate a table containing task managers
    TaskManagerTable := TTaskManagerTable.Create(dbConnection, settings.taskManagerTableSettings);
    try

      // instantiate the handler for the HTTP Requests
      HTTPHandler := THTTPHandlerClass.Create(TaskManagerTable);
      try

        // instantiate the HTTP server
        with TTCPHttpDaemon.Create(settings.httpPort, @HTTPHandler.handleRequest) do
        begin
          Active := true;
          while true do
            Sleep(0);
          Free;
        end;

      finally
        HTTPHandler.Free;
      end;

    finally
      TaskManagerTable.Free;
    end;

  finally
    dbConnection.Free;
  end;
end.

