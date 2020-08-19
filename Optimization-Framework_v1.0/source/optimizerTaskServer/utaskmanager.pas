unit uTaskManager;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

{
This unit contains a class which is responsible for the distribution of tasks
among the clients.
It aggregates clients, fetches tasks from the database according to given
task identification data, clears old tasks and clients, distributes tasks such
that a minimal overall finish time is achieved, passes results to the database
and creates an HTML page with status information.
}

uses
  Types, Classes, SysUtils, DB, SQLDB, PQConnection, SyncObjs,

  uDatabase, uClient, uClientTask, uVirtualThread, uClientBase;

type

  TOrganizeTasksProc = function() : Integer of object;
  TGetThreadCount = function() : Integer of object;

  { TTaskManagerSettings }

  TTaskManagerSettings = record
    // settings for the clients
    clientSettings : TClientSettings;

    // initial value for the max deadline which is used to compute a limit
    // after which a client is regarded as inactive and deleted from the list
    maxDeadline : TDateTime;

    // amount of re-evaluations if an evaluation has not been successful, yet
    maxFailureCounter : Cardinal;

    // interval for the background thread which periodically fetches tasks from
    // the database and redistributes them among the clients
    fetchTasksInterval : Cardinal;

    // name of the table in the SQL database
    sqlTable : string;

    // shall this program use SQL locks on the table?
    useDBLocks : boolean;

    // an initial value for the average run time of the first client
    initialAvgRunTime : TDateTime;

    // do not assign all but a limited number of tasks
    restrictedAssignments : boolean;
  end;

  { TReorganizationThread }

  TReorganizationThread = class(TThread)
  protected
    _organizeTasks : TOrganizeTasksProc;
    _pause : Cardinal;
    _reorganize : Boolean;
    _getThreadCount : TGetThreadCount;
    _lockReorganize : TCriticalSection;
    procedure Execute; override;
    function getReorganize : boolean;
  public
    procedure reorganize(value : boolean = true);
    constructor Create(organizeTasks: TOrganizeTasksProc;
      getThreadCount : TGetThreadCount; pause: Cardinal);
    destructor Destroy; override;
  end;

  { TTaskManager }

  TTaskManager = class
  protected
    // necessary for fetching the tasks from the database
    _userName, _taskName : string;

    // general settings
    _settings : TTaskManagerSettings;

    // synchronization primitives
    _lock,
    _lockTaskCounter,
    _lockActivity : TMultiReadExclusiveWriteSynchronizer;

    // thread which fetches tasks from the database periodically
    _reorganizationThread : TReorganizationThread;

    // list of clients
    _clients : TClientNodeList;

    // connection to the SQL database
    _connection : TDBConnection;

    // statistic
    _performanceFactor,
    _maxRuntimeFactor : Extended;
    _threadsTotal : Cardinal;
    _maxDeadline : TDateTime;
    _tasks, _availableTasks : Cardinal;
    _lastActivity : TDateTime;

    // above this boundary of the amount of tasks, tasks are distributed even
    // if no tasks are assigned to the requesting client
    // (for computational simplification)
    _unconstraintDistributionBoundary : Extended;

    // (non-synchronized) assigns a task among the clients
    procedure _assignTask(const task : TClientTask);

    // (non-synchronzied) clears all tasks assigned to clients
    procedure _clearLocalTasks;

    // (non-synchronized) set old tasks as non-successful and clear really old
    //                    tasks in the database
    procedure _clearOldTasks;

    // (non-synchronized) deletes clients which have been inactive for too long
    function _clearInactiveClients: TClientInfoArray;

    // (non-synchronized) fetches tasks from the database and distributes them
    //                    among the clients
    function _fetchAndDistributeTasks : Integer;

    // (non-synchronized) creates some statistics necessary for decisions
    procedure _createStatistics;

    // synchronized verion of the function above
    function fetchAndDistributeTasks : Integer;

    // gets a client instances according to the information in clientInfo
    function getClient(const clientInfo : TClientInfo;
      out clientIdx : Integer) : TClientNode;

    // gets a client instances according to the information in clientInfo
    function getClient(const clientInfo : TClientInfo) : TClientNode;

    // gets a client instances according to the information in clientInfo.
    // if it is not existing, yet, it is created
    function getOrCreateClient(const clientInfo: TClientInfo
      ): TClientNode;
  public
    constructor Create(connection: TDBConnection; const userName, taskName: string;
      settings: TTaskManagerSettings);
    destructor Destroy; override;

    // ping!
    procedure ping(const clientInfo: TClientInfo);

    // gets a task for computation
    function getTask(const clientInfo: TClientInfo;
      doNotMarkAsActive: boolean=false; forceAcquisition: boolean=false): TClientTask;

    // submits the results of a computation
    function submitResult(task: TClientTask; const clientInfo: TClientInfo): string;

    // deletes all tasks
    procedure clearTasks;

    // returns a HTML section with information
    function getStatisticsState() : string;
    function getState() : string;

    // returns total number of threads
    function getThreadCount() : Integer;

    function showTaskState(const task: TClientTask; const clientInfo: TClientInfo
      ): string;

    function getSleepTime() : Cardinal;
    function getUserName() : string;
    function getTaskName() : string;
    function getLastActivity() : TDateTime;
    procedure updateActivity;

    function ignoreClient(const clientInfo: TClientInfo; value: boolean): boolean;
  end;
  TTaskManagerList = array of TTaskManager;

implementation

uses
  Math, uHTTP_Utils;

{ TReorganizationThread }

procedure TReorganizationThread.Execute;
var
  x : Integer;
  lastTaskCount, lastThreadCount: Integer;
begin
  lastTaskCount    := 0;
  lastThreadCount := 1;
  while not terminated do
  begin
    for x := 1 to ((lastTaskCount div lastThreadCount) + _pause)*5 do
    begin
      if terminated then
        exit;
      if getReorganize then
        break;
      Sleep(200);
    end;

    reorganize(false);
    lastTaskCount    := _organizeTasks();
    lastThreadCount := max(_getThreadCount(), 1);
  end;
end;

function TReorganizationThread.getReorganize: boolean;
begin
  _lockReorganize.Enter;
  try
    result := _reorganize;
  finally
    _lockReorganize.Leave;
  end;
end;

procedure TReorganizationThread.reorganize(value: boolean);
begin
  _lockReorganize.Enter;
  try
    _reorganize := value;
  finally
    _lockReorganize.Leave;
  end;
end;

constructor TReorganizationThread.Create(organizeTasks: TOrganizeTasksProc;
  getThreadCount: TGetThreadCount; pause: Cardinal);
begin
  inherited Create(true);
  _pause := pause;
  _organizeTasks := organizeTasks;
  _getThreadCount := getThreadCount;

  _reorganize := false;
  _lockReorganize := TCriticalSection.Create;

  Start;
end;

destructor TReorganizationThread.Destroy;
begin
  _lockReorganize.Free;
  inherited Destroy;
end;

{ TTaskManager }

constructor TTaskManager.Create(connection: TDBConnection; const userName,
  taskName: string; settings: TTaskManagerSettings);
begin
  _connection := connection;
  _userName   := userName;
  _taskName    := taskName;
  _settings   := settings;

  _tasks          := 0;
  _availableTasks := 0;
  _maxDeadline   := settings.maxDeadline;

  _lock := TMultiReadExclusiveWriteSynchronizer.Create;
  _lockTaskCounter := TMultiReadExclusiveWriteSynchronizer.Create;
  _lockActivity := TMultiReadExclusiveWriteSynchronizer.Create;
  _reorganizationThread := TReorganizationThread.Create(@fetchAndDistributeTasks,
    @getThreadCount, _settings.fetchTasksInterval);

  //Writeln('TaskManager.Create (1)');
  fetchAndDistributeTasks;
  //Writeln('TaskManager.Create (2)');
end;

destructor TTaskManager.Destroy;
var
  x : integer;
begin
  _reorganizationThread.Terminate;
  _reorganizationThread.WaitFor;
  _reorganizationThread.Free;
  for x := 0 to high(_clients) do
    _clients[x].Free;
  SetLength(_clients, 0);
  _lockActivity.Free;
  _lockTaskCounter.Free;
  _lock.Free;
  inherited Destroy;
end;

function TTaskManager.getClient(const clientInfo: TClientInfo; out
  clientIdx: Integer): TClientNode;
var
  x : Integer;
begin
  result    := nil;
  clientIdx := -1;

  // look for the client which is identified by clientInfo
  for x := 0 to high(_clients) do
    if _clients[x].isIdentifiedBy(clientInfo) then
    begin
      result    := _clients[x];
      clientIdx := x;
      break;
    end;
end;

function TTaskManager.getClient(const clientInfo: TClientInfo): TClientNode;
var
  clientIdx : Integer;
begin
  result := getClient(clientInfo, clientIdx);
end;

function TTaskManager.getOrCreateClient(
  const clientInfo: TClientInfo): TClientNode;
var
  minAvgTime : TDateTime;
  perfInfo : TClientPerformanceInfo;
  x, y : integer;
begin
  result := getClient(clientInfo);

  if result = nil then
  begin
    // get an estimate for the average normalized runtime of the new client
    minAvgTime := SysUtils.MaxDateTime;
    for x := 0 to high(_clients) do
    begin
      perfInfo := _clients[x].getPerformanceInfo();
      for y := 0 to high(perfInfo.optimizations) do
      begin
        if (not perfInfo.optimizations[y].tAvg_InitialValue)
           and (perfInfo.optimizations[y].tAvg < minAvgTime) then
          minAvgTime := perfInfo.optimizations[y].tAvg;
      end;
    end;
    if minAvgTime = SysUtils.MaxDateTime then
      minAvgTime := _settings.initialAvgRunTime;

    // add client to list
    SetLength(_clients, Length(_clients)+1);
    _clients[high(_clients)] := TClientNode.Create(clientInfo, _settings.clientSettings);

    // initialize performance data
    _clients[high(_clients)].initAverageRunTime(minAvgTime);

    // re-distribute (available) tasks
    _fetchAndDistributeTasks;
  end;
end;

procedure TTaskManager._clearOldTasks;
var
  q : TSQLQuery;
begin
  q := _connection.createQuery();
  try

    {// give the optimizer server a chance to take note that the task is not
    // completed in time
    q.SQL.Text := 'UPDATE "'+_settings.sqlTable+'" ';
    q.SQL.Text := q.SQL.Text + 'SET "success" = :SUCCESS, "state" = ''finished'' ';
    q.SQL.Text := q.SQL.Text + 'WHERE "state" != ''free'' ';
    q.SQL.Text := q.SQL.Text + 'AND "startTime" + 5 * "deadline" < now()';
    if _userName <> '' then
      q.SQL.Text :=   q.SQL.Text
                    + ' AND "userName" = :USERNAME';
    if _taskName <> '' then
      q.SQL.Text :=   q.SQL.Text
                    + ' AND "name" = :TASKNAME';
    q.SQL.Text := q.SQL.Text + ';';

    q.ParamByName('success').AsBoolean := false;
    if _userName <> '' then
      q.ParamByName('username').AsString := _userName;
    if _taskName <> '' then
      q.ParamByName('taskname').AsString := _taskName;

    q.ExecSQL;


    // delete the even older tasks as they are assumed to be orphans
    q.SQL.Text := 'DELETE FROM "'+_settings.sqlTable+'" ';
    q.SQL.Text := q.SQL.Text + 'WHERE "state" != ''free'' ';
    q.SQL.Text := q.SQL.Text + 'AND "startTime" + 10 * "deadline" < now()';
    if _userName <> '' then
      q.SQL.Text :=   q.SQL.Text
                    + ' AND "userName" = :USERNAME';
    if _taskName <> '' then
      q.SQL.Text :=   q.SQL.Text
                    + ' AND "name" = :TASKNAME';
    q.SQL.Text := q.SQL.Text + ';';

    if _userName <> '' then
      q.ParamByName('username').AsString := _userName;
    if _taskName <> '' then
      q.ParamByName('taskname').AsString := _taskName;

    q.ExecSQL;}

    // delete orphan tasks
    q.SQL.Text := 'DELETE FROM "'+_settings.sqlTable+'" ';
    q.SQL.Text := q.SQL.Text + 'WHERE "lastPing" + ''00:30:00'' < now()';
    if _userName <> '' then
      q.SQL.Text :=   q.SQL.Text
                    + ' AND "userName" = :USERNAME';
    if _taskName <> '' then
      q.SQL.Text :=   q.SQL.Text
                    + ' AND "name" = :TASKNAME';
    q.SQL.Text := q.SQL.Text + ';';

    if _userName <> '' then
      q.ParamByName('username').AsString := _userName;
    if _taskName <> '' then
      q.ParamByName('taskname').AsString := _taskName;

    q.ExecSQL;

    _connection.commitQuery(q);
  finally
    _connection.finishQuery(q);
  end;
end;

procedure TTaskManager._clearLocalTasks;
var
  x : integer;
begin
  for x := 0 to high(_clients) do
    _clients[x].clearTasks;

  _lockTaskCounter.Beginwrite;
  try
    _tasks := 0;
    _availableTasks := 0;
  finally
    _lockTaskCounter.Endwrite;
  end;
end;

function TTaskManager._clearInactiveClients : TClientInfoArray;
var
  x, y : Integer;
  perfInfo : TClientPerformanceInfo;
begin
  SetLength(result, 0);
  for x := high(_clients) downto 0 do
  begin
    perfInfo := _clients[x].getPerformanceInfo();

    // if client has not shown any activity for too long
    //if perfInfo.tLastTaskActivity + 2 * _maxDeadline < now() then
    if perfInfo.tLastPing + EncodeTime(0, 2, 0, 0) < now() then
    begin
      SetLength(result, Length(result)+1);
      result[high(result)] := _clients[x].getStaticInfo();

      _clients[x].Free;
      for y := x to high(_clients)-1 do
        _clients[y] := _clients[y+1];
      SetLength(_clients, Length(_clients)-1);
    end;
  end;
end;

procedure TTaskManager._createStatistics;
var
  x : integer;
  best, worst, temp : TDateTime;
begin
  _threadsTotal      := 0;
  _maxRuntimeFactor  := 1;
  _performanceFactor := 1;

  worst := 0;
  best  := Math.MaxExtended;
  for x := 0 to high(_clients) do
  begin
    if not _clients[x].getIgnoreState() then
    begin
      _clients[x].updateThreadCount;
      _threadsTotal     := _threadsTotal + _clients[x].getThreadCount();
      _maxRuntimeFactor := Max(_maxRuntimeFactor, _clients[x].getMaxRuntimeFactor());

      temp := _clients[x].getMinExpectedRuntime();

      if (temp < best) and (temp > 0) then
        best := temp;
      if temp > worst then
        worst := temp;
    end;
  end;

  if (best < Math.MaxExtended) and (worst > 0) then
    _performanceFactor := worst / best;

  // above this boundary, even the slowest client would still be helpful.
  // so give it a task unconditionally
  _unconstraintDistributionBoundary :=   _maxRuntimeFactor
                                       * _performanceFactor
                                       * _settings.clientSettings.maxLoadFactor
                                       * _threadsTotal;
end;

procedure TTaskManager.updateActivity;
begin
  _lockActivity.Beginwrite;
  try
    _lastActivity := Now();
  finally
    _lockActivity.Endwrite;
  end;
end;

function TTaskManager.ignoreClient(const clientInfo: TClientInfo; value: boolean
  ) : boolean;
var
  client : TClientNode;
begin
  result := false;
  _lock.Beginread;
  try
    client := getClient(clientInfo);
    if client <> nil then
    begin
      result := true;
      if client.getIgnoreState() <> value then
        client.changeIgnoreState();
    end;
  finally
    _lock.Endread;
  end;
end;

procedure TTaskManager.ping(const clientInfo: TClientInfo);
var
  client : TClientNode;
begin
  _lock.Beginread;
  try
    client := getClient(clientInfo);
    if client = nil then
      exit;

    client.updateLastPing;
  finally
    _lock.Endread;
  end;
end;

function TTaskManager.getTask(const clientInfo: TClientInfo;
  doNotMarkAsActive: boolean; forceAcquisition: boolean): TClientTask;
var
  client : TClientNode;
  x, clientIdx : integer;
  found : boolean;
  q : TSQLQuery;
begin
  //Writeln('getTask (0)...');
  updateActivity;

  //Writeln('getTask (1)...');

  // get a client which can be identified
  _lock.BeginWrite;
  try
    client := getOrCreateClient(clientInfo);
  finally
    _lock.Endwrite;
  end;


  //Writeln('getTask (2)...');
  _lock.BeginRead;
  try
    //Writeln('getTask (3)...');
    // retrieve the index of the client in the list
    client := getClient(clientInfo, clientIdx);
    if client = nil then
      exit;

    if client.getIgnoreState() then
      exit;

    //Writeln('getTask (4)...');
    _lockTaskCounter.Beginwrite;
    try
      //Writeln('getTask (5)...');
      // mark the client as still alive
      client.updateLastTaskActivity;

      // get a task
      found := client.getNextScheduledTask(result, doNotMarkAsActive);

      if found and doNotMarkAsActive then
        exit;

      if not found then
      begin

        // if there are enough tasks available or the client forces acquisition,
        // give it a task assigned to a different client
        if (_availableTasks > _unconstraintDistributionBoundary)
           or (forceAcquisition) then
        begin
          for x := 0 to high(_clients) do
          begin
            if _clients[x].extractNextScheduledTask(result) then
            begin
              found := true;

              result.clientIdx := clientIdx;
              result.clientID  := clientInfo.instanceID;
              result.clientIP  := clientInfo.IP;

              if doNotMarkAsActive then
                exit;

              // mark it as active
              result.computationRunning := true;
              result.start              := now();

              // re-assign it
              client.assignTask(result);

              break;
            end;
          end;
        end;
      end;

      // decrement the number of available tasks
      if found then
        dec(_availableTasks);

      // if the boundary for unconstraint distribution is reached from above,
      // s.t. the next acquisition attempt may need an assignment, re-distribute
      // tasks
      if _availableTasks = _unconstraintDistributionBoundary then
        _reorganizationThread.reorganize();
    finally
      _lockTaskCounter.Endwrite;
    end;
    //Writeln('getTask (6)...');

    // tell the database that the task is already acquired
    if found then
    begin
      q := _connection.createQuery('getTask');
      try
        //Writeln('LOCK TABLE... (getTask)');
        if _settings.useDBLocks then
        begin
          q.SQL.Text := 'LOCK TABLE "'+_settings.sqlTable+'";';

          q.ExecSQL;
        end;
        //Writeln('TABLE LOCKED... (getTask)');

        q.SQL.Text :=   'UPDATE "'+_settings.sqlTable+'" SET '
                      + '"startTime" = CASE WHEN "state" = ''free'' '
                      + '                   THEN (now())'
                      + '                   ELSE ("startTime") '
                      + '              END, '
                      + '"deadline" = CASE WHEN "state" = ''free'' '
                      + '                  THEN ("deadline")'
                      //+ '                  ELSE ("deadline" + (now() - "startTime")) '
                      + '                  ELSE ("deadline" + ''00:05:00'') '
                      + '             END, '
                      + '"deadlineCounter" = CASE WHEN "state" = ''free'' '
                      + '                         THEN (0)'
                      + '                         ELSE ("deadlineCounter" + 1) '
                      + '                    END, '
                      + '"clientIP" = :CLIENTIP, '
                      + '"clientID" = :CLIENTID, '
                      + '"state" = ''running'' '
                      + 'WHERE "ID" = :ID;';

        try
          q.ParamByName('ClientIP').AsString := result.clientIP;
          q.ParamByName('ClientID').AsString := result.clientID;
          q.ParamByName('ID').AsInteger      := StrToIntDef(result.ID, 0);

          q.ExecSQL;

          _connection.commitQuery(q);
        except on e : exception do
        begin
          Writeln(q.SQL.Text);
          Writeln(e.Message);
        end
        end;
      finally
        _connection.finishQuery(q);
        //Writeln('QUERY finished... (getTask)');
      end;
    end;
  finally
    _lock.EndRead;
  end;
  //Writeln('getTask (7)...');
end;

function TTaskManager.submitResult(task: TClientTask;
  const clientInfo: TClientInfo) : string;
var
  q : TSQLQuery;
  failureCounter, deadlineCounter, x : Integer;
  computationTime : TDateTime;
  runTimeFactor : Cardinal;
  client : TClientNode;
  taskNew : TClientTask;
  dbEntryFound : boolean;
  optimizationID : string;
  tempParam : TParam;
begin
  result := '';
  updateActivity;

  // unsuccessful task
  if not task.success then
  begin
    // initialization which avoids any further action in case the database
    // query fails
    failureCounter := _settings.maxFailureCounter + 1;

    _lock.Beginwrite;
    try

      // first of all, read the failure counter of the current task from the
      // database
      q := _connection.createQuery('submitResult negative');
      try
        if _settings.useDBLocks then
        begin
          q.SQL.Text := 'LOCK TABLE "'+_settings.sqlTable+'";';
          q.ExecSQL;
        end;

        q.SQL.Text := 'SELECT "failureCounter" FROM "' + _settings.sqlTable + '" WHERE "ID" = :ID;';
        q.ParamByName('ID').AsInteger := StrToIntDef(task.ID, 0);

        try
          q.Open;
          if not q.EOF then
            failureCounter := q.FieldByName('failureCounter').AsInteger;
          q.Close;
        except on e: exception do
          Writeln('Exception: ', e.Message);
        end;


        // if the failure counter is low enough, give it another try
        if failureCounter < _settings.maxFailureCounter then
        begin
          q.SQL.Text :=   'UPDATE "'+_settings.sqlTable+'" SET "state" = ''free'', '
                        + '"failureCounter" = "failureCounter" + 1 '
                        + 'WHERE "ID" = :ID;';
          q.ParamByName('ID').AsInteger := StrToIntDef(task.ID, 0);

          q.ExecSQL;

          _connection.commitQuery(q);

          client := getClient(clientInfo);
          if client <> nil then
          begin

            // re-assign the task among all clients
            if client.extractTaskByID(taskNew, task.ID) then
            begin
              taskNew.computationRunning := false;

              _assignTask(taskNew);
            end;
          end;

          result := '1';

          exit;
        end;

        _connection.commitQuery(q);
      finally
        _connection.finishQuery(q);
      end;

    finally
      _lock.Endwrite;
    end;
  end;


  // successful task:
  // tell the database, s.t. the results may be fetched by the optimizer
  _lock.BeginRead;
  try
    q := _connection.createQuery('submitResult');
    try
      if _settings.useDBLocks then
      begin
        q.SQL.Text := 'LOCK TABLE "'+_settings.sqlTable+'";';
        q.ExecSQL;
      end;

      q.SQL.Text :=   'UPDATE "'+_settings.sqlTable+'" SET '
                    + '"state" = ''finished'', '
                    + '"success" = :SUCCESS, '
                    + '"errorValue" = :ERRORVALUE, ';
      for x := 0 to high(task.errorValues) do
      begin
        q.SQL.Text := q.SQL.Text
                      + '"errorName'+IntToStr(x) + '" = :ERRORNAME'+ IntToStr(x)+', '
                      + '"errorValue'+IntToStr(x) + '" = :ERRORVALUE'+ IntToStr(x)+', ';
      end;
      q.SQL.Text := q.SQL.Text
                    + '"output" = :OUTPUT, '
                    + '"finishTime" = now() '
                    + 'WHERE "ID" = :ID '
                    + 'RETURNING "finishTime" - "startTime" AS "computationTime", '
                    + '"runTimeFactor", "failureCounter", "deadlineCounter", "optimizationID";';
      q.ParamByName('success').AsBoolean   := task.success;
      q.ParamByName('errorValue').AsString := task.errorValue;
      for x := 0 to high(task.errorValues) do
      begin
        tempParam := q.Params.FindParam('errorName'+IntToStr(x));
        if tempParam <> nil then
        begin
          tempParam.AsString := task.errorNames[x];
          q.Params.FindParam('errorValue'+IntToStr(x)).AsString := task.errorValues[x];
        end;
      end;
      q.ParamByName('output').AsString     := task.output;
      q.ParamByName('ID').AsInteger        := StrToIntDef(task.ID, 0);

      dbEntryFound := false;
      q.Open;
      if not q.EOF then
      begin
        dbEntryFound := true;

        computationTime       := q.FieldByName('computationTime').AsDateTime;
        runTimeFactor         := q.FieldByName('runTimeFactor').AsInteger;
        failureCounter        := q.FieldByName('failureCounter').AsInteger;
        deadlineCounter       := q.FieldByName('deadlineCounter').AsInteger;
        optimizationID        := q.FieldByName('optimizationID').AsString;
      end;
      q.Close;

      _connection.commitQuery(q);
    finally
      _connection.finishQuery(q);
    end;

    // the total amount of tasks decreases
    _lockTaskCounter.BeginWrite;
    try
      if dbEntryFound then
        dec(_tasks);
    finally
      _lockTaskCounter.EndWrite;
    end;

    result := '1';

    client := getClient(clientInfo);
    if client <> nil then
    begin
      task.optimizationID  := optimizationID;
      task.runTimeFactor   := runTimeFactor;
      task.failureCounter  := failureCounter;
      task.deadlineCounter := deadlineCounter;

      // delete the task from local lists
      client.deleteTask(task);

      // update performance information of the current client
      if failureCounter = 0 then
        client.updatePerformanceInfo(task, computationTime);
    end;
  finally
    _lock.EndRead;
  end;
end;

function TTaskManager._fetchAndDistributeTasks: Integer;
var
  q : TSQLQuery;
  task : TClientTask;
  tmpDeadline, nowDB, DBTimeDelta : TDateTime;
  deletedClients : TClientInfoArray;
  activeOptimizationIDs : TStringDynArray;
  x, y, taskCounter : integer;
  found : boolean;
  tStart, tFinish : TDateTime;
begin
  DBTimeDelta := 0;
  tStart := Now();

  try
    // first of all, clear everything possible
    _clearLocalTasks;
    _clearOldTasks;
    deletedClients := _clearInactiveClients;

    try
      // mark reserved tasks of deleted clients as "free"
      if Length(deletedClients) > 0 then
      begin
        q := _connection.createQuery('freeOrphans');
        try
          if _settings.useDBLocks then
          begin
            q.SQL.Text := 'LOCK TABLE "'+_settings.sqlTable+'";';
            q.ExecSQL;
          end;

          try
            for x := 0 to high(deletedClients) do
            begin
              q.SQL.Text :=   'UPDATE "'+_settings.sqlTable+'" SET '
                              + '"state" = ''free'' '
                              + 'WHERE  "clientIP" = :CLIENTIP AND '
                              + '"clientID" = :CLIENTID;';

              q.ParamByName('ClientIP').AsString := deletedClients[x].IP;
              q.ParamByName('ClientID').AsString := deletedClients[x].instanceID;

              q.ExecSQL;
            end;

            _connection.commitQuery(q);
          except on e : exception do
          begin
            Writeln(q.SQL.Text);
            Writeln(e.Message);
          end
          end;
        finally
          _connection.finishQuery(q);
        end;
      end;

      // compute the difference between database time and application time
      q := _connection.createQuery('fetchNow');
      try
        q.SQL.Text := 'SELECT Now() AS "currentTime" FROM "' + _settings.sqlTable + '";';

        q.Open;
        if not q.EOF then
        begin
          nowDB := q.FieldByName('currentTime').AsDateTime;
          DBTimeDelta := Now() - nowDB;
        end;
        q.Close;

        _connection.commitQuery(q);
      finally
        _connection.finishQuery(q);
      end;

      // fetch all tasks
      q := _connection.createQuery('fetchTasks');
      try
        //Writeln('LOCK TABLE... (fetchTasks)');
        if _settings.useDBLocks then
        begin
          q.SQL.Text := 'LOCK TABLE "'+_settings.sqlTable+'";';
          q.ExecSQL;
        end;
        //Writeln('TABLE LOCKED... (fetchTasks)');

        q.SQL.Text :=   'SELECT *, ("state" = ''free'' OR "startTime" + "deadline" < now()) AS "available" FROM "' + _settings.sqlTable + '" '
                      + 'WHERE "state" != ''finished''';
        if _userName <> '' then
          q.SQL.Text :=   q.SQL.Text
                        + ' AND "userName" = :USERNAME';
        if _taskName <> '' then
          q.SQL.Text :=   q.SQL.Text
                        + ' AND "name" = :TASKNAME';

        // order by state is important because we want to assign tasks that are
        // currently computed BEFORE we distribute the available tasks.
        // Otherwise, the estimation of finish times while assigning goes wrong
        q.SQL.Text := q.SQL.Text + 'ORDER BY "state" DESC, "priority" DESC, "ID" ASC;';

        if _userName <> '' then
          q.ParamByName('userName').AsString := _userName;
        if _taskName <> '' then
          q.ParamByName('taskName').AsString  := _taskName;



        q.Open;
        while not q.EOF do
        begin
          // gather information about the current task
          task.ID := q.FieldByName('ID').AsString;
          task.optimizationID := q.FieldByName('optimizationID').AsString;

          task.computationRunning := not q.FieldByName('available').AsBoolean;
          if not task.computationRunning then
          begin
            task.clientID   := '';
            task.clientIP   := '';
          end
          else
          begin
            task.clientIP   := q.FieldByName('clientIP').AsString;
            task.clientID   := q.FieldByName('clientID').AsString;
          end;
          task.clientIdx  := -1;
          task.threadIdx  := -1;

          task.errorValue := '';
          task.success    := false;
          task.output     := '';
          task.start      := 0;
          task.finish     := 0;

          task.start            := q.FieldByName('startTime').AsDateTime + DBTimeDelta;
          task.runTimeFactor    := q.FieldByName('runTimeFactor').AsFloat;
          task.cmdLine          := q.FieldByName('cmdLine').AsString;
          task.workingDirectory := q.FieldByName('workingDirectory').AsString;
          task.evaluationBinary := q.FieldByName('evaluationBinary').AsString;
          task.deadline         := q.FieldByName('deadline').AsDateTime;
          task.errorName        := q.FieldByName('errorName').AsString;
          task.failureCounter   := q.FieldByName('failureCounter').AsInteger;

          // update the maximal deadline
          // (used to compute a boundary of inactivity after which a client
          //  will be deleted from the list)
          tmpDeadline          := q.FieldByName('deadline').AsDateTime;
          if tmpDeadline > _maxDeadline then
            _maxDeadline := tmpDeadline;

          // assign the task to an appropriate client
          _assignTask(task);

          q.Next;
        end;
        q.Close;

        _connection.commitQuery(q);
      finally
        _connection.finishQuery(q);
      end;

      activeOptimizationIDs := nil;

      q := _connection.createQuery('activeOptimizations');
      try
        q.SQL.Text :=   'SELECT DISTINCT "optimizationID" FROM "' + _settings.sqlTable + '" '
                      + 'WHERE "state" != ''finished''';
        if _userName <> '' then
          q.SQL.Text :=   q.SQL.Text
                        + ' AND "userName" = :USERNAME';
        if _taskName <> '' then
          q.SQL.Text :=   q.SQL.Text
                        + ' AND "name" = :TASKNAME';

        if _userName <> '' then
          q.ParamByName('userName').AsString := _userName;
        if _taskName <> '' then
          q.ParamByName('taskName').AsString  := _taskName;

        q.Open;
        while not q.EOF do
        begin
          x := Length(activeOptimizationIDs);
          SetLength(activeOptimizationIDs, x+1);
          activeOptimizationIDs[x] := q.FieldByName('optimizationID').AsString;

          q.Next;
        end;
        q.Close;

        _connection.commitQuery(q);
      finally
        _connection.finishQuery(q);
      end;
      //Writeln('QUERY FINISHED ... (fetchTasks)');
    except on e: exception do
      _connection.Reconnect(e.Message);
    end;

    // prevent deletion of statistics for all active optimizations
    for x := 0 to high(activeOptimizationIDs) do
      for y := 0 to high(_clients) do
        _clients[y].refreshOptimization(activeOptimizationIDs[x]);

    // create some statistics
    _createStatistics;
  except on e: exception do
    Writeln(e.Message);
  end;

  result := _tasks;

  tFinish := Now();
  //Writeln('Time for task distribution: ', (tFinish - tStart) * 86400, ' seconds');
end;

procedure TTaskManager.clearTasks;
var
  q : TSQLQuery;
begin
  updateActivity;

  _lock.BeginWrite;
  try
    _clearLocalTasks;

    q := _connection.createQuery('clear tasks');
    try
      if _settings.useDBLocks then
      begin
        q.SQL.Text := 'LOCK TABLE "'+_settings.sqlTable+'";';
        q.ExecSQL;
      end;

      q.SQL.Text :=   'DELETE FROM "'+_settings.sqlTable+'" WHERE 1 = 1';
      if _userName <> '' then
        q.SQL.Text :=   q.SQL.Text
                      + ' AND "userName" = :USERNAME';
      if _taskName <> '' then
        q.SQL.Text :=   q.SQL.Text
                      + ' AND "name" = :TASKNAME';
      q.SQL.Text := q.SQL.Text + ';';



      if _userName <> '' then
        q.ParamByName('userName').AsString := _userName;
      if _taskName <> '' then
        q.ParamByName('taskName').AsString  := _taskName;

      q.ExecSQL;

      _connection.commitQuery(q);
    finally
      _connection.finishQuery(q);
    end;
  finally
    _lock.EndWrite;
  end;
end;

function TTaskManager.fetchAndDistributeTasks: Integer;
begin
  _lock.BeginWrite;
  try
    result := _fetchAndDistributeTasks;
  finally
    _lock.EndWrite;
  end;
end;

procedure TTaskManager._assignTask(const task: TClientTask);
var
  x : integer;
  minFinishTime, temp : TDateTime;
  tmpThreadIdx, clientIdx, threadIdx : Integer;
  task2 : TClientTask;
begin

  // if the task is still available
  if not task.computationRunning then
  begin
    minFinishTime := SysUtils.MaxDateTime;
    clientIdx := -1;

    task2 := task;
    SetLength(task2.finishTimes, Length(_clients));
    SetLength(task2.clientIDs, Length(_clients));

    // in order to save time do only assign a limited number of tasks
    if (not _settings.restrictedAssignments) or (_tasks < _unconstraintDistributionBoundary) then
    begin

      // find the client which would have the least finish time including the
      // current task
      for x := 0 to high(_clients) do
      begin
        if _clients[x].getIgnoreState() then
          continue;

        temp := _clients[x].estimateFinishTime(task, tmpThreadIdx);
        task2.finishTimes[x] := temp;
        task2.clientIDs[x] := _clients[x].getStaticInfo().IP + '(' + _clients[x].getStaticInfo().instanceID + ')';
        if temp < minFinishTime then
        begin
          clientIdx := x;
          threadIdx := tmpThreadIdx;
          minFinishTime := temp;
        end;
      end;
    end;

    // if an appropriate client has been found
    if clientIdx <> -1 then
    begin

      // in order to save time do only assign a limited number of tasks
      if (not _settings.restrictedAssignments) or (_tasks < _unconstraintDistributionBoundary) then
      begin
        // assign the task to the client
        _clients[clientIdx].assignTask(task2, threadIdx);
      end;


      // update task statistics
      _lockTaskCounter.Beginwrite;
      try
        inc(_tasks);
        inc(_availableTasks);
      finally
        _lockTaskCounter.Endwrite;
      end;
    end;
  end

  // if the task is active (and thus not available)
  else
  begin
    for x := 0 to high(_clients) do
    begin
      if (_clients[x].getIP() = task.clientIP)
         and (_clients[x].getID() = task.clientID) then
      begin
        // assign the task to the client
        _clients[x].assignTask(task);

        // update task statistics
        _lockTaskCounter.Beginwrite;
        try
          inc(_tasks);
        finally
          _lockTaskCounter.Endwrite;
        end;

        break;
      end;
    end;
  end;
end;

function TTaskManager.getStatisticsState: string;
begin
  result := '';
  _lock.Beginread;
  try
    _lockTaskCounter.Beginread;
    try
      //Writeln('getState (3)...');
      result := result + '<b>Tasks (total/available): </b>' + IntToStr(_tasks)
                       + ' (' + IntToStr(_availableTasks) + ')'
                       + '<br>'#13#10;
    finally
      _lockTaskCounter.Endread;
    end;
    //Writeln('getState (4)...');
    result := result + '<b>Threads: </b>' + IntToStr(_threadsTotal) + '<br>'#13#10;
    result := result + '<b>Adaptiveness Boundary (Tasks): </b>' + IntToStr(Ceil(_unconstraintDistributionBoundary)) + '<br>'#13#10;
    result := result + '<b>Speed Factor: </b>' + FloatToStr(_performanceFactor) + '<br>'#13#10;
    result := result + '<b>Max Runtime Factor: </b>' + FloatToStr(_maxRuntimeFactor) + '<br>'#13#10;
    //result := result + '<b>Max Load Factor: </b>' + FloatToStr(_settings.clientSettings.maxLoadFactor) + '<br>'#13#10;
  finally
    _lock.Endread;
  end;
end;

function TTaskManager.getState: string;
var
  x, y, z : integer;
  threads : TClientThreadList;
  perfInfo : TClientPerformanceInfo;
  clientInfo : TClientInfo;
  tasks : TClientTaskList;
  threadFinishTime : TDateTime;
  taskTimes : TDateTimeDynArray;
  activeThreads : TDoubleDynArray;
  HTMLID : string;
begin
  //Writeln('getState (1)...');
  _lock.BeginRead;
  try
    //Writeln('getState (2)...');
    result := '<table cellspacing="5">'#13#10;
    result := result + '<tr><th>Hosts</th></tr>'#13#10;
    result := result + '<td valign="top">';
    for x := 0 to high(_clients) do
    begin
      threads := _clients[x].cloneThreads();
      perfInfo := _clients[x].getPerformanceInfo();
      clientInfo := _clients[x].getStaticInfo();

      if _clients[x].getIgnoreState() then
        result := result + '<span style="color: red;"><b>Ignored</b> ';
      result := result + 'Host: <b>' + _clients[x].getHost() + '</b> (';
      result := result + _clients[x].getID() + ', ' + _clients[x].getIP() + ') (n<sub>MaxThreads</sub> = <b>';
      result := result + IntToStr(perfInfo.nMaxThreads) + '</b>';
      //result := result + ', n<sub>FastThreads</sub> = <b>';
      //result := result + IntToStr(perfInfo.nFastThreads) + '</b>';
      result := result + ') ';
      if _clients[x].getIgnoreState() then
        result := result + '</span>';

      HTMLID := 'client'+IntToStr(x);
      if _clients[x].getIgnoreState() then
        result := result + '<a id="'+HTMLID+'"><a href="ignoreClient?userName='+_userName+'&taskName='+_taskName+'&clientID='+clientInfo.instanceID+'&clientIP='+clientInfo.IP+'&value=0#'+HTMLID+'">Resume Distribution</a></a>'
      else
        result := result + '<a id="'+HTMLID+'"><a href="ignoreClient?userName='+_userName+'&taskName='+_taskName+'&clientID='+clientInfo.instanceID+'&clientIP='+clientInfo.IP+'&value=1#'+HTMLID+'">STOP Distribution</a></a>';
      result := result + '<br><br>'#13#10;

      result := result + '<div style="background: #FFAAAA;">'#13#10;
      result := result + '<table cellspacing="10">'#13#10;

      result := result + '<tr>';
      result := result + '<td><b>t</b><sub>avg,min</sub></td><td><span style="color: blue;">';
      result := result + formatTime(_clients[x].getMinAverageTime());
      result := result + '</span></td>';
      result := result + '<td><b>t</b><sub>busy</sub></td><td><span style="color: blue;">';
      result := result + formatTime(perfInfo.tLastTaskActivity - Now());
      result := result + '</span></td>';
      result := result + '<td><b>t</b><sub>eff,min</sub></td><td><span style="color: blue;">';
      result := result + formatTime(_clients[x].getMinExpectedRuntime());
      result := result + '</span></td>';
      result := result + '<td><b>t</b><sub>finish</sub></td><td><span style="color: blue;">';
      result := result + formatTime(_clients[x].estimateFinishTime());
      result := result + '</span></td>';
      result := result + '<td><b>t</b><sub>silence</sub></td><td><span style="color: blue;">';
      result := result + formatTime(perfInfo.tLastPing - Now());
      result := result + '</span></td>';
      result := result + '</tr>';

      for y := 0 to high(perfInfo.optimizations) do
      begin
        result := result + '<tr>';
        result := result + '<td><b>Optimization ' + IntToStr(y+1) + '</b></td>';
        result := result + '<td>(' + perfInfo.optimizations[y].optimizationID + ')</td>';
        result := result + '<td><b>t</b><sub>avg</sub></td><td><span style="color: blue;">';
        result := result + formatTime(perfInfo.optimizations[y].tAvg);
        result := result + '</span></td>';
        result := result + '<td></td>';
        result := result + '<td></td>';
        result := result + '</tr>';
      end;

      result := result + '</table>'#13#10;
      result := result + '</div>'#13#10;

      result := result + '<br><br>'#13#10;

      result := result + '<div style="background: #EEEEEE;">'#13#10;
      result := result + '<span style="color: navy; font-weight: bold;">Threads:</span><br><br>'#13#10;
      for y := 0 to high(threads) do
      begin
        tasks := threads[y].getTaskList();
        threadFinishTime := threads[y].estimateFinishTime(taskTimes, activeThreads);

        result := result + '<b>'+IntToStr(y)+' (';
        result := result + formatTime(threads[y].getStartTime()) + ' - ';
        result := result + formatTime(threadFinishTime) + ' - ';
        result := result + formatTime(threadFinishTime - Now());
        result := result + ')</b>: ';

        for z := 0 to high(tasks) do
        begin
          if z <> 0 then
            result := result + ', ';
          if tasks[z].computationRunning then
            result := result + '<b><span style="color: red;">'
          else
            result := result + '<a href="showTaskState?taskID='+tasks[z].ID+'">';
          result := result + formatTime(taskTimes[z]); // + ' (<b>' + tasks[z].ID + '</b>)';
          //result := result + ' (' + FloatToStr(activeThreads[z]) + ')';
          if tasks[z].computationRunning then
            result := result + '</span></b>'
          else
            result := result + '</a>';
        end;
        result := result + '<br>'#13#10;
      end;

      for y := 0 to high(threads) do
      begin
        threads[y].Free;
      end;
      result := result + '</div>';
      result := result + '<br><br>'#13#10;
    end;
    result := result + '</td></tr>';
    result := result + '</table>';
  finally
    _lock.EndRead;
  end;
  //Writeln('getState (5)...');
end;

function TTaskManager.getThreadCount: Integer;
begin
  _lock.Beginread;
  try
    result := _threadsTotal;
  finally
    _lock.Endread;
  end;
end;

function TTaskManager.showTaskState(const task: TClientTask;
  const clientInfo: TClientInfo): string;
var
  tempTask : TClientTask;
  x, y : Integer;
begin
  result := '--- Task Times --- <br>'#13#10;
  _lock.Beginread;
  try
    for x := 0 to high(_clients) do
      if _clients[x].getTaskByID(tempTask, task.ID) then
      begin
        result := result + 'Optimization-ID: ' + tempTask.optimizationID + '<br>'#13#10;
        result := result + '(' + IntToStr(Length(tempTask.finishTimes)) + ')<br>'#13#10;
        for y := 0 to high(tempTask.finishTimes) do
        begin
          result := result + formatTime(tempTask.finishTimes[y]) + '<br>'#13#10;
        end;
      end;
  finally
    _lock.Endread;
  end;
end;

function TTaskManager.getSleepTime: Cardinal;
begin
  _lock.Beginread;
  try
    result := max(Length(_clients) div 10, 1);
  finally
    _lock.Endread;
  end;
end;

function TTaskManager.getUserName: string;
begin
  result := _userName;
end;

function TTaskManager.getTaskName: string;
begin
  result := _taskName;
end;

function TTaskManager.getLastActivity: TDateTime;
begin
  _lockActivity.Beginread;
  try
    result := _lastActivity;
  finally
    _lockActivity.Endread;
  end;
end;

end.

