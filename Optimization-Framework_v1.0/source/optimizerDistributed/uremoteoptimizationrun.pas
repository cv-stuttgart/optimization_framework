unit uRemoteOptimizationRun;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, DB, SQLDB, PQConnection,

  uDatabase, uDatatypesOptimizer, uOptimizationRun, uEvaluationTask,
  uEvaluationBinary;

type
  // general settings of the task which are to be inserted into DB
  TRemoteTaskSettings = record
    name, userName : string;
    taskType : string;
    errorValueName : string;
    evaluationBinary : string;
    runTimeFactor : Single;
    workingDirectory : string;
    optimizationID : string;
    runNo : Integer;
    deadline : TDateTime;
    priority : Single;
    useDBLocks : boolean;
  end;

  TRemoteOptimizationRun = class;

  { TRemoteEvaluationTask }

  TRemoteEvaluationTask = class(TEvaluationTaskBase)
  protected
    // connection to the database
    _connection : TDBConnection;

    // reference to the source of all tasks
    _source : TRemoteOptimizationRun;

    // name of the SQL table
    _sqlTable : string;

    // how often did the evaluation fail?
    _failureCounter : Cardinal;

    // settings which are important for the task
    _remoteTaskSettings : TRemoteTaskSettings;

    // ID in the database
    _rowID : string;

    // indices of this task in the matrix of all current tasks
    _originalIdxX, _originalIdxY : Integer;
  public
    constructor Create(const cmdLineParameters : string;
      chosenParameters : TChosenParameterSet;
      imageSequenceData : TImageSequenceData;
      connection : TDBConnection;
      source : TRemoteOptimizationRun;
      evaluationBinaryClass : TEvaluationBinaryClass;
      const sqlTable : string;
      remoteTaskSettings : TRemoteTaskSettings;
      originalIdxX,
      originalIdxY: Integer);

    // inserts the task into the DB
    procedure putOnlineTrans(SQLTransaction : TSQLTransaction);

    // setters which are useful after a task has finished
    procedure addErrorValue(const name : string; value : extended);
    procedure setSuccessful(value : boolean);
    procedure setFailureCounter(value : Cardinal);
    procedure setFinished(value : boolean);
    procedure setOutput(const value : string);

    // returns the failure counter
    function getFailureCounter() : Cardinal;
  end;
  TRemoteEvaluationTaskList = array of TRemoteEvaluationTask;
  TRemoteEvaluationTaskMatrix = array of TRemoteEvaluationTaskList;

  { TRemoteOptimizationRun }

  TRemoteOptimizationRun = class(TBasicOptimizationRun)
  protected
    _stopInserting : boolean;
    _connection : TDBConnection;
    _sqlTable : string;
    _remoteTaskSettings : TRemoteTaskSettings;
    _tempRuns : TRemoteEvaluationTaskList;

    procedure unsafeCheckFinished(var tempRuns : TRemoteEvaluationTaskList);
    procedure checkFinished(var tempRuns : TRemoteEvaluationTaskList);
    procedure postKeepAlive;
  public
    constructor Create(const cmdLineParameters : string;
      chosenParametersList : TChosenParameterSetList;
      imageSequences : TImageSequenceDataList;
      const environment : TOptimizationEnvironment); override;
    destructor Destroy; override;
    function go(resultMatrix: TChosenParameterSetAndResultList;
      runNo : Cardinal): boolean; override;
    procedure interruptTasks; override;

    procedure setRemoteTaskSettings(remoteTaskSettings : TRemoteTaskSettings);
    procedure openConnection(settings : TDBSettings);
  end;

  { TRemoteOptimizationRunFactory }

  TRemoteOptimizationRunFactory = class(TOptimizationRunFactory)
  public
    _remoteTaskSettings : TRemoteTaskSettings;
    _dbSettings : TDBSettings;
    function createInstance(const cmdLineParameters : string;
      chosenParametersList : TChosenParameterSetList;
      imageSequences : TImageSequenceDataList;
      const environment : TOptimizationEnvironment) : TBasicOptimizationRun; override;
  end;

implementation

uses
  uSimpleLog;

{ TRemoteOptimizationRunFactory }

function TRemoteOptimizationRunFactory.createInstance(
  const cmdLineParameters: string;
  chosenParametersList: TChosenParameterSetList;
  imageSequences: TImageSequenceDataList;
  const environment: TOptimizationEnvironment): TBasicOptimizationRun;
begin
  Result := TRemoteOptimizationRun.Create( cmdLineParameters, chosenParametersList,
                                           imageSequences,environment);
  TRemoteOptimizationRun(Result).setRemoteTaskSettings(_remoteTaskSettings);
  TRemoteOptimizationRun(Result).openConnection(_dbSettings);
end;

{ TRemoteEvaluationTask }

constructor TRemoteEvaluationTask.Create(const cmdLineParameters: string;
  chosenParameters: TChosenParameterSet; imageSequenceData: TImageSequenceData;
  connection: TDBConnection; source: TRemoteOptimizationRun;
  evaluationBinaryClass: TEvaluationBinaryClass; const sqlTable: string;
  remoteTaskSettings: TRemoteTaskSettings; originalIdxX, originalIdxY: Integer);
begin
  inherited Create( cmdLineParameters, chosenParameters, imageSequenceData,
                    evaluationBinaryClass);

  _connection := connection;
  _source     := source;
  _sqlTable   := sqlTable;
  _remoteTaskSettings := remoteTaskSettings;
  _originalIdxX := originalIdxX;
  _originalIdxY := originalIdxY;
end;

procedure TRemoteEvaluationTask.putOnlineTrans(SQLTransaction: TSQLTransaction);
var
  SQLQuery : TSQLQuery;
begin
  // save the final cmd line arguments for the case when they have to be printed
  _finalCmdLine := _evaluationBinaryClass.getCmdLineParameters(_chosenParameters, _cmdLineParameters, _imageSequenceData);

  SQLQuery := TSQLQuery.Create(nil);
  try
    SQLQuery.Transaction := SQLTransaction;

    SQLQuery.SQL.Add( 'INSERT INTO "' + _sqlTable + '"'
                      + ' ("name", "userName", "type", "runTimeFactor", "run", "cmdLine", "workingDirectory", "startTime", "deadline", "errorName", "optimizationID", "priority", "evaluationBinary", "lastPing")'
                      + ' VALUES'
                      + ' (:NAME, :USERNAME, :TYPE, :RUNTIMEFACTOR, :RUN, :CMDLINE, :WORKINGDIR, now(), :DEADLINE, :ERRORNAME, :OPTIMIZATIONID, :PRIORITY, :EVALUATIONBINARY, now())'
                      + ' returning "ID";');
    SQLQuery.Params.ParamByName('Name').AsString := _remoteTaskSettings.name;
    SQLQuery.Params.ParamByName('Username').AsString := _remoteTaskSettings.userName;
    SQLQuery.Params.ParamByName('Type').AsString := _remoteTaskSettings.taskType;
    SQLQuery.Params.ParamByName('RunTimeFactor').AsFloat := _remoteTaskSettings.runTimeFactor;
    SQLQuery.Params.ParamByName('Run').AsInteger := _remoteTaskSettings.runNo;
    SQLQuery.Params.ParamByName('Cmdline').AsString := _finalCmdLine;
    SQLQuery.Params.ParamByName('WorkingDir').AsString := _remoteTaskSettings.workingDirectory;
    SQLQuery.Params.ParamByName('Deadline').AsDateTime := _remoteTaskSettings.deadline;
    SQLQuery.Params.ParamByName('ErrorName').AsString := _remoteTaskSettings.errorValueName;
    SQLQuery.Params.ParamByName('OptimizationID').AsString := _remoteTaskSettings.optimizationID;
    SQLQuery.Params.ParamByName('Priority').AsFloat := _remoteTaskSettings.priority;
    SQLQuery.Params.ParamByName('EvaluationBinary').AsString := _remoteTaskSettings.evaluationBinary;

    SQLQuery.Open;
    self._rowID := SQLQuery.FieldByName('ID').AsString;
    //Writeln('Set _rowID = ' + self._rowID);
    SQLQuery.Close;
  finally
    SQLQuery.Free;
  end;
end;

procedure TRemoteEvaluationTask.addErrorValue(const name: string; value: extended
  );
begin
  SetLength(_errorValues, Length(_errorValues)+1);
  _errorValues[high(_errorValues)].name  := name;
  _errorValues[high(_errorValues)].value := value;
end;

procedure TRemoteEvaluationTask.setSuccessful(value: boolean);
begin
  _successful := value;
end;

procedure TRemoteEvaluationTask.setFailureCounter(value: Cardinal);
begin
  _failureCounter := value;
end;

procedure TRemoteEvaluationTask.setFinished(value: boolean);
begin
  _finished := value;
end;

procedure TRemoteEvaluationTask.setOutput(const value: string);
begin
  _output := value;
end;

function TRemoteEvaluationTask.getFailureCounter: Cardinal;
begin
  result := _failureCounter;
end;

{ TRemoteOptimizationRun }

procedure TRemoteOptimizationRun.unsafeCheckFinished(
  var tempRuns: TRemoteEvaluationTaskList);
var
  SQLQuery : TSQLQuery;
  DeleteSQLQuery : TSQLQuery;
  x, y : integer;
  currID, errorName, tmpErrorName : string;
  startTime, finishTime : TDateTime;
begin
  SQLQuery := _connection.createQuery();
  try
    if _remoteTaskSettings.useDBLocks then
    begin
      SQLQuery.SQL.Text := 'LOCK TABLE "' + _sqlTable + '";';
      SQLQuery.ExecSQL;
    end;

    // look for all tasks whose state is 'finished'
    SQLQuery.SQL.Text := 'SELECT "ID", "state", "output", "clientIP", "startTime",'
                         + ' "finishTime", "success", "failureCounter", "errorName", "errorValue",'
                         + ' "errorName0", "errorValue0", "errorName1", "errorValue1", "errorName2", "errorValue2"'
                         + ' FROM "' + _sqlTable + '"'
    //                     + ' WHERE "name" = :NAME AND "state" = ''finished'';';
                         + ' WHERE "optimizationID" = :OPTIMIZATIONID AND "state" = ''finished'';';
    //SQLQuery.Params.ParamByName('Name').AsString := _remoteTaskSettings.name;
    SQLQuery.Params.ParamByName('OptimizationID').AsString := _remoteTaskSettings.optimizationID;

    // "DeleteSQLQuery" is used to delete a task from the DB after its state
    // has been read
    DeleteSQLQuery := TSQLQuery.Create(nil);
    try
      DeleteSQLQuery.Transaction := SQLQuery.Transaction;

      SQLQuery.Open;
      while not SQLQuery.EOF do
      begin
        currID := SQLQuery.FieldByName('ID').AsString;
        for x := 0 to high(tempRuns) do
        begin

          // the current run has finished
          if tempRuns[x]._rowID = currID then
          begin
            // read error data, output, success state, failureCounter etc.
            errorName := SQLQuery.FieldByName('errorName').AsString;
            if trim(SQLQuery.FieldByName('errorValue').AsString) <> '' then
              tempRuns[x].addErrorValue(errorName, SQLQuery.FieldByName('errorValue').AsFloat)
            else
              tempRuns[x].addErrorValue(errorName, 0);

            //Writeln('Read additional errors...');
            for y := 0 to 2 do
            begin
              tmpErrorName := SQLQuery.FieldByName('errorName'+IntToStr(y)).AsString;
              //Writeln('Error "', tmpErrorName, '"');
              if (tmpErrorName <> '') and (errorName <> tmpErrorName) then
              begin
                tempRuns[x].addErrorValue(tmpErrorName, SQLQuery.FieldByName('errorValue'+IntToStr(y)).AsFloat);
                //Writeln(SQLQuery.FieldByName('errorValue'+IntToStr(y)).AsFloat);
              end;
            end;

            tempRuns[x].setSuccessful(SQLQuery.FieldByName('success').AsInteger <> 0);
            tempRuns[x].setFailureCounter(SQLQuery.FieldByName('failureCounter').AsInteger);
            tempRuns[x].setFinished(true);
            tempRuns[x].setOutput(SQLQuery.FieldByName('output').AsString);

            // compute the run time
            startTime  := SQLQuery.FieldByName('startTime').AsDateTime;
            finishTime := SQLQuery.FieldByName('finishTime').AsDateTime;

            WriteAndLogLn( 'Index: (' + Format('%4.4d, %4.4d', [tempRuns[x]._originalIdxX, tempRuns[x]._originalIdxY]) + ')'
                           + ', Time: '
                           + TEvaluationThread.formatTime(finishTime - startTime)
                           + ', IP: ' + PadRight(SQLQuery.FieldByName('clientIP').AsString, 15)
                           + ', Now: ' + TEvaluationThread.formatDayTime(now()) );

            // delete the task from the database
            DeleteSQLQuery.SQL.Add( 'DELETE FROM "' + _sqlTable + '"'
                                    + ' WHERE "ID" = ' + currID + ';');

            // delete run from temporary list
            tempRuns[x] := tempRuns[high(tempRuns)];
            SetLength(tempRuns, Length(tempRuns)-1);

            break;
          end;
        end;
        SQLQuery.Next;
      end;
      SQLQuery.Close;

      // execute delete statements
      if DeleteSQLQuery.SQL.Count > 0 then
      begin
        DeleteSQLQuery.ExecSQL;
      end;
    finally
      DeleteSQLQuery.Free;
    end;

    _connection.commitQuery(SQLQuery);
  finally
    _connection.finishQuery(SQLQuery);
  end;
end;

procedure TRemoteOptimizationRun.checkFinished(
  var tempRuns: TRemoteEvaluationTaskList);
begin
  try
    unsafeCheckFinished(tempRuns);
  except on e: exception do
    _connection.Reconnect(e.Message);
  end;
end;

procedure TRemoteOptimizationRun.postKeepAlive;
var
  KeepAliveQuery : TSQLQuery;
begin
  KeepAliveQuery := _connection.createQuery('keep-alive');
  try
    if _remoteTaskSettings.useDBLocks then
    begin
      KeepAliveQuery.SQL.Text := 'LOCK TABLE "' + _sqlTable + '";';
      KeepAliveQuery.ExecSQL;
      KeepAliveQuery.SQL.Clear;
    end;

    KeepAliveQuery.SQL.Add( 'UPDATE "' + _sqlTable + '"'
                            + ' SET "lastPing" = now()'
                            + ' WHERE "optimizationID" = ''' + _remoteTaskSettings.optimizationID + ''';');
    KeepAliveQuery.ExecSQL;

    _connection.commitQuery(KeepAliveQuery);
  finally
    _connection.finishQuery(KeepAliveQuery);
  end;
end;

procedure TRemoteOptimizationRun.interruptTasks();
var
  x : integer;
  DeleteSQLQuery : TSQLQuery;
begin
  Writeln('Delete tasks (', Length(_tempRuns),'):');
  DeleteSQLQuery := _connection.createQuery();
  try
    if _remoteTaskSettings.useDBLocks then
    begin
      DeleteSQLQuery.SQL.Text := 'LOCK TABLE "' + _sqlTable + '";';
      DeleteSQLQuery.ExecSQL;
      DeleteSQLQuery.SQL.Clear;
    end;

    for x := 0 to high(_tempRuns) do
    begin

        DeleteSQLQuery.SQL.Add( 'DELETE FROM "' + _sqlTable + '"'
                                + ' WHERE "ID" = ' + _tempRuns[x]._rowID + ';');

        // Both, execute AND commit necessary!
        DeleteSQLQuery.ExecSQL;
    end;
    _connection.commitQuery(DeleteSQLQuery);
  finally
    _connection.finishQuery(DeleteSQLQuery);
  end;
end;

constructor TRemoteOptimizationRun.Create(const cmdLineParameters: string;
  chosenParametersList: TChosenParameterSetList;
  imageSequences: TImageSequenceDataList;
  const environment: TOptimizationEnvironment);
begin
  inherited Create(cmdLineParameters, chosenParametersList, imageSequences,
    environment);

  _connection := nil;
  _sqlTable   := 'tasks';
end;

destructor TRemoteOptimizationRun.Destroy;
begin
  if _connection <> nil then
  begin
    _connection.Free;
  end;
  inherited Destroy;
end;

procedure TRemoteOptimizationRun.setRemoteTaskSettings(
  remoteTaskSettings: TRemoteTaskSettings);
begin
  _remoteTaskSettings := remoteTaskSettings;
end;

procedure TRemoteOptimizationRun.openConnection(settings: TDBSettings);
begin
  _connection := TDBConnection.Create(settings);
end;

function TRemoteOptimizationRun.go(
  resultMatrix: TChosenParameterSetAndResultList; runNo: Cardinal): boolean;
var
  t : TDateTime;
  x, y, waitCounter : integer;
  e : TRemoteEvaluationTask;
  evaluationTasks : TRemoteEvaluationTaskMatrix;
  tempRuns : TRemoteEvaluationTaskList;
  tasksPosted : boolean;

  // open transaction, iterate through tasks and insert them into DB
  procedure postTasks;
  var
    SQLTransaction: TSQLTransaction;
    SQLQuery : TSQLQuery;
    x : Integer;
  begin
    SQLTransaction := _connection.createTransaction();
    try
      SQLQuery := TSQLQuery.Create(nil);
      try
        SQLQuery.Transaction := SQLTransaction;

        if _remoteTaskSettings.useDBLocks then
        begin
          SQLQuery.SQL.Text := 'LOCK TABLE "' + _sqlTable + '";';
          SQLQuery.ExecSQL;
        end;

        SetLength(tempRuns, 0);
        for x := 0 to high(evaluationTasks) do
          for e in evaluationTasks[x] do
            if e <> nil then
            begin
              e.putOnlineTrans(SQLTransaction);

              SetLength(tempRuns, Length(tempRuns)+1);
              tempRuns[high(tempRuns)] := e;
            end;

      finally
        SQLQuery.Free;
      end;

      //Writeln(Length(tempRuns));

      SQLTransaction.Commit;

      tasksPosted := true;
    finally
      _connection.finishTransaction(SQLTransaction);
    end;
  end;

begin
  t := Now();

  _remoteTaskSettings.runNo := runNo;

  // create evaluation tasks out of given parameter sets and given list of
  // image sequences
  SetLength(evaluationTasks, Length(_chosenParametersList), Length(_imageSequences));
  for x := 0 to high(evaluationTasks) do
    for y := 0 to high(evaluationTasks[x]) do
      if resultMatrix[x].resultList[y].useForOptimization then
      begin
        evaluationTasks[x,y] := TRemoteEvaluationTask.Create( _cmdLineParameters,
                                                            _chosenParametersList[x],
                                                            _imageSequences[y],
                                                            _connection,
                                                            self,
                                                            _environment.evaluationBinaryClass,
                                                            _sqlTable,
                                                            _remoteTaskSettings,
                                                            x, y);
      end
      else
      begin
        evaluationTasks[x,y] := nil;
      end;

  try
    // store tasks in the database
    tasksPosted := false;
    while not tasksPosted do
    begin
      try
        postTasks;
        _tempRuns := tempRuns;
      except on e: exception do
        _connection.reconnect(e.Message);
      end;
    end;

    // wait for the tasks to be finished
    waitCounter := 0;
    while Length(tempRuns) > 0 do
    begin
      checkFinished(tempRuns);
      Sleep(1000);
      inc(waitCounter);

      if waitCounter >= 60 then
      begin
        postKeepAlive;
        waitCounter := 0;
      end;
    end;

    result        := true;

    // obtain results from the task instances and print them
    for x := 0 to high(evaluationTasks) do
    begin
      WriteAndLogLn('--- Parameter Index: ' + IntToStr(x));
      for y := 0 to high(evaluationTasks[x]) do
      begin
        if resultMatrix[x].resultList[y].useForOptimization then
        begin
          resultMatrix[x].resultList[y].result     := getChosenErrorValue(evaluationTasks[x,y].getErrorValues());
          resultMatrix[x].resultList[y].allErrors  := evaluationTasks[x][y].getErrorValues();
          resultMatrix[x].resultList[y].successful := evaluationTasks[x,y].wasSuccessful();

          WriteAndLogLn( 'Index: (' + Format('%4.4d, %4.4d', [x,y]) + ')'
                         + ', Successful: ' + IntToStr(Ord(resultMatrix[x].resultList[y].successful))
                         + ', f-Counter: ' + IntToStr(evaluationTasks[x,y].getFailureCounter())
                         + ', Result: '
                         + Format('%4.3f', [resultMatrix[x].resultList[y].result])
                         + ', GT: ' + evaluationTasks[x,y].getGTName());

          // if necessary, print the cmdline parameters and the output
          if _environment.printOutput or (not resultMatrix[x].resultList[y].successful) then
          begin
            WriteAndLogLn('__Parameters__:');
            WriteAndLogLn(evaluationTasks[x][y].getCmdLine());
            WriteAndLogLn('__Output__:');
            WriteAndLogLn(evaluationTasks[x,y].getOutput());
          end;
        end;
      end;

      // compute the average error among all sequences
      if Length(resultMatrix[x].resultList) > 0 then
      begin
        //Writeln('Error Measure Count: ', Length(resultMatrix[x].resultList[0].allErrors));
        SetLength(resultMatrix[x].allErrors, Length(resultMatrix[x].resultList[0].allErrors));
        for y := 0 to high(resultMatrix[x].allErrors) do
        begin
          resultMatrix[x].allErrors[y].name := resultMatrix[x].resultList[0].allErrors[y].name;
          resultMatrix[x].allErrors[y].value := computeAvgError(resultMatrix[x].resultList, y);
          //Writeln('Error Measure "', resultMatrix[x].allErrors[y].name, '" => ', resultMatrix[x].allErrors[y].value);
        end;
      end;
      resultMatrix[x].result := computeAvg(resultMatrix[x].resultList);
    end;
  finally
    for x := 0 to high(evaluationTasks) do
      for e in evaluationTasks[x] do
        if e <> nil then
          e.Free;
    SetLength(evaluationTasks, 0, 0);
  end;
  t := Now() - t;
  WriteAndLogLn('Time for optimization run: ' + TEvaluationThread.formatTime(t));
end;

end.

