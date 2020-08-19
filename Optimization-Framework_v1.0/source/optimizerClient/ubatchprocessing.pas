unit uBatchProcessing;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

{
This unit contains a class TBatchProcessor which performs the HTTP requests to
acquire evaluation tasks and submit their results.
It aggregates an instance of TFetchTasksThread and multiple instances of
TEvaluationThread. The instances of TEvaluationThread repeatedly try to get
tasks from TBatchProcessor which delegates this to TFetchTasksThread.
If an instance of TEvaluationThread "et" requests a task, it is queued to a list
of waiting threads in the instance of TFetchTasksThread. The latter request a task
via TBatchProcessor, removes the "et" from the list of waiting threads and
provides the requested task to it. This is done whenever the list of waiting
threads is not empty.
}

uses
  Classes, SysUtils, SyncObjs, Contnrs,

  uDatatypesOptimizer, uHTTP_Utils, uEvaluationTask, uEvaluationBinary;

type

  // information which is necessary for acquiring a task
  TTaskSettings = record
    taskName, userName, taskType, clientID : string;
    server : record
      fetchURL, submitURL, rejectURL, pingURL : string;
    end;
  end;

  TEnvironment = record
    numThreads : Integer;
    fastThreads : Integer;
    adaptiveScheduling : boolean;
    taskSettings : TTaskSettings;
    deadline : TDateTime;
    //ebClass : TEvaluationBinaryClass;
  end;

  TBatchProcessor = class;

  { TStdInThread }

  TStdInThread = class(TThread)
  protected
    _source : TBatchProcessor;
    procedure Execute; override;
  public
    constructor Create(source : TBatchProcessor);
  end;

  { TEvaluationThread }

  TEvaluationThread = class(TThread)
  protected
    _source : TBatchProcessor;
    _threadIdx : Cardinal;
    _myTask : TEvaluationTask;
    _CSMyTask : TCriticalSection;

    // extracts the task for the next computation (within thread)
    function extractTask() : TEvaluationTask;

    procedure Execute; override;
  public
    class function formatTime(t : TDateTime) : string;
    constructor Create(source : TBatchProcessor; threadIdx : Cardinal;
      directStart : boolean);
    destructor Destroy; override;

    // sets the task which is to be computed next (from outside the thread)
    procedure setTask(task : TEvaluationTask);
  end;
  TEvaluationThreadList = array of TEvaluationThread;

  { TFetchTasksThread }

  TFetchTasksThread = class(TThread)
  protected
    _source : TBatchProcessor;
  protected
    _threadsWaitingForATask : TFPObjectList;
    _CSThreadsWFAJ : TCriticalSection;

    // gets the thread which is assigned the next task
    function getNextWaitingThread(out t : TEvaluationThread) : boolean;
    procedure Execute; override;
  public
    constructor Create(source : TBatchProcessor);
    destructor Destroy; override;

    // adds a thread to the waiting list for a task
    procedure addToWaitingList(t : TEvaluationThread);
  end;

  { TPingThread }

  TPingThread = class(TThread)
  protected
    _source : TBatchProcessor;
    procedure Execute; override;
  public
    constructor Create(source : TBatchProcessor);
  end;

  { TBatchProcessor }

  TBatchProcessor = class
  protected
    _environment : TEnvironment;
    _fetchTasksThread : TFetchTasksThread;
    _pingThread : TPingThread;
    _terminateSoon : boolean;

    function httpGET(const url : string) : string;
    function httpPOST(const url, post: string): string;

    // obtains an evaluation task via HTTP from the server
    function getNextEvaluationTaskFromServer(out sleepSec : Cardinal) : TEvaluationTask;

    // request for an evaluation task
    procedure requestEvaluationTask(t : TEvaluationThread);

    // submit evaluation result via HTTP to server
    procedure finishEvaluationTask(e : TEvaluationTask);

    // just to tell the server that the client is still alive even when all
    // threads are busy with evaluations
    procedure sendPing;
  public
    constructor Create(const environment : TEnvironment);
    procedure run;
    procedure terminate;
  end;

implementation

uses
  Process, HTTPLoad,

  uSimpleLog;

{ TPingThread }

procedure TPingThread.Execute;
var
  counter : Integer;
begin
  counter := 0;
  while not terminated do
  begin
    Sleep(100);
    inc(counter);
    if counter = 300 then
    begin
      _source.sendPing;
      WriteAndLogLn('Ping sent! (' + TEvaluationThread.formatTime(time()) + ')');
      counter := 0;
    end;
  end;
end;

constructor TPingThread.Create(source: TBatchProcessor);
begin
  _source := source;

  inherited Create(false);
end;

{ TStdInThread }

procedure TStdInThread.Execute;
var
  c : Char;
begin
  c := #0;
  while c <> 'q' do
    Read(c);

  _source.terminate;
end;

constructor TStdInThread.Create(source: TBatchProcessor);
begin
  _source := source;

  inherited Create(false);
end;

{ TFetchTasksThread }

constructor TFetchTasksThread.Create(source: TBatchProcessor);
begin
  _threadsWaitingForATask    := TFPObjectList.Create(false);
  _CSThreadsWFAJ            := TCriticalSection.Create;
  _source                   := source;

  inherited Create(false);
end;

destructor TFetchTasksThread.Destroy;
begin
  inherited Destroy;

  _CSThreadsWFAJ.Free;
  _threadsWaitingForATask.Free;
end;

procedure TFetchTasksThread.addToWaitingList(t: TEvaluationThread);
begin
  _CSThreadsWFAJ.Enter;
  try
    _threadsWaitingForATask.Add(t);
  finally
    _CSThreadsWFAJ.Leave;
  end;
end;

function TFetchTasksThread.getNextWaitingThread(out t: TEvaluationThread): boolean;
begin
  _CSThreadsWFAJ.Enter;
  try
    result := _threadsWaitingForATask.Count <> 0;
    if result then
      t := TEvaluationThread(_threadsWaitingForATask[0])
    else
      t := nil;
  finally
    _CSThreadsWFAJ.Leave;
  end;
end;

procedure TFetchTasksThread.Execute;
var
  e : TEvaluationTask;
  t : TEvaluationThread;
  sleepSec, tmpSleepSec : Cardinal;
begin
  sleepSec := 1;
  while not terminated do
  begin

    // is any evaluation thread waiting for a task?
    if getNextWaitingThread(t) then
    begin

      // request a task
      e := _source.getNextEvaluationTaskFromServer(sleepSec);
      //Writeln('sleep: ', sleepSec);

      // is there an available task?
      if e <> nil then
      begin

        // delete the thread from the waiting list
        _CSThreadsWFAJ.Enter;
        try
          _threadsWaitingForATask.Delete(0);
        finally
          _CSThreadsWFAJ.Leave;
        end;

        // provide the task to the evaluation thread
        t.setTask(e);
      end
      else
      begin

        // if no task available, wait some time...
        // give the sleeping time a little variation in order to stagger the
        // requests from different clients
        tmpSleepSec := random(sleepSec div 2 + 1) + sleepSec div 2 + sleepSec mod 2;

        Sleep(tmpSleepSec * 1000);
      end;
    end
    else
      Sleep(50);
  end;
end;

{ TEvaluationThread }

function TEvaluationThread.extractTask: TEvaluationTask;
begin
  _CSMyTask.Enter;
  try
    result := _myTask;
    if result <> nil then
      _myTask := nil;
  finally
    _CSMyTask.Leave;
  end;
end;

procedure TEvaluationThread.setTask(task: TEvaluationTask);
begin
  _CSMyTask.Enter;
  try
    _myTask := task;
  finally
    _CSMyTask.Leave;
  end;
end;

procedure TEvaluationThread.Execute;
var
  evaluationTask : TEvaluationTask;
  t, t2 : TDateTime;
  submissionFailed : boolean;
begin
  _source.requestEvaluationTask(self);
  while not terminated do
  begin
    evaluationTask := extractTask;

    // has an evaluation task been assigned to this thread?
    if evaluationTask <> nil then
    begin
      WriteAndLogLn( 'Thread ' + IntToStr(_threadIdx)
                     + ' starts next estimation...');

      // start the execution and measure time
      t := Now();
      evaluationTask.execute;
      t := Now() - t;

      // submit evaluation results
      t2 := Now();
      try
        submissionFailed := false;
        _source.finishEvaluationTask(evaluationTask);
      except
        submissionFailed := true;
      end;
      t2 := Now() - t2;

      // if there is no severe evaluation error, ask for the next task
      if not evaluationTask.libraryError() then
        _source.requestEvaluationTask(self)
      else
      begin
        WriteAndLogLn('General evaluation error: terminating...');
        Terminate;
      end;

      if not submissionFailed then
        WriteAndLogLn( 'Thread ' + IntToStr(_threadIdx)
                     + ' has finished! Time: ' + formatTime(t)
                     + ' (Submission Time: ' + formatTime(t2) + ')')
      else
        WriteAndLogLn( 'Thread ' + IntToStr(_threadIdx) + ' failed to submit!');

      ResetLog;
    end
    else
    begin
      //Writeln( 'Thread ' + IntToStr(_threadIdx) + ' is unemployed!');
    end;

    Sleep(500);
  end;
end;

class function TEvaluationThread.formatTime(t: TDateTime): string;
var
  hour, minute, second, millisecond : Word;
begin
  DecodeTime(t, hour, minute, second, millisecond);
  if hour > 0 then
    result := Format('%2.2d', [hour]) + ':'
  else
    result := '';
  result := result + Format('%2.2d:%2.2d.%3.3d', [minute, second, millisecond]);
end;

constructor TEvaluationThread.Create(source: TBatchProcessor;
  threadIdx: Cardinal; directStart: boolean);
begin
  inherited create(true);
  _source    := source;
  _threadIdx := threadIdx;
  _CSMyTask   := TCriticalSection.Create;

  if directStart then
  begin
    start;
  end;
end;

destructor TEvaluationThread.Destroy;
begin
  _CSMyTask.Free;
  inherited Destroy;
end;

{ TBatchProcessor }

function TBatchProcessor.httpGET(const url: string): string;
var
  ms : TMemoryStream;
  output : TStringList;
begin
  ms := TMemoryStream.Create;
  try
    httpLoad.httpGet(url, ms);

    output := TStringList.Create;
    try
      output.LoadFromStream(ms);

      result := output.Text;
    finally
      output.Free;
    end;
  finally
    ms.Free;
  end;
end;

function TBatchProcessor.httpPOST(const url, post: string): string;
var
  ms : TMemoryStream;
  output : TStringList;
begin
  ms := TMemoryStream.Create;
  try
    httpLoad.httpPOST(url, post, ms);

    output := TStringList.Create;
    try
      output.LoadFromStream(ms);

      result := output.Text;

      //Writeln('Result: ', result);
    finally
      output.Free;
    end;
  finally
    ms.Free;
  end;
end;

function TBatchProcessor.getNextEvaluationTaskFromServer(out sleepSec : Cardinal
  ): TEvaluationTask;
var
  taskString : string;
  lines : TStringList;
  rowID, cmdLine, workingDir, evaluationBinary, getURL, errorName : string;
  deadline : TDateTime;
begin
  result := nil;
  sleepSec := 1;

  // assemble the URL for requesting an evaluation task
  getURL := _environment.taskSettings.server.fetchURL
            + '?taskName=' + ParamsEncode(_environment.taskSettings.taskName)
            + '&taskType=' + ParamsEncode(_environment.taskSettings.taskType)
            + '&userName=' + ParamsEncode(_environment.taskSettings.userName)
            + '&clientID=' + ParamsEncode(_environment.taskSettings.clientID);
  //if (_environment.fastThreads <> 1) or (_environment.numThreads <> 1) then
  if true then
  begin
    getURL := getURL
              + '&numMaxThreads=' + IntToStr(_environment.numThreads)
              + '&numFastThreads=' + IntToStr(_environment.fastThreads);
  end;
  if not _environment.adaptiveScheduling then
  begin
    getURL := getURL
              + '&force=1';
  end;

  // send request and read response
  taskString := httpGet(getURL);

  // parse response and assemble task
  lines := TStringList.Create;
  try
    lines.Delimiter       := #10;
    lines.StrictDelimiter := true;
    lines.DelimitedText   := taskString;

    // if a task is available
    if lines.Count > 4 then
    begin
      rowID      := trim(lines[1]);
      cmdLine    := trim(lines[2]);
      errorName  := trim(lines[3]);
      workingDir := trim(lines[4]);
      if lines.Count > 5 then
        deadline   := StrToFloatDef(trim(lines[5]), 0);
      if lines.Count > 6 then
        evaluationBinary := trim(lines[6])
      else
        evaluationBinary := '';

      result := TEvaluationTask.Create( rowID, cmdLine, errorName, workingDir,
                                        evaluationBinary, deadline);
    end

    // else: some estimated time for sleeping until the next request is provided
    else if lines.Count >= 1 then
    begin
      sleepSec := StrToIntDef(trim(lines[0]), sleepSec);
    end;
  finally
    lines.Free;
  end;
end;

procedure TBatchProcessor.requestEvaluationTask(t: TEvaluationThread);
begin
  _fetchTasksThread.addToWaitingList(t);
end;

procedure TBatchProcessor.finishEvaluationTask(e: TEvaluationTask);
var
  urlstring, httpResult : string;
  count, x : Cardinal;
  errorValues : TErrorValueList;
begin
  // if there has not been a severe error during evaluation, submit results
  if not e.libraryError() then
  begin
    urlstring := '?taskID=' + e.getRowID();
    urlstring := urlstring + '&successful=' + IntToStr(Ord(e.wasSuccessful()));
    urlstring := urlstring + '&errorValue='
                           +  StringReplace(FloatToStr(e.getErrorValue()), ',', '.', [rfReplaceAll]);
    errorValues := e.getErrorValues();
    for x := 0 to high(errorValues) do
    begin
      urlstring := urlstring + '&errorName' + IntToStr(x) + '=' + errorValues[x].name;
      urlstring := urlstring + '&errorValue' + IntToStr(x) + '='
                             + StringReplace(FloatToStr(errorValues[x].value), ',', '.', [rfReplaceAll]);
    end;

    //Writeln(urlstring);
    count := 0;
    repeat
      httpResult := httpPost( _environment.taskSettings.server.submitURL + urlstring,
                              'output=' + ParamsEncode(e.getOutput()));

      inc(count)
    until (trim(httpResult) = '1') or (count >= 3);
  end
  else
  begin
    // tell the server that the task is not computable
    urlstring := '?taskID=' + e.getRowID();

    count := 0;
    repeat
      httpResult := httpGet( _environment.taskSettings.server.rejectURL + urlstring);
      Writeln(_environment.taskSettings.server.rejectURL + urlstring);

      inc(count)
    until (trim(httpResult) = '1') or (count >= 3);

    Writeln('Library error: ');
    Writeln(e.getLibraryErrorMsg());
  end;
end;

procedure TBatchProcessor.sendPing;
var
  urlstring: string;
begin
  //Writeln('Send Ping!' + formatTime(Now()));
  urlstring := '?taskName=' + ParamsEncode(_environment.taskSettings.taskName)
               + '&taskType=' + ParamsEncode(_environment.taskSettings.taskType)
               + '&userName=' + ParamsEncode(_environment.taskSettings.userName)
               + '&clientID=' + ParamsEncode(_environment.taskSettings.clientID);

  httpGet(_environment.taskSettings.server.pingURL + urlstring);
  //Writeln('Ping result: ' + trim(httpResult));
end;

constructor TBatchProcessor.Create(const environment: TEnvironment);
begin
  _environment := environment;
end;

procedure TBatchProcessor.run;
var
  evaluationThreads : TEvaluationThreadList;
  stdInThread : TStdInThread;
  x : integer;
begin
  _terminateSoon := false;

  stdInThread := TStdInThread.Create(self);
  try
    WriteAndLogLn('Create ' + IntToStr(_environment.numThreads) + ' threads...');

    _pingThread := TPingThread.Create(self);
    try
      // create the thread fetching tasks
      _fetchTasksThread := TFetchTasksThread.Create(self);
      try

        // create (multiple) threads to perform the evaluations
        SetLength(evaluationThreads, _environment.numThreads);
        for x := 0 to high(evaluationThreads) do
          evaluationThreads[x] := TEvaluationThread.Create(self, x, true);

        // run until program is terminated or deadline is reached
        while not _terminateSoon do
        begin
          Sleep(1000);
          if (_environment.deadline > 0) and (Now() > _environment.deadline) then
          begin
            _terminateSoon := true;
            WriteAndLogLn('Deadline reached!');
          end;
        end;
        WriteAndLogLn('Terminating program...');

        // wait for termination
        for x := 0 to high(evaluationThreads) do
        begin
          evaluationThreads[x].Terminate;
          WriteAndLogLn('Waiting for termination of thread ' +  IntToStr(x) + '...');
          evaluationThreads[x].WaitFor;
          evaluationThreads[x].Free;
        end;
      finally
        _fetchTasksThread.Terminate;
        WriteAndLogLn('Waiting for termination of task-thread...');
        _fetchTasksThread.WaitFor;
        _fetchTasksThread.Free;
      end;
    finally
      _pingThread.Terminate;
      _pingThread.WaitFor;
      _pingThread.Free;
    end;
  finally
    //stdInThread.Terminate;
    //stdInThread.Free;
  end;
end;

procedure TBatchProcessor.terminate;
begin
  _terminateSoon := true;
end;

end.

