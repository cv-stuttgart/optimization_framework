unit uClient;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

{
This unit contains a class representing a client which performs the evaluations.
It aggregates virtual threads which tasks are assigned to.
The amount of tasks it is assigned depends on its performance (usually measured
by average runtime). If there are more threads than CPU cores, the performance,
however, depends on the load (the amount of tasks). Both, the load and the
performance are estimated.
The average runtime is normalized by a load factor and by a "run time" factor of
the task, such that it corresponds to the expected run time of a task with
"run time" factor 1 as if only one thread is running on a CPU core (no
overload).
The tasks are balanced interactively between the virtual threads of a client, as
the real threads of a client do not acquire tasks independently but via a common
pipeline. Thus, they cannot be distinguished and the task which is handed out
to a client is always the next available task from any of its virtual threads.
Afterwards, a rebalancing step among the threads is performed.
}

uses
  Classes, SysUtils, SyncObjs,

  uClientBase, uClientTask, uVirtualThread;

type
  TDateTimeList = array of TDateTime;

  TClientSettings = record

    // the factor of the quotient between the run time of a task in case tasks
    // are assigned to at most "nFastThreads" threads, and the run time of a task
    // if tasks are assigned to "nMaxThreads" threads.
    // If a client runs more threads than it has cores the run time of each task
    // increases
    maxLoadFactor : Extended;

    // some buffer which comes into account when the amount of threads are
    // estimated for a specific point or range in time
    epsilonFinishTime : TDateTime;

    // the weight of an actual run time for computing the average run time
    averagingWeight : Extended;

    // the expected run time usually corresponds to the computed weighted
    // average of actual run times. However, if a client does not show any
    // activity for a long time ("this factor" x "average run times")
    // the silence time (i.e. time since the last activity) is used to determine
    // the expected run time
    silenceTimeThresholdFactor : Extended
  end;

  { TClientNode }

  TClientNode = class(TClientNodeBase)
  protected
    _settings : TClientSettings;

    // some synchronisation primitives
    _lockPerfInfo,
    _lockPerfInfo_Threads : TMultiReadExclusiveWriteSynchronizer;
    _lockThreads : TCriticalSection;

    // threads
    _threads : TClientThreadList;

    // ignore for task distribution
    _ignore : boolean;

    // (non-synchronized) determines the expected task time if a task is started
    //                    at "startTime"
    function _getTaskTime2(const task: TClientTask; startTime: TDateTime; out
      activeThreads, finalLoadFactor: Extended; out finishTimes: TDateTimeList
  ): TDateTime;

    // (non-synchronized) the same as above, but only returning the expected
    //                    run time
    function _getTaskTime(const task : TClientTask;
      startTime : TDateTime) : TDateTime;

    // (non-synchronized) given a task, estimate the finish time of the current
    //                    client and the respective thread index
    //                    if the task is assigned to it
    function _estimateFinishTime(const task: TClientTask; out threadIdx: Integer
      ): TDateTime;

    // (non-synchronized) used to assign an available task to the thread with
    //                    the index given by "threadIdx"
    function _assignTask(task: TClientTask; threadIdx: Integer): boolean;

    // (non-synchronized) used to assign an active task to the current client
    function _assignTask(task: TClientTask) : boolean;

    // (non-synchronized) after tasks are marked as active or deleted from the
    //                    client, all active tasks and the last available task
    //                    are internally re-assigned between the threads
    //                    in order to have one active task per thread and
    //                    a minimal estimated finish time
    procedure _rebalanceThreads;

    procedure _recreateThreads(var count: Integer);
  public
    constructor Create(const staticInfo : TClientInfo;
      const settings : TClientSettings);
    destructor Destroy; override;

    // is necessary to identify the client by some data
    function isIdentifiedBy(const clientInfo : TClientInfo) : boolean;

    // functions for initialization and update of the performance data
    procedure initAverageRunTime(runTime: TDateTime);
    procedure updatePerformanceInfo(const task: TClientTask;
      taskRunTime: TDateTime);
    procedure updateLastTaskActivity;
    procedure updateLastPing;

    // if the client does not provide information about the number of threads
    // this function estimates the necessary information by counting the maximal
    // number of active tasks
    function updateThreadCount : boolean;


    // synchronized verion of the function above
    function getTaskTime(const task: TClientTask; startTime: TDateTime): TDateTime;
      override;

    // synchronized verion of the function above
    function getTaskTime2(const task: TClientTask; startTime: TDateTime; out
      activeThreads: Extended; out finishTimes: TDateTimeList): TDateTime;

    // synchronized verion of the function above
    function getTaskTime(const task: TClientTask; startTime: TDateTime;
      out activeThreads, loadFactor: Extended): TDateTime; override;

    // synchronized verion of the function above
    function estimateFinishTime(const task: TClientTask; out threadIdx: Integer
      ): TDateTime;

    // estimates the finish time of this client
    function estimateFinishTime(): TDateTime;

    // synchronized verion of the function above
    function assignTask(task: TClientTask; threadIdx: Integer): boolean;

    // synchronized verion of the function above
    function assignTask(task: TClientTask) : boolean;

    // empty all queues of the threads
    procedure clearTasks;


    // browses the threads for the task with the least starting time
    function getNextScheduledTask(out task : TClientTask; doNotMarkAsActive : boolean = false) : boolean;

    // browses the threads for extraction of the task with the least starting
    // time and make it available for re-assignment
    function extractNextScheduledTask(out task : TClientTask) : boolean;

    // browses the threads and deletes a task if it is owned by one of the
    // threads
    procedure deleteTask(const task: TClientTask);

    // browses the threads for extraction of the task with the given ID
    function extractTaskByID(out task : TClientTask; const ID : string) : boolean;

    // browses the threads for the task with the given ID
    function getTaskByID(out task : TClientTask; const ID : string) : boolean;

    // clone the threads
    function cloneThreads() : TClientThreadList;

    function getIP() : string;
    function getID() : string;
    function getHost() : string;

    function getExpectedRuntime(const task: TClientTask; out noDataLearnedYet: boolean
      ): TDateTime;
    function getExpectedRuntime(const task: TClientTask): TDateTime;
    function getMinExpectedRuntime: TDateTime;
    function getMinAverageTime : TDateTime;
    function getThreadCount() : Cardinal;
    function getMaxRuntimeFactor() : Extended;

    function getIgnoreState() : boolean;
    procedure changeIgnoreState();

    procedure refreshOptimization(const optimizationID : string);
  end;
  TClientNodeList = array of TClientNode;

implementation

uses
  Math;

{ TClientNode }

constructor TClientNode.Create(const staticInfo: TClientInfo;
  const settings: TClientSettings);
var
  x : integer;
begin
  inherited Create(staticInfo);

  _settings := settings;

  _ignore := false;

  _lockPerfInfo := TMultiReadExclusiveWriteSynchronizer.Create;
  _lockPerfInfo_Threads := TMultiReadExclusiveWriteSynchronizer.Create;
  _lockThreads  := TCriticalSection.Create;

  SetLength(_threads, max(staticInfo.nMaxThreads, 1));
  for x := 0 to high(_threads) do
    _threads[x] := TVirtualThread.Create(self);
end;

destructor TClientNode.Destroy;
var
  x : integer;
begin
  for x := 0 to high(_threads) do
    _threads[x].Free;
  SetLength(_threads, 0);

  _lockThreads.Free;
  _lockPerfInfo_Threads.Free;
  _lockPerfInfo.Free;
  inherited Destroy;
end;

function TClientNode._getTaskTime2(const task : TClientTask; startTime: TDateTime;
  out activeThreads, finalLoadFactor: Extended;
  out finishTimes: TDateTimeList): TDateTime;
var
  x, y : integer;
  taskCount, nMaxThreads, nFastThreads : Integer;
  tmpExpectedRuntime, finishTime, epsilon, stdRunTime : TDateTime;
  loadFactor, highLoad, threadFraction : Extended;
  noDataLearnedYet : boolean;
begin
  activeThreads   := 0;
  finalLoadFactor := 1;
  _lockPerfInfo_Threads.Beginread;
  try
    nMaxThreads  := _perfInfo.nMaxThreads;
    nFastThreads := _perfInfo.nFastThreads;
  finally
    _lockPerfInfo_Threads.Endread;
  end;

  // in general assume subsequent tasks to have a runtime that is more or less
  // the same as the average runtime
  stdRunTime := getExpectedRuntime(task, noDataLearnedYet);
  result := stdRunTime;
  exit;





  // compensate for the effects of Hyper-Threading or similar
  if (nFastThreads <> nMaxThreads) {and (not noDataLearnedYet)} then
  begin

    // a little tolerance
    epsilon    := _settings.epsilonFinishTime;

    // (estimated) multiplicative impact on the runtime of a task when using
    // twice as many threads as there are cores
    loadFactor := _settings.maxLoadFactor;

    // tmpExpectedRuntime will be the estimated runtime for earlier tasks and thus be rather
    // overestimated (as a high workload is expected at earlier stages)
    tmpExpectedRuntime := loadFactor * stdRunTime;
    y := 0;
    while y < 1 do
    begin

      // estimate the number of threads that will be working at "startTime"
      activeThreads := 0;
      SetLength(finishTimes, Length(_threads));
      for x := 0 to high(_threads) do
      begin
        taskCount   := _threads[x].getTaskCount();
        finishTime := _threads[x].getStartTime() + taskCount * tmpExpectedRuntime;

        finishTimes[x] := finishTime;

        // will this thread still be active at "startTime"?
        if (taskCount > 0) and (finishTime > startTime + epsilon) then
        begin
          activeThreads := activeThreads + 1;

          // will it be active for the whole expected run time?
          {if (tmpExpectedRuntime <= 0) or (finishTime > startTime + tmpExpectedRuntime - epsilon) then
            activeThreads := activeThreads + 1
          else
          begin
            threadFraction := (startTime + tmpExpectedRuntime - finishTime) / tmpExpectedRuntime;
            if threadFraction < 0 then
              threadFraction := 0;
            activeThreads := activeThreads + threadFraction;
          end;}
        end;
      end;

      // if there are more threads than cores
      highLoad := activeThreads - nFastThreads;
      if highLoad > 0 then
      begin
        // the more threads, the more impact
        finalLoadFactor := 1.0 + highLoad / (nMaxThreads - nFastThreads) * (loadFactor - 1.0);
        tmpExpectedRuntime := finalLoadFactor * stdRunTime;
        result := tmpExpectedRuntime;
      end;

      inc(y);
    end;
  end;
end;

function TClientNode.getTaskTime2(const task : TClientTask; startTime: TDateTime;
  out activeThreads: Extended; out finishTimes: TDateTimeList): TDateTime;
var
  loadFactor : extended;
begin
  _lockThreads.Enter;
  try
    result := _getTaskTime2(task, startTime, activeThreads, loadFactor, finishTimes);
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.getTaskTime(const task: TClientTask; startTime: TDateTime;
  out activeThreads, loadFactor: Extended): TDateTime;
var
  finishTimes: TDateTimeList;
begin
  _lockThreads.Enter;
  try
    result := _getTaskTime2(task, startTime, activeThreads, loadFactor, finishTimes);
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode._getTaskTime(const task: TClientTask; startTime: TDateTime
  ): TDateTime;
var
  activeThreads, loadFactor: Extended;
  finishTimes: TDateTimeList;
begin
  result := _getTaskTime2(task, startTime, activeThreads, loadFactor, finishTimes);
end;

function TClientNode.getTaskTime(const task: TClientTask; startTime: TDateTime): TDateTime;
begin
  _lockThreads.Enter;
  try
    result := _getTaskTime(task, startTime);
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode._estimateFinishTime(const task: TClientTask; out
  threadIdx: Integer): TDateTime;
var
  x : integer;
  temp : TDateTime;
begin
  result := SysUtils.MaxDateTime;
  threadIdx := 0;

  // in order to assign a task we look for the thread which finishes first.
  // assigning the task to this thread is likely to have the least impact
  // on the overall finish time
  for x := 0 to high(_threads) do
  begin
    temp := _threads[x].estimateFinishTime();

    if temp < result then
    begin
      threadIdx := x;
      result := temp;
    end;
  end;

  result := result + task.runTimeFactor * _getTaskTime(task, result);
end;

function TClientNode.estimateFinishTime(const task : TClientTask;
  out threadIdx : Integer) : TDateTime;
begin
  _lockThreads.Enter;
  try
    result := _estimateFinishTime(task, threadIdx);
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.estimateFinishTime: TDateTime;
var
  x : integer;
  temp : TDateTime;
begin
  result := Now();

  _lockThreads.Enter;
  try

    // in order to estimate the overall finish time we regard the highest
    // estimated finish time of a thread
    for x := 0 to high(_threads) do
    begin
      temp := _threads[x].estimateFinishTime();

      if temp > result then
      begin
        result := temp;
      end;
    end;
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode._assignTask(task: TClientTask; threadIdx: Integer) : boolean;
begin
  result := false;

  if (threadIdx >= 0) and (threadIdx < Length(_threads)) then
  begin
    task.threadIdx := threadIdx;
    _threads[threadIdx].assignTask(task);
    result := true;
  end;
end;

function TClientNode._assignTask(task: TClientTask): boolean;
var
  x, y, threadIdx, activeTaskCount, minActiveTaskCount : integer;
  tasks : TClientTaskList;
begin
  result := false;

  if task.computationRunning then
  begin
    minActiveTaskCount := MaxInt;
    threadIdx         := -1;

    // find the thread with the least amount of active tasks
    for x := 0 to High(_threads) do
    begin
      tasks := _threads[x].getTaskList();
      activeTaskCount := 0;
      for y := 0 to high(tasks) do
        if tasks[y].computationRunning then
          inc(activeTaskCount);

      if activeTaskCount < minActiveTaskCount then
      begin
        minActiveTaskCount := activeTaskCount;
        threadIdx         := x;
      end;
    end;

    // assign the task the the found thread
    if threadIdx <> -1 then
    begin
      task.threadIdx := threadIdx;
      _threads[threadIdx].assignTask(task);
    end;
  end;
end;

procedure TClientNode._rebalanceThreads;
var
  x, y, mftIdx, newThreadIdx : Integer;
  maxFinishTime, temp : TDateTime;
  task : TClientTask;
  tempTaskList, extraTasks : TClientTaskList;
  activeTaskCount : Integer;
begin

  // ensure uniform distribution of active tasks among threads
  SetLength(extraTasks, 0);
  for x := 0 to high(_threads) do
  begin
    tempTaskList := _threads[x].getTaskList();

    // starting with index 1:
    // if there is more than one active task in a thread, store it outside
    // the thread
    for y := 1 to high(tempTaskList) do
    begin
      if not tempTaskList[y].computationRunning then
        break;

      SetLength(extraTasks, Length(extraTasks)+1);
      extraTasks[high(extraTasks)] := tempTaskList[y];
      _threads[x].deleteTask(extraTasks[high(extraTasks)]);
    end;
  end;

  // re-assign the stored active tasks
  for x := 0 to high(extraTasks) do
  begin
    assignTask(extraTasks[x]);
  end;

  // if a client acquires less tasks than possible, recreate appropriate thread
  // list
  if (Length(tempTaskList) > Length(_threads))
     and (Length(extraTasks) < Length(_threads)) then
  begin
    // TO DO
    {activeTaskCount := Length(extraTasks);
    _perfInfo.threadCountEstimated := true;
    _lockThreads.Enter;
    try
      _recreateThreads(activeTaskCount);
    finally
      _lockThreads.Leave;
    end;}
  end;


  // take last available task and re-assign it
  maxFinishTime := 0;
  mftIdx        := -1;
  for x := 0 to high(_threads) do
  begin
    temp := _threads[x].estimateFinishTime();
    if temp > maxFinishTime then
    begin
      maxFinishTime := temp;
      mftIdx        := x;
    end;
  end;

  if mftIdx > -1 then
  begin
    if _threads[mftIdx].extractNextTaskForComputation(task) then
    begin
      estimateFinishTime(task, newThreadIdx);
      assignTask(task, newThreadIdx);
    end;
  end;
end;

procedure TClientNode._recreateThreads(var count: Integer);
var
  threadIdx, x, y : Integer;
  taskList : array of TClientTaskList;
begin
  // collect all tasks and free old virtual threads
  SetLength(taskList, Length(_threads));
  for x := 0 to high(_threads) do
  begin
    taskList[x] := _threads[x].getTaskList();
    _threads[x].Free;
  end;

  // re-allocate virtual threads
  SetLength(_threads, count);
  for x := 0 to high(_threads) do
    _threads[x] := TVirtualThread.Create(self);

  // re-distribute the tasks among the threads
  count := 0;
  for x := 0 to high(taskList) do
    for y := 0 to high(taskList[x]) do
    begin
      if taskList[x][y].computationRunning then
      begin
        _assignTask(taskList[x][y], count);
        inc(count);
      end
      else
      begin
        _estimateFinishTime(taskList[x][y], threadIdx);
        _assignTask(taskList[x][y], threadIdx);
      end;
    end;

  // update the numbers
  _lockPerfInfo_Threads.Beginwrite;
  try
    _perfInfo.nMaxThreads  := count;
    _perfInfo.nFastThreads := count;
  finally
    _lockPerfInfo_Threads.Endwrite;
  end;
end;

function TClientNode.updateThreadCount: boolean;
var
  activeTaskCount, x : Integer;
begin
  result := false;

  // in case the thread amounts have to be estimated
  if _perfInfo.threadCountEstimated then
  begin
    _lockThreads.Enter;
    try

      // count the number of active tasks
      activeTaskCount := 0;
      for x := 0 to high(_threads) do
        activeTaskCount += _threads[x].countActiveTasks();


      // if there are more active tasks than virtual threads,
      // collect all tasks, re-allocate the virtual threads, re-distribute
      // the tasks and update the numbers indicating the amount of threads
      if activeTaskCount > Length(_threads) then
      begin
        result := true;

        _recreateThreads(activeTaskCount);
      end;
    finally
      _lockThreads.Leave;
    end;
  end;
end;

function TClientNode.cloneThreads: TClientThreadList;
var
  x : integer;
begin
  _lockThreads.Enter;
  try
    SetLength(result, Length(_threads));
    for x := 0 to high(result) do
      result[x] := _threads[x].clone();
  finally
    _lockThreads.Leave;
  end;
end;


function TClientNode.getExpectedRuntime(const task : TClientTask;
  out noDataLearnedYet: boolean): TDateTime;
var
  tSilence, tAvg, maxTAvg : TDateTime;
  maxRunTimeFactor : Extended;
  x : integer;
begin
  _lockPerfInfo.Beginread;
  try
    // usually the expected run time of a task corresponds to the average run
    // time
    tAvg             := _perfInfo.tAvgInitializationValue;
    result           := _perfInfo.tAvgInitializationValue;
    noDataLearnedYet := true;
    //Writeln('ExpectedRuntime (1): ', Length(_perfInfo.optimizations));

    for x := 0 to high(_perfInfo.optimizations) do
    begin
      //Writeln('(2): ', _perfInfo.optimizations[x].optimizationID);
      if _perfInfo.optimizations[x].optimizationID = task.optimizationID then
      begin
        //Writeln('ExpectedRuntime: ', task.optimizationID);
        tAvg             := _perfInfo.optimizations[x].tAvg;
        result           := tAvg;
        noDataLearnedYet := _perfInfo.optimizations[x].tAvg_InitialValue;
        break;
      end;
    end;

    // if, however, a client is inactive for a long time, we expect the run time
    // to be at least the time of silence
    // thus, the expected time grows with inactivity. If the client does not
    // become active again, it will lose all assigned (available) tasks and
    // will not be assigned any new tasks
    tSilence := Now() - _perfInfo.tLastTaskActivity;
    if tSilence > tAvg * _settings.silenceTimeThresholdFactor then
    begin

      // as maxRuntimeFactor >= 1 it is ok to save this computation until
      // tSilence > _perfInfo.tAvg * _settings.silenceTimeThresholdFactor
      maxRunTimeFactor := getMaxRunTimeFactor();
      maxTAvg := tAvg;
      for x := 0 to high(_perfInfo.optimizations) do
        if _perfInfo.optimizations[x].tAvg > maxTAvg then
          maxTAvg := _perfInfo.optimizations[x].tAvg;

      if tSilence > maxTAvg * _settings.silenceTimeThresholdFactor * maxRunTimeFactor then
      begin
        result := (tSilence / maxRunTimeFactor);
        if maxTAvg > 0 then
          result := result * (tAvg / maxTAvg);
      end;
    end;
  finally
    _lockPerfInfo.Endread;
  end;
end;

function TClientNode.getExpectedRuntime(const task : TClientTask): TDateTime;
var
  noDataLearnedYet : boolean;
begin
  result := getExpectedRuntime(task, noDataLearnedYet);
end;

function TClientNode.getMinExpectedRuntime: TDateTime;
var
  task : TClientTask;
  tempAvg : TDateTime;
  x : integer;
begin
  tempAvg := SysUtils.MaxDateTime;
  _lockPerfInfo.Beginread;
  try
    task.optimizationID := '';
    for x := 0 to high(_perfInfo.optimizations) do
    begin
      if _perfInfo.optimizations[x].tAvg < tempAvg then
      begin
        task.optimizationID := _perfInfo.optimizations[x].optimizationID;
        tempAvg            := _perfInfo.optimizations[x].tAvg;
      end;
    end;
  finally
    _lockPerfInfo.Endread;
  end;

  result := getExpectedRuntime(task);
end;

function TClientNode.getMinAverageTime: TDateTime;
var
  x : integer;
begin
  result := SysUtils.MaxDateTime;
  _lockPerfInfo.Beginread;
  try
    for x := 0 to high(_perfInfo.optimizations) do
    begin
      if _perfInfo.optimizations[x].tAvg < result then
      begin
        result             := _perfInfo.optimizations[x].tAvg;
      end;
    end;
    if result = SysUtils.MaxDateTime then
      result := _perfInfo.tAvgInitializationValue;
  finally
    _lockPerfInfo.Endread;
  end;
end;

function TClientNode.getThreadCount: Cardinal;
begin
  _lockThreads.Enter;
  try
    result := Length(_threads);
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.getMaxRuntimeFactor: Extended;
var
  x : integer;
  temp : Extended;
begin
  result := 1;

  _lockThreads.Enter;
  try
    for x := 0 to high(_threads) do
    begin
      temp := _threads[x].getMaxRunTimeFactor();
      if temp > result then
        result := temp;
    end;
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.getIgnoreState: boolean;
begin
  result := _ignore;
end;

procedure TClientNode.changeIgnoreState;
begin
  _ignore := not _ignore;
end;

procedure TClientNode.refreshOptimization(const optimizationID: string);
var
  x : integer;
begin
  _lockPerfInfo.Beginwrite;
  try
    for x := 0 to high(_perfInfo.optimizations) do
    begin
      if _perfInfo.optimizations[x].optimizationID = optimizationID then
      begin
        _perfInfo.optimizations[x].tLastUpdate := Now();
        break;
      end;
    end;
  finally
    _lockPerfInfo.EndWrite;
  end;
end;

function TClientNode.isIdentifiedBy(const clientInfo: TClientInfo): boolean;
begin
  result := (clientInfo.IP = _staticInfo.IP)
            and (clientInfo.instanceID = _staticInfo.instanceID);
end;

procedure TClientNode.updatePerformanceInfo(const task: TClientTask;
  taskRunTime: TDateTime);
var
  nettoRunTime : TDateTime;
  loadFactor, activeThreadCount : Extended;
  found : boolean;
  x, y : integer;
  tNewInitialAvg : TDateTime;
begin
  // estimate the load factor for the time when the task was computing
  getTaskTime(task, Now() - taskRunTime, activeThreadCount, loadFactor);

  // normalize the run time by the load factor and its run time factor
  nettoRunTime := taskRunTime / (task.runTimeFactor * loadFactor);

  // update the performance information
  _lockPerfInfo.Beginwrite;
  try
    // clean up old optimizations
    for x := high(_perfInfo.optimizations) downto 0 do
      if (_perfInfo.optimizations[x].optimizationID <> task.optimizationID)
         and (Now() - _perfInfo.optimizations[x].tLastUpdate > 5 * _perfInfo.optimizations[x].tAvg)
         and (Now() - _perfInfo.optimizations[x].tLastUpdate > EncodeTime(0,30,0,0)) then
      begin
        for y := x to high(_perfInfo.optimizations)-1 do
          _perfInfo.optimizations[y] := _perfInfo.optimizations[y+1];
        SetLength(_perfInfo.optimizations, Length(_perfInfo.optimizations)-1);
      end;



    // find and update
    found := false;
    for x := 0 to high(_perfInfo.optimizations) do
    begin
      if _perfInfo.optimizations[x].optimizationID = task.optimizationID then
      begin
        //Writeln('Update Runtime: ', task.optimizationID);

        if task.failureCounter = 0 then
          _perfInfo.optimizations[x].tAvg :=        _settings.averagingWeight * nettoRunTime
                                             + (1 - _settings.averagingWeight) * _perfInfo.optimizations[x].tAvg;
        _perfInfo.optimizations[x].tLastUpdate       := Now();
        found := true;
        break;
      end
    end;



    // insert
    if (not found) and (task.failureCounter = 0) then
    begin
      //Writeln('Update Runtime: ', task.optimizationID);

      x := Length(_perfInfo.optimizations);
      SetLength(_perfInfo.optimizations, x+1);
      _perfInfo.optimizations[x].optimizationID    := task.optimizationID;
      _perfInfo.optimizations[x].tAvg_InitialValue := false;
      _perfInfo.optimizations[x].tAvg              := nettoRunTime;
      _perfInfo.optimizations[x].tLastUpdate       := Now();
    end;

    // update tInitialAvg
    tNewInitialAvg := SysUtils.MaxDateTime;
    for x := 0 to high(_perfInfo.optimizations) do
    begin
      if _perfInfo.optimizations[x].tAvg < tNewInitialAvg then
        tNewInitialAvg := _perfInfo.optimizations[x].tAvg;
    end;

    if tNewInitialAvg < SysUtils.MaxDateTime then
      _perfInfo.tAvgInitializationValue := tNewInitialAvg;


    inc(_perfInfo.nComputations);
    _perfInfo.tTotal := _perfInfo.tTotal + nettoRunTime;

    _perfInfo.tLastTaskActivity := Now();
  finally
    _lockPerfInfo.Endwrite;
  end;
end;

procedure TClientNode.updateLastTaskActivity;
begin
  _lockPerfInfo.Beginwrite;
  try
    _perfInfo.tLastTaskActivity := Now();
    _perfInfo.tLastPing        := Now();
  finally
    _lockPerfInfo.Endwrite;
  end;
end;

procedure TClientNode.updateLastPing;
begin
  _lockPerfInfo.Beginwrite;
  try
    _perfInfo.tLastPing := Now();;
  finally
    _lockPerfInfo.Endwrite;
  end;
end;

procedure TClientNode.initAverageRunTime(runTime: TDateTime);
begin
  _lockPerfInfo.Beginwrite;
  try
    _perfInfo.tAvgInitializationValue := runTime

  finally
    _lockPerfInfo.Endwrite;
  end;
end;

function TClientNode.assignTask(task: TClientTask; threadIdx: Integer): boolean;
begin
  _lockThreads.Enter;
  try
    result := _assignTask(task, threadIdx);
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.assignTask(task: TClientTask): boolean;
begin
  _lockThreads.Enter;
  try
    result := _assignTask(task);
  finally
    _lockThreads.Leave;
  end;
end;

procedure TClientNode.clearTasks;
var
  x : integer;
begin
  _lockThreads.Enter;
  try
    for x := 0 to high(_threads) do
      _threads[x].clearTasks;
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.getNextScheduledTask(out task: TClientTask;
  doNotMarkAsActive: boolean): boolean;
var
  minStartTime, temp : TDateTime;
  x, minSTIdx : integer;
begin
  result       := false;
  minStartTime := SysUtils.MaxDateTime;
  minSTIdx     := -1;

  _lockThreads.Enter;
  try
    // look for the thread in which the next task has the earliest starting time
    for x := 0 to high(_threads) do
    begin
      if _threads[x].getStartTimeOfNextTask(temp)
         and (temp < minStartTime) then
      begin
        minStartTime := temp;
        minSTIdx     := x;
      end;
    end;

    // if such a thread is found get the next task
    if minSTIdx > -1 then
    begin
      result := _threads[minSTIdx].getNextTaskForComputation(task, doNotMarkAsActive);

      // re-balance among the threads
      _rebalanceThreads;

      // assign the client to the task
      task.clientID := _staticInfo.instanceID;
      task.clientIP := _staticInfo.IP;
    end;
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.extractNextScheduledTask(out task: TClientTask): boolean;
var
  x : integer;
begin
  result := false;

  _lockThreads.Enter;
  try
    for x := 0 to high(_threads) do
    begin
      if _threads[x].extractNextTaskForComputation(task) then
      begin
        result := true;
        break;
      end;
    end;
  finally
    _lockThreads.Leave;
  end;
end;

procedure TClientNode.deleteTask(const task : TClientTask);
var
  x : integer;
begin
  _lockThreads.Enter;
  try
    for x := 0 to high(_threads) do
    begin
      if _threads[x].deleteTask(task) then
        break;
    end;

    _rebalanceThreads;
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.extractTaskByID(out task: TClientTask; const ID: string
  ): boolean;
var
  x : integer;
begin
  result := false;

  _lockThreads.Enter;
  try
    for x := 0 to high(_threads) do
    begin
      result := _threads[x].extractTaskByID(task, ID);
      if result then
        break;
    end;

    _rebalanceThreads;
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.getTaskByID(out task: TClientTask; const ID: string): boolean;
var
  x : integer;
begin
  result := false;

  _lockThreads.Enter;
  try
    for x := 0 to high(_threads) do
    begin
      result := _threads[x].getTaskByID(task, ID);
      if result then
        break;
    end;
  finally
    _lockThreads.Leave;
  end;
end;

function TClientNode.getIP: string;
begin
  result := _staticInfo.IP;
end;

function TClientNode.getID: string;
begin
  result := _staticInfo.instanceID;
end;

function TClientNode.getHost: string;
begin
  result := _staticInfo.host;
end;

end.

