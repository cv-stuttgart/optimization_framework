unit uVirtualThread;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

{
This unit contains a class representing a virtual thread. This virtual thread
contains the tasks one thread of the owner, a client, is expected to compute.
}

uses
  Types, Classes, SysUtils, SyncObjs,

  uClientBase, uClientTask;

type
  TDateTimeDynArray = array of TDateTime;

  { TVirtualThread }

  TVirtualThread = class
  protected
    _parent : TClientNodeBase;
    _tasks : TClientTaskList;

    _lockTasks : TCriticalSection;

  public
    constructor Create(parent : TClientNodeBase);
    destructor Destroy; override;
    function clone() : TVirtualThread;

    // assigns a task to this virtual thread
    procedure assignTask(const task : TClientTask);

    // deletes a task from the queue
    function deleteTask(const task: TClientTask) : boolean;

    // purpose of "extractTaskByID":
    // if a task's computation was not successful, it is extracted from the
    // queue of the current thread in order to re-assign it to the optimal
    // thread
    function extractTaskByID(out task : TClientTask; const ID : string) : boolean;

    // search for the task with the given ID and return it
    function getTaskByID(out task : TClientTask; const ID : string) : boolean;

    // empty the queue
    procedure clearTasks;

    // estimates the time when all the tasks of this thread will be finished
    function estimateFinishTime(out taskTimes: TDateTimeDynArray; out
      activeThreads: TDoubleDynArray): TDateTime;

    // estimates the time when all the tasks of this thread will be finished
    function estimateFinishTime() : TDateTime;

    // estimate the starting time of the next available task
    function getStartTimeOfNextTask(out startTime : TDateTime) : boolean;

    // get the next available task and mark it as "running"
    function getNextTaskForComputation(out task: TClientTask; doNotMarkAsActive : boolean = false): boolean;

    // get the next available task and delete it from the queue in order to
    // reassign it among all threads/clients
    function extractNextTaskForComputation(out task: TClientTask): boolean;

    function countActiveTasks : Integer;
    function getTaskList() : TClientTaskList;

    function getStartTime() : TDateTime;
    function getTaskCount() : Integer;
    function getMaxRunTimeFactor() : Extended;
  end;
  TClientThreadList = array of TVirtualThread;

implementation

{ TVirtualThread }

constructor TVirtualThread.Create(parent: TClientNodeBase);
begin
  _parent := parent;

  _lockTasks := TCriticalSection.Create;
  SetLength(_tasks, 0);
end;

destructor TVirtualThread.Destroy;
begin
  _lockTasks.Free;
  inherited Destroy;
end;

function TVirtualThread.clone: TVirtualThread;
begin
  result := TVirtualThread.Create(_parent);
  result._tasks := getTaskList();
end;

function TVirtualThread.estimateFinishTime(out taskTimes: TDateTimeDynArray; out
  activeThreads: TDoubleDynArray): TDateTime;
var
  x : integer;
  tempActiveThreads, tempLoadFactor:  Extended;
begin
  _lockTasks.Enter;
  try
    SetLength(taskTimes, Length(_tasks));
    SetLength(activeThreads, Length(_tasks));
    if Length(_tasks) = 0 then
    begin
      result := Now();
      exit;
    end;

    // get a start time
    result := _tasks[0].getStartTime();
    taskTimes[0] := _parent.getTaskTime(_tasks[0], result, tempActiveThreads, tempLoadFactor) * _tasks[0].runTimeFactor;
    activeThreads[0] := tempActiveThreads;
    result := result + taskTimes[0];
    taskTimes[0] := result - Now();
    if result < Now() then
    begin
      taskTimes[0] := 0;
      result := Now();
    end;

    // sum up expected runtimes of the tasks
    for x := 1 to high(_tasks) do
    begin
      taskTimes[x] := _tasks[x].runTimeFactor * _parent.getTaskTime(_tasks[x], result, tempActiveThreads, tempLoadFactor);
      activeThreads[x] := tempActiveThreads;
      result := result + taskTimes[x];
    end;

    // if there are still tasks assigned, the thread cannot have finished, yet
    if result < Now() then
      result := Now();
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.estimateFinishTime: TDateTime;
var
  taskTimes : TDateTimeDynArray;
  activeThreads : TDoubleDynArray;
begin
  result := estimateFinishTime(taskTimes, activeThreads);
end;

function TVirtualThread.getStartTimeOfNextTask(out startTime: TDateTime): boolean;
var
  x : integer;
begin
  result := false;

  _lockTasks.Enter;
  try
    for x := 0 to high(_tasks) do
    begin
      if x = 0 then
        startTime := _tasks[x].getStartTime();

      // we only want to sum up the run times of tasks which are computed now
      if not _tasks[x].computationRunning then
      begin
        result := true;
        break;
      end;

      // sum up expected run time of the task
      startTime := startTime + _tasks[x].runTimeFactor * _parent.getTaskTime(_tasks[x], startTime)
    end;
  finally
    _lockTasks.Leave;
  end;

  // in now case the next task may start int the past
  if startTime < Now() then
    startTime := Now();
end;

procedure TVirtualThread.assignTask(const task: TClientTask);
var
  x, taskIdx : integer;
begin
  _lockTasks.Enter;
  try
    if not task.computationRunning then
    begin

      // in case the task is still available, enqueue it
      SetLength(_tasks, Length(_tasks)+1);
      _tasks[high(_tasks)] := task;
    end
    else
    begin

      // in case the task is already running, put it right after all other
      // running tasks
      // in general, however, there shouldn't be any other running tasks, s.t.
      // it is pushed to the front of the queue
      taskIdx := Length(_tasks);
      for x := 0 to high(_tasks) do
      begin
        if not _tasks[x].computationRunning then
        begin
          taskIdx := x;
          break;
        end;
      end;

      // move all tasks one step to the end
      SetLength(_tasks, Length(_tasks)+1);
      for x := high(_tasks)-1 downto taskIdx do
        _tasks[x+1] := _tasks[x];

      // place the given task before them
      _tasks[taskIdx] := task;
    end;
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.deleteTask(const task: TClientTask): boolean;
var
  x, y : integer;
begin
  result := false;

  _lockTasks.Enter;
  try
    // find the task (via its ID) and delete it
    for x := 0 to high(_tasks) do
      if _tasks[x].ID = task.ID then
      begin
        for y := x to high(_tasks)-1 do
          _tasks[y] := _tasks[y+1];
        SetLength(_tasks, Length(_tasks)-1);
        result := true;
        break;
      end;
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.extractTaskByID(out task: TClientTask; const ID: string
  ): boolean;
var
  x, y : integer;
begin
  result := false;

  _lockTasks.Enter;
  try

    // find the task (via ID), copy it to "task" and delete it from the queue
    for x := 0 to high(_tasks) do
      if _tasks[x].ID = ID then
      begin
        result := true;
        task := _tasks[x];
        for y := x to high(_tasks)-1 do
          _tasks[y] := _tasks[y+1];
        SetLength(_tasks, Length(_tasks)-1);
        break;
      end;
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.getTaskByID(out task: TClientTask; const ID: string
  ): boolean;
var
  x : integer;
begin
  result := false;

  _lockTasks.Enter;
  try

    // find the task (via ID), copy it to "task" and delete it from the queue
    for x := 0 to high(_tasks) do
      if _tasks[x].ID = ID then
      begin
        result := true;
        task := _tasks[x];
        break;
      end;
  finally
    _lockTasks.Leave;
  end;
end;

procedure TVirtualThread.clearTasks;
begin
  _lockTasks.Enter;
  try
    // empty the queue
    SetLength(_tasks, 0);
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.countActiveTasks: Integer;
var
  x : integer;
begin
  result := 0;

  _lockTasks.Enter;
  try
    // count all tasks which are running
    for x := 0 to high(_tasks) do
      if _tasks[x].computationRunning then
        inc(result);
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.getTaskList: TClientTaskList;
begin
  _lockTasks.Enter;
  try

    // create a copy of the task queue and return it
    result := Copy(_tasks, 0, Length(_tasks));
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.getStartTime: TDateTime;
begin
  _lockTasks.Enter;
  try
    // return the starting time of the first task (if there is any)
    if Length(_tasks) > 0 then
      result := _tasks[0].getStartTime()
    else
      result := Now();
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.getTaskCount: Integer;
begin
  _lockTasks.Enter;
  try
    result := Length(_tasks);
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.getMaxRunTimeFactor: Extended;
var
  x : integer;
begin
  result := 1;

  _lockTasks.Enter;
  try
    for x := 0 to high(_tasks) do
      if _tasks[x].runTimeFactor > result then
        result := _tasks[x].runTimeFactor;
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.getNextTaskForComputation(out task: TClientTask;
  doNotMarkAsActive: boolean): boolean;
var
  x : integer;
begin
  result := false;

  _lockTasks.Enter;
  try
    for x := 0 to high(_tasks) do
    begin

      // look for a task which is not already reserved
      if not _tasks[x].computationRunning then
      begin
        if not doNotMarkAsActive then
        begin

          // mark the task as reserved
          _tasks[x].computationRunning := true;
          _tasks[x].start              := Now();
        end;
        task := _tasks[x];
        result := true;
        break;
      end;
    end;
  finally
    _lockTasks.Leave;
  end;
end;

function TVirtualThread.extractNextTaskForComputation(out task: TClientTask
  ): boolean;
var
  x, y : integer;
begin
  result := false;

  _lockTasks.Enter;
  try
    for x := 0 to high(_tasks) do
    begin

      // look for a task which is not already reserved
      if not _tasks[x].computationRunning then
      begin

        // return it
        task := _tasks[x];
        result := true;

        // delete it from the queue
        for y := x to high(_tasks)-1 do
          _tasks[y] := _tasks[y+1];
        SetLength(_tasks, Length(_tasks)-1);
        break;
      end;
    end;
  finally
    _lockTasks.Leave;
  end;
end;

end.

