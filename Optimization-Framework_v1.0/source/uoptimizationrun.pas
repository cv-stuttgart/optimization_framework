unit uOptimizationRun;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs,

  uDatatypesOptimizer, uEvaluationTask, uEvaluationBinary;

type

  TOptimizationEnvironment = record
    numThreads : Cardinal;
    errorValueName : string;
    uRunSettings : string;
    printOutput : boolean;
    evaluationBinaryClass : TEvaluationBinaryClass;
  end;

  TEvaluationTaskList = array of TEvaluationTask;
  TEvaluationTaskMatrix = array of TEvaluationTaskList;

  { TBasicOptimizationRun }

  TBasicOptimizationRun = class
  protected
    _cmdLineParameters : string;
    _chosenParametersList : TChosenParameterSetList;
    _imageSequences : TImageSequenceDataList;

    _environment : TOptimizationEnvironment;

    function getChosenErrorValue(errorValues : TErrorValueList) : extended;
  public
    constructor Create(const cmdLineParameters : string;
      dynamicParametersList : TChosenParameterSetList;
      imageSequences : TImageSequenceDataList;
      const environment : TOptimizationEnvironment); virtual;
    function go(resultMatrix : TChosenParameterSetAndResultList;
      runNo : Cardinal) : boolean; virtual; abstract;
    procedure interruptTasks; virtual;
  end;



  // The optimization run spawns different instances of TEstimationThread
  // which acquire tasks from its parent (TOptimizationRun) as long as there
  // are tasks available
  TOptimizationRun = class;

  { TEvaluationThread }

  TEvaluationThread = class(TThread)
  protected
    _source : TOptimizationRun;
    procedure Execute; override;
  public
    class function formatTime(t: TDateTime; timeOnly: boolean=false): string;
    class function formatDayTime(t: TDateTime): string;
    constructor Create(source : TOptimizationRun; directStart : boolean);
  end;
  TEvaluationThreadList = array of TEvaluationThread;

  { TOptimizationRun }

  TOptimizationRun = class(TBasicOptimizationRun)
  private
    _evaluationTasks : TEvaluationTaskMatrix;
    _CSEvaluationTask : TCriticalSection;

    _idxOfNextEvaluationTaskX : Cardinal;
    _idxOfNextEvaluationTaskY : Cardinal;
    function getNextEvaluationTask(out indexX, indexY : Cardinal) : TEvaluationTask;
  public
    constructor Create(const cmdLineParameters : string;
      dynamicParametersList : TChosenParameterSetList;
      imageSequences : TImageSequenceDataList;
      const environment : TOptimizationEnvironment); override;
    destructor Destroy; override;
    function go(resultMatrix : TChosenParameterSetAndResultList;
      runNo : Cardinal) : boolean; override;
  end;

  TOptimizationRunClass = class of TBasicOptimizationRun;

  { TOptimizationRunFactory }

  TOptimizationRunFactory = class(TObject)
  public
    function createInstance(const cmdLineParameters : string;
      dynamicParametersList : TChosenParameterSetList;
      imageSequences : TImageSequenceDataList;
      const environment : TOptimizationEnvironment) : TBasicOptimizationRun; virtual;
  end;

var
  optimizationRunFactory : TOptimizationRunFactory;

implementation

uses
  uSimpleLog, Math;

{ TOptimizationRunFactory }

function TOptimizationRunFactory.createInstance(
  const cmdLineParameters: string;
  dynamicParametersList: TChosenParameterSetList;
  imageSequences: TImageSequenceDataList;
  const environment: TOptimizationEnvironment): TBasicOptimizationRun;
begin
  result := TOptimizationRun.Create( cmdLineParameters, dynamicParametersList,
                                     imageSequences, environment);
end;

{ TBasicOptimizationRun }

function TBasicOptimizationRun.getChosenErrorValue(errorValues: TErrorValueList
  ): extended;
var
  errorValue : TErrorValue;
begin
  result := -1;
  for errorValue in errorValues do
  begin
    if errorValue.name = _environment.errorValueName then
    begin
      result := errorValue.value;
      exit;
    end;
  end;
end;

constructor TBasicOptimizationRun.Create(const cmdLineParameters: string;
  dynamicParametersList: TChosenParameterSetList;
  imageSequences: TImageSequenceDataList;
  const environment: TOptimizationEnvironment);
begin
  _cmdLineParameters     := cmdLineParameters;
  _chosenParametersList := dynamicParametersList;
  _imageSequences        := imageSequences;
  _environment           := environment;
end;

procedure TBasicOptimizationRun.interruptTasks;
begin
  // Dummy
end;

{ TEvaluationThread }

procedure TEvaluationThread.Execute;
var
  evaluationTask : TEvaluationTask;
  indexX, indexY : Cardinal;
  t : TDateTime;
begin
  evaluationTask := _source.getNextEvaluationTask(indexX, indexY);
  while evaluationTask <> nil do
  begin
    // let the evaluation execute and measure run time
    t := Now();
    evaluationTask.execute;
    t := Now() - t;

    // print and log the time
    WriteAndLogLn( 'Index: (' + Format('%4.4d, %4.4d', [indexX, indexY]) + ')'
                 + ', Time: ' + formatTime(t)
                 );

    // acquire the next task
    evaluationTask := _source.getNextEvaluationTask(indexX, indexY);
  end;

  Terminate;
end;

class function TEvaluationThread.formatTime(t: TDateTime; timeOnly: boolean
  ): string;
var
  hour, minute, second, millisecond : Word;
  days : Integer;
begin
  DecodeTime(t, hour, minute, second, millisecond);
  days := trunc(t);
  if (days > 0) then
    result := IntToStr(days) + ' day(s) ' + Format('%2.2d', [hour]) + ':'
  else if hour > 0 then
    result := Format('%2.2d', [hour]) + ':'
  else
    result := '';
  result := result + Format('%2.2d:%2.2d.%3.3d', [minute, second, millisecond]);
end;

class function TEvaluationThread.formatDayTime(t: TDateTime): string;
var
  hour, minute, second, millisecond : Word;
begin
  DecodeTime(t, hour, minute, second, millisecond);
  result := Format('%2.2d:%2.2d:%2.2d.%3.3d', [hour, minute, second, millisecond]);
end;

constructor TEvaluationThread.Create(source: TOptimizationRun;
  directStart: boolean);
begin
  inherited create(true);
  _source := source;

  if directStart then
  begin
    start;
  end;
end;

{ TOptimizationRun }

function TOptimizationRun.getNextEvaluationTask(out indexX, indexY: Cardinal
  ): TEvaluationTask;

  // if row within "_evaluationTasks" is complete, start in the next row
  procedure checkIndices;
  begin
    if _idxOfNextEvaluationTaskY >= Length(_evaluationTasks[_idxOfNextEvaluationTaskX]) then
    begin
      inc(_idxOfNextEvaluationTaskX);
      _idxOfNextEvaluationTaskY := 0;
    end;
  end;

  function indicesOk : boolean;
  begin
    result := (_idxOfNextEvaluationTaskX < Length(_evaluationTasks)) and
              (_idxOfNextEvaluationTaskY < Length(_evaluationTasks[_idxOfNextEvaluationTaskX]));
  end;

begin
  _CSEvaluationTask.Enter;
  try
    result := nil;
    if indicesOk then
    begin
      while (_idxOfNextEvaluationTaskY < Length(_evaluationTasks[_idxOfNextEvaluationTaskX]))
            and (_evaluationTasks[_idxOfNextEvaluationTaskX][_idxOfNextEvaluationTaskY] = nil) do
      begin
        inc(_idxOfNextEvaluationTaskY);
      end;
      checkIndices;

      if indicesOk then
      begin
        result := _evaluationTasks[_idxOfNextEvaluationTaskX][_idxOfNextEvaluationTaskY];
        indexX := _idxOfNextEvaluationTaskX;
        indexY := _idxOfNextEvaluationTaskY;

        inc(_idxOfNextEvaluationTaskY);
        checkIndices;

        // nil is returned, the thread would terminate. Hence, go on here
        if result = nil then
          result := getNextEvaluationTask(indexX, indexY);
      end;
    end;
  finally
    _CSEvaluationTask.Leave;
  end;
end;

constructor TOptimizationRun.Create(const cmdLineParameters: string;
  dynamicParametersList: TChosenParameterSetList;
  imageSequences: TImageSequenceDataList;
  const environment: TOptimizationEnvironment);
begin
  inherited Create( cmdLineParameters, dynamicParametersList, imageSequences,
                    environment);

  _CSEvaluationTask := TCriticalSection.Create;

  _idxOfNextEvaluationTaskX := 0;
  _idxOfNextEvaluationTaskY := 0;
end;

destructor TOptimizationRun.Destroy;
begin
  _CSEvaluationTask.Free;
end;

function TOptimizationRun.go(resultMatrix: TChosenParameterSetAndResultList;
  runNo: Cardinal): boolean;
var
  x, y : integer;
  evaluationThreads : TEvaluationThreadList;
  e : TEvaluationTask;
  t : TDateTime;
  evTaskCount, threadPoolSize : Cardinal;
begin
  t := Now();


  // create evaluation tasks out of given parameter sets and given list of
  // image sequences
  evTaskCount := 0;
  SetLength(_evaluationTasks, Length(_chosenParametersList), Length(_imageSequences));
  for x := 0 to high(_evaluationTasks) do
    for y := 0 to high(_evaluationTasks[x]) do
      if resultMatrix[x].resultList[y].useForOptimization then
      begin
        _evaluationTasks[x][y] := TEvaluationTask.Create(
                                                     _cmdLineParameters,
                                                     _chosenParametersList[x],
                                                     _imageSequences[y],
                                                     _environment.evaluationBinaryClass,
                                                     false);
        inc(evTaskCount);
      end
      else
      begin
        _evaluationTasks[x][y] := nil;
      end;

  try

    // create threads and let them run
    threadPoolSize := min(_environment.numThreads, evTaskCount);
    WriteAndLogLn('Create and start ' + IntToStr(threadPoolSize) + ' threads...');
    SetLength(evaluationThreads, threadPoolSize);

    // let all except the first instance run in newly spawned threads
    for x := 0 to high(evaluationThreads) do
      evaluationThreads[x] := TEvaluationThread.Create(self, x <> 0);

    // let the first thread run in the context of the main thread
    // (so the following "wait for termination" is not done far too early)
    if Length(evaluationThreads) > 0 then
      evaluationThreads[0].Execute;

    // wait for termination
    for x := 0 to high(evaluationThreads) do
    begin
      if x <> 0 then
      begin
        WriteAndLogLn('Wait for termination of thread ' +  IntToStr(x) + ' ...');
        evaluationThreads[x].WaitFor;
      end;
      evaluationThreads[x].Free;
    end;

    result        := Length(evaluationThreads) > 0;

    // evaluate results
    for x := 0 to high(_evaluationTasks) do
    begin
      WriteAndLogLn('--- Parameter Index: ' + IntToStr(x));
      for y := 0 to high(_evaluationTasks[x]) do
      begin
        if resultMatrix[x].resultList[y].useForOptimization then
        begin
          resultMatrix[x].resultList[y].result     := getChosenErrorValue(_evaluationTasks[x][y].getErrorValues());
          resultMatrix[x].resultList[y].allErrors  := _evaluationTasks[x][y].getErrorValues();
          resultMatrix[x].resultList[y].successful := _evaluationTasks[x][y].wasSuccessful();

          WriteAndLogLn( 'Index: ' + Format('%4.4d', [x])
                         + ', Successful: ' + IntToStr(Ord(resultMatrix[x].resultList[y].successful))
                         + ', Result: '
                         + FormatFloat('000.000', resultMatrix[x].resultList[y].result)
                         + ', GT: ' + _evaluationTasks[x][y].getGTName() );

          if _environment.printOutput or (not resultMatrix[x].resultList[y].successful) then
          begin
            WriteAndLogLn('__Parameters__:');
            WriteAndLogLn(_evaluationTasks[x][y].getCmdLine());
            WriteAndLogLn('__Output__:');
            WriteAndLogLn(_evaluationTasks[x][y].getOutput());
          end;
        end;
      end;
      if Length(resultMatrix[x].resultList) > 0 then
      begin
        SetLength(resultMatrix[x].allErrors, Length(resultMatrix[x].resultList[0].allErrors));
        for y := 0 to high(resultMatrix[x].allErrors) do
        begin
          resultMatrix[x].allErrors[y].name := resultMatrix[x].resultList[0].allErrors[y].name;
          resultMatrix[x].allErrors[y].value := computeAvgError(resultMatrix[x].resultList, y);
        end;
      end;
      resultMatrix[x].result := computeAvg(resultMatrix[x].resultList);
    end;
  finally
    for x := 0 to high(_evaluationTasks) do
      for e in _evaluationTasks[x] do
        if e <> nil then
          e.Free;
    SetLength(_evaluationTasks, 0);
  end;
  t := Now() - t;
  WriteAndLogLn('Time for optimization run: ' + TEvaluationThread.formatTime(t));
end;

initialization
  optimizationRunFactory := TOptimizationRunFactory.Create;
finalization
  optimizationRunFactory.free;

end.

