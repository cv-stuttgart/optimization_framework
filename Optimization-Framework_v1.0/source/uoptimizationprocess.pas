unit uOptimizationProcess;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  udatatypesOptimizer, uOptimizationRun, uParameterSelection;

type
  TWriteLogFileMethod = procedure of object;

  { TOptimizationProcess }

  TOptimizationProcess = class(TObject)
  protected
    // cmdline parameters which are directly passed to the optical flow binary
    _cmdLineParameters : string;

    // list of image sequences and associated data
    _imageSequences : TImageSequenceDataList;

    // some general global information passed to the evaluation process
    _optimizationEnvironment : TOptimizationEnvironment;

    // the current parameter selection strategy
    _parameterSelection : TParameterSelection;

    // factory for an instances of optimization run (direct or via client/server)
    _optimizationRunFactory : TOptimizationRunFactory;

    // method that writes the log file to disk
    _writeLogFile : TWriteLogFileMethod;

    // current optimization run
    _currOptimizationRun : TBasicOptimizationRun;

    // for interruptions
    _interrupted : boolean;
  protected
    _runCounter, _evaluationCounter : Cardinal;

    // contains information about the currently best run
    _bestRun : TOptimizationRunInfo;

    // - callback function which executes an optimization run based on
    //   a list of parameter sets and the given image sequences
    // - is called by the chosen parameter selection strategy
    function run(parameters: TChosenParameterSetList;
      imageSequenceResultList: TimageSequenceResultList
    ): TChosenParameterSetAndResultList;
  public
    constructor Create(const cmdLineParameters : string;
      imageSequences : TImageSequenceDataList;
      optimizationEnvironment : TOptimizationEnvironment;
      parameterSelection : TParameterSelection;
      optimizationRunFactory : TOptimizationRunFactory;
      writeLogFile : TWriteLogFileMethod);
    destructor Destroy; override;
    procedure optimize; virtual;
    function getSmallestResult() : extended;
    function getOptimalParameterSet() : TChosenParameterSet;
    function getAllErrors() : TErrorValueList;
    function getRunCounter() : Cardinal;
    function getEvaluationCounter() : Cardinal;
    procedure interruptTasks;
  end;

implementation

uses
  uSimpleLog;

{ TOptimizationProcess }


constructor TOptimizationProcess.Create(const cmdLineParameters: string;
  imageSequences: TImageSequenceDataList;
  optimizationEnvironment: TOptimizationEnvironment;
  parameterSelection: TParameterSelection;
  optimizationRunFactory: TOptimizationRunFactory;
  writeLogFile: TWriteLogFileMethod);
begin
  self._cmdLineParameters       := cmdLineParameters;
  self._imageSequences          := imageSequences;
  self._optimizationEnvironment := optimizationEnvironment;
  self._parameterSelection      := parameterSelection;
  self._optimizationRunFactory  := optimizationRunFactory;
  self._writeLogFile            := writeLogFile;

  _bestRun.run.result               := -1;
  SetLength(_bestRun.run.parameters.modelParams, 0);
  SetLength(_bestRun.run.parameters.solverParams, 0);
  _interrupted := false;
  _currOptimizationRun  := nil;
end;

destructor TOptimizationProcess.Destroy;
begin
  _parameterSelection.Free;
  inherited Destroy;
end;

function TOptimizationProcess.run(parameters: TChosenParameterSetList;
  imageSequenceResultList: TimageSequenceResultList
  ): TChosenParameterSetAndResultList;
var
  optimizationRun : TBasicOptimizationRun;
  tempParam : TRunParameter;
  x, y : Integer;
begin
  if _interrupted then
  begin
    WriteAndLogLn('Interrupted!');
    exit;
  end;

  WriteAndLogLn('Start next set of runs (' + IntToStr(Length(parameters)) + ')');
  WriteAndLogLn('Use ' + IntToStr(sequenceCount(imageSequenceResultList)) + ' sequences!');

  // copy lists containing image sequences and evaluation results for all
  // given sets of parameters
  SetLength(result, Length(parameters));
  for x := 0 to high(parameters) do
  begin
    result[x].resultList := Copy(imageSequenceResultList, 0, Length(imageSequenceResultList));
    result[x].parameters := parameters[x].clone();
    for y := 0 to high(result[x].resultList) do
      if result[x].resultList[y].useForOptimization then
        inc(_evaluationCounter);
  end;

  // initialize evaluation runs
  optimizationRun := _optimizationRunFactory.createInstance( _cmdLineParameters,
                                                 parameters,
                                                 _imageSequences,
                                                 _optimizationEnvironment);
  try
    _currOptimizationRun := optimizationRun;

    // let the evaluation run for the given parameter set, check success
    // and compare average evaluation results
    if optimizationRun.go(result, _runCounter) then
    begin
      for x := 0 to high(parameters) do
      begin
        // print and log current evaluation results and parameters
        WriteAndLogLn('Results for run ' + IntToStr(_runCounter + 1 + x) + ' with parameters:');
        for tempParam in parameters[x].modelParams do
        begin
          WriteAndLogLn(' - Name: ' + tempParam.name +', Value: ' +tempParam.value);
        end;


        // compute average evaluation result and store all information
        // for the current best evaluation result
        if (not _bestRun.Initialized) or
           (result[x].result < _bestRun.run.result) then
        begin
          _bestRun.run         := result[x].clone();
          _bestRun.initialized := true;
        end;
        WriteAndLogLn('Current result: ' + Format('%4.6f', [result[x].result]));
      end;
    end
    else
    begin
      SetLength(result, 0);
    end;
  finally
    _currOptimizationRun := nil;
    optimizationRun.Free;
  end;
  WriteAndLogLn('');

  // write log file to disk after each evaluation run
  if Assigned(_writeLogFile) then
    _writeLogFile;

  inc(_runCounter, Length(parameters));
end;

procedure TOptimizationProcess.optimize;
var
  optimizationRun : TBasicOptimizationRun;
  tempParamsList : TChosenParameterSetList;
  basicResultList : TimageSequenceResultList;
  currentResultMatrix : TChosenParameterSetAndResultList;
  x, y : integer;
  onlySubsetOptimized : boolean;
begin

  // initally use all image sequences for optimazion and initialize a list
  // which stores the result
  SetLength(basicResultList, Length(_imageSequences));
  for x := 0 to high(basicResultList) do
  begin
    basicResultList[x].useForOptimization := true;
    basicResultList[x].weight             := _imageSequences[x].resultWeight;

    basicResultList[x].result             := -1;
    basicResultList[x].successful         := false;
  end;
  _bestRun.Initialized := false;
  _runCounter          := 0;
  _evaluationCounter   := 0;



  // let the parameter selection control the optimization process
  _parameterSelection.runOptimization(basicResultList, @run, _bestRun);




  // compute results for remaining unused sequences if necessary
  onlySubsetOptimized := false;
  basicResultList  := Copy(_bestRun.run.resultList, 0, Length(_bestRun.run.resultList));
  for x := 0 to high(_bestRun.run.resultList) do
  begin

    // all image sequences that have been ignored so far shall now be evaluated
    // and vice versa
    basicResultList[x].useForOptimization := not _bestRun.run.resultList[x].useForOptimization;
    if basicResultList[x].useForOptimization then
    begin
      onlySubsetOptimized := true;
    end;
  end;

  // in case there is need for further evaluations
  if onlySubsetOptimized then
  begin

    // there is one set of parameters (the optimal ones!)
    SetLength(tempParamsList, 1);
    tempParamsList[0] := _bestRun.run.parameters;
    SetLength(currentResultMatrix, 1);
    currentResultMatrix[0].resultList := basicResultList;

    WriteAndLogLn('Compute results for disregarded sequences...');

    optimizationRun := _optimizationRunFactory.createInstance( _cmdLineParameters,
                                                   tempParamsList,
                                                   _imageSequences,
                                                   _optimizationEnvironment);
    try
      if optimizationRun.go(currentResultMatrix, _runCounter) then
      begin
        WriteAndLogLn('Re-compute average value...');

        // Now we have all results for the best parameters so the errors
        // are assumed to be the smallest ones
        for x := 0 to high(currentResultMatrix[0].resultList) do
        begin
          if currentResultMatrix[0].resultList[x].useForOptimization then
          begin
            _bestRun.run.resultList[x] := currentResultMatrix[0].resultList[x];
            inc(_evaluationCounter);
          end;
        end;

        // finally compute average error among ALL image sequences
        _bestRun.run.result := computeAvg(_bestRun.run.resultList);
        if Length(_bestRun.run.resultList) > 0 then
        begin
          SetLength(_bestRun.run.allErrors, Length(_bestRun.run.resultList[0].allErrors));
          for y := 0 to high(_bestRun.run.allErrors) do
          begin
            _bestRun.run.allErrors[y].name  := _bestRun.run.resultList[0].allErrors[y].name;
            _bestRun.run.allErrors[y].value := computeAvgError(_bestRun.run.resultList, y);
          end;
        end;
      end
      else
      begin
        WriteAndLogLn('...failed!');
      end;
    finally
      optimizationRun.Free;
    end;
  end;
end;

function TOptimizationProcess.getSmallestResult: extended;
begin
  result := _bestRun.run.result;
end;

function TOptimizationProcess.getOptimalParameterSet: TChosenParameterSet;
begin
  result := _bestRun.run.parameters;
end;

function TOptimizationProcess.getAllErrors: TErrorValueList;
begin
  result := _bestRun.run.allErrors;
end;

function TOptimizationProcess.getRunCounter: Cardinal;
begin
  result := _runCounter;
end;

function TOptimizationProcess.getEvaluationCounter: Cardinal;
begin
  result := _evaluationCounter;
end;

procedure TOptimizationProcess.interruptTasks;
begin
  _interrupted := true;
  if _currOptimizationRun <> nil then
  begin
    _currOptimizationRun.interruptTasks;
  end;
end;

end.

