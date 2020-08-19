unit ups_logarithmiccascadic;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Types,

  uDatatypesOptimizer, uParameterSelection, uOptimizationRun;


type

  TInterval = record
    a, b, d : Extended;
    isModelParam : boolean;
    name : string;
  end;

  TChosenParameterSetAndResultAndIndices = record
    paramsAndResult : TChosenParameterSetAndResult;
    indices : TIntegerDynArray;
  end;
  TChosenParameterSetAndResultAndIndicesList = array of TChosenParameterSetAndResultAndIndices;

  { TChosenParameterSetAndIndices }

  TChosenParameterSetAndIndices = record
    modelParams : TRunParameterList;
    solverParams : TRunParameterList;
    indices : TIntegerDynArray;
    function clone() : TChosenParameterSetAndIndices;
    function getParams() : TChosenParameterSet;
  end;
  TDynamicParametersAndIndicesList = array of TChosenParameterSetAndIndices;

  { TPS_logarithmicCascadic }

  TPS_logarithmicCascadic = class(TParameterSelection)
  protected
    // parameters of the method
    _numberOfSamples, _maxIterations : Cardinal;
    _relativeError : Extended;

    // initially computed intervals
    _intervals : array of TInterval;
  public
    constructor Create(parametersToOptimize : TParameterToOptimizeList;
      varianceQuantile : Extended; const params : string); override;
    procedure runOptimization(basicResultList : TimageSequenceResultList;
      executeRun : TOptimizationRunExecution;
      var bestRun : TOptimizationRunInfo); override;
    class function helpText: string; override;
  end;

implementation

uses
  Math, uSimpleLog;

{ TChosenParameterSetAndIndices }

function TChosenParameterSetAndIndices.clone: TChosenParameterSetAndIndices;
begin
  result.modelParams := Copy(modelParams, 0, Length(modelParams));
  result.solverParams := Copy(solverParams, 0, Length(solverParams));
  result.indices := Copy(indices, 0, Length(indices));
end;

function TChosenParameterSetAndIndices.getParams: TChosenParameterSet;
begin
  result.modelParams := Copy(modelParams, 0, Length(modelParams));
  result.solverParams := Copy(solverParams, 0, Length(solverParams));
end;

{ TPS_logarithmicCascadic }

constructor TPS_logarithmicCascadic.Create(
  parametersToOptimize: TParameterToOptimizeList; varianceQuantile: Extended;
  const params: string);
var
  paramsStr : TStringList;
  x, lastParam : integer;

  procedure readFloat(var value : extended; idx : Integer);
  begin
    value := StrToFloat(paramsStr[idx]);
  end;

  procedure readBool(var value : boolean; idx : Integer);
  begin
    value := (trim(paramsStr[idx]) <> '0') and (trim(paramsStr[idx]) <> '');
  end;

  procedure readInt(var value : integer; idx : Integer);
  begin
    value := StrToInt(paramsStr[idx]);
  end;

  procedure readNextParam(strIdx, arrIdx : Integer; paramList, pointerList : array of const);
  begin
    if (paramsStr.Count > strIdx) and (Length(paramList) > arrIdx) then
    begin
      try
        if paramList[arrIdx].VType = vtExtended then
          readFloat(extended(pointerList[arrIdx].VPointer^), strIdx)
        else if paramList[arrIdx].VType = vtInteger then
          readInt(integer(pointerList[arrIdx].VPointer^), strIdx)
        else if paramList[arrIdx].VType = vtBoolean then
          readBool(boolean(pointerList[arrIdx].VPointer^), strIdx);

      except
        raise Exception.Create('Invalid parameter ' + IntToStr(strIdx+1) + ' for "Logarithmic-Cascadic"!');
      end;

      readNextParam(strIdx+1, arrIdx+1, paramList, pointerList);
    end;
  end;
begin
  inherited Create(parametersToOptimize, varianceQuantile, params);

  _numberOfSamples := 5;
  _relativeError   := 1.01;
  _maxIterations   := 4;

  paramsStr := TStringList.Create;
  try
    paramsStr.Delimiter     := ';';
    paramsStr.DelimitedText := params;

    readNextParam( 0, 0,
                   [_numberOfSamples, _relativeError, _maxIterations],
                   [@_numberOfSamples, @_relativeError, @_maxIterations]);

    WriteAndLogln('LC: number of samples: ' + IntToStr(_numberOfSamples));
    WriteAndLogln('LC: relative error: ' + FloatToStr(_relativeError));
    WriteAndLogln('LC: max iterations: ' + FloatToStr(_maxIterations));
  finally
    paramsStr.Free;
  end;

  separateAndSampleDiscreteParameters(_numberOfSamples);

  SetLength(_intervals, Length(_parametersToOptimize));
  try
    for x := 0 to high(_parametersToOptimize) do
    begin
      lastParam := x;
      _intervals[x].a := StrToFloat(_parametersToOptimize[x].values[0]);
      _intervals[x].b := StrToFloat(_parametersToOptimize[x].values[1]);
      _intervals[x].d := StrToFloat(_parametersToOptimize[x].values[2]);

      _intervals[x].isModelParam := _parametersToOptimize[x].modelParam;
      _intervals[x].name         := _parametersToOptimize[x].name;
    end;
  except
    raise Exception.Create('Malformed "parameter to optimize", number: ' + IntToStr(lastParam+1));
  end;
end;

procedure TPS_logarithmicCascadic.runOptimization(
  basicResultList: TimageSequenceResultList;
  executeRun: TOptimizationRunExecution; var bestRun: TOptimizationRunInfo);
var
  x, y, z, i, d, startD, idx, count : integer;
  newList, oldList : TDynamicParametersAndIndicesList;
  samples : TSingleDynArray;
  paramChoiceList : ^TRunParameterList;
  bestResult, worstResult : Extended;
  bestResultIdx, currSampleIdx : Integer;
  tmpInterval : TInterval;
  currentSets : TChosenParameterSetAndResultAndIndicesList;
  imageSubsetDetermined : boolean;
  currentIntervals : array of TInterval;

  // copy results for all image sets in extended data structure
  procedure copyResults(src: TChosenParameterSetAndResult;
  var
    entry : TChosenParameterSetAndResultAndIndices);
  begin
    entry.paramsAndResult.resultList := Copy(src.resultList, 0, Length(src.resultList));
    entry.paramsAndResult.result     := src.result;
  end;


  // determine results for currently chosen parameter sets
  procedure executeRuns();
  var
    currentParamsList : TChosenParameterSetList;
    currentResultMatrix : TChosenParameterSetAndResultList;
    x : Integer;
  begin
    SetLength(currentParamsList, Length(currentSets));
    for x := 0 to high(currentParamsList) do
      if d <> -1 then
        currentParamsList[x] := currentSets[x].paramsAndResult.parameters.merge(_discreteParameters[d])
      else
        currentParamsList[x] := currentSets[x].paramsAndResult.parameters.clone();

    currentResultMatrix := executeRun(currentParamsList, basicResultList);

    for x := 0 to high(currentParamsList) do
      copyResults(currentResultMatrix[x], currentSets[x]);
  end;

  // determine subset of image sequences for further evaluations
  function determineImageSequenceSubset() : boolean;
  var
    currentResults : TChosenParameterSetAndResultList;
    x : Integer;
  begin
    SetLength(currentResults, Length(currentSets));
    for x := 0 to high(currentResults) do
      currentResults[x] := currentSets[x].paramsAndResult;

    result := self.determineImageSequenceSubset(basicResultList, currentResults);

    for x := 0 to high(currentResults) do
      copyResults(currentResults[x], currentSets[x]);
  end;

begin
  if Length(_discreteParameters) = 0 then
    startD := -1
  else
    startD := 0;

  imageSubsetDetermined := false;

  for d := startD to high(_discreteParameters) do
  begin
    SetLength(currentSets, 0);
    currentIntervals := Copy(_intervals, 0, Length(_intervals));

    for i := 0 to _maxIterations-1 do
    begin

      // determine new intervals in a logarithmic fashion
      if i > 0 then
      begin
        if (not imageSubsetDetermined) and determineImageSequenceSubset() then
        begin
          imageSubsetDetermined := true;
          bestRun.update(basicResultList);
        end;

        // determine best and worst results
        bestResult := Math.MaxExtended;
        worstResult := 0;
        bestResultIdx := -1;
        for x := 0 to high(currentSets) do
        begin
          if currentSets[x].paramsAndResult.result > worstResult then
            worstResult := currentSets[x].paramsAndResult.result;

          if currentSets[x].paramsAndResult.result < bestResult then
          begin
            bestResult := currentSets[x].paramsAndResult.result;
            bestResultIdx := x;
          end;
        end;

        // check early convergence
        // - checking for Math.MaxExtended is necessary to avoid overflow
        //   exceptions in the subsequent division
        if (bestResultIdx <> -1)
           and (worstResult <> Math.MaxExtended)
           and (bestResult <> 0)
           and ((worstResult / bestResult) <= _relativeError) then
          break;

        // compute next intervals for parameters
        for x := 0 to high(_intervals) do
        begin
          tmpInterval := currentIntervals[x];

          currSampleIdx := currentSets[bestResultIdx].indices[x];

          currentIntervals[x].a := tmpInterval.a + (tmpInterval.b - tmpInterval.a) / power(tmpInterval.d, currSampleIdx+2);
          currentIntervals[x].b := tmpInterval.a + (tmpInterval.b - tmpInterval.a) / power(tmpInterval.d, currSampleIdx+0);
          currentIntervals[x].d := sqrt(tmpInterval.d);
        end;
      end;

      // create parameter list
      if Length(currentIntervals) > 0 then
      begin
        SetLength(oldList, 1);
        SetLength(oldList[0].modelParams, 0);
        SetLength(oldList[0].solverParams, 0);
        SetLength(oldList[0].indices, 0);
      end
      else
        SetLength(oldList, 0);

      // sample each interval
      for x := 0 to high(currentIntervals) do
      begin
        SetLength(samples, _numberOfSamples);

        for y := 0 to _numberOfSamples-1 do
          samples[y] := currentIntervals[x].a + (currentIntervals[x].b - currentIntervals[x].a) / power(currentIntervals[x].d, y+1);



        // combine newValues with recent parameter combinations
        count := Length(oldList);
        SetLength(newList, count * Length(samples));

        for y := 0 to count-1 do
        begin
          for z := 0 to high(samples) do
          begin
            idx := y*Length(samples)+z;

            newList[idx] := oldList[y].clone();
            if currentIntervals[x].isModelParam then
              paramChoiceList := @newList[idx].modelParams
            else
              paramChoiceList := @newList[idx].solverParams;

            SetLength(paramChoiceList^, Length(paramChoiceList^)+1);
            paramChoiceList^[high(paramChoiceList^)].name  := currentIntervals[x].name;
            paramChoiceList^[high(paramChoiceList^)].value := FloatToStr(samples[z]);

            SetLength(newList[idx].indices, Length(newList[idx].indices)+1);
            newList[idx].indices[high(newList[idx].indices)] := z;
          end;
        end;
        oldList := newList;
      end;


      // assemble all information in order to execute runs
      SetLength(currentSets, Length(oldList));
      for x := 0 to high(oldList) do
      begin
        currentSets[x].paramsAndResult.parameters := oldList[x].getParams();
        currentSets[x].paramsAndResult.resultList := nil;
        currentSets[x].paramsAndResult.result     := Math.MaxExtended;

        currentSets[x].indices    := Copy(oldList[x].indices, 0, Length(oldList[x].indices));
      end;

      // execute runs
      executeRuns();
    end;
  end;
end;

class function TPS_logarithmicCascadic.helpText: string;
var
  entryName : string;
begin
  entryName := psClassManager.getRegisteredName(ClassName);
  result := ' - "' + entryName + '": "5;1.01;4"';
  result := result + #13#10 + '    + parameter 1: number of samples';
  result := result + #13#10 + '    + parameter 2: relative error';
  result := result + #13#10 + '    + parameter 3: maximum number of iterations';
end;

initialization
  psClassManager.registerClass('logarithmicCascadic', TPS_logarithmicCascadic);

end.

