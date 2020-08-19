unit uParameterSelection;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils,

  udatatypesOptimizer, uOptimizationRun;

type
  TOptimizationRunExecution = function (parameters: TChosenParameterSetList;
  imageSequenceResultList: TimageSequenceResultList
  ): TChosenParameterSetAndResultList of object;

  { TParameterSelection }

  TParameterSelection = class(TObject)
  protected

    // contains a list of defined parameters
    _parametersToOptimize : TParameterToOptimizeList;

    // method parameters of the respective parameter selection strategy
    _params : string;

    // parameter which defines which share of the image sequences with the most
    // varying results is used for advanced evaluation steps
    _varianceQuantile : Extended;

    // contains a list of sampled discrete parameters
    _discreteParameters : TChosenParameterSetList;


    // functions which are useful to separate discrete and continuous parameters
    function extractContinuousParameters(
      parametersToOptimize : TParameterToOptimizeList) : TParameterToOptimizeList;
    function extractDiscreteParameters(
      parametersToOptimize : TParameterToOptimizeList) : TParameterToOptimizeList;
    function sampleDiscreteParameters(parameters: TParameterToOptimizeList;
      maxSamples: Cardinal = 0): TChosenParameterSetList;
    procedure separateAndSampleDiscreteParameters(maxSamples: Cardinal = 0);


    // function which sets the "useForOptimization" flags for image sequences
    // according to their result variance and the value of "_varianceQuantile"
    function determineImageSequenceSubset(resultList: TImageSequenceResultList;
      var chosenParameterSetAndResultList: TChosenParameterSetAndResultList
    ): boolean;
  public
    constructor Create(parametersToOptimize : TParameterToOptimizeList;
      varianceQuantile : Extended; const params : string); virtual;

    // lets the respective parameter selection strategy choose parameter values
    // and perform evaluations based on these values
    procedure runOptimization(basicResultList : TimageSequenceResultList;
      executeRun : TOptimizationRunExecution;
      var bestRun : TOptimizationRunInfo); virtual; abstract;

    // the help text of the respective parameter selection strategy which is
    // displayed when calling the program with "--help" or "-h"
    class function helpText() : string; virtual;

    function getNumberOfParameters(): Integer;
  end;
  TParameterSelectionClass = class of TParameterSelection;
  TParameterSelectionClassList = array of TParameterSelectionClass;

  { TPSClassManager }

  TPSClassManager = class(TObject)
  protected
    type
      TPSClassEntry = record
        psClass : TParameterSelectionClass;
        name : string;
      end;

    var
      _registeredClasses : array of TPSClassEntry;
  public
    procedure registerClass(const name: string; psClass: TParameterSelectionClass);
    function getClass(const name : string) : TParameterSelectionClass;
    function getRegisteredName(const psClassName: string): string;
    function getClassList() : TParameterSelectionClassList;
  end;

const
  MIN_POSITIVE_FLOAT = 0.000000000001;

var
  psClassManager : TPSClassManager;


implementation

{ TPSClassManager }

procedure TPSClassManager.registerClass(const name: string;
  psClass: TParameterSelectionClass);
begin
  SetLength(_registeredClasses, length(_registeredClasses)+1);
  _registeredClasses[high(_registeredClasses)].name    := name;
  _registeredClasses[high(_registeredClasses)].psClass := psClass;
end;

function TPSClassManager.getClass(const name: string): TParameterSelectionClass;
var
  x : integer;
begin
  result := nil;
  for x := 0 to high(_registeredClasses) do
    if _registeredClasses[x].name = name then
    begin
      result := _registeredClasses[x].psClass;
      break;
    end;
end;

function TPSClassManager.getRegisteredName(const psClassName : string): string;
var
  x : integer;
begin
  result := '';
  for x := 0 to high(_registeredClasses) do
    if _registeredClasses[x].psClass.ClassName = psClassName then
    begin
      result := _registeredClasses[x].name;
      break;
    end;
end;

function TPSClassManager.getClassList: TParameterSelectionClassList;
var
  x : integer;
begin
  SetLength(result, length(_registeredClasses));
  for x := 0 to high(result) do
    result[x] := _registeredClasses[x].psClass;
end;

{ TParameterSelection }

function TParameterSelection.extractContinuousParameters(
  parametersToOptimize: TParameterToOptimizeList): TParameterToOptimizeList;
var
  x : integer;
begin
  SetLength(result, 0);
  for x := 0 to high(parametersToOptimize) do
  begin
    if (not parametersToOptimize[x].isEnumeration)
       and (parametersToOptimize[x].pType = ptFloat) then
    begin
      SetLength(result, Length(result)+1);
      result[high(result)] := parametersToOptimize[x];
    end;
  end;
end;

function TParameterSelection.extractDiscreteParameters(
  parametersToOptimize: TParameterToOptimizeList): TParameterToOptimizeList;
var
  x : integer;
begin
  SetLength(result, 0);
  for x := 0 to high(parametersToOptimize) do
  begin
    if (parametersToOptimize[x].isEnumeration)
       or (parametersToOptimize[x].pType = ptInteger)
       or (parametersToOptimize[x].pType = ptString) then
    begin
      SetLength(result, Length(result)+1);
      result[high(result)] := parametersToOptimize[x];
    end;
  end;
end;

function TParameterSelection.sampleDiscreteParameters(
  parameters: TParameterToOptimizeList; maxSamples: Cardinal = 0
  ): TChosenParameterSetList;
var
  x, y, z, idx, lastParam : integer;
  count, upperInt, lowerInt : integer;
  newValues : TStringDynArray;
  newList : TChosenParameterSetList;
  paramChoiceList : ^TRunParameterList;
begin
  // create parameter list
  if Length(parameters) > 0 then
  begin
    SetLength(result, 1);
    SetLength(result[0].modelParams, 0);
    SetLength(result[0].solverParams, 0);
  end
  else
    SetLength(result, 0);

  for x := 0 to high(parameters) do
  begin
    lastParam := x;
    SetLength(newValues, 0);
    try
      // generate list with new values for current parameter
      if parameters[x].isEnumeration then
      begin
        newValues := Copy(parameters[x].values, 0, Length(parameters[x].values));
      end
      else if parameters[x].pType = ptInteger then
      begin
        upperInt := StrToInt(parameters[x].values[1]);
        lowerInt := StrToInt(parameters[x].values[0]);
        count := upperInt - lowerInt + 1;
        if (count < maxSamples) or (maxSamples = 0) then
        begin
          SetLength(newValues, count);
          for y := lowerInt to upperInt do
            newValues[y - lowerInt] := IntToStr(y);
        end
        else
        begin
          SetLength(newValues, maxSamples);
          if maxSamples = 1 then
            newValues[0] := IntToStr(lowerInt)
          else
            for y := 0 to maxSamples-1 do
              newValues[y] := IntToStr(lowerInt + (y * (upperInt - lowerInt)) div (maxSamples - 1));
        end;
      end;


      // combine newValues with recent parameter combinations
      count := Length(result);
      SetLength(newList, count * Length(newValues));
      for y := 0 to count-1 do
      begin
        for z := 0 to high(newValues) do
        begin
          idx := y * Length(newValues) + z;

          newList[idx] := result[y].clone();
          if parameters[x].modelParam then
            paramChoiceList := @newList[idx].modelParams
          else
            paramChoiceList := @newList[idx].solverParams;

          SetLength(paramChoiceList^, Length(paramChoiceList^)+1);
          paramChoiceList^[high(paramChoiceList^)].name  := parameters[x].name;
          paramChoiceList^[high(paramChoiceList^)].value := newValues[z];
        end;
      end;
      result := newList;
    except
      raise Exception.Create('Malformed "discrete parameter", number: ' + IntToStr(lastParam+1));
    end;
  end;
end;

procedure TParameterSelection.separateAndSampleDiscreteParameters(
  maxSamples: Cardinal);
var
  discreteParameterList : TParameterToOptimizeList;
begin
  discreteParameterList := extractDiscreteParameters(_parametersToOptimize);
  _parametersToOptimize := extractContinuousParameters(_parametersToOptimize);

  _discreteParameters := sampleDiscreteParameters(discreteParameterList, maxSamples);
end;

function rangeCompare(Item1, Item2 : Pointer) : Integer;
begin
  if Extended(Item1^) < Extended(Item2^) then
    result := 1
  else if Extended(Item2^) < Extended(Item1^) then
    result := -1
  else
    result := 0;
end;

function rangeCompareOpposite(Item1, Item2 : Pointer) : Integer;
begin
  if Extended(Item1^) < Extended(Item2^) then
    result := -1
  else if Extended(Item2^) < Extended(Item1^) then
    result := 1
  else
    result := 0;
end;

function TParameterSelection.determineImageSequenceSubset(
  resultList: TImageSequenceResultList;
  var chosenParameterSetAndResultList: TChosenParameterSetAndResultList
  ): boolean;
var
  tempList : TList;
  x, z, y : integer;
  limitIdx : integer;
  limit : extended;
  ranges, maxResult, minResult : array of extended;
begin
  result := true;

  tempList := TList.Create;
  try
    z := high(chosenParameterSetAndResultList);

    // compute result ranges between best and worst parameter sets
    SetLength(ranges, Length(resultList));
    SetLength(maxResult, Length(resultList));
    SetLength(minResult, Length(resultList));
    for x := 0 to high(resultList) do
    begin
      maxResult[x] := chosenParameterSetAndResultList[0].resultList[x].result;
      minResult[x] := chosenParameterSetAndResultList[0].resultList[x].result;

      for y := 0 to z do
      begin
        if chosenParameterSetAndResultList[y].resultList[x].result > maxResult[x] then
          maxResult[x] := chosenParameterSetAndResultList[y].resultList[x].result;
        if chosenParameterSetAndResultList[y].resultList[x].result < minResult[x] then
          minResult[x] := chosenParameterSetAndResultList[y].resultList[x].result;
      end;

      if resultList[x].useForOptimization then
      begin
        ranges[x] := maxResult[x] - minResult[x];
        tempList.Add(@ranges[x]);
      end;
    end;

    // compute quantile
    tempList.Sort(@rangeCompare);
    //tempList.Sort(@rangeCompareOpposite);

    limitIdx := Trunc(tempList.Count * _varianceQuantile / 100);
    if limitIdx < 1 then
      exit;
    limit    := Extended(tempList[limitIdx-1]^);

    // mark irrelevant image sequences as unused
    for x := 0 to high(resultList) do
    begin
      if resultList[x].useForOptimization then
      begin
        resultList[x].useForOptimization := (ranges[x] >= limit);

        if not resultList[x].useForOptimization then
        begin
          for y := 0 to z do
            chosenParameterSetAndResultList[y].resultList[x].useForOptimization := false;
        end;
      end;
    end;

    // recompute average values
    for x := 0 to z do
    begin
      limit := chosenParameterSetAndResultList[x].result;
      chosenParameterSetAndResultList[x].result := computeAvg(chosenParameterSetAndResultList[x].resultList);
    end;
  finally
    tempList.Free;
  end;
end;

constructor TParameterSelection.Create(
  parametersToOptimize: TParameterToOptimizeList; varianceQuantile: Extended;
  const params: string);
begin
  self._varianceQuantile     := varianceQuantile;
  self._parametersToOptimize := parametersToOptimize;
  self._params               := params;
end;

class function TParameterSelection.helpText: string;
begin
  result := '';
end;

function TParameterSelection.getNumberOfParameters: Integer;
begin
  result := Length(_parametersToOptimize);
end;

initialization
  psClassManager := TPSClassManager.Create;

finalization
  psClassManager.Free;

end.

