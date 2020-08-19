unit udatatypesOptimizer;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode delphi}{$H+}

interface

uses
  Types, Classes, SysUtils;

type

  { TRunParameter }

  TFloatConstraints = (fcNone, fcNonnegative, fcPositive);
  TRunParameter = record
    name : string;
    value : string;
    floatConstraints : TFloatConstraints;
    function equals(t : TRunParameter) : boolean;
  end;
  TRunParameterList = array of TRunParameter;

  TImageSequenceData = class
    filenames, binaryFiles : TStringDynArray;
    size : Cardinal;
    numberOfRefFrame : Integer;
    groundTruthFile : string;
    resultWeight : extended;
    seqInfo : string;
  end;
  TImageSequenceDataList = array of TImageSequenceData;

  TDataReader = class
    class function createImageSequenceDataList(const input : string) : TImageSequenceDataList; virtual; abstract;
  end;

  { TChosenParameterSet }

  TChosenParameterSet = record
    modelParams,
    solverParams : TRunParameterList;
    function clone() : TChosenParameterSet;
    function equals(t : TChosenParameterSet) : boolean;
    function merge(parameters : TChosenParameterSet) : TChosenParameterSet;
  end;
  TChosenParameterSetList = array of TChosenParameterSet;

  TErrorValue = record
    name : string;
    value : Extended;
  end;
  TErrorValueList = array of TErrorValue;

  TImageSequenceResult = record
    result : extended;
    allErrors : TErrorValueList;
    successful : boolean;
    weight : extended;
    useForOptimization : boolean;
  end;
  TImageSequenceResultList = array of TImageSequenceResult;
  TImageSequenceResultMatrix = array of TImageSequenceResultList;

  { TChosenParameterSetAndResult }

  TChosenParameterSetAndResult = record
    parameters : TChosenParameterSet;
    result : extended;
    allErrors : TErrorValueList;
    resultList : TImageSequenceResultList;
    class function Create(_parameters : TChosenParameterSet;
      _resultList : TImageSequenceResultList) : TChosenParameterSetAndResult; static;
    function clone() : TChosenParameterSetAndResult;
  end;
  PChosenParameterSetAndResult = ^TChosenParameterSetAndResult;
  TChosenParameterSetAndResultList = array of TChosenParameterSetAndResult;
  PChosenParameterSetAndResultList = array of PChosenParameterSetAndResult;



  { TOptimizationRunInfo }

  TOptimizationRunInfo = record
    run : TChosenParameterSetAndResult;
    initialized : boolean;
    procedure update(resultList : TImageSequenceResultList);
  end;


  TParamType = (ptInteger, ptFloat, ptString);
  TParameterToOptimize = record
    name : string;
    pType : TParamType;
    values : TStringDynArray;
    isEnumeration : boolean;
    modelParam : boolean;
    //nonnegative : boolean;
    floatConstraints : TFloatConstraints;
  end;
  TParameterToOptimizeList = array of TParameterToOptimize;

function computeAvg(resultList : TImageSequenceResultList) : extended;
function computeAvgError(resultList : TImageSequenceResultList; errorNo : Integer) : extended;
function sequenceCount(resultList : TImageSequenceResultList) : Cardinal;

implementation

uses
  Math;

function computeAvg(resultList : TImageSequenceResultList) : extended;
var
  ir : TImageSequenceResult;
  counter : extended;
begin
  counter := 0;
  result  := 0;
  for ir in resultList do
  begin
    if ir.useForOptimization then
    begin
      if ir.successful then
      begin
        result  := result + ir.weight * ir.result;
        counter := counter + ir.weight;
      end
      else
      begin
        result := maxFloat;
        exit;
      end;
    end;
  end;

  if counter > 0 then
    result := result / counter;
end;

function computeAvgError(resultList: TImageSequenceResultList; errorNo: Integer
  ): extended;
var
  ir : TImageSequenceResult;
  counter : extended;
begin
  counter := 0;
  result  := 0;
  for ir in resultList do
  begin
    if ir.useForOptimization then
    begin
      if ir.successful then
      begin
        result  := result + ir.weight * ir.allErrors[errorNo].value;
        counter := counter + ir.weight;
      end
      else
      begin
        result := maxFloat;
        exit;
      end;
    end;
  end;

  result := result / counter;
end;

function sequenceCount(resultList: TImageSequenceResultList): Cardinal;
var
  ir : TImageSequenceResult;
begin
  result := 0;
  for ir in resultList do
    if ir.useForOptimization then
      inc(result);
end;

{ TOptimizationRunInfo }

procedure TOptimizationRunInfo.update(resultList: TImageSequenceResultList);
var
  x : Integer;
begin
  for x := 0 to high(resultList) do
    run.ResultList[x].useForOptimization := resultList[x].useForOptimization;
  run.result := computeAvg(run.resultList);
end;

{ TChosenParameterSetAndResult }

class function TChosenParameterSetAndResult.Create(_parameters: TChosenParameterSet;
  _resultList: TImageSequenceResultList): TChosenParameterSetAndResult;
var
  x : integer;
begin
  result.parameters := _parameters.clone();
  result.resultList := Copy(_resultList, 0, Length(_resultList));
  result.result     := computeAvg(result.resultList);
  if Length(result.resultList) > 0 then
  begin
    SetLength(result.allErrors, Length(result.resultList[0].allErrors));
    for x := 0 to high(result.allErrors) do
    begin
      result.allErrors[x].name  := result.resultList[0].allErrors[x].name;
      result.allErrors[x].value := computeAvgError(result.resultList, x);
    end;
  end;
end;

function TChosenParameterSetAndResult.clone: TChosenParameterSetAndResult;
begin
  result.parameters := parameters.clone();
  result.resultList := Copy(resultList, 0, Length(resultList));
  result.result     := self.result;
  result.allErrors  := Copy(allErrors, 0, Length(allErrors));
end;

{ TRunParameter }

function TRunParameter.equals(t: TRunParameter): boolean;
begin
  result := (name = t.name) and (value = t.value);
end;

{ TChosenParameterSet }

function TChosenParameterSet.clone: TChosenParameterSet;
begin
  result.modelParams := Copy(modelParams, 0, Length(modelParams));
  result.solverParams := Copy(solverParams, 0, Length(solverParams));
end;

function TChosenParameterSet.equals(t: TChosenParameterSet): boolean;
var
  x, y : integer;
  found : boolean;
begin
  result := (Length(modelParams) = Length(t.modelParams))
            and (Length(solverParams) = Length(t.solverParams));
  if result then
  begin
    for x := 0 to high(modelParams) do
    begin
      found := false;
      for y := 0 to high(t.modelParams) do
      begin
        if t.modelParams[y].equals(modelParams[x]) then
        begin
          found := true;
          break;
        end;
      end;

      if not found then
      begin
        result := false;
        exit;
      end;
    end;

    for x := 0 to high(solverParams) do
    begin
      found := false;
      for y := 0 to high(t.solverParams) do
      begin
        if t.solverParams[y].equals(solverParams[x]) then
        begin
          found := true;
          break;
        end;
      end;

      if not found then
      begin
        result := false;
        exit;
      end;
    end;
  end;
end;

function TChosenParameterSet.merge(parameters: TChosenParameterSet
  ): TChosenParameterSet;
var
  x : integer;
  oldLength : integer;
begin
  result := clone();

  // merge model parameters
  oldLength := Length(result.modelParams);
  SetLength(result.modelParams, Length(result.modelParams)+Length(parameters.modelParams));
  for x := 0 to high(parameters.modelParams) do
    result.modelParams[oldLength+x] := parameters.modelParams[x];

  // merge solver parameters
  oldLength := Length(result.solverParams);
  SetLength(result.solverParams, Length(result.solverParams)+Length(parameters.solverParams));
  for x := 0 to high(parameters.solverParams) do
    result.solverParams[oldLength+x] := parameters.solverParams[x];
end;


end.

