unit ups_equidistant;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils,

  udatatypesOptimizer, uParameterSelection, uOptimizationRun;

type

  { TPS_Equidistant }

  TPS_Equidistant = class(TParameterSelection)
  protected
    _numberOfSamples : Cardinal;
    _currIdx : Integer;
    _chosenParametersList : TChosenParameterSetList;
  public
    constructor Create(parametersToOptimize : TParameterToOptimizeList;
      varianceQuantile : Extended; const params : string); override;
    procedure runOptimization(basicResultList : TimageSequenceResultList;
      executeRun : TOptimizationRunExecution;
      var bestRun : TOptimizationRunInfo); override;
    class function helpText: string; override;

    function getSamples() : Cardinal;
    function getParametersList() : TChosenParameterSetList;
    procedure eliminateDuplicates(old: TPS_Equidistant);
  end;

implementation

procedure deleteArrItem(var t : TChosenParameterSetList; idx : Integer);
var
  x : integer;
begin
  for x := idx to high(t)-1 do
    t[x] := t[x+1];
  SetLength(t, Length(t)-1);
end;

{ TPS_Equidistant }

constructor TPS_Equidistant.Create(
  parametersToOptimize: TParameterToOptimizeList; varianceQuantile: Extended;
  const params: string);
var
  x, y, z, idx, lastParam : integer;
  count : integer;
  upperFloat, lowerFloat, temp : Extended;
  newValues : TStringDynArray;
  newList : TChosenParameterSetList;
  paramChoiceList : ^TRunParameterList;
begin
  inherited Create(parametersToOptimize, varianceQuantile, params);

  try
    _numberOfSamples := StrToInt(params);
  except
    raise Exception.Create('TPS_Equidistant: Invalid number of samples (' + params + ')');
  end;
  _currIdx         := 0;

  separateAndSampleDiscreteParameters(_numberOfSamples);
  _chosenParametersList := Copy(_discreteParameters, 0, Length(_discreteParameters));

  // if not done yet, initialize _dynamicParametersList
  if (Length(_chosenParametersList) = 0)
    and (Length(_parametersToOptimize) > 0) then
  begin
    SetLength(_chosenParametersList, 1);
    SetLength(_chosenParametersList[0].modelParams, 0);
    SetLength(_chosenParametersList[0].solverParams, 0);
  end;

  // sample continuous parameters
  for x := 0 to high(_parametersToOptimize) do
  begin
    lastParam := x;
    SetLength(newValues, 0);
    try
      // generate list with new values for current parameter
      if _parametersToOptimize[x].pType = ptFloat then
      begin
        upperFloat := StrToFloat(_parametersToOptimize[x].values[1]);
        lowerFloat := StrToFloat(_parametersToOptimize[x].values[0]);
        SetLength(newValues, _numberOfSamples);
        if _numberOfSamples = 1 then
          newValues[0] := FloatToStr(lowerFloat)
        else
          for y := 0 to _numberOfSamples-1 do
          begin
            temp := lowerFloat + (y * (upperFloat - lowerFloat)) / (_numberOfSamples - 1);
            if (_parametersToOptimize[x].floatConstraints = fcNonnegative) and (temp < 0) then
              temp := 0
            else if (_parametersToOptimize[x].floatConstraints = fcPositive) and (temp <= 0) then
              temp := MIN_POSITIVE_FLOAT;
            newValues[y] := FloatToStr(temp);
          end;
      end
      else
         raise Exception.Create('');


      // combine newValues with recent parameter combinations
      count := Length(_chosenParametersList);
      SetLength(newList, count * Length(newValues));
      for y := 0 to count-1 do
      begin
        for z := 0 to high(newValues) do
        begin
          idx := y * Length(newValues) + z;

          newList[idx] := _chosenParametersList[y].clone();
          if _parametersToOptimize[x].modelParam then
            paramChoiceList := @newList[idx].modelParams
          else
            paramChoiceList := @newList[idx].solverParams;

          SetLength(paramChoiceList^, Length(paramChoiceList^)+1);
          paramChoiceList^[high(paramChoiceList^)].name  := _parametersToOptimize[x].name;
          paramChoiceList^[high(paramChoiceList^)].value := newValues[z];
        end;
      end;
      _chosenParametersList := newList;
    except
      raise Exception.Create('Malformed continuous parameter, number: ' + IntToStr(lastParam+1));
    end;
  end;
end;

procedure TPS_Equidistant.runOptimization(
  basicResultList: TimageSequenceResultList;
  executeRun: TOptimizationRunExecution; var bestRun: TOptimizationRunInfo);
begin
  executeRun(_chosenParametersList, basicResultList);
end;

function TPS_Equidistant.getSamples: Cardinal;
begin
  result := _numberOfSamples;
end;

function TPS_Equidistant.getParametersList: TChosenParameterSetList;
begin
  result := _chosenParametersList;
end;

procedure TPS_Equidistant.eliminateDuplicates(old: TPS_Equidistant);
var
  x, y : Integer;
begin
  for x := high(_chosenParametersList) downto 0 do
  begin
    for y := 0 to high(old._chosenParametersList) do
    begin
      if _chosenParametersList[x].equals(old._chosenParametersList[y]) then
      begin
        deleteArrItem(_chosenParametersList, x);
        break;
      end;
    end;
  end;
end;

class function TPS_Equidistant.helpText: string;
var
  entryName : string;
begin
  entryName := psClassManager.getRegisteredName(ClassName);
  result := ' - "' + entryName + '": "5"';
  result := result + #13#10 + '    + parameter 1: number of samples';
end;

initialization
  psClassManager.registerClass('equidistant', TPS_Equidistant);

end.

