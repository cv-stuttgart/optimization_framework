unit ups_equidistantCascadic;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  udatatypesOptimizer, uParameterSelection, uOptimizationRun,

  ups_Equidistant;

type

  { TPS_EquidistantCascadic }

  TPS_EquidistantCascadic = class(TParameterSelection)
  protected
    _samplesInt : Integer;
    _samples : string;
    _depth : Integer;
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
  uSimpleLog;

{ TPS_EquidistantCascadic }

constructor TPS_EquidistantCascadic.Create(
  parametersToOptimize: TParameterToOptimizeList;
  varianceQuantile : Extended; const params: string);
var
  paramList : TStringList;
begin
  inherited Create(parametersToOptimize, varianceQuantile, params);

  paramList := TStringList.Create;
  try
    paramList.Delimiter     := ';';
    paramList.DelimitedText := params;

    _samples := paramList[0];
    try
      _samplesInt := StrToInt(_samples);
    except
      raise Exception.Create('TPS_EquidistantCascadic: Invalid number of samples (' + params + ')');
    end;

    try
      _depth := StrToInt(paramList[1]);
    except
      raise Exception.Create('TPS_EquidistantCascadic: Invalid depth (' + params + ')');
    end;

    separateAndSampleDiscreteParameters(_samplesInt);
  finally
    paramList.Free;
  end;
end;

function truncFloatStr(const fmt, value : string) : string;
begin
  result := Format(fmt, [StrToFloat(value)]);
end;

function makeFullCopy(src : TParameterToOptimizeList) : TParameterToOptimizeList;
var
  x : integer;
begin
  result := Copy(src, 0, Length(src));
  for x := 0 to high(result) do
    result[x].values := Copy(result[x].values, 0, Length(result[x].values));
end;

procedure TPS_EquidistantCascadic.runOptimization(
  basicResultList: TimageSequenceResultList;
  executeRun: TOptimizationRunExecution; var bestRun: TOptimizationRunInfo);
var
  psLevel : array of TPS_Equidistant;
  tmpParameters, tmpParameters2, parametersBase : TParameterToOptimizeList;
  tempList : TRunParameterList;
  currentParametersList : TChosenParameterSetList;
  currentResultMatrix : TChosenParameterSetAndResultList;
  x, y, d, e, f, h, startE, min_idx : integer;
  min_value, tmp_value : extended;
  imageSubsetDetermined : boolean;
  currValueFloat, lowerFloat, upperFloat, stepFloat : Extended;
  currValueInt, lowerInt, upperInt, stepInt : Integer;
  samples : Cardinal;
begin
  if _depth < 1 then
    exit;

  SetLength(psLevel, _depth);

  imageSubsetDetermined := false;

  if Length(_discreteParameters) = 0 then
    startE := -1
  else
    startE := 0;

  for e := startE to high(_discreteParameters) do
  begin
    for d := 0 to _depth-1 do
      psLevel[d] := nil;

    parametersBase := Copy(_parametersToOptimize, 0, Length(_parametersToOptimize));

    // merge chosen set of discrete parameters
    if e > -1 then
    begin
      h := Length(parametersBase);
      SetLength(parametersBase, Length(parametersBase)+Length(_discreteParameters[e].modelParams));
      for f := 0 to high(_discreteParameters[e].modelParams) do
      begin
        SetLength(parametersBase[h+f].values, 1);

        parametersBase[h+f].modelParam    := true;
        parametersBase[h+f].isEnumeration := true;
        parametersBase[h+f].pType         := ptFloat;
        parametersBase[h+f].name          := _discreteParameters[e].modelParams[f].name;
        parametersBase[h+f].floatConstraints   := _discreteParameters[e].modelParams[f].floatConstraints;
        parametersBase[h+f].values[0]     := _discreteParameters[e].modelParams[f].value;
      end;

      h := Length(parametersBase);
      SetLength(parametersBase, Length(parametersBase)+Length(_discreteParameters[e].solverParams));
      for f := 0 to high(_discreteParameters[e].solverParams) do
      begin
        SetLength(parametersBase[h+f].values, 1);

        parametersBase[h+f].modelParam    := true;
        parametersBase[h+f].isEnumeration := true;
        parametersBase[h+f].pType         := ptFloat;
        parametersBase[h+f].name          := _discreteParameters[e].solverParams[f].name;
        parametersBase[h+f].floatConstraints   := _discreteParameters[e].solverParams[f].floatConstraints;
        parametersBase[h+f].values[0]     := _discreteParameters[e].solverParams[f].value;
      end;
    end;

    tmpParameters2 := makeFullCopy(parametersBase);

    // initialize coarsest level of cascade
    psLevel[0] := TPS_Equidistant.Create(tmpParameters2, _varianceQuantile, _samples);
    try
      // evaluate the coarsest level
      currentParametersList := psLevel[0].getParametersList();

      currentResultMatrix := executeRun(currentParametersList, basicResultList);
      samples := psLevel[0].getSamples();
      if samples < 2 then
        exit;

      // Copy all parameters into a temporary array to be modified later
      tmpParameters := Copy(parametersBase, 0, Length(parametersBase));

      // if intended, determine a subset of images to continue with
      if (not imageSubsetDetermined) and determineImageSequenceSubset(basicResultList, currentResultMatrix) then
      begin
        imageSubsetDetermined := true;
        bestRun.update(basicResultList);
      end;

      // go on with finer levels
      for d := 1 to _depth-1 do
      begin
        if Length(currentResultMatrix) < 2 then
          break;

        WriteAndLogLn('Go to next cascadic depth...');

        // identify point with best result
        min_value := currentResultMatrix[0].result;
        min_idx   := 0;
        for x := 1 to high(currentResultMatrix) do
        begin
          tmp_value := currentResultMatrix[x].result;
          if tmp_value < min_value then
          begin
            min_value := tmp_value;
            min_idx   := x;
          end;
        end;

        // make a local copy of the parameter list for next level
        tmpParameters := makeFullCopy(tmpParameters);

        // create next interval of values for all parameters
        for x := 0 to high(tmpParameters) do
        begin
          if tmpParameters[x].modelParam then
            tempList := currentParametersList[min_idx].modelParams
          else
            tempList := currentParametersList[min_idx].solverParams;

          for y := 0 to high(tempList) do
          begin
            if tempList[y].name = tmpParameters[x].name then
            begin
              if not tmpParameters[x].isEnumeration then
              begin
                if tmpParameters[x].pType = ptFloat then
                begin
                  currValueFloat := StrToFloat(tempList[y].value);
                  lowerFloat     := StrToFloat(tmpParameters[x].values[0]);
                  upperFloat     := StrToFloat(tmpParameters[x].values[1]);

                  stepFloat      := (upperFloat - lowerFloat) / (samples - 1);

                  lowerFloat := currValueFloat - stepFloat;
                  if (tmpParameters[x].floatConstraints = fcNonnegative) and (lowerFloat < 0) then
                    lowerFloat := 0
                  else if (tmpParameters[x].floatConstraints = fcPositive) and (lowerFloat <= 0) then
                    lowerFloat := MIN_POSITIVE_FLOAT;
                  upperFloat := currValueFloat + stepFloat;

                  tmpParameters[x].values[0] := FloatToStr(lowerFloat);
                  tmpParameters[x].values[1] := FloatToStr(upperFloat);
                end
                else if tmpParameters[x].pType = ptInteger then
                begin
                  currValueInt   := StrToInt(tempList[y].value);
                  lowerInt       := StrToInt(tmpParameters[x].values[0]);
                  upperInt       := StrToInt(tmpParameters[x].values[1]);

                  stepInt        := (upperInt - lowerInt) div (samples - 1);

                  lowerInt := currValueInt - stepInt;
                  upperInt := currValueInt + stepInt;

                  tmpParameters[x].values[0] := IntToStr(lowerInt);
                  tmpParameters[x].values[1] := IntToStr(upperInt);
                end;
                WriteAndLogLn( '- name: ' + tempList[y].name
                               + ', lower: ' + truncFloatStr('%4.3f', tmpParameters[x].values[0])
                               + ', upper: ' + truncFloatStr('%4.3f', tmpParameters[x].values[1]));
              end;
              break;
            end;
          end;
        end;

        // create next level eliminate duplicates
        tmpParameters2 := makeFullCopy(tmpParameters);

        psLevel[d] := TPS_Equidistant.Create(tmpParameters2, _varianceQuantile, _samples);
        psLevel[d].eliminateDuplicates(psLevel[d-1]);

        // let it run
        currentParametersList := psLevel[d].getParametersList();
        currentResultMatrix := executeRun(currentParametersList, basicResultList);
      end;
    finally
      for d := 0 to _depth-1 do
        if psLevel[d] <> nil then
          psLevel[d].Free;
    end;
  end;
end;

class function TPS_EquidistantCascadic.helpText: string;
var
  entryName : string;
begin
  entryName := psClassManager.getRegisteredName(ClassName);
  result := ' - "' + entryName + '": "5;3"';
  result := result + #13#10 + '    + parameter 1: number of samples';
  result := result + #13#10 + '    + parameter 2: depth of cascade';
end;

initialization
  psClassManager.registerClass('equidistantCascadic', TPS_EquidistantCascadic);

end.

