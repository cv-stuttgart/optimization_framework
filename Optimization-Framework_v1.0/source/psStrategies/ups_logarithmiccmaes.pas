unit uPS_LogarithmicCMAeS;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  uDatatypesOptimizer, uParameterSelection, uOptimizationRun,
  uPS_LogarithmicCascadic, uPS_CMAeS;

type

  { TRunExecution }

  TRunExecution = class
  public
    _discreteParameters : TChosenParameterSetList;
    _dpIdx : Integer;
    _executeRun : TOptimizationRunExecution;
    function executeRun(parameters: TChosenParameterSetList;
      imageSequenceResultList: TimageSequenceResultList
      ): TChosenParameterSetAndResultList;
  end;

  { TPS_logarithmicCMAeS }

  TPS_logarithmicCMAeS = class(TParameterSelection)
  protected
    // parameters of the method
    _numberOfSamples : Cardinal;
    _relativeError : Extended;

    // initially computed intervals
    _intervals : array of TInterval;

    // submethods
    _logCasc : TPS_logarithmicCascadic;
  public
    constructor Create(parametersToOptimize : TParameterToOptimizeList;
      varianceQuantile : Extended; const params : string); override;
    destructor Destroy; override;
    procedure runOptimization(basicResultList : TimageSequenceResultList;
      executeRun : TOptimizationRunExecution;
      var bestRun : TOptimizationRunInfo); override;
    class function helpText: string; override;
  end;

implementation

{ TRunExecution }

function TRunExecution.executeRun(parameters: TChosenParameterSetList;
  imageSequenceResultList: TimageSequenceResultList
  ): TChosenParameterSetAndResultList;
var
  x : Integer;
  currentParamsList : TChosenParameterSetList;
begin
  SetLength(currentParamsList, Length(parameters));
  for x := 0 to high(currentParamsList) do
    if _dpIdx <> -1 then
      currentParamsList[x] := parameters[x].merge(_discreteParameters[_dpIdx])
    else
      currentParamsList[x] := parameters[x].clone();

   result := _executeRun(currentParamsList, imageSequenceResultList);
end;

{ TPS_logarithmicCMAeS }

constructor TPS_logarithmicCMAeS.Create(
  parametersToOptimize: TParameterToOptimizeList; varianceQuantile: Extended;
  const params: string);
var
  paramsStr : TStringList;
  paramsLog : string;

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

  _numberOfSamples := 3;
  _relativeError   := 1.01;

  paramsStr := TStringList.Create;
  try
    paramsStr.Delimiter     := ';';
    paramsStr.DelimitedText := params;

    readNextParam( 0, 0,
                   [_numberOfSamples, _relativeError],
                   [@_numberOfSamples, @_relativeError]);
  finally
    paramsStr.Free;
  end;

  paramsLog := IntToStr(_numberOfSamples) + ';' + FloatToStr(_relativeError) + ';1';

  Writeln(paramsLog);

  separateAndSampleDiscreteParameters(_numberOfSamples);

  _logCasc := TPS_LogarithmicCascadic.Create(_parametersToOptimize, 100, paramsLog);
end;

destructor TPS_logarithmicCMAeS.Destroy;
begin
  _logCasc.Free;
  inherited Destroy;
end;

procedure TPS_logarithmicCMAeS.runOptimization(
  basicResultList: TimageSequenceResultList;
  executeRun: TOptimizationRunExecution; var bestRun: TOptimizationRunInfo);
var
  subsetComputed : boolean;
  runExecution : TRunExecution;
  parametersToOptimizeCMA: TParameterToOptimizeList;
  cmaES : TPS_CMAeS;
  d, startD, x, y : Integer;
  tmpRunParameterList : TRunParameterList;
begin
  subsetComputed := _varianceQuantile > 99.9;

  if Length(_discreteParameters) = 0 then
    startD := -1
  else
    startD := 0;

  for d := startD to high(_discreteParameters) do
  begin
    runExecution := TRunExecution.Create;
    try
      runExecution._discreteParameters := _discreteParameters;
      runExecution._dpIdx              := d;
      runExecution._executeRun         := executeRun;


      // run logarithmic optimization
      _logCasc.runOptimization(basicResultList, runExecution._executeRun, bestRun);


      // Initialize parameters for CMAeS with best values from logarithmic run
      SetLength(parametersToOptimizeCMA, Length(_parametersToOptimize));
      for x := 0 to high(_parametersToOptimize) do
      begin
        parametersToOptimizeCMA[x] := _parametersToOptimize[x];
        parametersToOptimizeCMA[x].values := Copy(_parametersToOptimize[x].values, 0, Length(_parametersToOptimize[x].values));
        SetLength(parametersToOptimizeCMA[x].values, 1);

        if parametersToOptimizeCMA[x].modelParam then
          tmpRunParameterList := bestRun.run.parameters.modelParams
        else
          tmpRunParameterList := bestRun.run.parameters.solverParams;

        for y := 0 to high(tmpRunParameterList) do
        begin
          if tmpRunParameterList[y].name = parametersToOptimizeCMA[x].name then
          begin
            parametersToOptimizeCMA[x].values[0] := tmpRunParameterList[y].value;
            break;
          end;
        end;
      end;

      // run CMAeS
      cmaEs   := TPS_CMAeS.Create(parametersToOptimizeCMA, _varianceQuantile, '');
      try
        cmaEs.runOptimization(basicResultList, runExecution._executeRun, bestRun);
      finally
        cmaES.Free;
      end;
    finally
      runExecution.Free;
    end;
  end;
end;

class function TPS_logarithmicCMAeS.helpText: string;
var
  entryName : string;
begin
  entryName := psClassManager.getRegisteredName(ClassName);
  result := ' - "' + entryName + '": "5;1.01"';
  result := result + #13#10 + '    + parameter 1: number of samples';
  result := result + #13#10 + '    + parameter 2: relative error';
end;

initialization
  psClassManager.registerClass('logarithmicCMAeS', TPS_logarithmicCMAeS);

end.

