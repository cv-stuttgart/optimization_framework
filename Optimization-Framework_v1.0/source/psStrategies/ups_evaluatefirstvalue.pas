unit ups_evaluateFirstValue;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  uDatatypesOptimizer, uParameterSelection, uOptimizationRun;

type

  { TPS_EvaluateFirstValue }

  TPS_EvaluateFirstValue = class(TParameterSelection)
  public
    procedure runOptimization(basicResultList : TimageSequenceResultList;
      executeRun : TOptimizationRunExecution;
      var bestRun : TOptimizationRunInfo); override;
    class function helpText() : string; override;
  end;

implementation

{ TPS_EvaluateFirstValue }

procedure TPS_EvaluateFirstValue.runOptimization(
  basicResultList: TimageSequenceResultList;
  executeRun: TOptimizationRunExecution; var bestRun: TOptimizationRunInfo);
var
  paramTemplate : TParameterToOptimize;
  paramChoice : TRunParameter;
  paramChoiceList : ^TRunParameterList;
  usedParameterSet : TChosenParameterSetList;
begin
  SetLength(usedParameterSet, 1);

  SetLength(usedParameterSet[0].modelParams, 0);
  SetLength(usedParameterSet[0].solverParams, 0);
  for paramTemplate in _parametersToOptimize do
  begin
    paramChoice.name := paramTemplate.name;
    if paramTemplate.values <> nil then
    begin
      paramChoice.value := paramTemplate.values[0];
      if paramTemplate.modelParam then
        paramChoiceList := @usedParameterSet[0].modelParams
      else
        paramChoiceList := @usedParameterSet[0].solverParams;

      SetLength(paramChoiceList^, Length(paramChoiceList^)+1);
      paramChoiceList^[high(paramChoiceList^)] := paramChoice;
    end;
  end;

  executeRun(usedParameterSet, basicResultList);
end;

class function TPS_EvaluateFirstValue.helpText: string;
var
  entryName : string;
begin
  entryName := psClassManager.getRegisteredName(ClassName);
  result := ' - "' + entryName + '"';
end;

initialization
  psClassManager.registerClass('evaluateFirstValue', TPS_EvaluateFirstValue);

end.

