unit ups_evaluateFromLog;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  uDatatypesOptimizer, uParameterSelection, uOptimizationRun;

type

  { TPS_EvaluateFromLog }

  TPS_EvaluateFromLog = class(TParameterSelection)
  public
    procedure runOptimization(basicResultList : TimageSequenceResultList;
      executeRun : TOptimizationRunExecution;
      var bestRun : TOptimizationRunInfo); override;
    class function helpText() : string; override;
  end;

implementation

{ TPS_EvaluateFromLog }

procedure TPS_EvaluateFromLog.runOptimization(
  basicResultList: TimageSequenceResultList;
  executeRun: TOptimizationRunExecution; var bestRun: TOptimizationRunInfo);
var
  paramChoice : TRunParameter;
  paramChoiceList : ^TRunParameterList;
  usedParameterSet : TChosenParameterSetList;
  lines : TStringList;
  x, y, count : integer;
  tmpName, tmpValue : string;
begin
  SetLength(usedParameterSet, 1);

  SetLength(usedParameterSet[0].modelParams, 0);
  SetLength(usedParameterSet[0].solverParams, 0);

  if not fileexists(_params) then
    exit;

  lines := TStringList.Create;
  try
    lines.LoadFromFile(_params);
    for x := lines.Count-1 downto 0 do
    begin
      if trim(lines[x]) = 'Dynamic Params:' then
      begin
        for y := x+1 to lines.count-1 do
        begin
          lines[y] := StringReplace(lines[y], ',', '', []);
          count := sscanf(lines[y], '- Name: %s', [@tmpName]);
          //Writeln('Count: ', count);
          if count < 1 then
            break;

          count := sscanf(lines[y], '- Name: ' + tmpName + ' Value: %s', [@tmpValue]);
          if count < 1 then
            break;


          //Writeln('Name: ', tmpName);
          //Writeln('Value: ', tmpValue);
          //Writeln(lines[y]);
          paramChoice.name  := tmpName;
          paramChoice.value := trim(tmpValue);
          if _parametersToOptimize[y-(x+1)].modelParam then
            paramChoiceList := @usedParameterSet[0].modelParams
          else
            paramChoiceList := @usedParameterSet[0].solverParams;

          SetLength(paramChoiceList^, Length(paramChoiceList^)+1);
          paramChoiceList^[high(paramChoiceList^)] := paramChoice;
        end;
        break;
      end;
    end;
  finally
    lines.Free;
  end;

  executeRun(usedParameterSet, basicResultList);
end;

class function TPS_EvaluateFromLog.helpText: string;
var
  entryName : string;
begin
  entryName := psClassManager.getRegisteredName(ClassName);
  result := ' - "' + entryName + '"';
end;

initialization
  psClassManager.registerClass('evaluateFromLog', TPS_EvaluateFromLog);

end.

