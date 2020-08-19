program optimizerOF;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

{$DEFINE UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, SysUtils, Classes, uEvaluationTask, uOptimizationRun, {uTestcases,}
  uOptimizationProcess, uParameterSelection, uSimpleLog, uHelper,
  umain, uMath, udatatypesOptimizer, uEvaluationBinary,

  upsIncludeAll, uebIncludeAll, uDatatypesOF
  { you can add units after this };


begin
  with TProcessController.Create(optimizationRunFactory) do
  begin
    try
      Run;
    finally
      Free;
    end;
  end;
end.

