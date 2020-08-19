unit uebExample;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, StrUtils,

  uDatatypesOptimizer, uEvaluationBinary;

type
  { TEB_Empty }

  TEB_Empty = class(TEvaluationBinary)
    class procedure executeProcess(const cmdLine, workingDir: string;
      verbose: boolean; deadline: TDateTime; out successful, systemError: boolean; out output,
      errorMsg: string; out errorValues: TErrorValueList); override;
    class function getCmdLineParameters(chosenParameters: TChosenParameterSet;
      const cmdLineParameters: string; imageSequenceData: TImageSequenceData
      ): string; override;
  end;

implementation

{ TEB_Empty }

class procedure TEB_Empty.executeProcess(const cmdLine, workingDir: string;
  verbose: boolean; deadline: TDateTime; out successful, systemError: boolean;
  out output, errorMsg: string; out errorValues: TErrorValueList);
begin
  // TO DO
end;

class function TEB_Empty.getCmdLineParameters(
  chosenParameters: TChosenParameterSet; const cmdLineParameters: string;
  imageSequenceData: TImageSequenceData): string;
begin
  // TO DO
end;

initialization

  // TO ADAPT
  ebClassManager.registerClass('ofEmpty', TEB_Empty);

end.

