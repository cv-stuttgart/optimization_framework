unit uClientTask;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TClientTask }

  TClientTask = record
    // meta information
    ID : string;
    runTimeFactor : Double;
    optimizationID : string;

    // execution information
    workingDirectory : string;
    evaluationBinary : string;
    cmdLine : string;
    deadline : TDateTime;

    // state information
    clientIdx, threadIdx : Integer;
    clientIP, clientID : string;
    start, finish : TDateTime;
    computationRunning : boolean;

    // result information
    success : boolean;
    failureCounter : Integer;
    deadlineCounter : Integer;
    output : string;
    errorName, errorValue : string;
    errorNames, errorValues : array[0..2] of string;

    finishTimes : array of TDateTime;
    clientIDs : array of string;

    function getStartTime() : TDateTime;
  end;
  TClientTaskList = array of TClientTask;

function createTask() : TClientTask;

implementation

function createTask: TClientTask;
begin
  result.runTimeFactor := 1;
  result.success := false;
  result.computationRunning := false;
  result.failureCounter := 0;
  result.start := 0;
end;

{ TClientTask }

function TClientTask.getStartTime: TDateTime;
begin
  if (start <> 0) and (computationRunning) then
    result := start
  else
    result := Now();
end;

end.

