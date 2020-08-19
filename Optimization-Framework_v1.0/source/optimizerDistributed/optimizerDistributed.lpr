program optimizerDistributed;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, BaseUnix, Classes, DOM, XMLRead, StrUtils, SysUtils, DateUtils,
  Process,

  uSimpleLog, uParameterSelection, uOptimizationRun, uDatabase,
  uOptimizationProcess, uHelper, umain,
  uRemoteOptimizationRun, uEvaluationBinary,

  uebIncludeAll, upsIncludeAll;

function EncodeMinutes(minutes : Integer) : TDateTime;
begin
  result := minutes / 1440;
end;

function getHostName() : string;
const
  READ_BYTES = 2048;
var
  p : TProcess;

  processOutput : TMemoryStream;
  poString : TStringList;
  bytesRead, n : Integer;

  procedure doReadStep;
  var
    temp : string;
  begin
    SetLength(temp, READ_BYTES + 1);
    processOutput.SetSize(bytesRead + READ_BYTES);

    n := p.Output.Read(temp[1], READ_BYTES);

    if n > 0 then
    begin
      SetLength(temp, n);
      processOutput.Write(temp[1], n);
      inc(bytesRead, n);
    end
    else
      Sleep(10);
  end;

begin
  p := TProcess.Create(nil);
  try
    p.options     := [poUsePipes];
    p.CommandLine := 'hostname';

    // run process and read console output
    processOutput := TMemoryStream.Create;
    try
      p.Execute;



      // read pipe while running
      bytesRead := 0;
      n         := 0;
      while p.Running do
      begin
        doReadStep;
      end;

      // read remaining output after run has finished
      repeat
        doReadStep;
      until n <= 0;

      // set size of buffer to proper value
      processOutput.SetSize(bytesRead);
      processOutput.Position := 0;

      // convert to string
      poString := TStringList.Create;
      try
        poString.LoadFromStream(processOutput);

        result := trim(poString.Text);
      finally
        poString.Free;
      end;
    finally
      processOutput.Free;
    end;
  finally
    p.Free;
  end;
end;

function getRemoteTaskSettings : TRemoteTaskSettings;
var
  x : integer;
  value : string;

  function isParam(const Name: string; parampos: integer;
    out Value: string): boolean;
  var
    param: string;
  begin
    param := ParamStr(parampos);
    Result := StrUtils.AnsiStartsStr(Name, param)
              and (Length(param) > Length(name))
              and (param[Length(name)+1] = '=');
    if Result then
    begin
      Value := Copy(param, Length(Name + '=') + 1, Length(param));
      Value := StringReplace(Value, '''', '"', [rfReplaceAll]);
    end
  end;

begin
  result.deadline := EncodeTime(0,15,0,0);
  result.taskType  := '';
  result.name     := GetEnvironmentVariable('USER');
  result.userName := GetEnvironmentVariable('USER');
  result.optimizationID := getHostName + '-' + IntToStr(GetProcessID);
  result.runTimeFactor := 1;
  result.workingDirectory := '';
  result.errorValueName := '';
  result.runNo    := 1;
  result.priority := 0;
  result.useDBLocks := false;

  for x := 1 to ParamCount do
  begin
    if isParam('--userName', x, value) then
      result.userName := value
    else if isParam('--taskName', x, value) then
      result.name := value
    else if isParam('--taskType', x, value) then
      result.taskType := value
    else if isParam('--errorValueName', x, value) then
    begin
      result.errorValueName := value;
    end
    else if isParam('--evaluationBinary', x, value) then
    begin
      result.evaluationBinary := value;
    end
    else if isParam('--runTimeFactor', x, value) then
      result.runTimeFactor := StrToFloatDef(value, result.runTimeFactor)
    else if isParam('--workingDir', x, value) then
    begin
      result.workingDirectory := value;
    end
    else if isParam('--priority', x, value) then
    begin
      result.priority := StrToIntDef(trim(value), 0);
    end
    else if isParam('--useDBLocks', x, value) then
    begin
      result.useDBLocks := trim(value) = '1';
    end
    else if isParam('--deadline', x, value) then
      result.deadline := EncodeMinutes(StrToIntDef(value, 15)); //EncodeTime(0, StrToIntDef(value, 15), 0, 0);
  end;
  Writeln('Task-Name: ' + result.name);
end;

var
  dbSettings : TDBSettings;
  remoteTaskSettings : TRemoteTaskSettings;
  remoteOptimizationRunFactory : TRemoteOptimizationRunFactory;
  processController : TProcessController;
  NSigActionRec : PSigActionRec;

procedure SignalHandler(sig : cint); cdecl;
begin
  Writeln('Optimizer interrupted. Clean up...');
  processController.interruptTasks;
  Writeln('finished!');
  halt;
end;

begin
  dbSettings        := readDBSettings('optimizerOF_Distributed.xml');
  remoteTaskSettings := getRemoteTaskSettings;

  remoteOptimizationRunFactory := TRemoteOptimizationRunFactory.Create;
  try
    remoteOptimizationRunFactory._dbSettings        := dbSettings;
    remoteOptimizationRunFactory._remoteTaskSettings := remoteTaskSettings;

    processController := TProcessController.Create(remoteOptimizationRunFactory);
    with processController do
    begin
      try
        New(NSigActionRec);
        NSigActionRec^.sa_handler := SigActionHandler(@SignalHandler);
        fillchar(NSigActionRec^.Sa_Mask,sizeof(NSigActionRec^.sa_mask),#0);
        NSigActionRec^.Sa_Flags    :=0;
        NSigActionRec^.sa_restorer := nil;

        fpSigAction(SigInt, NSigActionRec, nil);

        run;
      finally
        Free;
      end;
    end;
  finally
    remoteOptimizationRunFactory.Free;
  end;
end.

