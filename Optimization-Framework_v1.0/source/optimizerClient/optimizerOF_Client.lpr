program optimizerOF_Client;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

{$DEFINE UseCThreads}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, DOM, XMLRead, SysUtils, StrUtils,

  uEvaluationBinary, uEvaluationTask, uBatchProcessing, uSimpleLog,

  uebIncludeAll
  { you can add units after this };

function EncodeMinutes(minutes : Integer) : TDateTime;
begin
  result := minutes / 1440;
end;

function getSettings() : TEnvironment;
var
  x, tmp : integer;
  value, configFile : string;
  Doc:      TXMLDocument;

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

  function isParamSet(const Name: string; parampos: integer): boolean;
  var
    param: string;
  begin
    param := ParamStr(parampos);
    Result := StrUtils.AnsiStartsStr(Name, param)
              and (Length(param) >= Length(name));
  end;

begin
  uSimpleLog.doLog := false;

  configFile := 'optimizerOF_Client.xml';
  result.taskSettings.taskName  := GetEnvironmentVariable('USER');
  result.taskSettings.userName := GetEnvironmentVariable('USER');
  result.taskSettings.clientID := IntToStr(GetProcessID);
  result.taskSettings.taskType  := '';
  result.numThreads           := -1;
  result.fastThreads          := -1;
  result.deadline             := -1;
  result.adaptiveScheduling   := true;
  //result.ebClass              := ebClassManager.getClass('');

  result.taskSettings.server.fetchURL  := '';
  result.taskSettings.server.submitURL := '';
  result.taskSettings.server.rejectURL := '';

  for x := 1 to ParamCount do
  begin
    if isParam('--userName', x, value) then
      result.taskSettings.userName := value
    else if isParam('--taskName', x, value) then
      result.taskSettings.taskName := value
    else if isParam('--taskType', x, value) then
      result.taskSettings.taskType := value
    else if isParam('--numThreads', x, value) then
      result.numThreads := StrToIntDef(value, result.numThreads)
    else if isParam('--fastThreads', x, value) then
      result.fastThreads := StrToIntDef(value, result.fastThreads)
    else if isParamSet('--noAdaptiveScheduling', x) then
      result.adaptiveScheduling := false
    {else if isParam('--evaluationBinary', x, value) then
      result.ebClass := ebClassManager.getClass(value)}
    else if isParam('--deadline', x, value) then
    begin
      tmp := StrToIntDef(value, -1);
      if tmp <> -1 then
      begin
        result.deadline := Now() + EncodeMinutes(tmp);
        Writeln('Setting deadline: ' + FormatDateTime('d.m.y HH:MM:SS', result.deadline));
      end;
    end;
  end;

  Writeln('Task-Name: ' + result.taskSettings.taskName);
  Writeln('Client-ID: ' + result.taskSettings.clientID);

  if fileexists(configFile) then
  begin
    ReadXMLFile(Doc, configFile);
    try
      try
        result.taskSettings.server.pingURL := Doc.DocumentElement.FindNode('server').FindNode('pingURL').FirstChild.NodeValue;
        result.taskSettings.server.fetchURL  := Doc.DocumentElement.FindNode('server').FindNode('fetchURL').FirstChild.NodeValue;
        result.taskSettings.server.submitURL := Doc.DocumentElement.FindNode('server').FindNode('submitURL').FirstChild.NodeValue;
        result.taskSettings.server.rejectURL := Doc.DocumentElement.FindNode('server').FindNode('rejectURL').FirstChild.NodeValue;
      except
        Writeln('Error: Invalid config file!');
        halt;
      end;
    finally
      Doc.Free;
    end;
  end
  else
  begin
    Writeln('Error: Config file "optimizerOF_Client.xml" not found!');
    halt;
  end;
end;

var
  environment : TEnvironment;

begin
  environment := getSettings();

  {if environment.ebClass = nil then
    raise Exception.Create('Evaluation Binary Class not found!');}

  with TBatchProcessor.Create(environment) do
  begin
    try
      run;
    finally
      Free;
    end;
  end;
end.

