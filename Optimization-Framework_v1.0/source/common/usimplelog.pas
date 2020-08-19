unit uSimpleLog;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

var
  logstring : string;
  csLog : TCriticalSection;
  doLog : boolean = true;

procedure WriteAndLogLn(const s : string);
procedure WriteAndLogLn(const sWrite, sLog : string);
procedure ResetLog();

implementation

procedure WriteAndLogLn(const s : string);
begin
  csLog.Enter;
  try
    Writeln(s);
    if doLog then
      logstring := logstring + s + #10;
  finally
    csLog.Leave;
  end;
end;

procedure WriteAndLogLn(const sWrite, sLog : string);
begin
  csLog.Enter;
  try
    Writeln(sWrite);
    if doLog then
      logstring := logstring + sLog + #10;
  finally
    csLog.Leave;
  end;
end;

procedure ResetLog;
begin
  csLog.Enter;
  try
    logstring := '';
  finally
    csLog.Leave;
  end;
end;

initialization
  logstring := '';
  csLog := TCriticalSection.Create;

finalization
  csLog.Free;

end.

