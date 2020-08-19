unit uHelper;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure writeTextfile(const filename, value : string);
function readTextfile(const filename : string) : string;
function readStdIn() : string;
function DumpExceptionCallStack(E: Exception) : string;

implementation

procedure writeTextfile(const filename, value : string);
var
  sstream : TStringStream;
  ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    sstream := TStringStream.Create(value);
    try
       ms.CopyFrom(sstream, sstream.Size);
    finally
      sstream.Free;
    end;
    ms.SaveToFile(filename);
  finally
    ms.Free;
  end;
end;

function readTextfile(const filename : string) : string;
var
  sstream : TStringStream;
  ms : TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromFile(filename);
    sstream := TStringStream.Create('');
    try
      sstream.CopyFrom(ms, ms.Size);
      result := sstream.DataString;
    finally
      sstream.Free;
    end;
  finally
    ms.Free;
  end;
end;

function readStdIn() : string;
var
  c: char;
  t: text;
begin
  result := '';
  AssignFile(t, '');
  Reset(t);

  while not eof(t) do
  begin
    Read(t, c);
    result := result + c;
  end;
end;

function DumpExceptionCallStack(E: Exception) : string;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  result := report;
end;

end.

