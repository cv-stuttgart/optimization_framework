unit uEvaluationTask;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,

  uDatatypesOptimizer, uEvaluationBinary;

type

  { TEvaluationTask }

  TEvaluationTask = class
  protected
    _rowID : string;
    _cmdLine : string;
    _errorName : string;
    _workingDir : string;
    _evaluationBinary : string;
    _deadline : TDateTime;
    _verbose : boolean;
    _evaluationBinaryClass : TEvaluationBinaryClass;
  protected
    _successful, _libraryError : boolean;
    _errorValues : TErrorValueList;
    _output, _libraryErrorMsg : string;
  public
    constructor Create(const rowID, cmdLine, errorName, workingDir,
      evaluationBinary: string; deadline: TDateTime);
    procedure execute;
    function getRowID() : string;
    function getErrorValues() : TErrorValueList;
    function getErrorValue() : Extended;
    function wasSuccessful() : boolean;
    function libraryError() : boolean;
    function getLibraryErrorMsg() : string;
    function getOutput() : string;
  end;

implementation

{ TEvaluationTask }

constructor TEvaluationTask.Create(const rowID, cmdLine, errorName,
  workingDir, evaluationBinary: string; deadline : TDateTime);
begin
  _rowID        := rowID;
  _cmdLine      := cmdLine;
  _errorName    := errorName;
  _workingDir   := workingDir;
  _evaluationBinary := evaluationBinary;
  _deadline     := deadline;
  _successful   := false;
  _libraryError := false;
  _libraryErrorMsg := '';
  _verbose      := false;
  //_evaluationBinaryClass := evaluationBinaryClass;
  _evaluationBinaryClass := ebClassManager.getClass(evaluationBinary);
end;

procedure TEvaluationTask.execute;
begin
  if _evaluationBinaryClass <> nil then
    _evaluationBinaryClass.executeProcess( _cmdLine, _workingDir, _verbose,
                                           _deadline, _successful, _libraryError,
                                           _output, _libraryErrorMsg,
                                           _errorValues)
  else
  begin
    _successful := false;
    _output     := 'Evaluation Binary Class not found!';
  end;
end;

function TEvaluationTask.getRowID: string;
begin
  result := _rowID;
end;

function TEvaluationTask.getErrorValues: TErrorValueList;
begin
  result := _errorValues;
end;

function TEvaluationTask.getErrorValue: Extended;
var
  x : integer;
begin
  result := 0;
  for x := 0 to high(_errorValues) do
    if _errorValues[x].name = _errorName then
    begin
      result := _errorValues[x].value;
      break;
    end;
end;

function TEvaluationTask.wasSuccessful: boolean;
begin
  result := _successful;
end;

function TEvaluationTask.libraryError: boolean;
begin
  result := _libraryError;
end;

function TEvaluationTask.getLibraryErrorMsg: string;
begin
  result := _libraryErrorMsg;
end;

function TEvaluationTask.getOutput: string;
begin
  result := _output;
end;

end.

