unit uEvaluationBinary;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,

  uDatatypesOptimizer;

type
  TEvaluationBinary = class
    class function getCmdLineParameters(chosenParameters : TChosenParameterSet;
      const cmdLineParameters : string; imageSequenceData : TImageSequenceData
      ) : string; virtual; abstract;
    class procedure executeProcess(const cmdLine, workingDir : string;
      verbose : boolean; deadline: TDateTime; out successful, systemError : boolean;
      out output, errorMsg : string; out errorValues : TErrorValueList
      ); virtual; abstract;
  end;
  TEvaluationBinaryClass = class of TEvaluationBinary;

  { TEBClassManager }

  TEBClassManager = class(TObject)
  protected
    type
      TEBClassEntry = record
        ebClass : TEvaluationBinaryClass;
        name : string;
      end;

    var
     _registeredClasses : array of TEBClassEntry;
  public
    procedure registerClass(const name: string; ebClass: TEvaluationBinaryClass);
    function getClass(const name : string) : TEvaluationBinaryClass;
    function getRegisteredName(const ebClassName: string): string;
  end;

var
  ebClassManager : TEBClassManager;

implementation


{ TEBClassManager }

procedure TEBClassManager.registerClass(const name: string;
  ebClass: TEvaluationBinaryClass);
begin
  SetLength(_registeredClasses, length(_registeredClasses)+1);
  _registeredClasses[high(_registeredClasses)].name    := name;
  _registeredClasses[high(_registeredClasses)].ebClass := ebClass;
end;

function TEBClassManager.getClass(const name: string): TEvaluationBinaryClass;
var
  x : integer;
begin
  result := nil;
  for x := 0 to high(_registeredClasses) do
    if _registeredClasses[x].name = name then
    begin
      result := _registeredClasses[x].ebClass;
      break;
    end;
end;

function TEBClassManager.getRegisteredName(const ebClassName: string): string;
var
  x : integer;
begin
  result := '';
  for x := 0 to high(_registeredClasses) do
    if _registeredClasses[x].ebClass.ClassName = ebClassName then
    begin
      result := _registeredClasses[x].name;
      break;
    end;
end;

initialization
  ebClassManager := TEBClassManager.Create;

finalization
  ebClassManager.Free;

end.

