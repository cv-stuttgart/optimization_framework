unit uEvaluationTask;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Process,

  uDatatypesOptimizer, uEvaluationBinary;

type

  { TEvaluationTaskBase }

  TEvaluationTaskBase = class(TObject)
  protected
    // parameters for this run
    _chosenParameters : TChosenParameterSet;
    _cmdLineParameters : string;
    _imageSequenceData : TImageSequenceData;
    _evaluationBinaryClass : TEvaluationBinaryClass;

    // state & log
    _finished : boolean;
    _successful : boolean;
    _errorValues : TErrorValueList;
    _output : string;
    _finalCmdLine : string;
  public
    constructor Create(const cmdLineParameters : string;
      chosenParameters : TChosenParameterSet;
      imageSequenceData : TImageSequenceData;
      evaluationBinaryClass : TEvaluationBinaryClass);
    function getErrorValues() : TErrorValueList;
    function wasSuccessful() : boolean;
    function getWeight() : extended;
    function getGTName() : string;
    function getOutput() : string;
    function getCmdLine() : string;
  end;

  { TEvaluationTask }

  TEvaluationTask = class(TEvaluationTaskBase)
  protected
    _verbose : boolean;
  public
    constructor Create(const cmdLineParameters : string;
      chosenParameters : TChosenParameterSet;
      imageSequenceData : TImageSequenceData;
      evaluationBinaryClass : TEvaluationBinaryClass;
      verbose : boolean);
    procedure execute; virtual;
  end;

implementation

uses
  uSimpleLog;

{ TEvaluationTaskBase }

constructor TEvaluationTaskBase.Create(const cmdLineParameters: string;
  chosenParameters: TChosenParameterSet;
  imageSequenceData: TImageSequenceData;
  evaluationBinaryClass: TEvaluationBinaryClass);
begin
  self._cmdLineParameters := cmdLineParameters;
  self._chosenParameters := chosenParameters;
  self._imageSequenceData := imageSequenceData;
  self._evaluationBinaryClass := evaluationBinaryClass;

  _finished   := false;
  _successful := false;
  SetLength(_errorValues, 0);
end;

function TEvaluationTaskBase.getErrorValues: TErrorValueList;
begin
  result := _errorValues;
end;

function TEvaluationTaskBase.wasSuccessful: boolean;
begin
    result := self._successful;
end;

function TEvaluationTaskBase.getWeight: extended;
begin
  result := _imageSequenceData.resultWeight;
end;

function TEvaluationTaskBase.getGTName: string;
begin
  result := _imageSequenceData.groundTruthFile;
end;

function TEvaluationTaskBase.getOutput: string;
begin
  result := _output;
end;

function TEvaluationTaskBase.getCmdLine: string;
begin
  result := _finalCmdLine;
end;

{ TEvaluationTask }

constructor TEvaluationTask.Create(const cmdLineParameters: string;
  chosenParameters: TChosenParameterSet;
  imageSequenceData: TImageSequenceData;
  evaluationBinaryClass: TEvaluationBinaryClass; verbose: boolean);
begin
  inherited Create( cmdLineParameters, chosenParameters, imageSequenceData,
                    evaluationBinaryClass);

  self._verbose           := verbose;
end;

procedure TEvaluationTask.execute;
var
  errorMsg : string;
  systemError : boolean;
begin
  _finalCmdLine := _evaluationBinaryClass.getCmdLineParameters(
                         _chosenParameters, _cmdLineParameters, _imageSequenceData);

  _evaluationBinaryClass.executeProcess( _finalCmdLine, '', _verbose, 0,
                                         _successful, systemError, _output,
                                         errorMsg,_errorValues);

  _finished := true;
end;

end.

