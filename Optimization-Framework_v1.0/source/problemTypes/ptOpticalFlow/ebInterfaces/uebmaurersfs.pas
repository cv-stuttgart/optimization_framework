unit uebMaurerSfS;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process,

  uDatatypesOptimizer, uEvaluationBinary;

type
  { TEB_MaurerSfS }

  TEB_MaurerSfS = class(TEvaluationBinary)
    class procedure executeProcess(const cmdLine, workingDir: string;
      verbose: boolean; deadline: TDateTime; out successful, systemError: boolean; out output,
      errorMsg: string; out errorValues: TErrorValueList); override;
    class function getCmdLineParameters(chosenParameters: TChosenParameterSet;
      const cmdLineParameters: string; imageSequenceData: TImageSequenceData
      ): string; override;

  end;

implementation

{ TEB_MaurerSfS }

class procedure TEB_MaurerSfS.executeProcess(const cmdLine, workingDir: string;
  verbose: boolean; deadline: TDateTime; out successful, systemError: boolean;
  out output, errorMsg: string; out errorValues: TErrorValueList);
const
  READ_BYTES = 2048;
var
  estimationProcess : TProcess;
  processOutput : TMemoryStream;
  poString : TStringList;
  bytesRead, n, x : Integer;
  errorValueNames : array of string;

  procedure doReadStep;
  var
    temp : string;
  begin
    SetLength(temp, READ_BYTES + 1);
    processOutput.SetSize(bytesRead + READ_BYTES);

    n := estimationProcess.Output.Read(temp[1], READ_BYTES);

    if n > 0 then
    begin
      SetLength(temp, n);
      processOutput.Write(temp[1], n);
      if verbose then
        Write(temp);
      inc(bytesRead, n);
    end
    else
      Sleep(10);
  end;

begin
  SetLength(errorValueNames, 6);
  errorValueNames[0] := 'maeP';
  errorValueNames[1] := 'maeI';
  errorValueNames[2] := 'maeZ';
  errorValueNames[3] := 'nmaeP';
  errorValueNames[4] := 'nmaeI';
  errorValueNames[5] := 'nmaeZ';

  estimationProcess := TProcess.Create(nil);
  try
    // assemble process information
    {$WARNINGS Off}
    estimationProcess.CommandLine := 'main ' + cmdLine;
    Writeln(estimationProcess.CommandLine);
    {$WARNINGS On}
    estimationProcess.options         := [poUsePipes];

    // run process and read console output
    processOutput := TMemoryStream.Create;
    try
      estimationProcess.Execute;

      // read pipe while running
      bytesRead := 0;
      n         := 0;
      while estimationProcess.Running do
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

        successful := poString.Count >= Length(errorValueNames);
        if successful then
        begin
          SetLength(errorValues, Length(errorValueNames));

          for x := 0 to high(errorValueNames) do
          begin
            errorValues[x].name  := errorValueNames[x];
            try
              if SysUtils.SScanf(poString[poString.Count-(Length(errorValueNames)-x)], errorValueNames[x] + ' = %f', [@errorValues[x].value]) = 0 then
              begin
                errorValues[x].value := 0;
                successful           := false;
              end;
            except
              successful           := false;
              errorValues[x].value := 0;
            end;

            Writeln(' Scan: ' + poString[poString.Count-(Length(errorValueNames)-x)]);
            Writeln(' For:  ' + errorValueNames[x] + ' = %f');
            Writeln(' Res:  ', errorValues[x].value);
          end;
        end;

        output := poString.Text;
      finally
        poString.Free;
      end;
    finally
      processOutput.Free;
    end;
  finally
    estimationProcess.Free;
  end;
end;

class function TEB_MaurerSfS.getCmdLineParameters(
  chosenParameters: TChosenParameterSet; const cmdLineParameters: string;
  imageSequenceData: TImageSequenceData): string;
var
  runParameter : TRunParameter;
begin
  // assemble cmdline parameter list
  result := cmdLineParameters;

  // add images to cmdLine
  if Length(imageSequenceData.filenames) > 0 then
  begin
    result := result + ' -c="';
    result := result + imageSequenceData.filenames[0];
    result := result + '"';
  end;

  // add model parameters
  for runParameter in chosenParameters.modelParams do
  begin
    result := result
              + ' --vmset="'
              + runParameter.name
              + '='
              + runParameter.value
              + '"';
  end;
end;

initialization
  ebClassManager.registerClass('sfsMaurer', TEB_MaurerSfS);

end.

