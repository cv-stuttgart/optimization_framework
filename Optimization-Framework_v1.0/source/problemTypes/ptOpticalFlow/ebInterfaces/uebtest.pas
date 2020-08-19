unit uebTest;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, StrUtils,

  uDatatypesOptimizer, uEvaluationBinary;

type
  { TEB_Test }

  TEB_Test = class(TEvaluationBinary)
    class procedure executeProcess(const cmdLine, workingDir: string;
      verbose: boolean; deadline: TDateTime; out successful, systemError: boolean; out output,
      errorMsg: string; out errorValues: TErrorValueList); override;
    class function getCmdLineParameters(chosenParameters: TChosenParameterSet;
      const cmdLineParameters: string; imageSequenceData: TImageSequenceData
      ): string; override;
  end;

implementation

{ TEB_Test }

class procedure TEB_Test.executeProcess(const cmdLine, workingDir: string;
  verbose: boolean; deadline: TDateTime; out successful, systemError: boolean;
  out output, errorMsg: string; out errorValues: TErrorValueList);
const
  READ_BYTES = 2048;
var
  estimationProcess : TProcess;
  tmp : string;
  processOutput : TMemoryStream;
  poString : TStringList;
  bytesRead, n, x : Integer;
  t : TDateTime;

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
  systemError := false;
  successful  := false;
  output      := '';
  errorMsg    := '';
  SetLength(errorValues, 0);

  estimationProcess := TProcess.Create(nil);
  try
    // assemble process information
    {$WARNINGS Off}
    estimationProcess.CommandLine := 'optimizerTest ' + cmdLine;

    //Writeln(estimationProcess.CommandLine);
    if workingDir <> '' then
    begin
      estimationProcess.CommandLine := IncludeTrailingPathDelimiter(workingDir)
                                       + estimationProcess.CommandLine;
      estimationProcess.CurrentDirectory := workingDir;
    end;
    {$WARNINGS On}
    estimationProcess.options          := [poUsePipes];

    //Writeln(estimationProcess.CommandLine);

    // run process and read console output
    processOutput := TMemoryStream.Create;
    try
      estimationProcess.Execute;
      t := Now();


      // read pipe while running
      bytesRead := 0;
      n         := 0;
      while estimationProcess.Running
            and ((Now() - t < 2 * deadline) or (deadline = 0)) do
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

        //Writeln('Resulting string:');
        //Writeln(poString.Text);

        successful := poString.Count >= 1;
        if successful then
        begin
          SetLength(errorValues, 1);

          // save AEE
          errorValues[0].name  := 'T';
          try
            if SysUtils.SScanf(poString[poString.Count-1], 'T: %f', [@errorValues[0].value]) = 0 then
            begin
              errorValues[0].value := 0;
              successful           := false;
            end;
          except
            successful           := false;
            errorValues[0].value := 0;
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

class function TEB_Test.getCmdLineParameters(
  chosenParameters: TChosenParameterSet; const cmdLineParameters: string;
  imageSequenceData: TImageSequenceData): string;
var
  s : string;
  runParameter : TRunParameter;
begin
  result := cmdLineParameters;

  // add model parameters
  for runParameter in chosenParameters.modelParams do
  begin
    result := result
              + ' "--'
              + runParameter.name
              + '='
              + runParameter.value
              + '"';
  end;
end;

initialization
  ebClassManager.registerClass('ofTest', TEB_Test);

end.

