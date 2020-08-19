unit uEBStollSF;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, StrUtils,

  uDatatypesOptimizer, uDatatypesST, uEvaluationBinary;

type

  { TEB_StollSF }

  TEB_StollSF = class(TEvaluationBinary)
    class procedure executeProcess(const cmdLine, workingDir: string;
      verbose: boolean; deadline: TDateTime; out successful, systemError: boolean; out output,
      errorMsg: string; out errorValues: TErrorValueList); override;
    class function getCmdLineParameters(chosenParameters: TChosenParameterSet;
      const cmdLineParameters: string; imageSequenceData: TImageSequenceData
      ): string; override;
  end;

implementation

{ TEB_StollSF }

class procedure TEB_StollSF.executeProcess(const cmdLine, workingDir: string;
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
    estimationProcess.CommandLine := 'batchSF ' + cmdLine;

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

        successful := poString.Count >= 4;
        if successful then
        begin
          SetLength(errorValues, 4);

          // save AAE
          errorValues[0].name  := 'AAE';
          try
            if SysUtils.SScanf(poString[poString.Count-4], 'AAE: %f', [@errorValues[0].value]) = 0 then
            begin
              errorValues[0].value := 0;
              successful           := false;
            end;
          except
            successful           := false;
            errorValues[0].value := 0;
          end;

          // save NRMS_P
          errorValues[1].name  := 'NRMS_P';
          try
            if SysUtils.SScanf(poString[poString.Count-3], 'NRMS_P: %f', [@errorValues[1].value]) = 0 then
            begin
              successful           := false;
              errorValues[1].value := 0;
            end;
          except
            successful           := false;
            errorValues[1].value := 0;
          end;

          // save NRMS_V
          errorValues[2].name  := 'NRMS_V';
          try
            if SysUtils.SScanf(poString[poString.Count-2], 'NRMS_V: %f', [@errorValues[2].value]) = 0 then
            begin
              successful           := false;
              errorValues[2].value := 0;
            end;
          except
            successful           := false;
            errorValues[2].value := 0;
          end;

          // save BP
          errorValues[3].name  := 'BP';
          try
            tmp := Copy(poString[poString.Count-1], 0, Length(poString[poString.Count-1]) - 1);
            if SysUtils.SScanf(tmp, 'BP:  %f', [@errorValues[3].value]) = 0 then
            begin
              successful           := false;
              errorValues[3].value := 0;
            end;
          except
            successful           := false;
            errorValues[3].value := 0;
          end;
        end;

        for x := poString.Count-1 downto 0 do
          if    StrUtils.AnsiStartsStr( lowercase('Estimation Error'),
                                        lowercase(poString[x]))
             or StrUtils.AnsiStartsStr( lowercase('Exception:'),
                                        lowercase(poString[x])) then
          begin
            successful := false;
            break;
          end;

        if not successful then
        begin
          for x := poString.Count-1 downto 0 do
            if StrUtils.AnsiStartsStr( lowercase('Estimation Error - Could not load OF library'),
                                       lowercase(poString[x])) then
            begin
              systemError := true;
              errorMsg := poString[x];
              break;
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

class function TEB_StollSF.getCmdLineParameters(
  chosenParameters: TChosenParameterSet; const cmdLineParameters: string;
  imageSequenceData: TImageSequenceData): string;
var
  s : string;
  runParameter : TRunParameter;
begin
  // assemble cmdline parameter list
  result := cmdLineParameters;

  result := StringReplace(result, '__SEQ_INFO__', imageSequenceData.seqInfo, [rfReplaceAll]);

  // add images to cmdLine
  if Length(imageSequenceData.filenames) > 0 then
  begin
    result := result+ ' -i="';
    for s in imageSequenceData.filenames do
    begin
      result := result + s + ';';
    end;
    result[Length(result)] := '"';
  end;

  // add camera information to cmdLine
  if (imageSequenceData is TImageSequenceDataST) and (Length(TImageSequenceDataST(imageSequenceData).cameraInformation) > 0) then
  begin
    result := result+ ' --camInf="';
    for s in TImageSequenceDataST(imageSequenceData).cameraInformation do
    begin
      result := result + s + ';';
    end;
    result[Length(result)] := '"';
  end;

  // add binary files to cmdLine
  if Length(imageSequenceData.binaryFiles) > 0 then
  begin
    result := result + ' --binaryFiles="';
    for s in imageSequenceData.binaryFiles do
    begin
      result := result + s + ';';
    end;
    result[Length(result)] := '"';
  end;

  // add reference frame to cmdLine
  result := result
            + ' -r='
            + IntToStr(imageSequenceData.numberOfRefFrame);

  // add ground truth to cmdLine
  if imageSequenceData.groundTruthFile <> '' then
  begin
    result := result
              + ' -g="'
              + imageSequenceData.groundTruthFile
              + '"';
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

  // add solver parameters
  for runParameter in chosenParameters.solverParams do
  begin
    result := result
              + ' --vsset="'
              + runParameter.name
              + '='
              + runParameter.value
              + '"';
  end;
end;

initialization
  ebClassManager.registerClass('sfStoll', TEB_StollSF);

end.

