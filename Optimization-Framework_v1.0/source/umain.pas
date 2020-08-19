unit umain;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Types, StrUtils, Classes, SysUtils,

  uDatatypesOptimizer, uOptimizationRun, uOptimizationProcess,
  uParameterSelection, uHelper,

  {$IFDEF OpticalFlow}
  uDatatypesOF,
  {$ENDIF}
  {$IFDEF Stereo}
  uDatatypesST,
  {$ENDIF}

  uEvaluationBinary;

type
  TSettings = record
    imageSequencesString : string;
    environment : TOptimizationEnvironment;
    parametersToOptimize : TStringDynArray;
    varianceQuantile : Extended;
    cmdLine : string;
    psStrategy, psStrategyParams : string;
    logFile : string;
    sortImages : boolean;
  end;

  { TProcessController }

  TProcessController = class(TObject)
  protected
    _optimizationRunFactory : TOptimizationRunFactory;
  protected
    _settings : TSettings;
    _currOptimizationProcess : TOptimizationProcess;
    function getSettingsFromCmdLine() : TSettings;
    {$IFDEF ImageSorting}
    function getImageBasePath() : string;
    {$ENDIF}
    procedure writeLogFile;
    procedure printHelp;
  public
    constructor Create(optimizationRunFactory : TOptimizationRunFactory);
    procedure Run;
    procedure interruptTasks;
  end;

implementation

uses
  {$IFDEF ImageSorting}
  uImages, uImage_Formats,
  {$ENDIF}

  uSimpleLog;

function TProcessController.getSettingsFromCmdLine() : TSettings;
var
  x : integer;
  value : string;

  // checks if there is a parameter "Name"
  function issetParam(const Name: string; parampos: integer): boolean;
  var
    param: string;
  begin
    param := ParamStr(parampos);
    Result := StrUtils.AnsiStartsStr(Name, param)
              and (Length(param) >= Length(name));
  end;

  // checks if there is a parameter "Name=Value" and outputs value
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

  // initialize settings
  result.imageSequencesString       := '';
  result.environment.errorValueName := '';
  result.environment.numThreads     := 1;
  result.environment.printOutput    := false;
  result.environment.evaluationBinaryClass := ebClassManager.getClass('');
  SetLength(result.parametersToOptimize, 0);
  result.varianceQuantile := 100;
  result.cmdLine    := '';
  result.psStrategy := '';
  result.psStrategyParams := '';
  result.logFile := '';
  result.sortImages := false;

  // iterate over all parameters given via the commandline
  // and store the respective values in the returned record of settings
  for x := 1 to ParamCount do
  begin
    if issetParam('--help', x) or issetParam('-h', x) then
    begin
      printHelp;
      halt;
    end
    else if isParam('--imgSrc', x, value) then
    begin
      if value = 'stdin' then
        result.imageSequencesString := readStdIn()
      else
      begin
        if not fileexists(value) then
          continue;

        result.imageSequencesString := readTextfile(value);
      end;
    end
    else if isParam('--errorValueName', x, value) then
    begin
      result.environment.errorValueName := value;
    end
    else if isParam('--numThreads', x, value) then
    begin
      result.environment.numThreads := StrToIntDef(value, result.environment.numThreads);
    end
    else if isParam('--sortImages', x, value) then
    begin
      result.sortImages := trim(value) <> '0';
    end
    else if isParam('--varianceQuantile', x, value) then
    begin
      result.varianceQuantile := StrToFloatDef(value, 100);
    end
    else if isParam('--logFile', x,  value) then
    begin
      result.logFile := value;
    end
    else if isParam('--printOutput', x, value) then
    begin
      result.environment.printOutput := trim(value) <> '0';
    end
    else if isParam('--cmdLine', x, value) then
    begin
      result.cmdLine := value;
    end
    else if isParam('--psStrategy', x, value) then
    begin
      result.psStrategy := value;
    end
    else if isParam('--psStrategyParams', x, value) then
    begin
      result.psStrategyParams := value;
    end
    else if isParam('--evaluationBinary', x, value) then
    begin
      result.environment.evaluationBinaryClass := ebClassManager.getClass(value);
    end
    else if isParam('-p', x, value) then
    begin
      SetLength(result.parametersToOptimize, Length(result.parametersToOptimize)+1);
      result.parametersToOptimize[high(result.parametersToOptimize)] := value;
    end;
  end;

  if result.psStrategy = '' then
  begin
    printHelp();
    halt;
  end;
end;

{$IFDEF ImageSorting}
function TProcessController.getImageBasePath: string;
var
  x : integer;
  state : (stNormal, stString, stParamStart, stParamInside);
  paramName : string;
begin
  result := '';
  paramName := '';
  state := stNormal;
  for x := 1 to Length(_settings.cmdLine) do
  begin
    case state of
      stNormal:
        if _settings.cmdLine[x] = '-' then
        begin
          state := stParamStart;
          paramName := '';
        end;

      stParamStart:
        case _settings.cmdLine[x] of
          '-': ;
          '=':
            begin
              state := stParamInside;
            end;
          ' ': state := stNormal;
        else
          paramName := paramName + _settings.cmdLine[x];
        end;

      stParamInside:
        case _settings.cmdLine[x] of
          '''': state := stString;
          ' ':
            begin
              state := stNormal;
              if (paramName = 'b') or (paramName = 'basepath') then
                exit;
            end;
        else
          if (paramName = 'b') or (paramName = 'basepath') then
            result := result + _settings.cmdLine[x];
        end;

      stString:
        case _settings.cmdLine[x] of
          '''':
            begin
              state := stNormal;
              if (paramName = 'b') or (paramName = 'basepath') then
                exit;
            end;
        else
          if (paramName = 'b') or (paramName = 'basepath') then
            result := result + _settings.cmdLine[x];
        end;
    end;
  end;
end;
{$ENDIF}

procedure TProcessController.writeLogFile;
begin
  if _settings.logFile <> '' then
    writeTextfile(_settings.logFile, uSimpleLog.logstring)
  else
    writeln('No filename for log provided!');
end;

procedure TProcessController.printHelp;
var
  entry : TParameterSelectionClass;
begin
  Writeln('--help or -h:');
  Writeln('  prints this help text.');
  Writeln;
  Writeln('--imgSrc=FILENAME:');
  Writeln('  loads the set of image sequences from a file with the name FILENAME');
  Writeln('  or if FILENAME equals "stdin" from the standard input.');
  Writeln;
  Writeln('  format:');
  Writeln('  image_basepath;image1,image2,image3,...;no_of_reference_frame;groundtruth_file;weight_of_sequence');
  Writeln('    - "image_basepath": must include a terminating path delimiter ("/").');
  Writeln('    - "no_of_reference_frame": usually 0.');
  Writeln('    - "weight_of_sequence": defines how the resulting error value is weighted in the computation of the (weighted) average value.');
  Writeln('  example:');
  Writeln('  "RubberWhale/;frame08.png,frame09.png,frame10.png,frame11.png,frame12.png;2;flow10.flo;1.0"');
  Writeln;
  Writeln('--errorValueName=ERRORVALUE:');
  Writeln('  "AAE", "AEE" or "BP".');
  Writeln;
  Writeln('--numThreads=NUMBER:');
  Writeln;
  Writeln('--sortImages=1/0:');
  Writeln('  sorts image sequences descendingly by their spatial size (number of pixels).');
  Writeln;
  Writeln('--logFile=FILENAME:');
  Writeln('  defines a file which contains the optimizer''s output (updated after each optimization run).');
  Writeln;
  Writeln('--printOutput=1/0:');
  Writeln('  prints and logs the output of the estimation process.');
  Writeln;
  Writeln('--varianceQuantile=0..100:');
  Writeln('  after the initial set of parameters is evaluated, only a share of all sequences is used for further optimization.');
  Writeln('   The relative size of the share is given by varianceQuantile (in %) and the most volatile sequences w.r.t. the error measure are chosen.');
  Writeln;
  Writeln('--cmdLine=PARAMETERLIST:');
  Writeln('  contains all static parameters for the evaluation binary. Double quotes must be replaced by single quotes.');
  Writeln;
  Writeln('--workingDir=DIRECTORY:');
  Writeln('  directory where the evaluation binary should be started in');
  Writeln;
  Writeln('  example:');
  Writeln('  "--runandclose -b=images/middlebury/training -c=''TC Flow (SOR)''"');
  Writeln;
  Writeln('--psStrategy=NAME:');
  Writeln('  chooses a strategy for parameter selection');
  Writeln;
  Writeln('  possible values (and values for "psStrategyParams"):');

  for entry in psClassManager.getClassList() do
    if entry.helpText() <> '' then
      Writeln(entry.helpText());

  Writeln;
  Writeln('--psStrategyParams=PARAMLIST:');
  Writeln('  defines parameters for the chosen parameter selection strategy (examples above).');
  Writeln;
  Writeln(' -p=PARAMETERDEFINITION:');
  Writeln('  defines a parameter to be optimized.');
  Writeln;
  Writeln('  format:');
  Writeln('  parameter_name;is_model_param;is_enumeration;parameter_type;example_values');
  Writeln('  - "parameter_type":');
  Writeln('    + "string" (standard)');
  Writeln('    + "int"');
  Writeln('    + "float"');
  Writeln('    + "floatNN" (non-negative floats)');
  Writeln('  - "example_values" (separated by '',''):');
  Writeln('    + in case of an enumeration all possible values');
  Writeln('    + otherwise two values (min/max for equidistant sampling)');
  Writeln('    + if logarithmic sampling is used, the third value is the basis of the logarithm');
  Writeln;
  Writeln('  example:');
  Writeln('  "Alpha (1st Order);1;0;floatNN;481.25,1100"');
  Writeln;
end;

constructor TProcessController.Create(
  optimizationRunFactory : TOptimizationRunFactory);
begin
  self._optimizationRunFactory := optimizationRunFactory;
  _currOptimizationProcess := nil;
end;

// sort images sequences by spatial size, first filename
function compareImageEntries(Item1, Item2 : Pointer) : Integer;
begin
  if TImageSequenceData(Item1^).size > TImageSequenceData(Item2^).size then
    result := -1
  else if TImageSequenceData(Item1^).size < TImageSequenceData(Item2^).size then
    result := 1
  else
  begin
    if (Length(TImageSequenceData(Item1^).filenames) = 0)
       and (Length(TImageSequenceData(Item2^).filenames) = 0) then
      result := 0
    else if (Length(TImageSequenceData(Item1^).filenames) = 0)
       and (Length(TImageSequenceData(Item2^).filenames) > 0) then
      result := -1
    else if (Length(TImageSequenceData(Item1^).filenames) > 0)
       and (Length(TImageSequenceData(Item2^).filenames) = 0) then
      result := 1
    else
    begin
      if TImageSequenceData(Item1^).filenames[0] < TImageSequenceData(Item2^).filenames[0] then
        result := -1
      else if TImageSequenceData(Item1^).filenames[0] > TImageSequenceData(Item2^).filenames[0] then
        result := 1
      else
        result := 0;
    end;
  end;
end;

procedure TProcessController.Run;
var
  psStrategyClass : TParameterSelectionClass;
  optimization : TOptimizationProcess;
  x, y, lastLine, lastCol : Integer;
  parametersToOptimize : TParameterToOptimizeList;
  imageSequences : TImageSequenceDataList;
  t : TDateTime;
  cols, colEntries : TStringList;
  dynamicParams : TChosenParameterSet;
  param : TRunParameter;
  errorValue : TErrorValue;
  {$IFDEF ImageSorting}
  tmpImgList : TList;
  tmpImgArray : TImageSequenceDataList;
  basePath : string;
  tmpWidth, tmpHeight : Cardinal;
  {$ENDIF}
begin
  _settings := getSettingsFromCmdLine();

  WriteAndLogLn('Print output: ' + IntToStr(Ord(_settings.environment.printOutput)));

  imageSequences := nil;
  {$IFDEF OpticalFlow}
  imageSequences := TDataReaderOF.createImageSequenceDataList(_settings.imageSequencesString);
  {$ENDIF}
  {$IFDEF Stereo}
  imageSequences := TDataReaderST.createImageSequenceDataList(_settings.imageSequencesString);
  {$ENDIF}
  try
    {$IFDEF ImageSorting}
    if _settings.sortImages then
    begin
      WriteAndLogLn('Sort images by size...');

      // read images and determine size
      basePath := getImageBasePath();
      for x := 0 to high(imageSequences) do
      begin
        if Length(imageSequences[x].filenames) > 0 then
        begin
          if fileexists(basePath + imageSequences[x].filenames[0]) then
          begin
            TRGBImage.getDimensions( basePath + imageSequences[x].filenames[0],
                                     tmpWidth, tmpHeight);
            imageSequences[x].size := tmpWidth * tmpHeight;
          end;
        end;
      end;

      // sort images by size in descending order
      tmpImgList := TList.Create;
      try
        SetLength(tmpImgArray, Length(imageSequences));
        for x := 0 to high(imageSequences) do
          tmpImgList.Add(@imageSequences[x]);

        tmpImgList.Sort(@compareImageEntries);

        for x := 0 to high(imageSequences) do
          tmpImgArray[x] := TImageSequenceData(tmpImgList[x]^);
        imageSequences := tmpImgArray;
      finally
        tmpImgList.Free;
      end;
    end;
    {$ENDIF}

    // get parameters to optimize
    // format: 'name [string]; isModelParam ["0"/"1"]; isEnumeration ["0"/"1"];
    //          type ["int","float","floatNN"]; value1, value2, value3, ...'
    lastLine   := -1;
    cols       := TStringList.Create;
    colEntries := TStringList.Create;
    try
      SetLength(parametersToOptimize, Length(_settings.parametersToOptimize));
      for x := 0 to high(_settings.parametersToOptimize) do
      begin
        try
          lastLine := x;

          // obtain columns separated by ';'
          lastCol  := -1;
          cols.Delimiter := ';';
          cols.StrictDelimiter := true;
          cols.DelimitedText := _settings.parametersToOptimize[x];

          // obtain values of parameter in fifth column separated by ','
          lastCol := 4;
          colEntries.Delimiter := ',';
          colEntries.StrictDelimiter := true;
          colEntries.DelimitedText := trim(cols[4]);

          parametersToOptimize[x].floatConstraints := fcNone;

          SetLength(parametersToOptimize[x].values, colEntries.Count);
          for y := 0 to colEntries.Count-1 do
          begin
            parametersToOptimize[x].values[y] := trim(colEntries[y]);
          end;

          // "name" in first column
          lastCol := 0;
          parametersToOptimize[x].name := trim(cols[0]);

          // "isModelParam" in second column
          lastCol := 1;
          parametersToOptimize[x].modelParam := (trim(cols[1]) <> '0')
                                                and (trim(cols[1]) <> '');

          // "isEnumeration" in third column
          lastCol := 2;
          parametersToOptimize[x].isEnumeration := (trim(cols[2]) <> '0')
                                                   and (trim(cols[2]) <> '');

          // "type" in fourth column
          lastCol := 3;
          parametersToOptimize[x].pType := ptString;
          if cols[3] = 'float' then
            parametersToOptimize[x].pType := ptFloat
          else if cols[3] = 'floatNN' then
          begin
            parametersToOptimize[x].pType            := ptFloat;
            parametersToOptimize[x].floatConstraints := fcNonnegative;
          end
          else if cols[3] = 'floatP' then
          begin
            parametersToOptimize[x].pType            := ptFloat;
            parametersToOptimize[x].floatConstraints := fcPositive;
          end
          else if cols[3] = 'int' then
            parametersToOptimize[x].pType := ptInteger;

        except
          raise Exception.Create( 'Malformed parameter input number '
                                  + IntToStr(lastline+1) + ', column: '
                                  + IntToStr(lastCol+1));
        end;
      end;
    finally
      colEntries.Free;
      cols.Free;
    end;

    if _settings.environment.evaluationBinaryClass = nil then
      raise Exception.Create('Evaluation Binary Class not found!');

    // choose parameter selection strategy
    psStrategyClass := psClassManager.getClass(_settings.psStrategy);
    if psStrategyClass = nil then
      raise Exception.Create('Parameter Selection Strategy not found! ("' + _settings.psStrategy + '")');

    // create optimization run
    optimization := TOptimizationProcess.Create( _settings.cmdLine, imageSequences,
                                                 _settings.environment,
                                                 psStrategyClass.Create( parametersToOptimize,
                                                                         _settings.varianceQuantile,
                                                                         _settings.psStrategyParams),
                                                 _optimizationRunFactory,
                                                 @writeLogFile);
    try
      _currOptimizationProcess := optimization;
      try

        // let the optimization process run and measure time
        t := Now();
        optimization.optimize;
        t := Now() - t;

        // read estimated optimal parameters and write them
        dynamicParams := optimization.getOptimalParameterSet();
        WriteAndLogLn( 'Avg. ' + _settings.environment.errorValueName + ': ' +
                       Format('%4.3f', [optimization.getSmallestResult()]));
        WriteAndLogLn('All avg. errors:');
        for errorValue in optimization.getAllErrors() do
          WriteAndLogLn('- ' + errorValue.name + ': ' + Format('%4.3f', [errorValue.value]));
        WriteAndLogLn('Dynamic parameters:');
        for param in dynamicParams.modelParams do
          WriteAndLogLn('- Name: ' + param.name + ', Value: ' + param.value);
        for param in dynamicParams.solverParams do
          WriteAndLogLn('- Name: ' + param.name + ', Value: ' + param.value);
        WriteAndLogLn( 'Total optimization time: ' + TEvaluationThread.formatTime(t)
                       + ' (' + IntToStr(optimization.getRunCounter()) + ' runs, '
                       + IntToStr(optimization.getEvaluationCounter()) + ' evaluations)');
      except
        on e: exception do
        begin
          WriteAndLogLn( DumpExceptionCallStack(e) );
        end;
      end;

      writeLogFile;
    finally
      _currOptimizationProcess := nil;
      optimization.Free;
    end;

  finally
    for x := 0 to high(imageSequences) do
      imageSequences[x].Free;
  end;
end;

procedure TProcessController.interruptTasks;
begin
  if _currOptimizationProcess <> nil then
  begin
    _currOptimizationProcess.interruptTasks;
  end;
end;

end.

