unit ups_DownhillSimplex;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  udatatypesOptimizer, uParameterSelection, uOptimizationRun;

type
  TDSAlgorithmState = ( dsasCalculateAll, dsasCalculateAllFinished, dsasReflection, dsasExpansion,
                        dsasContraction, dsasReduction);
  TDSConvergenceCriterion = (dscResult, dscParameter);
  TDSConvergenceCriterionSet = set of TDSConvergenceCriterion;

  { TPS_DownhillSimplex }

  TPS_DownhillSimplex = class(TParameterSelection)
  protected
    // parameters of the method
    _alpha : extended;
    _gamma : extended;
    _rho : extended;
    _sigma : extended;

    // convergence parameters
    _convergenceFactorPM, _convergenceFactorRES : extended;

    // convergence critria
    _cvCriteria : TDSConvergenceCriterionSet;

    // parameters further variants of the method
    _weightedCentroid : boolean;
    _noInitialSimplices : integer;
  protected
    // size of the current simplex
    _simplexSize : Cardinal;

    // contains the best results
    _bestResults : array of Extended;

    // contains initial simplex
    _initialSimplex : TChosenParameterSetAndResultList;

    // contains the current simplex
    _currentSimplex : TChosenParameterSetAndResultList;

    // contain specific points in the parameter space which are computed
    // out of the simplex' values
    _centroid : TChosenParameterSetAndResult;
    _reflection : TChosenParameterSetAndResult;
    _expansion : TChosenParameterSetAndResult;
    _contraction : TChosenParameterSetAndResult;

    procedure sortSimplex;
    procedure calcCentroid; virtual;
    procedure sortSimplexAndCalcCentroid;
    function computeReflection : boolean;
    function computeExpansion : boolean;
    function computeContraction : boolean;
    function computeReduction : boolean;
    function checkResultConvergence() : boolean;
    function checkParameterConvergence(point : TChosenParameterSetAndResult) : boolean;
  public
    constructor Create(parametersToOptimize: TParameterToOptimizeList;
      varianceQuantile: Extended; const params: string); override;
    procedure runOptimization(basicResultList : TimageSequenceResultList;
      executeRun : TOptimizationRunExecution;
      var bestRun : TOptimizationRunInfo); override;
    class function helpText: string; override;
  end;

implementation

uses
  Math, uSimpleLog;

{ TPS_DownhillSimplex }

constructor TPS_DownhillSimplex.Create(
  parametersToOptimize: TParameterToOptimizeList;
  varianceQuantile : Extended; const params: string);
var
  k, x, y, simplexIdx, paramsStart : integer;
  tmpParam : TRunParameter;
  paramChoiceList : ^TRunParameterList;
  paramsStr : TStringList;

  procedure readFloat(var value : extended; idx : Integer);
  begin
    value := StrToFloat(paramsStr[idx]);
  end;

  procedure readBool(var value : boolean; idx : Integer);
  begin
    value := (trim(paramsStr[idx]) <> '0') and (trim(paramsStr[idx]) <> '');
  end;

  procedure readInt(var value : integer; idx : Integer);
  begin
    value := StrToInt(paramsStr[idx]);
  end;

  procedure readNextParam(strIdx, arrIdx : Integer; paramList, pointerList : array of const);
  begin
    if (paramsStr.Count > strIdx) and (Length(paramList) > arrIdx) then
    begin
      try
        if paramList[arrIdx].VType = vtExtended then
          readFloat(extended(pointerList[arrIdx].VPointer^), strIdx)
        else if paramList[arrIdx].VType = vtInteger then
          readInt(integer(pointerList[arrIdx].VPointer^), strIdx)
        else if paramList[arrIdx].VType = vtBoolean then
          readBool(boolean(pointerList[arrIdx].VPointer^), strIdx);

      except
        raise Exception.Create('Invalid parameter ' + IntToStr(strIdx+1) + ' for "Downhill-Simplex"!');
      end;

      readNextParam(strIdx+1, arrIdx+1, paramList, pointerList);
    end;
  end;

begin
  inherited Create(parametersToOptimize, varianceQuantile, params);


  // set default values for parameters
  _convergenceFactorPM  := 1.05;
  _convergenceFactorRES := 1.05;
  _alpha                := 1;
  _gamma                := 2;
  _rho                  := -0.5;
  _sigma                := 0.5;

  _weightedCentroid  := false;
  _noInitialSimplices := 1;

  // parse parameter string
  paramsStr := TStringList.Create;
  try
    paramsStr.Delimiter     := ';';
    paramsStr.DelimitedText := params;

    // read criterion for convergence
    _cvCriteria := [];
    if (paramsStr.Count = 0) or (paramsStr[0] = 'cvResult') or (paramsStr[0] = '') then
      _cvCriteria := [dscResult]
    else if paramsStr[0] = 'cvParameter' then
      _cvCriteria := [dscParameter]
    else if paramsStr[0] = 'cvParameterAndResult' then
      _cvCriteria := [dscResult, dscParameter]
    else
      raise Exception.Create('Invalid convergence criterion ("' + paramsStr[0] + '") for "Downhill-Simplex"!');

    if dscResult in _cvCriteria then
      WriteAndLogLn('DS: Convergence criterion "result"');
    if dscParameter in _cvCriteria then
      WriteAndLogLn('DS: Convergence criterion "parameter"');

    try
      // read parameters for convergence criteria
      paramsStart := 2;
      if _cvCriteria = [dscResult] then
      begin
        if paramsStr.Count > 1 then
          readFloat(_convergenceFactorRES, 1)
      end
      else if _cvCriteria = [dscParameter] then
      begin
        if paramsStr.Count > 1 then
          readFloat(_convergenceFactorPM, 1)
      end
      else if _cvCriteria = [dscResult, dscParameter] then
      begin
        if paramsStr.Count > 1 then
        begin
          readFloat(_convergenceFactorRES, 1);
          if paramsStr.Count > 2 then
             readFloat(_convergenceFactorPM, 2)
          else
            _convergenceFactorPM := _convergenceFactorRES;
        end;
        paramsStart := 3;
      end;

      // read all subsequent parameters setting the variables in the order they
      // are passed to "readNextParam"
      readNextParam( paramsStart, 0,
                     [ _weightedCentroid,
                       _noInitialSimplices,
                       _alpha, _gamma, _rho, _sigma],
                     [ @_weightedCentroid,
                       @_noInitialSimplices,
                       @_alpha, @_gamma, @_rho, @_sigma]);
    except
      raise Exception.Create('Invalid parameter for "Downhill-Simplex"!');
    end;

    // print all parameters
    WriteAndLogLn('DS: convergence factor (result): ' + FloatToStr(_convergenceFactorRES));
    WriteAndLogLn('DS: convergence factor (pm): ' + FloatToStr(_convergenceFactorPM));
    WriteAndLogLn('DS: weighted centroid: ' + IntToStr(Ord(_weightedCentroid)));
    WriteAndLogLn('DS: initial simplices: ' + IntToStr(_noInitialSimplices));
    WriteAndLogLn('');
  finally
    paramsStr.Free;
  end;

  // initialize state variables
  if _noInitialSimplices < 1 then
    _noInitialSimplices := 1;

  separateAndSampleDiscreteParameters();


  // construct initial simplex/simplices out of the provided initial values
  // for the parameters
  // - multiple simplices will simply be concatenated
  _simplexSize := Length(_parametersToOptimize)+1;
  SetLength(_initialSimplex, _noInitialSimplices * _simplexSize);
  for k := 0 to _noInitialSimplices-1 do
    for x := 0 to _simplexSize-1 do
    begin
      for y := 0 to high(_parametersToOptimize) do
      begin

        // set parameter name and choose a possible value
        tmpParam.name  := _parametersToOptimize[y].name;
        if (x = y) or ( (x = _simplexSize-1)
                        and (high(_parametersToOptimize) <> 0)) then
          tmpParam.value := _parametersToOptimize[y].values[k+1]
        else
          tmpParam.value := _parametersToOptimize[y].values[0+0];
        tmpParam.floatConstraints := _parametersToOptimize[y].floatConstraints;

        // determine entry of the simplex (concatenation)
        simplexIdx := k*_simplexSize + x;

        // select appropriate list of parameters to insert "tmpParam"
        if _parametersToOptimize[y].modelParam then
          paramChoiceList := @_initialSimplex[simplexIdx].parameters.modelParams
        else
          paramChoiceList := @_initialSimplex[simplexIdx].parameters.solverParams;

        SetLength(paramChoiceList^, Length(paramChoiceList^)+1);
        paramChoiceList^[high(paramChoiceList^)] := tmpParam;

        // initialize further values of the current simplex entry
        _initialSimplex[simplexIdx].result     := Math.MaxExtended;
        _initialSimplex[simplexIdx].resultList := nil;
      end;
    end;
end;





procedure TPS_DownhillSimplex.runOptimization(
  basicResultList: TimageSequenceResultList;
  executeRun: TOptimizationRunExecution; var bestRun: TOptimizationRunInfo);
var
  currentState : TDSAlgorithmState;
  subsetComputed : boolean;
  currentParamsList : TChosenParameterSetList;
  currentResultMatrix : TChosenParameterSetAndResultList;
  d, startD, x : Integer;

  // check if parameters have converged
  function parametersConverged(point : TChosenParameterSetAndResult) : boolean;
  begin
    result := checkParameterConvergence(point);
    if result then
    begin
      WriteAndLogLn('DS: Parameters have converged!');
      WriteAndLogLn('DS: Termination...');
      WriteAndLogLn('');
    end;
  end;

  // check if results have converged
  function resultsConverged() : boolean;
  var
    x : Integer;
  begin
    result := checkResultConvergence();
    if result then
    begin
      WriteAndLogLn('DS: Last results...');
      for x := high(_currentSimplex) downto 0 do
      begin
        WriteAndLogLn('- ' + FloatToStr(_currentSimplex[x].result));
      end;
      WriteAndLogLn('DS: Best results...');
      for x := high(_bestResults) downto 0 do
      begin
        WriteAndLogLn('- ' + FloatToStr(_bestResults[x]));
      end;
      WriteAndLogLn('DS: Termination...');
      WriteAndLogLn('');
    end;
  end;

  // results for all image sequences and compute (weighted) average
  procedure copyResults(resultList: TImageSequenceResultList;
    var entry : TChosenParameterSetAndResult);
  var
    x, y : Integer;
  begin
    entry.resultList := Copy(resultList, 0, Length(resultList));
    entry.result     := computeAvg(entry.resultList);

    // copy result into list of best results (in order to determine convergence)
    for x := 0 to high(_bestResults) do
    begin
      if entry.result < _bestResults[x] then
      begin
        for y := high(_bestResults)-1 downto x do
          _bestResults[y+1] := _bestResults[y];
        _bestResults[x] := entry.result;
        break;
      end;
    end;
  end;


  // compute the error values for some point (after reflection, expansion etc.)
  function evaluatePoint(var point : TChosenParameterSetAndResult) : boolean;
  begin
    SetLength(currentParamsList, 1);
    if d > -1 then
      currentParamsList[0] := point.parameters.merge(_discreteParameters[d])
    else
      currentParamsList[0] := point.parameters.clone();

    currentResultMatrix := executeRun(currentParamsList, basicResultList);
    if Length(currentResultMatrix) = 1 then
    begin
      copyResults(currentResultMatrix[0].resultList, point);
      result := true;
    end
    else
    begin
      result := false;
      WriteAndLogLn('DS: Error! Run could not be evaluated');
      WriteAndLogLn('');
    end;
  end;


  // compute the error values for the vertices of the _currentSimplex
  function evaluateSimplex(offset : Integer = 0) : boolean;
  var
    x : Integer;
  begin
    result := true;
    SetLength(currentParamsList, Length(_currentSimplex)-offset);
    for x := 0 to high(currentParamsList) do
      if d > -1 then
        currentParamsList[x] := _currentSimplex[x+offset].parameters.merge(_discreteParameters[d])
      else
        currentParamsList[x] := _currentSimplex[x+offset].parameters.clone();

    currentResultMatrix := executeRun(currentParamsList, basicResultList);

    if Length(currentResultMatrix) <> Length(_currentSimplex)-offset then
    begin
       result := false;
       WriteAndLogLn('DS: Error! Runs could not be evaluated');
       WriteAndLogLn('');
       exit;
    end;

    for x := 0 to high(currentResultMatrix) do
      copyResults(currentResultMatrix[x].resultList, _currentSimplex[x+offset]);
  end;


  // copy the inital simplex into _currentSimplex for restart with a different
  // set of discrete parameters
  procedure cloneInitialSimplex;
  var
    x : Integer;
  begin
    _currentSimplex   := Copy(_initialSimplex, 0, Length(_initialSimplex));
    for x := 0 to high(_currentSimplex) do
      _currentSimplex[x].parameters := _currentSimplex[x].parameters.clone();
  end;

begin
  subsetComputed    := _varianceQuantile > 99.9;
  if Length(_discreteParameters) = 0 then
    startD := -1
  else
    startD := 0;

  for d := startD to high(_discreteParameters) do
  begin
    // initialize state variables
    currentState      := dsasCalculateAll;
    cloneInitialSimplex;

    // Initialize list of best results
    SetLength(_bestResults, _simplexSize);
    for x := 0 to _simplexSize-1 do
      _bestResults[x] := MaxExtended;

    // start iterations
    while true do
    begin
      case currentState of

        // evaluate simplex (concatenation)
        dsasCalculateAll:
          begin
            if not evaluateSimplex() then
              break;

            // sort simplex by results
            sortSimplex;

            // reduce simplex size to normal simplex size
            SetLength(_currentSimplex, Length(_parametersToOptimize)+1);

            // calculate centroid
            calcCentroid;

            // determine subset of images which is used for further evaluations
            if (not subsetComputed) and
               determineImageSequenceSubset(basicResultList, _currentSimplex) then
            begin
              subsetComputed := true;

              // Resort simplex
              sortSimplex;

              // re-compute list of best results
              for x := 0 to high(_currentSimplex) do
              begin
                _bestResults[x] := _currentSimplex[x].result;
              end;

              // update in bestRun which image sequences are still used for the
              // optimization and re-compute average value
              bestRun.update(basicResultList);
            end;

            currentState := dsasReflection;
          end;

        // Reflection step
        dsasReflection:
          begin
            WriteAndLogLn('DS: Reflection step...');
            if computeReflection then
            begin
              if parametersConverged(_reflection) then
                break;

              if not evaluatePoint(_reflection) then
                break;

              if (_reflection.result < _currentSimplex[high(_currentSimplex)-1].result)
                 and (_reflection.result >= _currentSimplex[0].result) then
              begin
                _currentSimplex[high(_currentSimplex)] := _reflection;

                sortSimplexAndCalcCentroid;
                currentState := dsasReflection;
              end
              else if (_reflection.result < _currentSimplex[0].result) then
                currentState := dsasExpansion
              else
                currentState := dsasContraction;
            end
            else
            begin
              currentState := dsasContraction;
            end;
          end;



        // Expansion step
        dsasExpansion:
          begin
            WriteAndLogLn('DS: Expansion step...');
            if computeExpansion() then
            begin
              if parametersConverged(_expansion) then
                break;

              if not evaluatePoint(_expansion) then
                break;

              if (_expansion.result < _reflection.result) then
                _currentSimplex[high(_currentSimplex)] := _expansion
              else
                _currentSimplex[high(_currentSimplex)] := _reflection;

              sortSimplexAndCalcCentroid;
              currentState := dsasReflection;
            end
            else
            begin
              _currentSimplex[high(_currentSimplex)] := _reflection;

              sortSimplexAndCalcCentroid;
              currentState := dsasReflection;
            end;
          end;



        // Contraction step
        dsasContraction:
          begin
            WriteAndLogLn('DS: Contraction step...');
            if computeContraction then
            begin
              if parametersConverged(_contraction) then
                break;

              if not evaluatePoint(_contraction) then
                break;

              if _contraction.result < _currentSimplex[high(_currentSimplex)].result then
              begin
                _currentSimplex[high(_currentSimplex)] := _contraction;

                sortSimplexAndCalcCentroid;
                currentState := dsasReflection;
              end
              else
                currentState := dsasReduction;
            end
            else
            begin
              currentState := dsasReduction;
            end;
          end;



        // Reduction step
        dsasReduction:
          begin
            WriteAndLogLn('DS: Reduction step...');
            computeReduction;

            // evaluate all vertices except for the best one
            if not evaluateSimplex(1) then
              break;

            sortSimplexAndCalcCentroid;
            currentState := dsasReflection;
          end;
      end;


      // check if results have converged
      if resultsConverged() then
        break;
    end;
  end;
end;

procedure TPS_DownhillSimplex.sortSimplex;
var
  temp : TChosenParameterSetAndResultList;
  x, y, z : Integer;
begin
  SetLength(temp, 0);
  for x := 0 to high(_currentSimplex) do
    for y := 0 to high(temp)+1 do
      if (y = high(temp)+1) or
         (_currentSimplex[x].result < temp[y].result) then
      begin
        SetLength(temp, Length(temp)+1);
        for z := high(temp)-1 downto y do
          temp[z+1] := temp[z];
        temp[y] := _currentSimplex[x];
        break;
      end;

  _currentSimplex := temp;
end;

procedure TPS_DownhillSimplex.calcCentroid;
var
  x, y, z : integer;
  listsSimplex : array[0..1] of TRunParameterList;
  listsTarget : array[0..1] of TRunParameterList;
  sumWeights, weight, temp : extended;
begin
  // initialize centroid
  _centroid.result     := Math.MaxExtended;
  _centroid.parameters := _currentSimplex[0].parameters.clone();

  // calculate normalization factor
  if _weightedCentroid then
  begin
    sumWeights := 0;
    for x := 0 to high(_currentSimplex) do
    begin
      sumWeights := sumWeights + 1 / (_currentSimplex[x].result + 0.001);
    end;
  end
  else
  begin
    sumWeights := length(_currentSimplex) - 1;
  end;

  // compute new parameter values for centroid as (weighted) average of
  // the parameter values in the simplex
  for x := 0 to high(_currentSimplex)-1 do
  begin
    listsSimplex[0] := _currentSimplex[x].parameters.modelParams;
    listsSimplex[1] := _currentSimplex[x].parameters.solverParams;
    listsTarget[0] := _centroid.parameters.modelParams;
    listsTarget[1] := _centroid.parameters.solverParams;

    if _weightedCentroid then
      weight := 1 / (_currentSimplex[x].result + 0.001)
    else
      weight := 1;

    for z := 0 to 1 do
      for y := 0 to high(listsSimplex[z]) do
      begin
        temp := StrToFloat(listsSimplex[z][y].value) * weight;
        if x > 0 then
          temp := temp + StrToFloat(listsTarget[z][y].value) * sumWeights;

        listsTarget[z][y].value := FloatToStr(temp / sumWeights);
      end;
  end;
end;

procedure TPS_DownhillSimplex.sortSimplexAndCalcCentroid;
begin
  sortSimplex;
  calcCentroid;
end;

function TPS_DownhillSimplex.computeReflection: boolean;
var
  y, z : integer;
  listsSimplex : array[0..1] of TRunParameterList;
  listsTarget : array[0..1] of TRunParameterList;
  temp : extended;
begin
  result := true;
  _reflection.result     := Math.MaxExtended;
  _reflection.parameters := _centroid.parameters.clone();

  listsSimplex[0] := _currentSimplex[high(_currentSimplex)].parameters.modelParams;
  listsSimplex[1] := _currentSimplex[high(_currentSimplex)].parameters.solverParams;
  listsTarget[0]  := _reflection.parameters.modelParams;
  listsTarget[1]  := _reflection.parameters.solverParams;
  for z := 0 to 1 do
    for y := 0 to high(listsSimplex[z]) do
    begin
      temp := StrToFloat(listsTarget[z][y].value)
              + _alpha * ( StrToFloat(listsTarget[z][y].value)
                           - StrToFloat(listsSimplex[z][y].value)
                           );
      listsTarget[z][y].value := FloatToStr(temp);

      if    ((listsTarget[z][y].floatConstraints = fcNonnegative) and (temp < 0))
         or ((listsTarget[z][y].floatConstraints = fcPositive) and (temp <= 0)) then
        result := false;
    end;
end;

function TPS_DownhillSimplex.computeExpansion: boolean;
var
  y, z : integer;
  listsSimplex : array[0..1] of TRunParameterList;
  listsTarget : array[0..1] of TRunParameterList;
  temp : extended;
begin
  result := true;
  _expansion.result     := Math.MaxExtended;
  _expansion.parameters := _centroid.parameters.clone();

  listsSimplex[0] := _currentSimplex[high(_currentSimplex)].parameters.modelParams;
  listsSimplex[1] := _currentSimplex[high(_currentSimplex)].parameters.solverParams;
  listsTarget[0] := _expansion.parameters.modelParams;
  listsTarget[1] := _expansion.parameters.solverParams;
  for z := 0 to 1 do
    for y := 0 to high(listsSimplex[z]) do
    begin
      temp := StrToFloat(listsTarget[z][y].value)
              + _gamma * ( StrToFloat(listsTarget[z][y].value)
                           - StrToFloat(listsSimplex[z][y].value)
                           );
      listsTarget[z][y].value := FloatToStr(temp);

      if    ((listsTarget[z][y].floatConstraints = fcNonnegative) and (temp < 0))
         or ((listsTarget[z][y].floatConstraints = fcPositive) and (temp <= 0)) then
        result := false;
    end;
end;

function TPS_DownhillSimplex.computeContraction: boolean;
var
  y, z : integer;
  listsSimplex : array[0..1] of TRunParameterList;
  listsTarget : array[0..1] of TRunParameterList;
  temp : extended;
begin
  result := true;
  _contraction.result     := Math.MaxExtended;
  _contraction.parameters := _centroid.parameters.clone();

  listsSimplex[0] := _currentSimplex[high(_currentSimplex)].parameters.modelParams;
  listsSimplex[1] := _currentSimplex[high(_currentSimplex)].parameters.solverParams;
  listsTarget[0] := _contraction.parameters.modelParams;
  listsTarget[1] := _contraction.parameters.solverParams;
  for z := 0 to 1 do
    for y := 0 to high(listsSimplex[z]) do
    begin
      temp := StrToFloat(listsTarget[z][y].value)
              + _rho * ( StrToFloat(listsTarget[z][y].value)
                         - StrToFloat(listsSimplex[z][y].value)
                         );
      listsTarget[z][y].value := FloatToStr(temp);

      if    ((listsTarget[z][y].floatConstraints = fcNonnegative) and (temp < 0))
         or ((listsTarget[z][y].floatConstraints = fcPositive) and (temp <= 0)) then
        result := false;
    end;
end;

function TPS_DownhillSimplex.computeReduction: boolean;
var
  x, y, z : integer;
  listsSimplex : array[0..1] of TRunParameterList;
  listsSrc : array[0..1] of TRunParameterList;
begin
  result := true;
  listsSrc[0] := _currentSimplex[0].parameters.modelParams;
  listsSrc[1] := _currentSimplex[0].parameters.solverParams;
  for x := 1 to high(_currentSimplex) do
  begin
    listsSimplex[0] := _currentSimplex[x].parameters.modelParams;
    listsSimplex[1] := _currentSimplex[x].parameters.solverParams;
    for z := 0 to 1 do
      for y := 0 to high(listsSimplex[z]) do
      begin
        listsSimplex[z][y].value := FloatToStr(
                                       StrToFloat(listsSrc[z][y].value)
                                       + _sigma * ( StrToFloat(listsSimplex[z][y].value)
                                                    - StrToFloat(listsSrc[z][y].value))
                                     );
      end;
  end;
end;

function TPS_DownhillSimplex.checkResultConvergence: boolean;
var
  temp : extended;
begin
  result := false;
  if not (dscResult in _cvCriteria) then
    exit;

  // first check results in the current simplex
  if _currentSimplex[high(_currentSimplex)].result < Math.MaxExtended then
  begin
    if _currentSimplex[0].result = 0 then
    begin
      result := true;
      exit;
    end;

    temp := _currentSimplex[high(_currentSimplex)].result / _currentSimplex[0].result;
    if temp < _convergenceFactorRES then
      result := true;
  end;

  // check results in the list of best results
  if _bestResults[high(_bestResults)] < Math.MaxExtended then
  begin
    temp := _bestResults[high(_bestResults)] / _bestResults[0];
    if temp < _convergenceFactorRES then
      result := true;
  end;
end;

function TPS_DownhillSimplex.checkParameterConvergence(
  point: TChosenParameterSetAndResult): boolean;
var
  temp, tempCurrent : extended;
  y, z, x, c : integer;
  lists : array[0..1] of array of TRunParameterList;
  tempMax, tempMin : array of extended;
begin
  result := false;

  if not (dscParameter in _cvCriteria) then
    exit;

  SetLength(lists[0], Length(_currentSimplex)+1);
  SetLength(lists[1], Length(_currentSimplex)+1);
  for z := 0 to high(_currentSimplex) do
  begin
    lists[0,z] := _currentSimplex[z].parameters.modelParams;
    lists[1,z] := _currentSimplex[z].parameters.solverParams;
  end;
  lists[0, high(lists[0])] := point.parameters.modelParams;
  lists[1, high(lists[1])] := point.parameters.solverParams;

  c := 0;
  for z := 0 to 1 do
    if Length(lists[z]) > 0 then
      for x := 0 to high(lists[z,0]) do
      begin
        SetLength(tempMax, c+1);
        SetLength(tempMin, c+1);
        tempMax[c] := 0;
        tempMin[c] := MaxExtended;
        for y := 0 to high(lists[z]) do
        begin
          tempCurrent := abs(StrToFloat(lists[z,y,x].value));
          if tempCurrent > tempMax[c] then
            tempMax[c] := tempCurrent;
          if tempCurrent < tempMin[x] then
            tempMin[c] := tempCurrent;
        end;
        inc(c);
      end;

  temp := 0;
  for x := 0 to high(tempMax) do
  begin
    temp := max(temp, abs(tempMax[x]) / (abs(tempMin[x]) + 0.00001) );
  end;

  if temp < _convergenceFactorPM then
    result := true;
end;

class function TPS_DownhillSimplex.helpText: string;
var
  entryName : string;
begin
  entryName := psClassManager.getRegisteredName(ClassName);
  result := ' - "' + entryName + '": "cvParameterAndResult;1.001;1.002;1;2;-0.5;0.5"';
  result := result + #13#10 + '    + parameter 1: convergence criterion ("cvParameter", "cvResult" or "cvParameterAndResult"';
  result := result + #13#10 + '    + parameter 2: maximal factor between lowest and highest value (parameter and/or result)';
  result := result + #13#10 + '    + (only if criterion is "cvParameterAndResult") ';
  result := result + #13#10 + '      parameter 3 (optional): maximal factor between lowest and highest value (result)';
  result := result + #13#10 + '    + parameter 3/4 (optional): alpha';
  result := result + #13#10 + '    + parameter 4/5 (optional): gamma';
  result := result + #13#10 + '    + parameter 5/6 (optional): rho';
  result := result + #13#10 + '    + parameter 6/7 (optional): sigma';
end;

initialization
  psClassManager.registerClass('downhillSimplex', TPS_DownhillSimplex);

end.

