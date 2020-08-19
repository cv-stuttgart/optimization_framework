unit ups_CMAeS;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils,

  uDatatypesOptimizer, uParameterSelection, uOptimizationRun,

  uMath;

type

  TCMAeSAlgorithmState = (cmaesGeneratePopulation, cmaesEvolvePopulation);

  TCMAeSSortResult = record
    values : TExtendedDynArray;
    indices : TIntegerDynArray;
  end;

  { TPS_CMAeS }

  TPS_CMAeS = class(TParameterSelection)
  protected
    // method parameters
    _sigma, _sigmaInit : Extended;
    _stopFitness : Extended;
    _stopEval : Integer;

    function sortPopulation(v : TExtendedDynArray) : TCMAeSSortResult;
  public
    constructor Create(parametersToOptimize : TParameterToOptimizeList;
      varianceQuantile : Extended; const params : string); override;
    procedure runOptimization(basicResultList : TimageSequenceResultList;
      executeRun : TOptimizationRunExecution;
      var bestRun : TOptimizationRunInfo); override;
    class function helpText: string; override;
  end;

implementation

uses
  Math, uSimpleLog;

{ TPS_CMAeS }

constructor TPS_CMAeS.Create(parametersToOptimize: TParameterToOptimizeList;
  varianceQuantile: Extended; const params: string);
var
  N : Integer;
begin
  inherited Create(parametersToOptimize, varianceQuantile, params);

  separateAndSampleDiscreteParameters();

  N            := Length(parametersToOptimize);
  _sigmaInit   := 0.3;
  _stopfitness := 1E-10;
  _stopeval    := trunc(1E3) * N * N;
end;

function TPS_CMAeS.sortPopulation(v: TExtendedDynArray): TCMAeSSortResult;
var
  x, y, z : Integer;
begin
  SetLength(result.values, 0);
  SetLength(result.indices, 0);
  for x := 0 to high(v) do
    for y := 0 to high(result.values)+1 do
      if (y = high(result.values)+1) or
         (v[x] < result.values[y]) then
      begin
        SetLength(result.values, Length(result.values)+1);
        SetLength(result.indices, Length(result.values));
        for z := high(result.values)-1 downto y do
        begin
          result.values[z+1] := result.values[z];
          result.indices[z+1] := result.indices[z];
        end;
        result.values[y] := v[x];
        result.indices[y] := x;
        break;
      end;
end;

procedure TPS_CMAeS.runOptimization(basicResultList: TimageSequenceResultList;
  executeRun: TOptimizationRunExecution; var bestRun: TOptimizationRunInfo);
var
  // parameters: selection
  _lambda, _mu, _N : Integer;
  _mueff : Extended;
  _weights : TExtendedDynArray;

  // parameters: adaption
  _cc, _cs, _c1, _cmu, _damps : Extended;

  // parameters: dynamic
  _xmean, _pc, _ps, _D : TExtendedDynArray;
  _B, _C, _invsqrtC : TExtendedDynMatrix;
  _eigeneval, _chiN : Extended;
  _arx : TExtendedDynMatrix;

  // state parameters
  _counteval : Integer;
  _currentPopulation : TChosenParameterSetAndResultList;
  _currentState : TCMAeSAlgorithmState;
  subsetComputed : boolean;

  tmpParam : TRunParameter;
  paramChoiceList : ^TRunParameterList;
  x, y, d, startD : integer;

  arfitness, xold : TExtendedDynArray;
  arsortResult : TCMAeSSortResult;
  tmpArx, artmp, tmpMat : TExtendedDynMatrix;
  hsig : Integer;
  maxRelativeChange, mu_fp : Extended;

  // copy results for all image sets in extended data structure
  procedure copyResults(src: TChosenParameterSetAndResult;
    var entry : TChosenParameterSetAndResult);
  begin
    entry.resultList := Copy(src.resultList, 0, Length(src.resultList));
    entry.result     := src.result;
  end;

  // evaluate population
  procedure evaluatePopulation();
  var
    currentParamsList : TChosenParameterSetList;
    currentResultMatrix : TChosenParameterSetAndResultList;
    x : Integer;
  begin
    SetLength(currentParamsList, Length(_currentPopulation));
    for x := 0 to high(currentParamsList) do
      if d <> -1 then
        currentParamsList[x] := _currentPopulation[x].parameters.merge(_discreteParameters[d])
      else
        currentParamsList[x] := _currentPopulation[x].parameters.clone();

    currentResultMatrix := executeRun(currentParamsList, basicResultList);

    for x := 0 to high(currentParamsList) do
      copyResults(currentResultMatrix[x], _currentPopulation[x]);
  end;

begin
  _N             := Length(_parametersToOptimize);
  subsetComputed := _varianceQuantile > 99.9;

  if Length(_discreteParameters) = 0 then
    startD := -1
  else
    startD := 0;

  for d := startD to high(_discreteParameters) do
  begin
    // initialize _sigma
    _sigma := _sigmaInit;

    // initialize _xmean
    SetLength(_xmean, _N);
    for x := 0 to _N-1 do
    begin
      _xmean[x] := StrToFloat(_parametersToOptimize[x].values[0]);
    end;

    // parameters: selection
    _lambda := 4 + floor(3 * lnxp1(_N));
    mu_fp   := 0.5 * _lambda;
    _mu     := floor(mu_fp);
    SetLength(_weights, _mu);
    for x := 0 to high(_weights) do
      _weights[x] := lnxp1(mu_fp + 0.5) - lnxp1(x+1);
    _weights := compDiv(_weights, sum(_weights));
    _mueff   := sqr(sum(_weights)) / norm(_weights);

    // parameters: adaption
    _cc  := (4 + _mueff / _N) / (_N+4 + 2*_mueff / _N);
    _cs  := (_mueff + 2) / (_N + _mueff + 5);
    _c1  := 2 / (sqr(_N + 1.3) + _mueff);
    _cmu := Math.min(1 - _c1, 2 * (_mueff -2 + 1/_mueff) / (sqr(_N+2) + _mueff));
    _damps := 1 + 2 * Math.max(0, sqrt( (_mueff-1) / (_N+1)) - 1) + _cs;

    // parameters and constants: dynamic
    _pc := zeros(_N);
    _ps := zeros(_N);
    _B  := eye(_N, _N);
    _D  := ones(_N);
    for x := 0 to _N-1 do
      _D[x] := _D[x] * (max(abs(StrToFloat(_parametersToOptimize[x].values[0]) / 2), 0.001));
    _C  := multiply(multiply(_B, diag(compSqr(_D))), transpose(_B));
    _invsqrtC := multiply(multiply(_B, diag(compInv(_D))), transpose(_B));
    _eigeneval := 0;
    _chiN := sqrt(_N) * (1 - 1/(4*_N) + 1/(21 * sqr(_N)));

    // initialize state variables
    _counteval            := 0;
    SetLength(_currentPopulation, _lambda);
    SetLength(_arx, _lambda);
    _currentState := cmaesGeneratePopulation;

    while _counteval < _stopeval do
    begin
      case _currentState of


        cmaesGeneratePopulation:
          begin
            WriteAndLogLn('CMAeS: mean');
            WriteAndLogLn(printVector(_xmean));

            WriteAndLogln('CMAeS: D');
            WriteAndLogln(printVector(_D));

            WriteAndLogLn('CMAeS: sigma');
            WriteAndLogLn(FloatToStr(_sigma));
            WriteAndLogLn('');

            for x := 0 to high(_currentPopulation) do
            begin
              _currentPopulation[x].resultList := nil;
              _currentPopulation[x].result     := Math.MaxExtended;

              _arx[x] := compMul(_D, randVecN(_N));

              tmpMat := columnVec(_arx[x]);

              _arx[x] := columnVec(multiply(_B,tmpMat), 0);
              _arx[x] := compMul(_arx[x], _sigma);
              _arx[x] := add(_xmean, _arx[x]);

              SetLength(_currentPopulation[x].parameters.modelParams, 0);
              SetLength(_currentPopulation[x].parameters.solverParams, 0);
              for y := 0 to _N-1 do
              begin
                tmpParam.name        := _parametersToOptimize[y].name;
                tmpParam.floatConstraints := _parametersToOptimize[y].floatConstraints;

                if (tmpParam.floatConstraints = fcNonnegative) and (_arx[x][y] < 0) then
                  _arx[x][y] := 0
                else if (tmpParam.floatConstraints = fcPositive) and (_arx[x][y] <= 0) then
                  _arx[x][y] := MIN_POSITIVE_FLOAT;

                tmpParam.value     := FloatToStr(_arx[x][y]);

                if _parametersToOptimize[y].modelParam then
                  paramChoiceList := @_currentPopulation[x].parameters.modelParams
                else
                  paramChoiceList := @_currentPopulation[x].parameters.solverParams;

                SetLength(paramChoiceList^, Length(paramChoiceList^)+1);
                paramChoiceList^[high(paramChoiceList^)] := tmpParam;
              end;

              inc(_counteval);
            end;

            evaluatePopulation();

            if (not subsetComputed)
               and determineImageSequenceSubset(basicResultList, _currentPopulation) then
            begin
              subsetComputed := true;

              bestRun.update(basicResultList);
            end;

            _currentState := cmaesEvolvePopulation;
          end;




        cmaesEvolvePopulation:
          begin
            // copy fitnesses into array
            SetLength(arfitness, Length(_currentPopulation));
            for x := 0 to high(arfitness) do
              arfitness[x] := _currentPopulation[x].result;

            // sort by fitness
            arsortResult := sortPopulation(arfitness);
            xold         := copyVec(_xmean);

            WriteAndLogLn('Sorted results:');
            for x := 0 to high(arsortResult.values) do
              WriteAndLogLn(FloatToStr(arsortResult.values[x]) + ' (' + IntToStr(arsortResult.indices[x]) + ')');
            WriteAndLogLn('');


            // take the parameters of the _mu best parameter sets and compute the
            // weighted sum
            SetLength(tmpArx, _mu);
            for x := 0 to _mu-1 do
              tmpArx[x] := copyVec(_arx[arsortResult.indices[x]]);
            tmpArx := transpose(tmpArx);

            _xmean := columnVec(multiply(tmpArx, columnVec(_weights)), 0);

            // cumulation: update evolution paths
            _ps := compMul(_ps, (1 - _cs));
            _ps := add(_ps, compMul( columnVec(
                                       multiply(_invsqrtC, columnVec(sub(_xmean, xold)))
                                     , 0),
                                     sqrt(_cs*(2-_cs) * _mueff) / _sigma));
            hsig := ord(norm(_ps) / sqrt(1 - power(1-_cs, 2*_counteval/_lambda)) / _chiN < 1.4 * 2 / (_N+1));
            _pc := compMul(_pc, (1 - _cc));
            _pc := add(_pc, compMul( sub(_xmean, xold), hsig * sqrt(_cc * (2 - _cc) * _mueff) / _sigma));

            // adapt covariance matrix _C
            artmp := compMul(sub(tmpArx, repMat(columnVec(xold),1,_mu)), 1/_sigma);

            _C := add(
                    compMul(_C, (1-_c1-_cmu)),
                    add(
                      compMul(
                        add(
                          multiply(columnVec(_pc), rowVec(_pc)),
                          compMul(_C, (1-hsig) * _cc * (2-_cc))
                          ),
                        _c1
                      ),
                      compMul(
                        multiply(artmp, multiply(diag(_weights), transpose(artmp))),
                        _cmu
                      )
                    )
                  );

            // Adapt step size sigma
            _sigma := _sigma * exp( (_cs/_damps) * (norm(_ps)/_chiN - 1) );

            // Decomposition of _C into B * diag(D.^2) * B' (diagonalization)
            if _counteval - _eigeneval > _lambda / (_c1 + _cmu) / _N / 10 then
            begin
              _eigeneval := _counteval;

              // enforce symmetry
              _C := add(triu(_C), transpose(triu(_C,1)));

              // eigenvalue decomposition
              eig(_C, _B, _D);

              _D := apply(_D, @sqrt1);

              //_invsqrtC := multiply(_B, multiply(diag(apply(_D, @inv)), transpose(_B)));
              _invsqrtC := multiply(_B, multiply(diag(apply(_D, @invRegularized)), transpose(_B)));
            end;






            // check termination
            maxRelativeChange := 0;
            for x := 0 to high(_D) do
            begin
              maxRelativeChange := max(
                                     maxRelativeChange,
                                     _sigma * _D[x] / (abs(_xmean[x]) + 0.001)
                                   )
            end;

            if (arsortResult.values[0] <= _stopfitness) or
               (max(_D) > 1e7 * min(_D)) or
               (maxRelativeChange < 0.03) then
            begin
              break;
            end;

            _currentState := cmaesGeneratePopulation;
          end;
      end;
    end;
  end;
end;

class function TPS_CMAeS.helpText: string;
var
  entryName : string;
begin
  entryName := psClassManager.getRegisteredName(ClassName);
  result := ' - "' + entryName + '": ""';
  result := result + #13#10 + '    (does not take any parameters)';
end;

initialization
  psClassManager.registerClass('cma_es', TPS_CMAeS);

end.

