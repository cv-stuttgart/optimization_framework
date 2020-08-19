unit uMath;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

{
This unit contains:
- matrix- and vector-datatypes and -operations
}

uses
  Classes, SysUtils;

type
  TExtendedDynArray = array of extended;
  TExtendedDynMatrix = array of TExtendedDynArray;

  TSingleFunc = function(x : Single): Single;
  TExtendedFunc = function(s : Extended): Extended;

  TEigenDecompResult = record
    B: TExtendedDynMatrix;
    D: TExtendedDynArray;
  end;

function sqr(x : Extended) : Extended;
function sqrt1(x : Extended) : Extended;
function inv(x : Extended) : Extended;
function invRegularized(x : Extended) : Extended;
function power(a, b : Extended) : Extended;
function power(a : Single; b : Integer) : Single;

function randVec(N : integer) : TExtendedDynArray;
function randVecN(N : integer) : TExtendedDynArray;
function zeros(N : integer) : TExtendedDynArray;
function ones(N : integer) : TExtendedDynArray;
function add(v1, v2 : TExtendedDynArray): TExtendedDynArray;
function sub(v1, v2 : TExtendedDynArray): TExtendedDynArray;
function compMul(v1, v2 : TExtendedDynArray): TExtendedDynArray;
function copyVec(v : TExtendedDynArray): TExtendedDynArray;
function norm(v : TExtendedDynArray): Extended;
function sum(v : TExtendedDynArray): Extended;
function max(v : TExtendedDynArray) : Extended;
function min(v : TExtendedDynArray) : Extended;
function compDiv(v : TExtendedDynArray; val : Extended): TExtendedDynArray;
function compMul(v : TExtendedDynArray; val : Extended): TExtendedDynArray;
function compSqr(v : TExtendedDynArray): TExtendedDynArray;
function compInv(v : TExtendedDynArray): TExtendedDynArray;

function columnVec(v : TExtendedDynArray): TExtendedDynMatrix;
function rowVec(v : TExtendedDynArray): TExtendedDynMatrix;
function columnVec(A : TExtendedDynMatrix; no : Integer): TExtendedDynArray;
function rowVec(A : TExtendedDynMatrix; no : Integer): TExtendedDynArray;
function repMat(C : TExtendedDynMatrix; rows, cols : Cardinal): TExtendedDynMatrix;
function eye(N1, N2 : integer): TExtendedDynMatrix;
function transpose(B : TExtendedDynMatrix): TExtendedDynMatrix;
function diag(v : TExtendedDynArray): TExtendedDynMatrix;
function multiply(A, B: TExtendedDynMatrix): TExtendedDynMatrix;
function add(A, B: TExtendedDynMatrix): TExtendedDynMatrix;
function sub(A, B: TExtendedDynMatrix): TExtendedDynMatrix;
function compMul(A : TExtendedDynMatrix; val : Extended): TExtendedDynMatrix;
function triU(A: TExtendedDynMatrix; startOff : Integer = 0): TExtendedDynMatrix;
function apply(A: TExtendedDynArray; func : TSingleFunc): TExtendedDynArray;
function apply(A: TExtendedDynArray; func : TExtendedFunc): TExtendedDynArray;
function apply(A: TExtendedDynMatrix; func : TSingleFunc): TExtendedDynMatrix;
function apply(A: TExtendedDynMatrix; func : TExtendedFunc): TExtendedDynMatrix;
function eig(B: TExtendedDynMatrix; sort : boolean = false): TEigenDecompResult;
procedure eig(C: TExtendedDynMatrix; out B: TExtendedDynMatrix;
  out D: TExtendedDynArray; sort : boolean = false);
function copyMat(A: TExtendedDynMatrix): TExtendedDynMatrix;
function printMatrix(A: TExtendedDynMatrix): string;
function printVector(v: TExtendedDynArray): string;

implementation

uses
  Math;

function sqr(x : Extended) : Extended;
begin
  result := x * x;
end;

function sqrt1(x : Extended) : Extended;
begin
  result := sqrt(x);
end;

function inv(x : Extended) : Extended;
begin
  result := 1 / x;
end;

function invRegularized(x : Extended) : Extended;
begin
  result := 1 / (sign(x) * (abs(x) + 0.000001));
end;

function power(a, b : Extended) : Extended;
begin
  result := exp(b * lnxp1(a));
end;

function power(a: Single; b: Integer): Single;
var
  x : integer;
begin
  result := 1;
  for x := 1 to b do
    result := result * a;
end;

function randVec(N : integer) : TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, N);
  for x := 0 to N-1 do
    result[x] := random();
end;

function randVecN(N : integer) : TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, N);
  for x := 0 to N-1 do
    result[x] := randg(0,1);
end;

function zeros(N : integer) : TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, N);
  for x := 0 to N-1 do
    result[x] := 0;
end;

function ones(N : integer) : TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, N);
  for x := 0 to N-1 do
    result[x] := 1;
end;

function add(v1, v2 : TExtendedDynArray): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(v1));
  for x := 0 to high(result) do
    result[x] := v1[x] + v2[x];
end;

function sub(v1, v2 : TExtendedDynArray): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(v1));
  for x := 0 to high(result) do
    result[x] := v1[x] - v2[x];
end;

function compMul(v1, v2 : TExtendedDynArray): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(v1));
  for x := 0 to high(result) do
    result[x] := v1[x] * v2[x];
end;

function copyVec(v : TExtendedDynArray): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(v));
  for x := 0 to high(result) do
    result[x] := v[x];
end;

function norm(v : TExtendedDynArray): Extended;
var
  x : integer;
begin
  result := 0;
  for x := 0 to high(v) do
    result := result + v[x] * v[x];
end;

function sum(v : TExtendedDynArray): Extended;
var
  x : integer;
begin
  result := 0;
  for x := 0 to high(v) do
    result := result + v[x];
end;

function max(v : TExtendedDynArray) : Extended;
var
  x : integer;
begin
  result := v[0];
  for x := 1 to high(v) do
    if v[x] > result then
      result := v[x];
end;

function min(v : TExtendedDynArray) : Extended;
var
  x : integer;
begin
  result := v[0];
  for x := 1 to high(v) do
    if v[x] < result then
      result := v[x];
end;

function compDiv(v : TExtendedDynArray; val : Extended): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(v));
  for x := 0 to high(v) do
    result[x] := v[x] / val;
end;

function compMul(v : TExtendedDynArray; val : Extended): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(v));
  for x := 0 to high(v) do
    result[x] := v[x] * val;
end;

function compSqr(v : TExtendedDynArray): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(v));
  for x := 0 to high(v) do
    result[x] := sqr(v[x]);
end;

function compInv(v : TExtendedDynArray): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(v));
  for x := 0 to high(v) do
    result[x] := 1 / v[x];
end;

function columnVec(v : TExtendedDynArray): TExtendedDynMatrix;
var
  x : integer;
begin
  SetLength(result, Length(v), 1);
  for x := 0 to high(v) do
    result[x, 0] := v[x];
end;

function rowVec(v : TExtendedDynArray): TExtendedDynMatrix;
var
  x : integer;
begin
  SetLength(result, 1, Length(v));
  for x := 0 to high(v) do
    result[0, x] := v[x];
end;

function columnVec(A : TExtendedDynMatrix; no : Integer): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(A));
  for x := 0 to high(result) do
    result[x] := A[x, no];
end;

function rowVec(A : TExtendedDynMatrix; no : Integer): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(A[no]));
  for x := 0 to high(result) do
    result[x] := A[no, x];
end;

function repMat(C : TExtendedDynMatrix; rows, cols : Cardinal): TExtendedDynMatrix;
var
  a, b, x, y : integer;
begin
  SetLength(result, rows * Length(C), cols * Length(C[0]));
  for a := 0 to rows-1 do
    for b := 0 to cols-1 do
      for x := 0 to high(C) do
        for y := 0 to high(C[x]) do
        begin
          result[a*Length(C) + x, b*Length(C[0]) + y] := C[x,y];
        end;
end;

function eye(N1, N2 : integer): TExtendedDynMatrix;
var
  x, y : integer;
begin
  SetLength(result, N1, N2);
  for x := 0 to N1-1 do
    for y := 0 to N2-1 do
    begin
      if (x = y) then
        result[x,y] := 1
      else
        result[x,y] := 0;
    end;
end;

function transpose(B : TExtendedDynMatrix): TExtendedDynMatrix;
var
  x, y : integer;
begin
  SetLength(result, Length(B[0]), Length(B));
  for x := 0 to high(B) do
    for y := 0 to high(B[0]) do
    begin
      result[y,x] := B[x,y];
    end;
end;

function diag(v : TExtendedDynArray): TExtendedDynMatrix;
var
  x, y : integer;
begin
  SetLength(result, Length(v), Length(v));
  for x := 0 to high(v) do
    for y := 0 to high(v) do
    begin
      if (x = y) then
        result[x,y] := v[x]
      else
        result[x,y] := 0;
    end;
end;

function multiply(A, B: TExtendedDynMatrix): TExtendedDynMatrix;
var
  x, y, z, p, q : integer;
  temp : Extended;
begin
  //Writeln('multiply');
  p := Length(B[0]);
  q := Length(A);
  {Writeln('multiply-0');
  Writeln('B: ', Length(B), ' x ', p);
  Writeln('A: ', Length(A));
  Writeln('A2: ', Length(A[0]));}
  //SetLength(result, Length(A), p);
  SetLength(result, q);
  for x := 0 to high(result) do
    SetLength(result[x], p);
  //Writeln('multiply-1');
  for x := 0 to high(result) do
  begin
    //Writeln('multiply-2');
    for y := 0 to high(result[x]) do
    begin
      //Writeln('Set result[x,y] := 0');
      result[x,y] := 0;
      for z := 0 to high(A[x]) do
      begin
        //Writeln('(', x, ', ', y, ', ', z, ')');
        temp := A[x,z];
        //Writeln('Read B');
        temp := temp * B[z,y];
        //Writeln('Write result');
        result[x,y] := result[x,y] + temp;
        //Writeln();
        //Writeln();
      end;
    end;
  end;
end;

function add(A, B: TExtendedDynMatrix): TExtendedDynMatrix;
var
  x, y : integer;
begin
  SetLength(result, Length(A), Length(A[0]));
  for x := 0 to high(result) do
    for y := 0 to high(result[x]) do
      result[x, y] := A[x,y] + B[x,y];
end;

function sub(A, B: TExtendedDynMatrix): TExtendedDynMatrix;
var
  x, y : integer;
begin
  SetLength(result, Length(A), Length(A[0]));
  for x := 0 to high(result) do
    for y := 0 to high(result[x]) do
      result[x, y] := A[x,y] - B[x,y];
end;

function compMul(A : TExtendedDynMatrix; val : Extended): TExtendedDynMatrix;
var
  x, y : integer;
begin
  SetLength(result, Length(A), Length(A[0]));
  for x := 0 to high(result) do
    for y := 0 to high(result[x]) do
      result[x, y] := A[x,y] * val
end;

function triU(A: TExtendedDynMatrix; startOff : Integer = 0): TExtendedDynMatrix;
var
  x, y : integer;
begin
  SetLength(result, Length(A), Length(A[0]));
  for x := 0 to high(A) do
    for y := 0 to high(A[x]) do
    begin
      if y <= x - startOff then
        result[x,y] := 0
      else
        result[x,y] := A[x,y];
    end;
end;

function apply(A: TExtendedDynArray; func : TSingleFunc): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(A));
  for x := 0 to high(A) do
    result[x] := func(A[x]);
end;


function apply(A: TExtendedDynArray; func : TExtendedFunc): TExtendedDynArray;
var
  x : integer;
begin
  SetLength(result, Length(A));
  for x := 0 to high(A) do
    result[x] := func(A[x]);
end;

function apply(A: TExtendedDynMatrix; func : TSingleFunc): TExtendedDynMatrix;
var
  x, y : integer;
begin
  SetLength(result, Length(A), Length(A[0]));
  for x := 0 to high(A) do
    for y := 0 to high(A[x]) do
    begin
        result[x,y] := func(A[x,y]);
    end;
end;


function apply(A: TExtendedDynMatrix; func : TExtendedFunc): TExtendedDynMatrix;
var
  x, y : integer;
begin
  SetLength(result, Length(A), Length(A[0]));
  for x := 0 to high(A) do
    for y := 0 to high(A[x]) do
    begin
        result[x,y] := func(A[x,y]);
    end;
end;

function eig(B: TExtendedDynMatrix; sort: boolean): TEigenDecompResult;
const
  delta = 0.00000000000000001;
  eps = 0.0000000000001;
var
  i,j,p,q,k : Integer;
  sum, theta, t, c : Extended;
  r, s, g, h : Extended;
  help : Extended;
  a : TExtendedDynMatrix;
  N : Integer;
begin
  N := Length(B);
  a := copyMat(B);

  result.B := eye(N,N);

  repeat
    sum := 0;
    for i := 1 to N-1 do
      for j := 0 to I-1 do
        sum := sum + a[i,j] * a[i,j];

    if sum + sum > eps * eps then
    begin
      for p := 0 to N-2 do
        for q := p+1 to N-1 do
        begin
          if abs(a[q,p]) >= eps * eps then
          begin
            theta := (a[q,q] - a[p,p]) / (2* a[q,p]);

            t := 1;
            if abs(theta) > delta then
              t := 1 / (theta + theta / abs(theta) * sqrt(theta * theta + 1));

            c := 1 / sqrt(1 + t * t);
            s := c * t;
            r := s / (1 + c);

            a[p,p] := a[p,p] - t * a[q,p];
            a[q,q] := a[q,q] + t * a[q,p];
            a[q,p] := 0;

            for j := 0 to p-1 do
            begin
              g := a[p,j] + r * a[i,p];
              h := a[p,j] - r * a[q,j];

              a[p,j] := a[p,j] - s * g;
              a[q,j] := a[q,j] + s * h;
            end;

            for i := p+1 to q-1 do
            begin
              g := a[q,i] + r * a[i,p];
              h := a[i,p] - r * a[q,i];
              a[i,p] := a[i,p] - s * g;
              a[q,i] := a[q,i] + s * h;
            end;

            for i := q+1 to N-1 do
            begin
              g := a[i,q] + r * a[i,p];
              h := a[i,p] - r * a[i,q];
              a[i,p] := a[i,p] - s * g;
              a[i,q] := a[i,q] + s * h;
            end;

            for i := 0 to N-1 do
            begin
              g := result.B[i,q] + r * result.B[i,p];
              h := result.B[i,p] - r * result.B[i,q];
              result.B[i,p] := result.B[i,p] - s * g;
              result.B[i,q] := result.B[i,q] + s * h;
            end;
          end;
        end;
    end;
  until sum + sum <= eps * eps;

  SetLength(result.D, N);
  for i := 0 to N-1 do
    result.D[i] := a[i,i];

  if not sort then
    exit;

  // order eigenvalues and eigenvectors
  for i := 0 to N-2 do
  begin
    k := i;
    for j := i+1 to N-1 do
      if abs(result.D[j]) > abs(result.D[k]) then
        k := j;

    if k <> i then
    begin
      help := result.D[k];
      result.D[k] := result.D[i];
      result.D[i] := help;

      for j := 0 to N-1 do
      begin
        help := result.B[j,k];
        result.B[j,k] := result.B[j,i];
        result.B[j,i] := help;
      end;
    end;
  end;
end;

procedure eig(C: TExtendedDynMatrix; out B: TExtendedDynMatrix;
  out D: TExtendedDynArray; sort: boolean);
var
  tmp : TEigenDecompResult;
begin
  tmp := eig(C);
  B := tmp.B;
  D := tmp.D;
end;

function copyMat(A: TExtendedDynMatrix): TExtendedDynMatrix;
var
  x,y : integer;
begin
  SetLength(result, Length(A), Length(A[0]));
  for x := 0 to high(A) do
    for y := 0 to high(A[0]) do
    begin
      result[x,y] := A[x,y];
    end;
end;

function printMatrix(A: TExtendedDynMatrix): string;
var
  x, y : integer;
begin
  result := '';
  for x := 0 to high(A) do
  begin
    for y := 0 to high(A[x]) do
    begin
      result := result + Format('%4.3f', [A[x,y]]) + ' ';
    end;
    result := result + #10;
  end;
end;

function printVector(v: TExtendedDynArray): string;
var
  x : integer;
begin
  result := '';
  for x := 0 to high(v) do
    result := result + ' ' + FloatToStr(v[x]) + #10;
end;

end.

