program optimizerTest;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

uses
  SysUtils, StrUtils;

function isParam(const Name: string; parampos: integer;
  out Value: string): boolean;
var
  param: string;
begin
  param := ParamStr(parampos);
  Result := StrUtils.AnsiStartsStr(Name, param);
  if Result then
  begin
    Value := Copy(param, Length(Name + '=') + 1, Length(param));
    Value := StringReplace(Value, '"', '', [rfReplaceAll]);
  end;
end;

var
  x : integer;
  value : string;
  alpha, beta, a, b : Single;
  mode : Word;
  result : extended;
  //formatSettings : TFormatSettings;

  function sqr(a : Extended) : Extended;
  begin
    result := a * a;
  end;

begin
  //formatSettings.DecimalSeparator := '.';

  alpha := 100000;
  beta  := alpha;
  mode  := 0;
  for x := 1 to ParamCount() do
  begin
    Writeln('Param: ',  ParamStr(x));
    if isParam('--alpha', x, value) then
    begin
      alpha := StrToFloatDef(value, 100000);
    end
    else if isParam('--beta', x, value) then
    begin
      beta := StrToFloatDef(value, 100000);
    end
    else if isParam('--mode', x, value) then
    begin
      mode := StrToIntDef(value, 0);
    end;
  end;

  Writeln('Mode: ', mode);

  case mode of
    0:
      begin
        a := 1;
        b := 100;

        result := sqr(a - alpha) + b * sqr(beta - sqr(alpha));
      end;

    1:
      result := alpha * beta;
  end;


  Writeln('T: ', FloatToStr(result));
end.

