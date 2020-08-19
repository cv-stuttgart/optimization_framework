unit uHTTP_Utils;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

{
This unit contains functions for encoding and decoding strings to be used
as parts of URLs
}

uses
  Classes, SysUtils;

function formatTime(t : TDateTime) : string;
function ParamsEncode(const ASrc: string): string;
function ParamsDecode(const ASrc: string): string;

implementation

function formatTime(t : TDateTime) : string;
var
  h, m, s, ms : Word;
begin
  DecodeTime(t, h, m, s, ms);
  result := Format('%2.2d', [h]) + ':' + Format('%2.2d', [m]) + ':'
            + Format('%2.2d', [s]);
end;

function CharRange(c1, c2 : Char) : string;
var
  x : Char;
begin
  result := '';
  for x := c1 to c2 do
    result := result + x;
end;

function CharIsInSet(const ASrc : string; i : Integer; const s : string) : boolean;
var
  x : integer;
begin
  for x := 1 to Length(s) do
    if ASrc[i] = s[x] then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

function ParamsEncode(const ASrc: string): string;
var
  i : Integer;
const
  UnsafeChars = '&*#%<> []';
begin
  Result := '';
  for i := 1 to Length(ASrc) do
  begin
    if CharIsInSet(ASrc, i, UnsafeChars)
       or (not CharIsInSet(ASrc, i, CharRange(#33,#128))) then
    begin
      Result := Result + '%' + IntToHex(Ord(ASrc[i]), 2);
    end
    else
    begin
      Result := Result + ASrc[i];
    end;
  end;
end;

function ParamsDecode(const ASrc: string): string;
var
  i, hexValue : Integer;
  hexCode : string;
begin
  Result := '';
  i := 1;
  while i <= Length(ASrc) do
  begin
    case ASrc[i] of
      '%':
      begin
        if i+2 <= Length(ASrc) then
        begin
          hexCode := ASrc[i+1] + ASrc[i+2];
          hexValue := StrToIntDef('$'+hexCode, 0);
          if (hexValue > 0) and (hexValue <= 255) then
            result := result + Char(hexValue);

          inc(i,3);
        end;
      end;

      else
      begin
        result := result + ASrc[i];
        inc(i);
      end;
    end;
  end;
end;

end.

