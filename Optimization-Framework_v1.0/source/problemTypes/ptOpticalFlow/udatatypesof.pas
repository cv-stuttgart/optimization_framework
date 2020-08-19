unit uDatatypesOF;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  uDatatypesOptimizer;

type
  TImageSequenceDataOF = class(TImageSequenceData)

  end;

  { TDataReaderOF }

  TDataReaderOF = class(TDataReader)
    class function createImageSequenceDataList(const input : string) : TImageSequenceDataList; override;
  end;

implementation

{ TDataReaderOF }

class function TDataReaderOF.createImageSequenceDataList(const input: string
  ): TImageSequenceDataList;
var
  x, y, lastLine, lastCol : Integer;
  lines, cols, colEntries : TStringList;
  imageSequence : TImageSequenceDataOF;
begin
  result := nil;

  // get image sequences
  // format: 'basePath [string]; image1, image2,... [string];
  //          number of reference frame ["0","1",...];
  //          ground truth file [string]; weight of image sequence for weighted
  //          averaging of error results [float];
  //          (optional) raw file1, raw file2,... [string]'
  lines      := TStringList.Create;
  cols       := TStringList.Create;
  colEntries := TStringList.Create;
  try
    lastLine := -1;
    lines.Delimiter     := #10;
    lines.DelimitedText := input;

    SetLength(result, lines.Count);
    try
      for x := 0 to lines.Count-1 do
      begin
        imageSequence := TImageSequenceDataOF.Create;

        lastLine := x;

        // obtain columns separated by ';'
        lastCol  := -1;
        cols.Delimiter     := ';';
        cols.StrictDelimiter := true;
        cols.DelimitedText := trim(lines[x]);

        // obtain filenames of image sequence in second column separated by ','
        // base path is provided in first column
        lastCol := 1;
        colEntries.Delimiter     := ',';
        colEntries.StrictDelimiter := true;
        colEntries.DelimitedText := trim(cols[1]);

        SetLength(imageSequence.filenames, colEntries.count);
        for y := 0 to colEntries.Count-1 do
        begin
          imageSequence.filenames[y] := trim(cols[0]) + trim(colEntries[y]);
        end;

        // obtain number of reference frame in third column
        lastCol := 2;
        imageSequence.numberOfRefFrame := StrToInt(trim(cols[2]));

        // obtain ground truth file in fourth column
        lastCol := 3;
        imageSequence.groundTruthFile  := trim(cols[0]) + trim(cols[3]);

        // obtain weight for weighted averaging of results in fifth column
        lastCol := 4;
        imageSequence.resultWeight     := StrToFloat(trim(cols[4]));

        // obtain additional files whose content is directly passed without
        // any interpretation
        SetLength(imageSequence.binaryFiles, 0);
        if cols.Count > 5 then
        begin
          lastCol := 5;

          colEntries.DelimitedText := trim(cols[5]);

          SetLength(imageSequence.binaryFiles, colEntries.Count);
          for y := 0 to colEntries.Count-1 do
          begin
            imageSequence.binaryFiles[y] := trim(cols[0]) + trim(colEntries[y]);
          end;
        end;

        if cols.Count > 6 then
        begin
          lastCol := 6;

          imageSequence.seqInfo := trim(cols[6]);
        end;

        imageSequence.size             := 0;

        result[x] := imageSequence;
      end;
    except
      raise Exception.Create( 'Malformed image sequences input on line '
                              + IntToStr(lastline+1) + ', column: '
                              + IntToStr(lastCol+1));
    end;
  finally
    colEntries.Free;
    cols.Free;
    lines.Free;
  end;
end;

end.

