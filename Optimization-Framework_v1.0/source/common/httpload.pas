unit httpLoad;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function httpGet(const URL : string; output : TMemoryStream) : boolean;
function httpPOST(const url, post: string; output : TMemoryStream): boolean;

implementation

uses
  Pipes, Process;

function EncodeSeconds(t : Cardinal) : TDateTime;
var
  h, m, s : Word;
begin
  t := t mod 86400;
  h := t div 3600;
  t := t mod 3600;
  m := t div 60;
  t := t mod 60;
  s := t;

  result := EncodeTime(h, m, s, 0);
end;

function executeProcess(const cmdLine : string; output : TMemoryStream;
  error : TMemoryStream = nil; timeout : Cardinal = 0): boolean;
const
  READ_BYTES = 2048;
var
  wget : TProcess;
  maxTime : TDateTime;

  processOutput : TMemoryStream;
  bytesRead, bytesReadError, n, nError : Integer;

  procedure doReadStep(var bRead, nTemp : Integer; src : TInputPipeStream;
    target : TMemoryStream);
  var
    temp : string;
  begin
    SetLength(temp, READ_BYTES + 1);
    target.SetSize(bRead + READ_BYTES);

    nTemp := src.Read(temp[1], READ_BYTES);

    if nTemp > 0 then
    begin
      SetLength(temp, nTemp);
      target.Write(temp[1], nTemp);
      inc(bRead, nTemp);
    end
    else
      Sleep(10);
  end;

begin
  result := false;

  wget := TProcess.Create(nil);
  try
    {$WARNINGS Off}
    wget.CommandLine := cmdLine;
    {$WARNINGS On}

    wget.Options := [poUsePipes];

    processOutput := TMemoryStream.Create;
    try
      wget.Execute;

      bytesRead := 0;
      n         := 0;
      maxTime   := time() + EncodeSeconds(timeout);
      while (wget.Running) and ( (timeout = 0) or (time() < maxTime)) do
        doReadStep(bytesRead, n, wget.Output, processOutput);

      // read remaining output after run has finished
      repeat
        doReadStep(bytesRead, n, wget.Output, processOutput);
      until n <= 0;

      // set size of buffer to proper value
      processOutput.SetSize(bytesRead);
      processOutput.Position := 0;

      output.CopyFrom(processOutput, processOutput.Size);
    finally
      processOutput.Free;
    end;
    output.Position := 0;

    //Writeln('Read error...');
    if (error <> nil) then
    begin
      //wget.Stderr.Position := 0;
      nError         := 0;
      bytesReadError := 0;
      repeat
        doReadStep(bytesReadError, nError, wget.Stderr, error);
      until nError <= 0;

      error.SetSize(bytesReadError);
      error.Position := 0;

      result := bytesReadError <> 0;
    end
    else
      result := wget.Stderr.Read(n, sizeof(n)) = 0;
    //Writeln('...finished');
    //Writeln(result, #13#10, url, #13#10, post);
  finally
    wget.Free;
  end;
end;

function executeProcess(const cmdLine : string; out output : string): boolean;
var
  temp : TStringList;
  tempOut : TMemoryStream;
begin
  tempOut := TMemoryStream.Create;
  try
   result := executeProcess(cmdLine, tempOut);

    temp := TStringList.Create;
    try
      temp.LoadFromStream(tempOut);

      output := temp.Text;
    finally
      temp.Free;
  end;
  finally
    tempOut.Free;
  end;
end;

function httpPOST(const url, post: string; output : TMemoryStream): boolean;
var
  tempFile : string;
  tempPostData, tempError : TStringList;
  error : TMemoryStream;
begin
  executeProcess('mktemp', tempFile);
  tempFile := trim(tempFile);

  tempPostData := TStringList.Create;
  try
    tempPostData.Text := post;

    tempPostData.SaveToFile(tempFile);
  finally
    tempPostData.Free;
  end;

  //Writeln('Temp file exists: ', FileExists(tempFile));

  //Writeln('Post....');
  error := TMemoryStream.Create;
  try
    result := executeProcess('wget -qO- -t 3 -T 20 --post-file=' + tempFile + ' ' + url, output, error, 70);
    //result := executeProcess('wget -qO- ' + url, output, error);

    //Writeln('O-Size: ', output.Size);
    //Writeln('E-Size: ', error.Size);

    if error.Size <> 0 then
    begin
      error.position := 0;
      tempError := TStringList.Create;
      try
        tempError.LoadFromStream(error);

        //Writeln('Error: ', tempError.Text);
      finally
        tempError.Free;
      end;
    end;

  finally
    error.Free;
  end;

  //Writeln('wget -qO- --post-file=' + tempFile + ' ' + url);

  SysUtils.DeleteFile(tempFile);

  //result := executeProcess('wget -qO- --post-data "' + post + '" ' + url, output);
end;

function httpGet(const URL : string; output : TMemoryStream) : boolean;
var
  error : TMemoryStream;
begin
  error := TMemoryStream.Create;
  try
    //Writeln('wget -qO- -t 3 -T 20 ' + url);
    result := executeProcess('wget -qO- -t 3 -T 20 ' + url, output, error, 70);

    error.Position  := 0;
    output.Position := 0;
  finally
    error.Free;
  end;
end;

{function httpGet(const URL : string; output : TMemoryStream) : boolean;
const
  READ_BYTES = 2048;
var
  wget : TProcess;
  processOutput : TMemoryStream;
  bytesRead, n : Integer;

  procedure doReadStep;
  var
    temp : string;
  begin
    SetLength(temp, READ_BYTES + 1);
    processOutput.SetSize(bytesRead + READ_BYTES);

    n := wget.Output.Read(temp[1], READ_BYTES);

    if n > 0 then
    begin
      SetLength(temp, n);
      processOutput.Write(temp[1], n);
      inc(bytesRead, n);
    end
    else
      Sleep(10);
  end;

begin
  result := false;

  wget := TProcess.Create(nil);
  try
    {$WARNINGS Off}
    wget.CommandLine := 'wget -qO- ' + url;
    {$WARNINGS On}

    wget.Options := [poUsePipes];

    processOutput := TMemoryStream.Create;
    try
      wget.Execute;

      bytesRead := 0;
      n         := 0;
      while wget.Running do
        doReadStep;

      // read remaining output after run has finished
      repeat
        doReadStep;
      until n <= 0;

      // set size of buffer to proper value
      processOutput.SetSize(bytesRead);
      processOutput.Position := 0;

      output.CopyFrom(processOutput, processOutput.Size);
    finally
      processOutput.Free;
    end;

    output.Position := 0;

    result := wget.Stderr.Read(n, sizeof(n)) = 0;
  finally
    wget.Free;
  end;
end;}

end.

