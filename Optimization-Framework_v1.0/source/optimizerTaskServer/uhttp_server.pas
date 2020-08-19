unit uHTTP_Server;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

{
This unit contains a webserver based on the "synapse" package preprocessing
requests which then are handled using a method implementing the THTTPHandler
signature.
TTCPHttpDaemon is the server class which creates instances of TTCPHttpThrd when
incoming connections are established. The latter read the request, pass it to
"processHTTPRequest" of TTCPHttpDaemon and send the response.
}

uses
  Classes, blcksock, Synsock, Synautil, SysUtils;

type

  THTTPHandler = procedure( const filePath, fileName : string;
                            _GET, _POST, _SERVER : TStringList;
                            var ContentType, ContentText : string;
                            var ContentStream : TMemoryStream;
                            var httpResult : Integer) of object;

  { TTCPHttpDaemon }

  TTCPHttpDaemon = class(TThread)
  private
    _HTTPHandler : THTTPHandler;

    // server status
    _active : boolean;
    _newPort : Word;

    // socket
    Sock:TTCPBlockSocket;

    // pre-processes an HTTP request
    function processHTTPRequest(const Request, URI, InputData: string;
      currSock: TTCPBlockSocket; Headers: TStringList; OutputData: TMemoryStream
    ): Integer;
  protected
    procedure Execute; override;
  public
    property Active : boolean read _active write _active;
    Constructor Create(port : Word; HTTPHandler : THTTPHandler);
    Destructor Destroy; override;
  end;

  { TTCPHttpThrd }

  TTCPHttpThrd = class(TThread)
  private
    Sock:TTCPBlockSocket;
    _daemon : TTCPHttpDaemon;
  public
    Headers: TStringList;
    InputData, OutputData: TMemoryStream;
    Constructor Create (hsock:tSocket; daemon : TTCPHttpDaemon);
    Destructor Destroy; override;
    procedure Execute; override;
  end;

implementation

uses
  uHTTP_utils;

{ TTCPHttpDaemon }

function TTCPHttpDaemon.processHTTPRequest(const Request, URI, InputData: string;
  currSock: TTCPBlockSocket; Headers: TStringList; OutputData: TMemoryStream
  ): Integer;
var
  document, filePath, fileName, hostName : string;
  paramPos, x : Integer;
  ContentText, ContentType : string;
  ContentStream : TMemoryStream;
  ls, queryParam, postParam, serverInfo : TStringList;
begin
  document := URI;
  paramPos := Pos('?', document);

  // list containing GET parameters
  queryParam := TStringList.Create;

  // list containing POST parameters
  postParam := TStringList.Create;

  // list containing SERVER parameters
  serverInfo := TStringList.Create;
  try
    if paramPos > 0 then
    begin
      // GET: separating parameters using '&' and keys and values using '='
      queryParam.Delimiter := '&';
      queryParam.DelimitedText := Copy(document, paramPos+1, Length(document));
      for x := 0 to queryParam.Count-1 do
        if Pos('=', queryParam[x]) = 0 then
          queryParam[x] := queryParam[x] + '=';
      queryParam.NameValueSeparator := '=';

      // POST: separating parameters using '&' and keys and values using '='
      postParam.Delimiter := '&';
      postParam.DelimitedText := InputData;
      for x := 0 to postParam.Count-1 do
        postParam[x] := paramsDecode(postParam[x]);
      postParam.NameValueSeparator := '=';

      // "document" shall contain the file path
      document := Copy(document, 0, paramPos-1);
    end;

    // add IP and host information to the SERVER information
    serverInfo.Add('REMOTE_ADDR=' + currSock.GetRemoteSinIP);
    hostName := currSock.ResolveIPToName(currSock.GetRemoteSinIP);
    if hostName = '' then
      hostName := currSock.GetRemoteSinIP;
    serverInfo.Add('REMOTE_HOST=' + hostName);
    serverInfo.NameValueSeparator := '=';

    // initialize response data
    ContentText := '';
    ContentType := 'text/plain';
    ContentStream := nil;

    result := 200;

    // extract file path and file name
    filePath := SysUtils.ExtractFilePath(document);
    fileName := SysUtils.ExtractFileName(document);
    try

      // call the HTTP handler
      _HTTPHandler( filePath, fileName, queryParam, postParam, serverInfo,
                    ContentType, ContentText, ContentStream, result);
    finally

      // assemble the response headers and response stream
      Headers.Add('Content-type: ' + ContentType);
      if Assigned(ContentStream) then
      begin
        ContentStream.Position := 0;
        OutputData.CopyFrom(ContentStream, ContentStream.Size);
        ContentStream.Free;
      end
      else
      begin
        ls := TStringList.Create;
        try
          ls.Text := ContentText;
          ls.SaveToStream(OutputData);
        finally
          ls.Free;
        end;
      end;
    end;
  finally
    serverInfo.Free;
    queryParam.Free;
    postParam.Free;
  end;
end;

constructor TTCPHttpDaemon.Create(port: Word; HTTPHandler: THTTPHandler);
begin
  sock:=TTCPBlockSocket.create;
  _newPort := port;
  _active := false;
  _HTTPHandler := HTTPHandler;
  FreeOnTerminate:=true;
  inherited create(false);
end;

destructor TTCPHttpDaemon.Destroy;
begin
  Sock.free;
  inherited Destroy;
end;

procedure TTCPHttpDaemon.Execute;
var
  ClientSock:TSocket;
begin
  with sock do
  begin
    CreateSocket;
    setLinger(true,10000);
    EnableReuse(true);
    bind('0.0.0.0',IntToStr(_newPort));
    listen;


    repeat
      if terminated then
        break;

      try
        if canread(1000) then
        begin
          ClientSock:=accept;
          if _active and (lastError=0) then
          begin
            TTCPHttpThrd.create(ClientSock, self);
          end;
        end;
      except on e: exception do
        Writeln(e.Message);
      end;
    until false;
  end;
end;

{ TTCPHttpThrd }

constructor TTCPHttpThrd.Create(hsock: tSocket; daemon: TTCPHttpDaemon);
begin
  sock:=TTCPBlockSocket.create;
  Headers := TStringList.Create;
  InputData := TMemoryStream.Create;
  OutputData := TMemoryStream.Create;
  Sock.socket:=HSock;
  _daemon:=daemon;
  FreeOnTerminate:=true;
  Priority:=tpNormal;
  inherited create(false);
end;

Destructor TTCPHttpThrd.Destroy;
begin
  Sock.free;
  Headers.Free;
  InputData.Free;
  OutputData.Free;
  inherited Destroy;
end;

procedure TTCPHttpThrd.Execute;
var
  timeout: integer;
  s, inputDataString: string;
  method, uri, protocol: string;
  size: integer;
  x, n: integer;
  resultcode: integer;
  close: boolean;
begin
  timeout := 120000;
  repeat
    // read one line of the request
    s := sock.RecvString(timeout);
    if sock.lasterror <> 0 then
      Exit;
    if s = '' then
      Exit;

    // parse the first line to obtain HTTP method, URI and protocol
    method := fetch(s, ' ');
    if (s = '') or (method = '') then
      Exit;
    uri := fetch(s, ' ');
    if uri = '' then
      Exit;
    protocol := fetch(s, ' ');

    // initialize headers
    headers.Clear;
    size := -1;
    close := false;
    // read lines for headers
    if protocol <> '' then
    begin
      if pos('HTTP/', protocol) <> 1 then
        Exit;
      if pos('HTTP/1.1', protocol) <> 1 then
        close := true;

      repeat
        // read next line
        s := sock.RecvString(Timeout);
        if sock.lasterror <> 0 then
          Exit;

        // add headers
        if s <> '' then
          Headers.add(s);

        // parse important headers for connection
        if Pos('CONTENT-LENGTH:', Uppercase(s)) = 1 then
          Size := StrToIntDef(SeparateRight(s, ' '), -1);
        if Pos('CONNECTION: CLOSE', Uppercase(s)) = 1 then
          close := true;
      until s = '';
    end;

    // read the content of the request
    InputData.Clear;
    inputDataString := '';
    if size >= 0 then
    begin
      InputData.SetSize(Size);
      x := Sock.RecvBufferEx(InputData.Memory, Size, Timeout);
      InputData.SetSize(x);
      if sock.lasterror <> 0 then
        Exit;

      // we usually await only POST data as content so transform them to a
      // string
      if size > 0 then
      begin
        SetLength(inputDataString, InputData.Size);
        inputData.Read(inputDataString[1], inputData.Size);
      end;
    end;

    // clear output data (headers are used for output as we do not need input
    // headers here)
    OutputData.Clear;
    Headers.Clear;
    try

      // process the request
      ResultCode := _daemon.ProcessHttpRequest(method, uri, inputDataString, Sock, headers, OutputData);
    except on e: exception do
      Writeln(e.Message);
    end;

    // send protocol header
    sock.SendString(protocol + ' ' + IntTostr(ResultCode) + CRLF);

    // send more headers
    if protocol <> '' then
    begin
      headers.Add('Content-length: ' + IntTostr(OutputData.Size));
      if close then
        headers.Add('Connection: close');
      headers.Add('Date: ' + Rfc822DateTime(now));
      headers.Add('Server: optimizerJob_Server');
      headers.Add('');
      for n := 0 to headers.count - 1 do
        sock.sendstring(headers[n] + CRLF);
    end;
    if sock.lasterror <> 0 then
      Exit;

    // send content
    Sock.SendBuffer(OutputData.Memory, OutputData.Size);
    if close then
      Break;
  until Sock.LastError <> 0;
end;

end.

