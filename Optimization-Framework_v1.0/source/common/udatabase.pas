unit uDatabase;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

{
This unit contains a class which represents a connection do a SQL database.
"createQuery" creates a SQLQuery object with exclusive access to the database
connection until "finishQuery" is called.
Thus, no overlapping accesses on the database connection are possible
}

uses
  Classes, SysUtils, DOM, XMLRead, SQLDB, PQConnection, SyncObjs;

type
  TDBSettings = record
    host, port : string;
    user, password : string;
    dbname : string;
  end;

  { TDBConnection }

  TDBConnection = class
  protected
    _settings : TDBSettings;
    _connection : TPQConnection;
    _lock : TCriticalSection;

    procedure Open;
  public
    constructor Create(settings : TDBSettings);
    destructor Destroy; override;

    // tries reconnecting to the database
    procedure Reconnect(const msg: string='');

    // acquires the lock and creates a TSQLTransaction object
    function createTransaction() : TSQLTransaction;

    // frees the transaction and releases the lock
    procedure finishTransaction(t: TSQLTransaction);

    // acquires the lock and creates a TSQLQuery object
    function createQuery(const name : string = '') : TSQLQuery;

    // commits the query
    procedure commitQuery(q: TSQLQuery);

    // frees the query and releases the lock
    procedure finishQuery(q: TSQLQuery);
  end;

function readDBSettings(const configFile : string) : TDBSettings;

implementation

function readDBSettings(const configFile : string) : TDBSettings;
var
  Doc:      TXMLDocument;
begin
  if fileexists(configFile) then
  begin
    ReadXMLFile(Doc, configFile);
    try
      try
        result.host     := Doc.DocumentElement.FindNode('DB').FindNode('host').FirstChild.NodeValue;
        result.port     := Doc.DocumentElement.FindNode('DB').FindNode('port').FirstChild.NodeValue;
        result.user     := Doc.DocumentElement.FindNode('DB').FindNode('user').FirstChild.NodeValue;
        result.password := Doc.DocumentElement.FindNode('DB').FindNode('password').FirstChild.NodeValue;
        result.dbname   := Doc.DocumentElement.FindNode('DB').FindNode('dbname').FirstChild.NodeValue;
      except
        Writeln('Error: Invalid config file!');
        halt;
      end;
    finally
      Doc.Free;
    end;
  end
  else
  begin
    Writeln('Error: Config file "' + configFile + '" not found!');
    halt;
  end;
end;

{ TDBConnection }

constructor TDBConnection.Create(settings: TDBSettings);
begin
  _settings := settings;
  _connection := nil;

  _lock                 := TCriticalSection.Create;

  Open;
end;

destructor TDBConnection.Destroy;
begin
  if _connection <> nil then
  begin
    _connection.Close;
    _connection.Free;
  end;
  _lock.Free;
  inherited Destroy;
end;

procedure TDBConnection.Open;
begin
  _connection := TPQConnection.Create(nil);

  _connection.HostName     := _settings.host;
  _connection.UserName     := _settings.user;
  _connection.Password     := _settings.password;
  _connection.DatabaseName := _settings.dbname;
  _connection.Params.Add('port=' + _settings.port);

  _connection.Open;
end;

procedure TDBConnection.Reconnect(const msg : string = '');
begin
  while true do
  begin
    try
      WriteLn('Database-Error: "' + msg + '"');

      WriteLn('Closing transactions...');
      _connection.CloseTransactions;
      _connection.Close;

      WriteLn('Try reconnecting database ... ');
      _connection.Open;

      break;
    except
      Sleep(100);
    end;
  end;
end;

function TDBConnection.createTransaction: TSQLTransaction;
begin
  _lock.Enter;

  result := TSQLTransaction.Create(nil);
  result.DataBase := _connection;
  result.StartTransaction;
end;

procedure TDBConnection.finishTransaction(t: TSQLTransaction);
begin
  try
    t.Free;
  finally
    _lock.Leave;
  end;
end;

function TDBConnection.createQuery(const name: string): TSQLQuery;
var
  t : TSQLTransaction;
begin
  _lock.Enter;

  t := TSQLTransaction.Create(nil);
  t.DataBase := _connection;
  t.StartTransaction;

  result := TSQLQuery.Create(nil);
  result.Transaction := t;
end;

procedure TDBConnection.commitQuery(q: TSQLQuery);
var
  t : TSQLTransaction;
begin
  t := q.Transaction as TSQLTransaction;

  try
    t.Commit;
  except on e: exception do
    Writeln(e.Message);
  end;
end;

procedure TDBConnection.finishQuery(q: TSQLQuery);
var
  t : TSQLTransaction;
begin
  try
    t := q.Transaction as TSQLTransaction;

    q.Free;

    t.Free;
  finally
    _lock.Leave;
  end;
end;

end.

