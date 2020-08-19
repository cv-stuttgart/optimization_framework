unit uTaskManagerTable;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

{
This unit contains a class which aggregates task managers according to different
task identification data. It provides the ability to handle different
optimization tasks at the same time, each marked by different task identification
data and handled by different task manager instances.
}

uses
  Classes, SysUtils, SyncObjs, SQLDB, PQConnection,

  uDatabase, uClientBase, uTaskManager;

type

  // information for identifying the right task manager
  TTaskManagerIdentifier = record
    userName, taskName : string;
  end;
  TTaskManagerIdentifierList = array of TTaskManagerIdentifier;


  TTaskManagerTableSettings = record
    taskManagerSettings : TTaskManagerSettings;

    // is used to determine after which amount of time a task manager is not
    // needed anymore and can thus be deleted
    maxInactivity : TDateTime;
  end;

  { TTaskManagerTable }

  TTaskManagerTable = class
  protected
    _settings : TTaskManagerTableSettings;
    _lock : TCriticalSection;
    _dbConnection : TDBConnection;

    // a list of identifiers and a list of task managers
    // entries with the same index correspond to each other
    _identifierList : TTaskManagerIdentifierList;
    _taskManagerList : TTaskManagerList;

    // deletes task managers which have not shown activity for a long time
    procedure clearInactiveTaskManagers;
  public
    constructor Create(dbConnection : TDBConnection;
      settings : TTaskManagerTableSettings);
    destructor Destroy; override;

    // gets the task manager which is identified by "identifier"
    function getTaskManager(const identifier : TTaskManagerIdentifier) : TTaskManager;

    // gets a list of identifiers (useful for the HTML status page)
    function getIdentifierList() : TTaskManagerIdentifierList;

    // gets the task manager list
    function getTaskManagerList() : TTaskManagerList;

    // retrieves a client identifier of the client assigned to the task given by
    // "taskID"
    procedure updateClientInfoByTaskID(const taskID: string;
      var clientInfo: TClientInfo);

    // returns a HTML status page
    function getState(const currentJM: TTaskManager): string;
  end;

function createIdentifier(const userName, taskName : string) : TTaskManagerIdentifier;
operator = (z1, z2 : TTaskManagerIdentifier) b : boolean;

implementation

function createIdentifier(const userName, taskName: string
  ): TTaskManagerIdentifier;
begin
  result.userName := userName;
  result.taskName  := taskName;
end;

operator=(z1, z2: TTaskManagerIdentifier)b: boolean;
begin
  result := (z1.taskName = z2.taskName) and (z1.userName = z2.userName);
end;

{ TTaskManagerList }

procedure TTaskManagerTable.clearInactiveTaskManagers;
var
  x, y : integer;
begin
  //Writeln('clearInactiveTaskmanagers (1)');
  _lock.Enter;
  try
    //Writeln('clearInactiveTaskmanagers (2)');
    for x := 0 to high(_taskManagerList) do
    begin

      // no activity for a long time
      if Now() - _taskManagerList[x].getLastActivity() > _settings.maxInactivity then
      begin
        // free the instance
        _taskManagerList[x].Free;

        // delete the corresponding list entries
        for y := x to high(_taskManagerList)-1 do
        begin
          _identifierList[y] := _identifierList[y+1];
          _taskManagerList[y] := _taskManagerList[y+1];
        end;
        SetLength(_taskManagerList, Length(_taskManagerList)-1);
        SetLength(_identifierList, Length(_identifierList)-1);
      end;
    end;
    //Writeln('clearInactiveTaskmanagers (3)');
  finally
    _lock.Leave;
  end;
  //Writeln('clearInactiveTaskmanagers (4)');
end;

constructor TTaskManagerTable.Create(dbConnection: TDBConnection;
  settings: TTaskManagerTableSettings);
begin
  _dbConnection := dbConnection;
  _settings     := settings;
  _lock         := TCriticalSection.Create;

  SetLength(_identifierList, 0);
  SetLength(_taskManagerList, 0);
end;

destructor TTaskManagerTable.Destroy;
var
  x : integer;
begin
  for x := 0 to high(_taskManagerList) do
    _taskManagerList[x].Free;
  SetLength(_taskManagerList, 0);
  SetLength(_identifierList, 0);

  _lock.Free;
  inherited Destroy;
end;

function TTaskManagerTable.getTaskManager(const identifier: TTaskManagerIdentifier
  ): TTaskManager;
var
  x : integer;
begin
  //Writeln('getTaskManager (1)');
  clearInactiveTaskManagers;

  //Writeln('getTaskManager (2)');
  _lock.Enter;
  try
    //Writeln('getTaskManager (3)');
    result := nil;

    // look for an appropriate task manager
    for x := 0 to high(_taskManagerList) do
    begin
      if _identifierList[x] = identifier then
      begin
        result := _taskManagerList[x];
        break;
      end;
    end;
    // Writeln('getTaskManager (4)');

    // if not found yet
    if result = nil then
    begin
      if (identifier.taskName <> '') or (identifier.userName <> '') then
      begin

        // create a new task manager
        x := Length(_taskManagerList);
        SetLength(_taskManagerList, x+1);
        SetLength(_identifierList, x+1);

        _identifierList[x] := identifier;
        //Writeln('getTaskManager (4a)');
        _taskManagerList[x] := TTaskManager.Create( _dbConnection,
                                                  identifier.userName,
                                                  identifier.taskName,
                                                  _settings.taskManagerSettings);
        //Writeln('getTaskManager (4b)');
        result := _taskManagerList[x];
      end;
    end;
    //Writeln('getTaskManager (5)');

    // if found/created update its activity status
    if result <> nil then
      result.updateActivity;

    //Writeln('getTaskManager (6)');
  finally
    _lock.Leave;
  end;

  //Writeln('getTaskManager (7)');
end;

function TTaskManagerTable.getIdentifierList: TTaskManagerIdentifierList;
begin
  _lock.Enter;
  try
    result := Copy(_identifierList, 0, Length(_identifierList));
  finally
    _lock.Leave;
  end;
end;

function TTaskManagerTable.getTaskManagerList: TTaskManagerList;
begin
  result := _taskManagerList;
end;

procedure TTaskManagerTable.updateClientInfoByTaskID( const taskID: string;
  var clientInfo: TClientInfo);
var
  q : TSQLQuery;
  sqlTable : string;
begin
  sqlTable := _settings.taskManagerSettings.sqlTable;

  q := _dbConnection.createQuery('get client info');
  try
    q.SQL.Text := 'LOCK TABLE "'+sqlTable+'";';
    q.ExecSQL;

    q.SQL.Text :=   'SELECT "clientID", "name", "userName" FROM "'+sqlTable+'" '
                  + 'WHERE "ID" = :ID;';
    q.ParamByName('ID').AsInteger := StrToIntDef(taskID, 0);

    q.Open;
    if not q.EOF then
    begin
      clientInfo.userName   := q.FieldByName('userName').AsString;
      clientInfo.taskName    := q.FieldByName('name').AsString;
      clientInfo.instanceID := q.FieldByName('clientID').AsString;
    end;
    q.Close;

    _dbConnection.commitQuery(q);
  finally
    _dbConnection.finishQuery(q);
  end;
end;

function TTaskManagerTable.getState(const currentJM : TTaskManager): string;
var
  jmIdentifierList: TTaskManagerIdentifierList;
  x : integer;
begin
  jmIdentifierList := getIdentifierList;

  result := '<html><head><title>Optimizer Tasks</title>';
  result := result + '<style>* {font-family: Arial;}';
  result := result + ' .title {font-size: 16pt; font-weight: bold; text-align: center;}';
  result := result + ' .details {font-weight: normal; font-size: 14pt;}';
  result := result + '</style>';
  result := result + '</head><body><div align="center">';
  if currentJM <> nil then
  begin
    result := result + '<span class="title">Current Task Group (';
    result := result + currentJM.getUserName() + ' - ' + currentJM.getTaskName();
    result := result + ')</span>';
  end;
  result := result + '<table cellspacing="10">';
  result := result + '<tr><td valign="top">';
  if currentJM <> nil then
  begin
    result := result + '<div class="title">Statistics</div><br>';
    result := result + currentJM.getStatisticsState();
    result := result + '<br><br>';
  end;
  result := result + '<div class="title">Task Groups<br><span class="details">("user"-"taskName")</span></div><br>';
  for x := 0 to high(jmIdentifierList) do
  begin
    result := result + '<a href="showState?userName='+jmIdentifierList[x].userName+'&taskName='+jmIdentifierList[x].taskName+'">';
    result := result + jmIdentifierList[x].userName+' - '+jmIdentifierList[x].taskName;
    result := result + '</a><br>';
  end;
  result := result + '</td><td valign="top">';
  if currentJM <> nil then
    result := result + currentJM.getState();
  result := result + '</td></tr></table></div></body></html>';
end;

end.

