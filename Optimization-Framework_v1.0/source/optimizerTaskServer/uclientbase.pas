unit uClientBase;

// Copyright by Michael Stoll, 2015 - 2017, University of Stuttgart, Germany
// License: see "license.txt"

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uClientTask;

type

  TClientInfo = record

    // a client is identified by an IP address and an identifier which should be
    // unique per IP address
    IP, instanceID : string;

    // tasks which are to be computed by the client have a name and a username
    // who owns the tasks
    taskName, userName : string;

    // DNS entry for the IP
    host : string;

    // numbers of total and fast threads as provided by the client
    nMaxThreads, nFastThreads : Integer;
  end;
  TClientInfoArray = array of TClientInfo;

  { TClientPerformanceInfo }

  TOptimizationPerformance = record
    optimizationID : string;

    // average run time in order to estimate run times for the tasks
    tAvg : TDateTime;

    // does "tAvg" still contain the initial value?
    tAvg_InitialValue : boolean;

    // when was the last update?
    tLastUpdate : TDateTime;
  end;
  TOptimizationPerformances = array of TOptimizationPerformance;

  TClientPerformanceInfo = record

    // numbers of total and fast threads as either provided by the client or
    // estimated later by the amount of active tasks
    nMaxThreads, nFastThreads : Integer;

    // flag whether the numbers above are (to be) estimated
    threadCountEstimated : boolean;

    // timing statistics in order to estimate run times for the tasks
    tLastTaskActivity : TDateTime;

    // statistics separated by optimization
    optimizations : TOptimizationPerformances;

    // initial value for tAvg
    tAvgInitializationValue : TDateTime;

    // when was the client's last activity?
    tLastPing : TDateTime;

    // further statistics
    nComputations : Integer;
    tTotal : TDateTime;
  end;

  { TClientNodeBase }

  TClientNodeBase = class
  protected
    // performance information
    _perfInfo : TClientPerformanceInfo;
    _staticInfo : TClientInfo;

  public
    constructor Create(const staticInfo: TClientInfo);
    function getTaskTime(const task: TClientTask;
      startTime : TDateTime) : TDateTime; virtual; abstract;
    function getTaskTime(const task: TClientTask;
      startTime: TDateTime;
      out activeThreads, loadFactor: Extended): TDateTime; virtual; abstract;
    function getStaticInfo() : TClientInfo;
    function getPerformanceInfo() : TClientPerformanceInfo;
    procedure setPerformanceInfo(value : TClientPerformanceInfo);
  end;

implementation

{ TClientNodeBase }

constructor TClientNodeBase.Create(const staticInfo : TClientInfo);
begin
  _staticInfo            := staticInfo;

  _perfInfo.nFastThreads := _staticInfo.nFastThreads;
  _perfInfo.nMaxThreads  := _staticInfo.nMaxThreads;
  _perfInfo.threadCountEstimated := (_staticInfo.nMaxThreads = -1);
  if (_staticInfo.nMaxThreads <> -1) and (_staticInfo.nFastThreads = -1) then
    _perfInfo.nFastThreads := _staticInfo.nMaxThreads;

  _perfInfo.tLastTaskActivity := Now();
  _perfInfo.tLastPing        := Now();
  SetLength(_perfInfo.optimizations, 0);
  _perfInfo.tAvgInitializationValue := 0;
  //_perfInfo.tAvg     := 0;
  //_perfInfo.tAvg_InitialValue := true;
  _perfInfo.nComputations := 0;
  _perfInfo.tTotal   := 0;
end;

function TClientNodeBase.getStaticInfo: TClientInfo;
begin
  result := _staticInfo;
end;

function TClientNodeBase.getPerformanceInfo: TClientPerformanceInfo;
begin
  result := _perfInfo;
end;

procedure TClientNodeBase.setPerformanceInfo(value: TClientPerformanceInfo);
begin
  _perfInfo := value;
end;

end.

