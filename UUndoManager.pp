unit UUndoManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

const
  MAX_HISTORY_SIZE = 9999;

type
  TUndoRedoGroup = class;
  TUndoRedoRecord = class;
  THistoryLeaf = class;


  {from: Pharo smalltak}
  THistoryIterator = class
  private
    FRecorder: TUndoRedoGroup;
    FPlugged: boolean;
    //FMaxSize: longint;
    FIndex: longint;

    function GetCurrent: THistoryLeaf;
    function GetRecorder: TUndoRedoGroup;
    function GetSize: longint;
    function GetCanUndo: boolean;
    function GetCanRedo: boolean;
    function GetItem(Index: integer): THistoryLeaf;
    procedure SetItem(Index: integer; AObject: THistoryLeaf);

    function Next: THistoryLeaf;
    function Previous: THistoryLeaf;

    procedure UpdateIndex;
    procedure RemoveFirst;

  public
    constructor Create;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean;
    procedure OpenGroup;
    procedure CloseGroup;
    procedure Reset;

    function Last: THistoryLeaf;
    function HasNext: boolean;
    function HasPrevious: boolean;
    function DoIt: boolean;
    function Redo: boolean;
    function Undo: boolean;
    function UndoableCount: longint;
    function RedoableCount: longint;
    function DoAndAddRecord(ARecord: TUndoRedoRecord): boolean;

    property Recorder: TUndoRedoGroup read GetRecorder;
    property Current: THistoryLeaf read GetCurrent;
    property Size: longint read GetSize;
    property Index: longint read FIndex;
    property Items[ANIndex: integer]: THistoryLeaf read GetItem write SetItem; default;
    property CanUndo: boolean read GetCanUndo;
    property CanRedo: boolean read GetCanRedo;
  end;


  THistoryLeaf = class
  public
    function Opend: boolean; virtual;
    function IsComposite: boolean; virtual;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean; virtual;
    function DoIt: boolean; virtual; abstract;
    function Redo: boolean; virtual; abstract;
    function Undo: boolean; virtual; abstract;
  end;


  THistoryNode = class(THistoryLeaf)
  private
    FOpend: boolean;
    FHistory: TObjectList;

    function GetHistory: TObjectList;
    function GetSize: longint;
    function GetCurrent: THistoryLeaf;
    function GetItem(Index: integer): THistoryLeaf;
    procedure SetItem(Index: integer; AObject: THistoryLeaf);
  public
    constructor Create;
    procedure Open;
    procedure Close;
    procedure Reset;
    function OpenGroup: boolean; virtual;
    procedure CloseGroup;

    function Opend: boolean; override;
    function Closed: boolean;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean; override;
    function IsComposite: boolean; override;
    function IsEmpty: boolean;

    property History: TObjectList read GetHistory;
    property Current: THistoryLeaf read GetCurrent;
    property Size: longint read GetSize;
    property Items[Index: integer]: THistoryLeaf read GetItem write SetItem; default;

    procedure RemoveFirst;
    procedure RmoveLast;
    procedure RmoveLast(ACount: longint);

  end;

  TUndoRedoGroup = class(THistoryNode)
  public
    function OpenGroup: boolean; override;
    function DoIt: boolean; override;
    function Redo: boolean; override;
    function Undo: boolean; override;
  end;


  TUndoRedoRecord = class(THistoryLeaf)
  private
    FRedo: TNotifyEvent;
    FUndo: TNotifyEvent;
  public
    function DoIt: boolean; override;
    function Redo: boolean; override;
    function Undo: boolean; override;

    property OnRedo: TNotifyEvent read FRedo write FRedo;
    property OnUndo: TNotifyEvent read FUndo write FUndo;
  end;



implementation


constructor THistoryIterator.Create;
begin
  FIndex := -1;
  FPlugged := True;
end;


procedure THistoryIterator.Reset;
begin
  Recorder.Reset;
  FIndex := -1;
end;


function THistoryIterator.DoIt: boolean;
begin
  Result := Redo;
end;


function THistoryIterator.Redo: boolean;
var
  savedPlugged: boolean;
begin
  savedPlugged := FPlugged;
  Result := False;
  try
    FPlugged := False;
    if HasNext then
    begin
      Next.Redo;
      Result := True;
    end;
  finally
    FPlugged := savedPlugged;
  end;
end;


function THistoryIterator.Undo: boolean;
var
  savedPlugged: boolean;
  cmd: THistoryLeaf;
begin
  savedPlugged := FPlugged;
  try
    FPlugged := False;
    cmd := Current;
    if cmd <> nil then
      cmd.Undo;
    Previous;
  finally
    FPlugged := savedPlugged;
  end;
  Result := True;
end;


function THistoryIterator.GetRecorder: TUndoRedoGroup;
begin
  if not Assigned(FRecorder) then
    FRecorder := TUndoRedoGroup.Create;
  Result := FRecorder;
end;


function THistoryIterator.Last: THistoryLeaf;
begin
  Result := THistoryLeaf(Recorder.History.Last);
end;

function THistoryIterator.HasNext: boolean;
begin
  Result := (Recorder.History.Count - Index) > 1;
end;


function THistoryIterator.HasPrevious: boolean;
begin
  Result := Index >= 0;
end;


function THistoryIterator.Next: THistoryLeaf;
begin
  if HasNext then
  begin
    FIndex := FIndex + 1;
    Result := GetCurrent;
  end
  else
    Result := nil;
end;


function THistoryIterator.Previous: THistoryLeaf;
begin
  if HasPrevious then
  begin
    FIndex := FIndex - 1;
    Result := GetCurrent;
  end
  else
    Result := nil;
end;


function THistoryIterator.AddItem(ANHistoryItem: THistoryLeaf): boolean;
begin
  if not FPlugged then
  begin
    Result := False;
    exit;
  end;
  GetRecorder.RmoveLast(Size - Index - 1);
  Result := GetRecorder.AddItem(ANHistoryItem);
  UpdateIndex;
end;


function THistoryIterator.GetCurrent: THistoryLeaf;
begin
  if (FIndex < GetSize) and (FIndex >= 0) then
    Result := THistoryLeaf(GetRecorder.History[FIndex])
  else
    Result := nil;
end;


function THistoryIterator.UndoableCount: longint;
begin
  if GetSize > 0 then
    Result := FIndex + 1
  else
    Result := 0;
end;


function THistoryIterator.RedoableCount: longint;
begin
  Result := GetSize - FIndex - 1;
end;


function THistoryIterator.DoAndAddRecord(ARecord: TUndoRedoRecord): boolean;
begin
  Result := ARecord.DoIt;
  AddItem(ARecord);
end;


function THistoryIterator.GetItem(Index: integer): THistoryLeaf;
begin
  Result := Recorder.History.Items[Index] as THistoryLeaf;
end;


procedure THistoryIterator.SetItem(Index: integer; AObject: THistoryLeaf);
begin
  Recorder.History.Items[Index] := AObject;
end;


procedure THistoryIterator.UpdateIndex;
begin
  if GetSize > MAX_HISTORY_SIZE then
    RemoveFirst;
  FIndex := GetSize - 1;
end;


procedure THistoryIterator.RemoveFirst;
begin
  GetRecorder.RemoveFirst;
end;


function THistoryIterator.GetSize: longint;
begin
  Result := Recorder.History.Count;
end;


function THistoryIterator.GetCanUndo: boolean;
begin
  Result := UndoableCount > 0;
end;


function THistoryIterator.GetCanRedo: boolean;
begin
  Result := RedoableCount > 0;
end;


procedure THistoryIterator.OpenGroup;
begin
  GetRecorder.OpenGroup;
  UpdateIndex;
end;


procedure THistoryIterator.CloseGroup;
begin
  GetRecorder.CloseGroup;
end;


function THistoryLeaf.Opend: boolean;
begin
  Result := False;
end;


function THistoryLeaf.IsComposite: boolean;
begin
  Result := False;
end;


function THistoryLeaf.AddItem(ANHistoryItem: THistoryLeaf): boolean;
begin
  Result := False;
end;


constructor THistoryNode.Create;
begin
  FOpend := True;
end;


function THistoryNode.Opend: boolean;
begin
  Result := FOpend;
end;


function THistoryNode.Closed: boolean;
begin
  Result := not Opend;
end;


function THistoryNode.IsComposite: boolean;
begin
  Result := True;
end;


function THistoryNode.IsEmpty: boolean;
begin
  Result := GetHistory.Count = 0;
end;


procedure THistoryNode.Open;
begin
  FOpend := True;
end;


procedure THistoryNode.Close;
begin
  FOpend := False;
end;


procedure THistoryNode.Reset;
begin
  if Assigned(FHistory) then
    FHistory.Clear;
  FOpend := True;
end;


function THistoryNode.OpenGroup: boolean;
begin
  Result := AddItem(THistoryNode.Create);
end;


procedure THistoryNode.CloseGroup;
begin
  if GetCurrent <> nil then
    if GetCurrent.IsComposite then
      if GetCurrent.Opend then
      begin
        THistoryNode(GetCurrent).CloseGroup;
        exit;
      end;
  Close;
end;


function THistoryNode.GetHistory: TObjectList;
begin
  if not Assigned(FHistory) then
    FHistory := TObjectList.Create;
  Result := FHistory;
end;


function THistoryNode.GetSize: longint;
begin
  if Assigned(FHistory) then
    Result := FHistory.Count
  else
    Result := 0;
end;


function THistoryNode.GetCurrent: THistoryLeaf;
begin
  if GetHistory.Count = 0 then
    Result := nil
  else
    Result := THistoryLeaf(GetHistory.Last);
end;


function THistoryNode.GetItem(Index: integer): THistoryLeaf;
begin
  Result := History.Items[Index] as THistoryLeaf;
end;


procedure THistoryNode.SetItem(Index: integer; AObject: THistoryLeaf);
begin
  History.Items[Index] := AObject;
end;


function THistoryNode.AddItem(ANHistoryItem: THistoryLeaf): boolean;
begin
  if GetCurrent <> nil then
    if GetCurrent.AddItem(ANHistoryItem) then
    begin
      Result := True;
      exit;
    end;
  if Opend then
  begin
    GetHistory.Add(ANHistoryItem);
    Result := True;
    exit;
  end;
  Result := False;
end;


procedure THistoryNode.RemoveFirst;
begin
  History.Remove(History.First);
end;


procedure THistoryNode.RmoveLast;
begin
  History.Remove(History.Last);
end;


procedure THistoryNode.RmoveLast(ACount: longint);
var
  i: longint;
begin
  if ACount > 0 then
  begin
    for i := 1 to ACount do
    begin
      History.Remove(History.Last);
    end;
  end;
end;


function TUndoRedoGroup.OpenGroup: boolean;
begin
  Result := AddItem(TUndoRedoGroup.Create);
end;



function TUndoRedoGroup.DoIt: boolean;
begin
  Result := Redo;
end;


function TUndoRedoGroup.Redo: boolean;
var
  each: Pointer;
  node: TUndoRedoGroup;
begin
  for each in GetHistory do
  begin
    node := TUndoRedoGroup(each);
    node.Redo;
  end;
  Result := True;
end;


function TUndoRedoGroup.Undo: boolean;
var
  each: TObject;
  node: TUndoRedoGroup;
  i: longint;
begin
  for i := GetHistory.Count - 1 downto 0 do
  begin
    each := GetHistory[i];
    node := TUndoRedoGroup(each);
    node.Undo;
  end;
  Result := True;
end;


function TUndoRedoRecord.DoIt: boolean;
begin
  Result := Redo;
end;


function TUndoRedoRecord.Redo: boolean;
begin
  Result := False;
  if Assigned(FRedo) then
    FRedo(self);
end;


function TUndoRedoRecord.Undo: boolean;
begin
  Result := False;
  if Assigned(FUndo) then
    FUndo(Self);
end;


end.
