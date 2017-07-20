unit UUndoManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs;

const
  MAX_HISTORY_SIZE = 9999;

type
  TUndoRedoGroup = class;
  THistoryLeaf = class;

  {from: Pharo smalltak}
  THistoryIterator = class
  private
    FRecorder: TUndoRedoGroup;
    FPlugged: Boolean;
    //FMaxSize: longint;
    FIndex: longint;

    function GetCurrent: THistoryLeaf;
    function GetRecorder: TUndoRedoGroup;
    function GetSize: longint;
    function GetCanUndo: Boolean;
    function GetCanRedo: Boolean;

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

    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function DoIt: Boolean;
    function Redo: Boolean;
    function Undo: Boolean;
    function UndoableCount: longint;
    function RedoableCount: longint;

    property Recorder: TUndoRedoGroup read GetRecorder;
    property Current: THistoryLeaf read GetCurrent;
    property Size: longint read GetSize;
    property Index: longint read FIndex;
    property CanUndo: Boolean read GetCanUndo;
    property CanRedo: Boolean read GetCanRedo;
  end;


  THistoryLeaf = class
  public
    function Opend: boolean; virtual;
    function IsComposit: boolean; virtual;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean; virtual;
    function DoIt: Boolean; virtual; abstract;
    function Redo: Boolean ; virtual; abstract;
    function Undo: Boolean ; virtual; abstract;
  end;


  THistoryNode = class(THistoryLeaf)
  private
    FOpend: boolean;
    FHistory: TObjectList;

    function GetHistory: TObjectList;
    function GetSize: longint;
    function GetCurrent: THistoryLeaf;

  public
    constructor Create;
    procedure Open;
    procedure Close;
    procedure Reset;
    function OpenGroup:Boolean;
    procedure CloseGroup;

    function Opend: boolean; override;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean; override;

    property History: TObjectList read GetHistory;
    property Current: THistoryLeaf read GetCurrent;
    property Size: longint read GetSize;


    procedure RemoveFirst;
    procedure RmoveLast;
    procedure RmoveLast(ACount: longint);

  end;

  TUndoRedoGroup = class(THistoryNode)
  public
    function DoIt: Boolean; override;
    function Redo: Boolean; override;
    function Undo: Boolean; override;
  end;


  TUndoRedoRecord = class(THistoryLeaf)
  private
    FRedo: TNotifyEvent;
    FUndo: TNotifyEvent;
  public
    function DoIt: Boolean; override;
    function Redo: Boolean; override;
    function Undo: Boolean; override;

    property OnDoIt: TNotifyEvent read FRedo write FRedo;
    property OnRedo: TNotifyEvent read FRedo write FRedo;
    property OnUndo: TNotifyEvent read FUndo write FUndo;
  end;



implementation

constructor THistoryIterator.Create;
begin
  FIndex := -1;
  FPlugged := true;
end;


procedure THistoryIterator.Reset;
begin
  Recorder.Reset;
  FIndex := -1;
end;


function THistoryIterator.DoIt: Boolean;
begin
  Result := Redo;
end;

function THistoryIterator.Redo: Boolean;
var
  savedPlugged: Boolean;
begin
  savedPlugged := FPlugged;
  Result := False;
  try
    FPlugged := false;
    if HasNext then
    begin
      Next.Redo;
      Result := true;
    end;
  finally
    FPlugged := savedPlugged;
  end;
end;


function THistoryIterator.Undo: Boolean;
var
  savedPlugged: Boolean;
begin
  savedPlugged := FPlugged;
  try
    FPlugged := false;
    if Current <> nil then
    begin
      Current.Undo;
      Previous;
    end;
  finally
    FPlugged := savedPlugged;
  end;
  Result := true;
end;


function THistoryIterator.GetRecorder: TUndoRedoGroup;
begin
  if not Assigned(FRecorder) then
    FRecorder := TUndoRedoGroup.Create;
  Result := FRecorder;
end;


function THistoryIterator.HasNext: Boolean;
begin
  Result := (Recorder.History.Count - Index) > 1;
end;


function THistoryIterator.HasPrevious: Boolean;
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
    Result := false;
    exit;
  end;
  Recorder.RmoveLast(Size - Index);
  Result := Recorder.AddItem(ANHistoryItem);
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


procedure THistoryIterator.UpdateIndex;
begin
  if GetSize > MAX_HISTORY_SIZE then
    RemoveFirst;
  FIndex := GetSize;
end;


procedure THistoryIterator.RemoveFirst;
begin
  GetRecorder.RemoveFirst;
end;


function THistoryIterator.GetSize: longint;
begin
  Result := Recorder.History.Count;
end;


function THistoryIterator.GetCanUndo: Boolean;
begin
  Result := UndoableCount > 0;
end;


function THistoryIterator.GetCanRedo: Boolean;
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


function THistoryLeaf.IsComposit: boolean;
begin
  Result := False;
end;


function THistoryLeaf.AddItem(ANHistoryItem: THistoryLeaf): boolean;
begin
  Result := False;
end;


constructor THistoryNode.Create;
begin
  FOpend := true;
end;


function THistoryNode.Opend: boolean;
begin
  Result := FOpend;
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


function THistoryNode.OpenGroup:Boolean;
begin
  Result := AddItem(TUndoRedoGroup.Create);
end;

procedure THistoryNode.CloseGroup;
begin
  if GetCurrent <> nil then
     if GetCurrent.IsComposit then
         if GetCurrent.Opend then
         begin
            THistoryNode(GetCurrent).CloseGroup;
            exit;
         end;
  CLose
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
  for i := 1 to ACount do
  begin
    History.Remove(History.Last);
  end;
end;


function TUndoRedoGroup.DoIt: Boolean;
begin
  Result := Redo;
end;

function TUndoRedoGroup.Redo: Boolean;
var
  each: Pointer;
  node: TUndoRedoGroup;
begin
  for each in GetHistory do
  begin
    node := TUndoRedoGroup(each);
    node.Redo;
  end;
  Result := true
end;


function TUndoRedoGroup.Undo: Boolean;
var
  each: TObject;
  node: TUndoRedoGroup;
  i: longint;
begin
  for i := GetHistory.Count - 1 downto 0 do
  begin
    each := GetHistory[i];
    node := each as TUndoRedoGroup;
    node.Undo;
  end;
  Result := true;
end;


function TUndoRedoRecord.DoIt: Boolean;
begin
  Result := Redo;
end;

function TUndoRedoRecord.Redo: Boolean;
begin
  Result := false;
  if Assigned(FRedo) then
     FRedo(self);
end;

function TUndoRedoRecord.Undo: Boolean;
begin
  Result := false;
  if Assigned(FUndo) then
     FUndo(Self);
end;


end.
