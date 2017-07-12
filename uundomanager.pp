unit UUndoManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MAX_HISTORY_SIZE = 9999;

type
  TUndoRedoGroup = class;

  THistoryIterator = class

  private
    FRecorder: TUndoRedoGroup;
    FPlugged: Boolean;
    FMaxSize: longint;
    FIndex: longint;

    function GetCurrent: THistoryLeaf;
    function GetRecorder: TUndoRedoGroup;
    function GetSize: longint;

    function HasNext: Boolean;
    function HasPrevious: Boolean;
    function Next: THistoryLeaf;
    function Previous: THistoryLeaf;

    procedure UpdateIndex;
    procedure Reset;
    procedure RemoveFirst;

    procedure DoIt;
    procedure Redo;
    procedure Undo;

  public
    constructor Create;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean;
    procedure OpenGroup;
    procedure CloseGroup;


    property Recorder: TUndoRedoGroup read GetRecorder;
  end;


  THistoryLeaf = class
  public
    function Opend: boolean; virtual;
    function IsComposit: boolean; virtual;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean; virtual;
    procedure DoIt; virtual; abstract;
    procedure Redo; virtual; abstract;
    procedure Undo; virtual; abstract;
  end;


  THistoryNode = class(THistoryLeaf)
  private
    FOpend: boolean;
    FHistory: TList;

    function GetHistory: TList;
    function GetCurrent: THistoryLeaf;

  public
    procedure Open;
    procedure Close;
    procedure Reset;
    function OpenGroup:Boolean;
    procedure CloseGroup;

    function Opend: boolean; override;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean; override;

    property History: TList read GetHistory;
    property Curent: THistoryLeaf read GetCurrent


    procedure RemoveFirst;
    procedure RmoveLast;
    procedure RmoveLast(ACount: longint);

  end;

  TUndoRedoGroup = class(THistoryNode)
  public
    procedure DoIt; override;
    procedure Redo; override;
    procedure Undo; override;
  end;


  TUndoRedoRecord = class(THistoryLeaf)
  private
    FRedo: TNotifyEvent;
    FUndo: TNotifyEvent;
  public
    procedure DoIt; override;
    procedure Redo; override;
    procedure Undo; override;

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

procedure THistoryIterator.DoIt;
begin
  Redo;
end;

procedure THistoryIterator.Redo;
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


procedure THistoryIterator.Undo;
var
  savedPlugged: Boolean;
begin
  savedPlugged := FPlugged;
  try
    FPlugged := false;
    if GetCurrent <> nil then
    begin
      GetCurrent.Undo;
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
  Result := (GetRecorder.History.Count - GetIndex) > 1;
end;


function THistoryIterator.HasPrevious: Boolean;
begin
  Result := GetIndex >= 0;
end;


function THistoryIterator.Next: THistoryLeaf;
begin
  if HasNext then
    Result := FIndex = GetIndex + 1;
  else
    Result := nil;
end;


function THistoryIterator.Previous: THistoryLeaf;
begin
  if HasPrevious then
    Result := FIndex = GetIndex - 1;
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
  GetRecorder.RmoveLast(GetSize - FIndex);
  Result := GetRecorder.AddItem(ANHistoryItem);
  UpdateIndex;
end;


function THistoryIterator.GetCurrent: THistoryLeaf;
begin
  if GetIndex < GetSize and FIndex >= 0 then
    Result := GetRecorder.History[GetIndex]
  else
    Result := nil;
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
  Result := GetRecorder.Size;
end;


procedure THistoryIterator.Reset;
begin
  FIndex := -1;
  FPlugged := true;
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
  FHistory.Clear;
  FOpend := True;
end;


function THistoryNode.OpenGroup:Boolean;
begin
  Result := AddItem(THistoryNode.Create);
end;

procedure THistoryNode.CloseGroup;
begin
  if GetCurrent <> nil then
     if GetCurrent.IsComposit then
         if GetCurrent.Opend then
         begin
            GetCurrent.CloseGroup
            exit;
         end;
  CLose
end;


function THistoryNode.GetHistory: TList;
begin
  if not Assigned(FHistory) then
    FHistory := TList.Create;
  Result := FHistory;
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


procedure TUndoRedoGroup.DoIt;
begin
  Redo;
end;

procedure TUndoRedoGroup.Redo;
var
  each: TObject;
  node: TUndoRedoGroup;
begin
  for each in GetHistory do
  begin
    node := TUndoRedoGroup(each);
    node.Redo;
  end;
end;


procedure TUndoRedoGroup.Undo;
var
  each: TObject;
  node: TUndoRedoGroup;
begin
  for i = GetHistory.Count - 1 downto 0 do
  begin
    each = GetHistory[i]
    node := TUndoRedoGroup(each);
    node.Undo;
  end;
end;


procedure TUndoRedoRecord.DoIt;
begin
  Redo;
end;

procedure TUndoRedoRecord.Redo;
begin
  if Assigned(FRedo) then
     FRedo(self);
end;

procedure TUndoRedoRecord.Undo;
begin
  if Assigned(FUndo) then
     FUndo(Self);
end;


end.
