unit UUndoManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TUndoRedoGroup = class;

  THistoryIterator = class

  private
    FRecorder: TUndoRedoGroup;
    function GetRecorder: TUndoRedoGroup;

  public
    property Recorder: TUndoRedoGroup read GetRecorder;
  end;


  THistoryLeaf = class
  public
    function Opend: boolean; virtual;
    function IsComposit: boolean; virtual;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean; virtual;
  end;

  THistoryLeafClass = class of THistoryLeaf;

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

    function Opend: boolean; override;
    function AddItem(ANHistoryItem: THistoryLeaf): boolean; override;

  end;

  TUndoRedoRecord = class(THistoryLeaf)

  end;

  TUndoRedoGroup = class(THistoryNode)

  end;



implementation


function THistoryIterator.GetRecorder: TUndoRedoGroup;
begin
  if not Assigned(FRecorder) then
    FRecorder := TUndoRedoGroup.Create;
  Result := FRecorder;
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
  //AddItem((class of Self).Create);
  Result := false;
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

end.
