unit TestHistoryIterator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  UUndoManager;

type

  THistoryIteratorTest = class(TTestCase)
  private
    FUndoManager: THistoryIterator;
  public
    procedure UndoHandler(Sender: TObject);
    procedure RedoHandler(Sender: TObject);


  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCurrentOnStartup;
    procedure TestUndo1;
    procedure TestUndo2;
    procedure TestGrouping;
    procedure TestGroupedUndo2;
  end;

implementation

uses
  fgl, Dialogs;

type
  TIntegerList = specialize TFPGList<integer>;

  { TAddElementCommand }

  TAddElementCommand = class(TUndoRedoRecord)
  private
    FAddNumber: integer;
    FIntegerList: TIntegerList;
  public
    constructor Create(AList: TIntegerList; ANumber: integer);
    function Redo: boolean; override;
    function Undo: boolean; override;
  end;

  TRemoveElementCommand = class(TUndoRedoRecord)
  private
    FLast: integer;
    FIntegerList: TIntegerList;
  public
    constructor Create(AList: TIntegerList);
    function Redo: boolean; override;
    function Undo: boolean; override;
  end;


constructor TAddElementCommand.Create(AList: TIntegerList; ANumber: integer);
begin
  FIntegerList := AList;
  FAddNumber := ANumber;
end;


function TAddElementCommand.Redo: boolean;
begin
  FIntegerList.Add(FAddNumber);
end;


function TAddElementCommand.Undo: boolean;
begin
  FIntegerList.Delete(FIntegerList.Count - 1);
end;


constructor TRemoveElementCommand.Create(AList: TIntegerList);
begin
  FIntegerList := AList;
end;


function TRemoveElementCommand.Redo: boolean;
begin
  FLast := FIntegerList.Last;
  FIntegerList.Delete(FIntegerList.Count - 1);
end;


function TRemoveElementCommand.Undo: boolean;
begin
  FIntegerList.Add(FLast);
end;


procedure THistoryIteratorTest.TestCurrentOnStartup;
begin
  FUndoManager.Reset;
  AssertNull(FUndoManager.Current);
  AssertEquals('UndoableCount must be 0', 0, FUndoManager.UndoableCount);
  AssertEquals('RedoableCount must be 0', 0, FUndoManager.RedoableCount);
  AssertFalse('HasNext must be false', FUndoManager.HasNext);
  AssertFalse('HasPrevious must be false', FUndoManager.HasPrevious);
  AssertFalse('CanUndo must be false', FUndoManager.CanUndo);
  AssertFalse('CanUndo must be false', FUndoManager.CanRedo);
end;


procedure THistoryIteratorTest.TestUndo1;
var
  col: TIntegerList;
  cmd: TUndoRedoRecord;
begin
  try
    col := TIntegerList.Create;
    cmd := TAddElementCommand.Create(col, 1);
    FUndoManager.DoAndAddRecord(cmd);
    AssertEquals('FUndoManager.Size = 1', 1, FUndoManager.Size);
    AssertNotNull('FUndoManager.Recorder[0] is not null', FUndoManager.Recorder[0]);
    AssertNotNull('FUndoManager.Current is not null', FUndoManager.Current);
    FUndoManager.Undo;
    AssertEquals('col.Count = 0', 0, col.Count);
  finally
    FreeAndNil(col);
  end;
end;


procedure THistoryIteratorTest.TestUndo2;
var
  col: TIntegerList;
  cmd: TUndoRedoRecord;
begin
  try
    col := TIntegerList.Create;
    col.Add(1);
    col.Add(2);
    col.Add(3);
    cmd := TRemoveElementCommand.Create(col);
    FUndoManager.DoAndAddRecord(cmd);
    AssertEquals('col.Count = 2', 2, col.Count);
    AssertEquals('col.Last = 2', 2, col.Last);
    FUndoManager.Undo;
    AssertEquals('col.Count = 3', 3, col.Count);
    AssertEquals('col.Last = 3', 3, col.Last);
  finally
    FreeAndNil(col);
  end;
end;


procedure THistoryIteratorTest.TestGrouping;
var
  col: TIntegerList;
  cmd: TUndoRedoRecord;
begin
  try
    col := TIntegerList.Create;

    FUndoManager.OpenGroup;
    AssertEquals('FUndoManager.Size = 1', 1, FUndoManager.Size);
    AssertTrue('FUndoManager[0].isComposite = True', FUndoManager[0].IsComposite);
    AssertTrue('FUndoManager[0].Opend = True', FUndoManager[0].Opend);

    cmd := TAddElementCommand.Create(col, 1);
    FUndoManager.DoAndAddRecord(cmd);
    cmd := TAddElementCommand.Create(col, 2);
    FUndoManager.DoAndAddRecord(cmd);
    AssertEquals('FUndoManager.Size = 1', 1, FUndoManager.Size);
    AssertEquals('FUndoManager[0].Size = 2', 2, THistoryNode(FUndoManager[0]).Size);
    FUndoManager.CloseGroup;
    AssertTrue('FUndoManager[0].Closed = True', THistoryNode(FUndoManager[0]).Closed);
    AssertEquals('col.Count = 2', 2, col.Count);
    AssertEquals('col.First = 1', 1, col.First);
    AssertEquals('col.Last = 2', 2, col.Last);
    FUndoManager.Undo;
    AssertEquals('col.Count = 0', 0, col.Count);
  finally
    FreeAndNil(col);
  end;
end;


procedure THistoryIteratorTest.TestGroupedUndo2;
var
  col: TIntegerList;
  cmd: TUndoRedoRecord;
begin
  try
    col := TIntegerList.Create;
    col.Add(1);
    col.Add(2);
    col.Add(3);
    FUndoManager.OpenGroup;
    cmd := TRemoveElementCommand.Create(col);
    FUndoManager.DoAndAddRecord(cmd);
    cmd := TRemoveElementCommand.Create(col);
    FUndoManager.DoAndAddRecord(cmd);
    FUndoManager.CloseGroup;

    AssertTrue('(FUndoManager.Size = 1) and ( FUndoManager.Last.InheritsFrom(TUndoRedoGroup))', (FUndoManager.Size = 1) and (FUndoManager.Last.InheritsFrom(TUndoRedoGroup)));
    FUndoManager.Undo;
    AssertEquals('col.Count = 3', 3, col.Count);
    AssertEquals('col[1] = 2', 2, col[1]);
  finally
    FreeAndNil(col);
  end;

end;


procedure THistoryIteratorTest.UndoHandler(Sender: TObject);
begin
  ShowMessage('UndoHandler');
end;


procedure THistoryIteratorTest.RedoHandler(Sender: TObject);
begin
  ShowMessage('RedoHandler');
end;


procedure THistoryIteratorTest.SetUp;
begin
  FUndoManager := THistoryIterator.Create;
end;


procedure THistoryIteratorTest.TearDown;
begin
  FreeAndNil(FUndoManager);
end;

initialization

  RegisterTest(THistoryIteratorTest);
end.
