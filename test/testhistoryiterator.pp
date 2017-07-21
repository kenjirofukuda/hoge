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
  end;

implementation

uses
  fgl, Dialogs;

type
  TIntegerList = specialize TFPGList<integer>;


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
    cmd := TUndoRedoRecord.Create;
    cmd.OnRedo := @RedoHandler;
    cmd.OnUndo := @UndoHandler;
    FUndoManager.DoAndAddRecord(cmd);
    AssertEquals('FUndoManager.Size = 1', 1, FUndoManager.Size);
    AssertNotNull('FUndoManager.Recorder[0] is not null', FUndoManager.Recorder[0]);
    AssertNotNull('FUndoManager.Current is not null', FUndoManager.Current);
    FUndoManager.Undo;
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
