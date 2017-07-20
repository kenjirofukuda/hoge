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
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCurrentOnStartup;
  end;

implementation


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
