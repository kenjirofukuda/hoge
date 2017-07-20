unit TestHistoryNode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  UUndoManager;

type

  THistoryNodeTest = class(TTestCase)
  published
    procedure TestClose;
    procedure TestEmptyHistory;
    procedure TestReset;
  end;

implementation


procedure THistoryNodeTest.TestClose;
var
  h: THistoryNode;
  item: THistoryLeaf;
begin
  try
    h := THistoryNode.Create;
    h.Close;
    item := THistoryLeaf.Create;
    h.AddItem(item);
    h.Open;
    h.AddItem(item);
    AssertSame(item, h.Current);
  finally
    FreeAndNil(item);
    FreeAndNil(h);
  end;
end;


procedure THistoryNodeTest.TestEmptyHistory;
var
  h: THistoryNode;
  item: THistoryLeaf;
begin
  try
    h := THistoryNode.Create;
    AssertEquals(0, h.Size);
    AssertNull(h.Current);
  finally
    FreeAndNil(h);
  end;
end;


procedure THistoryNodeTest.TestReset;
var
  h: THistoryNode;
  item: THistoryLeaf;
begin
  try
    h := THistoryNode.Create;
    item := THistoryLeaf.Create;
    h.AddItem(item);
    AssertSame(item, h.Current);
    AssertEquals(1, h.Size);
    h.Reset;
    AssertEquals(0, h.Size);
  finally
    //FreeAndNil(item); // Already Disposed!
    FreeAndNil(h);
  end;
end;


initialization

  RegisterTest(THistoryNodeTest);
end.
