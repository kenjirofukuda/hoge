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
    procedure TestGroup;
    procedure TestOneGroup;
    procedure TestTwoGroups;
    procedure TestTwoConsecutiveCloseGroup;
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


procedure THistoryNodeTest.TestGroup;
var
  h: THistoryNode;
  item: THistoryLeaf;
begin
  try
    h := THistoryNode.Create;
    item := THistoryLeaf.Create;
    h.AddItem(item);
    AssertEquals(1, h.Size);
    AssertSame(item, h[0]);
    h.OpenGroup;
    AssertEquals('Size is 2', 2, h.Size);
    AssertSame('first no change', item, h[0]);
    AssertTrue('Must be composite', h[1].IsComposite);
  finally
    FreeAndNil(h);
  end;
end;


procedure THistoryNodeTest.TestOneGroup;
var
  h: THistoryNode;
  item, item2, item3, item4: THistoryLeaf;
  c: THistoryNode;
begin
  try
    h := THistoryNode.Create;
    item := THistoryLeaf.Create;
    h.AddItem(item);
    AssertEquals(1, h.Size);
    AssertSame(item, h[0]);

    h.OpenGroup;
    AssertEquals('Size is 2', 2, h.Size);
    AssertSame('first no change', item, h[0]);
    c := h[1] as THistoryNode;
    AssertTrue('Must be composite', c.IsComposite);
    AssertTrue('Must be empty', c.IsEmpty);

    item2 := THistoryLeaf.Create;
    h.AddItem(item2);
    AssertEquals('Size is 2', 2, h.Size);
    AssertEquals('C Size is 1', 1, c.Size);
    AssertSame('c.Current is item2', item2, c.Current);

    h.CloseGroup;
    item3 := THistoryLeaf.Create;
    h.AddItem(item3);
    AssertEquals('h.Size is 3', 3, h.Size);
    AssertSame('h[2] = item3', item3, h[2]);

    h.CloseGroup;
    item4 := THistoryLeaf.Create;
    h.AddItem(item4);
    AssertEquals('item4''s h.Size is 3', 3, h.Size);
  finally
    FreeAndNil(h);
  end;
end;


procedure THistoryNodeTest.TestTwoGroups;
var
  h: THistoryNode;
  item, item2, item3, item4: THistoryLeaf;
  grp1, grp2: THistoryNode;
begin
  try
    h := THistoryNode.Create;
    item := THistoryLeaf.Create;
    h.AddItem(item);
    AssertEquals(1, h.Size);
    AssertSame(item, h[0]);

    h.OpenGroup;
    AssertEquals('h.Size is 2', 2, h.Size);
    grp1 := h[1] as THistoryNode;
    h.OpenGroup;
    AssertEquals('OpenGroup*2 Size is 2', 2, h.Size);
    AssertEquals('grp1.Size = 1', 1, grp1.Size);
    grp2 := grp1[0] as THistoryNode;
    AssertEquals('grp2.Size = 0', 0, grp2.Size);

    item2 := THistoryLeaf.Create;
    h.AddItem(item2);
    AssertEquals('grp2.Size = 1', 1, grp2.Size);

    h.CloseGroup;
    item3 := THistoryLeaf.Create;
    h.AddItem(item3);
    AssertEquals('item3: grp2.Size = 1', 1, grp2.Size);
    AssertEquals('item3: grp1.Size = 2', 2, grp1.Size);
    AssertSame('item3: grp1.Current is item3', item3, grp1.Current);

    h.CloseGroup;
    item4 := THistoryLeaf.Create;
    h.AddItem(item4);
    AssertEquals('item4: grp2.Size = 1', 1, grp2.Size);
    AssertEquals('item4: grp1.Size = 2', 2, grp1.Size);
    AssertEquals('item4: h.Size is 3', 3, h.Size);
    AssertSame('item4: h.Current is item4', item4, h.Current);
  finally
    FreeAndNil(h);
  end;
end;


procedure THistoryNodeTest.TestTwoConsecutiveCloseGroup;
var
  h: THistoryNode;
  item, item2: THistoryLeaf;
  grp1, grp2: THistoryNode;
begin
  try
    h := THistoryNode.Create;
    item := THistoryLeaf.Create;
    h.AddItem(item);
    AssertEquals(1, h.Size);
    AssertSame(item, h[0]);

    h.OpenGroup;
    AssertEquals('h.Size is 2', 2, h.Size);
    grp1 := h[1] as THistoryNode;
    h.OpenGroup;
    AssertEquals('OpenGroup*2 h.Size is 2', 2, h.Size);
    AssertEquals('grp1.Size = 1', 1, grp1.Size);
    grp2 := grp1[0] as THistoryNode;
    AssertEquals('grp2.Size = 0', 0, grp2.Size);
    h.CloseGroup;
    h.CloseGroup;
    item2 := THistoryLeaf.Create;
    h.AddItem(item2);
    AssertEquals('item2: h.Size is 3', 3, h.Size);
    AssertSame('item2: h.Current is item2', item2, h.Current);
  finally
    FreeAndNil(h);
  end;
end;

initialization

  RegisterTest(THistoryNodeTest);
end.
