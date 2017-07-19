unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, UDocument,
  Forms, Controls, Menus, ExtCtrls, ComCtrls,
  Dialogs, LCLIntf, ActnList, StdCtrls, UGraphicBase, UGraphicView;

type
  { TMainForm }

  TMainForm = class(TForm)
    RedoAction: TAction;
    UndoAction: TAction;
    DeselectAllAction: TAction;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    RedoMenuItem: TMenuItem;
    UndoMenuItem: TMenuItem;
    SelectAllMenuItem: TMenuItem;
    DeselectAllMenuItem: TMenuItem;
    SelectAllAction: TAction;
    PointMenuItem: TMenuItem;
    SelectMenuItem: TMenuItem;
    ToolsMenu: TMenuItem;
    StatusBar: TStatusBar;

    {Actions}
    ActionList: TActionList;
    ClearAction: TAction;
    ViewFitAction: TAction;
    ShowExtentBoundsAction: TAction;
    ShowAxisLineAction: TAction;

    MainMenu: TMainMenu;
    {[Edit}
    EditMenu: TMenuItem;
    ClearMenuItem: TMenuItem;

    {[View}
    ViewMenu: TMenuItem;
    FitMenuItem: TMenuItem;

    {[Debug}
    DebugMenu: TMenuItem;
    RevealAppConfigDirMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    ShowExtentBoundsMenuItem: TMenuItem;
    ShowAxisLineMenuItem: TMenuItem;

    {Actions}
    procedure DeselectAllActionExecute(Sender: TObject);
    procedure DeselectAllActionUpdate(Sender: TObject);
    procedure RedoActionExecute(Sender: TObject);
    procedure RedoActionUpdate(Sender: TObject);
    procedure SelectAllActionExecute(Sender: TObject);
    procedure ClearActionExecute(Sender: TObject);
    procedure ClearActionUpdate(Sender: TObject);
    procedure PointMenuItemClick(Sender: TObject);
    procedure SelectAllActionUpdate(Sender: TObject);
    procedure SelectMenuItemClick(Sender: TObject);
    procedure ShowAxisLineActionExecute(Sender: TObject);
    procedure ShowAxisLineActionUpdate(Sender: TObject);
    procedure ShowExtentBoundsActionExecute(Sender: TObject);
    procedure UndoActionExecute(Sender: TObject);
    procedure UndoActionUpdate(Sender: TObject);
    procedure ViewFitActionExecute(Sender: TObject);
    procedure RevealAppConfigDirMenuItemClick(Sender: TObject);

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GraphicViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GraphicViewResize(Sender: TObject);

  private
    FDocument: TDocument;
    FGraphicDrawer: TGraphicDrawer;
    FGraphicView: TGraphicView;
    procedure DocumentChange(Sender: TObject);
    procedure UpdateMouseStatus(H, V: integer);
  end;


var
  MainForm: TMainForm;

implementation

uses
  UGraphicDrawer;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDocument := TDocument.Create;
  FDocument.LoadFromDefault;
  FGraphicDrawer := TGraphicDrawerImpl.Create;
  FDocument.OnChange := @DocumentChange;

  FGraphicView := TGraphicView.Create(self);
  with FGraphicView do
  begin
    Document := FDocument;
    GraphicDrawer := FGraphicDrawer;
    AnchorSideLeft.Control := Self;
    AnchorSideTop.Control := Self;
    AnchorSideRight.Control := Self;
    AnchorSideRight.Side := asrBottom;
    AnchorSideBottom.Control := StatusBar;
    Anchors := [akTop, akLeft, akRight, akBottom];
    Visible := True;
    Parent := Self;
    OnMouseMove := @GraphicViewMouseMove;
    OnResize := @GraphicViewResize;
  end;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDocument.SaveToDefault;
  FreeAndNil(FGraphicDrawer);
  FreeAndNil(FDocument);
end;


(* ----- Actions ----- *)
procedure TMainForm.ClearActionUpdate(Sender: TObject);
begin
  ClearMenuItem.Enabled := FDocument.SelectedCount > 0;
end;


procedure TMainForm.ClearActionExecute(Sender: TObject);
begin
  FDocument.RemoveSelectedGraphic;
end;


procedure TMainForm.SelectAllActionUpdate(Sender: TObject);
begin
  SelectAllMenuItem.Enabled := FDocument.GetGraphics.Count > 0;
end;


procedure TMainForm.SelectAllActionExecute(Sender: TObject);
begin
  FDocument.SetAllSelected(True);
end;


procedure TMainForm.DeselectAllActionUpdate(Sender: TObject);
begin
  DeselectAllMenuItem.Enabled := FDocument.GetGraphics.Count > 0;
end;

procedure TMainForm.RedoActionExecute(Sender: TObject);
begin
  FDocument.UndoManager.Redo;
end;

procedure TMainForm.RedoActionUpdate(Sender: TObject);
begin
  UndoAction.Enabled := FDocument.UndoManager.HasNext;
end;



procedure TMainForm.DeselectAllActionExecute(Sender: TObject);
begin
  FDocument.SetAllSelected(False);
end;


procedure TMainForm.RevealAppConfigDirMenuItemClick(Sender: TObject);
begin
  OpenDocument(UDocument.AppConfigDir);
end;


procedure TMainForm.ShowAxisLineActionExecute(Sender: TObject);
begin
  FGraphicView.ShowAxisLine := not FGraphicView.ShowAxisLine;
  FGraphicView.Invalidate;
end;


procedure TMainForm.ShowAxisLineActionUpdate(Sender: TObject);
begin
  ShowAxisLineMenuItem.Checked := FGraphicView.ShowAxisLine;
end;


procedure TMainForm.ShowExtentBoundsActionExecute(Sender: TObject);
begin
  FGraphicView.ShowExtentBounds := not FGraphicView.ShowExtentBounds;
  FGraphicView.Invalidate;
end;

procedure TMainForm.UndoActionExecute(Sender: TObject);
begin
  FDocument.UndoManager.Undo;
end;


procedure TMainForm.UndoActionUpdate(Sender: TObject);
begin
  UndoAction.Enabled := FDocument.UndoManager.Current <> nil;
end;


procedure TMainForm.ViewFitActionExecute(Sender: TObject);
begin
  FGraphicDrawer.Viewport.SetWorldBounds(FDocument.Bounds);
  FGraphicView.Invalidate;
end;

(* ----- MenuItem handlers ----- *)
procedure TMainForm.SelectMenuItemClick(Sender: TObject);
begin
  FGraphicView.ChooseTool('select');
end;


procedure TMainForm.PointMenuItemClick(Sender: TObject);
begin
  FGraphicView.ChooseTool('point');
end;



(* ----- Events ----- *)
procedure TMainForm.GraphicViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  UpdateMouseStatus(X, Y);
end;


procedure TMainForm.GraphicViewResize(Sender: TObject);
begin
  StatusBar.SimpleText := Format('W: %4d, H: %4d',
    [FGraphicView.ClientWidth, FGraphicView.ClientHeight]);
end;


procedure TMainForm.UpdateMouseStatus(H, V: integer);
var
  pt: TPointF;
  strs: TStringList;
begin
  pt := FGraphicDrawer.Viewport.DeviceToWorld(H, V);
  strs := TStringList.Create;
  strs.Delimiter := ' ';
  strs.QuoteChar := ' ';
  strs.Add(Format('H: %4d, V: %4d', [H, V]));
  strs.Add(Format('X: %10.4f, Y: %10.4f', [pt.x, pt.y]));
  strs.Add(Format('W: %4d, H: %4d', [FGraphicView.ClientWidth,
    FGraphicView.ClientHeight]));
  StatusBar.SimpleText := strs.DelimitedText;
  strs.Free;
end;


procedure TMainForm.DocumentChange(Sender: TObject);
begin
  FGraphicView.Invalidate;
end;

end.
