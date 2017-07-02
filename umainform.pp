unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, UDocument,
  Forms, Controls, Menus, ExtCtrls, ComCtrls,
  Dialogs, LCLIntf, ActnList, UGraphicBase, UGraphicView;

type
  { TMainForm }

  TMainForm = class(TForm)
    StatusBar: TStatusBar;

    {Actions}
    ActionList: TActionList;
    ClearAllAction: TAction;
    ViewFitAction: TAction;
    ShowExtentBoundsAction: TAction;
    ShowAxisLineAction: TAction;

    MainMenu: TMainMenu;
    {[Edit}
    EditMenu: TMenuItem;
    ClearAllMenuItem: TMenuItem;

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
    procedure ClearAllActionExecute(Sender: TObject);
    procedure ClearAllActionUpdate(Sender: TObject);
    procedure ShowAxisLineActionExecute(Sender: TObject);
    procedure ShowAxisLineActionUpdate(Sender: TObject);
    procedure ShowExtentBoundsActionExecute(Sender: TObject);
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
procedure TMainForm.ClearAllActionUpdate(Sender: TObject);
begin
  ClearAllMenuItem.Enabled := FDocument.GetGraphics.Count > 0;
end;


procedure TMainForm.ClearAllActionExecute(Sender: TObject);
begin
  FDocument.RemoveAllPoints();
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


procedure TMainForm.ViewFitActionExecute(Sender: TObject);
begin
  FGraphicDrawer.Viewport.SetWorldBounds(FDocument.Bounds);
  FGraphicView.Invalidate;
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
