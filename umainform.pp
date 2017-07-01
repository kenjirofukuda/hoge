unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, UGeometyUtils, UDocument,
  Forms, Controls, Menus, ExtCtrls, ComCtrls,
  Dialogs, LCLIntf, ActnList, UPointsDrawer, UGraphicView;

type
  { TMainForm }

  TMainForm = class(TForm)
    ViewFitAction: TAction;
    FitMenuItem: TMenuItem;
    ViewMenu: TMenuItem;
    ShowExtentBoundsMenuItem: TMenuItem;
    ShowExtentBoundsAction: TAction;
    MenuItem1: TMenuItem;
    ShowAxisLineMenuItem: TMenuItem;
    ShowAxisLineAction: TAction;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    StatusBar: TStatusBar;

    EditMenu: TMenuItem;
    ClearAllMenuItem: TMenuItem;
    DebugMenu: TMenuItem;
    RevealAppConfigDirMenuItem: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure GraphicViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure GraphicViewResize(Sender: TObject);

    procedure ClearAllMenuItemClick(Sender: TObject);
    procedure RevealAppConfigDirMenuItemClick(Sender: TObject);
    procedure ShowAxisLineActionExecute(Sender: TObject);
    procedure ShowAxisLineActionUpdate(Sender: TObject);

    procedure ShowExtentBoundsActionExecute(Sender: TObject);
    procedure ViewFitActionExecute(Sender: TObject);
  private
    FDocument: TDocument;
    FPointsDrawer: TPointsDrawer;
    FGraphicView: TGraphicView;
    procedure DocumentChange(Sender: TObject);
    procedure UpdateMouseStatus(H, V: integer);
  public
    { public declarations }
  end;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDocument := TDocument.Create;
  FDocument.LoadFromDefault;
  FPointsDrawer := TPointsDrawer.Create(FDocument);
  FDocument.OnChange := @DocumentChange;
  ClearAllMenuItem.Enabled := FDocument.ClearAllAction.Enabled;

  FGraphicView := TGraphicView.Create(self);
  with FGraphicView do
  begin
    Document := FDocument;
    PointsDrawer := FPointsDrawer;
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


procedure TMainForm.ClearAllMenuItemClick(Sender: TObject);
begin
  FDocument.ClearAllAction.DoIt;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDocument.SaveToDefault;
  FreeAndNil(FPointsDrawer);
  FreeAndNil(FDocument);
end;


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


procedure TMainForm.RevealAppConfigDirMenuItemClick(Sender: TObject);
begin
  OpenDocument(UDocument.AppConfigDir);
end;


procedure TMainForm.ShowAxisLineActionExecute(Sender: TObject);
begin
  FPointsDrawer.ShowAxisLine := not FPointsDrawer.ShowAxisLine;
  FGraphicView.Invalidate;
end;


procedure TMainForm.ShowAxisLineActionUpdate(Sender: TObject);
begin
  ShowAxisLineMenuItem.Checked := FPointsDrawer.ShowAxisLine;
end;


procedure TMainForm.ShowExtentBoundsActionExecute(Sender: TObject);
begin
  FPointsDrawer.ShowExtentBounds := not FPointsDrawer.ShowExtentBounds;
  FGraphicView.Refresh;
end;


procedure TMainForm.ViewFitActionExecute(Sender: TObject);
begin
  FPointsDrawer.Viewport.SetWorldBounds(FDocument.Bounds);
  FGraphicView.Refresh;
end;


procedure TMainForm.UpdateMouseStatus(H, V: integer);
var
  pt: TPointF;
  strs: TStringList;
begin
  pt := FPointsDrawer.Viewport.DeviceToWorld(H, V);
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
  ClearAllMenuItem.Enabled := FDocument.ClearAllAction.Enabled;
end;

end.
