unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fgl, UGeometyUtils, UDocument,
  Forms, Controls, Menus, ExtCtrls, ComCtrls,
  Dialogs, LCLIntf, ActnList, UPointsDrawer;

type

  TTrackingAttributes = record
    MiddleDown: boolean;
    CurrPoint: TPoint;
    DownPoint: TPoint;
    MovePoints: THVPointList;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    ShowExtentBoundsMenuItem: TMenuItem;
    ShowExtentBoundsAction: TAction;
    MenuItem1: TMenuItem;
    ShowAxisLineMenuItem: TMenuItem;
    ShowAxisLineAction: TAction;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    PointsView: TPaintBox;
    StatusBar: TStatusBar;

    EditMenu: TMenuItem;
    ClearAllMenuItem: TMenuItem;
    DebugMenu: TMenuItem;
    RevealAppConfigDirMenuItem: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PointsViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

    procedure PointsViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure PointsViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PointsViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure PointsViewPaint(Sender: TObject);
    procedure PointsViewResize(Sender: TObject);

    procedure ClearAllMenuItemClick(Sender: TObject);
    procedure RevealAppConfigDirMenuItemClick(Sender: TObject);
    procedure ShowAxisLineActionExecute(Sender: TObject);

    procedure ShowExtentBoundsActionExecute(Sender: TObject);
  private
    FDocument: TDocument;
    FPointsDrawer: TPointsDrawer;
    FTrackingAttributes: TTrackingAttributes;
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
  FTrackingAttributes.MovePoints := THVPointList.Create;
  ClearAllMenuItem.Enabled := FDocument.ClearAllAction.Enabled;
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
  FreeAndNil(FTrackingAttributes.MovePoints);
end;


procedure TMainForm.PointsViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbMiddle then
  begin
    with FTrackingAttributes do
    begin
      MiddleDown := True;
      DownPoint := Point(X, Y);
      if MovePoints = nil then
        MovePoints := THVPointList.Create;
      MovePoints.Clear;
    end;
  end;
end;


procedure TMainForm.PointsViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  p1, p2: TPoint;
  wp1, wp2, moved: TPointF;
begin
  UpdateMouseStatus(X, Y);
  with FTrackingAttributes do
  begin
    if not MiddleDown then
      exit;
    if DownPoint = Point(X, Y) then
      exit;
    CurrPoint := Point(X, Y);
    MovePoints.Add(CurrPoint);
    if MovePoints.Count > 2 then
    begin
      p1 := MovePoints.Items[MovePoints.Count - 2];
      p2 := MovePoints.Items[MovePoints.Count - 1];
      with FPointsDrawer.Viewport do
      begin
        wp1 := DeviceToWorld(p1.x, p1.y);
        wp2 := DeviceToWorld(p2.x, p2.y);
      end;
      moved := wp2 - wp1;
      FPointsDrawer.Viewport.OffSetWorldCenter(-moved.x, -moved.y);
      PointsView.Invalidate;
    end;
  end;
end;


procedure TMainForm.PointsViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  xyPoint: TPointF;
begin
  if Button = mbLeft then
  begin
    xyPoint := FPointsDrawer.Viewport.DeviceToWorld(X, Y);
    FDocument.AddPoint(xyPoint.x, xyPoint.y);
  end;
  FTrackingAttributes.MiddleDown := False;
end;


procedure TMainForm.PointsViewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  direction: single;
begin
  if WheelDelta < 0 then
    direction := -1.0
  else
    direction := 1.0;
  FPointsDrawer.Viewport.WheelZoom(MousePos.x, MousePos.y, direction);
  PointsView.Invalidate;
end;


procedure TMainForm.PointsViewPaint(Sender: TObject);
begin
  FPointsDrawer.DrawOn(PointsView.Canvas);
end;


procedure TMainForm.PointsViewResize(Sender: TObject);
begin
  StatusBar.SimpleText := Format('W: %4d, H: %4d',
    [PointsView.ClientWidth, PointsView.ClientHeight]);
  FPointsDrawer.Viewport.SetPortSize(PointsView.ClientWidth, PointsView.ClientHeight);
end;


procedure TMainForm.RevealAppConfigDirMenuItemClick(Sender: TObject);
begin
  OpenDocument(UDocument.AppConfigDir);
end;

procedure TMainForm.ShowAxisLineActionExecute(Sender: TObject);
begin
  FPointsDrawer.ShowAxisLine := not FPointsDrawer.ShowAxisLine;
  PointsView.Refresh;
end;


procedure TMainForm.ShowExtentBoundsActionExecute(Sender: TObject);
begin
  FPointsDrawer.ShowExtentBounds := not FPointsDrawer.ShowExtentBounds;
  PointsView.Refresh;
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
  strs.Add(Format('W: %4d, H: %4d', [PointsView.ClientWidth, PointsView.ClientHeight]));
  StatusBar.SimpleText := strs.DelimitedText;
  strs.Free;
end;


procedure TMainForm.DocumentChange(Sender: TObject);
begin
  PointsView.Invalidate;
  ClearAllMenuItem.Enabled := FDocument.ClearAllAction.Enabled;
end;

end.
