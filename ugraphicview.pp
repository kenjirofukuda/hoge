unit UGraphicView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, UGeometyUtils, UDocument,
  Controls, Menus, ExtCtrls,
  Dialogs, LCLIntf, UGraphicBase;

type
  TGraphicView = class;

  TViewTracking = class
  public
    constructor Create(AGraphicView: TGraphicView);
    destructor Destroy; override;

    procedure TrackBegin(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TrackMove(Shift: TShiftState; X, Y: integer);
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure TrackWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint; var Handled: boolean);
  private
    FMiddleDown: boolean;
    FCurrPoint: TPoint;
    FDownPoint: TPoint;
    FMovePoints: THVPointList;
    FGraphicView: TGraphicView;

    procedure ViewMove(Shift: TShiftState; X, Y: integer);
  end;


  TGraphicView = class(TPaintBox)
    constructor Create(AOwner: TComponent); override;

    procedure Paint; override;
    procedure DoOnResize; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint): boolean; override;

  private
    FDocument: TDocument;
    FGraphicDrawer: TGraphicDrawer;
    FTrackingAttributes: TViewTracking;
    FFirstResizeHandled: boolean;
    FShowExtentBounds: boolean;
    FShowAxisLine: boolean;

    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure HandlePaint(Sender: TObject);
    procedure HandleResize(Sender: TObject);

  public
    destructor Destroy; override;
    property Document: TDocument read FDocument write FDocument;
    property GraphicDrawer: TGraphicDrawer read FGraphicDrawer write FGraphicDrawer;
    property ShowExtentBounds: boolean read FShowExtentBounds write FShowExtentBounds;
    property ShowAxisLine: boolean read FShowAxisLine write FShowAxisLine;
  end;

implementation


constructor TGraphicView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowExtentBounds := False;
  FShowAxisLine := True;
  FTrackingAttributes := TViewTracking.Create(self);
end;


destructor TGraphicView.Destroy;
begin
  FreeAndNil(FTrackingAttributes);
  inherited Destroy;
end;


procedure TGraphicView.HandleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if Assigned(FTrackingAttributes) then
    FTrackingAttributes.TrackBegin(Button, Shift, X, Y);
end;


procedure TGraphicView.HandleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if Assigned(FTrackingAttributes) then
    FTrackingAttributes.TrackMove(Shift, X, Y);
end;


procedure TGraphicView.HandleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if not Assigned(FDocument) then
    exit;
  if Assigned(FTrackingAttributes) then
    FTrackingAttributes.TrackEnd(Button, Shift, X, Y);
end;


procedure TGraphicView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if Assigned(FTrackingAttributes) then
    FTrackingAttributes.TrackWheel(Shift, WheelDelta, MousePos, Handled);
end;


procedure TGraphicView.HandlePaint(Sender: TObject);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if ShowAxisLine then
  begin
    FGraphicDrawer.DrawAxisLineOn(Canvas);
  end;
  Canvas.Pen.Color := clBlack;
  FGraphicDrawer.DrawOn(Canvas, FDocument.GetGraphics);
  Canvas.Pen.Color := clLtGray;
  if ShowExtentBounds then
    FGraphicDrawer.FrameBoundsOn(Canvas, FDocument.Bounds);
end;


procedure TGraphicView.HandleResize(Sender: TObject);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  FGraphicDrawer.Viewport.SetPortSize(ClientWidth, ClientHeight);
  if not FFirstResizeHandled then
  begin
    FGraphicDrawer.Viewport.ResetPortCenter;
    FFirstResizeHandled := True;
  end;
end;


procedure TGraphicView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  HandleMouseDown(self, Button, Shift, X, Y);
end;


procedure TGraphicView.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  HandleMouseMove(self, Shift, X, Y);
end;


procedure TGraphicView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  HandleMouseUp(self, Button, Shift, X, Y);
end;


function TGraphicView.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  HandleMouseWheel(self, Shift, WheelDelta, MousePos, Result);
end;


procedure TGraphicView.Paint;
begin
  HandlePaint(self);
  inherited Paint;
end;


procedure TGraphicView.DoOnResize;
begin
  inherited DoOnResize;
  HandleResize(Self);
end;


constructor TViewTracking.Create(AGraphicView: TGraphicView);
begin
  FGraphicView := AGraphicView;
  if not Assigned(FMovePoints) then
    FMovePoints := THVPointList.Create;
  FMovePoints.Clear;
end;


destructor TViewTracking.Destroy;
begin
  FreeAndNil(FMovePoints);
  inherited Destroy;
end;


procedure TViewTracking.ViewMove(Shift: TShiftState; X, Y: integer);
var
  p1, p2, movedDevice: TPoint;
  wp1, wp2, moved: TPointF;
begin
  FCurrPoint := Point(X, Y);
  FMovePoints.Add(FCurrPoint);
  if FMovePoints.Count > 2 then
  begin
    p1 := FMovePoints.Items[FMovePoints.Count - 2];
    p2 := FMovePoints.Items[FMovePoints.Count - 1];
    movedDevice := p2 - p1;
    movedDevice.y := movedDevice.y * -1;

    with FGraphicView.GraphicDrawer.Viewport do
    begin
      wp1 := DeviceToWorld(p1.x, p1.y);
      wp2 := DeviceToWorld(p2.x, p2.y);
      moved := wp2 - wp1;
      OffSetWorldCenter(-moved.x, -moved.y);
    end;
    FGraphicView.Invalidate;
  end;
end;


procedure TViewTracking.TrackBegin(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if Button = mbMiddle then
  begin
    FMiddleDown := True;
    FDownPoint := Point(X, Y);
  end;
end;


procedure TViewTracking.TrackMove(Shift: TShiftState; X, Y: integer);
begin
  if not FMiddleDown then
    exit;
  if FDownPoint = Point(X, Y) then
    exit;
  ViewMove(Shift, X, Y);
end;


procedure TViewTracking.TrackEnd(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  xyPoint: TPointF;
begin
  if Button = mbLeft then
  begin
    xyPoint := FGraphicView.GraphicDrawer.Viewport.DeviceToWorld(X, Y);
    FGraphicView.Document.AddPoint(xyPoint.x, xyPoint.y);
  end;
  FMiddleDown := False;
end;


procedure TViewTracking.TrackWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint; var Handled: boolean);
var
  direction: single;
begin
  if WheelDelta < 0 then
    direction := -1.0
  else
    direction := 1.0;
  FGraphicView.GraphicDrawer.Viewport.WheelZoom(MousePos.x, MousePos.y, direction);
  FGraphicView.Invalidate;
end;


end.
