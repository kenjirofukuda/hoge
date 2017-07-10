unit UTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Types, UGeometyUtils, UGraphicView;

type
  TViewTrackingImpl = class(TViewTracking)
  public
    procedure TrackBegin(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure TrackMove(Shift: TShiftState; X, Y: integer); override;
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure TrackWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint; var Handled: boolean); override;
  private
    procedure ViewMove(Shift: TShiftState; X, Y: integer);
  end;


type
  TPointTool = class(TViewTrackingImpl)
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  end;


implementation

procedure TViewTrackingImpl.ViewMove(Shift: TShiftState; X, Y: integer);
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


procedure TViewTrackingImpl.TrackBegin(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  if Button = mbMiddle then
  begin
    FMiddleDown := True;
    FDownPoint := Point(X, Y);
    FMovePoints.Add(FDownPoint);
  end;
end;


procedure TViewTrackingImpl.TrackMove(Shift: TShiftState; X, Y: integer);
begin
  if not FMiddleDown then
    exit;
  if FDownPoint = Point(X, Y) then
    exit;
  ViewMove(Shift, X, Y);
end;


procedure TViewTrackingImpl.TrackEnd(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  xyPoint: TPointF;
begin
  FMiddleDown := False;
end;


procedure TViewTrackingImpl.TrackWheel(Shift: TShiftState; WheelDelta: integer;
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


procedure TPointTool.TrackEnd(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  xyPoint: TPointF;
begin
  inherited;
  if Button = mbLeft then
  begin
    xyPoint := FGraphicView.GraphicDrawer.Viewport.DeviceToWorld(X, Y);
    FGraphicView.Document.AddPoint(xyPoint.x, xyPoint.y);
  end;
end;


end.
