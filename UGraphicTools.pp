unit UGraphicTools;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Controls, Types, UWorldView;

type
  TPointTool = class(TViewTracking)
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
  end;

  TSelectTool = class(TViewTracking)
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
  end;


implementation

uses
  UGraphicEnvirons, UGraphicDocument, UGraphicView;

procedure TPointTool.TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  xyPoint: TPointF;
begin
  inherited;
  if Button = mbLeft then
  begin
    xyPoint := FWorldView.WorldDrawer.Viewport.DeviceToWorld(X, Y);
    FWorldView.Document.UndoManager.DoAndAddRecord(
      TAddGraphicsCommand.Create((FWorldView.Document as TGraphicDocument),
      TPointGraphic.Create(xyPoint.x, xyPoint.y)));
  end;
end;


procedure TSelectTool.TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
const
  SENSOR_RADIUS = 7;
var
  xyPoint: TPointF;
  g: TKFGraphic;
begin
  inherited;
  if Button = mbLeft then
  begin
    xyPoint := FWorldView.WorldDrawer.Viewport.DeviceToWorld(X, Y);
    g := (FWorldView.Document as TGraphicDocument).FindGraphicAt(xyPoint,
      SENSOR_RADIUS, FWorldView.WorldDrawer.Viewport.WorldScale);
    if g <> nil then
    begin
      g.Selected := not g.Selected;
      FWorldView.Invalidate;
    end;
  end;
end;

end.
