unit UGraphicTools;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Controls, Types, UWorldView, UGraphicView;

type

  { TGraphicTool }

  TGraphicTool = class(TViewTracking)
  private
    function GetGraphicView: TGraphicView;
  public
    property GraphicView: TGraphicView read GetGraphicView;
  end;

  TPointTool = class(TGraphicTool)
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
  end;

  TSelectTool = class(TGraphicTool)
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
  end;


implementation

uses
  UGraphicEnvirons, UGraphicDocument, UGraphicCore;

{ TGraphicTool }

function TGraphicTool.GetGraphicView: TGraphicView;
begin
  Result := (FWorldView as TGraphicView);
end;

procedure TPointTool.TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  xyPoint: TPointF;
begin
  inherited;
  if Button = mbLeft then
  begin
    xyPoint := GraphicView.WorldDrawer.Viewport.DeviceToWorld(X, Y);
    GraphicView.Document.UndoManager.DoAndAddRecord(
      TAddGraphicsCommand.Create(GraphicView.Document,
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
    xyPoint := GraphicView.WorldDrawer.Viewport.DeviceToWorld(X, Y);
    g := GraphicView.Document.FindGraphicAt(xyPoint,
      SENSOR_RADIUS, GraphicView.WorldDrawer.Viewport.WorldScale);
    if g <> nil then
    begin
      g.Selected := not g.Selected;
      FWorldView.Invalidate;
    end;
  end;
end;

end.
