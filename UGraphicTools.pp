unit UGraphicTools;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Controls, Types, UWorldView, UGraphicView, UGeometryUtils;

type

  { TGraphicTool }

  TGraphicTool = class(TViewTracking)
  private
    function GetGraphicView: TGraphicView;
  public
    property GraphicView: TGraphicView read GetGraphicView;
    function ToWorldPoint(H, V: Integer): TPointF;
  end;

  TPointTool = class(TGraphicTool)
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
  end;

  { TRectTool }

  TRectTool = class(TGraphicTool)
  private
    FPoints: TXYPointList;
  public
    constructor Create(AWorldView: TWorldView);
    destructor Destroy; override;

    procedure TrackMove(Shift: TShiftState; X, Y: integer); override;
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure Reset; override;
  end;

  TSelectTool = class(TGraphicTool)
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
  end;


implementation

uses
  UGraphicEnvirons, UGraphicDocument, UGraphicCore;

{ TRectTool }

constructor TRectTool.Create(AWorldView: TWorldView);
begin
  inherited;
  FPoints := TXYPointList.Create;
end;

destructor TRectTool.Destroy;
begin
  FreeAndNil(FPoints);
  inherited Destroy;
end;

procedure TRectTool.TrackMove(Shift: TShiftState; X, Y: integer);
var
  g: TKFGraphic;
begin
  inherited TrackMove(Shift, X, Y);
  g := GraphicView.GetFeedBack1;
  if g = nil then Exit;
  (g as TRectGraphic).SetCorner(ToWorldPoint(X, Y));
  GraphicView.Invalidate;
end;

procedure TRectTool.TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  g: TKFGraphic;
begin
  inherited;
  if Button = mbLeft then
  begin
    FPoints.Add(ToWorldPoint(X, Y));
    if FPoints.Count = 1 then
    begin
      GraphicView.AddFeedback(
        TRectGraphic.Create(FPoints.Items[0], FPoints.Items[0]));
    end;

    if FPoints.Count = 2 then
    begin
      g := GraphicView.GetFeedBack1;
      if g = nil then Exit;
      with (g as TRectGraphic) do
      begin
        SetCorner(ToWorldPoint(X, Y));
        ValidateGeometry;
      end;
      GraphicView.Document.UndoManager.DoAndAddRecord(
        TAddGraphicsCommand.Create(GraphicView.Document, g));
      Reset;
    end;
  end;
end;

procedure TRectTool.Reset;
begin
  inherited Reset;
  FPoints.Clear;
  GraphicView.ClearFeedback;
end;

{ TGraphicTool }

function TGraphicTool.GetGraphicView: TGraphicView;
begin
  Result := (FWorldView as TGraphicView);
end;

function TGraphicTool.ToWorldPoint(H, V: Integer): TPointF;
begin
  Result := GraphicView.WorldDrawer.Viewport.DeviceToWorld(H, V);
end;

procedure TPointTool.TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  xyPoint: TPointF;
begin
  inherited;
  if Button = mbLeft then
  begin
    xyPoint := ToWorldPoint(X, Y);
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
    xyPoint := ToWorldPoint(X, Y);
    g := GraphicView.Document.FindGraphicAt(xyPoint, SENSOR_RADIUS,
      GraphicView.WorldDrawer.Viewport.WorldScale);
    if g <> nil then
    begin
      g.Selected := not g.Selected;
      FWorldView.Invalidate;
    end;
  end;
end;

end.
