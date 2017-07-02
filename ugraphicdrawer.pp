unit UGraphicDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, ExtCtrls,
  UViewport, UGeometyUtils, UGraphic;

type
  TGraphicDrawer = class
    constructor Create;

  private
    FViewport: TViewport;
    FShowAxisLine: boolean;

  public
    destructor Destroy; override;
    procedure DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList);
    property Viewport: TViewport read FViewport;
    property ShowAxisLine: boolean read FShowAxisLine write FShowAxisLine;
    procedure VLine(Canvas: TCanvas; AXValue: single);
    procedure HLine(Canvas: TCanvas; AYValue: single);
    procedure FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF; AUnitSize: integer);
    procedure FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF);
  end;


implementation



constructor TGraphicDrawer.Create;
begin
  FViewport := TViewport.Create;
  FShowAxisLine := True;
  FViewport.ResetWorld;
  FViewport.ResetPortCenter;
end;


destructor TGraphicDrawer.Destroy;
begin
  FreeAndNil(FViewport);
  inherited;
end;


procedure TGraphicDrawer.DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList);
const
  UNIT_SIZE = 2;
var
  g: TGraphic;
  point: TPointGraphic;
begin
  Canvas.Pen.Color := clBlack;
  if FShowAxisLine then
  begin
    VLine(Canvas, 0);
    HLine(Canvas, 0);
  end;
  for g in AGraphicList do
  begin
    point := g as TPointGraphic;
    FramePointOn(Canvas, point.Origin, UNIT_SIZE);
  end;
end;



procedure TGraphicDrawer.FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF;
  AUnitSize: integer);
var
  hvPoint: TPointF;
begin
  hvPoint := FViewport.WorldToDevice(AWorldPoint.x, AWorldPoint.y);
  Canvas.Ellipse(round(hvPoint.x - AUnitSize), round(hvPoint.y - AUnitSize),
    round(hvPoint.x + AUnitSize), round(hvPoint.y + AUnitSize));
end;


procedure TGraphicDrawer.FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF);
var
  hvOrigin: TPointF;
  hvCorner: TPointF;
begin
  hvOrigin := FViewport.WorldToDevice(AWorldBounds.Origin.x, AWorldBounds.Origin.y);
  hvCorner := FViewport.WorldToDevice(AWorldBounds.Corner.x, AWorldBounds.Corner.y);
  Canvas.Frame(round(hvOrigin.x), round(hvCorner.y), round(hvCorner.x),
    round(hvOrigin.y));
end;


procedure TGraphicDrawer.VLine(Canvas: TCanvas; AXValue: single);
var
  xyPoint: TPointF;
  hvPoint: TPointF;
begin
  xyPoint.x := AXValue;
  xyPoint.y := 0;
  hvPoint := FViewport.WorldToDevice(xyPoint.x, xyPoint.y);
  Canvas.Line(round(hvPoint.x), 0, round(hvPoint.x), Canvas.Height);
end;


procedure TGraphicDrawer.HLine(Canvas: TCanvas; AYValue: single);
var
  xyPoint: TPointF;
  hvPoint: TPointF;
begin
  xyPoint.x := 0;
  xyPoint.y := AYValue;
  hvPoint := FViewport.WorldToDevice(xyPoint.x, xyPoint.y);
  Canvas.Line(0, round(hvPoint.y), Canvas.Width, round(hvPoint.y));
end;

end.
