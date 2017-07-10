unit UGraphicDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, ExtCtrls,
  UViewport, UGeometyUtils, UGraphicBase;

type
  TGraphicDrawerImpl = class(TGraphicDrawer)

  private
    procedure VLine(Canvas: TCanvas; AXValue: single);
    procedure HLine(Canvas: TCanvas; AYValue: single);

  public
    procedure DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList); override;
    procedure DrawAxisLineOn(Canvas: TCanvas); override;
    procedure FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF;
      AUnitSize: integer); override;
    procedure FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF); override;
    procedure FillHandle(Canvas: TCanvas; At: TPointF); override;
  end;


implementation

uses
  UGraphic;


procedure TGraphicDrawerImpl.DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList);
var
  g: TGraphic;
begin
  for g in AGraphicList do
    g.DrawOn(Canvas, self);
end;


procedure TGraphicDrawerImpl.DrawAxisLineOn(Canvas: TCanvas);
var
  g: TGraphic;
begin
  Canvas.Pen.Color := clBlack;
  VLine(Canvas, 0);
  HLine(Canvas, 0);
end;


procedure TGraphicDrawerImpl.FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF;
  AUnitSize: integer);
var
  hvPoint: TPointF;
begin
  hvPoint := Viewport.WorldToDevice(AWorldPoint.x, AWorldPoint.y);
  Canvas.Ellipse(round(hvPoint.x - AUnitSize), round(hvPoint.y - AUnitSize),
    round(hvPoint.x + AUnitSize), round(hvPoint.y + AUnitSize));
end;


procedure TGraphicDrawerImpl.FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF);
var
  hvOrigin: TPointF;
  hvCorner: TPointF;
begin
  if not AWorldBounds.IsValid then
    exit;
  hvOrigin := Viewport.WorldToDevice(AWorldBounds.Origin.x, AWorldBounds.Origin.y);
  hvCorner := Viewport.WorldToDevice(AWorldBounds.Corner.x, AWorldBounds.Corner.y);
  Canvas.Frame(round(hvOrigin.x), round(hvCorner.y), round(hvCorner.x),
    round(hvOrigin.y));
end;


procedure TGraphicDrawerImpl.VLine(Canvas: TCanvas; AXValue: single);
var
  xyPoint: TPointF;
  hvPoint: TPointF;
begin
  xyPoint.x := AXValue;
  xyPoint.y := 0;
  hvPoint := Viewport.WorldToDevice(xyPoint.x, xyPoint.y);
  Canvas.Line(round(hvPoint.x), 0, round(hvPoint.x), Canvas.Height);
end;


procedure TGraphicDrawerImpl.HLine(Canvas: TCanvas; AYValue: single);
var
  xyPoint: TPointF;
  hvPoint: TPointF;
begin
  xyPoint.x := 0;
  xyPoint.y := AYValue;
  hvPoint := Viewport.WorldToDevice(xyPoint.x, xyPoint.y);
  Canvas.Line(0, round(hvPoint.y), Canvas.Width, round(hvPoint.y));
end;


procedure TGraphicDrawerImpl.FillHandle(Canvas: TCanvas; At: TPointF);
var
  i: longint;
  hvPoint: TPointF;
  r: TRect;
  savedColor: TColor;
begin
  i := 4;
  hvPoint := Viewport.WorldToDevice(At.x, At.y);
  r := Rect(round(hvPoint.x) - i, round(hvPoint.y) - i, round(hvPoint.x) +
    i, round(hvPoint.y) + i);
  Canvas.FillRect(r);
end;

end.
