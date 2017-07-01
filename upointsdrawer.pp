unit UPointsDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, ExtCtrls,
  UDocument, UViewport, UGeometyUtils;

type
  TPointsDrawer = class
    constructor Create(ADocument: TDocument);

  private
    FDocument: TDocument;
    FViewport: TViewport;
    FShowAxisLine: Boolean;
    FShowExtentBounds: Boolean;

    procedure VLine(Canvas: TCanvas; AXValue: single);
    procedure HLine(Canvas: TCanvas; AYValue: single);
    procedure FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF;
  AUnitSize: integer);
    procedure FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF);

  public
    destructor Destroy; override;
    procedure DrawOn(Canvas: TCanvas);
    procedure FrameExtentBoundsOn(Canvas: TCanvas);
    property Viewport: TViewport read FViewport;
    property ShowAxisLine: Boolean read FShowAxisLine write FShowAxisLine;
    property ShowExtentBounds: Boolean read FShowExtentBounds write FShowExtentBounds;
  end;

implementation


constructor TPointsDrawer.Create(ADocument: TDocument);
begin
  FDocument := ADocument;
  FViewport := TViewport.Create;
  FShowAxisLine := False;
  FShowExtentBounds := False;
end;


destructor TPointsDrawer.Destroy;
begin
  FreeAndNil(FViewport);
  inherited;
end;


procedure TPointsDrawer.DrawOn(Canvas: TCanvas);
const
  UNIT_SIZE = 2;
var
  xyPoint: TPointF;
begin
  if FShowAxisLine then
  begin
    VLine(Canvas, 0);
    HLine(Canvas, 0);
  end;
  for xyPoint in FDocument.GetPoints do
  begin
    FramePointOn(Canvas, xyPoint, UNIT_SIZE);
  end;
  if ShowExtentBounds then
     FrameExtentBoundsOn(Canvas);
end;


procedure TPointsDrawer.FrameExtentBoundsOn(Canvas: TCanvas);
begin
  FrameBoundsOn(Canvas, FDocument.Bounds);
end;


procedure TPointsDrawer.FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF;
  AUnitSize: integer);
var
  hvPoint: TPointF;
begin
  hvPoint := FViewport.WorldToDevice(AWorldPoint.x, AWorldPoint.y);
  Canvas.Ellipse(round(hvPoint.x - AUnitSize), round(hvPoint.y - AUnitSize),
    round(hvPoint.x + AUnitSize), round(hvPoint.y + AUnitSize));
end;


procedure TPointsDrawer.FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF);
var
  hvOrigin: TPointF;
  hvCorner: TPointF;
begin
  hvOrigin := FViewport.WorldToDevice(AWorldBounds.Origin.x, AWorldBounds.Origin.y);
  hvCorner := FViewport.WorldToDevice(AWorldBounds.Corner.x, AWorldBounds.Corner.y);
  Canvas.Frame(round(hvOrigin.x), round(hvCorner.y), round(hvCorner.x),
    round(hvOrigin.y));
end;


procedure TPointsDrawer.VLine(Canvas: TCanvas; AXValue: single);
var
  xyPoint: TPointF;
  hvPoint: TPointF;
begin
  xyPoint.x := AXValue;
  xyPoint.y := 0;
  hvPoint := FViewport.WorldToDevice(xyPoint.x, xyPoint.y);
  Canvas.Line(round(hvPoint.x), 0, round(hvPoint.x), Canvas.Height);
end;


procedure TPointsDrawer.HLine(Canvas: TCanvas; AYValue: single);
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
