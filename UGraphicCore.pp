unit UGraphicCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl, UGeometryUtils,
  Controls, Menus, ExtCtrls,
  Dialogs, LCLIntf, UWorldView;

type
  TKFGraphic = class;

  TGraphicList = specialize TFPGObjectList<TKFGraphic>;

  { TGraphicDrawer }

  TGraphicDrawer = class(TWorldDrawer)
  public
    procedure DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList; AFeedback: boolean);
    procedure DrawAxisLineOn(Canvas: TCanvas); override;
  end;

  { TKFGraphic }

  TKFGraphic = class
    abstract
  private
    FSelected: boolean;
  public
    procedure DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer; AFeedback: boolean); virtual; abstract;
    function Distance(APoint: TPointF): single; virtual; abstract;
    function AsList: TGraphicList;
    function Bounds: TRectangleF; virtual; abstract;
    property Selected: boolean read FSelected write FSelected;
  end;

  { TKFGraphic }

  { TPointGraphic }

  TPointGraphic = class(TKFGraphic)
  private
    FPoint: TPointF;
  public
    constructor Create(APoint: TPointF);
    constructor Create(X, Y: single);

    procedure DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer; AFeedback: boolean); override;
    function Distance(APoint: TPointF): single; override;
    function Bounds: TRectangleF; override;

    property x: single read FPoint.x write FPoint.x;
    property y: single read FPoint.y write FPoint.y;
    property Origin: TPointF read FPoint;
  end;


  { TRectGraphic }

  TRectGraphic = class(TKFGraphic)
  private
    FRectGeom: TRectangleF;
  public
    function Bounds: TRectangleF; override;
    constructor Create(X1, Y1, X2, Y2: single);
    constructor Create(P1, P2: TPointF);
    function Distance(APoint: TPointF): single; override;
    procedure SetCorner(APoint: TPointF);
    procedure ValidateGeometry;
    procedure DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer; AFeedback: boolean); override;

  end;


implementation

uses
  Math, UGraphicTools, UGraphicEnvirons;

{ TRectGraphic }

function TRectGraphic.Bounds: TRectangleF;
begin
  Result := FRectGeom;
end;

constructor TRectGraphic.Create(X1, Y1, X2, Y2: single);
begin
  FRectGeom := RectangleF(X1, Y1, X2, Y2);
end;

constructor TRectGraphic.Create(P1, P2: TPointF);
begin
  FRectGeom := RectangleF(P1, P2);
end;

function TRectGraphic.Distance(APoint: TPointF): single;
var
  dist: single;
  p: TPointF;
begin
  dist := MaxSingle;
  for p in FRectGeom.CornerPoints do
    dist := Min(dist, p.Distance(APoint));
  Result := dist;
end;

procedure TRectGraphic.SetCorner(APoint: TPointF);
begin
  FRectGeom.Corner := APoint;
  // FRectGeom := RectangleF(FRectGeom.Origin, FRectGeom.Corner);
end;

procedure TRectGraphic.ValidateGeometry;
begin
  FRectGeom := RectangleF(FRectGeom.Origin, FRectGeom.Corner);
end;

procedure TRectGraphic.DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer; AFeedback: boolean);
var
  savedColor: TColor;
  p: TPointF;
begin
  if Selected then
  begin
    savedColor := ACanvas.Brush.Color;
    ACanvas.Brush.Color := GraphicEnvirons.SelectedHandleColor.Value;
    for p in FRectGeom.CornerPoints do
      ADrawer.FillHandle(ACanvas, p);
    ACanvas.Brush.Color := savedColor;
  end;
  with ACanvas.Pen do
  begin
    Style := psSolid;
    Color := GraphicEnvirons.PointColor.Value;
    Width := 1;
  end;
  if AFeedback then
    ACanvas.Pen.Color := clWhite;
  ADrawer.FrameBoundsOn(ACanvas, FRectGeom);
end;

{ TKFGraphic }

function TKFGraphic.AsList: TGraphicList;
begin
  Result := TGraphicList.Create(False);
  Result.Add(Self);
end;

{ TGraphicDrawer }

procedure TGraphicDrawer.DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList;
  AFeedback: boolean);
var
  g: TKFGraphic;
begin
  for g in AGraphicList do
    g.DrawOn(Canvas, self, AFeedback);
end;

procedure TGraphicDrawer.DrawAxisLineOn(Canvas: TCanvas);
var
  pat: TPenPattern;
begin
  SetLength(pat, 2);
  pat[0] := 3;
  pat[1] := 3;
  Canvas.Pen.Style := psPattern;
  Canvas.Pen.Color := GraphicEnvirons.AxisLineColor.Value;
  Canvas.Pen.Width := 1;
  Canvas.Pen.SetPattern(pat);
  inherited;
end;

constructor TPointGraphic.Create(APoint: TPointF);
begin
  inherited Create;
  FPoint := APoint;
end;


constructor TPointGraphic.Create(X, Y: single);
begin
  Create(PointF(X, Y));
end;


procedure TPointGraphic.DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer; AFeedback: boolean);
const
  UNIT_SIZE = 3;
var
  savedColor: TColor;
begin
  if Selected then
  begin
    savedColor := ACanvas.Brush.Color;
    ACanvas.Brush.Color := GraphicEnvirons.SelectedHandleColor.Value;
    ADrawer.FillHandle(ACanvas, Origin);
    ACanvas.Brush.Color := savedColor;
  end;
  savedColor := ACanvas.Brush.Color;
  ACanvas.Brush.Color := GraphicEnvirons.PointColor.Value;
  ADrawer.FramePointOn(ACanvas, Origin, UNIT_SIZE);
  ACanvas.Brush.Color := savedColor;
end;


function TPointGraphic.Distance(APoint: TPointF): single;
begin
  Result := FPoint.Distance(APoint);
end;

function TPointGraphic.Bounds: TRectangleF;
begin
  Result := RectangleF(Origin, Origin);
end;


end.
