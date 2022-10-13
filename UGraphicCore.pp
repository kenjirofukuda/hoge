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
    procedure DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList);
    procedure DrawAxisLineOn(Canvas: TCanvas); override;
  end;

  { TKFGraphic }

  TKFGraphic = class
    abstract
  private
    FSelected: boolean;
  public
    procedure DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer); virtual; abstract;
    function Distance(APoint: TPointF): single; virtual; abstract;
    function AsList: TGraphicList;
    property Selected: boolean read FSelected write FSelected;
  end;

  { TKFGraphic }

  TPointGraphic = class(TKFGraphic)
    constructor Create(APoint: TPointF);
    constructor Create(X, Y: single);

  private
    FPoint: TPointF;

  public
    procedure DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer); override;
    function Distance(APoint: TPointF): single; override;

    property x: single read FPoint.x write FPoint.x;
    property y: single read FPoint.y write FPoint.y;
    property Origin: TPointF read FPoint;
  end;

implementation

uses
  UGraphicTools,UGraphicEnvirons;

{ TKFGraphic }

function TKFGraphic.AsList: TGraphicList;
begin
  Result := TGraphicList.Create(False);
  Result.Add(Self);
end;

{ TGraphicDrawer }

procedure TGraphicDrawer.DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList);
var
  g: TKFGraphic;
begin
  for g in AGraphicList do
    g.DrawOn(Canvas, self);
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


procedure TPointGraphic.DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer);
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


end.
