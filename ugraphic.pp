unit UGraphic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics,
  UGeometyUtils, UGraphicBase;

type
  TPointGraphic = class(TGraphic)
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
  UNIT_SIZE = 2;
var
  savedColor: TColor;
begin
  if Selected then
  begin
    savedColor := ACanvas.Pen.Color;
    ACanvas.Pen.Color := clRed;
  end;
  ADrawer.FramePointOn(ACanvas, Origin, UNIT_SIZE);
  if Selected then
     ACanvas.Pen.Color := savedColor;
end;


function TPointGraphic.Distance(APoint: TPointF): single;
begin
  Result := FPoint.Distance(APoint);
end;


end.
