unit UGraphic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, fgl,
  UGeometyUtils;

type
  TGraphic = class
  end;

  TPointGraphic = class(TGraphic)
    constructor Create(APoint: TPointF);
    constructor Create(X, Y: single);
  private
    FPoint: TPointF;
  public
    property x: single read FPoint.x write FPoint.x;
    property y: single read FPoint.y write FPoint.y;
    property Origin: TPointF read FPoint;
  end;

  TGraphicList = specialize TFPGObjectList<TGraphic>;

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

end.
