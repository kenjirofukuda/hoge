unit UPointsDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, Controls, Graphics, ExtCtrls,
  UDocument;

type
  TPointsDrawer = class
    constructor Create(ADocument: TDocument);
  private
    FDocument: TDocument;
  public
    procedure DrawOn(Canvas: TCanvas);
  end;

implementation


constructor TPointsDrawer.Create(ADocument: TDocument);
begin
  FDocument := ADocument;
end;


procedure TPointsDrawer.DrawOn(Canvas: TCanvas);
const
  UNIT_SIZE = 2;
var
  point: TPointF;
begin
  for point in FDocument.GetPoints do
  begin
    Canvas.Ellipse(trunc(point.x - UNIT_SIZE), trunc(point.y - UNIT_SIZE),
                   trunc(point.x + UNIT_SIZE), trunc(point.y + UNIT_SIZE));
  end;
end;


end.
