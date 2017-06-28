unit UPointsDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls,
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
  point: TPoint;
begin
  for point in FDocument.GetPoints do
  begin
    Canvas.Ellipse(point.x - UNIT_SIZE, point.y - UNIT_SIZE,
                   point.x + UNIT_SIZE, point.y + UNIT_SIZE);
  end;
end;


end.
