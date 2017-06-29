unit UPointsDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Controls, Graphics, ExtCtrls,
  UDocument, UViewport;

type
  TPointsDrawer = class
    constructor Create(ADocument: TDocument);
  private
    FDocument: TDocument;
    FViewport: TViewport;
  public
    destructor Destroy; override;
    procedure DrawOn(Canvas: TCanvas);
    property Viewport: TViewport read FViewport;
  end;

implementation


constructor TPointsDrawer.Create(ADocument: TDocument);
begin
  FDocument := ADocument;
  FViewport := TViewport.Create;
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
  hvPoint: TPointF;
begin
  for xyPoint in FDocument.GetPoints do
  begin
    hvPoint := FViewport.WorldToDevice(xyPoint.x, xyPoint.y);
    Canvas.Ellipse(trunc(hvPoint.x - UNIT_SIZE), trunc(hvPoint.y - UNIT_SIZE),
      trunc(hvPoint.x + UNIT_SIZE), trunc(hvPoint.y + UNIT_SIZE));
  end;
end;


end.
