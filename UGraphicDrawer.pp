unit UGraphicDrawer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, ExtCtrls,
  UGraphicBase;

type
  TGraphicDrawerImpl = class(TGraphicDrawer)

  public
    procedure DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList); override;
    procedure DrawAxisLineOn(Canvas: TCanvas); override;
  end;


implementation


procedure TGraphicDrawerImpl.DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList);
var
  g: TKFGraphic;
begin
  for g in AGraphicList do
    g.DrawOn(Canvas, self);
end;


procedure TGraphicDrawerImpl.DrawAxisLineOn(Canvas: TCanvas);
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


end.
