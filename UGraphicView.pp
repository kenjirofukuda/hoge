unit UGraphicView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl, UGeometryUtils,
  Controls, Menus, ExtCtrls,
  Dialogs, LCLIntf, UWorldView;

type

  TGraphicView = class(TWorldView)
    constructor Create(AOwner: TComponent); override;

  public
    procedure HandlePaint(Sender: TObject); override;
    procedure UIColorChanged(Sender: TObject);

  private
    procedure InstallTools;
    procedure RegisterUIColorHandlers;

  end;


implementation

uses
  UGraphicTools, UGraphicDocument, UGraphicEnvirons, UGraphicCore;

{ TGraphicView }

constructor TGraphicView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViewTracking := TPointTool.Create(self);
  InstallTools;
  RegisterUIColorHandlers;
  ChooseTool('select');
end;

procedure TGraphicView.InstallTools;
begin
  FToolMap.Add('select', TSelectTool.Create(self));
  FToolMap.Add('point', TPointTool.Create(self));
end;


procedure TGraphicView.RegisterUIColorHandlers;
var
  key: string;
begin
  for key in GraphicEnvirons.ColorSlotNames do
  begin
    GraphicEnvirons.ColorSlotMap[key].OnChange := @UIColorChanged;
  end;
end;


procedure TGraphicView.HandlePaint(Sender: TObject);
begin
  Canvas.Brush.Color := GraphicEnvirons.BackgroundColor.Value;
  Canvas.Clear;
  if not Assigned(FWorldDrawer) then
    exit;
  if ShowAxisLine then
  begin
    FWorldDrawer.DrawAxisLineOn(Canvas);
  end;
  Canvas.Pen.Color := clBlack;
  (FWorldDrawer as TGraphicDrawer)
  .DrawOn(Canvas, (FDocument as TGraphicDocument).GetGraphics);
  if ShowExtentBounds then
  begin
    Canvas.Pen.Color := GraphicEnvirons.ExtentBoundsColor.Value;
    Canvas.Pen.Style := psDash;
    FWorldDrawer.FrameBoundsOn(Canvas, (FDocument as TGraphicDocument).Bounds);
  end;
end;


procedure TGraphicView.UIColorChanged(Sender: TObject);
begin
  Invalidate;
end;

//constructor TPointGraphic.Create(APoint: TPointF);
//begin
//  inherited Create;
//  FPoint := APoint;
//end;
//
//
//constructor TPointGraphic.Create(X, Y: single);
//begin
//  Create(PointF(X, Y));
//end;
//
//
//procedure TPointGraphic.DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer);
//const
//  UNIT_SIZE = 3;
//var
//  savedColor: TColor;
//begin
//  if Selected then
//  begin
//    savedColor := ACanvas.Brush.Color;
//    ACanvas.Brush.Color := GraphicEnvirons.SelectedHandleColor.Value;
//    ADrawer.FillHandle(ACanvas, Origin);
//    ACanvas.Brush.Color := savedColor;
//  end;
//  savedColor := ACanvas.Brush.Color;
//  ACanvas.Brush.Color := GraphicEnvirons.PointColor.Value;
//  ADrawer.FramePointOn(ACanvas, Origin, UNIT_SIZE);
//  ACanvas.Brush.Color := savedColor;
//end;
//
//
//function TPointGraphic.Distance(APoint: TPointF): single;
//begin
//  Result := FPoint.Distance(APoint);
//end;


end.
