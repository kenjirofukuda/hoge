unit UGraphicView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl, UGeometryUtils,
  Controls, Menus, ExtCtrls,
  Dialogs, LCLIntf, UWorldView, UGraphicDocument;

type

  TGraphicView = class(TWorldView)
  private
    FDocument: TGraphicDocument;

  public
    constructor Create(AOwner: TComponent); override;

    procedure HandlePaint(Sender: TObject); override;
    procedure UIColorChanged(Sender: TObject);

    property Document: TGraphicDocument read FDocument write FDocument;

  private
    procedure InstallTools;
    procedure RegisterUIColorHandlers;


  end;


implementation

uses
  UGraphicTools, UGraphicEnvirons, UGraphicCore;

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
  FToolMap.Add('rect', TRectTool.Create(self));
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
  .DrawOn(Canvas, Document.GetGraphics);
  if ShowExtentBounds then
  begin
    Canvas.Pen.Color := GraphicEnvirons.ExtentBoundsColor.Value;
    Canvas.Pen.Style := psDash;
    FWorldDrawer.FrameBoundsOn(Canvas, Document.Bounds);
  end;
end;


procedure TGraphicView.UIColorChanged(Sender: TObject);
begin
  Invalidate;
end;


end.
