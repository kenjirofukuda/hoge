unit UGraphicView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl, UGeometryUtils,
  Controls, Menus, ExtCtrls,
  Dialogs, LCLIntf, UWorldView, UGraphicDocument, UGraphicCore;

type

  { TGraphicView }

  TGraphicView = class(TWorldView)
  private
    FDocument: TGraphicDocument;
    FFeedbacks: TGraphicList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure HandlePaint(Sender: TObject); override;
    procedure UIColorChanged(Sender: TObject);

    procedure AddFeedback(AGraphic: TKFGraphic);
    procedure ClearFeedback;
    function GetFeedBack1: TKFGraphic;
    property Document: TGraphicDocument read FDocument write FDocument;

  private
    procedure InstallTools;
    procedure RegisterUIColorHandlers;


  end;


implementation

uses
  UGraphicTools, UGraphicEnvirons;

{ TGraphicView }

constructor TGraphicView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFeedbacks := TGraphicList.Create(False);
  FViewTracking := TPointTool.Create(self);
  InstallTools;
  RegisterUIColorHandlers;
  ChooseTool('select');
end;

destructor TGraphicView.Destroy;
begin
  FreeAndNil(FFeedbacks);
  inherited Destroy;
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
var
  GraphicDrawer: TGraphicDrawer;
begin

  Canvas.Brush.Color := GraphicEnvirons.BackgroundColor.Value;
  Canvas.Clear;
  if not Assigned(FWorldDrawer) then
    exit;
  GraphicDrawer := (FWorldDrawer as TGraphicDrawer);
  if ShowAxisLine then
  begin
    FWorldDrawer.DrawAxisLineOn(Canvas);
  end;
  Canvas.Pen.Color := clBlack;
  GraphicDrawer
    .DrawOn(Canvas, Document.GetGraphics, False);
  if ShowExtentBounds then
  begin
    Canvas.Pen.Color := GraphicEnvirons.ExtentBoundsColor.Value;
    Canvas.Pen.Style := psDash;
    GraphicDrawer.FrameBoundsOn(Canvas, Document.Bounds);
  end;
  GraphicDrawer.DrawOn(Canvas, FFeedbacks, True);
end;


procedure TGraphicView.UIColorChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TGraphicView.AddFeedback(AGraphic: TKFGraphic);
begin
  FFeedbacks.Add(AGraphic);
end;

procedure TGraphicView.ClearFeedback;
begin
  FFeedbacks.Clear;
  Invalidate;
end;

function TGraphicView.GetFeedBack1: TKFGraphic;
begin
  Result := nil;
  if FFeedbacks.Count > 0 then
    Result := FFeedbacks[0];
end;


end.
