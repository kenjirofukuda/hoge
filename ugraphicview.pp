unit UGraphicView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl, UGeometyUtils, UDocument,
  Controls, Menus, ExtCtrls,
  Dialogs, LCLIntf, UGraphicBase;

type
  TViewTracking = class;
  TToolMap = specialize TFPGMap<string, TViewTracking>;


  TGraphicView = class(TPaintBox)
    constructor Create(AOwner: TComponent); override;

    procedure Paint; override;
    procedure DoOnResize; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint): boolean; override;

  private
    FDocument: TDocument;
    FGraphicDrawer: TGraphicDrawer;
    FViewTracking: TViewTracking;
    FFirstResizeHandled: boolean;
    FShowExtentBounds: boolean;
    FShowAxisLine: boolean;
    FToolMap: TToolMap;

    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure HandlePaint(Sender: TObject);
    procedure HandleResize(Sender: TObject);

    procedure InstallTools;

  public
    destructor Destroy; override;

    procedure ChooseTool(toolName: string);

    property Document: TDocument read FDocument write FDocument;
    property GraphicDrawer: TGraphicDrawer read FGraphicDrawer write FGraphicDrawer;
    property ShowExtentBounds: boolean read FShowExtentBounds write FShowExtentBounds;
    property ShowAxisLine: boolean read FShowAxisLine write FShowAxisLine;

  end;

  TViewTracking = class
  public
    constructor Create(AGraphicView: TGraphicView);
    destructor Destroy; override;

    procedure TrackBegin(Button: TMouseButton; Shift: TShiftState; X, Y: integer); virtual; abstract;
    procedure TrackMove(Shift: TShiftState; X, Y: integer); virtual; abstract;
    procedure TrackEnd(Button: TMouseButton; Shift: TShiftState; X, Y: integer); virtual; abstract;
    procedure TrackWheel(Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint; var Handled: boolean); virtual; abstract;
  protected
    FMiddleDown: boolean;
    FCurrPoint: TPoint;
    FDownPoint: TPoint;
    FMovePoints: THVPointList;
    FGraphicView: TGraphicView;
  end;


implementation

uses
  UTools;

constructor TGraphicView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowExtentBounds := False;
  FShowAxisLine := True;
  FViewTracking := TPointTool.Create(self);
  FToolMap := TToolMap.Create;
  InstallTools;
  ChooseTool('select');
end;


destructor TGraphicView.Destroy;
begin
  FViewTracking := nil;
  FreeAndNil(FToolMap);
  inherited Destroy;
end;


procedure TGraphicView.InstallTools;
begin
  FToolMap.Add('select', TSelectTool.Create(self));
  FToolMap.Add('point', TPointTool.Create(self));
end;


procedure TGraphicView.ChooseTool(toolName: string);
begin
  FViewTracking := FToolMap.KeyData[toolName];
end;


procedure TGraphicView.HandleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if Assigned(FViewTracking) then
    FViewTracking.TrackBegin(Button, Shift, X, Y);
end;


procedure TGraphicView.HandleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if Assigned(FViewTracking) then
    FViewTracking.TrackMove(Shift, X, Y);
end;


procedure TGraphicView.HandleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if not Assigned(FDocument) then
    exit;
  if Assigned(FViewTracking) then
    FViewTracking.TrackEnd(Button, Shift, X, Y);
end;


procedure TGraphicView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if Assigned(FViewTracking) then
    FViewTracking.TrackWheel(Shift, WheelDelta, MousePos, Handled);
end;


procedure TGraphicView.HandlePaint(Sender: TObject);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if ShowAxisLine then
  begin
    FGraphicDrawer.DrawAxisLineOn(Canvas);
  end;
  Canvas.Pen.Color := clBlack;
  FGraphicDrawer.DrawOn(Canvas, FDocument.GetGraphics);
  Canvas.Pen.Color := clLtGray;
  if ShowExtentBounds then
    FGraphicDrawer.FrameBoundsOn(Canvas, FDocument.Bounds);
end;


procedure TGraphicView.HandleResize(Sender: TObject);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  FGraphicDrawer.Viewport.SetPortSize(ClientWidth, ClientHeight);
  if not FFirstResizeHandled then
  begin
    FGraphicDrawer.Viewport.ResetPortCenter;
    FFirstResizeHandled := True;
  end;
end;


procedure TGraphicView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  HandleMouseDown(self, Button, Shift, X, Y);
end;


procedure TGraphicView.MouseMove(Shift: TShiftState; X, Y: integer);
begin
  inherited MouseMove(Shift, X, Y);
  HandleMouseMove(self, Shift, X, Y);
end;


procedure TGraphicView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  HandleMouseUp(self, Button, Shift, X, Y);
end;


function TGraphicView.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  HandleMouseWheel(self, Shift, WheelDelta, MousePos, Result);
end;


procedure TGraphicView.Paint;
begin
  HandlePaint(self);
  inherited Paint;
end;


procedure TGraphicView.DoOnResize;
begin
  inherited DoOnResize;
  HandleResize(Self);
end;


constructor TViewTracking.Create(AGraphicView: TGraphicView);
begin
  FGraphicView := AGraphicView;
  if not Assigned(FMovePoints) then
    FMovePoints := THVPointList.Create;
  FMovePoints.Clear;
end;


destructor TViewTracking.Destroy;
begin
  FreeAndNil(FMovePoints);
  inherited Destroy;
end;


end.
