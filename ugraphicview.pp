unit UGraphicView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, UGeometyUtils, UDocument,
  Controls, Menus, ExtCtrls,
  Dialogs, LCLIntf, UGraphicDrawer;

type
  TTrackingAttributes = record
    MiddleDown: boolean;
    CurrPoint: TPoint;
    DownPoint: TPoint;
    MovePoints: THVPointList;
  end;

  TGraphicView = class(TPaintBox)
    constructor Create(AOwner: TComponent); override;

    procedure HandleMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

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
    FTrackingAttributes: TTrackingAttributes;
    FFirstResizeHandled: boolean;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure HandleMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure HandleMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure HandlePaint(Sender: TObject);
    procedure HandleResize(Sender: TObject);

  public
    destructor Destroy; override;
    property Document: TDocument read FDocument write FDocument;
    property GraphicDrawer: TGraphicDrawer read FGraphicDrawer write FGraphicDrawer;
  end;

implementation


constructor TGraphicView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;


destructor TGraphicView.Destroy;
begin
  FreeAndNil(FTrackingAttributes.MovePoints);
  inherited Destroy;
end;


procedure TGraphicView.HandleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if Button = mbMiddle then
  begin
    with FTrackingAttributes do
    begin
      MiddleDown := True;
      DownPoint := Point(X, Y);
      if MovePoints = nil then
        MovePoints := THVPointList.Create;
      MovePoints.Clear;
    end;
  end;
end;


procedure TGraphicView.HandleMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
var
  p1, p2: TPoint;
  wp1, wp2, moved: TPointF;
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  with FTrackingAttributes do
  begin
    if not MiddleDown then
      exit;
    if DownPoint = Point(X, Y) then
      exit;
    CurrPoint := Point(X, Y);
    MovePoints.Add(CurrPoint);
    if MovePoints.Count > 2 then
    begin
      p1 := MovePoints.Items[MovePoints.Count - 2];
      p2 := MovePoints.Items[MovePoints.Count - 1];
      with FGraphicDrawer.Viewport do
      begin
        wp1 := DeviceToWorld(p1.x, p1.y);
        wp2 := DeviceToWorld(p2.x, p2.y);
      end;
      moved := wp2 - wp1;
      FGraphicDrawer.Viewport.OffSetWorldCenter(-moved.x, -moved.y);
      Invalidate;
    end;
  end;
end;


procedure TGraphicView.HandleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  xyPoint: TPointF;
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if not Assigned(FDocument) then
    exit;
  if Button = mbLeft then
  begin
    xyPoint := FGraphicDrawer.Viewport.DeviceToWorld(X, Y);
    FDocument.AddPoint(xyPoint.x, xyPoint.y);
  end;
  FTrackingAttributes.MiddleDown := False;
end;


procedure TGraphicView.HandleMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  direction: single;
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  if WheelDelta < 0 then
    direction := -1.0
  else
    direction := 1.0;
  FGraphicDrawer.Viewport.WheelZoom(MousePos.x, MousePos.y, direction);
  Invalidate;
end;


procedure TGraphicView.HandlePaint(Sender: TObject);
begin
  if not Assigned(FGraphicDrawer) then
    exit;
  FGraphicDrawer.DrawOn(Canvas);
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

end.
