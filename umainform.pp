unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Menus, ExtCtrls, ComCtrls,
  Dialogs,
  UDocument, UPointsDrawer, Types;

type
  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    PointsView: TPaintBox;
    StatusBar: TStatusBar;

    EditMenu: TMenuItem;
    ClearAllMenuItem: TMenuItem;
    RevealAppConfigDirMenuItem: TMenuItem;
    DebugMenu: TMenuItem;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure PointsViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure PointsViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PointsViewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure PointsViewPaint(Sender: TObject);
    procedure PointsViewResize(Sender: TObject);

    procedure ClearAllMenuItemClick(Sender: TObject);
    procedure RevealAppConfigDirMenuItemClick(Sender: TObject);
  private
    FDocument: TDocument;
    FPointsDrawer: TPointsDrawer;
    procedure DocumentChange(Sender: TObject);
    procedure UpdateMouseStatus(H, V: integer);
  public
    { public declarations }
  end;


var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDocument := TDocument.Create;
  FDocument.LoadFromDefault;
  FPointsDrawer := TPointsDrawer.Create(FDocument);
  FDocument.OnChange := @DocumentChange;
end;


procedure TMainForm.ClearAllMenuItemClick(Sender: TObject);
begin
  FDocument.RemoveAllPoints();
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDocument.SaveToDefault;
  FreeAndNil(FPointsDrawer);
  FreeAndNil(FDocument);
end;


procedure TMainForm.PointsViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  UpdateMouseStatus(X, Y);
end;


procedure TMainForm.PointsViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  xyPoint: TPointF;
begin
  xyPoint := FPointsDrawer.Viewport.DeviceToWorld(X, Y);
  FDocument.AddPoint(xyPoint.x, xyPoint.y);
end;


procedure TMainForm.PointsViewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  direction: single;
begin
  if WheelDelta < 0 then
    direction := -1.0
  else
    direction := 1.0;
  FPointsDrawer.Viewport.WheelZoom(MousePos.x, MousePos.y, direction);
  PointsView.Invalidate;
end;


procedure TMainForm.PointsViewPaint(Sender: TObject);
begin
  FPointsDrawer.DrawOn(PointsView.Canvas);
end;


procedure TMainForm.PointsViewResize(Sender: TObject);
begin
  StatusBar.SimpleText := Format('W: %4d, H: %4d',
    [PointsView.ClientWidth, PointsView.ClientHeight]);
  FPointsDrawer.Viewport.SetPortSize(PointsView.ClientWidth, PointsView.ClientHeight);
end;


procedure TMainForm.RevealAppConfigDirMenuItemClick(Sender: TObject);
begin
  ShowMessage(UDocument.AppConfigDir);
end;


procedure TMainForm.UpdateMouseStatus(H, V: integer);
var
  pt: TPointF;
  strs: TStringList;
begin
  pt := FPointsDrawer.Viewport.DeviceToWorld(H, V);
  strs := TStringList.Create;
  strs.Delimiter := ' ';
  strs.QuoteChar := ' ';
  strs.Add(Format('H: %4d, V: %4d', [H, V]));
  strs.Add(Format('X: %10.4f, Y: %10.4f', [pt.x, pt.y]));
  strs.Add(Format('W: %4d, H: %4d', [PointsView.ClientWidth, PointsView.ClientHeight]));
  StatusBar.SimpleText := strs.DelimitedText;
  strs.Free;
end;


procedure TMainForm.DocumentChange(Sender: TObject);
begin
  PointsView.Invalidate;
end;

end.
