unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Menus, ExtCtrls, ComCtrls,
  Dialogs,
  UDocument, UPointsDrawer;

type
  { TMainForm }

  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    EditMenu: TMenuItem;
    ClearAllMenuItem: TMenuItem;
    PointsView: TPaintBox;
    StatusBar: TStatusBar;
    procedure ClearAllMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PointsViewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: integer);
    procedure PointsViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PointsViewPaint(Sender: TObject);
    procedure PointsViewResize(Sender: TObject);
  private
    FDocument: TDocument;
    FPointsDrawer: TPointsDrawer;
    { private declarations }
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
end;


procedure TMainForm.ClearAllMenuItemClick(Sender: TObject);
begin
  FDocument.RemoveAllPoints();
  PointsView.Invalidate;
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
  StatusBar.SimpleText := Format('H: %4d, V: %4d', [X, Y]);
end;


procedure TMainForm.PointsViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FDocument.AddPoint(X, Y);
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
end;

end.
