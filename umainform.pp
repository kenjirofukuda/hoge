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
    procedure PointsViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PointsViewPaint(Sender: TObject);
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
  ShowMessage('Must be implement ClearAllMenuItemClick');
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDocument.SaveToDefault;
  FreeAndNil(FPointsDrawer);
  FreeAndNil(FDocument);
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

end.
