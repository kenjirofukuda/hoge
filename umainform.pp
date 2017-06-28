unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  UDocument, UPointsDrawer;

type
  { TMainForm }

  TMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
                          Shift: TShiftState; X, Y: integer);
    procedure FormPaint(Sender: TObject);
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


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FDocument.SaveToDefault;
  FreeAndNil(FPointsDrawer);
  FreeAndNil(FDocument);
end;


procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FDocument.AddPoint(X, Y);
  MainForm.Invalidate;
end;


procedure TMainForm.FormPaint(Sender: TObject);
begin
  FPointsDrawer.DrawOn(Canvas);
end;

end.
