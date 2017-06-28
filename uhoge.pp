unit UHoge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LazFileUtils, UDocument;

type
  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure FormPaint(Sender: TObject);
  private
    FDocument: TDocument;
    { private declarations }
  public
    { public declarations }
  end;



var
  Form1: TForm1;

implementation

{$R *.lfm}



{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDocument := TDocument.Create;
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
  FDocument.SaveToDefault;
  FreeAndNil(FDocument);
end;


procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  FDocument.AddPoint(X, Y);
  Form1.Invalidate;
end;


procedure TForm1.FormPaint(Sender: TObject);
const
  UNIT_SIZE = 2;
var
  point: TPoint;
begin
  for point in FDocument.GetPoints do
  begin
    Canvas.Ellipse(point.x - UNIT_SIZE, point.y - UNIT_SIZE,
                   point.x + UNIT_SIZE, point.y + UNIT_SIZE);
  end;
end;

end.
