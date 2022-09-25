unit UOptionsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ColorBox,
  Grids, UGraphicBase;

type


  { TOptionsForm }

  TOptionsForm = class(TForm)
    SelectedHandleColorBox: TColorBox;
    PointColorLabel: TLabel;
    AxisLineColorLabel: TLabel;
    BackgroundColorLabel: TLabel;
    ExtentBoundsColorLabel: TLabel;

    ExtentBoundsColorBox: TColorBox;
    BackgroundColorBox: TColorBox;
    AxisLineColorBox: TColorBox;
    PointColorBox: TColorBox;

    CloseButton: TButton;
    SelectedHandleColor: TLabel;

    procedure FormCreate(Sender: TObject);

    procedure CloseButtonClick(Sender: TObject);

    procedure AxisLineColorBoxChange(Sender: TObject);
    procedure BackgroundColorBoxChange(Sender: TObject);
    procedure ExtentBoundsColorBoxChange(Sender: TObject);
    procedure PointColorBoxChange(Sender: TObject);
    procedure SelectedHandleColorBoxChange(Sender: TObject);
  private

  public

  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

{ TOptionsForm }

procedure TOptionsForm.CloseButtonClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TOptionsForm.ExtentBoundsColorBoxChange(Sender: TObject);
begin
  GraphicEnvirons.ExtentBoundsColor.Value := ExtentBoundsColorBox.Selected;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  BackgroundColorBox.Selected := GraphicEnvirons.BackgroundColor.Value;
  ExtentBoundsColorBox.Selected := GraphicEnvirons.ExtentBoundsColor.Value;
  AxisLineColorBox.Selected := GraphicEnvirons.AxisLineColor.Value;
  PointColorBox.Selected := GraphicEnvirons.PointColor.Value;
  SelectedHandleColorBox.Selected := GraphicEnvirons.SelectedHandleColor.Value;
end;

procedure TOptionsForm.PointColorBoxChange(Sender: TObject);
begin
  GraphicEnvirons.PointColor.Value := PointColorBox.Selected;
end;

procedure TOptionsForm.SelectedHandleColorBoxChange(Sender: TObject);
begin
  GraphicEnvirons.SelectedHandleColor.Value := SelectedHandleColorBox.Selected;
end;

procedure TOptionsForm.BackgroundColorBoxChange(Sender: TObject);
begin
  GraphicEnvirons.BackgroundColor.Value := BackgroundColorBox.Selected;
end;

procedure TOptionsForm.AxisLineColorBoxChange(Sender: TObject);
begin
  GraphicEnvirons.AxisLineColor.Value := AxisLineColorBox.Selected;
end;

end.
