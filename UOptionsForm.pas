unit UOptionsForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ColorBox,
  Grids, UGraphicBase;

type
  { TOptionsForm }
  TOptionsForm = class(TForm)
    ColorGroupBox: TGroupBox;
    CloseButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure ColorBoxChange(Sender: TObject);
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


procedure TOptionsForm.FormCreate(Sender: TObject);
var
  key: string;
  AColorBox: TColorBox;
  ALabel: TLabel;
begin
  ColorGroupBox.AutoSize := False;
  for key in GraphicEnvirons.ColorSlotNames do
  begin
    ALabel := TLabel.Create(ColorGroupBox);
    with ALabel do
    begin
      Name := key + 'Label';
      Caption := key;
      Parent := ColorGroupBox;
    end;
    AColorBox := TColorBox.Create(ColorGroupBox);
    with AColorBox do
    begin
      Name := key;
      Style := [cbStandardColors, cbExtendedColors, cbCustomColor,
        cbCustomColors, cbPrettyNames];
      OnChange := @ColorBoxChange;
      Selected := GraphicEnvirons.ColorSlotMap.KeyData[key].Value;
      Parent := ColorGroupBox;
    end;
  end;
  ColorGroupBox.AutoSize := True;
end;


procedure TOptionsForm.ColorBoxChange(Sender: TObject);
var
  AColorBox: TColorBox;
begin
  AColorBox := Sender as TColorBox;
  GraphicEnvirons.ColorSlotMap[AColorBox.Name].Value := AColorBox.Selected;
end;


end.
