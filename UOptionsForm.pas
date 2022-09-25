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
      Parent := ColorGroupBox;
      Name := key + 'Label';
      Caption := key;
    end;
    AColorBox := TColorBox.Create(ColorGroupBox);
    with AColorBox do
    begin
      Parent := ColorGroupBox;
      Name := key;
      Style := [cbStandardColors, cbExtendedColors, cbCustomColor,
        cbCustomColors, cbPrettyNames];
      OnChange := @ColorBoxChange;
      Selected := GraphicEnvirons.ColorSlotMap.KeyData[key].Value;
    end;
  end;
  ColorGroupBox.AutoSize := True;
end;


procedure TOptionsForm.ColorBoxChange(Sender: TObject);
var
  AColorBox: TColorBox;
  AKey: string;
begin
  AColorBox := Sender as TColorBox;
  AKey := AColorBox.Name;
  GraphicEnvirons.ColorSlotMap.KeyData[AKey].Value := AColorBox.Selected;
end;


end.
