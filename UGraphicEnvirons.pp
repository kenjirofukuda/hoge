unit UGraphicEnvirons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl;

type

  generic TValueSlot<T> = class
    FValue: T;
    FName: string;
    FOnChange: TNotifyEvent;
    procedure SetValue(AValue: T);
  protected
    procedure Change;
  public
    property Value: T read FValue write SetValue;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Name: string read FName write FName;
  end;

  TColorSlot = specialize TValueSlot<TColor>;
  TColorSlotMap = specialize TFPGMap<string, TColorSlot>;
  TSlotNames = specialize TFPGList<string>;

  TGraphicEnvirons = class
  private
    FColorSlotMap: TColorSlotMap;
    FSColorSlotNames: TSlotNames;
    FBackgroundColor: TColorSlot;
    FExtentBoundsColor: TColorSlot;
    FAxisLineColor: TColorSlot;
    FPointColor: TColorSlot;
    FSelectedHandleColor: TColorSlot;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property BackgroundColor: TColorSlot read FBackgroundColor;
    property ExtentBoundsColor: TColorSlot read FExtentBoundsColor;
    property AxisLineColor: TColorSlot read FAxisLineColor;
    property PointColor: TColorSlot read FPointColor;
    property SelectedHandleColor: TColorSlot read FSelectedHandleColor;

    property ColorSlotNames: TSlotNames read FSColorSlotNames;
    property ColorSlotMap: TColorSlotMap read FColorSlotMap;
  end;


var
  GraphicEnvirons: TGraphicEnvirons;

implementation

constructor TGraphicEnvirons.Create;
var
  key: string;
  AColorSlot: TColorSlot;
  keys: array of string = ('Background', 'ExtentBounds', 'AxisLine',
    'Point', 'SelectedHandle');
begin
  FColorSlotMap := TColorSlotMap.Create;
  FSColorSlotNames := TSlotNames.Create;
  for key in keys do
  begin
    FSColorSlotNames.Add(key);
    AColorSlot := TColorSlot.Create;
    AColorSlot.Name := key;
    FColorSlotMap.Add(key, AColorSlot);
  end;
  FBackgroundColor := FColorSlotMap.KeyData['Background'];
  FExtentBoundsColor := FColorSlotMap.KeyData['ExtentBounds'];
  FAxisLineColor := FColorSlotMap.KeyData['AxisLine'];
  FPointColor := FColorSlotMap.KeyData['Point'];
  FSelectedHandleColor := FColorSlotMap.KeyData['SelectedHandle'];
end;

destructor TGraphicEnvirons.Destroy;
begin
  FreeAndNil(FColorSlotMap);
  FreeAndNil(FSColorSlotNames);
  inherited;
end;


procedure TValueSlot.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TValueSlot.SetValue(AValue: T);
begin
  if FValue = AValue then
    exit;
  FValue := AValue;
  Change;
end;


initialization
  GraphicEnvirons := TGraphicEnvirons.Create;
  with GraphicEnvirons do
  begin
    ColorSlotMap.KeyData['Background'].Value := clNavy;
    ColorSlotMap.KeyData['ExtentBounds'].Value := clLtGray;
    ColorSlotMap.KeyData['AxisLine'].Value := clWhite;
    ColorSlotMap.KeyData['SelectedHandle'].Value := clWhite;
    ColorSlotMap.KeyData['Point'].Value := clRed;
  end;

finalization;
  FreeAndNil(GraphicEnvirons);
end.
