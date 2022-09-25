unit UGraphicBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl,
  UViewport, UGeometryUtils;

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
  //TColorSlots = specialize

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

  TKFGraphic = class;

  TGraphicList = specialize TFPGObjectList<TKFGraphic>;

  TGraphicDrawer = class
    abstract
    constructor Create; virtual;

  private
    FViewport: TViewport;

  public
    destructor Destroy; override;
    procedure DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList); virtual; abstract;
    procedure DrawAxisLineOn(Canvas: TCanvas); virtual; abstract;
    procedure FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF; AUnitSize: integer);
      virtual; abstract;
    procedure FillPointOn(Canvas: TCanvas; AWorldPoint: TPointF; AUnitSize: integer);
      virtual; abstract;
    procedure FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF);
      virtual; abstract;
    procedure FillHandle(Canvas: TCanvas; At: TPointF); virtual; abstract;

    property Viewport: TViewport read FViewport;
  end;

  TKFGraphic = class
    abstract
  private
    FSelected: boolean;
  public
    procedure DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer); virtual; abstract;
    function Distance(APoint: TPointF): single; virtual; abstract;
    function AsList: TGraphicList;
    property Selected: boolean read FSelected write FSelected;
  end;

var
  GraphicEnvirons: TGraphicEnvirons;

implementation

constructor TGraphicEnvirons.Create;
var
  key: string;
  AColorSlot: TColorSlot;
  keys: array of string = ('Background', 'ExtentBounds', 'AxisLine', 'Point', 'SelectedHandle');
begin
  //FBackgroundColor := TColorSlot.Create;
  //FExtentBoundsColor := TColorSlot.Create;
  //FAxisLineColor := TColorSlot.Create;
  //FPointColor := TColorSlot.Create;
  //FSelectedHandleColor := TColorSlot.Create;
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

  //FreeAndNil(FSelectedHandleColor);
  //FreeAndNil(FPointColor);
  //FreeAndNil(FAxisLineColor);
  //FreeAndNil(FExtentBoundsColor);
  //FreeAndNil(FBackgroundColor);
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


constructor TGraphicDrawer.Create;
begin
  FViewport := TViewport.Create;
  FViewport.ResetWorld;
  FViewport.ResetPortCenter;
end;


destructor TGraphicDrawer.Destroy;
begin
  FreeAndNil(FViewport);
  inherited;
end;


function TKFGraphic.AsList: TGraphicList;
begin
  Result := TGraphicList.Create(False);
  Result.Add(Self);
end;

initialization
  GraphicEnvirons := TGraphicEnvirons.Create;
  with GraphicEnvirons do
  begin
    BackgroundColor.Value := clNavy;
    ExtentBoundsColor.Value := clLtGray;
    AxisLineColor.Value := clWhite;
    PointColor.Value := clRed;
    ColorSlotMap.KeyData['Background'].Value := clNavy;
    ColorSlotMap.KeyData['ExtentBounds'].Value := clLtGray;
    ColorSlotMap.KeyData['AxisLine'].Value := clWhite;
    ColorSlotMap.KeyData['SelectedHandle'].Value := clWhite;
    ColorSlotMap.KeyData['Point'].Value := clRed;
  end;

finalization;
  FreeAndNil(GraphicEnvirons);
end.
