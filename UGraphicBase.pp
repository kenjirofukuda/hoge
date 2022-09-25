unit UGraphicBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl,
  UViewport, UGeometryUtils;

type

  generic TValueSlot<T> = class
    FValue: T;
    FOnChange: TNotifyEvent;
    procedure SetValue(AValue: T);
  protected
    procedure Change;
  public
    property Value: T read FValue write SetValue;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TColorSlot = specialize TValueSlot<TColor>;

  TGraphicEnvirons = class
  private
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
begin
  FBackgroundColor := TColorSlot.Create;
  FExtentBoundsColor := TColorSlot.Create;
  FAxisLineColor := TColorSlot.Create;
  FPointColor := TColorSlot.Create;
  FSelectedHandleColor := TColorSlot.Create;
end;

destructor TGraphicEnvirons.Destroy;
begin
  FreeAndNil(FSelectedHandleColor);
  FreeAndNil(FPointColor);
  FreeAndNil(FAxisLineColor);
  FreeAndNil(FExtentBoundsColor);
  FreeAndNil(FBackgroundColor);
  inherited;
end;


procedure TValueSlot.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TValueSlot.SetValue(AValue: T);
begin
  if FValue = AValue then exit;
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
  end;

finalization;
  FreeAndNil(GraphicEnvirons);
end.
