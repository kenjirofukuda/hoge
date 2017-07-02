unit UGraphicBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl,
  UViewport, UGeometyUtils;

type
  TGraphic = class;

  TGraphicList = specialize TFPGObjectList<TGraphic>;

  TGraphicDrawer = class
    constructor Create; virtual;

  private
    FShowAxisLine: boolean;
    FViewport: TViewport;

  public
    destructor Destroy; override;
    procedure DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList); virtual; abstract;
    //  procedure VLine(Canvas: TCanvas; AXValue: single);
    //  procedure HLine(Canvas: TCanvas; AYValue: single);
    procedure FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF; AUnitSize: integer);
      virtual; abstract;
    procedure FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF);
      virtual; abstract;
    property Viewport: TViewport read FViewport;
    property ShowAxisLine: boolean read FShowAxisLine write FShowAxisLine;
  end;

  TGraphic = class
    procedure DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer); virtual; abstract;
  end;


implementation


constructor TGraphicDrawer.Create;
begin
  FViewport := TViewport.Create;
  FShowAxisLine := True;
  FViewport.ResetWorld;
  FViewport.ResetPortCenter;
end;


destructor TGraphicDrawer.Destroy;
begin
  FreeAndNil(FViewport);
  inherited;
end;



end.
