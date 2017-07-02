unit UGraphicBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Graphics, fgl,
  UViewport, UGeometyUtils;

type
  TGraphic = class;

  TGraphicList = specialize TFPGObjectList<TGraphic>;

  TGraphicDrawer = class abstract
    constructor Create; virtual;

  private
    FViewport: TViewport;

  public
    destructor Destroy; override;
    procedure DrawOn(Canvas: TCanvas; AGraphicList: TGraphicList); virtual; abstract;
    procedure DrawAxisLineOn(Canvas: TCanvas); virtual; abstract;
    procedure FramePointOn(Canvas: TCanvas; AWorldPoint: TPointF; AUnitSize: integer);
      virtual; abstract;
    procedure FrameBoundsOn(Canvas: TCanvas; AWorldBounds: TRectangleF);
      virtual; abstract;
    property Viewport: TViewport read FViewport;
  end;

  TGraphic = class abstract
    procedure DrawOn(ACanvas: TCanvas; ADrawer: TGraphicDrawer); virtual; abstract;
  end;


implementation


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



end.
