unit UGeometyUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Math, fgl;

type

  { Smalltalk style }
  TRectangleF = object
    Origin: TPointF;
    Corner: TPointF;
  private
    function GetWidth: single;
    function GetHeight: single;
    function GetExtent: TPointF;
    function GetCenter: TPointF;
  public
    function Merge(ABounds: TRectangleF): TRectangleF;
    property Width: single read GetWidth;
    property Height: single read GetHeight;
    property Extent: TPointF read GetExtent;
  end;

  TXYPointList = specialize TFPGList<TPointF>;
  THVPointList = specialize TFPGList<TPoint>;


function MidValue(V1, V2: single): single; inline;

function RectangleF(X1, Y1, X2, Y2: single): TRectangleF;
function RectangleF(P1, P2: TPointF): TRectangleF;
function EmptyRectangleF: TRectangleF;

implementation


function TRectangleF.GetWidth: single;
begin
  Result := Corner.x - Origin.x;
end;


function TRectangleF.GetHeight: single;
begin
  Result := Corner.y - Origin.y;
end;


function TRectangleF.GetExtent: TPointF;
begin
  Result := Corner - Origin;
end;


function TRectangleF.GetCenter: TPointF;
begin
  Result := Corner - Origin;
end;


function RectangleF(X1, Y1, X2, Y2: single): TRectangleF;
begin
  Result.Origin.x := Min(X1, X2);
  Result.Origin.y := Min(Y1, Y2);
  Result.Corner.x := Max(X1, X2);
  Result.Corner.y := Max(Y1, Y2);
end;


function RectangleF(P1, P2: TPointF): TRectangleF;
begin
  Result := RectangleF(P1.x, P1.y, P2.x, P2.y);
end;


function TRectangleF.Merge(ABounds: TRectangleF): TRectangleF;
var
  minX, minY, maxX, maxY: single;
begin
  minX := Min(Origin.x, ABounds.Origin.x);
  minY := Min(Origin.y, ABounds.Origin.y);
  maxX := Max(Corner.x, ABounds.Corner.x);
  maxY := Max(Corner.y, ABounds.Corner.y);
  Result := RectangleF(minX, minY, maxX, maxY);
end;


function MidValue(V1, V2: single): single;
begin
  Result := (Max(V2, V1) - Min(V2, V1)) * 0.5;
end;


function EmptyRectangleF: TRectangleF;
begin
  Result.Origin.x := single.MaxValue;
  Result.Origin.y := single.MaxValue;
  Result.Corner.x := single.MinValue;
  Result.Corner.y := single.MinValue;
end;

end.
