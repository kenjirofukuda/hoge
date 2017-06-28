unit UDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LazFileUtils, fgl;

type
  TPoints = specialize TFPGList<TPoint>;

  TDocument = class
    constructor Create;
    destructor Destroy; override;

  private
    FPoints: TPoints;
  public
    procedure AddPoint(X, Y: integer);
    function GetPoints: TPoints;
    procedure SaveToFile(APath: String);
    procedure SaveToDefault;

  end;

  function AppConfigDir:string;

implementation


constructor TDocument.Create;
begin
  inherited;
  FPoints := TPoints.Create;
end;


destructor TDocument.Destroy;
begin
  FreeAndNil(FPoints);
end;


procedure TDocument.AddPoint(X, Y: integer);
begin
  FPoints.Add(Point(X, Y));
end;


function TDocument.GetPoints: TPoints;
begin
  Result := FPoints;
end;


procedure TDocument.SaveToFile(APath: String);
var
  list: TStringList;
  point: TPoint;
begin
  list := TStringList.Create;
  try
     for point in FPoints do
     begin
       list.Add(Format('%d, %d', [point.x, point.y]));
     end;
     list.LineBreak := #10;
     list.SaveToFile(APath);
  finally
    list.Free;
  end;
end;

procedure TDocument.SaveToDefault;
begin
  SaveToFile(ConcatPaths([AppConfigDir, 'hoge_data.txt']));
end;

function AppConfigDir:string;
begin
  Result := GetAppConfigDirUTF8(False, True);
end;

end.
