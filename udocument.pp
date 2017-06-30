unit UDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Types, LazFileUtils, LazLogger, fgl;

type
  TXYPointList = specialize TFPGList<TPointF>;

  TDocument = class
    constructor Create;
  private
    FPoints: TXYPointList;
    FOnChange: TNotifyEvent;
    procedure SaveToFile(APath: String);
    procedure LoadFromFile(APath: String);
  public
    destructor Destroy; override;
    procedure AddPoint(X, Y: single);
    procedure RemoveAllPoints();
    function GetPoints: TXYPointList;
    procedure SaveToDefault;
    procedure LoadFromDefault;
    procedure Change;
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function AppConfigDir: String;
function AppConfigFilePath: String;
function PointF(X, Y: single): TPointF;

implementation


constructor TDocument.Create;
begin
  inherited;
  FPoints := TXYPointList.Create;
end;


destructor TDocument.Destroy;
begin
  FreeAndNil(FPoints);
end;


procedure TDocument.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


procedure TDocument.AddPoint(X, Y: single);
begin
  FPoints.Add(PointF(X, Y));
  Change;
end;


function TDocument.GetPoints: TXYPointList;
begin
  Result := FPoints;
end;


procedure TDocument.RemoveAllPoints;
begin
  FPoints.Clear;
  Change;
end;


procedure TDocument.SaveToFile(APath: String);
var
  list: TStringList;
  point: TPointF;
begin
  list := TStringList.Create;
  try
    for point in FPoints do
    begin
      list.Add(Format('%f,%f', [point.x, point.y]));
    end;
    list.LineBreak := #10;
    list.SaveToFile(APath);
  finally
    list.Free;
  end;
end;


procedure TDocument.LoadFromFile(APath: String);
var
  list, items: TStringList;
  str, s1, s2: String;
  x, y: single;
begin
  if not FileExistsUTF8(APath) then
    exit;
  if not FileIsReadable(APath) then
    exit;

  list := TStringList.Create;
  items := TStringList.Create;
  try
    list.LineBreak := #10;
    list.LoadFromFile(APath);
    for str in list do
    begin
      items.Clear;
      items.Delimiter := ',';
      items.DelimitedText := str;
      if items.Count = 2 then
      begin
        s1 := Trim(items.Strings[0]);
        s2 := Trim(items.Strings[1]);
        try
          x := StrToFloat(s1);
          y := StrToFloat(s2);
          AddPoint(x, y);
        except
          on E: EConvertError do
            DebugLn('skip not a number');
        end;
      end;
    end;
  finally
    items.Free;
    list.Free;
  end;
end;


procedure TDocument.SaveToDefault;
begin
  SaveToFile(AppConfigFilePath);
end;


procedure TDocument.LoadFromDefault;
begin
  LoadFromFile(AppConfigFilePath);
end;


function AppConfigFilePath: String;
begin
  Result := ConcatPaths([AppConfigDir, 'hoge.csv']);
end;


function AppConfigDir: String;
begin
  Result := GetAppConfigDirUTF8(False, True);
end;


function PointF(X, Y: single): TPointF;
begin
  Result.x := X;
  Result.y := Y;
end;

end.
