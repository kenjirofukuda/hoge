unit UDocument;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Types, LazFileUtils, LazLogger, fgl,
  UGeometyUtils, UGraphicBase, UGraphic;

type
  TClearAllAction = class;

  TDocument = class
    constructor Create;
  private
    FGraphicList: TGraphicList;

    FOnChange: TNotifyEvent;
    FLockChange: boolean;
    FLockCount: longint;
    FClearAllAction: TClearAllAction;
    procedure SaveToFile(APath: String);
    procedure LoadFromFile(APath: String);
    procedure LockChange;
    procedure UnlockChange;
    procedure Change;
    function GetBounds: TRectangleF;

  public
    destructor Destroy; override;
    procedure AddPoint(X, Y: single);
    procedure AddGraphic(AGraphic: TGraphic);
    procedure RemoveAllPoints();
    function GetGraphics: TGraphicList;
    procedure SaveToDefault;
    procedure LoadFromDefault;

  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ClearAllAction: TClearAllAction read FClearAllAction;
    property Bounds: TRectangleF read GetBounds;
  end;

  { TODO : Use TBasicAction if undoable mechanizum enabled }
  TClearAllAction = class
    constructor Create(ADocument: TDocument);
  private
    FDocument: TDocument;
    FGraphicList: TGraphicList;
    function GetEnabled: boolean;
    function GetUndoable: boolean;
  public
    procedure DoIt;
    procedure UndoIt;
    property Enabled: boolean read GetEnabled;
    property Undoable: boolean read GetUndoable;
  end;


function AppConfigDir: String;
function AppConfigFilePath: String;


implementation



constructor TDocument.Create;
begin
  inherited;
  FGraphicList := TGraphicList.Create;
  FLockChange := False;
  FClearAllAction := TClearAllAction.Create(Self);
end;


destructor TDocument.Destroy;
begin
  FreeAndNil(FGraphicList);
end;


procedure TDocument.Change;
begin
  if FLockChange then
  begin
    Inc(FLockCount);
  end
  else
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;


procedure TDocument.LockChange;
begin
  FLockChange := True;
  FLockCount := 0;
end;


procedure TDocument.UnlockChange;
begin
  FLockChange := False;
  if FLockCount > 0 then
    Change;
end;


procedure TDocument.AddPoint(X, Y: single);
begin
  AddGraphic(TPointGraphic.Create(X, Y));
end;


procedure TDocument.AddGraphic(AGraphic: TGraphic);
begin
  FGraphicList.Add(AGraphic);
  Change;
end;


function TDocument.GetGraphics: TGraphicList;
begin
  Result := FGraphicList;
end;


procedure TDocument.RemoveAllPoints;
begin
  FGraphicList.Clear;
  Change;
end;


procedure TDocument.SaveToFile(APath: String);
var
  list: TStringList;
  g: TGraphic;
  point: TPointGraphic;
begin
  list := TStringList.Create;
  try
    for g in FGraphicList do
    begin
      point := g as TPointGraphic;
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


function TDocument.GetBounds: TRectangleF;
var
  rect: TRectangleF;
  g: TGraphic;
  point: TPointGraphic;
begin
  rect := EmptyRectangleF;
  for g in FGraphicList do
  begin
    point := g as TPointGraphic;
    rect := rect.Merge(RectangleF(point.Origin, point.Origin));
  end;
  Result := rect;
end;


procedure TDocument.SaveToDefault;
begin
  SaveToFile(AppConfigFilePath);
end;


procedure TDocument.LoadFromDefault;
begin
  LoadFromFile(AppConfigFilePath);
end;


constructor TClearAllAction.Create(ADocument: TDocument);
begin
  FDocument := ADocument;
  //FPoints := TXYPointList.Create;
  FGraphicList := TGraphicList.Create;
end;


procedure TClearAllAction.DoIt;
begin
  FGraphicList.Assign(FDocument.GetGraphics);
  FDocument.RemoveAllPoints();
end;


procedure TClearAllAction.UndoIt;
var
  g: TGraphic;
begin
  FDocument.LockChange;
  for g in FGraphicList do
  begin
    FDocument.AddGraphic(g);
  end;
  FDocument.UnlockChange;
end;


function TClearAllAction.GetEnabled: boolean;
begin
  Result := FDocument.GetGraphics.Count > 0;
end;


function TClearAllAction.GetUndoable: boolean;
begin
  Result := FGraphicList.Count > 0;
end;


function AppConfigFilePath: String;
begin
  Result := ConcatPaths([AppConfigDir, 'hoge.csv']);
end;


function AppConfigDir: String;
begin
  Result := GetAppConfigDirUTF8(False, True);
end;



end.
