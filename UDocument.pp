unit UDocument;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}


interface

uses
  Classes, SysUtils, Controls, Types, LazFileUtils, LazLogger, fgl,
  UGeometryUtils, UGraphicBase, UGraphic, UUndoManager;

type

  TDocument = class
    constructor Create;
  private
    FGraphicList: TGraphicList;

    FOnChange: TNotifyEvent;
    FLockChange: boolean;
    FLockCount: longint;
    FUndoManager: THistoryIterator;

    procedure SaveToFile(APath: string);
    procedure LoadFromFile(APath: string);
    procedure LockChange;
    procedure UnlockChange;
    procedure Change;
    function GetBounds: TRectangleF;
    function GetSelectedCount: longint;

  public
    destructor Destroy; override;
    procedure AddPoint(X, Y: single);
    procedure AddGraphic(AGraphic: TKFGraphic);
    procedure AddGraphics(AGraphicList: TGraphicList);

    function FindGraphicAt(APoint: TPointF; ARadius: longint;
      AViewScale: single): TKFGraphic;

    procedure SetAllSelected(State: boolean);
    procedure RemoveAllGraphic;
    procedure RemoveGraphics(AGraphicList: TGraphicList);
    procedure RemoveSelectedGraphic;
    function GetSelectedGraphics: TGraphicList;
    function GetGraphics: TGraphicList;
    procedure SaveToDefault;
    procedure LoadFromDefault;

    procedure InstallSampleGraphics(xmin, ymin, xmax, ymax: single;
      ACount: longint = 100);

  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Bounds: TRectangleF read GetBounds;
    property SelectedCount: longint read GetSelectedCount;
    property UndoManager: THistoryIterator read FUndoManager;
  end;

  TDocumentCommand = class(TUndoRedoRecord)
  private
    FSelectedGraphics: TGraphicList;
    FDocument: TDocument;
  public
    constructor Create(ADocument: TDocument);
  end;


  TClearCommand = class(TDocumentCommand)
  public
    function Redo: boolean; override;
    function Undo: boolean; override;
  end;


  TAddGraphicsCommand = class(TDocumentCommand)
  private
    FCreatedGraphics: TGraphicList;
  public
    constructor Create(ADocument: TDocument; AGraphics: TGraphicList);
    constructor Create(ADocument: TDocument; AGraphic: TKFGraphic);

    function Redo: boolean; override;
    function Undo: boolean; override;
  end;


function AppConfigDir: string;
function AppConfigFilePath: string;


implementation

uses
  Math;

type
  TGraphicDistance = record
    FGraphic: TKFGraphic;
    FDistance: single;
    class operator = (a, b: TGraphicDistance): boolean;
  end;

  TGraphicDistanceList = specialize TFPGList<TGraphicDistance>;

class operator TGraphicDistance. = (a, b: TGraphicDistance): boolean;
begin
  Result := (a.FGraphic = b.FGraphic) and SameValue(a.FDistance, b.FDistance);
end;


constructor TDocument.Create;
begin
  inherited;
  FGraphicList := TGraphicList.Create(False);
  FLockChange := False;
  FUndoManager := THistoryIterator.Create;
end;


destructor TDocument.Destroy;
begin
  FreeAndNil(FUndoManager);
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


procedure TDocument.AddGraphic(AGraphic: TKFGraphic);
begin
  FGraphicList.Add(AGraphic);
  Change;
end;


function TDocument.GetGraphics: TGraphicList;
begin
  Result := FGraphicList;
end;


function TDocument.GetSelectedGraphics: TGraphicList;
var
  g: TKFGraphic;
begin
  Result := TGraphicList.Create(False);
  for g in FGraphicList do
  begin
    if g.Selected then
      Result.Add(g);
  end;
end;


function TDocument.GetSelectedCount: longint;
var
  g: TKFGraphic;
begin
  Result := 0;
  for g in FGraphicList do
  begin
    if g.Selected then
      Inc(Result);
  end;
end;


procedure TDocument.InstallSampleGraphics(xmin, ymin, xmax, ymax: single;
  ACount: longint);
var
  samples: TGraphicList;

  function CreateSampleGraphics(xmin, ymin, xmax, ymax: single;
    ACount: longint): TGraphicList;
  var
    i: longint;
    x, y: single;
  begin
    Result := TGraphicList.Create(False);
    for i := 1 to ACount do
    begin
      x := Random * (xmax - xmin) + xmin;
      y := Random * (ymax - ymin) + ymin;
      Result.Add(TPointGraphic.Create(x, y));
    end;
  end;

begin
  samples := CreateSampleGraphics(xmin, ymin, xmax, ymax, ACount);
  UndoManager.DoAndAddRecord(TAddGraphicsCommand.Create(Self, samples));
end;


procedure TDocument.RemoveGraphics(AGraphicList: TGraphicList);
var
  g: TKFGraphic;
begin
  for g in AGraphicList do
    FGraphicList.Remove(g);
  Change;
end;


procedure TDocument.AddGraphics(AGraphicList: TGraphicList);
var
  g: TKFGraphic;
begin
  for g in AGraphicList do
    FGraphicList.Add(g);
  Change;
end;



procedure TDocument.RemoveAllGraphic;
begin
  SetAllSelected(True);
  RemoveSelectedGraphic;
end;


procedure TDocument.SetAllSelected(State: boolean);
var
  g: TKFGraphic;
begin
  for g in FGraphicList do
  begin
    g.Selected := State;
  end;
  Change;
end;


procedure TDocument.RemoveSelectedGraphic;
var
  selectedGraphics: TGraphicList;
begin
  // TODO: each Graphic Free
  selectedGraphics := GetSelectedGraphics;
  try
    RemoveGraphics(selectedGraphics);
  finally
    //selectedGraphics.Free;
  end;
end;


procedure TDocument.SaveToFile(APath: string);
var
  list: TStringList;
  g: TKFGraphic;
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


procedure TDocument.LoadFromFile(APath: string);
var
  list, items: TStringList;
  str, s1, s2: string;
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
  g: TKFGraphic;
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


function distanceCompare(const a, b: TGraphicDistance): integer;
begin
  if SameValue(a.FDistance, b.FDistance) then
    Result := 0
  else if a.FDistance < b.FDistance then
    Result := -1
  else
    Result := 1;
end;


function TDocument.FindGraphicAt(APoint: TPointF; ARadius: longint;
  AViewScale: single): TKFGraphic;
var
  g: TKFGraphic;
  graphicDistance: TGraphicDistance;
  distanceList: TGraphicDistanceList;

begin
  Result := nil;
  distanceList := TGraphicDistanceList.Create;
  for g in FGraphicList do
  begin
    graphicDistance.FGraphic := g;
    graphicDistance.FDistance := g.Distance(APoint);
    distanceList.Add(graphicDistance);
  end;
  distanceList.Sort(@distanceCompare);
  graphicDistance := distanceList.First;
  distanceList.Free;
  if graphicDistance.FDistance <= (ARadius / AViewScale) then
    Result := graphicDistance.FGraphic
  else
    Result := nil;
end;


constructor TDocumentCommand.Create(ADocument: TDocument);
begin
  inherited Create;
  FDocument := ADocument;
  FSelectedGraphics := TGraphicList.Create;
  FSelectedGraphics.Assign(FDocument.GetSelectedGraphics);
end;


function TClearCommand.Redo: boolean;
begin
  FDocument.RemoveGraphics(FSelectedGraphics);
  Result := True;
end;


function TClearCommand.Undo: boolean;
begin
  FDocument.AddGraphics(FSelectedGraphics);
  Result := False;
end;


constructor TAddGraphicsCommand.Create(ADocument: TDocument; AGraphics: TGraphicList);
begin
  inherited Create(ADocument);
  FCreatedGraphics := TGraphicList.Create(False);
  FCreatedGraphics.Assign(AGraphics);
end;


constructor TAddGraphicsCommand.Create(ADocument: TDocument; AGraphic: TKFGraphic);
begin
  Self.Create(ADocument, AGraphic.AsList);
end;


function TAddGraphicsCommand.Redo: boolean;
begin
  FDocument.AddGraphics(FCreatedGraphics);
  Result := True;
end;


function TAddGraphicsCommand.Undo: boolean;
begin
  FDocument.RemoveGraphics(FCreatedGraphics);
  Result := False;
end;


function AppConfigFilePath: string;
begin
  Result := ConcatPaths([AppConfigDir, 'hoge.csv']);
end;


function AppConfigDir: string;
begin
  Result := GetAppConfigDirUTF8(False, True);
end;



end.
