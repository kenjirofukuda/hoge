unit UGraphicDocument;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}


interface

uses
  Classes, SysUtils, Controls, Types, LazFileUtils, LazLogger, fgl,
  UGeometryUtils, UDocument, UGraphicCore;

type
  { TGraphicDocument }

  TGraphicDocument = class(TDocument)
    constructor Create;

  private
    FGraphicList: TGraphicList;

    function GetBounds: TRectangleF;
    function GetSelectedCount: longint;
    procedure LoadGraphic(items: TStringList);

  public
    destructor Destroy; override;

    procedure SaveToFile(APath: string); override;
    procedure LoadFromFile(APath: string); override;

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
    property Bounds: TRectangleF read GetBounds;
    property SelectedCount: longint read GetSelectedCount;
  end;

  TGraphicDocumentCommand = class(TDocumentCommand)
  private
    FSelectedGraphics: TGraphicList;
  public
    constructor Create(ADocument: TGraphicDocument);
  end;


  TClearCommand = class(TGraphicDocumentCommand)
  public
    function Redo: boolean; override;
    function Undo: boolean; override;
  end;


  TAddGraphicsCommand = class(TGraphicDocumentCommand)
  private
    FCreatedGraphics: TGraphicList;
  public
    constructor Create(ADocument: TGraphicDocument; AGraphics: TGraphicList);
    constructor Create(ADocument: TGraphicDocument; AGraphic: TKFGraphic);

    function Redo: boolean; override;
    function Undo: boolean; override;
  end;


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


constructor TGraphicDocument.Create;
begin
  inherited;
  FGraphicList := TGraphicList.Create(False);
  DefaultBaseName := 'hoge.csv';
end;


destructor TGraphicDocument.Destroy;
begin
  FreeAndNil(FGraphicList);
  inherited;
end;


procedure TGraphicDocument.AddPoint(X, Y: single);
begin
  AddGraphic(TPointGraphic.Create(X, Y));
end;


procedure TGraphicDocument.AddGraphic(AGraphic: TKFGraphic);
begin
  FGraphicList.Add(AGraphic);
  Change;
end;


function TGraphicDocument.GetGraphics: TGraphicList;
begin
  Result := FGraphicList;
end;


function TGraphicDocument.GetSelectedGraphics: TGraphicList;
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


function TGraphicDocument.GetSelectedCount: longint;
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


procedure TGraphicDocument.InstallSampleGraphics(xmin, ymin, xmax, ymax: single;
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


procedure TGraphicDocument.RemoveGraphics(AGraphicList: TGraphicList);
var
  g: TKFGraphic;
begin
  for g in AGraphicList do
    FGraphicList.Remove(g);
  Change;
end;


procedure TGraphicDocument.AddGraphics(AGraphicList: TGraphicList);
var
  g: TKFGraphic;
begin
  for g in AGraphicList do
    FGraphicList.Add(g);
  Change;
end;


procedure TGraphicDocument.RemoveAllGraphic;
begin
  SetAllSelected(True);
  RemoveSelectedGraphic;
end;


procedure TGraphicDocument.SetAllSelected(State: boolean);
var
  g: TKFGraphic;
begin
  for g in FGraphicList do
  begin
    g.Selected := State;
  end;
  Change;
end;


procedure TGraphicDocument.RemoveSelectedGraphic;
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


procedure TGraphicDocument.SaveToFile(APath: string);
var
  list: TStringList;
  g: TKFGraphic;
  point: TPointGraphic;
begin
  list := TStringList.Create;
  try
    for g in FGraphicList do
    begin
      list.Add(g.ToCSVRecord);
    end;
    list.LineBreak := #10;
    list.SaveToFile(APath);
  finally
    list.Free;
  end;
end;


procedure TGraphicDocument.LoadFromFile(APath: string);
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
      LoadGraphic(items);
    end;
  finally
    items.Free;
    list.Free;
  end;
end;


procedure TGraphicDocument.LoadGraphic(items: TStringList);
var
  s0, s1, s2, s3, s4: string;
  v1, v2, v3, v4, v5: single;
  g: TKFGraphic;
begin
  g := nil;
  if items.Count >= 1 then
  begin
    s0 := Trim(items.Strings[0]);
    try
      if items.Count >= 3 then
      begin
        s1 := Trim(items.Strings[1]);
        s2 := Trim(items.Strings[2]);
        v1 := StrToFloat(s1);
        v2 := StrToFloat(s2);
        if s0.Equals('point') then
          g := TPointGraphic.Create(v1, v2);
      end;
      if items.Count >= 5 then
      begin
        s3 := Trim(items.Strings[3]);
        s4 := Trim(items.Strings[4]);
        v3 := StrToFloat(s3);
        v4 := StrToFloat(s4);
        if s0.Equals('rect') then
          g := TRectGraphic.Create(v1, v2, v3, v4);
      end;
      if g <> nil then
        AddGraphic(g);
    except
      on E: EConvertError do
        DebugLn('skip not a number');
    end;
  end;
end;


function TGraphicDocument.GetBounds: TRectangleF;
var
  rect: TRectangleF;
  g: TKFGraphic;
  point: TPointGraphic;
begin
  rect := EmptyRectangleF;
  for g in FGraphicList do
  begin
    rect := rect.Merge(g.Bounds);
  end;
  Result := rect;
end;


procedure TGraphicDocument.SaveToDefault;
begin
  SaveToFile(AppConfigFilePath);
end;


procedure TGraphicDocument.LoadFromDefault;
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


function TGraphicDocument.FindGraphicAt(APoint: TPointF; ARadius: longint;
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


constructor TGraphicDocumentCommand.Create(ADocument: TGraphicDocument);
begin
  inherited Create(ADocument);
  FSelectedGraphics := TGraphicList.Create;
  FSelectedGraphics.Assign(ADocument.GetSelectedGraphics);
end;


function TClearCommand.Redo: boolean;
begin
  (FDocument as TGraphicDocument).RemoveGraphics(FSelectedGraphics);
  Result := True;
end;


function TClearCommand.Undo: boolean;
begin
  (FDocument as TGraphicDocument).AddGraphics(FSelectedGraphics);
  Result := False;
end;


constructor TAddGraphicsCommand.Create(ADocument: TGraphicDocument;
  AGraphics: TGraphicList);
begin
  inherited Create(ADocument);
  FCreatedGraphics := TGraphicList.Create(False);
  FCreatedGraphics.Assign(AGraphics);
end;


constructor TAddGraphicsCommand.Create(ADocument: TGraphicDocument;
  AGraphic: TKFGraphic);
begin
  Self.Create(ADocument, AGraphic.AsList);
end;


function TAddGraphicsCommand.Redo: boolean;
begin
  (FDocument as TGraphicDocument).AddGraphics(FCreatedGraphics);
  Result := True;
end;


function TAddGraphicsCommand.Undo: boolean;
begin
  (FDocument as TGraphicDocument).RemoveGraphics(FCreatedGraphics);
  Result := False;
end;


end.
