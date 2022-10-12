unit UDocument;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}


interface

uses
  Classes, SysUtils, Controls, LazFileUtils, LazLogger,
  UUndoManager;

type

  { TDocument }

  TDocument = class
    constructor Create;

  private {UDocument}
    FOnChange: TNotifyEvent;
    FLockChange: boolean;
    FLockCount: longint;
    FUndoManager: THistoryIterator;
    FDefaultBaseName: string;


  public
    destructor Destroy; override;

    procedure SaveToFile(APath: string); virtual;
    procedure LoadFromFile(APath: string); virtual;
    procedure LockChange;
    procedure UnlockChange;
    procedure Change;
    procedure SaveToDefault;
    procedure LoadFromDefault;
    function AppConfigDir: string;
    function AppConfigFilePath: string;

  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property UndoManager: THistoryIterator read FUndoManager;
    property DefaultBaseName: string read FDefaultBaseName write FDefaultBaseName;
  end;

  TDocumentCommand = class(TUndoRedoRecord)
  protected
    FDocument: TDocument;
  public
    constructor Create(ADocument: TDocument);
  end;


implementation

constructor TDocument.Create;
begin
  inherited;
  FLockChange := False;
  FUndoManager := THistoryIterator.Create;
end;

procedure TDocument.SaveToFile(APath: string);
begin
  DebugLn('TDocument.SaveToFile');
end;

procedure TDocument.LoadFromFile(APath: string);
begin
  DebugLn('TDocument.LoadFromFile(%s)', [APath]);
end;


destructor TDocument.Destroy;
begin
  FreeAndNil(FUndoManager);
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


procedure TDocument.SaveToDefault;
begin
  SaveToFile(AppConfigFilePath);
end;


procedure TDocument.LoadFromDefault;
begin
  LoadFromFile(AppConfigFilePath);
end;


constructor TDocumentCommand.Create(ADocument: TDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;


function TDocument.AppConfigFilePath: string;
begin
  Result := ConcatPaths([AppConfigDir, DefaultBaseName]);
end;


function TDocument.AppConfigDir: string;
begin
  Result := GetAppConfigDirUTF8(False, True);
end;


end.
