unit Demo.Scene;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application;

{ TRangeInfo }

type
  TDemoScene = class;

  TRangeInfo = class
  private
    FDescription: string;
    FPosition: Float;
    FStep: Float;
    FMin: Float;
    FMax: Float;
    FScene: TDemoScene;
    procedure SetPosition(const Value: Float);
  public
    constructor Create(Description: string; Position, Step, Min, Max: Float);
    property Description: string read FDescription;
    property Position: Float read FPosition write SetPosition;
    property Step: Float read FStep;
    property Min: Float read FMin;
    property Max: Float read FMax;
  end;

{ TDemoScene }

  TDemoScene = class(TScene)
  private
    FRangeInfo: TRangeInfo;
    function GetRangeInfo: TRangeInfo;
  protected
    function GetTitle: string; virtual;
    function GetDescription: string; virtual;
    function GetGlyph: string; virtual;
    function RangeGenerate: TRangeInfo; virtual;
    procedure RangeChanged(NewPosition: Float); virtual;
    function GetHideMouse: Boolean; virtual;
  public
    property Title: string read GetTitle;
    property Description: string read GetDescription;
    property Glyph: string read GetGlyph;
    property RangeInfo: TRangeInfo read GetRangeInfo;
    property HideMouse: Boolean read GetHideMouse;
  end;

  TDemoSceneClass = class of TDemoScene;

implementation

{ TRangeInfo }

constructor TRangeInfo.Create(Description: string; Position, Step, Min,
  Max: Float);
begin
  inherited Create;
  FDescription := Description;
  FPosition := Position;
  FStep := Step;
  FMin := Min;
  FMax := Max;
end;

procedure TRangeInfo.SetPosition(const Value: Float);
begin
  if FPosition = Value then Exit;
  FPosition := Value;
  FScene.RangeChanged(FPosition);
end;

{ TDemoScene }

function TDemoScene.GetRangeInfo: TRangeInfo;
begin
  if FRangeInfo = nil then
  begin
    FRangeInfo := RangeGenerate;
    FRangeInfo.FScene := Self;
  end;
  Result := FRangeInfo;
end;

function TDemoScene.GetTitle: string;
begin
  Result := 'Title';
end;

function TDemoScene.GetDescription: string;
begin
  Result := 'Description';
end;

function TDemoScene.GetGlyph: string;
begin
  Result := '󰄱';
end;

function TDemoScene.RangeGenerate: TRangeInfo;
begin
  Result := TRangeInfo.Create('', 0, 0, 0, 0);
end;

procedure TDemoScene.RangeChanged(NewPosition: Float);
begin
end;

function TDemoScene.GetHideMouse: Boolean;
begin
  Result := False;
end;

end.

