unit Demo.CrowdWalk;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application,
  Tiny.Graphics,
  Demo.Scene;

const
  CrowdCount = 500;

type
  PPerson = ^TPerson;
  TPerson = record
    Index: Integer;
    Dir: Integer;
    Pos: Single;
    Jitter: Single;
    Speed: Double;
    Time: Double;
    procedure Generate(T: Double);
  end;

{ TCrowdWalk }

  TCrowdWalk = class(TDemoScene)
  private
    FImage: IBitmap;
    FCity: IBitmap;
    FCrowdSize: Integer;
    FCrowd: array[0..CrowdCount - 1] of TPerson;
    procedure DrawPerson(X, Y: Single; Index: Integer; Dir: Integer);
  protected
    function GetTitle: string; override;
    function GetDescription: string; override;
    function GetGlyph: string; override;
    function RangeGenerate: TRangeInfo; override;
    procedure RangeChanged(NewPosition: Float); override;
    procedure Load; override;
    procedure Logic(Width, Height: Integer; const Time: Double); override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
  end;

implementation

const
  ImageCols = 15;
  ImageRows = 7;
  ImageCells = ImageCols * ImageRows;

procedure TPerson.Generate(T: Double);
begin
  Index := Trunc(Random * ImageCells);
  if Random < 0.5 then
    Dir := -1
  else
    Dir := 1;
  Pos := Random * 300 + 300;
  Speed := Random * 100 + 80;
  Jitter := Random * 15;
  Time := T;
end;

{ TCrowdWalk }

function TCrowdWalk.GetTitle: string;
begin
  Result := 'Crowd Walk';
end;

function TCrowdWalk.GetDescription: string;
begin
  Result := 'A crowd of people cross a street intersection in a busy city.';
end;

function TCrowdWalk.GetGlyph: string;
begin
  Result := '󰖃';
end;

function TCrowdWalk.RangeGenerate: TRangeInfo;
begin
  Result := TRangeInfo.Create('Number of pedestrians', FCrowdSize, 1, 1, 500);
end;

procedure TCrowdWalk.RangeChanged(NewPosition: Float);
begin
  FCrowdSize := Round(NewPosition);
end;

procedure TCrowdWalk.Load;
var
  I: Integer;
begin
  inherited Load;
  if FCrowdSize = 0 then
  begin
    FCrowdSize := 50;
    FImage := Canvas.LoadBitmap('crowd', '../assets/crowd.png');
    FCity := Canvas.LoadBitmap('street', '../assets/street.gif');
  end;
  for I := 0 to CrowdCount - 1 do
    FCrowd[I].Generate(Random * 20 - 20);
end;

procedure TCrowdWalk.Logic(Width, Height: Integer; const Time: Double);
const
  Reset = 400;
var
  P: PPerson;
  D: Single;
  I: Integer;
begin
  inherited Logic(Width, Height, Time);
  for I := 0 to CrowdCount - 1 do
  begin
    P := @FCrowd[I];
    D := Time - P.Time;
    if (P.Dir < 0) and (StudioWidth - D * P.Speed + P.Pos < -Reset) then
      P.Generate(Time)
    else if (P.Dir > 0) and (D * P.Speed - P.Pos > StudioWidth + Reset) then
      P.Generate(Time);
  end;
end;

procedure TCrowdWalk.DrawPerson(X, Y: Single; Index: Integer; Dir: Integer);
var
  S, D: TRectF;
begin
  S.Width := FImage.Width div ImageCols;
  S.Height := FImage.Height div ImageRows;
  S.X := (Index mod ImageCols) * S.Width + 20;
  S.Y := (Index div ImageCols) * S.Height;
  D.Width := S.Width + 40;
  if Dir < 0 then
    D.Width := -D.Width;
  D.Height := S.Height;
  D.X := X - S.Width / 2 + 20;
  D.Y := Y - S.Height + 40;
  Canvas.DrawImage(FImage, S, D);
end;

procedure TCrowdWalk.Render(Width, Height: Integer; const Time: Double);
var
  P: PPerson;
  D, X, Y: Single;
  I: Integer;
begin
  inherited Render(Width, Height, Time);
  Canvas.BlendMode := blendAlpha;
  ScaleToStudio;
  if FCrowdSize < 90 then
    Canvas.Matrix.ScaleAt(1.35, 1.35, StudioWidth / 2, 0)
  else if FCrowdSize < 150 then
    Canvas.Matrix.ScaleAt(1.3, 1.3, StudioWidth / 2, 0)
  else if FCrowdSize < 250 then
    Canvas.Matrix.ScaleAt(1.2, 1.2, StudioWidth / 2, 0)
  else if FCrowdSize < 350 then
    Canvas.Matrix.ScaleAt(1.1, 1.1, StudioWidth / 2, 0);
  Canvas.Matrix.Push;
  Canvas.Matrix.ScaleAt(1 / 1.5, 1 / 1.5, 0, 0);
  Canvas.DrawImage(FCity, 0, 0);
  Canvas.Matrix.Pop;
  for I := 0 to FCrowdSize - 1 do
  begin
    P := @FCrowd[I];
    D := Time - P.Time;
    if P.Dir < 0 then
      X := StudioWidth - D * P.Speed + P.Pos
    else
      X := D * P.Speed - P.Pos;
    if I < 40 then
      Y := 775
    else if I < 150 then
      Y := 875
    else if I < 275 then
      Y := 1000
    else if I < 400 then
      Y := 1100
    else
      Y := 1200;
    Y := Y + Abs(Sin(D * 3) * 20) + P.Jitter + 50;
    DrawPerson(X, Y, P.Index, P.Dir);
  end;
end;

end.

