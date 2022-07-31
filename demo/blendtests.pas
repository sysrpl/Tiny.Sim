unit BlendTests;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Application,
  Tiny.Graphics;

{ TBlendTests }

type
  TBlendTests = class(TScene)
  protected
    FImage: IBitmap;
    FSpots: IBitmap;
    FBrush: IBrush;
    procedure Load; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
  end;

implementation

{ TBlendTests }

procedure TBlendTests.Load;
begin
  inherited Load;
  Font := Canvas.LoadFont('roboto', '../assets/Roboto-Medium.ttf');
  Font.Size := 30;
  FImage := Canvas.LoadBitmap('rocks', '../assets/rocks.jpg');
  FBrush := NewBrush(FImage);
  FSpots := Canvas.LoadBitmap('spots', '../assets/spots.gif');
end;

procedure TBlendTests.Render(Width, Height: Integer; const Time: Double);
var
  S: string;
begin
  inherited Render(Width, Height, Time);
  ScaleToStudio;
  Canvas.DrawImage(FImage, 200, 200);
  if Round(Time) mod 2 = 1 then
  begin
    Canvas.BlendMode := blendNegative;
    S := 'Invert';
  end
  else
  begin
    Canvas.BlendMode := blendDarken;
    S := 'Dark';
  end;
  with PointToStudio(Mouse.X, Mouse.Y) do
    Canvas.DrawImage(FSpots, X, Y);
  Canvas.BlendMode := blendAlpha;
  DrawSceneInfo(S);
end;

end.

