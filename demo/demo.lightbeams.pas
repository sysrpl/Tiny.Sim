unit Demo.LightBeams;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application,
  Tiny.Graphics,
  Demo.Scene;

{ TLightBeams }

const
  BeamCount = 15;

type
  PLightPoint = ^TLightPoint;
  TLightPoint =  record
    X, Y, DX, DY: Single;
    Color: TColorF;
    Next: PLightPoint;
    procedure Move(const Bounds: TRectF; Step: Double);
    procedure Generate(const Bounds: TRectF; Range: Integer);
  end;

  TLightBeams = class(TDemoScene)
  private
    FBuffer: IRenderBitmap;
    FWidth, FHeight: Integer;
    FBounds: TRectF;
    FTotal: Double;
    FLast: Double;
    FSpeed: Float;
    FBeams: array[0..BeamCount - 1] of TLightPoint;
  protected
    function GetTitle: string; override;
    function GetDescription: string; override;
    function GetGlyph: string; override;
    function RangeGenerate: TRangeInfo; override;
    procedure RangeChanged(NewPosition: Float); override;
    procedure Load; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
  end;

implementation

procedure TLightPoint.Move(const Bounds: TRectF; Step: Double);
begin
  X := X + DX * Step;
  if (X < Bounds.Left) or (X > Bounds.Right) then
    DX := -DX;
  Y := Y + DY * Step;
  if (Y < Bounds.Top) or (Y > Bounds.Bottom) then
    DY := -DY;
end;

procedure TLightPoint.Generate(const Bounds: TRectF; Range: Integer);
var
  A: Single;
begin
  X := Random * Bounds.Width + Bounds.Left;
  Y := Random * Bounds.Height + Bounds.Top;
  A := Random * Pi;
  DX := Cos(A);
  DY := Sin(A);
  Color := NewHSL(Random / 5 + Range / 5, 0.5, 0.5, 0.025);
  Next := nil;
end;

{ TLightBeams }

function TLightBeams.GetTitle: string;
begin
  Result := 'Light Beams';
end;

function TLightBeams.GetDescription: string;
begin
  Result := 'A progressive painting of light beams moving around your screen ' +
    'generating a random image.';
end;

function TLightBeams.GetGlyph: string;
begin
  Result := '󱒄';
end;

function TLightBeams.RangeGenerate: TRangeInfo;
begin
  Result := TRangeInfo.Create('Speed of generation', FSpeed, 0.1, 0.1, 10);
end;

procedure TLightBeams.RangeChanged(NewPosition: Float);
begin
  FSpeed := NewPosition;
end;

procedure TLightBeams.Load;
begin
  inherited Load;
  if FSpeed = 0 then
  begin
    FSpeed := 2;
    FBuffer := Canvas.NewBitmap('buffer', FWidth, FHeight);
    FBounds := NewRectF(StudioWidth, StudioHeight);
    FBounds.Inflate(200, 200);
  end;
  FLast := 0;
end;

procedure TLightBeams.Render(Width, Height: Integer; const Time: Double);

  procedure Reset;
  var
    R: Integer;
    I: Integer;
  begin
    FBuffer.Bind;
    Canvas.Clear;
    FBuffer.Unbind;
    FLast := Time;
    FTotal := 0;
    Randomize;
    R := Trunc(Random * 6);
    for I := 0 to BeamCount - 1 do
    begin
      FBeams[I].Generate(FBounds, R);
      if I < BeamCount - 1 then
        FBeams[I].Next := @FBeams[I + 1];
    end;
    FBeams[BeamCount - 1].Next := @FBeams[0];
  end;

const
  Factor = LogicStep / (1 / 120);
  Delta = 1 / 800 * Factor;
var
  I: Integer;
begin
  inherited Render(Width, Height, Time);
  Canvas.BlendMode := blendAdditive;
  if (Width <> FWidth) or (Height <> FHeight) then
  begin
    FWidth := Width;
    FHeight := Height;
    FBuffer.Resize(FWidth, FHeight);
    Reset;
    Exit;
  end;
  ScaleToStudio;
  FBuffer.Bind;
  while FLast < Time do
  begin
    if FTotal < 5 then
    begin
      for I := 0 to BeamCount - 1 do
        FBeams[I].Move(FBounds, Delta * FSpeed * 400);
      for I := 0 to BeamCount - 1 do
      begin
        Canvas.MoveTo(FBeams[I].X, FBeams[I].Y);
        Canvas.LineTo(FBeams[I].Next.X, FBeams[I].Next.Y);
        Canvas.Stroke(FBeams[I].Color);
      end;
    end;
    FLast := FLast + Delta;
    FTotal := FTotal + Delta;
  end;
  FBuffer.Unbind;
  Canvas.Matrix.Identity;
  Canvas.DrawImage(FBuffer, 0, 0);
  if FTotal > 8 then
    Reset;
  ScaleToStudio;
  DrawSceneInfo;
end;

end.

