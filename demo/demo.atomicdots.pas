unit Demo.AtomicDots;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application,
  Tiny.Graphics,
  Demo.Scene;

{ TSpaceDots }

const
  ColorCount = 5;
  ParticleColors: array[0..ColorCount - 1] of TColorB = (
    $FFF35D4F, $FF8768FF, $FFC0D988, $FF6DDAF1, $FF90E8F0);

type
  TParticle = record
    Pos, Veloc: TPointF;
    Radius: Single;
    Color: TColorF;
    Size: Single;
    procedure Generate(Index: Integer; const Bounds: TRectF);
  end;
  PParticle = ^TParticle;

  TAtomicDots = class(TDemoScene)
  private
    FParticleCount: Integer;
    FParticles: array [0..ColorCount - 1, 0..500] of TParticle;
    FBackground: IRadialGradientBrush;
    FAtomic: IBitmapBrush;
    FBounds: TRectF;
    procedure Regen;
    function Particle(I, J: Integer): PParticle;
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

procedure TParticle.Generate(Index: Integer; const Bounds: TRectF);
begin
  Pos.X := Random * Bounds.Width + Bounds.Left;
  Pos.Y := Random * Bounds.Height + Bounds.Top;
  Veloc.X := Random * 3 - 1.5;
  Veloc.Y := Random * 3 - 1.5;
  Radius := (Random * 2) + 1.5;
  Color := ParticleColors[Index];
end;

{ TAtomicDots }

function TAtomicDots.GetTitle: string;
begin
  Result := 'Atomic Dots';
end;

function TAtomicDots.GetDescription: string;
begin
  Result := 'A particle simulation where atoms of the same color grow as they ' +
    'approach each other.';
end;

function TAtomicDots.GetGlyph: string;
begin
  Result := '󰝨';
end;

function TAtomicDots.Particle(I, J: Integer): PParticle;
begin
  Result := @FParticles[I, J];
end;

procedure TAtomicDots.Regen;
var
  I, J: Integer;
begin
  FBounds := ClientRect;
  FBounds.Inflate(20, 20);
  for I := 0 to ColorCount - 1 do
    for J := 0 to 500 do
      Particle(I, J).Generate(I, FBounds);
end;

function TAtomicDots.RangeGenerate: TRangeInfo;
begin
  Result := TRangeInfo.Create('Number of atoms', FParticleCount * ColorCount, 1, 10, 250 * ColorCount);
end;

procedure TAtomicDots.RangeChanged(NewPosition: Float);
begin
  FParticleCount := Round(NewPosition) div ColorCount;
end;

procedure TAtomicDots.Load;
var
  R: TRectF;
begin
  inherited Load;
  if FParticleCount = 0 then
  begin
    FParticleCount := 20;
    Regen;
    R := ClientRect;
    R.Y := R.Height / 1.5;
    FBackground := NewBrush(R);
    FBackground.NearStop.Offset := 0.2;
    FBackground.NearStop.Color := colorBlack;
    FBackground.FarStop.Offset := 2;
    FBackground.FarStop.Color := colorDarkBlue;
    FAtomic := NewBrush(Canvas.LoadBitmap('atomic', '../assets/atomic.gif'));
    FAtomic.Scale := NewPointF(1.5, 1.5);
  end;
end;

const
  ConnectDist = 80;

procedure TAtomicDots.Logic(Width, Height: Integer; const Time: Double);
const
  Speed = 150;
var
  P0, P1: PParticle;
  Factor: Integer;
  I, J, K: Integer;
begin
  for I := 0 to ColorCount - 1 do
    for J := 0 to 500 do
    begin
      P0 := Particle(I, J);
      if J < FParticleCount then
      begin
        Factor := 1;
        for K := 0 to FParticleCount - 1 do
        begin
          P1 := Particle(I, K);
          if P0.Pos.Distance(P1.Pos) < ConnectDist then
            Inc(Factor);
        end;
        if P0.Radius * Factor > P0.Size then
          P0.Size := P0.Size + 20 * LogicStep * P0.Radius
        else
          P0.Size := P0.Size - 20 * LogicStep * P0.Radius;
        if P0.Size > 50 * P0.Radius then
          P0.Size := 50 * P0.Radius;
        if P0.Size < P0.Radius then
          P0.Size := P0.Radius;
      end;
      P0.Pos.Move(P0.Veloc.X * Speed * LogicStep, P0.Veloc.Y * Speed * LogicStep);
      if P0.Pos.X < FBounds.Left then
        P0.Pos.X := FBounds.Right
      else if P0.Pos.X > FBounds.Right then
        P0.Pos.X := FBounds.Left;
      if P0.Pos.Y < FBounds.Top then
        P0.Pos.Y := FBounds.Bottom
      else if P0.Pos.Y > FBounds.Bottom then
        P0.Pos.Y := FBounds.Top;
    end;
end;

procedure TAtomicDots.Render(Width, Height: Integer; const Time: Double);
var
  P0, P1: PParticle;
  I, J, K: Integer;
begin
  inherited Render(Width, Height, Time);
  ScaleToStudio;
  Canvas.Matrix.Push;
  Canvas.Matrix.RotateAt(Time / 3, StudioWidth / 2, StudioHeight / 2);
  Canvas.Rect(NewRectF(-10000, -10000, 20000, 20000));
  Canvas.Fill(FBackground);
  Canvas.Matrix.Pop;
  Canvas.Matrix.Push;
  Canvas.Matrix.RotateAt(Time / -50, StudioWidth / 2, StudioHeight / 2);
  Canvas.Rect(NewRectF(-10000, -10000, 20000, 20000));
  Canvas.BlendMode := blendInvert;
  FAtomic.Offset := NewPointF(30 * Time, -15 * Time);
  Canvas.Fill(FAtomic);
  Canvas.Matrix.Pop;
  Canvas.BlendMode := blendAdditive;
  for I := 0 to ColorCount - 1 do
    for J := 0 to FParticleCount - 1 do
    begin
      P0 := Particle(I, J);
      for K := 0 to FParticleCount - 1 do
      begin
        P1 := Particle(I, K);
        if P0.Pos.Distance(P1.Pos) < ConnectDist then
        begin
          Canvas.MoveTo(P0.Pos.X, P0.Pos.Y);
          Canvas.LineTo(P1.Pos.X, P1.Pos.Y);
          Canvas.Stroke(P0.Color, 0.75);
        end;
      end;
      Canvas.Circle(P0.Pos.X, P0.Pos.Y, P0.Size);
      Canvas.Fill(P0.Color);
    end;
end;

end.

