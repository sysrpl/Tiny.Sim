unit Demo.VectorClock;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application,
  Tiny.Graphics,
  Demo.Scene;

{ TVectorClock }

type
  TVectorClock = class(TDemoScene)
  private
    colorHand: TColorF;
    colorSecondHand: TColorF;
    colorShadow: TColorF;
    FClock: IRenderBitmap;
    FWorld: IBitmap;
    FWorldBrush: IBitmapBrush;
    FRingBrush: IRadialGradientBrush;
    FTickPen: IPen;
    FScale: Float;
    procedure Clock(Scale: Float);
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

{ TVectorClock }

function TVectorClock.GetTitle: string;
begin
  Result := 'Vector Clock';
end;

function TVectorClock.GetDescription: string;
begin
  Result := 'An analog clock drawn using vector graphics.';
end;

function TVectorClock.GetGlyph: string;
begin
  Result := '󰅐';
end;

function TVectorClock.RangeGenerate: TRangeInfo;
begin
  Result := TRangeInfo.Create('Scale of the clocks', FScale, 0, 0.2, 5);
end;

procedure TVectorClock.RangeChanged(NewPosition: Float);
begin
  FScale := NewPosition;
end;

procedure TVectorClock.Load;
begin
  inherited Load;
  if FClock = nil then
  begin
    FClock := Canvas.NewBitmap('clock', 250, 250);
    FWorld := Canvas.LoadBitmap('world', '../assets/world.jpg');
    FWorldBrush := NewBrush(FWorld);
    FWorldBrush.Scale := NewPointF(1.5, 1);

    colorHand := $FF3F3C45;
    colorSecondHand := TColorF.FromBytes(168, 32, 32);
    colorShadow := $40000000;
    FRingBrush := NewBrush(ClientRect);
    FTickPen := NewPen(colorGray, 6);
    FTickPen.LineCap := capRound;
    FScale := 1;
  end;
end;

procedure TVectorClock.Clock(Scale: Float);

  procedure MatrixScale;
  begin
    Canvas.Matrix.Identity;
    Canvas.Matrix.Scale(Scale, Scale);
  end;

  procedure Rings;
  var
    R: TRectF;
  begin
    FRingBrush.NearStop.Offset := 0;
    FRingBrush.FarStop.Offset := 1;
    R := NewRectF(-300, -300, 600, 600);
    R.Move(140, -140);
    FRingBrush.Rect := R;
    FRingBrush.NearStop.Color := $FFF0F0F0;
    FRingBrush.FarStop.Color := $FF303030;
    Canvas.Circle(0, 0, 240);
    Canvas.Stroke($20000000, 20, True);
    Canvas.Fill(FRingBrush);
    R := NewRectF(-250, -250, 500, 500);
    R.Move(200, -200);
    FRingBrush.Rect := R;
    FRingBrush.NearStop.Color := $FF303030;
    FRingBrush.FarStop.Color := $FFD0D0D0;
    Canvas.Circle(0, 0, 205);
    Canvas.Fill(FRingBrush);
    Canvas.Circle(0, 0, 195);
    Canvas.Stroke($40505050, 1);
  end;

  procedure Face;
  var
    R: TRectF;
  begin
    FRingBrush.NearStop.Offset := 0;
    FRingBrush.FarStop.Offset := 1;
    R := NewRectF(-150, -150, 300, 300);
    R.Move(-50, 50);
    FRingBrush.Rect := R;
    FRingBrush.NearStop.Color := $FFFFFFFF;
    FRingBrush.FarStop.Color := $FFAAAAAA;
    Canvas.Circle(0, 0, 195);
    Canvas.Fill(FRingBrush);
  end;

  procedure BigTick(Color: TColorF);
  const
    BigSegment = PI / 6;
    SmallSegment = BigSegment / 5;
  var
    A, B, C: TPointF;
    I: Integer;
  begin
    A := NewPointF(0, 170);
    B := NewPointF(0, 185);
    Canvas.BeginPath;
    for I := 0 to 11 do
    begin
      C := A.Rotate(I * BigSegment);
      Canvas.MoveTo(C.X, C.Y);
      C := B.Rotate(I * BigSegment);
      Canvas.LineTo(C.X, C.Y);
    end;
    FTickPen.Width := 5;
    Canvas.Stroke(FTickPen);
    A := NewPointF(0, 177);
    B := NewPointF(0, 185);
    for I := 0 to 59 do
    begin
      C := A.Rotate(I * SmallSegment);
      Canvas.MoveTo(C.X, C.Y);
      C := B.Rotate(I * SmallSegment);
      Canvas.LineTo(C.X, C.Y);
    end;
    FTickPen.Width := 2.5;
    Canvas.Stroke(FTickPen);
  end;

  procedure LittleTick(Color: TColorF);
  const
    BigSegment = PI / 6;
  var
    A, B, C: TPointF;
    I: Integer;
  begin
    Canvas.Circle(0, 0, 22);
    Canvas.Stroke($06000000, 1, True);
    Canvas.Fill($06000000);
    A := NewPointF(0, 15);
    B := NewPointF(0, 20);
    Canvas.BeginPath;
    for I := 0 to 11 do
    begin
      C := A.Rotate(I * BigSegment);
      Canvas.MoveTo(C.X, C.Y);
      C := B.Rotate(I * BigSegment);
      Canvas.LineTo(C.X, C.Y);
    end;
    FTickPen.Width := 1.5;
    Canvas.Stroke(FTickPen);
  end;

  procedure MicroHand;
  begin
    Canvas.MoveTo(1.5, 2);
    Canvas.LineTo(0.75, -16);
    Canvas.LineTo(0, -17);
    Canvas.LineTo(-0.75, -16);
    Canvas.LineTo(-1.5, 2);
    Canvas.LineTo(0, 3);
    Canvas.ClosePath;
    Canvas.Fill($FF202060);
  end;

  procedure HourHand(Color: TColorF);
  begin
    Canvas.MoveTo(5, 25);
    Canvas.LineTo(5, -100);
    Canvas.LineTo(-5, -100);
    Canvas.LineTo(-5, 25);
    Canvas.Fill(Color);
  end;

  procedure MinuteHand(Color: TColorF);
  begin
    Canvas.MoveTo(3.25, 35);
    Canvas.LineTo(3.25, -150);
    Canvas.LineTo(-3.25, -150);
    Canvas.LineTo(-3.25, 35);
    Canvas.Fill(Color);
  end;

  procedure SecondHand(Color: TColorF);
  begin
    Canvas.MoveTo(0, 40);
    Canvas.LineTo(2.5, 40);
    Canvas.LineTo(0.7, -136);
    Canvas.LineTo(-0.7, -136);
    Canvas.LineTo(-2.5, 40);
    Canvas.ClosePath;
    Canvas.MoveTo(0.7, -144);
    Canvas.LineTo(0.5, -180);
    Canvas.LineTo(-0.5, -180);
    Canvas.LineTo(-0.7, -144);
    Canvas.ClosePath;
    Canvas.Circle(0, 0, 7.5);
    Canvas.Fill(Color);
    Canvas.Circle(0, -140, 4);
    Canvas.Stroke(Color, 1.3);
  end;

  procedure SecondHole;
  begin
    Canvas.Circle(0, 0, 4);
    Canvas.Fill(colorHand);
  end;

  procedure Lens;
  var
    R: TRectF;
  begin
    Canvas.BlendMode := blendLighten;
    R := NewRectF(-300, -300, 600, 600);
    R.Move(110, 110);
    FRingBrush.Rect := R;
    FRingBrush.NearStop.Color := 0;
    FRingBrush.NearStop.Offset := 0.5;
    FRingBrush.FarStop.Color := $30FFFFFF;
    FRingBrush.FarStop.Offset := 0.505;
    Canvas.Circle(0, 0, 240);
    Canvas.Fill(FRingBrush);
    Canvas.BlendMode := blendAdditive;
    R := NewRectF(-500, -500, 1000, 1000);
    R.Move(-275, -225);
    FRingBrush.Rect := R;
    FRingBrush.NearStop.Color := 0;
    FRingBrush.NearStop.Offset := 0.5;
    FRingBrush.FarStop.Color := $05FFFFFF;
    FRingBrush.FarStop.Offset := 0.503;
    Canvas.Circle(0, 0, 240);
    Canvas.Fill(FRingBrush);
    Canvas.BlendMode := blendAlpha;
  end;

  procedure RenderClock;
  var
    H, M, S, MS: Word;
    X, Y: Float;
  begin
    NowTime(H, M, S, MS);
    X := FClock.Width / 2;
    Y := FClock.Height / 2;
    { Draw rings and face}
    MatrixScale;
    Canvas.Matrix.Translate(X, Y);
    Rings;
    Face;
    { Draw ticks }
    BigTick(colorDarkGray);
    Canvas.Matrix.Translate(100 * Scale * Width / StudioWidth, 0);
    LittleTick(colorDarkGray);

    MatrixScale;
    Canvas.Matrix.Rotate(MS / 1000 * PI * 2);
    Canvas.Matrix.Translate(X, Y);
    Canvas.Matrix.Translate(100 * Scale * Width / StudioWidth, 0);
    MicroHand;
    { Draw hours }
    MatrixScale;
    Canvas.Matrix.Rotate((H + M / 60 + S / 3600) / 12 * PI * 2);
    Canvas.Matrix.Translate(X, Y);
    Canvas.Matrix.Translate(3 * Scale, 2 * Scale);
    HourHand(colorShadow);
    Canvas.Matrix.Translate(-3 * Scale, -2 * Scale);
    HourHand(colorHand);
    { Draw minutes }
    MatrixScale;
    Canvas.Matrix.Rotate((M + S / 60 + MS / 60000) / 60 * PI * 2);
    Canvas.Matrix.Translate(X, Y);
    Canvas.Matrix.Translate(3 * Scale, 2 * Scale);
    MinuteHand(colorShadow);
    Canvas.Matrix.Translate(-3 * Scale, -2 * Scale);
    MinuteHand(colorHand);
    { Draw seconds }
    MatrixScale;
    Canvas.Matrix.Rotate((S + MS / 1000) / 60 * PI * 2);
    Canvas.Matrix.Translate(X, Y);
    Canvas.Matrix.Translate(3 * Scale, 2 * Scale);
    SecondHand(colorShadow);
    Canvas.Matrix.Translate(-3 * Scale, -2 * Scale);
    SecondHand(colorSecondHand);
    SecondHole;
    { Draw lens }
    MatrixScale;
    Canvas.Matrix.Translate(X, Y);
    Canvas.Matrix.Translate(3 * Scale, 2 * Scale);
    Lens;
  end;

const
  Size = 500;
var
  S: LongWord;
begin
  S := Round(Scale * Size);
  if FClock.Width <> S then
    FClock.Resize(S, S);
  FClock.Bind;
  Canvas.Clear;
  RenderClock;
  FClock.Unbind;
end;

function Divide(const Quotient, Divisor: Extended): Extended;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Round(Quotient / Divisor) * Divisor;
end;

function Remainder(const Quotient, Divisor: Extended): Extended;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Quotient - (Trunc(Quotient) div Trunc(Divisor)) * Divisor;
end;

procedure TVectorClock.Render(Width, Height: Integer; const Time: Double);
const
  Speed = 50;
var
  I, J: Integer;
  X, Y: Float;
  T: Double;
begin
  inherited Render(Width, Height, Time);
  ScaleToStudio;
  Canvas.Rect(ClientRect);
  FWorldBrush.Angle := Pi / -12;
  FWorldBrush.Offset := NewPointF(Time * 20, Time * -10);
  Canvas.Fill(FWorldBrush);
  Clock(FScale);
  T := Time;
  Canvas.Matrix.RotateAt(T / 16, StudioWidth / 2, StudioHeight / 2);
  for I := -20 to 20 do
  begin
    X := (StudioWidth - FClock.Width) / 2 + I * (FClock.Width + 100 * FScale);
      {if X < -(FClock.Width + 100 * FScale) then
        Continue;
      if X > StudioWidth then
        Continue;}
    for J := -20 to 20 do
    begin
      if Abs(I) mod 2 = 1 then
      begin
        Y := (StudioHeight - FClock.Height) / 2 + J * (FClock.Height + 100 * FScale) +
          -T * Speed * FScale;
        while Y < -FClock.Height * 10 do
          Y := Y + (FClock.Height + 100 * FScale) * 21;
        {if Y > StudioHeight then
          Continue;}
        Canvas.DrawImage(FClock, X, Y);
      end
      else
      begin
        Y := (StudioHeight - FClock.Height) / 2 + J * (FClock.Height + 100 * FScale) +
          T * Speed * FScale;
        while Y > StudioHeight * 1.5 do
          Y := Y - (FClock.Height + 100 * FScale) * 21;
        {if Y < -(FClock.Height + 100 * FScale) then
          Continue;}
        Canvas.DrawImage(FClock, X, Y);
      end;
    end;
  end;
end;

end.

