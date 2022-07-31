unit Tiny.Types;

{$i tiny.inc}

interface

{ TPointF }

type
  TGetProcAddress = function(ProcName: PChar): Pointer; cdecl;

  Float = Single;
  PFloat = ^Float;
  LargeInt = Int64;
  PLargeInt = ^LargeInt;
  LargeWord = QWord;
  PLargeWord = ^LargeWord;
  HFile = Pointer;

  TPointF = record
  public
    class operator Negative(const A: TPointF): TPointF; inline;
    class operator Equal(const A, B: TPointF): Boolean; inline;
    class operator NotEqual(const A, B: TPointF): Boolean; inline;
    class operator Add(const A, B: TPointF): TPointF; inline;
    class operator Subtract(const A, B: TPointF): TPointF; inline;
    class operator Multiply(const A, B: TPointF): TPointF; inline;
    class operator Multiply(A: Float; const B: TPointF): TPointF; inline;
    class operator Multiply(const A: TPointF; B: Float): TPointF; inline;
    class operator Divide(a: Float; const B: TPointF): TPointF; inline;
  public
    X, Y: Float;
    function Rotate(Angle: Float): TPointF;
    function Distance: Float; overload;
    function Distance(const Point: TPointF): Float; overload;
    procedure Move(X, Y: Float); inline;
    function Mix(const Point: TPointF; Percent: Float): TPointF;
    function Round(Width: LongWord = 1): TPointF;
    function PointAtDistance(const A: TPointF; Distance: Float): TPointF;
    function PointAtMix(const A: TPointF; Percent: Float): TPointF;
    function Normal(const A: TPointF; Scale: Float = 1): TPointF;
    procedure Normalize;
    function NormalizeAt: TPointF;
    function NormalAtDistance(const A: TPointF; Distance: Float; Scale: Float = 1): TPointF;
    function NormalAtMix(const A: TPointF; Percent: Float; Scale: Float = 1): TPointF;
  end;
  PPointF = ^TPointF;

  TSizeF = TPointF;
  PSizeF = PPointF;

{ TPointD }

  TPointD = record
  public
    class operator Implicit(const A: TPointD): TPointF; inline;
    class operator Implicit(const A: TPointF): TPointD; inline;
    class operator Negative(const A: TPointD): TPointD; inline;
    class operator Equal(const A, B: TPointD): Boolean; inline;
    class operator NotEqual(const A, B: TPointD): Boolean; inline;
    class operator Add(const A, B: TPointD): TPointD; inline;
    class operator Subtract(const A, B: TPointD): TPointD; inline;
    class operator Multiply(const A, B: TPointD): TPointD; inline;
    class operator Multiply(A: Double; const B: TPointD): TPointD; inline;
    class operator Multiply(const A: TPointD; B: Double): TPointD; inline;
    class operator Divide(a: Double; const B: TPointD): TPointD; inline;
  public
    X, Y: Double;
    function Rotate(Angle: Double): TPointD;
    function Distance: Double; overload;
    function Distance(const Point: TPointD): Double; overload;
    procedure Move(X, Y: Double); inline;
    function Mix(const Point: TPointD; Percent: Double): TPointD;
    function Round(Width: LongWord = 1): TPointD;
    function PointAtDistance(const A: TPointD; Distance: Double): TPointD;
    function PointAtMix(const A: TPointD; Percent: Double): TPointD;
    function Normal(const A: TPointD; Scale: Double = 1): TPointD;
    procedure Normalize;
    function NormalizeAt: TPointD;
    function NormalAtDistance(const A: TPointD; Distance: Double; Scale: Double = 1): TPointD;
    function NormalAtMix(const A: TPointD; Percent: Double; Scale: Double = 1): TPointD;
  end;
  PPointD = ^TPointD;

{ TRectF }

  TRectF = record
  private
    function GetRight: Float;
    procedure SetRight(Value: Float);
    function GetBottom: Float;
    procedure SetBottom(Value: Float);
  public
    X, Y, Width, Height: Float;
    class function Create(Width, Height: Float): TRectF; static; overload;
    class function Create(X, Y, Width, Height: Float): TRectF; static; overload;
    procedure Clear;
    function IsEmpty: Boolean;
    function Contains(X, Y: Float): Boolean; inline;
    procedure Inflate(X, Y: Float); inline;
    procedure Move(X, Y: Float); inline;
    function MidPoint: TPointF;
    function Sector(S: Integer): TPointF;
    function Round(Width: LongWord = 1): TRectF;
    property Left: Float read X write X;
    property Top: Float read Y write Y;
    property Right: Float read GetRight write SetRight;
    property Bottom: Float read GetBottom write SetBottom;
  end;
  PRectF = ^TRectF;

implementation

{ TPointF }

class operator TPointF.Negative(const A: TPointF): TPointF;
begin
  Result.x := -a.x; Result.y := -a.y;
end;

class operator TPointF.Equal(const A, B: TPointF): Boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y);
end;

class operator TPointF.NotEqual(const A, B: TPointF): Boolean;
begin
  Result :=  (a.x <> b.x) or (a.y <> b.y);
end;

class operator TPointF.Add(const A, B: TPointF): TPointF;
begin
  Result.x := a.x + b.x; Result.y := a.y + b.y;
end;

class operator TPointF.Subtract(const A, B: TPointF): TPointF;
begin
  Result.x := a.x - b.x; Result.y := a.y - b.y;
end;

class operator TPointF.Multiply(const A, B: TPointF): TPointF;
begin
  Result.x := a.x * b.x; Result.y := a.y * b.y;
end;

class operator TPointF.Multiply(A: Float; const B: TPointF): TPointF;
begin
  Result.x := b.x * a; Result.y := b.y * a;
end;

class operator TPointF.Multiply(const A: TPointF; B: Float): TPointF;
begin
  Result.x := a.x * b; Result.y := a.y * b;
end;

class operator TPointF.Divide(a: Float; const B: TPointF): TPointF;
begin
  Result.x := a / b.x / a; Result.y := a / b.y;
end;

procedure SinCos(X: Extended; out S, C: Extended);
begin
  S := Sin(X);
  C := Cos(X);
end;

function TPointF.Rotate(Angle: Float): TPointF;
var
  S, C: Extended;
begin
  if Angle = 0 then
    Exit(Self);
  SinCos(Angle, S, C);
  Result.X := (C * Self.X) + (S * Self.Y);
  Result.Y := (C * Self.Y) - (S * Self.X);
end;

function TPointF.Distance: Float;
begin
  Result := Sqrt(X * X + Y * Y);
end;

function TPointF.Distance(const Point: TPointF): Float;
var
  X1, Y1: Double;
begin
  X1 := Point.X - X; Y1 := Point.Y - Y;
  Result := Sqrt(X1 * X1 + Y1 * Y1);
end;

procedure TPointF.Move(X, Y: Float);
begin
  Self.X := Self.X + X;
  Self.Y := Self.Y + Y;
end;

function TPointF.Mix(const Point: TPointF; Percent: Float): TPointF;
var
  C: Float;
begin
  C := 1 - Percent;
  Result.X := X * C + Point.X * Percent;
  Result.Y := Y * C + Point.Y * Percent;
end;

function TPointF.Round(Width: LongWord = 1): TPointF;
begin
  Result.X := Trunc(X);
  Result.Y := Trunc(Y);
  if Width mod 2 = 1 then
  begin
    Result.X := Result.X + 0.5;
    Result.Y := Result.Y + 0.5;
  end;
end;

function TPointF.PointAtDistance(const A: TPointF; Distance: Float): TPointF;
begin
  Result := Distance * (A - Self).NormalizeAt + Self;
end;

function TPointF.PointAtMix(const A: TPointF; Percent: Float): TPointF;
var
  Inv: Float;
begin
  Inv := 1 - Percent;
  Result.X := A.X * Percent + X * Inv;
  Result.Y := A.Y * Percent + Y * Inv;
end;

procedure TPointF.Normalize;
var
  D: Float;
begin
  D := Distance + 0.000001;
  D := 1 / D;
  X := X * D;
  Y := Y * D;
end;

function TPointF.NormalizeAt: TPointF;
var
  D: Float;
begin
  D := Distance + 0.000001;
  D := 1 / D;
  Result.X := X * D;
  Result.Y := Y * D;
end;

function TPointF.Normal(const A: TPointF; Scale: Float = 1): TPointF;
var
  T: Float;
begin
  Result := (Self - A).NormalizeAt * Scale;
  T := -Result.X;
  Result.X := Result.Y;
  Result.Y := T;
end;

function TPointF.NormalAtDistance(const A: TPointF; Distance: Float; Scale: Float = 1): TPointF;
begin
  Result := Normal(A, Scale) + NormalizeAt * Distance;
end;

function TPointF.NormalAtMix(const A: TPointF; Percent: Float; Scale: Float = 1): TPointF;
begin
  Result := Normal(A, Scale) + PointAtMix(A, Percent);
end;

{ TPointD }

class operator TPointD.Implicit(const A: TPointD): TPointF;
begin
  Result.X := A.X;
  Result.Y := A.Y;
end;

class operator TPointD.Implicit(const A: TPointF): TPointD;
begin
  Result.X := A.X;
  Result.Y := A.Y;
end;

class operator TPointD.Negative(const A: TPointD): TPointD;
begin
  Result.X := -A.X; Result.Y := -A.Y;
end;

class operator TPointD.Equal(const A, B: TPointD): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y);
end;

class operator TPointD.NotEqual(const A, B: TPointD): Boolean;
begin
  Result :=  (a.X <> b.X) or (a.Y <> b.Y);
end;

class operator TPointD.Add(const A, B: TPointD): TPointD;
begin
  Result.X := a.X + b.X; Result.Y := a.Y + b.Y;
end;

class operator TPointD.Subtract(const A, B: TPointD): TPointD;
begin
  Result.X := a.X - b.X; Result.Y := a.Y - b.Y;
end;

class operator TPointD.Multiply(const A, B: TPointD): TPointD;
begin
  Result.X := a.X * b.X; Result.Y := a.Y * b.Y;
end;

class operator TPointD.Multiply(A: Double; const B: TPointD): TPointD;
begin
  Result.X := b.X * a; Result.Y := b.Y * a;
end;

class operator TPointD.Multiply(const A: TPointD; B: Double): TPointD;
begin
  Result.X := a.X * b; Result.Y := a.Y * b;
end;

class operator TPointD.Divide(A: Double; const B: TPointD): TPointD;
begin
  Result.X := a / b.X / a; Result.Y := a / b.Y;
end;

function TPointD.Rotate(Angle: Double): TPointD;
var
  S, C: Extended;
begin
  if Angle = 0 then
    Exit(Self);
  SinCos(Angle, S, C);
  Result.X := (C * Self.X) + (S * Self.Y);
  Result.Y := (C * Self.Y) - (S * Self.X);
end;

function TPointD.Distance: Double;
begin
  Result := Sqrt(X * X + Y * Y);
end;

function TPointD.Distance(const Point: TPointD): Double;
var
  X1, Y1: Double;
begin
  X1 := Point.X - X; Y1 := Point.Y - Y;
  Result := Sqrt(X1 * X1 + Y1 * Y1);
end;

procedure TPointD.Move(X, Y: Double);
begin
  Self.X := Self.X + X;
  Self.Y := Self.Y + Y;
end;

function TPointD.Mix(const Point: TPointD; Percent: Double): TPointD;
var
  C: Double;
begin
  C := 1 - Percent;
  Result.X := X * C + Point.X * Percent;
  Result.Y := Y * C + Point.Y * Percent;
end;

function TPointD.Round(Width: LongWord = 1): TPointD;
begin
  Result.X := Trunc(X);
  Result.Y := Trunc(Y);
  if Width mod 2 = 1 then
  begin
    Result.X := Result.X + 0.5;
    Result.Y := Result.Y + 0.5;
  end;
end;

function TPointD.PointAtDistance(const A: TPointD; Distance: Double): TPointD;
begin
  Result := Distance * (A - Self).NormalizeAt + Self;
end;

function TPointD.PointAtMix(const A: TPointD; Percent: Double): TPointD;
var
  Inv: Double;
begin
  Inv := 1 - Percent;
  Result.X := A.X * Percent + X * Inv;
  Result.Y := A.Y * Percent + Y * Inv;
end;

procedure TPointD.Normalize;
var
  D: Double;
begin
  D := Distance + 0.000001;
  D := 1 / D;
  X := X * D;
  Y := Y * D;
end;

function TPointD.NormalizeAt: TPointD;
var
  D: Double;
begin
  D := Distance + 0.000001;
  D := 1 / D;
  Result.X := X * D;
  Result.Y := Y * D;
end;

function TPointD.Normal(const A: TPointD; Scale: Double = 1): TPointD;
var
  T: Double;
begin
  Result := (Self - A).NormalizeAt * Scale;
  T := -Result.X;
  Result.X := Result.Y;
  Result.Y := T;
end;

function TPointD.NormalAtDistance(const A: TPointD; Distance: Double; Scale: Double = 1): TPointD;
begin
  Result := Normal(A, Scale) + NormalizeAt * Distance;
end;

function TPointD.NormalAtMix(const A: TPointD; Percent: Double; Scale: Double = 1): TPointD;
begin
  Result := Normal(A, Scale) + PointAtMix(A, Percent);
end;

{ TRectF }

class function TRectF.Create(Width, Height: Float): TRectF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := Width;
  Result.Height := Height;
end;

class function TRectF.Create(X, Y, Width, Height: Float): TRectF;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Width := Width;
  Result.Height := Height;
end;

function TRectF.GetRight: Float;
begin
  Result := X + Width;
end;

procedure TRectF.SetRight(Value: Float);
begin
  Width := Value - X;
end;

function TRectF.GetBottom: Float;
begin
  Result := Y + Height;
end;

procedure TRectF.SetBottom(Value: Float);
begin
  Height := Value - Y;
end;

procedure TRectF.Clear;
begin
  X := 0; Y := 0; Width := 0; Height := 0;
end;

function TRectF.IsEmpty: Boolean;
begin
  Result := (Width = 0) or (Height = 0)
end;

function TRectF.Contains(X, Y: Float): Boolean;
begin
  Result := (X > Self.X) and (Y > Self.Y) and (X < Right) and (Y < Bottom);
end;

procedure TRectF.Inflate(X, Y: Float);
begin
  Self.X := Self.X - X;
  Self.Y := Self.Y - Y;
  Width := Width + X * 2;
  Height := Height + Y * 2;
end;

procedure TRectF.Move(X, Y: Float);
begin
  Self.X := Self.X + X;
  Self.Y := Self.Y + Y;
end;

function TRectF.MidPoint: TPointF;
begin
  Result.X := Width / 2 + X;
  Result.Y := Height / 2 + Y;
end;

function TRectF.Sector(S: Integer): TPointF;
begin
  Result.X := 0;
  Result.Y := 0;
  case S of
    1..3: Result.Y := Top;
    4..6: Result.Y := Height / 2 + Y;
    7..9: Result.Y := Bottom;
  end;
  case S of
    1,4,7: Result.X := Left;
    2,5,8: Result.X := Width / 2 + X;
    3,6,9: Result.X := Right;
  end;
end;

function TRectF.Round(Width: LongWord = 1): TRectF;
begin
  Result.X := Trunc(X);
  Result.Y := Trunc(Y);
  Result.Width := System.Round(Self.Width);
  Result.Height := System.Round(Height);
  if Width mod 2 = 1 then
  begin
    Result.X := Result.X + 0.5;
    Result.Y := Result.Y + 0.5;
  end;
end;

end.

