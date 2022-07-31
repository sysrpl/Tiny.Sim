unit Tiny.Geometry;

{$i tiny.inc}

interface

uses
  Tiny.System,
  Tiny.Types;

{$region basic geometry}
{ Returns true if a value is a power of two }
function IsPowerOfTwo(Value: Integer): Boolean;
{ Returns true if both w and h are powers of two }
function IsPowerOfTwos(W, H: Integer): Boolean;
{ Returns the next lowest power given a value  }
function PowerOfTwo(Value: Integer): Integer;
{$endregion}

{$region advanced geometry}
{ TVertMode described how vectors below are sequenced by by draw buffers }

type
  TVertMode = (
    vertPoints,
    vertLines,
    vertLineLoop,
    vertLineStrip,
    vertTriangles,
    vertTriangleStrip,
    vertTriangleFan,
    vertQuads);

  { TVec1 is a one component vector }
  TVec1 = Float;
  {doc ignore}
  PVec1 = ^TVec1;
  {doc ignore}
  TVec1Array = TArray<TVec1>;

{ TVec2 is a two component vector
  See also
  <link Overview.Tiny.Geometry.TVec2, TVec2 members> }

  TVec2 = record
  public
    {doc off}
    class operator Implicit(const Value: TPointF): TVec2; inline;
    class operator Implicit(const Value: TVec2): TPointF; inline;
    class operator Implicit(const Value: TPointD): TVec2; inline;
    class operator Implicit(const Value: TVecD): TPointF; inline;
    class operator Negative(const A: TVec2): TVec2; inline;
    class operator Equal(const A, B: TVec2): Boolean; inline;
    class operator NotEqual(const A, B: TVec2): Boolean; inline;
    class operator Add(const A, B: TVec2): TVec2; inline;
    class operator Subtract(const A, B: TVec2): TVec2; inline;
    class operator Multiply(const A, B: TVec2): TVec2; overload; inline;
    class operator Multiply(const A: TVec2; B: Float): TVec2; overload; inline;
    class operator Divide(const A, B: TVec2): TVec2; overload; inline;
    class operator Divide(const A: TVec2; B: Float): TVec2; overload; inline;
    function Equals(const Value: TVec2): Boolean; inline;
    function Angle: Float; overload;
    function Angle(X, Y: Float): Float; overload;
    function Angle(const V: TVec2): Float; overload;
    function Distance: Float; overload;
    function Distance(X, Y: Float): Float; overload;
    function Distance(const V: TVec2): Float; overload;
    procedure Offset(X, Y: Float); overload;
    procedure Offset(const V: TVec2); overload;
    function Move(X, Y: Float): TVec2; overload;
    function Move(const V: TVec2): TVec2; overload;
    procedure Normalize;
    function Normal: TVec2;
    function Binormal: TVec2;
    function Mid(const V: TVec2): TVec2;
    function Extend(const V: TVec2; Dist: Float): TVec2;
    function Rotate(Angle: Float): TVec2; overload;
    function Rotate(const V: TVec2; Angle: Float): TVec2; overload;
    {doc on}
  public
    case Integer of
      0: (X, Y: Float);
      1: (S, T: Float);
      2: (Vec1: TVec1);
      3: (V: array[0..1] of Float);
  end;
  {doc ignore}
  PVec2 = ^TVec2;
  {doc ignore}
  TVec2Array = TArray<TVec2>;
  { A series of 2d vectors }
  TPolygon2 = TVec2Array;

{ TVec3 is a three component vector
  See also
  <link Overview.Tiny.Geometry.TVec3, TVec3 members> }

  TVec3 = record
  public
    {doc off}
    class operator Negative(const A: TVec3): TVec3; inline;
    class operator Equal(const A, B: TVec3): Boolean; inline;
    class operator NotEqual(const A, B: TVec3): Boolean; inline;
    class operator Add(const A, B: TVec3): TVec3; inline;
    class operator Subtract(const A, B: TVec3): TVec3; inline;
    class operator Multiply(const A, B: TVec3): TVec3; overload; inline;
    class operator Multiply(const A: TVec3; B: Float): TVec3; overload; inline;
    class operator Divide(const A, B: TVec3): TVec3; overload; inline;
    class operator Divide(const A: TVec3; B: Float): TVec3; overload; inline;
    function Equals(const Value: TVec3): Boolean; inline;
    function Cross(const V: TVec3): TVec3;
    function Dot(const V: TVec3): Float;
    function Distance: Float;
    procedure Normalize;
    {doc on}
  public
    case Integer of
      0: (X, Y, Z: Float);
      1: (R, G, B: Float);
      2: (Red, Green, Blue: Float);
      3: (Heading, Pitch, Roll: Float);
      4: (Hue, Saturation, Lightness: Float);
      5: (Vec1: TVec1);
      6: (Vec2: TVec2);
      7: (V: array[0..2] of Float);
  end;
  {doc ignore}
  PVec3 = ^TVec3;
  {doc ignore}
  TVec3Array = TArray<TVec3>;
  { TDirection represents a heading, pitch, and roll }
  TDirection = TVec3;

{ TVec4 is a four component vector
  See also
  <link Overview.Tiny.Geometry.TVec4, TVec4 members> }

  TVec4 = record
  public
    {doc off}
    class operator Negative(const A: TVec4): TVec4; inline;
    class operator Equal(const A, B: TVec4): Boolean; inline;
    class operator NotEqual(const A, B: TVec4): Boolean; inline;
    class operator Multiply(const A, B: TVec4): TVec4; overload; inline;
    class operator Multiply(const A: TVec4; B: Float): TVec4; overload; inline;
    class operator Divide(const A, B: TVec4): TVec4; overload; inline;
    class operator Divide(const A: TVec4; B: Float): TVec4; overload; inline;
    function Equals(const Value: TVec4): Boolean;
    {doc on}
  public
    case Integer of
      0: (X, Y, Z, W: Float);
      1: (R, G, B, A: Float);
      2: (Red, Green, Blue, Alpha: Float);
      3: (S0, T0, S1, T1: Float);
      4: (Vec1: TVec1);
      5: (Vec2: TVec2);
      6: (Vec3: TVec3);
      7: (V: array[0..3] of Float);
  end;
  {doc ignore}
  PVec4 = ^TVec4;
  {doc ignore}
  TVec4Array = TArray<TVec4>;

function Vec(const X, Y: Float): TVec2; overload; inline;
function Vec(const X, Y, Z: Float): TVec3; overload; inline;
function Vec(const X, Y, Z, W: Float): TVec4; overload; inline;
function Vec(const V: TVec2; Z: Float): TVec3; overload; inline;
function Vec(const V: TVec2; Z, W: Float): TVec4; overload; inline;
function Vec(const V: TVec3; W: Float): TVec4; overload; inline;
function Vec2(X: Float): TVec2; overload; inline;
function Vec2(const X, Y: Float): TVec2; overload; inline;
function Vec2(const V: TVec3): TVec2; overload; inline;
function Vec2(const V: TVec4): TVec2; overload; inline;
function Vec3(X: Float): TVec3; overload; inline;
function Vec3(const X, Y, Z: Float): TVec3; overload; inline;
function Vec3(const V: TVec4): TVec3; overload; inline;
function Vec4(X: Float): TVec4; overload; inline;
function Vec4(const X, Y, Z, W: Float): TVec4; overload; inline;
function PolygonBounds(const P: TPolygon2): TRectF;
function PointInPolygon(X, Y: Float; const P: TPolygon2): Boolean;

type
  TTransformOrder = (toTRS, toRST, toSTR, toTSR, toSRT, toRTS);
  TRotationOrder = (roXYZ, roYZX, roZXY, roXZY, roZYX, roYXZ);
  TMatrixOrder = (moPrepend, moAppend);

{ TMatrix4x4 is used to transform vertices
  See also
  <link Overview.Tiny.Geometry.TMatrix4x4, TMatrix4x4 members> }

  TMatrix4x4 = record
  public
    class operator Equal(const A, B: TMatrix4x4): Boolean;
    class operator NotEqual(const A, B: TMatrix4x4): Boolean;
    class operator Add(const A, B: TMatrix4x4): TMatrix4x4;
    class operator Subtract(const A, B: TMatrix4x4): TMatrix4x4;
    class operator Multiply(const A: TMatrix4x4; const B: TVec2): TVec2; overload;
    class operator Multiply(const A: TMatrix4x4; const B: TVec3): TVec3; overload;
    class operator Multiply(const A, B: TMatrix4x4): TMatrix4x4; overload;
    class operator Divide(const A, B: TMatrix4x4): TMatrix4x4;
    function Equals(const Value: TMatrix4x4): Boolean;
    procedure Identity;
    function IsIdentity: Boolean;
    function CanInvert: Boolean;
    function Invert: Boolean;
    procedure Transpose;
    procedure Rotate(X, Y, Z: Float); overload;
    procedure Rotate(X, Y, Z: Float; Order: TRotationOrder); overload;
    procedure RotateAt(X, Y, Z: Float; const Pivot: TVec3); overload;
    procedure RotateAt(X, Y, Z: Float; const Pivot: TVec3; Order: TRotationOrder); overload;
    procedure Scale(X, Y, Z: Float);
    procedure ScaleAt(X, Y, Z: Float; const Pivot: TVec3);
    procedure Translate(X, Y, Z: Float);
    function Transform(const V: TVec2): TVec2; overload;
    function Transform(const V: TVec3): TVec3; overload;
    function Transform(const M: TMatrix4x4): TMatrix4x4; overload;
    procedure Perspective(FoV, AspectRatio, NearPlane, FarPlane: Float);
    procedure Frustum(Left, Right, Top, Bottom, NearPlane, FarPlane: Float);
    procedure LookAt(Eye, Center, Up: TVec3);
    case Integer of
      0: (M: array[0..3, 0..3] of Float);
      1: (M0, M1, M2, M3: array[0..3] of Float);
      2: (V: array[0..15] of Float);
  end;
  {doc ignore}
  PMatrix4x4 = ^TMatrix4x4;
  { Alias for TMatrix4x4 }
  TMatrix = TMatrix4x4;
  {doc ignore}
  PMatrix = TMatrix;

{ TQuaternion }

  TQuaternion = record
    class operator Explicit(const A: TQuaternion): TMatrix4x4;
    class operator Implicit(const A: TMatrix4x4): TQuaternion;
    class operator Multiply(const A, B: TQuaternion): TQuaternion;
    procedure Conjugate;
    procedure Normalize;
    case Integer of
      0: (W, X, Y, Z: Float);
      1: (Q: array[0..3] of Float);
  end;
  PQuaternion = ^TQuaternion;

{ TSlope2 }

  TSlope2 = record
    Undefined: Boolean;
    Ratio: Float;
    Intercept: Float;
  end;

{ TLine2 represents a 2d line }

  TLine2 = record
  public
    { Slope of the line }
    function Slope: TSlope2;
    { Normal heading of the line }
    function Normal: TVec2;
    { Binormal is a normal perpendicular to the line }
    function Binormal: TVec2;
    { Find a point at a distance along the line }
    function FindPoint(Dist: Float): TVec2;
    { Total distance along the line }
    function Distance: Float;
    { Determine if a line intersects another line }
    function Intersects(const Line: TLine2): Boolean; overload;
    { Determine if a line intersects another line }
    function Intersects(const Line: TLine2; out Point: TVec2): Boolean; overload;
  public
    case Integer of
      0: (P0, P1: TVec2);
      1: (P: array[0..1] of TVec2);
  end;

{ Build a 2d line given two points }
function NewLine2(const P0, P1: TVec2): TLine2; overload; inline;
{ Build a 2d line given four components }
function NewLine2(X0, Y0, X1, Y1: Float): TLine2; overload; inline;

{ TAngleCurve describes how a line curves to a point following the line end }

type
  TLineCurve = (
    { Exterior line angle is greater than 180 degrees and needs miter or round cap }
    curveConvex,
    { Exterior line angle is less that 180 degrees and needs intersection }
    curveConcave,
    { Exterior line angle is straight and needs nothing }
    curveNone);

{ Detect how point C coming after line would curve a line }
function LineCurve(const Line: TLine2; const C: TVec2): TLineCurve; overload;
function LineCurve(const P0, P1, C: TVec2): TLineCurve; overload;
{ Detect the intersection of 2 line segments }
function LineIntersect(const A, B: TLine2; out V: TVec2): Boolean; overload;
function LineIntersect(const P0, P1, P2, P3: TVec2; out V: TVec2): Boolean; overload;

{ TCurve2 is a series of points which approximate a 2d bezier curve
  See also
  <link Overview.Tiny.Geometry.TCurve2, TCurve2 members>
  <link Tiny.Geometry.TBezier2, TBezier2 record> }

type
  TCurve2 = record
  public
    class operator Implicit(const Value: TPolygon2): TCurve2;
    class operator Explicit(const Value: TCurve2): TPolygon2;
    { Transform a curve by a matrix }
    class operator Multiply(const A: TMatrix4x4; const B: TCurve2): TCurve2;
    { Find a heading normal at a distance along the curve }
    function FindNormal(Dist: Float): TVec2;
    { Find a point at a distance along the curve }
    function FindPoint(Dist: Float): TVec2;
  public
    { Total distance over the curve }
    Distance: Float;
    { Distances between each point }
    D: TVec1Array;
    { Approximated points over the curve }
    P: TVec2Array;
  end;

{ TBezier2 represents a 2d cubic bezier curve
  Remarks
  Points 0 and 3 are the end points while points 1 and 2 are the control points
  See also
  <link Overview.Tiny.Geometry.TBezier2, TBezier2 members>
  <link Tiny.Geometry.TCurve2, TCurve2 record>
  <exref target="http://en.wikipedia.org/wiki/Bezier_curve">External: Bezier curve on wikipedia</exref> }

  TBezier2 = record
  private
    function InternalFlatten(Count: Integer): TCurve2;
  public
    { Flatten into a 2d approximation of the curve with count points
      Remarks
      If count < 2 flatten will pick an optimal count based on the curve distance }
    function Flatten(Count: Integer = 0): TCurve2;
  public
    case Integer of
      0: (P0, P1, P2, P3: TVec2);
      1: (P: array[0..3] of TVec2);
  end;

{ Build a 2d cubic bezier curve given four points }

function NewBezier2(const P0, P1, P2, P3: TVec2): TBezier2;

{ A plane in 3d space as defined by 3 points }

type
  TPlane3 = record
    { The points defining the plane }
    V: array[0..2] of TVec3;
    { The direction the place faces }
    function Normal: TVec3;
    { The point where a ray intersects the place }
    function RayIntersect(Ray: TVec3; out Point: TVec3): Boolean;
  end;

const
  StockDirection: TDirection = (
    Heading: 0; Pitch: 0; Roll: 0);
  StockMatrix: TMatrix = (M: (
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1)));
  StockQuaternion: TQuaternion = (W: 1; X: 0; Y: 0; Z: 0);

var
  DefaultRotationOrder: TRotationOrder = roZXY;
{$endregion}

implementation

{$region basic geometry}
function IsPowerOfTwo(Value: Integer): Boolean;
var
  T: Integer;
begin
  T := $1;
  repeat
    Result := Value = T;
    T := T shl 1;
  until Result or (T = $10000000);
end;

function IsPowerOfTwos(W, H: Integer): Boolean;
begin
  Result := IsPowerOfTwo(W) and IsPowerOfTwo(H);
end;

function PowerOfTwo(Value: Integer): Integer;
begin
  Result := Value;
  if IsPowerOfTwo(Result) then Exit;
  Result := $1;
  while Value > Result do
    Result := Result shl 1;
  Result := Result shr 1;
end;
{$endregion}

{$region advanced geometry}
{ TVec2 }
class operator TVec2.Implicit(const Value: TPointF): TVec2;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TVec2.Implicit(const Value: TVec2): TPointF;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TVec2.Negative(const A: TVec2): TVec2;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
end;

class operator TVec2.Equal(const A, B: TVec2): Boolean;
begin
  Result := FloatEqual(A.X, B.X) and FloatEqual(A.Y, B.Y);
end;

class operator TVec2.NotEqual(const A, B: TVec2): Boolean;
begin
  Result := not (FloatEqual(A.X, B.X) and FloatEqual(A.Y, B.Y));
end;

class operator TVec2.Add(const A, B: TVec2): TVec2;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TVec2.Subtract(const A, B: TVec2): TVec2;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TVec2.Multiply(const A, B: TVec2): TVec2;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
end;

class operator TVec2.Multiply(const A: TVec2; B: Float): TVec2;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
end;

class operator TVec2.Divide(const A, B: TVec2): TVec2;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
end;

class operator TVec2.Divide(const A: TVec2; B: Float): TVec2;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
end;

function TVec2.Equals(const Value: TVec2): Boolean;
begin
  Result := Self = Value;
end;

function TVec2.Distance: Float;
begin
  Result := Sqrt(X * X + Y * Y);
end;

function TVec2.Angle: Float;
const
  Origin: TVec2 = (X: 0; Y: 0);
begin
  Result := Origin.Angle(Self);
end;

function TVec2.Angle(X, Y: Float): Float;
begin
  Result := Angle(Vec2(X, Y));
end;

function TVec2.Angle(const V: TVec2): Float;
var
  X, Y: Float;
begin
  X := Self.X - V.X;
  Y := Self.Y - V.Y;
  if X = 0 then
    if Y < 0 then
      Exit(Pi)
    else
      Exit(0);
  Result := Arctan(Y / X) + Pi / 2;
  if X > 0 then
    Result := Result + Pi;
end;

function TVec2.Distance(X, Y: Float): Float;
begin
  Result := (Self - Vec2(X, Y)).Distance;
end;

function TVec2.Distance(const V: TVec2): Float;
begin
  Result := (Self - V).Distance;
end;

procedure TVec2.Offset(X, Y: Float);
begin
  Self.X :=  Self.X + X;
  Self.Y :=  Self.Y + Y;
end;

procedure TVec2.Offset(const V: TVec2);
begin
  X :=  X + V.X;
  Y :=  Y + V.Y;
end;

function TVec2.Move(X, Y: Float): TVec2;
begin
  Result.X := Self.X + X;
  Result.Y := Self.Y + Y;
end;

function TVec2.Move(const V: TVec2): TVec2;
begin
  Result.X := X + V.X;
  Result.Y := Y + V.Y;
end;

procedure TVec2.Normalize;
var
  Ratio: Float;
begin
  Ratio := Sqrt(X * X + Y * Y);
  if Ratio > 0 then
  begin
    Ratio := 1 / Ratio;
    X := X * Ratio;
    Y := Y * Ratio;
  end
  else
  begin
    X := 0;
    Y := 0;
  end;
end;

function TVec2.Normal: TVec2;
begin
  Result := Self;
  Result.Normalize;
end;

function TVec2.Binormal: TVec2;
var
  F: Float;
begin
  Result := Normal;
  F := Result.X;
  Result.X := Result.Y;
  Result.Y := -F;
end;

function TVec2.Mid(const V: TVec2): TVec2;
begin
  Result.X := (X + V.X) / 2;
  Result.Y := (Y + V.Y) / 2;
end;

function TVec2.Extend(const V: TVec2; Dist: Float): TVec2;
var
  X, Y, R: Float;
begin
  X := Self.X - V.X;
  Y := Self.Y - V.Y;
  R := Sqrt(X * X + Y * Y);
  if R = 0 then
    Exit(Self);
  R := 1 / R;
  Result.X := Self.X - X * R * Dist;
  Result.Y := Self.Y - Y * R * Dist;
end;

function TVec2.Rotate(Angle: Float): TVec2;
var
  S, C: Extended;
begin
  if Angle = 0 then
    Exit(Self);
  SinCos(Angle, S, C);
  Result.X := (C * Self.X) + (S * Self.Y);
  Result.Y := (C * Self.Y) - (S * Self.X);
end;

function TVec2.Rotate(const V: TVec2; Angle: Float): TVec2;
var
  S, C: Extended;
  X, Y: Float;
begin
  if Angle = 0 then
    Exit(V);
  Angle := Angle / 180 * Pi;
  SinCos(Angle, S, C);
  X := Self.Y * S - Self.X * C + Self.X;
  Y := -Self.X * S - Self.Y * C + Self.Y;
  Result.X := V.X * C - V.Y * S + X;
  Result.Y := V.X * S + V.Y * C + Y;
end;

{ TVec3 }

class operator TVec3.Negative(const A: TVec3): TVec3;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
end;

class operator TVec3.Equal(const A, B: TVec3): Boolean;
begin
  Result := FloatEqual(A.X, B.X) and FloatEqual(A.Y, B.Y) and FloatEqual(A.Z, B.Z);
end;

class operator TVec3.NotEqual(const A, B: TVec3): Boolean;
begin
  Result := not (FloatEqual(A.X, B.X) and FloatEqual(A.Y, B.Y) and FloatEqual(A.Z, B.Z));
end;

class operator TVec3.Add(const A, B: TVec3): TVec3;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TVec3.Subtract(const A, B: TVec3): TVec3;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;

class operator TVec3.Multiply(const A, B: TVec3): TVec3;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
end;

class operator TVec3.Multiply(const A: TVec3; B: Float): TVec3;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
end;

class operator TVec3.Divide(const A, B: TVec3): TVec3;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
  Result.Z := A.Z / B.Z;
end;

class operator TVec3.Divide(const A: TVec3; B: Float): TVec3;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
  Result.Z := A.Z / B;
end;

function TVec3.Equals(const Value: TVec3): Boolean;
begin
  Result := Self = Value;
end;

function TVec3.Cross(const V: TVec3): TVec3;
begin
  Result.X := (Y * V.Z) - (V.Y * Z);
  Result.Y := (Z * V.X) - (V.Z * X);
  Result.Z := (X * V.Y) - (V.X * Y);
end;

function TVec3.Dot(const V: TVec3): Float;
begin
  Result := X * V.X + Y * V.Y + Z * V.Z;
end;

function TVec3.Distance: Float;
begin
  Result := Sqrt(X * X + Y * Y + Z + Z);
end;

procedure TVec3.Normalize;
var
  D: Float;
begin
  D := Sqrt(X * X + Y * Y + Z + Z);
  if D > 0 then
  begin
    D := 1 / D;
    X := X * D;
    Y := Y * D;
    Z := Z * D;
  end
  else
  begin
    X := 0;
    Y := 0;
    Z := 0;
  end;
end;

{ TVec4 }

class operator TVec4.Negative(const A: TVec4): TVec4;
begin
  Result.X := -A.X;
  Result.Y := -A.Y;
  Result.Z := -A.Z;
  Result.W := -A.W;
end;

class operator TVec4.Equal(const A, B: TVec4): Boolean;
begin
  Result := FloatEqual(A.X, B.X) and FloatEqual(A.Y, B.Y) and FloatEqual(A.Z, B.Z) and FloatEqual(A.W, B.W);
end;

class operator TVec4.NotEqual(const A, B: TVec4): Boolean;
begin
  Result := not (FloatEqual(A.X, B.X) and FloatEqual(A.Y, B.Y) and FloatEqual(A.Z, B.Z) and FloatEqual(A.W, B.W));
end;

function TVec4.Equals(const Value: TVec4): Boolean;
begin
  Result := Self = Value;
end;

class operator TVec4.Multiply(const A, B: TVec4): TVec4;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
  Result.W := A.W * B.W;
end;

class operator TVec4.Multiply(const A: TVec4; B: Float): TVec4;
begin
  Result.X := A.X * B;
  Result.Y := A.Y * B;
  Result.Z := A.Z * B;
  Result.W := A.W * B;
end;

class operator TVec4.Divide(const A, B: TVec4): TVec4;
begin
  Result.X := A.X / B.X;
  Result.Y := A.Y / B.Y;
  Result.Z := A.Z / B.Z;
  Result.W := A.W / B.W;
end;

class operator TVec4.Divide(const A: TVec4; B: Float): TVec4;
begin
  Result.X := A.X / B;
  Result.Y := A.Y / B;
  Result.Z := A.Z / B;
  Result.W := A.W / B;
end;

{ Vec }

function Vec(const X, Y: Float): TVec2;
begin
  Result.X := X; Result.Y := Y;
end;

function Vec(const X, Y, Z: Float): TVec3;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z;
end;

function Vec(const X, Y, Z, W: Float): TVec4;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z; Result.W := W;
end;

function Vec(const V: TVec2; Z: Float): TVec3;
begin
  Result.X := V.X; Result.Y := V.Y; Result.Z := Z;
end;

function Vec(const V: TVec2; Z, W: Float): TVec4;
begin
  Result.X := V.X; Result.Y := V.Y; Result.Z := Z; Result.W := W;
end;

function Vec(const V: TVec3; W: Float): TVec4;
begin
  Result.X := V.X; Result.Y := V.Y; Result.Z := V.Z; Result.W := W;
end;

function Vec2(X: Float): TVec2;
begin
  Result.X := X; Result.Y := X;
end;

function Vec2(const X, Y: Float): TVec2;
begin
  Result.X := X; Result.Y := Y;
end;

function Vec2(const V: TVec3): TVec2;
begin
  Result.X := V.X; Result.Y := V.Y;
end;

function Vec2(const V: TVec4): TVec2;
begin
  Result.X := V.X; Result.Y := V.Y;
end;

function Vec3(X: Float): TVec3;
begin
  Result.X := X; Result.Y := X; Result.Z := X;
end;

function Vec3(const X, Y, Z: Float): TVec3;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z;
end;

function Vec3(const V: TVec4): TVec3;
begin
  Result.X := V.X; Result.Y := V.Y; Result.Z := V.Z;
end;

function Vec4(X: Float): TVec4;
begin
  Result.X := X; Result.Y := X; Result.Z := X; Result.W := X;
end;

function Vec4(const X, Y, Z, W: Float): TVec4;
begin
  Result.X := X; Result.Y := Y; Result.Z := Z; Result.W := W;
end;

function PolygonBounds(const P: TPolygon2): TRectF;
var
  I: Integer;
begin
  if Length(P) < 1 then
    Exit(TRectF.Create(0, 0));
  Result := TRectF.Create(P[0].X, P[0].Y, 0, 0);
  for I := 1 to High(P) do
  begin
    if P[I].X < Result.Left then
      Result.Left := P[I].X
    else if P[I].X > Result.Right then
      Result.Right := P[I].X;
    if P[I].Y < Result.Top then
      Result.Top := P[I].Y
    else if P[I].Y > Result.Bottom then
      Result.Bottom := P[I].Y;
  end;
end;

function PointInPolygon(X, Y: Float; const P: TPolygon2): Boolean;
var
  XN, YN, XO, YO, X1, Y1, X2, Y2: Float;
  N, I: Integer;
begin
  Result := False;
  N := Length(P);
  if N < 3 then
    Exit;
  XO := P[N - 1].X;
  YO := P[N - 1].Y;
  for I := 0 to N - 1 do
  begin
    XN := P[I].X;
    YN := P[I].Y;
    if XN > XO then
    begin
      X1 := XO; X2 := XN; Y1 := YO; Y2 := YN;
    end
    else
    begin
      X1 := XN; X2 := XO; Y1 := YN; Y2 := YO;
    end;
    if ((XN < X) = (X <= XO)) and ((Y - Y1) * (X2 - X1) < (Y2 - Y1) * (X - X1)) then
      Result := not Result;
    XO := XN;
    YO := YN;
  end;
end;

function NewLine2(const P0, P1: TVec2): TLine2;
begin
  Result.P0 := P0;
  Result.P1 := P1;
end;

function NewLine2(X0, Y0, X1, Y1: Float): TLine2;
begin
  Result.P0.X := X0;
  Result.P0.Y := Y0;
  Result.P1.X := X1;
  Result.P1.Y := Y1;
end;

function LineCurve(const Line: TLine2; const C: TVec2): TLineCurve;
begin
  Result := LineCurve(Line.P0, Line.P1, C);
end;

function LineCurve(const P0, P1, C: TVec2): TLineCurve;
const
  Epsilon = 0.0001;
var
  F: Float;
begin
  F := (P1.X - P0.X) * (C.Y - P0.Y) - (P1.Y - P0.Y) * (C.X - P0.X);
  if F < -Epsilon then
    Result := curveConvex
  else if F > Epsilon then
    Result := curveConcave
  else
    Result := curveNone;
end;

function LineIntersect(const A, B: TLine2; out V: TVec2): Boolean;
begin
  Result := LineIntersect(A.P0, A.P1, B.P0, B.P1, V)
end;

// from https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect

function LineIntersect(const P0, P1, P2, P3: TVec2; out V: TVec2): Boolean;
var
  S1X, S1Y, S2X, S2Y: Float;
  S, T: Float;
begin
  V.X := 0;
  V.Y := 0;
  Result := False;
  if (P1.X = P2.X) and (P1.Y = P2.Y) then
    Exit;
  if (P0.X = P3.X) and (P0.Y = P3.Y) then
    Exit;
  S1X := P1.X - P0.X;
  S1Y := P1.Y - P0.Y;
  S2X := P3.X - P2.X;
  S2Y := P3.Y - P2.Y;
  S := (-S1Y * (P0.X - P2.X) + S1X * (P0.Y - P2.Y)) / (-S2X * S1Y + S1X * S2Y);
  T := (S2X * (P0.Y - P2.Y) - S2Y * (P0.X - P2.X)) / (-S2X * S1Y + S1X * S2Y);
  if (S >= 0) and (S <= 1) and (T >= 0) and (T <= 1) then
  begin
    V.X := P0.X + (T * S1X);
    V.Y := P0.Y + (T * S1Y);
    Result := True;
  end;
end;

{ TMatrix4x4 }

class operator TMatrix4x4.Equal(const A, B: TMatrix4x4): Boolean;
begin
  Result := MemCompare(A, B, SizeOf(TMatrix4x4));
end;

class operator TMatrix4x4.NotEqual(const A, B: TMatrix4x4): Boolean;
begin
  Result := not MemCompare(A, B, SizeOf(TMatrix4x4));
end;

class operator TMatrix4x4.Add(const A, B: TMatrix4x4): TMatrix4x4;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result.M[X, Y] := A.M[X, Y] + B.M[X, Y];
end;

class operator TMatrix4x4.Subtract(const A, B: TMatrix4x4): TMatrix4x4;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result.M[X, Y] := A.M[X, Y] - B.M[X, Y];
end;

class operator TMatrix4x4.Multiply(const A: TMatrix4x4; const B: TVec2): TVec2;
begin
  Result.X := A.M[0, 0] * B.X + A.M[1, 0] * B.Y + A.M[3, 0];
  Result.Y := A.M[0, 1] * B.X + A.M[1, 1] * B.Y + A.M[3, 1];
end;

class operator TMatrix4x4.Multiply(const A: TMatrix4x4; const B: TVec3): TVec3;
begin
  Result.X := A.M[0, 0] * B.X + A.M[1, 0] * B.Y + A.M[2, 0] * B.Z + A.M[3, 0];
  Result.Y := A.M[0, 1] * B.X + A.M[1, 1] * B.Y + A.M[2, 1] * B.Z + A.M[3, 1];
  Result.Z := A.M[0, 2] * B.X + A.M[1, 2] * B.Y + A.M[2, 2] * B.Z + A.M[3, 2];
end;

class operator TMatrix4x4.Multiply(const A, B: TMatrix4x4): TMatrix4x4;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result.M[X, Y] := A.M[0, Y] * B.M[X, 0] + A.M[1, Y] * B.M[X, 1] + A.M[2, Y] *
        B.M[X, 2] + A.M[3, Y] * B.M[X, 3];
end;

class operator TMatrix4x4.Divide(const A, B: TMatrix4x4): TMatrix4x4;
var
  X, Y: Integer;
begin
  for Y := 0 to 3 do
    for X := 0 to 3 do
      Result.M[X, Y] := A.M[0, Y] / B.M[X, 0] + A.M[1, Y] / B.M[X, 1] + A.M[2, Y] /
        B.M[X, 2];
end;

function TMatrix4x4.Equals(const Value: TMatrix4x4): Boolean;
begin
  Result := Self = Value;
end;

procedure TMatrix4x4.Identity;
begin
  Self := StockMatrix;
end;

function TMatrix4x4.IsIdentity: Boolean;
begin
  Result := Self = StockMatrix;
end;

function TMatrix4x4.CanInvert: Boolean;
var
  A0, A1, A2, A3, A4, A5, B0, B1, B2, B3, B4, B5: Float;
begin
  A0 := V[0] * V[5] - V[1] * V[4];
  A1 := V[0] * V[6] - V[2] * V[4];
  A2 := V[0] * V[7] - V[3] * V[4];
  A3 := V[1] * V[6] - V[2] * V[5];
  A4 := V[1] * V[7] - V[3] * V[5];
  A5 := V[2] * V[7] - V[3] * V[6];
  B0 := V[8] * V[13] - V[9] * V[12];
  B1 := V[8] * V[14] - V[10] * V[12];
  B2 := V[8] * V[15] - V[11] * V[12];
  B3 := V[9] * V[14] - V[10] * V[13];
  B4 := V[9] * V[15] - V[11] * V[13];
  B5 := V[10] * V[15] - V[11] * V[14];
  Result := A0 * B5 - A1 * B4 + A2 * B3 + A3 * B2 - A4 * B1 + A5 * B0 <> 0;
end;

function TMatrix4x4.Invert: Boolean;
var
  M: TMatrix4x4;
  A0, A1, A2, A3, A4, A5, B0, B1, B2, B3, B4, B5, D: Float;
begin
  A0 := V[0] * V[5] - V[1] * V[4];
  A1 := V[0] * V[6] - V[2] * V[4];
  A2 := V[0] * V[7] - V[3] * V[4];
  A3 := V[1] * V[6] - V[2] * V[5];
  A4 := V[1] * V[7] - V[3] * V[5];
  A5 := V[2] * V[7] - V[3] * V[6];
  B0 := V[8] * V[13] - V[9] * V[12];
  B1 := V[8] * V[14] - V[10] * V[12];
  B2 := V[8] * V[15] - V[11] * V[12];
  B3 := V[9] * V[14] - V[10] * V[13];
  B4 := V[9] * V[15] - V[11] * V[13];
  B5 := V[10] * V[15] - V[11] * V[14];
  D := A0 * B5 - A1 * B4 + A2 * B3 + A3 * B2 - A4 * B1 + A5 * B0;
  if D = 0 then
    Exit(False);
  M := Self;
  V[0] := M.V[5] * B5 - M.V[6] * B4 + M.V[7] * B3;
  V[4] := -M.V[4] * B5 + M.V[6] * B2 - M.V[7] * B1;
  V[8] := M.V[4] * B4 - M.V[5] * B2 + M.V[7] * B0;
  V[12] := -M.V[4] * B3 + M.V[5] * B1 - M.V[6] * B0;
  V[1] := -M.V[1] * B5 + M.V[2] * B4 - M.V[3] * B3;
  V[5] := M.V[0] * B5 - M.V[2] * B2 + M.V[3] * B1;
  V[9] := -M.V[0] * B4 + M.V[1] * B2 - M.V[3] * B0;
  V[13] := M.V[0] * B3 - M.V[1] * B1 + M.V[2] * B0;
  V[2] := M.V[13] * A5 - M.V[14] * A4 + M.V[15] * A3;
  V[6] := -M.V[12] * A5 + M.V[14] * A2 - M.V[15] * A1;
  V[10] := M.V[12] * A4 - M.V[13] * A2 + M.V[15] * A0;
  V[14] := -M.V[12] * A3 + M.V[13] * A1 - M.V[14] * A0;
  V[3] := -M.V[9] * A5 + M.V[10] * A4 - M.V[11] * A3;
  V[7] := M.V[8] * A5 - M.V[10] * A2 + M.V[11] * A1;
  V[11] := -M.V[8] * A4 + M.V[9] * A2 - M.V[11] * A0;
  V[15] := M.V[8] * A3 - M.V[9] * A1 + M.V[10] * A0;
  D := 1 / D;
  V[0] := V[0] * D;
  V[1] := V[1] * D;
  V[2] := V[2] * D;
  V[3] := V[3] * D;
  V[4] := V[4] * D;
  V[5] := V[5] * D;
  V[6] := V[6] * D;
  V[7] := V[7] * D;
  V[8] := V[8] * D;
  V[9] := V[9] * D;
  V[10] := V[10] * D;
  V[11] := V[11] * D;
  V[12] := V[12] * D;
  V[13] := V[13] * D;
  V[14] := V[14] * D;
  V[15] := V[15] * D;
  Result := True;
end;

procedure TMatrix4x4.Transpose;
var
  F: Float;
begin
  F := M[0, 1]; M[0, 1] := M[1, 0]; M[1, 0] := F;
  F := M[0, 2]; M[0, 2] := M[2, 0]; M[2, 0] := F;
  F := M[0, 3]; M[0, 3] := M[3, 0]; M[3, 0] := F;
  F := M[1, 2]; M[1, 2] := M[2, 1]; M[2, 1] := F;
  F := M[1, 3]; M[1, 3] := M[3, 1]; M[3, 1] := F;
  F := M[2, 3]; M[2, 3] := M[3, 2]; M[3, 2] := F;
end;

procedure TMatrix4x4.Rotate(X, Y, Z: Float);
begin
  Rotate(X, Y, Z, DefaultRotationOrder);
end;

procedure TMatrix4x4.Rotate(X, Y, Z: Float; Order: TRotationOrder);
var
  A, B: TMatrix;

  procedure RotateX;
  begin
    if X <> 0 then
    begin
      B := StockMatrix;
      B.M[1, 1] := Cos(X);
      B.M[1, 2] := Sin(X);
      B.M[2, 1] := -B.M[1, 2];
      B.M[2, 2] := B.M[1, 1];
      A := A * B;
    end;
  end;

  procedure RotateY;
  begin
    if Y <> 0 then
    begin
      B := StockMatrix;
      B.M[0, 0] := Cos(Y);
      B.M[2, 0] := Sin(Y);
      B.M[0, 2] := -B.M[2, 0];
      B.M[2, 2] := B.M[0, 0];
      A := A * B;
    end;
  end;

  procedure RotateZ;
  begin
    if Z <> 0 then
    begin
      B := StockMatrix;
      B.M[0, 0] := Cos(Z);
      B.M[1, 0] := Sin(Z);
      B.M[0, 1] := -B.M[1, 0];
      B.M[1, 1] := B.M[0, 0];
      A := A * B;
    end;
  end;

begin
  A := Self;
  X := X * (PI / 180);
  Y := Y * (PI / 180);
  Z := Z * (PI / 180);
  case Order of
    roXYZ:
      begin
        RotateX;
        RotateY;
        RotateZ;
      end;
    roYZX:
      begin
        RotateY;
        RotateZ;
        RotateX;
      end;
    roZXY:
      begin
        RotateZ;
        RotateX;
        RotateY;
      end;
    roXZY:
      begin
        RotateX;
        RotateZ;
        RotateY;
      end;
    roZYX:
      begin
        RotateZ;
        RotateY;
        RotateX;
      end;
    roYXZ:
      begin
        RotateY;
        RotateX;
        RotateZ;
      end;
  end;
  Self := A;
end;

procedure TMatrix4x4.RotateAt(X, Y, Z: Float; const Pivot: TVec3);
begin
  RotateAt(X, Y, Z, Pivot, DefaultRotationOrder)
end;

procedure TMatrix4x4.RotateAt(X, Y, Z: Float; const Pivot: TVec3; Order: TRotationOrder);
begin
  Translate(Pivot.X, Pivot.Y, Pivot.Z);
  Rotate(X, Y, Z, Order);
  Translate(-Pivot.X, -Pivot.Y, -Pivot.Z);
end;

procedure TMatrix4x4.Scale(X, Y, Z: Float);
var
  S: TMatrix;
begin
  S := StockMatrix;
  S.M[0, 0] := X;
  S.M[1, 1] := Y;
  S.M[2, 2] := Z;
  Self := Self * S;
end;

procedure TMatrix4x4.ScaleAt(X, Y, Z: Float; const Pivot: TVec3);
begin
  Translate(Pivot.X, Pivot.Y, Pivot.Z);
  Scale(X, Y, Z);
  Translate(-Pivot.X, -Pivot.Y, -Pivot.Z);
end;

procedure TMatrix4x4.Translate(X, Y, Z: Float);
var
  T: TMatrix;
begin
  T := StockMatrix;
  T.M[3, 0] := X;
  T.M[3, 1] := Y;
  T.M[3, 2] := Z;
  Self := Self * T;
end;

function TMatrix4x4.Transform(const V: TVec2): TVec2;
begin
  Result := Self * V;
end;

function TMatrix4x4.Transform(const V: TVec3): TVec3;
begin
  Result := Self * V;
end;

function TMatrix4x4.Transform(const M: TMatrix4x4): TMatrix4x4;
begin
  Result := Self * M;
end;

procedure TMatrix4x4.Perspective(FoV, AspectRatio, NearPlane, FarPlane: Float);
var
  XMax, YMax: Float;
begin
  YMax := NearPlane * Tan(FoV * PI / 360);
  XMax := YMax * AspectRatio;
  Frustum(-XMax, XMax, YMax, -YMax, NearPlane, FarPlane);
end;

procedure TMatrix4x4.Frustum(Left, Right, Top, Bottom, NearPlane, FarPlane: Float);
var
  F1, F2, F3, F4: Float;
begin
  F1 := 2.0 * NearPlane;
  F2 := Right - Left;
  F3 := Top - Bottom;
  F4 := FarPlane - NearPlane;
  V[0] := F1 / F2;
  V[1] := 0;
  V[2] := 0;
  V[3] := 0;
  V[4] := 0;
  V[5] := F1 / F3;
  V[6] := 0;
  V[7] := 0;
  V[8] := (Right + Left) / F2;
  V[9] := (Top + Bottom) / F3;
  V[10] := (-FarPlane - NearPlane) / F4;
  V[11] := -1;
  V[12] := 0;
  V[13] := 0;
  V[14] := (-F1 * FarPlane) / F4;
  V[15] := 0;
end;

{ from https://developer.tizen.org/community/code-snippet/native-code-snippet/set-lookat-matrix-opengl-es-2.0 }

procedure TMatrix4x4.LookAt(Eye, Center, Up: TVec3);
var
  F, S, U: TVec3;
begin
  F := Center - Eye;
  F.Normalize;
  S := F.Cross(Up);
  S.Normalize;
  if (S.V[0] = 0) and (S.V[1] = 0) and (S.V[2] = 0) then
    Exit;
  U := S.Cross(F);
  V[0] := S.X;
  V[1] := U.X;
  V[2] := -F.X;
  V[3] := 0;
  V[4] := S.Y;
  V[5] := U.Y;
  V[6] := -F.Y;
  V[7] := 0;
  V[8] := S.Z;
  V[9] := U.Z;
  V[10] := -F.Z;
  V[11] := 0;
  V[12] := 0;
  V[13] := 0;
  V[14] := 0;
  V[15] := 1;
  Translate(-Eye.X, -Eye.Y, -Eye.Z);
end;

{ TQuaternion }

class operator TQuaternion.Explicit(const A: TQuaternion): TMatrix4x4;
var
  SW, SX, SY, SZ, S, T: Float;
begin
  Result := StockMatrix;
  SW := A.W * A.W;
  SX := A.X * A.X;
  SY := A.Y * A.Y;
  SZ := A.Z * A.Z;
  Result.M[0, 0] :=  SX - SY - SZ + SW;
  Result.M[1, 1] := -SX + SY - SZ + SW;
  Result.M[2, 2] := -SX - SY + SZ + SW;
  S := A.X * A.Y;
  T := A.Z * A.W;
  Result.M[1, 0] := 2 * (S + T);
  Result.M[0, 1] := 2 * (S - T);
  S := A.X * A.Z;
  T := A.Y * A.W;
  Result.M[2, 0] := 2 * (S - T);
  Result.M[0, 2] := 2 * (S + T);
  S := A.Y * A.Z;
  T := A.X * A.W;
  Result.M[2, 1] := 2 * (S + T);
  Result.M[1, 2] := 2 * (S - T);
end;

class operator TQuaternion.Implicit(const A: TMatrix4x4): TQuaternion;
const
  Epsilon = 0.001;
var
  S, T: Float;
begin
  T := A.M[0, 0] + A.M[1, 1] + A.M[2, 2] + 1;
  if T > Epsilon then
  begin
    S := 0.5 / Sqrt(T);
    Result.W := 0.25 / S;
    Result.X := (A.M[2, 1] - A.M[1, 2]) * S;
    Result.Y := (A.M[0, 2] - A.M[2, 0]) * S;
    Result.Z := (A.M[1, 0] - A.M[0, 1]) * S;
  end
  else if (A.M[0, 0] > A.M[1, 1]) and (A.M[0, 0] > A.M[2, 2]) then
  begin
    S := 2 * Sqrt(1.0 + A.M[0, 0] - A.M[1, 1] - A.M[2, 2]);
    Result.W := (A.M[1, 2] - A.M[2, 1]) / S;
    Result.X := 0.25 * S;
    Result.Y := (A.M[0, 1] + A.M[1, 0]) / S;
    Result.Z := (A.M[0, 2] + A.M[2, 0]) / S;
  end
  else if A.M[1, 1] > A.M[2, 2] then
  begin
    S := 2 * Sqrt(1.0 + A.M[1, 1] - A.M[0, 0] - A.M[2, 2]);
    Result.W := (A.M[0, 2] - A.M[2, 0]) / S;
    Result.X := (A.M[0, 1] + A.M[1, 0]) / S;
    Result.Y := 0.25 * S;
    Result.Z := (A.M[1, 2] + A.M[2, 1]) / S;
  end
  else
  begin
    S := 2 * Sqrt(1.0 + A.M[2, 2] - A.M[0, 0] - A.M[1, 1]);
    Result.W := (A.M[0, 1] - A.M[1, 0]) / S;
    Result.X := (A.M[0, 2] + A.M[2, 0]) / S;
    Result.Y := (A.M[1, 2] + A.M[2, 1]) / S;
    Result.Z := 0.25 * S;
  end;
end;

class operator TQuaternion.Multiply(const A, B: TQuaternion): TQuaternion;
begin
  Result.X := A.W * B.X + A.X * B.W + A.Y * B.Z - A.Z * B.Y;
  Result.Y := A.W * B.Y - A.X * B.Z + A.Y * B.W + A.Z * B.X;
  Result.Z := A.W * B.Z + A.X * B.Y - A.Y * B.X + A.Z * B.W;
  Result.W := A.W * B.W - A.X * B.X - A.Y * B.Y - A.Z * B.Z;
  Result.Normalize;
end;

procedure TQuaternion.Conjugate;
begin
  X := -X;
  Y := -Y;
  Z := -Z;
  W := W;
end;

procedure TQuaternion.Normalize;
var
  Ratio: Float;
begin
  Ratio := Sqrt(W * W + X * X + Y * Y + Z * Z);
  if Ratio > 0 then
  begin
    Ratio := 1 / Ratio;
    W := W * Ratio;
    X := X * Ratio;
    Y := Y * Ratio;
    Z := Z * Ratio;
  end
  else
    Self := StockQuaternion;
end;

{ TLine2 }

function TLine2.Slope: TSlope2;
begin
  Result.Undefined := False;
  Result.Ratio := 0;
  if P0.X = P1.X then
  begin
    Result.Undefined := True;
    Result.Intercept := P0.X
  end
  else if P0.Y = P1.Y then
    Result.Intercept := P0.Y
  else
  begin
    Result.Ratio := (P0.Y - P1.Y) / (P0.X - P1.X);
    Result.Intercept := P0.Y - Result.Ratio * P0.X;
  end;
end;

function TLine2.Normal: TVec2;
begin
  Result := (P1 - P0).Normal;
end;

function TLine2.Binormal: TVec2;
var
  A: Float;
begin
  Result := (P1 - P0).Normal;
  A := Result.Y;
  Result.Y := -Result.X;
  Result.X := A;
end;

function TLine2.FindPoint(Dist: Float): TVec2;
begin
  if Dist < Epsilon then
    Exit(P0);
  if Dist > P0.Distance(P1) - Epsilon then
    Exit(P1);
  Result := P0 + Normal * Dist;
end;

function TLine2.Distance: Float;
begin
  Result := P0.Distance(P1);
end;

function TLine2.Intersects(const Line: TLine2): Boolean;
var
	A, B: Float;
begin
  Result := False;
	A := (P1.X - P0.X) * (Line.P1.Y - Line.P0.Y) - (P1.Y - P0.Y) * (Line.P1.X - Line.P0.X);
	if Abs(A) < Epsilon then
		Exit;
	B := ((P0.Y - Line.P0.Y) * (Line.P1.X - Line.P0.X) - (P0.X - Line.P0.X) * (Line.P1.Y - Line.P0.Y)) / A;
	if (B > 0.0) and (B < 1.0) then
	begin
		B := ((P0.Y - Line.P0.Y) * (P1.X - P0.X) - (P0.X - Line.P0.X) * (P1.Y - P0.Y)) / A;
		if (B > 0.0) and (B < 1.0) then
			Result := True;
	end;
end;

function TLine2.Intersects(const Line: TLine2; out Point: TVec2): Boolean;
var
  S1X, S1Y, S2X, S2Y: Float;
  S, T: Float;
begin
  Point.X := 0;
  Point.Y := 0;
  Result := False;
  if (P1.X = Line.P0.X) and (P1.Y = Line.P0.Y) then
    Exit;
  if (P0.X = Line.P1.X) and (P0.Y = Line.P1.Y) then
    Exit;
  S1X := P1.X - P0.X;
  S1Y := P1.Y - P0.Y;
  S2X := Line.P1.X - Line.P0.X;
  S2Y := Line.P1.Y - Line.P0.Y;
  S := (-S1Y * (P0.X - Line.P0.X) + S1X * (P0.Y - Line.P0.Y)) / (-S2X * S1Y + S1X * S2Y);
  T := (S2X * (P0.Y - Line.P0.Y) - S2Y * (P0.X - Line.P0.X)) / (-S2X * S1Y + S1X * S2Y);
  if (S >= 0) and (S <= 1) and (T >= 0) and (T <= 1) then
  begin
    Point.X := P0.X + (T * S1X);
    Point.Y := P0.Y + (T * S1Y);
    Result := True;
  end;
end;

{ TCurve2 }

class operator TCurve2.Implicit(const Value: TPolygon2): TCurve2;
var
  I: Integer;
begin
  Result.P := Value;
  SetLength(Result.P, Length(Result.P));
  Result.Distance := 0;
  Result.D := nil;
  I := Length(Result.P);
  if I > 1 then
  begin
    SetLength(Result.D, I - 1);
    for I := Low(Result.D) to High(Result.D) do
    begin
      Result.D[I] := Result.P[I].Distance(Result.P[I + 1]);
      Result.Distance := Result.Distance + Result.D[I];
    end;
  end;
end;

class operator TCurve2.Explicit(const Value: TCurve2): TPolygon2;
begin
  Result := Value.P;
  SetLength(Result, Length(Result));
end;

class operator TCurve2.Multiply(const A: TMatrix4x4; const B: TCurve2): TCurve2;
var
  C: TCurve2 absolute Result;
  I: Integer;
begin
  C.P := nil;
  SetLength(C.P, Length(B.P));
  SetLength(C.D, Length(B.D));
  C.Distance := 0;
  for I := Low(C.P) to High(C.P) do
  begin
    C.P[I] := A * C.P[I];
    if I > 0 then
    begin
      C.D[I - 1] := NewLine2(C.P[I - 1], C.P[I]).Distance;
      C.Distance := C.Distance + C.D[I - 1];
    end;
  end;
end;

function TCurve2.FindNormal(Dist: Float): TVec2;
var
  F: Float;
  I: Integer;
begin
  if Dist < Epsilon then
    Exit(NewLine2(P[0], P[1]).Normal);
  if Dist > Distance - Epsilon then
    Exit(NewLine2(P[High(P) - 1], P[High(P)]).Normal);
  F := 0;
  for I := Low(D) to High(D) do
  begin
    F := F + D[I];
    if Dist < F then
      Exit(NewLine2(P[I], P[I + 1]).Normal);
  end;
  Result := NewLine2(P[High(P) - 1], P[High(P)]).Normal;
end;

function TCurve2.FindPoint(Dist: Float): TVec2;
var
  A, B: Float;
  I: Integer;
begin
  if Dist < Epsilon then
    Exit(P[Low(P)]);
  if Dist > Distance - Epsilon then
    Exit(P[High(P)]);
  A := 0;
  B := 0;
  for I := Low(D) to High(D) do
  begin
    B := A + D[I];
    if Dist < B then
      Exit(NewLine2(P[I], P[I + 1]).FindPoint(Dist - A));
    A := B;
  end;
  Result := P[High(P)];
end;

{ TBezier2 }

function TBezier2.InternalFlatten(Count: Integer): TCurve2;
var
  X0, X1, X2: Float;
  Y0, Y1, Y2: Float;
  TD, T0, T1, T2: Float;
  C: TCurve2;
  I: Integer;
begin
  X2 := (P[1].X - P[0].X) * 3;
  Y2 := (P[1].Y - P[0].Y) * 3;
  X1 := (P[2].X - P[1].X) * 3 - X2;
  Y1 := (P[2].Y - P[1].Y) * 3 - Y2;
  X0 := P[3].X - P[0].X - X2 - X1;
  Y0 := P[3].Y - P[0].Y - Y2 - Y1;
  TD := 1 / (Count - 1);
  T0 := 0;
  C.Distance := 0;
  SetLength(C.D, Count - 1);
  SetLength(C.P, Count);
  for I := 0 to Count - 1 do
  begin
    T1 := T0 * T0;
    T2 := T1 * T0;
    C.P[I].X :=(X0 * T2) +(X1 * T1) +(X2 * T0) + P[0].X;
    C.P[I].Y :=(Y0 * T2) +(Y1 * T1) +(Y2 * T0) + P[0].Y;
    T0 := T0 + TD;
    if I > 0 then
    begin
      C.D[I - 1] := C.P[I - 1].Distance(C.P[I]);
      C.Distance := C.Distance + C.D[I - 1];
    end;
  end;
  Result := C;
end;

function TBezier2.Flatten(Count: Integer = 0): TCurve2;
const
  MinPoints = 8;
var
  Curve: TCurve2;
  I: Integer;
begin
  if Count > 1 then
    Exit(InternalFlatten(Count));
  I := MinPoints;
  Result := InternalFlatten(I);
  if Result.Distance < MinPoints then
    Exit(Result);
  if Result.Distance < 20 then
    Exit(Result);
  I := I * 2;
  Curve := InternalFlatten(I);
  while Curve.Distance - Result.Distance > 0.2 do
  begin
    Result := Curve;
    if I > 70 then
      Exit;
    I := I + 15;
    Curve := InternalFlatten(I);
  end;
end;

function NewBezier2(const P0, P1, P2, P3: TVec2): TBezier2;
begin
  Result.P0 := P0;
  Result.P1 := P1;
  Result.P2 := P2;
  Result.P3 := P3;
end;

{ TPlane3 }

function TPlane3.Normal: TVec3;
var
  A, B: TVec3;
begin
  A.X := V[2].X - V[1].X;
  A.Y := V[2].Y - V[1].Y;
  A.Z := V[2].Z - V[1].Z;
  B.X := V[0].X - V[1].X;
  B.Y := V[0].Y - V[1].Y;
  B.Z := V[0].Z - V[1].Z;
  Result := A.Cross(B);
end;

function TPlane3.RayIntersect(Ray: TVec3; out Point: TVec3): Boolean;
var
  N: TVec3;
  Num, Den, FabsDen, FabsTolerance: Float;
begin
  N := Normal;
  Num := N.Dot(V[0]);
  Den := N.X * Ray.X + N.Y * Ray.Y + N.Z * Ray.Z;
  if Den < 0 then
    FabsDen := -Den
  else
    FabsDen := Den;
  if Num < 0 then
    FabsTolerance := -Num * Epsilon
  else
    FabsTolerance := Num * Epsilon;
  if FabsDen <= FabsTolerance then
  begin
    Point := Vec3(0);
    Exit(False);
  end;
  Num := Num / Den;
  Point.X := Num * Ray.X;
  Point.Y := Num * Ray.Y;
  Point.Z := Num * Ray.Z;
  Result := Num >= 0;
  if not Result then
    Point := Vec3(0);
end;
{$endregion}

end.

