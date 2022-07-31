unit Play.SvgPhysics;

{$mode delphi}

interface

uses
  Tiny.System, Tiny.Types, Tiny.Application, Tiny.Graphics,
  Tiny.Physics, Tiny.Physics.Scene, Tiny.Geometry,
  Play.Svg;

{ TPhysicsStyle }

type
  TPhysicsStyle = class(TSvgStyleEx)
  protected
    procedure Copy(StyleEx: TSvgStyleEx); override;
    procedure Read(const Name, Value: string); override;
  public
    Body: string;
    Group: string;
    Density: Float;
    Elasticity: Float;
    Friction: Float;
    Category: LongWord;
    Mask: LongWord;
    constructor Create; override;
  end;

{ TSvgPhysics }

  TSvgPhysics = class(TPhysicsScene)
  private
    FPaper: IBitmapBrush;
    FChalkBitmap: IBitmapBrush;
    FChalk: IPen;
    procedure LoadDocument;
  protected
    procedure DoKeyDown(var Args: TKeyboardArgs); override;
    procedure Load; override;
    procedure Unload; override;
    function DrawCustomBody(Body: TBody): Boolean; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
  end;

implementation

{ TPhysicsStyle }

constructor TPhysicsStyle.Create;
begin
  inherited Create;
  Body := 'kinematic';
  Density := 1;
  Elasticity := 0.5;
  Friction := 0.75;
  Category := High(LongWord);
  Mask := High(LongWord);
end;

procedure TPhysicsStyle.Copy(StyleEx: TSvgStyleEx);
var
  P: TPhysicsStyle absolute StyleEx;
begin
  Body := P.Body;
  Group := P.Group;
  Density := P.Density;
  Elasticity := P.Elasticity;
  Friction := P.Friction;
end;

procedure TPhysicsStyle.Read(const Name, Value: string);
begin
  if Name = 'body' then
    Body := Value
  else if Name = 'group' then
    Group := Value
  else if Name = 'density' then
    Density := StrToFloat(Value)
  else if Name = 'elasticity' then
    Elasticity := StrToFloat(Value)
  else if Name = 'friction' then
    Friction := StrToFloat(Value)
  else if Name = 'category' then
    Category := StrToMask(Value)
  else if Name = 'mask' then
    Mask := StrToMask(Value);
end;

{ TSvgPhysics }

procedure TSvgPhysics.DoKeyDown(var Args: TKeyboardArgs);
begin
  if Args.Key = keyF5 then
  begin
    Space.ReleaseJoints;
    Space.ReleaseBodies;
    LoadDocument;
    Args.Handled := True;
  end;
  inherited DoKeyDown(Args);
end;

procedure TSvgPhysics.LoadDocument;
var
  Ground: TBody;
  Last: TBody;

  procedure AddCircle(Circle: TSvgCircle);
  var
    Style: TPhysicsStyle;
    Body: TBody;
    Shape: TShape;
    Center: TPointD;
  begin
    Style := Circle.StyleEx as TPhysicsStyle;
    if Style.Body = 'motor' then
    begin
      Space.NewMotor(Ground, Last, -0.5);
      Exit;
    end;
    if Style.Body = 'kinematic' then
      Body := Space.NewBody
    else
      Body := Space.NewStaticBody;
    Center := Vect(Circle.X, Circle.Y);
    Shape := Body.NewCircle(Circle.R, Center);
    Shape.Density := Style.Density;
    Shape.Elasticity := Style.Elasticity;
    Shape.Friction := Style.Friction;
    Shape.Category := Style.Category;
    Shape.Mask := Style.Mask;
    if Style.Body = 'pivot' then
      Space.NewPivot(Last, Body, Center);
  end;

  procedure AddPath(Path: TSvgPath);
  var
    Style: TPhysicsStyle;
    Body: TBody;
    C: TSvgCommand;
    S0, P0, P1: TPointF;

    procedure AddSegment;
    var
      S: TShape;
    begin
      S := Body.NewSegment(P0, P1, Path.Style.StrokeWidth * 1.25);
      S.Density := Style.Density;
      S.Elasticity := Style.Elasticity;
      S.Friction := Style.Friction;
      S.Category := Style.Category;
      S.Mask := Style.Mask;
    end;

    procedure AddBezier;
    var
      Bezier: TBezier2;
      Curve: TCurve2;
      I: Integer;
    begin
      Bezier.P0 := P0;
      Bezier.P1 := Vec2(C.X1, C.Y1);
      Bezier.P2 := Vec2(C.X2, C.Y2);
      Bezier.P3 := P1;
      if Path.ClassId = 'ball' then
        Curve := Bezier.Flatten(6)
      else
        Curve := Bezier.Flatten;
      for I := 1 to Length(Curve.P) - 1 do
      begin
        P0 := Curve.P[I - 1];
        P1 := Curve.P[I];
        AddSegment;
      end;
    end;

  begin
    Style := Path.StyleEx as TPhysicsStyle;
    if Style.Body = 'static' then
    begin
      Body := Space.NewStaticBody;
      if Ground = nil then
        Ground := Body;
    end
    else
      Body := Space.NewBody;
    Last := Body;
    P0 := NewPointF(0, 0);
    for C in Path.Commands do
    begin
      P1.X := C.X;
      P1.Y := C.Y;
      case C.Action of
        svgMove:
          S0 := P1;
        svgLine, svgHLine, svgVLine:
          AddSegment;
        svgCubic:
          AddBezier;
        svgClose:
          begin
            P1 := S0;
            AddSegment;
          end;
      else
      end;
      P0 := P1;
    end;
  end;

  procedure AddBodies(Nodes: TSvgCollection);
  var
    N: TSvgNode;
  begin
    for N in Nodes do
      if N is TSvgPath then
        AddPath(N as TSvgPath)
      else if N is TSvgCircle then
        AddCircle(N as TSvgCircle)
      else if N is TSvgCollection then
        AddBodies(N as TSvgCollection);
  end;

var
  D: TSvgDocument;
begin
  D := NewSvgDocument;
  try
    D.ParseFile('../assets/playground.svg', TPhysicsStyle);
    AddBodies(D);
  finally
    D.Free;
  end;
end;

procedure TSvgPhysics.Load;
var
  B: IBitmap;
begin
  inherited Load;
  Font := Canvas.LoadFont('Roboto', '../assets/Roboto-Medium.ttf');
  Font.Color := colorLightBlue;
  Font.Size := 24;
  B := Canvas.LoadBitmap('chalk', '../assets/white-crayon.png');
  FChalkBitmap := NewBrush(B);
  FChalk := NewPen;
  FChalk.Brush := FChalkBitmap;
  FChalk.LineCap := capRound;
  FChalk.LineJoin := joinRound;
  FChalk.Width := 4;
  B := Canvas.LoadBitmap('blueprint', '../assets/blueprint.png');
  FPaper := NewBrush(B);
  Space.Gravity := Vect(0, 1000);
  LoadDocument;
end;

procedure TSvgPhysics.Unload;
begin
  FPaper := nil;
  inherited Unload;
end;

function TSvgPhysics.DrawCustomBody(Body: TBody): Boolean;
var
  S: TShape;
  P: TPointF;
begin
  Result := True;
  for S in Body.Shapes do
    case S.Kind of
      shapeCircle:
        begin
          P := Body.BodyToWorld(S.AsCircle.Offset);
          Canvas.Circle(P.X, P.Y, S.AsCircle.Radius);
        end;
      shapeSegment:
        begin
          P := Body.BodyToWorld(S.AsSegment.A);
          Canvas.MoveTo(P.X, P.Y);
          P := Body.BodyToWorld(S.AsSegment.B);
          Canvas.LineTo(P.X, P.Y);
        end;
    else
    end;
    P := Body.Position;
    FChalkBitmap.Offset := P;
    FChalkBitmap.Angle := Body.Angle;
    Canvas.Stroke(FChalk);
end;


procedure TSvgPhysics.Render(Width, Height: Integer; const Time: Double);
begin
  inherited Render(Width, Height, Time);
  ScaleToStudio;
  Canvas.Rect(ClientRect);
  Canvas.Fill(FPaper);
  Canvas.BlendMode := blendLighten;
  DrawPhysics;
end;

end.

