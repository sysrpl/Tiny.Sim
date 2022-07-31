unit Tiny.Physics.Scene;

{$i tiny.inc}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application,
  Tiny.Graphics,
  Tiny.Physics,
  Tiny.Interop.Chipmunk2D;

type
  TPhysicsScene = class(TWidgetScene)
  private
    FSpace: TSpace;
    FGrab: TBody;
    FGrabSpring: TJoint;
    FGrabBodies: Boolean;
    FGrabCentered: Boolean;
    FBrush: ISolidBrush;
    FPen: IPen;
  protected
    function ShapeNearPoint(X, Y, Distance: Float): TShape;
    procedure DoMouseDown(var Args: TMouseArgs); override;
    procedure DoMouseMove(var Args: TMouseArgs); override;
    procedure DoMouseUp(var Args: TMouseArgs); override;
    procedure Load; override;
    procedure Logic(Width, Height: Integer; const Time: Double); override;
    procedure Unload; override;
    { Add some default static side and ground walls to the scene }
    procedure GenerateStudioWalls;
    { Return true to disable default drawing }
    function DrawCustomBody(Body: TBody): Boolean; virtual;
    function DrawCustomShape(Shape: TShape): Boolean; virtual;
    function DrawCustomJoint(Joint: TJoint): Boolean; virtual;
    { Draw physics included in the category mask }
    procedure DrawPhysics(Categories: TBitmask = $FFFFFFFF);
    property Space: TSpace read FSpace;
    property GrabBodies: Boolean read FGrabBodies write FGrabBodies;
    property GrabCentered: Boolean read FGrabCentered write FGrabCentered;
  end;

implementation

{ TPhysicsScene }

function TPhysicsScene.ShapeNearPoint(X, Y, Distance: Float): TShape;
var
  Info: cpPointQueryInfoStruct;
  P: TPointD;
begin
  P.X := X;
  P.Y := Y;
  Result := cpSpacePointQueryNearest(Space, P, Distance, FilterAll, @Info);
end;

procedure TPhysicsScene.DoMouseDown(var Args: TMouseArgs);
var
  Shape: TShape;
  Info: cpPointQueryInfoStruct;
  P: TPointD;
begin
  inherited DoMouseDown(Args);
  if Args.Handled then Exit;
  if not FGrabBodies then
    Exit;
  if Args.Button <> mbLeft then
    Exit;
  FGrabSpring.Free;
  P := PointToStudio(Args.X, Args.Y);
  Shape := cpSpacePointQueryNearest(Space, P, 100, FilterAll, @Info);
  if Shape.IsNil then
    Exit;
  if Shape.Body.Kind <> bodyDynamic then
    Exit;
  FGrab.Position := P;
  if FGrabCentered then
    P := Shape.Body.BodyToWorld(Shape.CenterOfGravity)
  else if Info.distance < 0 then
  begin
    // P := Vect(X, Y)
  end
  else
    P := info.point;
  P := Shape.Body.WorldToBody(P);
  FGrabSpring := Space.NewDampedSpring(Shape.Body, FGrab, P, VectZero, 1, 9000000, 0.1);
  Args.Handled := True;
end;

procedure TPhysicsScene.DoMouseMove(var Args: TMouseArgs);
begin
  inherited DoMouseMove(Args);
  if Args.Handled then Exit;
  if not FGrabBodies then
    Exit;
  if FGrabSpring.IsNil then
    Exit;
  FGrab.Position := PointToStudio(Args.X, Args.Y);
  Args.Handled := True;
end;

procedure TPhysicsScene.DoMouseUp(var Args: TMouseArgs);
begin
  inherited DoMouseUp(Args);
  if Args.Handled then Exit;
  if Args.Button = mbLeft then
  begin
    FGrabSpring.Free;
    Args.Handled := True;
  end;
end;

procedure TPhysicsScene.Load;
begin
  inherited Load;
  FBrush := NewBrush(colorBlack);
  FPen := NewPen;
  FPen.Width := 4;
  FSpace := NewSpace;
  FGrab := Space.NewKinematicBody;
  with FGrab.NewCircle(20) do
  begin
    Friction := 0;
    Elasticity := 0;
    Category := 0;
    CollisionType := collideGrab;
  end
end;

procedure TPhysicsScene.Logic(Width, Height: Integer; const Time: Double);
begin
  FSpace.Step(LogicStep);
end;

procedure TPhysicsScene.Unload;
begin
  FSpace.Free;
  inherited Unload;
end;

procedure TPhysicsScene.GenerateStudioWalls;
const
  WallHeight = -500000;
  WallThick = 20;
begin
  with Space.Ground.NewSegment(Vect(0, WallHeight), Vect(0, StudioHeight), WallThick) do
  begin
    Friction := 0.7;
    Elasticity := 0.5;
  end;
  with Space.Ground.NewSegment(Vect(0, StudioHeight), Vect(StudioWidth, StudioHeight), WallThick) do
  begin
    Friction := 0.7;
    Elasticity := 0.5;
  end;
  with Space.Ground.NewSegment(Vect(StudioWidth, StudioHeight), Vect(StudioWidth, WallHeight), WallThick) do
  begin
    Friction := 0.7;
    Elasticity := 0.5;
  end;
end;

function TPhysicsScene.DrawCustomBody(Body: TBody): Boolean;
begin
  Result := False;
end;

function TPhysicsScene.DrawCustomShape(Shape: TShape): Boolean;
begin
  Result := False;
end;

function TPhysicsScene.DrawCustomJoint(Joint: TJoint): Boolean;
begin
  Result := False;
end;

procedure TPhysicsScene.DrawPhysics(Categories: TBitmask = $FFFFFFFF);
var
  Shape: TShape;
  Body: TBody;
  Joint: TJoint;

  procedure DrawPoint(P: TPointD);
  const
    PointSize = 8;
  begin
    Canvas.Circle(P.X, P.Y, PointSize);
    Canvas.Fill(FBrush);
  end;

  procedure DrawCircle(C: TCircle);
  var
    A, B: TPointD;
  begin
    A := C.Base.Body.BodyToWorld(C.Offset);
    B := C.Offset;
    B.Y := B.Y - C.Radius;
    B := C.Base.Body.BodyToWorld(B);
    if C.Base.Body.IsSleeping then
      FBrush.Color := NewColorB(150, 150, 150)
    else if C.Base.Body.Kind = bodyDynamic then
      FBrush.Color := NewColorB(100, 180, 240)
    else
      FBrush.Color := colorLightBlue;
    with C.Base.Body.BodyToWorld(C.Offset) do
      Canvas.Circle(X, Y, C.Radius);
    Canvas.Fill(FBrush);
    if C.Base.Body.Kind <> bodyDynamic then
      Exit;
    FPen.Color := NewColorB(60, 130, 200);
    FPen.Width := 4;
    FPen.LineCap := capButt;
    Canvas.MoveTo(A.X, A. Y);
    Canvas.LineTo(B.X, B. Y);
    Canvas.Stroke(FPen);
  end;

  procedure DrawSegment(S: TSegment);
  var
    Color: TColorF;
  begin
    if S.Base.Body.IsSleeping then
      Color := NewColorB(150, 150, 150)
    else if S.Base.Body.Kind = bodyDynamic then
      Color := colorLightBlue
    else
      Color := colorLightBlue;
    FPen.Color := Color;
    FPen.Width := S.Border * 2 + 1;
    FPen.LineCap := capRound;
    with S.Base.Body.BodyToWorld(S.A) do
      Canvas.MoveTo(X, Y);
    with S.Base.Body.BodyToWorld(S.B) do
      Canvas.LineTo(X, Y);
    Canvas.Stroke(FPen);
  end;

  procedure DrawPolygon(P: TPolygon);
  var
    I: Integer;
  begin
    if P.VertCount < 3 then
      Exit;
    with P.Base.Body.BodyToWorld(P.Vert[0]) do
      Canvas.MoveTo(X, Y);
    for I := 1 to P.VertCount - 1 do
      with P.Base.Body.BodyToWorld(P.Vert[I])  do
        Canvas.LineTo(X, Y);
    Canvas.ClosePath;
    if P.Base.Body.IsSleeping then
      FBrush.Color := NewColorB(150, 150, 150)
    else if P.Base.Body.Kind = bodyDynamic then
      FBrush.Color := NewColorB(140, 230, 200)
    else
      FBrush.Color := NewColorB(230, 230, 40);
    Canvas.Fill(FBrush);
  end;

  procedure DrawGrab(J: TJoint);
  var
    A, B: TPointD;
  begin
    A := J.A.BodyToWorld(J.AsDampedSpring.PinA);
    B := J.B.BodyToWorld(J.AsDampedSpring.PinB);
    with A do
      Canvas.MoveTo(X, Y);
    with A.NormalAtMix(B, 0.8, 5) do
      Canvas.LineTo(X, Y);
    with A.NormalAtMix(B, 0.8, 15) do
      Canvas.LineTo(X, Y);
    with B do
      Canvas.LineTo(X, Y);
    with A.NormalAtMix(B, 0.8, -15) do
      Canvas.LineTo(X, Y);
    with A.NormalAtMix(B, 0.8, -5) do
      Canvas.LineTo(X, Y);
    Canvas.ClosePath;
    FBrush.Color := NewColorB(50, 150, 30);
    Canvas.Fill(FBrush);
  end;

  procedure DrawPin(J: TPinJoint);
  var
    P: TPointD;
  begin
    Exit;
    P := J.Base.A.BodyToWorld(J.PinA);
    DrawPoint(P);
    Canvas.MoveTo(P.x, P.y);
    P := J.Base.B.BodyToWorld(J.PinB);
    Canvas.LineTo(P.x, P.y);
    Canvas.Stroke(FPen);
    DrawPoint(P);
  end;

  procedure DrawSpring(J: TDampedSpringJoint);
  var
    A, B: TPointD;
  begin
    A := J.Base.A.BodyToWorld(J.PinA);
    B := J.Base.B.BodyToWorld(J.PinB);
    DrawPoint(A);
    DrawPoint(B);
    Canvas.MoveTo(A.x, A.y);
    Canvas.LineTo(B.x, B.y);
    Canvas.Stroke(FPen);
  end;

begin
  FBrush.Color := NewColorB(230, 230, 40);
  FPen.Color := NewColorB(60, 230, 60);
  FPen.LineCap := capRound;
  if not DrawCustomBody(Space.Ground) then
    for Shape in Space.Ground.Shapes do
    begin
      if DrawCustomShape(Shape) then
        Continue;
      if Shape.Category and Categories = 0 then
        Continue;
      with Shape.AsSegment.A do
        Canvas.MoveTo(X, Y);
      with Shape.AsSegment.B do
        Canvas.LineTo(X, Y);
      FPen.Width := Shape.AsSegment.Border * 2 + 1;
      Canvas.Stroke(FPen);
    end;
  for Body in Space.Bodies do
    if DrawCustomBody(Body) then
      Continue
    else for Shape in Body.Shapes do
      if DrawCustomShape(Shape) then
        Continue
      else if Shape.Category and Categories <> 0 then
        case Shape.Kind of
          shapeCircle: DrawCircle(Shape.AsCircle);
          shapeSegment: DrawSegment(Shape.AsSegment);
          shapePolygon: DrawPolygon(Shape.AsPolygon);
        end;
  FBrush.Color := colorRed;
  FPen.Color := colorRed;
  FPen.Width := 3;
  for Joint in Space.Joints do
  begin
    if DrawCustomJoint(Joint) then
      Continue
    else if Joint.IsGrab then
      DrawGrab(Joint)
    else case Joint.Kind of
      jointPin: DrawPin(Joint.AsPin);
      jointDampedSpring: DrawSpring(Joint.AsDampedSpring);
    end;
  end;
end;

end.

