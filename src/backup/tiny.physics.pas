unit Tiny.Physics;

{ This unit provides aassistance in understanding and effeticely using the
  Chipmunk2D physics library.

  It exposes the flat Chipmunk2D C style API as a collection ofrecord types.
  These records directly correspond to the pointer types used  by Chipmunk2D,
  including pointer types such as cpSpace, cpBody, cpShape, cpConstraint,
  and more. The these pascal records convert to and from the Chipmunk2D pointer
  types implicitly and can be freely interchanged with the functions of the
  Chipmunk2D API without penalty.

  Using pascal records in place of traditional class wrappers the Chipmunk2D
  library benefits the user in the following ways. Internally the Chipmunk2D
  library dynamically creates and manipulates both the state and relationship
  between its data structures. The corresponding pascal records in this unit
  allow Chipmunk2D to manage all these things without reduplication in pascal.

  Additionally, pascal records benefit the user by grouping logical concepts
  into distinct record types. Most of the properties and methods on the pascal
  records map neatly to the Chipmunk2D C style APIs. There are, however, some
  enhancements such as pascal for...in enumerators that have been added to
  make programming more convient.

  Consider the following Chipmunk2D C style API to create a circle body:

      cpBody* body = cpBodyNew(0, 0);
      cpSpaceAddBody(space, body);
      cpShape* circle = cpCircleShapeNew(radius, cpv(0, 0));
      cpShapeSetDensity(circle, 1);
      cpBodyAddShape(body, shape);
      cpSpaceAddShape(space, shape);

  Using our pascal records approach this becomes:

      Space.NewBody.NewCircle(Radius);

  The above code is following exactly the same steps as the C approach, but it
  greatly siplifies the task of creating a viable circle body. It also does
  this without creating additional pascal objects to intermediate this or
  other tasks. This style of simplication is repeated throughout this project.

  As a further example, to list all bodies in our pascal system you may write:

      for Shape in Space.Shapes do

  This is a much easiers to use approach when compared to the more cumbersome
  enuemration callbacks must forced upon the user by the C API. }

{$i tiny.inc}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Interop.Chipmunk2D;

type
  TBitmask = cpBitmask;
  TCollisionType = cpCollisionType;
  TCollisionHandler = cpCollisionHandler;

const
  VectZero: TPointD = (X: 0; Y: 0);
  MatrixIdentity: cpTransform = (a: 1; b: 0; c: 0; d: 1; tx: 0; ty: 0);
  BoolFlags: array[Boolean] of cpBool = (cpFalse, cpTrue);
  MaskAll = $FFFFFFFF;
  FilterAll: cpShapeFilterStruct = (group: nil; categories: MaskAll; mask: MaskAll);

{ Some prebuilt collision types }

  collideGrab = cpCollisionType($FFFF);

function Vect(X, Y: Double): TPointD; inline;
function MaskToStr(M: TBitmask): string;
function StrToMask(const S: string): TBitmask;

type
  TMatrix2D = record
  public
    Ref: cpTransform;
    class operator Implicit(const Value: TMatrix2D): cpTransform;
    class operator Implicit(const Value: cpTransform): TMatrix2D;
    class operator Multiply(const A, B: TMatrix2D): TMatrix2D; overload;
    class operator Multiply(const A: TMatrix2D; const B: TPointD): TPointD; overload;
    class function Compose(const a, b, c, d, tx, ty: Double): TMatrix2D; static;
    class function Transpose(const a, c, tx, b, d, ty: Double): TMatrix2D; static;
  public
    procedure Identity; inline;
    procedure Translate(const X, Y: Double);
    procedure Rotate(const Angle: Double);
    procedure Scale(const X, Y: Double);
  end;
  PMatrix2D = ^TMatrix2D;

  TShapeKind = LongWord;

const
  shapeCircle = CP_CIRCLE_SHAPE;
  shapeSegment = CP_SEGMENT_SHAPE;
  shapePolygon = CP_POLY_SHAPE;

type
  TBodyKind = LongWord;

const
  bodyDynamic = CP_BODY_TYPE_DYNAMIC;
  bodyKinematic = CP_BODY_TYPE_KINEMATIC;
  bodyStatic = CP_BODY_TYPE_STATIC;

type
  TJointKind = LongWord;

const
	jointPin = CP_JOINT_PIN;
	jointSlide = CP_JOINT_SLIDE;
	jointPivot = CP_JOINT_PIVOT;
	jointGroove = CP_JOINT_GROOVE;
	jointDampedSpring = CP_JOINT_DAMPED_SPRING;
	jointDampedRotarySpring = CP_JOINT_DAMPED_ROTARY_SPRING;
	jointRotaryLimit = CP_JOINT_ROTARY_LIMIT;
	jointRatchet = CP_JOINT_RATCHET;
	jointGear = CP_JOINT_GEAR;
	jointMotor = CP_JOINT_SIMPLE_MOTOR;

{ TCircle }

type
  TCircle = record
  public
    Ref: cpCircleShape;
    class operator Implicit(const Value: TCircle): cpCircleShape; inline;
    class operator Implicit(const Value: cpCircleShape): TCircle; inline;
    class operator Implicit(const Value: TCircle): cpShape; inline;
    class operator Implicit(const Value: cpShape): TCircle; inline;
  private
    function GetRadius: Double;
    procedure SetRadius(const Value: Double);
    function GetOffset: TPointD;
    procedure SetOffset(const Value: TPointD);
  public
    procedure Free;
    function IsNil: Boolean; inline;
    procedure Reshape(Radius: Double; const Offset: TPointD);
    property Radius: Double read GetRadius write SetRadius;
    property Offset: TPointD read GetOffset write SetOffset;
  end;

{ TSegment }

  TSegment = record
  public
    Ref: cpSegmentShape;
    class operator Implicit(const Value: TSegment): cpSegmentShape; inline;
    class operator Implicit(const Value: cpSegmentShape): TSegment; inline;
    class operator Implicit(const Value: TSegment): cpShape; inline;
    class operator Implicit(const Value: cpShape): TSegment; inline;
  private
    function GetBorder: Double;
    procedure SetBorder(const Value: Double);
    function GetA: TPointD;
    procedure SetA(const Value: TPointD);
    function GetB: TPointD;
    procedure SetB(const Value: TPointD);
    function GetNormal: TPointD;
  public
    procedure Free;
    function IsNil: Boolean; inline;
    procedure Join(const Prev, Next: TPointD);
    property Border: Double read GetBorder write SetBorder;
    property A: TPointD read GetA write SetA;
    property B: TPointD read GetB write SetB;
    property Normal: TPointD read GetNormal;
  end;

{ TPolygon }

  TPolygon = record
  public
    Ref: cpPolyShape;
    class operator Implicit(const Value: TPolygon): cpPolyShape; inline;
    class operator Implicit(const Value: cpPolyShape): TPolygon; inline;
    class operator Implicit(const Value: TPolygon): cpShape; inline;
    class operator Implicit(const Value: cpShape): TPolygon; inline;
  private
    function GetBorder: Double;
    function GetVert(Index: Integer): TPointD;
    function GetVertCount: Integer;
    procedure SetBorder(const Value: Double);
  public
    procedure Free;
    function IsNil: Boolean; inline;
    procedure Reshape(Verts: PPointD; Count: Integer; Transform: PMatrix2D = nil); overload;
    procedure Reshape(const Width, Height: Double; Transform: PMatrix2D = nil); overload;
    property Border: Double read GetBorder write SetBorder;
    property VertCount: Integer read GetVertCount;
    property Vert[Index: Integer]: TPointD read GetVert;
  end;

{ TShape }

  TShape = record
  public
    Ref: cpShape;
    class operator Implicit(const Value: TShape): cpShape; inline;
    class operator Implicit(const Value: cpShape): TShape; inline;
    class operator Equal(const A, B: TShape): Boolean; inline;
    class operator NotEqual(const A, B: TShape): Boolean; inline;
  private
    function GetNext: TShape;
    function GetKind: TShapeKind;
    function GetMass: Double;
    procedure SetMass(Value: Double);
    function GetDensity: Double;
    procedure SetDensity(Value: Double);
    function GetArea: Double;
    function GetCenterOfGravity: TPointD;
    function GetBoundingBox: cpBB;
    function GetSensor: Boolean;
    procedure SetSensor(Value: Boolean);
    function GetElasticity: Double;
    procedure SetElasticity(Value: Double);
    function GetFriction: Double;
    procedure SetFriction(Value: Double);
    function GetSurfaceVelocity: TPointD;
    procedure SetSurfaceVelocity(const Value: TPointD);
    function GetUserData: Pointer;
    procedure SetUserData(Value: Pointer);
    function GetGroup: Pointer;
    procedure SetGroup(Value: Pointer);
    function GetCategories: TBitmask;
    procedure SetCategories(Value: TBitmask);
    function GetMask: TBitmask;
    procedure SetMask(Value: TBitmask);
    function GetCollisionType: TCollisionType;
    procedure SetCollisionType(Value: TCollisionType);
  public
    procedure Free;
    function IsNil: Boolean; inline;
    function AsCircle: TCircle; inline;
    function AsPolygon: TPolygon; inline;
    function AsSegment: TSegment; inline;
    function PointQuery(const P: TPointD; out Info: cpPointQueryInfoStruct): Double;
    function SegmentQuery(const A, B: TPointD; Radius: Double; out Info: cpSegmentQueryInfoStruct): Boolean;
    function Collide(Shape: TShape): cpContactPointSetStruct;
    { The next shape sibling for its body }
    property Next: TShape read GetNext;
    { The kind of the shape }
    property Kind: TShapeKind read GetKind;
    { Mass and desity are mutlally exclusive. It's best to set the density and
      allow the physics engine determine the mass based on the area of the shape }
    property Mass: Double read GetMass write SetMass;
    property Density: Double read GetDensity write SetDensity;
    property CenterOfGravity: TPointD read GetCenterOfGravity;
    property BoundingBox: cpBB read GetBoundingBox;
    { When a shape is set as a sensor it does not cause changes to physics, but
      it can be used to with cpCollisionHandler beginFunc and separateFunc to
      detect collisions }
    property Sensor: Boolean read GetSensor write SetSensor;
    { The bounciness of a shape is determined by multiplying the elasticity of
      two shapes. A product of 0 gives no boucnce while 1 represents a perfect
      bounce }
    property Elasticity: Double read GetElasticity write SetElasticity;
    { Friction adds dampening to a shape. A value of 0 is frictionless. Total
      friction is a product of two shapes. }
    property Friction: Double read GetFriction write SetFriction;
    { Surface velocity is used to in the calculation of moving shape friction,
    such as a conveyor belt.}
    property SurfaceVelocity: TPointD read GetSurfaceVelocity write SetSurfaceVelocity;
    { USer definable data associated with the shape }
    property UserData: Pointer read GetUserData write SetUserData;
    { Collision filtering explained

      Shapes in the same group do not collide with each other. This overrides
      the categories and mask properties below.

      By default a shape belongs to the nil group is eligible to collide
      with everything. Shapes will only collide if it belongs a dynamic
      body, is in a different group than the other shape, and the bits of
      the two shapes categories and mask are a non zero value when bitwise
      `and`ed together. }
    property Group: Pointer read GetGroup write SetGroup;
    { Bitmask defining which categories a shape belongs.
      Note: defaults to all or $FFFFFFFF }
    property Categories: TBitmask read GetCategories write SetCategories;
    { Bitmask defining which categories of other shapes it is allowed to collide.
      Note: defaults to all or $FFFFFFFF }
    property Mask: TBitmask read GetMask write SetMask;
    { See: cpSpaceAddCollisionHandler for more information }
    property CollisionType: TCollisionType read GetCollisionType write SetCollisionType;
  end;

{ TBodyShapeEnumerator }

  TBodyShapeEnumerator = record
  private
    FBody: cpBody;
    FRoot: cpShape;
    FCurrent: TShape;
  public
    class function Create(Body: cpBody): TBodyShapeEnumerator; static;
    function MoveNext: Boolean;
    property Current: TShape read FCurrent;
    function GetEnumerator: TBodyShapeEnumerator;
  end;

{ TBody }

  TBody = record
  public
    Ref: cpBody;
    class operator Implicit(const Value: TBody): cpBody; inline;
    class operator Implicit(const Value: cpBody): TBody; inline;
    class operator Equal(const A, B: TBody): Boolean;
    class operator NotEqual(const A, B: TBody): Boolean;
  private
    function GetKind: TBodyKind;
    procedure SetKind(Value: TBodyKind);
    function GetAngle: Double;
    procedure SetAngle(const Value: Double);
    function GetPosition: TPointD;
    procedure SetPosition(const Value: TPointD);
    function GetShape: TShape;
    function GetUserData: Pointer;
    procedure SetUserData(Value: Pointer);
    function GetShapes: TBodyShapeEnumerator;
  public
    procedure Free;
    function IsNil: Boolean; inline;
    { Add shape to the body. Note: A default density one 1 is set on all shapes.
      As shapes are added to bodies they become a rigid part of the body and
      do not move or collide in relation to other shapes in the same body }
    function NewCircle(Radius: Double): TShape; overload;
    function NewCircle(Radius: Double; const Offset: TPointD): TShape; overload;
    function NewSegment(const A, B: TPointD; Border: Double = 0): TShape;
    function NewBox(Width, Height: Double; Border: Double = 0; Transform: PMatrix2D = nil): TShape; overload;
    function NewBox(X, Y, Width, Height: Double; Border: Double = 0; Transform: PMatrix2D = nil): TShape; overload;
    function NewPolygon(Verts: PPointD; Count: Integer; Border: Double = 0; Transform: PMatrix2D = nil): TShape;

    { TODO: Rethink design of add/remove as related to space copies }

    { Force a dynamic body and every other dynamic body it touches to fall
      asleep immediately. Sleeping bodies do not consume physics calculations
      unless they are awoken or are hit by a non-sleeping body }
    procedure Sleep;
    { Join two groups of dynamic sleeping bodies together }
    procedure SleepWithGroup(Body: TBody);
    { Test if a dynamic body is sleeping }
    function IsSleeping: Boolean;
    { Wake up a this body and anything it touches. Note: Works on non-dynamic
      bodies as well }
    procedure WakeUp;
    { Translate a body point to a world point }
    function BodyToWorld(const Point: TPointD): TPointD;
    { Translate a world point to a body point }
    function WorldToBody(const Point: TPointD): TPointD;
    {procedure ApplyWorldForce
    procedure ApplyBodyForce
    procedure ApplyPointForce
    procedure ApplyWorldImpulse
    procedure ApplyBodyImpulse
    procedure VelocityAtWorld
    procedure VelocityAtBody}

    { Kind can be either dynamic, kinematic, or static. Note: Kind is set for
      you automatically during body creation }
    property Kind: TBodyKind read GetKind write SetKind;

    property Angle: Double read GetAngle write SetAngle;
    property Position: TPointD read GetPosition write SetPosition;

    {property Mass: Double Get Set;
    property Moment: Double Get Set;
    property CenterOfGravity: TPointD Get Set;
    property Velocity: TPointD Get Set;
    property Force: TPointD Get Set;
    property Angle: Double Get Set;
    property AngularVelocity: Double Get Set;
    property Torque: Double Get Set;
    property Rotation: TPointD Get;}

    { The first shape if one exists or a nil reference }
    property Shape: TShape read GetShape;
    { User assignable data associated with the body}
    property UserData: Pointer read GetUserData write SetUserData;
    { A for...in enumerator for the shapes in this body }
    property Shapes: TBodyShapeEnumerator read GetShapes;
  end;

{ TPinJoint }

	TPinJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TPinJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TPinJoint; inline;
  private
    function GetPinA: TPointD;
    procedure SetPinA(const Value: TPointD);
    function GetPinB: TPointD;
    procedure SetPinB(const Value: TPointD);
  public
    property PinA: TPointD read GetPinA write SetPinA;
    property PinB: TPointD read GetPinB write SetPinB;
  end;

{ TSlideJoint }

	TSlideJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TSlideJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TSlideJoint; inline;
  end;

{ TPivotJoint }

	TPivotJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TPivotJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TPivotJoint; inline;
  end;

{ TGrooveJoint }

	TGrooveJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TGrooveJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TGrooveJoint; inline;
  end;

{ TDampedSpringJoint }

	TDampedSpringJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TDampedSpringJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TDampedSpringJoint; inline;
  private
    function GetPinA: TPointD;
    procedure SetPinA(const Value: TPointD);
    function GetPinB: TPointD;
    procedure SetPinB(const Value: TPointD);
    function GetRestLength: Double;
    procedure SetRestLength(Value: Double);
    function GetStiffness: Double;
    procedure SetStiffness(Value: Double);
    function GetDamping: Double;
    procedure SetDamping(Value: Double);
  public
    property PinA: TPointD read GetPinA write SetPinA;
    property PinB: TPointD read GetPinB write SetPinB;
    property RestLength: Double read GetRestLength write SetRestLength;
    property Stiffness: Double read GetStiffness write SetStiffness;
    property Damping: Double read GetDamping write SetDamping;
  end;

{ TDampedRotarySpringJoint }

	TDampedRotarySpringJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TDampedRotarySpringJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TDampedRotarySpringJoint; inline;
  end;

{ TRotaryLimitJoint }

	TRotaryLimitJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TRotaryLimitJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TRotaryLimitJoint; inline;
  end;

{ TRatchetJoint }

	TRatchetJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TRatchetJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TRatchetJoint; inline;
  end;

{ TGearJoint }

	TGearJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TGearJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TGearJoint; inline;
  end;

{ TMotorJoint }

  TMotorJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TMotorJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TMotorJoint; inline;
  private
    function GetRate: Double;
    procedure SetRate(Value: Double);
  public
    property Rate: Double read GetRate write SetRate;
  end;

{ TJoint }

  TJoint = record
  public
    Ref: cpConstraint;
    class operator Implicit(const Value: TJoint): cpConstraint; inline;
    class operator Implicit(const Value: cpConstraint): TJoint; inline;
  private
    function GetKind: TJointKind;
    function GetA: TBody;
    function GetB: TBody;
    function GetCollide: Boolean;
    procedure SetCollide(Value: Boolean);
    function GetErrorBias: Double;
    procedure SetErrorBias(Value: Double);
    function GetMaxBias: Double;
    procedure SetMaxBias(Value: Double);
    function GetMaxForce: Double;
    procedure SetMaxForce(Value: Double);
    function GetUserData: Pointer;
    procedure SetUserData(Value: Pointer);
  public
    procedure Free;
    function IsNil: Boolean;
    function IsGrab: Boolean;
    function AsPin: TPinJoint; inline;
    function AsSlide: TSlideJoint; inline;
    function AsPivot: TPivotJoint; inline;
    function AsGroove: TGrooveJoint; inline;
    function AsDampedSpring: TDampedSpringJoint; inline;
    function AsDampedRotarySpring: TDampedRotarySpringJoint; inline;
    function AsRotaryLimit: TRotaryLimitJoint; inline;
    function AsRatchet: TRatchetJoint; inline;
    function AsGear: TGearJoint; inline;
    function AsMotor: TMotorJoint; inline;
    property Kind: TJointKind read GetKind;
    { The first body in the joint }
    property A: TBody read GetA;
    { The second body in the joint }
    property B: TBody read GetB;
    { When collide bodies is set to false A an B will pass through each other }
    property CollideBodies: Boolean read GetCollide write SetCollide;
    property ErrorBias: Double read GetErrorBias write SetErrorBias;
    property MaxBias: Double read GetMaxBias write SetMaxBias;
    property MaxForce: Double read GetMaxForce write SetMaxForce;
    property UserData: Pointer read GetUserData write SetUserData;
  end;

{ TSpaceBodyEnumerator }

  TSpaceBodyEnumerator = record
  private
    FSpace: cpSpace;
    FIndex: Integer;
    FCurrent: TBody;
  public
    class function Create(Space: cpSpace): TSpaceBodyEnumerator; static;
    function MoveNext: Boolean;
    property Current: TBody read FCurrent;
    function GetEnumerator: TSpaceBodyEnumerator;
  end;

{ TSpaceJointEnumerator }

  TSpaceJointEnumerator = record
  private
    FSpace: cpSpace;
    FIndex: Integer;
    FCurrent: TJoint;
  public
    class function Create(Space: cpSpace): TSpaceJointEnumerator; static;
    function MoveNext: Boolean;
    property Current: TJoint read FCurrent;
    function GetEnumerator: TSpaceJointEnumerator;
  end;

{ TSpace contains your physics simulation }

  TSpace = record
  public
    Ref: cpSpace;
    class operator Implicit(const Value: TSpace): cpSpace; inline;
    class operator Implicit(const Value: cpSpace): TSpace; inline;
  private
    function GetGround: TBody;
    function GetIterations: LongWord;
    procedure SetIterations(Value: LongWord);
    function GetDamping: Double;
    procedure SetDamping(Value: Double);
    function GetGravity: TPointD;
    procedure SetGravity(Value: TPointD);
    function GetIdleSpeedThreshold: Double;
    procedure SetIdleSpeedThreshold(Value: Double);
    function GetSleepTimeThreshold: Double;
    procedure SetSleepTimeThreshold(Value: Double);
    function GetCollisionSlop: Double;
    procedure SetCollisionSlop(Value: Double);
    function GetCollisionBias: Double;
    procedure SetCollisionBias(Value: Double);
    function GetCollisionPersistence: LongWord;
    procedure SetCollisionPersistence(Value: LongWord);
    function GetUserData: Pointer;
    procedure SetUserData(Value: Pointer);
    function GetBodies: TSpaceBodyEnumerator;
    function GetJoints: TSpaceJointEnumerator;
  public
    procedure Free;
    function IsNil: Boolean; inline;
    { Release all bodies }
    procedure ReleaseBodies;
    { Release all joints }
    procedure ReleaseJoints;
    { Release all bodies, shapes, and constraints in this space }
    procedure ReleaseObjects;
    { Calculate changes to all bodies }
    procedure Step(DeltaTime: Double);
    { Add a custom collision handler for shapes of type A and B  }
    function AddCollisionHandler(A, B: TCollisionType): TCollisionHandler;

    {$region Bodies
      Create a variety of bodies in this space.

      Note: You are required to add at least one shape to every body or a
      segment fault will occur when physics changes are calculate using the
      method Step() directly above this comment.

      See also: AddCircle, AddSegment, AddBox and and AddPolyon. }
    { Create a dynamic body in this space. A dynamic body reacts to and is
      updated by physics  }
    function NewBody: TBody;
    { Create a kinematic body in  this space. A kinematic body is one you can
      move but is not updated by physics }
    function NewKinematicBody: TBody;
    { Create a static body in this space. A static body is one you cannot move
      and is not updated by physics }
    function NewStaticBody: TBody;
    {$endregion}

    {$region Create Joints
      Create a variety of joints in this space.

      Note: Arguments such as pins or pivots should be in world coordinates. }
    function NewPin(A, B: TBody; const PinA, PinB: TPointD): TJoint;
    function NewSlide(A, B: TBody; const PinA, PinB: TPointD; Min, Max: Double): TJoint;
    function NewPivot(A, B: TBody; const Pivot: TPointD): TJoint;
    function NewGroove(A, B: TBody; const GrooveA, GrooveB, PinB: TPointD): TJoint;
    function NewDampedSpring(A, B: TBody; const PinA, PinB: TPointD; RestLength,
      Stiffness, Damping: Double): TJoint;
    function NewDampedRotarySpring(A, B: TBody; RestAngle, Stiffness,
      Damping: Double): TJoint;
    function NewRotaryLimit(A, B: TBody; Min, Max: Double): TJoint;
    function NewRatchet(A, B: TBody; Phase, Ratchet: Double): TJoint;
    function NewGear(A, B: TBody; Phase, Ratio: Double): TJoint;
    { To simulate an axle tie one body to a static body such as ground }
    function NewMotor(A, B: TBody; Rate: Double): TJoint;
    {$endregion}

    {$region Management
      Add or remove items to this space. }
    function Add(Body: TBody): TBody; overload;
    function Add(Joint: TJoint): TJoint; overload;
    function Add(Shape: TShape): TShape; overload;
    procedure Remove(Body: TBody); overload;
    procedure Remove(Joint: TJoint); overload;
    procedure Remove(Shape: TShape); overload;
    function Contains(Body: TBody): Boolean; overload;
    function Contains(Joint: TJoint): Boolean; overload;
    function Contains(Shape: TShape): Boolean; overload;
    {$endregion}

    { Ground is a built in static body. Note: Do not remove or free this body }
    property Ground: TBody read GetGround;
    { The number of iterations in each step. The default value is 3 }
    property Iterations: LongWord read GetIterations write SetIterations;
    { Damping on space is a global reduction of movement applied to dynamic bodies
      with each step }
    property Damping: Double read GetDamping write SetDamping;
    { Force applied to all dynamic bodies }
    property Gravity: TPointD read GetGravity write SetGravity;
    { Speed limit under which dynamic bodies are put to spleep, Increasing this
      value causes bodies to sleep faster }
    property IdleSpeedThreshold: Double read GetIdleSpeedThreshold write SetIdleSpeedThreshold;
    { Amount of time when a dynamic body is put to sleep while idle. The default
      value is 0, indicating that bodies will never sleep  }
    property SleepTimeThreshold: Double read GetSleepTimeThreshold write SetSleepTimeThreshold;
    property CollisionSlop: Double read GetCollisionSlop write SetCollisionSlop;
    property CollisionBias: Double read GetCollisionBias write SetCollisionBias;
    property CollisionPersistence: LongWord read GetCollisionPersistence write SetCollisionPersistence;
    property UserData: Pointer read GetUserData write SetUserData;

    {$region Enumerators}
    { A for...in enumerator for all bodies (except ground) inside this space }
    property Bodies: TSpaceBodyEnumerator read GetBodies;
    { A for...in enumerator for all joints inside this space }
    property Joints: TSpaceJointEnumerator read GetJoints;
    {$endregion}
  end;

{ TCircleHelper }

  TCircleHelper = record helper for TCircle
  public
    function Base: TShape; inline;
  end;

{ TSegmentHelper }

  TSegmentHelper = record helper for TSegment
  public
    function Base: TShape; inline;
  end;

{ TPolygonHelper }

  TPolygonHelper = record helper for TPolygon
  public
    function Base: TShape; inline;
  end;

{ TShapeHelper }

  TShapeHelper = record helper for TShape
  public
    function Body: TBody; inline;
    function Space: TSpace; inline;
  end;

{ TPinJointHelper }

  TPinJointHelper = record helper for TPinJoint
  public
    function Base: TJoint; inline;
  end;

{ TSlideJointHelper }

  TSlideJointHelper = record helper for TSlideJoint
  public
    function Base: TJoint; inline;
  end;

{ TPivotJointHelper }

  TPivotJointHelper = record helper for TPivotJoint
  public
    function Base: TJoint; inline;
  end;

{ TGrooveJointHelper }

  TGrooveJointHelper = record helper for TGrooveJoint
  public
    function Base: TJoint; inline;
  end;

{ TDampedSpringJointHelper }

  TDampedSpringJointHelper = record helper for TDampedSpringJoint
  public
    function Base: TJoint; inline;
  end;

{ TDampedRotarySpringJointHelper }

  TDampedRotarySpringJointHelper = record helper for TDampedRotarySpringJoint
  public
    function Base: TJoint; inline;
  end;

{ TRotaryLimitJointHelper }

  TRotaryLimitJointHelper = record helper for TRotaryLimitJoint
  public
    function Base: TJoint; inline;
  end;

{ TRatchetJointHelper }

  TRatchetJointHelper = record helper for TRatchetJoint
  public
    function Base: TJoint; inline;
  end;

{ TGearJointHelper }

  TGearJointHelper = record helper for TGearJoint
  public
    function Base: TJoint; inline;
  end;

{ TMotorJointHelper }

  TMotorJointHelper = record helper for TMotorJoint
  public
    function Base: TJoint; inline;
  end;

{ TJointHelper }

  TJointHelper = record helper for TJoint
  public
    function Space: TSpace; inline;
  end;

{ TBodyHelper }

  TBodyHelper = record helper for TBody
  public
    function Space: TSpace; inline;
  end;

{ TBodyCollection }

  (*TBodyCollection = class
  private
    FList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    { The  }
    procedure AddRoot(Body: TBody)
    { Add a body at point }
    procedure Add(Body: TBody; const Point: TPointD);
    procedure Remove(Body: TBody);
    procedure Delete(Index: Integer);
    function Contains(Body: TBody): Boolean;
    function IndexOf(Body: TBody): Integer;
    procedure Move(Position: TPointD);
    procedure Rotate(Angle: Double);
    { Force is applied from  }
    procedure ApplyForce(const P, F: TPointD);
    procedure ApplyVelocity(V: TPointD);
    procedure Sleep;
    procedure Wakeup;
    property Pivot: TPointD read GetPivot write SetPivot;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: TShape read GetItem;
  end;*)


function NewSpace: TSpace;

implementation

function Vect(X, Y: Double): TPointD;
begin
  Result.X := X; Result.Y := Y;
end;

function MaskToStr(M: TBitmask): string;
const
  Bit: array[0..1] of Char = ('0', '1');
var
  I: Integer;
begin
  Result := '';
  SetLength(Result, 32);
  for I := 32 downto 1 do
  begin
    Result[I] := Bit[M and 1];
    M := M shr 1;
  end;
end;

function StrToMask(const S: string): TBitmask;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(S) do
  begin
    if I = 33 then
      Exit;
    Result := Result shl 1;
    if S[I] <> '0' then
      Result := Result or 1;
  end;
end;

{ TMatrix2D }

class operator TMatrix2D.Implicit(const Value: TMatrix2D): cpTransform;
begin
  Result := Value.Ref;
end;

class operator TMatrix2D.Implicit(const Value: cpTransform): TMatrix2D;
begin
  Result.Ref := Value;
end;

class operator TMatrix2D.Multiply(const A, B: TMatrix2D): TMatrix2D;
var
  T1: cpTransform absolute A;
  T2: cpTransform absolute B;
  R: cpTransform absolute Result;
begin
  R.a := T1.a * T2.a + T1.c * T2.b;
  R.b := T1.b * T2.a + T1.d * T2.b;
  R.c := T1.a * T2.c + T1.c * T2.d;
  R.d := T1.b * T2.c + T1.d * T2.d;
  R.tx := T1.a * T2.tx + T1.c * T2.ty + T1.tx;
  R.ty := T1.b * T2.tx + T1.d * T2.ty + T1.ty;
end;

class operator TMatrix2D.Multiply(const A: TMatrix2D; const B: TPointD): TPointD;
var
  T: cpTransform absolute A;
  R: TPointD absolute Result;
begin
  R.X := T.a * B.X + T.c * B.Y + T.tx;
  R.Y := T.b * B.X + T.d * B.Y + T.ty;
end;

class function TMatrix2D.Compose(const a, b, c, d, tx, ty: Double): TMatrix2D;
var
  R: TMatrix2D absolute Result;
begin
  R.Ref.a := a; R.Ref.b := b; R.Ref.c := c;
  R.Ref.d := d; R.Ref.tx := tx; R.Ref.ty := ty;
end;

class function TMatrix2D.Transpose(const a, c, tx, b, d, ty: Double): TMatrix2D;
var
  R: TMatrix2D absolute Result;
begin
  R.Ref.a := a; R.Ref.b := b; R.Ref.c := c;
  R.Ref.d := d; R.Ref.tx := tx; R.Ref.ty := ty;
end;

procedure TMatrix2D.Identity;
begin
  Ref := MatrixIdentity;
end;

procedure TMatrix2D.Translate(const X, Y: Double);
begin
  Ref := (Transpose(1, 0, X, 0, 1, Y) * Self).Ref;
end;

procedure TMatrix2D.Rotate(const Angle: Double);
var
  X, Y: Double;
begin
  X := Cos(Angle);
  Y := Sin(Angle);
  Ref := (Transpose(X, -Y, 0, Y, X, 0) * Self).Ref;
end;

procedure TMatrix2D.Scale(const X, Y: Double);
begin
  Ref := (Transpose(X, 0, 0, 0, Y, 0) * Self).Ref;
end;

{ TCircle }

class operator TCircle.Implicit(const Value: TCircle): cpCircleShape;
begin
  Result := Value.Ref;
end;

class operator TCircle.Implicit(const Value: cpCircleShape): TCircle;
begin
  Result.Ref := Value;
end;

class operator TCircle.Implicit(const Value: TCircle): cpShape;
begin
  Result := Pointer(Value.Ref);
end;

class operator TCircle.Implicit(const Value: cpShape): TCircle;
begin
  Result.Ref := Pointer(Value);
end;

procedure TCircle.Free;
begin
  if Ref <> nil then
    cpShapeFree(Self);
  Ref := nil;
end;

function TCircle.IsNil: Boolean;
begin
  Result := Ref = nil;
end;

procedure TCircle.Reshape(Radius: Double; const Offset: TPointD);
begin
  cpCircleShapeSetRadius(Self, Radius);
  cpCircleShapeSetOffset(Self, Offset);
end;

function TCircle.GetRadius: Double;
begin
  Result := cpCircleShapeGetRadius(Self);
end;

procedure TCircle.SetRadius(const Value: Double);
begin
  cpCircleShapeSetRadius(Self, Value);
end;

function TCircle.GetOffset: TPointD;
begin
  Result := cpCircleShapeGetOffset(Self);
end;

procedure TCircle.SetOffset(const Value: TPointD);
begin
  cpCircleShapeSetOffset(Self, Value);
end;

{ TPolygon }

class operator TPolygon.Implicit(const Value: TPolygon): cpPolyShape;
begin
  Result := Value.Ref;
end;

class operator TPolygon.Implicit(const Value: cpPolyShape): TPolygon;
begin
  Result.Ref := Value;
end;

class operator TPolygon.Implicit(const Value: TPolygon): cpShape;
begin
  Result := Pointer(Value.Ref);
end;

class operator TPolygon.Implicit(const Value: cpShape): TPolygon;
begin
  Result.Ref := Pointer(Value);
end;

procedure TPolygon.Free;
begin
  if Ref <> nil then
    cpShapeFree(Self);
  Ref := nil;
end;

function TPolygon.IsNil: Boolean;
begin
  Result := Ref = nil;
end;

procedure TPolygon.Reshape(Verts: PPointD; Count: Integer; Transform: PMatrix2D = nil);
begin
  if Transform <> nil then
    cpPolyShapeSetVerts(Self, Count, Verts, Transform.Ref)
  else
    cpPolyShapeSetVerts(Self, Count, Verts, MatrixIdentity);
end;

procedure TPolygon.Reshape(const Width, Height: Double; Transform: PMatrix2D = nil);
var
  V: array[0..3] of TPointD;
begin
  V[0].X := Width / -2; V[0].Y := Height / -2;
  V[1].X := V[0].X; V[1].Y := -V[0].Y;
  V[2].X := -V[0].X; V[2].Y := V[0].Y;
  V[3].X := V[0].X; V[3].Y := -V[0].Y;
  if Transform <> nil then
    cpPolyShapeSetVerts(Self, 4, @V, Transform.Ref)
  else
    cpPolyShapeSetVerts(Self, 4, @V, MatrixIdentity);
end;

function TPolygon.GetBorder: Double;
begin
  Result := cpPolyShapeGetRadius(Self);
end;

procedure TPolygon.SetBorder(const Value: Double);
begin
  cpPolyShapeSetRadius(Self, Value);
end;

function TPolygon.GetVertCount: Integer;
begin
  Result := cpPolyShapeGetCount(Self);
end;

function TPolygon.GetVert(Index: Integer): TPointD;
begin
  Result := cpPolyShapeGetVert(Self, Index);
end;

{ TSegment }

class operator TSegment.Implicit(const Value: TSegment): cpSegmentShape;
begin
  Result := Value.Ref;
end;

class operator TSegment.Implicit(const Value: cpSegmentShape): TSegment;
begin
  Result.Ref := Value;
end;

class operator TSegment.Implicit(const Value: TSegment): cpShape;
begin
  Result := Pointer(Value.Ref);
end;

class operator TSegment.Implicit(const Value: cpShape): TSegment;
begin
  Result.Ref := Pointer(Value);
end;

procedure TSegment.Free;
begin
  if Ref <> nil then
    cpShapeFree(Self);
  Ref := nil;
end;

function TSegment.IsNil: Boolean;
begin
  Result := Ref = nil;
end;

procedure TSegment.Join(const Prev, Next: TPointD);
begin
  cpSegmentShapeSetNeighbors(Ref, Prev, Next);
end;

function TSegment.GetBorder: Double;
begin
  Result := cpSegmentShapeGetRadius(Self);
end;

procedure TSegment.SetBorder(const Value: Double);
begin
  cpSegmentShapeSetRadius(Self, Value);
end;

function TSegment.GetA: TPointD;
begin
  Result := cpSegmentShapeGetA(Self);
end;

procedure TSegment.SetA(const Value: TPointD);
begin
  cpSegmentShapeSetEndpoints(Self, Value, cpSegmentShapeGetB(Self));
end;

function TSegment.GetB: TPointD;
begin
  Result := cpSegmentShapeGetB(Self);
end;

procedure TSegment.SetB(const Value: TPointD);
begin
  cpSegmentShapeSetEndpoints(Self, cpSegmentShapeGetA(Self), Value);
end;

function TSegment.GetNormal: TPointD;
begin
  Result := cpSegmentShapeGetNormal(Self);
end;

{ TShape }

class operator TShape.Implicit(const Value: TShape): cpShape;
begin
  Result := Value.Ref;
end;

class operator TShape.Implicit(const Value: cpShape): TShape;
begin
  Result.Ref := Value;
end;

class operator TShape.Equal(const A, B: TShape): Boolean;
begin
  Result := A.Ref = B.Ref;
end;

class operator TShape.NotEqual(const A, B: TShape): Boolean;
begin
  Result := A.Ref <> B.Ref;
end;

procedure TShape.Free;
begin
  if Ref <> nil then
  begin
    cpSpaceRemoveShape(TShape(Self).Body.Space, Ref);
    cpBodyRemoveShape(TShape(Self).Body, Ref);
    cpShapeFree(Ref);
    Ref := nil;
  end;
end;

function TShape.IsNil: Boolean;
begin
  Result := Ref = nil;
end;

function TShape.AsCircle: TCircle;
begin
  Result.Ref := Pointer(Ref);
end;

function TShape.AsPolygon: TPolygon;
begin
  Result.Ref := Pointer(Ref);
end;

function TShape.AsSegment: TSegment;
begin
  Result.Ref := Pointer(Ref);
end;

function TShape.PointQuery(const P: TPointD; out Info: cpPointQueryInfoStruct): Double;
begin
  Result := cpShapePointQuery(Ref, P, Info);
end;

function TShape.SegmentQuery(const A, B: TPointD; Radius: Double; out Info: cpSegmentQueryInfoStruct): Boolean;
begin
  Result := cpShapeSegmentQuery(Ref, A, B, Radius, Info) <> cpFalse;
end;

function TShape.Collide(Shape: TShape): cpContactPointSetStruct;
begin
  Result := cpShapesCollide(Ref, Shape.Ref);
end;

function TShape.GetNext: TShape;
begin
  Result.Ref := nil;
  if IsNil then Exit;
  Result.Ref := cpShape(Ref).next;
end;

function TShape.GetKind: TShapeKind;
begin
  Result := cpShapeClass(Ref.klass)._type;
end;

function TShape.GetGroup: Pointer;
begin
  Result := Ref.filter.group;
end;

procedure TShape.SetGroup(Value: Pointer);
begin
  Ref.filter.group := Value
end;

function TShape.GetCategories: TBitmask;
begin
  Result := Ref.filter.categories;
end;

procedure TShape.SetCategories(Value: TBitmask);
begin
  Ref.filter.categories := Value;
end;

function TShape.GetMask: TBitmask;
begin
  Result := Ref.filter.mask;
end;

procedure TShape.SetMask(Value: TBitmask);
begin
  Ref.filter.mask := Value;
end;

function TShape.GetMass: Double;
begin
  Result := cpShapeGetMass(Ref);
end;

procedure TShape.SetMass(Value: Double);
begin
  cpShapeSetMass(Ref, Value);
end;

function TShape.GetDensity: Double;
begin
  Result := cpShapeGetDensity(Ref);
end;

procedure TShape.SetDensity(Value: Double);
begin
  cpShapeSetDensity(Ref, Value);
end;

function TShape.GetArea: Double;
begin
  Result := cpShapeGetArea(Ref);
end;

function TShape.GetCenterOfGravity: TPointD;
begin
  Result := cpShapeGetCenterOfGravity(Ref);
end;

function TShape.GetBoundingBox: cpBB;
begin
  Result := cpShapeGetBB(Ref);
end;

function TShape.GetSensor: Boolean;
begin
  Result := cpShapeGetSensor(Ref) <> cpFalse;
end;

procedure TShape.SetSensor(Value: Boolean);
begin
  cpShapeSetSensor(Ref, BoolFlags[Value]);
end;

function TShape.GetElasticity: Double;
begin
  Result := cpShapeGetElasticity(Ref);
end;

procedure TShape.SetElasticity(Value: Double);
begin
  cpShapeSetElasticity(Ref, Value);
end;

function TShape.GetFriction: Double;
begin
  Result := cpShapeGetFriction(Ref);
end;

procedure TShape.SetFriction(Value: Double);
begin
  cpShapeSetFriction(Ref, Value);
end;

function TShape.GetSurfaceVelocity: TPointD;
begin
  Result := cpShapeGetSurfaceVelocity(Ref);
end;

procedure TShape.SetSurfaceVelocity(const Value: TPointD);
begin
  cpShapeSetSurfaceVelocity(Ref, Value);
end;

function TShape.GetUserData: Pointer;
begin
  Result := cpShapeGetUserData(Ref);
end;

procedure TShape.SetUserData(Value: Pointer);
begin
  cpShapeSetUserData(Ref, Value);
end;

function TShape.GetCollisionType: TCollisionType;
begin
  Result := cpShapeGetCollisionType(Ref);
end;

procedure TShape.SetCollisionType(Value: TCollisionType);
begin
  cpShapeSetCollisionType(Ref, Value);
end;

{ TBodyShapeEnumerator }

class function TBodyShapeEnumerator.Create(Body: cpBody): TBodyShapeEnumerator;
begin
  Result.FBody := Body;
  Result.FRoot := nil;
  Result.FCurrent.Ref := nil;
end;

function TBodyShapeEnumerator.MoveNext: Boolean;
begin
  if FRoot = nil then
  begin
    FRoot := cpShape(FBody.shapeList);
    FCurrent.Ref := FRoot;
    Result := FRoot <> nil;
  end
  else
  begin
    FCurrent.Ref := FCurrent.Ref.next;
    Result := (FCurrent.Ref <> nil) and (FCurrent.Ref <> FRoot);
  end;
end;

function TBodyShapeEnumerator.GetEnumerator: TBodyShapeEnumerator;
begin
  Result := Self;
end;

{ TBody }

class operator TBody.Implicit(const Value: TBody): cpBody;
begin
  Result := Value.Ref;
end;

class operator TBody.Implicit(const Value: cpBody): TBody;
begin
  Result.Ref := Value;
end;

class operator TBody.Equal(const A, B: TBody): Boolean;
begin
  Result := A.Ref = B.Ref;
end;

class operator TBody.NotEqual(const A, B: TBody): Boolean;
begin
  Result := A.Ref <> B.Ref;
end;

procedure TBody.Free;
var
  List: TArrayList<TShape>;
  Space: TSpace;
  S: TShape;
  I: Integer;
begin
  if IsNil then Exit;
  I := 0;
  S := Shape;
  while S <> nil do
  begin
    Inc(I);
    S := S.Next;
  end;
  List.Length := I;
  I := 0;
  S := Shape;
  while S <> nil do
  begin
    List[I] := S;
    Inc(I);
    S := S.Next;
  end;
  Space := cpBodyGetSpace(Ref);
  for I := 0 to List.Length - 1 do
  begin
    cpSpaceRemoveShape(Space, List[I].Ref);
    cpBodyRemoveShape(Ref, List[I].Ref);
  end;
  cpSpaceRemoveBody(Space, Ref);
  for I := 0 to List.Length - 1 do
    cpShapeFree(List[I].Ref);
  cpBodyFree(Ref);
  Ref := nil;
end;

function TBody.IsNil: Boolean;
begin
  Result := Ref = nil;
end;

function TBody.NewCircle(Radius: Double): TShape;
begin
  Result.Ref := cpSpaceAddShape(cpBodyGetSpace(Ref), cpCircleShapeNew(Ref, Radius, VectZero));
  cpShapeSetDensity(Result.Ref, 1);
end;

function TBody.NewCircle(Radius: Double; const Offset: TPointD): TShape;
begin
  Result.Ref := cpSpaceAddShape(cpBodyGetSpace(Ref), cpCircleShapeNew(Ref, Radius, Offset));
  cpShapeSetDensity(Result.Ref, 1);
end;

function TBody.NewBox(Width, Height: Double; Border: Double = 0; Transform: PMatrix2D = nil): TShape;
var
  V: array[0..3] of TPointD;
begin
  V[0].X := Width / -2; V[0].Y := Height / -2;
  V[1].X := V[0].X; V[1].Y := -V[0].Y;
  V[2].X := -V[0].X; V[2].Y := -V[0].Y;
  V[3].X := -V[0].X; V[3].Y := V[0].Y;
  Result := NewPolygon(@V, 4, Border, Transform);
end;

function TBody.NewBox(X, Y, Width, Height: Double; Border: Double = 0; Transform: PMatrix2D = nil): TShape;
var
  V: array[0..3] of TPointD;
begin
  V[0].X := X - Width / 2; V[0].Y := Y - Height / 2;
  V[1].X := V[0].X; V[1].Y := V[0].Y + Height;
  V[2].X := V[0].X + Width; V[2].Y := V[0].Y + Height;
  V[3].X := V[0].X + Width; V[3].Y := V[0].Y;
  Result := NewPolygon(@V, 4, Border, Transform);
end;

function TBody.NewPolygon(Verts: PPointD; Count: Integer; Border: Double = 0; Transform: PMatrix2D = nil): TShape;
begin
  if Transform = nil then
    Result.Ref := cpSpaceAddShape(cpBodyGetSpace(Ref),
      cpPolyShapeNew(Ref, Count, Verts, MatrixIdentity, Border))
  else
    Result.Ref := cpSpaceAddShape(cpBodyGetSpace(Ref),
      cpPolyShapeNew(Ref, Count, Verts, Transform^, Border));
  cpShapeSetDensity(Result.Ref, 1);
end;

function TBody.NewSegment(const A, B: TPointD; Border: Double = 0): TShape;
begin
  Result.Ref := cpSpaceAddShape(cpBodyGetSpace(Ref), cpSegmentShapeNew(Ref, A, B, Border));
  cpShapeSetDensity(Result.Ref, 1);
end;

procedure TBody.Sleep;
begin
  if Kind = bodyDynamic then
    cpBodySleep(Ref);
end;

procedure TBody.SleepWithGroup(Body: TBody);
begin
  if (Kind = bodyDynamic) and (Body.Kind = bodyDynamic) then
    cpBodySleepWithGroup(Ref, Body.Ref);
end;

function TBody.IsSleeping: Boolean;
begin
  if Kind = bodyDynamic then
    Result := cpBodyIsSleeping(Ref) <> cpFalse
  else
    Result := False;
end;

procedure TBody.WakeUp;
begin
  if Kind = bodyDynamic then
    cpBodyActivate(Ref)
  else
    cpBodyActivateStatic(Ref, nil);
end;

function TBody.BodyToWorld(const Point: TPointD): TPointD;
begin
  Result := cpBodyLocalToWorld(Ref, Point);
end;

function TBody.WorldToBody(const Point: TPointD): TPointD;
begin
  Result := cpBodyWorldToLocal(Ref, Point);
end;

function TBody.GetKind: TBodyKind;
begin
  Result := cpBodyGetType(Ref);
end;

procedure TBody.SetKind(Value: TBodyKind);
begin
  cpBodySetType(Ref, Value);
end;

function TBody.GetAngle: Double;
begin
  Result := cpBodyGetAngle(Ref);
end;

procedure TBody.SetAngle(const Value: Double);
begin
  cpBodySetAngle(Ref, Value);
end;

function TBody.GetPosition: TPointD;
begin
  Result := cpBodyGetPosition(Ref);
end;

procedure TBody.SetPosition(const Value: TPointD);
begin
  cpBodySetPosition(Ref, Value);
end;

function TBody.GetShape: TShape;
begin
  Result.Ref := nil;
  if IsNil then Exit;
  Result.Ref := Ref.shapeList;
end;

function TBody.GetUserData: Pointer;
begin
  Result := nil;
  if IsNil then
    Exit;
  Result := cpBodyGetUserData(Ref)
end;

procedure TBody.SetUserData(Value: Pointer);
begin
  if IsNil then
    Exit;
  cpBodySetUserData(Ref, Value);
end;

function TBody.GetShapes: TBodyShapeEnumerator;
begin
  Result := TBodyShapeEnumerator.Create(Ref);
end;

{ TPinJoint }

class operator TPinJoint.Implicit(const Value: TPinJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TPinJoint.Implicit(const Value: cpConstraint): TPinJoint;
begin
  Result.Ref := Value;
end;

function TPinJoint.GetPinA: TPointD;
begin
  Result := cpPinJointGetAnchorA(Pointer(Ref));
end;

procedure TPinJoint.SetPinA(const Value: TPointD);
begin
  cpPinJointSetAnchorA(Pointer(Ref), Value);
end;

function TPinJoint.GetPinB: TPointD;
begin
  Result := cpPinJointGetAnchorB(Pointer(Ref));
end;

procedure TPinJoint.SetPinB(const Value: TPointD);
begin
  cpPinJointSetAnchorB(Pointer(Ref), Value);
end;

{ TSlideJoint }

class operator TSlideJoint.Implicit(const Value: TSlideJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TSlideJoint.Implicit(const Value: cpConstraint): TSlideJoint;
begin
  Result.Ref := Value;
end;

{ TPivotJoint }

class operator TPivotJoint.Implicit(const Value: TPivotJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TPivotJoint.Implicit(const Value: cpConstraint): TPivotJoint;
begin
  Result.Ref := Value;
end;

{ TGrooveJoint }

class operator TGrooveJoint.Implicit(const Value: TGrooveJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TGrooveJoint.Implicit(const Value: cpConstraint): TGrooveJoint;
begin
  Result.Ref := Value;
end;

{ TDampedSpringJoint }

class operator TDampedSpringJoint.Implicit(const Value: TDampedSpringJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TDampedSpringJoint.Implicit(const Value: cpConstraint): TDampedSpringJoint;
begin
  Result.Ref := Value;
end;

function TDampedSpringJoint.GetPinA: TPointD;
begin
  Result := cpDampedSpringGetAnchorA(Ref);
end;

procedure TDampedSpringJoint.SetPinA(const Value: TPointD);
begin
  cpDampedSpringSetAnchorA(Ref, Value);
end;

function TDampedSpringJoint.GetPinB: TPointD;
begin
  Result := cpDampedSpringGetAnchorB(Ref);
end;

procedure TDampedSpringJoint.SetPinB(const Value: TPointD);
begin
  cpDampedSpringSetAnchorB(Ref, Value);
end;

function TDampedSpringJoint.GetRestLength: Double;
begin
  Result := cpDampedSpringGetRestLength(Ref);
end;

procedure TDampedSpringJoint.SetRestLength(Value: Double);
begin
  cpDampedSpringSetRestLength(Ref, Value);
end;

function TDampedSpringJoint.GetStiffness: Double;
begin
  Result := cpDampedSpringGetStiffness(Ref);
end;

procedure TDampedSpringJoint.SetStiffness(Value: Double);
begin
  cpDampedSpringSetStiffness(Ref, Value);
end;

function TDampedSpringJoint.GetDamping: Double;
begin
  Result := cpDampedSpringGetDamping(Ref);
end;

procedure TDampedSpringJoint.SetDamping(Value: Double);
begin
  cpDampedSpringSetDamping(Ref, Value);
end;

{ TDampedRotarySpringJoint }

class operator TDampedRotarySpringJoint.Implicit(const Value: TDampedRotarySpringJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TDampedRotarySpringJoint.Implicit(const Value: cpConstraint): TDampedRotarySpringJoint;
begin
  Result.Ref := Value;
end;

{ TRotaryLimitJoint }

class operator TRotaryLimitJoint.Implicit(const Value: TRotaryLimitJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TRotaryLimitJoint.Implicit(const Value: cpConstraint): TRotaryLimitJoint;
begin
  Result.Ref := Value;
end;

{ TRatchetJoint }

class operator TRatchetJoint.Implicit(const Value: TRatchetJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TRatchetJoint.Implicit(const Value: cpConstraint): TRatchetJoint;
begin
  Result.Ref := Value;
end;

{ TGearJoint }

class operator TGearJoint.Implicit(const Value: TGearJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TGearJoint.Implicit(const Value: cpConstraint): TGearJoint;
begin
  Result.Ref := Value;
end;

{ TMotorJoint }

class operator TMotorJoint.Implicit(const Value: TMotorJoint): cpConstraint;
begin
  Result := Pointer(Value.Ref);
end;

class operator TMotorJoint.Implicit(const Value: cpConstraint): TMotorJoint;
begin
  Result.Ref := Value;
end;

function TMotorJoint.GetRate: Double;
begin
  Result := cpSimpleMotorGetRate(Ref);
end;

procedure TMotorJoint.SetRate(Value: Double);
begin
  cpSimpleMotorSetRate(Ref, Value);
end;

{ TJoint }

class operator TJoint.Implicit(const Value: TJoint): cpConstraint;
begin
  Result := Value.Ref;
end;

class operator TJoint.Implicit(const Value: cpConstraint): TJoint;
begin
  Result.Ref := Value;
end;

procedure TJoint.Free;
begin
  if Ref <> nil then
  begin
    cpSpaceRemoveConstraint(cpConstraintGetSpace(Ref), Ref);
    cpConstraintFree(Ref);
    Ref := nil;
  end;
end;

function TJoint.IsNil: Boolean;
begin
  Result := Ref = nil;
end;

function TJoint.IsGrab: Boolean;
begin
  Result := False;
  if IsNil then
    Exit;
  Result := (A.Shape.CollisionType = collideGrab) or
    (B.Shape.CollisionType = collideGrab);
end;

function TJoint.AsPin: TPinJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.AsSlide: TSlideJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.AsPivot: TPivotJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.AsGroove: TGrooveJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.AsDampedSpring: TDampedSpringJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.AsDampedRotarySpring: TDampedRotarySpringJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.AsRotaryLimit: TRotaryLimitJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.AsRatchet: TRatchetJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.AsGear: TGearJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.AsMotor: TMotorJoint;
begin
  Result.Ref := Pointer(Ref);
end;

function TJoint.GetKind: TJointKind;
begin
  Result := cpConstraintGetJointType(Ref);
end;

function TJoint.GetA: TBody;
begin
  Result.Ref := cpConstraintGetBodyA(Ref);
end;

function TJoint.GetB: TBody;
begin
  Result.Ref := cpConstraintGetBodyB(Ref);
end;

function TJoint.GetCollide: Boolean;
begin
  Result := cpConstraintGetCollideBodies(Ref) <> cpFalse;
end;

procedure TJoint.SetCollide(Value: Boolean);
begin
  cpConstraintSetCollideBodies(Ref, BoolFlags[Value]);
end;

function TJoint.GetErrorBias: Double;
begin
  Result := cpConstraintGetErrorBias(Ref);
end;

procedure TJoint.SetErrorBias(Value: Double);
begin
  cpConstraintSetErrorBias(Ref, Value);
end;

function TJoint.GetMaxBias: Double;
begin
  Result := cpConstraintGetMaxBias(Ref);
end;

procedure TJoint.SetMaxBias(Value: Double);
begin
  cpConstraintSetMaxBias(Ref, Value);
end;

function TJoint.GetMaxForce: Double;
begin
  Result := cpConstraintGetMaxForce(Ref);
end;

procedure TJoint.SetMaxForce(Value: Double);
begin
  cpConstraintSetMaxForce(Ref, Value);
end;

function TJoint.GetUserData: Pointer;
begin
  Result := cpConstraintGetUserData(Ref);
end;

procedure TJoint.SetUserData(Value: Pointer);
begin
  cpConstraintSetUserData(Ref, Value);
end;

{ TSpaceBodyEnumerator }

class function TSpaceBodyEnumerator.Create(Space: cpSpace): TSpaceBodyEnumerator;
begin
  Result.FSpace := Space;
  Result.FIndex := 0;
  Result.FCurrent.Ref := nil;
  cpSpaceLock(Result.FSpace);
end;

function TSpaceBodyEnumerator.MoveNext: Boolean;
var
  B: cpBody;
  I, J: Integer;
begin
  Result := True;
  I := FIndex;
  Inc(FIndex);
  if I < FSpace.dynamicBodies.num then
  begin
    FCurrent.Ref := FSpace.dynamicBodies.arr[I];
    Exit;
  end;
  Dec(I, FSpace.dynamicBodies.num);
  if I < FSpace.staticBodies.num then
  begin
    FCurrent.Ref := FSpace.staticBodies.arr[I];
    Exit;
  end;
  Dec(I, FSpace.staticBodies.num);
  if I < FSpace.sleepingComponents.num then
  begin
    FCurrent.Ref := FSpace.sleepingComponents.arr[I];
    Exit;
  end;
  Dec(I, FSpace.sleepingComponents.num);
  for J := 0 to FSpace.sleepingComponents.num - 1 do
  begin
    B := FSpace.sleepingComponents.arr[J];
    B := B.sleeping.next;
    while B <> nil do
    begin
      if I = 0 then
      begin
        FCurrent.Ref := B;
        Exit;
      end;
      Dec(I);
      B := B.sleeping.next;
    end;
  end;
  FCurrent.Ref := nil;
  Result := False;
  cpSpaceUnlock(FSpace, cpTrue);
end;

function TSpaceBodyEnumerator.GetEnumerator: TSpaceBodyEnumerator;
begin
  Result := Self;
end;

{ TSpaceJointEnumerator }

class function TSpaceJointEnumerator.Create(Space: cpSpace): TSpaceJointEnumerator;
begin
  Result.FSpace := Space;
  Result.FIndex := 0;
  Result.FCurrent.Ref := nil;

end;

function TSpaceJointEnumerator.MoveNext: Boolean;
begin
  if FSpace.constraints.num > FIndex then
  begin
    FCurrent.Ref := FSpace.constraints.arr[FIndex];
    Inc(FIndex);
    Result := True;
  end
  else
  begin
    FCurrent.Ref := nil;
    Result := False;
  end;
end;

function TSpaceJointEnumerator.GetEnumerator: TSpaceJointEnumerator;
begin
  Result := Self;
end;

{ TSpace }

class operator TSpace.Implicit(const Value: TSpace): cpSpace;
begin
  Result := Value.Ref;
end;

class operator TSpace.Implicit(const Value: cpSpace): TSpace;
begin
  Result.Ref := Value;
end;

procedure TSpace.Free;
begin
  if Ref <> nil then
  begin
    ReleaseObjects;
    cpSpaceFree(Ref);
    Ref := nil;
  end;
end;

function TSpace.IsNil: Boolean;
begin
  Result := Ref = nil;
end;

procedure TSpace.ReleaseBodies;
var
  Items: TArrayList<TBody>;
  B: TBody;
begin
  for B in Bodies do
    if B <> Ground then
      Items.Push(B);
  for B in Items do
    B.Free;
end;

procedure TSpace.ReleaseJoints;
var
  Items: TArrayList<TJoint>;
  J: TJoint;
begin
  for J in Joints do
    Items.Push(J);
  for J in Items do
    J.Free;
end;

procedure TSpace.ReleaseObjects;
begin
  cpSpaceDestroyChildren(Ref);
end;

procedure TSpace.Step(DeltaTime: Double);
begin
  if DeltaTime > 0 then
    cpSpaceStep(Ref, DeltaTime);
end;

function TSpace.AddCollisionHandler(A, B: TCollisionType): TCollisionHandler;
begin
  Result := cpSpaceAddCollisionHandler(Ref, A, B);
end;

function TSpace.NewBody: TBody;
begin
  Result.Ref := cpBodyNew(0, 0);
  cpSpaceAddBody(Ref, Result.Ref);
end;

function TSpace.NewKinematicBody: TBody;
begin
  Result.Ref := cpBodyNewKinematic;
  cpSpaceAddBody(Ref, Result.Ref);
end;

function TSpace.NewStaticBody: TBody;
begin
  Result.Ref := cpBodyNewStatic;
  cpSpaceAddBody(Ref, Result.Ref);
end;

function TSpace.NewPin(A, B: TBody; const PinA, PinB: TPointD): TJoint;
begin
  Result.Ref := cpPinJointNew(A.Ref, B.Ref, PinA, PinB);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.NewSlide(A, B: TBody; const PinA, PinB: TPointD; Min, Max: Double): TJoint;
begin
  Result.Ref := cpSlideJointNew(A.Ref, B.Ref, PinA, PinB, Min, Max);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.NewPivot(A, B: TBody; const Pivot: TPointD): TJoint;
begin
  // Result.Ref := cpPivotJointNew(A.Ref, B.Ref, Pivot);
  Result.Ref := cpPivotJointNew2(A.Ref, B.Ref, Pivot, Pivot);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.NewGroove(A, B: TBody; const GrooveA, GrooveB, PinB: TPointD): TJoint;
begin
  Result.Ref := cpGrooveJointNew(A.Ref, B.Ref, GrooveA, GrooveB, PinB);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.NewDampedSpring(A, B: TBody; const PinA, PinB: TPointD; RestLength,
  Stiffness, Damping: Double): TJoint;
begin
  Result.Ref := cpDampedSpringNew(A.Ref, B.Ref, PinA, PinB, RestLength, Stiffness, Damping);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.NewDampedRotarySpring(A, B: TBody; RestAngle, Stiffness,
  Damping: Double): TJoint;
begin
  Result.Ref := cpDampedRotarySpringNew(A.Ref, B.Ref, RestAngle, Stiffness, Damping);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.NewRotaryLimit(A, B: TBody; Min, Max: Double): TJoint;
begin
  Result.Ref := cpRotaryLimitJointNew(A.Ref, B.Ref, Min, Max);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.NewRatchet(A, B: TBody; Phase, Ratchet: Double): TJoint;
begin
  Result.Ref := cpRatchetJointNew(A.Ref, B.Ref, Phase, Ratchet);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.NewGear(A, B: TBody; Phase, Ratio: Double): TJoint;
begin
  Result.Ref := cpGearJointNew(A.Ref, B.Ref, Phase, Ratio);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.NewMotor(A, B: TBody; Rate: Double): TJoint;
begin
  Result.Ref := cpSimpleMotorNew(A.Ref, B.Ref, Rate);
  cpSpaceAddConstraint(Ref, Result.Ref);
end;

function TSpace.Add(Body: TBody): TBody;
begin
  cpSpaceAddBody(Ref, Body.Ref);
  Result := Body;
end;

function TSpace.Add(Joint: TJoint): TJoint;
begin
  cpSpaceAddConstraint(Ref, Joint.Ref);
  Result := Joint;
end;

function TSpace.Add(Shape: TShape): TShape;
begin
  cpSpaceAddShape(Ref, Shape.Ref);
  Result := Shape;
end;

function TSpace.GetGround: TBody;
begin
  Result.Ref := cpSpaceGetStaticBody(Ref);
end;

procedure TSpace.Remove(Body: TBody);
var
  S: TShape;
begin
  for S in Body.Shapes do
    cpSpaceRemoveShape(Ref, S.Ref);
  cpSpaceRemoveBody(Ref, Body.Ref);
end;

procedure TSpace.Remove(Joint: TJoint);
begin
  cpSpaceRemoveConstraint(Ref, Joint.Ref);
end;

procedure TSpace.Remove(Shape: TShape);
begin
  cpSpaceRemoveShape(Ref, Shape.Ref);
end;

function TSpace.Contains(Body: TBody): Boolean;
begin
  Result := cpSpaceContainsBody(Ref, Body.Ref) <> cpFalse;
end;

function TSpace.Contains(Joint: TJoint): Boolean;
begin
  Result := cpSpaceContainsConstraint(Ref, Joint.Ref) <> cpFalse;
end;

function TSpace.Contains(Shape: TShape): Boolean;
begin
  Result := cpSpaceContainsShape(Ref, Shape.Ref) <> cpFalse;
end;

function TSpace.GetIterations: LongWord;
begin
  Result := cpSpaceGetIterations(Ref);
end;

procedure TSpace.SetIterations(Value: LongWord);
begin
  cpSpaceSetIterations(Ref, Value);
end;

function TSpace.GetDamping: Double;
begin
  Result := cpSpaceGetDamping(Ref);
end;

procedure TSpace.SetDamping(Value: Double);
begin
  cpSpaceSetDamping(Ref, Value);
end;

function TSpace.GetGravity: TPointD;
begin
  Result := cpSpaceGetGravity(Ref);
end;

procedure TSpace.SetGravity(Value: TPointD);
begin
  cpSpaceSetGravity(Ref, Value);
end;

function TSpace.GetIdleSpeedThreshold: Double;
begin
  Result := cpSpaceGetIdleSpeedThreshold(Ref);
end;

procedure TSpace.SetIdleSpeedThreshold(Value: Double);
begin
  cpSpaceSetIdleSpeedThreshold(Ref, Value);
end;

function TSpace.GetSleepTimeThreshold: Double;
begin
  Result := cpSpaceGetSleepTimeThreshold(Ref);
end;

procedure TSpace.SetSleepTimeThreshold(Value: Double);
begin
  cpSpaceSetSleepTimeThreshold(Ref, Value);
end;

function TSpace.GetCollisionSlop: Double;
begin
  Result := cpSpaceGetCollisionSlop(Ref);
end;

procedure TSpace.SetCollisionSlop(Value: Double);
begin
  cpSpaceSetCollisionSlop(Ref, Value);
end;

function TSpace.GetCollisionBias: Double;
begin
  Result := cpSpaceGetCollisionBias(Ref);
end;

procedure TSpace.SetCollisionBias(Value: Double);
begin
  cpSpaceSetCollisionBias(Ref, Value);
end;

function TSpace.GetCollisionPersistence: LongWord;
begin
  Result := cpSpaceGetCollisionPersistence(Ref);
end;

procedure TSpace.SetCollisionPersistence(Value: LongWord);
begin
  cpSpaceSetCollisionPersistence(Ref, Value);
end;

function TSpace.GetUserData: Pointer;
begin
  Result := cpSpaceGetUserData(Ref);
end;

procedure TSpace.SetUserData(Value: Pointer);
begin
  cpSpaceSetUserData(Ref, Value);
end;

function TSpace.GetBodies: TSpaceBodyEnumerator;
begin
  Result := TSpaceBodyEnumerator.Create(Ref);
end;

function TSpace.GetJoints: TSpaceJointEnumerator;
begin
  Result := TSpaceJointEnumerator.Create(Self);
end;

{ TCircleHelper }

function TCircleHelper.Base: TShape;
begin
  Result.Ref := Pointer(Ref);
end;

{ TPolygonHelper }

function TPolygonHelper.Base: TShape;
begin
  Result.Ref := Pointer(Ref);
end;

{ TSegmentHelper }

function TSegmentHelper.Base: TShape;
begin
  Result.Ref := Pointer(Ref);
end;

{ TShapeHelper }

function TShapeHelper.Body: TBody;
begin
  Result.Ref := cpShapeGetBody(Ref);
end;

function TShapeHelper.Space: TSpace;
begin
  Result.Ref := cpShapeGetSpace(Ref);
end;

function NewSpace: TSpace;
begin
  Result.Ref := cpSpaceNew;
end;

{ TPinJointHelper }

function TPinJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TSlideJointHelper }

function TSlideJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TPivotJointHelper }

function TPivotJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TGrooveJointHelper }

function TGrooveJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TDampedSpringJointHelper }

function TDampedSpringJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TDampedRotarySpringJointHelper }

function TDampedRotarySpringJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TRotaryLimitJointHelper }

function TRotaryLimitJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TRatchetJointHelper }

function TRatchetJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TGearJointHelper }

function TGearJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TMotorJointHelper }

function TMotorJointHelper.Base: TJoint;
begin
  Result.Ref := Ref;
end;

{ TJointHelper }

function TJointHelper.Space: TSpace;
begin
  Result.Ref := cpConstraintGetSpace(Ref);
end;

{ TBodyHelper }

function TBodyHelper.Space: TSpace;
begin
  Result.Ref := cpBodyGetSpace(Ref);
end;


  {
  Simulate or countact gravity

  M := MatrixIdentity;
  M.Rotate(-cpBodyGetAngle(FBallA));
  A := Vect(0, 100000);
  B := M * A;
  cpBodyApplyForceAtLocalPoint(FBallA, B, Vect(0, 0));

  M := MatrixIdentity;
  M.Rotate(-cpBodyGetAngle(FBallB));
  A := Vect(0, 2000000);
  B := M * A;
  cpBodyApplyForceAtLocalPoint(FBallB, B, Vect(0, 0));}

end.

