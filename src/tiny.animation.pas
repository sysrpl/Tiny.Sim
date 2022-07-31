unit Tiny.Animation;

{$i tiny.inc}

interface

{ How to use animations

  1) Declare a TVecNProp on the object you want to animate
  Example: property Color: TVec4Prop read FColor;

  2) Link the backing field as a real property when your object is created
  Example: FColor.Link;

  3) During a logic phase animate the property
  Example: Animations.Add(Color.Red, 0.5).Duration(0.25).Easing('bouncy');

  4) During logic or render phase update the animation time
  Example: Animations.Animate(Stopwatch.Time); }

{ TODO: Consider removing animation nodes and using a dictionary keyed on TVec1Prop }

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Geometry;

{ TEasing is the function prototype for change over time [group animation]
  See also
  <link Tiny.Animation.Easings, Easings function>
  <link Tiny.Animation.TEasingDefaults, TEasingDefaults class>
  <exref target="http://easings.net/">External: Easing functions on easings.net</exref> }

type
  TEasing = function(Percent: Float): Float;

{ TEasingDefaults provides some default easing functions which conform to
  the <link Tiny.Animation.TEasing, TEasing prototype> [group animation]
  See also
  <link Overview.Tiny.Animation.TEasingDefaults, TEasingDefaults members>
  <link Tiny.Animation.Easings, Easings function>
  <exref target="http://easings.net/">External: Easing functions on easings.net</exref> }

  TEasingDefaults = record
  public
    { The default easing function }
    class function Linear(Percent: Float): Float; static;
    { An easing function }
    class function Easy(Percent: Float): Float; static;
    { An easing function }
    class function EasySlow(Percent: Float): Float; static;
    { An easing function }
    class function Extend(Percent: Float): Float; static;
    { An easing function }
    class function Drop(Percent: Float): Float; static;
    { An easing function }
    class function DropSlow(Percent: Float): Float; static;
    { An easing function }
    class function Snap(Percent: Float): Float; static;
    { An easing function }
    class function Bounce(Percent: Float): Float; static;
    { An easing function }
    class function Bouncy(Percent: Float): Float; static;
    { An easing function }
    class function Rubber(Percent: Float): Float; static;
    { An easing function }
    class function Spring(Percent: Float): Float; static;
    { An easing function }
    class function Boing(Percent: Float): Float; static;
  end;

{ TEasings is a dictionary that stores easings by name [group animation]
  See also
  <link Tiny.Animation.Easings, Easings function> }

  TEasings = class(TDictionary<string, TEasing>)
  protected
    {doc off}
    function DefaultValue: TEasing; override;
  public
    procedure RegisterDefaults;
    {doc on}
  end;

{ Calculates the percent change of an easing, optionally reversing the curve [group animation] }
function Interpolate(Easing: TEasing; Percent: Float; Reverse: Boolean = False): Float; overload;
{ Calculates the effect of an easing on values, optionally reversing the curve [group animation] }
function Interpolate(Easing: TEasing; Percent: Float; Start, Finish: Float; Reverse: Boolean = False): Float; overload;

{ IDependencyProperty allows vector properties to be dettached from their owning
  objects [group animation]
  <link Overview.Tiny.Animation.IDependencyProperty, IDependencyProperty members> }

type
  IDependencyProperty = interface
  ['{E021AD95-9985-48AB-B29F-8D25A7BBE10E}']
    {doc ignore}
    function GetCount: Integer;
    { Get a component value }
    function GetValue(Index: Integer): Float;
    { Set a component value }
    procedure SetValue(Value: Float; Index: Integer);
    { Returns the number of component values }
    property Count: Integer read GetCount;
  end;

{ TDependencyChangeNotify allows objects which own dependency properties to be
  notified when the property values change [group animation] }

  TDependencyChangeNotify = procedure(Prop: IDependencyProperty; Index: Integer) of object;

{ TVec1Prop is a 1 component dependency property [group animation]
  <link Overview.Tiny.Animation.TVec1Prop, TVec1Prop members> }

  TVec1Prop = record
  private
    {doc off}
    function GetValue: TVec1;
    procedure SetValue(Value: TVec1);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
  public
    class operator Implicit(const Value: TVec1): TVec1Prop;
    class operator Implicit(const Value: TVec1Prop): TVec1;
    class operator Negative(const A: TVec1Prop): TVec1;
    class operator Positive(const A: TVec1Prop): TVec1;
    class operator Equal(const A, B: TVec1Prop) : Boolean;
    class operator NotEqual(const A, B: TVec1Prop): Boolean;
    class operator GreaterThan(const A, B: TVec1Prop): Boolean;
    class operator GreaterThanOrEqual(const A, B: TVec1Prop): Boolean;
    class operator LessThan(const A, B: TVec1Prop): Boolean;
    class operator LessThanOrEqual(const A, B: TVec1Prop): Boolean;
    class operator Add(const A, B: TVec1Prop): TVec1;
    class operator Subtract(const A, B: TVec1Prop): TVec1;
    class operator Multiply(const A, B: TVec1Prop): TVec1;
    class operator Divide(const A, B: TVec1Prop): TVec1;
    procedure Link(OnChange: TDependencyChangeNotify = nil); overload;
    procedure Link(Prop: IDependencyProperty; Index: LongInt); overload;
    procedure Unlink;
    function Linked: Boolean;
    function Equals(const A: TVec1Prop): Boolean;
    function Same(const A: TVec1Prop): Boolean;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Value: TVec1 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec1);
    {doc on}
  end;

{ TVec2Prop is a 2 component dependency property [group animation]
  <link Overview.Tiny.Animation.TVec2Prop, TVec2Prop members> }

  TVec2Prop = record
  private
    {doc off}
    function GetValue: TVec2;
    procedure SetValue(const Value: TVec2);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
  public
    class operator Implicit(const Value: TVec2Prop): TVec2;
    class operator Implicit(const Value: TVec2): TVec2Prop;
    class operator Implicit(const Value: TPointF): TVec2Prop;
    class operator Implicit(const Value: TVec2Prop): TPointF;
    class operator Negative(const A: TVec2Prop): TVec2;
    class operator Add(const A, B: TVec2Prop): TVec2;
    class operator Subtract(const A, B: TVec2Prop): TVec2;
    class operator Multiply(const A: TVec2Prop; B: Float): TVec2;
    class operator Divide(const A: TVec2Prop; B: Float): TVec2;
    procedure Link(OnChange: TDependencyChangeNotify = nil); overload;
    procedure Link(Prop: IDependencyProperty; Index: LongInt); overload;
    procedure Unlink;
    function Linked: Boolean;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Y: TVec1Prop index 1 read GetVec write SetVec;
    property AsVec1: TVec1Prop index 0 read GetVec write SetVec;
    property Value: TVec2 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
  private
    FProp: IDependencyProperty;
    case Boolean of
      True: (FIndex: LongInt);
      False: (FValue: TVec2);
    {doc on}
  end;

{ TVec3Prop is a 3 component dependency property
  <link Overview.Tiny.Animation.TVec3Prop, TVec3Prop members> }

  TVec3Prop = record
  private
    {doc off}
    function GetValue: TVec3;
    procedure SetValue(const Value: TVec3);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
    function GetAsVec2: TVec2Prop;
    procedure SetAsVec2(const Value: TVec2Prop);
  public
    class operator Implicit(const Value: TVec3Prop): TVec3;
    class operator Implicit(const Value: TVec3): TVec3Prop;
    class operator Negative(const A: TVec3Prop): TVec3;
    class operator Add(const A, B: TVec3Prop): TVec3;
    class operator Subtract(const A, B: TVec3Prop): TVec3;
    class operator Multiply(const A: TVec3Prop; B: Float): TVec3;
    class operator Divide(const A: TVec3Prop; B: Float): TVec3;
    procedure Link(OnChange: TDependencyChangeNotify = nil); overload;
    procedure Link(Prop: IDependencyProperty; Index: LongInt); overload;
    procedure Unlink;
    function Linked: Boolean;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Y: TVec1Prop index 1 read GetVec write SetVec;
    property Z: TVec1Prop index 2 read GetVec write SetVec;
    property Pitch: TVec1Prop index 0 read GetVec write SetVec;
    property Heading: TVec1Prop index 1 read GetVec write SetVec;
    property Roll: TVec1Prop index 2 read GetVec write SetVec;
    property XY: TVec2Prop read GetAsVec2 write SetAsVec2;
    property AsVec1: TVec1Prop index 0 read GetVec write SetVec;
    property AsVec2: TVec2Prop read GetAsVec2 write SetAsVec2;
    property Value: TVec3 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
  private
    FProp: IDependencyProperty;
    case Boolean of
   True: (FIndex: LongInt);
   False: (FValue: TVec3);
    {doc on}
  end;

{ TVec4Prop is a 4 component dependency property [group animation]
  <link Overview.Tiny.Animation.TVec4Prop, TVec4Prop members> }

  TVec4Prop = record
  private
    {doc off}
    function GetValue: TVec4;
    procedure SetValue(const Value: TVec4);
    function GetVec(Index: Integer): TVec1Prop;
    procedure SetVec(Index: Integer; const Value: TVec1Prop);
    function GetAsVec2: TVec2Prop;
    procedure SetAsVec2(const Value: TVec2Prop);
    function GetAsVec3: TVec3Prop;
    procedure SetAsVec3(const Value: TVec3Prop);
  public
    class operator Implicit(const Value: TVec4): TVec4Prop;
    class operator Implicit(const Value: TVec4Prop): TVec4;
    procedure Link(OnChange: TDependencyChangeNotify = nil); overload;
    procedure Link(Prop: IDependencyProperty; Index: LongInt); overload;
    procedure Unlink;
    function Linked: Boolean;
    property X: TVec1Prop index 0 read GetVec write SetVec;
    property Y: TVec1Prop index 1 read GetVec write SetVec;
    property Z: TVec1Prop index 2 read GetVec write SetVec;
    property W: TVec1Prop index 3 read GetVec write SetVec;
    property Red: TVec1Prop index 0 read GetVec write SetVec;
    property Green: TVec1Prop index 1 read GetVec write SetVec;
    property Blue: TVec1Prop index 2 read GetVec write SetVec;
    property Alpha: TVec1Prop index 3 read GetVec write SetVec;
    property S0: TVec1Prop index 0 read GetVec write SetVec;
    property T0: TVec1Prop index 1 read GetVec write SetVec;
    property S1: TVec1Prop index 2 read GetVec write SetVec;
    property T1: TVec1Prop index 3 read GetVec write SetVec;
    property XY: TVec2Prop read GetAsVec2 write SetAsVec2;
    property XYZ: TVec3Prop read GetAsVec3 write SetAsVec3;
    property RGB: TVec3Prop read GetAsVec3 write SetAsVec3;
    property AsVec1: TVec1Prop index 0 read GetVec write SetVec;
    property AsVec2: TVec2Prop read GetAsVec2 write SetAsVec2;
    property AsVec3: TVec3Prop read GetAsVec3 write SetAsVec3;
    property Value: TVec4 read GetValue write SetValue;
    property Vec[Index: Integer]: TVec1Prop read GetVec write SetVec;
  private
    FProp: IDependencyProperty;
    case Boolean of
   True: (FIndex: LongInt);
   False: (FValue: TVec4);
    {doc on}
  end;

{ Link a dependency property [group animation] }
procedure DependencyLink(var Prop: IDependencyProperty; Count: Integer; OnChange: TDependencyChangeNotify);
{ Unlink a dependency property  [group animation] }
procedure DependencyUnlink(var Prop: IDependencyProperty);

type
  {doc ignore}
  IPropertyResolver = interface;

{ TVectorProperty is the result of resolved vector properties
  <link Overview.Tiny.Animation.TVectorProperty, TVectorProperty members> }

  TVectorProperty = record
    Vec1Prop: TVec1Prop;
    Vec2Prop: TVec2Prop;
    Vec3Prop: TVec3Prop;
    Vec4Prop: TVec4Prop;
    Resolver: IPropertyResolver;
  end;

{ IPropertyResolver is used to convert a name to a vector property
  <link Overview.Tiny.Animation.IPropertyResolver, IPropertyResolver members> }

  IPropertyResolver = interface
  ['{1638C795-D894-4B7F-9491-47F57A88F622}']
    { Ask the object to resolve a name  }
    function Resolve(const Name: string; out Prop: TVectorProperty): Boolean;
  end;

{ Return false while clearing a vector property }

function VectorPropertyEmpty(out Prop: TVectorProperty): Boolean;

const
  { Start the animation after the last property animation completes [group animation] }
  startAppend = -1;
  { Start the animation immediately erasing any previous property animations [group animation] }
  startOverwrite = 0;
  { Loop is infinite and the animation runs continuously  [group animation] }
  loopInfinite = -1;

{ TAnimationLoop specifies the behaviour of an animation when complete [group animation]
  See also
  <link Tiny.Animation.TAnimationArgs.Loop, TAnimationArgs.Loop method> }

type
  TAnimationLoop = (
    { When the time expires the animation is removed }
    loopNone,
    { When the time expires repeat the animation from the start for count times
   before the animation is removed}
    loopRepeat,
    { When the time expires reverse the animation for count times before the
   animation is removed }
    loopReverse);

  {doc off}
  TAnimationContainer = class;
  TStoryboard = class;
  TAnimations = class;
  TAnimationArgs = class;

  TAnimation = record
  public
    Initialized: Boolean;
    Prop: TVec1Prop;
    Easing: TEasing;
    Reverse: Boolean;
    StartTime: Double;
    StartValue: Float;
    FinishTime: Double;
    FinishValue: Float;
    LoopBehaviour: TAnimationLoop;
    LoopCount: Integer;
    Storyboard: TStoryboard;
    OnComplete: TEmptyEvent;
  end;

  TAnimationNode = class(TObject)
  public
    Animation: TAnimation;
    Prior: TAnimationNode;
    Next: TAnimationNode;
    constructor Create(var A: TAnimation);
    destructor Destroy; override;
  end;

  TAnimationNodes = TObjectList<TAnimationNode>;

  TAnimationProperties = class(TIndexedList<TVec1Prop>)
  public
    constructor Create;
    function IndexOf(const Item: ItemType): Integer; override;
  end;
  {doc on}

{ TAnimationArgs is used to specify animation options [group animation]
  See also
  <link Overview.Tiny.Animation.TAnimationArgs, TAnimationArgs members> }

  TAnimationArgs = class
  private
    FContainer: TAnimationContainer;
    FTime: Double;
    FAnimation: TAnimation;
    FPropPath: string;
    FPropResolver: IPropertyResolver;
    FProp: IDependencyProperty;
    FIndex: Integer;
    FCount: Integer;
    FTarget: TVec4;
    procedure Apply;
    procedure Reset(const PropPath: string; Target: TVec4); overload;
    procedure Reset(Prop: IDependencyProperty; Index: Integer; Count: Integer; Target: TVec4); overload;
    procedure Resolve;
  protected
    {doc ignore}
    function Clone: TAnimationArgs;
  public
    {doc ignore}
    constructor Create(Container: TAnimationContainer);
    { Specify the animation easing by function }
    function Easing(Func: TEasing): TAnimationArgs; overload;
    { Specify the animation easing by name }
    function Easing(const Name: string): TAnimationArgs; overload;
    { Reverse the contour of the easing function }
    function Reverse: TAnimationArgs;
    { Start the animation after the last property animation completes }
    function Append: TAnimationArgs;
    { Start the animation immediately erasing any previous property animations }
    function Overwrite: TAnimationArgs;
    { Start after a specified number of seconds }
    function Start(const Seconds: Double): TAnimationArgs;
    { Start the the after a specified number of seconds }
    function Duration(const Seconds: Double): TAnimationArgs;
    { Loop the animation using a behaviour continuing count times
   See also
   <link Tiny.Animation.loopInfinite, loopInfinite constant> }
    function Loop(Behaviour: TAnimationLoop; Count: Integer): TAnimationArgs;
    { Notification that the property animation has finished }
    function OnComplete(Handler: TEmptyEvent): TAnimationArgs;
  end;

  {doc ignore}
  TAnimationArgsList = TObjectList<TAnimationArgs>;

{ TAnimationContainer stores a collection of animations [group animation]
  See also
  <link Overview.Tiny.Animation.TAnimationContainer, TAnimationContainer members> }

  TAnimationContainer = class
  private
    FArgs: TAnimationArgs;
  protected
    {doc ignore}
    function AllowApply: Boolean; virtual;
  public
    {doc ignore}
    constructor Create;
    destructor  Destroy; override;
    { Store a vec1 property animation returning customization arguments }
    function Add(const Prop: TVec1Prop; const Target: TVec1): TAnimationArgs; overload;
    { Store a vec2 property animation returning customization arguments }
    function Add(const Prop: TVec2Prop; const Target: TVec2): TAnimationArgs; overload;
    { Store a vec3 property animation returning customization arguments }
    function Add(const Prop: TVec3Prop; const Target: TVec3): TAnimationArgs; overload;
    { Store a vec4 property animation returning customization arguments }
    function Add(const Prop: TVec4Prop; const Target: TVec4): TAnimationArgs; overload;
  end;

{ TStoryboard stores a reusable list of property animations
  Remarks
  Looping is the domain of animation args and not an entire storyboard
  See also
  <link Overview.Tiny.Animation.TStoryboard, TStoryboard members> }

  TStoryboard = class(TAnimationContainer)
  private
    FArgsList: TAnimationArgsList;
    FCount: Integer;
    FOnComplete: TEmptyEvent;
    procedure Remove;
    function GetPlaying: Boolean;
  protected
    {doc off}
    function AllowApply: Boolean; override;
    {doc on}
  public
    { Create a new storyboard }
    constructor Create;
    destructor Destroy; override;
    { Store a vec1 property path animation returning customization arguments }
    function Add(const PropPath: string; const Target: TVec1): TAnimationArgs; overload;
    { Store a vec2 property path animation returning customization arguments }
    function Add(const PropPath: string; const Target: TVec2): TAnimationArgs; overload;
    { Store a vec3 property path animation returning customization arguments }
    function Add(const PropPath: string; const Target: TVec3): TAnimationArgs; overload;
    { Store a vec4 property path property animation returning customization arguments }
    function Add(const PropPath: string; const Target: TVec4): TAnimationArgs; overload;
    { Play the storyboard }
    procedure Play;
    { Stop the storyboard }
    procedure Stop;
    { Stop and clear the contents of the storyboard }
    procedure Reset;
    { Returns true if the storyboard is playing }
    property Playing: Boolean read GetPlaying;
    { Run the storyboard using a property resolver
   Remarks
   Run differs from play in that a running animation cannot be stopped and
   does not invoke completion events }
    procedure Run(PropResolver: IPropertyResolver);
    { Notification that the storyboard finished playing }
    property OnComplete: TEmptyEvent read FOnComplete write FOnComplete;
  end;

  {doc ignore}
  TStoryboards = TObjectList<TStoryboard>;

{ TAnimations updates vector properties based on time [group animation]
  See also
  <link Overview.Tiny.Animation.TAnimations, TAnimations members>
  <link Overview.Tiny.Animation.TAnimations.Time, Time property> }

  TAnimations = class(TAnimationContainer)
  private
    FRoot: TAnimationNode;
    FGroup: TAnimationNodes;
    FProperties: TAnimationProperties;
    FTime: Double;
    function AddAnimation(var Animation: TAnimation; const Time: Double): TAnimationNode;
    procedure FreeNode(Node: TAnimationNode);
    procedure GroupProp(const Prop: TVec1Prop);
    procedure RemoveProp(const Prop: TVec1Prop);
    procedure RemoveUnlinked;
  public
    { Do not use. Use <link Tiny.Animation.Animations, Animations function> instead. }
    constructor Create;
    destructor Destroy; override;
    { Animate updates previously stored properties using time }
    procedure Animate(Time: Double);
    { Remove all animations linked to a vec1 property }
    procedure Remove(const Prop: TVec1Prop); overload;
    { Remove all animations linked to a vec2 property }
    procedure Remove(const Prop: TVec2Prop); overload;
    { Remove all animations linked to a vec3 property }
    procedure Remove(const Prop: TVec3Prop); overload;
    { Remove all animations linked to a vec4 property }
    procedure Remove(const Prop: TVec4Prop); overload;
    { Returns true if the vec1 property is stored as a running animation }
    function Playing(const Prop: TVec1Prop): Boolean; overload;
    { Returns true if the vec2 property is stored as a running animation }
    function Playing(const Prop: TVec2Prop): Boolean; overload;
    { Returns true if the vec3 property is stored as a running animation }
    function Playing(const Prop: TVec3Prop): Boolean; overload;
    { Returns true if the vec4 property is stored as a running animation }
    function Playing(const Prop: TVec4Prop): Boolean; overload;
    { The time property reflects the last animation time }
    property Time: Double read FTime;
  end;

{ Provides access to <link Tiny.Animation.TEasings, TEasings class> [group animation] }
function Easings: TEasings;
{ Provides access to <link Tiny.Animation.TAnimations, TAnimations class> [group animation] }
function Animations: TAnimations;

{ Causes a thread to wait for a ownership of the animation lock [group animation] }
procedure AnimationLock;
{ Releases ownership of the animation lock [group animation] }
procedure AnimationUnlock;

implementation

const
  NegCosPi = 1.61803398874989; { 2 / -Cos(Pi * 1.2) }

var
  EasingsInstance: TObject;
  AnimationsInstance: TObject;
  AnimationMutex: IMutex;

function Easings: TEasings;
begin
  if EasingsInstance = nil then
  begin
    AnimationLock;
    if EasingsInstance = nil then
    begin
   EasingsInstance := TEasings.Create;
   TEasings(EasingsInstance).RegisterDefaults;
    end;
    AnimationUnlock;
  end;
  Result := TEasings(EasingsInstance);
end;

function Animations: TAnimations;
begin
  if AnimationsInstance = nil then
  begin
    AnimationLock;
    if AnimationsInstance = nil then
   AnimationsInstance := TAnimations.Create;
    AnimationUnlock;
  end;
  Result := TAnimations(AnimationsInstance);
end;

procedure AnimationLock;
begin
  if AnimationMutex = nil then
  begin
    Lock;
    AnimationMutex := CreateMutex;
    Unlock;
  end;
  AnimationMutex.Lock;
end;

procedure AnimationUnlock;
begin
  AnimationMutex.Unlock;
end;

class function TEasingDefaults.Linear(Percent: Float): Float;
begin
  Result := Percent;
end;

class function TEasingDefaults.Easy(Percent: Float): Float;
begin
  Result := Percent * Percent * (3 - 2 * Percent);
end;

class function TEasingDefaults.EasySlow(Percent: Float): Float;
begin
  Percent := Easy(Percent);
  Result := Percent * Percent * (3 - 2 * Percent);
end;

class function TEasingDefaults.Extend(Percent: Float): Float;
begin
  Percent := (Percent * 1.4) - 0.2;
  Result := 0.5 - Cos(Pi * Percent) / NegCosPi;
end;

class function Power(const Base, Exponent: Float): Float;
begin
  if Exponent = 0 then
    Result := 1
  else if (Base = 0) and (Exponent > 0) then
    Result := 0
  else
    Result := Exp(Exponent * Ln(Base));
end;

class function TEasingDefaults.Drop(Percent: Float): Float;
begin
  Result := Percent * Percent;
end;

class function TEasingDefaults.DropSlow(Percent: Float): Float;
begin
  Result := Percent * Percent * Percent;
end;

class function TEasingDefaults.Snap(Percent: Float): Float;
begin
  Percent := Percent * Percent;
  Percent := (Percent * 1.4) - 0.2;
  Result := 0.5 - Cos(Pi * Percent) / NegCosPi;
end;

class function TEasingDefaults.Bounce(Percent: Float): Float;
begin
  if Percent > 0.9 then
  begin
    Result := Percent - 0.95;
    Result := 1 + Result * Result * 20 - (0.05 * 0.05 * 20);
  end
  else if Percent > 0.75 then
  begin
    Result := Percent - 0.825;
    Result := 1 + Result * Result * 16 - (0.075 * 0.075 * 16);
  end
  else if Percent > 0.5 then
  begin
    Result := Percent - 0.625;
    Result := 1 + Result * Result * 12 - (0.125 * 0.125 * 12);
  end
  else
  begin
    Percent := Percent * 2;
    Result := Percent * Percent;
  end;
end;

class function TEasingDefaults.Bouncy(Percent: Float): Float;
var
  Scale, Start, Step: Float;
begin
  Result := 1;
  Scale := 5;
  Start := 0.5;
  Step := 0.2;
  if Percent < Start then
  begin
    Result := Percent / Start;
    Result :=  Result * Result;
  end
  else
  while Step > 0.01 do
    if Percent < Start + Step then
    begin
   Step := Step / 2;
   Result := (Percent - (Start + Step)) * Scale;
   Result :=  Result * Result;
   Result := Result + 1 - Power(Step * Scale, 2);
   Break;
    end
    else
    begin
   Start := Start + Step;
   Step := Step * 0.6;
    end;
end;

class function TEasingDefaults.Rubber(Percent: Float): Float;
begin
  if Percent > 0.9 then
  begin
    Result := Percent - 0.95;
    Result := 1 - Result * Result * 20 + (0.05 * 0.05 * 20);
  end
  else if Percent > 0.75 then
  begin
    Result := Percent - 0.825;
    Result := 1 + Result * Result * 18 - (0.075 * 0.075 * 18);
  end
  else if Percent > 0.5 then
  begin
    Result := Percent - 0.625;
    Result := 1 - Result * Result * 14 + (0.125 * 0.125 * 14);
  end
  else
  begin
    Percent := Percent * 2;
    Result := Percent * Percent;
  end;
end;

class function TEasingDefaults.Spring(Percent: Float): Float;
begin
  Percent := Percent * Percent;
  Result := Sin(PI * Percent * Percent * 10 - PI / 2) / 4;
  Result := Result * (1 - Percent) + 1;
  if Percent < 0.3 then
    Result := Result * Easy(Percent / 0.3);
end;

class function TEasingDefaults.Boing(Percent: Float): Float;
begin
  Percent := Power(Percent, 1.5);
  Result := Sin(PI * Power(Percent, 2) * 20 - PI / 2) / 4;
  Result := Result * (1 - Percent) + 1;
  if Percent < 0.2 then
    Result := Result * Easy(Percent / 0.2);
end;

function EasingKeyCompare(const A, B: string): IntPtr;
begin
  Result := StrCompare(A, B, True);
end;

function TEasings.DefaultValue: TEasing;
begin
  Result := @TEasingDefaults.Linear;
end;

procedure TEasings.RegisterDefaults;
begin
  Comparer := EasingKeyCompare;
  Self['Linear'] := @TEasingDefaults.Linear;
  Self['Easy'] := @TEasingDefaults.Easy;
  Self['EasySlow'] := @TEasingDefaults.EasySlow;
  Self['Extend'] := @TEasingDefaults.Extend;
  Self['Drop'] := @TEasingDefaults.Drop;
  Self['DropSlow'] := @TEasingDefaults.DropSlow;
  Self['Snap'] := @TEasingDefaults.Snap;
  Self['Bounce'] := @TEasingDefaults.Bounce;
  Self['Bouncy'] := @TEasingDefaults.Bouncy;
  Self['Rubber'] := @TEasingDefaults.Rubber;
  Self['Spring'] := @TEasingDefaults.Spring;
  Self['Boing'] := @TEasingDefaults.Boing;
end;

function Interpolate(Easing: TEasing; Percent: Float; Reverse: Boolean = False): Float;
begin
  if Percent < 0 then
    Result := 0
  else if Percent > 1 then
    Result := 1
  else if Reverse then
    Result := 1 - Easing(1 - Percent)
  else
    Result := Easing(Percent);
end;

function Interpolate(Easing: TEasing; Percent: Float; Start, Finish: Float; Reverse: Boolean = False): Float;
begin
  if Percent < 0 then
    Result := Start
  else if Percent > 1 then
    Result := Finish
  else
  begin
    if Reverse then
   Percent := 1 - Easing(1 - Percent)
    else
   Percent := Easing(Percent);
    Result := Start * (1 - Percent) + Finish * Percent;
  end;
end;

{ TVec1Prop }

class operator TVec1Prop.Implicit(const Value: TVec1Prop): TVec1;
begin
  Result := Value.Value;
end;

class operator TVec1Prop.Implicit(const Value: TVec1): TVec1Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec1Prop.Negative(const A: TVec1Prop): TVec1;
begin
  Result := -A.Value;
end;

class operator TVec1Prop.Positive(const A: TVec1Prop): TVec1;
begin
  Result := A.Value;
end;

class operator TVec1Prop.Equal(const A, B: TVec1Prop) : Boolean;
begin
  Result := A.Value = B.Value;
end;

class operator TVec1Prop.NotEqual(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value <> B.Value;
end;

class operator TVec1Prop.GreaterThan(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value > B.Value;
end;

class operator TVec1Prop.GreaterThanOrEqual(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value >= B.Value;
end;

class operator TVec1Prop.LessThan(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value < B.Value;
end;

class operator TVec1Prop.LessThanOrEqual(const A, B: TVec1Prop): Boolean;
begin
  Result := A.Value <= B.Value;
end;

class operator TVec1Prop.Add(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value + B.Value;
end;

class operator TVec1Prop.Subtract(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value - B.Value;
end;

class operator TVec1Prop.Multiply(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value * B.Value;
end;

class operator TVec1Prop.Divide(const A, B: TVec1Prop): TVec1;
begin
  Result := A.Value / B.Value;
end;

procedure TVec1Prop.Link(OnChange: TDependencyChangeNotify = nil);
begin
  DependencyLink(FProp, 1, OnChange);
  FIndex := 0;
end;

procedure TVec1Prop.Link(Prop: IDependencyProperty; Index: LongInt);
begin
  FProp := Prop;
  FIndex := Index;
end;

procedure TVec1Prop.Unlink;
begin
  FProp := nil;
end;

function TVec1Prop.Linked: Boolean;
var
  B: Boolean;
begin
  B := FProp <> nil;
  Result := B;
end;

function TVec1Prop.Equals(const A: TVec1Prop): Boolean;
begin
  Result := Value = A.Value;
end;

function TVec1Prop.Same(const A: TVec1Prop): Boolean;
begin
  if FProp = nil then
    Result := False
  else if FProp = A.FProp then
    Result := FIndex = A.FIndex
  else
    Result := False;
end;

function TVec1Prop.GetValue: TVec1;
begin
  if FProp = nil then
    Result := FValue
  else
    Result := FProp.GetValue(FIndex);
end;

procedure TVec1Prop.SetValue(Value: TVec1);
begin
  if FProp = nil then
    FValue := Value
  else
    FProp.SetValue(Value, FIndex);
end;

function TVec1Prop.GetVec(Index: Integer): TVec1Prop;
begin
  Exit(Self);
end;

procedure TVec1Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if not Same(Value) then
    SetValue(Value.Value);
end;

{ TVec2Prop }

class operator TVec2Prop.Implicit(const Value: TVec2Prop): TVec2;
begin
  Result := Value.Value;
end;

class operator TVec2Prop.Implicit(const Value: TVec2): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec2Prop.Implicit(const Value: TPointF): TVec2Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue.X := Value.X;
  Result.FValue.Y := Value.Y;
end;

class operator TVec2Prop.Implicit(const Value: TVec2Prop): TPointF;
var
  V: TVec2;
begin
  V := Value.Value;
  Result.X := V.X;
  Result.Y := V.Y;
end;

class operator TVec2Prop.Negative(const A: TVec2Prop): TVec2;
begin
  Result := -A.Value;
end;

class operator TVec2Prop.Add(const A, B: TVec2Prop): TVec2;
begin
  Result := A.Value + B.Value;
end;

class operator TVec2Prop.Subtract(const A, B: TVec2Prop): TVec2;
begin
  Result := A.Value - B.Value;
end;

class operator TVec2Prop.Multiply(const A: TVec2Prop; B: Float): TVec2;
begin
  Result := A.Value * B;
end;

class operator TVec2Prop.Divide(const A: TVec2Prop; B: Float): TVec2;
begin
  Result := A.Value / B;
end;

procedure TVec2Prop.Link(OnChange: TDependencyChangeNotify = nil);
begin
  DependencyLink(FProp, 2, OnChange);
  FIndex := 0;
end;

procedure TVec2Prop.Link(Prop: IDependencyProperty; Index: LongInt);
begin
  FProp := Prop;
  FIndex := Index;
end;

procedure TVec2Prop.Unlink;
begin
  FProp := nil;
end;

function TVec2Prop.Linked: Boolean;
begin
  Result := FProp <> nil;
end;

function TVec2Prop.GetValue: TVec2;
begin
  if FProp = nil then
    Result := FValue
  else
  begin
    Result.X := FProp.GetValue(FIndex);
    Result.Y := FProp.GetValue(FIndex + 1);
  end;
end;

procedure TVec2Prop.SetValue(const Value: TVec2);
begin
  if FProp = nil then
    FValue := Value
  else
  begin
    FProp.SetValue(Value.X, FIndex);
    FProp.SetValue(Value.Y, FIndex + 1);
  end;
end;

function TVec2Prop.GetVec(Index: Integer): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  if FProp = nil then
  begin
    if Index < 1 then
   V.FValue := FValue.X
    else
   V.FValue := FValue.Y;
  end
  else
  begin
    V.FProp := FProp;
    if Index < 1 then
      V.FIndex := FIndex
    else
      V.FIndex := FIndex + 1;
  end;
  Exit(V);
end;

procedure TVec2Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if FProp = nil then
  begin
    FProp := nil;
    if Index < 1 then
      FValue.X := Value.Value
    else
      FValue.Y := Value.Value;
  end
  else
  begin
    if Index < 1 then
      FProp.SetValue(Value.Value, FIndex)
    else
      FProp.SetValue(Value.Value, FIndex + 1);
  end;
end;

{ TVec3Prop }

class operator TVec3Prop.Implicit(const Value: TVec3): TVec3Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec3Prop.Implicit(const Value: TVec3Prop): TVec3;
begin
  Result := Value.Value;
end;

class operator TVec3Prop.Negative(const A: TVec3Prop): TVec3;
begin
  Result := -A.Value;
end;

class operator TVec3Prop.Add(const A, B: TVec3Prop): TVec3;
begin
  Result := A.Value + B.Value;
end;

class operator TVec3Prop.Subtract(const A, B: TVec3Prop): TVec3;
begin
  Result := A.Value - B.Value;
end;

class operator TVec3Prop.Multiply(const A: TVec3Prop; B: Float): TVec3;
begin
  Result := A.Value * B;
end;

class operator TVec3Prop.Divide(const A: TVec3Prop; B: Float): TVec3;
begin
  Result := A.Value / B;
end;

procedure TVec3Prop.Link(OnChange: TDependencyChangeNotify = nil);
begin
  DependencyLink(FProp, 3, OnChange);
  FIndex := 0;
end;

procedure TVec3Prop.Link(Prop: IDependencyProperty; Index: LongInt);
begin
  FProp := Prop;
  FIndex := Index;
end;

procedure TVec3Prop.Unlink;
begin
  FProp := nil;
end;

function TVec3Prop.Linked: Boolean;
begin
  Result := FProp <> nil;
end;

function TVec3Prop.GetValue: TVec3;
begin
  if FProp = nil then
    Result := FValue
  else
  begin
    Result.X := FProp.GetValue(FIndex);
    Result.Y := FProp.GetValue(FIndex + 1);
    Result.Z := FProp.GetValue(FIndex + 2);
  end;
end;

procedure TVec3Prop.SetValue(const Value: TVec3);
begin
  if FProp = nil then
    FValue := Value
  else
  begin
    FProp.SetValue(Value.X, FIndex);
    FProp.SetValue(Value.Y, FIndex + 1);
    FProp.SetValue(Value.Z, FIndex + 2);
  end;
end;

function TVec3Prop.GetVec(Index: Integer): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  if FProp = nil then
  begin
    if Index < 1 then
   V.FValue := FValue.X
    else if Index < 2 then
   V.FValue := FValue.Y
    else
   V.FValue := FValue.Z;
  end
  else
  begin
    V.FProp := FProp;
    if Index < 1 then
   V.FIndex := FIndex
    else if Index < 2 then
   V.FIndex := FIndex + 1
    else
   V.FIndex := FIndex + 2;
  end;
  Exit(V);
end;

procedure TVec3Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if FProp = nil then
  begin
    FProp := nil;
    if Index < 1 then
   FValue.X := Value.Value
    else if Index < 2 then
   FValue.Y := Value.Value
    else
   FValue.Z := Value.Value;
  end
  else
  begin
    if Index < 1 then
   FProp.SetValue(Value.Value, FIndex)
    else if Index < 2 then
   FProp.SetValue(Value.Value, FIndex + 1)
    else
   FProp.SetValue(Value.Value, FIndex + 2);
  end;
end;

function TVec3Prop.GetAsVec2: TVec2Prop;
begin
  Result.Link(FProp, 0);
end;

procedure TVec3Prop.SetAsVec2(const Value: TVec2Prop);
var
  V: TVec2;
begin
  V := Value.Value;
  X := V.X;
  Y := V.Y;
end;

{ TVec4Prop }

class operator TVec4Prop.Implicit(const Value: TVec4): TVec4Prop;
begin
  UIntPtr(Result.FProp) := 0;
  Result.FValue := Value;
end;

class operator TVec4Prop.Implicit(const Value: TVec4Prop): TVec4;
begin
  Result := Value.Value;
end;

procedure TVec4Prop.Link(OnChange: TDependencyChangeNotify = nil);
begin
  DependencyLink(FProp, 4, OnChange);
  FIndex := 0;
end;

procedure TVec4Prop.Link(Prop: IDependencyProperty; Index: LongInt);
begin
  FProp := Prop;
  FIndex := Index;
end;

procedure TVec4Prop.Unlink;
begin
  FProp := nil;
end;

function TVec4Prop.Linked: Boolean;
begin
  Result := FProp <> nil;
end;

function TVec4Prop.GetValue: TVec4;
begin
  if FProp = nil then
    Result := FValue
  else
  begin
    Result.X := FProp.GetValue(FIndex);
    Result.Y := FProp.GetValue(FIndex + 1);
    Result.Z := FProp.GetValue(FIndex + 2);
    Result.W := FProp.GetValue(FIndex + 3);
  end;
end;

procedure TVec4Prop.SetValue(const Value: TVec4);
begin
  if FProp = nil then
    FValue := Value
  else
  begin
    FProp.SetValue(Value.X, FIndex);
    FProp.SetValue(Value.Y, FIndex + 1);
    FProp.SetValue(Value.Z, FIndex + 2);
    FProp.SetValue(Value.W, FIndex + 3);
  end;
end;

function TVec4Prop.GetVec(Index: Integer): TVec1Prop;
var
  V: TVec1Prop;
begin
  UIntPtr(V.FProp) := 0;
  if FProp = nil then
  begin
    if Index < 1 then
   V.FValue := FValue.X
    else if Index < 2 then
   V.FValue := FValue.Y
    else if Index < 3 then
   V.FValue := FValue.Z
    else
   V.FValue := FValue.W;
  end
  else
  begin
    V.FProp := FProp;
    if Index < 1 then
   V.FIndex := FIndex
    else if Index < 2 then
   V.FIndex := FIndex + 1
    else if Index < 3 then
   V.FIndex := FIndex + 2
    else
   V.FIndex := FIndex + 3;
  end;
  Exit(V);
end;

procedure TVec4Prop.SetVec(Index: Integer; const Value: TVec1Prop);
begin
  if FProp = nil then
  begin
    FProp := nil;
    if Index < 1 then
   FValue.X := Value.Value
    else if Index < 2 then
   FValue.Y := Value.Value
    else if Index < 3 then
   FValue.Z := Value.Value
    else
   FValue.W := Value.Value;
  end
  else
  begin
    if Index < 1 then
   FProp.SetValue(Value.Value, FIndex)
    else if Index < 2 then
   FProp.SetValue(Value.Value, FIndex + 1)
    else if Index < 3 then
   FProp.SetValue(Value.Value, FIndex + 2)
    else
   FProp.SetValue(Value.Value, FIndex + 3);
  end;
end;

function TVec4Prop.GetAsVec2: TVec2Prop;
begin
  Result.Link(FProp, 0);
end;

procedure TVec4Prop.SetAsVec2(const Value: TVec2Prop);
var
  V: TVec2;
begin
  V := Value.Value;
  X := V.X;
  Y := V.Y;
end;

function TVec4Prop.GetAsVec3: TVec3Prop;
begin
  Result.Link(FProp, 0);
end;

procedure TVec4Prop.SetAsVec3(const Value: TVec3Prop);
var
  V: TVec3;
begin
  V := Value.Value;
  X := V.X;
  Y := V.Y;
  Z := V.Z;
end;

{ TDependencyProperty }

type
  TPropertyValues = TArray<Float>;

  TDependencyProperty = class(TInterfacedObject, IDependencyProperty)
  private
    FValues: TPropertyValues;
    FOnChange: TDependencyChangeNotify;
  public
    function GetCount: Integer;
    function GetValue(Index: Integer): Float;
    procedure SetValue(Value: Float; Index: Integer);
  end;

function TDependencyProperty.GetCount: Integer;
begin
  Result := Length(FValues);
end;

function TDependencyProperty.GetValue(Index: Integer): Float;
begin
  Result := FValues[Index];
end;

procedure TDependencyProperty.SetValue(Value: Float; Index: Integer);
begin
  if FValues[Index] <> Value then
  begin
    FValues[Index] := Value;
    if Assigned(FOnChange) then
      FOnChange(Self, Index);
  end;
end;

procedure DependencyLink(var Prop: IDependencyProperty; Count: Integer; OnChange: TDependencyChangeNotify);
var
  Dependency: TDependencyProperty;
begin
  if Prop = nil then
    Dependency := TDependencyProperty.Create
  else
    Dependency := Prop as TDependencyProperty;
  SetLength(Dependency.FValues, Count);
  Dependency.FOnChange := OnChange;
  Prop := Dependency;
end;

procedure DependencyUnlink(var Prop: IDependencyProperty);
var
  Dependency: TDependencyProperty;
begin
  if Prop = nil then
    Exit;
  Dependency := Prop as TDependencyProperty;
  Dependency.FOnChange := nil;
  Prop := nil;
end;

function VectorPropertyEmpty(out Prop: TVectorProperty): Boolean;
begin
  Prop.Vec1Prop.Value := 0;
  Prop.Vec2Prop.Value := Vec2(0);
  Prop.Vec3Prop.Value := Vec3(0);
  Prop.Vec4Prop.Value := Vec4(0);
  Prop.Resolver := nil;
  Result := False;
end;

{ TAnimation }

function SortNodes(const A, B: TAnimationNode): IntPtr;
var
  D: Double;
begin
  if A = B then
    Exit(0);
  D := A.Animation.StartTime - B.Animation.StartTime;
  if D < 0 then
    Exit(-1);
  if D > 0 then
    Exit(1);
  D := A.Animation.FinishTime - B.Animation.FinishTime;
  if D < 0 then
    Exit(-1);
  if D > 0 then
    Exit(1);
  Result := IntPtr(A) - IntPtr(B);
end;

constructor TAnimationNode.Create(var A: TAnimation);
var
  S: TStoryboard;
begin
  inherited Create;
  Animation := A;
  S := Animation.Storyboard;
  if S <> nil then
    Inc(S.FCount);
end;

destructor TAnimationNode.Destroy;
var
  S: TStoryboard;
begin
  S := Animation.Storyboard;
  if S <> nil then
    Dec(S.FCount);
  inherited Destroy;
end;

{ TAnimationProperties }

function FindProp(const A, B: TVec1Prop): IntPtr;
begin
  if A.Same(B) then
    Result := 0
  else
    Result := -1;
end;

constructor TAnimationProperties.Create;
begin
  inherited Create;
  Duplicates := duplicateIgnore;
end;

function TAnimationProperties.IndexOf(const Item: ItemType): Integer;
begin
  if Item.Linked then
    Result := Find(FindProp, Item)
  else
    Result := -1;
end;

{ TAnimationArgs }

constructor TAnimationArgs.Create(Container: TAnimationContainer);
begin
  inherited Create;
  FContainer := Container;
end;

function TAnimationArgs.Clone: TAnimationArgs;
begin
  Result := TAnimationArgs.Create(FContainer);
  Result.FAnimation := FAnimation;
  Result.FPropPath := FPropPath;
  Result.FPropResolver := FPropResolver;
  Result.FTime := FTime;
  Result.FProp := FProp;
  Result.FIndex := FIndex;
  Result.FCount := FCount;
  Result.FTarget := FTarget;
end;

procedure TAnimationArgs.Apply;
const
  Sigma = 0.01;
var
  StartTime, FinishTime: Double;
  Nodes: array[0..3] of TAnimationNode;
  I: Integer;
begin
  if not FContainer.AllowApply then
    Exit;
  Resolve;
  if FProp = nil then
    Exit;
  AnimationLock;
  try
    if FAnimation.FinishTime < Sigma then
   FAnimation.FinishTime := Sigma;
    FAnimation.Prop.FProp := FProp;
    StartTime := 0;
    FinishTime := 0;
    for I := 0 to FCount - 1 do
    begin
   FAnimation.Prop.FIndex := FIndex + I;
   FAnimation.FinishValue := FTarget.V[I];
   Nodes[I] := Animations.AddAnimation(FAnimation, FTime);
   StartTime := Max(StartTime, Nodes[I].Animation.StartTime);
   FinishTime := Max(FinishTime, Nodes[I].Animation.FinishTime);
    end;
    { Fix it so that compound properties start and finish at the same time }
    for I := 0 to FCount - 1 do
    begin
   Nodes[I].Animation.StartTime := StartTime;
   Nodes[I].Animation.FinishTime := FinishTime;
    end;
    FProp := nil;
  finally
    AnimationUnlock;
  end;
end;

procedure TAnimationArgs.Reset(const PropPath: string; Target: TVec4);
begin
  Apply;
  FTime := Animations.FTime;
  FAnimation.Prop.Unlink;
  FillChar(FAnimation, SizeOf(FAnimation), 0);
  FAnimation.FinishTime := 1;
  FProp := nil;
  FIndex := 0;
  FCount := 0;
  FPropPath := PropPath;
  FPropResolver := nil;
  FTarget := Target;
end;

procedure TAnimationArgs.Reset(Prop: IDependencyProperty; Index: Integer; Count: Integer; Target: TVec4);
begin
  Apply;
  FTime := Animations.FTime;
  FAnimation.Prop.Unlink;
  FillChar(FAnimation, SizeOf(FAnimation), 0);
  FAnimation.FinishTime := 1;
  FPropPath := '';
  FPropResolver := nil;
  FProp := Prop;
  FIndex := Index;
  FCount := Count;
  FTarget := Target;
end;

procedure TAnimationArgs.Resolve;
var
  R: IPropertyResolver;
  P: TVectorProperty;
  S: string;
  A: TArray<string>;
  I, J: Integer;
begin
  R := FPropResolver;
  if R = nil then
    Exit;
  FProp := nil;
  S := StrReplace(FPropPath, '/', '.');
  A := StrSplit(S, '.');
  J := Length(A) - 1;
  for I := 0 to J do
  begin
    S := A[I];
    if not R.Resolve(S, P) then
   Exit;
    if P.Resolver <> nil then
    begin
   R := P.Resolver;
   Continue;
    end;
    if P.Vec1Prop.Linked then
    begin
   FProp := P.Vec1Prop.FProp;
   FIndex := P.Vec1Prop.FIndex;
   FCount := 1;
    end
    else if P.Vec2Prop.Linked then
    begin
   if I = J then
   begin
  FProp := P.Vec2Prop.FProp;
  FIndex := P.Vec2Prop.FIndex;
  FCount := 2;
   end
   else
   begin
  S := A[I + 1];
  if StrEquals(S,'X') then
  begin
    FProp := P.Vec2Prop.X.FProp;
    FIndex := P.Vec2Prop.X.FIndex;
    FCount := 1;
  end
  else if StrEquals(S, 'Y') then
  begin
    FProp := P.Vec2Prop.Y.FProp;
    FIndex := P.Vec2Prop.Y.FIndex;
    FCount := 1;
  end;
   end;
    end
    else if P.Vec3Prop.Linked then
    begin
   if I = J then
   begin
  FProp := P.Vec3Prop.FProp;
  FIndex := P.Vec3Prop.FIndex;
  FCount := 3;
   end
   else
   begin
  S := A[I + 1];
  if StrEquals(S, ['X', 'Pitch']) then
  begin
    FProp := P.Vec3Prop.X.FProp;
    FIndex := P.Vec3Prop.X.FIndex;
    FCount := 1;
  end
  else if StrEquals(S, ['Y', 'Heading']) then
  begin
    FProp := P.Vec3Prop.Y.FProp;
    FIndex := P.Vec3Prop.Y.FIndex;
    FCount := 1;
  end
  else if StrEquals(S, ['Z', 'Roll']) then
  begin
    FProp := P.Vec3Prop.Z.FProp;
    FIndex := P.Vec3Prop.Z.FIndex;
    FCount := 1;
  end
  else if StrEquals(S, 'XY') then
  begin
    FProp := P.Vec3Prop.XY.FProp;
    FIndex := P.Vec3Prop.XY.FIndex;
    FCount := 2;
  end;
   end;
    end
    else if P.Vec4Prop.Linked then
    begin
   if I = J then
   begin
  FProp := P.Vec4Prop.FProp;
  FIndex := P.Vec4Prop.FIndex;
  FCount := 4;
   end
   else
   begin
  S := A[I + 1];
  if StrEquals(S, ['X', 'Red', 'S0']) then
  begin
    FProp := P.Vec4Prop.X.FProp;
    FIndex := P.Vec4Prop.X.FIndex;
    FCount := 1;
  end
  else if StrEquals(S, ['Y', 'Green', 'T0']) then
  begin
    FProp := P.Vec4Prop.Y.FProp;
    FIndex := P.Vec4Prop.Y.FIndex;
    FCount := 1;
  end
  else if StrEquals(S, ['Z', 'Blue', 'S1']) then
  begin
    FProp := P.Vec4Prop.Z.FProp;
    FIndex := P.Vec4Prop.Z.FIndex;
    FCount := 1;
  end
  else if StrEquals(S, ['W', 'Alpha', 'T1']) then
  begin
    FProp := P.Vec4Prop.W.FProp;
    FIndex := P.Vec4Prop.W.FIndex;
    FCount := 1;
  end
  else if StrEquals(S, 'XY') then
  begin
    FProp := P.Vec4Prop.XY.FProp;
    FIndex := P.Vec4Prop.XY.FIndex;
    FCount := 2;
  end
  else if StrEquals(S, ['XYZ', 'RGB']) then
  begin
    FProp := P.Vec4Prop.XYZ.FProp;
    FIndex := P.Vec4Prop.XYZ.FIndex;
    FCount := 3;
  end;
   end;
    end;
    Exit;
  end;
end;

function TAnimationArgs.Easing(Func: TEasing): TAnimationArgs;
begin
  FAnimation.Easing := Func;
  Result := Self;
end;

function TAnimationArgs.Easing(const Name: string): TAnimationArgs;
begin
  FAnimation.Easing := Easings[Name];
  Result := Self;
end;

function TAnimationArgs.Reverse: TAnimationArgs;
begin
  FAnimation.Reverse := not FAnimation.Reverse;
  Result := Self;
end;

function TAnimationArgs.Append: TAnimationArgs;
begin
  FAnimation.StartTime := startAppend;
  Result := Self;
end;

function TAnimationArgs.Overwrite: TAnimationArgs;
begin
  FAnimation.StartTime := startOverwrite;
  Result := Self;
end;

function TAnimationArgs.Start(const Seconds: Double): TAnimationArgs;
begin
  FAnimation.StartTime := Seconds;
  Result := Self;
end;

function TAnimationArgs.Duration(const Seconds: Double): TAnimationArgs;
begin
  FAnimation.FinishTime := Seconds;
  Result := Self;
end;

function TAnimationArgs.Loop(Behaviour: TAnimationLoop; Count: Integer): TAnimationArgs;
begin
  FAnimation.LoopBehaviour := Behaviour;
  FAnimation.LoopCount := Count;
  Result := Self;
end;

function TAnimationArgs.OnComplete(Handler: TEmptyEvent): TAnimationArgs;
begin
  FAnimation.OnComplete := Handler;
  Result := Self;
end;

{ TAnimationContainer }

constructor TAnimationContainer.Create;
begin
  inherited Create;
  FArgs := TAnimationArgs.Create(Self);
end;

destructor TAnimationContainer.Destroy;
begin
  FArgs.Free;
  inherited Destroy;
end;

function TAnimationContainer.AllowApply: Boolean;
begin
  Result := True;
end;

function TAnimationContainer.Add(const Prop: TVec1Prop; const Target: TVec1): TAnimationArgs;
begin
  FArgs.Reset(Prop.FProp, Prop.FIndex, 1, Vec4(Target, 0, 0, 0));
  Result := FArgs;
end;

function TAnimationContainer.Add(const Prop: TVec2Prop; const Target: TVec2): TAnimationArgs;
begin
  FArgs.Reset(Prop.FProp, Prop.FIndex, 2, Vec4(Target.X, Target.Y, 0, 0));
  Result := FArgs;
end;

function TAnimationContainer.Add(const Prop: TVec3Prop; const Target: TVec3): TAnimationArgs;
begin
  FArgs.Reset(Prop.FProp, Prop.FIndex, 3, Vec4(Target.X, Target.Y, Target.Z, 0));
  Result := FArgs;
end;

function TAnimationContainer.Add(const Prop: TVec4Prop; const Target: TVec4): TAnimationArgs;
begin
  FArgs.Reset(Prop.FProp, Prop.FIndex, 4, Target);
  Result := FArgs;
end;

{ TStoryboard }

constructor TStoryboard.Create;
begin
  inherited Create;
  FArgsList := TAnimationArgsList.Create(True);
end;

destructor TStoryboard.Destroy;
begin
  Stop;
  FArgsList.Free;
  inherited Destroy;
end;

function TStoryboard.AllowApply: Boolean;
begin
  Result := False;
  FArgsList.Add(FArgs.Clone);
  { Remove the property from the argument }
  FArgs.FProp := nil;
  FArgs.FPropPath := '';
  FArgs.FPropResolver := nil;
end;

function TStoryboard.Add(const PropPath: string; const Target: TVec1): TAnimationArgs;
begin
  FArgs.Reset(PropPath, Vec4(Target, 0, 0, 0));
  Result := FArgs;
end;

function TStoryboard.Add(const PropPath: string; const Target: TVec2): TAnimationArgs;
begin
  FArgs.Reset(PropPath, Vec4(Target.X, Target.Y, 0, 0));
  Result := FArgs;
end;

function TStoryboard.Add(const PropPath: string; const Target: TVec3): TAnimationArgs;
begin
  FArgs.Reset(PropPath, Vec4(Target.X, Target.Y, Target.Z, 0));
  Result := FArgs;
end;

function TStoryboard.Add(const PropPath: string; const Target: TVec4): TAnimationArgs;
begin
  FArgs.Reset(PropPath, Target);
  Result := FArgs;
end;

procedure TStoryboard.Remove;
var
  Container: TAnimations;
  Properties: TAnimationProperties;
  Args: TAnimationArgs;
  Prop: TVec1Prop;
  I, J: Integer;
begin
  if FArgsList.Count = 0 then
    Exit;
  Container := Animations;
  { Remove all running properties referenced by the storyboard }
  Properties := TAnimationProperties.Create;
  try
    for I := 0 to FArgsList.Count - 1 do
    begin
   Args := FArgsList[I];
   Prop.FProp := Args.FProp;
   if Prop.FProp = nil then
  Continue;
   Prop.FIndex := Args.FIndex;
   for J := 0 to Args.FCount - 1 do
   begin
  Properties.Add(Prop);
  Inc(Prop.FIndex);
   end;
    end;
    for I := 0 to Properties.Count - 1 do
   Container.Remove(Properties[I]);
  finally
    Properties.Free;
  end;
end;

procedure TStoryboard.Play;
var
  Container: TAnimations;
  Args: TAnimationArgs;
  I: Integer;
begin
  if Playing then
    Exit;
  Remove;
  { Apply the last added animation }
  FArgs.Apply;
  { Play the animations }
  Container := Animations;
  for I := 0 to FArgsList.Count - 1 do
  begin
    Args := FArgsList[I].Clone;
    try
   Args.FContainer := Container;
   Args.FTime := Container.Time;
   Args.FAnimation.Storyboard := Self;
   Args.Apply;
    finally
   Args.Free;
    end;
  end;
end;

procedure TStoryboard.Stop;
begin
  if not Playing then
    Exit;
  Remove;
end;

procedure TStoryboard.Reset;
begin
  Stop;
  FArgsList.Clear;
  FCount := 0;
end;

procedure TStoryboard.Run(PropResolver: IPropertyResolver);
var
  Container: TAnimations;
  Args: TAnimationArgs;
  I: Integer;
begin
  FArgs.Apply;
  Container := Animations;
  for I := 0 to FArgsList.Count - 1 do
  begin
    Args := FArgsList[I].Clone;
    try
      Args.FContainer := Container;
      Args.FTime := Container.Time;
      Args.FProp := nil;
      Args.FPropResolver := PropResolver;
      Args.Apply;
    finally
      Args.Free;
    end;
  end;
end;

function TStoryboard.GetPlaying: Boolean;
begin
  Result := FCount > 0;
end;

{ TAnimations }

constructor TAnimations.Create;
var
  A: TAnimation;
begin
  inherited Create;
  MemZero(A, SizeOf(A));
  FRoot := TAnimationNode.Create(A);
  FRoot.Next := FRoot;
  FRoot.Prior := FRoot;
  FGroup := TAnimationNodes.Create(False);
  FProperties := TAnimationProperties.Create;
end;

destructor TAnimations.Destroy;
var
  A, B: TAnimationNode;
begin
  A := FRoot.Next;
  while A <> FRoot do
  begin
    B := A.Next;
    A.Free;
    A := B;
  end;
  FRoot.Free;
  FGroup.Free;
  FProperties.Free;
  inherited Destroy;
end;

procedure TAnimations.FreeNode(Node: TAnimationNode);
var
  A: TAnimationNode;
begin
  A := Node.Next;
  A.Prior := Node.Prior;
  A := Node.Prior;
  A.Next := Node.Next;
  Node.Free;
end;

procedure TAnimations.GroupProp(const Prop: TVec1Prop);
var
  A: TAnimationNode;
begin
  { Group animated properties }
  FGroup.Clear;
  A := FRoot.Next;
  { Find links }
  while A <> FRoot do
  begin
    if Prop.Same(A.Animation.Prop) then
   FGroup.Add(A);
    A := A.Next;
  end;
end;

procedure TAnimations.RemoveProp(const Prop: TVec1Prop);
var
  A: TAnimationNode;
begin
  { Properties without dependencies are ignored }
  if not Prop.Linked then
    Exit;
  GroupProp(Prop);
  { Remove links }
  for A in FGroup do
    FreeNode(A);
  FProperties.Remove(Prop);
end;

procedure TAnimations.RemoveUnlinked;
var
  A, B: TAnimationNode;
  I: Integer;
begin
  for I := FProperties.Count - 1 downto 0 do
    if not FProperties[I].Linked then
   FProperties.Delete(I);
  A := FRoot.Next;
  while A <> FRoot do
  begin
    B := A.Next;
    if not A.Animation.Prop.Linked then
   FreeNode(A);
    A := B;
  end;
end;

function TAnimations.AddAnimation(var Animation: TAnimation; const Time: Double): TAnimationNode;
var
  A: TAnimationNode;
begin
  A := TAnimationNode.Create(Animation);
  if not Assigned(A.Animation.Easing) then
    A.Animation.Easing := @TEasingDefaults.Linear;
  { If start is negative then append the animation }
  if A.Animation.StartTime < 0 then
  begin
    GroupProp(A.Animation.Prop);
    FGroup.Sort(SortNodes);
    if FGroup.First <> nil then
   A.Animation.StartTime := FGroup.Last.Animation.FinishTime - FGroup.First.Animation.StartTime
    else
   A.Animation.StartTime := 0;
  end
  { If the animation is set to start immediately then remove the group }
  else if A.Animation.StartTime = 0 then
    RemoveProp(A.Animation.Prop);
  FProperties.Add(A.Animation.Prop);
  { Convert start and duration from add methods to actual times }
  A.Animation.StartTime := Time + A.Animation.StartTime;
  A.Animation.FinishTime := A.Animation.StartTime + A.Animation.FinishTime;
  { Insert link }
  A.Prior := FRoot.Prior;
  A.Prior.Next := A;
  A.Next := FRoot;
  FRoot.Prior := A;
  Result := A;
end;

function TAnimations.Playing(const Prop: TVec1Prop): Boolean;
begin
  Result := FProperties.Contains(Prop);
end;

function TAnimations.Playing(const Prop: TVec2Prop): Boolean;
begin
  Result := FProperties.Contains(Prop.X) or FProperties.Contains(Prop.Y);
end;

function TAnimations.Playing(const Prop: TVec3Prop): Boolean;
begin
  Result := FProperties.Contains(Prop.X) or FProperties.Contains(Prop.Y) or
    FProperties.Contains(Prop.Z);
end;

function TAnimations.Playing(const Prop: TVec4Prop): Boolean;
begin
  Result := FProperties.Contains(Prop.X) or FProperties.Contains(Prop.Y) or
    FProperties.Contains(Prop.Z) or FProperties.Contains(Prop.W);
end;

procedure TAnimations.Animate(Time: Double);
var
  CompletedEvents: TArrayList<TEmptyEvent>;
  Storyboards: TStoryboards;
  S: TStoryboard;
  A: TAnimationNode;
  C: Integer;
  D: Double;
  F: Float;
  I, J: Integer;
begin
  FTime := Time;
  Storyboards := TStoryboards.Create(False);
  Storyboards.Duplicates := duplicateIgnore;
  AnimationLock;
  try
    { Apply any previous unapplied args }
    FArgs.Apply;
    { Remove any properties which were unlinked while still animating }
    RemoveUnlinked;
    { Count down so we can easily remove expired animations }
    for I := FProperties.Count - 1 downto 0 do
    begin
   GroupProp(FProperties[I]);
   FGroup.Sort(SortNodes);
   C := FGroup.Count;
   J := 0;
   while J < FGroup.Count do
   begin
  A := FGroup[J];
  S := A.Animation.Storyboard;
  if S <> nil then
    Storyboards.Add(S);
  if not A.Animation.Initialized then
    A.Animation.StartValue := A.Animation.Prop.Value;
  A.Animation.Initialized := True;
  { The animation has expired }
  if A.Animation.FinishTime <= FTime then
  begin
    if (A.Animation.LoopBehaviour <> loopNone) and (A.Animation.LoopCount <> 0) then
    begin
   if A.Animation.LoopCount > 0 then
  Dec(A.Animation.LoopCount);
   D := A.Animation.FinishTime - A.Animation.StartTime;
   A.Animation.StartTime := A.Animation.FinishTime;
   A.Animation.FinishTime := A.Animation.StartTime + D;
   if A.Animation.LoopBehaviour = loopReverse then
   begin
  A.Animation.Reverse := not A.Animation.Reverse;
  D := A.Animation.StartValue;
  A.Animation.StartValue := A.Animation.FinishValue;
  A.Animation.FinishValue := D;
   end;
   A.Animation.Prop.Value := A.Animation.StartValue;
   Continue;
    end
    else if A.Animation.LoopBehaviour = loopRepeat then
   A.Animation.Prop.Value := A.Animation.StartValue
    else
   A.Animation.Prop.Value := A.Animation.FinishValue;
    if Assigned(A.Animation.OnComplete) then
   CompletedEvents.Push(A.Animation.OnComplete);
    FreeNode(A);
    Dec(C);
    Inc(J);
    Continue;
  end;
  { The animation is ongoing }
  if A.Animation.StartTime <= FTime then
  begin
    D := A.Animation.FinishTime - A.Animation.StartTime;
    if D <= 0 then
    begin
   A.Animation.Prop.Value := A.Animation.FinishValue;
   FreeNode(A);
   Dec(C);
   Continue;
    end;
    D := (FTime - A.Animation.StartTime) / D;
    F := Interpolate(A.Animation.Easing, D,
   A.Animation.StartValue, A.Animation.FinishValue, A.Animation.Reverse);
    A.Animation.Prop.Value := F;
  end;
  Break;
   end;
   if C = 0 then
  FProperties.Delete(I);
    end;
  finally
    AnimationUnlock;
  end;
  { Notifications of completion }
  try
    for I := 0 to CompletedEvents.Length - 1 do
   CompletedEvents[I](Self, EmptyArgs);
    for I := 0 to Storyboards.Count - 1 do
    begin
   S := Storyboards[I];
   if (S.FCount = 0) and Assigned(S.FOnComplete) then
  S.FOnComplete(S, EmptyArgs);
    end;
  finally
    Storyboards.Free;
  end;
end;

procedure TAnimations.Remove(const Prop: TVec1Prop);
begin
  RemoveProp(Prop);
  FProperties.Remove(Prop);
end;

procedure TAnimations.Remove(const Prop: TVec2Prop);
begin
  Remove(Prop.X);
  Remove(Prop.Y);
end;

procedure TAnimations.Remove(const Prop: TVec3Prop);
begin
  Remove(Prop.X);
  Remove(Prop.Y);
  Remove(Prop.Z);
end;

procedure TAnimations.Remove(const Prop: TVec4Prop);
begin
  Remove(Prop.X);
  Remove(Prop.Y);
  Remove(Prop.Z);
  Remove(Prop.W);
end;

finalization
  EasingsInstance.Free;
  AnimationsInstance.Free;
  AnimationMutex := nil;
end.

