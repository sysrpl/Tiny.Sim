unit Tiny.Application;

{$i tiny.inc}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Graphics,
  Tiny.Widgets,
  Tiny.Widgets.Themes,
  Tiny.Interop.SDL2;

{ Studio size is useful when contending with unknown resolutions or render
  areas. By forcing a scene to work within a fixed studio size, the position
  and size of render elements can be treated as if you're using a fixed
  resolution.

  See also: Scene.ScaleToStudio }

const
  StudioWidth = 1920;
  StudioHeight = 1080;

{ The fixed time interval enforced between each scene logic frame }

  LogicStep = 1 / 120;

{ TScene represents a visual scene within an application. Place custom code
  in load when you scene starts, code in render to draw your scene, and code
  in ubload if you need to perform cleanup. Code may also be placed in the logic
  method to calculate state at fixed time intervals if required by your scene.

  To simply handling various resolutions or different sized windows, you can
  treat the scene as taking placed in a fixed size studio using the ScaleToStudio
  and other studio functions and constants. See the examples where studio size
  and scaling are frequently referenced. }

type
  TScene = class
  private
    FCanvas: ICanvas;
    FWidth: Integer;
    FHeight: Integer;
    FTime: Double;
    FSubTime: Double;
    FSubScene: TScene;
    procedure SetSubScene(Value: TScene);
  protected
    { A placeholder for your default font }
    Font: IFont;
    { A convenient studio shaped rectangle }
    function ClientRect: TRectF;
    { Scale all rendering to force the render area to fit a universal studio size.
      See note at the top of this unit. }
    procedure ScaleToStudio;
    { Convert between window coordinates and studio coordinates }
    function PointToStudio(X, Y: Single): TPointF; overload;
    function PointToStudio(const P: TPointF): TPointF; overload;
    { Convert between studio coordinates and window coordinates }
    function StudioToPoint(X, Y: Single): TPointF; overload;
    function StudioToPoint(const P: TPointF): TPointF; overload;
    { Draw frame rate, time, and optionally a title }
    procedure DrawSceneInfo(const Title: string = ''); virtual;
    { Input event notification }
    procedure DoKeyDown(var Args: TKeyboardArgs); virtual;
    procedure DoKeyUp(var Args: TKeyboardArgs); virtual;
    procedure DoMouseDown(var Args: TMouseArgs); virtual;
    procedure DoMouseMove(var Args: TMouseArgs); virtual;
    procedure DoMouseUp(var Args: TMouseArgs); virtual;
    { The canvas property should only be used inside load, render, or unload }
    procedure Load; virtual;
    { Logic is invoked at a fixed logic steps before render }
    procedure Logic(Width, Height: Integer; const Time: Double); virtual;
    { Render is invoked after a fixed numer of logical steps }
    procedure Render(Width, Height: Integer; const Time: Double); virtual;
    procedure Unload; virtual;
    property Canvas: ICanvas read FCanvas;
  protected
    { If want mouse or key returns true, then a scene always receives events
      even if they are handled elsewhere }
    function WantMouse: Boolean; virtual;
    function WantKeys: Boolean; virtual;
    { Sub scenes are owned and run by other scenes. If you use the SubScene
      property, these methods can be ignored. However if you want to run more
      that one sub scene at the same time, these methods aid in that endeavor. }
    procedure SubKeyDown(Sub: TScene; var Args: TKeyboardArgs);
    procedure SubKeyUp(Sub: TScene; var Args: TKeyboardArgs);
    procedure SubMouseDown(Sub: TScene; var Args: TMouseArgs);
    procedure SubMouseMove(Sub: TScene; var Args: TMouseArgs);
    procedure SubMouseUp(Sub: TScene; var Args: TMouseArgs);
    procedure SubLogic(Sub: TScene; Width, Height: Integer; const Time: Double);
    procedure SubLoad(Sub: TScene);
    procedure SubRender(Sub: TScene; Width, Height: Integer; const Time: Double);
    procedure SubUnload(Sub: TScene);
    { The above sub scene related methods can be ignored if you use the property below }
    property SubScene: TScene read FSubScene write SetSubScene;
  public
    constructor Create(Canvas: ICanvas); virtual;
    { Duplicates of logic and render arguments }
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Time: Double read FTime;
  end;

{ Scene class types are used to by the application run method }

  TSceneClass = class of TScene;

{ The TWidgetScene provides UI widgets through the Widget property. In most
  cases it makes sense to have only one widget scene in your application. }

  TWidgetScene = class(TScene)
  private
    FWidget: TMainWidget;
    FDefaultTheme: TTheme;
    FWidgetMatrix: IMatrix;
    procedure MouseMatrix(var Args: TMouseArgs);
    procedure SetWidgetMatrix(Value: IMatrix);
    function GetWidget: TMainWidget;
  protected
    procedure DoKeyDown(var Args: TKeyboardArgs); override;
    procedure DoKeyUp(var Args: TKeyboardArgs); override;
    procedure DoMouseDown(var Args: TMouseArgs); override;
    procedure DoMouseMove(var Args: TMouseArgs); override;
    procedure DoMouseUp(var Args: TMouseArgs); override;
    procedure Unload; override;
    { Render all widgets using the current widget tranform matrix }
    procedure WidgetsRender(Width, Height: Integer; const Time: Double);
    { Override this method to set your preferred default theme }
    function DefaultTheme: TTheme; virtual;
    { The main widget for the scene }
    property Widget: TMainWidget read GetWidget;
    { Widget matrix can be used to transform all widgets }
    property WidgetMatrix: IMatrix read FWidgetMatrix write SetWidgetMatrix;
  public
    constructor Create(Canvas: ICanvas); override;
  end;

{ Use the global Application function to gain access to TApplication }

  TApplication = class
  private
    FWindow: Pointer;
    FTime: Double;
    FScene: TScene;
    FRunning: Boolean;
    FFullscreen: Boolean;
    FTitle: string;
    FSceneClass: TSceneClass;
    FWidth: Integer;
    FHeight: Integer;
    FVSync: Boolean;
    FFrameRate: Integer;
    procedure SetFullscreen(Value: Boolean);
    procedure SetTitle(Value: string);
    procedure SetWidth(Value: Integer);
    procedure SetHeight(Value: Integer);
    procedure SetVSync(Value: Boolean);
  public
    { Do not call. Use the global application function instead. }
    constructor Create;
    { Run a scene given a class.
      Note: You may change scenes by calling run with a different scene class.
      If no class is given then the application will be asked to terminate. }
    procedure Run(SceneClass: TSceneClass = nil);
    { Ask the application to stop running }
    procedure Terminate;
    { Properties of the application }
    property FrameRate: Integer read FFrameRate;
    property Fullscreen: Boolean read FFullscreen write SetFullscreen;
    property SceneClass: TSceneClass read FSceneClass write FSceneClass;
    property Title: string read FTitle write SetTitle;
    property Time: Double read FTime;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    { Vertical sync defaults to false. When set to true scene wait after each
      render frame for the next sync, typically 1/60 of a second. }
    property VSync: Boolean read FVSync write SetVSync;
  end;

type
  TMessageType = (mtError, mtWarning, mtInformation);

{ Show a modal dialog box with a title and message }
procedure MessageBox(const Title, Message: string; MessageType: TMessageType = mtInformation); overload;
function MessageBox(const Title, Message: string; MessageType: TMessageType;
  const Buttons: array of string): Integer; overload;

{$region input}
{ THardwareCollection<T> represents a collection of connectable devices such as joysticks
  See also
  <link Overview.Tiny.Game.THardwareCollection, THardwareCollection members> }

type
  THardwareCollection<T: TObject> = class(TOwnerCollection<T>)
  public
    {doc ignore}
    constructor Create;
    { Scan for connected devices }
    procedure ScanHardware; virtual;
    property Count;
  end;

{ TInputDevice is an abstract class
  See also
  <link Overview.Tiny.Game.TInputDevice, TInputDevice members> }

  TInputDevice = class
  public
    { Abstract method to scan an input device state }
    procedure Scan; virtual; abstract;
  end;

{ TKeyboard allows you to detect which keys are pressed. You should never create an
  instance of TKeyboard, instead use the global <link Tiny.Game.Keyboard, Keyboard function>.
  See also
  <link Overview.Tiny.Game.TKeyboard, TKeyboard members> }

  TKeyboard = class(TInputDevice)
  private
    FKeys: PUint8;
    FCount: LongWord;
    function GetKey(Index: TKeyCode): Boolean;
    function GetShiftState: TShiftState;
  public
    { Scan the keyboard state }
    procedure Scan; override;
    { The default indexer returns true is a key is pressed down }
    property Key[Index: TKeyCode]: Boolean read GetKey;
    { Returns the current shift state }
    property ShiftState: TShiftState read GetShiftState;
  end;

{ TMouse allows you to detect the state of the mouse. You should never create an
  instance of TMouse, instead use the global <link Tiny.Game.Mouse, Mouse function>.
  See also
  <link Overview.Tiny.Game.TMouse, TMouse members> }

  TMouse = class(TInputDevice)
  private
    FButtons: TMouseButtons;
    FX: Integer;
    FY: Integer;
    function GetCaptured: Boolean;
    procedure SetCaptured(Value: Boolean);
    function GetVisible: Boolean;
    procedure SetVisible(Value: Boolean);
  public
    { Scan the mouse state }
    procedure Scan; override;

    { Move the mouse to a postion relative to the window. Removed for now.
      procedure Move(X, Y: Integer); }

    { The mouse button state }
    property Buttons: TMouseButtons read FButtons;
    { The x position of the mouse relative to the window with mouse focus }
    property X: Integer read FX;
    { The y position of the mouse relative to the window with mouse focus }
    property Y: Integer read FY;
    { When captured the mouse cursor is hidden and events report relative coordinates }
    property Captured: Boolean read GetCaptured write SetCaptured;
    { Shows or hides the mouse cursor }
    property Visible: Boolean read GetVisible write SetVisible;
  end;

{ TJoystickPart<T> represents a series of axes, buttons, or trackballs which are
  part of one joystick
  See also
  <link Overview.Tiny.Game.TJoystickPart, TJoystickPart members>
  <link Tiny.Game.TJoystick, TJoystick class> }

  TJoystickPart<T> = class(TInputDevice)
  private
    FJoystick: PSDL_Joystick;
    FValues: TArrayList<T>;
    function GetCount: Integer;
    function GetValue(Index: Integer): T;
  protected
    procedure DoScan; virtual; abstract;
  public
    { The enumerator type for  TJoystickPart }
    type TEnumerator = TArrayEnumerator<T>;
    { Returns a part enumerator }
    function GetEnumerator: TEnumerator;
  public
    {doc ignore}
    constructor Create(Joystick: PSDL_Joystick);
    { Scan joystick parts and their states }
    procedure Scan; override;
    { Number of axes, buttons, or trackballs on a joystick }
    property Count: Integer read GetCount;
    { The state of an axis, button, or trackball on a joystick }
    property Value[Index: Integer]: T read GetValue; default;
  end;

{ TJoystickAxes is a collection of analog sticks which are parts of one joystick.
  Values range from -1 to 1.
  Remarks
  Typically value[0, 1] are x, y on the first stick, value[2, 3] is [x, y] second
  stick, and so on. Side analog triggers can also represented by value[n]. The
  configuration varies by joystick.
  See also
  <link Overview.Tiny.Game.TJoystickAxes, TJoystickAxes members>
  <link Tiny.Game.TJoystick, TJoystick class> }

  TJoystickAxes = class(TJoystickPart<Float>)
  private
    FAxis: array[0..11] of Float;
  protected
    { Scan axes }
    procedure DoScan; override;
  end;

{ TJoystickButtons is a collection of buttons which are parts of one joystick
  See also
  <link Overview.Tiny.Game.TJoystickButtons, TJoystickButtons members>
  <link Tiny.Game.TJoystick, TJoystick class> }

  TJoystickButtons = class(TJoystickPart<Boolean>)
  protected
    { Scan button }
    procedure DoScan; override;
  end;

{ TJoystickHats is a collection of 9 position switches which are parts of one joystick
  See also
  <link Overview.Tiny.Game.TJoystickHats, TJoystickHats members>
  <link Tiny.Game.TJoystick, TJoystick class> }

  TJoystickHats = class(TJoystickPart<TJoystickHat>)
  protected
    { Scan hats }
    procedure DoScan; override;
  end;

{ TJoystickBall represents a trackball which is part of a joystick
  See also
  <link Overview.Tiny.Game.TJoystickBall, TJoystickBall members>
  <link Tiny.Game.TJoystick, TJoystick class> }

  TJoystickTrackball = record
    { The x change for a trackball }
    XDelta: Integer;
    { The y change for a trackball }
    YDelta: Integer;
  end;

{ TJoystickBalls is a collection of trackballs which are parts of one joystick
  See also
  <link Overview.Tiny.Game.TJoystickBalls, TJoystickBalls members>
  <link Tiny.Game.TJoystick, TJoystick class> }

  TJoystickTrackballs = class(TJoystickPart<TJoystickTrackball>)
  protected
    { Scan trackballs }
    procedure DoScan; override;
  end;

{ TJoystick is an interface to a peripheral with zero or more analog sticks (axes),
  buttons, hats, or trackballs
  See also
  <link Overview.Tiny.Game.TJoystick, TJoystick members> }

  TJoystick = class(TInputDevice)
  private
    FJoystick: PSDL_Joystick;
    FIndex: Integer;
    FName: string;
    FAxes: TJoystickAxes;
    FButtons: TJoystickButtons;
    FHats: TJoystickHats;
    FTrackballs: TJoystickTrackballs;
  public
    {doc ignore}
    constructor Create(Index: Integer);
    destructor Destroy; override;
    { Scan for joystick parts and their states }
    procedure Scan; override;
    { The name of the joystick }
    property Name: string read FName;
    { A collection of analog sticks }
    property Axes: TJoystickAxes read FAxes;
    { A collection of buttons }
    property Buttons: TJoystickButtons read FButtons;
    { A collection of 9-way hat switches }
    property Hats: TJoystickHats read FHats;
    { A collection of trackballs }
    property Trackballs: TJoystickTrackballs read FTrackballs;
  end;

{ TJoysticks provides access to one or more joysticks. You should never create an
  instance of TJoysticks, instead use the global <link Tiny.Game.Joysticks, Joysticks function>.
  See also
  <link Overview.Tiny.Game.TJoysticks, TJoysticks members>
  <link Tiny.Game.TJoystick, TJoystick class> }

  TJoysticks = class(THardwareCollection<TJoystick>)
  private
    function GetJoystick(Index: Integer): TJoystick;
  public
    { Scan for new joysticks or update the state of existing ones }
    procedure ScanHardware; override;
    { The default indexer of type <link Tiny.Game.TJoystick, TJoystick> }
    property Joystick[Index: Integer]: TJoystick read GetJoystick; default;
  end;

{ Scan for and updates the state of your mouse, keyboard, and joysticks
  Remarks
  This procedure is called for you automatically }
procedure InputScanHardware;
{ InputDown returns true if a key is down
  Remarks
  Key is the keyboard key to check }
function InputDown(Key: LongWord): Boolean; overload;
{ InputDown returns true if a key or joystick button is down
  Remarks
  Key is the keyboard key to check
  Joystick is the joystick index to check
  Button is the joystick button index to check }
function InputDown(Key, Joystick, Button: LongWord): Boolean; overload;
{ InputPressed returns true if a key transitioned from up to down state
  Remarks
  Down is a reference value you pass to hold the prior down state
  Key is the keyboard key to track }
function InputPressed(var Down: Boolean; Key: LongWord): Boolean; overload;
{ InputPressed returns true if a key or joystick button transitioned from up to down state
  Remarks
  Down is a reference value you pass to hold the prior down state
  Key is the keyboard key to track
  Joystick is the joystick index to track
  Button is the joystick button index to track }
function InputPressed(var Down: Boolean; Key, Joystick, Button: LongWord): Boolean; overload;
{ InputAxis returns true if a key is down or a joystick axis exceeds some limit
  Remarks
  Key is the keyboard key to check
  Joystick is the joystick index to check
  Axis is the joystick axis index to check
  Limit is the value of the axis when exceeded causes true to be returned }
function InputAxis(Key, Joystick, Axis: LongWord; Limit: Float): Boolean;
{$endregion}

{$region globals}
{ Provides access to <link Tiny.Game.TApplication, TApplication class> }
function Application: TApplication;
{ Provides access to <link Tiny.Game.TKeyboard, Keyboard TKeyboard> }
function Keyboard: TKeyboard;
{ Provides access to <link Tiny.Game.TMouse, TMouse class> }
function Mouse: TMouse;
{ Provides access to <link Tiny.Game.TJoysticks, TJoysticks class> }
function Joysticks: TJoysticks;
{$endregion}

implementation

uses
  Tiny.Interop.GL,
  Tiny.Interop.NanoVG;

{$region input}
function GetShiftState: TShiftState;
var
  KeyMod: LongWord;
begin
  KeyMod := SDL_GetModState;
  Result := [];
  if KMOD_CTRL and KeyMod <> 0 then
    Include(Result, ssCtrl);
  if KMOD_SHIFT and KeyMod <> 0 then
    Include(Result, ssShift);
  if KMOD_ALT and KeyMod <> 0 then
    Include(Result, ssAlt);
end;

{ THardwareCollection<T> }

constructor THardwareCollection<T>.Create;
begin
  inherited Create;
  ScanHardware;
end;

procedure THardwareCollection<T>.ScanHardware;
var
  I: Integer;
begin
  for I := Items.Lo to Items.Hi do
    Items[I].Free;
  Items := nil;
end;

{ TKeyboard }

procedure TKeyboard.Scan;
begin
  Lock;
  try
    FKeys := SDL_GetKeyboardState(@FCount);
  finally
    Unlock;
  end;
end;

function TKeyboard.GetKey(Index: TKeyCode): Boolean;
var
  ScanCode: LongWord;
  Key: PUint8;
begin
  if FKeys = nil then
    Scan;
  ScanCode := SDL_GetScancodeFromKey(LongWord(Index));
  if ScanCode >= FCount then
    Exit(False);
  Key := FKeys;
  Inc(Key, ScanCode);
  Result := Key^ <> 0;
end;

function TKeyboard.GetShiftState: TShiftState;
begin
  if FKeys = nil then
    Scan;
  Result := GetShiftState;
end;

{ TMouse }

procedure TMouse.Scan;
var
  Button: TMouseButton;
  B: Uint32;
begin
  Lock;
  try
    FButtons := [];
    B := SDL_GetMouseState(FX, FY);
    for Button := Low(Button) to High(Button) do
      if SDL_Button(Ord(Button)) and B <> 0 then
        Include(FButtons, Button);
  finally
    Unlock;
  end;
end;

function TMouse.GetCaptured: Boolean;
begin
  Result := SDL_GetRelativeMouseMode;
end;

procedure TMouse.SetCaptured(Value: Boolean);
begin
  SDL_SetRelativeMouseMode(Value);
end;

function TMouse.GetVisible: Boolean;
begin
  Result := SDL_ShowCursor(-1) <> 0;
end;

procedure TMouse.SetVisible(Value: Boolean);
begin
  if Value then
    SDL_ShowCursor(1)
  else
    SDL_ShowCursor(0);
end;

{ TJoystickPart<T> }

constructor TJoystickPart<T>.Create(Joystick: PSDL_Joystick);
begin
  inherited Create;
  FJoystick := Joystick;
  DoScan;
end;

function TJoystickPart<T>.GetEnumerator: TEnumerator;
begin
  Result.Create(FValues);
end;

procedure TJoystickPart<T>.Scan;
begin
  if FJoystick = nil then
    FValues := nil
  else
  begin
    Lock;
    try
      DoScan;
    finally
      Unlock;
    end;
  end;
end;

function TJoystickPart<T>.GetCount: Integer;
begin
  Result := FValues.Length;
end;

function TJoystickPart<T>.GetValue(Index: Integer): T;
begin
  Result := FValues[Index];
end;

{ Scan retrieves the state of the analog sticks }

procedure TJoystickAxes.DoScan;
const
  Sigma = 0.01;
  MaxJoyAxis = 32767;
var
  I: Integer;
begin
  I := SDL_JoystickNumAxes(FJoystick);
  if I < 1 then
  begin
    FValues := nil;
    Exit;
  end;
  FValues.Length := I;
  while I > 0 do
  begin
    Dec(I);
    FValues[I] := SDL_JoystickGetAxis(FJoystick, I) / MaxJoyAxis;
    if Abs(FValues[I]) < Sigma then
      FValues[I] := 0
    else if Abs(FValues[I]) > 1 - Sigma then
      if FValues[I] < 0 then
        FValues[I] := -1
      else
        FValues[I] := 1
    { The line below smooths axis 'jitter' }
    else if Abs(FAxis[I] - FValues[I]) < Sigma then
      FValues[I] := FAxis[I];
    FAxis[I] := FValues[I];
  end;
end;

{ Scan retrieves the pressed state the buttons }

procedure TJoystickButtons.DoScan;
var
  I: Integer;
begin
  I := SDL_JoystickNumButtons(FJoystick);
  if I < 1 then
  begin
    FValues := nil;
    Exit;
  end;
  FValues.Length := I;
  while I > 0 do
  begin
    Dec(I);
    FValues[I] := SDL_JoystickGetButton(FJoystick, I) <> 0;
  end;
end;

{ Scan retrieves the state the hats }

procedure TJoystickHats.DoScan;
var
  I: Integer;
begin
  I := SDL_JoystickNumHats(FJoystick);
  if I < 1 then
  begin
    FValues := nil;
    Exit;
  end;
  FValues.Length := I;
  while I > 0 do
  begin
    Dec(I);
    case SDL_JoystickGetHat(FJoystick, I) of
      SDL_HAT_UP: FValues[I] := hatUp;
      SDL_HAT_RIGHT: FValues[I] := hatRight;
      SDL_HAT_DOWN: FValues[I] := hatDown;
      SDL_HAT_LEFT: FValues[I] := hatLeft;
      SDL_HAT_RIGHTUP: FValues[I] := hatRightUp;
      SDL_HAT_RIGHTDOWN: FValues[I] := hatRightDown;
      SDL_HAT_LEFTUP: FValues[I] := hatLeftUp;
      SDL_HAT_LEFTDOWN: FValues[I] := hatLeftDown;
    else
      FValues[I] := hatCenter;
    end;
  end;
end;

{ Scan retrieves the pressed state the trackballs }

procedure TJoystickTrackballs.DoScan;
var
  B: TJoystickTrackball;
  I: Integer;
begin
  I := SDL_JoystickNumBalls(FJoystick);
  if I < 1 then
  begin
    FValues := nil;
    Exit;
  end;
  FValues.Length := I;
  while I > 0 do
  begin
    Dec(I);
    SDL_JoystickGetBall(FJoystick, I, B.XDelta, B.YDelta);
    FValues[I] := B;
  end;
end;

{ TJoystick }

constructor TJoystick.Create(Index: Integer);
begin
  inherited Create;
  FIndex := Index;
  FJoystick := SDL_JoystickOpen(Index);
  FName := SDL_JoystickName(FJoystick);
  FAxes := TJoystickAxes.Create(FJoystick);
  FButtons := TJoystickButtons.Create(FJoystick);
  FHats := TJoystickHats.Create(FJoystick);
  FTrackballs := TJoystickTrackballs.Create(FJoystick);
end;

destructor TJoystick.Destroy;
begin
  FAxes.Free;
  FButtons.Free;
  FHats.Free;
  FTrackballs.Free;
  SDL_JoystickClose(FJoystick);
  inherited Destroy;
end;

{ Scan retrieves the state of the joystick }

procedure TJoystick.Scan;
begin
  Lock;
  try
    FAxes.Scan;
    FButtons.Scan;
    FHats.Scan;
    FTrackballs.Scan;
  finally
    Unlock;
  end;
end;

{ TJoysticks }

procedure TJoysticks.ScanHardware;
var
  I: Integer;
begin
  Lock;
  try
    I := SDL_NumJoysticks;
    if I <> Items.Length then
    begin
      inherited ScanHardware;
      if I < 1 then
        Exit;
      Items.Length := I;
      for I := Items.Lo to Items.Hi do
        Items[I] := TJoystick.Create(I);
    end;
    for I := Items.Lo to Items.Hi do
      Items[I].Scan;
  finally
    Unlock;
  end;
end;

function TJoysticks.GetJoystick(Index: Integer): TJoystick;
begin
  Result := Items[Index];
end;

{ Input related routines }

function InputDown(Key: LongWord): Boolean;
begin
  Result := Keyboard.Key[Key];
end;

function InputDown(Key, Joystick, Button: LongWord): Boolean;
begin
  Result := False;
  if Keyboard.Key[Key] then
    Exit(True);
  if (Joystick < Joysticks.Count) and (Button < Joysticks[Joystick].Buttons.Count) then
    Result := Joysticks[Joystick].Buttons[Button];
end;

function InputPressed(var Down: Boolean; Key: LongWord): Boolean;
var
  WasDown: Boolean;
begin
  WasDown := Down;
  Down := Keyboard.Key[Key];
  Result := Down and (not WasDown);
end;

function InputPressed(var Down: Boolean; Key, Joystick, Button: LongWord): Boolean;
var
  WasDown: Boolean;
begin
  WasDown := Down;
  Down := Keyboard.Key[Key];
  if (Joystick < Joysticks.Count) and (Button < Joysticks[Joystick].Buttons.Count) then
    Down := Down or Joysticks[Joystick].Buttons[Button];
  Result := Down and (not WasDown);
end;

function InputAxis(Key, Joystick, Axis: LongWord; Limit: Float): Boolean;
begin
  Result := False;
  if Keyboard.Key[Key] then
    Exit(True);
  if (Joystick < Joysticks.Count) and (Axis < Joysticks[Joystick].Axes.Count) then
    if Limit < 0 then
      Result := Joysticks[Joystick].Axes[Axis] < Limit
    else
      Result := Joysticks[Joystick].Axes[Axis] > Limit;
end;
{$endregion}

{$region globals}
var
  ApplicationInstance: TObject;
  KeyboardInstance: TObject;
  MouseInstance: TObject;
  JoysticksInstance: TObject;

function Application: TApplication;
begin
  if ApplicationInstance = nil then
  try
    Lock;
    if ApplicationInstance = nil then
      ApplicationInstance := TApplication.Create;
  finally
    Unlock;
  end;
  Result := TApplication(ApplicationInstance);
end;

function Keyboard: TKeyboard;
begin
  if KeyboardInstance = nil then
  try
    Lock;
    if KeyboardInstance = nil then
      KeyboardInstance := TKeyboard.Create;
  finally
    Unlock;
  end;
  Result := TKeyboard(KeyboardInstance);
end;

function Mouse: TMouse;
begin
  if MouseInstance = nil then
  try
    Lock;
    if MouseInstance = nil then
      MouseInstance := TMouse.Create;
  finally
    Unlock;
  end;
  Result := TMouse(MouseInstance);
end;

function Joysticks: TJoysticks;
begin
  if JoysticksInstance = nil then
  try
    Lock;
    if JoysticksInstance = nil then
      JoysticksInstance := TJoysticks.Create;
  finally
    Unlock;
  end;
  Result := TJoysticks(JoysticksInstance);
end;

procedure InputScanHardware;
begin
  Lock;
  try
    if MouseInstance <> nil then
      TInputDevice(MouseInstance).Scan;
    if KeyboardInstance <> nil then
      TInputDevice(KeyboardInstance).Scan;
    if JoysticksInstance <> nil then
      TJoysticks(JoysticksInstance).ScanHardware;
  finally
    Unlock;
  end;
end;
{$endregion}

{ TScene }

constructor TScene.Create(Canvas: ICanvas);
begin
  inherited Create;
  FCanvas := Canvas;
end;

function TScene.ClientRect: TRectF;
begin
  Result := NewRectF(StudioWidth, StudioHeight);
end;

procedure TScene.ScaleToStudio;
begin
  Canvas.Matrix.Identity;
  Canvas.Matrix.Scale(FWidth / StudioWidth, FHeight / StudioHeight);
end;

function TScene.PointToStudio(X, Y: Single): TPointF;
begin
  Result.X := X * StudioWidth / Width;
  Result.Y := Y * StudioHeight / Height;
end;

function TScene.PointToStudio(const P: TPointF): TPointF;
begin
  Result.X := P.X * StudioWidth / Width;
  Result.Y := P.Y * StudioHeight / Height;
end;

function TScene.StudioToPoint(X, Y: Single): TPointF;
begin
  Result.X := X * Width / StudioWidth;
  Result.Y := Y * Height / StudioHeight;
end;

function TScene.StudioToPoint(const P: TPointF): TPointF;
begin
  Result.X := P.X * Width / StudioWidth;
  Result.Y := P.Y * Height / StudioHeight;
end;

procedure TScene.DrawSceneInfo(const Title: string = '');
var
  S: string;
begin
  if Font = nil then
    Exit;
  S := 'FPS: ' + IntToStr(Application.Framerate) + #10'Time: ' + FloatToStr(Application.Time, 2);
  {$ifdef backend}
  S := 'Backend: ' + SNanoVGBackend + #10 + S;
  {$endif}
  if Title <> '' then
    S := S + #10#10 + Title;
  Font.Align := fontLeft;
  Font.Layout := fontTop;
  Canvas.DrawTextMemo(Font, S, 20, 20, StudioWidth - 40);
end;

procedure TScene.DoKeyDown(var Args: TKeyboardArgs);
begin
  if FSubScene <> nil then SubKeyDown(FSubScene, Args);
end;

procedure TScene.DoKeyUp(var Args: TKeyboardArgs);
begin
  if FSubScene <> nil then SubKeyUp(FSubScene, Args);
end;

procedure TScene.DoMouseDown(var Args: TMouseArgs);
begin
  if FSubScene <> nil then SubMouseDown(FSubScene, Args);
end;

procedure TScene.DoMouseMove(var Args: TMouseArgs);
begin
  if FSubScene <> nil then SubMouseMove(FSubScene, Args);
end;

procedure TScene.DoMouseUp(var Args: TMouseArgs);
begin
  if FSubScene <> nil then SubMouseUp(FSubScene, Args);
end;

procedure TScene.Load;
begin
end;

procedure TScene.Unload;
begin
end;

procedure TScene.Logic(Width, Height: Integer; const Time: Double);
begin
  FWidth := Width;
  FHeight := Height;
  FTime := Time;
  if FSubScene <> nil then SubLogic(FSubScene, Width, Height, FTime - FSubTime);
end;

procedure TScene.Render(Width, Height: Integer; const Time: Double);
begin
  FWidth := Width;
  FHeight := Height;
  FTime := Time;
  if FSubScene <> nil then SubRender(FSubScene, Width, Height, FTime - FSubTime);
end;

procedure TScene.SetSubScene(Value: TScene);
begin
  if Value = FSubScene then
    Exit;
  if FSubScene <> nil then
    SubUnload(FSubScene);
  FSubScene := Value;
  if FSubScene <> nil then
  begin
    FSubTime := FTime;
    FSubScene.FTime := 0;
    SubLoad(FSubScene);
  end;
end;

function TScene.WantMouse: Boolean;
begin
  Result := False;
end;

function TScene.WantKeys: Boolean;
begin
  Result := False;
end;

procedure TScene.SubKeyDown(Sub: TScene; var Args: TKeyboardArgs);
begin
  if Sub.WantKeys then
    Sub.DoKeyDown(Args)
  else if not Args.Handled then
    Sub.DoKeyDown(Args);
end;

procedure TScene.SubKeyUp(Sub: TScene; var Args: TKeyboardArgs);
begin
  if Sub.WantKeys then
    Sub.DoKeyUp(Args)
  else if not Args.Handled then
    Sub.DoKeyUp(Args);
end;

procedure TScene.SubMouseDown(Sub: TScene; var Args: TMouseArgs);
begin
  if Sub.WantMouse then
    Sub.DoMouseDown(Args)
  else if not Args.Handled then
    Sub.DoMouseDown(Args);
end;

procedure TScene.SubMouseMove(Sub: TScene; var Args: TMouseArgs);
begin
  if Sub.WantMouse then
    Sub.DoMouseMove(Args)
  else if not Args.Handled then
    Sub.DoMouseMove(Args);
end;

procedure TScene.SubMouseUp(Sub: TScene; var Args: TMouseArgs);
begin
  if Sub.WantMouse then
    Sub.DoMouseUp(Args)
  else if not Args.Handled then
    Sub.DoMouseUp(Args);
end;

procedure TScene.SubLogic(Sub: TScene; Width, Height: Integer; const Time: Double);
begin
  Sub.Logic(Width, Height, Time);
end;

procedure TScene.SubLoad(Sub: TScene);
begin
  Sub.Load;
end;

procedure TScene.SubRender(Sub: TScene; Width, Height: Integer; const Time: Double);
begin
  Canvas.Push;
  Sub.Render(Width, Height, Time);
  Canvas.Pop;
end;

procedure TScene.SubUnload(Sub: TScene);
begin
  Sub.Unload;
end;

{ TWidgetScene }

constructor TWidgetScene.Create(Canvas: ICanvas);
begin
  inherited Create(Canvas);
  FWidgetMatrix := NewMatrix;
end;

procedure TWidgetScene.MouseMatrix(var Args: TMouseArgs);
var
  P: TPointF;
begin
  P.X := Args.X;
  P.Y := Args.Y;
  P := FWidgetMatrix.Inverse.Multiply(P);
  Args.X := P.X;
  Args.Y := P.Y;
end;

procedure TWidgetScene.SetWidgetMatrix(Value: IMatrix);
begin
  FWidgetMatrix.Copy(Value);
end;

procedure TWidgetScene.WidgetsRender(Width, Height: Integer; const Time: Double);
begin
  Canvas.Matrix.Push;
  Canvas.Matrix := FWidgetMatrix;
  FWidget.Render(Width, Height, Time);
  Canvas.Matrix.Pop;
end;

procedure TWidgetScene.DoKeyDown(var Args: TKeyboardArgs);
begin
  if FWidget <> nil then FWidget.DispatchKeyDown(Args);
  inherited DoKeyDown(Args);
end;

procedure TWidgetScene.DoKeyUp(var Args: TKeyboardArgs);
begin
  if FWidget <> nil then FWidget.DispatchKeyUp(Args);
  inherited DoKeyUp(Args);
end;

procedure TWidgetScene.DoMouseDown(var Args: TMouseArgs);
var
  A: TMouseArgs;
begin
  A := Args;
  MouseMatrix(A);
  if FWidget <> nil then FWidget.DispatchMouseDown(A);
  Args.Handled := A.Handled;
  inherited DoMouseDown(Args);
end;

procedure TWidgetScene.DoMouseMove(var Args: TMouseArgs);
var
  A: TMouseArgs;
begin
  A := Args;
  MouseMatrix(A);
  if FWidget <> nil then FWidget.DispatchMouseMove(A);
  Args.Handled := A.Handled;
  inherited DoMouseMove(Args);
end;

procedure TWidgetScene.DoMouseUp(var Args: TMouseArgs);
var
  A: TMouseArgs;
begin
  A := Args;
  MouseMatrix(A);
  if FWidget <> nil then FWidget.DispatchMouseUp(A);
  Args.Handled := A.Handled;
  inherited DoMouseUp(Args);
end;

procedure TWidgetScene.Unload;
begin
  FWidget.Free;
  FWidget := nil;
  FDefaultTheme.Free;
  FDefaultTheme := nil;
  inherited Unload;
end;

function TWidgetScene.DefaultTheme: TTheme;
begin
  if FDefaultTheme <> nil then
    Exit(FDefaultTheme);
  FDefaultTheme := NewTheme(Canvas, TArcDarkTheme);
  Result := FDefaultTheme;
end;

function TWidgetScene.GetWidget: TMainWidget;
begin
  if FWidget = nil then
    FWidget := TMainWidget.Create(DefaultTheme);
  Result := FWidget;
end;

{ TApplication }

function Now: Double;
const
  TimeFrequency: UInt64 = 0;
  TimeStart: Double = 0;
begin
  if TimeFrequency = 0 then
  begin
    TimeFrequency := SDL_GetPerformanceFrequency;
    TimeStart := SDL_GetPerformanceCounter / TimeFrequency;
  end;
  Result := SDL_GetPerformanceCounter / TimeFrequency - TimeStart;
end;

constructor TApplication.Create;
begin
  inherited Create;
  {$ifdef nodisplay}
  FFullscreen := True;
  {$else}
  FTitle := 'Scene';
  FWidth := StudioWidth div 2;
  FHeight := StudioHeight div 2;
  {$endif}
end;

procedure TApplication.Run(SceneClass: TSceneClass = nil);
const
  SyncFlag: array[Boolean] of UInt32 = (0, 1);
  FrameMeasure = 0.25;
var
  {$ifdef nodisplay}
	Mode: SDL_DisplayMode;
  {$endif}
  CtxGL: PSDL_GLContext;
  Canvas: ICanvas;
  Buffer: IBackBuffer;
  Event: TSDL_Event;
  Start, Current, LocalTime, Last, Frame: Double;
  KeyArgs: TKeyboardArgs;
  MouseArgs: TMouseArgs;
begin
  { Check if running }
  if FRunning then
    Exit;
  { Check if there is a scene class }
  if SceneClass <> nil then
    FSceneClass := SceneClass;
  if FSceneClass = nil then
    Exit;
  SceneClass := FSceneClass;
  { Start SDL }
  if SDL_Init(SDL_INIT_EVERYTHING) < 0 then
    Exit;
  SDL_GL_SetAttribute(SDL_GL_STENCIL_SIZE, 8);
  SDL_GL_SetAttribute(SDL_GL_DOUBLEBUFFER, 1);
  {$ifdef gles2}
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 2);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, 	SDL_GL_CONTEXT_PROFILE_ES);
  {$else}
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 0);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_PROFILE_MASK, 	SDL_GL_CONTEXT_PROFILE_CORE);
  {$endif}
  {$ifdef nodisplay}
	SDL_GetCurrentDisplayMode(0, Mode);
  FWidth := Mode.w;
  FHeight := Mode.h;
	FWindow := SDL_CreateWindow('', SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, FWidth, FHeight,
		SDL_WINDOW_OPENGL or SDL_WINDOW_FULLSCREEN or SDL_WINDOW_SHOWN);
  {$else}
  FWindow := SDL_CreateWindow(PChar(FTitle), SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, FWidth, FHeight,
    SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE or SDL_WINDOW_SHOWN);
  {$endif}
  if FWindow = nil then
    Exit;
  { Create a GL context }
  CtxGL := SDL_GL_CreateContext(FWindow);
  SDL_GL_MakeCurrent(FWindow, CtxGL);
  SDL_GL_SetSwapInterval(SyncFlag[FVSync]);
  { Start running }
  Start := Now;
  Current := Start;
  FTime := 0;
  LocalTime := 0;
  Last := Start;
  Frame := 0;
  FRunning := True;
  try
    if not LoadGL(SDL_GL_GetProcAddress) then
    begin
      MessageBox('Error', 'Could not load the required OpenGL functions for ' + SNanoVGBackend + ' backend');
      Exit;
    end;
    glClearColor(0, 0, 0, 0);
    { Create a VG context using }
    {$ifdef gles2}
    LoadNanoVG(SDL_GL_GetProcAddress);
    {$endif}
    {$ifdef gl3}
    if not LoadNanoVG(SDL_GL_GetProcAddress) then
    begin
      MessageBox('Error', 'Could not load the required OpenGL functions for ' + SNanoVGBackend + ' backend');
      Exit;
    end;
    {$endif}
    Canvas := NewCanvas;
    Buffer := Canvas as IBackBuffer;
    {$ifndef nodisplay}
    if FFullscreen then
      SDL_SetWindowFullscreen(FWindow, FFullscreen);
    {$endif}
    { Create the scene }
    FScene := SceneClass.Create(Canvas);
    try
      {$ifndef nodisplay}
      SDL_GetWindowSize(FWindow, FWidth, FHeight);
      {$endif}
      Buffer.Flip(FWidth, FHeight);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
      FScene.Load;
      Buffer.Flip(FWidth, FHeight);
      { The window event and render loop }
      while FRunning do
      begin
        InputScanHardware;
        { Process events }
        while SDL_PollEvent(Event) <> 0 do
        begin
          if event.type_ = SDL_QUIT_EVENT then
            FRunning := False
          else if event.type_ = SDL_KEYDOWN then
          begin
            KeyArgs.Key := event.key.keysym.sym;
            KeyArgs.Shift := GetShiftState;
            KeyArgs.Repeated := event.key.repeat_ <> 0;
            KeyArgs.Handled := False;
            FScene.DoKeyDown(KeyArgs);
            if not KeyArgs.Handled then
              case KeyArgs.Key of
                SDLK_ESCAPE: FRunning := False;
                SDLK_F1: Fullscreen := not Fullscreen;
                SDLK_F2: VSync := not VSync;
              end;
          end
          else if event.type_ = SDL_KEYUP then
          begin
            KeyArgs.Key := event.key.keysym.sym;
            KeyArgs.Shift := GetShiftState;
            KeyArgs.Repeated := event.key.repeat_ <> 0;
            KeyArgs.Handled := False;
            FScene.DoKeyUp(KeyArgs);
          end
          else if event.type_ = SDL_MOUSEBUTTONDOWN then
          begin
            MouseArgs.Button := TMouseButton(event.button.button);
            MouseArgs.X := event.button.x;
            MouseArgs.Y := event.button.y;
            MouseArgs.XRel := 0;
            MouseArgs.YRel := 0;
            MouseArgs.Shift := GetShiftState;
            MouseArgs.Handled := False;
            FScene.DoMouseDown(MouseArgs);
          end
          else if event.type_ = SDL_MOUSEMOTION then
          begin
            MouseArgs.Button := mbNone;
            MouseArgs.X := event.motion.x;
            MouseArgs.Y := event.motion.y;
            MouseArgs.XRel := event.motion.xrel;
            MouseArgs.YRel := event.motion.yrel;
            MouseArgs.Shift := GetShiftState;
            MouseArgs.Handled := False;
            FScene.DoMouseMove(MouseArgs);
          end
          else if event.type_ = SDL_MOUSEBUTTONUP then
          begin
            MouseArgs.Button := TMouseButton(event.button.button);
            MouseArgs.X := event.button.x;
            MouseArgs.Y := event.button.y;
            MouseArgs.XRel := 0;
            MouseArgs.YRel := 0;
            MouseArgs.Shift := GetShiftState;
            MouseArgs.Handled := False;
            FScene.DoMouseUp(MouseArgs);
          end
        end;
        { Check if the scene class has changed }
        if FSceneClass <> SceneClass then
          if FSceneClass = nil then
            FRunning := False
          else
          begin
            SceneClass := FSceneClass;
            FScene.Unload;
            FScene.Free;
            FScene := SceneClass.Create(Canvas);
            Start := Now;
            Current := Start;
            Last := Start;
            FTime := 0;
            LocalTime := 0;
            Frame := 0;
            FScene.Load;
          end;
        { Compute the frame rate }
        Current := Now;
        Frame := Frame + 1;
        if Current - Last > FrameMeasure then
        begin
          FFrameRate := Round(Frame * (1 / (Current - Last)));
          Last := Current;
          Frame := 0;
        end;
        FTime := Current - Start;
        { Handle the possibility of window resizing }
        SDL_GetWindowSize(FWindow, FWidth, FHeight);
        { Advance the logic in fixed size logic steps steps }
        while LocalTime < FTime - LogicStep do
        begin
          LocalTime := LocalTime + LogicStep;
          FScene.Logic(FWidth, FHeight, LocalTime);
        end;
        { Asjust the viewport }
        glViewport(0, 0, FWidth, FHeight);
        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
        { Render the scene }
        Buffer.Flip(FWidth, FHeight);
        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
        FScene.Render(FWidth, FHeight, FTime);
        Buffer.Flip(FWidth, FHeight);
        SDL_GL_SwapWindow(FWindow);
      end;
      Buffer.Flip(FWidth, FHeight);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);
      FScene.Unload;
      Buffer.Flip(FWidth, FHeight);
    finally
      FScene.Free;
      Canvas := nil;
    end;
  finally
    FreeAndNil(JoysticksInstance);
    FreeAndNil(KeyboardInstance);
    FreeAndNil(MouseInstance);
    FreeAndNil(ApplicationInstance);
    SDL_GL_DeleteContext(CtxGL);
    SDL_DestroyWindow(FWindow);
    SDL_Quit;
    FWindow := nil;
  end;
end;

procedure TApplication.Terminate;
begin
  FRunning := False;
end;

procedure TApplication.SetFullscreen(Value: Boolean);
begin
  {$ifndef nodisplay}
  if Value = FFullscreen then
    Exit;
  FFullscreen := Value;
  if FWindow <> nil then
    SDL_SetWindowFullscreen(FWindow, FFullscreen);
  {$endif}
end;

procedure TApplication.SetTitle(Value: string);
begin
  {$ifndef nodisplay}
  if FTitle = Value then
    Exit;
  FTitle := Value;
  if FWindow <> nil then
    SDL_SetWindowTitle(FWindow, PChar(FTitle));
  {$endif}
end;

procedure TApplication.SetWidth(Value: Integer);
begin
  {$ifndef nodisplay}
  if Value = FWidth then
    Exit;
  if FWindow = nil then
    FWidth := Value
  else if not FFullscreen then
    SDL_SetWindowSize(FWindow, FWidth, FHeight);
  {$endif}
end;

procedure TApplication.SetHeight(Value: Integer);
begin
  {$ifndef nodisplay}
  if Value = FHeight then
    Exit;
  if FWindow = nil then
    FHeight := Value
  else if not FFullscreen then
    SDL_SetWindowSize(FWindow, FWidth, FHeight);
  {$endif}
end;

procedure TApplication.SetVSync(Value: Boolean);
begin
  if Value = FVSync then
    Exit;
  FVSync := Value;
  if FVSync then
    SDL_GL_SetSwapInterval(1)
  else
    SDL_GL_SetSwapInterval(0);
end;

const
  Msgs: array[TMessageType] of LongWord =
    (SDL_MESSAGEBOX_ERROR, SDL_MESSAGEBOX_WARNING, SDL_MESSAGEBOX_INFORMATION);

procedure MessageBox(const Title, Message: string; MessageType: TMessageType = mtInformation);
begin
  {$ifdef nodisplay}
  WriteLn('MessageBox');
  WriteLn('  Title: ', Title);
  WriteLn('  Message: ', Message);
  {$else}
  SDL_ShowSimpleMessageBox(Msgs[MessageType], PChar(Title), PChar(Message), Application.FWindow);
  {$endif}
end;


function MessageBox(const Title, Message: string; MessageType: TMessageType;
  const Buttons: array of string): Integer;
var
  Data: TSDL_MessageBoxData;
  Item: TSDL_MessageBoxButtonData;
  List: TArrayList<TSDL_MessageBoxButtonData>;
  I: Integer;
begin
  if High(Buttons) < 0 then
    Exit(-1);
  Data.flags := Msgs[MessageType];
  Data.title := PChar(Title);
  Data.message := PChar(Message);
  Data.window := nil;
  Data.colorScheme := nil;
  for I := Low(Buttons) to High(Buttons) do
  begin
    Item.flags := 0;
    Item.buttonid := I;
    Item.text := PChar(Buttons[I]);
    List.Push(Item);
  end;
  Data.buttons := @List.Items[0];
  Data.numbuttons := List.Length;
  I := SDL_ShowMessageBox(Data, Result);
  if I < 0 then
  begin
    MEssageBox('Error', SDL_GetError);
    Result := -1;
  end;
end;

end.

