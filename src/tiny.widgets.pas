unit Tiny.Widgets;

{$i tiny.inc}

interface

uses
  Tiny.System,
  Tiny.Types;

{ TThemeColor are theme dependent colors }

type
  TThemeColor = LongWord;

const
  colorBase = TThemeColor(0);
  colorFace = TThemeColor(colorBase + 1);
  colorBorder = TThemeColor(colorFace + 1);
  colorActive = TThemeColor(colorBorder + 1);
  colorPressed = TThemeColor(colorActive + 1);
  colorSelected = TThemeColor(colorPressed + 1);
  colorHot = TThemeColor(colorSelected + 1);
  colorCaption = TThemeColor(colorHot + 1);
  colorTitle = TThemeColor(colorCaption + 1);
  colorText = TThemeColor(colorTitle + 1);
  colorHighlight = TThemeColor(colorText + 1);
  colorShadow = TThemeColor(colorHighlight + 1);
  colorDarkShadow = TThemeColor(colorShadow + 1);

{ TModalButton is used with by main widget message methods }

type
  TModalButton = (mbOkay, mbYes, mbNo, mbAccept, mbCancel);
  TModalButtons = set of TModalButton;
  TModalResult = LongWord;

const
  modalNone = 0;
  modalOk = modalNone + 1;
  modalCancel = modalOk + 1;
  modalYes = modalCancel + 1;
  modalNo = modalYes + 1;
  modalAccept = modalNo + 1;
  modalRetry = modalAccept + 1;
  modalError = modalRetry + 1;
  modelQuit = modalError + 1;

{ The widget system is defined by this unit. Rendering is handled by decedents
  of the theme system class. }

type
  TComputedWidget = class;
  TWidget = class;
  TMainWidget = class;
  TSpacer = class;
  TEdit = class;
  TPushButton = class;
  TGlyphButton = class;
  TGlyphImage = class;
  TCheckBox = class;
  TLabel = class;
  TSlider = class;
  TCustomWidget = class;
  TContainerWidget = class;
  THBox = class;
  TVBox = class;
  TWindow = class;

{ Modal result event type }

  TModalResultEvent = procedure(Sender: TObject; ModalResult: TModalResult) of object;

{ TTheme draws the widgets and provides widget part sizes }

  TThemePart = (tpEverything, tpIndent, tpCaption, tpNode, tpThumb);

  TTheme = class
  public
    { Calculate the actual theme color }
    function CalcColor(Widget: TWidget; Color: TThemeColor): LongWord; virtual; abstract;
    { Calculate the size of a widget }
    function CalcSize(Widget: TWidget; Part: TThemePart): TSizeF; virtual; abstract;
    { Render the widget }
    procedure Render(Widget: TWidget); virtual; abstract;
    { Render a hint for the widget }
    procedure RenderHint(Widget: TWidget; Opacity: Float); virtual; abstract;
  end;

  TThemeClass = class of TTheme;

{ TWidgetStatePart tracks the various stats a widget may contain }

  TWidgetStatePart = (
    { Widget is disabled }
    wsDisabled,
    { Mouse is down and the widget has mouse capture }
    wsPressed,
    { Mouse is hovering over the widget }
    wsHot,
    { The widget has input focus }
    wsSelected,
    { The wiget is in a toggle state such as checked }
    wsToggled);

  TWidgetState = set of TWidgetStatePart;

{ TComputedWidget is used to calculate widget properties inherited from a
  list of parent widgets }

  TComputedWidget = class
  private
    FWidget: TWidget;
  public
    constructor Create(Widget: TWidget);
    function Theme: TTheme;
    function Opacity: Float;
    function Bounds: TRectF;
    function Enabled: Boolean;
    function Visible: Boolean;
    function State: TWidgetState;
  end;

{ TWidgetAlign is used when packing a widget inside a container }

  TWidgetAlign = (alignNear, alignCenter, alignFar);

  TWidgetList = TArrayList<TWidget>;

{ TWidget is the base class for ui controls. A widget is created by giving it
  a parent and rectangle using the create constructor. A widget is destroyed by
  freeing its parent, freeing the widget, or deleteing it from its parent.

  Any children of a widget are destroyed when the widget is destroyed. }

  TWidget = class
  public
    type TWidgetEnumerator = TWidgetList.TArrayListEnumerator;
    function GetEnumerator: TWidgetEnumerator;
  private
    FAlign: TWidgetAlign;
    FTheme: TTheme;
    FSector: Integer;
    FStayOnTop: Boolean;
    FBounds: TRectF;
    FParent: TWidget;
    FChildren: TWidgetList;
    FComputed: TComputedWidget;
    FHint: string;
    FText: string;
    FName: string;
    FOpacity: Float;
    FEnabled: Boolean;
    FVisible: Boolean;
    FModalResult: TModalResult;
    FUnpacked: Boolean;
    FTag: IntPtr;
    FState: TWidgetState;
    FOnChange: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FOnKeyDown: TKeyboardEvent;
    FOnKeyUp: TKeyboardEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseEvent;
    FOnMouseUp: TMouseEvent;
    FMargin: Float;
    FIndent: Integer;
    FNeedsPack: Boolean;
    procedure ThemeRender;
    procedure ThemeChange;
    function GetMain: TMainWidget;
    function GetContainer: TContainerWidget;
    function GetFirstChild: TWidget;
    function GetLastChild: TWidget;
    function GetDim(Index: Integer): Float;
    procedure SetDim(Index: Integer; const Value: Float);
    procedure SetMargin(Value: Float);
    procedure SetIndent(Value: Integer);
    function GetChildCount: Integer;
    function GetChild(Index: Integer): TWidget;
    procedure SetAlign(Value: TWidgetAlign);
    procedure SetTheme(Value: TTheme);
    procedure SetBounds(const Value: TRectF);
    procedure SetText(const Value: string);
    procedure SetVisible(Value: Boolean);
    procedure SetModalResult(Value: TModalResult);
    procedure SetUnpacked(Value: Boolean);
  protected
    procedure Paint; virtual;
    procedure Resize; virtual;
    procedure Repack; virtual;
    function GetBorders: TRectF; virtual;
    function CanSelect: Boolean; virtual;
    procedure AddState(Part: TWidgetStatePart);
    procedure RemoveState(Part: TWidgetStatePart);
    procedure Change;
    procedure DoModalResult; virtual;
    procedure DoClick; virtual;
    procedure DoKeyDown(var Args: TKeyboardArgs); virtual;
    procedure DoKeyUp(var Args: TKeyboardArgs); virtual;
    procedure DoMouseDown(var Args: TMouseArgs); virtual;
    procedure DoMouseMove(var Args: TMouseArgs); virtual;
    procedure DoMouseUp(var Args: TMouseArgs); virtual;
    { Events }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnKeyDown: TKeyboardEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TKeyboardEvent read FOnKeyUp write FOnKeyUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); virtual;
    destructor Destroy; override;
    { Alias for self }
    function This: TWidget;
    { Add a widget of type T as a child of this widget optinally giving it a name }
    function Add<T: TWidget>(const Name: string = ''): T; overload;
    { Add overload ot type T capturing the new child }
    function Add<T: TWidget>(out Child: T; const Name: string = ''): T; overload;
    { Search for a widget at position x, y }
    function FindWidget(X, Y: Float): TWidget;
    { Search for a widget by name }
    function FindWidget<T: TWidget>(const Name: string): T;
    { Pack arranges child controls returning true if a pack was needed }
    function Pack: Boolean; virtual;
    { Invoke a click on the widget }
    procedure Click;
    { Deletes and destroys the child widget at index }
    procedure Delete(Index: Integer);
    { Returns true if the widget is in the child chain of a parent widget }
    function IsParent(Parent: TWidget): Boolean;
    { If the widget is top level activate it }
    procedure Activate;
    { If the widget is top level bring it to the top of the zorder }
    procedure BringToFront;
    { If the widget is top level send it to the bottom of the zorder }
    procedure SendToBack;
    { Move the widget up or down in zorder }
    procedure ZOrder(Delta: Integer);
    { Calculate a theme color for this widget }
    function Color(Color: TThemeColor): LongWord;
    { The computed properties of the widget }
    property Computed: TComputedWidget read FComputed;
    { Themes can be applied down to the widget level }
    property Theme: TTheme read FTheme write SetTheme;
    { Align is used when packing the widget inside a container }
    property Align: TWidgetAlign read FAlign write SetAlign;
    { If the parent is the main widget, then sector applies a fixed placement }
    property Sector: Integer read FSector write FSector;
    { If the parent is the main widget, then put the wiget above all others }
    property StayOnTop: Boolean read FStayOnTop write FStayOnTop;
    { A rectangle relative to the partent designating size and position of this widget }
    property Bounds: TRectF read FBounds write SetBounds;
    { If hint is set and the main widgets allows hints, then display a tooltip }
    property Hint: string read FHint write FHint;
    { The main widget to which this widget belongs }
    property Main: TMainWidget read GetMain;
    { The immediate parent of this widget }
    property Parent: TWidget read FParent;
    { The top level contaier of this widget }
    property Container: TContainerWidget read GetContainer;
    { The first child of this widget }
    property FirstChild: TWidget read GetFirstChild;
    { The last child of this widget }
    property LastChild: TWidget read GetLastChild;
    { The number of child widgets }
    property ChildCount: Integer read GetChildCount;
    { Access to child widgets }
    property Child[Index: Integer]: TWidget read GetChild;
    { Position and size properties }
    property X: Float index 0 read GetDim write SetDim;
    property Y: Float index 1 read GetDim write SetDim;
    property Width: Float index 2 read GetDim write SetDim;
    property Height: Float index 3 read GetDim write SetDim;
    property Margin: Float read FMargin write SetMargin;
    property Indent: Integer read FIndent write SetIndent;
    { Name can be used to seearch for the widget }
    property Name: string read FName write FName;
    { Opacity of the widget in the range from 0 to 1 }
    property Opacity: Float read FOpacity write FOpacity;
    property Text: string read FText write SetText;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write FEnabled;
    { Singal the parent window with this value if the widget is clicked }
    property ModalResult: TModalResult read FModalResult write SetModalResult;
    { See TWidgetStatePart }
    property State: TWidgetState read FState;
    { When unpacked is true the widget ignores all packing rules }
    property Unpacked: Boolean read FUnpacked write SetUnpacked;
    { User data tag property }
    property Tag: IntPtr read FTag write FTag;
  end;

{ TMainWidget should serve as the base parent widget for your layouts }

  TMainWidget = class(TWidget)
  private
    FShowHint: Boolean;
    FCapture: TWidget;
    FSelected: TWidget;
    FActiveWindow: TWindow;
    FHot: TWidget;
    FTime: Double;
    FHover: Double;
    FModalWindow: TWindow;
    FIsDestroying: Boolean;
    procedure MessageBoxClose(Sender: TObject; ModalResult: TModalResult);
    procedure MessageProxy(Sender: TObject; ModalResult: TModalResult);
    procedure SetActiveWindow(Value: TWindow);
  protected
    procedure SetModal(Window: TWindow);
    procedure UnsetModal(Window: TWindow);
  public
    { The main widget using a theme }
    constructor Create(Theme: TTheme); reintroduce;
    destructor Destroy; override;
    { Modal window message related functions }
    procedure MessageBox(const Message: string);
    procedure MessageConfirm(const Message: string; OnResult: TModalResultEvent);
    procedure MessageDialog(const Title, Glyph, Message: string; Buttons: TModalButtons; OnResult: TModalResultEvent);
    { Commmunicate events with the widgets }
    procedure DispatchKeyDown(var Args: TKeyboardArgs);
    procedure DispatchKeyUp(var Args: TKeyboardArgs);
    procedure DispatchMouseDown(var Args: TMouseArgs);
    procedure DispatchMouseMove(var Args: TMouseArgs);
    procedure DispatchMouseUp(var Args: TMouseArgs);
    { The current active window }
    property ActiveWindow: TWindow read FActiveWindow write SetActiveWindow;
    { The current modal window }
    property ModalWindow: TWindow read FModalWindow;
    { The current selected widget with input focus }
    property Selected: TWidget read FSelected;
    { Render all the widgets }
    procedure Render(Width, Height: Integer; const Time: Double);
    { The time passed in render above }
    property Time: Double read FTime;
    { When show hint is true, hints are displayed if a widget has a hint value }
    property ShowHint: Boolean read FShowHint write FShowHint;
  public
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TSpacer just gives you some empty space }

  TSpacer = class(TWidget)
  end;

{ TEdit not implemented }

  TEdit = class(TWidget)
  protected
    function CanSelect: Boolean; override;
  public
    property Text;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TButton  }

  TButton = class(TWidget)
  private
    FCanToggle: Boolean;
    FDown: Boolean;
    FGroup: Integer;
    procedure SetDown(Value: Boolean);
  protected
    procedure DoClick; override;
  public
    property CanToggle: Boolean read FCanToggle write FCanToggle;
    property Down: Boolean read FDown write SetDown;
    property Group: Integer read FGroup write FGroup;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TPushButton }

  TPushButton = class(TButton)
  protected
    function CanSelect: Boolean; override;
  end;

{ TGlyphButton }

  TGlyphButton = class(TButton)
  end;

{ TGlyphButton }

  TGlyphImage = class(TWidget)
  public
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TCheckBox }

  TCheckBox = class(TWidget)
  private
    FChecked: Boolean;
    FRound: Boolean;
    procedure SetChecked(Value: Boolean);
  protected
    procedure DoClick; override;
    function CanSelect: Boolean; override;
  public
    property Checked: Boolean read FChecked write SetChecked;
    property Round: Boolean read FRound write FRound;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TLabel }

  TLabel = class(TWidget)
  private
    FMaxWidth: Float;
    procedure SetMaxWidth(Value: Float);
  public
    property MaxWidth: Float read FMaxWidth write SetMaxWidth;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TSlider }

  TSlider = class(TWidget)
  private
    FMin: Float;
    FMax: Float;
    FPosition: Float;
    FStep: Float;
    function GetGripRect: TRectF;
    procedure SetMin(Value: Float);
    procedure SetMax(Value: Float);
    procedure SetPosition(Value: Float);
    procedure SetStep(Value: Float);
  protected
    procedure Track(X: Float);
    procedure DoMouseDown(var Args: TMouseArgs); override;
    procedure DoMouseMove(var Args: TMouseArgs); override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    property GripRect: TRectF read GetGripRect;
    property Min: Float read FMin write SetMin;
    property Max: Float read FMax write SetMax;
    property Position: Float read FPosition write SetPosition;
    property Step: Float read FStep write SetStep;
    property OnChange;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TCustomWidget }

  TCustomWidget = class(TWidget)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TContainerWidget }

  TContainerWidget = class(TWidget)
  private
    FFade: Float;
  public
    procedure SelectNext(Dir: Integer = 1);
    property Fade: Float read FFade write FFade;
  end;

{ THBox }

  THBox = class(TContainerWidget)
  public
    function Pack: Boolean; override;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TVBox }

  TVBox = class(TContainerWidget)
  public
    function Pack: Boolean; override;
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

{ TWindow }

  TWindow = class(TContainerWidget)
  private
    FDrag: Boolean;
    FDragX, FDragY: Float;
    FNextModal: TWindow;
    FShowingModal: Boolean;
    FOnModalProxy: TModalResultEvent;
    FOnModalResult: TModalResultEvent;
  protected
    function GetBorders: TRectF; override;
    procedure DoModalResult; override;
    procedure DoMouseDown(var Args: TMouseArgs); override;
    procedure DoMouseMove(var Args: TMouseArgs); override;
    procedure DoMouseUp(var Args: TMouseArgs); override;
  public
    function Pack: Boolean; override;
    procedure Show;
    procedure Hide;
    { Causes a window to be shown above other windows returning the status on close }
    procedure ShowModal(OnModalResult: TModalResultEvent);
    property OnClick;
    property OnKeyDown;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

implementation

{ TComputedWidget }

constructor TComputedWidget.Create(Widget: TWidget);
begin
  inherited Create;
  FWidget := Widget;
end;

function TComputedWidget.Opacity: Float;
var
  W: TWidget;
begin
  Result := FWidget.FOpacity;
  W := FWidget.FParent;
  while W <> nil do
  begin
    if Result = 0 then Exit;
    Result := Result * W.FOpacity;
    W := W.FParent;
  end;
end;

function TComputedWidget.Theme: TTheme;
var
  W: TWidget;
begin
  Result := FWidget.FTheme;
  W := FWidget.FParent;
  while Result = nil do
  begin
    Result := W.Theme;
    W := W.FParent;
  end;
end;

function TComputedWidget.Bounds: TRectF;
var
  W: TWidget;
begin
  Result := FWidget.FBounds;
  W := FWidget.FParent;
  while W <> nil do
  begin
    Result.X := Result.X + W.FBounds.X;
    Result.Y := Result.Y + W.FBounds.Y;
    W := W.FParent;
  end;
end;

function TComputedWidget.Enabled: Boolean;
var
  W: TWidget;
begin
  Result := FWidget.Enabled;
  W := FWidget.FParent;
  while W <> nil do
  begin
    if not Result then Exit;
    Result := Result and W.Enabled;
    W := W.FParent;
  end;
end;

function TComputedWidget.Visible: Boolean;
var
  W: TWidget;
begin
  Result := FWidget.Visible;
  W := FWidget.FParent;
  while W <> nil do
  begin
    if not Result then Exit;
    Result := Result and W.Visible;
    W := W.FParent;
  end;
end;

function TComputedWidget.State: TWidgetState;
begin
  Result := FWidget.FState;
  if not Enabled then
    Include(Result, wsDisabled);
end;

{ TWidget }

function TWidget.GetEnumerator: TWidgetEnumerator;
begin
  Result := FChildren.GetEnumerator;
end;

constructor TWidget.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create;
  FParent := Parent;
  FParent.FChildren.Push(Self);
  FChildren.Length := 0;
  FName := Name;
  FVisible := True;
  FEnabled := True;
  FOpacity := 1;
  FMargin := 8;
  FNeedsPack := True;
  FComputed := TComputedWidget.Create(Self);
  Resize;
end;

destructor TWidget.Destroy;
var
  M: TMainWidget;
  I: Integer;
begin
  if FParent <> nil then
  begin
    I := FParent.FChildren.IndexOf(Self);
    if I > -1 then
      FParent.FChildren.Delete(I);
  end;
  for I := FChildren.Length - 1 downto 0 do
    FChildren[I].Free;
  M := Main;
  if (M <> nil) and (not M.FIsDestroying) then
  begin
    if M.FCapture = Self then
      M.FCapture := nil;
    if M.FSelected = Self then
      M.FSelected := nil;
    if M.FHot = Self then
      M.FHot := nil;
    Repack;
  end;
  FComputed.Free;
  inherited Destroy;
end;

procedure TWidget.Resize;
var
  P: TSizeF;
begin
  P := Main.Theme.CalcSize(Self, tpEverything);
  if (P.X > 0) and (P.Y > 0) then
  begin
    FBounds.Width := P.X;
    FBounds.Height := P.Y;
  end;
  Repack;
end;

procedure TWidget.Repack;
var
  P: TWidget;
begin
  if FChildren.Length > 0 then
    FNeedsPack := True;
  P := Parent;
  while P <> nil do
  begin
    if P is TMainWidget then
      Break;
    P.Repack;
    P := P.Parent;
  end;
end;

procedure TWidget.ThemeRender;
var
  W: TWidget;
begin
  if Opacity = 0 then Exit;
  if not Visible then Exit;
  if Self is TMainWidget then Exit;
  Pack;
  FNeedsPack := False;
  if Parent is TMainWidget then
    case Sector of
      1:
        begin
          FBounds.X := Margin;
          FBounds.Y := Margin;
        end;
      2:
        begin
          FBounds.X := (Parent.Width - Width) / 2;
          FBounds.Y := Margin;
        end;
      3:
        begin
          FBounds.X := Parent.Width - Width - Margin;
          FBounds.Y := Margin;
        end;
      4:
        begin
          FBounds.X := Margin;
          FBounds.Y := (Parent.Height - Height) / 2;
        end;
      5:
        begin
          FBounds.X := (Parent.Width - Width) / 2;
          FBounds.Y := (Parent.Height - Height) / 2;
        end;
      6:
        begin
          FBounds.X := Parent.Width - Width - Margin;
          FBounds.Y := (Parent.Height - Height) / 2;
        end;
      7:
        begin
          FBounds.X := Margin;
          FBounds.Y := Parent.Height - Height - Margin;
        end;
      8:
        begin
          FBounds.X := (Parent.Width - Width) / 2;
          FBounds.Y := Parent.Height - Height - Margin;
        end;
      9:
        begin
          FBounds.X := Parent.Width - Width - Margin;
          FBounds.Y := Parent.Height - Height - Margin;
        end;
    end;
  Computed.Theme.Render(Self);
  Paint;
  for W in FChildren do
    W.ThemeRender;
end;

procedure TWidget.Paint;
begin
end;

function TWidget.CanSelect: Boolean;
begin
  Result := False; //Computed.Enabled and Computed.Visible;
end;

function TWidget.GetBorders: TRectF;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.Width := 0;
  Result.Height := 0;
end;

procedure TWidget.AddState(Part: TWidgetStatePart);
begin
  Include(FState, Part);
end;

procedure TWidget.RemoveState(Part: TWidgetStatePart);
begin
  Exclude(FState, Part);
end;

procedure TWidget.Change;
begin
  if Assigned(OnChange) then
    FOnChange(Self);
end;

procedure TWidget.DoModalResult;
begin
end;

procedure TWidget.DoClick;
var
  W: TWidget;
begin
  if ModalResult > modalNone then
  begin
    W := Parent;
    while W <> nil do
      if W is TWindow then
      begin
        W.ModalResult := ModalResult;
        Break;
      end
      else
        W := W.Parent;
  end;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TWidget.DoKeyDown(var Args: TKeyboardArgs);
var
  C: TContainerWidget;
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Args);
  if Args.Handled then
    Exit;
  C := Container;
  if C = nil then
    Exit;
  if Args.Key = keyTab then
    if ssShift in Args.Shift then
      C.SelectNext(-1)
    else
      C.SelectNext(1)
  else if Args.Key = keyLeft then
    C.SelectNext(-1)
  else if Args.Key = keyRight then
    C.SelectNext(1)
  else if Args.Key = keyUp then
    C.SelectNext(-1)
  else if Args.Key = keyDown then
    C.SelectNext(1);
end;

procedure TWidget.DoKeyUp(var Args: TKeyboardArgs);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Args);
  if Args.Handled then
    Exit;
  if Args.Key = keyReturn then
    Click
  else if Args.Key = keySpace then
    Click;
end;

procedure TWidget.DoMouseDown(var Args: TMouseArgs);
begin
  if Args.Button = mbLeft then
    Activate;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Args);
end;

procedure TWidget.DoMouseMove(var Args: TMouseArgs);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Args);
end;

procedure TWidget.DoMouseUp(var Args: TMouseArgs);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Args);
end;

procedure TWidget.Click;
begin
  DoClick;
end;

function TWidget.This: TWidget;
begin
  Result := Self;
end;

function TWidget.Add<T>(const Name: string): T;
begin
  Result := T.Create(Self, Name);
end;

function TWidget.Add<T>(out Child: T; const Name: string = ''): T;
begin
  Child := T.Create(Self, Name);
  Result := Child;
end;

function TWidget.FindWidget(X, Y: Float): TWidget;
var
  W: TWidget;
  I: Integer;
begin
  Result := nil;
  if not FComputed.Visible then
    Exit;
  if not FComputed.Enabled then
    Exit;
  if Self <> Main then
    if Main.FModalWindow <> nil then
      if Self <> Main.FModalWindow then
        if not IsParent(Main.FModalWindow) then
          Exit;
  if FChildren.Length > 0 then
    for I := FChildren.Length - 1 downto 0 do
    begin
      W := FChildren[I];
      if not W.FStayOnTop then
        Continue;
      Result := W.FindWidget(X, Y);
      if Result <> nil then
        Exit;
    end;
  if FChildren.Length > 0 then
    for I := FChildren.Length - 1 downto 0 do
    begin
      W := FChildren[I];
      if W.FStayOnTop then
        Continue;
      Result := W.FindWidget(X, Y);
      if Result <> nil then
        Exit;
    end;
  if (not (Self is TMainWidget)) and FComputed.Bounds.Contains(X, Y) then
    Result := Self;
end;

function TWidget.FindWidget<T>(const Name: string): T;
var
  W: TWidget;
begin
  Result := nil;
  if FChildren.Length > 0 then
    for W in FChildren do
    begin
      Result := W.FindWidget<T>(Name);
      if Result <> nil then
        Exit;
    end;
  if (Self.Name = Name) and (Self is T) then
    Result := T(Self);
end;

function TWidget.Pack: Boolean;
var
  W: TWidget;
begin
  Result := FNeedsPack;
  if not Result then
    Exit;
  FNeedsPack := False;
  if FChildren.Length > 1 then
    for W in Self do
      W.Pack;
end;

procedure TWidget.Delete(Index: Integer);
begin
  if Index < 0 then Exit;
  if Index > FChildren.Length - 1 then Exit;
  FChildren[Index].Free;
end;

function TWidget.IsParent(Parent: TWidget): Boolean;
var
  W: TWidget;
begin
  W := Self.Parent;
  while W <> nil do
    if W = Parent then
      Exit(True)
    else
      W := W.Parent;
  Result := False;
end;

procedure TWidget.Activate;
var
  M: TMainWidget;

  function Select(W: TWidget): Boolean;
  var
    C: TWidget;
  begin
    if W.CanSelect then
    begin
      if M.FSelected <> nil then
        M.FSelected.RemoveState(wsSelected);
      M.FSelected := W;
      W.AddState(wsSelected);
      Result := True;
    end
    else for C in W do
      if Select(C) then
        Break;
  end;

  procedure ContainerActivate(C: TWidget);
  begin
    if C.Parent = nil then
      Exit;
    C.Visible := True;
    if C.Parent = M then
      if C is TWindow then
      begin
        C.BringToFront;
        if M.ActiveWindow = TWindow(C) then
          Exit;
        M.ActiveWindow := TWindow(C);
        if M.FSelected <> nil then
          if M.FSelected.IsParent(C) then
            Exit;
        Select(C);
      end
      else
        C.BringToFront
    else
      ContainerActivate(C.Parent)
  end;

begin
  M := Main;
  ContainerActivate(Self);
  if CanSelect then
    Select(Self);
end;

procedure TWidget.BringToFront;
begin
  if Parent = Main then
    ZOrder(Parent.ChildCount)
  else
    Parent.BringToFront;
end;

procedure TWidget.SendToBack;
begin
  if Parent = Main then
    ZOrder(-Parent.ChildCount)
  else
    Parent.SendToBack;
end;

procedure TWidget.ZOrder(Delta: Integer);
var
  M, I: Integer;
begin
  if FParent = nil then Exit;
  if Delta < 0 then
  begin
    I := FParent.FChildren.IndexOf(Self);
    while (I > 0) and (Delta < 0) do
    begin
      FParent.FChildren.Exchange(I, I - 1);
      Dec(I);
      Inc(Delta);
    end;
    Repack;
  end
  else if Delta > 0 then
  begin
    M := FParent.FChildren.Length - 1;
    I := FParent.FChildren.IndexOf(Self);
    while (I < M) and (Delta > 0) do
    begin
      FParent.FChildren.Exchange(I, I + 1);
      Inc(I);
      Dec(Delta);
    end;
    Repack;
  end;
end;

function TWidget.Color(Color: TThemeColor): LongWord;
begin
  Result := Main.Theme.CalcColor(Self, Color);
end;

function TWidget.GetMain: TMainWidget;
var
  W: TWidget;
begin
  Result := nil;
  W := Self;
  while W <> nil do
  begin
    if W is TMainWidget then
      Exit(TMainWidget(W));
    W := W.FParent;
  end;
end;

function TWidget.GetContainer: TContainerWidget;
var
  W: TWidget;
begin
  Result := nil;
  W := Self;
  while W <> nil do
  begin
    if (W is TContainerWidget) and (W.FParent is TMainWidget) then
      Exit(TContainerWidget(W));
    W := W.FParent;
  end;
end;

function TWidget.GetFirstChild: TWidget;
begin
  if FChildren.Length > 0 then
    Result := FChildren.First
  else
    Result := nil;
end;

function TWidget.GetLastChild: TWidget;
begin
  if FChildren.Length > 0 then
    Result := FChildren.Last
  else
    Result := nil;
end;

function TWidget.GetDim(Index: Integer): Float;
begin
  case Index of
    0: Result := FBounds.X;
    1: Result := FBounds.Y;
    2: Result := FBounds.Width;
    3: Result := FBounds.Height;
  else
    Result := 0;
  end;
end;

procedure TWidget.SetDim(Index: Integer; const Value: Float);
begin
  if Self is TMainWidget then Exit;
  if Value = GetDim(Index) then Exit;
  case Index of
    0: FBounds.X := Value;
    1: FBounds.Y := Value;
    2:
      begin
        FBounds.Width := Value;
        Repack;
      end;
    3:
      begin
        FBounds.Height := Value;
        Repack;
      end;
  end;
end;

procedure TWidget.SetMargin(Value: Float);
begin
  if Value = FMargin then Exit;
  FMargin := Value;
  Repack;
end;

procedure TWidget.SetIndent(Value: Integer);
begin
  if Value = FIndent then Exit;
  FIndent := Value;
  Repack;
end;

function TWidget.GetChildCount: Integer;
begin
  Result := FChildren.Length;
end;

function TWidget.GetChild(Index: Integer): TWidget;
begin
  Result := FChildren[Index];
end;

procedure TWidget.SetAlign(Value: TWidgetAlign);
begin
  if Value = FAlign then Exit;
  FAlign := Value;
  Repack;
end;

procedure TWidget.ThemeChange;
var
  W: TWidget;
begin
  Resize;
  for W in Self do
    W.Resize;
end;

procedure TWidget.SetTheme(Value: TTheme);
begin
  if Value = FTheme then Exit;
  FTheme := Value;
  ThemeChange;
end;

procedure TWidget.SetBounds(const Value: TRectF);
begin
  FBounds := Value;
  Repack;
end;

procedure TWidget.SetText(const Value: string);
begin
  if Value = FText then Exit;
  FText := Value;
  Resize;
end;

procedure TWidget.SetVisible(Value: Boolean);
var
  W: TWindow;
  C: TWidget;
  I: Integer;
begin
  if Value = FVisible then Exit;
  FVisible := Value;
  Repack;
  if Parent <> Main then
    Exit;
  if not (Self is TWindow) then
    Exit;
  if Main.ModalWindow <> nil then
  begin
    Main.ModalWindow.Activate;
    Exit;
  end;
  W := TWindow(Self);
  if FVisible and (Main.ActiveWindow = nil) then
    W.Activate
  else if (not FVisible) and (Main.ActiveWindow = W) then
  begin
    Main.ActiveWindow := nil;
    for I := Parent.FChildren.Length - 1 downto 0 do
    begin
      C := Parent.FChildren[I];
      if (C is TWindow) and C.Enabled and C.Visible then
      begin
        C.Activate;
        Break;
      end;
    end;
  end;
end;

procedure TWidget.SetModalResult(Value: TModalResult);
begin
  if Value = FModalResult then Exit;
  FModalResult := Value;
  if FModalResult > modalNone then
    DoModalResult;
end;

procedure TWidget.SetUnpacked(Value: Boolean);
begin
  if Value = FUnpacked then Exit;
  FUnpacked := Value;
  Repack;
end;

{ TMainWidget }

constructor TMainWidget.Create(Theme: TTheme);
begin
  FTheme := Theme;
  FShowHint := True;
  FVisible := True;
  FEnabled := True;
  FOpacity := 1;
  FComputed := TComputedWidget.Create(Self);
end;

destructor TMainWidget.Destroy;
begin
  FIsDestroying := True;
  inherited Destroy;
end;

procedure TMainWidget.MessageBoxClose(Sender: TObject; ModalResult: TModalResult);
begin
  Sender.Free;
end;

procedure TMainWidget.MessageProxy(Sender: TObject; ModalResult: TModalResult);
var
  Proxy: TModalResultEvent;
begin
  Proxy := (Sender as TWindow).FOnModalProxy;
  Sender.Free;
  if Assigned(Proxy) then
    Proxy(Self, ModalResult);
end;

procedure TMainWidget.MessageBox(const Message: string);
begin
  with (Add<TWindow>) do
  begin
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      with (This.Add<TGlyphImage>) do
      begin
        Align := alignCenter;
        Text := '';
      end;
      with (This.Add<TLabel>) do
      begin
        Align := alignCenter;
        Margin := 20;
        MaxWidth := 400;
        Text := Message;
      end;
    end;
    with (This.Add<TPushButton>) do
    begin
      Align := alignCenter;
      Text := 'OK';
      ModalResult := modalOk;
    end;
    Text := 'Message';
    Pack;
    X := (Self.Width - This.Width) / 2;
    Y := (Self.Height - This.Height) / 2;
    ShowModal(MessageBoxClose);
  end;
end;

procedure TMainWidget.MessageConfirm(const Message: string; OnResult: TModalResultEvent);
begin
  with (Add<TWindow>) do
  begin
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      with (This.Add<TGlyphImage>) do
      begin
        Align := alignCenter;
        Text := '󰠗';
      end;
      with (This.Add<TLabel>) do
      begin
        Align := alignCenter;
        Margin := 20;
        MaxWidth := 400;
        Text := Message;
      end;
    end;
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      with (This.Add<TPushButton>) do
      begin
        Text := 'Yes';
        ModalResult := modalYes;
      end;
      with (This.Add<TPushButton>) do
      begin
        Text := 'No';
        ModalResult := modalNo;
      end;
    end;
    Text := 'Confirmation';
    Pack;
    X := (Self.Width - This.Width) / 2;
    Y := (Self.Height - This.Height) / 2;
    FOnModalProxy := OnResult;
    ShowModal(MessageProxy);
  end;
end;

procedure TMainWidget.MessageDialog(const Title, Glyph, Message: string;
  Buttons: TModalButtons; OnResult: TModalResultEvent);
var
  B: TModalButton;
begin
  if Buttons = [] then
    Buttons := [mbOkay];
  with (Add<TWindow>) do
  begin
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      if Glyph <> '' then
        with (This.Add<TGlyphImage>) do
        begin
          Align := alignCenter;
          Text := Glyph;
        end;
      with (This.Add<TLabel>) do
      begin
        Align := alignCenter;
        Margin := 20;
        MaxWidth := 400;
        Text := Message;
      end;
    end;
    with (This.Add<THBox>) do
    begin
      Align := alignCenter;
      for B := Low(TModalButton) to High(TModalButton) do
        if B in Buttons then
          case B of
            mbOkay:
              with (This.Add<TPushButton>) do
              begin
                Text := 'OK';
                ModalResult := modalok;
              end;
            mbYes:
              with (This.Add<TPushButton>) do
              begin
                Text := 'Yes';
                ModalResult := modalYes;
              end;
            mbNo:
              with (This.Add<TPushButton>) do
              begin
                Text := 'No';
                ModalResult := modalNo;
              end;
            mbAccept:
              with (This.Add<TPushButton>) do
              begin
                Text := 'Accept';
                ModalResult := modalAccept;
              end;
            mbCancel:
              with (This.Add<TPushButton>) do
              begin
                Text := 'Cancel';
                ModalResult := modalCancel;
              end;
          end;
    end;
    Text := Title;
    Pack;
    X := (Self.Width - This.Width) / 2;
    Y := (Self.Height - This.Height) / 2;
    FOnModalProxy := OnResult;
    ShowModal(MessageProxy);
  end;
end;

procedure TMainWidget.SetModal(Window: TWindow);
begin
  Window.FShowingModal := True;
  Window.FNextModal := FModalWindow;
  Window.Activate;
  FModalWindow := Window;
end;

procedure TMainWidget.UnsetModal(Window: TWindow);
begin
  Window.FShowingModal := False;
  FModalWindow := Window.FNextModal;
  Window.Visible := False;
end;

procedure TMainWidget.SetActiveWindow(Value: TWindow);
begin
  if Value = FActiveWindow then
    Exit;
  if Value <> nil then
    if Value.Parent <> Self then
      Exit
    else
    begin
      Value.FVisible := True;
      Value.FEnabled := True;
    end;
  FActiveWindow := Value;
  if (FSelected <> FActiveWindow) and (FSelected <> nil) then
  begin
    FSelected.RemoveState(wsSelected);
    FSelected := nil;
  end;
  if (FHot <> FActiveWindow) and (FHot <> nil) then
  begin
    FHot.RemoveState(wsHot);
    FHot := nil;
  end;
  if (FCapture <> FActiveWindow) and (FCapture <> nil) then
  begin
    FCapture.RemoveState(wsPressed);
    FCapture := nil;
  end;
  if (FSelected <> FActiveWindow) and (FSelected <> nil) then
  begin
    FSelected.RemoveState(wsPressed);
    FSelected := nil;
  end;
  if FActiveWindow <> nil then
    FActiveWindow.BringToFront;
end;

procedure TMainWidget.DispatchKeyDown(var Args: TKeyboardArgs);
begin
  DoKeyDown(Args);
  if Args.Handled then Exit;
  if FSelected <> nil then
    FSelected.DoKeyDown(Args);
end;

procedure TMainWidget.DispatchKeyUp(var Args: TKeyboardArgs);
begin
  DoKeyUp(Args);
  if Args.Handled then Exit;
  if FSelected <> nil then
    FSelected.DoKeyUp(Args);
end;

procedure TMainWidget.DispatchMouseDown(var Args: TMouseArgs);
var
  X0, Y0: Float;
  C, S, H: TWidget;
  R: TRectF;
begin
  X0 := Args.X;
  Y0 := Args.Y;
  try
    DoMouseDown(Args);
    if Args.Handled then Exit;
    C := nil;
    S := nil;
    H := FindWidget(Args.X, Args.Y);
    if Args.Button = mbLeft then
    begin
      C := H;
      if (C <> nil) and (C.CanSelect) then
        S := C;
      if S <> nil then
      begin
        if FSelected <> nil then
          FSelected.RemoveState(wsSelected);
        FSelected := S;
        FSelected.AddState(wsSelected);
      end;
    end;
    if FCapture <> nil then
      FCapture.RemoveState(wsPressed);
    if FHot <> nil then
      FHot.RemoveState(wsHot);
    FCapture := C;
    FHot := H;
    if FHot <> nil then
    begin
      R := H.FComputed.Bounds;
      Args.X := Args.X - R.X;
      Args.Y := Args.Y - R.Y;
      H.DoMouseDown(Args);
      FHot := H;
      FHot.AddState(wsHot);
    end;
    FCapture := C;
    if FCapture <> nil then
    begin
      Args.Handled := True;
      FCapture.AddState(wsPressed);
    end;
    if (FActiveWindow <> nil) and (S <> nil) and S.IsParent(FActiveWindow) then
    begin
      if FSelected <> nil then
        FSelected.RemoveState(wsSelected);
      FSelected := S;
      FSelected.AddState(wsSelected);
    end;
  finally
    Args.X := X0;
    Args.Y := Y0;
  end;
end;

procedure TMainWidget.DispatchMouseMove(var Args: TMouseArgs);
var
  X0, Y0: Float;
  H: TWidget;
  R: TRectF;
begin
  X0 := Args.X;
  Y0 := Args.Y;
  try
    DoMouseMove(Args);
    if Args.Handled then Exit;
    FHover := Time;
    if FCapture <> nil then
    begin
      Args.Handled := True;
      R := FCapture.FComputed.Bounds;
      H := FindWidget(Args.X, Args.Y);
      if H <> FCapture then
      begin
        if FHot <> nil then
          FHot.RemoveState(wsHot);
        FHot := nil;
      end
      else
      begin
        FHot := FCapture;
        FCapture.AddState(wsHot);
      end;
      Args.X := Args.X - R.X;
      Args.Y := Args.Y - R.Y;
      FCapture.DoMouseMove(Args);
      Exit;
    end;
    H := FindWidget(Args.X, Args.Y);
    if FHot <> nil then
      FHot.RemoveState(wsHot);
    FHot := H;
    if FHot <> nil then
      FHot.AddState(wsHot);
    if H <> nil then
    begin
      R := H.FComputed.Bounds;
      Args.X := Args.X - R.X;
      Args.Y := Args.Y - R.Y;
      H.DoMouseMove(Args);
    end;
  finally
    Args.X := X0;
    Args.Y := Y0;
  end;
end;

procedure TMainWidget.DispatchMouseUp(var Args: TMouseArgs);
var
  X0, Y0: Float;
  H, C: TWidget;
  R: TRectF;
begin
  X0 := Args.X;
  Y0 := Args.Y;
  try
    DoMouseUp(Args);
    if Args.Handled then Exit;
    H := FindWidget(Args.X, Args.Y);
    if FHot <> nil then
      FHot.RemoveState(wsHot);
    FHot := H;
    if FHot <> nil then
      FHot.AddState(wsHot);
    if (Args.Button = mbLeft) and (FCapture <> nil) then
    begin
      Args.Handled := True;
      C := FCapture;
      FCapture := nil;
      C.RemoveState(wsPressed);
      R := C.FComputed.Bounds;
      Args.X := Args.X - R.X;
      Args.Y := Args.Y - R.Y;
      C.DoMouseUp(Args);
      if C = H then
        C.Click;
    end
    else if H <> nil then
    begin
      R := H.FComputed.Bounds;
      Args.X := Args.X - R.X;
      Args.Y := Args.Y - R.Y;
      H.DoMouseUp(Args);
    end;
  finally
    Args.X := X0;
    Args.Y := Y0;
  end;
end;

procedure TMainWidget.Render(Width, Height: Integer; const Time: Double);
const
  HintTime = 0.5;
var
  W: TWidget;
  H: Double;
begin
  FBounds.X := 0;
  FBounds.Y := 0;
  FBounds.Width := Width;
  FBounds.Height := Height;
  FTime := Time;
  if FOpacity = 0 then Exit;
  if not FVisible then Exit;
  for W in FChildren do
    if (not W.FStayOnTop) and (W <> FModalWindow) then
      W.ThemeRender;
  for W in FChildren do
    if W.FStayOnTop  and (W <> FModalWindow) then
      W.ThemeRender;
  for W in FChildren do
    if W = FModalWindow then
      W.ThemeRender;
  if (not ShowHint) or (FHot = nil) then
    Exit;
  H := Time - FHover;
  if H < HintTime then
    Exit;
  H := (H - HintTime) / 0.3;
  if H > 1 then
    H := 1;
  if FHot.Hint <> '' then
    FHot.Computed.Theme.RenderHint(FHot, H);
end;

{ TEdit }

function TEdit.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

{ TButton }

procedure TButton.DoClick;
begin
  if FCanToggle then
    if FGroup > 0 then
      Down := True
    else
      Down := not Down;
  inherited DoClick;
end;

procedure TButton.SetDown(Value: Boolean);
var
  W: TWidget;
  B: TGlyphButton absolute W;
begin
  if Value = FDown then Exit;
  FDown := Value;
  if FDown then
  begin
    AddState(wsToggled);
    if FGroup > 0 then
      for W in Parent do
        if W = Self then
          Continue
        else if (W is TButton) and B.CanToggle and (B.Group = Group) then
          B.Down := False;
  end
  else
    RemoveState(wsToggled);
  Change;
end;

{ TPushButton }

function TPushButton.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

{ TCheckBox }

function TCheckBox.CanSelect: Boolean;
begin
  Result := Computed.Enabled and Computed.Visible;
end;

procedure TCheckBox.DoClick;
begin
  Checked := not Checked;
  inherited DoClick;
end;

procedure TCheckBox.SetChecked(Value: Boolean);
begin
  if Value = FChecked then Exit;
  FChecked := Value;
  if FChecked then
    AddState(wsToggled)
  else
    RemoveState(wsToggled);
  Change;
end;

{ TLabel }

procedure TLabel.SetMaxWidth(Value: Float);
begin
  if FMaxWidth = Value then Exit;
  FMaxWidth := Value;
  Resize;
end;

{ TSlider }

constructor TSlider.Create(Parent: TWidget; const Name: string);
begin
  inherited Create(Parent, Name);
  FMin := 0;
  FMax := 100;
  FStep := 1;
end;

procedure TSlider.Track(X: Float);
var
  S: TSizeF;
  H, P: Float;
begin
  if Width < 1 then
    Exit;
  S := Main.Theme.CalcSize(Self, tpThumb);
  H := S.X / 2;
  if X < H then
    Position := FMin
  else if X > Width - H then
    Position := FMax
  else
  begin
    X := X - H;
    P := X / (Width - S.X);
    P := FMin + P * (FMax - FMin);
    Position := P;
  end;
end;

procedure TSlider.DoMouseDown(var Args: TMouseArgs);
begin
  inherited DoMouseDown(Args);
  if wsPressed in State then
    Track(Args.X);
end;

procedure TSlider.DoMouseMove(var Args: TMouseArgs);
begin
  inherited DoMouseMove(Args);
  if wsPressed in State then
    Track(Args.X);
end;

function TSlider.GetGripRect: TRectF;
var
  S: TSizeF;
  P: Float;
begin
  S := Main.Theme.CalcSize(Self, tpThumb);
  P := (Position - FMin) / (FMax - FMin);
  P := Round(P * (Width - S.X));
  Result.X := P;
  Result.Y := Height / 2 - S.Y / 2;
  Result.Width := S.X;
  Result.Height := S.Y;
end;

procedure TSlider.SetMin(Value: Float);
begin
  FMin := Value;
  SetPosition(FPosition);
end;

procedure TSlider.SetMax(Value: Float);
begin
  FMax := Value;
  SetPosition(FPosition);
end;

procedure TSlider.SetPosition(Value: Float);
var
  V: Double;
begin
  if Value <= FMin then
    Value := FMin
  else if Value >= FMax then
    Value := FMax
  else if FStep > 0 then
  begin
    V := Divide(Value, FStep);
    if Abs(Value - V) > 0.01 then
      Value := V;
  end;
  if Value = FPosition then
    Exit;
  FPosition := Value;
  Change;
end;

procedure TSlider.SetStep(Value: Float);
begin
  if Value < 0 then
    Exit;
  FStep := Value;
end;

{ TCustomWidget }

procedure TCustomWidget.Paint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

{ TContainerWidget }

var
  SelectStack: TArrayList<TWidget>;

procedure TContainerWidget.SelectNext(Dir: Integer = 1);
var
  S: TWidget;
  I, J: Integer;

  procedure Add(W: TWidget);
  var
    C: TWidget;
  begin
    for C in W do
    begin
      if C.CanSelect then
      begin
        SelectStack[I] := C;
        if wsSelected in C.State then
          S := C;
        Inc(I);
      end;
      Add(C);
    end;
  end;

begin
  if SelectStack.Length < 1000 then
    SelectStack.Length := 1000;
  S := nil;
  I := 0;
  if Dir < 0 then
    Dir := -1
  else
    Dir := 1;
  Add(Self);
  if I < 2 then
    Exit;
  if S = nil then
  begin
    SelectStack[0].Activate;
    Exit;
  end;
  J := SelectStack.IndexOf(S) + Dir;
  if J < 0 then
    J := I - 1
  else if J = I then
    J := 0;
  SelectStack[J].Activate;
end;

{ THBox }

function THBox.Pack: Boolean;
var
  Child, Last: TWidget;
  S: TSizeF;
  H, X, Y: Float;
  A, B: Float;
begin
  Result := inherited Pack;
  if not Result then
    Exit;
  S := Main.Theme.CalcSize(Self, tpEverything);
  H := S.Y;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    if Child.FNeedsPack then
      Child.Pack;
  end;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    Y := Child.Margin * 2 + Child.Height;
    if Y > H then
      H := Y;
  end;
  X := 0;
  A := 0;
  B := 0;
  Last := nil;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    B := Child.Margin;
    if B > A then
      X := X + B
    else
      X := X + A;
    A := Child.Margin;
    Child.X := X;
    case Child.Align of
      alignNear: Child.Y := Child.Margin;
      alignCenter: Child.Y := (H - Child.Height) / 2;
      alignFar: Child.Y := H - Child.Height - Child.Margin;
    end;
    X := X + Child.Width;
    Last := Child;
  end;
  if Last <> nil then
    X := X + LastChild.Margin;
  if X < S.X then
    X := S.X;
  FBounds.Width := X;
  FBounds.Height := H;
end;

function TVBox.Pack: Boolean;
var
  Child, Last: TWidget;
  S: TSizeF;
  W, X, Y: Float;
  A, B: Float;
begin
  Result := inherited Pack;
  if not Result then
    Exit;
  S := Main.Theme.CalcSize(Self, tpEverything);
  W := S.X;
  X := 0;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    if Child.FNeedsPack then
      Child.Pack;
  end;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    X := Child.Margin * 2 + Child.Width;
    if X > W then
      W := X;
  end;
  Y := 0;
  A := 0;
  B := 0;
  Last := nil;
  for Child in Self do
  begin
    if Child.Unpacked then
      Continue;
    if not Child.Computed.Visible then
      Continue;
    B := Child.Margin;
    if B > A then
      Y := Y + B
    else
      Y := Y + A;
    A := Child.Margin;
    Child.Y := Y;
    case Child.Align of
      alignNear: Child.X := Child.Margin;
      alignCenter: Child.X := (W - Child.Width) / 2;
      alignFar: Child.X := W - Child.Width - Child.Margin;
    end;
    Y := Y + Child.Height;
    Last := Child;
  end;
  if Last <> nil then
    Y := Y + Last.Margin;
  if Y < S.Y then
    Y := S.Y;
  FBounds.Width := W;
  FBounds.Height := Y;
end;

{ TWindow }

function TWindow.GetBorders: TRectF;
begin
  Result.Clear;
  Result.Top := Main.Theme.CalcSize(Self, tpCaption).Y;
end;

function TWindow.Pack: Boolean;
var
  Y: Float;
  I: Float;
  W: TWidget;
  A, B: Float;
begin
  Result := inherited Pack;
  if not Result then
    Exit;
  Y := GetBorders.Top;
  I := Main.Theme.CalcSize(Self, tpIndent).X;
  A := 100;
  B := 100;
  for W in Self do
  begin
    if W.Unpacked then
      Continue;
    if not W.Computed.Visible then
      Continue;
    B := W.Width + W.Margin * 2 + W.Indent * I;
    if B > A then
      A := B;
  end;
  FBounds.Width := A;
  A := 0;
  B := 0;
  for W in Self do
  begin
    if W.Unpacked then
      Continue;
    if not W.Computed.Visible then
      Continue;
    case W.Align of
      alignNear: W.X := W.Margin + I * W.Indent;
      alignCenter: W.X := (Width - W.Width) / 2;
      alignFar: W.X := Width - W.Width - W.Margin - I * W.Indent;
    end;
    B := W.Margin;
    if B > A then
      Y := Y + B
    else
      Y := Y + A;
    A := W.Margin;
    W.Y := Y;
    Y := Y + W.Height;
  end;
  Height := Y + B;
end;

procedure TWindow.DoModalResult;
var
  R: TModalResult;
begin
  R := ModalResult;
  FModalResult := modalNone;
  if FShowingModal then
  begin
    Main.UnsetModal(Self);
    if Assigned(FOnModalResult) then
      FOnModalResult(Self, R);
  end;
end;

procedure TWindow.Show;
begin
  Visible := True;
end;

procedure TWindow.Hide;
begin
  if FShowingModal then
    ModalResult := modalCancel
  else
    Visible := False;
end;

procedure TWindow.ShowModal(OnModalResult: TModalResultEvent);
begin
  if FShowingModal then
    Exit;
  FModalResult := modalNone;
  FOnModalResult := OnModalResult;
  Main.SetModal(Self);
end;

procedure TWindow.DoMouseDown(var Args: TMouseArgs);
begin
  inherited DoMouseDown(Args);
  if Args.Button = mbLeft then
    if Args.Y < Main.Theme.CalcSize(Self, tpCaption).Y then
    begin
      FDrag := True;
      FDragX := Args.X;
      FDragY := Args.Y;
    end;
end;

procedure TWindow.DoMouseMove(var Args: TMouseArgs);
begin
  inherited DoMouseMove(Args);
  if FDrag then
  begin
    FBounds.X := FBounds.X + Args.X - FDragX;
    FBounds.Y := FBounds.Y + Args.Y - FDragY;
  end;
end;

procedure TWindow.DoMouseUp(var Args: TMouseArgs);
begin
  inherited DoMouseUp(Args);
  if Args.Button = mbLeft then
    FDrag := False;
end;

function CompareWidgets(const A, B: TWidget): IntPtr;
begin
  Result := IntPtr(A) - IntPtr(B);
end;

initialization
  TWidgetList.DefaultCompare := CompareWidgets;
end.

