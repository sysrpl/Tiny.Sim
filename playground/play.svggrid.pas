unit Play.SvgGrid;

{$mode delphi}
{$WARN 6060 off : }
{$define animation}

interface

uses
  Tiny.System,
  Tiny.Types,
  {$ifdef animation}
  Tiny.Animation,
  {$endif}
  Tiny.Widgets,
  Tiny.Widgets.Custom,
  Tiny.Widgets.Themes,
  Tiny.Application,
  Tiny.Graphics,
  Play.Svg,
  Play.SvgRender;

type
  TSvgGrid = class;

{ TSvgGridThread }

  TSvgGridThread = class(TThread)
  protected
    function Execute: LongWord; override;
  public
    Grid: TSvgGrid;
    FileCount: Integer;
    constructor Create(AGrid: TSvgGrid);
  end;

{ TSvgGrid }

  TSvgRenderItems = TArrayList<TSvgRender>;

  TSvgGrid = class(TWidgetScene)
  private
    Thread: TSvgGridThread;
    Themes: array[0..2] of TTheme;
    Font: IFont;
    Brush: IBitmapBrush;
    Title: ILinearGradientBrush;
    Files: StringArray;
    FileIndex: Integer;
    RenderItems: TSvgRenderItems;
    Window: TWindow;
    Info: string;
    InfoLabel: TLabel;
    ScaleLabel: TLabel;
    ScaleSlider: TSlider;
    RenderStrokesBox: TCheckBox;
    RenderOutlinesBox: TCheckBox;
    RenderNodesBox: TCheckBox;
    RenderBackgroundBox: TCheckBox;
    RenderAnimateBox: TCheckBox;
    Graph: TPerformanceGraph;
    {$ifdef animation}
    Skew: TVec1Prop;
    Scale: TVec1Prop;
    Rotation: TVec1Prop;
    {$else}
    Skew: Float;
    Scale: Float;
    Rotation: Float;
    {$endif}
    procedure GenerateWidgets;
    procedure SelectDocument(Index: Integer);
    procedure IconChange(Sender: TObject);
    procedure SliderChange(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure ReloadClick(Sender: TObject);
  protected
    function WantKeys: Boolean; override;
    procedure DoKeyDown(var Args: TKeyboardArgs); override;
    procedure Load; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
    procedure Unload; override;
  end;

implementation

{ TSvgGridThread }

constructor TSvgGridThread.Create(AGrid: TSvgGrid);
begin
  Grid := AGrid;
  inherited Create;
end;

function TSvgGridThread.Execute: LongWord;
var
  I: Integer;
begin
  for I := Grid.RenderItems.Lo to Grid.RenderItems.Hi do
  begin
    if Terminated then
      Break;
    Grid.RenderItems[I] := TSvgRender.Create(Grid.Canvas);
    Grid.RenderItems[I].ParseFile(Grid.Files[I]);
    InterlockedIncrement(FileCount);
  end;
  Result := 0;
end;

{ TSvgGrid }

procedure TSvgGrid.SelectDocument(Index: Integer);
var
  Render: TSvgRender;
  R: TRectF;
  S: string;
begin
  if Index < 0 then
    Index := Thread.FileCount - 1
  else if Index > Thread.FileCount - 1 then
    Index := 0;
  FileIndex := Index;
  Render := RenderItems[FileIndex];
  R := Render.ViewBox;
  S := 'Viewing SVG document that is %d bytes in size with %d command nodes.'#10#10 +
    'View box: %.1f %.1f %.1f %.1f';
  Info := StrFormat(S, [Render.DocumentSize, Render.NodeCount, R.X, R.Y, R.Width, R.Height]);
  if InfoLabel <> nil then
    InfoLabel.Text := Info;
end;

procedure TSvgGrid.ReloadClick(Sender: TObject);
var
  I: Integer;
begin
  Thread.Free;
  for I := RenderItems.Lo to RenderItems.Hi do
  begin
    RenderItems[I].Free;
    RenderItems[I] := nil;
  end;
  Thread := TSvgGridThread.Create(Self);
  while Thread.FileCount < 1 do
    Sleep(1);
  SelectDocument(0);
end;

procedure TSvgGrid.IconChange(Sender: TObject);
var
  W: TWidget absolute Sender;
begin
  SelectDocument(FileIndex + W.Tag);
end;

procedure TSvgGrid.SliderChange(Sender: TObject);
begin
  ScaleLabel.Text := 'Icon scaling in pixels: ' + FloatToStr(Round(ScaleSlider.Position));
end;

procedure TSvgGrid.CloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSvgGrid.DoKeyDown(var Args: TKeyboardArgs);
{$ifdef animation}
const
  AnimTime = 0.25;
  AnimEase = 'linear';
{$endif}
  Angle = PI / 12;
var
  S: Float;
begin
  Args.Handled := (Args.Key = keyLeft) or (Args.Key = keyRight);
  case Args.Key of
    key1..key3: Widget.Theme := Themes[Args.Key - key1];
    keyLeft:
      if ssShift in Args.Shift then
      begin
        {$ifdef animation}
        if not Animations.Playing(Skew) then
          Animations.Add(Skew, Skew - 0.1).Duration(AnimTime).Easing(AnimEase);
        {$else}
        Skew := Skew - 0.1;
        {$endif}
      end
      else if ssAlt in Args.Shift then
      begin
        S := Scale - 0.1;
        if S < 0.3 then
          S := 0.3;
        {$ifdef animation}
        if not Animations.Playing(Scale) then
          if S <> Scale then
            Animations.Add(Scale, S).Duration(AnimTime).Easing(AnimEase);;
        {$else}
        Scale := S;
        {$endif}
      end
      else if ssCtrl in Args.Shift then
      begin
        {$ifdef animation}
        if not Animations.Playing(Rotation) then
          Animations.Add(Rotation, Rotation - Angle).Duration(AnimTime).Easing(AnimEase);;
        {$else}
        Rotation := Rotation - Angle;
        {$endif}
      end
      else
      begin
        Dec(FileIndex);
        SelectDocument(FileIndex);
      end;
    keyRight:
      if ssShift in Args.Shift then
      begin
        {$ifdef animation}
        if not Animations.Playing(Skew) then
          Animations.Add(Skew, Skew + 0.1).Duration(AnimTime).Easing(AnimEase);
        {$else}
        Skew := Skew + 0.1;
        {$endif}
      end
      else if ssAlt in Args.Shift then
      begin
        S := Scale + 0.1;
        if S > 3 then
          S := 3;
        {$ifdef animation}
        if not Animations.Playing(Scale) then
          if S <> Scale then
            Animations.Add(Scale, S).Duration(AnimTime).Easing(AnimEase);;
        {$else}
        Scale := S;
        {$endif}
      end
      else if ssCtrl in Args.Shift then
      begin
        {$ifdef animation}
        if not Animations.Playing(Rotation) then
          Animations.Add(Rotation, Rotation + Angle).Duration(AnimTime).Easing(AnimEase);;
        {$else}
        Rotation := Rotation + Angle;
        {$endif}
      end
      else
      begin
        Inc(FileIndex);
        SelectDocument(FileIndex);
      end;
  end;
  if not Args.Handled then
    inherited DoKeyDown(Args);
end;

procedure TSvgGrid.GenerateWidgets;
begin
  with Widget.Add<TWindow>(Window) do
  begin
    Fade := 0.1;
    Text := 'SVG OpenGL Renderer';
    with This.Add<THBox> do
    begin
      Align := alignCenter;
      with This.Add<TGlyphImage> do
        Text := '󰕙;';
      with This.Add<TLabel> do
      begin
        MaxWidth := 250;
        Text := 'This program parses and renders SVG documents in realtime ' +
          'using OpenGL vertex data and compute shaders as the rendering backend.';
      end;
    end;
    with This.Add<TLabel> do
      Text := 'Performance graph:';
    with This.Add<TPerformanceGraph>(Graph) do
    begin
      Width := 300;
      Height := 100;
      FrameMeasure :=  1 / 10;
      Step := 4;
    end;
    with This.Add<THBox> do
    begin
      Align := alignCenter;
      with This.Add<TPushButton> do
      begin
        Text := 'Reload';
        OnClick := ReloadClick;
      end;
      with This.Add<TPushButton> do
      begin
        Tag := -1;
        Text := 'Prior';
        OnClick := IconChange;
      end;
      with This.Add<TPushButton> do
      begin
        Tag := 1;
        Text := 'Next';
        OnClick := IconChange;
      end;
    end;
    with This.Add<TLabel>(ScaleLabel) do
      Text := 'Icon scaling in pixels:';
    with This.Add<TSlider>(ScaleSlider) do
    begin
      Align := alignCenter;
      Min := 100;
      Max := 1000;
      Position := 200;
      Step := 1;
      Width := 350;
      OnChange := SliderChange;
    end;
    with This.Add<TLabel> do
    begin
      Text := 'Current SVG document information:';
    end;
    with This.Add<TLabel>(InfoLabel) do
    begin
      MaxWidth := 250;
      Indent := 1;
      Text := Info;
    end;
    with This.Add<TLabel> do
      Text := 'Rendering options:';
    with This.Add<TCheckBox>(RenderStrokesBox) do
    begin
      Checked := True;
      Indent := 1;
      Text := 'Draw strokes and fills';
    end;
    with This.Add<TCheckBox>(RenderOutlinesBox) do
    begin
      Indent := 1;
      Text := 'Draw vector outlines';
    end;
    with This.Add<TCheckBox>(RenderNodesBox) do
    begin
      Indent := 1;
      Text := 'Draw nodes';
    end;
    with This.Add<TCheckBox>(RenderBackgroundBox) do
    begin
      Indent := 1;
      Text := 'Transparent background';
    end;
    with This.Add<TCheckBox>(RenderAnimateBox) do
    begin
      Checked := True;
      Indent := 1;
      Text := 'Animated scale and rotation';
    end;
    with This.Add<TPushButton> do
    begin
      Align := alignCenter;
      Text := 'Close';
      OnClick := CloseClick;
    end;
  end;
  SliderChange(nil);
end;

function TSvgGrid.WantKeys: Boolean;
begin
  Result := True;
end;

procedure TSvgGrid.Load;
begin
  inherited Load;
  {$ifdef animation}
  Skew.Link;
  Scale.Link;
  Scale.Value := 1;
  Rotation.Link;
  {$else}
  Scale := 1;
  {$endif}
  Themes[0] := NewTheme(Canvas, TArcDarkTheme);
  Themes[1] := NewTheme(Canvas, TChicagoTheme);
  Themes[2] := NewTheme(Canvas, TGraphiteTheme);
  Font := Canvas.LoadFont('Roboto', '../assets/Roboto-Medium.ttf');
  Font.Color := colorBlack;
  Font.Size := 24;
  Files := FindFiles('../assets/icons/*.svg');
  Files.Sort;
  RenderItems.Length := Files.Length;
  Thread := TSvgGridThread.Create(Self);
  while Thread.FileCount < 1 do
    Sleep(1);
  SelectDocument(0);
  Brush := NewBrush(Canvas.LoadBitmap('alpha', '../assets/alpha.gif'));
  Title := NewBrush(NewPointF(0, 0), NewPointF(0, 50));
  Title.NearStop.Color := colorWhite;
  Title.FarStop.Color := colorGray;
  GenerateWidgets;
end;

procedure TSvgGrid.Render(Width, Height: Integer; const Time: Double);
var
  Options: TSvgRenderOptions;
  R: TRectF;
  T: Float;
  I, J: Integer;
  S, C, X, Y, Offset: Integer;
begin
  inherited Render(Width, Height, Time);
  {$ifdef animation}
  Animations.Animate(Time);
  {$endif}
  Canvas.Rect(ClientRect);
  if RenderBackgroundBox.Checked then
    Canvas.Fill(Brush)
  else
    Canvas.Fill(colorWhite);
  Options := [];
  if RenderStrokesBox.Checked then
    Include(Options, renderColor);
  if RenderOutlinesBox.Checked then
    Include(Options, renderOutlines);
  if RenderNodesBox.Checked then
    Include(Options, renderNodes);
  if RenderAnimateBox.Checked then
    T := Time
  else
    T := 0;
  Offset := 0;
  S := Round(ScaleSlider.Position);
  for I := 0 to Thread.FileCount - 1 do
  begin
    C := Width div S;
    if C < 1 then C := 1;
    if Offset = 0 then
      Offset := (Width - C * S) div 2;
    X := I mod C * S + (S div 2) + Offset;
    Y := I div C * S + (S div 2) + 50;
    if Y > Height then
      Break;
    J := (I + FileIndex) mod Thread.FileCount;
    RenderItems[J].DrawAt(X, Y, 1, T, S - 30, Options, 0.5);
  end;
  R := ClientRect;
  R.Bottom := 42;
  Canvas.Rect(R);
  Canvas.Fill(Title);
  R.Top := R.Bottom;
  R.Height := 1;
  Canvas.Rect(R);
  Canvas.Fill($FF303030);
  Canvas.DrawText(Font, FileExtractName(Files[FileIndex]) + StrFormat(' / icon %d of %d',
    [FileIndex + 1, Thread.FileCount]), 10, 10);
  WidgetMatrix.Identity;
  WidgetMatrix.SkewX(Skew);
  WidgetMatrix.ScaleAt(Scale, Scale, Width / 2, Height / 2);
  WidgetMatrix.RotateAt(Rotation, Width / 2, Height / 2);
  WidgetsRender(Width, Height, Time);
  Graph.Update;
  if Thread.FileCount < Files.Length then
    Sleep(5);
  if Time < 0.25 then
  begin
    Window.X := 100;
    Window.Y := 100;
  end;
end;

procedure TSvgGrid.Unload;
var
  I: Integer;
begin
  Thread.Free;
  for I := RenderItems.Lo to RenderItems.Hi do
    RenderItems[I].Free;
  for I := Low(Themes) to High(Themes) do
    Themes[I].Free;
  inherited Unload;
end;

end.

