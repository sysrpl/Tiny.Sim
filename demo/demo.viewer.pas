unit Demo.Viewer;

{$mode delphi}

interface

uses
  { Our library units }
  Tiny.System,
  Tiny.Application,
  Tiny.Widgets,
  Tiny.Widgets.Themes,
  Tiny.Widgets.Custom,
  { Our demo scenes }
  Demo.Scene,
  Demo.AsteroidsGame,
  Demo.AtomicDots,
  Demo.CrowdWalk,
  Demo.FireSparks,
  Demo.LightBeams,
  Demo.MouseTrack,
  Demo.Synthwave,
  Demo.TextBrush,
  Demo.VectorClock;

{ TDemoViewer is the main scene allowing users to pick and run demos }

var
  DemoClasses: array of TDemoSceneClass = [TAtomicDots, TCrowdWalk, TFireSparks,
    TLightBeams, TMouseTrack, TTextBrush, TVectorClock, TSynthWave, TAsteroidsGame];

type
  TDemoViewer = class(TWidgetScene)
  private
    FActivateTime: Double;
    FDemoScenes: TArrayList<TDemoScene>;
    FArcTheme: TTheme;
    FChicagoTheme: TTheme;
    FGraphiteTheme: TTheme;
    FInfo: TGlyphButton;
    FSync: TGlyphButton;
    FFullscreen: TGlyphButton;
    FGraph: TGlyphButton;
    FStats: TPerformanceGraph;
    FAbout: TWindow;
    FIcon: TGlyphImage;
    FDescription: TLabel;
    FRangeDescription: TLabel;
    FRangeSlider: TSlider;
    FArc: TCheckBox;
    FChicago: TCheckBox;
    FGraphite: TCheckBox;
    FOptionSync: TCheckBox;
    FOptionFullscreen: TCheckBox;
    procedure GenerateWidgets;
    procedure SceneClick(Sender: TObject);
    procedure StatsClick(Sender: TObject);
    procedure VSyncClick(Sender: TObject);
    procedure VSyncOptionClick(Sender: TObject);
    procedure FullscreenClick(Sender: TObject);
    procedure FullscreenOptionClick(Sender: TObject);
    procedure AboutClick(Sender: TObject);
    procedure AboutClose(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure ThemeClick(Sender: TObject);
    procedure SliderChange(Sender: TObject);
    procedure WidgetKeyDown(Sender: TObject; var Args:TKeyboardArgs);
    procedure WidgetKeyUp(Sender: TObject; var Args:TKeyboardArgs);
    procedure WidgetMouseMove(Sender: TObject; var Args:TMouseArgs);
  protected
    function DefaultTheme: TTheme; override;
    procedure Load; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
    procedure Unload; override;
  end;

implementation

procedure TDemoViewer.WidgetKeyDown(Sender: TObject; var Args: TKeyboardArgs);
begin
  case Args.Key of
    key1..key9, keyF1..keyF4: Args.Handled := True
  end;
end;

procedure TDemoViewer.WidgetKeyUp(Sender: TObject; var Args: TKeyboardArgs);
var
  Handled: Boolean;
  I: Integer;
begin
  Handled := True;
  case Args.Key of
    key1..key9:
      begin
        I := Args.Key - key0;
        if I <= FDemoScenes.Length then
          Widget.FindWidget<TWidget>(IntToStr(I)).Click;
      end;
    keyF1: FInfo.Click;
    keyF2: FSync.Click;
    keyF3: FFullscreen.Click;
    keyF4: FGraph.Click;
  else
    Handled := False;
  end;
  Args.Handled := Handled;
end;

procedure TDemoViewer.WidgetMouseMove(Sender: TObject; var Args:TMouseArgs);
begin
  if SubScene is TMouseTrack then
    Mouse.Visible := Widget.FindWidget(Args.X, Args.Y) <> nil;
end;

procedure TDemoViewer.SliderChange(Sender: TObject);
var
  D: TDemoScene;
  R: TRangeInfo;
begin
  D := SubScene as TDemoScene;
  R := D.RangeInfo;
  R.Position := FRangeSlider.Position;
  if R.Step > 0.9 then
    FRangeDescription.Text := R.Description + ': ' + FloatToStr(R.Position, 0)
  else if R.Step > 0.09 then
    FRangeDescription.Text := R.Description + ': ' + FloatToStr(R.Position, 1)
  else
    FRangeDescription.Text := R.Description + ': ' + FloatToStr(R.Position, 2);
end;

procedure TDemoViewer.SceneClick(Sender: TObject);
var
  W: TGlyphButton absolute Sender;
  D: TDemoScene;
  R: TRangeInfo;
begin
  D := FDemoScenes[W.Tag];
  SubScene := D;
  FAbout.Text := 'About ' + D.Title;
  FIcon.Text := D.Glyph;
  FDescription.Text := D.Description;
  R := D.RangeInfo;
  if R.Min <> R.Max then
  begin
    FRangeDescription.Visible := True;
    FRangeSlider.Visible := True;
    FRangeSlider.OnChange := nil;
    FRangeSlider.Min := R.Min;
    FRangeSlider.Max := R.Max;
    FRangeSlider.Min := R.Min;
    FRangeSlider.Step := R.Step;
    FRangeSlider.Position := R.Position;
    FRangeSlider.OnChange := SliderChange;
    SliderChange(nil);
    if R.Step > 0.9 then
      FRangeDescription.Text := R.Description + ': ' + FloatToStr(R.Position, 0)
    else if R.Step > 0.09 then
      FRangeDescription.Text := R.Description + ': ' + FloatToStr(R.Position, 1)
    else
      FRangeDescription.Text := R.Description + ': ' + FloatToStr(R.Position, 2);
  end
  else
  begin
    FRangeDescription.Visible := False;
    FRangeSlider.Visible := False;
    FRangeSlider.OnChange := nil;
  end;
end;

procedure TDemoViewer.StatsClick(Sender: TObject);
var
  W: TGlyphButton absolute Sender;
begin
  FStats.Visible := W.Down;
  if W.Down then
    W.Hint := 'Hide performance information F4'
  else
    W.Hint := 'Show performance information F4';
end;

procedure TDemoViewer.VSyncClick(Sender: TObject);
var
  W: TGlyphButton absolute Sender;
begin
  Application.VSync := not W.Down;
  if W.Down then
  begin
    W.Text := '󰍹';
    W.Hint := 'Lock to vertical sync F2';
  end
  else
  begin
    W.Text := '󰷛';
    W.Hint := 'Unlock vertical sync F2';
  end;
  FOptionSync.Checked := not W.Down;
end;

procedure TDemoViewer.VSyncOptionClick(Sender: TObject);
begin
  FSync.Click;
end;

procedure TDemoViewer.FullscreenClick(Sender: TObject);
var
  W: TGlyphButton absolute Sender;
begin
  Application.Fullscreen := W.Down;
  if W.Down then
  begin
    W.Text := '󰊔';
    W.Hint := 'Switch to windowed mode F3';
  end
  else
  begin
    W.Text := '󰊓';
    W.Hint := 'Switch to fullscreen mode F3';
  end;
  FOptionFullscreen.Checked := W.Down;
  FAbout.Sector := 4;
  FActivateTime := Time;
end;

procedure TDemoViewer.FullscreenOptionClick(Sender: TObject);
begin
  FFullscreen.Click;
end;

procedure TDemoViewer.AboutClick(Sender: TObject);
var
  W: TGlyphButton absolute Sender;
begin
  if W.Down then
  begin
    FAbout.Activate;
    FInfo.Hint := 'Close scene information F1';
  end
  else
  begin
    FAbout.Hide;
    FInfo.Hint := 'Show scene information F1';
  end;
  FAbout.Sector := 4;
  FActivateTime := Time;
end;

procedure TDemoViewer.AboutClose(Sender: TObject);
begin
  FAbout.Hide;
  FInfo.Down := False;
end;

procedure TDemoViewer.ExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TDemoViewer.ThemeClick(Sender: TObject);
begin
  if Sender = FArc then
  begin
    FArc.Checked := True;
    FChicago.Checked := False;
    FGraphite.Checked := False;
    Widget.Theme := FArcTheme;
  end
  else if Sender = FChicago then
  begin
    FArc.Checked := False;
    FChicago.Checked := True;
    FGraphite.Checked := False;
    Widget.Theme := FChicagoTheme;
  end
  else
  begin
    FArc.Checked := False;
    FChicago.Checked := False;
    FGraphite.Checked := True;
    Widget.Theme := FGraphiteTheme;
  end
end;

function TDemoViewer.DefaultTheme: TTheme;
begin
  Result := FArcTheme;
end;

procedure TDemoViewer.GenerateWidgets;
var
  Numbers: array of string = [
    '󰲠',
    '󰲢',
    '󰲤',
    '󰲦',
    '󰲨',
    '󰲪',
    '󰲬',
    '󰲮',
    '󰲰'];
var
  I: Integer;
begin
  with Widget.Add<THBox> do
  begin
    Sector := 2;
    Margin := -5;
    Fade := 0.15;
    with This.Add<TVBox> do
    begin
      Margin := 0;
      with This.Add<THBox> do
      begin
        Align := alignCenter;
        Margin := 0;
        for I := 0 to FDemoScenes.Length - 1 do
          with This.Add<TGlyphButton> do
          begin
            CanToggle := True;
            Text := Numbers[I];
            Group := 1;
            Down := True;
            Tag := I;
            Name := IntToStr(I + 1);
            Hint := 'Switch to scene ' + Name;
            OnClick := SceneClick;
          end;
        This.Add<TSpacer>;
        with This.Add<TGlyphButton>(FInfo) do
        begin
          CanToggle := True;
          Text := '󰆆';
          Hint := 'Show scene information F1';
          OnClick := AboutClick;
        end;
        with This.Add<TGlyphButton>(FSync) do
        begin
          CanToggle := True;
          Text := '󰷛';
          Hint := 'Unlock verticle sync F2';
          OnClick := VSyncClick;
        end;
        with This.Add<TGlyphButton>(FFullscreen) do
        begin
          Down := Application.Fullscreen;
          CanToggle := True;
          Text := '󰊓';
          Hint := 'Switch to fullscreen mode F3';
          OnClick := FullscreenClick;
        end;
        with This.Add<TGlyphButton>(FGraph) do
        begin
          CanToggle := True;
          Text := '󰄧';
          Hint := 'Show performance information F4';
          OnClick := StatsClick;
        end;
        This.Add<TSpacer>;
        with This.Add<TGlyphButton> do
        begin
          Text := '󰅚';
          OnClick := ExitClick;
          Hint := 'Exit this program ESC';
        end;
      end;
      with This.Add<TPerformanceGraph>(FStats) do
      begin
        Align := alignCenter;
        Width := 500;
        Height := 50;
        Margin := 5;
        Visible := False;
      end;
    end;
  end;
  with Widget.Add<TWindow>(FAbout) do
  begin
    Text := 'About';
    Visible := False;
    with This.Add<THBox> do
    begin
      with This.Add<TGlyphImage>(FIcon) do
        Text := '󰮣';
      with This.Add<TLabel>(FDescription) do
      begin
        MaxWidth := 200;
        Text := 'A description of the scene goes here. It could be a very long paragraph';
      end;
      Margin := 16;
    end;
    with This.Add<TLabel>(FRangeDescription) do
      Text := 'Range description:';
    with This.Add<TSlider>(FRangeSlider) do
    begin
      Width := 285;
    end;
    with This.Add<TLabel> do
      Text := 'Available visual styles:';
    with This.Add<TCheckBox>(FArc) do
    begin
      Indent := 1;
      Round := True;
      Text := 'Arc Dark';
      OnClick := ThemeClick;
    end;
    with This.Add<TCheckBox>(FChicago) do
    begin
      Indent := 1;
      Round := True;
      Text := 'Chicago';
      OnClick := ThemeClick;
    end;
    with This.Add<TCheckBox>(FGraphite) do
    begin
      Indent := 1;
      Round := True;
      Text := 'Graphite';
      OnClick := ThemeClick;
    end;
    with This.Add<TLabel> do
      Text := 'Options:';
    with This.Add<TCheckBox>(FOptionSync) do
    begin
      Indent := 1;
      Checked := True;
      Text := 'Vertical sync';
      OnClick := VSyncOptionClick;
    end;
    with This.Add<TCheckBox>(FOptionFullscreen) do
    begin
      Indent := 1;
      Text := 'Fullscreen';
      OnClick := FullscreenOptionClick;
    end;
    with This.Add<TPushButton> do
    begin
      Align := alignCenter;
      Text := 'Close';
      OnClick := AboutClose;
    end;
  end;
end;

procedure TDemoViewer.Load;
var
  C: TDemoSceneClass;
begin
  inherited Load;
  FArcTheme := NewTheme(Canvas, TArcDarkTheme);
  FChicagoTheme := NewTheme(Canvas, TChicagoTheme);
  FGraphiteTheme := NewTheme(Canvas, TGraphiteTheme);
  for C in DemoClasses do
    FDemoScenes.Push(C.Create(Canvas));
  GenerateWidgets;
  // FInfo.Click;
  FArc.Click;
  FOptionSync.Checked := Application.VSync;
  FOptionFullscreen.Checked := Application.Fullscreen;
  Widget.OnKeyDown := WidgetKeyDown;
  Widget.OnKeyUp := WidgetKeyUp;
  Widget.OnMouseMove := WidgetMouseMove;
  Widget.FindWidget<TWidget>(IntToStr(FDemoScenes.Length - 1)).Click;
end;

procedure TDemoViewer.Render(Width, Height: Integer; const Time: Double);
begin
  inherited Render(Width, Height, Time);
  Widget.Render(Width, Height, Time);
  FStats.Update;
  if Time - FActivateTime > 0.25 then
    FAbout.Sector := 0;
end;

procedure TDemoViewer.Unload;
var
  D: TDemoScene;
begin
  SubScene := nil;
  FArcTheme.Free;
  FChicagoTheme.Free;
  FGraphiteTheme.Free;
  for D in FDemoScenes do
    D.Free;
  FDemoScenes.Length := 0;
  inherited Unload;
end;

end.

