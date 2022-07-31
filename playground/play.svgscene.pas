unit Play.SvgScene;

{$mode delphi}
{$WARN 6060 off : }

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Widgets,
  Tiny.Widgets.Custom,
  Tiny.Application,
  Tiny.Graphics,
  Play.Svg,
  Play.SvgRender;

{ TSvgScene }

type
  TSvgScene = class(TWidgetScene)
  private
    Font: IFont;
    Brush: IBitmapBrush;
    Title: ILinearGradientBrush;
    Files: StringArray;
    FileIndex: Integer;
    SvgRender: TSvgRender;
    Info: string;
    InfoLabel: TLabel;
    RenderStrokesBox: TCheckBox;
    RenderOutlinesBox: TCheckBox;
    RenderNodesBox: TCheckBox;
    RenderBackgroundBox: TCheckBox;
    RenderAnimateBox: TCheckBox;
    Graph: TPerformanceGraph;
    PanButton: TGlyphButton;
    ZoomButton: TGlyphButton;
    RotateButton: TGlyphButton;
    Pan: TPointF;
    Zoom: Float;
    Rotate: Float;
    Dragging: Boolean;
    procedure GenerateWidgets;
    procedure SelectDocument(Index: Integer);
    procedure ChangeIcon(Sender: TObject);
    procedure CloseClick(Sender: TObject);
    procedure SvgRenderAt(Svg: TSvgRender; X, Y, Scale, Angle, Size: Float;
      Options: TSvgRenderOptions = [renderColor]; PenWidth: Float = 1);
    procedure PanZoomRotate(Sender: TObject; var Args: TMouseArgs);
  protected
    procedure DoKeyDown(var Args: TKeyboardArgs); override;
    procedure DoMouseDown(var Args: TMouseArgs); override;
    procedure DoMouseMove(var Args: TMouseArgs); override;
    procedure DoMouseUp(var Args: TMouseArgs); override;
    function WantKeys: Boolean; override;
    procedure Load; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
    procedure Unload; override;
  end;

implementation

{ TSvgScene }

procedure TSvgScene.SelectDocument(Index: Integer);
var
  R: TRectF;
  S: string;
begin
  if Index < 0 then
    Index := Files.Length - 1
  else if Index > Files.Length - 1 then
    Index := 0;
  FileIndex := Index;
  SvgRender.ParseFile(Files[FileIndex]);
  R := SvgRender.ViewBox;
  S := 'Viewing SVG document that is %d bytes in size with %d command nodes.'#10#10 +
    'View box: %.1f %.1f %.1f %.1f';
  Info := StrFormat(S, [SvgRender.DocumentSize, SvgRender.NodeCount, R.X, R.Y, R.Width, R.Height]);
  if InfoLabel <> nil then
    InfoLabel.Text := Info;
end;

procedure TSvgScene.ChangeIcon(Sender: TObject);
var
  W: TWidget absolute Sender;
begin
  SelectDocument(FileIndex + W.Tag);
end;

procedure TSvgScene.CloseClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TSvgScene.DoKeyDown(var Args: TKeyboardArgs);
begin
  Args.Handled := (Args.Key = keyLeft) or (Args.Key = keyRight);
  if Args.Key = keyLeft then
    Dec(FileIndex)
  else if Args.Key = keyRight then
    Inc(FileIndex);
  if Args.Handled then
    SelectDocument(FileIndex)
  else
    inherited DoKeyDown(Args);
end;

procedure TSvgScene.PanZoomRotate(Sender: TObject; var Args: TMouseArgs);
var
  W: TWidget absolute Sender;
  Z: Float;
begin
  Exit;
  if wsPressed in (Sender as TGlyphButton).State then
    case W.Tag of
      1:
        begin
          Pan.X := Pan.X + Args.XRel;
          Pan.Y := Pan.Y + Args.YRel;
        end;
      2:
        begin
          Zoom := Zoom + Args.YRel / 10;
          if Zoom < -2.95 then Zoom := -2.95;
        end;
      3:
        begin
          Z := Zoom + 3;
          Rotate := Rotate + Args.XRel / 40 / Z;
        end;
    end;
end;

procedure TSvgScene.DoMouseDown(var Args: TMouseArgs);
begin
  inherited;
  if Args.Handled then Exit;
  Dragging := Args.Button = mbLeft;
end;

procedure TSvgScene.DoMouseMove(var Args: TMouseArgs);
var
  Z: Float;
begin
  inherited;
  if Args.Handled then Exit;
  if Dragging then
    if PanButton.Down then
    begin
      Pan.X := Pan.X + Args.XRel;
      Pan.Y := Pan.Y + Args.YRel;
    end
    else if ZoomButton.Down then
    begin
      Zoom := Zoom + Args.YRel / 10;
      if Zoom < -2.95 then Zoom := -2.95;
    end
    else if RotateButton.Down then
    begin
      Z := Zoom + 3;
      Rotate := Rotate + Args.XRel / 40 / Z;
    end;
end;

procedure TSvgScene.DoMouseUp(var Args: TMouseArgs);
begin
  inherited;
  if Args.Handled then Exit;
  if Args.Button = mbLeft then
    Dragging := False;
end;

procedure TSvgScene.GenerateWidgets;
begin
  with Widget.Add<TWindow> do
  begin
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
    end;
    with This.Add<THBox> do
    begin
      Align := alignCenter;
      Margin := 0;
      with This.Add<TGlyphButton> do
      begin
        Margin := 0;
        Tag := -1;
        Text := '󰅁';
        OnClick := ChangeIcon;
      end;
      with This.Add<TGlyphButton> do
      begin
        Margin := 0;
        Tag := 1;
        Text := '󰅂';
        OnClick := ChangeIcon;
      end;
      with This.Add<TSpacer> do
        Margin := 0;
      with This.Add<TGlyphButton>(PanButton) do
      begin
        CanToggle := True;
        Group := 1;
        Margin := 0;
        Text := '󰆾';
        Hint := 'Pan';
        Tag := 1;
        OnMouseMove := PanZoomRotate;
      end;
      with This.Add<TGlyphButton>(ZoomButton) do
      begin
        CanToggle := True;
        Group := 1;
        Margin := 0;
        Text := '󰍉';
        Hint := 'Zoom';
        Tag := 2;
        OnMouseMove := PanZoomRotate;
      end;
      with This.Add<TGlyphButton>(RotateButton) do
      begin
        CanToggle := True;
        Group := 1;
        Margin := 0;
        Text := '󰑥';
        Hint := 'Rotate';
        Tag := 3;
        OnMouseMove := PanZoomRotate;
      end;
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
      Checked := False;
      Indent := 1;
      Text := 'Draw vector outlines';
    end;
    with This.Add<TCheckBox>(RenderNodesBox) do
    begin
      Checked := False;
      Indent := 1;
      Text := 'Draw nodes';
    end;
    with This.Add<TCheckBox>(RenderBackgroundBox) do
    begin
      Checked := False;
      Indent := 1;
      Text := 'Transparent background';
    end;
    with This.Add<TCheckBox>(RenderAnimateBox) do
    begin
      Checked := False;
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
end;

function TSvgScene.WantKeys: Boolean;
begin
  Result := True;
end;

procedure TSvgScene.Load;
begin
  inherited Load;
  Font := Canvas.LoadFont('Roboto', '../assets/Roboto-Medium.ttf');
  Font.Color := colorBlack;
  Font.Size := 24;
  //Files := FindFiles('../assets/icons/*.svg');
  Files := FindFiles('../assets/vectors/*.svg');

  //Files.Push('/home/gigauser/Desktop/other/cartman.svg');
  // Files.Push('/home/gigauser/Desktop/flat/bell-svgrepo-com.svg');
  //Files.Push('/home/gigauser/Desktop/gtiger.svg');
  //Files.Push('/home/gigauser/Desktop/landscape.svg');

  Files.Sort;
  SvgRender := TSvgRender.Create(Canvas);
  SelectDocument(0);
  Brush := NewBrush(Canvas.LoadBitmap('alpha', '../assets/alpha.gif'));
  Title := NewBrush(NewPointF(0, 0), NewPointF(0, 50));
  Title.NearStop.Color := colorWhite;
  Title.FarStop.Color := colorGray;
  GenerateWidgets;
end;

procedure TSvgScene.SvgRenderAt(Svg: TSvgRender; X, Y, Scale, Angle, Size: Float; Options: TSvgRenderOptions = [renderColor]; PenWidth: Float = 1);
var
  P: TPointF;
  S: Float;
begin
  Canvas.Matrix.Push;
  P := SvgRender.ViewBox.MidPoint;
  S :=  Scale / (SvgRender.ViewBox.Width / Size);
  if Angle <> 0 then
    Canvas.Matrix.RotateAt(Angle, P.X, P.Y);
  Canvas.Matrix.Translate(X - P.X, Y - P.Y);
  Canvas.Matrix.ScaleAt(S, S, X, Y);
  if renderColor in Options then
    Svg.Draw;
  if renderOutlines in Options then
    Svg.DrawOutline(PenWidth);
  if renderNodes in Options then
    Svg.DrawNodes(PenWidth);
  Canvas.Matrix.Pop;
end;

procedure TSvgScene.Render(Width, Height: Integer; const Time: Double);
var
  Options: TSvgRenderOptions;
  R: TRectF;
  S, T: Float;
  P: TPointF;
begin
  inherited Render(Width, Height, Time);
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
  begin
    S := 4 + Sin(Time / 2) * 3;
    T := Time;
    Pan.X := 0;
    Pan.Y := 0;
    Zoom := 0;
    Rotate := 0;
    SvgRender.DrawAt(Width / 2, Height / 2, S, T, 256, Options, 4 / S);
  end
  else
  begin
    S := 3 + Zoom;
    if S < 0.1 then S := 0.1;
    T := Rotate;
    P := Pan;
    Canvas.Matrix.Translate(Width / -2, Height / -2);
    Canvas.Matrix.Rotate(T);
    Canvas.Matrix.Translate(Width / 2, Height / 2);
    Canvas.Matrix.Translate(P.X / (S / 3), P.Y / (S / 3));
    SvgRender.DrawAt(Width / 2, Height / 2, S, 0, 256, Options, 4 / S);
    Canvas.Matrix.Identity;
  end;

  R := ClientRect;
  R.Bottom := 42;
  Canvas.Rect(R);
  Canvas.Fill(Title);
  R.Top := R.Bottom;
  R.Height := 1;
  Canvas.Rect(R);
  Canvas.Fill($FF303030);
  Canvas.DrawText(Font, FileExtractName(Files[FileIndex]) + StrFormat(' / icon %d of %d', [FileIndex + 1, Files.Length]), 10, 10);
  Widget.Render(Width, Height, Time);
  Graph.Update;
end;

procedure TSvgScene.Unload;
begin
  SvgRender.Free;
  inherited Unload;
end;

end.

