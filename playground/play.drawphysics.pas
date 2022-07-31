unit Play.DrawPhysics;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application,
  Tiny.Graphics,
  Tiny.Widgets,
  Tiny.Widgets.Themes,
  Tiny.Widgets.Custom,
  Tiny.Physics,
  Tiny.Physics.Scene;

{ TDrawPhysics is a scene where users can draw hollow or solid physics objects
  using a simulated crayon style }

type
  TDrawing = TArrayList<TPointD>;

  TDrawPhysics = class(TPhysicsScene)
  private
    FOutlinePen: IPen;
    FSolidPen: IPen;
    FBlueCrayon: IBitmapBrush;
    FRedCrayon: IBitmapBrush;
    FBackground: IBitmap;
    FGlyph: IFont;
    FDraw: TGlyphButton;
    FFill: TGlyphButton;
    FGrab: TGlyphButton;
    FErase: TGlyphButton;
    FSync: TGlyphButton;
    FFullscreen: TGlyphButton;
    FGraph: TGlyphButton;
    FStats: TPerformanceGraph;
    FDrawing: TDrawing;
    FIsDrawing: Boolean;
    FIsFilling: Boolean;
    FIsErasing: Boolean;
    procedure GenerateWidgets;
    procedure SyncClick(Sender: TObject);
    procedure FullscreenClick(Sender: TObject);
    procedure GraphClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);
    procedure GrabClick(Sender: TObject);
  protected
    procedure DoMouseDown(var Args: TMouseArgs); override;
    procedure DoMouseMove(var Args: TMouseArgs); override;
    procedure DoMouseUp(var Args: TMouseArgs); override;
    function DrawCustomBody(Body: TBody): Boolean; override;
    procedure Load; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
    procedure Unload; override;
  end;

implementation

const
  Outline = Pointer(1);
  Fill = Pointer(2);
  Thick = 10;
  Thin = 6;

procedure TDrawPhysics.GrabClick(Sender: TObject);
begin
  { Allow the user to move physics objects if the grab button is toggled }
  GrabBodies := FGrab.Down;
end;

procedure TDrawPhysics.DoMouseDown(var Args: TMouseArgs);
var
  P: TPointF;
begin
  if (Widget.FindWidget(Args.X, Args.Y) = nil) and (Args.Button = mbLeft) then
    if FDraw.Down or FFill.Down then
    begin
      { If hollow or solid drawing }
      if FDraw.Down then
        FIsDrawing := True
      else
        FIsFilling := True;
      { Start capturing drawing points }
      FDrawing.Clear;
      P := PointToStudio(Args.X, Args.Y);
      FDrawing.Push(P);
      Mouse.Visible := False;
      Args.Handled := True;
    end
    else if FErase.Down then
    begin
      { If erasing }
      FIsErasing := True;
      Args.Handled := True;
    end;
  if not Args.Handled then
    inherited DoMouseDown(Args);
end;

procedure TDrawPhysics.DoMouseMove(var Args: TMouseArgs);
var
  A, B: TPointF;
  S: TShape;
begin
  if FIsDrawing or FIsFilling then
  begin
    { If hollow or solid drawing }
    A := PointToStudio(Args.X, Args.Y);
    B := FDrawing.Last;
    if A.Distance(B) > 10 then
      FDrawing.Push(A);
    Mouse.Visible := False;
    Args.Handled := True;
  end
  else if FIsErasing then
  begin
    { If erasing }
    A := PointToStudio(Args.X, Args.Y);
    S := ShapeNearPoint(A.X, A.Y, 10);
    if (S <> nil) and (S.Body.Kind = bodyDynamic) then
      S.Body.Free;
    Args.Handled := True;
  end
  else
    { Turn off the mouse cursor if there is no widget underneath the mouse }
    Mouse.Visible := Widget.FindWidget(Args.X, Args.Y) <> nil;
  if not Args.Handled then
    inherited DoMouseMove(Args);
end;

procedure TDrawPhysics.DoMouseUp(var Args: TMouseArgs);
var
  B: TBody;
  S: TShape;
  I: Integer;
begin
  if (Args.Button = mbLeft) and (FIsDrawing or FIsFilling) then
  begin
    if FDrawing.Length > 1 then
    begin
      { If we were drawing then }
      B := Space.NewBody;
      if FIsDrawing then
      begin
        { Add a hollow physics object }
        for I := 1 to FDrawing.Length - 1 do
        begin
          S := B.NewSegment(FDrawing[I - 1], FDrawing[I], 5);
          S.Density := 2;
          S.Friction := 0.6;
          S.Elasticity := 0.4;
          S.Category := 1;
        end;
        B.UserData := Outline;
      end
      else
      begin
        { Add a solid physics object }
        S := B.NewPolygon(@FDrawing.Items[0], FDrawing.Length);
        S.Density := 4;
        S.Friction := 0.6;
        S.Elasticity := 0.4;
        S.Category := 1;
        B.UserData := Fill;
      end;
      FIsDrawing := False;
      FIsFilling := False;
    end;
    FDrawing.Clear;
    Args.Handled := True;
  end
  else if (Args.Button = mbLeft) and FIsErasing then
  begin
    { Else stop erasing }
    FIsErasing := False;
    Args.Handled := True;
  end;
  if not Args.Handled then
    inherited DoMouseUp(Args);
end;

procedure TDrawPhysics.SyncClick(Sender: TObject);
begin
  Application.VSync := FSync.Down;
end;

procedure TDrawPhysics.FullscreenClick(Sender: TObject);
begin
  Application.Fullscreen := FFullscreen.Down;
end;

procedure TDrawPhysics.GraphClick(Sender: TObject);
begin
  FStats.Visible := FGraph.Down;
end;

procedure TDrawPhysics.ExitClick(Sender: TObject);
begin
  Application.Terminate;
end;

function TDrawPhysics.DrawCustomBody(Body: TBody): Boolean;
var
  S: TShape;
  G: TPolygon;
  P: TPointF;
  I: Integer;
begin
  { Return true if we want to draw the physics ourselves }
  Result := True;
  { Don't draw ground plane }
  if Body = Space.Ground then
    Exit;
  if Body.UserData = Outline then
  begin
    { Draw the hollow body }
    for S in Body.Shapes do
    begin
      P := Body.BodyToWorld(S.AsSegment.A);
      Canvas.MoveTo(P.X, P.Y);
      P := Body.BodyToWorld(S.AsSegment.B);
      Canvas.LineTo(P.X, P.Y);
    end;
    P := Body.Position;
    FBlueCrayon.Offset := P;
    FBlueCrayon.Angle := Body.Angle;
    FOutlinePen.Width := Thin;
    Canvas.Stroke(FOutlinePen, True);
    FOutlinePen.Width := Thick;
    Canvas.Stroke(FOutlinePen);
  end
  else if Body.UserData = Fill then
  begin
    { Draw the solid body }
    G := Body.Shape.AsPolygon;
    for I := 0 to G.VertCount - 1 do
    begin
      P := Body.BodyToWorld(G.Vert[I]);
      if I = 0 then
        Canvas.MoveTo(P.X, P.Y)
      else
        Canvas.LineTo(P.X, P.Y);
    end;
    Canvas.ClosePath;
    P := Body.Position;
    FRedCrayon.Offset := P;
    FRedCrayon.Angle := Body.Angle;
    Canvas.Fill(FRedCrayon, True);
    FSolidPen.Width := Thin;
    Canvas.Stroke(FSolidPen);
    FSolidPen.Width := Thick;
    Canvas.Stroke(FSolidPen);
  end
  else
    Result := False;
end;

procedure TDrawPhysics.GenerateWidgets;
begin
  { Generate the toolbar at the top of our window }
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
        with This.Add<TGlyphButton>(FDraw) do
        begin
          CanToggle := True;
          Text := '󰽉';
          Hint := 'Draw shapes outlines';
          Down := true;
          Group := 1;
        end;
        with This.Add<TGlyphButton>(FFill) do
        begin
          CanToggle := True;
          Text := '󱠓';
          Hint := 'Draw solid shapes';
          Group := 1;
        end;
        with This.Add<TGlyphButton>(FGrab) do
        begin
          CanToggle := True;
          Text := '󰆽';
          Hint := 'Grab shapes';
          OnClick := GrabClick;
          Group := 1;
        end;
        with This.Add<TGlyphButton>(FErase) do
        begin
          CanToggle := True;
          Text := '󰙂';
          Hint := 'Erase shapes';
          Group := 1;
        end;
        This.Add<TSpacer>;
        with This.Add<TGlyphButton>(FSync) do
        begin
          CanToggle := True;
          Text := '󰷛';
          Hint := 'Unlock verticle sync';
          OnClick := SyncClick;
        end;
        with This.Add<TGlyphButton>(FFullscreen) do
        begin
          Down := Application.Fullscreen;
          CanToggle := True;
          Text := '󰊓';
          Hint := 'Switch to fullscreen mode';
          OnClick := FullscreenClick;
        end;
        with This.Add<TGlyphButton>(FGraph) do
        begin
          CanToggle := True;
          Text := '󰄧';
          Hint := 'Show performance information';
          OnClick := GraphClick;
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
  Mouse.Visible := False;
end;

procedure TDrawPhysics.Load;
begin
  inherited Load;
  { Create our pens and brushes }
  FOutlinePen := NewPen;
  FOutlinePen.LineCap := capRound;
  FOutlinePen.LineJoin := joinRound;
  FOutlinePen.Width := Thick;
  FBlueCrayon := NewBrush(Canvas.LoadBitmap('blue-crayon', '../assets/blue-crayon.png'));
  FBlueCrayon.Opacity := 0.5;
  FOutlinePen.Brush := FBlueCrayon;
  FSolidPen := NewPen;
  FSolidPen.LineCap := capRound;
  FSolidPen.LineJoin := joinRound;
  FSolidPen.Width := Thick;
  FRedCrayon := NewBrush(Canvas.LoadBitmap('red-crayon', '../assets/red-crayon.png'));
  FRedCrayon.Opacity := 0.5;
  FSolidPen.Brush := FRedCrayon;
  FBackground := Canvas.LoadBitmap('meadow', '../assets/meadow.png');
  { Generate our user interface controls }
  GenerateWidgets;
  { Generate our cursor }
  FGlyph := Canvas.LoadFont('glyph');
  FGlyph.Color := colorBlack;
  FGlyph.Size := 32;
  FGlyph.Align := fontCenter;
  FGlyph.Layout := fontMiddle;
  { Setup our physics }
  GenerateStudioWalls;
  Space.Gravity := Vect(0, 1000);
end;

procedure TDrawPhysics.Render(Width, Height: Integer; const Time: Double);
var
  P: TPointF;
  S: string;
  I: Integer;
begin
  inherited Render(Width, Height, Time);
  { Make the physics rendering fill the entire window }
  ScaleToStudio;
  { Draw the background }
  Canvas.DrawImage(FBackground, 0, 0);
  { Draw the physics objects }
  DrawPhysics;
  { If we are drawings something then render it manually }
  if FDrawing.Length > 1 then
  begin
    { Create the path on the canvas }
    for I := 0 to FDrawing.Length - 1 do
      with FDrawing[I] do
        if I = 0 then
          Canvas.MoveTo(X, Y)
        else
          Canvas.LineTo(X, Y);
    if FIsDrawing then
    begin
      { Draw a blue outline for hollow shapes }
      FBlueCrayon.Offset := NewPointF(0, 0);
      FBlueCrayon.Angle := 0;
      FOutlinePen.Width := Thin;
      Canvas.Stroke(FOutlinePen, True);
      FOutlinePen.Width := Thick;
      Canvas.Stroke(FOutlinePen)
    end
    else
    begin
      { Draw a red outline for solid shapes }
      FRedCrayon.Offset := NewPointF(0, 0);
      FRedCrayon.Angle := 0;
      FSolidPen.Width := Thin;
      Canvas.Stroke(FSolidPen, True);
      FSolidPen.Width := Thick;
      Canvas.Stroke(FSolidPen)
    end;
  end;
  P := NewPointF(Mouse.X, Mouse.Y);
  if Widget.FindWidget(P.X, P.Y) = nil then
  begin
    { Draw a custom custor based on a glyph }
    if FDraw.Down then
      S := '󰃣'
    else if  FFill.Down then
      S := '󰃣'
    else if FGrab.Down then
      S := FGrab.Text
    else
      S := FErase.Text;
    P := PointToStudio(P);
    { Make a soft black drop shadow }
    FGlyph.Color := colorBlack;
    FGlyph.Blur := 2;
    Canvas.DrawText(FGlyph, S, P.X, P.Y);
    Canvas.DrawText(FGlyph, S, P.X, P.Y);
    Canvas.DrawText(FGlyph, S, P.X, P.Y);
    { Then draw the cursor in white }
    FGlyph.Color := colorWhite;
    FGlyph.Blur := 0;
    Canvas.DrawText(FGlyph, S, P.X, P.Y);
  end;
  { Record frame information to the performance graph }
  FStats.Update;
  { Reset the physics view matrix and draw the user interface controls }
  Canvas.Matrix.Identity;
  Widget.Render(Width, Height, Time);
end;

procedure TDrawPhysics.Unload;
begin
  FGlyph := nil;
  inherited Unload;
end;

end.

