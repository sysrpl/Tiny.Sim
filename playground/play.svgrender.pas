unit Play.SvgRender;

{$WARN 6060 off : }
interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Graphics,
  Play.Svg;

{ TSvgRender }

type
  TSvgRenderOptions = set of (renderColor, renderOutlines, renderNodes);

  TSvgRender = class
  private
    FCanvas: ICanvas;
    FDoc: TSvgDocument;
    FPen: IPen;
    FDocumentSize: Integer;
    FNodeCount: Integer;
    function GetViewBox: TRectF;
  public
    constructor Create(Canvas: ICanvas);
    destructor Destroy; override;
    procedure Parse(const Xml: string);
    procedure ParseFile(const FileName: string);
    procedure Draw;
    procedure DrawOutline(PenWidth: Float);
    procedure DrawNodes(PenWidth: Float);
    procedure DrawAt(X, Y, Scale, Angle, Size: Float; Options: TSvgRenderOptions = [renderColor]; PenWidth: Float = 1);
    property DocumentSize: Integer read FDocumentSize;
    property NodeCount: Integer read FNodeCount;
    property ViewBox: TRectF read GetViewBox;
  end;

implementation

{ TSvgRender }

constructor TSvgRender.Create(Canvas: ICanvas);
begin
  FCanvas := Canvas;
  FDoc := TSvgDocument.Create(nil);
  FPen := NewPen;
end;

destructor TSvgRender.Destroy;
begin
  FDoc.Free;
  inherited Destroy;
end;

procedure TSvgRender.Parse(const Xml: string);

  procedure CountNodes(N: TSvgNode);
  var
    G: TSvgGroup absolute N;
    S: TSvgNode;
  begin
    if N is TSvgLine then
      Inc(FNodeCount, 2)
    else if N is TSvgPolyLine then
      Inc(FNodeCount, TSvgPolyline(N).Points.Length)
    else if N is TSvgCircle then
      Inc(FNodeCount)
    else if N is TSvgEllipse then
      Inc(FNodeCount)
    else if N is TSvgRect then
      Inc(FNodeCount)
    else if N is TSvgPath then
      Inc(FNodeCount, TSvgPath(N).Commands.Length)
    else if N is TSvgGroup then
    begin
      Inc(FNodeCount);
      for S in G do
        CountNodes(S);
    end;
  end;

var
  N: TSvgNode;
begin
  FDoc.ParseText(Xml);
  FDocumentSize := Xml.Length;
  FNodeCount := 0;
  for N in FDoc do
    CountNodes(N);
end;

procedure TSvgRender.ParseFile(const FileName: string);
begin
  Parse(FileLoadText(FileName));
end;

procedure TSvgRender.Draw;

  function BuildBrush(Shape: TSvgShape; Opacity: Float): TColorF;
  begin
    Result := Shape.Style.Fill;
    Result.A := Result.A * Shape.Style.FillOpacity * Opacity;
  end;

  function BuildPen(Shape: TSvgShape; Opacity: Float): IPen;
  var
    C: TColorF;
  begin
    FPen.Width := Shape.Style.StrokeWidth;
    C := Shape.Style.Stroke;
    C.A := C.A * Shape.Style.StrokeOpacity * Opacity;
    FPen.Color := C;
    FPen.MiterLimit := Shape.Style.StrokeMiter;
    case Shape.Style.StrokeCap of
      svgCapRound: FPen.LineCap := capRound;
      svgCapButt: FPen.LineCap := capButt;
      svgCapSquare: FPen.LineCap := capSquare;
    end;
    case Shape.Style.StrokeJoin of
      svgJoinRound: FPen.LineJoin := joinRound;
      svgJoinBevel: FPen.LineJoin := joinBevel;
      svgJoinMiter: FPen.LineJoin := joinMiter;
    end;
    Result := FPen;
  end;

  procedure RenderPath(Path: TSvgPath);
  var
    C: TSvgCommand;
    I: Integer;
  begin
    for I := 0 to Path.Commands.Length - 1 do
    begin
      C := Path.Commands[I];
      case C.Action of
        svgMove: FCanvas.MoveTo(C.X, C.Y);
        svgLine,
        svgHLine,
        svgVLine:
          FCanvas.LineTo(C.X, C.Y);
        svgCubic: FCanvas.BezierTo(C.X1, C.Y1, C.X2, C.Y2, C.X, C.Y);
        svgQuadratic: FCanvas.QuadTo(C.X1, C.Y1, C.X, C.Y);
        svgClose: FCanvas.ClosePath;
      end;
    end;
  end;

  procedure Polyline(N: TSvgPolyline);
  var
    P: TPointF;
    I: Integer;
  begin
    for I := 0 to N.Points.Length - 1 do
    begin
      P := N.Points[I];
      if I = 0 then
        FCanvas.MoveTo(P.X, P.Y)
      else
        FCanvas.LineTo(P.X, P.Y);
    end;
    if N is TSvgPolygon then
      FCanvas.ClosePath;
  end;

  procedure Render(N: TSvgNode; Opacity: Float);
  var
    L: TSvgLine absolute N;
    C: TSvgCircle absolute N;
    E: TSvgEllipse absolute N;
    R: TSvgRect absolute N;
    P: TSvgPath absolute N;
    G: TSvgGroup absolute N;
    S: TSvgNode;
    T: TSvgTransform;
    M: IMatrix;
  begin
    if N.Removed then Exit;
    M := nil;
    if N.Transform <> '' then
    begin
      N.BuildTransform(T);
      M := NewMatrix;
      M.Copy(T[0], T[1], T[2], T[3], T[4], T[5]);
      M.Transform(FCanvas.Matrix);
      FCanvas.Matrix.Push;
      FCanvas.Matrix := M;
    end;
    if N is TSvgLine then
    begin
      FCanvas.MoveTo(L.X1, L.Y1);
      FCanvas.LineTo(L.X2, L.Y2);
    end
    else if N is TSvgPolyLine then
      Polyline(TSvgPolyline(N))
    else if N is TSvgCircle then
      FCanvas.Circle(C.X, C.Y, C.R)
    else if N is TSvgEllipse then
      FCanvas.Ellipse(E.X, E.Y, E.W, E.H)
    else if N is TSvgRect then
      if R.R > 0 then
        FCanvas.RoundRect(R.X, R.Y, R.W, R.H, R.R)
      else
        FCanvas.Rect(R.X, R.Y, R.W, R.H)
    else if N is TSvgPath then
      RenderPath(P)
    else if N is TSvgGroup then
    begin
      for S in G do
        Render(S, Opacity * G.Opacity);
      if M <> nil then
        FCanvas.Matrix.Pop;
      Exit;
    end;
    if not (N is TSvgLine) then
      if R.Style.Fill <> 0 then
        FCanvas.Fill(BuildBrush(R, Opacity * R.Opacity), True);
    if R.Style.Stroke <> 0 then
      FCanvas.Stroke(BuildPen(R, Opacity * R.Opacity), True);
    FCanvas.BeginPath;
    if M <> nil then
      FCanvas.Matrix.Pop;
  end;

var
  N: TSvgNode;
begin
  for N in FDoc do
    Render(N, 1);
end;

procedure TSvgRender.DrawOutline(PenWidth: Float);

  procedure RenderPath(Path: TSvgPath);
  var
    C: TSvgCommand;
    I: Integer;
  begin
    for I := 0 to Path.Commands.Length - 1 do
    begin
      C := Path.Commands[I];
      case C.Action of
        svgMove: FCanvas.MoveTo(C.X, C.Y);
        svgLine,
        svgHLine,
        svgVLine:
          FCanvas.LineTo(C.X, C.Y);
        svgCubic: FCanvas.BezierTo(C.X1, C.Y1, C.X2, C.Y2, C.X, C.Y);
        svgQuadratic: FCanvas.QuadTo(C.X1, C.Y1, C.X, C.Y);
        svgClose: FCanvas.ClosePath;
      end;
    end;
  end;

  procedure Polyline(N: TSvgPolyline);
  var
    P: TPointF;
    I: Integer;
  begin
    for I := 0 to N.Points.Length - 1 do
    begin
      P := N.Points[I];
      if I = 0 then
        FCanvas.MoveTo(P.X, P.Y)
      else
        FCanvas.LineTo(P.X, P.Y);
    end;
    if N is TSvgPolygon then
      FCanvas.ClosePath;
  end;

  procedure Render(N: TSvgNode; Opacity: Float);
  var
    L: TSvgLine absolute N;
    C: TSvgCircle absolute N;
    E: TSvgEllipse absolute N;
    R: TSvgRect absolute N;
    P: TSvgPath absolute N;
    G: TSvgGroup absolute N;
    S: TSvgNode;
    T: TSvgTransform;
    M: IMatrix;
  begin
    if N.Removed then Exit;
    M := nil;
    if N.Transform <> '' then
    begin
      N.BuildTransform(T);
      M := NewMatrix;
      M.Copy(T[0], T[1], T[2], T[3], T[4], T[5]);
      M.Transform(FCanvas.Matrix);
      FCanvas.Matrix.Push;
      FCanvas.Matrix := M;
    end;
    if N is TSvgLine then
    begin
      FCanvas.MoveTo(L.X1, L.Y1);
      FCanvas.LineTo(L.X2, L.Y2);
    end
    else if N is TSvgPolyLine then
      Polyline(TSvgPolyline(N))
    else if N is TSvgCircle then
      FCanvas.Circle(C.X, C.Y, C.R)
    else if N is TSvgEllipse then
      FCanvas.Ellipse(E.X, E.Y, E.W, E.H)
    else if N is TSvgRect then
      if R.R > 0 then
        FCanvas.RoundRect(R.X, R.Y, R.W, R.H, R.R)
      else
        FCanvas.Rect(R.X, R.Y, R.W, R.H)
    else if N is TSvgPath then
      RenderPath(P)
    else if N is TSvgGroup then
    begin
      for S in G do
        Render(S, Opacity * G.Opacity);
      if M <> nil then
        FCanvas.Matrix.Pop;
      Exit;
    end;
    FCanvas.Stroke(colorSteelBlue, PenWidth);
    if M <> nil then
      FCanvas.Matrix.Pop;
  end;

var
  N: TSvgNode;
begin
  for N in FDoc do
    Render(N, 1);
end;

procedure TSvgRender.DrawNodes(PenWidth: Float);
const
  NodeSize = 4;

  procedure RenderPath(Path: TSvgPath);
  var
    F, H: Float;
    C: TSvgCommand;
    I: Integer;
  begin
    F := PenWidth * NodeSize;
    H := F / 2;
    for I := 0 to Path.Commands.Length - 1 do
    begin
      C := Path.Commands[I];
      case C.Action of
        svgMove, svgLine: FCanvas.Rect(C.X - H, C.Y - H, F, F);
        svgCubic:
          begin
            FCanvas.Rect(C.X1 - H, C.Y1 - H, F, F);
            FCanvas.Rect(C.X2 - H, C.Y2 - H, F, F);
            FCanvas.Rect(C.X - H, C.Y - H, F, F);
            FCanvas.MoveTo(C.X1, C.Y1);
            FCanvas.LineTo(C.X, C.Y);
            FCanvas.LineTo(C.X2, C.Y2);
          end;
        svgQuadratic:
          begin
            FCanvas.Rect(C.X1 - H, C.Y1 - H, F, F);
            FCanvas.Rect(C.X - H, C.Y - H, F, F);
            FCanvas.MoveTo(C.X1, C.Y1);
            FCanvas.LineTo(C.X, C.Y);
          end;
      end;
    end;
  end;

  procedure Polyline(N: TSvgPolyline);
  var
    P: TPointF;
    I: Integer;
  begin
    for I := 0 to N.Points.Length - 1 do
    begin
      P := N.Points[I];
      FCanvas.Rect(P.X - PenWidth * NodeSize / 2, P.Y - PenWidth * NodeSize / 2, PenWidth * NodeSize, PenWidth * NodeSize);
    end;
  end;

  procedure Render(N: TSvgNode; Opacity: Float);
  var
    L: TSvgLine absolute N;
    C: TSvgCircle absolute N;
    E: TSvgEllipse absolute N;
    R: TSvgRect absolute N;
    P: TSvgPath absolute N;
    G: TSvgGroup absolute N;
    S: TSvgNode;
    T: TSvgTransform;
    M: IMatrix;
  begin
    if N.Removed then Exit;
    M := nil;
    if N.Transform <> '' then
    begin
      N.BuildTransform(T);
      M := NewMatrix;
      M.Copy(T[0], T[1], T[2], T[3], T[4], T[5]);
      M.Transform(FCanvas.Matrix);
      FCanvas.Matrix.Push;
      FCanvas.Matrix := M;
    end;
    if N is TSvgLine then
    begin
      FCanvas.MoveTo(L.X1, L.Y1);
      FCanvas.LineTo(L.X2, L.Y2);
    end
    else if N is TSvgPolyLine then
      Polyline(TSvgPolyline(N))
    else if N is TSvgCircle then
      FCanvas.Circle(C.X, C.Y, C.R)
    else if N is TSvgEllipse then
      FCanvas.Ellipse(E.X, E.Y, E.W, E.H)
    else if N is TSvgRect then
      if R.R > 0 then
        FCanvas.RoundRect(R.X, R.Y, R.W, R.H, R.R)
      else
        FCanvas.Rect(R.X, R.Y, R.W, R.H)
    else if N is TSvgPath then
      RenderPath(P)
    else if N is TSvgGroup then
    begin
      for S in G do
        Render(S, Opacity * G.Opacity);
      if M <> nil then
        FCanvas.Matrix.Pop;
      Exit;
    end;
    FCanvas.Stroke(colorSteelBlue, PenWidth);
    FCanvas.BeginPath;
    if M <> nil then
      FCanvas.Matrix.Pop;
  end;

var
  N: TSvgNode;
begin
  for N in FDoc do
    Render(N, 1);
end;

procedure TSvgRender.DrawAt(X, Y, Scale, Angle, Size: Float; Options: TSvgRenderOptions = [renderColor]; PenWidth: Float = 1);
var
  P: TPointF;
  S: Float;
begin
  FCanvas.Matrix.Push;
  P := FDoc.ViewBox.MidPoint;
  S :=  Scale / (FDoc.ViewBox.Width / Size);
  if Angle <> 0 then
    FCanvas.Matrix.RotateAt(Angle, P.X, P.Y);
  FCanvas.Matrix.Translate(X - P.X, Y - P.Y);
  FCanvas.Matrix.ScaleAt(S, S, X, Y);
  if renderColor in Options then
    Draw;
  if renderOutlines in Options then
    DrawOutline(PenWidth);
  if renderNodes in Options then
    DrawNodes(PenWidth);
  FCanvas.Matrix.Pop;
end;

function TSvgRender.GetViewBox: TRectF;
begin
  Result := FDoc.ViewBox;
end;

end.

