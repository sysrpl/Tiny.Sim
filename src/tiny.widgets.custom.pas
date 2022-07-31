unit Tiny.Widgets.Custom;

{$i tiny.inc}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Graphics,
  Tiny.Widgets,
  Tiny.Application,
  Tiny.Widgets.Themes;

{ TPerformanceGraph }

type
  TPerformanceGraph = class(TCustomWidget)
  private
    FData: TArrayList<LongWord>;
    FFrameRate: LongWord;
    FFrame: LongWord;
    FTime: Double;
    FStart: Double;
    FLast: Double;
    FMax: LongWord;
    FFrameMeasure: Double;
    FStep: Float;
    procedure RenderData(Canvas: ICanvas; Rect: TRectF);
  protected
    procedure Paint; override;
  public
    constructor Create(Parent: TWidget; const Name: string = ''); override;
    procedure Update;
    property FrameMeasure: Double read FFrameMeasure write FFrameMeasure;
    property Step: Float read FStep write FStep;
  end;

implementation

{ TPerformanceGraph }

constructor TPerformanceGraph.Create(Parent: TWidget; const Name: string = '');
begin
  inherited Create(Parent, Name);
  FFrameMeasure := 1 / 6;
  FStep := 7.5;
end;

procedure TPerformanceGraph.Update;
var
  Current: Double;
  M: TMainWidget;
begin
  M := Main;
  if M = nil then
    Exit;
  if FStart = 0 then
  begin
    FStart := M.Time;
    FLast := FStart;
  end;
  Current := M.Time;
  FFrame := FFrame + 1;
  if Current - FLast > FFrameMeasure then
  begin
    FFrameRate := Round(FFrame * (1 / (Current - FLast)));
    FData.Push(FFrameRate);
    FLast := FLast + FFrameMeasure;
    while (FLast + FFrameMeasure < Current) do
    begin
      FLast := FLast + FFrameMeasure;
      FFrameRate := 0;
      FData.Push(FFrameRate);
    end;
    FFrame := 0;
  end;
  FTime := Current - FStart;
  while FData.Length > 5000 do
    FData.Delete(0);
end;

procedure TPerformanceGraph.RenderData(Canvas: ICanvas; Rect: TRectF);
var
  Offset, X, Y, X0, Y0: Float;
  I, J: Integer;
  C: TColorF;
begin
  Canvas.RoundRect(Rect, 4);
  C := Color(colorFace);
  Canvas.Fill(C);
  Rect.Inflate(0, -4);
  Offset := (FTime - FLast + FStart) / FFrameMeasure * FStep - FStep;
  X := Rect.Left;
  Y := Rect.Bottom;
  if FData.Length < 2 then
    Exit;
  FMax := 120;
  I := 0;
  J := FData.Length;
  for I := 0 to FData.Length  - 1 do
  begin
    Dec(J);
    if FData[J] > FMax then
      FMax := FData[J];
    if I * FStep > Rect.Width then
      Break;
  end;
  Canvas.MoveTo(X, Y - FData.Last / FMax * Rect.Height);
  J := FData.Length - 1;
  for I := 1 to FData.Length  - 1 do
  begin
    Dec(J);
    X0 := X + I * FStep + Offset;
    if X0 < Rect.Left then
      X0 := Rect.Left;
    if X0 > Rect.Right then
      X0 := Rect.Right;
    Y0 := Y - FData[J] / FMax * Rect.Height;
    if Y0 < Rect.Top then
      Y0 := Rect.Top;
    Canvas.LineTo(X0, Y0);
    X0 := X + I * FStep + Offset;
    if X0 > Rect.Right then
      Break;
  end;
  Canvas.Stroke(Color(colorActive));
  Rect.Inflate(0, 4);
  Canvas.RoundRect(Rect, 4);
  Canvas.Stroke(Color(colorBorder));
end;

procedure TPerformanceGraph.Paint;
var
  T: TCanvasTheme;
  C: ICanvas;
  R: TRectF;
  F: IFont;
begin
  if Computed.Theme is TCanvasTheme then
  begin
    T := TCanvasTheme(Computed.Theme);
    C := T.Canvas;
    R := Computed.Bounds;
    R.X := R.X + 60;
    R.Width := R.Width - 60;
    RenderData(C, R);
    F := T.Font;
    F.Size := 10;
    F.Align := fontRight;
    F.Color := Color(colorText);
    with R.Sector(4) do
    begin
      C.DrawText(F, IntToStr(FMax) + ' Max' , X - 4, Y - 15);
      C.DrawText(F, IntToStr(Application.FrameRate) + ' FPS' , X - 4, Y);
      if Application.VSync then
        C.DrawText(F, 'vsync', X - 4, Y + 15)
      else
        C.DrawText(F, 'vsync off', X - 4, Y + 15);
    end;
  end;
end;

end.

