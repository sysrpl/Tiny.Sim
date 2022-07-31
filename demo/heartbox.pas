unit HeartBox;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Application,
  Tiny.Graphics,
  Math;

{ THeartBox }

type
  THeartBox = class(TScene)
  private
    FPen: IPen;
    procedure DrawHeart(X, Y, W, H: Single);
  protected
    procedure Load; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
  end;

implementation

{ THeartBox }

procedure THeartBox.Load;
begin
  inherited Load;
  Font := Canvas.LoadFont('roboto', '../assets/Roboto-Medium.ttf');
  Font.Size := 30;
  FPen := NewPen(colorPink, 1);
  FPen.LineJoin := joinRound;
end;

procedure THeartBox.DrawHeart(X, Y, W, H: Single);
var
  T: Single;
begin
  T := H * 0.3;
  Canvas.MoveTo(X, Y + H);
  Canvas.BezierTo(X, Y + (H + T) / 2, X + W / 2, Y + (H + T) / 2, X + W / 2, Y + T);
  Canvas.BezierTo(X + W / 2, Y, X, Y, X, Y + T);
  Canvas.BezierTo(X, Y, X - W / 2, Y, X - W / 2, Y + T);
  Canvas.BezierTo(X - W / 2, Y + (H + T) / 2, X, Y + (H + T) / 2, X, Y + H);
  Canvas.ClosePath;
end;

procedure THeartBox.Render(Width, Height: Integer; const Time: Double);
const
  SX = 60;
  SY = 68;
var
  C: TColorF;
  T: Double;
  H : Single;
  I: Integer;
begin
  inherited Render(Width, Height, Time);
  ScaleToStudio;
  Canvas.BlendMode := blendAdditive;
  T := Time - Trunc(Time);
  for I := 0 to 100 do
  begin
    H := SY * (I + 1) + T * SY;
    DrawHeart(StudioWidth / 2, StudioHeight / 2 - H * 0.3 - I * 3.5, SX * (I + 1) + T * SX, H);
    C := NewHSL(Time / 5 - I / 200, 0.8, 0.4);
    if I = 0 then
      C.A := T
    else
      C.A := 1;
    C.A := C.A * 0.5;
    FPen.Width := 1 + T / 2 + I / 2;
    FPen.Color := C;
    Canvas.Stroke(FPen);
  end;
  Canvas.Matrix.RotateAt(0.3, 300, 500);
  DrawHeart(300, 500, 200, 220);
  FPen.Width := 15;
  Canvas.Stroke(FPen);
  ScaleToStudio;
  DrawSceneInfo;
end;

end.

