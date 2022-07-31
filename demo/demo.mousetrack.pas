unit Demo.MouseTrack;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application,
  Tiny.Graphics,
  Demo.Scene;

{ TMouseTrack }

type
  TMouseTrack = class(TDemoScene)
  private
    FShadow, FArrow, FStripes: IRenderBitmap;
    FBrush: IBitmapBrush;
    FArrowDensity: Float;
    procedure InitBitmaps;
  protected
    function GetTitle: string; override;
    function GetDescription: string; override;
    function GetGlyph: string; override;
    function RangeGenerate: TRangeInfo; override;
    procedure RangeChanged(NewPosition: Float); override;
    procedure Load; override;
    procedure Unload; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
  end;

implementation

{ TMouseTrack }

function TMouseTrack.GetTitle: string;
begin
  Result := 'Mouse Track';
end;

function TMouseTrack.GetDescription: string;
begin
  Result := 'Blue arrows point towards your yellow ball anywhere it goes.';
end;

function TMouseTrack.GetGlyph: string;
begin
  Result := '󰍽';
end;

function TMouseTrack.RangeGenerate: TRangeInfo;
begin
  Result := TRangeInfo.Create('Blue arrow density', FArrowDensity, 0.001, 0.1, 3);
end;

procedure TMouseTrack.RangeChanged(NewPosition: Float);
begin
  FArrowDensity := NewPosition;
end;

procedure TMouseTrack.InitBitmaps;

  procedure RenderArrow;
  begin
    Canvas.Clear;
    Canvas.MoveTo(50, 10);
    Canvas.LineTo(90, 60);
    Canvas.LineTo(70, 60);
    Canvas.LineTo(70, 90);
    Canvas.LineTo(30, 90);
    Canvas.LineTo(30, 60);
    Canvas.LineTo(10, 60);
    Canvas.ClosePath;
  end;

var
  R: TRectF;
begin
  { Init background brush }
  FStripes := Canvas.NewBitmap('stripes', 128, 128);
  FStripes.Bind;
  Canvas.Clear;
  R := FStripes.ClientRect;
  Canvas.Rect(R);
  Canvas.Fill(colorTeal);
  R.X := R.X + 80;
  R.Width := R.Width - 80;
  Canvas.Rect(R);
  Canvas.Fill(colorAquamarine);
  FStripes.Unbind;
  FBrush := NewBrush(FStripes);
  { Init arrow shadow }
  FShadow := Canvas.NewBitmap('shadow', 100, 100);
  FShadow.Bind;
  RenderArrow;
  Canvas.Fill($60000000);
  FShadow.Unbind;
  { Init arrow }
  FArrow := Canvas.NewBitmap('arrow', 100, 100);
  FArrow.Bind;
  RenderArrow;
  Canvas.Fill(colorBlue, True);
  Canvas.Stroke(colorDarkBlue, 8);
  FArrow.Unbind;
end;

procedure TMouseTrack.Load;
begin
  inherited Load;
  if FArrowDensity = 0 then
  begin
    FArrowDensity := 1;
    InitBitmaps;
  end;
  Mouse.Visible := False;
end;

procedure TMouseTrack.Unload;
begin
  Mouse.Visible := True;
  inherited Unload;
end;

procedure TMouseTrack.Render(Width, Height: Integer; const Time: Double);
var
  Angle: Single;
  S, D: TRectF;
  A, B: TPointF;
  X, Y: Integer;
begin
  inherited Render(Width, Height, Time);
  ScaleToStudio;
  Canvas.Rect(ClientRect);
  FBrush.Offset := NewPointF(StudioWidth / 2, StudioHeight / 2);
  FBrush.Angle := Time / 4;
  Canvas.Fill(FBrush);
  S := FArrow.ClientRect;
  D := S;
  D.Y := D.Height / -2;
  D.X := D.Width / -2;
  B := NewPointF(Mouse.X * StudioWidth / Width, Mouse.Y * StudioHeight / Height);
  B := StudioToPoint(B);
  for X := 0 to 15 * 20 do
  begin
    if StudioToPoint(X * 150 * FArrowDensity, 0).X > StudioWidth + 100 then Continue;
    for Y := 0 to 7 * 20 do
    begin
      if StudioToPoint(0, Y * 150 * FArrowDensity).Y > StudioHeight + 100 then Continue;
      ScaleToStudio;
      A := StudioToPoint(X * 150 * FArrowDensity, Y * 150 * FArrowDensity);
      Angle := ArcTan2(A.Y - B.Y, A.X - B.X) - Pi / 2;
      Canvas.Matrix.Rotate(Angle);
      A := StudioToPoint(X * 150 * FArrowDensity + 20, Y * 150 * FArrowDensity + 10);
      Canvas.Matrix.Translate(A.X, A.Y);
      Canvas.DrawImage(FShadow, S, D);
      ScaleToStudio;
      Canvas.Matrix.Rotate(Angle);
      A := StudioToPoint(X * 150 * FArrowDensity, Y * 150 * FArrowDensity);
      Canvas.Matrix.Translate(A.X, A.Y);
      Canvas.DrawImage(FArrow, S, D);
    end;
  end;
  ScaleToStudio;
  B := PointToStudio(B);
  Canvas.Circle(B.X, B.Y, 25 + Sin(Time * 8) * 4);
  Canvas.Fill(colorYellow, True);
  Canvas.Stroke(colorDarkGoldenRod, 5);
  DrawSceneInfo;
end;

end.


