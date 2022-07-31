unit Demo.Synthwave;

{$mode delphi}

interface

uses
  Tiny.System, Tiny.Types, Tiny.Geometry, Tiny.Application, Tiny.Graphics,
  Demo.Scene;

{ TSynthwave }

type
  TPoints = TArrayList<TVec3>;
  TPointList = TArrayList<TPoints>;

  TSynthwave = class(TDemoScene)
  private
    FPoints: TPointList;
    FLast: Double;
    FSpeed: Float;
  protected
    function GetTitle: string; override;
    function GetDescription: string; override;
    function GetGlyph: string; override;
    function RangeGenerate: TRangeInfo; override;
    procedure RangeChanged(NewPosition: Float); override;
    procedure Load; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
  end;

implementation

const
  ListLength = 30;
  PointsLength = 60;
  Spacing = 40;
  Offset = PointsLength * Spacing / 2;
  Depth = 300;

function TSynthwave.GetTitle: string;
begin
  Result := 'Synthwave';
end;

function TSynthwave.GetDescription: string;
begin
  Result := 'Synthwave stems from a microgenre of electronic music drawing predominantly from 1980s culture.';
end;

function TSynthwave.GetGlyph: string;
begin
  Result := '󱑽';
end;

function TSynthwave.RangeGenerate: TRangeInfo;
begin
  Result := TRangeInfo.Create('Driving speed', FSpeed, 0.001, 0, 10);
end;

procedure TSynthwave.RangeChanged(NewPosition: Float);
begin
  FSpeed := NewPosition;
end;

procedure TSynthwave.Load;
var
  PointData: TPoints;
  A, I, J: Integer;
begin
  inherited Load;
  { Reset the speed timer }
  FLast := 0;
  if FPoints.Length = 0 then
    FSpeed := 1;
  FPoints.Length := ListLength;
  { Generate our ground geometry }
  for I := 0 to ListLength - 1 do
  begin
    PointData := nil;
    PointData.Length := PointsLength;
    for J := 0 to PointsLength - 1 do
    begin
      A := Abs(J - ListLength);
      PointData[J] := Vec3(J * Spacing, Random * -(A * A) + ListLength, I * -10);
    end;
    FPoints[I] := PointData;
  end;
end;

procedure TSynthwave.Render(Width, Height: Integer; const Time: Double);
const
{ Skip allows for reduction in overall scene geometry }
  Skip = 6;
  colorDusk = $FF6E073A;
  colorCorona = $00FF0000;
var
  Brush: IGradientBrush;
  Rect: TRectF;
  Delta: Double;
  Reset: Boolean;
  A, B: Float;
  I, J: Integer;
begin
  inherited Render(Width, Height, Time);
  { Draw background }
  Brush := NewBrush(NewPointF(0, Height * 0.6), NewPointF(0, 0));
  Brush.NearStop.Color := colorDusk;
  Brush.FarStop.Color := colorBlack;
  Canvas.Rect(ClientRect);
  Canvas.Fill(Brush);
  { Draw corona }
  Rect := NewRectF(350, 350);
  Rect.X := Width / 2 - Rect.Width / 2;
  Rect.Y := Height / 2 - Rect.Height / 2;
  Brush := NewBrush(Rect);
  Brush.FarStop.Offset := 0.9;
  Brush.NearStop.Color := colorOrange;
  Brush.NearStop.Offset := 0.1;
  Brush.FarStop.Color := colorCorona;
  Canvas.Circle(Width / 2, Height / 2, 350);
  Canvas.Fill(Brush);
  { Draw sun }
  Brush := NewBrush(NewPointF(0, Height * 0.7), NewPointF(0, Height * 0.3));
  Brush.NearStop.Color := colorRed;
  Brush.FarStop.Color := colorYellow;
  Canvas.Circle(Width / 2, Height / 2, 225);
  Canvas.Fill(Brush);
  { Adjust for speed }
  Delta := (Time - FLast) * 50 * FSpeed;
  FLast := Time;
  { Check if we need to rebuild next row }
  for I := 0 to ListLength - 1 do
  begin
    Reset := False;
    for J := Skip to PointsLength - (Skip + 1) do
    begin
      FPoints[I].Items[J].Z := FPoints[I].Items[J].Z - 0.5 * Delta;
      if FPoints[I].Items[J].Z < -Depth then
      begin
        Reset := True;
        Break;
      end;
    end;
    { Rebuild row }
    if Reset then
    begin
      FPoints.Move(ListLength - 1, 0);
      for J := Skip to PointsLength - (Skip + 1) do
      begin
        A := Abs(J - ListLength);
        FPoints[0].Items[J].Z := 0;
        FPoints[0].Items[J].Y := Random * -(A * A) + ListLength;
      end;
    end;
  end;
  { Draw the ground grid }
  Canvas.Matrix.Translate(Width / 2, Height / 2);
  for I := 0 to ListLength - 2 do
    for J := Skip to PointsLength - (Skip + 2) do
    begin
      A := Depth / (Depth + FPoints[I][J].Z);
      B := Depth / (Depth + FPoints[I + 1][J].Z);
      Canvas.MoveTo((FPoints[I][J].X - Offset) * A, FPoints[I][J].Y * A);
      Canvas.LineTo((FPoints[I][J + 1].X - Offset) * A, FPoints[I][J + 1].Y * A);
      Canvas.LineTo((FPoints[I + 1][J + 1].X - Offset) * B, FPoints[I + 1][J + 1].Y * B);
      Canvas.LineTo((FPoints[I + 1][J].X - Offset) * B, FPoints[I + 1][J].Y * B);
      { Fill and stroke }
      Canvas.Fill(NewColorF(0, 0, 0, -FPoints[I][J].Z / 100), True);
      A := Depth + FPoints[I][J].Z;
      Canvas.Stroke(NewColorF((250 - A) / 255, 0, (50 + A) / 255, 1 - A / 300), 2);
    end;
end;

end.

