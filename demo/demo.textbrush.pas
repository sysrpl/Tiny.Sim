unit Demo.TextBrush;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Application,
  Tiny.Graphics,
  Demo.Scene;

const
  LetterCount = 2000;

type
  PLetter = ^TLetter;
  TLetter = record
    Pos: TPointF;
    Size: Single;
    Angle: Single;
    Glyph: Char;
    Time: Double;
  end;

{ TTextBrush demo }

  TTextBrush = class(TDemoScene)
  private
    FHint: string;
    FDown: Boolean;
    FPos: TPointF;
    FMouse: TPointF;
    FLetterIndex: Integer;
    FPhraseIndex: Integer;
    FSerifFont: IFont;
    FFontSize: Float;
    FLetters: array[0..LetterCount - 1] of TLetter;
    FBrush: IBrush;
  protected
    function GetTitle: string; override;
    function GetDescription: string; override;
    function GetGlyph: string; override;
    function RangeGenerate: TRangeInfo; override;
    procedure RangeChanged(NewPosition: Float); override;
    procedure DoMouseDown(var Args: TMouseArgs); override;
    procedure DoMouseMove(var Args: TMouseArgs); override;
    procedure DoMouseUp(var Args: TMouseArgs); override;
    procedure Load; override;
    procedure Render(Width, Height: Integer; const Time: Double); override;
  end;

implementation

const
  Phrase =
    'He was a very silent man by custom. All day he hung round the cove or upon the'+
    ' cliffs with a brass telescope; all evening he sat in a corner of the parlour '+
    'next the fire and drank rum and water very strong. Mostly he would not speak w'+
    'hen spoken to, only look up sudden and fierce and blow through his nose like a'+
    ' fog-horn; and we and the people who came about our house soon learned to let '+
    'him be. Every day when he came back from his stroll he would ask if any seafar'+
    'ing men had gone by along the road. At first we thought it was the want of com'+
    'pany of his own kind that made him ask this question, but at last we began to '+
    'see he was desirous to avoid them. When a seaman did put up at the Admiral Ben'+
    'bow (as now and then some did, making by the coast road for Bristol) he would '+
    'look in at him through the curtained door before he entered the parlour; and h'+
    'e was always sure to be as silent as a mouse when any such was present. For me'+
    ', at least, there was no secret about the matter, for I was, in a way, a share'+
    'r in his alarms. He had taken me aside one day and promised me a silver fourpe'+
    'nny on the first of every month if I would only keep my “weather-eye open for '+
    'a seafaring man with one leg” and let him know the moment he appeared. Often e'+
    'nough when the first of the month came round and I applied to him for my wage,'+
    ' he would only blow through his nose at me and stare me down, but before the w'+
    'eek was out he was sure to think better of it, bring me my four-penny piece, a'+
    'nd repeat his orders to look out for “the seafaring man with one leg.”';

{ TTextBrush }

function TTextBrush.GetTitle: string;
begin
  Result := 'Text Brush';
end;

function TTextBrush.GetDescription: string;
begin
  Result := 'Click and drag on the paper to paint out words. This scene ' +
    'demonstrates accurate text rendering at any angle or scale.'
end;

function TTextBrush.GetGlyph: string;
begin
  Result := '󰊄';
end;

function TTextBrush.RangeGenerate: TRangeInfo;
begin
  Result := TRangeInfo.Create('Font size', FFontSize, 1, 5, 200);
end;

procedure TTextBrush.RangeChanged(NewPosition: Float);
begin
  FFontSize := NewPosition;
end;

procedure TTextBrush.DoMouseDown(var Args: TMouseArgs);
begin
  FDown := True;
  FPos := PointToStudio(Args.X, Args.Y);
  FMouse := PointToStudio(Args.X, Args.Y);
end;

procedure TTextBrush.DoMouseMove(var Args: TMouseArgs);
begin
  if FDown then
    FMouse := PointToStudio(Args.X, Args.Y);
end;

procedure TTextBrush.DoMouseUp(var Args: TMouseArgs);
begin
  FDown := False;
end;

procedure TTextBrush.Load;
var
  I: Integer;
begin
  inherited Load;
  if FFontSize = 0 then
  begin
    FFontSize := 60;
    FHint := 'Click and drag to draw text';
    FSerifFont := Canvas.LoadFont('antique', '../assets/ZenAntique-Regular.ttf');
    FSerifFont.Layout := fontMiddle;
    FSerifFont.Color := colorBlack;
    FBrush := NewBrush(Canvas.LoadBitmap('paper', '../assets/paper.gif'));
  end;
  for I := 0 to LetterCount - 1 do
    FLetters[I].Glyph := ' ';
end;

procedure TTextBrush.Render(Width, Height: Integer; const Time: Double);
var
  L: PLetter;

  procedure CheckLetter;
  var
    D, S: Single;
    G: Char;
  begin
    D := FPos.Distance(FMouse);
    G := Phrase[FPhraseIndex];
    FSerifFont.Size := FFontSize;
    S := Canvas.MeasureText(FSerifFont, G).X;
    if D  > S then
    begin
      L := @FLetters[FLetterIndex];
      L.Angle := ArcTan2(FMouse.Y - FPos.Y, FMouse.X - FPos.X) ;
      L.Size := FSerifFont.Size;
      L.Pos := FPos;
      L.Glyph := G;
      L.Time := Time;
      FPos.X := FPos.x + Cos(L.Angle) * S;
      FPos.Y := FPos.y + Sin(L.Angle) * S;
      FLetterIndex := (FLetterIndex + 1) mod LetterCount;
      FPhraseIndex := (FPhraseIndex + 1) mod (Length(Phrase) + 1);
    end;
  end;

var
  Delta: Double;
  C: TColorF;
  P: TPointF;
  Found: Boolean;
  I: Integer;
begin
  inherited Render(Width, Height, Time);
  ScaleToStudio;
  Canvas.Rect(ClientRect);
  Canvas.Fill(FBrush);
  if FDown then
    CheckLetter;
  Found := False;
  for I := 0 to LetterCount - 1 do
  begin
    L := @FLetters[I];
    if L.Glyph = ' ' then
      Continue;
    Delta := Time - L.Time;
    if Delta > 6 then
      Continue;
    C := colorBlack;
    if Delta > 4.5 then
      C.A := 1 - (Delta - 4.5) / 1.5;
    FSerifFont.Color := C;
    FSerifFont.Size := L.Size;
    P := StudioToPoint(L.Pos.X, L.Pos.Y);
    Canvas.Matrix.Rotate(L.Angle);
    Canvas.Matrix.Translate(P.X, P.Y);
    Canvas.DrawText(FSerifFont, L.Glyph, 0, 0);
    ScaleToStudio;
    Found := True;
  end;
  if not Found then
  begin
    FSerifFont.Size := 80;
    FSerifFont.Color := colorBlack;
    FSerifFont.Align := fontCenter;
    Canvas.DrawText(FSerifFont, FHint, StudioWidth / 2, StudioHeight / 2);
    FSerifFont.Align := fontLeft;
  end;
end;

end.


