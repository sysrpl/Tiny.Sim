unit Tiny.Widgets.Themes;

{$i tiny.inc}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Graphics,
  Tiny.Widgets;

{ TCanvasTheme }

type
  TCanvasTheme = class(TTheme)
  private
    FCanvas: ICanvas;
    FFont: IFont;
    FTitle: IFont;
    FGlyph: IFont;
    FPen: IPen;
    FTitleHeight: Float;
    FFontHeight: Float;
    FGlyphHeight: Float;
    FHintBrush: ILinearGradientBrush;
  protected
    function MeasureText(Font: IFont; const Text: string): TPointF;
    function MeasureMemo(Font: IFont; const Text: string; Width: Float): Float;
    procedure DrawText(Font: IFont; const Text: string; X, Y: Float);
    procedure DrawTextMemo(Font: IFont; const Text: string; X, Y, Width: Float);
    procedure DrawCaption(Widget: TWidget; const Rect: TRectF); virtual;
    procedure DrawEdit(Edit: TEdit); virtual; abstract;
    procedure DrawButton(Button: TPushButton); virtual; abstract;
    procedure DrawGlyphButton(GlyphButton: TGlyphButton); virtual; abstract;
    procedure DrawGlyphImage(Widget: TGlyphImage); virtual; abstract;
    procedure DrawCheckBox(CheckBox: TCheckBox); virtual; abstract;
    procedure DrawSlider(Widget: TSlider); virtual; abstract;
    procedure DrawLabel(ALabel: TLabel); virtual; abstract;
    procedure DrawWindow(Window: TWindow); virtual; abstract;
    procedure DrawContainer(Widget: TContainerWidget); virtual; abstract;
  protected
    function FontSize: Float; virtual; abstract;
    function GlyphSize: Float; virtual; abstract;
    function TitleSize: Float; virtual; abstract;
    procedure Init(Canvas: ICanvas); virtual;
    procedure Fixup;
    property Pen: IPen read FPen;
  public
    procedure Render(Widget: TWidget); override;
    procedure RenderHint(Widget: TWidget; Opacity: Float); override;
    property Canvas: ICanvas read FCanvas;
    property Font: IFont read FFont write FFont;
    property FontHeight: Float read FFontHeight;
    property Glyph: IFont read FGlyph write FGlyph;
    property Title: IFont read FTitle write FTitle;
  end;

{ TDefaultTheme }

  TArcDarkTheme = class(TCanvasTheme)
  protected
    function FontSize: Float; override;
    function GlyphSize: Float; override;
    function TitleSize: Float; override;
    procedure Init(Canvas: ICanvas); override;
    procedure DrawEdit(Widget: TEdit); override;
    procedure DrawButton(Widget: TPushButton); override;
    procedure DrawGlyphButton(Widget: TGlyphButton); override;
    procedure DrawGlyphImage(Widget: TGlyphImage); override;
    procedure DrawCheckBox(Widget: TCheckBox); override;
    procedure DrawSlider(Widget: TSlider); override;
    procedure DrawLabel(Widget: TLabel); override;
    procedure DrawWindow(Widget: TWindow); override;
    procedure DrawContainer(Widget: TContainerWidget); override;
  public
    function CalcColor(Widget: TWidget; Color: TThemeColor): LongWord; override;
    function CalcSize(Widget: TWidget; Part: TThemePart): TSizeF; override;
  end;

  TChicagoTheme = class(TCanvasTheme)
  private
    FFocus: IPen;
  protected
    function FontSize: Float; override;
    function GlyphSize: Float; override;
    function TitleSize: Float; override;
    procedure DrawCaption(Widget: TWidget; const Rect: TRectF); override;
    procedure DrawFocus(Widget: TWidget; const Rect: TRectF);
    procedure DrawThinBorder(Widget: TWidget; const Rect: TRectF);
    procedure DrawSunken(Widget: TWidget; const Rect: TRectF);
    procedure DrawThickBorder(Widget: TWidget);
    procedure Init(Canvas: ICanvas); override;
    procedure DrawEdit(Widget: TEdit); override;
    procedure DrawButton(Widget: TPushButton); override;
    procedure DrawGlyphButton(Widget: TGlyphButton); override;
    procedure DrawGlyphImage(Widget: TGlyphImage); override;
    procedure DrawCheckBox(Widget: TCheckBox); override;
    procedure DrawSlider(Widget: TSlider); override;
    procedure DrawLabel(Widget: TLabel); override;
    procedure DrawWindow(Widget: TWindow); override;
    procedure DrawContainer(Widget: TContainerWidget); override;
  public
    function CalcColor(Widget: TWidget; Color: TThemeColor): LongWord; override;
    function CalcSize(Widget: TWidget; Part: TThemePart): TSizeF; override;
  end;

{ TGraphiteTheme }

  TGraphiteTheme = class(TCanvasTheme)
  private
    FBrush: ILinearGradientBrush;
  protected
    procedure Init(Canvas: ICanvas); override;
    function FontSize: Float; override;
    function GlyphSize: Float; override;
    function TitleSize: Float; override;
    procedure DrawCaption(Widget: TWidget; const Rect: TRectF); override;
    procedure DrawEdit(Widget: TEdit); override;
    procedure DrawButton(Widget: TPushButton); override;
    procedure DrawGlyphButton(Widget: TGlyphButton); override;
    procedure DrawGlyphImage(Widget: TGlyphImage); override;
    procedure DrawCheckBox(Widget: TCheckBox); override;
    procedure DrawSlider(Widget: TSlider); override;
    procedure DrawLabel(Widget: TLabel); override;
    procedure DrawWindow(Widget: TWindow); override;
    procedure DrawContainer(Widget: TContainerWidget); override;
  public
    function CalcColor(Widget: TWidget; Color: TThemeColor): LongWord; override;
    function CalcSize(Widget: TWidget; Part: TThemePart): TSizeF; override;
  end;

function NewTheme(Canvas: ICanvas; ThemeClass: TThemeClass): TTheme;

implementation

function NewTheme(Canvas: ICanvas; ThemeClass: TThemeClass): TTheme;
begin
  Result := ThemeClass.Create;
  if Result is TCanvasTheme then
  begin
    TCanvasTheme(Result).Init(Canvas);
    TCanvasTheme(Result).Fixup;
  end;
end;

{ TCanvasTheme }

procedure TCanvasTheme.Init(Canvas: ICanvas);
begin
  FCanvas := Canvas;
  FPen := NewPen(colorBlack);
  FHintBrush := NewBrush(NewPointF(0, 0), NewPointF(10, 0));
end;

procedure TCanvasTheme.Fixup;
begin
  Font.Size := FontSize;
  Title.Size := TitleSize;
  Glyph.Size := GlyphSize;
  FFontHeight := Round(Canvas.MeasureText(Font, 'Wg').Y);
  FTitleHeight := Round(Canvas.MeasureText(Title, 'Wg').Y);
  FGlyphHeight := Round(Canvas.MeasureText(Glyph, 'Wg').Y);
end;

function TCanvasTheme.MeasureText(Font: IFont; const Text: string): TPointF;
begin
  Result := Canvas.MeasureText(Font, Text);
end;

function TCanvasTheme.MeasureMemo(Font: IFont; const Text: string; Width: Float): Float;
begin
  Result := Canvas.MeasureMemo(Font, Text, Width);
end;

procedure TCanvasTheme.DrawText(Font: IFont; const Text: string; X, Y: Float);
begin
  Canvas.DrawText(Font, Text, X, Y);
end;

procedure TCanvasTheme.DrawTextMemo(Font: IFont; const Text: string; X, Y, Width: Float);
begin
  Canvas.DrawTextMemo(Font, Text, X, Y, Width);
end;

procedure TCanvasTheme.DrawCaption(Widget: TWidget; const Rect: TRectF);
var
  S, W: Float;
  F: IFont;
  P: TPointF;
begin
  S := 0;
  W := 0;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Font.Size := FontSize;
  Title.Align := fontCenter;
  Title.Layout := fontMiddle;
  Title.Size := TitleSize;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  Glyph.Size := GlyphSize;
  if Widget is TWindow then
  begin
    F := Title;
    F.Color := CalcColor(Widget, colorTitle);
  end
  else if Widget is TGlyphButton then
  begin
    F := Glyph;
    F.Color := CalcColor(Widget, colorText);
  end
  else if Widget is TGlyphImage then
  begin
    F := Glyph;
    F.Color := CalcColor(Widget, colorText);
    S := F.Size;
    F.Size := Rect.Height;
  end
  else
  begin
    F := Font;
    F.Color := CalcColor(Widget, colorText);
  end;
  if Widget is TLabel then
  begin
    F.Align := fontLeft;
    W := TLabel(Widget).MaxWidth;
    if W < 1 then
      P.Y := Rect.MidPoint.Y
    else
      P.Y := Rect.Top + Round(FFontHeight / 2) - 5;
    P.X := Rect.X;
  end
  else
  begin
    F.Align := fontCenter;
    P := Rect.MidPoint;
  end;
  if W < 1 then
    DrawText(F, Widget.Text, P.X, P.Y)
  else
  begin
    F.Layout := fontTop;
    DrawTextMemo(F, Widget.Text, P.X, P.Y, W);
  end;
  if S > 0 then
    F.Size := S;
end;

procedure TCanvasTheme.Render(Widget: TWidget);
begin
  if Widget is TEdit then
    DrawEdit(TEdit(Widget))
  else if Widget is TPushButton then
    DrawButton(TPushButton(Widget))
  else if Widget is TGlyphButton then
    DrawGlyphButton(TGlyphButton(Widget))
  else if Widget is TGlyphImage then
    DrawGlyphImage(TGlyphImage(Widget))
  else if Widget is TCheckBox then
    DrawCheckBox(TCheckBox(Widget))
  else if Widget is TSlider then
    DrawSlider(TSlider(Widget))
  else if Widget is TLabel then
    DrawLabel(TLabel(Widget))
  else if Widget is TWindow then
    DrawWindow(TWindow(Widget))
  else if Widget is TContainerWidget then
    DrawContainer(TContainerWidget(Widget))
end;

procedure TCanvasTheme.RenderHint(Widget: TWidget; Opacity: Float);
var
  R: TRectF;
  P: TPointF;
  S: TSizeF;
  Brush, Pen: TColorF;

  procedure NearAbove;
  begin
    R.Y := R.Top - 34;
    R.X := R.X + S.X / 2 - 12;
    if R.X < 0 then
      R.X := 0;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y * 2;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left + 24, R.Bottom);
    Canvas.LineTo(R.Left + 16, R.Bottom + 8);
    Canvas.LineTo(R.Left + 8, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
  end;

  procedure NearBelow;
  begin
    R.Y := R.Bottom + 5;
    R.X := R.X + S.X / 2 - 12;
    if R.X < 0 then
      R.X := 0;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y * 2;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Left + 8, R.Top);
    Canvas.LineTo(R.Left + 16, R.Top - 8);
    Canvas.LineTo(R.Left + 24, R.Top);
  end;

  procedure CenterAbove;
  begin
    R.Y := R.Top - 34;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y * 2;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.MidPoint.X + 8, R.Bottom);
    Canvas.LineTo(R.MidPoint.X, R.Bottom + 8);
    Canvas.LineTo(R.MidPoint.X - 8, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
  end;

  procedure CenterBelow;
  begin
    R.Y := R.Bottom + 5;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y * 2;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.MidPoint.X - 8, R.Top);
    Canvas.LineTo(R.MidPoint.X, R.Top - 8);
    Canvas.LineTo(R.MidPoint.X + 8, R.Top);
  end;

  procedure FarAbove;
  begin
    R.Y := R.Top - 34;
    R.X := R.X - S.X / 2 + 12;
    if R.Right > Widget.Main.Width - 12 then
      R.X := Widget.Main.Width - S.X - 28;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y * 2;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Right - 8, R.Bottom);
    Canvas.LineTo(R.Right - 16, R.Bottom + 8);
    Canvas.LineTo(R.Right - 24, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
  end;

  procedure FarBelow;
  begin
    R.Y := R.Bottom + 5;
    R.X := R.X - S.X / 2 + 12;
    if R.Right > Widget.Main.Width - 12 then
      R.X := Widget.Main.Width - S.X - 28;
    R.Height := 20;
    R.X := R.MidPoint.X;
    R.X := R.X - S.X / 2 - 5;
    R.Width := S.X + 10;
    R.Height := S.Y * 2;
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 24, R.Top);
    Canvas.LineTo(R.Right - 16, R.Top - 8);
    Canvas.LineTo(R.Right - 8, R.Top);
  end;

var
  Align: TWidgetAlign;
  Layout: TWidgetAlign;
  A: TRectF;
begin
  Font.Size := FontSize;
  Font.Align := fontCenter;
  R := Widget.Computed.Bounds.Round;
  P := R.MidPoint;
  S := MeasureText(Font, Widget.Hint);
  Align := alignCenter;
  if P.X - S.X / 2 < 20 then
    Align := alignNear
  else if P.X + S.X / 2 > Widget.Main.Width - 20 then
    Align := alignFar;
  Layout := alignFar;
  if P.Y + Widget.Height / 2 + S.Y > Widget.Main.Height - 20 then
    Layout := alignNear;
  Font.Size := FontSize;
  A := R;
  R.Move(6, 4);
  case Align of
    alignNear:
      if Layout = alignFar then
        NearBelow
      else
        NearAbove;
    alignCenter:
      if Layout = alignFar then
        CenterBelow
      else
        CenterAbove;
    alignFar:
      if Layout = alignFar then
        FarBelow
      else
        FarAbove;
  end;
  Canvas.ClosePath;
  Brush := $30000000;
  Brush.A := Brush.A * Opacity;
  Canvas.Fill(Brush);
  R := A;
  case Align of
    alignNear:
      if Layout = alignFar then
        NearBelow
      else
        NearAbove;
    alignCenter:
      if Layout = alignFar then
        CenterBelow
      else
        CenterAbove;
    alignFar:
      if Layout = alignFar then
        FarBelow
      else
        FarAbove;
  end;
  Canvas.ClosePath;
  Pen := $404040;
  Pen.A := Opacity;
  Brush := $EDEDAF;
  Brush.A := Opacity;
  FHintBrush.A := R.Sector(2);
  FHintBrush.NearStop.Color := Brush;
  Brush := $C0C080;
  Brush.A := Opacity;
  FHintBrush.B := R.Sector(8);
  FHintBrush.FarStop.Color := Brush;
  Canvas.Fill(FHintBrush, True);
  Canvas.Stroke(Pen);
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Font.Color := Pen;
  with R.MidPoint do
    DrawText(Font, Widget.Hint, X, Y + 1);
end;

{ TArcDarkTheme }

function TArcDarkTheme.FontSize: Float;
begin
  Result := 14;
end;

function TArcDarkTheme.GlyphSize: Float;
begin
  Result := 22;
end;

function TArcDarkTheme.TitleSize: Float;
begin
  Result := 15;
end;

procedure TArcDarkTheme.Init(Canvas: ICanvas);
begin
  inherited Init(Canvas);
  Font := Canvas.LoadFont('Ubuntu', '../assets/Ubuntu-R.ttf');
  Font.Size := FontSize;
  Font.Color := colorWhite;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Glyph := Canvas.LoadFont('glyph', '../assets/materialdesignicons-webfont.ttf');
  Glyph.Size := GlyphSize;
  Glyph.Color := colorText;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  Title := Canvas.LoadFont('Ubuntu-M', '../assets/Ubuntu-M.ttf');
  Title.Size := TitleSize;
  Title.Color := colorText;
  Title.Align := fontCenter;
  Title.Layout := fontMiddle;
end;

function TArcDarkTheme.CalcColor(Widget: TWidget; Color: TThemeColor): LongWord;
var
  A: LongWord;
begin
  if Widget.Computed.Enabled then
    case Color of
      colorBase: Result := $383C4A;
      colorFace: Result := $444A58;
      colorBorder: Result := $2B2E39;
      colorActive: Result := $5294E2;
      colorPressed: Result := $5294E2;
      colorSelected: Result := $FFFFFF;
      colorHot: Result := $4E5467;
      colorCaption: Result := $2F343F;
      colorTitle: Result := $C0C0C0;
      colorText: Result := $D0D0D0;
    else
      Result := 0;
    end
  else
    case Color of
      colorBase: Result := $383C4A;
      colorFace: Result := $3E4350;
      colorBorder: Result := $313541;
      colorActive: Result := $5294E2;
      colorPressed: Result := $466C9D;
      colorSelected: Result := $FFFFFF;
      colorHot: Result := $3E4350;
      colorCaption: Result := $2F343F;
      colorTitle: Result := $7D818B;
      colorText: Result := $7D818B;
    end;
  if Color = colorSelected then
    A := Round($FF * Widget.Computed.Opacity * 0.2) shl 24
  else
    A := Round($FF * Widget.Computed.Opacity) shl 24;
  Result := Result or A;
end;

function TArcDarkTheme.CalcSize(Widget: TWidget; Part: TThemePart): TSizeF;
var
  M: Float;
begin
  Result := NewPointF(0, 0);
  { Entire widget defaiult sizes }
  if Part = tpEverything then
  begin
    if Widget is TSpacer then
      Result := NewPointF(8, 8)
    else if Widget is TWindow then
      Result := NewPointF(400, 300)
    else if Widget is TContainerWidget then
      Result := NewPointF(10, 10)
    else if Widget is TPushButton then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X;
      Result.Y := 30;
      if Result.X < 80 then
        Result.X := 80;
    end
    else if Widget is TGlyphButton then
      Result := NewPointF(30, 30)
    else if Widget is TGlyphImage then
      Result := NewPointF(48, 48)
    else if Widget is TCheckBox then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X + 24;
      Result.Y := 24;
    end
    else if Widget is TSlider then
      Result := NewPointF(150, 20)
    else if Widget is TLabel then
    begin
      M := TLabel(Widget).MaxWidth;
      { Realted to issue detailed in TCanvas.MeasureMemo }
      Font.Size := FontSize;
      Font.Align := fontLeft;
      Font.Layout := fontMiddle;
      if M < 1 then
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        Result.Y := Result.Y + 4;
      end
      else
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        if Result.X > M then
        begin
          Result.X := M + 4;
          Result.Y := MeasureMemo(Font, Widget.Text, M);
        end;
        Result.Y := Result.Y + 4;
      end;
    end;
    Exit;
  end;
  { Indentation }
  if Part = tpIndent then
  begin
    Result := NewPointF(16, 0);
    Exit;
  end;
  { TWindow parts }
  if Widget is TWindow then
    case Part of
      tpCaption:
        begin
          Result.X := Widget.Width;
          Result.Y := 22;
        end;
    else
    end
  { TSlider parts }
  else if Widget is TSlider then
    case Part of
      tpThumb: Result := NewPointF(14, 14);
    else
    end
  { TCheckBox parts }
  else if Widget is TCheckBox then
    case Part of
      tpNode:
          Result := NewPointF(18, 18);
      tpCaption:
        begin
          Result := MeasureText(Font, '[ ' + Widget.Text + ' ]');
          Result.X := Result.X + 6;
          Result.Y := Result.Y + 6;
        end;
    else
    end;
end;

procedure TArcDarkTheme.DrawEdit(Widget: TEdit);
begin
end;

procedure TArcDarkTheme.DrawButton(Widget: TPushButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 3);
  if wsHot in Widget.State then
    if wsPressed in Widget.State then
      Canvas.Fill(CalcColor(Widget, colorPressed), True)
    else
      Canvas.Fill(CalcColor(Widget, colorHot), True)
  else
    Canvas.Fill(CalcColor(Widget, colorFace), True);
  Canvas.Stroke(CalcColor(Widget, colorBorder));
  if wsSelected in Widget.State then
  begin
    R.Inflate(-3, -3);
    Canvas.RoundRect(R, 3);
    Canvas.Stroke(CalcColor(Widget, colorSelected));
  end;
  DrawCaption(Widget, R);
end;

procedure TArcDarkTheme.DrawGlyphButton(Widget: TGlyphButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 3);
  if Widget.Down then
    Canvas.Fill(CalcColor(Widget, colorPressed), True)
  else if wsHot in Widget.State then
    if wsPressed in Widget.State then
      Canvas.Fill(CalcColor(Widget, colorPressed), True)
    else
      Canvas.Fill(CalcColor(Widget, colorHot), True)
  else
    Canvas.Fill(CalcColor(Widget, colorFace), True);
  Canvas.Stroke(CalcColor(Widget, colorBorder));
  DrawCaption(Widget, R);
end;

procedure TArcDarkTheme.DrawGlyphImage(Widget: TGlyphImage);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

procedure TArcDarkTheme.DrawCheckBox(Widget: TCheckBox);
var
  B, R: TRectF;
  P: TPointF;
begin
  B := Widget.Computed.Bounds.Round;
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  if wsToggled in Widget.State then
  begin
    Canvas.Fill(CalcColor(Widget, colorPressed));
    if Widget.Round then
    begin
      R.Inflate(-4, -4);
      Canvas.RoundRect(R, 7);
      Canvas.Fill(CalcColor(Widget, colorFace));
    end
    else
    begin
      Pen.Color := CalcColor(Widget, colorFace);
      Pen.Width := 3;
      Pen.LineCap := capButt;
      Pen.LineJoin := joinMiter;
      P := R.MidPoint;
      P.Move(-1, 1);
      Canvas.MoveTo(P.X - 2.5, P.Y - 2);
      Canvas.LineTo(P.X, P.Y + 2);
      Canvas.LineTo(P.X + 5, P.Y - 5);
      Canvas.Stroke(Pen);
    end
  end
  else
    Canvas.Fill(CalcColor(Widget, colorBorder));
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  Canvas.Stroke(CalcColor(Widget, colorBorder), 2);
  R := B;
  R.X := R.X + 16;
  P := MeasureText(Font, Widget.Text);
  R.Width := P.X + 8;
  R.Inflate(-2, 2);
  if wsSelected in Widget.State then
  begin
    B.Inflate(-3, -3);
    Canvas.RoundRect(R, 3);
    Canvas.Stroke(CalcColor(Widget, colorSelected), 1);
  end;
  DrawCaption(Widget, R);
end;

procedure TArcDarkTheme.DrawSlider(Widget: TSlider);
var
  B, G, R: TRectF;
begin
  B := Widget.Computed.Bounds.Round;
  G := Widget.GripRect;
  R.Y := B.MidPoint.Y - 2;
  R.X := B.X + G.Width / 2;
  R.Width := Widget.Width - G.Width;
  R.Height := 4;
  Canvas.RoundRect(R, 2);
  Canvas.Fill(CalcColor(Widget, colorSelected), True);
  Canvas.Stroke(CalcColor(Widget, colorBorder));
  G.Move(B.X, B.Y);
  with G.MidPoint do
    Canvas.Circle(X, Y, G.Width / 2);
  if wsPressed in Widget.State then
    Canvas.Fill(CalcColor(Widget, colorPressed), True)
  else if wsHot in Widget.State then
    Canvas.Fill(CalcColor(Widget, colorHot), True)
  else
    Canvas.Fill(CalcColor(Widget, colorFace), True);
  Canvas.Stroke(CalcColor(Widget, colorBorder), 1);
end;

procedure TArcDarkTheme.DrawLabel(Widget: TLabel);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

procedure TArcDarkTheme.DrawWindow(Widget: TWindow);
var
  C: TColorF;
  R: TRectF;
begin
  if Widget = Widget.Main.ModalWindow then
  begin
    Canvas.Rect(Widget.Main.Bounds.Round);
    Canvas.Fill($80000000);
  end;
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRectVarying(R, 4, 4, 0, 0);
  C := CalcColor(Widget, colorBase);
  C.A := C.A * (1 - Clamp(Widget.Fade));
  Canvas.Fill(C, True);
  Canvas.Stroke(CalcColor(Widget, colorBorder));
  R.Height := CalcSize(Widget, tpCaption).Y;
  R.Y := R.Y + 1;
  C := CalcColor(Widget, colorCaption);
  Canvas.RoundRectVarying(R, 4, 4, 0, 0);
  Canvas.Fill(C);
  DrawCaption(Widget, R);
end;

procedure TArcDarkTheme.DrawContainer(Widget: TContainerWidget);
var
  C: TColorF;
begin
  if Widget.Parent is TMainWidget then
  begin
    Canvas.RoundRect(Widget.Computed.Bounds.Round, 4);
    C := CalcColor(Widget, colorBase);
    C.A := C.A * (1 - Clamp(Widget.Fade));
    Canvas.Fill(C, True);
    Canvas.Stroke(CalcColor(Widget, colorBorder));
  end;
end;

{ TChicagoTheme }

function TChicagoTheme.FontSize: Float;
begin
  Result := 14;
end;

function TChicagoTheme.GlyphSize: Float;
begin
  Result := 22;
end;

function TChicagoTheme.TitleSize: Float;
begin
  Result := 15;
end;

procedure TChicagoTheme.Init(Canvas: ICanvas);
var
  B: IRenderBitmap;
begin
  inherited Init(Canvas);
  Canvas.Matrix.Push;
  Canvas.Matrix.Identity;
  B := Canvas.NewBitmap('chicago-focus', 2, 2);
  B.Bind;
  Canvas.Clear;
  Canvas.Rect(0, 0, 1, 1);
  Canvas.Fill(colorBlack);
  Canvas.Rect(1, 1, 1, 1);
  Canvas.Fill(colorBlack);
  B.Unbind;
  Canvas.Matrix.Pop;
  FFocus := NewPen;
  FFocus.Brush := NewBrush(B);
  Font := Canvas.LoadFont('NotoSans', '../assets/NotoSans-Regular.ttf');
  Font.Size := FontSize;
  Font.Color := colorWhite;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Glyph := Canvas.LoadFont('glyph', '../assets/materialdesignicons-webfont.ttf');
  Glyph.Size := GlyphSize;
  Glyph.Color := colorText;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  Title := Canvas.LoadFont('NotoSans-Bold', '../assets/NotoSans-Bold.ttf');
  Title.Size := TitleSize;
  Title.Color := colorSilver;
  Title.Align := fontCenter;
  Title.Layout := fontMiddle;
end;

function TChicagoTheme.CalcColor(Widget: TWidget; Color: TThemeColor): LongWord;
var
  A: LongWord;
begin
  if Widget.Computed.Enabled then
    case Color of
      colorBase: Result := $C3C6CC;
      colorFace: Result := $C3C6CC;
      colorBorder: Result := $2B2E39;
      colorActive: Result := $0101A9;
      colorPressed: Result := $5294E2;
      colorSelected: Result := $FFFFFF;
      colorHot: Result := $4E5467;
      colorCaption: Result := $0101A9;
      colorTitle: Result := $FFFFFF;
      colorText: Result := $000000;
      colorHighlight: Result := $FEFEFE;
      colorShadow: Result := $808080;
      colorDarkShadow: Result := $101010;
  else
      Result := 0;
    end
  else
    case Color of
      colorBase: Result := $C3C6CC;
      colorFace: Result := $C3C6CC;
      colorBorder: Result := $2B2E39;
      colorActive: Result := $0101A9;
      colorPressed: Result := $5294E2;
      colorSelected: Result := $FFFFFF;
      colorHot: Result := $4E5467;
      colorCaption: Result := $0101A9;
      colorTitle: Result := $FFFFFF;
      colorText: Result := $606060;
      colorHighlight: Result := $B0B0B0;
      colorShadow: Result := $808080;
      colorDarkShadow: Result := $606060;
    end;
  if Color = colorSelected then
    A := Round($FF * Widget.Computed.Opacity * 0.2) shl 24
  else
    A := Round($FF * Widget.Computed.Opacity) shl 24;
  Result := Result or A;
end;

function TChicagoTheme.CalcSize(Widget: TWidget; Part: TThemePart): TSizeF;
var
  M: Float;
begin
  Result := NewPointF(0, 0);
  { Entire widget defaiult sizes }
  if Part = tpEverything then
  begin
    if Widget is TSpacer then
      Result := NewPointF(8, 8)
    else if Widget is TWindow then
      Result := NewPointF(400, 300)
    else if Widget is TContainerWidget then
      Result := NewPointF(10, 10)
    else if Widget is TPushButton then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X;
      Result.Y := 25;
      if Result.X < 75 then
        Result.X := 75;
    end
    else if Widget is TGlyphButton then
      Result := NewPointF(30, 30)
    else if Widget is TGlyphImage then
      Result := NewPointF(48, 48)
    else if Widget is TCheckBox then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X + 24;
      Result.Y := 24;
    end
    else if Widget is TSlider then
      Result := NewPointF(150, 20)
    else if Widget is TLabel then
    begin
      M := TLabel(Widget).MaxWidth;
      { Realted to issue detailed in TCanvas.MeasureMemo }
      Font.Size := FontSize;
      Font.Align := fontLeft;
      Font.Layout := fontMiddle;
      if M < 1 then
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        Result.Y := Result.Y + 4;
      end
      else
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        if Result.X > M then
        begin
          Result.X := M + 4;
          Result.Y := Canvas.MeasureMemo(Font, Widget.Text, M);
        end;
        Result.Y := Result.Y + 4;
      end;
    end;
    Exit;
  end;
  { Indentation }
  if Part = tpIndent then
  begin
    Result := NewPointF(16, 0);
    Exit;
  end;
  { TWindow parts }
  if Widget is TWindow then
    case Part of
      tpCaption:
        begin
          Result.X := Widget.Width;
          Result.Y := 26;
        end;
    else
    end
  { TSlider parts }
  else if Widget is TSlider then
    case Part of
      tpThumb: Result := NewPointF(8, 16);
    else
    end
  { TCheckBox parts }
  else if Widget is TCheckBox then
    case Part of
      tpNode:
          Result := NewPointF(18, 18);
      tpCaption:
        begin
          Result := MeasureText(Font, '[ ' + Widget.Text + ' ]');
          Result.X := Result.X + 6;
          Result.Y := Result.Y + 6;
        end;
    else
    end;
end;

procedure TChicagoTheme.DrawCaption(Widget: TWidget; const Rect: TRectF);
begin
  if Widget is TWindow then
  begin
    Title.Size := TitleSize;
    Title.Align := fontLeft;
    Title.Layout := fontMiddle;
    Title.Color := CalcColor(Widget, colorTitle);
    DrawText(Title, Widget.Text, Rect.X + 8, Rect.Y + Rect.Height / 2);
  end
  else
    inherited DrawCaption(Widget, Rect);
end;

procedure TChicagoTheme.DrawThinBorder(Widget: TWidget; const Rect: TRectF);
var
  R: TRectF;
begin
  R := Rect.Round;
  if Widget is TSlider then
  begin
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.Stroke(CalcColor(Widget, colorHighlight));
  end
  else if (wsPressed in Widget.State) or (wsToggled in Widget.State) then
  begin
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.Stroke(CalcColor(Widget, colorHighlight));
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
  end
  else if wsHot in Widget.State then
  begin
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.Stroke(CalcColor(Widget, colorHighlight));
  end;
end;

procedure TChicagoTheme.DrawSunken(Widget: TWidget; const Rect: TRectF);
var
  R: TRectF;
begin
  R := Rect.Round;
  Canvas.MoveTo(R.Right, R.Top);
  Canvas.LineTo(R.Right, R.Bottom);
  Canvas.LineTo(R.Left, R.Bottom);
  Canvas.Stroke(CalcColor(Widget, colorHighlight));
  Canvas.MoveTo(R.Left, R.Bottom - 1);
  Canvas.LineTo(R.Left, R.Top);
  Canvas.LineTo(R.Right - 1, R.Top);
  Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
  Canvas.MoveTo(R.Right - 1, R.Top + 1);
  Canvas.LineTo(R.Right - 1, R.Bottom - 1);
  Canvas.LineTo(R.Left + 1, R.Bottom - 1);
  Canvas.Stroke(CalcColor(Widget, colorFace));
  Canvas.MoveTo(R.Left + 1, R.Bottom - 2);
  Canvas.LineTo(R.Left + 1, R.Top + 1);
  Canvas.LineTo(R.Right - 2, R.Top + 1);
  Canvas.Stroke(CalcColor(Widget, colorShadow));
end;

procedure TChicagoTheme.DrawThickBorder(Widget: TWidget);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  if (wsPressed in Widget.State) and (Widget is TPushButton) then
    DrawSunken(Widget, R)
  else
  begin
    Canvas.MoveTo(R.Right, R.Top);
    Canvas.LineTo(R.Right, R.Bottom);
    Canvas.LineTo(R.Left, R.Bottom);
    Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
    Canvas.MoveTo(R.Left, R.Bottom - 1);
    Canvas.LineTo(R.Left, R.Top);
    Canvas.LineTo(R.Right - 1, R.Top);
    Canvas.Stroke(CalcColor(Widget, colorHighlight));
    Canvas.MoveTo(R.Right - 1, R.Top + 1);
    Canvas.LineTo(R.Right - 1, R.Bottom - 1);
    Canvas.LineTo(R.Left + 1, R.Bottom - 1);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
    Canvas.MoveTo(R.Left + 1, R.Bottom - 2);
    Canvas.LineTo(R.Left + 1, R.Top + 1);
    Canvas.LineTo(R.Right - 2, R.Top + 1);
    Canvas.Stroke(CalcColor(Widget, colorFace));
  end;
end;

procedure TChicagoTheme.DrawEdit(Widget: TEdit);
begin
end;

procedure TChicagoTheme.DrawFocus(Widget: TWidget; const Rect: TRectF);
var
  R: TRectF;
begin
  R := Rect.Round;
  Canvas.Rect(R);
  FFocus.Width := 1;
  (FFocus.Brush as IBitmapBrush).Opacity := Widget.Computed.Opacity;
  Canvas.Stroke(FFocus);
end;

procedure TChicagoTheme.DrawButton(Widget: TPushButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorFace));
  DrawThickBorder(Widget);
  if wsPressed in Widget.State then
    R.Y := R.Y + 1;
  DrawCaption(Widget, R);
  R.Inflate(-4, -4);
  if wsSelected in Widget.State then
    DrawFocus(Widget, R);
end;

procedure TChicagoTheme.DrawGlyphButton(Widget: TGlyphButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  DrawThinBorder(Widget, R);
  DrawCaption(Widget, R);
end;

procedure TChicagoTheme.DrawGlyphImage(Widget: TGlyphImage);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

procedure TChicagoTheme.DrawCheckBox(Widget: TCheckBox);
var
  B, R: TRectF;
  P: TPointF;
begin
  B := Widget.Computed.Bounds.Round;
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  if wsToggled in Widget.State then
  begin
    if Widget.Round then
    begin
      Canvas.Fill(CalcColor(Widget, colorHighlight));
      if Widget.Checked then
      begin
        R.Inflate(-4, -4);
        Canvas.RoundRect(R, 7);
        Canvas.Fill(CalcColor(Widget, colorText));
      end;
    end
    else
    begin
      Canvas.Fill(CalcColor(Widget, colorHighlight));
      Pen.Color := CalcColor(Widget, colorDarkShadow);
      Pen.Width := 3;
      Pen.LineCap := capButt;
      Pen.LineJoin := joinMiter;
      P := R.MidPoint;
      P.Move(-1, 1);
      Canvas.MoveTo(P.X - 2.5, P.Y - 2);
      Canvas.LineTo(P.X, P.Y + 1);
      Canvas.LineTo(P.X + 4, P.Y - 4);
      Canvas.Stroke(Pen);
    end
  end
  else
    Canvas.Fill(CalcColor(Widget, colorHighlight));
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
  begin
    Canvas.RoundRect(R, 7);
    Canvas.Stroke(CalcColor(Widget, colorDarkShadow));
    R.Inflate(-1, -1);
    Canvas.RoundRect(R, 6);
    Canvas.Stroke(CalcColor(Widget, colorShadow));
  end
  else
    DrawSunken(Widget, R);
  R := B;
  R.X := R.X + 16;
  P := MeasureText(Font, Widget.Text);
  R.Width := P.X + 8;
  R.Inflate(-2, 2);
  if wsSelected in Widget.State then
    DrawFocus(Widget, R);
  DrawCaption(Widget, R);
end;

procedure TChicagoTheme.DrawSlider(Widget: TSlider);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  with R.MidPoint.Round do
    begin
      Canvas.MoveTo(R.Left, Y);
      Canvas.LineTo(R.Right, Y);
    end;
  Canvas.Stroke(CalcColor(Widget, colorShadow));
  R := Widget.GripRect.Round;
  with Widget.Computed.Bounds do
    R.Move(X, Y);
  Canvas.Rect(R);
  Canvas.Fill(CalcColor(Widget, colorFace));
  DrawThinBorder(Widget, R);
end;

procedure TChicagoTheme.DrawLabel(Widget: TLabel);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

procedure TChicagoTheme.DrawWindow(Widget: TWindow);
var
  C: TColorF;
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.Rect(R);
  C := CalcColor(Widget, colorBase);
  C.A := C.A * (1 - Clamp(Widget.Fade));
  Canvas.Fill(C);
  DrawThickBorder(Widget);
  R.Height := CalcSize(Widget, tpCaption).Y;
  R.Inflate(-2, -2);
  Canvas.Rect(R);
  C := CalcColor(Widget, colorCaption);
  if Widget.Main.ActiveWindow <> Widget then
    C.A := C.A * 0.5;
  Canvas.Fill(C);
  DrawCaption(Widget, R);
end;

procedure TChicagoTheme.DrawContainer(Widget: TContainerWidget);
var
  C: TColorF;
begin
  if Widget.Parent is TMainWidget then
  begin
    Canvas.Rect(Widget.Computed.Bounds.Round);
    C := CalcColor(Widget, colorBase);
    C.A := C.A * (1 - Clamp(Widget.Fade));
    Canvas.Fill(C, True);
    DrawThickBorder(Widget);
  end;
end;

{ TGraphiteTheme }

procedure TGraphiteTheme.Init(Canvas: ICanvas);
begin
  inherited Init(Canvas);
  Font := Canvas.LoadFont('NotoSans', '../assets/NotoSans-Regular.ttf');
  Font.Size := FontSize;
  Font.Color := colorWhite;
  Font.Align := fontCenter;
  Font.Layout := fontMiddle;
  Glyph := Canvas.LoadFont('glyph', '../assets/materialdesignicons-webfont.ttf');
  Glyph.Size := GlyphSize;
  Glyph.Color := colorText;
  Glyph.Align := fontCenter;
  Glyph.Layout := fontMiddle;
  Title := Canvas.LoadFont('NotoSans-Bold', '../assets/NotoSans-Bold.ttf');
  Title.Size := TitleSize;
  Title.Color := colorText;
  Title.Align := fontCenter;
  Title.Layout := fontMiddle;
  FBrush := NewBrush(NewPointF(0, 0), NewPointF(0, 0));
end;

function TGraphiteTheme.FontSize: Float;
begin
  Result := 14;
end;

function TGraphiteTheme.GlyphSize: Float;
begin
  Result := 22;
end;

function TGraphiteTheme.TitleSize: Float;
begin
  Result := 15;
end;

function TGraphiteTheme.CalcColor(Widget: TWidget; Color: TThemeColor): LongWord;
var
  A: LongWord;
begin
  if Widget.Computed.Enabled then
    case Color of
      colorBase: Result := $CCCCCC;
      colorFace: Result := $DDDDDD;
      colorBorder: Result := $222222;
      colorActive: Result := $D79852;
      colorShadow: Result := $AAAAAA;
      colorDarkShadow: Result := $999999;
      colorPressed: Result := $707070;
      colorSelected: Result := $D79852;
      colorHot: Result := $D09050;
      colorCaption: Result := $909090;
      colorTitle: Result := $404040;
      colorText: Result := $303030;
    else
      Result := 0;
    end
  else
    case Color of
      colorBase: Result := $CCCCCC;
      colorFace: Result := $DDDDDD;
      colorBorder: Result := $222222;
      colorActive: Result := $D79852;
      colorShadow: Result := $CCCCCC;
      colorDarkShadow: Result := $BBBBBB;
      colorPressed: Result := $707070;
      colorSelected: Result := $D79852;
      colorHot: Result := $C0A070;
      colorCaption: Result := $999999;
      colorTitle: Result := $505050;
      colorText: Result := $888888;
    end;
  if Color = colorBorder then
    A := Round($FF * Widget.Computed.Opacity * 0.5) shl 24
  else if Color = colorSelected then
    A := Round($FF * Widget.Computed.Opacity * 0.8) shl 24
  else
    A := Round($FF * Widget.Computed.Opacity) shl 24;
  Result := Result or A;
end;

function TGraphiteTheme.CalcSize(Widget: TWidget; Part: TThemePart): TSizeF;
var
  M: Float;
begin
  Result := NewPointF(0, 0);
  { Entire widget defaiult sizes }
  if Part = tpEverything then
  begin
    if Widget is TSpacer then
      Result := NewPointF(8, 8)
    else if Widget is TWindow then
      Result := NewPointF(400, 300)
    else if Widget is TContainerWidget then
      Result := NewPointF(10, 10)
    else if Widget is TPushButton then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X;
      Result.Y := 30;
      if Result.X < 80 then
        Result.X := 80;
    end
    else if Widget is TGlyphButton then
      Result := NewPointF(30, 30)
    else if Widget is TGlyphImage then
      Result := NewPointF(48, 48)
    else if Widget is TCheckBox then
    begin
      Result.X := MeasureText(Font, '[ ' + Widget.Text + ' ]').X + 24;
      Result.Y := 24;
    end
    else if Widget is TSlider then
      Result := NewPointF(150, 20)
    else if Widget is TLabel then
    begin
      M := TLabel(Widget).MaxWidth;
      { Realted to issue detailed in TCanvas.MeasureMemo }
      Font.Size := FontSize;
      Font.Align := fontLeft;
      Font.Layout := fontMiddle;
      if M < 1 then
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        Result.Y := Result.Y + 4;
      end
      else
      begin
        Result := MeasureText(Font, Widget.Text);
        Result.X := Result.X + 4;
        if Result.X > M then
        begin
          Result.X := M + 4;
          Result.Y := Canvas.MeasureMemo(Font, Widget.Text, M);
        end;
        Result.Y := Result.Y + 4;
      end;
    end;
    Exit;
  end;
  { Indentation }
  if Part = tpIndent then
  begin
    Result := NewPointF(16, 0);
    Exit;
  end;
  { TWindow parts }
  if Widget is TWindow then
    case Part of
      tpCaption:
        begin
          Result.X := Widget.Width;
          Result.Y := 30;
        end;
    else
    end
  { TSlider parts }
  else if Widget is TSlider then
    case Part of
      tpThumb: Result := NewPointF(14, 14);
    else
    end
  { TCheckBox parts }
  else if Widget is TCheckBox then
    case Part of
      tpNode:
          Result := NewPointF(18, 18);
      tpCaption:
        begin
          Result := MeasureText(Font, '[ ' + Widget.Text + ' ]');
          Result.X := Result.X + 6;
          Result.Y := Result.Y + 6;
        end;
    else
    end;
end;

procedure TGraphiteTheme.DrawCaption(Widget: TWidget; const Rect: TRectF);
begin
  inherited DrawCaption(Widget, Rect);
end;

procedure TGraphiteTheme.DrawEdit(Widget: TEdit);
begin

end;

procedure TGraphiteTheme.DrawButton(Widget: TPushButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  FBrush.A := R.Sector(2);
  FBrush.B := R.Sector(8);
  FBrush.NearStop.Color := Widget.Color(colorFace);
  FBrush.FarStop.Color := Widget.Color(colorShadow);
  FBrush.FarStop.Offset := 0.8;
  Canvas.RoundRect(R, 6);
  if wsHot in Widget.State then
    if wsPressed in Widget.State then
    begin
      Canvas.Fill(Widget.Color(colorShadow), True);
      Canvas.Stroke(Widget.Color(colorDarkShadow), 1, True);
    end
    else
    begin
      Canvas.Fill(FBrush, True);
      Canvas.Stroke(Widget.Color(colorHot), 1, True)
    end
  else
  begin
    Canvas.Fill(FBrush, True);
    Canvas.Stroke(Widget.Color(colorDarkShadow), 1);
    if wsSelected in Widget.State then
    begin
      R.Inflate(-3, -3);
      Canvas.RoundRect(R, 4);
      Canvas.Stroke(Widget.Color(colorSelected), 1);
      R.Inflate(3, 3);
    end;
  end;
  R.Move(0, 1);
  DrawCaption(Widget, R);
end;

procedure TGraphiteTheme.DrawGlyphButton(Widget: TGlyphButton);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  FBrush.A := R.Sector(2);
  FBrush.B := R.Sector(8);
  FBrush.NearStop.Color := Widget.Color(colorFace);
  FBrush.FarStop.Color := Widget.Color(colorShadow);
  FBrush.FarStop.Offset := 0.8;
  Canvas.RoundRect(R, 6);
  if (wsPressed in Widget.State) or (wsToggled in Widget.State) then
  begin
    Canvas.Fill(Widget.Color(colorDarkShadow), True);
    Canvas.Stroke(Widget.Color(colorPressed), 1, True);
  end
  else if wsHot in Widget.State then
  begin
    Canvas.Fill(FBrush, True);
    Canvas.Stroke(Widget.Color(colorHot), 1, True)
  end;
  R.Move(0, 1);
  DrawCaption(Widget, R);
end;

procedure TGraphiteTheme.DrawGlyphImage(Widget: TGlyphImage);
var
  R: TRectF;
begin
  R := Widget.Computed.Bounds.Round;
  DrawCaption(Widget, R);
end;

procedure TGraphiteTheme.DrawCheckBox(Widget: TCheckBox);
var
  B, R: TRectF;
  P: TPointF;
begin
  B := Widget.Computed.Bounds.Round;
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  if wsToggled in Widget.State then
  begin
    Canvas.Fill(CalcColor(Widget, colorBase));
    if Widget.Round then
    begin
      R.Inflate(-4, -4);
      Canvas.RoundRect(R, 7);
      Canvas.Fill(CalcColor(Widget, colorHot));
    end
    else
    begin
      Pen.Color := CalcColor(Widget, colorHot);
      Pen.Width := 3;
      Pen.LineCap := capButt;
      Pen.LineJoin := joinMiter;
      P := R.MidPoint;
      P.Move(-1, 1);
      Canvas.MoveTo(P.X - 2.5, P.Y - 2);
      Canvas.LineTo(P.X, P.Y + 2);
      Canvas.LineTo(P.X + 5, P.Y - 5);
      Canvas.Stroke(Pen);
    end;
  end
  else
    Canvas.Fill(CalcColor(Widget, colorDarkShadow));
  R := B;
  R.Y := B.MidPoint.Y - 7;
  R.Height := 14;
  R.Width := 14;
  if Widget.Round then
    Canvas.RoundRect(R, 7)
  else
    Canvas.RoundRect(R, 3);
  Canvas.Stroke(CalcColor(Widget, colorDarkShadow), 1);
  R := B;
  R.X := R.X + 16;
  P := MeasureText(Font, Widget.Text);
  R.Width := P.X + 8;
  R.Inflate(-2, 2);
  if wsSelected in Widget.State then
  begin
    Canvas.RoundRect(R, 3);
    Canvas.Stroke(CalcColor(Widget, colorSelected), 1);
  end;
  DrawCaption(Widget, R);
end;

procedure TGraphiteTheme.DrawSlider(Widget: TSlider);
var
  R, S: TRectF;
begin
  R := Widget.GripRect.Round;
  with Widget.Computed.Bounds do
    R.Move(X, Y);
  R := R.Round;
  S := Widget.Computed.Bounds.Round;
  S.Y := R.MidPoint.Y;
  S.Height := 0;
  S.Inflate(2, R.Height / 2 + 1);
  Canvas.RoundRect(S, S.Height / 2);
  Canvas.Fill(Widget.Color(colorShadow), True);
  Canvas.Stroke(Widget.Color(colorDarkShadow));
  with R.MidPoint do
    Canvas.Circle(X, Y, R.Width / 2);
  Canvas.Fill(Widget.Color(colorFace), True);
  if wsPressed in Widget.State then
    Canvas.Fill(Widget.Color(colorHot))
  else if wsHot in Widget.State then
    Canvas.Stroke(Widget.Color(colorHot))
  else
    Canvas.Stroke(Widget.Color(colorDarkShadow));
end;

procedure TGraphiteTheme.DrawLabel(Widget: TLabel);
begin
  DrawCaption(Widget, Widget.Computed.Bounds);
end;

procedure TGraphiteTheme.DrawWindow(Widget: TWindow);
var
  R: TRectF;
  C: TColorF;
begin
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 8);
  C := CalcColor(Widget, colorBase);
  C.A := C.A * (1 - Clamp(Widget.Fade));
  Canvas.Fill(C);
  R.Height := CalcSize(Widget, tpCaption).Y - 4;
  Canvas.RoundRectVarying(R, 8, 8, 0, 0);
  C := Widget.Color(colorCaption);
  if Widget.Main.ActiveWindow <> Widget then
    C.A := C.A * 0.25;
  Canvas.Fill(C);
  R.Y := R.Y + 2;
  DrawCaption(Widget, R);
  R.Y := R.Y - 2;
  R.Top := R.Bottom;
  R.Height := 1;
  Canvas.Rect(R);
  Canvas.Fill(Widget.Color(colorBorder));
  R := Widget.Computed.Bounds.Round;
  Canvas.RoundRect(R, 8);
  Canvas.Stroke(Widget.Color(colorBorder));
end;

procedure TGraphiteTheme.DrawContainer(Widget: TContainerWidget);
var
  C: TColorF;
begin
  if Widget.Parent is TMainWidget then
  begin
    Canvas.RoundRect(Widget.Computed.Bounds.Round, 8);
    C := CalcColor(Widget, colorBase);
    C.A := C.A * (1 - Clamp(Widget.Fade));
    Canvas.Fill(C, True);
    Canvas.Stroke(CalcColor(Widget, colorBorder));
  end;
end;

end.

