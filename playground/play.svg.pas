unit Play.Svg;

{$mode delphi}

interface

uses
  Tiny.System,
  Tiny.Types,
  Tiny.Text.Xml;

{ Forward declarations }

type
  TSvgNode = class;
  TSvgShape = class;
  TSvgLine = class;
  TSvgPolyline = class;
  TSvgPolygon = class;
  TSvgCircle = class;
  TSvgEllipse = class;
  TSvgRect = class;
  TSvgPath = class;
  TSvgCollection = class;
  TSvgGroup = class;
  TSvgDocument = class;

  TSvgColor = LongWord;

  TSvgCap = (svgCapRound, svgCapButt, svgCapSquare);
  TSvgJoin = (svgJoinRound, svgJoinBevel, svgJoinMiter);

  TSvgStyle = record
  public
    Fill: TSvgColor;
    FillOpacity: Float;
    Stroke: TSvgColor;
    StrokeOpacity: Float;
    StrokeWidth: Float;
    StrokeCap: TSvgCap;
    StrokeJoin: TSvgJoin;
    StrokeMiter: Float;
  end;

  TSvgStyleEx = class
  protected
    procedure Copy(StyleEx: TSvgStyleEx); virtual; abstract;
    procedure Read(const Name, Value: string); virtual; abstract;
  public
    constructor Create; virtual;
  end;

  TSvgStyleExClass = class of TSvgStyleEx;

{ TSvgNode }

  TSvgTransform = array[0..5] of Float;

  TSvgNode = class
  private
    procedure FindStyle(Ident: string);
  protected
    procedure ApplyStyle(S: string); virtual;
    procedure Parse(N: INode); virtual;
  public
    Doc: TSvgDocument;
    Parent: TSvgNode;
    Id: string;
    ClassId: string;
    Transform: string;
    Opacity: Float;
    Removed: Boolean;
    Style: TSvgStyle;
    StyleEx: TSvgStyleEx;
    constructor Create(ParentNode: TSvgNode); virtual;
    destructor Destroy; override;
    procedure BuildTransform(out T: TSvgTransform);
  end;
  TSvgNodeClass = class of TSvgNode;

  TSvgNodes = TList<TSvgNode>;
  TSvgEnumerator = TSvgNodes.TListItemEnumerator;

{ TSvgShape }

  TSvgShape = class(TSvgNode)
  end;

{ TSvgCircle }

  TSvgLine = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    X1, Y1, X2, Y2: Float;
  end;

{ TSvgPolyline }

  TSvgPoints = TArrayList<TPointF>;

  TSvgPolyline = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    Points: TSvgPoints;
  end;

{ TSvgPolygon }

  TSvgPolygon = class(TSvgPolyline)
  end;

{ TSvgCircle }

  TSvgCircle = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    X, Y, R: Float;
  end;

{ TSvgEllipse }

  TSvgEllipse = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    X, Y, W, H: Float;
  end;

{ TSvgRect }

  TSvgRect = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    X, Y, W, H, R: Float;
  end;

  TSvgPathAction = (svgMove, svgLine, svgHLine, svgVLine, svgCubic, svgQuadratic, svgClose);

  TSvgCommand = record
    Action: TSvgPathAction;
    Compressed: Boolean;
    X, Y, X1, Y1, X2, Y2: Float;
  end;

  TSvgCommands = TArrayList<TSvgCommand>;

{ TSvgPath }

  TSvgPath = class(TSvgShape)
  protected
    procedure Parse(N: INode); override;
  public
    Commands: TSvgCommands;
  end;

{ TSvgCollection }

  TSvgCollection = class(TSvgNode)
  private
    FNodes: TSvgNodes;
    procedure Cleanup;
    function GetCount: Integer;
    function GetNode(Index: Integer): TSvgNode;
  public
    function GetEnumerator: TSvgEnumerator;
  public
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Node[Index: Integer]: TSvgNode read GetNode; default;
  end;

{ TSvgGroup }

  TSvgGroup = class(TSvgCollection)
  end;

{ TSvgDocument }

  TSvgDocument = class(TSvgCollection)
  private
    FViewBox: TRectF;
    FStyleSection: string;
    FStyleExClass: TSvgStyleExClass;
  public
    procedure ParseText(const Xml: string; StyleExClass: TSvgStyleExClass = nil);
    procedure ParseFile(const FileName: string; StyleExClass: TSvgStyleExClass = nil);
    property NodeCount: Integer read GetCount;
    property Node[Index: Integer]: TSvgNode read GetNode;
    property ViewBox: TRectF read FViewBox;
  end;

function NewSvgDocument: TSvgDocument;

implementation

const
  ColorNames: array of string = [
    'black', 'white', 'aliceblue', 'antiquewhite', 'aqua',
    'aquamarine', 'azure', 'beige', 'bisque', 'blanchedalmond',
    'blue', 'blueviolet', 'brown', 'burlywood', 'cadetblue',
    'chartreuse', 'chocolate', 'coral', 'cornflowerblue', 'cornsilk',
    'crimson', 'cyan', 'darkblue', 'darkcyan', 'darkgoldenrod',
    'darkgray', 'darkgrey', 'darkgreen', 'darkkhaki', 'darkmagenta',
    'darkolivegreen', 'darkorange', 'darkorchid', 'darkred', 'darksalmon',
    'darkseagreen', 'darkslateblue', 'darkslategray', 'darkslategrey', 'darkturquoise',
    'darkviolet', 'deeppink', 'deepskyblue', 'dimgray', 'dimgrey',
    'dodgerblue', 'firebrick', 'floralwhite', 'forestgreen', 'fuchsia',
    'gainsboro', 'ghostwhite', 'gold', 'goldenrod', 'gray',
    'grey', 'green', 'greenyellow', 'honeydew', 'hotpink',
    'indianred', 'indigo', 'ivory', 'khaki', 'lavender',
    'lavenderblush', 'lawngreen', 'lemonchiffon', 'lightblue', 'lightcoral',
    'lightcyan', 'lightgoldenrodyellow', 'lightgray', 'lightgrey', 'lightgreen',
    'lightpink', 'lightsalmon', 'lightseagreen', 'lightskyblue', 'lightslategray',
    'lightslategrey', 'lightsteelblue', 'lightyellow', 'lime', 'limegreen',
    'linen', 'magenta', 'maroon', 'mediumaquamarine', 'mediumblue',
    'mediumorchid', 'mediumpurple', 'mediumseagreen', 'mediumslateblue', 'mediumspringgreen',
    'mediumturquoise', 'mediumvioletred', 'midnightblue', 'mintcream', 'mistyrose',
    'moccasin', 'navajowhite', 'navy', 'oldlace', 'olive',
    'olivedrab', 'orange', 'orangered', 'orchid', 'palegoldenrod',
    'palegreen', 'paleturquoise', 'palevioletred', 'papayawhip', 'peachpuff',
    'peru', 'pink', 'plum', 'powderblue', 'purple',
    'rebeccapurple', 'red', 'rosybrown', 'royalblue', 'saddlebrown',
    'salmon', 'sandybrown', 'seagreen', 'seashell', 'sienna',
    'silver', 'skyblue', 'slateblue', 'slategray', 'slategrey',
    'snow', 'springgreen', 'steelblue', 'tan', 'teal',
    'thistle', 'tomato', 'turquoise', 'violet', 'wheat',
    'whitesmoke', 'yellow', 'yellowgreen'];

  ColorValues: array of TSvgColor = [
    $FF000000, $FFFFFFFF, $FFF0F8FF, $FFFAEBD7, $FF00FFFF,
    $FF7FFFD4, $FFF0FFFF, $FFF5F5DC, $FFFFE4C4, $FFFFEBCD,
    $FF0000FF, $FF8A2BE2, $FFA52A2A, $FFDEB887, $FF5F9EA0,
    $FF7FFF00, $FFD2691E, $FFFF7F50, $FF6495ED, $FFFFF8DC,
    $FFDC143C, $FF00FFFF, $FF00008B, $FF008B8B, $FFB8860B,
    $FFA9A9A9, $FFA9A9A9, $FF006400, $FFBDB76B, $FF8B008B,
    $FF556B2F, $FFFF8C00, $FF9932CC, $FF8B0000, $FFE9967A,
    $FF8FBC8F, $FF483D8B, $FF2F4F4F, $FF2F4F4F, $FF00CED1,
    $FF9400D3, $FFFF1493, $FF00BFFF, $FF696969, $FF696969,
    $FF1E90FF, $FFB22222, $FFFFFAF0, $FF228B22, $FFFF00FF,
    $FFDCDCDC, $FFF8F8FF, $FFFFD700, $FFDAA520, $FF808080,
    $FF808080, $FF008000, $FFADFF2F, $FFF0FFF0, $FFFF69B4,
    $FFCD5C5C, $FF4B0082, $FFFFFFF0, $FFF0E68C, $FFE6E6FA,
    $FFFFF0F5, $FF7CFC00, $FFFFFACD, $FFADD8E6, $FFF08080,
    $FFE0FFFF, $FFFAFAD2, $FFD3D3D3, $FFD3D3D3, $FF90EE90,
    $FFFFB6C1, $FFFFA07A, $FF20B2AA, $FF87CEFA, $FF778899,
    $FF778899, $FFB0C4DE, $FFFFFFE0, $FF00FF00, $FF32CD32,
    $FFFAF0E6, $FFFF00FF, $FF800000, $FF66CDAA, $FF0000CD,
    $FFBA55D3, $FF9370DB, $FF3CB371, $FF7B68EE, $FF00FA9A,
    $FF48D1CC, $FFC71585, $FF191970, $FFF5FFFA, $FFFFE4E1,
    $FFFFE4B5, $FFFFDEAD, $FF000080, $FFFDF5E6, $FF808000,
    $FF6B8E23, $FFFFA500, $FFFF4500, $FFDA70D6, $FFEEE8AA,
    $FF98FB98, $FFAFEEEE, $FFDB7093, $FFFFEFD5, $FFFFDAB9,
    $FFCD853F, $FFFFC0CB, $FFDDA0DD, $FFB0E0E6, $FF800080,
    $FF663399, $FFFF0000, $FFBC8F8F, $FF4169E1, $FF8B4513,
    $FFFA8072, $FFF4A460, $FF2E8B57, $FFFFF5EE, $FFA0522D,
    $FFC0C0C0, $FF87CEEB, $FF6A5ACD, $FF708090, $FF708090,
    $FFFFFAFA, $FF00FF7F, $FF4682B4, $FFD2B48C, $FF008080,
    $FFD8BFD8, $FFFF6347, $FF40E0D0, $FFEE82EE, $FFF5DEB3,
    $FFF5F5F5, $FFFFFF00, $FF9ACD32];

function StrToColor(S: string): TSvgColor;
var
  Items: StringArray;
  R, G, B, A: Byte;
  F: string;
  I: Integer;
begin
  if S.Length < 1 then
    Exit(0);
  S := S.ToLower;
  if S = 'none' then
    Exit(0);
  for I := Low(ColorNames) to High(ColorNames) do
    if S = ColorNames[I] then
      Exit(ColorValues[I]);
  if S.BeginsWith('rgb(') then
  begin
    S := S.Replace('rgb(', '').Replace(')', '');
    Items := S.Split(',');
    R := StrToInt(Items[0].Trim);
    G := StrToInt(Items[1].Trim);
    B := StrToInt(Items[2].Trim);
    Result := (R shl 16) or (G shl 8) or B or $FF000000;
    Exit;
  end;
  if S.BeginsWith('rgba(') then
  begin
    S := S.Replace('rgba(', '').Replace(')', '');
    Items := S.Split(',');
    R := StrToInt(Items[0].Trim);
    G := StrToInt(Items[1].Trim);
    B := StrToInt(Items[2].Trim);
    S := Items[3].Trim;
    A := Round(StrToFloat(S) * $FF);
    Result := (A shl 24) or (R shl 16) or (G shl 8) or B;
    Exit;
  end;
  if S[1] <> '#' then
    Exit(0);
  S[1] := '$';
  if S.Length = 4 then
  begin
    F := '       ';
    F[1] := '$';
    F[2] := S[2]; F[3] := S[2];
    F[4] := S[3]; F[5] := S[3];
    F[6] := S[4]; F[7] := S[4];
    S := F;
  end;
  Result := StrToInt(S);
  if Result < $1000000 then
    Result := Result or $FF000000;
end;

{ TSvgStyleEx }

constructor TSvgStyleEx.Create;
begin
  inherited Create;
end;

{ TSvgNode }

constructor TSvgNode.Create(ParentNode: TSvgNode);
var
  N: TSvgNode;
begin
  inherited Create;
  N := ParentNode;
  if N <> nil then
  begin
    while N.Parent <> nil do
      N := N.Parent;
    if N is TSvgDocument then
    begin
      Doc := N as TSvgDocument;
      if Doc.FStyleExClass <> nil then
        StyleEx := Doc.FStyleExClass.Create;
    end;
  end;
  Parent := ParentNode;
  if Parent = nil then
  begin
    Style.Fill := $FF000000;
    Style.FillOpacity := 1;
    Style.Stroke := 0;
    Style.StrokeWidth := 1;
    Style.StrokeCap := svgCapButt;
    Style.StrokeMiter := 4;
    Style.StrokeOpacity := 1;
  end
  else
  begin
    Style := Parent.Style;
    if (StyleEx <> nil) and (Parent.StyleEx <> nil) then
      StyleEx.Copy(Parent.StyleEx);
  end;
  Opacity := 1;
end;

destructor TSvgNode.Destroy;
begin
  if StyleEx <> nil then
    StyleEx.Free;
  inherited Destroy;
end;

procedure TSvgNode.FindStyle(Ident: string);
var
  S: string;
begin
  S := Doc.FStyleSection;
  if S.IndexOf(Ident) < 1 then
    Exit;
  S := S.SecondOf(Ident);
  if S = '' then
    Exit;
  S := S.SecondOf('{');
  if S = '' then
    Exit;
  S := S.FirstOf('}');
  ApplyStyle(S);
end;

procedure TSvgNode.ApplyStyle(S: string);
var
  Items: StringArray;
  L, R: string;
begin
  Items := S.Split(';');
  for S in Items do
  begin
    L := S.FirstOf(':').Trim;
    R := S.SecondOf(':').Trim;
    if L = 'fill' then
      Style.Fill := StrToColor(R)
    else if L = 'fill-opacity' then
      Style.FillOpacity := StrToFloat(R)
    else if L = 'stroke' then
      Style.Stroke := StrToColor(R)
    else if L = 'stroke-opacity' then
      Style.StrokeOpacity := StrToFloat(R)
    else if L = 'stroke-width' then
    begin
      R := R.Replace('px', '');
      Style.StrokeWidth := StrToFloat(R);
    end
    else if L = 'stroke-linecap' then
    begin
      if R = 'round' then
        Style.StrokeCap := svgCapRound
      else if R = 'butt' then
        Style.StrokeCap := svgCapButt
      else
        Style.StrokeCap := svgCapSquare;
    end
    else if L = 'stroke-linejoin' then
    begin
      if R = 'round' then
        Style.StrokeJoin := svgJoinRound
      else if R = 'miter' then
        Style.StrokeJoin := svgJoinMiter
      else
        Style.StrokeJoin := svgJoinBevel;
    end
    else if L = 'stroke-miterlimit' then
      Style.StrokeMiter := StrToFloat(R)
    else if L = 'opacity' then
      Opacity := StrToFloat(R)
    else if L = 'display' then
      Removed := R = 'none'
    else if L = 'transform' then
      Transform := R
    else if StyleEx <> nil then
      StyleEx.Read(L, R);
  end;
end;

procedure TSvgNode.Parse(N: INode);
var
  F: IFiler;
  S: string;
  A: INodeList;
  C: INode;
  I: Integer;
begin
  F := N.Filer;
  ClassId := F.ReadStr('@class').Trim;
  if ClassId <> '' then
    FindStyle('.' + ClassId);
  Id := F.ReadStr('@id').Trim;
  if Id <> '' then
    FindStyle('#' + Id);
  S := F.ReadStr('@style');
  if S <> '' then
    ApplyStyle(S);
  S := F.ReadStr('@fill');
  if S <> '' then
    Style.Fill := StrToColor(S);
  S := F.ReadStr('@stroke');
  if S <> '' then
    Style.Stroke := StrToColor(S);
  S := F.ReadStr('@stroke-width');
  if S <> '' then
    Style.StrokeWidth := StrToFloat(S);
  S := F.ReadStr('@opacity');
  if S <> '' then
    Opacity  := StrToFloat(S);
  S := F.ReadStr('@transform').Trim;
  if S <> '' then
    Transform := S;
  if StyleEx <> nil then
  begin
    A := N.Attributes;
    for I := 0 to A.Count - 1 do
    begin
      C := A.ByIndex[I];
      StyleEx.Read(C.Name, C.Text);
    end;
  end;
end;

procedure TSvgNode.BuildTransform(out T: TSvgTransform);
var
  Items: StringArray;
  S: string;
  I: Integer;
begin
  T[0] := 1; T[1] := 0; T[2] := 0;
  T[3] := 1; T[4] := 0; T[5] := 0;
  if Transform.BeginsWith('matrix(') then
  begin
    S := Transform.Replace('matrix(', '').Replace(')', '').Replace(',', ' ');
    Items := S.Split(' ');
    I := 0;
    for S in Items do
    begin
      if S = '' then
        Continue;
      T[I] := StrToFloat(S);
      Inc(I);
      if I = 6 then
        Break;
    end;
  end
  else if Transform.BeginsWith('translate(') then
  begin
    S := Transform.Replace('translate(', '').Replace(')', '').Replace(',', ' ');
    Items := S.Split(' ');
    I := 0;
    for S in Items do
    begin
      if S = '' then
        Continue;
      T[4 + I] := StrToFloat(S);
      Inc(I);
      if I = 2 then
        Break;
    end;
  end
  else if Transform.BeginsWith('scale(') then
  begin
    S := Transform.Replace('scale(', '').Replace(')', '').Replace(',', ' ');
    Items := S.Split(' ');
    I := 0;
    for S in Items do
    begin
      if S = '' then
        Continue;
      T[I * 4] := StrToFloat(S);
      Inc(I);
      if I = 2 then
        Break;
    end;
  end;
end;

{ TSvgLine }

procedure TSvgLine.Parse(N: INode);
var
  F: IFiler;
begin
  inherited Parse(N);
  F := N.Filer;
  X1 := F.ReadFloat('@x1');
  Y1 := F.ReadFloat('@y1');
  X2 := F.ReadFloat('@x2');
  Y2 := F.ReadFloat('@y2');
end;

{ TSvgPolyline }

procedure TSvgPolyline.Parse(N: INode);
var
  F: IFiler;
  Items: StringArray;
  S, A, B: string;
  P: TPointF;
  I: Integer;
begin
  inherited Parse(N);
  F := N.Filer;
  S := F.ReadStr('@points').Trim;
  Items := S.Split(' ');
  for I := 0 to Items.Length - 1 do
  begin
    S := Items[I].Trim;
    if S = '' then
      Continue;
    A := S.FirstOf(',').Trim;
    B := S.SecondOf(',').Trim;
    if (A = '') or (B = '') then
      Continue;
    P.X := StrToFloat(A);
    P.Y := StrToFloat(B);
    Points.Push(P);
  end;
end;

{ TSvgCircle }

procedure TSvgCircle.Parse(N: INode);
var
  F: IFiler;
begin
  inherited Parse(N);
  F := N.Filer;
  X := F.ReadFloat('@cx');
  Y := F.ReadFloat('@cy');
  R := F.ReadFloat('@r');
end;

{ TSvgEllipse }

procedure TSvgEllipse.Parse(N: INode);
var
  F: IFiler;
begin
  inherited Parse(N);
  F := N.Filer;
  W := F.ReadFloat('@rx');
  H := F.ReadFloat('@ry');
  X := F.ReadFloat('@cx') - W;
  Y := F.ReadFloat('@cy') - H;
  W := W * 2;
  H := H * 2;
end;

{ TSvgRect }

procedure TSvgRect.Parse(N: INode);
var
  F: IFiler;
begin
  inherited Parse(N);
  F := N.Filer;
  X := F.ReadFloat('@x');
  Y := F.ReadFloat('@y');
  W := F.ReadFloat('@width');
  H := F.ReadFloat('@height');
  R := F.ReadFloat('@rx');
  if R = 0 then
    R := F.ReadFloat('@ry');
end;

type
  TSvgPathData = record
  private
    Data: string;
    Seek: PChar;
    Buffer: array[0..63] of Char;
  public
    Item: string;
    procedure Init(const D: string);
    function Next: Boolean;
  end;

procedure TSvgPathData.Init(const D: string);
begin
  Data := D;
  Seek := nil;
  if Data <> '' then
    Seek := PChar(Data);
end;

function TSvgPathData.Next: Boolean;
const
  L = 63;
var
  I: Integer;
begin
  Result := False;
  Item := '';
  if Seek = nil then
    Exit;
  while (Seek[0] > #0) and ((Seek[0] <= ' ') or (Seek[0] = ',')) do
    Inc(Seek);
  if Seek[0] = #0 then
    Exit;
  I := 0;
  if (Seek[0] = '-') or (Seek[0] in ['0'..'9', '.']) then
  begin
    if (Seek[0] = '-') then
    begin
      Buffer[I] := Seek[0];
      Inc(I); Inc(Seek);
    end;
    while Seek[0] in ['0'..'9', '.'] do
    begin
      Buffer[I] := Seek[0];
      Inc(I); Inc(Seek);
      if I = L then
        Exit;
    end;
    if (UpCase(Seek[0]) = 'E') and (Seek[1] in ['+', '-']) then
    begin
      Buffer[I] := Seek[0];
      Inc(I); Inc(Seek);
      if I = L then
        Exit;
      Buffer[I] := Seek[0];
      Inc(I); Inc(Seek);
      if I = L then
        Exit;
      while Seek[0] in ['0'..'9', '.'] do
      begin
        Buffer[I] := Seek[0];
        Inc(I); Inc(Seek);
        if I = L then
          Exit;
      end;
    end;
    Buffer[I] := #0;
    Item := Buffer;
    Result := True;
  end
  else
  begin
    Item := Seek[0];
    Inc(Seek);
    Result := True;
  end;
end;

procedure TSvgPath.Parse(N: INode);
var
  F: IFiler;
  Data: TSvgPathData;
  Command: TSvgCommand;
  Relative: Boolean;
  WasCurve: Boolean;
  P: TPointF;
begin
  inherited Parse(N);
  F := N.Filer;
  Data.Init(F.ReadStr('@d').Trim);
  MemZero(Command, SizeOf(Command));
  Relative := False;
  while Data.Next do
  begin
    P.X := 0;
    P.Y := 0;
    if Data.Item[1] in ['M', 'm'] then
    begin
      Command.Action := svgMove;
      Relative := Data.Item[1] = 'm';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      Data.Next;
      Command.X := P.X + StrToFloat(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToFloat(Data.Item);
      Commands.Push(Command);
      Command.Action := svgLine;
      Continue;
    end;
    if Data.Item[1] in ['L', 'l'] then
    begin
      Command.Action := svgLine;
      Relative := Data.Item[1] = 'l';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      Data.Next;
      Command.X := P.X + StrToFloat(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToFloat(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['H', 'h'] then
    begin
      Command.Action := svgHLine;
      Relative := Data.Item[1] = 'h';
      if Relative then
        P.X := Command.X;
      Data.Next;
      Command.X := P.X + StrToFloat(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['V', 'v'] then
    begin
      Command.Action := svgVLine;
      Relative := Data.Item[1] = 'v';
      if Relative then
        P.Y := Command.Y;
      Data.Next;
      Command.Y := P.Y + StrToFloat(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['C','c'] then
    begin
      Command.Action := svgCubic;
      Command.Compressed := False;
      Relative := Data.Item[1] = 'c';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      Data.Next;
      Command.X1 := P.X + StrToFloat(Data.Item);
      Data.Next;
      Command.Y1 := P.Y + StrToFloat(Data.Item);
      Data.Next;
      Command.X2 := P.X + StrToFloat(Data.Item);
      Data.Next;
      Command.Y2 := P.Y + StrToFloat(Data.Item);
      Data.Next;
      Command.X := P.X + StrToFloat(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToFloat(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['S','s'] then
    begin
      WasCurve := Command.Action = svgCubic;
      Command.Action := svgCubic;
      Command.Compressed := True;
      Relative := Data.Item[1] = 's';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      if WasCurve then
      begin
        Command.X1 := Command.X * 2 - Command.X2;
        Command.Y1 := Command.Y * 2 - Command.Y2;
      end
      else
      begin
        Command.X1 := Command.X;
        Command.Y1 := Command.Y;
      end;
      Data.Next;
      Command.X2 := P.X + StrToFloat(Data.Item);
      Data.Next;
      Command.Y2 := P.Y + StrToFloat(Data.Item);
      Data.Next;
      Command.X := P.X + StrToFloat(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToFloat(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['Q','q'] then
    begin
      Command.Action := svgQuadratic;
      Command.Compressed := False;
      Relative := Data.Item[1] = 'q';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      Data.Next;
      Command.X1 := P.X + StrToFloat(Data.Item);
      Data.Next;
      Command.Y1 := P.Y + StrToFloat(Data.Item);
      Data.Next;
      Command.X := P.X + StrToFloat(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToFloat(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['T','t'] then
    begin
      WasCurve := Command.Action = svgCubic;
      Command.Action := svgQuadratic;
      Command.Compressed := True;
      Relative := Data.Item[1] = 't';
      if Relative then
      begin
        P.X := Command.X;
        P.Y := Command.Y;
      end;
      if WasCurve then
      begin
        Command.X1 := Command.X * 2 - Command.X1;
        Command.Y1 := Command.Y * 2 - Command.Y1;
      end
      else
      begin
        Command.X1 := Command.X1;
        Command.Y1 := Command.Y1;
      end;
      Data.Next;
      Command.X := P.X +StrToFloat(Data.Item);
      Data.Next;
      Command.Y := P.Y + StrToFloat(Data.Item);
      Commands.Push(Command);
      Continue;
    end;
    if Data.Item[1] in ['Z','z'] then
    begin
      Command.Action := svgClose;
      Commands.Push(Command);
      Continue;
    end;
    { Abort if unsupported commands are encountered }
    if IsAlpha(Data.Item[1]) then
    begin
      WriteLn('command not supported ', Data.Item[1]);
      Break;
    end;
    case Command.Action of
      svgLine:
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X := P.X + StrToFloat(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToFloat(Data.Item);
          Commands.Push(Command);
        end;
      svgHLine:
        begin
          if Relative then
            P.X := Command.X;
          Command.X := P.X + StrToFloat(Data.Item);
          Commands.Push(Command);
        end;
      svgVLine:
        begin
          if Relative then
            P.Y := Command.Y;
          Command.Y := P.Y + StrToFloat(Data.Item);
          Commands.Push(Command);
        end;
      svgCubic:
        if Command.Compressed then
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X1 := Command.X * 2 - Command.X2;
          Command.Y1 := Command.Y * 2 - Command.Y2;
          Command.X2 := P.X + StrToFloat(Data.Item);
          Data.Next;
          Command.Y2 := P.Y + StrToFloat(Data.Item);
          Data.Next;
          Command.X := P.X + StrToFloat(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToFloat(Data.Item);
          Commands.Push(Command);
        end
        else
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X1 := P.X + StrToFloat(Data.Item);
          Data.Next;
          Command.Y1 := P.Y + StrToFloat(Data.Item);
          Data.Next;
          Command.X2 := P.X + StrToFloat(Data.Item);
          Data.Next;
          Command.Y2 := P.Y + StrToFloat(Data.Item);
          Data.Next;
          Command.X := P.X + StrToFloat(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToFloat(Data.Item);
          Commands.Push(Command);
        end;
      svgQuadratic:
        if Command.Compressed then
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X1 := Command.X * 2 - Command.X1;
          Command.Y1 := Command.Y * 2 - Command.Y1;
          Command.X := P.X +StrToFloat(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToFloat(Data.Item);
          Commands.Push(Command);
        end
        else
        begin
          if Relative then
          begin
            P.X := Command.X;
            P.Y := Command.Y;
          end;
          Command.X1 := P.X + StrToFloat(Data.Item);
          Data.Next;
          Command.Y1 := P.Y + StrToFloat(Data.Item);
          Data.Next;
          Command.X := P.X + StrToFloat(Data.Item);
          Data.Next;
          Command.Y := P.Y + StrToFloat(Data.Item);
          Commands.Push(Command);
        end;
    else
      Break;
    end;
  end;
end;

{ TSvgCollection }

destructor TSvgCollection.Destroy;
begin
  Cleanup;
  FNodes.Free;
  inherited Destroy;
end;

function TSvgCollection.GetEnumerator: TSvgEnumerator;
begin
  Result := FNodes.GetEnumerator;
end;

procedure TSvgCollection.Cleanup;
var
  I: Integer;
begin
  if FNodes = nil then
    Exit;
  for I := 0 to FNodes.Count - 1 do
    FNodes[I].Free;
  FNodes.Free;
  FNodes := nil;
end;

function TSvgCollection.GetNode(Index: Integer): TSvgNode;
begin
  Result := FNodes[Index];
end;

function TSvgCollection.GetCount: Integer;
begin
  Result := FNodes.Count;
end;

{ TSvgDocument }

procedure TSvgDocument.ParseText(const Xml: string; StyleExClass: TSvgStyleExClass = nil);
var
  D: IDocument;

  procedure Add(Parent: TSvgNode; NodeClass: TSvgNodeClass; Nodes: TSvgNodes; N: INode);
  var
    S: TSvgNode;
  begin
    S := NodeClass.Create(Parent);
    S.Parse(N);
    Nodes.Add(S);
  end;

  procedure AddNodes(Parent: TSvgNode; Nodes: TSvgNodes; Items: INodeList);
  var
    Group: TSvgGroup;
    N: INode;
    S: string;
    I: Integer;
  begin
    for I := 0 to Items.Count - 1 do
    begin
      N := Items[I];
      S := N.Name;
      if S = 'line' then
        Add(Parent, TSvgLine, Nodes, N)
      else if S = 'polyline' then
        Add(Parent, TSvgPolyline, Nodes, N)
      else if S = 'polygon' then
        Add(Parent, TSvgPolygon, Nodes, N)
      else if S = 'circle' then
        Add(Parent, TSvgCircle, Nodes, N)
      else if S = 'ellipse' then
        Add(Parent, TSvgEllipse, Nodes, N)
      else if S = 'rect' then
        Add(Parent, TSvgRect, Nodes, N)
      else if S = 'path' then
        Add(Parent, TSvgPath, Nodes, N)
      else if S = 'g' then
      begin
        Group := TSvgGroup.Create(Parent);
        Group.Parse(N);
        Nodes.Add(Group);
        Group.FNodes := TSvgNodes.Create;
        AddNodes(Group, Group.FNodes, N.Nodes);
      end;
    end;
  end;

  procedure GenerateErrorImage;
  var
    R: TSvgRect;
    L: TSvgLine;
  begin
    R := TSvgRect.Create(Self);
    R.X := 0; R.Y := 0;
    R.W := 100; R.H := 100; R.R := 10;
    R.Style.Fill := $FF900000;
    FNodes.Add(R);
    L := TSvgLine.Create(Self);
    L.X1 := 25; L.X2 := 75;
    L.Y1 := 50; L.Y2 := 50;
    L.Style.Fill := 0;
    L.Style.Stroke := $FFC0C0C0;
    L.Style.StrokeWidth := 20;
    L.Style.StrokeCap := svgCapRound;
    FNodes.Add(L);
    FViewBox.X := 0; FViewBox.Y := 0;
    FViewBox.Width := 100; FViewBox.Height := 100;
  end;

  procedure FindViewBox;
  var
    Items: StringArray;
    F: IFiler;
    S: string;
    I: Integer;
  begin
    FViewBox.X := 0; FViewBox.Y := 0;
    FViewBox.Width := 0; FViewBox.Height := 0;
    F := D.Root.Filer;
    S := F.ReadStr('@viewBox');
    if S = '' then
      Exit;
    Items := S.Replace(',', ' ').Split(' ');
    I := 0;
    for S in Items do
    begin
      if S = '' then
        Continue;
      case I of
        0: FViewBox.X := StrToFloat(S);
        1: FViewBox.Y := StrToFloat(S);
        2: FViewBox.Width := StrToFloat(S);
        3: FViewBox.Height := StrToFloat(S);
      end;
      Inc(I);
      if I = 4 then
        Break;
    end;
  end;

  procedure FindStyle;
  var
    N: INode;
  begin
    FStyleSection := '';
    N := D.Root.Nodes.ByName['style'];
    if N <> nil then
      FStyleSection := N.Text;
  end;

begin
  if StyleEx <> nil then
    StyleEx.Free;
  StyleEx := nil;
  if StyleExClass <> nil then
  begin
    FStyleExClass := StyleExClass;
    StyleEx := StyleExClass.Create;
  end;
  D := NewDocument;
  Cleanup;
  try
    FNodes := TSvgNodes.Create;
    D.Xml := Xml;
    if (D.Root = nil) or (D.Root.Name <> 'svg') then
      GenerateErrorImage
    else
    begin
      FindViewBox;
      FindStyle;
      AddNodes(Self, FNodes, D.Root.Nodes)
    end;
  except
    Cleanup;
    FNodes := TSvgNodes.Create;
    GenerateErrorImage;
  end;
end;

procedure TSvgDocument.ParseFile(const FileName: string; StyleExClass: TSvgStyleExClass = nil);
var
  S: string;
begin
  S := FileLoadText(FileName);
  ParseText(S, StyleExClass);
end;

function NewSvgDocument: TSvgDocument;
begin
  Result := TSvgDocument.Create(nil);
end;

end.
