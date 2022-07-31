unit Tiny.Text.Xml;

{$i tiny.inc}

interface

uses
  Tiny.System;

{$region xml interface}
type
  TNodeKind = (nkDocument, nkElement, nkAttribute, nkText, nkOther);

  INodeList = interface;
  IDocument = interface;

{ IFiler }

  IFiler = interface
    ['{3DC4CC5C-AFFC-449F-9983-11FE39194CF5}']
    function GetDocument: IDocument;
    function ReadStr(const Key: string; const DefValue: string = ''; Stored: Boolean = False): string;
    procedure WriteStr(const Key, Value: string);
    function ReadBool(const Key: string; const DefValue: Boolean = False; Stored: Boolean = False): Boolean;
    procedure WriteBool(const Key: string; Value: Boolean);
    function ReadInt(const Key: string; const DefValue: Integer = 0; Stored: Boolean = False): Integer;
    procedure WriteInt(const Key: string; Value: Integer);
    function ReadFloat(const Key: string; const DefValue: Single = 0; Stored: Boolean = False): Single;
    procedure WriteFloat(const Key: string; Value: Single);
    property Document: IDocument read GetDocument;
  end;

{ INode }

  INode = interface
    ['{BC90FD97-E83D-41BB-B4D8-3E25AA5EB2C6}']
    function GetDocument: IDocument;
    function GetParent: INode;
    function GetFiler: IFiler;
    function GetAttributes: INodeList;
    function GetNodes: INodeList;
    function GetKind: TNodeKind;
    function GetName: string;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetXml: string;
    procedure SetXml(const Value: string);
    function Instance: Pointer;
    function Next: INode;
    function SelectNode(const XPath: string): INode;
    function SelectList(const XPath: string): INodeList;
    function Force(const Path: string): INode;
    property Document: IDocument read GetDocument;
    property Parent: INode read GetParent;
    property Filer: IFiler read GetFiler;
    property Attributes: INodeList read GetAttributes;
    property Nodes: INodeList read GetNodes;
    property Kind: TNodeKind read GetKind;
    property Name: string read GetName;
    property Text: string read GetText write SetText;
    property Xml: string read GetXml write SetXml;
  end;

{ INodeList }

  INodeList = interface(IEnumerable<INode>)
    ['{D36A2B84-D31D-4134-B878-35E8D33FD067}']
    function GetCount: Integer;
    function GetByName(const Name: string): INode; overload;
    function GetByIndex(Index: Integer): INode; overload;
    procedure Clear;
    procedure Add(Node: INode); overload;
    function Add(const Name: string): INode; overload;
    procedure Remove(Node: INode); overload;
    procedure Remove(const Name: string); overload;
    property Count: Integer read GetCount;
    property ByName[const Name: string]: INode read GetByName;
    property ByIndex[Index: Integer]: INode read GetByIndex; default;
  end;

{ IDocument }

  IDocument = interface(INode)
    ['{B713CB91-C809-440A-83D1-C42BDF806C4A}']
    procedure SetRoot(Value: INode);
    function GetRoot: INode;
    procedure Beautify;
    function CreateAttribute(const Name: string): INode;
    function CreateElement(const Name: string): INode;
    procedure Load(const FileName: string);
    procedure Save(const FileName: string);
    property Root: INode read GetRoot write SetRoot;
  end;

{ Create a new xml document }
function NewDocument: IDocument;
{ Create a new filer given a document and a node }
function NewFiler(Document: IDocument; Node: INode): IFiler;
{$endregion}

{ Check if an xml is properly closed }
function XmlValidate(const Xml: string): Boolean;

implementation

{$ifdef unix}
  {$i tiny.text.xml.unix.inc}
{$endif}
{$ifdef windows}
  {$i tiny.text.xml.windows.inc}
{$endif}

{$region xml interface}
type
  TFiler = class(TInterfacedObject, IFiler)
  private
    FDocument: IDocument;
    FNode: INode;
  public
    function GetDocument: IDocument;
    function ReadStr(const Key: string; const DefValue: string = ''; Stored: Boolean = False): string;
    procedure WriteStr(const Key, Value: string);
    function ReadBool(const Key: string; const DefValue: Boolean = False; Stored: Boolean = False): Boolean;
    procedure WriteBool(const Key: string; Value: Boolean);
    function ReadInt(const Key: string; const DefValue: Integer = 0; Stored: Boolean = False): Integer;
    procedure WriteInt(const Key: string; Value: Integer);
    function ReadFloat(const Key: string; const DefValue: Single = 0; Stored: Boolean = False): Single;
    procedure WriteFloat(const Key: string; Value: Single);
  public
    constructor Create(Document: IDocument; Node: INode);
  end;

constructor TFiler.Create(Document: IDocument; Node: INode);
begin
  inherited Create;
  FDocument := Document;
  FNode := Node;
end;

function TFiler.GetDocument: IDocument;
begin
  Result := FDocument;
end;

function TFiler.ReadStr(const Key: string; const DefValue: string = ''; Stored: Boolean = False): string;
var
  N: INode;
begin
  N := FNode.SelectNode(Key);
  if N <> nil then
  begin
    Result := N.Text;
    Exit;
  end;
  if Stored then
    WriteStr(Key, DefValue);
  Result := DefValue;
end;

procedure TFiler.WriteStr(const Key, Value: string);
var
  N: INode;
begin
  N := FNode.SelectNode(Key);
  if N = nil then
    N := FNode.Force(Key);
  if N = nil then
    Exit;
  N.Text := Value;
end;

const
  BoolStr: array[Boolean] of string = ('false', 'true');

function StrToBoolDef(S: string; DefValue: Boolean): Boolean;
begin
  S := LowerCase(StrTrim(S));
  Result := DefValue;
  if (S = 'true') or (S = 'y') or (S = 'yes') or (S = 't') or (S = '1') then
    Result := True
  else if (S = 'false') or (S = 'n') or (S = 'no') or (S = 'f') or (S = '0') then
    Result := False;
end;

function TFiler.ReadBool(const Key: string; const DefValue: Boolean = False; Stored: Boolean = False): Boolean;
var
  S: string;
begin
  S := ReadStr(Key, BoolStr[DefValue], Stored);
  Result := StrToBoolDef(S, DefValue);
end;

procedure TFiler.WriteBool(const Key: string; Value: Boolean);
begin
  WriteStr(Key, BoolStr[Value]);
end;

function TFiler.ReadInt(const Key: string; const DefValue: Integer = 0; Stored: Boolean = False): Integer;
var
  S: string;
begin
  S := ReadStr(Key, IntToStr(DefValue), Stored);
  Result := StrToIntDef(S, DefValue);
end;

procedure TFiler.WriteInt(const Key: string; Value: Integer);
begin
  WriteStr(Key, IntToStr(Value));
end;

function TFiler.ReadFloat(const Key: string; const DefValue: Single = 0; Stored: Boolean = False): Single;
var
  S: string;
begin
  S := ReadStr(Key, FloatToStr(DefValue), Stored);
  Result := StrToFloatDef(S, DefValue);
end;

procedure TFiler.WriteFloat(const Key: string; Value: Single);
begin
  WriteStr(Key, FloatToStr(Value));
end;
{$endregion}

function XmlValidate(const Xml: string): Boolean;
var
  OpenTag, CloseTag, CloseBracket: Integer;
  Closed: Boolean;
  I: Integer;
begin
  OpenTag := Xml.MatchCount('<');
  I := Xml.MatchCount('</') * 2 + Xml.MatchCount('/>');
  Closed := I > 0;
  CloseTag := I;
  I := Xml.MatchCount('?>');
  Inc(CloseTag, I);
  CloseBracket := Xml.MatchCount('>');
  Result := Closed and (OpenTag = CloseTag) and (OpenTag = CloseBracket);
end;

var
  FilerStack: TList<IFiler>;

function NewFiler(Document: IDocument; Node: INode): IFiler;
var
  F: TFiler;
  I: Integer;
begin
  if FilerStack = nil then
  begin
    FilerStack := TList<IFiler>.Create;
    FilerStack.Capacity := StackSize;
  end;
  if FilerStack.Count mod 2 = 0 then
    for I := 0 to FilerStack.Count - 1 do
    begin
      F := FilerStack.Item[I] as TFiler;
      if F.RefCount = 2 then
      begin
        F.FDocument := Document;
        F.FNode := Node;
        Exit(F);
      end;
    end
  else
    for I := FilerStack.Count - 1 downto 0 do
    begin
      F := FilerStack.Item[I] as TFiler;
      if F.RefCount = 2 then
      begin
        F.FDocument := Document;
        F.FNode := Node;
        Exit(F);
      end;
    end;
  Result := TFiler.Create(Document, Node);
  if FilerStack.Count < StackSize then
    FilerStack.Add(Result);
end;

finalization
  FilerStack.Free;
  NodeStack.Free;
end.


