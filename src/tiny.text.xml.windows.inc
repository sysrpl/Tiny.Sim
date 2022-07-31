{ included by codebot.text.xml.pas }

{$ifdef windows}
{$region xml interface}
uses
  ComObj,
  Codebot.Interop.Windows.Msxml;

type
  TDocument = class;
  TNodeList = class;

{ TNode }

  TNode = class(TInterfacedObject, INode)
  private
    FNode: IXMLDOMNode;
  public
    function Instance: Pointer;
    function Next: INode;
    function SelectNode(const XPath: string): INode;
    function SelectList(const XPath: string): INodeList;
    function Force(const Path: string): INode;
    function GetDocument: IDocument; virtual;
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
  public
    constructor Create(Node: IXMLDOMNode);
  end;

{ TNodeList }

  TNodeList = class(TInterfacedObject, INodeList)
  private
    FNode: IXMLDOMNode;
    FList: IInterface;
  public
    function GetCount: Integer;
    function GetByName(const Name: string): INode; overload;
    function GetByIndex(Index: Integer): INode; overload;
    procedure Clear;
    procedure Add(Node: INode); overload;
    function Add(const Name: string): INode; overload;
    procedure Remove(Node: INode); overload;
    procedure Remove(const Name: string); overload;
  public
    constructor Create(Node: IXMLDOMNode; List: IInterface);
    function GetEnumerator: IEnumerator<INode>;
  end;

{ TDocument }

  TDocument = class(TNode, IDocument)
  private
    FDocument: IXMLDOMDocument;
  public
    function GetDocument: IDocument; override;
    procedure SetRoot(Value: INode);
    function GetRoot: INode;
    procedure Beautify;
    function CreateAttribute(const Name: string): INode;
    function CreateElement(const Name: string): INode;
    procedure Load(const FileName: string);
    procedure Save(const FileName: string);
  public
    constructor Create(Document: IXMLDOMDocument);
  end;

{ TNode }

constructor TNode.Create(Node: IXMLDOMNode);
begin
  inherited Create;
  FNode := Node;
end;

function TNode.Instance: Pointer;
begin
  Result := Self;
end;

function TNode.Next: INode;
var
  T: LongWord;
  N: IXMLDOMNode;
begin
  Result := nil;
  if FNode = nil then
     Exit;
  if FNode.nodeType = NODE_DOCUMENT then
    Exit;
  T := FNode.nodeType;
  N := FNode.nextSibling;
  while N <> nil do
  begin
    if N.nodeType = T then
       Exit(TNode.Create(N));
    N := FNode.nextSibling;
  end;
end;

function TNode.GetDocument: IDocument;
var
  D: IXMLDOMDocument;
begin
  D := nil;
  D := FNode.ownerDocument;
  if D <>  nil then
    Result := TDocument.Create(D)
  else
    Result := nil;
end;

function TNode.GetParent: INode;
var
  N: IXMLDOMNode;
begin
  N := FNode.parentNode;
  if N <> nil then
    Result := TNode.Create(N)
  else
    Result := nil;
end;

function TNode.GetFiler: IFiler;
begin
  Result := FilerCreate(GetDocument, Self);
end;

function AnsiToWide(const S: string): WideString; inline;
begin
  Result := {%H-}S;
end;

function WideToAnsi(const S: WideString): string; inline;
begin
  Result := {%H-}S;
end;

function TNode.SelectNode(const XPath: string): INode;
var
  N: IXMLDOMNode;
begin
  Result := nil;
  try
    N := FNode.selectSingleNode(AnsiToWide(XPath));
    if N <> nil then
      Result := TNode.Create(N);
  except
    Result := nil;
  end;
end;

function TNode.SelectList(const XPath: string): INodeList;
var
  L: IXMLDOMNodeList;
begin
  Result := nil;
  try
    L := FNode.selectNodes(AnsiToWide(XPath));
    if L <> nil then
      Result := TNodeList.Create(nil, L) ;
  except
    Result := nil;
  end;
end;

function TNode.Force(const Path: string): INode;
var
  Items: StringArray;
  D: IDocument;
  N, C: INode;
  S: string;
  I: Integer;
begin
  Result := nil;
  Items := StrSplit(Path, '/');
  if Items.IsEmpty then
    Exit;
  D := GetDocument;
  if D = nil then
    Exit;
  I := 0;
  S := Items[I];
  case FNode.nodeType of
    NODE_ELEMENT:
      N := Self;
    NODE_DOCUMENT:
      begin
        Inc(I);
        N := D.Root;
        if (N = nil) or (N.Name <> S) then
        begin
          N := D.CreateElement(S);
          if N = nil then
            Exit;
          D.SetRoot(N);
        end;
      end;
  else
    Exit;
  end;
  while I < Items.Length do
  begin
    if N.Kind <> nkElement then
      Exit;
    S := Items[I];
    Inc(I);
    C := N.SelectNode(S);
    if C <> nil then
    begin
      N := C;
      Continue;
    end;
    if StrIsAttr(S) then
      N := N.Attributes.Add(Copy(S, 2, Length(S) - 1))
    else if StrIsIdent(S) then
      N := N.Nodes.Add(S);
  end;
  Result := N;
end;

function TNode.GetAttributes: INodeList;
begin
  Result := TNodeList.Create(FNode, FNode.attributes);
end;

function TNode.GetNodes: INodeList;
begin
  Result := TNodeList.Create(FNode, FNode.childNodes);
end;

function TNode.GetKind: TNodeKind;
begin
  case FNode.nodeType of
    NODE_ELEMENT:
      Result := nkElement;
    NODE_ATTRIBUTE:
      Result := nkAttribute;
    NODE_TEXT:
      Result := nkText;
    NODE_DOCUMENT:
      Result := nkDocument;
  else
    Result := nkOther;
  end;
end;

function TNode.GetName: string;
begin
  case FNode.nodeType of
    NODE_ELEMENT:
      Result := WideToAnsi((FNode as IXMLDOMElement).tagName);
    NODE_ATTRIBUTE:
      Result := WideToAnsi((FNode as IXMLDOMAttribute).name);
    NODE_DOCUMENT:
      Result := 'DOCUMENT';
  else
    Result := '';
  end;
end;

function TNode.GetText: string;
begin
  case FNode.nodeType of
    NODE_ELEMENT:
      Result := WideToAnsi((FNode as IXMLDOMElement).text);
    NODE_ATTRIBUTE:
      Result := WideToAnsi((FNode as IXMLDOMAttribute).value);
    NODE_TEXT:
      Result := WideToAnsi((FNode as IXMLDOMText).data);
    NODE_DOCUMENT:
      Result := WideToAnsi((FNode as IXMLDOMDocument).xml);
  else
    Result := WideToAnsi(FNode.nodeValue);
  end;
end;

procedure TNode.SetText(const Value: string);
begin
  case FNode.nodeType of
    NODE_ELEMENT:
      (FNode as IXMLDOMElement).text := AnsiToWide(Value);
    NODE_ATTRIBUTE:
      (FNode as IXMLDOMAttribute).value := AnsiToWide(Value);
    NODE_TEXT:
      (FNode as IXMLDOMText).data := AnsiToWide(Value);
    NODE_DOCUMENT:
      (FNode as IXMLDOMDocument).loadXML(AnsiToWide(Value));
  else
    FNode.nodeValue := Value;
  end;
end;

function TNode.GetXml: string;
begin
  case FNode.nodeType of
    NODE_ELEMENT,
    NODE_ATTRIBUTE,
    NODE_TEXT,
    NODE_DOCUMENT:
      Result := WideToAnsi(FNode.xml);
  else
    Result := '';
  end;
end;

procedure TNode.SetXml(const Value: string);
begin
  if FNode.nodeType = NODE_DOCUMENT then
      (FNode as IXMLDOMDocument).loadXML(AnsiToWide(Value));
end;

{ TNodeListEnumerator }

type
  TNodeListEnumerator = class(TInterfacedObject, IEnumerator<INode>)
  private
    FList: INodeList;
    FPosition: Integer;
  public
    constructor Create(List: INodeList);
    function GetCurrent: INode;
    function MoveNext: Boolean;
    procedure Reset;
  end;

constructor TNodeListEnumerator.Create(List: INodeList);
begin
	inherited Create;
  FList := List;
  FPosition := -1;
end;

function TNodeListEnumerator.GetCurrent: INode;
begin
  Result := FList[FPosition];
end;

function TNodeListEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

procedure TNodeListEnumerator.Reset;
begin
  FPosition := -1;
end;

{ TNodeList }

constructor TNodeList.Create(Node: IXMLDOMNode; List: IInterface);
begin
  inherited Create;
  FNode := Node;
  FList := List;
end;

function TNodeList.GetEnumerator: IEnumerator<INode>;
begin
	Result := TNodeListEnumerator.Create(Self);
end;

procedure TNodeList.Clear;
var
  L: IXMLDOMNamedNodeMap;
  I: Integer;
begin
  if FNode = nil then
    Exit;
  L := FList as IXMLDOMNamedNodeMap;
  if L <> nil then
    for I := L.length - 1 downto 0 do
      L.removeNamedItem(L.item[I].nodeName)
  else
    while FNode.firstChild <> nil do
      FNode.removeChild(FNode.firstChild);
end;

procedure TNodeList.Add(Node: INode);
var
  N: IXMLDOMNode;
begin
  if FNode = nil then
    Exit;
  N := TNode(Node.Instance).FNode;
  if Supports(N, IXMLDOMElement) then
    FNode.appendChild(N)
  else if Supports(N, IXMLDOMAttribute) then
    FNode.attributes.setNamedItem(N);
end;

function TNodeList.Add(const Name: string): INode;
var
  D: IXMLDOMDocument;
  N: IXMLDOMNode;
begin
  Result := nil;
  if FNode = nil then
    Exit;
  D := FNode.ownerDocument;
  if D = nil then
    Exit;
  if Supports(FList, IXMLDOMNodeList) then
  begin
    N := D.createElement(AnsiToWide(Name));
    FNode.appendChild(N);
  end
  else
  begin
    N := D.createAttribute(AnsiToWide(Name));
    (FList as IXMLDOMNamedNodeMap).setNamedItem(N);
  end;
  Result := TNode.Create(N);
end;

procedure TNodeList.Remove(Node: INode);
begin
  if FNode <> nil then
    FNode.removeChild(TNode(Node.Instance).FNode);
end;

procedure TNodeList.Remove(const Name: string);
var
  Node: INode;
begin
  if FNode = nil then
    Exit;
  Node := GetByName(Name);
  if Node <> nil then
    FNode.removeChild(TNode(Node.Instance).FNode);
end;

function TNodeList.GetByIndex(Index: Integer): INode;
begin
  if Supports(FList, IXMLDOMNamedNodeMap) then
    Result := TNode.Create((FList as IXMLDOMNamedNodeMap).item[Index])
  else
    Result := TNode.Create((FList as IXMLDOMNodeList).item[Index]);
end;

function TNodeList.GetByName(const Name: string): INode;
var
  N: IXMLDOMNode;
  C: IXMLDOMElement;
  L: IXMLDOMNodeList;
  S: WideString;
  I: Integer;
begin
  Result := nil;
  N := nil;
  if Supports(FList, IXMLDOMNamedNodeMap) then
    (FList as IXMLDOMNamedNodeMap).getNamedItem(AnsiToWide(Name))
  else
  begin
    S := AnsiToWide(Name);
    L := FList as IXMLDOMNodeList;
    for I := 0 to L.length - 1 do
    begin
      C := L.item[I] as IXMLDOMElement;
      if C = nil then
        Continue;
      if C.tagName = S then
      begin
        N := C;
        Break;
      end;
    end;
  end;
  if N <> nil then
    Result := TNode.Create(N)
  else
    Result := nil;
end;

function TNodeList.GetCount: Integer;
begin
  if Supports(FList, IXMLDOMNamedNodeMap) then
    Result := (FList as IXMLDOMNamedNodeMap).length
  else
    Result := (FList as IXMLDOMNodeList).length;
end;

{ TDocument }

constructor TDocument.Create(Document: IXMLDOMDocument);
begin
  inherited Create(Document);
  FDocument := Document;
end;

function TDocument.GetDocument: IDocument;
begin
  Result := Self;
end;

procedure StripEmptyText(N: IXMLDOMNode; Level: Integer = 0);
var
  T: IXMLDOMText;
  O: IXMLDOMNode;
  S: string;
begin
  if N = nil then
    Exit;
  while N <> nil do
  begin
    if N is IXMLDOMText then
    begin
      T := N as IXMLDOMText;
      S := WideToAnsi(T.data);
      S := StrTrim(S);
      if S = '' then
      begin
        //S := #13#10 + StrOf(#9, Level);
        //T.Set_text(AnsiToWide(S));
        O := N.nextSibling;
        N.parentNode.removeChild(N);
        N := O;
      end
      else
        N := N.nextSibling;
    end
    else if N is IXMLDOMElement then
    begin
      StripEmptyText(N.firstChild, Level + 1);
      N := N.nextSibling;
    end
    else
      N := N.nextSibling;
  end;
end;

procedure TDocument.Beautify;
var
  S: string;
begin
  StripEmptyText(FDocument.firstChild);
  S := GetXml;
  S := StrReplace(S, '><', '>'#13#10'<');
  SetXml(S);
end;

function TDocument.CreateAttribute(const Name: string): INode;
begin
  Result := TNode.Create(FDocument.createAttribute(AnsiToWide(Name)));
end;

function TDocument.CreateElement(const Name: string): INode;
begin
  Result := TNode.Create(FDocument.createElement(AnsiToWide(Name)));
end;

procedure TDocument.SetRoot(Value: INode);
var
  N: IXMLDOMElement;
begin
  if Value = nil then
    FDocument.documentElement := nil
  else
  begin
    N := TNode(Value.Instance).FNode as IXMLDOMElement;
    if N <> nil then
      FDocument.documentElement := N;
  end;
end;

function TDocument.GetRoot: INode;
var
  N: IXMLDOMElement;
begin
  N := FDocument.documentElement;
  if N <> nil then
    Result := TNode.Create(N)
  else
    Result := nil;
end;

procedure TDocument.Load(const FileName: string);
begin
  FDocument.load(FileName);
end;

procedure TDocument.Save(const FileName: string);
begin
  FDocument.save(FileName);
end;

{ DocumentCreate }

function DocumentCreate: IDocument;
begin
  Result := TDocument.Create(CreateOleObject('Msxml2.DOMDocument') as IXMLDOMDocument);
end;
{$endregion}
{$endif}

