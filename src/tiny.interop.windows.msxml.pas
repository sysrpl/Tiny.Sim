(********************************************************)
(*                                                      *)
(*  Codebot Pascal Library                              *)
(*  http://cross.codebot.org                            *)
(*  Modified August 2019                                *)
(*                                                      *)
(********************************************************)

{ <include docs/codebot.interop.windows.msxml.txt> }
unit Codebot.Interop.Windows.Msxml;

{$i codebot.inc}

interface

{$ifdef windows}
uses
  Windows, ActiveX;

type
  tagDOMNodeType = TOleEnum;

const
  NODE_INVALID = $00000000;
  NODE_ELEMENT = $00000001;
  NODE_ATTRIBUTE = $00000002;
  NODE_TEXT = $00000003;
  NODE_CDATA_SECTION = $00000004;
  NODE_ENTITY_REFERENCE = $00000005;
  NODE_ENTITY = $00000006;
  NODE_PROCESSING_INSTRUCTION = $00000007;
  NODE_COMMENT = $00000008;
  NODE_DOCUMENT = $00000009;
  NODE_DOCUMENT_TYPE = $0000000A;
  NODE_DOCUMENT_FRAGMENT = $0000000B;
  NODE_NOTATION = $0000000C;

type
  tagXMLEMEM_TYPE = TOleEnum;

const
  XMLELEMTYPE_ELEMENT = $00000000;
  XMLELEMTYPE_TEXT = $00000001;
  XMLELEMTYPE_COMMENT = $00000002;
  XMLELEMTYPE_DOCUMENT = $00000003;
  XMLELEMTYPE_DTD = $00000004;
  XMLELEMTYPE_PI = $00000005;
  XMLELEMTYPE_OTHER = $00000006;

type
  _SERVERXMLHTTP_OPTION = TOleEnum;

const
  SXH_OPTION_URL_CODEPAGE = $00000000;
  SXH_OPTION_ESCAPE_PERCENT_IN_URL = $00000001;
  SXH_OPTION_IGNORE_SERVER_SSL_CERT_ERROR_FLAGS = $00000002;
  SXH_OPTION_SELECT_CLIENT_SSL_CERT = $00000003;

type
  _SXH_SERVER_CERT_OPTION = TOleEnum;

const
  SXH_SERVER_CERT_IGNORE_UNKNOWN_CA = $00000100;
  SXH_SERVER_CERT_IGNORE_WRONG_USAGE = $00000200;
  SXH_SERVER_CERT_IGNORE_CERT_CN_INVALID = $00001000;
  SXH_SERVER_CERT_IGNORE_CERT_DATE_INVALID = $00002000;
  SXH_SERVER_CERT_IGNORE_ALL_SERVER_ERRORS = $00003300;

type
  IXMLDOMImplementation = interface;
  IXMLDOMNode = interface;
  IXMLDOMNodeList = interface;
  IXMLDOMNamedNodeMap = interface;
  IXMLDOMDocument = interface;
  IXMLDOMDocumentType = interface;
  IXMLDOMElement = interface;
  IXMLDOMAttribute = interface;
  IXMLDOMDocumentFragment = interface;
  IXMLDOMCharacterData = interface;
  IXMLDOMText = interface;
  IXMLDOMComment = interface;
  IXMLDOMCDATASection = interface;
  IXMLDOMProcessingInstruction = interface;
  IXMLDOMEntityReference = interface;
  IXMLDOMParseError = interface;
  IXMLDOMSchemaCollection = interface;
  IXMLDOMDocument2 = interface;
  IXMLDOMNotation = interface;
  IXMLDOMEntity = interface;
  IXTLRuntime = interface;
  IXSLTemplate = interface;
  IXSLProcessor = interface;
  ISAXXMLReader = interface;
  ISAXEntityResolver = interface;
  ISAXContentHandler = interface;
  ISAXLocator = interface;
  ISAXAttributes = interface;
  ISAXDTDHandler = interface;
  ISAXErrorHandler = interface;
  ISAXXMLFilter = interface;
  ISAXLexicalHandler = interface;
  ISAXDeclHandler = interface;
  IVBSAXXMLReader = interface;
  IVBSAXEntityResolver = interface;
  IVBSAXContentHandler = interface;
  IVBSAXLocator = interface;
  IVBSAXAttributes = interface;
  IVBSAXDTDHandler = interface;
  IVBSAXErrorHandler = interface;
  IVBSAXXMLFilter = interface;
  IVBSAXLexicalHandler = interface;
  IVBSAXDeclHandler = interface;
  IMXWriter = interface;
  IMXAttributes = interface;
  IMXReaderControl = interface;
  IXMLElementCollection = interface;
  IXMLDocument = interface;
  IXMLElement = interface;
  IXMLDocument2 = interface;
  IXMLElement2 = interface;
  IXMLAttribute = interface;
  IXMLError = interface;
  IXMLDOMSelection = interface;
  IDSOControl = interface;
  IXMLHTTPRequest = interface;
  IServerXMLHTTPRequest = interface;

  PWord1 = ^Word; {*}
  PUserType1 = ^TGUID; {*}

  DOMNodeType = tagDOMNodeType;

  _xml_error = packed record
    _nLine: SYSUINT;
    _pchBuf: WideString;
    _cchBuf: SYSUINT;
    _ich: SYSUINT;
    _pszFound: WideString;
    _pszExpected: WideString;
    _reserved1: LongWord;
    _reserved2: LongWord;
  end;

  XMLELEM_TYPE = tagXMLEMEM_TYPE;
  SERVERXMLHTTP_OPTION = _SERVERXMLHTTP_OPTION;
  SXH_SERVER_CERT_OPTION = _SXH_SERVER_CERT_OPTION;

  IXMLDOMImplementation = interface(IDispatch)
    ['{2933BF8F-7B36-11D2-B20E-00C04F983E60}']
    function  hasFeature(const feature: WideString; const version: WideString): WordBool; safecall;
  end;

  IXMLDOMNode = interface(IDispatch)
    ['{2933BF80-7B36-11D2-B20E-00C04F983E60}']
    function  Get_nodeName: WideString; safecall;
    function  Get_nodeValue: OleVariant; safecall;
    procedure Set_nodeValue(value: OleVariant); safecall;
    function  Get_nodeType: DOMNodeType; safecall;
    function  Get_parentNode: IXMLDOMNode; safecall;
    function  Get_childNodes: IXMLDOMNodeList; safecall;
    function  Get_firstChild: IXMLDOMNode; safecall;
    function  Get_lastChild: IXMLDOMNode; safecall;
    function  Get_previousSibling: IXMLDOMNode; safecall;
    function  Get_nextSibling: IXMLDOMNode; safecall;
    function  Get_attributes: IXMLDOMNamedNodeMap; safecall;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; safecall;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; safecall;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; safecall;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; safecall;
    function  hasChildNodes: WordBool; safecall;
    function  Get_ownerDocument: IXMLDOMDocument; safecall;
    function  cloneNode(deep: WordBool): IXMLDOMNode; safecall;
    function  Get_nodeTypeString: WideString; safecall;
    function  Get_text: WideString; safecall;
    procedure Set_text(const text: WideString); safecall;
    function  Get_specified: WordBool; safecall;
    function  Get_definition: IXMLDOMNode; safecall;
    function  Get_nodeTypedValue: OleVariant; safecall;
    procedure Set_nodeTypedValue(typedValue: OleVariant); safecall;
    function  Get_dataType: OleVariant; safecall;
    procedure Set_dataType(const dataTypeName: WideString); safecall;
    function  Get_xml: WideString; safecall;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; safecall;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; safecall;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; safecall;
    function  Get_parsed: WordBool; safecall;
    function  Get_namespaceURI: WideString; safecall;
    function  Get_prefix: WideString; safecall;
    function  Get_baseName: WideString; safecall;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); safecall;
    property nodeName: WideString read Get_nodeName;
    property nodeValue: OleVariant read Get_nodeValue write Set_nodeValue;
    property nodeType: DOMNodeType read Get_nodeType;
    property parentNode: IXMLDOMNode read Get_parentNode;
    property childNodes: IXMLDOMNodeList read Get_childNodes;
    property firstChild: IXMLDOMNode read Get_firstChild;
    property lastChild: IXMLDOMNode read Get_lastChild;
    property previousSibling: IXMLDOMNode read Get_previousSibling;
    property nextSibling: IXMLDOMNode read Get_nextSibling;
    property attributes: IXMLDOMNamedNodeMap read Get_attributes;
    property ownerDocument: IXMLDOMDocument read Get_ownerDocument;
    property nodeTypeString: WideString read Get_nodeTypeString;
    property text: WideString read Get_text write Set_text;
    property specified: WordBool read Get_specified;
    property definition: IXMLDOMNode read Get_definition;
    property nodeTypedValue: OleVariant read Get_nodeTypedValue write Set_nodeTypedValue;
    property xml: WideString read Get_xml;
    property parsed: WordBool read Get_parsed;
    property namespaceURI: WideString read Get_namespaceURI;
    property prefix: WideString read Get_prefix;
    property baseName: WideString read Get_baseName;
  end;

  IXMLDOMNodeList = interface(IDispatch)
    ['{2933BF82-7B36-11D2-B20E-00C04F983E60}']
    function  Get_item(index: Integer): IXMLDOMNode; safecall;
    function  Get_length: Integer; safecall;
    function  nextNode: IXMLDOMNode; safecall;
    procedure reset; safecall;
    function  Get__newEnum: IUnknown; safecall;
    property item[index: Integer]: IXMLDOMNode read Get_item; default;
    property length: Integer read Get_length;
    property _newEnum: IUnknown read Get__newEnum;
  end;

  IXMLDOMNamedNodeMap = interface(IDispatch)
    ['{2933BF83-7B36-11D2-B20E-00C04F983E60}']
    function  getNamedItem(const name: WideString): IXMLDOMNode; safecall;
    function  setNamedItem(const newItem: IXMLDOMNode): IXMLDOMNode; safecall;
    function  removeNamedItem(const name: WideString): IXMLDOMNode; safecall;
    function  Get_item(index: Integer): IXMLDOMNode; safecall;
    function  Get_length: Integer; safecall;
    function  getQualifiedItem(const baseName: WideString; const namespaceURI: WideString): IXMLDOMNode; safecall;
    function  removeQualifiedItem(const baseName: WideString; const namespaceURI: WideString): IXMLDOMNode; safecall;
    function  nextNode: IXMLDOMNode; safecall;
    procedure reset; safecall;
    function  Get__newEnum: IUnknown; safecall;
    property item[index: Integer]: IXMLDOMNode read Get_item; default;
    property length: Integer read Get_length;
    property _newEnum: IUnknown read Get__newEnum;
  end;

  IXMLDOMDocument = interface(IXMLDOMNode)
    ['{2933BF81-7B36-11D2-B20E-00C04F983E60}']
    function  Get_doctype: IXMLDOMDocumentType; safecall;
    function  Get_implementation_: IXMLDOMImplementation; safecall;
    function  Get_documentElement: IXMLDOMElement; safecall;
    procedure Set_documentElement(const DOMElement: IXMLDOMElement); safecall;
    function  createElement(const tagName: WideString): IXMLDOMElement; safecall;
    function  createDocumentFragment: IXMLDOMDocumentFragment; safecall;
    function  createTextNode(const data: WideString): IXMLDOMText; safecall;
    function  createComment(const data: WideString): IXMLDOMComment; safecall;
    function  createCDATASection(const data: WideString): IXMLDOMCDATASection; safecall;
    function  createProcessingInstruction(const target: WideString; const data: WideString): IXMLDOMProcessingInstruction; safecall;
    function  createAttribute(const name: WideString): IXMLDOMAttribute; safecall;
    function  createEntityReference(const name: WideString): IXMLDOMEntityReference; safecall;
    function  getElementsByTagName(const tagName: WideString): IXMLDOMNodeList; safecall;
    function  createNode(type_: OleVariant; const name: WideString; const namespaceURI: WideString): IXMLDOMNode; safecall;
    function  nodeFromID(const idString: WideString): IXMLDOMNode; safecall;
    function  load(xmlSource: OleVariant): WordBool; safecall;
    function  Get_readyState: Integer; safecall;
    function  Get_parseError: IXMLDOMParseError; safecall;
    function  Get_url: WideString; safecall;
    function  Get_async: WordBool; safecall;
    procedure Set_async(isAsync: WordBool); safecall;
    procedure abort; safecall;
    function  loadXML(const bstrXML: WideString): WordBool; safecall;
    procedure save(destination: OleVariant); safecall;
    function  Get_validateOnParse: WordBool; safecall;
    procedure Set_validateOnParse(isValidating: WordBool); safecall;
    function  Get_resolveExternals: WordBool; safecall;
    procedure Set_resolveExternals(isResolving: WordBool); safecall;
    function  Get_preserveWhiteSpace: WordBool; safecall;
    procedure Set_preserveWhiteSpace(isPreserving: WordBool); safecall;
    procedure Set_onreadystatechange(Param1: OleVariant); safecall;
    procedure Set_ondataavailable(Param1: OleVariant); safecall;
    procedure Set_ontransformnode(Param1: OleVariant); safecall;
    property doctype: IXMLDOMDocumentType read Get_doctype;
    property implementation_: IXMLDOMImplementation read Get_implementation_;
    property documentElement: IXMLDOMElement read Get_documentElement write Set_documentElement;
    property readyState: Integer read Get_readyState;
    property parseError: IXMLDOMParseError read Get_parseError;
    property url: WideString read Get_url;
    property async: WordBool read Get_async write Set_async;
    property validateOnParse: WordBool read Get_validateOnParse write Set_validateOnParse;
    property resolveExternals: WordBool read Get_resolveExternals write Set_resolveExternals;
    property preserveWhiteSpace: WordBool read Get_preserveWhiteSpace write Set_preserveWhiteSpace;
    property onreadystatechange: OleVariant write Set_onreadystatechange;
    property ondataavailable: OleVariant write Set_ondataavailable;
    property ontransformnode: OleVariant write Set_ontransformnode;
  end;

  IXMLDOMDocumentType = interface(IXMLDOMNode)
    ['{2933BF8B-7B36-11D2-B20E-00C04F983E60}']
    function  Get_name: WideString; safecall;
    function  Get_entities: IXMLDOMNamedNodeMap; safecall;
    function  Get_notations: IXMLDOMNamedNodeMap; safecall;
    property name: WideString read Get_name;
    property entities: IXMLDOMNamedNodeMap read Get_entities;
    property notations: IXMLDOMNamedNodeMap read Get_notations;
  end;

  IXMLDOMElement = interface(IXMLDOMNode)
    ['{2933BF86-7B36-11D2-B20E-00C04F983E60}']
    function  Get_tagName: WideString; safecall;
    function  getAttribute(const name: WideString): OleVariant; safecall;
    procedure setAttribute(const name: WideString; value: OleVariant); safecall;
    procedure removeAttribute(const name: WideString); safecall;
    function  getAttributeNode(const name: WideString): IXMLDOMAttribute; safecall;
    function  setAttributeNode(const DOMAttribute: IXMLDOMAttribute): IXMLDOMAttribute; safecall;
    function  removeAttributeNode(const DOMAttribute: IXMLDOMAttribute): IXMLDOMAttribute; safecall;
    function  getElementsByTagName(const tagName: WideString): IXMLDOMNodeList; safecall;
    procedure normalize; safecall;
    property tagName: WideString read Get_tagName;
  end;

  IXMLDOMAttribute = interface(IXMLDOMNode)
    ['{2933BF85-7B36-11D2-B20E-00C04F983E60}']
    function  Get_name: WideString; safecall;
    function  Get_value: OleVariant; safecall;
    procedure Set_value(attributeValue: OleVariant); safecall;
    property name: WideString read Get_name;
    property value: OleVariant read Get_value write Set_value;
  end;

  IXMLDOMDocumentFragment = interface(IXMLDOMNode)
    ['{3EFAA413-272F-11D2-836F-0000F87A7782}']
  end;

  IXMLDOMCharacterData = interface(IXMLDOMNode)
    ['{2933BF84-7B36-11D2-B20E-00C04F983E60}']
    function  Get_data: WideString; safecall;
    procedure Set_data(const data: WideString); safecall;
    function  Get_length: Integer; safecall;
    function  substringData(offset: Integer; count: Integer): WideString; safecall;
    procedure appendData(const data: WideString); safecall;
    procedure insertData(offset: Integer; const data: WideString); safecall;
    procedure deleteData(offset: Integer; count: Integer); safecall;
    procedure replaceData(offset: Integer; count: Integer; const data: WideString); safecall;
    property data: WideString read Get_data write Set_data;
    property length: Integer read Get_length;
  end;

  IXMLDOMText = interface(IXMLDOMCharacterData)
    ['{2933BF87-7B36-11D2-B20E-00C04F983E60}']
    function  splitText(offset: Integer): IXMLDOMText; safecall;
  end;

  IXMLDOMComment = interface(IXMLDOMCharacterData)
    ['{2933BF88-7B36-11D2-B20E-00C04F983E60}']
  end;

  IXMLDOMCDATASection = interface(IXMLDOMText)
    ['{2933BF8A-7B36-11D2-B20E-00C04F983E60}']
  end;

  IXMLDOMProcessingInstruction = interface(IXMLDOMNode)
    ['{2933BF89-7B36-11D2-B20E-00C04F983E60}']
    function  Get_target: WideString; safecall;
    function  Get_data: WideString; safecall;
    procedure Set_data(const value: WideString); safecall;
    property target: WideString read Get_target;
    property data: WideString read Get_data write Set_data;
  end;

  IXMLDOMEntityReference = interface(IXMLDOMNode)
    ['{2933BF8E-7B36-11D2-B20E-00C04F983E60}']
  end;

  IXMLDOMParseError = interface(IDispatch)
    ['{3EFAA426-272F-11D2-836F-0000F87A7782}']
    function  Get_errorCode: Integer; safecall;
    function  Get_url: WideString; safecall;
    function  Get_reason: WideString; safecall;
    function  Get_srcText: WideString; safecall;
    function  Get_line: Integer; safecall;
    function  Get_linepos: Integer; safecall;
    function  Get_filepos: Integer; safecall;
    property errorCode: Integer read Get_errorCode;
    property url: WideString read Get_url;
    property reason: WideString read Get_reason;
    property srcText: WideString read Get_srcText;
    property line: Integer read Get_line;
    property linepos: Integer read Get_linepos;
    property filepos: Integer read Get_filepos;
  end;

  IXMLDOMSchemaCollection = interface(IDispatch)
    ['{373984C8-B845-449B-91E7-45AC83036ADE}']
    procedure add(const namespaceURI: WideString; var_: OleVariant); safecall;
    function  get(const namespaceURI: WideString): IXMLDOMNode; safecall;
    procedure remove(const namespaceURI: WideString); safecall;
    function  Get_length: Integer; safecall;
    function  Get_namespaceURI(index: Integer): WideString; safecall;
    procedure addCollection(const otherCollection: IXMLDOMSchemaCollection); safecall;
    function  Get__newEnum: IUnknown; safecall;
    property length: Integer read Get_length;
    property namespaceURI[index: Integer]: WideString read Get_namespaceURI; default;
    property _newEnum: IUnknown read Get__newEnum;
  end;

  IXMLDOMDocument2 = interface(IXMLDOMDocument)
    ['{2933BF95-7B36-11D2-B20E-00C04F983E60}']
    function  Get_namespaces: IXMLDOMSchemaCollection; safecall;
    function  Get_schemas: OleVariant; safecall;
    procedure Set_schemas(otherCollection: OleVariant); safecall;
    function  validate: IXMLDOMParseError; safecall;
    procedure setProperty(const name: WideString; value: OleVariant); safecall;
    function  getProperty(const name: WideString): OleVariant; safecall;
    property namespaces: IXMLDOMSchemaCollection read Get_namespaces;
    property schemas: OleVariant read Get_schemas write Set_schemas;
  end;

  IXMLDOMNotation = interface(IXMLDOMNode)
    ['{2933BF8C-7B36-11D2-B20E-00C04F983E60}']
    function  Get_publicId: OleVariant; safecall;
    function  Get_systemId: OleVariant; safecall;
    property publicId: OleVariant read Get_publicId;
    property systemId: OleVariant read Get_systemId;
  end;

  IXMLDOMEntity = interface(IXMLDOMNode)
    ['{2933BF8D-7B36-11D2-B20E-00C04F983E60}']
    function  Get_publicId: OleVariant; safecall;
    function  Get_systemId: OleVariant; safecall;
    function  Get_notationName: WideString; safecall;
    property publicId: OleVariant read Get_publicId;
    property systemId: OleVariant read Get_systemId;
    property notationName: WideString read Get_notationName;
  end;


  IXTLRuntime = interface(IXMLDOMNode)
    ['{3EFAA425-272F-11D2-836F-0000F87A7782}']
    function  uniqueID(const pNode: IXMLDOMNode): Integer; safecall;
    function  depth(const pNode: IXMLDOMNode): Integer; safecall;
    function  childNumber(const pNode: IXMLDOMNode): Integer; safecall;
    function  ancestorChildNumber(const bstrNodeName: WideString; const pNode: IXMLDOMNode): Integer; safecall;
    function  absoluteChildNumber(const pNode: IXMLDOMNode): Integer; safecall;
    function  formatIndex(lIndex: Integer; const bstrFormat: WideString): WideString; safecall;
    function  formatNumber(dblNumber: Double; const bstrFormat: WideString): WideString; safecall;
    function  formatDate(varDate: OleVariant; const bstrFormat: WideString;
                         varDestLocale: OleVariant): WideString; safecall;
    function  formatTime(varTime: OleVariant; const bstrFormat: WideString;
                         varDestLocale: OleVariant): WideString; safecall;
  end;

  IXSLTemplate = interface(IDispatch)
    ['{2933BF93-7B36-11D2-B20E-00C04F983E60}']
    procedure Set_stylesheet(const stylesheet: IXMLDOMNode); safecall;
    function  Get_stylesheet: IXMLDOMNode; safecall;
    function  createProcessor: IXSLProcessor; safecall;
    property stylesheet: IXMLDOMNode read Get_stylesheet write Set_stylesheet;
  end;

  IXSLProcessor = interface(IDispatch)
    ['{2933BF92-7B36-11D2-B20E-00C04F983E60}']
    procedure Set_input(pVar: OleVariant); safecall;
    function  Get_input: OleVariant; safecall;
    function  Get_ownerTemplate: IXSLTemplate; safecall;
    procedure setStartMode(const mode: WideString; const namespaceURI: WideString); safecall;
    function  Get_startMode: WideString; safecall;
    function  Get_startModeURI: WideString; safecall;
    procedure Set_output(pOutput: OleVariant); safecall;
    function  Get_output: OleVariant; safecall;
    function  transform: WordBool; safecall;
    procedure reset; safecall;
    function  Get_readyState: Integer; safecall;
    procedure addParameter(const baseName: WideString; parameter: OleVariant;
                           const namespaceURI: WideString); safecall;
    procedure addObject(const obj: IDispatch; const namespaceURI: WideString); safecall;
    function  Get_stylesheet: IXMLDOMNode; safecall;
    property input: OleVariant read Get_input write Set_input;
    property ownerTemplate: IXSLTemplate read Get_ownerTemplate;
    property startMode: WideString read Get_startMode;
    property startModeURI: WideString read Get_startModeURI;
    property output: OleVariant read Get_output write Set_output;
    property readyState: Integer read Get_readyState;
    property stylesheet: IXMLDOMNode read Get_stylesheet;
  end;

  ISAXXMLReader = interface(IUnknown)
    ['{A4F96ED0-F829-476E-81C0-CDC7BD2A0802}']
    function  getFeature(var pwchName: Word; out pvfValue: WordBool): HResult; stdcall;
    function  putFeature(var pwchName: Word; vfValue: WordBool): HResult; stdcall;
    function  getProperty(var pwchName: Word; out pvarValue: OleVariant): HResult; stdcall;
    function  putProperty(var pwchName: Word; varValue: OleVariant): HResult; stdcall;
    function  getEntityResolver(out ppResolver: ISAXEntityResolver): HResult; stdcall;
    function  putEntityResolver(const pResolver: ISAXEntityResolver): HResult; stdcall;
    function  getContentHandler(out ppHandler: ISAXContentHandler): HResult; stdcall;
    function  putContentHandler(const pHandler: ISAXContentHandler): HResult; stdcall;
    function  getDTDHandler(out ppHandler: ISAXDTDHandler): HResult; stdcall;
    function  putDTDHandler(const pHandler: ISAXDTDHandler): HResult; stdcall;
    function  getErrorHandler(out ppHandler: ISAXErrorHandler): HResult; stdcall;
    function  putErrorHandler(const pHandler: ISAXErrorHandler): HResult; stdcall;
    function  getBaseURL(out ppwchBaseUrl: PWord1): HResult; stdcall;
    function  putBaseURL(var pwchBaseUrl: Word): HResult; stdcall;
    function  getSecureBaseURL(out ppwchSecureBaseUrl: PWord1): HResult; stdcall;
    function  putSecureBaseURL(var pwchSecureBaseUrl: Word): HResult; stdcall;
    function  parse(varInput: OleVariant): HResult; stdcall;
    function  parseURL(var pwchUrl: Word): HResult; stdcall;
  end;

  ISAXEntityResolver = interface(IUnknown)
    ['{99BCA7BD-E8C4-4D5F-A0CF-6D907901FF07}']
    function  resolveEntity(var pwchPublicId: Word; var pwchSystemId: Word;
                            out pvarInput: OleVariant): HResult; stdcall;
  end;

  ISAXContentHandler = interface(IUnknown)
    ['{1545CDFA-9E4E-4497-A8A4-2BF7D0112C44}']
    function  putDocumentLocator(const pLocator: ISAXLocator): HResult; stdcall;
    function  startDocument: HResult; stdcall;
    function  endDocument: HResult; stdcall;
    function  startPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT; var pwchUri: Word;
                                 cchUri: SYSINT): HResult; stdcall;
    function  endPrefixMapping(var pwchPrefix: Word; cchPrefix: SYSINT): HResult; stdcall;
    function  startElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT;
                           var pwchLocalName: Word; cchLocalName: SYSINT; var pwchQName: Word;
                           cchQName: SYSINT; const pAttributes: ISAXAttributes): HResult; stdcall;
    function  endElement(var pwchNamespaceUri: Word; cchNamespaceUri: SYSINT;
                         var pwchLocalName: Word; cchLocalName: SYSINT; var pwchQName: Word;
                         cchQName: SYSINT): HResult; stdcall;
    function  characters(var pwchChars: Word; cchChars: SYSINT): HResult; stdcall;
    function  ignorableWhitespace(var pwchChars: Word; cchChars: SYSINT): HResult; stdcall;
    function  processingInstruction(var pwchTarget: Word; cchTarget: SYSINT; var pwchData: Word;
                                    cchData: SYSINT): HResult; stdcall;
    function  skippedEntity(var pwchName: Word; cchName: SYSINT): HResult; stdcall;
  end;

  ISAXLocator = interface(IUnknown)
    ['{9B7E472A-0DE4-4640-BFF3-84D38A051C31}']
    function  getColumnNumber(out pnColumn: SYSINT): HResult; stdcall;
    function  getLineNumber(out pnLine: SYSINT): HResult; stdcall;
    function  getPublicId(out ppwchPublicId: PWord1): HResult; stdcall;
    function  getSystemId(out ppwchSystemId: PWord1): HResult; stdcall;
  end;

  ISAXAttributes = interface(IUnknown)
    ['{F078ABE1-45D2-4832-91EA-4466CE2F25C9}']
    function  getLength(out pnLength: SYSINT): HResult; stdcall;
    function  getURI(nIndex: SYSINT; out ppwchUri: PWord1; out pcchUri: SYSINT): HResult; stdcall;
    function  getLocalName(nIndex: SYSINT; out ppwchLocalName: PWord1; out pcchLocalName: SYSINT): HResult; stdcall;
    function  getQName(nIndex: SYSINT; out ppwchQName: PWord1; out pcchQName: SYSINT): HResult; stdcall;
    function  getName(nIndex: SYSINT; out ppwchUri: PWord1; out pcchUri: SYSINT;
                      out ppwchLocalName: PWord1; out pcchLocalName: SYSINT;
                      out ppwchQName: PWord1; out pcchQName: SYSINT): HResult; stdcall;
    function  getIndexFromName(var pwchUri: Word; cchUri: SYSINT; var pwchLocalName: Word;
                               cchLocalName: SYSINT; out pnIndex: SYSINT): HResult; stdcall;
    function  getIndexFromQName(var pwchQName: Word; cchQName: SYSINT; out pnIndex: SYSINT): HResult; stdcall;
    function  getType(nIndex: SYSINT; out ppwchType: PWord1; out pcchType: SYSINT): HResult; stdcall;
    function  getTypeFromName(var pwchUri: Word; cchUri: SYSINT; var pwchLocalName: Word;
                              cchLocalName: SYSINT; out ppwchType: PWord1; out pcchType: SYSINT): HResult; stdcall;
    function  getTypeFromQName(var pwchQName: Word; cchQName: SYSINT; out ppwchType: PWord1;
                               out pcchType: SYSINT): HResult; stdcall;
    function  getValue(nIndex: SYSINT; out ppwchValue: PWord1; out pcchValue: SYSINT): HResult; stdcall;
    function  getValueFromName(var pwchUri: Word; cchUri: SYSINT; var pwchLocalName: Word;
                               cchLocalName: SYSINT; out ppwchValue: PWord1; out pcchValue: SYSINT): HResult; stdcall;
    function  getValueFromQName(var pwchQName: Word; cchQName: SYSINT; out ppwchValue: PWord1;
                                out pcchValue: SYSINT): HResult; stdcall;
  end;

  ISAXDTDHandler = interface(IUnknown)
    ['{E15C1BAF-AFB3-4D60-8C36-19A8C45DEFED}']
    function  notationDecl(var pwchName: Word; cchName: SYSINT; var pwchPublicId: Word;
                           cchPublicId: SYSINT; var pwchSystemId: Word; cchSystemId: SYSINT): HResult; stdcall;
    function  unparsedEntityDecl(var pwchName: Word; cchName: SYSINT; var pwchPublicId: Word;
                                 cchPublicId: SYSINT; var pwchSystemId: Word; cchSystemId: SYSINT;
                                 var pwchNotationName: Word; cchNotationName: SYSINT): HResult; stdcall;
  end;

  ISAXErrorHandler = interface(IUnknown)
    ['{A60511C4-CCF5-479E-98A3-DC8DC545B7D0}']
    function  error(const pLocator: ISAXLocator; var pwchErrorMessage: Word; hrErrorCode: HResult): HResult; stdcall;
    function  fatalError(const pLocator: ISAXLocator; var pwchErrorMessage: Word;
                         hrErrorCode: HResult): HResult; stdcall;
    function  ignorableWarning(const pLocator: ISAXLocator; var pwchErrorMessage: Word;
                               hrErrorCode: HResult): HResult; stdcall;
  end;

  ISAXXMLFilter = interface(ISAXXMLReader)
    ['{70409222-CA09-4475-ACB8-40312FE8D145}']
    function  getParent(out ppReader: ISAXXMLReader): HResult; stdcall;
    function  putParent(const pReader: ISAXXMLReader): HResult; stdcall;
  end;

  ISAXLexicalHandler = interface(IUnknown)
    ['{7F85D5F5-47A8-4497-BDA5-84BA04819EA6}']
    function  startDTD(var pwchName: Word; cchName: SYSINT; var pwchPublicId: Word;
                       cchPublicId: SYSINT; var pwchSystemId: Word; cchSystemId: SYSINT): HResult; stdcall;
    function  endDTD: HResult; stdcall;
    function  startEntity(var pwchName: Word; cchName: SYSINT): HResult; stdcall;
    function  endEntity(var pwchName: Word; cchName: SYSINT): HResult; stdcall;
    function  startCDATA: HResult; stdcall;
    function  endCDATA: HResult; stdcall;
    function  comment(var pwchChars: Word; cchChars: SYSINT): HResult; stdcall;
  end;

  ISAXDeclHandler = interface(IUnknown)
    ['{862629AC-771A-47B2-8337-4E6843C1BE90}']
    function  elementDecl(var pwchName: Word; cchName: SYSINT; var pwchModel: Word; cchModel: SYSINT): HResult; stdcall;
    function  attributeDecl(var pwchElementName: Word; cchElementName: SYSINT;
                            var pwchAttributeName: Word; cchAttributeName: SYSINT;
                            var pwchType: Word; cchType: SYSINT; var pwchValueDefault: Word;
                            cchValueDefault: SYSINT; var pwchValue: Word; cchValue: SYSINT): HResult; stdcall;
    function  internalEntityDecl(var pwchName: Word; cchName: SYSINT; var pwchValue: Word;
                                 cchValue: SYSINT): HResult; stdcall;
    function  externalEntityDecl(var pwchName: Word; cchName: SYSINT; var pwchPublicId: Word;
                                 cchPublicId: SYSINT; var pwchSystemId: Word; cchSystemId: SYSINT): HResult; stdcall;
  end;

  IVBSAXXMLReader = interface(IDispatch)
    ['{8C033CAA-6CD6-4F73-B728-4531AF74945F}']
    function  getFeature(const strName: WideString): WordBool; safecall;
    procedure putFeature(const strName: WideString; fValue: WordBool); safecall;
    function  getProperty(const strName: WideString): OleVariant; safecall;
    procedure putProperty(const strName: WideString; varValue: OleVariant); safecall;
    function  Get_entityResolver: IVBSAXEntityResolver; safecall;
    procedure Set_entityResolver(const oResolver: IVBSAXEntityResolver); safecall;
    function  Get_contentHandler: IVBSAXContentHandler; safecall;
    procedure Set_contentHandler(const oHandler: IVBSAXContentHandler); safecall;
    function  Get_dtdHandler: IVBSAXDTDHandler; safecall;
    procedure Set_dtdHandler(const oHandler: IVBSAXDTDHandler); safecall;
    function  Get_errorHandler: IVBSAXErrorHandler; safecall;
    procedure Set_errorHandler(const oHandler: IVBSAXErrorHandler); safecall;
    function  Get_baseURL: WideString; safecall;
    procedure Set_baseURL(const strBaseURL: WideString); safecall;
    function  Get_secureBaseURL: WideString; safecall;
    procedure Set_secureBaseURL(const strSecureBaseURL: WideString); safecall;
    procedure parse(varInput: OleVariant); safecall;
    procedure parseURL(const strURL: WideString); safecall;
    property entityResolver: IVBSAXEntityResolver read Get_entityResolver write Set_entityResolver;
    property contentHandler: IVBSAXContentHandler read Get_contentHandler write Set_contentHandler;
    property dtdHandler: IVBSAXDTDHandler read Get_dtdHandler write Set_dtdHandler;
    property errorHandler: IVBSAXErrorHandler read Get_errorHandler write Set_errorHandler;
    property baseURL: WideString read Get_baseURL write Set_baseURL;
    property secureBaseURL: WideString read Get_secureBaseURL write Set_secureBaseURL;
  end;

  IVBSAXEntityResolver = interface(IDispatch)
    ['{0C05D096-F45B-4ACA-AD1A-AA0BC25518DC}']
    function  resolveEntity(var strPublicId: WideString; var strSystemId: WideString): OleVariant; safecall;
  end;

  IVBSAXContentHandler = interface(IDispatch)
    ['{2ED7290A-4DD5-4B46-BB26-4E4155E77FAA}']
    procedure Set_documentLocator(const Param1: IVBSAXLocator); safecall;
    procedure startDocument; safecall;
    procedure endDocument; safecall;
    procedure startPrefixMapping(var strPrefix: WideString; var strURI: WideString); safecall;
    procedure endPrefixMapping(var strPrefix: WideString); safecall;
    procedure startElement(var strNamespaceURI: WideString; var strLocalName: WideString;
                           var strQName: WideString; const oAttributes: IVBSAXAttributes); safecall;
    procedure endElement(var strNamespaceURI: WideString; var strLocalName: WideString;
                         var strQName: WideString); safecall;
    procedure characters(var strChars: WideString); safecall;
    procedure ignorableWhitespace(var strChars: WideString); safecall;
    procedure processingInstruction(var strTarget: WideString; var strData: WideString); safecall;
    procedure skippedEntity(var strName: WideString); safecall;
    property documentLocator: IVBSAXLocator write Set_documentLocator;
  end;

  IVBSAXLocator = interface(IDispatch)
    ['{796E7AC5-5AA2-4EFF-ACAD-3FAAF01A3288}']
    function  Get_columnNumber: SYSINT; safecall;
    function  Get_lineNumber: SYSINT; safecall;
    function  Get_publicId: WideString; safecall;
    function  Get_systemId: WideString; safecall;
    property columnNumber: SYSINT read Get_columnNumber;
    property lineNumber: SYSINT read Get_lineNumber;
    property publicId: WideString read Get_publicId;
    property systemId: WideString read Get_systemId;
  end;

  IVBSAXAttributes = interface(IDispatch)
    ['{10DC0586-132B-4CAC-8BB3-DB00AC8B7EE0}']
    function  Get_length: SYSINT; safecall;
    function  getURI(nIndex: SYSINT): WideString; safecall;
    function  getLocalName(nIndex: SYSINT): WideString; safecall;
    function  getQName(nIndex: SYSINT): WideString; safecall;
    function  getIndexFromName(const strURI: WideString; const strLocalName: WideString): SYSINT; safecall;
    function  getIndexFromQName(const strQName: WideString): SYSINT; safecall;
    function  getType(nIndex: SYSINT): WideString; safecall;
    function  getTypeFromName(const strURI: WideString; const strLocalName: WideString): WideString; safecall;
    function  getTypeFromQName(const strQName: WideString): WideString; safecall;
    function  getValue(nIndex: SYSINT): WideString; safecall;
    function  getValueFromName(const strURI: WideString; const strLocalName: WideString): WideString; safecall;
    function  getValueFromQName(const strQName: WideString): WideString; safecall;
    property length: SYSINT read Get_length;
  end;

  IVBSAXDTDHandler = interface(IDispatch)
    ['{24FB3297-302D-4620-BA39-3A732D850558}']
    procedure notationDecl(var strName: WideString; var strPublicId: WideString;
                           var strSystemId: WideString); safecall;
    procedure unparsedEntityDecl(var strName: WideString; var strPublicId: WideString;
                                 var strSystemId: WideString; var strNotationName: WideString); safecall;
  end;

  IVBSAXErrorHandler = interface(IDispatch)
    ['{D963D3FE-173C-4862-9095-B92F66995F52}']
    procedure error(const oLocator: IVBSAXLocator; var strErrorMessage: WideString;
                    nErrorCode: Integer); safecall;
    procedure fatalError(const oLocator: IVBSAXLocator; var strErrorMessage: WideString;
                         nErrorCode: Integer); safecall;
    procedure ignorableWarning(const oLocator: IVBSAXLocator; var strErrorMessage: WideString;
                               nErrorCode: Integer); safecall;
  end;

  IVBSAXXMLFilter = interface(IDispatch)
    ['{1299EB1B-5B88-433E-82DE-82CA75AD4E04}']
    function  Get_parent: IVBSAXXMLReader; safecall;
    procedure Set_parent(const oReader: IVBSAXXMLReader); safecall;
    property parent: IVBSAXXMLReader read Get_parent write Set_parent;
  end;

  IVBSAXLexicalHandler = interface(IDispatch)
    ['{032AAC35-8C0E-4D9D-979F-E3B702935576}']
    procedure startDTD(var strName: WideString; var strPublicId: WideString;
                       var strSystemId: WideString); safecall;
    procedure endDTD; safecall;
    procedure startEntity(var strName: WideString); safecall;
    procedure endEntity(var strName: WideString); safecall;
    procedure startCDATA; safecall;
    procedure endCDATA; safecall;
    procedure comment(var strChars: WideString); safecall;
  end;

  IVBSAXDeclHandler = interface(IDispatch)
    ['{E8917260-7579-4BE1-B5DD-7AFBFA6F077B}']
    procedure elementDecl(var strName: WideString; var strModel: WideString); safecall;
    procedure attributeDecl(var strElementName: WideString; var strAttributeName: WideString;
                            var strType: WideString; var strValueDefault: WideString;
                            var strValue: WideString); safecall;
    procedure internalEntityDecl(var strName: WideString; var strValue: WideString); safecall;
    procedure externalEntityDecl(var strName: WideString; var strPublicId: WideString;
                                 var strSystemId: WideString); safecall;
  end;

  IMXWriter = interface(IDispatch)
    ['{4D7FF4BA-1565-4EA8-94E1-6E724A46F98D}']
    procedure Set_output(varDestination: OleVariant); safecall;
    function  Get_output: OleVariant; safecall;
    procedure Set_encoding(const strEncoding: WideString); safecall;
    function  Get_encoding: WideString; safecall;
    procedure Set_byteOrderMark(fWriteByteOrderMark: WordBool); safecall;
    function  Get_byteOrderMark: WordBool; safecall;
    procedure Set_indent(fIndentMode: WordBool); safecall;
    function  Get_indent: WordBool; safecall;
    procedure Set_standalone(fValue: WordBool); safecall;
    function  Get_standalone: WordBool; safecall;
    procedure Set_omitXMLDeclaration(fValue: WordBool); safecall;
    function  Get_omitXMLDeclaration: WordBool; safecall;
    procedure Set_version(const strVersion: WideString); safecall;
    function  Get_version: WideString; safecall;
    procedure Set_disableOutputEscaping(fValue: WordBool); safecall;
    function  Get_disableOutputEscaping: WordBool; safecall;
    procedure flush; safecall;
    property output: OleVariant read Get_output write Set_output;
    property encoding: WideString read Get_encoding write Set_encoding;
    property byteOrderMark: WordBool read Get_byteOrderMark write Set_byteOrderMark;
    property indent: WordBool read Get_indent write Set_indent;
    property standalone: WordBool read Get_standalone write Set_standalone;
    property omitXMLDeclaration: WordBool read Get_omitXMLDeclaration write Set_omitXMLDeclaration;
    property version: WideString read Get_version write Set_version;
    property disableOutputEscaping: WordBool read Get_disableOutputEscaping write Set_disableOutputEscaping;
  end;

  IMXAttributes = interface(IDispatch)
    ['{F10D27CC-3EC0-415C-8ED8-77AB1C5E7262}']
    procedure addAttribute(const strURI: WideString; const strLocalName: WideString;
                           const strQName: WideString; const strType: WideString;
                           const strValue: WideString); safecall;
    procedure addAttributeFromIndex(varAtts: OleVariant; nIndex: SYSINT); safecall;
    procedure clear; safecall;
    procedure removeAttribute(nIndex: SYSINT); safecall;
    procedure setAttribute(nIndex: SYSINT; const strURI: WideString;
                           const strLocalName: WideString; const strQName: WideString;
                           const strType: WideString; const strValue: WideString); safecall;
    procedure setAttributes(varAtts: OleVariant); safecall;
    procedure setLocalName(nIndex: SYSINT; const strLocalName: WideString); safecall;
    procedure setQName(nIndex: SYSINT; const strQName: WideString); safecall;
    procedure setType(nIndex: SYSINT; const strType: WideString); safecall;
    procedure setURI(nIndex: SYSINT; const strURI: WideString); safecall;
    procedure setValue(nIndex: SYSINT; const strValue: WideString); safecall;
  end;

  IMXReaderControl = interface(IDispatch)
    ['{808F4E35-8D5A-4FBE-8466-33A41279ED30}']
    procedure abort; safecall;
    procedure resume; safecall;
    procedure suspend; safecall;
  end;

  IXMLElementCollection = interface(IDispatch)
    ['{65725580-9B5D-11D0-9BFE-00C04FC99C8E}']
    procedure Set_length(p: Integer); safecall;
    function  Get_length: Integer; safecall;
    function  Get__newEnum: IUnknown; safecall;
    function  item(var1: OleVariant; var2: OleVariant): IDispatch; safecall;
    property length: Integer read Get_length write Set_length;
    property _newEnum: IUnknown read Get__newEnum;
  end;

  IXMLDocument = interface(IDispatch)
    ['{F52E2B61-18A1-11D1-B105-00805F49916B}']
    function  Get_root: IXMLElement; safecall;
    function  Get_fileSize: WideString; safecall;
    function  Get_fileModifiedDate: WideString; safecall;
    function  Get_fileUpdatedDate: WideString; safecall;
    function  Get_url: WideString; safecall;
    procedure Set_url(const p: WideString); safecall;
    function  Get_mimeType: WideString; safecall;
    function  Get_readyState: Integer; safecall;
    function  Get_charset: WideString; safecall;
    procedure Set_charset(const p: WideString); safecall;
    function  Get_version: WideString; safecall;
    function  Get_doctype: WideString; safecall;
    function  Get_dtdURL: WideString; safecall;
    function  createElement(vType: OleVariant; var1: OleVariant): IXMLElement; safecall;
    property root: IXMLElement read Get_root;
    property fileSize: WideString read Get_fileSize;
    property fileModifiedDate: WideString read Get_fileModifiedDate;
    property fileUpdatedDate: WideString read Get_fileUpdatedDate;
    property url: WideString read Get_url write Set_url;
    property mimeType: WideString read Get_mimeType;
    property readyState: Integer read Get_readyState;
    property charset: WideString read Get_charset write Set_charset;
    property version: WideString read Get_version;
    property doctype: WideString read Get_doctype;
    property dtdURL: WideString read Get_dtdURL;
  end;

  IXMLElement = interface(IDispatch)
    ['{3F7F31AC-E15F-11D0-9C25-00C04FC99C8E}']
    function  Get_tagName: WideString; safecall;
    procedure Set_tagName(const p: WideString); safecall;
    function  Get_parent: IXMLElement; safecall;
    procedure setAttribute(const strPropertyName: WideString; PropertyValue: OleVariant); safecall;
    function  getAttribute(const strPropertyName: WideString): OleVariant; safecall;
    procedure removeAttribute(const strPropertyName: WideString); safecall;
    function  Get_children: IXMLElementCollection; safecall;
    function  Get_type_: Integer; safecall;
    function  Get_text: WideString; safecall;
    procedure Set_text(const p: WideString); safecall;
    procedure addChild(const pChildElem: IXMLElement; lIndex: Integer; lReserved: Integer); safecall;
    procedure removeChild(const pChildElem: IXMLElement); safecall;
    property tagName: WideString read Get_tagName write Set_tagName;
    property parent: IXMLElement read Get_parent;
    property children: IXMLElementCollection read Get_children;
    property type_: Integer read Get_type_;
    property text: WideString read Get_text write Set_text;
  end;

  IXMLDocument2 = interface(IDispatch)
    ['{2B8DE2FE-8D2D-11D1-B2FC-00C04FD915A9}']
    function  Get_root(out p: IXMLElement2): HResult; stdcall;
    function  Get_fileSize(out p: WideString): HResult; stdcall;
    function  Get_fileModifiedDate(out p: WideString): HResult; stdcall;
    function  Get_fileUpdatedDate(out p: WideString): HResult; stdcall;
    function  Get_url(out p: WideString): HResult; stdcall;
    function  Set_url(const p: WideString): HResult; stdcall;
    function  Get_mimeType(out p: WideString): HResult; stdcall;
    function  Get_readyState(out pl: Integer): HResult; stdcall;
    function  Get_charset(out p: WideString): HResult; stdcall;
    function  Set_charset(const p: WideString): HResult; stdcall;
    function  Get_version(out p: WideString): HResult; stdcall;
    function  Get_doctype(out p: WideString): HResult; stdcall;
    function  Get_dtdURL(out p: WideString): HResult; stdcall;
    function  createElement(vType: OleVariant; var1: OleVariant; out ppElem: IXMLElement2): HResult; stdcall;
    function  Get_async(out pf: WordBool): HResult; stdcall;
    function  Set_async(pf: WordBool): HResult; stdcall;
  end;

  IXMLElement2 = interface(IDispatch)
    ['{2B8DE2FF-8D2D-11D1-B2FC-00C04FD915A9}']
    function  Get_tagName: WideString; safecall;
    procedure Set_tagName(const p: WideString); safecall;
    function  Get_parent: IXMLElement2; safecall;
    procedure setAttribute(const strPropertyName: WideString; PropertyValue: OleVariant); safecall;
    function  getAttribute(const strPropertyName: WideString): OleVariant; safecall;
    procedure removeAttribute(const strPropertyName: WideString); safecall;
    function  Get_children: IXMLElementCollection; safecall;
    function  Get_type_: Integer; safecall;
    function  Get_text: WideString; safecall;
    procedure Set_text(const p: WideString); safecall;
    procedure addChild(const pChildElem: IXMLElement2; lIndex: Integer; lReserved: Integer); safecall;
    procedure removeChild(const pChildElem: IXMLElement2); safecall;
    function  Get_attributes: IXMLElementCollection; safecall;
    property tagName: WideString read Get_tagName write Set_tagName;
    property parent: IXMLElement2 read Get_parent;
    property children: IXMLElementCollection read Get_children;
    property type_: Integer read Get_type_;
    property text: WideString read Get_text write Set_text;
    property attributes: IXMLElementCollection read Get_attributes;
  end;

  IXMLAttribute = interface(IDispatch)
    ['{D4D4A0FC-3B73-11D1-B2B4-00C04FB92596}']
    function  Get_name: WideString; safecall;
    function  Get_value: WideString; safecall;
    property name: WideString read Get_name;
    property value: WideString read Get_value;
  end;

  IXMLError = interface(IUnknown)
    ['{948C5AD3-C58D-11D0-9C0B-00C04FC99C8E}']
    function  GetErrorInfo(var pErrorReturn: TGUID): HResult; stdcall;
  end;

  IXMLDOMSelection = interface(IXMLDOMNodeList)
    ['{AA634FC7-5888-44A7-A257-3A47150D3A0E}']
    function  Get_expr: WideString; safecall;
    procedure Set_expr(const expression: WideString); safecall;
    function  Get_context: IXMLDOMNode; safecall;
    procedure Set_context(const ppNode: IXMLDOMNode); safecall;
    function  peekNode: IXMLDOMNode; safecall;
    function  matches(const pNode: IXMLDOMNode): IXMLDOMNode; safecall;
    function  removeNext: IXMLDOMNode; safecall;
    procedure removeAll; safecall;
    function  clone: IXMLDOMSelection; safecall;
    function  getProperty(const name: WideString): OleVariant; safecall;
    procedure setProperty(const name: WideString; value: OleVariant); safecall;
    property expr: WideString read Get_expr write Set_expr;
    property context: IXMLDOMNode read Get_context write Set_context;
  end;

  IDSOControl = interface(IDispatch)
    ['{310AFA62-0575-11D2-9CA9-0060B0EC3D39}']
    function  Get_XMLDocument: IXMLDOMDocument; safecall;
    procedure Set_XMLDocument(const ppDoc: IXMLDOMDocument); safecall;
    function  Get_JavaDSOCompatible: Integer; safecall;
    procedure Set_JavaDSOCompatible(fJavaDSOCompatible: Integer); safecall;
    function  Get_readyState: Integer; safecall;
    property XMLDocument: IXMLDOMDocument read Get_XMLDocument write Set_XMLDocument;
    property JavaDSOCompatible: Integer read Get_JavaDSOCompatible write Set_JavaDSOCompatible;
    property readyState: Integer read Get_readyState;
  end;

  IXMLHTTPRequest = interface(IDispatch)
    ['{ED8C108D-4349-11D2-91A4-00C04F7969E8}']
    procedure open(const bstrMethod: WideString; const bstrUrl: WideString; varAsync: OleVariant;
                   bstrUser: OleVariant; bstrPassword: OleVariant); safecall;
    procedure setRequestHeader(const bstrHeader: WideString; const bstrValue: WideString); safecall;
    function  getResponseHeader(const bstrHeader: WideString): WideString; safecall;
    function  getAllResponseHeaders: WideString; safecall;
    procedure send(varBody: OleVariant); safecall;
    procedure abort; safecall;
    function  Get_status: Integer; safecall;
    function  Get_statusText: WideString; safecall;
    function  Get_responseXML: IDispatch; safecall;
    function  Get_responseText: WideString; safecall;
    function  Get_responseBody: OleVariant; safecall;
    function  Get_responseStream: OleVariant; safecall;
    function  Get_readyState: Integer; safecall;
    procedure Set_onreadystatechange(const Param1: IDispatch); safecall;
    property status: Integer read Get_status;
    property statusText: WideString read Get_statusText;
    property responseXML: IDispatch read Get_responseXML;
    property responseText: WideString read Get_responseText;
    property responseBody: OleVariant read Get_responseBody;
    property responseStream: OleVariant read Get_responseStream;
    property readyState: Integer read Get_readyState;
    property onreadystatechange: IDispatch write Set_onreadystatechange;
  end;

  IServerXMLHTTPRequest = interface(IXMLHTTPRequest)
    ['{2E9196BF-13BA-4DD4-91CA-6C571F281495}']
    procedure setTimeouts(resolveTimeout: Integer; connectTimeout: Integer; sendTimeout: Integer;
                          receiveTimeout: Integer); safecall;
    function  waitForResponse(timeoutInSeconds: OleVariant): WordBool; safecall;
    function  getOption(option: SERVERXMLHTTP_OPTION): OleVariant; safecall;
    procedure setOption(option: SERVERXMLHTTP_OPTION; value: OleVariant); safecall;
  end;
{$endif}

implementation

end.

