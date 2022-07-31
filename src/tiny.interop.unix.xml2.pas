unit Tiny.Interop.Unix.Xml2;

{$i tiny.inc}

interface

{$ifdef unix}
type
  cint = LongInt;
  pcint = PLongInt;
  cuint = LongWord;
  cushort = Word;
  clong = Integer;
  culong = Cardinal;
  csize_t = Cardinal;
  cdouble = Double;

  xmlCharPtr = PChar;
  xmlCharPtrPtr = PPChar;
  xmlChar = Char;
  xmlBufferPtr = ^xmlBuffer;
  xmlNotationPtr = ^xmlNotation;
  xmlEnumerationPtr = ^xmlEnumeration;
  xmlAttributePtr = ^xmlAttribute;
  xmlElementContentPtr = ^xmlElementContent;
  xmlElementPtr = ^xmlElement;
  xmlNsPtr = ^xmlNs;
  xmlNsPtrPtr = ^xmlNsPtr;
  xmlNodePtr = ^xmlNode;
  xmlNodePtrPtr = ^xmlNodePtr;
  xmlDtdPtr = ^xmlDtd;
  xmlAttrPtr = ^xmlAttr;
  xmlIDPtr = ^xmlID;
  xmlRefPtr = ^xmlRef;
  xmlDocPtr = ^xmlDoc;
  xmlBufferAllocationSchemePtr = ^xmlBufferAllocationScheme;
  xmlParserInputBufferPtr = ^xmlParserInputBuffer;
  xmlOutputBufferPtr = ^xmlOutputBuffer;
  xmlXPathContextPtr = ^xmlXPathContext;
  xmlXPathParserContextPtr = ^xmlXPathParserContext;
  xmlNodeSetPtr = ^xmlNodeSet;
  xmlXPathObjectPtr = ^xmlXPathObject;
  xmlXPathObjectPtrPtr = ^xmlXPathObjectPtr;
  xmlXPathTypePtr = ^xmlXPathType;
  xmlXPathVariablePtr = ^xmlXPathVariable;
  xmlXPathFuncPtr = ^xmlXPathFunc;
  xmlXPathAxisPtr = ^xmlXPathAxis;
  xmlXPathCompExprPtr = ^xmlXPathCompExpr;
  xmlCharEncodingHandlerPtr = ^xmlCharEncodingHandler;
  xmlDictPtr = Pointer;
  xmlHashTablePtr = Pointer;
  xmlStructuredErrorFunc = Pointer;

  xmlCharEncoding = (
    XML_CHAR_ENCODING_ERROR = -1, (* No char encoding detected *)
    XML_CHAR_ENCODING_NONE = 0, (* No char encoding detected *)
    XML_CHAR_ENCODING_UTF8 = 1, (* UTF-8 *)
    XML_CHAR_ENCODING_UTF16LE = 2, (* UTF-16 little endian *)
    XML_CHAR_ENCODING_UTF16BE = 3, (* UTF-16 big endian *)
    XML_CHAR_ENCODING_UCS4LE = 4, (* UCS-4 little endian *)
    XML_CHAR_ENCODING_UCS4BE = 5, (* UCS-4 big endian *)
    XML_CHAR_ENCODING_EBCDIC = 6, (* EBCDIC uh! *)
    XML_CHAR_ENCODING_UCS4_2143= 7, (* UCS-4 unusual ordering *)
    XML_CHAR_ENCODING_UCS4_3412= 8, (* UCS-4 unusual ordering *)
    XML_CHAR_ENCODING_UCS2=  9, (* UCS-2 *)
    XML_CHAR_ENCODING_8859_1 = 10, (* ISO-8859-1 ISO Latin 1 *)
    XML_CHAR_ENCODING_8859_2 = 11, (* ISO-8859-2 ISO Latin 2 *)
    XML_CHAR_ENCODING_8859_3 = 12, (* ISO-8859-3 *)
    XML_CHAR_ENCODING_8859_4 = 13, (* ISO-8859-4 *)
    XML_CHAR_ENCODING_8859_5 = 14, (* ISO-8859-5 *)
    XML_CHAR_ENCODING_8859_6 = 15, (* ISO-8859-6 *)
    XML_CHAR_ENCODING_8859_7 = 16, (* ISO-8859-7 *)
    XML_CHAR_ENCODING_8859_8 = 17, (* ISO-8859-8 *)
    XML_CHAR_ENCODING_8859_9 = 18, (* ISO-8859-9 *)
    XML_CHAR_ENCODING_2022_JP = 19, (* ISO-2022-JP *)
    XML_CHAR_ENCODING_SHIFT_JIS = 20, (* Shift_JIS *)
    XML_CHAR_ENCODING_EUC_JP = 21, (* EUC-JP *)
    XML_CHAR_ENCODING_ASCII = 22 (* pure ASCII *)
  );

  xmlCharEncodingInputFunc = function(_out: pchar; outlen: pcint; _in: pchar; inlen: pcint): cint; cdecl;
  xmlCharEncodingOutputFunc = function(_out: pchar; outlen: pcint; _in: pchar; inlen: pcint): cint; cdecl;

  xmlCharEncodingHandler = record
    name: pchar;
    input: xmlCharEncodingInputFunc;
    output: xmlCharEncodingOutputFunc;
  end;

(*
 * xmlBufferAllocationScheme:
 *
 * A buffer allocation scheme can be defined to either match exactly the
 * need or double it's allocated size each time it is found too small.
 *)
  xmlBufferAllocationScheme = (
    XML_BUFFER_ALLOC_DOUBLEIT,
    XML_BUFFER_ALLOC_EXACT,
    XML_BUFFER_ALLOC_IMMUTABLE
  );

(*
 * xmlBuffer:
 *
 * A buffer structure.
 *)
  xmlBuffer = record
    content: xmlCharPtr; (* The buffer content UTF8 *)
    use: cuint; (* The buffer size used *)
    size: cuint; (* The buffer size *)
    alloc: xmlBufferAllocationScheme; (* The realloc method *)
  end;

(*
 * The different element types carried by an XML tree.
 *
 * NOTE: This is synchronized with DOM Level1 values
 *       See http://www.w3.org/TR/REC-DOM-Level-1/
 *
 * Actually this had diverged a bit, and now XML_DOCUMENT_TYPE_NODE should
 * be deprecated to use an XML_DTD_NODE.
 *)
  xmlElementType = (
    XML_ELEMENT_NODE = 1,
    XML_ATTRIBUTE_NODE = 2,
    XML_TEXT_NODE = 3,
    XML_CDATA_SECTION_NODE = 4,
    XML_ENTITY_REF_NODE = 5,
    XML_ENTITY_NODE = 6,
    XML_PI_NODE = 7,
    XML_COMMENT_NODE = 8,
    XML_DOCUMENT_NODE = 9,
    XML_DOCUMENT_TYPE_NODE = 10,
    XML_DOCUMENT_FRAG_NODE = 11,
    XML_NOTATION_NODE = 12,
    XML_HTML_DOCUMENT_NODE = 13,
    XML_DTD_NODE = 14,
    XML_ELEMENT_DECL = 15,
    XML_ATTRIBUTE_DECL = 16,
    XML_ENTITY_DECL = 17,
    XML_NAMESPACE_DECL = 18,
    XML_XINCLUDE_START = 19,
    XML_XINCLUDE_END = 20
  );

(*
 * xmlNotation:
 *
 * A DTD Notation definition.
 *)
  xmlNotation = record
    name: xmlCharPtr; (* Notation name *)
    PublicID: xmlCharPtr; (* Public identifier, if any *)
    SystemID: xmlCharPtr; (* System identifier, if any *)
  end;

(*
 * xmlAttributeType:
 *
 * A DTD Attribute type definition.
 *)
  xmlAttributeType = (
    XML_ATTRIBUTE_CDATA = 1,
    XML_ATTRIBUTE_ID,
    XML_ATTRIBUTE_IDREF  ,
    XML_ATTRIBUTE_IDREFS,
    XML_ATTRIBUTE_ENTITY,
    XML_ATTRIBUTE_ENTITIES,
    XML_ATTRIBUTE_NMTOKEN,
    XML_ATTRIBUTE_NMTOKENS,
    XML_ATTRIBUTE_ENUMERATION,
    XML_ATTRIBUTE_NOTATION
  );

(*
 * xmlAttributeDefault:
 *
 * A DTD Attribute default definition.
 *)
  xmlAttributeDefault = (
    XML_ATTRIBUTE_NONE = 1,
    XML_ATTRIBUTE_REQUIRED,
    XML_ATTRIBUTE_IMPLIED,
    XML_ATTRIBUTE_FIXED
  );

(*
 * xmlEnumeration:
 *
 * List structure used when there is an enumeration in DTDs.
 *)
  xmlEnumeration = record
    next: xmlEnumerationPtr; (* next one *)
    name: xmlCharPtr;
  end;

(*
 * xmlAttribute:
 *
 * An Attribute declaration in a DTD.
 *)
  xmlAttribute = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_ATTRIBUTE_DECL, must be second ! *)
    name: xmlCharPtr; (* Attribute name *)
    children: xmlNodePtr; (* NULL *)
    last: xmlNodePtr; (* NULL *)
    parent: xmlDtdPtr; (* -> DTD *)
    next: xmlNodePtr; (* next sibling link  *)
    prev: xmlNodePtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    nexth: xmlAttributePtr; (* next in hash table *)
    atype: xmlAttributeType; (* The attribute type *)
    def: xmlAttributeDefault; (* the default *)
    defaultValue: xmlCharPtr; (* or the default value *)
    tree: xmlEnumerationPtr; (* or the enumeration tree if any *)
    prefix: xmlCharPtr; (* the namespace prefix if any *)
    elem: xmlCharPtr; (* Element holding the attribute *)
  end;

(*
 * xmlElementContentType:
 *
 * Possible definitions of element content types.
 *)
  xmlElementContentType = (
    XML_ELEMENT_CONTENT_PCDATA = 1,
    XML_ELEMENT_CONTENT_ELEMENT,
    XML_ELEMENT_CONTENT_SEQ,
    XML_ELEMENT_CONTENT_OR
  );

(*
 * xmlElementContentOccur:
 *
 * Possible definitions of element content occurrences.
 *)
  xmlElementContentOccur = (
    XML_ELEMENT_CONTENT_ONCE = 1,
    XML_ELEMENT_CONTENT_OPT,
    XML_ELEMENT_CONTENT_MULT,
    XML_ELEMENT_CONTENT_PLUS
  );

(*
 * xmlElementContent:
 *
 * An XML Element content as stored after parsing an element definition
 * in a DTD.
 *)
  xmlElementContent = record
    _type: xmlElementContentType; (* PCDATA, ELEMENT, SEQ or OR *)
    ocur: xmlElementContentOccur; (* ONCE, OPT, MULT or PLUS *)
    name: xmlCharPtr; (* Element name *)
    c1: xmlElementContentPtr; (* first child *)
    c2: xmlElementContentPtr; (* second child *)
    parent: xmlElementContentPtr; (* parent *)
    prefix: xmlCharPtr; (* Namespace prefix *)
  end;

(*
 * xmlElementTypeVal:
 *
 * The different possibilities for an element content type.
 *)
  xmlElementTypeVal = (
    XML_ELEMENT_TYPE_UNDEFINED = 0,
    XML_ELEMENT_TYPE_EMPTY = 1,
    XML_ELEMENT_TYPE_ANY,
    XML_ELEMENT_TYPE_MIXED,
    XML_ELEMENT_TYPE_ELEMENT
  );

(*
 * xmlElement:
 *
 * An XML Element declaration from a DTD.
 *)
  xmlElement = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_ELEMENT_DECL, must be second ! *)
    name: xmlCharPtr; (* Element name *)
    children: xmlNodePtr; (* NULL *)
    last: xmlNodePtr; (* NULL *)
    parent: xmlDtdPtr; (* -> DTD *)
    next: xmlNodePtr; (* next sibling link  *)
    prev: xmlNodePtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    etype: xmlElementTypeVal; (* The type *)
    content: xmlElementContentPtr; (* the allowed element content *)
    attributes: xmlAttributePtr; (* List of the declared attributes *)
    prefix: xmlCharPtr; (* the namespace prefix if any *)
    contModel: pointer;
  end;

  xmlNsType = xmlElementType;

(*
 * xmlNs:
 *
 * An XML namespace.
 * Note that prefix == NULL is valid, it defines the default namespace
 * within the subtree (until overridden).
 *
 * xmlNsType is unified with xmlElementType.
 *)
  xmlNs = record
    next: xmlNsPtr; (* next Ns link for this node  *)
    _type: xmlNsType; (* global or local *)
    href: xmlCharPtr; (* URL for the namespace *)
    prefix: xmlCharPtr; (* prefix for the namespace *)
    _private: pointer; (* application data *)
    context: xmlDocPtr; (* normally an xmlDoc *)
  end;

(*
 * xmlDtd:
 *
 * An XML DTD, as defined by <!DOCTYPE ... There is actually one for
 * the internal subset and for the external subset.
 *)
  xmlDtd = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_DTD_NODE, must be second ! *)
    name: xmlCharPtr; (* Name of the DTD *)
    children: xmlNodePtr; (* the value of the property link *)
    last: xmlNodePtr; (* last child link *)
    parent: xmlDocPtr; (* child->parent link *)
    next: xmlNodePtr; (* next sibling link  *)
    prev: xmlNodePtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    notations: pointer; (* Hash table for notations if any *)
    elements: pointer; (* Hash table for elements if any *)
    attributes: pointer; (* Hash table for attributes if any *)
    entities: pointer; (* Hash table for entities if any *)
    ExternalID: xmlCharPtr; (* External identifier for PUBLIC DTD *)
    SystemID: xmlCharPtr; (* URI for a SYSTEM or PUBLIC DTD *)
    pentities: pointer; (* Hash table for param entities if any *)
  end;

(*
 * xmlAttr:
 *
 * An attribute on an XML node.
 *)
  xmlAttr = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_ATTRIBUTE_NODE, must be second ! *)
    name: xmlCharPtr; (* the name of the property *)
    children: xmlNodePtr; (* the value of the property *)
    last: xmlNodePtr; (* NULL *)
    parent: xmlNodePtr; (* child->parent link *)
    next: xmlAttrPtr; (* next sibling link  *)
    prev: xmlAttrPtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    ns: xmlNsPtr; (* pointer to the associated namespace *)
    atype: xmlAttributeType; (* the attribute type if validating *)
    psvi: pointer; (* for type/PSVI informations *)
  end;

(*
 * xmlID:
 *
 * An XML ID instance.
 *)
  xmlID = record
    next: xmlIDPtr; (* next ID *)
    value: xmlCharPtr; (* The ID name *)
    attr: xmlAttrPtr; (* The attribute holding it *)
    name: xmlCharPtr; (* The attribute if attr is not available *)
    lineno: cint; (* The line number if attr is not available *)
    doc: xmlDocPtr; (* The document holding the ID *)
  end;

(*
 * xmlRef:
 *
 * An XML IDREF instance.
 *)
  xmlRef = record
    next: xmlRefPtr; (* next Ref *)
    value: xmlCharPtr; (* The Ref name *)
    attr: xmlAttrPtr; (* The attribute holding it *)
    name: xmlCharPtr; (* The attribute if attr is not available *)
    lineno: cint; (* The line number if attr is not available *)
  end;

(*
 * xmlNode:
 *
 * A node in an XML tree.
 *)
  xmlNode = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* type number, must be second ! *)
    name: xmlCharPtr; (* the name of the node, or the entity *)
    children: xmlNodePtr; (* parent->childs link *)
    last: xmlNodePtr; (* last child link *)
    parent: xmlNodePtr; (* child->parent link *)
    next: xmlNodePtr; (* next sibling link  *)
    prev: xmlNodePtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* the containing document *)
    ns: xmlNsPtr; (* pointer to the associated namespace *)
    content: xmlCharPtr; (* the content *)
    properties: xmlAttrPtr;(* properties list *)
    nsDef: xmlNsPtr; (* namespace definitions on this node *)
    psvi: pointer; (* for type/PSVI informations *)
    line: cushort; (* line number *)
    extra: cushort; (* extra data for XPath/XSLT *)
  end;

  xmlDoc = record
    _private: pointer; (* application data *)
    _type: xmlElementType; (* XML_DOCUMENT_NODE, must be second ! *)
    name: pchar; (* name/filename/URI of the document *)
    children: xmlCharPtr; (* the document tree *)
    last: xmlCharPtr; (* last child link *)
    parent: xmlCharPtr; (* child->parent link *)
    next: xmlCharPtr; (* next sibling link  *)
    prev: xmlCharPtr; (* previous sibling link  *)
    doc: xmlDocPtr; (* autoreference to itself *)
    compression: cint; (* level of zlib compression *)
    standalone: cint; (* standalone document (no external refs)
             1 if standalone="yes"
             0 if standalone="no"
            -1 if there is no XML declaration
            -2 if there is an XML declaration, but no
          standalone attribute was specified *)
    intSubset: xmlDtdPtr; (* the document internal subset *)
    extSubset: xmlDtdPtr; (* the document external subset *)
    oldNs: xmlNsPtr; (* Global namespace, the old way *)
    version: xmlCharPtr; (* the XML version string *)
    encoding: xmlCharPtr; (* external initial encoding, if any *)
    ids: pointer; (* Hash table for ID attributes if any *)
    refs: pointer; (* Hash table for IDREFs attributes if any *)
    URL: xmlCharPtr; (* The URI for that document *)
    charset: cint; (* encoding of the in-memory content actually an xmlCharEncoding *)
    dict: xmlDictPtr; (* dict used to allocate names or NULL *)
    psvi: pointer; (* for type/PSVI informations *)
  end;

(*
 * xmlInputMatchCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to detect if the current handler
 * can provide input fonctionnalities for this resource.
 *
 * Returns 1 if yes and 0 if another Input module should be used
 *)
  xmlInputMatchCallback = function(filename: pchar): cint; cdecl;

(*
 * xmlInputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Input API to open the resource
 *
 * Returns an Input context or NULL in case or error
 *)
  xmlInputOpenCallback = function(filename: pchar): pointer; cdecl;

(*
 * xmlInputReadCallback:
 * @context:  an Input context
 * @buffer:  the buffer to store data read
 * @len:  the length of the buffer in bytes
 *
 * Callback used in the I/O Input API to read the resource
 *
 * Returns the number of bytes read or -1 in case of error
 *)
  xmlInputReadCallback = function(context: pointer; buffer: pchar; len: cint): cint; cdecl;

(*
 * xmlInputCloseCallback:
 * @context:  an Input context
 *
 * Callback used in the I/O Input API to close the resource
 *
 * Returns 0 or -1 in case of error
 *)
  xmlInputCloseCallback = function(context: pointer): cint; cdecl;

(*
 * Those are the functions and datatypes for the library output
 * I/O structures.
 *)

(*
 * xmlOutputMatchCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to detect if the current handler
 * can provide output fonctionnalities for this resource.
 *
 * Returns 1 if yes and 0 if another Output module should be used
 *)
  xmlOutputMatchCallback = function(filename: pchar): cint; cdecl;

(*
 * xmlOutputOpenCallback:
 * @filename: the filename or URI
 *
 * Callback used in the I/O Output API to open the resource
 *
 * Returns an Output context or NULL in case or error
 *)
  xmlOutputOpenCallback = function(filename: pchar): pointer; cdecl;

(*
 * xmlOutputWriteCallback:
 * @context:  an Output context
 * @buffer:  the buffer of data to write
 * @len:  the length of the buffer in bytes
 *
 * Callback used in the I/O Output API to write to the resource
 *
 * Returns the number of bytes written or -1 in case of error
 *)
  xmlOutputWriteCallback = function(context: pointer; buffer: pchar; len: cint): cint; cdecl;

(*
 * xmlOutputCloseCallback:
 * @context:  an Output context
 *
 * Callback used in the I/O Output API to close the resource
 *
 * Returns 0 or -1 in case of error
 *)
  xmlOutputCloseCallback = function(context: pointer): cint; cdecl;

  xmlParserInputBuffer = record
    context: pointer;
    readcallback: xmlInputReadCallback;
    closecallback: xmlInputCloseCallback;
    encoder: xmlCharEncodingHandlerPtr; (* I18N conversions to UTF-8 *)
    buffer: xmlBufferPtr; (* Local buffer encoded in UTF-8 *)
    raw: xmlBufferPtr; (* if encoder != NULL buffer for raw input *)
    compressed: cint; (* -1=unknown, 0=not compressed, 1=compressed *)
    error: cint;
    rawconsumed: culong; (* amount consumed from raw *)
  end;

  xmlOutputBuffer = record
    context: pointer;
    writecallback: xmlOutputWriteCallback;
    closecallback: xmlOutputCloseCallback;
    encoder: xmlCharEncodingHandlerPtr; (* I18N conversions to UTF-8 *)
    buffer: xmlBufferPtr; (* Local buffer encoded in UTF-8 or ISOLatin *)
    conv: xmlBufferPtr; (* if encoder != NULL buffer for output *)
    written: cint; (* total number of byte written *)
    error: cint;
  end;

(*
 * xmlErrorLevel:
 *
 * Indicates the level of an error
 *)
  xmlErrorLevel = (
    XML_ERR_NONE = 0,
    XML_ERR_WARNING = 1, (* A simple warning *)
    XML_ERR_ERROR = 2, (* A recoverable error *)
    XML_ERR_FATAL = 3    (* A fatal error *)
  );

(*
 * xmlErrorDomain:
 *
 * Indicates where an error may have come from
 *)
  xmlErrorDomain = (
    XML_FROM_NONE = 0,
    XML_FROM_PARSER, (* The XML parser *)
    XML_FROM_TREE, (* The tree module *)
    XML_FROM_NAMESPACE, (* The XML Namespace module *)
    XML_FROM_DTD, (* The XML DTD validation with parser context*)
    XML_FROM_HTML, (* The HTML parser *)
    XML_FROM_MEMORY, (* The memory allocator *)
    XML_FROM_OUTPUT, (* The serialization code *)
    XML_FROM_IO, (* The Input/Output stack *)
    XML_FROM_FTP, (* The FTP module *)
    XML_FROM_HTTP, (* The HTTP module *)
    XML_FROM_XINCLUDE, (* The XInclude processing *)
    XML_FROM_XPATH, (* The XPath module *)
    XML_FROM_XPOINTER, (* The XPointer module *)
    XML_FROM_REGEXP, (* The regular expressions module *)
    XML_FROM_DATATYPE, (* The W3C XML Schemas Datatype module *)
    XML_FROM_SCHEMASP, (* The W3C XML Schemas parser module *)
    XML_FROM_SCHEMASV, (* The W3C XML Schemas validation module *)
    XML_FROM_RELAXNGP, (* The Relax-NG parser module *)
    XML_FROM_RELAXNGV, (* The Relax-NG validator module *)
    XML_FROM_CATALOG, (* The Catalog module *)
    XML_FROM_C14N, (* The Canonicalization module *)
    XML_FROM_XSLT, (* The XSLT engine from libxslt *)
    XML_FROM_VALID, (* The XML DTD validation with valid context *)
    XML_FROM_CHECK, (* The error checking module *)
    XML_FROM_WRITER, (* The xmlwriter module *)
    XML_FROM_MODULE, (* The dynamically loaded module module*)
    XML_FROM_I18N, (* The module handling character conversion *)
    XML_FROM_SCHEMATRONV  (* The Schematron validator module *)
  );

(*
 * xmlError:
 *
 * An XML Error instance.
 *)
  xmlError = record
    domain: cint; (* What part of the library raised this error *)
    code: cint; (* The error code, e.g. an xmlParserError *)
    message: pchar;(* human-readable informative error message *)
    level: xmlErrorLevel;(* how consequent is the error *)
    _file: pchar; (* the filename *)
    line: cint; (* the line number if available *)
    str1: pchar; (* extra string information *)
    str2: pchar; (* extra string information *)
    str3: pchar; (* extra string information *)
    int1: cint; (* extra number information *)
    int2: cint; (* column number of the error or 0 if N/A (todo: rename this field when we would break ABI) *)
    ctxt: pointer; (* the parser context if available *)
    node: pointer; (* the node in the tree *)
  end;

(*
 * A node-set (an unordered collection of nodes without duplicates).
 *)
  xmlNodeSet = record
    nodeNr: cint; (* number of nodes in the set *)
    nodeMax: cint; (* size of the array as allocated *)
    nodeTab: xmlNodePtrPtr; (* array of nodes in no particular order *)
    (* @@ with_ns to check wether namespace nodes should be looked at @@ *)
  end;

(*
 * An expression is evaluated to yield an object, which
 * has one of the following four basic types:
 *   - node-set
 *   - boolean
 *   - number
 *   - string
 *
 * @@ XPointer will add more types !
 *)

  xmlXPathObjectType = (
    XPATH_UNDEFINED = 0,
    XPATH_NODESET = 1,
    XPATH_BOOLEAN = 2,
    XPATH_NUMBER = 3,
    XPATH_STRING = 4,
    XPATH_POINT = 5,
    XPATH_RANGE = 6,
    XPATH_LOCATIONSET = 7,
    XPATH_USERS = 8,
    XPATH_XSLT_TREE = 9  (* An XSLT value tree, non modifiable *)
  );

  xmlXPathObject = record
    _type: xmlXPathObjectType;
    nodesetval: xmlNodeSetPtr;
    boolval: cint;
    floatval: cdouble;
    stringval: xmlCharPtr;
    user: pointer;
    index: cint;
    user2: pointer;
    index2: cint;
  end;

(*
 * xmlXPathConvertFunc:
 * @obj:  an XPath object
 * @type:  the number of the target type
 *
 * A conversion function is associated to a type and used to cast
 * the new type to primitive values.
 *
 * Returns -1 in case of error, 0 otherwise
 *)
  xmlXPathConvertFunc = function(obj: xmlXPathObjectPtr; _type: cint): cint; cdecl;

(*
 * Extra type: a name and a conversion function.
 *)
  xmlXPathType = record
    name: xmlCharPtr; (* the type name *)
    func: xmlXPathConvertFunc; (* the conversion function *)
  end;

(*
 * Extra variable: a name and a value.
 *)
  xmlXPathVariable = record
    name: xmlCharPtr; (* the variable name *)
    value: xmlXPathObjectPtr; (* the value *)
  end;

(*
 * xmlXPathEvalFunc:
 * @ctxt: an XPath parser context
 * @nargs: the number of arguments passed to the function
 *
 * An XPath evaluation function, the parameters are on the XPath context stack.
 *)

  xmlXPathEvalFunc = procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); cdecl;

(*
 * Extra function: a name and a evaluation function.
 *)

  xmlXPathFunc = record
    name: xmlCharPtr; (* the function name *)
    func: xmlXPathEvalFunc; (* the evaluation function *)
  end;

(*
 * xmlXPathAxisFunc:
 * @ctxt:  the XPath interpreter context
 * @cur:  the previous node being explored on that axis
 *
 * An axis traversal function. To traverse an axis, the engine calls
 * the first time with cur == NULL and repeat until the function returns
 * NULL indicating the end of the axis traversal.
 *
 * Returns the next node in that axis or NULL if at the end of the axis.
 *)

  xmlXPathAxisFunc = function(ctxt: xmlXPathParserContextPtr; cur: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl;

(*
 * Extra axis: a name and an axis function.
 *)
  xmlXPathAxis = record
    name: xmlCharPtr; (* the axis name *)
    func: xmlXPathAxisFunc; (* the search function *)
  end;

(*
 * xmlXPathFunction:
 * @ctxt:  the XPath interprestation context
 * @nargs:  the number of arguments
 *
 * An XPath function.
 * The arguments (if any) are popped out from the context stack
 * and the result is pushed on the stack.
 *)
  xmlXPathFunction = procedure(ctxt: xmlXPathParserContextPtr; nargs: cint); cdecl;

(*
 * Function and Variable Lookup.
 *)

(*
 * xmlXPathVariableLookupFunc:
 * @ctxt:  an XPath context
 * @name:  name of the variable
 * @ns_uri:  the namespace name hosting this variable
 *
 * Prototype for callbacks used to plug variable lookup in the XPath
 * engine.
 *
 * Returns the XPath object value or NULL if not found.
 *)
  xmlXPathVariableLookupFunc = function(ctxt: pointer; name, ns_uri: xmlCharPtr): xmlXPathObjectPtr; cdecl;

(*
 * xmlXPathFuncLookupFunc:
 * @ctxt:  an XPath context
 * @name:  name of the function
 * @ns_uri:  the namespace name hosting this function
 *
 * Prototype for callbacks used to plug function lookup in the XPath
 * engine.
 *
 * Returns the XPath function or NULL if not found.
 *)
  xmlXPathFuncLookupFunc = function(ctxt: pointer; name, ns_uri: xmlCharPtr): xmlXPathFunction; cdecl;

(*
 * xmlXPathContext:
 *
 * Expression evaluation occurs with respect to a context.
 * he context consists of:
 *    - a node (the context node)
 *    - a node list (the context node list)
 *    - a set of variable bindings
 *    - a function library
 *    - the set of namespace declarations in scope for the expression
 * Following the switch to hash tables, this need to be trimmed up at
 * the next binary incompatible release.
 * The node may be modified when the context is passed to libxml2
 * for an XPath evaluation so you may need to initialize it again
 * before the next call.
 *)

  xmlXPathContext = record
    doc: xmlDocPtr; (* The current document *)
    node: xmlNodePtr; (* The current node *)

    nb_variables_unused: cint; (* unused (hash table) *)
    max_variables_unused: cint; (* unused (hash table) *)
    varHash: xmlHashTablePtr; (* Hash table of defined variables *)

    nb_types: cint; (* number of defined types *)
    max_types: cint; (* max number of types *)
    types: xmlXPathTypePtr; (* Array of defined types *)

    nb_funcs_unused: cint; (* unused (hash table) *)
    max_funcs_unused: cint; (* unused (hash table) *)
    funcHash: xmlHashTablePtr; (* Hash table of defined funcs *)

    nb_axis: cint; (* number of defined axis *)
    max_axis: cint; (* max number of axis *)
    axis: xmlXPathAxisPtr; (* Array of defined axis *)

    (* the namespace nodes of the context node *)
    namespaces: xmlNsPtrPtr; (* Array of namespaces *)
    nsNr: cint; (* number of namespace in scope *)
    user: pointer; (* function to free *)

    (* extra variables *)
    contextSize: cint; (* the context size *)
    proximityPosition: cint; (* the proximity position *)

    (* extra stuff for XPointer *)
    xptr: cint; (* is this an XPointer context? *)
    here: xmlNodePtr; (* for here() *)
    origin: xmlNodePtr; (* for origin() *)

    (* the set of namespace declarations in scope for the expression *)
    nsHash: xmlHashTablePtr; (* The namespaces hash table *)
    varLookupFunc: xmlXPathVariableLookupFunc;(* variable lookup func *)
    varLookupData: pointer; (* variable lookup data *)

    (* Possibility to link in an extra item *)
    extra: pointer; (* needed for XSLT *)

    (* The function name and URI when calling a function *)
    _function: xmlCharPtr;
    functionURI: xmlCharPtr;

    (* function lookup function and data *)
    funcLookupFunc: xmlXPathFuncLookupFunc;(* function lookup func *)
    funcLookupData: pointer; (* function lookup data *)

    (* temporary namespace lists kept for walking the namespace axis *)
    tmpNsList: xmlNsPtr; (* Array of namespaces *)
    tmpNsNr: cint; (* number of namespaces in scope *)

    (* error reporting mechanism *)
    userData: pointer; (* user specific data block *)
    error: xmlStructuredErrorFunc; (* the callback in case of errors *)
    lastError: xmlError; (* the last error *)
    debugNode: xmlNodePtr; (* the source node XSLT *)

    (* dictionary *)
    dict: xmlDictPtr; (* dictionary if any *)

    flags: cint; (* flags to control compilation *)

    (* Cache for reusal of XPath objects *)
    cache: pointer;
  end;

(*
 * The structure of a compiled expression form is not public.
 *)
  xmlXPathCompExpr = record end;

(*
 * xmlXPathParserContext:
 *
 * An XPath parser context. It contains pure parsing informations,
 * an xmlXPathContext, and the stack of objects.
 *)
  xmlXPathParserContext = record
    cur: xmlCharPtr; (* the current char being parsed *)
    base: xmlCharPtr; (* the full expression *)
    error: cint; (* error code *)
    context: xmlXPathContextPtr; (* the evaluation context *)
    value: xmlXPathObjectPtr; (* the current value *)
    valueNr: cint; (* number of values stacked *)
    valueMax: cint; (* max number of values stacked *)
    valueTab: xmlXPathObjectPtrPtr; (* stack of values *)
    comp: xmlXPathCompExprPtr; (* the precompiled expression *)
    xptr: cint; (* it this an XPointer expression *)
    ancestor: xmlNodePtr; (* used for walking preceding axis*)
  end;

type
  xmlGenericErrorFunc = procedure(ctx: pointer; msg: pchar); cdecl; varargs;

const
  xml2 = 'xml2';

function xmlStrdup(cur: xmlCharPtr): xmlCharPtr; cdecl; external xml2;
function xmlStrndup(cur: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external xml2;
function xmlCharStrndup(cur: pchar; len: cint): xmlCharPtr; cdecl; external xml2;
function xmlCharStrdup(cur: pchar): xmlCharPtr; cdecl; external xml2;
function xmlStrsub(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; cdecl; external xml2;
function xmlStrchr(str: xmlCharPtr; val: xmlChar): xmlCharPtr; cdecl; external xml2;
function xmlStrstr(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; cdecl; external xml2;
function xmlStrcasestr(str: xmlCharPtr; val: xmlCharPtr): xmlCharPtr; cdecl; external xml2;
function xmlStrcmp(str1: xmlCharPtr; str2: xmlCharPtr): cint; cdecl; external xml2;
function xmlStrncmp(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; cdecl; external xml2;
function xmlStrcasecmp(str1: xmlCharPtr; str2: xmlCharPtr): cint; cdecl; external xml2;
function xmlStrncasecmp(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): cint; cdecl; external xml2;
function xmlStrEqual(str1: xmlCharPtr; str2: xmlCharPtr): cint; cdecl; external xml2;
function xmlStrQEqual(pref: xmlCharPtr; name: xmlCharPtr; str: xmlCharPtr): cint; cdecl; external xml2;
function xmlStrlen(str: xmlCharPtr): cint; cdecl; external xml2;
function xmlStrcat(cur: xmlCharPtr; add: xmlCharPtr): xmlCharPtr; cdecl; external xml2;
function xmlStrncat(cur: xmlCharPtr; add: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external xml2;
function xmlStrncatNew(str1: xmlCharPtr; str2: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external xml2;
function xmlStrPrintf(buf: xmlCharPtr; len: cint; msg: xmlCharPtr; args: array of const): cint; cdecl; external xml2;
function xmlGetUTF8Char(utf: pchar; len: pcint): cint; cdecl; external xml2;
function xmlCheckUTF8(utf: pchar): cint; cdecl; external xml2;
function xmlUTF8Strsize(utf: xmlCharPtr; len: cint): cint; cdecl; external xml2;
function xmlUTF8Strndup(utf: xmlCharPtr; len: cint): xmlCharPtr; cdecl; external xml2;
function xmlUTF8Strpos(utf: xmlCharPtr; pos: cint): xmlCharPtr; cdecl; external xml2;
function xmlUTF8Strloc(utf: xmlCharPtr; utfchar: xmlCharPtr): cint; cdecl; external xml2;
function xmlUTF8Strsub(str: xmlCharPtr; start: cint; len: cint): xmlCharPtr; cdecl; external xml2;
function xmlUTF8Strlen(utf: xmlCharPtr): cint; cdecl; external xml2;
function xmlUTF8Size(utf: xmlCharPtr): cint; cdecl; external xml2;
function xmlUTF8Charcmp(utf1: xmlCharPtr; utf2: xmlCharPtr): cint; cdecl; external xml2;
function xmlEncodeSpecialChars(doc: xmlDocPtr; input: xmlCharPtr): xmlCharPtr; cdecl; external xml2;
function xmlDetectCharEncoding(_in: pchar; len: cint): xmlCharEncoding; cdecl; external xml2;
function xmlCharEncOutFunc(handler: xmlCharEncodingHandlerPtr; _out, _in: xmlBufferPtr): cint; cdecl; external xml2;
function xmlCharEncInFunc(handler: xmlCharEncodingHandlerPtr; _out, _in: xmlBufferPtr): cint; cdecl; external xml2;
function xmlCharEncFirstLine(handler: xmlCharEncodingHandlerPtr; _out, _in: xmlBufferPtr): cint; cdecl; external xml2;
function xmlCharEncCloseFunc(handler: xmlCharEncodingHandlerPtr): cint; cdecl; external xml2;
function xmlBufferCreate: xmlBufferPtr; cdecl; external xml2;
function xmlBufferCreateSize(size: csize_t): xmlBufferPtr; cdecl; external xml2;
function xmlBufferCreateStatic(mem: pointer; size: csize_t): xmlBufferPtr; cdecl; external xml2;
function xmlBufferResize(buf: xmlBufferPtr; size: cuint): cint; cdecl; external xml2;
procedure xmlBufferFree(buf: xmlBufferPtr); cdecl; external xml2;
function xmlBufferAdd(buf: xmlBufferPtr; str: xmlCharPtr; len: cint): cint; cdecl; external xml2;
function xmlBufferAddHead(buf: xmlBufferPtr; str: xmlCharPtr; len: cint): cint; cdecl; external xml2;
function xmlBufferCat(buf: xmlBufferPtr; str: xmlCharPtr): cint; cdecl; external xml2;
function xmlBufferCCat(buf: xmlBufferPtr; str: pchar): cint; cdecl; external xml2;
function xmlBufferShrink(buf: xmlBufferPtr; len: cuint): cint; cdecl; external xml2;
function xmlBufferGrow(buf: xmlBufferPtr; len: cuint): cint; cdecl; external xml2;
procedure xmlBufferEmpty(buf: xmlBufferPtr); cdecl; external xml2;
function xmlBufferContent(buf: xmlBufferPtr): xmlCharPtr; cdecl; external xml2;
procedure xmlBufferSetAllocationScheme(buf: xmlBufferPtr; scheme: xmlBufferAllocationScheme); cdecl; external xml2;
function xmlBufferLength(buf: xmlBufferPtr): cint; cdecl; external xml2;
function xmlCreateIntSubset(doc: xmlDocPtr; name, ExternalID, SystemID: xmlCharPtr): xmlDtdPtr; cdecl; external xml2;
function xmlNewDtd(doc: xmlDocPtr; name, ExternalID, SystemID: xmlCharPtr): xmlDtdPtr; cdecl; external xml2;
function xmlGetIntSubset(doc: xmlDocPtr): xmlDtdPtr; cdecl; external xml2;
procedure xmlFreeDtd(cur: xmlDtdPtr); cdecl; external xml2;
function xmlNewNs(node: xmlNodePtr; href, prefix: xmlCharPtr): xmlNsPtr; cdecl; external xml2;
procedure xmlFreeNs(cur: xmlNsPtr); cdecl; external xml2;
procedure xmlFreeNsList(cur: xmlNsPtr); cdecl; external xml2;
function xmlSaveFile(filename: xmlCharPtr; cur: xmlDocPtr): cint; cdecl; external xml2;
function xmlParseFile(filename: xmlCharPtr): xmlDocPtr; cdecl; external xml2;
function xmlParseDoc(cur: xmlCharPtr): xmlDocPtr; cdecl; external xml2;
function xmlNewDoc(version: xmlCharPtr): xmlDocPtr; cdecl; external xml2;
procedure xmlFreeDoc(cur: xmlDocPtr); cdecl; external xml2;
function xmlNewDocProp(doc: xmlDocPtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl; external xml2;
function xmlNewProp(node: xmlNodePtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl; external xml2;
function xmlNewNsProp(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl; external xml2;
function xmlNewNsPropEatName(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl; external xml2;
procedure xmlFreePropList(cur: xmlAttrPtr); cdecl; external xml2;
procedure xmlFreeProp(cur: xmlAttrPtr); cdecl; external xml2;
function xmlCopyProp(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; cdecl; external xml2;
function xmlCopyPropList(target: xmlNodePtr; cur: xmlAttrPtr): xmlAttrPtr; cdecl; external xml2;
function xmlCopyDtd(dtd: xmlDtdPtr): xmlDtdPtr; cdecl; external xml2;
function xmlCopyDoc(doc: xmlDocPtr; recursive: cint): xmlDocPtr; cdecl; external xml2;
function xmlNewDocNode(doc: xmlDocPtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewDocNodeEatName(doc: xmlDocPtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewNode(ns: xmlNsPtr; name: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewNodeEatName(ns: xmlNsPtr; name: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewChild(parent: xmlNodePtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewDocText(doc: xmlDocPtr; content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewText(content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewDocPI(doc: xmlDocPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewPI(name, content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewDocTextLen(doc: xmlDocPtr; content: xmlCharPtr; len: cint): xmlNodePtr; cdecl; external xml2;
function xmlNewTextLen(content: xmlCharPtr; len: cint): xmlNodePtr; cdecl; external xml2;
function xmlNewDocComment(doc: xmlDocPtr; content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewComment(content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewCDataBlock(doc: xmlDocPtr; content: xmlCharPtr; len: cint): xmlNodePtr; cdecl; external xml2;
function xmlNewCharRef(doc: xmlDocPtr; name: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewReference(doc: xmlDocPtr; name: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlCopyNode(node: xmlNodePtr; recursive: cint): xmlNodePtr; cdecl; external xml2;
function xmlDocCopyNode(node: xmlNodePtr; doc: xmlDocPtr; recursive: cint): xmlNodePtr; cdecl; external xml2;
function xmlDocCopyNodeList(doc: xmlDocPtr; node: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
function xmlCopyNodeList(node: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
function xmlNewTextChild(parent: xmlNodePtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewDocRawNode(doc: xmlDocPtr; ns: xmlNsPtr; name, content: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlNewDocFragment(doc: xmlDocPtr): xmlNodePtr; cdecl; external xml2;
function xmlGetLineNo(node: xmlNodePtr): clong; cdecl; external xml2;
function xmlGetNodePath(node: xmlNodePtr): xmlCharPtr; cdecl; external xml2;
function xmlDocGetRootElement(doc: xmlDocPtr): xmlNodePtr; cdecl; external xml2;
function xmlGetLastChild(parent: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
function xmlNodeIsText(node: xmlNodePtr): cint; cdecl; external xml2;
function xmlIsBlankNode(node: xmlNodePtr): cint; cdecl; external xml2;
function xmlDocSetRootElement(doc: xmlDocPtr; root: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
procedure xmlNodeSetName(cur: xmlNodePtr; name: xmlCharPtr); cdecl; external xml2;
procedure xmlNodeSetContent(cur: xmlNodePtr; content: xmlCharPtr); cdecl; external xml2;
function xmlAddChild(parent, cur: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
function xmlAddChildList(parent, cur: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
function xmlReplaceNode(old, cur: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
function xmlAddPrevSibling(cur, elem: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
function xmlAddSibling(cur, elem: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
function xmlAddNextSibling(cur, elem: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
procedure xmlUnlinkNode(cur: xmlNodePtr); cdecl; external xml2;
function xmlTextMerge(first, second: xmlNodePtr): xmlNodePtr; cdecl; external xml2;
function xmlTextConcat(node: xmlNodePtr; name: xmlCharPtr; len: cint): cint; cdecl; external xml2;
procedure xmlFreeNodeList(cur: xmlNodePtr); cdecl; external xml2;
procedure xmlFreeNode(cur: xmlNodePtr); cdecl; external xml2;
procedure xmlSetTreeDoc(tree: xmlNodePtr; doc: xmlDocPtr); cdecl; external xml2;
procedure xmlSetListDoc(list: xmlNodePtr; doc: xmlDocPtr); cdecl; external xml2;
function xmlSearchNs(doc: xmlDocPtr; node: xmlNodePtr; nameSpace: xmlCharPtr): xmlNsPtr; cdecl; external xml2;
function xmlSearchNsByHref(doc: xmlDocPtr; node: xmlNodePtr; href: xmlCharPtr): xmlNsPtr; cdecl; external xml2;
function xmlGetNsList(doc: xmlDocPtr; node: xmlNodePtr): xmlNsPtr; cdecl; external xml2;
procedure xmlSetNs(node: xmlNodePtr; ns: xmlNsPtr); cdecl; external xml2;
function xmlCopyNamespace(cur: xmlNsPtr): xmlNsPtr; cdecl; external xml2;
function xmlCopyNamespaceList(cur: xmlNsPtr): xmlNsPtr; cdecl; external xml2;
function xmlRemoveProp(cur: xmlAttrPtr): cint; cdecl; external xml2;
function xmlSetProp(node: xmlNodePtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl; external xml2;
function xmlSetNsProp(node: xmlNodePtr; ns: xmlNsPtr; name, value: xmlCharPtr): xmlAttrPtr; cdecl; external xml2;
function xmlGetNoNsProp(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr; cdecl; external xml2;
function xmlGetProp(node: xmlNodePtr; name: xmlCharPtr): xmlCharPtr; cdecl; external xml2;
function xmlHasProp(node: xmlNodePtr; name: xmlCharPtr): xmlAttrPtr; cdecl; external xml2;
function xmlHasNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlAttrPtr; cdecl; external xml2;
function xmlGetNsProp(node: xmlNodePtr; name, nameSpace: xmlCharPtr): xmlCharPtr; cdecl; external xml2;
function xmlStringGetNodeList(doc: xmlDocPtr; value: xmlCharPtr): xmlNodePtr; cdecl; external xml2;
function xmlStringLenGetNodeList(doc: xmlDocPtr; value: xmlCharPtr; len: cint): xmlNodePtr; cdecl; external xml2;
function xmlNodeListGetString(doc: xmlDocPtr; list: xmlNodePtr; _inLine: cint): xmlCharPtr; cdecl; external xml2;
procedure xmlNodeAddContent(cur: xmlNodePtr; content: xmlCharPtr); cdecl; external xml2;
procedure xmlNodeAddContentLen(cur: xmlNodePtr; content: xmlCharPtr; len: cint); cdecl; external xml2;
function xmlNodeGetContent(cur: xmlNodePtr): xmlCharPtr; cdecl; external xml2;
function xmlNodeBufGetContent(buffer: xmlBufferPtr; cur: xmlNodePtr): cint; cdecl; external xml2;
function xmlNodeGetLang(cur: xmlNodePtr): xmlCharPtr; cdecl; external xml2;
function xmlNodeGetSpacePreserve(cur: xmlNodePtr): cint; cdecl; external xml2;
function xmlNodeDump(buf: xmlBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr; level: cint; format: cint): cint; cdecl; external xml2;
procedure xmlInitCharEncodingHandlers; cdecl; external xml2;
procedure xmlCleanupCharEncodingHandlers; cdecl; external xml2;
procedure xmlRegisterCharEncodingHandler(handler: xmlCharEncodingHandlerPtr); cdecl; external xml2;
function xmlGetCharEncodingHandler(enc: xmlCharEncoding): xmlCharEncodingHandlerPtr; cdecl; external xml2;
function xmlFindCharEncodingHandler(name: pchar): xmlCharEncodingHandlerPtr; cdecl; external xml2;
function xmlNewCharEncodingHandler(name: pchar; input: xmlCharEncodingInputFunc; output: xmlCharEncodingOutputFunc): xmlCharEncodingHandlerPtr; cdecl; external xml2;
function xmlAddEncodingAlias(name: pchar; alias: pchar): cint; cdecl; external xml2;
function xmlDelEncodingAlias(alias: pchar): cint; cdecl; external xml2;
function xmlGetEncodingAlias(alias: pchar): pchar; cdecl; external xml2;
procedure xmlCleanupEncodingAliases; cdecl; external xml2;
function xmlParseCharEncoding(name: pchar): xmlCharEncoding; cdecl; external xml2;
function xmlGetCharEncodingName(enc: xmlCharEncoding): pchar; cdecl; external xml2;
procedure xmlCleanupInputCallbacks; cdecl; external xml2;
function xmlPopInputCallbacks: cint; cdecl; external xml2;
procedure xmlRegisterDefaultInputCallbacks; cdecl; external xml2;
function xmlAllocParserInputBuffer(enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl; external xml2;
function xmlParserInputBufferCreateFilename(URI: pchar; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl; external xml2;
function xmlParserInputBufferCreateFd(fd: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl; external xml2;
function xmlParserInputBufferCreateMem(mem: pchar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl; external xml2;
function xmlParserInputBufferCreateStatic(mem: pchar; size: cint; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl; external xml2;
function xmlParserInputBufferCreateIO(ioread: xmlInputReadCallback; ioclose: xmlInputCloseCallback; ioctx: pointer; enc: xmlCharEncoding): xmlParserInputBufferPtr; cdecl; external xml2;
function xmlParserInputBufferRead(_in: xmlParserInputBufferPtr; len: cint): cint; cdecl; external xml2;
function xmlParserInputBufferGrow(_in: xmlParserInputBufferPtr; len: cint): cint; cdecl; external xml2;
function xmlParserInputBufferPush(_in: xmlParserInputBufferPtr; len: cint; buf: pchar): cint; cdecl; external xml2;
procedure xmlFreeParserInputBuffer(_in: xmlParserInputBufferPtr); cdecl; external xml2;
function xmlParserGetDirectory(filename: pchar): pchar; cdecl; external xml2;
procedure xmlCleanupOutputCallbacks; cdecl; external xml2;
procedure xmlRegisterDefaultOutputCallbacks; cdecl; external xml2;
function xmlAllocOutputBuffer(encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; cdecl; external xml2;
function xmlOutputBufferCreateFilename(URI: pchar; encoder: xmlCharEncodingHandlerPtr; compression: cint): xmlOutputBufferPtr; cdecl; external xml2;
function xmlOutputBufferCreateBuffer(buffer: xmlBufferPtr; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; cdecl; external xml2;
function xmlOutputBufferCreateFd(fd: cint; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; cdecl; external xml2;
function xmlOutputBufferCreateIO(iowrite: xmlOutputWriteCallback; ioclose: xmlOutputCloseCallback; ioctx: pointer; encoder: xmlCharEncodingHandlerPtr): xmlOutputBufferPtr; cdecl; external xml2;
function xmlOutputBufferWrite(_out: xmlOutputBufferPtr; len: cint; buf: pchar): cint; cdecl; external xml2;
function xmlOutputBufferWriteString(_out: xmlOutputBufferPtr; str: pchar): cint; cdecl; external xml2;
function xmlOutputBufferWriteEscape(_out: xmlOutputBufferPtr; str: xmlCharPtr; escaping: xmlCharEncodingOutputFunc): cint; cdecl; external xml2;
function xmlOutputBufferFlush(_out: xmlOutputBufferPtr): cint; cdecl; external xml2;
function xmlOutputBufferClose(_out: xmlOutputBufferPtr): cint; cdecl; external xml2;
procedure xmlXPathFreeObject(obj: xmlXPathObjectPtr); cdecl; external xml2;
function xmlXPathNodeSetCreate(val: xmlNodePtr): xmlNodeSetPtr; cdecl; external xml2;
procedure xmlXPathFreeNodeSetList(obj: xmlXPathObjectPtr); cdecl; external xml2;
procedure xmlXPathFreeNodeSet(obj: xmlNodeSetPtr); cdecl; external xml2;
function xmlXPathObjectCopy(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external xml2;
function xmlXPathCmpNodes(node1, node2: xmlNodePtr): cint; cdecl; external xml2;
function xmlXPathCastNumberToBoolean(val: cdouble): cint; cdecl; external xml2;
function xmlXPathCastStringToBoolean(val: xmlCharPtr): cint; cdecl; external xml2;
function xmlXPathCastNodeSetToBoolean(ns: xmlNodeSetPtr): cint; cdecl; external xml2;
function xmlXPathCastToBoolean(ns: xmlXPathObjectPtr): cint; cdecl; external xml2;
function xmlXPathCastBooleanToNumber(val: cint): cdouble; cdecl; external xml2;
function xmlXPathCastStringToNumber(val: xmlCharPtr): cdouble; cdecl; external xml2;
function xmlXPathCastNodeToNumber(val: xmlNodePtr): cdouble; cdecl; external xml2;
function xmlXPathCastNodeSetToNumber(val: xmlNodeSetPtr): cdouble; cdecl; external xml2;
function xmlXPathCastToNumber(val: xmlXPathObjectPtr): cdouble; cdecl; external xml2;
function xmlXPathCastBooleanToString(val: cint): xmlCharPtr; cdecl; external xml2;
function xmlXPathCastNumberToString(val: cdouble): xmlCharPtr; cdecl; external xml2;
function xmlXPathCastNodeToString(val: xmlNodePtr): xmlCharPtr; cdecl; external xml2;
function xmlXPathCastNodeSetToString(val: xmlNodeSetPtr): xmlCharPtr; cdecl; external xml2;
function xmlXPathCastToString(val: xmlXPathObjectPtr): xmlCharPtr; cdecl; external xml2;
function xmlXPathConvertBoolean(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external xml2;
function xmlXPathConvertNumber(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external xml2;
function xmlXPathConvertString(val: xmlXPathObjectPtr): xmlXPathObjectPtr; cdecl; external xml2;
function xmlXPathNewContext(doc: xmlDocPtr): xmlXPathContextPtr; cdecl; external xml2;
procedure xmlXPathFreeContext(ctxt: xmlXPathContextPtr); cdecl; external xml2;
function xmlXPathContextSetCache(ctxt: xmlXPathContextPtr; active, value, options: cint): cint; cdecl; external xml2;
function xmlXPathOrderDocElems(doc: xmlDocPtr): clong; cdecl; external xml2;
function xmlXPathEval(str: xmlCharPtr; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl; external xml2;
function xmlXPathEvalExpression(str: xmlCharPtr; ctx: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl; external xml2;
function xmlXPathEvalPredicate(ctxt: xmlXPathContextPtr; res: xmlXPathObjectPtr): cint; cdecl; external xml2;
function xmlXPathCompile(str: xmlCharPtr): xmlXPathCompExprPtr; cdecl; external xml2;
function xmlXPathCtxtCompile(ctxt: xmlXPathContextPtr; str: xmlCharPtr): xmlXPathCompExprPtr; cdecl; external xml2;
function xmlXPathCompiledEval(comp: xmlXPathCompExprPtr; ctxt: xmlXPathContextPtr): xmlXPathObjectPtr; cdecl; external xml2;
function xmlXPathCompiledEvalToBoolean(comp: xmlXPathCompExprPtr; ctxt: xmlXPathContextPtr): cint; cdecl; external xml2;
procedure xmlXPathFreeCompExpr(comp: xmlXPathCompExprPtr); cdecl; external xml2;
procedure xmlXPathInit; cdecl; external xml2;
function xmlXPathIsNaN(val: cdouble): cint; cdecl; external xml2;
function xmlXPathIsInf(val: cdouble): cint; cdecl; external xml2;
procedure xmlMemFree(ptr: pointer); cdecl; external xml2;
procedure initGenericErrorDefaultFunc(var handler: xmlGenericErrorFunc); cdecl; external xml2;
{$endif}

implementation

end.

