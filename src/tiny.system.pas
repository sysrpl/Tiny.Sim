unit Tiny.System;

{$i tiny.inc}

interface

uses
{$ifdef unix}
  CThreads,
{$endif}
  Tiny.Types;

const
{$ifdef windows}
  SysPlatform = 'win';
{$endif}
{$ifdef linux}
  SysPlatform = 'linux';
{$endif}
{$ifdef darwin}
  SysPlatform = 'darwin';
{$endif}
{$ifdef cpui386}
  SysArch = '32';
{$endif}
{$ifdef cpux86_64}
  SysArch = '64';
{$endif}
{$ifdef cpuarm}
  SysArch = 'arm';
{$endif}
  Epsilon = 0.0001;
  HiByte = High(Byte);
  InvalidHandle = nil;
  StackSize = 100;
  {$ifdef darwin}
  ThreadZero = nil;
  {$else}
  ThreadZero = 0;
  {$endif}

{$region runtime library}
{ Free an object and set its reference to nil }
procedure FreeAndNil(var Obj: TObject);
{ Fill a block of memory with zeros }
procedure MemZero(out Buffer; Size: UIntPtr);
{ Compare two blocks of memory returning true if they are the same }
function MemCompare(const A, B; Size: LongWord): Boolean;
{ Compare two Float adjusting for rounding }
function FloatEqual(A, B: Float): Boolean;

{ The LibraryLoad function loads a dynamic library or shared object file
  See also
  <link Tiny.System.LibraryPath, LibraryPath function>
  <link Tiny.System.LibraryFree, LibraryFree procedure>
  <link Tiny.System.GetProcAddress, GetProcAddress function> }
function LibraryLoad(const FileName: string): HModule;
{ The LibraryFree procedure unloads a dynamic library or shared object from memory
  See also
  <link Tiny.System.LibraryLoad, LibraryLoad function>
  <link Tiny.System.GetProcAddress, GetProcAddress function> }
procedure LibraryFree(Module: HModule);
{ The GetProcAddress function returns the address of a function in a loaded dynamic
  library or shared object
  See also
  <link Tiny.System.LibraryLoad, LibraryLoad function>
  <link Tiny.System.LibraryFree, LibraryFree procedure>
  <link Tiny.System.GetProcAddress, GetProcAddress function> }
function GetProcAddress(Module: HModule; const ProcName: string): Pointer;
{ The Sleep procedure suspends the current thread a for a designated number of
  milliseconds
  See also
  <link Tiny.System.Now, Now function> }
procedure Sleep(Milliseconds: LongWord);
{ The Now function returns the time in seconds since application startup
  See also
  <link Tiny.System.Sleep, Sleep procedure> }
function Now: Double;
{ The NowTime function returns the local time of day
  See also
  <link Tiny.System.Sleep, Sleep procedure> }
function NowTime: TDateTime; overload;
{ The NowTime function returns the local time of day
  See also
  <link Tiny.System.Sleep, Sleep procedure> }
procedure NowTime(out Hour, Min, Sec, MSec: Word); overload;
{ Display a message in a popup window }
//procedure ShowMessage(const Msg: string);
{ Display an error in a popup window }
//procedure ShowError(ExceptObject: TObject; const Msg: string = '');
{ Write text to the console }
procedure WriteLine(const S: string); overload;
{ Write formattted text to the console }
procedure WriteLine(const S: string; Args: array of const); overload;
{$endregion}

{$region input}
{ Keyboard codes }

type
  TKeyCode = LongWord;

const
  keyReturn = $D;
  keyEscape = $1B;
  keyBackspace = $8;
  keyTab = $9;
  keySpace = Ord(' ');
  keyExclaim = Ord('!');
  keyQuotedbl = Ord('"');
  keyHash = Ord('#');
  keyDollar = Ord('$');
  keyPercent = Ord('%');
  keyAmpersand = Ord('&');
  keyQuote = Ord('''');
  keyLeftparen = Ord('(');
  keyRightparen = Ord(')');
  keyAsterisk = Ord('*');
  keyPlus = Ord('+');
  keyComma = Ord(',');
  keyMinus = Ord('-');
  keyPeriod = Ord('.');
  keySlash = Ord('/');
  key0 = Ord('0');
  key1 = Ord('1');
  key2 = Ord('2');
  key3 = Ord('3');
  key4 = Ord('4');
  key5 = Ord('5');
  key6 = Ord('6');
  key7 = Ord('7');
  key8 = Ord('8');
  key9 = Ord('9');
  keyColon = Ord(':');
  keySemicolon = Ord(';');
  keyLess = Ord('<');
  keyEquals = Ord('=');
  keyGreater = Ord('>');
  keyQuestion = Ord('?');
  keyAt = Ord('@');
  keyLeftbracket = Ord('[');
  keyBackslash = Ord('\');
  keyRightbracket = Ord(']');
  keyCaret = Ord('^');
  keyUnderscore = Ord('_');
  keyBackquote = Ord('`');
  keyA = Ord('a');
  keyB = Ord('b');
  keyC = Ord('c');
  keyD = Ord('d');
  keyE = Ord('e');
  keyF = Ord('f');
  keyG = Ord('g');
  keyH = Ord('h');
  keyI = Ord('i');
  keyJ = Ord('j');
  keyK = Ord('k');
  keyL = Ord('l');
  keyM = Ord('m');
  keyN = Ord('n');
  keyO = Ord('o');
  keyP = Ord('p');
  keyQ = Ord('q');
  keyR = Ord('r');
  keyS = Ord('s');
  keyT = Ord('t');
  keyU = Ord('u');
  keyV = Ord('v');
  keyW = Ord('w');
  keyX = Ord('x');
  keyY = Ord('y');
  keyZ = Ord('z');
  keyCapslock = $40000039;
  keyF1 = $4000003A;
  keyF2 = $4000003B;
  keyF3 = $4000003C;
  keyF4 = $4000003D;
  keyF5 = $4000003E;
  keyF6 = $4000003F;
  keyF7 = $40000040;
  keyF8 = $40000041;
  keyF9 = $40000042;
  keyF10 = $40000043;
  keyF11 = $40000044;
  keyF12 = $40000045;
  keyPrintscreen = $40000046;
  keyScrolllock = $40000047;
  keyPause = $40000048;
  keyInsert = $40000049;
  keyHome = $4000004A;
  keyPageup = $4000004B;
  keyDelete = $7F;
  keyEnd = $4000004D;
  keyPagedown = $4000004E;
  keyRight = $4000004F;
  keyLeft = $40000050;
  keyDown = $40000051;
  keyUp = $40000052;
  keyNumlockclear = $40000053;
  keyKPDivide = $40000054;
  keyKPMultiply = $40000055;
  keyKPMinus = $40000056;
  keyKPPlus = $40000057;
  keyKPEnter = $40000058;
  keyKP1 = $40000059;
  keyKP2 = $4000005A;
  keyKP3 = $4000005B;
  keyKP4 = $4000005C;
  keyKP5 = $4000005D;
  keyKP6 = $4000005E;
  keyKP7 = $4000005F;
  keyKP8 = $40000060;
  keyKP9 = $40000061;
  keyKP0 = $40000062;
  keyKPPeriod = $40000063;
  keyApplication = $40000065;
  keyPower = $40000066;
  keyKPEquals = $40000067;

{ Keyboard and mouse types }

type
  TShiftState = set of (ssShift, ssAlt, ssCtrl);
  TMouseButton = (mbNone, mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2);

{ Mouse button set }

  TMouseButtons = set of TMouseButton;

{ A joystick hat represents a switch with 9 possible positions }

  TJoystickHat = (hatCenter, hatUp, hatRight, hatDown, hatLeft,
    hatRightUp, hatRightDown, hatLeftUp, hatLeftDown);

  TMouseArgs = record
    { The button triggerign te event }
    Button: TMouseButton;
    { The mouse coordinates in both absolute and relative values }
    X, Y, XRel, YRel: Float;
    { State of the keyboardmodifier keys }
    Shift: TShiftState;
    { When the user sets handled to true the event should not bubble upwards }
    Handled: Boolean;
  end;

  TKeyboardArgs = record
    { Refer to virtual key codes in Bare.Game }
    Key: TKeyCode;
    { State of the modifier keys }
    Shift: TShiftState;
    { True if the keyboard event was generated as a result of a key
      repeating while being held down }
    Repeated: Boolean;
    { When the user sets handled to true the event should not bubble upwards }
    Handled: Boolean;
  end;

{ Event types }

  { TNotifyEvent is a basic event sent from another object }
  TNotifyEvent = procedure(Sender: TObject) of object;
  { Vanilla method }
  TMethodEvent = procedure of object;
  { Events realted to keyboard input }
  TKeyboardEvent = procedure(Sender: TObject; var Args: TKeyboardArgs) of object;
  { Events realted to mouse input }
  TMouseEvent = procedure(Sender: TObject; var Args: TMouseArgs) of object;
{$endregion}

{$region math routines}
{ Returns the uppermost value of a number before rounding }
function Ceil(const X: Extended): Integer;
{ Returns the lowermost value of a number before rounding }
function Floor(const X: Extended): Integer;
{ Returns the whole number of times a quotient be divided
  See also
  <link Tiny.System.Remainder, Remainder function> }
function Divide(const Quotient, Divisor: Extended): Extended;
{ Returns the fractional remainder of a divide
  See also
  <link Tiny.System.Divide, Divide function> }
function Remainder(const Quotient, Divisor: Extended): Extended;
{ Returns the maximum of two integer }
function Max(A, B: Integer): Integer; overload;
{ Returns the maximum of two singles }
function Max(A, B: Single): Single; overload;
{ Returns the maximum of two doubles }
function Max(const A, B: Double): Double; overload;
{ Returns the minimum of two integer }
function Min(A, B: Integer): Integer; overload;
{ Returns the minimum of two singles }
function Min(A, B: Single): Single; overload;
{ Returns the minimum of two doubles }
function Min(const A, B: Double): Double; overload;
{ Tanget trigometric function }
function Tan(const X: Extended): Extended;
{ Arc tangent trigometric function }
function ArcTan2(Y, X: Extended) : Extended;
{ Combined sine and cosine single trigometric function }
procedure SinCos(const X: Extended; out S, C: Extended); overload;
{ Clamps a number between the range 0..1 }
function Clamp(Value: Extended): Extended; overload;
{ Clamps a number between a specified range }
function Clamp(Value, Lo, Hi: Extended): Extended; overload;
{ Mix a percentage based value between low and high }
function Mix(Value, Lo, Hi: Extended): Extended;
{ Convert a float to a byte }
function FloatToByte(F: Float): Byte;
{ Convert degrees to radians }
function DegToRad(D: Float): Float;
{ Convert radians to degrees }
function RadToDeg(R: Float): Float;
{ Raise a float to an integer power }
function IntPower(Base: Float; Exponent: Integer): Float;
{$endregion}

{$region generic containers}
type
{ TCompare\<T\> is used to compare two items }
  TCompare<T> = function(const A, B: T): IntPtr;
{ TConvert\<Source, Target\> is used to convert from one type to another }
  // TConvert<TItem, T> = function(constref Item: TItem): T; see issue #28766
{ TConvertString\<T\> is used to convert a type to a string }
 TConvertString<TItem> = function(const Item: TItem): string;

{ TFilterFunc\<T\> is used to test if and item passes a test }

  TFilterFunc<T> = function(constref Value: T): Boolean;

{doc off}
  TArrayEnumerator<T> = record
  private
    FItems: TArray<T>;
    FPosition: Integer;
    FCount: Integer;
  public
    procedure Create(Items: TArray<T>; Count: Integer = -1);
    { IEnumerator<T> }
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
  end;
{doc on}

{ TSortingOrder can be used to a sort items forward, backwards, or not at all }

  TSortingOrder = (soAscend, soDescend, soNone);

{ TArrayList\<T\> is a simple extension to dynamic arrays
  See also
  <link Overview.Tiny.System.TArrayList, TArrayList\<T\> members> }

  TArrayList<T> = record
  public type
    {doc ignore}
    TArrayListEnumerator = TArrayEnumerator<T>;
    TCompareFunc = TCompare<T>;
    { Get the enumerator for the list }
    function GetEnumerator: TArrayListEnumerator;
  private
    function CompareExists: Boolean;
    procedure QuickSort(Order: TSortingOrder; Compare: TCompare<T>; L, R: Integer);
    function GetIsEmpty: Boolean;
    function GetFirst: T;
    procedure SetFirst(const Value: T);
    function GetLast: T;
    procedure SetLast(const Value: T);
    function GetLength: Integer;
    procedure SetLength(Value: Integer);
    function GetData: Pointer;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; const Value: T);
  public
    class var DefaultCompare: TCompare<T>;
    class var DefaultConvertString: TConvertString<T>;
    { The array acting as a list }
    var Items: TArray<T>;
    class function ArrayOf(const Items: array of T): TArrayList<T>; static;
    class function Convert: TArrayList<T>; static;
    { Convert a list to an array }
    class operator Implicit(const Value: TArrayList<T>): TArray<T>;
    { Convert an array to a list }
    class operator Implicit(const Value: TArray<T>): TArrayList<T>;
    { Convert an open array to a list }
    class operator Implicit(const Value: array of T): TArrayList<T>;
    { Performs a simple safe copy of up to N elements }
    procedure Copy(out List: TArrayList<T>; N: Integer);
    { Performs a fast unsafe copy of up to N elements }
    procedure CopyFast(out List: TArrayList<T>; N: Integer);
    { Returns the lower bounds of the list }
    function Lo: Integer;
    { Returns the upper bounds of the list }
    function Hi: Integer;
    { Reverses theitems in the list }
    procedure Reverse;
    { Move item at old index to new index }
    procedure Move(OldIndex, NewIndex: Integer);
    { Swap two items in the list }
    procedure Exchange(A, B: Integer);
    { Adds and item to the end of the list }
    procedure Push(const Item: T);
    { Appends an array of items to the list }
    procedure PushRange(const Collection: array of T);
    { Remove an item from the end of the list }
    function Pop: T;
    { Remove an item randomly from the list }
    function PopRandom: T;
    { Return a copy of the list with items passing through a filter }
    function Filter(Func: TFilterFunc<T>): TArrayList<T>;
    { Resurn the first item matching a condition }
    function FirstOf(Func: TFilterFunc<T>): T;
    { Removes an item by index from the list and decresaes the count by one }
    procedure Delete(Index: Integer);
    { Removes all items setting the count of the list to 0 }
    procedure Clear;
    { Sort the items using a comparer }
    procedure Sort(Order: TSortingOrder = soAscend; Comparer: TCompare<T> = nil);
    { Attempt to find the item using DefaultCompare }
    function IndexOf(const Item: T): Integer; overload;
    { Attempt to find the item using a supplied comparer }
    function IndexOf(const Item: T; Comparer: TCompare<T>): Integer; overload;
    { Join a the array into a string using a separator }
    function Join(const Separator: string; Convert: TConvertString<T> = nil): string;
    { Returns true if ther are no items in the list }
    property IsEmpty: Boolean read GetIsEmpty;
    { First item in the list }
    property First: T read GetFirst write SetFirst;
    { Last item in the list }
    property Last: T read GetLast write SetLast;
    { Number of items in the list }
    property Length: Integer read GetLength write SetLength;
    { Address where to the first item is located }
    property Data: Pointer read GetData;
    { Get or set an item }
    property Item[Index: Integer]: T read GetItem write SetItem; default;
  end;

{ TMap\<K, V\> is a array like simple dictionary
  See also
  <link Overview.Tiny.System.TMap, TMap\<K, V\> members> }

  TMap<K, V> = record
  private
    FKeys: TArrayList<K>;
    FValues: TArrayList<V>;
    function GetItem(const Key: K): V;
    procedure SetItem(const Key: K; const Value: V);
  public
    { Get or set and item using a key }
    property Item[const Key: K]: V read GetItem write SetItem;
  end;

{doc off}
  StringArray = TArrayList<string>;
  WordArray = TArrayList<Word>;
  IntArray = TArrayList<Integer>;
  Int64Array = TArrayList<Int64>;
  FloatArray = TArrayList<Float>;
  BoolArray = TArrayList<Boolean>;
  PointerArray = TArrayList<Pointer>;
  ObjectArray = TArrayList<TObject>;
  InterfaceArray = TArrayList<IInterface>;

function DefaultStringCompare(const A, B: string): IntPtr;
function DefaultStringConvertString(const Item: string): string;
function DefaultWordCompare(const A, B: Word): IntPtr;
function DefaultWordConvertString(const Item: Word): string;
function DefaultIntCompare(const A, B: Integer): IntPtr;
function DefaultIntConvertString(const Item: Integer): string;
function DefaultInt64Compare(const A, B: Int64): IntPtr;
function DefaultInt64ConvertString(const Item: Int64): string;
function DefaultFloatCompare(const A, B: Float): IntPtr;
function DefaultFloatConvertString(const Item: Float): string;
function DefaultObjectCompare(const A, B: TObject): IntPtr;
function DefaultInterfaceCompare(const A, B: IInterface): IntPtr;
function DefaultCompare8(const A, B: Byte): IntPtr;
function DefaultCompare16(const A, B: Word): IntPtr;
function DefaultCompare32(const A, B: LongInt): IntPtr;
function DefaultCompare64(const A, B: LargeInt): IntPtr;
function FindObject(const A, B: TObject): Integer;

type
  TListEnumerator<T> = record
  private
    FItems: TArrayList<T>;
    FPosition: Integer;
    FCount: Integer;
    function GetCount: Integer;
    function GetItem(Index: Integer): T;
  public
    procedure Create(Items: TArrayList<T>; Count: Integer = -1);
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: T read GetItem; default;
  end;
{doc on}

{ TList\<TItem\> is a generic growable list of items
  See also
  <link Overview.Tiny.Types.TList, TList members> }

  TList<TItem> = class
  public
    type ItemType = TItem;
    type PItemType = ^TItem;
    type TListCompare = TCompare<TItem>;
    type TListItemEnumerator = TListEnumerator<TItem>;
    { Get the enumerator for the list }
    function GetEnumerator: TListItemEnumerator;
  private
    FCount: Integer;
    FCapacity: Integer;
    procedure QuickSort(Compare: TListCompare; L, R: Integer);
    procedure CheckBounds(const Method: string; Index: Integer);
    function GetHead: PItemType;
    function GetFirst: ItemType;
    function GetLast: ItemType;
    function GetCapacity: Integer;
    procedure SetCapacity(Value: Integer);
    function GetItem(Index: Integer): ItemType;
    procedure SetItem(Index: Integer; const Value: ItemType);
  protected
    FItems: TArrayList<ItemType>;
    { Allows list types to take action on add }
    procedure AddItem(constref Item: ItemType); virtual;
    { Allows list types to take action on delete }
    procedure DeleteItem(var Item: ItemType); virtual;
    { If require delete is true delete item will be called for every item on clear }
    function RequiresDelete: Boolean; virtual;
    { Search the list for an equal item }
    function Find(Comparer: TListCompare; const Item: ItemType): Integer;
    { Request room for at least a minimum number of items }
    procedure Grow(MinCapacity: Integer);
  public
    destructor Destroy; override;
    { Add an item to the end of the list }
    procedure Add(const Item: ItemType);
    { Delete an item by index from the list }
    procedure Delete(Index: Integer);
    { Remove an item from the list }
    procedure Remove(const Item: ItemType);
    { Move item at old index to new index }
    procedure Move(OldIndex, NewIndex: Integer);
    { Exchange positions of two items in the list }
    procedure Exchange(A, B: Integer);
    { Returns the index of the item or -1 if not found }
    function IndexOf(const Item: ItemType): Integer;
    { Remove all items from the list setting capcity and count to zero }
    procedure Clear;
    { Reclaim unused capacity }
    procedure Compact;
    { Sort the list }
    procedure Sort(Compare: TListCompare = nil); virtual;
    { A reference to the first item in the list }
    property Head: PItemType read GetHead;
    { The first item in the list }
    property First: ItemType read GetFirst;
    { The last item in the list }
    property Last: ItemType read GetLast;
    { Number of items in the list }
    property Count: Integer read FCount;
    { Allocated space in terms of number of items }
    property Capacity: Integer read GetCapacity write SetCapacity;
    { Get or set an item in the list }
    property Item[Index: Integer]: ItemType read GetItem write SetItem; default;
  end;

{ TListDuplicates allows, ignores, or generates errors which a matching value is
  added to an indexed list }

  TListDuplicates = (duplicateAllow, duplicateIgnore, duplicateError);

{ TIndexedList\<TItem\> allows for search and removing of items by value
  See also
  <link Overview.Bare.Types.TIndexedList, TCollection members> }

  TIndexedList<TItem> = class(TList<TItem>)
  private
    FDuplicates: TListDuplicates;
  protected
    { AddItem checks for duplicates }
    procedure AddItem(constref Item: ItemType); override;
    { Allow, ignore, or error on adding a matching item }
    property Duplicates: TListDuplicates read FDuplicates write FDuplicates;
  public
    { Returns true it the list contains item }
    function Contains(const Item: ItemType): Boolean;
    { Returns the index of the item or -1 if it cannot be found }
    function IndexOf(const Item: ItemType): Integer; virtual; abstract;
    { Return the item by value }
    function Remove(const Item: ItemType): Boolean;
  end;

{ TObjectList\<TItem\> holds objects and can optionally be set to manage their life
  See also
  <link Overview.Bare.Types.TObjectList, TObjectList\<TItem\> members> }

  TObjectList<TItem: TObject> = class(TIndexedList<TItem>)
  private
    FOwnsObjects: Boolean;
  protected
    { Prevents items from being added twice }
    procedure AddItem(constref Item: ItemType); override;
    { Frees the items if the list owns the objects }
    procedure DeleteItem(var Item: ItemType); override;
    { Returns true if the list owns the objects }
    function RequiresDelete: Boolean; override;
  public
    { Create the list optionally owning objects added to it }
    constructor Create(OwnsObjects: Boolean);
    { Returns the index of the object or -1 if it cannot be found }
    function IndexOf(const Item: ItemType): Integer; override;
    { Allow, ignore, or error on adding a duplicate object }
    property Duplicates;
  end;

  { TDictionary\<K, V\> holds key value pairs allowing items to be indexed by a key
  See also
  <link Overview.Bare.Types.TDictionary, TDictionary\<K, V\> members> }

  TDictionary<K, V> = class(TObject)

    { TDictionary\<K, V\>.TKeyValue holds a key value pairs
   See also
   <link Overview.Bare.Types.TDictionary.TKeyValue, TDictionary\<K, V\>.TKeyValue members> }

  public type
    TKeyValue = class
    private
      FKey: K;
      FValue: V;
    public
     constructor Create(const Key: K);
     { The key }
     property Key: K read FKey;
     { The value }
     property Value: V read FValue write FValue;
    end;

    TDictionaryEnumerator = TList<TKeyValue>.TListItemEnumerator;

  public
    { Get the enumerator for the dictionary }
    function GetEnumerator: TDictionaryEnumerator;
  private
    FAutoKey: Boolean;
    FList: TList<TKeyValue>;
    FComparer: TCompare<K>;
    function KeyEquals(const A, B: K): Boolean;
    function GetCount: Integer;
    function GetItem(Index: Integer): TKeyValue;
    function GetKey(Index: Integer): K;
    function GetValue(const Key: K): V;
    procedure SetValue(const Key: K; const Value: V);
  protected
    procedure AddKeyValue(KeyValue: TKeyValue); virtual;
    function CreateKeyValue(const Key: K): TKeyValue; virtual;
    procedure DestroyKeyValue(KeyValue: TKeyValue); virtual;
    procedure ChangeKeyValue(KeyValue: TKeyValue; Value: V); virtual;
    { Return a default value if no key exists }
    function DefaultValue: V; virtual;
    { When true key values will automatically be created when a value is requested
   if it doesn't exist }
    property AutoKey: Boolean read FAutoKey write FAutoKey;
  public
    { Create the dictionary }
    constructor Create;
    destructor Destroy; override;
    { Remove an item by key index }
    procedure Remove(const Key: K);
    { Remove all items from the dictionary }
    procedure Clear;
    { Returns true if the key is in the dictionary }
    function KeyExists(const Key: K): Boolean;
    { Used to compare keys }
    property Comparer: TCompare<K> read FComparer write FComparer;
    { Items indexed by integer }
    property Items[Index: Integer]: TKeyValue read GetItem;
    { Keys indexed by integer }
    property Keys[Index: Integer]: K read GetKey;
    { Values indexed by key }
    property Values[const Key: K]: V read GetValue write SetValue; default;
    { Number of key value pairs }
    property Count: Integer read GetCount;
  end;

{ TCollection\<TItem\> it a simple collection of items
  See also
  <link Overview.Tiny.Types.TCollection, TCollection members> }

  TCollection<TItem> = class
  public
    { The list type }
    type TItems = TArrayList<TItem>;
    type TCollectionEnumerator = TArrayEnumerator<TItem>;
    { Get the enumerator for the list }
    function GetEnumerator: TCollectionEnumerator;
  protected
    { The list of items }
    Items: TItems;
    { Returns the number of items in the collection }
    function GetCount: Integer;
    { Returns item and index position }
    function GetItem(Index: Integer): TItem;
    { Number of items in the collection }
    property Count: Integer read GetCount;
    { An integer based indexer }
    property Item[Index: Integer]: TItem read GetItem;
  end;

{ TOwnerCollection\<T\> is a simple managed collection of objects }

  TOwnerCollection<TItem: TObject> = class(TCollection<TItem>)
  public
    destructor Destroy; override;
  end;

{ Named objects have an identifiable that can be set or get }

  INamed = interface
    ['{B9A5411D-561B-43F6-B5E0-0C690EB3D4DB}']
    function GetName: string;
    procedure SetName(const Value: string);
    { An identifiable name }
    property Name: string read GetName write SetName;
  end;

{ TNamedList\<T\> is a generic growable list of named items
  See also
  <link Overview.Tiny.Types.TNamedList, TNamedList members> }

  TNamedList<T: IInterface> = class(TList<T>)
  public
    { Add an item and set its name }
    procedure AddName(const Name: string; Item: T);
    { Remove an item by name }
    procedure RemoveName(const Name: string);
    { Find an item by name }
    function FindName(const Name: string): T;
  end;
{$endregion}

{ These string routines support UTF8 text (needs testing) }

{$region string routines}
const
  { End of line characters used by various operating systems [group string] }
  LineBreakStyles: array[TTextLineBreakStyle] of string = (#10, #13#10, #13);
  { The character used to begin command line switches [group string] }
  SwitchChar = '-';

{ Returns the line break sequence for the current operating system [group string] }
function LineBreak: string; inline;
{ Convert a string to uppercase [group string] }
function StrUpper(const S: string): string;
{ Convert a string to lowercase [group string] }
function StrLower(const S: string): string;
{ Copies a substring given a start and length [group string] }
function StrCopy(const S: string; Start: Integer; Len: Integer = 0): string;
{ Copy a memory buffer into a string [group string] }
function StrCopyData(P: Pointer; Len: Integer): string;
{ Inserts a substring into a string at a position [group string] }
function StrInsert(const S, SubStr: string; Position: Integer): string;
{ Compares two strings optionally ignoring case returning -1 if A comes before
  before B, 1 if A comes after b, ord 0 if A and B are equal [group string] }
function StrCompare(const A, B: string; IgnoreCase: Boolean = False): Integer;
{ Searches a string for a substring optionally ignoring case [group string] }
function StrFind(const S, SubStr: string; IgnoreCase: Boolean = False): Integer; overload;
{ Searches a string for a substring from a start position optionally ignoring case [group string] }
function StrFind(const S, SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer; overload;
{ Returns the number of a substring matches within a string [group string] }
function StrFindCount(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
{ Returns an array of indices of a substring matches within a string [group string] }
function StrFindIndex(const S, SubStr: string; IgnoreCase: Boolean = False): IntArray;
{ Replaces every instance of a pattern in a string [group string] }
function StrReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
{ Replaces the first instance of a pattern in a string [group string] }
function StrReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
{ Replaces everything aftger the first instance of a pattern in a string [group string] }
function StrReplaceAfter(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
{ Trims white space from both sides of a string [group string] }
function StrTrim(const S: string): string;
{ Returns true if a case insensitive string matches a value [group string] }
function StrEquals(const S: string; Value: string): Boolean; overload;
{ Returns true if a case insensitive string matches a set of value [group string] }
function StrEquals(const S: string; const Values: array of string): Boolean; overload;
{ Returns the index of a string in a string array or -1 if there is no match [group string] }
function StrIndex(const S: string; const Values: array of string): Integer;
{ Splits a string into a string array using a separator [group string] }
function StrSplit(const S, Separator: string): StringArray;
{ Splits a string into a int array using a separator [group string] }
function StrSplitInt(const S, Separator: string): IntArray;
{ Splits a string into a int64 array using a separator [group string] }
function StrSplitInt64(const S, Separator: string): Int64Array;
{ Join a string array into a string using a separator [group string] }
function StrJoin(const A: StringArray; const Separator: string): string;
{ Join an int array into a string using a separator [group string] }
function StrJoinInt(const A: IntArray; const Separator: string): string;
{ Returns the first subsection of a string if it were split using a separator [group string] }
function StrFirstOf(const S, Separator: string): string;
{ Returns the second subsection of a string if it were split using a separator [group string] }
function StrSecondOf(const S, Separator: string): string;
{ Returns the last subsection of a string if it were split using a separator [group string] }
function StrLastOf(const S, Separator: string): string;
{ Search a string for a substring optionally ignoring case [group string] }
function StrContains(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
{ Returns true if a string begins with a substring while optionally ignoring case [group string] }
function StrBeginsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
{ Returns true if a string end with a substring while optionally ignoring case [group string] }
function StrEndsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
{ Returns a hashed value of a string [group string] }
function StrHash(const S: string): LargeWord;
{ Returns a string of a given length filled with one repeating character [group string] }
function StrOf(C: Char; Len: Integer): string;
{ Returns a string made to fit a given length padded on the left with a character [group string] }
function StrPadLeft(const S: string; C: Char; Len: Integer): string;
{ Returns a string made to fit a given length padded on the right with a character [group string] }
function StrPadRight(const S: string; C: Char; Len: Integer): string;
{ Returns a string surrounded by quotes if it contains whitespace [group string] }
function StrQuote(const S: string): string;
{ Returns true if a string contains only whitespace characters [group string] }
function StrIsBlank(const S: string): Boolean;
{ Returns true if a string matches to rules of an identifier [group string] }
function StrIsIdent(const S: string): Boolean;
{ Returns true if a string matches to rules of an attribute [group string] }
function StrIsAttr(const S: string): Boolean;
{ Returns the line break style for a block of text [group string] }
function StrLineBreakStyle(const S: string): TTextLineBreakStyle;
{ Converts the line break style of a block of text using the desired style [group string] }
function StrAdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string; overload;
{ Converts the line break style of a block of text using the system defined style [group string] }
function StrAdjustLineBreaks(const S: string): string; overload;

function IsAlpha(C: Char): Boolean;

{ Convert a string to a wide string }
function StrToWide(const S: string): WideString;
{ Convert a wide string to string }
function WideToStr(const S: WideString): string;

{ Returns true if a program has a matching switch
  See also
  <link Tiny.System.SwitchIndex, SwitchIndex function>
  <link Tiny.System.SwitchValue, SwitchValue function> [group string] }
function SwitchExists(const Switch: string): Boolean;
{ Returns the index if of a program's matching switch or -1 if no match was found
  See also
  <link Tiny.System.SwitchExists, SwitchExists function>
  <link Tiny.System.SwitchValue, SwitchValue function> [group string] }
function SwitchIndex(const Switch: string): Integer;
{ Returns the value if of a program's switch
  See also
  <link Tiny.System.SwitchExists, SwitchExists function>
  <link Tiny.System.SwitchIndex, SwitchIndex function> [group string] }
function SwitchValue(const Switch: string): string;
{ Convert an integer to a string [group string] }
function IntToStr(Value: Integer): string;
{ Convert a string to an integer. Can throw an EConvertError exception. [group string] }
function StrToInt(const S: string): Integer;
{ Convert a string an integer. Returns a default value if conversion cannot be done. [group string] }
function StrToIntDef(const S: string; Default: Integer): Integer;
{ Convert an Int64 to a string [group string] }
function Int64ToStr(Value: Int64): string;
{ Convert a string to an Int64. Can throw an EConvertError exception. [group string] }
function StrToInt64(const S: string): Int64;
{ Convert a string an Int64. Returns a default value if conversion cannot be done. [group string] }
function StrToInt64Def(const S: string; Default: Int64): Int64;
{ Convert a float to a string [group string] }
function FloatToStr(Value: Extended): string; overload;
{ Convert a float to a string with a given number of decimals [group string] }
function FloatToStr(Value: Extended; Decimals: Integer): string; overload;
{ Convert a string to a float. Can throw an EConvertError exception. [group string] }
function StrToFloat(const S: string): Extended;
{ Convert a string a float. Returns a default value if conversion cannot be done. [group string] }
function StrToFloatDef(const S: string; Default: Extended): Extended;
{ Formats a series of argument into a string
  Remarks
  %c to format an object
  %s to format a string
  %d to format a whole number
  %f to format a float
  %.nf to format a float to n decimal places (e.g. %.2f might return 1.23) [group string] }
function StrFormat(const S: string; Args: array of const): string;
{$endregion}

{$region helpers}
{ StringHelper }

type
  StringHelper = record helper for string
  private
    function GetIsEmpty: Boolean;
    function GetIsWhitespace: Boolean;
    function GetIsIdentifier: Boolean;
    function GetIsAttribute: Boolean;
    function GetLength: Integer;
    procedure SetLength(Value: Integer);
  public
    { Convert to a string representation }
    function ToString: string;
    { Make a string unique, reducing its reference count to one }
    procedure Unique;
    { Repeat a character a given length a into string }
    procedure CharInto(C: Char; Len: Integer);
    { Copy a memory buffer into string }
    procedure CopyInto(P: Pointer; Len: Integer);
    { Inserts a substring at a position into string }
    procedure InsertInto(const SubStr: string; Position: Integer);
    { Returns true if a string matches a case insensitive value }
    function Equals(const Value: string; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns true if a string matches any in a set of case insensitive values }
    function Equals(const Values: array of string; IgnoreCase: Boolean = False): Boolean; overload;
    { Compares two strings optionally ignoring case returning -1 if string comes before
   before value, 1 if string comes after value, ord 0 if string and value are equal }
    function Compare(const Value: string; IgnoreCase: Boolean = False): Integer;
    { Convert a string to uppercase }
    function ToUpper: string;
    { Convert a string to lowercase }
    function ToLower: string;
    { Copies a substring given a start and length }
    function Copy(Start: Integer; Len: Integer = 0): string;
    { Insert a substring given a start and length }
    function Insert(const SubStr: string; Position: Integer): string;
    { Searches a string for a substring optionally ignoring case }
    function IndexOf(const SubStr: string; IgnoreCase: Boolean = False): Integer; overload;
    { Searches a string for a substring from a start position optionally ignoring case }
    function IndexOf(const SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer; overload;
    { Returns the number of a substring matches within a string }
    function MatchCount(const SubStr: string; IgnoreCase: Boolean = False): Integer;
    { Returns an array of indices of a substring matches within a string }
    function Matches(const SubStr: string; IgnoreCase: Boolean = False): IntArray;
    { Replaces every instance of a pattern in a string }
    function Replace(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
    { Replaces the first instance of a pattern in a string }
    function ReplaceOne(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
    { Replaces everything aftger the first instance of a pattern in a string }
    function ReplaceAfter(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
    { Trims white space from both sides of a string }
    function Trim: string;
    { Returns the index of a string in a string array or -1 if there is no match }
    function ArrayIndex(const Values: array of string): Integer;
    { Break a string into lines separated by the break style  }
    function Lines: StringArray;
    { Returns the first entire line containing SubStr }
    function LineWith(const SubStr: string; IgnoreCase: Boolean = False): string;
    { Splits a string into a string array using a separator }
    function Split(Separator: string): StringArray;
    { Splits a string into a int array using a separator }
    function SplitInt(const Separator: string): IntArray;
    { Splits a string into a int64 array using a separator }
    function SplitInt64(const Separator: string): Int64Array;
    { Splits a string into word separated by whitespace }
    function Words(MaxColumns: Integer = 0): StringArray;
    { Returns the first subsection of a string if it were split using a separator }
    function FirstOf(const Separator: string): string;
    { Returns the second subsection of a string if it were split using a separator }
    function SecondOf(const Separator: string): string;
    { Returns the last subsection of a string if it were split using a separator }
    function LastOf(const Separator: string): string;
    { Returns the text exclusive between markers A and B }
    function Between(const MarkerA, MarkerB: string): string;
    { Search a string for a substring optionally ignoring case }
    function Contains(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
    { Returns true if a string begins with a substring while optionally ignoring case }
    function BeginsWith(const SubStr: string; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns true if a string begins with any substring while optionally ignoring case }
    function BeginsWith(const SubStrs: StringArray; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns true if a string end with a substring while optionally ignoring case }
    function EndsWith(const SubStr: string; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns true if a string end with any substring while optionally ignoring case }
    function EndsWith(const SubStrs: StringArray; IgnoreCase: Boolean = False): Boolean; overload;
    { Returns a string made to fit a given length padded on the left with a character }
    function PadLeft(C: Char; Len: Integer): string;
    { Returns a string made to fit a given length padded on the right with a character }
    function PadRight(C: Char; Len: Integer): string;
    { Returns a hash of the string }
    function Hash: LargeWord;
    { Returns a string surrounded by quotes if it contains whitespace }
    function Quote: string;
    { Formats a series of argument into a string }
    function Format(Args: array of const): string;
    { Analyze a string and find its line break style }
    function LineBreakStyle: TTextLineBreakStyle;
    { Converts the line break style of a string to a the desired style }
    function AdjustLineBreaks(Style: TTextLineBreakStyle): string; overload;
    { Converts the line break style of a string to the system preferred defined style }
    function AdjustLineBreaks: string; overload;
    { Gets true if a string contains only whitespace characters }
    property IsEmpty: Boolean read GetIsEmpty;
    { Gets true if a string contains only whitespace characters }
    property IsWhitespace: Boolean read GetIsWhitespace;
    { Gets true if a string matches to rules of an identifier }
    property IsIdentifier: Boolean read GetIsIdentifier;
    { Gets true if a string matches to rules of an attribute }
    property IsAttribute: Boolean read GetIsAttribute;
    {  Gets or sets the length allocated for the string }
    property Length: Integer read GetLength write SetLength;
  end;
{$endregion}

{$region delegates}
{ IDelegate\<T\> allows event subscribers to add or remove their event handlers
  See also
  <link Overview.Tiny.System.IDelegate, IDelegate\<T\> members> }

  IDelegate<T> = interface
  ['{ADBC29C1-4F3D-4E4C-9A79-C805E8B9BD92}']
    { Check if there are no subscribers }
    function GetIsEmpty: Boolean;
    { A subscriber calls add to register an event handler }
    procedure Add(const Handler: T);
    { A subscriber calls remove to unregister an event handler }
    procedure Remove(const Handler: T);
    { Empty is true when there are no subscribers }
    property IsEmpty: Boolean read GetIsEmpty;
  end;

{doc off}
  IDelegateContainer<T> = interface
  ['{ED255F00-3112-4315-9E25-3C1B3064C932}']
    function GetEnumerator: TArrayEnumerator<T>;
    function GetDelegate: IDelegate<T> ;
    property Delegate: IDelegate<T> read GetDelegate;
  end;

  TDelegateImpl<T> = class(TInterfacedObject, IDelegate<T>)
  private
    FList: TArrayList<T>;
    function IndexOf(Event: T): Integer;
  protected
    function GetIsEmpty: Boolean;
    procedure Add(const Event: T);
    procedure Remove(const Event: T);
  end;

  TDelegateContainerImpl<T> = class(TInterfacedObject, IDelegateContainer<T>)
  private
    type TDelegateClass = TDelegateImpl<T>;
    var FDelegateClass: TDelegateClass;
    var FDelegate: IDelegate<T>;
  protected
    { IDelegateContainer<T> }
    function GetEnumerator: TArrayEnumerator<T>;
    function GetDelegate: IDelegate<T>;
  end;
{doc on}

{ TDelegate\<T\> allows an event publisher accept multiple subscribers.
  Concept described here http://www.codebot.org/delphi/?doc=9568
  See also
  <link Overview.Tiny.System.TDelegate, TDelegate\<T\> members> }

  TDelegate<T> = record
  private
    type TDelegateContainer = TDelegateContainerImpl<T>;
    var FContainer: IDelegateContainer<T>;
    function GetContainer: IDelegateContainer<T>;
  public
    { Convert a delegate into an interface suitable for subscribers }
    class operator Implicit(var Delegate: TDelegate<T>): IDelegate<T>;
    { Get the enumerator of subscriber's events }
    function GetEnumerator: TArrayEnumerator<T>;
    { Check is there are no subscribers }
    function GetIsEmpty: Boolean;
    { Add an event handler }
    procedure Add(const Handler: T);
    { Remove an event handler }
    procedure Remove(const Handler: T);
    { Returns true is there a no subscribers }
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  { Notify event publisher }
  TNotifyDelegate = TDelegate<TNotifyEvent>;
  { Notify event subscriber }
  INotifyDelegate = IDelegate<TNotifyEvent>;
  { Method event publisher }
  TMethodDelegate = TDelegate<TMethodEvent>;
  { Method event subscriber }
  IMethodDelegate = IDelegate<TMethodEvent>;

  { User interface events shall pack their arguments using  TEventHandler\<T\> }
  TEventHandler<T> = procedure(Sender: TObject; Args: T) of object;
  { TEmptyArgs is the default when no args exsist }
  TEmptyArgs = record end;
  { TEventEvent is the alias for an event event handler type }
  TEmptyEvent = TEventHandler<TEmptyArgs>;
  { Method event publisher }
  TEmptyDeletegate = TNotifyDelegate<TEmptyEvent>;
  { Notify event subscriber }
  IEmptyDeletegate = IDelegate<TEmptyEvent>;

  { Reusable empty arguments }

var
  EmptyArgs: TEmptyArgs;
{$endregion}

{$region file system routines}
type
  { Mode used when creating a file handle or file stream [group files] }
  TFileMode = (
    { Create a new file overwriting any existing contents }
    fmCreate,
    { Open an existing file }
    fmOpen,
    { Open an existing file or create one if none exists }
    fmOpenOrCreate,
    { Create or opens file which automatically performs writes at the end }
    fmAppend);

  { Determines the relationship of offset when seeking a file handle or stream [group files] }
  TSeekOrigin = (
    { Seek offset is distance from the start of the file }
    soBegin,
    { Seek offset is from the current file position }
    soCurrent,
    { Seek offset is distance backwards from the end of file }
    soEnd);

{ Returns true if a file exists [group files] }
function FileExists(const FileName: string): Boolean;
{ Delete a file [group files] }
function FileDelete(const FileName: string): Boolean;
{ Rename a file [group files] }
function FileRename(const OldName, NewName: string): Boolean;
{ Copy a file [group files] }
function FileCopy(const FileName, CopyName: string): Boolean;
{ Given a file name find the size in bytes of a file or -1 if the file is not present [group files] }
function FileSize(const FileName: string): LargeInt; overload;
{ Given a file handle find the in bytes of a file or -1 if the file is not present [group files] }
function FileSize(F: HFile): LargeInt; overload;
{ Generates a file handle given a TFileMode [group files] }
function FileAccess(const FileName: string; Mode: TFileMode): HFile;
{ Create a new file returning a file handle [group files] }
function FileCreate(const FileName: string): HFile;
{ Open or create anew file overwriting any existing contents returning a file handle [group files] }
function FileOpen(const FileName: string): HFile;
{ Create or opens file which automatically performs writes at the end returning a file handle [group files] }
function FileAppend(const FileName: string): HFile;
{ Moves the file cursor an offset distance from the origin returning the
  new position in relation to the start of the file or -1 if there was an error [group files] }
function FileSeek(F: HFile; Offset: LargeInt; Origin: TSeekOrigin): LargeInt;
{ Read from a len bytes from file returning the actual bytes read or -1 if there was an error [group files] }
function FileRead(F: HFile; Buffer: Pointer; Len: LargeInt): LargeInt;
{ Read len character from a file [group files] }
function FileReadString(F: HFile; Len: LargeInt): string;
{ Write len bytes to a file returning actual bytes written [group files] }
function FileWrite(F: HFile; Buffer: Pointer; Len: LargeInt): LargeInt;
{ Write a string to a file returning actual bytes written [group files] }
function FileWriteString(F: HFile; const S: string): LargeInt;
{ Write a line a file returning actual bytes written [group files] }
function FileWriteLine(F: HFile; const S: string): LargeInt;
{ Close a file handle [group files] }
function FileClose(var F: HFile): Boolean;
{ Load the entire contents of a file as text [group files] }
function FileLoadText(const FileName: string): string;
{ Save text overwritting the entire contents of a file [group files] }
procedure FileSaveText(const FileName, Text: string);
{ Append text to a file [group files] }
procedure FileAppendText(const FileName, Text: string); overload;
{ Append formatted text to a file [group files] }
procedure FileAppendText(const FileName, S: string; Args: array of const); overload;
{ Append a line to a file [group files] }
procedure FileAppendLine(const FileName, Text: string); overload;
{ Append a formatted line to a file [group files] }
procedure FileAppendLine(const FileName, S: string; Args: array of const); overload;
{ Extract the name portion of a file name [group files] }
function FileExtractName(const FileName: string): string;
{ Extract the extension portion of a file name [group files] }
function FileExtractExt(const FileName: string): string;
{ Change the extension portion of a file name [group files] }
function FileChangeExt(const FileName, Extension: string): string;
{ Extract the path portion of a file name [group files] }
function FileExtractPath(const FileName: string): string;
{ Returns true if a directory exists [group files] }
function DirExists(const Directory: string): Boolean;
{ Rename a directory return true is successful [group files] }
function DirRename(const OldName, NewName: string): Boolean;
{ Delete a directory return true is successful [group files] }
function DirDelete(const Directory: string): Boolean;
{ Create a directory return true is successful [group files] }
function DirCreate(const Directory: string): Boolean;
{ Retrieve the current directory [group files] }
function DirGetCurrent: string;
{ Change the current directory return true is successful [group files] }
function DirSetCurrent(const Directory: string): Boolean;
{ Change path delimiter to match system settings [group files] }
function PathAdjustDelimiters(const Path: string): string;
{ Include the system defined path delimiter at the end of the path [group files] }
function PathIncludeTrailingDelimiter(const Path: string): string;
{ Exclude the system defined path delimiter from the end of the path [group files] }
function PathExcludeTrailingDelimiter(const Path: string): string;
{ Combine two path using the system defined path delimiter [group files] }
function PathCombine(const A, B: string): string;
{ Expands a path to be absolute to the file system root [group files] }
function PathExpand(S: string): string;

{$endregion}

{$region findfile}
{ Find folders, files, or both }

type
  TFindKind = (findFolder, findFile, findBoth);

{ TFindResult is used by find first, find next and find close }

  TFindResult = record
    { The path being searched }
    Path: string;
    { The file or folder name }
    Name: string;
    { The kind of item found }
    Kind: TFindKind;
    { Reserved }
    Handle: Pointer;
  end;

{ Begin a find file or directory search capturing information in find result
  Remarks
  You may use the * wildcard in the file name portion of search. If find first
  returns true, a match was found and you must release find result using
  find close when done searching. }
function FindFirst(const Search: string; Kind: TFindKind; out FindResult: TFindResult): Boolean;
{ Search for the next find result returning true if a match was found }
function FindNext(var FindResult: TFindResult): Boolean;
{ Closes a find search releasing resources allocated by find first }
procedure FindClose(var FindResult: TFindResult);
{ Find a list of files given a search string }
function FindFiles(const Search: string): StringArray;
{$endregion}

{$region streams}
{ <include docs/tiny.system.tstream.txt> }
type
  TStream = class
  private
    FBufferSize: Word;
    procedure SetBufferSize(Value: Word);
  protected
    function GetSize: LargeWord; virtual; abstract;
    procedure SetSize(const Value: LargeWord); virtual; abstract;
    function GetPosition: LargeWord; virtual; abstract;
    procedure SetPosition(const Value: LargeWord); virtual; abstract;
  public
    constructor Create;
    { Read len bytes from a buffer and return actual bytes read }
    function Read(var Buffer; Len: LargeWord): LargeWord; virtual; abstract;
    { Write len bytes to a buffer and return actual bytes written }
    function Write(var Buffer; Len: LargeWord): LargeWord; virtual; abstract;
    { Move the position by an offset relative to a seek origin }
    function Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord; virtual; abstract;
    { Read a number of characters from the stream }
    function ReadStr(Len: LargeWord): string;
    { Write a string to the stream }
    procedure WriteStr(const S: string);
    { Load the stream given a file name }
    procedure LoadFromFile(const FileName: string);
    { Save the stream given a file name }
    procedure SaveToFile(const FileName: string);
    { Copy to another stream. If count is 0, the entire steam is copied. }
    function Copy(Source: TStream; Len: LargeWord): LargeWord;
    { The current stream position }
    property Position: LargeWord read GetPosition write SetPosition;
    { The size in bytes of the stream }
    property Size: LargeWord read GetSize write SetSize;
    { A hint to stream consumers suggesting the maximum number of bytes that
   should be used in a streaming operation }
    property BufferSize: Word read FBufferSize write SetBufferSize;
  end;

{ TFileStream is a stream which reads from and writes to files
  See also
  <link Overview.Tiny.TFileStream, TFileStream members> }

  TFileStream = class(TStream)
  private
    FFileName: string;
    FFile: HFile;
  protected
    function GetSize: LargeWord; override;
    procedure SetSize(const {%H-}Value: LargeWord); override;
    function GetPosition: LargeWord; override;
    procedure SetPosition(const Value: LargeWord); override;
  public
    constructor Create(FileName: string; FileMode: TFileMode = fmOpen);
    destructor Destroy; override;
    function Read(var Buffer; Len: LargeWord): LargeWord; override;
    function Write(var Buffer; Len: LargeWord): LargeWord; override;
    function Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord; override;
    { The underlying file handle }
    property Handle: HFile read FFile;
  end;

{ TStringStream is a stream which reads from and writes to a data string
  See also
  <link Overview.Tiny.TStringStream, TStringStream members> }

  TStringStream = class(TStream)
  private
    FData: string;
    FPosition: LargeWord;
    procedure SetData(const Value: string);
  protected
    function GetSize: LargeWord; override;
    procedure SetSize(const Value: LargeWord); override;
    function GetPosition: LargeWord; override;
    procedure SetPosition(const Value: LargeWord); override;
  public
    constructor Create(const S: string = '');
    function Read(var Buffer; Len: LargeWord): LargeWord; override;
    function Write(var Buffer; Len: LargeWord): LargeWord; override;
    function Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord; override;
    { The underlying string data }
    property Data: string read FData write SetData;
  end;

{ TMemoryStream is a stream which reads from and writes to memory
  See also
  <link Overview.Tiny.TMemoryStream, TMemoryStream members> }

  TMemoryStream = class(TStream)
  private
    FMemory: Pointer;
    FSize: LargeWord;
    FAllocSize: LargeWord;
    FPosition: LargeWord;
  protected
    function GetSize: LargeWord; override;
    procedure SetSize(const Value: LargeWord); override;
    function GetPosition: LargeWord; override;
    procedure SetPosition(const Value: LargeWord); override;
  public
    constructor Create(AllocSize: LargeWord = 0);
    function Read(var Buffer; Len: LargeWord): LargeWord; override;
    function Write(var Buffer; Len: LargeWord): LargeWord; override;
    function Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord; override;
    { The underlying memory }
    property Memory: Pointer read FMemory;
  end;
{$endregion}

{$region exceptions}
{ <include docs/tiny.system.exception.txt> }

type
  Exception = class(TObject)
  private
    FMessage: string;
  public
    { Creates an exception }
    constructor Create(const Msg: string); virtual;
    constructor CreateFmt(const Msg: string; Args: array of const); virtual;
    property Message: string read FMessage;
  end;

{ ExceptionClass defines a type of exception }

  ExceptionClass = class of Exception;

{doc off}
  EAssertError = class(Exception)
  private
    FFileName: string;
    FLineNumber: Integer;
    procedure Generate(const FileName: string; LineNumber: Integer);
  public
    property FileName: string read FFileName;
    property LineNumber: Integer read FLineNumber;
  end;

  EIOError = class(Exception)
  private
    FCode: Integer;
    procedure Generate(Code: Integer);
  public
    property Code: Integer read FCode;
  end;

  EHeapError = class(Exception)
  public
    AllowFree: Boolean;
    procedure FreeInstance; override;
  end;

  EListError = class(Exception)
  end;

  EOutOfMemoryError = class(EHeapError);
  EInvalidPtrError = class(EHeapError);
  EDivByZeroError = class(Exception);
  ERangeError = class(Exception);
  EIntOverflowError = class(Exception);
  EInvalidOpError = class(Exception);
  EZeroDivideError = class(Exception);
  EOverflowError = class(Exception);
  EUnderflowError = class(Exception);
  EObjectCheckError = class(Exception);
  EInvalidCastError = class(Exception);
  EAccessViolationError = class(Exception);
  EBusError = class(Exception);
  EControlBreakError = class(Exception);
  EPrivInstructionError = class(Exception);
  EStackOverflowError = class(Exception);
  EVariantError = class(Exception);
  EExternalError = class(Exception);
  EIntfCastError = class(Exception);
  ESafeCallError = class(Exception);
  EAbstractError = class(Exception);
  EAbortError = class(Exception);
  EQuitSignalError = class(Exception);
  ENoThreadSupportError = class(Exception);
  ENoWideStringSupportError = class(Exception);
  ENotImplementedError = class(Exception);
  EConvertError = class(Exception);
  EStreamError = class(Exception);
  ESelfReferenceError = class(Exception);
  EResNotFoundError = class(Exception);
  ESynchronizeError = class(Exception);
  EAssetError = class(Exception);
  ELibraryException = class(Exception);

  ESDLError = class(Exception)
  public
    constructor CreateFunc(const FuncName: string);
  end;
{doc on}

{ Raises an EAbortError exception causing the current method chain to silently exit }

procedure Abort;
{$endregion}

{$region threading}
{ TThread is the abstract class for executing multithreaded code
  Remarks
  Derive your own TThread class and override the Execute method with your
  logic. You may want to periodically check Terminated in your Execute method
  to exit the thread early.
  See also
  <link Overview.Tiny.System.TThread, TThread members>
  <link Tiny.System.IMutex, IMutex interface>
  <link Tiny.System.Lock, Lock function>
  <link Tiny.Game.TWindow.Multithreaded, TWindow.Multithreaded property> }

type
  TThread = class(TObject)
  private
    FHandle: TThreadID;
    FCreateSuspended: Boolean;
    FSemaphore: Pointer;
    { 0 = not started, 1 = started, 2 = done }
    FState: LongInt;
    FTerminated: LongInt;
    FExitCode: LongWord;
    FFreeOnTerminate: Boolean;
    FOnTerminate: TNotifyEvent;
    function GetTerminated: Boolean;
    function GetDone: Boolean;
  protected
    { Override this method to define your own thread logic returning an exit code }
    function Execute: LongWord; virtual; abstract;
    { Returns true when a request to terminate the thread has been made }
    property Terminated: Boolean read GetTerminated;
    { Set this value to true the to automatically destroy the thread after execution
   Remarks
   It is unsafe to use a thread after its execution begins when this value
   is set to true, as the thread can complete and be destroyed at anytime }
    property FreeOnTerminate: Boolean read FFreeOnTerminate write FFreeOnTerminate;
  public
    constructor Create(Suspended: Boolean = False); overload;
    constructor Create(OnTerminate: TNotifyEvent; Suspended: Boolean = False); overload;
    destructor Destroy; override;
    procedure Resume;
    procedure Suspend;
    procedure Terminate;
    function WaitFor(Timeout: LongInt): Boolean;
    { Returns true if the thread has completed its execution }
    property Done: Boolean read GetDone;
    { The thread's exit code }
    property ExitCode: LongWord read FExitCode;
  end;

{ IMutex allows you to lock other threads which share the mutex
  See also
  <link Overview.Tiny.System.IMutex, IMutex members>
  <link Tiny.System.CreateMutex, CreateMutex function>
  <link Tiny.System.TThread, TThread class>
  <link Tiny.Game.TWindow.Multithreaded, TWindow.Multithreaded property> }

type
  IMutex = interface(IInterface)
    ['{53ACDF7C-CFE5-421A-B494-5D8EAEAF12DD}']
    { Causes a thread to wait to acquire a ownership of the mutex }
    procedure Lock;
    { Causes a thread to wait a given number of milliseconds for a ownership of the mutex }
    function TryLock(Timeout: LongWord = 0): Boolean;
    { Releases ownership of the mutex allowing the next waiting thread to continue }
    procedure Unlock;
  end;

{ Creates a mutex }

function CreateMutex: IMutex;

{ Causes a thread to wait to acquire a ownership lock
  Remarks
  It is safe to call for a lock multiple nested times in a thread, but each lock
  must be matched with an unlock
  See also
  <link Tiny.System.TryLock, TryLock function>
  <link Tiny.System.Unlock, Unlock procedure> }
procedure Lock;
{ Causes a thread to wait a given number of milliseconds for a ownership lock
  See also
  <link Tiny.System.Lock, Lock function>
  <link Tiny.System.Unlock, Unlock procedure> }
function TryLock(Timeout: LongWord = 0): Boolean;
{ Releases ownership of a lock allowing the next waiting thread to continue
  See also
  <link Tiny.System.Lock, Lock function>
  <link Tiny.System.TryLock, TryLock procedure> }
procedure Unlock;
{$endregion}

implementation

uses
  {$ifdef unix}
  Tiny.Interop.Unix,
  {$endif}
  {$ifdef windows}
  Tiny.Interop.Windows,
  {$endif}
  Tiny.Interop.SDL2,
  Tiny.Constants;

var
  SystemLock: IMutex;

{$ifdef linux}
  {$linklib c}
  {$linklib m}
{$endif}
{$ifdef windows}
  procedure {%H-}___chkstk_ms; public name '___chkstk_ms'; begin end;
  {$linklib ../libs/libmsvcr120.a}
  {$linklib ../libs/libmsvcr120_app.a}
{$endif}

{$region runtime library}
procedure FreeAndNil(var Obj: TObject);
var
  A: TObject;
begin
  A := Obj;
  Obj := nil;
  A.Free;
end;

procedure MemZero(out Buffer; Size: UIntPtr);
begin
  FillChar(Buffer{%H-}, Size, 0);
end;

function MemCompare(const A, B; Size: LongWord): Boolean;
var
  C, D: PByte;
begin
  C := @A;
  D := @B;
  if (C = nil) or (D = nil) then
    Exit(False);
  while Size > 0 do
  begin
    if C^ <> D^ then
      Exit(False);
    Inc(C);
    Inc(D);
    Dec(Size);
  end;
  Result := True;
end;

function FloatEqual(A, B: Float): Boolean;
begin
  Result := Abs(A - B) < Epsilon;
end;

function LibraryLoad(const FileName: string): HModule;
begin
  Result := SDL_LoadObject(PChar(FileName));
end;

procedure LibraryFree(Module: HModule);
begin
  SDL_UnloadObject(Module);
end;

function GetProcAddress(Module: HModule; const ProcName: string): Pointer;
begin
  Result := SDL_LoadFunction(Module, PChar(ProcName));
end;

procedure Sleep(Milliseconds: LongWord);
begin
  SDL_Delay(Milliseconds);
end;

function Now: Double;
const
  TimeFrequency: Uint64 = 0;
  TimeStart: Double = 0;
begin
  if TimeFrequency = 0 then
  begin
    TimeFrequency := SDL_GetPerformanceFrequency;
    TimeStart := SDL_GetPerformanceCounter / TimeFrequency;
  end;
  Result := SDL_GetPerformanceCounter / TimeFrequency - TimeStart;
end;

{$ifdef windows}
type
  TSystemTime = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;

procedure GetLocalTime(out T: TSystemTime); cdecl; external 'kernel32.dll';

function NowTime: TDateTime;
var
  T: TSystemTime;
begin
  GetLocalTime(T);
  Result := T.wHour / 24 + T.wMinute / (24 * 60) + T.wSecond / (24 * 60 * 60) + T.wMilliseconds / (24 * 60 * 60 * 1000);
end;

procedure NowTime(out Hour, Min, Sec, MSec: Word);
var
  T: TSystemTime;
begin
  GetLocalTime(T);
  Hour := T.wHour;
  Min := T.wMinute;
  Sec := T.wSecond;
  MSec := T.wMilliseconds;
end;
{$else}
type
  PTimeVal = ^TTimeVal;
  TTimeVal = record
    tv_sec: Int64;     // seconds
    tv_usec: Int64;    // microseconds
  end;

  PTm = ^TTm;
  TTm = record
    tm_sec: Integer;   // seconds
    tm_min: Integer;   // minutes
    tm_hour: Integer;  // hours
    tm_mday: Integer;  // day of the month
    tm_mon: Integer;   // month
    tm_year: Integer;  // year
    tm_wday: Integer;  // day of the week
    tm_yday: Integer;  // day in the year
    tm_isdst: Integer; // daylight saving time
  end;

function gettimeofday(tp: PTimeVal; restrict: Pointer): Integer; cdecl; external 'c';
function localtime(tv_sec: PInt64): PTm; cdecl; external 'c';

function NowTime: TDateTime;
var
  T: TTimeVal;
  L: TTm;
begin
  gettimeofday(@T, nil);
  L := localtime(@T.tv_sec)^;
  Result := L.tm_hour + L.tm_min / 60 + L.tm_sec / 3600 + T.tv_usec / (3600 * 1000 * 1000);
end;

procedure NowTime(out Hour, Min, Sec, MSec: Word);
var
  T: TTimeVal;
  L: TTm;
begin
  gettimeofday(@T, nil);
  L := localtime(@T.tv_sec)^;
  Hour := L.tm_hour;
  Min := L.tm_min;
  Sec := L.tm_sec;
  MSec := Round(T.tv_usec / 1000);
end;
{$endif}

{procedure ShowMessage(const Msg: string);
begin
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_INFORMATION, 'Message', PChar(Msg), nil);
end;

procedure ShowError(ExceptObject: TObject; const Msg: string = '');
var
  S: string;
begin
  if ExceptObject <> nil then
  begin
    S := ExceptObject.ClassName +  ': ';
    if ExceptObject is Exception then
   S := S + (ExceptObject as Exception).Message;
    if Msg <> '' then
   S := S + LineEnding + LineEnding + Msg;
  end
  else
    S := Msg;
  if S = '' then
    S := 'Unknown reason';
  SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, 'Error', PChar(S), nil);
end;}

procedure WriteLine(const S: string);
begin
  WriteLn(S);
end;

procedure WriteLine(const S: string; Args: array of const);
begin
  WriteLn(StrFormat(S, Args));
end;
{$endregion}

{$region math routines}
function Ceil(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) > 0 then
    Inc(Result);
end;

function Floor(const X: Extended): Integer;
begin
  Result := Integer(Trunc(X));
  if Frac(X) < 0 then
    Dec(Result);
end;

function Divide(const Quotient, Divisor: Extended): Extended;
begin
  if Divisor = 0 then
    Result := 0
  else
  begin
    Result := Trunc(Quotient / Divisor + 0.001);
    Result := Result * Divisor;
  end;
end;

function Remainder(const Quotient, Divisor: Extended): Extended;
begin
  if Divisor = 0 then
    Result := 0
  else
    Result := Quotient - (Trunc(Quotient) div Trunc(Divisor)) * Divisor;
end;

function Max(A, B: Integer): Integer;
begin
  if A > B then Result := A else Result := B;
end;

function Max(A, B: Single): Single;
begin
  if A > B then Result := A else Result := B;
end;

function Max(const A, B: Double): Double;
begin
  if A > B then Result := A else Result := B;
end;

function Min(A, B: Integer): Integer;
begin
  if A < B then Result := A else Result := B;
end;

function Min(A, B: Single): Single;
begin
  if A < B then Result := A else Result := B;
end;

function Min(const A, B: Double): Double;
begin
  if A < B then Result := A else Result := B;
end;

function Tan(const X: Extended): Extended;
begin
  Result := Sin(X) / Cos(X);
end;

function ArcTan2(Y, X: Extended) : Extended;
begin
  if X = 0 then
  begin
    if Y = 0 then
      Result := 0
    else if Y > 0 then
      Result := PI / 2
    else
      Result := - PI / 2;
  end
  else
    Result := ArcTan(Y / X);
  if X < 0 then
    Result := Result + PI;
  if Result > PI then
    Result := Result - 2 * PI;
end;

procedure SinCos(const X: Extended; out S, C: Extended);
begin
  S := Sin(X);
  C := Cos(X);
end;

function Clamp(Value: Extended): Extended;
begin
  if Value < 0 then
    Result := 0
  else if Value > 1 then
    Result := 1
  else
    Result := Value;
end;

function Clamp(Value, Lo, Hi: Extended): Extended;
begin
  if Value < Lo then
    Result := Lo
  else if Value > Hi then
    Result := Hi
  else
    Result := Value;
end;

function Mix(Value, Lo, Hi: Extended): Extended;
begin
  if Value <= 0 then
    Result := Lo
  else if Value >= 1 then
    Result := Hi
  else
    Result := (1 - Value) * Lo + (Value) * Hi;
end;

function FloatToByte(F: Float): Byte;
begin
  if F < 0 then
    F := 0;
  if F > 1 then
    Result := 1
  else
    Result := Round(F * $FF);
end;

function DegToRad(D: Float): Float;
begin
  Result := D / 180 * Pi;
end;

function RadToDeg(R: Float): Float;
begin
  Result := R * 180 / Pi;
end;

function IntPower(Base: Float; Exponent: Integer): Float;
var
  I: LongInt;
begin
 if (Base = 0.0) and (Exponent = 0) then
   Exit(1);
  I := Abs(Exponent);
  Result := 1.0;
  while I > 0 do
  begin
    while (I and 1)=0 do
    begin
      I := I shr 1;
      Base := Sqr(Base);
    end;
    I := I - 1;
    Result := Result * Base;
  end;
  if Exponent < 0 then
    Result := 1 / Result;
end;
{$endregion}

{$region generic containers}
{ TArrayEnumerator<T> }

procedure TArrayEnumerator<T>.Create(Items: TArray<T>; Count: Integer = -1);
begin
  FItems := Items;
  FPosition := -1;
  if Count < 0 then
    FCount := Length(Items)
  else
    FCount := Count;
end;

function TArrayEnumerator<T>.GetCurrent: T;
begin
  Result := FItems[FPosition];
end;

function TArrayEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FCount;
end;

procedure TArrayEnumerator<T>.Reset;
begin
  FPosition := -1;
end;

{ TArrayList<T> }

function TArrayList<T>.GetEnumerator: TArrayListEnumerator;
begin
  Result.Create(Items);
end;

class operator TArrayList<T>.Implicit(const Value: TArrayList<T>): TArray<T>;
begin
  Result := Value.Items;
end;

class operator TArrayList<T>.Implicit(const Value: TArray<T>): TArrayList<T>;
begin
  Result.Items := Value;
end;

class operator TArrayList<T>.Implicit(const Value: array of T): TArrayList<T>;
var
  I: T;
begin
  for I in Value do
    Result.Push(I);
end;

class function TArrayList<T>.ArrayOf(const Items: array of T): TArrayList<T>;
var
  I: T;
begin
  for I in Items do
    Result.Push(I);
end;

procedure TArrayList<T>.Copy(out List: TArrayList<T>; N: Integer);
var
  I: Integer;
begin
  if N < 1 then
    N := Length
  else if N > Length then
    N := Length;
  List.Length := N;
  if N < 1 then
    Exit;
  for I := 0 to N - 1 do
    List.Items[I] := Items[I];
end;

procedure TArrayList<T>.CopyFast(out List: TArrayList<T>; N: Integer);
begin
  if N < 1 then
    N := Length
  else if N > Length then
    N := Length;
  List.Length := N;
  if N < 1 then
    Exit;
  System.Move(Items[0], List.Items[0], N * SizeOf(T));
end;

procedure TArrayList<T>.Reverse;
var
  Swap: T;
  I, J: Integer;
begin
  I := 0;
  J := Length;
  while I < J do
  begin
    Swap := Items[I];
    Items[I] := Items[J];
    Items[J] := Swap;
    Inc(I);
    Dec(J);
  end;
end;

function TArrayList<T>.Lo: Integer;
begin
  Result := Low(Items);
end;

function TArrayList<T>.Hi: Integer;
begin
  Result := High(Items);
end;

procedure TArrayList<T>.Move(OldIndex, NewIndex: Integer);
var
  I: T;
  J: Integer;
begin
  if OldIndex < NewIndex then
  begin
    Inc(OldIndex);
    for J := OldIndex to NewIndex do
    begin
      I := Items[J - 1];
      Items[J - 1] := Items[J];
      Items[J] := I;
    end
  end
  else if OldIndex > NewIndex then
  begin
    Dec(OldIndex);
    for J := OldIndex downto NewIndex do
    begin
       I := Items[J + 1];
       Items[J + 1] := Items[J];
       Items[J] := I;
    end;
  end;
end;

procedure TArrayList<T>.Exchange(A, B: Integer);
var
  Item: T;
begin
  if A <> B then
  begin
    Item := Items[A];
    Items[A] := Items[B];
    Items[B] := Item;
  end;
end;

procedure TArrayList<T>.Push(const Item: T);
var
  I: Integer;
begin
  I := Length;
  Length := I + 1;
  Items[I] := Item;
end;

procedure TArrayList<T>.PushRange(const Collection: array of T);
var
  I, J: Integer;
begin
  I := Length;
  J := High(Collection) - Low(Collection) + 1;
  if J < 1 then
    Exit;
  Length := I + J;
  for J := Low(Collection) to High(Collection) do
  begin
    Items[I] := Collection[J];
    Inc(I);
  end;
end;

function TArrayList<T>.Pop: T;
var
  I: Integer;
begin
  I := Length - 1;
  if I < 0 then
  begin
    Result := Default(T);
    Length := 0;
  end
  else
  begin
    Result := Items[I];
    Length := I;
  end;
end;

function TArrayList<T>.PopRandom: T;
var
  I: Integer;
begin
  I := Length;
  if I < 2 then
    Result := Pop
  else
  begin
    I := System.Random(I);
    Result := Items[I];
    Delete(I);
  end;
end;

function TArrayList<T>.Filter(Func: TFilterFunc<T>): TArrayList<T>;
var
  I, J: Integer;
begin
  Result.Items :=  nil;
  J := System.Length(Items);
  System.SetLength(Result.Items, J);
  J := 0;
  for I := 0 to System.Length(Items) - 1 do
    if Func(Items[I]) then
    begin
   Result.Items[J] := Items[I];
   Inc(J);
    end;
  System.SetLength(Result.Items, J);
end;

function TArrayList<T>.FirstOf(Func: TFilterFunc<T>): T;
var
  I: Integer;
begin
  for I := 0 to System.Length(Items) - 1 do
    if Func(Items[I]) then
   Exit(Items[I]);
  Result := Default(T);
end;

procedure TArrayList<T>.Delete(Index: Integer);
var
  I, J: Integer;
begin
  I := Length - 1;
  for J := Index + 1 to I do
    Items[J - 1] := Items[J];
  Length := I;
end;

procedure TArrayList<T>.Clear;
begin
  Length := 0;
end;

{ TMap<K, V> }

function TMap<K, V>.GetItem(const Key: K): V;
var
  I: Integer;
begin
  I := FKeys.IndexOf(Key);
  if I > -1 then
    Result := FValues.Items[I]
  else
    Result := Default(V);
end;

procedure TMap<K, V>.SetItem(const Key: K; const Value: V);
var
  I: Integer;
begin
  I := FKeys.IndexOf(Key);
  if I > -1 then
    FValues.Items[I] := Value
  else
  begin
    FKeys.Push(Key);
    FValues.Push(Value);
  end;
end;

function DefaultCompare8(const A, B: Byte): IntPtr;
begin
  Result := B - A;
end;

function DefaultCompare16(const A, B: Word): IntPtr;
begin
  Result := B - A;
end;

function DefaultCompare32(const A, B: LongInt): IntPtr;
begin
  Result := B - A;
end;

function DefaultCompare64(const A, B: LargeInt): IntPtr;
begin
  Result := B - A;
end;

function TArrayList<T>.CompareExists: Boolean;
begin
  Result := True;
  if Assigned(DefaultCompare) then
    Exit;
  case SizeOf(T) of
    8: DefaultCompare := TCompareFunc(DefaultCompare8);
    16: DefaultCompare := TCompareFunc(DefaultCompare16);
    32: DefaultCompare := TCompareFunc(DefaultCompare32);
    64: DefaultCompare := TCompareFunc(DefaultCompare64);
  else
    Result := False;
  end;
end;

procedure TArrayList<T>.QuickSort(Order: TSortingOrder; Compare: TCompare<T>; L, R: Integer);
var
  F, I, J, P: Integer;
begin
  repeat
    if Order = soDescend then
   F := -1
    else
   F := 1;
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
   while Compare(Items[I], Items[P]) * F < 0 do Inc(I);
   while Compare(Items[J], Items[P]) * F > 0 do Dec(J);
   if I <= J then
   begin
  Exchange(I, J);
  if P = I then
    P := J
  else if P = J then
    P := I;
  Inc(I);
  Dec(J);
   end;
    until I > J;
    if L < J then QuickSort(Order, Compare, L, J);
    L := I;
  until I >= R;
end;

procedure TArrayList<T>.Sort(Order: TSortingOrder = soAscend; Comparer: TCompare<T> = nil);
var
  I: Integer;
begin
  if Order = soNone then
    Exit;
  I := Length;
  if I < 2 then
    Exit;
  if Assigned(Comparer) then
    QuickSort(Order, Comparer, 0, I - 1)
  else if CompareExists then
    QuickSort(Order, DefaultCompare, 0, I - 1);
end;

function TArrayList<T>.IndexOf(const Item: T): Integer;
var
  I: Integer;
begin
  Result := -1;
  if (Length > 0) and CompareExists then
    for I := 0 to Length - 1 do
      if DefaultCompare(Item, Items[I]) = 0 then
        Exit(I);
end;

function TArrayList<T>.IndexOf(const Item: T; Comparer: TCompare<T>): Integer;
var
  I: Integer;
begin
  Result := -1;
  I := Length;
  if I > 0 then
    for I := Lo to Hi do
   if Comparer(Item, Items[I]) = 0 then
  Exit(I);
end;

function TArrayList<T>.Join(const Separator: string; Convert: TConvertString<T> = nil): string;
var
  I: Integer;
begin
  Result := '';
  if Length < 1 then
    Exit;
  if Assigned(Convert) then
  begin
    Result := Convert(First);
    for I := Low(Items) + 1 to High(Items) do
   Result := Result + Separator + Convert(Items[I]);
  end
  else if Assigned(DefaultConvertString) then
  begin
    Result := DefaultConvertString(First);
    for I := Low(Items) + 1 to High(Items) do
   Result := Result + Separator + DefaultConvertString(Items[I]);
  end;
end;

function TArrayList<T>.GetIsEmpty: Boolean;
begin
  Result := Length = 0;
end;

function TArrayList<T>.GetFirst: T;
begin
  if Length > 0 then
    Result := Items[0]
  else
    Result := Default(T);
end;

procedure TArrayList<T>.SetFirst(const Value: T);
begin
  if Length > 0 then
    Items[0] := Value;
end;

function TArrayList<T>.GetLast: T;
begin
  if Length > 0 then
    Result := Items[Length - 1]
  else
    Result := Default(T);
end;

procedure TArrayList<T>.SetLast(const Value: T);
begin
  if Length > 0 then
    Items[Length - 1] := Value;
end;

function TArrayList<T>.GetLength: Integer;
begin
  Result := System.Length(Items);
end;

procedure TArrayList<T>.SetLength(Value: Integer);
begin
  System.SetLength(Items, Value);
end;

function TArrayList<T>.GetData: Pointer;
begin
  Result := @Items[0];
end;

function TArrayList<T>.GetItem(Index: Integer): T;
begin
  Result := Items[Index];
end;

procedure TArrayList<T>.SetItem(Index: Integer; const Value: T);
begin
  Items[Index] := Value;
end;

class function TArrayList<T>.Convert: TArrayList<T>;
begin
  Result.Length := 0;
end;

function DefaultStringCompare(const A, B: string): IntPtr;
begin
  Result := StrCompare(A, B);
end;

function DefaultStringConvertString(const Item: string): string;
begin
  Result := Item;
end;

function DefaultWordCompare(const A, B: Word): IntPtr;
begin
  Result := B - A;
end;

function DefaultWordConvertString(const Item: Word): string;
begin
  Result := IntToStr(Item);
end;

function DefaultIntCompare(const A, B: Integer): IntPtr;
begin
  Result := B - A;
end;

function DefaultIntConvertString(const Item: Integer): string;
begin
  Result := IntToStr(Item);
end;

function DefaultInt64Compare(const A, B: Int64): IntPtr;
begin
  Result := B - A;
end;

function DefaultInt64ConvertString(const Item: Int64): string;
begin
  Result := IntToStr(Item);
end;

function DefaultFloatCompare(const A, B: Float): IntPtr;
begin
  if A < B then
    Result := -1
  else if A > B then
    Result := 1
  else
    Result := 0;
end;

function DefaultFloatConvertString(const Item: Float): string;
begin
  Result := FloatToStr(Item);
end;

function DefaultObjectCompare(const A, B: TObject): IntPtr;
begin
  Result := IntPtr(A) - IntPtr(B);
end;

function DefaultInterfaceCompare(const A, B: IInterface): IntPtr;
begin
  Result := IntPtr(A) - IntPtr(B);
end;

{ TListEnumerator<T> }

procedure TListEnumerator<T>.Create(Items: TArrayList<T>; Count: Integer = -1);
begin
  FItems := Items;
  FPosition := -1;
  if Count < 0 then
    FCount := Items.Length
  else
    FCount := Count;
end;

function TListEnumerator<T>.GetCurrent: T;
begin
  Result := FItems[FPosition];
end;

function TListEnumerator<T>.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FCount;
end;

procedure TListEnumerator<T>.Reset;
begin
  FPosition := -1;
end;

function TListEnumerator<T>.GetCount: Integer;
begin
  Result := FCount;
end;

function TListEnumerator<T>.GetItem(Index: Integer): T;
begin
  Result := FItems[Index];
end;

{ TList<TItem> }

function TList<TItem>.GetEnumerator: TListItemEnumerator;
begin
  Result.Create(FItems, FCount);
end;

procedure TList<TItem>.AddItem(constref Item: ItemType);
begin
  Grow(FCount + 1);
  FItems[FCount] := Item;
  Inc(FCount);
end;

procedure TList<TItem>.DeleteItem(var Item: ItemType);
begin
  Item := default(ItemType);
end;

function TList<TItem>.RequiresDelete: Boolean;
begin
  Result := False;
end;

function TList<TItem>.Find(Comparer: TListCompare; const Item: ItemType): Integer;
var
  I: Integer;
begin
  for I := 0 to FCount - 1 do
    if Comparer(Item, FItems[I]) = 0 then
   Exit(I);
  Result := -1;
end;

procedure TList<TItem>.Grow(MinCapacity: Integer);
const
  ActualMinCapacity = 10;
begin
  if MinCapacity > FCapacity then
  begin
    if MinCapacity < ActualMinCapacity then
   MinCapacity := ActualMinCapacity;
    FCapacity := MinCapacity + FCapacity div 4;
    FItems.Length := FCapacity;
  end;
end;

destructor TList<TItem>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TList<TItem>.CheckBounds(const Method: string; Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    raise ERangeError.CreateFmt(SRangeMethodError, [ClassName, Method]);
end;

procedure TList<TItem>.Clear;
var
  I: Integer;
begin
  if RequiresDelete then
    for I := 0 to FCount - 1 do
   DeleteItem(FItems.Items[I]);
  FCount := 0;
  Compact;
end;

procedure TList<TItem>.Compact;
const
  ActualMinCapacity = 10;
var
  I: Integer;
begin
  I := FCount;
  if I < ActualMinCapacity then
    I := ActualMinCapacity;
  if FCount = 0 then
    I := 0;
  if I < FCapacity then
  begin
    FCapacity := I;
    FItems.Length := FCapacity;
  end;
end;

procedure TList<TItem>.QuickSort(Compare: TListCompare; L, R: Integer);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
   while Compare(Item[I], Item[P]) < 0 do Inc(I);
   while Compare(Item[J], Item[P]) > 0 do Dec(J);
   if I <= J then
   begin
  Exchange(I, J);
  if P = I then
    P := J
  else if P = J then
    P := I;
  Inc(I);
  Dec(J);
   end;
    until I > J;
    if L < J then QuickSort(Compare, L, J);
    L := I;
  until I >= R;
end;

procedure TList<TItem>.Sort(Compare: TListCompare = nil);
begin
  if Count < 2 then
    Exit;
  if Assigned(Compare) then
    QuickSort(Compare, 0, Count - 1)
  else if Assigned(TArrayList<ItemType>.DefaultCompare) then
    QuickSort(TArrayList<ItemType>.DefaultCompare, 0, Count - 1);
end;

procedure TList<TItem>.Add(const Item: ItemType);
begin
  AddItem(Item);
end;

procedure TList<TItem>.Delete(Index: Integer);
var
  I: Integer;
begin
  CheckBounds('Delete', Index);
  if RequiresDelete then
    DeleteItem(FItems.Items[Index]);
  for I := Index + 1 to FCount - 1 do
    FItems[I - 1] := FItems[I];
  Dec(FCount);
  FItems[FCount] := default(ItemType);
end;

procedure TList<TItem>.Remove(const Item: ItemType);
var
  I: Integer;
begin
  I := IndexOf(Item);
  if I > -1 then
    Delete(I)
end;

procedure TList<TItem>.Move(OldIndex, NewIndex: Integer);
var
  I: ItemType;
  J: Integer;
begin
  CheckBounds('Move', OldIndex);
  CheckBounds('Move', NewIndex);
  {TODO: Consider using System.Move}
  if OldIndex < NewIndex then
  begin
    Inc(OldIndex);
    for J := OldIndex to NewIndex do
    begin
      I := FItems[J - 1];
      FItems[J - 1] := FItems[J];
      FItems[J] := I;
    end
  end
  else if OldIndex > NewIndex then
  begin
    Dec(OldIndex);
    for J := OldIndex downto NewIndex do
    begin
       I := FItems[J + 1];
       FItems[J + 1] :=  FItems[J];
       FItems[J] := I;
    end;
  end;
end;

procedure TList<TItem>.Exchange(A, B: Integer);
var
  I: ItemType;
begin
  CheckBounds('Exchange', A);
  CheckBounds('Exchange', B);
  if A <> B then
  begin
    I := FItems[A];
    FItems[A] := FItems[B];
    FItems[B] := I;
  end;
end;

function TList<TItem>.IndexOf(const Item: ItemType): Integer;
var
  I: Integer;
begin
  if FItems.CompareExists then
    for I := 0 to Count - 1 do
      if FItems.DefaultCompare(Item, FItems.Items[I]) = 0 then
        Exit(I);
  Result := -1;
end;

function TList<TItem>.GetHead: PItemType;
begin
  if FCount > 0 then
    Result := PItemType(@FItems.Items[0])
  else
    Result := nil;
end;

function TList<TItem>.GetFirst: ItemType;
begin
  if FCount > 0 then
    Result := FItems[0]
  else
    Result := default(ItemType);
end;

function TList<TItem>.GetLast: ItemType;
begin
  if FCount > 0 then
    Result := FItems[FCount - 1]
  else
    Result := default(ItemType);
end;

function TList<TItem>.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TList<TItem>.SetCapacity(Value: Integer);
begin
  if Value > FCapacity then
    Grow(Value);
end;

function TList<TItem>.GetItem(Index: Integer): ItemType;
begin
  CheckBounds('GetItem', Index);
  Result := FItems[Index];
end;

procedure TList<TItem>.SetItem(Index: Integer; const Value: ItemType);
begin
  CheckBounds('SetItem', Index);
  if RequiresDelete then
    DeleteItem(FItems.Items[Index]);
  FItems[Index] := Value;
end;

{ TIndexedList<TItem> }

procedure TIndexedList<TItem>.AddItem(constref Item: ItemType);
var
  I: Integer;
begin
  if FDuplicates = duplicateAllow then
    inherited AddItem(Item)
  else
  begin
    I := IndexOf(Item);
    if I < 0 then
   inherited AddItem(Item)
    else if FDuplicates = duplicateError then
   raise EListError.Create(SListDuplicateError);
  end;
end;

function TIndexedList<TItem>.Contains(const Item: ItemType): Boolean;
begin
  Result := IndexOf(Item) > -1;
end;

function TIndexedList<TItem>.Remove(const Item: ItemType): Boolean;
var
  I: Integer;
begin
  I := IndexOf(Item);
  Result := I > -1;
  if Result then
    Self.Delete(I);
end;

{ TObjectList<TItem> }

constructor TObjectList<TItem>.Create(OwnsObjects: Boolean);
begin
  inherited Create;
  FOwnsObjects := OwnsObjects;
  if FOwnsObjects then
    Duplicates := duplicateError;
end;

procedure TObjectList<TItem>.AddItem(constref Item: ItemType);
var
  CanAdd: Boolean;
begin
  if FOwnsObjects then
    CanAdd := IndexOf(Item) < 0
  else
    CanAdd := True;
  if CanAdd then
    inherited AddItem(Item);
end;

procedure TObjectList<TItem>.DeleteItem(var Item: ItemType);
begin
  if FOwnsObjects then
    Item.Free;
  Item := TObject(nil);
end;

function TObjectList<TItem>.RequiresDelete: Boolean;
begin
  Result := FOwnsObjects;
end;

function FindObject(const A, B: TObject): Integer;
begin
  Result := PtrInt(A) - PtrInt(B);
end;

function FindInterface(constref A, B: IInterface): Integer;
begin
  Result := PtrInt(A) - PtrInt(B);
end;

function TObjectList<TItem>.IndexOf(const Item: ItemType): Integer;
begin
  Result := Find(TCompare<Titem>(@FindObject), Item);
end;

{ TDictionary<K, V>.TKeyValue }

constructor TDictionary<K, V>.TKeyValue.Create(const Key: K);
begin
  inherited Create;
  FKey := Key;
end;

{ TDictionary<K, V> }

function TDictionary<K, V>.GetEnumerator: TDictionaryEnumerator;
begin
  Result := FList.GetEnumerator;
end;

constructor TDictionary<K, V>.Create;
begin
  inherited Create;
  FList := TList<TKeyValue>.Create;
end;

destructor TDictionary<K, V>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDictionary<K, V>.AddKeyValue(KeyValue: TKeyValue);
begin
  FList.Add(KeyValue);
end;

function TDictionary<K, V>.CreateKeyValue(const Key: K): TKeyValue;
begin
  Result := TKeyValue.Create(Key);
end;

procedure TDictionary<K, V>.DestroyKeyValue(KeyValue: TKeyValue);
begin
  KeyValue.Free;
end;

procedure TDictionary<K, V>.ChangeKeyValue(KeyValue: TKeyValue; Value: V);
begin
  KeyValue.Value := Value;
end;

function TDictionary<K, V>.DefaultValue: V;
begin
  Result := default(V);
end;

function TDictionary<K, V>.KeyEquals(const A, B: K): Boolean;
begin
  if Assigned(FComparer) then
    Result := FComparer(A, B) = 0
  else
    Result := A = B;
end;

procedure TDictionary<K, V>.Remove(const Key: K);
var
  KeyValue: TKeyValue;
  C: Integer;
  I: Integer;
begin
  C := FList.Count;
  I := 0;
  repeat
    if I = C then
   Exit;
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
    begin
   DestroyKeyValue(KeyValue);
   FList.Delete(I);
   Exit;
    end;
    Inc(I);
  until False;
end;

procedure TDictionary<K, V>.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    DestroyKeyValue(FList[I]);
  FList.Clear;
end;

function TDictionary<K, V>.KeyExists(const Key: K): Boolean;
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  Result := True;
  for I := 0 to FList.Count - 1 do
  begin
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
   Exit;
  end;
  Result := False;
end;

function TDictionary<K, V>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDictionary<K, V>.GetItem(Index: Integer): TKeyValue;
begin
  Result := FList[Index];
end;

function TDictionary<K, V>.GetKey(Index: Integer): K;
begin
  Result := FList[Index].Key;
end;

function TDictionary<K, V>.GetValue(const Key: K): V;
var
  KeyValue: TKeyValue;
  C: Integer;
  I: Integer;
begin
  C := FList.Count;
  I := 0;
  repeat
    if I = C then
    begin
   if AutoKey then
   begin
  KeyValue := CreateKeyValue(Key);
  if KeyValue <> nil then
  begin
    AddKeyValue(KeyValue);
    Result := KeyValue.Value;
  end;
   end
   else
  Result := DefaultValue;
   Exit;
    end;
    KeyValue := FList[I];
    if KeyEquals(KeyValue.Key, Key) then
   Exit(KeyValue.Value);
    Inc(I);
  until False;
end;

procedure TDictionary<K, V>.SetValue(const Key: K; const Value: V);
var
  KeyValue: TKeyValue;
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
  begin
    KeyValue := TKeyValue(FList[I]);
    if KeyEquals(KeyValue.Key, Key) then
    begin
   ChangeKeyValue(KeyValue, Value);
   Exit;
    end;
  end;
  KeyValue := CreateKeyValue(Key);
  if KeyValue <> nil then
  begin
    KeyValue.FValue := Value;
    AddKeyValue(KeyValue);
  end;
end;

{ TCollection<TItem> }

function TCollection<TItem>.GetEnumerator: TCollectionEnumerator;
begin
  Result := Items.GetEnumerator;
end;

function TCollection<TItem>.GetCount: Integer;
begin
  Result := Items.Length;
end;

function TCollection<TItem>.GetItem(Index: Integer): TItem;
begin
  Result := Items[Index];
end;

{ TOwnerCollection<T> }

destructor TOwnerCollection<TItem>.Destroy;
var
  I: Integer;
begin
  for I := 0 to Items.Length - 1 do
    Items[I].Free;
  inherited Destroy;
end;

{ TNamedList<T> }

procedure TNamedList<T>.AddName(const Name: string; Item: T);
begin
  (Item as INamed).Name := Name;
  Add(Item);
end;

procedure TNamedList<T>.RemoveName(const Name: string);
var
  Item: T;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Item := Self[I];
     if (Item as INamed).Name = Name then
     begin
       Delete(I);
       Exit;
     end;
  end;
end;

function TNamedList<T>.FindName(const Name: string): T;
var
  Item: T;
begin
  for Item in Self do
    if (Item as INamed).Name = Name then
      Exit(Item);
  Result := nil;
end;
{$endregion}

{$region string routines}
function LineBreak: string;
begin
  Result := LineBreakStyles[DefaultTextLineBreakStyle];
end;

function StrUpper(const S: string): string;
begin
  Result := UpCase(S);
end;

function StrLower(const S: string): string;
begin
  Result := LowerCase(S);
end;

function StrBufCompareI(A, B: PChar): Integer;
const
  CharA = Ord('A');
  CharZ = Ord('Z');
  CharDelta = Ord('a') - Ord('A');
var
  B1: PByte absolute A;
  B2: PByte absolute B;
  C1, C2: Byte;
begin
  repeat
    C1 := B1^;
    C2 := B2^;
    if (C1 >= CharA) and (C1 <= CharZ) then
   Inc(C1, CharDelta);
    if (C2 >= CharA) and (C2 <= CharZ) then
   Inc(C2, CharDelta);
    Inc(B1);
    Inc(B2);
  until (C1 <> C2) or (C1 = 0);
  if C1 < C2 then
    Exit(-1);
  if C1 > C2 then
    Exit(1);
  Exit(0);
end;

function StrBufCompare(A, B: PChar): Integer;
var
  B1: PByte absolute A;
  B2: PByte absolute B;
  C1, C2: Byte;
begin
  repeat
    C1 := B1^;
    C2 := B2^;
    Inc(B1);
    Inc(B2);
  until (C1 <> C2) or (C1 = 0);
  if C1 < C2 then
    Exit(-1);
  if C1 > C2 then
    Exit(1);
  Exit(0);
end;

function StrCompare(const A, B: string; IgnoreCase: Boolean = False): Integer;
begin
  if (Length(A) = 0) and (Length(B) = 0) then
    Exit(0);
  if Length(A) = 0 then
    Exit(-1);
  if Length(B) = 0 then
    Exit(1);
  if IgnoreCase then
    Result := StrBufCompareI(PChar(A), PChar(B))
  else
    Result := StrBufCompare(PChar(A), PChar(B));
end;

function StrCopy(const S: string; Start: Integer; Len: Integer = 0): string;
  var
  A, B: PChar;
  I: Integer;
begin
  Result := '';
  if S = '' then
    Exit;
  if Start < 1 then
    Exit;
  I := Length(S);
  if Start > I then
    Exit;
  if Len < 1 then
    Len := Length(S);
  Dec(Start);
  if Start + Len > I then
    Len := I - Start;
  Setlength(Result, Len);
  A := PChar(S);
  B := PChar(Result);
  Inc(A, Start);
  System.Move(A^, B^, Len);
end;

function StrCopyData(P: Pointer; Len: Integer): string;
begin
  if Len < 1 then
    Exit('');
  SetLength(Result, Len);
  System.Move(P^, PChar(Result)^, Len);
end;

function StrInsert(const S, SubStr: string; Position: Integer): string;
begin
  if Position < 1 then
    Position := 1
  else if Position > Length(S) then
    Position := Length(S);
  if Position = 1 then
    Exit(SubStr + S);
  if Position = Length(S) then
    Exit(S + SubStr);
  Result := StrCopy(S, 1, Position - 1) + SubStr + StrCopy(S, Position);
end;

function StrFindBuffer(S, SubStr: PChar; SLen, SubStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if (SLen = 0) or (SubStrLen = 0) then
    Exit;
  Dec(S);
  Dec(SubStr);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(SubStr[I]);
    Lookup[B] := SubStrLen - I;
  end;
  Last := SubStr[SubStrLen];
  I := SubStrLen;
  while I <= SLen do
  begin
    Current := S[I];
    if Current = Last then
    begin
      J := I - SubStrLen;
      K := 1;
      while K < SubStrLen do
      begin
        if SubStr[K] <> S[J + K] then
          Break;
        Inc(K);
      end;
      if K = SubStrLen then
      begin
        Result := J + 1;
        Exit;
      end;
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end
    else
    begin
      B := Ord(Current);
      Inc(I, Lookup[B]);
    end;
  end;
end;

function StrFindBufferI(S, SubStr: PChar; SLen, SubStrLen: Integer): Integer;
var
  Current, Last: Char;
  Lookup: array[Low(Byte)..High(Byte)] of Integer;
  B: Byte;
  I, J, K: Integer;
begin
  Result := 0;
  if (SubStrLen = 0) or (SLen = 0) then
    Exit;
  Dec(SubStr);
  Dec(S);
  for I := Low(Lookup) to High(Lookup) do
    Lookup[I] := SubStrLen;
  for I := 1 to SubStrLen - 1 do
  begin
    B := Ord(UpCase(SubStr[I]));
    Lookup[B] := SubStrLen - I;
  end;
  Last := UpCase(SubStr[SubStrLen]);
  I := SubStrLen;
  while I <= SLen do
  begin
    Current := UpCase(S[I]);
    if Current = Last then
    begin
   J := I - SubStrLen;
   K := 1;
   while K < SubStrLen do
   begin
  if UpCase(SubStr[K]) <> UpCase(S[J + K]) then
    Break;
  Inc(K);
   end;
   if K = SubStrLen then
   begin
  Result := J + 1;
  Exit;
   end;
   B := Ord(Current);
   Inc(I, Lookup[B]);
    end
    else
    begin
   B := Ord(Current);
   Inc(I, Lookup[B]);
    end;
  end;
end;

function StrTrim(const S: string): string;
const
  WhiteSpace = [#0..' '];
var
  Len, I: Integer;
begin
  Len := Length(S);
  while (Len > 0) and (S[Len] in WhiteSpace) do
   Dec(Len);
  I := 1;
  while ( I <= Len) and (S[I] in WhiteSpace) do
    Inc(I);
  Result := Copy(S, I, 1 + Len - I);
end;

function StrFind(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
begin
  if (S = '') or (SubStr = '') then
    Exit(0);
  if IgnoreCase then
    Result := StrFindBufferI(PChar(S), PChar(SubStr), Length(S), Length(SubStr))
  else
    Result := StrFindBuffer(PChar(S), PChar(SubStr), Length(S), Length(SubStr));
end;

function StrFind(const S, SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer;
var
  P: PChar;
  I: Integer;
begin
  if (S = '') or (SubStr = '') then
    Exit(0);
  P := PChar(S);
  I := Length(S);
  if (Start < 1) or (Start > I) then
  begin
    Result := 0;
    Exit;
  end;
  Dec(Start);
  Inc(P, Start);
  Dec(I, Start);
  if IgnoreCase then
    Result := StrFindBufferI(P, PChar(SubStr), I, Length(SubStr))
  else
    Result := StrFindBuffer(P, PChar(SubStr), I, Length(SubStr));
  if Result > 0 then
    Inc(Result, Start);
end;

function StrFindCount(const S, SubStr: string; IgnoreCase: Boolean = False): Integer;
var
  Start, Index: Integer;
begin
  Result := 0;
  Start := 1;
  repeat
    Index := StrFind(S, SubStr, Start, IgnoreCase);
    if Index > 0 then
    begin
   Inc(Result);
   Start := Index + Length(SubStr);
    end;
  until Index = 0;
end;

function StrFindIndex(const S, SubStr: string; IgnoreCase: Boolean = False): IntArray;
var
  Start, Index: Integer;
begin
  Result.Length := StrFindCount(S, SubStr, IgnoreCase);
  Start := 1;
  Index := 0;
  while Index < Result.Length do
  begin
    Start := StrFind(S, SubStr, Start, IgnoreCase);
    Result[Index] := Start;
    Inc(Start, Length(SubStr));
    Inc(Index);
  end;
end;

function StrReplace(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  PosIndex: IntArray;
  FindIndex, FindLen, OldIndex, OldLen, NewIndex, NewLen, I: Integer;
begin
  PosIndex := StrFindIndex(S, OldPattern, IgnoreCase);
  FindLen := PosIndex.Length;
  if FindLen = 0 then
  begin
    Result := S;
    Exit;
  end;
  OldLen := S.Length;
  NewLen := OldLen + NewPattern.Length * FindLen - OldPattern.Length * FindLen;
  SetLength(Result, NewLen);
  OldIndex := 1;
  NewIndex := 1;
  FindIndex := 0;
  while OldIndex <= OldLen do
  begin
    if (FindIndex < FindLen) and (OldIndex = PosIndex[FindIndex]) then
    begin
   Inc(OldIndex, OldPattern.Length);
   for I := 0 to NewPattern.Length - 1 do
  Result[NewIndex + I] := NewPattern[I + 1];
   Inc(NewIndex, NewPattern.Length);
   Inc(FindIndex);
    end
    else
    begin
   Result[NewIndex] := S[OldIndex];
   Inc(OldIndex);
   Inc(NewIndex);
    end;
  end;
end;

function StrReplaceOne(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  I: Integer;
begin
  I := StrFind(S, OldPattern, IgnoreCase);
  if I > 0 then
    Result := Copy(S, 1, I - 1) + NewPattern + Copy(S, I + Length(OldPattern), Length(S))
  else
    Result := S;
end;

function StrReplaceAfter(const S, OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
var
  I: Integer;
begin
  I := StrFind(S, OldPattern, IgnoreCase);
  if I > 0 then
    Result := Copy(S, 1, I - 1) + NewPattern
  else
    Result := S;
end;

function StrEquals(const S: string; Value: string): Boolean;
begin
  Result := StrCompare(S, Value, True) = 0;
end;

function StrEquals(const S: string; const Values: array of string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Values) to High(Values) do
    if StrCompare(S, Values[I], True) = 0 then
    begin
   Result := True;
   Break;
    end;
end;

function StrIndex(const S: string; const Values: array of string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(Values) to High(Values) do
    if S = Values[I] then
    begin
   Result := I;
   Break;
    end;
end;

function StrSplit(const S, Separator: string): StringArray;
var
  Splits: IntArray;
  Pos: Integer;
  I: Integer;
begin
  if S.Length < 1 then
    Exit;
  if Separator.Length < 1 then
    Exit;
  if StrFind(S, Separator) < 1 then
  begin
    Result.Length := 1;
    Result[0] := S;
    Exit;
  end;
  Splits := StrFindIndex(S, Separator);
  Result.Length := Splits.Length + 1;
  Pos := 1;
  for I := Splits.Lo to Splits.Hi do
  begin
    Result[I] := Copy(S, Pos, Splits[I] - Pos);
    Pos := Splits[I] + Separator.Length;
  end;
  Result.Items[Splits.Length] := Copy(S, Pos, S.Length);
end;

function StrSplitInt(const S, Separator: string): IntArray;
var
  Data: StringArray;
  I: Integer;
begin
  Data := StrSplit(S, Separator);
  Result.Length := Data.Length;
  try
    for I := Data.Lo to Data.Hi do
   Result[I] := StrToInt(Data[I]);
  except
    Result.Clear;
  end;
end;

function StrSplitInt64(const S, Separator: string): Int64Array;
var
  Data: StringArray;
  I: Integer;
begin
  Data := StrSplit(S, Separator);
  Result.Length := Data.Length;
  try
    for I := Data.Lo to Data.Hi do
   Result[I] := StrToInt64(Data[I]);
  except
    Result.Clear;
  end;
end;

function StrJoin(const A: StringArray; const Separator: string): string;
var
  I: Integer;
begin
  Result := '';
  if A.Length < 1 then
    Exit;
  Result := A.First;
  for I := A.Lo + 1 to A.Hi do
    Result := Result + Separator + A[I];
end;

function StrJoinInt(const A: IntArray; const Separator: string): string;
var
  I: Integer;
begin
  Result := '';
  if A.Length < 1 then
    Exit;
  Result := IntToStr(A.First);
  for I := A.Lo + 1 to A.Hi do
    Result := Result + Separator + IntToStr(A[I]);
end;

function StrFirstOf(const S, Separator: string): string;
var
  I: Integer;
begin
  I := StrFind(S, Separator);
  if I > 0 then
    if I = 1 then
   Result := ''
    else
   Result := StrCopy(S, 1, I - 1)
  else
    Result := S;
end;

function StrSecondOf(const S, Separator: string): string;
var
  I: Integer;
begin
  I := StrFind(S, Separator);
  if I > 0 then
    Result := StrCopy(S, I + Length(Separator))
  else
    Result := '';
end;

function StrLastOf(const S, Separator: string): string;
var
  A: StringArray;
begin
  A := StrSplit(S, Separator);
  if A.Length > 0 then
    Result := A.Last
  else
    Result := '';
end;

function StrContains(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  if Length(SubStr) > Length(S) then
    Exit(False);
  Result := StrFind(S, SubStr, IgnoreCase) > 0;
end;

function StrBeginsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
var
  C: string;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  if Length(SubStr) > Length(S) then
    Exit(False);
  C := StrCopy(S, 1, Length(SubStr));
  Result := StrCompare(C, SubStr, IgnoreCase) = 0;
end;

function StrEndsWith(const S, SubStr: string; IgnoreCase: Boolean = False): Boolean;
var
  C: string;
begin
  if Length(S) < 1 then
    Exit(False);
  if Length(SubStr) < 1 then
    Exit(False);
  if Length(SubStr) > Length(S) then
    Exit(False);
  C := StrCopy(S, Length(S) - Length(SubStr) + 1, Length(SubStr));
  Result := StrCompare(C, SubStr, IgnoreCase) = 0;
end;

function StrHash(const S: string): LargeWord;
var
  B: PByte;
  I: Integer;
begin
  I := Length(S);
  Result := 0;
  if I = 0 then
    Exit;
  Result := 5381;
  B := PByte(S);
  while I > 0 do
  begin
    Result := (Result shl 5) + Result + B^;
    Inc(B);
    Dec(I);
  end;
end;

function StrOf(C: Char; Len: Integer): string;
var
  I: Integer;
begin
  if Len < 1 then
    Exit;
  Result := '';
  SetLength(Result, Len);
  for I := 1 to Len do
    Result[I] := C;
end;

function StrPadLeft(const S: string; C: Char; Len: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := Length(S);
  if I < 1 then
    Exit;
  if Len < 1 then
    Exit;
  if I > Len then
  begin
    Result := Copy(S, 1, Len);
    Exit;
  end;
  Result := S + StrOf(C, Len - I);
end;

function StrPadRight(const S: string; C: Char; Len: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := Length(S);
  if I > Len then
  begin
    Result := Copy(S, Len - I, Len);
    Exit;
  end;
  Result := StrOf(C,  Len - I) + S;
end;

function StrQuote(const S: string): string;
begin
  if StrContains(S, ' ' ) then
    Result := '"' + StrReplace(S, '"', '''') + '"'
  else
    Result := S;
end;

function IsAlpha(C: Char): Boolean;
begin
  Result := (C >= 'A') and (C <= 'Z');
  if Result then Exit;
  Result := (C >= 'a') and (C <= 'z');
end;

function IsUnderscore(C: Char): Boolean;
begin
  Result := C = '_';
end;

function IsNumeric(C: Char): Boolean;
begin
  Result := (C >= '0') and (C <= '9');
end;

function StrIsBlank(const S: string): Boolean;
var
  I: Integer;
begin
  for I := 1 to Length(S) do
    if S[I] > ' ' then
   Exit(False);
  Result := True;
end;

function StrIsIdent(const S: string): Boolean;
var
  AlphaFound: Boolean;
  C: Char;
  I: Integer;
begin
  Result := False;
  if Length(S) < 1 then
    Exit;
  C := S[1];
  AlphaFound := IsAlpha(C);
  if (not AlphaFound) and (not IsUnderscore(C)) then
    Exit;
  for I := 2 to Length(S) do
  begin
    C := S[I];
    AlphaFound := AlphaFound or IsAlpha(C);
    if IsAlpha(C) or IsUnderscore(C) or IsNumeric(C) then
   Continue;
    Exit;
  end;
  Result := AlphaFound;
end;

function StrIsAttr(const S: string): Boolean;
begin
  Result := False;
  if Length(S) < 2 then
    Exit;
  if S[1] <> '@' then
    Exit;
  Result := StrIsIdent(Copy(S, 2, Length(S) - 1));
end;

function StrLineBreakStyle(const S: string): TTextLineBreakStyle;
var
  Count: array[TTextLineBreakStyle] of Integer;
  I: TTextLineBreakStyle;
begin
  for I := Low(Count) to High(Count) do
    Count[I] := StrFindCount(S, LineBreakStyles[I]);
  Result := DefaultTextLineBreakStyle;
  for I := Low(Count) to High(Count) do
    if Count[I] > Count[Result] then
   Result := I;
end;

function StrAdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
var
  Source, Dest: PChar;
  DestLen: Integer;
  I, J, L: Longint;
begin
  Source := Pointer(S);
  L := Length(S);
  DestLen := L;
  I := 1;
  while I <= L do
  begin
    case S[I] of
   #10:
  if Style = tlbsCRLF then Inc(DestLen);
   #13:
  if Style = tlbsCRLF then
    if (I < L) and (S[I+1] = #10) then
   Inc(I)
    else
   Inc(DestLen)
    else if (I < L) and (S[I + 1] = #10) then
   Dec(DestLen);
    end;
    Inc(I);
  end;
  if DestLen = L then
    Result := S
  else
  begin
    SetLength(Result, DestLen);
    FillChar(Result[1], DestLen, 0);
    Dest := Pointer(Result);
    J := 0;
    I := 0;
    while I < L do
   case Source[I] of
  #10:
    begin
   if Style=tlbsCRLF then
   begin
    Dest[J] := #13;
    Inc(J);
   end;
   Dest[J] := #10;
   Inc(J);
   Inc(I);
    end;
    #13:
      begin
        if Style = tlbsCRLF then
        begin
          Dest[J] := #13;
          Inc(J);
        end;
        Dest[J] := #10;
        Inc(J);
        Inc(I);
        if Source[I] = #10 then
          Inc(I);
      end;
      else
        Dest[J] := Source[I];
      Inc(J);
      Inc(I);
    end;
  end;
end;

function StrAdjustLineBreaks(const S: string): string;
begin
  Result := StrAdjustLineBreaks(S, DefaultTextLineBreakStyle);
end;

function StrToWide(const S: string): WideString;
var
  I: Integer;
begin
  I := Length(S);
  if I < 1 then
    Exit('');
  SetLength(Result, I);
  StringToWideChar(S, PWideChar(Result), I + 1);
end;

function WideToStr(const S: WideString): string;
begin
  if Length(S) < 1 then
    Exit('');
  WideCharToStrVar(PWideChar(S), Result);
end;

function SwitchExists(const Switch: string): Boolean;
begin
  Result := SwitchIndex(Switch) > 0;
end;

function SwitchIndex(const Switch: string): Integer;
var
  S: string;
  I: Integer;
begin
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if S = SwitchChar + Switch then
   Exit(I)
  end;
  Result := -1;
end;

function SwitchValue(const Switch: string): string;
var
  F: Boolean;
  S: string;
  I: Integer;
begin
  F := False;
  for I := 1 to ParamCount do
  begin
    S := ParamStr(I);
    if F then
   Exit(S);
    if S = SwitchChar + Switch then
   F := True;
  end;
  Result := '';
end;

function IntToStr(Value: Integer): string;
begin
  Str(Value, Result);
end;

function StrToInt(const S: string): Integer;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    raise EConvertError.CreateFmt(SConvertError, ['string', 'Integer']);
end;

function StrToIntDef(const S: string; Default: Integer): Integer;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    Result := Default;
end;

function Int64ToStr(Value: Int64): string;
begin
  Str(Value, Result);
end;

function StrToInt64(const S: string): Int64;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    raise EConvertError.CreateFmt(SConvertError, ['string', 'Int64']);
end;

function StrToInt64Def(const S: string; Default: Int64): Int64;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    Result := Default;
end;

function FloatToStr(Value: Extended): string;
const
  Epsilon = 0.0001;
var
  E: Extended;
  I: Integer;
begin
  E := Value - Trunc(Value);
  I := 0;
  while E > Epsilon do
  begin
    E := E * 10;
    E := E - Trunc(E);
    Inc(I);
  end;
  Str(Value:0:I, Result);
end;

function FloatToStr(Value: Extended; Decimals: Integer): string;
begin
  Str(Value:0:Decimals, Result);
end;

function StrToFloat(const S: string): Extended;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    raise EConvertError.CreateFmt(SConvertError, ['string', 'Float']);
end;

function StrToFloatDef(const S: string; Default: Extended): Extended;
var
  Code: Integer;
begin
  Val(S, Result, Code);
  if Code > 0 then
    Result := Default;
end;

function StrFormat(const S: string; Args: array of const): string;

  function ExtractString(const V: TVarRec): string;
  const
    BoolStr: array[Boolean] of string = ('no', 'yes');
  begin
    case V.VType of
   vtChar: Result := V.VChar;
   vtString: Result := V.VString^;
   vtPChar: Result := V.VPChar;
   {vtWideChar: Result := V.VWideChar;
   vtPWideChar: Result := V.VPWideChar^;}
   vtAnsiString: Result := PChar(V.VAnsiString);
   vtWideString: Result := PWideChar(V.VWideString);
   vtBoolean: Result := BoolStr[V.VBoolean];
    else
   raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

  function ExtractInteger(const V: TVarRec): string;
  begin
    case V.VType of
   vtInteger: Result := IntToStr(V.VInteger);
   vtInt64: Result := IntToStr(V.VInt64^);
   vtQWord: Result := IntToStr(V.VQWord^);
    else
   raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

  function ExtractFloat(const V: TVarRec): string;
  begin
    case V.VType of
   vtInteger: Result := FloatToStr(V.VInteger);
   vtExtended: Result := FloatToStr(V.VExtended^);
   vtCurrency: Result := FloatToStr(V.VCurrency^);
    else
   raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

  function ExtractFloatDecimal(const V: TVarRec; Decimals: Integer): string;
  begin
    case V.VType of
   vtInteger: Result := FloatToStr(V.VInteger, Decimals);
   vtExtended: Result := FloatToStr(V.VExtended^, Decimals);
   vtCurrency: Result := FloatToStr(V.VCurrency^, Decimals);
    else
   raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

  function ExtractObject(const V: TVarRec): string;
  begin
    case V.VType of
      vtObject: Result := V.VObject.ToString;
    else
      raise EVariantError.Create(SVarInvalidOp);
    end;
  end;

var
  Splits: IntArray;
  Item: string;
  Index, I, J: Integer;
  FormatStrings: StringArray;
begin
  Splits := nil;
  Result := S;
  if (S = '') or (StrFind(S, '%') < 1) then
    Exit;
  FormatStrings.Length := 13;
  FormatStrings[0] := '%s';
  FormatStrings[1] := '%d';
  FormatStrings[2] := '%f';
  FormatStrings[3] := '%c';
  for I := 4 to FormatStrings.Hi do
    FormatStrings[I] := '%.' + IntToStr(I - 4) + 'f';
  Index := 0;
  Splits := StrFindIndex(S, '%');
  for I := Splits.Lo to Splits.Hi do
  begin
    Item := StrCopy(S, Splits[I], 2);
    if not StrEquals(Item, ['%s', '%d', '%f']) then
   Item := StrCopy(S, Splits[I], 4);
    J := StrIndex(Item, FormatStrings.Items);
    if J < 0 then
   Continue;
    case J of
   0: Result := StrReplaceOne(Result, Item, ExtractString(Args[Index]));
   1: Result := StrReplaceOne(Result, Item, ExtractInteger(Args[Index]));
   2: Result := StrReplaceOne(Result, Item, ExtractFloat(Args[Index]));
   3..11: Result := StrReplaceOne(Result, Item, ExtractFloatDecimal(Args[Index], J - 4));
    end;
    Inc(Index);
  end;
end;
{$endregion}

{$region helpers}
{ StringHelper }

function StringHelper.ToString: string;
begin
  Result := Self;
end;

procedure StringHelper.Unique;
begin
  System.UniqueString(Self);
end;

procedure StringHelper.CharInto(C: Char; Len: Integer);
begin
  Self := StrOf(C, Len);
end;

procedure StringHelper.CopyInto(P: Pointer; Len: Integer);
begin
  Self := StrCopyData(P, Len);
end;

procedure StringHelper.InsertInto(const SubStr: string; Position: Integer);
begin
  Self := StrInsert(Self, SubStr, Position);
end;

function StringHelper.Equals(const Value: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrCompare(Self, Value, IgnoreCase) = 0;
end;

function StringHelper.Equals(const Values: array of string; IgnoreCase: Boolean = False): Boolean;
var
  S: string;
begin
  for S in Values do
    if StrCompare(Self, S, IgnoreCase) = 0 then
   Exit(True);
  Result := False;
end;

function StringHelper.Compare(const Value: string; IgnoreCase: Boolean = False): Integer;
begin
  Result := StrCompare(Self, Value, IgnoreCase);
end;

function StringHelper.ToUpper: string;
begin
  Result := StrUpper(Self);
end;

function StringHelper.ToLower: string;
begin
  Result := StrLower(Self);
end;

function StringHelper.Copy(Start: Integer; Len: Integer = 0): string;
begin
  Result := StrCopy(Self, Start, Len);
end;

function StringHelper.Insert(const SubStr: string; Position: Integer): string;
begin
  Result := StrInsert(Self, SubStr, Position);
end;

function StringHelper.IndexOf(const SubStr: string; IgnoreCase: Boolean = False): Integer;
begin
  Result := StrFind(Self, SubStr, IgnoreCase);
end;

function StringHelper.IndexOf(const SubStr: string; Start: Integer; IgnoreCase: Boolean = False): Integer;
begin
  Result := StrFind(Self, SubStr, Start, IgnoreCase);
end;

function StringHelper.MatchCount(const SubStr: string; IgnoreCase: Boolean = False): Integer;
begin
  Result := StrFindCount(Self, SubStr, IgnoreCase);
end;

function StringHelper.Matches(const SubStr: string; IgnoreCase: Boolean = False): IntArray;
begin
  Result := StrFindIndex(Self, SubStr, IgnoreCase);
end;

function StringHelper.Replace(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
begin
  Result := StrReplace(Self, OldPattern, NewPattern, IgnoreCase);
end;

function StringHelper.ReplaceOne(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
begin
  Result := StrReplaceOne(Self, OldPattern, NewPattern, IgnoreCase);
end;

function StringHelper.ReplaceAfter(const OldPattern, NewPattern: string; IgnoreCase: Boolean = False): string;
begin
  Result := StrReplaceAfter(Self, OldPattern, NewPattern, IgnoreCase);
end;

function StringHelper.Trim: string;
begin
  Result := StrTrim(Self);
end;

function StringHelper.ArrayIndex(const Values: array of string): Integer;
begin
  Result := StrIndex(Self, Values);
end;

function StringHelper.Lines: StringArray;
var
  S: string;
begin
  S := StrAdjustLineBreaks(Self, DefaultTextLineBreakStyle);
  Result := StrSplit(S, LineBreak);
end;

function StringHelper.LineWith(const SubStr: string; IgnoreCase: Boolean = False): string;
var
  A: StringArray;
  S: string;
begin
  A := Lines;
  for S in A do
    if S.Contains(SubStr, IgnoreCase) then
   Exit(S);
  Result := '';
end;

function StringHelper.Split(Separator: string): StringArray;
begin
  Result := StrSplit(Self, Separator);
end;

function StringHelper.SplitInt(const Separator: string): IntArray;
begin
  Result := StrSplitInt(Self, Separator);
end;

function StringHelper.SplitInt64(const Separator: string): Int64Array;
begin
  Result := StrSplitInt64(Self, Separator);
end;

function StringHelper.Words(MaxColumns: Integer = 0): StringArray;
var
  W: string;
  C, I: Integer;
begin
  if MaxColumns < 1 then
    MaxColumns := High(Integer);
  W := '';
  C := 0;
  for I := 1 to Length do
  begin
    if C >= MaxColumns then
      W := W + Self[I]
    else if Self[I] <= ' ' then
    begin
      if W.Length > 0 then
      begin
        Result.Push(W);
        Inc(C);
     end;
     W := '';
    end
    else
     W := W + Self[I];
  end;
  if W.Length > 0 then
    Result.Push(W)
end;

function StringHelper.FirstOf(const Separator: string): string;
begin
  Result := StrFirstOf(Self, Separator);
end;

function StringHelper.SecondOf(const Separator: string): string;
begin
  Result := StrSecondOf(Self, Separator);
end;

function StringHelper.LastOf(const Separator: string): string;
begin
  Result := StrLastOf(Self, Separator);
end;

function StringHelper.Between(const MarkerA, MarkerB: string): string;
begin
  Result := Self.SecondOf(MarkerA).FirstOf(MarkerB);
end;

function StringHelper.Contains(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrContains(Self, SubStr, IgnoreCase);
end;

function StringHelper.BeginsWith(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrBeginsWith(Self, SubStr, IgnoreCase);
end;

function StringHelper.BeginsWith(const SubStrs: StringArray; IgnoreCase: Boolean = False): Boolean;
var
  S: string;
begin
  Result := False;
  for S in SubStrs do
  begin
    Result := StrBeginsWith(Self, S, IgnoreCase);
    if Result then
   Exit;
  end;
end;

function StringHelper.EndsWith(const SubStr: string; IgnoreCase: Boolean = False): Boolean;
begin
  Result := StrEndsWith(Self, SubStr, IgnoreCase);
end;

function StringHelper.EndsWith(const SubStrs: StringArray; IgnoreCase: Boolean = False): Boolean;
var
  S: string;
begin
  Result := False;
  for S in SubStrs do
  begin
    Result := StrEndsWith(Self, S, IgnoreCase);
    if Result then
   Exit;
  end;
end;

function StringHelper.PadLeft(C: Char; Len: Integer): string;
begin
  Result := StrPadLeft(Self, C, Len);
end;

function StringHelper.PadRight(C: Char; Len: Integer): string;
begin
  Result := StrPadRight(Self, C, Len);
end;

function StringHelper.Hash: LargeWord;
begin
  Result := StrHash(Self);
end;

function StringHelper.Quote: string;
begin
  Result := StrQuote(Self);
end;

function StringHelper.Format(Args: array of const): string;
begin
  Result := StrFormat(Self, Args);
end;

function StringHelper.LineBreakStyle: TTextLineBreakStyle;
begin
  Result := StrLineBreakStyle(Self);
end;

function StringHelper.AdjustLineBreaks(Style: TTextLineBreakStyle): string;
begin
  Result := StrAdjustLineBreaks(Self, Style);
end;

function StringHelper.AdjustLineBreaks: string;
begin
  Result := StrAdjustLineBreaks(Self);
end;

function StringHelper.GetIsEmpty: Boolean;
begin
  Result := Length = 0;
end;

function StringHelper.GetIsWhitespace: Boolean;
begin
  Result := StrIsBlank(Self);
end;

function StringHelper.GetIsIdentifier: Boolean;
begin
  Result := StrIsIdent(Self);
end;

function StringHelper.GetIsAttribute: Boolean;
begin
  Result := StrIsAttr(Self);
end;

function StringHelper.GetLength: Integer;
begin
  Result := System.Length(Self);
end;

procedure StringHelper.SetLength(Value: Integer);
begin
  System.SetLength(Self, Value);
end;
{$endregion}

{$region events}
{ TDelegateImpl<T> }

function TDelegateImpl<T>.IndexOf(Event: T): Integer;
var
  Item: T;
  I: Integer;
begin
  I := 0;
  for Item in FList do
    if MemCompare(Item, Event, SizeOf(T)) then
   Exit(I)
    else
   Inc(I);
  Result := -1;
end;

{ TDelegateImpl<T>.IDelegate<T> }

function TDelegateImpl<T>.GetIsEmpty: Boolean;
begin
  Result := FList.IsEmpty;
end;

procedure TDelegateImpl<T>.Add(const Event: T);
var
  I: Integer;
begin
  I := IndexOf(Event);
  if I < 0 then
    FList.Push(Event);
end;

procedure TDelegateImpl<T>.Remove(const Event: T);
var
  I: Integer;
begin
  I := IndexOf(Event);
  if I > -1 then
    FList.Delete(I);
end;

{ TDelegateContainerImpl<T>.IDelegateContainer<T> }

function TDelegateContainerImpl<T>.GetDelegate: IDelegate<T>;
begin
  if FDelegate = nil then
  begin
    FDelegate := TDelegateImpl<T>.Create;
    FDelegateClass := FDelegate as TDelegateClass;
  end;
  Result := FDelegate;
end;

function TDelegateContainerImpl<T>.GetEnumerator: TArrayEnumerator<T>;
begin
  GetDelegate;
  Result := FDelegateClass.FList.GetEnumerator;
end;

{ TDelegate<T> }

class operator TDelegate<T>.Implicit(var Delegate: TDelegate<T>): IDelegate<T>;
begin
  Result := Delegate.GetContainer.Delegate;
end;

function TDelegate<T>.GetContainer: IDelegateContainer<T>;
begin
  if FContainer = nil then
    FContainer := TDelegateContainer.Create;
  Result := FContainer;
end;

function TDelegate<T>.GetEnumerator: TArrayEnumerator<T>;
begin
  if FContainer = nil then
    FContainer := TDelegateContainer.Create;
  Result := FContainer.GetEnumerator;
end;

function TDelegate<T>.GetIsEmpty: Boolean;
begin
  Result := GetContainer.Delegate.IsEmpty;
end;

procedure TDelegate<T>.Add(const Handler: T);
begin
  GetContainer.Delegate.Add(Handler);
end;

procedure TDelegate<T>.Remove(const Handler: T);
begin
  GetContainer.Delegate.Remove(Handler);
end;
{$endregion}

{$region file system routines}
function FileExists(const FileName: string): Boolean;
var
  S: string;
  F: PSDL_RWops;
begin
  S := PathAdjustDelimiters(FileName);
  F := SDL_RWFromFile(PChar(S), 'rb');
  Result := F <> nil;
  if Result then
    SDL_RWClose(F);
end;

function FileDelete(const FileName: string): Boolean;
var
  S: string;
begin
  if FileName = '' then
    Exit(False);
  S := PathAdjustDelimiters(FileName);
  {$ifdef unix}
  Result := unlink(PChar(S)) = 0;
  {$endif}
  {$ifdef windows}
  Result := DeleteFileA(PChar(S));
  {$endif}
end;

function FileRename(const OldName, NewName: string): Boolean;
var
  O, N: string;
begin
  if (OldName = '') or (NewName = '') then
    Exit(False);
  O := PathAdjustDelimiters(OldName);
  N := PathAdjustDelimiters(NewName);
  {$ifdef unix}
  Result := rename(PChar(O), PChar(N)) = 0;
  {$endif}
  {$ifdef windows}
  Result := MoveFileA(PChar(O), PChar(N));
  {$endif}
end;

function FileCopy(const FileName, CopyName: string): Boolean;
const
  BufferSize = 1024 * 16;
var
  Buffer: Pointer;
  S, D: HFILE;
  I: LargeInt;
begin
  Buffer := GetMem(BufferSize);
  try
    S := FileOpen(FileName);
    if S = nil then
      Exit(False);
    try
      D := FileCreate(CopyName);
      if D = nil then
        Exit(False);
      try
        repeat
          I := FileRead(S, Buffer, BufferSize);
          if I > 0 then
            FileWrite(D, Buffer, I);
          until I = 0;
      finally
        FileClose(D);
      end;
    finally
      FileClose(S);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

function FileSize(const FileName: string): LargeInt;
var
  S: string;
  F: PSDL_RWops;
begin
  S := PathAdjustDelimiters(FileName);
  F := SDL_RWFromFile(PChar(S), 'rb');
  if F = nil then
    Exit(-1);
  Result := SDL_RWSize(F);
  SDL_RWClose(F);
end;

function FileSize(F: HFile): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if F <> nil then
    Result :=  SDL_RWSize(R)
  else
    Result := -1;
end;

function FileAccess(const FileName: string; Mode: TFileMode): HFile;
var
  S: string;
begin
  S := PathAdjustDelimiters(FileName);
  case Mode of
    fmCreate: Result := SDL_RWFromFile(PChar(S), 'w+b');
    fmOpen: Result := SDL_RWFromFile(PChar(S), 'r+b');
    fmOpenOrCreate:
    begin
      Result := SDL_RWFromFile(PChar(S), 'r+b');
      if Result = nil then
      Result := SDL_RWFromFile(PChar(S), 'w+b');
    end;
    fmAppend: Result := SDL_RWFromFile(PChar(S), 'a+b');
  end;
end;

function FileCreate(const FileName: string): HFile;
var
  S: string;
begin
  S := PathAdjustDelimiters(FileName);
  Result := SDL_RWFromFile(PChar(S), 'w+b');
end;

function FileOpen(const FileName: string): HFile;
var
  S: string;
begin
  S := PathAdjustDelimiters(FileName);
  Result := SDL_RWFromFile(PChar(S), 'r+b');
end;

function FileAppend(const FileName: string): HFile;
var
  S: string;
begin
  S := PathAdjustDelimiters(FileName);
  Result := SDL_RWFromFile(PChar(S), 'a+b');
end;

function FileSeek(F: HFile; Offset: LargeInt; Origin: TSeekOrigin): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if F = nil then
    Exit(-1);
  Result := SDL_RWSeek(R, Offset, Ord(Origin));
end;

function FileRead(F: HFile; Buffer: Pointer; Len: LargeInt): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if F = nil then
    Exit(0);
  Result := SDL_RWRead(R, Buffer, 1, Len);
end;

function FileReadString(F: HFile; Len: LargeInt): string;
var
  R: PSDL_RWops absolute F;
begin
  if (F = nil) or (Len < 1) then
    Exit('');
  SetLength(Result, Len);
  Len := SDL_RWRead(R, Pointer(Result), 1, Len);
  if Len < 1 then
    Exit('');
  SetLength(Result, Len);
end;

function FileWrite(F: HFile; Buffer: Pointer; Len: LargeInt): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if (F = nil) or (Len < 1) then
    Exit(0);
  Result := SDL_RWWrite(R, Buffer, 1, Len);
end;

function FileWriteString(F: HFile; const S: string): LargeInt;
var
  R: PSDL_RWops absolute F;
begin
  if (F = nil) or (Length(S) < 1) then
    Exit(0);
  Result := SDL_RWWrite(R, Pointer(S), 1, Length(S));
end;

function FileWriteLine(F: HFile; const S: string): LargeInt;
var
  R: PSDL_RWops absolute F;
  L: string;
begin
  if F = nil then
    Exit(0);
  if Length(S) > 0 then
    Result := SDL_RWWrite(R, Pointer(S), 1, Length(S))
  else
    Result := 0;
  L := LineBreakStyles[DefaultTextLineBreakStyle];
  Result := Result + SDL_RWWrite(R, Pointer(L), 1, Length(L));
end;

function FileClose(var F: HFile): Boolean;
var
  R: PSDL_RWops absolute F;
begin
  if F <> nil then
  begin
    Result := SDL_RWClose(R) = 0;
    F := nil;
  end
  else
    Result := False;
end;

function FileLoadText(const FileName: string): string;
var
  F: HFile;
begin
  F := FileAccess(FileName, fmOpen);
  if F = nil then
    Exit('');
  try
    Result := FileReadString(F, FileSize(F));
  finally
    FileClose(F);
  end;
end;

procedure FileSaveText(const FileName, Text: string);
var
  F: HFile;
begin
  F := FileCreate(FileName);
  if F = nil then
    Exit;
  try
    FileWriteString(F, Text);
  finally
    FileClose(F);
  end;
end;

procedure FileAppendText(const FileName, Text: string);
var
  F: HFile;
begin
  F := FileAppend(FileName);
  if F = nil then
    Exit;
  try
    FileWriteString(F, Text);
  finally
    FileClose(F);
  end;
end;

procedure FileAppendText(const FileName, S: string; Args: array of const);
begin
  FileAppendText(FileName, StrFormat(S, Args));
end;

procedure FileAppendLine(const FileName, Text: string);
var
  F: HFile;
begin
  F := FileAppend(FileName);
  if F = nil then
    Exit;
  try
    FileWriteString(F, Text);
    FileWriteString(F, LineBreakStyles[DefaultTextLineBreakStyle]);
  finally
    FileClose(F);
  end;
end;

procedure FileAppendLine(const FileName, S: string; Args: array of const);
var
  F: HFile;
begin
  F := FileAppend(FileName);
  if F = nil then
    Exit;
  try
    FileWriteString(F, StrFormat(S, Args));
    FileWriteString(F, LineBreakStyles[DefaultTextLineBreakStyle]);
  finally
    FileClose(F);
  end;
end;

function FileExtractName(const FileName: string): string;
begin
  Result := StrLastOf(PathAdjustDelimiters(FileName), DirectorySeparator);
end;

function FileExtractExt(const FileName: string): string;
begin
  Result := StrLastOf(PathAdjustDelimiters(FileName), DirectorySeparator);
  if StrFind(Result, '.') > 0 then
    Result := '.' + StrLastOf(Result, '.')
  else
    Result := '';
end;

function FileChangeExt(const FileName, Extension: string): string;
var
  S: string;
begin
  S := FileExtractExt(FileName);
  if S = '' then
    Result := FileName + Extension
  else
    Result := StrCopy(FileName, 1, Length(FileName) - Length(S)) + Extension;
end;

function FileExtractPath(const FileName: string): string;
var
  S: string;
begin
  S:= StrLastOf(PathAdjustDelimiters(FileName), DirectorySeparator);
  if S = '' then
    Result := ''
  else
    Result := StrCopy(FileName, 1, Length(FileName) - Length(S) - 1);
end;

function DirExists(const Directory: string): Boolean;
var
  D: string;
  S: string;
begin
  if Directory = '' then
    Exit(False);
  D := PathAdjustDelimiters(Directory);
  // SystemLock.Lock;
  S := DirGetCurrent;
  Result := DirSetCurrent(D);
  DirSetCurrent(S);
  // SystemLock.Unlock;
end;

function DirRename(const OldName, NewName: string): Boolean;
begin
  Result := FileRename(OldName, NewName);
end;

function DirDelete(const Directory: string): Boolean;
begin
  if Directory = '' then
    Exit(False);
  {$ifdef unix}
  Result := rmdir(PChar(Directory)) = 0;
  {$endif}
  {$ifdef windows}
  Result := RemoveDirectoryA(PChar(Directory));
  {$endif}
end;

function DirCreate(const Directory: string): Boolean;
begin
  if Directory = '' then
    Exit(False);
  {$ifdef unix}
  Result := mkdir(PChar(Directory), S_IAUSR) = 0;
  {$endif}
  {$ifdef windows}
  Result := CreateDirectoryA(PChar(Directory), nil);
  {$endif}
end;

function DirGetCurrent: string;
var
  Buffer: array[0..$FF] of Char;
begin
  Result := '';
  {$ifdef unix}
  if getcwd(Buffer, SizeOf(Buffer)) <> nil then
    Result := PChar(Buffer);
  {$endif}
  {$ifdef windows}
  if GetCurrentDirectoryA(SizeOf(Buffer), Buffer) > 0 then
    Result := PChar(Buffer);
  {$endif}
end;

function DirSetCurrent(const Directory: string): Boolean;
var
  D: string;
begin
  D := PathAdjustDelimiters(Directory);
  if Directory = '' then
    Exit(False);
  {$ifdef unix}
  Result := chdir(PChar(D)) = 0;
  {$endif}
  {$ifdef windows}
  Result := SetCurrentDirectoryA(PChar(D));
  {$endif}
end;

function PathAdjustDelimiters(const Path: string): string;
begin
  if DirectorySeparator = '/' then
    {%H-}Result := StrReplace(Path, '\', DirectorySeparator)
  else
    {%H-}Result := StrReplace(Path, '/', DirectorySeparator);
end;

function PathIncludeTrailingDelimiter(const Path: string): string;
begin
  if StrEndsWith(PathAdjustDelimiters(Path), DirectorySeparator) then
    Result := Path
  else
    Result := Path + DirectorySeparator;
end;

function PathExcludeTrailingDelimiter(const Path: string): string;
begin
  if StrEndsWith(PathAdjustDelimiters(Path), DirectorySeparator) then
    Result := StrCopy(Path, 1, Length(Path) - 1)
  else
    Result := Path;
end;

function PathCombine(const A, B: string): string;
begin
  Result := PathIncludeTrailingDelimiter(A) + B;
end;

function PathExpand(S: string): string;
begin
  Result := S;
end;

{$endregion}

{$region findfile}
type
  TFindMatchKind = (matchAll, matchContains, matchBegins, matchEnds);

  TFindParams = record
    Kind: TFindKind;
    Folder: string;
    Match: string;
    MatchKind: TFindMatchKind;
  end;

procedure FindSearch(const Search: string; out Params: TFindParams);
var
  FileName: string;
begin
  Params.Match := '';
  Params.MatchKind := matchAll;
  if StrFind(Search, '*') < 1 then
    Params.Folder := Search
  else
  begin
    Params.Folder := FileExtractPath(Search);
    FileName := FileExtractName(Search);
    if Length(FileName) > 1 then
    begin
      Params.Match := StrReplace(FileName, '*', '');
      if StrBeginsWith(FileName, '*') and StrEndsWith(FileName, '*') then
        Params.MatchKind := matchContains
      else if StrBeginsWith(FileName, '*') then
        Params.MatchKind := matchEnds
      else
        Params.MatchKind := matchBegins;
    end;
  end;
  if Params.Folder = '' then
    Params.Folder := '.';
end;

{$ifdef unix}
type
  TFindData = record
    Dir: PDir;
    Ent: PDirEnt;
    Name: string;
    Kind: TFindKind;
    Params: TFindParams;
  end;
  PFindData = ^TFindData;

function FindMatch(var FindData: TFindData): Boolean;
var
  FindKind: TFindKind;
  Path: string;
begin
  FindKind := FindData.Params.Kind;
  Result := False;
  while FindData.Ent <> nil do
  begin
    FindData.Name := FindData.Ent.d_name;
    FindData.Name := FindData.Name.Trim;
    case FindData.Params.MatchKind of
       matchContains: Result := StrContains(FindData.Name, FindData.Params.Match);
       matchBegins: Result:= StrBeginsWith(FindData.Name, FindData.Params.Match);
       matchEnds: Result := StrEndsWith(FindData.Name, FindData.Params.Match);
    else
      Result := True;
    end;
    Path := PathCombine(FindData.Params.Folder, FindData.Name);
    if Result then
      case FindData.Ent.d_type of
        DT_UNKNOWN:
          if DirExists(Path) then
          begin
            FindData.Kind := findFolder;
            Result := FindKind <> findFile;
          end
          else if FileExists(Path) then
          begin
            FindData.Kind := findFile;
            Result := FindKind <> findFolder;
          end
          else
            Result := False;
        DT_DIR:
          begin
            FindData.Kind := findFolder;
            Result := FindKind <> findFile;
          end;
        DT_REG:
          begin
            FindData.Kind := findFile;
            Result := FindKind <> findFolder;
          end;
      else
        Result := False;
      end;
    FindData.Ent := readdir(FindData.Dir);
    if Result then
      Exit;
    Result := False;
  end;
end;

function FindFirst(const Search: string; Kind: TFindKind; out FindResult: TFindResult): Boolean;
var
  FindData: TFindData;
  Data: PFindData;
begin
  FindSearch(Search, FindData.Params);
  FindResult.Path := FindData.Params.Folder;
  FindData.Params.Kind := Kind;
  FindData.Dir := opendir(PChar(FindData.Params.Folder));
  if FindData.Dir <> nil then
  begin
    FindData.Ent := readdir(FindData.Dir);
    Result := FindMatch(FindData);
    if Result then
    begin
      FindResult.Name := FindData.Name;
      FindResult.Kind := FindData.Kind;
      New(Data);
      Data^ := FindData;
      FindResult.Handle := Data;
      Exit;
    end;
    closedir(FindData.Dir);
  end;
  FindResult.Path := '';
  FindResult.Name := '';
  FindResult.Kind := Kind;
  FindResult.Handle := nil;
end;

function FindNext(var FindResult: TFindResult): Boolean;
var
  Data: PFindData;
begin
  if FindResult.Handle = nil then
    Exit(False);
  Data := PFindData(FindResult.Handle);
  Result := FindMatch(Data^);
  if Result then
  begin
    FindResult.Name := Data.Name;
    FindResult.Kind := Data.Kind;
  end
  else
  begin
    FindResult.Path := '';
    FindResult.Name := '';
    FindResult.Kind := findBoth;
  end;
end;

procedure FindClose(var FindResult: TFindResult);
var
  Data: PFindData;
begin
  if FindResult.Handle = nil then
    Exit;
  Data := PFindData(FindResult.Handle);
  closedir(Data.Dir);
  Dispose(Data);
  FindResult.Path := '';
  FindResult.Name := '';
  FindResult.Kind := findBoth;
  FindResult.Handle := nil;
end;
{$endif}

{$ifdef windows}
type
  TFindData = record
    MatchKind: TFindKind;
    MatchPath: string;
    Path: string;
    Name: string;
    Kind: TFindKind;
    Data: TWIN32FindData;
    Handle: THandle;
  end;
  PFindData = ^TFindData;

function FindMatch(FindData: PFindData): Boolean;
var
  FindDone: Boolean;
  S: string;
begin
  FindDone := False;
  repeat
    case FindData.MatchKind of
   findFolder:
  Result := FindData.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =
    FILE_ATTRIBUTE_DIRECTORY;
   findFile:
  Result := FindData.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0;
    else
   Result := True;
    end;
    if Result then
    begin
   S := FindData.Data.cFileName;
   if S = '.' then
  Result := False
   else if S = '..' then
  Result := False;
    end;
    FindDone := not FindNextFileA(FindData.Handle, FindData.Data);
  until Result or FindDone;
  if FindDone then
  begin
    FindCloseA(FindData.Handle);
    FindData.Handle := 0;
  end;
  if Result then
  begin
    if FindData.Data.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0 then
   FindData.Kind := findFile
    else
   FindData.Kind := findFolder;
    FindData.Path := FindData.MatchPath;
    FindData.Name := FindData.Data.cFileName;
  end;
end;

function FindFirst(const Search: string; Kind: TFindKind; out FindResult: TFindResult): Boolean;
var
  S: string;
  FindData: PFindData;
begin
  FindResult.Path := '';
  FindResult.Name := '';
  FindResult.Kind := findBoth;
  FindResult.Handle := nil;
  Result := False;
  New(FindData);
  S := PathAdjustDelimiters(Search);
  FindData.MatchPath := FileExtractPath(S);
  if FindData.MatchPath = '' then
    FindData.MatchPath := DirGetCurrent;
  FindData.MatchKind := Kind;
  FindData.Handle := FindFirstFileA(PChar(S), FindData.Data);
  if FindData.Handle <> INVALID_HANDLE_VALUE then
  begin
    Result := FindMatch(FindData);
    if Result then
    begin
   FindResult.Handle := FindData;
   FindResult.Kind := FindData.Kind;
   FindResult.Path := FindData.Path;
   FindResult.Name := FindData.Name;
    end
    else
    begin
   FindResult.Handle := nil;
   FindCloseA(FindData.Handle);
   Dispose(FindData);
    end;
  end
  else
    Dispose(FindData);
end;

function FindNext(var FindResult: TFindResult): Boolean;
var
  FindData: PFindData;
begin
  FindResult.Path := '';
  FindResult.Name := '';
  FindResult.Kind := findBoth;
  if FindResult.Handle = nil then
    Exit(False);
  FindData := PFindData(FindResult.Handle);
  if FindData.Handle = 0 then
  begin
    Result := False;
    FindResult.Handle := nil;
    Dispose(FindData);
    Exit;
  end;
  Result := FindMatch(FindData);
  if Result then
  begin
    FindResult.Kind := FindData.Kind;
    FindResult.Path := FindData.Path;
    FindResult.Name := FindData.Name;
  end
  else
  begin
    FindResult.Handle := nil;
    FindCloseA(FindData.Handle);
    Dispose(FindData);
  end;
end;

procedure FindClose(var FindResult: TFindResult);
var
  FindData: PFindData;
begin
  FindResult.Path := '';
  FindResult.Name := '';
  FindResult.Kind := findBoth;
  if FindResult.Handle = nil then
    Exit;
  FindData := PFindData(FindResult.Handle);
  if FindData.Handle <> 0 then
    FindCloseA(FindData.Handle);
  Dispose(FindData);
  FindResult.Handle := nil;
end;
{$endif}

function FindFiles(const Search: string): StringArray;
var
  F: TFindResult;
begin
  Result.Length := 0;
  if FindFirst(Search, findFile, F) then
  begin
    Result.Push(PathCombine(F.Path, F.Name));
    while FindNext(F) do
      Result.Push(PathCombine(F.Path, F.Name));
    FindClose(F);
  end;
end;
{$endregion}

{$region streams}
constructor TStream.Create;
const
  { A 4 kilobyte default buffer size }
  DefaultBufferSize = 1024 * 4;
begin
  inherited Create;
  SetBufferSize(DefaultBufferSize);
end;

function TStream.ReadStr(Len: LargeWord): string;
var
  P, Z: LargeWord;
begin
  P := Position;
  Z := Size;
  if Len = 0 then
    Len := Z;
  if P + Len > Z then
    Len := Z - P;
  Result := '';
  SetLength(Result, Len);
  Read(PChar(Result)^, Len);
end;

procedure TStream.WriteStr(const S: string);
begin
  Write(PChar(S)^, Length(S));
end;

procedure TStream.LoadFromFile(const FileName: string);
var
  Source: TStream;
begin
  Source := TFileStream.Create(FileName);
  try
    Copy(Source, 0);
  finally
    Source.Free;
  end;
end;

procedure TStream.SaveToFile(const FileName: string);
var
  Dest: TStream;
begin
  Dest := TFileStream.Create(FileName, fmCreate);
  try
    Dest.Copy(Self, 0);
  finally
    Dest.Free;
  end;
end;

function TStream.Copy(Source: TStream; Len: LargeWord): LargeWord;
var
  BufferSize, I: LargeWord;
  Buffer: PByte;
begin
  if Len = 0 then
  begin
    Seek(0, soBegin);
    Len := Source.Size;
    Size := Len;
    Source.Seek(0, soBegin);
  end;
  Result := Len;
  if Result = 0 then Exit;
  if Len > FBufferSize then
    BufferSize := FBufferSize
  else
    BufferSize := Len;
  GetMem(Buffer, BufferSize);
  try
    while Len > 0 do
    begin
   if Len > BufferSize then
  I := BufferSize
   else
  I := Len;
   Source.Read(Buffer^, I);
   Write(Buffer^, I);
   Dec(Len, I);
    end;
  finally
    FreeMem(Buffer);
  end;
end;

procedure TStream.SetBufferSize(Value: Word);
begin
  if Value < $100 then
    Value := $100;
  FBufferSize := Value;
end;

{ Creates a file stream given a file name and mode
  Remarks
  Raises EIOError exception if the file cannot be accessed }

constructor TFileStream.Create(FileName: string; FileMode: TFileMode = fmOpen);
begin
  inherited Create;
  FFileName := FileName;
  FFile := FileAccess(FFileName, FileMode);
  if FFile = InvalidHandle then
    raise EIOError.Create(SIOFileHandleError);
end;

destructor TFileStream.Destroy;
begin
  if FFile <> InvalidHandle then
    FileClose(FFile);
  inherited Destroy;
end;

function SignedToUnsigned(I: LargeInt): LargeWord;
begin
  if I < 0 then
    Result := 0
  else
    Result := LargeWord(I);
end;

function TFileStream.Read(var Buffer; Len: LargeWord): LargeWord;
begin
  if Len = 0 then
    Exit(0);
  Result := SignedToUnsigned(FileRead(FFile, @Buffer, Len));
end;

function TFileStream.Write(var Buffer; Len: LargeWord): LargeWord;
begin
  if Len = 0 then
    Exit(0);
  Result := SignedToUnsigned(FileWrite(FFile, @Buffer, Len));
end;

function TFileStream.Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord;
begin
  Result := SignedToUnsigned(FileSeek(FFile, Offset, Origin));
end;

function TFileStream.GetSize: LargeWord;
begin
  Result := SignedToUnsigned(FileSize(FFile));
end;

procedure TFileStream.SetSize(const Value: LargeWord);
begin
end;

function TFileStream.GetPosition: LargeWord;
begin
  Result := SignedToUnsigned(FileSeek(FFile, 0, soCurrent));
end;

procedure TFileStream.SetPosition(const Value: LargeWord);
begin
  FileSeek(FFile, LargeInt(Value), soBegin);
end;

{ Creates a string steam with an optional initial value }

constructor TStringStream.Create(const S: string = '');
begin
  inherited Create;
  FData := S;
end;

function TStringStream.Read(var Buffer; Len: LargeWord): LargeWord;
var
  MaxLen: LargeWord;
begin
  if Len = 0 then
    Exit(0);
  MaxLen := Length(FData) - FPosition;
  if MaxLen = 0 then
    Exit(0);
  Result := Len;
  if Result > MaxLen then
    Result := MaxLen;
  System.Move(PChar(FData)[FPosition], Buffer, Result);
  Inc(FPosition, Result);
end;

function TStringStream.Write(var Buffer; Len: LargeWord): LargeWord;
begin
  if Len = 0 then
    Exit(0);
  Result := Len;
  SetSize(FPosition + Len);
  System.Move(Buffer, PChar(FData)[FPosition], Len);
  Inc(FPosition, Len);
end;

function TStringStream.Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord;
begin
  {TODO: Fix these typical mixing of signed/unsigned types}
  case Origin of
    soBegin: FPosition := SignedToUnsigned(Offset);
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := Length(FData) - Offset;
  end;
  if FPosition > Length(FData) then
    FPosition := Length(FData);
  Result := FPosition;
end;

function TStringStream.GetSize: LargeWord;
begin
  Result := Length(FData);
end;

procedure TStringStream.SetSize(const Value: LargeWord);
begin
  { Streams can only grow. To resize call Reset. }
  if Value > Length(FData) then
    SetLength(FData, Value);
end;

function TStringStream.GetPosition: LargeWord;
begin
  Result := FPosition;
end;

procedure TStringStream.SetPosition(const Value: LargeWord);
begin
  if Value > Length(FData) then
    FPosition := Length(FData)
  else
    FPosition := Value;
end;

procedure TStringStream.SetData(const Value: string);
begin
  FData := Value;
  FPosition := 0;
end;

{ TMemoryStream }

constructor TMemoryStream.Create(AllocSize: LargeWord = 0);
begin
  inherited Create;
  SetSize(AllocSize);
end;

function TMemoryStream.Read(var Buffer; Len: LargeWord): LargeWord;
var
  MaxLen: LargeWord;
begin
  if Len = 0 then
    Exit(0);
  MaxLen := FSize - FPosition;
  if MaxLen = 0 then
    Exit(0);
  Result := Len;
  if Result > MaxLen then
    Result := MaxLen;
  System.Move(PChar(FMemory)[FPosition], Buffer, Result);
  Inc(FPosition, Result);
end;

function TMemoryStream.Write(var Buffer; Len: LargeWord): LargeWord;
begin
  if Len = 0 then
    Exit(0);
  Result := Len;
  SetSize(FPosition + Len);
  System.Move(Buffer, PChar(FMemory)[FPosition], Len);
  Inc(FPosition, Len);
end;

function TMemoryStream.Seek(Offset: LargeInt; Origin: TSeekOrigin): LargeWord;
begin
  case Origin of
    soBegin: FPosition := Offset;
    soCurrent: Inc(FPosition, Offset);
    soEnd: FPosition := FSize - Offset;
  end;
  if FPosition > FSize then
    FPosition := FSize;
  Result := FPosition;
end;

function TMemoryStream.GetSize: LargeWord;
begin
  Result := FSize;
end;

procedure TMemoryStream.SetSize(const Value: LargeWord);
const
  GrowSize = 4096;
begin
  if Value > FSize then
  begin
    FSize := Value;
    if FSize > FAllocSize then
    begin
     FAllocSize := (5 * FSize) div 4;
     {TODO: Review this line}
     FAllocSize := (FAllocSize + (GrowSize - 1)) and not (GrowSize - 1);
     ReallocMem(FMemory, FAllocSize);
    end;
  end;
end;

function TMemoryStream.GetPosition: LargeWord;
begin
  Result := FPosition;
end;

procedure TMemoryStream.SetPosition(const Value: LargeWord);
begin
  if Value > FSize then
    FPosition := FSize
  else
    FPosition := Value;
end;
{$endregion}


{$region exceptions}
constructor Exception.Create(const Msg: string);
begin
  inherited Create;
  FMessage := Msg;
end;

{ Creates an exception using a formatted string }

constructor Exception.CreateFmt(const Msg: string; Args: array of const);
begin
  inherited Create;
  FMessage := StrFormat(Msg, Args);
end;

{ Do not use }

procedure EAssertError.Generate(const FileName: string; LineNumber: Integer);
begin
  FFileName := FileName;
  FLineNumber := LineNumber;
end;

{ Do not use }

procedure EIOError.Generate(Code: Integer);
begin

  FCode := Code;
end;

{ Do not use }

procedure EHeapError.FreeInstance;
begin
  if AllowFree then
    inherited FreeInstance;
end;

constructor ESDLError.CreateFunc(const FuncName: string);
var
  S: string;
begin
  S := SDL_GetError;
  if S = '' then
    S := SUnknownReason;
  inherited CreateFmt(SSDLFunctionFailed, [FuncName, S]);
end;

procedure Abort;
begin
  raise EAbortError.Create(SNoneError) at Pointer(Get_Caller_addr(Get_Frame));
end;

var
  OutOfMemoryError: EHeapError;
  InvalidPointerError: EHeapError;

procedure AbstractErrorProcHandler;
begin
  raise EAbstractError.Create(SAbstractError);
end;

procedure AssertErrorHandler(const Msg, FileName: ShortString; LineNum: LongInt; Address: Pointer);
var
  E: EAssertError;
  S: string;
begin
  if Msg = '' then
    S := SAssertionFailed
  else
    S := Msg;
  E := EAssertError.CreateFmt(SAssertionError, [S, FileName, LineNum]);
  E.Generate(FileName, LineNum);
  raise E at get_caller_addr(Address), get_caller_frame(Address);
end;

procedure ErrorProcHandler(ErrNo: LongInt; {%H-}Address: Pointer; {%H-}Frame: Pointer);
var
  E: Exception;
begin
  { This routine is called when the system unit detects an error occurs }
  case ErrNo of
    1: E := OutOfMemoryError;
    200: E := EDivByZeroError.Create(SDivByZero);
    201: E := ERangeError.Create(SRangeError);
    202: E := EStackOverflowError.Create(SStackOverflow);
    203: E := OutOfMemoryError;
    204: E := InvalidPointerError;
    205: E := EOverflowError.Create(SOverflow);
    206: E := EUnderflowError.Create(SUnderflow);
    207: E := EInvalidOpError.Create(SInvalidOp);
    208: E := EDivByZeroError.Create(SZeroDivide);
    210: E := EObjectCheckError.Create(SObjectCheck);
    211: E := EAbstractError.Create(SAbstractError);
    212: E := EExternalError.Create(SExternalException);
    214: E := EBusError.Create(SBusInvalid);
    215: E := EIntOverflowError.Create(SIntOverflow);
    216: E := EAccessViolationError.Create(SAccessViolation);
    217: E := EControlBreakError.Create(SControlBreak);
    218: E := EPrivInstructionError.Create(SPrivInstruction);
    219: E := EInvalidCastError.Create(SInvalidCast);
    220: E := EVariantError.Create(SVarTypeCast);
    221: E := EVariantError.Create(SVarInvalidOp);
    222: E := EVariantError.Create(SVarDispatch);
    223: E := EVariantError.Create(SVarArrayCreate);
    224: E := EVariantError.Create(SVarNotArray);
    225: E := EVariantError.Create(SVarArrayBounds);
    227: E := EAssertError.Create(SAssertionFailed);
    228: E := EIntfCastError.Create(SIntfCast);
    229: E := ESafeCallError.Create(SSafeCallError);
    231: E := EConvertError.Create(SConvertNoneError);
    232: E := ENoThreadSupportError.Create(SNoThreadSupport);
    233: E := EQuitSignalError.Create(SQuitSignal);
    234: E := ENoWideStringSupportError.Create(SNoWideStringSupport);
  else
    E := Exception.CreateFmt(SUnknownError, [ErrNo]);
  end;
  raise E at Address, Frame;
end;

procedure ExceptProcHandler(Obj: TObject; Address: Pointer; FrameCount: LongInt; Frame: PPointer);//[public, alias: 'FPC_BREAK_UNHANDLED_EXCEPTION'];
var
  Addr: PtrUInt absolute Address;
  OutHandle: ^Text;
  S: string;
  I: LongInt;
begin
  { This routine is called when an unhandled error occurs
    i.e. an error that is not stopped by a except block }
  OutHandle := @StdOut;
  WriteLn(OutHandle^, 'An unhandled exception occurred at $', HexStr(Addr, SizeOf(Addr) * 2));
  if Obj is Exception then
  begin
    S := Exception(Obj).ClassName + ' : ' + Exception(Obj).Message;
    Writeln(OutHandle^,S);
  end
  else
    WriteLn(OutHandle^,'Exception object ', Obj.ClassName, ' is not of class Exception');
  WriteLn(OutHandle^, BackTraceStrFunc(Address));
  if FrameCount > 0 then
    for I := 0 to FrameCount - 1 do
   WriteLn(OutHandle^, BackTraceStrFunc(Frame[I]));
  WriteLn(OutHandle^, '');
end;

procedure SafeCallErrorProcHandler({%H-}ErrorCode: Integer; {%H-}Address: Pointer);
begin
  raise ESafeCallError.Create(SSafeCallError) at ErrorAddr;
end;

procedure InitExceptions;
begin
  OutOfMemoryError := EOutOfMemoryError.Create(SOutOfMemory);
  OutOfMemoryError.AllowFree := False;
  InvalidPointerError := EInvalidPtrError.Create(SInvalidPointer);
  InvalidPointerError.AllowFree := False;
  AbstractErrorProc := @AbstractErrorProcHandler;
  AssertErrorProc := @AssertErrorHandler;
  ErrorProc := @ErrorProcHandler;
  ExceptProc := @ExceptProcHandler;
  SafeCallErrorProc := @SafeCallErrorProcHandler;
end;

procedure DoneExceptions;
begin
  OutOfMemoryError.AllowFree := True;
  OutOfMemoryError.Free;
  InvalidPointerError.AllowFree := True;
  InvalidPointerError.Free;
end;
{$endregion}

{$region threading}
var
  ThreadCount: LongInt;

function ThreadRun(Param: Pointer): PtrInt;
var
  Thread: TThread absolute Param;
  ExitCode: LongWord;
begin
  { Track the number of running threads so that DoneThreads waits }
  InterLockedIncrement(ThreadCount);
  SDL_SemWait(Thread.FSemaphore);
  InterLockedIncrement(Thread.FState);
  ExitCode := 0;
  try
    ExitCode := Thread.Execute;
  except
    ExitCode := 1;
  end;
  Thread.Terminate;
  InterLockedExchange(Thread.FExitCode, ExitCode);
  InterLockedIncrement(Thread.FState);
  SDL_SemPost(Thread.FSemaphore);
  if Assigned(Thread.FOnTerminate) then
  try
    Thread.FOnTerminate(Thread);
  except
    ExitCode := 1;
  end;
  if Thread.FFreeOnTerminate then
    Thread.Free;
  InterLockedDecrement(ThreadCount);
  Result := ExitCode;
  EndThread(Result);
end;

{ Create a thread with and optionally suspend its execution }

constructor TThread.Create(Suspended: Boolean = False);
begin
  inherited Create;
  FCreateSuspended := True;
  if not Suspended then
    Resume;
end;

constructor TThread.Create(OnTerminate: TNotifyEvent; Suspended: Boolean = False);
begin
  FOnTerminate := OnTerminate;
  Create(Suspended);
end;

destructor TThread.Destroy;
begin
  Terminate;
  WaitFor(0);
  if not FCreateSuspended then
  begin
    CloseThread(FHandle);
    SDL_DestroySemaphore(FSemaphore);
  end;
  inherited Destroy;
end;

{ Resumes execution of a thread }

procedure TThread.Resume;
begin
  if FCreateSuspended then
  begin
    if Terminated then
      Exit;
    FCreateSuspended := False;
    FSemaphore := SDL_CreateSemaphore(0);
    FHandle := BeginThread(@ThreadRun, Self);
    SDL_SemPost(FSemaphore);
    while FState < 1 do
   ThreadSwitch;
  end
  else if FHandle <> ThreadZero then
    ResumeThread(FHandle);
end;

{ Suspends execution of a thread }

procedure TThread.Suspend;
begin
  if FHandle = ThreadZero then
    Exit
  else if Done then
    Exit
  else
    SuspendThread(FHandle);
end;

{ Politely request a thread to exit. It is up to the thread's Execute method to
  check Terminated status and exit early. }

procedure TThread.Terminate;
begin
  InterLockedIncrement(FTerminated);
end;

{ Waits for thread to complete given a timeout in milliseconds
  Remarks
  Pass zero to wait indefinitely. Returns true if the thread was done before the
  timeout expired }

function TThread.WaitFor(Timeout: LongInt): Boolean;
begin
  if FHandle = ThreadZero then
    Result := False
  else if GetCurrentThreadId = FHandle then
    Result := False
  else if Done then
  begin
    WaitForThreadTerminate(FHandle, Timeout);
    Result := True;
  end
  else
  begin
    Resume;
    if Timeout = 0 then
    begin
     WaitForThreadTerminate(FHandle, 0);
     Result := True;
    end
    else
    begin
      Result := SDL_SemWaitTimeout(FSemaphore, Timeout) = 0;
      if Result then
        WaitForThreadTerminate(FHandle, 0);
    end;
  end;
end;

function TThread.GetTerminated: Boolean;
begin
  Result := FTerminated <> 0;
end;

function TThread.GetDone: Boolean;
begin
  Result := FState > 1;
end;

threadvar
  MutexLocks: array[0..31] of SmallInt;

var
  MutexIdents: Integer;

type
  TMutex = class(TInterfacedObject, IInterface, IMutex)
  private
    FSemaphore: Pointer;
    FIdent: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    function TryLock(Timeout: LongWord = 0): Boolean;
    procedure Unlock;
  end;

constructor TMutex.Create;
var
  I: Integer;
begin
  inherited Create;
  FSemaphore := SDL_CreateSemaphore(1);
  if FSemaphore = nil then
    raise ESynchronizeError.Create(SSynchronizeError);
  Tiny.System.Lock;
  try
    I := 1;
    while MutexIdents and I <> 0 do
    begin
   I := I shl 1;
   Inc(FIdent);
    end;
    MutexIdents := MutexIdents or I;
  finally
    Tiny.System.Unlock;
  end;
end;

destructor TMutex.Destroy;
var
  I: Integer;
begin
  if FSemaphore <> nil then
  begin
    Tiny.System.Lock;
    try
   I := 1 shr FIdent;
   MutexIdents := MutexIdents and (not I);
    finally
   Tiny.System.Unlock;
    end;
    MutexLocks[FIdent] := 0;
    SDL_DestroySemaphore(FSemaphore);
  end;
end;

procedure TMutex.Lock;
begin
  Inc(MutexLocks[FIdent]);
  if MutexLocks[FIdent] = 1 then
    SDL_SemWait(FSemaphore);
end;

function TMutex.TryLock(Timeout: LongWord = 0): Boolean;
begin
  Inc(MutexLocks[FIdent]);
  if MutexLocks[FIdent] = 1 then
    if Timeout = 0 then
   Result := SDL_SemTryWait(FSemaphore) = 0
    else
   Result := SDL_SemWaitTimeout(FSemaphore, Timeout) = 0
  else
    Result := True;
end;

procedure TMutex.Unlock;
begin
  Dec(MutexLocks[FIdent]);
  if MutexLocks[FIdent] = 0 then
    SDL_SemPost(FSemaphore)
  else if MutexLocks[FIdent] < 0 then
    raise ESynchronizeError.Create(SSynchronizeError);
end;

function CreateMutex: IMutex;
begin
  Result := TMutex.Create;
end;

var
  Semaphore: Pointer;

threadvar
  SemaphoreLock: Integer;

procedure Lock;
begin
  Inc(SemaphoreLock);
  if SemaphoreLock = 1 then
    SDL_SemWait(Semaphore);
end;

function TryLock(Timeout: LongWord = 0): Boolean;
begin
  Inc(SemaphoreLock);
  if SemaphoreLock = 1 then
    if Timeout = 0 then
   Result := SDL_SemTryWait(Semaphore) = 0
    else
   Result := SDL_SemWaitTimeout(Semaphore, Timeout) = 0
  else
    Result := True;
end;

procedure Unlock;
begin
  Dec(SemaphoreLock);
  if SemaphoreLock = 0 then
    SDL_SemPost(Semaphore)
  else if SemaphoreLock < 0 then
    raise ESynchronizeError.Create(SSynchronizeError);
end;

function ThreadStack({%H-}Param: Pointer): PtrInt;
begin
  Result := 0;
  EndThread(Result);
end;

procedure InitThreads;
var
  Thread: TThreadID;
begin
  { I've been told this is the only way to setup rtl threading safely }
  Thread := BeginThread(ThreadStack);
  SDL_Delay(10);
  WaitForThreadTerminate(Thread, 0);
  CloseThread(Thread);
  Semaphore := SDL_CreateSemaphore(1);
  SDL_SemPost(Semaphore);
  if Semaphore = nil then
    raise ESynchronizeError.Create(SSynchronizeError);
  SDL_SemPost(Semaphore);
end;

procedure DoneThreads;
begin
  { Wait for threads to complete }
  while ThreadCount > 0 do
    Sleep(1);
  SDL_SemWait(Semaphore);
end;
{$endregion}

{$if defined(cpui386) or defined(cpux86_64)}
function GetExceptionMask: TFPUExceptionMask;
begin
{$ifdef FPC_HAS_TYPE_EXTENDED}
  Result := TFPUExceptionMask(DWord(Get8087CW and $3F));
{$else}
  Result := TFPUExceptionMask(DWord((GetMXCSR shr 7) and $3f));
{$endif}
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
  CtlWord: DWord;
  SSECSR: DWord;
begin
  CtlWord := Get8087CW;
  SSECSR := GetMXCSR;
  Set8087CW((CtlWord and $FFC0) or Byte(LongInt(Mask)));
  SetMXCSR((SSECSR and $FFFFE07F) or (DWord(Mask) shl 7));
{$ifdef FPC_HAS_TYPE_EXTENDED}
  Result := TFPUExceptionMask(DWord(CtlWord and $3F));
{$else}
  Result := TFPUExceptionMask((SSECSR shr 7) and $3F);
{$endif FPC_HAS_TYPE_EXTENDED}
end;
{$ifend}

initialization
  {$if defined(cpui386) or defined(cpux86_64)}
  SetExceptionMask(GetExceptionMask + [exZeroDivide, exInvalidOp, exOverflow, exUnderflow]);
  {$ifend}
  InitExceptions;
  StringArray.DefaultCompare := DefaultStringCompare;
  StringArray.DefaultConvertString := DefaultStringConvertString;
  WordArray.DefaultCompare := DefaultWordCompare;
  WordArray.DefaultConvertString := DefaultWordConvertString;
  IntArray.DefaultCompare := DefaultIntCompare;
  IntArray.DefaultConvertString := DefaultIntConvertString;
  Int64Array.DefaultCompare := DefaultInt64Compare;
  Int64Array.DefaultConvertString := DefaultInt64ConvertString;
  FloatArray.DefaultCompare := DefaultFloatCompare;
  FloatArray.DefaultConvertString := DefaultFloatConvertString;
  InitThreads;
  SystemLock := CreateMutex;
  Now;
finalization
  DoneThreads;
  DoneExceptions;
end.

