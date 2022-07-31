unit Tiny.Interop.Windows;

{$i tiny.inc}

interface

{$ifdef windows}
  {$define kernel32 := external 'kernel32.dll'}
  {$define ole32 := external 'ole32.dll'}
type
  BOOL = LongBool;

function MoveFileA(lpExistingFileName, lpNewFileName: PChar): BOOL; stdcall; kernel32;
function DeleteFileA(lpFileName: PChar): BOOL; stdcall; kernel32;
function GetCurrentDirectoryA(nBufferLength: DWORD; lpBuffer: PChar): DWORD; stdcall; kernel32;
function SetCurrentDirectoryA(lpPathName: PChar): BOOL; stdcall; kernel32;
function RemoveDirectoryA(lpPathName: PChar): BOOL; stdcall; kernel32;
function CreateDirectoryA(lpPathName: PChar; lpSecurityAttributes: Pointer): BOOL; stdcall; kernel32;
function GetFileAttributesA(lpFileName: PChar): DWORD; stdcall; kernel32;

const
  MAX_PATH = 260;
  INVALID_HANDLE_VALUE = DWORD(-1);
  FILE_ATTRIBUTE_DIRECTORY = $00000010;

type
  TFileTime = record
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
  end;

  TWIN32FindData = record
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    cFileName: array[0..MAX_PATH - 1] of AnsiChar;
    cAlternateFileName: array[0..13] of AnsiChar;
  end;

{    dwFileAttributes: DWORD;
    ftCreationTime: Int64;
    ftLastAccessTime: Int64;
    ftLastWriteTime: Int64;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    cFileName: array[0..MAX_PATH - 1] of Char;
    cAlternateFileName: array[0..13] of Char;
  end;}

function FindFirstFileA(lpFileName: PAnsiChar; var lpFindFileData: TWIN32FindData): THandle; stdcall; kernel32;
function FindNextFileA(hFindFile: THandle; var lpFindFileData: TWIN32FindData): BOOL; stdcall; kernel32;
function FindCloseA(hFindFile: THandle): BOOL; stdcall; kernel32 name 'FindClose';

function OleInitialize(pvReserved: Pointer): HResult; stdcall; ole32;
procedure OleUninitialize; stdcall; ole32;

type
  LargeInt = Int64;
  PLargeInt = ^LargeInt;

const
  STGTY_STORAGE    = 1;
  STGTY_STREAM  = 2;
  STGTY_LOCKBYTES  = 3;
  STGTY_PROPERTY   = 4;

  LOCK_WRITE   = 1;
  LOCK_EXCLUSIVE  = 2;
  LOCK_ONLYONCE   = 4;

  STG_E_INVALIDFUNCTION    = HResult($80030001);
  STG_E_FILENOTFOUND    = HResult($80030002);
  STG_E_PATHNOTFOUND    = HResult($80030003);
  STG_E_TOOMANYOPENFILES   = HResult($80030004);
  STG_E_ACCESSDENIED    = HResult($80030005);
  STG_E_INVALIDHANDLE   = HResult($80030006);
  STG_E_INSUFFICIENTMEMORY    = HResult($80030008);
  STG_E_INVALIDPOINTER  = HResult($80030009);
  STG_E_NOMOREFILES  = HResult($80030012);
  STG_E_DISKISWRITEPROTECTED  = HResult($80030013);
  STG_E_SEEKERROR    = HResult($80030019);
  STG_E_WRITEFAULT   = HResult($8003001D);
  STG_E_READFAULT    = HResult($8003001E);
  STG_E_SHAREVIOLATION  = HResult($80030020);
  STG_E_LOCKVIOLATION   = HResult($80030021);
  STG_E_FILEALREADYEXISTS  = HResult($80030050);
  STG_E_INVALIDPARAMETER   = HResult($80030057);
  STG_E_MEDIUMFULL   = HResult($80030070);
  STG_E_PROPSETMISMATCHED  = HResult($800300F0);
  STG_E_ABNORMALAPIEXIT    = HResult($800300FA);
  STG_E_INVALIDHEADER   = HResult($800300FB);
  STG_E_INVALIDNAME  = HResult($800300FC);
  STG_E_UNKNOWN   = HResult($800300FD);
  STG_E_UNIMPLEMENTEDFUNCTION = HResult($800300FE);
  STG_E_INVALIDFLAG  = HResult($800300FF);
  STG_E_INUSE  = HResult($80030100);
  STG_E_NOTCURRENT   = HResult($80030101);
  STG_E_REVERTED  = HResult($80030102);
  STG_E_CANTSAVE  = HResult($80030103);
  STG_E_OLDFORMAT    = HResult($80030104);
  STG_E_OLDDLL    = HResult($80030105);
  STG_E_SHAREREQUIRED   = HResult($80030106);
  STG_E_EXTANTMARSHALLINGS    = HResult($80030108);
  STG_E_DOCFILECORRUPT  = HResult($80030109);
  STG_E_BADBASEADDRESS  = HResult($80030110);
  STG_E_INCOMPLETE   = HResult($80030201);
  STG_E_TERMINATED   = HResult($80030202);

type
  TStatStg = record
    pwcsName: PWideChar;
    dwType: DWORD;
    cbSize: LargeInt;
    mtime: TFileTime;
    ctime: TFileTime;
    atime: TFileTime;
    grfMode: DWORD;
    grfLocksSupported: DWORD;
    clsid: TGuid;
    grfStateBits: DWORD;
    reserved: DWORD;
  end;
  PStatStg = ^TStatStg;

  ISequentialStream = interface(IUnknown)
  ['{0C733A30-2A1C-11CE-ADE5-00AA0044773D}']
  function Read(pv: Pointer; cb: DWORD; pcbRead: PDWORD): HResult; stdcall;
  function Write(pv: Pointer; cb: DWORD; pcbWritten: PDWORD): HResult; stdcall;
  end;

  IStream = interface(ISequentialStream)
  ['{0000000C-0000-0000-C000-000000000046}']
    function Seek(dlibMove: LargeInt; dwOrigin: LongInt; libNewPosition: PLargeInt): HResult; stdcall;
    function SetSize(libNewSize: LargeInt): HResult; stdcall;
    function CopyTo(stm: IStream; cb: LargeInt; out cbRead: LargeInt; out cbWritten: LargeInt): HResult; stdcall;
    function Commit(grfCommitFlags: Longint): HResult; stdcall;
    function Revert: HResult; stdcall;
    function LockRegion(libOffset: LargeInt; cb: LargeInt; dwLockType: LongInt): HResult; stdcall;
    function UnlockRegion(libOffset: LargeInt; cb: LargeInt; dwLockType: LongInt): HResult; stdcall;
    function Stat(statstg: PStatStg; grfStatFlag: Longint): HResult; stdcall;
    function Clone(out stm: IStream): HResult; stdcall;
  end;
{$endif}

implementation

end.

