unit Tiny.Interop.Unix;

{$i tiny.inc}

interface

{$ifdef unix}
  {$ifdef linux}
    {$define libunix :=  external 'c'}
  {$endif}
  {$ifdef darwin}
    {$define libunix :=  external 'System'}
  {$endif}

const
  S_IRUSR = $0100;
  S_IWUSR = $0080;
  S_IXUSR = $0040;
  S_IAUSR = S_IRUSR or S_IWUSR or S_IXUSR;

function rename(oldname, newname: PChar): LongInt; cdecl; libunix;
function unlink(path: PChar): LongInt; cdecl; libunix;
function getcwd(buf: PChar; size: IntPtr): PChar; cdecl; libunix;
function chdir(path: PChar): LongInt; cdecl; libunix;
function rmdir(path: PChar): LongInt; cdecl; libunix;
function mkdir(path: PChar; mode: LongWord): LongInt; cdecl; libunix;

const
  { unknown }
  DT_UNKNOWN = 0;
  { named pipe }
  DT_FIFO = 1;
  { character device }
  DT_CHR = 2;
  { directory }
  DT_DIR = 4;
  { block device }
  DT_BLK = 6;
  { file }
  DT_REG = 8;
  { symbolic link }
  DT_LNK = 10;
  { socket }
  DT_SOCK = 12;

type
  PDir = Pointer;
{$ifdef darwin}
  dirent = record
    d_fileno: LongWord;
    d_reclen: Word;
    d_type: Byte;
    d_namlen: Byte;
    d_name: array[0..10240] of Char;
  end;
{$else}
(*
  {$packrecords 4}
  { available on Mac OS X 10.6 and later, and used by all iPhoneOS versions }
  dirent  = record
    d_fileno   : cuint64;   // file number of entry
    d_seekoff  : cuint64;   // seek offset (optional, used by servers)
    d_reclen   : cuint16;   // length of this record
    d_namlen   : cuint16;   // length of string in d_name
    d_type  : cuint8;    // file type, see below
    d_name  : array[0..PATH_MAX-1] of char;  // name must be no longer than this
  end;
*)
  dirent = packed record
    {$ifdef cpuarm}
    { inode number }
    d_ino: Pointer;
    { offset to the next dirent }
    d_off: Pointer;
    {$else}
    { inode number }
    d_ino: QWord;
    { offset to the next dirent }
    d_off: QWord;
    {$endif}
    { length of this record }
    d_reclen: Byte;
    { type of file }
    d_type: Byte;
    { filler }
    //d_fill: Byte;
    { filename }
    d_name: array[0..10240] of Char;
  end;
{$endif}

  TDirEnt = dirent;
  PDirEnt = ^TDirEnt;

function opendir(dirname: PChar): PDir; cdecl; libunix;
function readdir(d: PDir): PDirEnt; cdecl; libunix;
function closedir(d: PDir): LongInt; cdecl; libunix;
{$endif}

implementation

end.

