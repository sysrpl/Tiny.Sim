unit Tiny.Interop.SDL2;

{$i tiny.inc}
{$define libsdl2 := external 'SDL2'}

interface

uses
  Tiny.Types;

type
  Uint8 = Byte;
  PUint8 = ^Uint8;
  Uint16 = Word;
  PUint16 = ^Uint16;
  Sint16 = SmallInt;
  Uint32 = LongWord;
  Sint32 = LongInt;
  Sint64 = Int64;
  SDL_Bool = LongBool;
  SDL_Char = PChar;
  SDL_Float = Single;
  SDL_Double = Double;
  SDL_Long = Cardinal;
  TSDL_Window = record end;
  PSDL_Window = ^TSDL_Window;
  TSDL_GLContext = record end;
  PSDL_GLContext = ^TSDL_GLContext;

{ SDL_version.h }

type
  SDL_version = record
    major: Uint8;
    minor: Uint8;
    patch: Uint8;
  end;
  TSDL_version = SDL_version;
  PSDL_version = ^SDL_version;

{ SDL.h }

const
  SDL_INIT_TIMER          = $00000001;
  SDL_INIT_AUDIO          = $00000010;
  SDL_INIT_VIDEO          = $00000020;
  SDL_INIT_JOYSTICK       = $00000200;
  SDL_INIT_HAPTIC         = $00001000;
  { turn on game controller also implicitly does JOYSTICK }
  SDL_INIT_GAMECONTROLLER = $00002000;
  { don't catch fatal signals }
  SDL_INIT_NOPARACHUTE    = $00100000;
  SDL_INIT_EVERYTHING     =
    SDL_INIT_TIMER or SDL_INIT_AUDIO or SDL_INIT_VIDEO or
    SDL_INIT_JOYSTICK or SDL_INIT_HAPTIC or SDL_INIT_GAMECONTROLLER;

function SDL_Init(flags: Uint32): LongInt; cdecl; libsdl2;
function SDL_InitSubSystem(flags: Uint32): LongInt; cdecl; libsdl2;
procedure SDL_QuitSubSystem(flags: Uint32); cdecl; libsdl2;
function SDL_WasInit(flags: Uint32): LongInt; cdecl; libsdl2;
procedure SDL_Quit; cdecl; libsdl2;

{ SDL_error.h }

function SDL_GetError: SDL_Char; cdecl; libsdl2;
procedure SDL_ClearError; cdecl; libsdl2;

{ SDL_rect.h }

type
  SDL_Point = packed record
    x: LongInt;
    y: LongInt;
  end;
  TSDL_Point = SDL_Point;
  PSDL_Point = ^TSDL_Point;

  SDL_Rect = packed record
    x: LongInt;
    y: LongInt;
    w: LongInt;
    h: LongInt;
  end;
  TSDL_Rect = SDL_Rect;
  PSDL_Rect = ^TSDL_Rect;

function SDL_HasIntersection(constref A, B: TSDL_Rect): SDL_Bool; cdecl; libsdl2;
function SDL_IntersectRect(constref A, B: TSDL_Rect; out R: TSDL_Rect): SDL_Bool; cdecl; libsdl2;
procedure SDL_UnionRect(constref A, B: TSDL_Rect; out R: TSDL_Rect); cdecl; libsdl2;
function SDL_EnclosePoints(var points: TSDL_Point; count: LongInt;
  clip: PSDL_Rect; var result): SDL_Bool; cdecl; libsdl2;
function SDL_IntersectRectAndLine(constref rect: TSDL_Rect;
  var x1, y1, x2, y2: LongInt): SDL_Bool; cdecl; libsdl2;

{ SDL_pixels.h }

const
  SDL_ALPHA_OPAQUE = 255;
  SDL_ALPHA_TRANSPARENT = 0;

{ Pixel type }

  SDL_PIXELTYPE_UNKNOWN = 0;
  SDL_PIXELTYPE_INDEX1 = SDL_PIXELTYPE_UNKNOWN + 1;
  SDL_PIXELTYPE_INDEX4 = SDL_PIXELTYPE_INDEX1 + 1;
  SDL_PIXELTYPE_INDEX8 = SDL_PIXELTYPE_INDEX4 + 1;
  SDL_PIXELTYPE_PACKED8 = SDL_PIXELTYPE_INDEX8 + 1;
  SDL_PIXELTYPE_PACKED16 = SDL_PIXELTYPE_PACKED8 + 1;
  SDL_PIXELTYPE_PACKED32 = SDL_PIXELTYPE_PACKED16 + 1;
  SDL_PIXELTYPE_ARRAYU8 = SDL_PIXELTYPE_PACKED32 + 1;
  SDL_PIXELTYPE_ARRAYU16 = SDL_PIXELTYPE_ARRAYU8 + 1;
  SDL_PIXELTYPE_ARRAYU32 = SDL_PIXELTYPE_ARRAYU16 + 1;
  SDL_PIXELTYPE_ARRAYF16 = SDL_PIXELTYPE_ARRAYU32 + 1;
  SDL_PIXELTYPE_ARRAYF32 = SDL_PIXELTYPE_ARRAYF16 + 1;

{ Bitmap pixel order, high bit -> low bit }

  SDL_BITMAPORDER_NONE = 0;
  SDL_BITMAPORDER_4321 = SDL_BITMAPORDER_NONE + 1;
  SDL_BITMAPORDER_1234 = SDL_BITMAPORDER_4321 + 1;

{ Packed component order }

  SDL_PACKEDORDER_NONE = 0;
  SDL_PACKEDORDER_XRGB = SDL_PACKEDORDER_NONE + 1;
  SDL_PACKEDORDER_RGBX = SDL_PACKEDORDER_XRGB + 1;
  SDL_PACKEDORDER_ARGB = SDL_PACKEDORDER_RGBX + 1;
  SDL_PACKEDORDER_RGBA = SDL_PACKEDORDER_ARGB + 1;
  SDL_PACKEDORDER_XBGR = SDL_PACKEDORDER_RGBA + 1;
  SDL_PACKEDORDER_BGRX = SDL_PACKEDORDER_XBGR + 1;
  SDL_PACKEDORDER_ABGR = SDL_PACKEDORDER_BGRX + 1;
  SDL_PACKEDORDER_BGRA = SDL_PACKEDORDER_ABGR + 1;

{ Array component order }

  SDL_ARRAYORDER_NONE = 0;
  SDL_ARRAYORDER_RGB = SDL_ARRAYORDER_NONE + 1;
  SDL_ARRAYORDER_RGBA = SDL_ARRAYORDER_RGB + 1;
  SDL_ARRAYORDER_ARGB = SDL_ARRAYORDER_RGBA + 1;
  SDL_ARRAYORDER_BGR = SDL_ARRAYORDER_ARGB + 1;
  SDL_ARRAYORDER_BGRA = SDL_ARRAYORDER_BGR + 1;
  SDL_ARRAYORDER_ABGR = SDL_ARRAYORDER_BGRA + 1;

{ Packed component layout }

  SDL_PACKEDLAYOUT_NONE = 0;
  SDL_PACKEDLAYOUT_332 = SDL_PACKEDLAYOUT_NONE + 1;
  SDL_PACKEDLAYOUT_4444 = SDL_PACKEDLAYOUT_332 + 1;
  SDL_PACKEDLAYOUT_1555 = SDL_PACKEDLAYOUT_4444 + 1;
  SDL_PACKEDLAYOUT_5551 = SDL_PACKEDLAYOUT_1555 + 1;
  SDL_PACKEDLAYOUT_565 = SDL_PACKEDLAYOUT_5551 + 1;
  SDL_PACKEDLAYOUT_8888 = SDL_PACKEDLAYOUT_565 + 1;
  SDL_PACKEDLAYOUT_2101010 = SDL_PACKEDLAYOUT_8888 + 1;
  SDL_PACKEDLAYOUT_1010102 = SDL_PACKEDLAYOUT_2101010 + 1;

  SDL_PIXELFORMAT_UNKNOWN = 0;
  SDL_PIXELFORMAT_INDEX1LSB = $11100100;
  SDL_PIXELFORMAT_INDEX1MSB = $11200100;
  SDL_PIXELFORMAT_INDEX4LSB = $12100400;
  SDL_PIXELFORMAT_INDEX4MSB = $12200400;
  SDL_PIXELFORMAT_INDEX8 = $13000801;
  SDL_PIXELFORMAT_RGB332 = $14110801;
  SDL_PIXELFORMAT_RGB444 = $15120C02;
  SDL_PIXELFORMAT_RGB555 = $15130F02;
  SDL_PIXELFORMAT_BGR555 = $15530F02;
  SDL_PIXELFORMAT_ARGB4444 = $15321002;
  SDL_PIXELFORMAT_RGBA4444 = $15421002;
  SDL_PIXELFORMAT_ABGR4444 = $15721002;
  SDL_PIXELFORMAT_BGRA4444 = $15821002;
  SDL_PIXELFORMAT_ARGB1555 = $15331002;
  SDL_PIXELFORMAT_RGBA5551 = $15441002;
  SDL_PIXELFORMAT_ABGR1555 = $15731002;
  SDL_PIXELFORMAT_BGRA5551 = $15841002;
  SDL_PIXELFORMAT_RGB565 = $15151002;
  SDL_PIXELFORMAT_BGR565 = $15551002;
  SDL_PIXELFORMAT_RGB24 = $17101803;
  SDL_PIXELFORMAT_BGR24 = $17401803;
  SDL_PIXELFORMAT_RGB888 = $16161804;
  SDL_PIXELFORMAT_RGBX8888 = $16261804;
  SDL_PIXELFORMAT_BGR888 = $16561804;
  SDL_PIXELFORMAT_BGRX8888 = $16661804;
  SDL_PIXELFORMAT_ARGB8888 = $16362004;
  SDL_PIXELFORMAT_RGBA8888 = $16462004;
  SDL_PIXELFORMAT_ABGR8888 = $16762004;
  SDL_PIXELFORMAT_BGRA8888 = $16862004;
  SDL_PIXELFORMAT_ARGB2101010 = $16372004;

{ See http://wiki.libsdl.org/moin.fcg/SDL_PixelFormatEnum }
function SDL_PIXELTYPE(X: Uint32): Uint32;
function SDL_PIXELORDER(X: Uint32): Uint32;
function SDL_PIXELLAYOUT(X: Uint32): Uint32;
function SDL_BITSPERPIXEL(X: Uint32): Uint32;

type
  SDL_Color = packed record
    r, g, b, a: Uint8;
  end;
  TSDL_Color = SDL_Color;
  PSDL_Color = ^TSDL_Color;

  SDL_Palette = packed record
    ncolors: LongInt;
    colors: PSDL_Color;
    version: Uint32;
    refcount: Uint32;
  end;
  TSDL_Palette = SDL_Palette;
  PSDL_Palette = ^TSDL_Palette;

  PSDL_PixelFormat = ^TSDL_PixelFormat;
  SDL_PixelFormat = packed record
    { Everything in the pixel format structure is read-only }
    format: Uint32;
    palette: PSDL_Palette;
    BitsPerPixel: Uint8;
    BytesPerPixel: Uint8;
    pad1: Uint8;
    pad2: Uint8;
    Rmask: Uint32;
    Gmask: Uint32;
    Bmask: Uint32;
    Amask: Uint32;
    Rloss: Uint8;
    Gloss: Uint8;
    Bloss: Uint8;
    Aloss: Uint8;
    Rshift: Uint8;
    Gshift: Uint8;
    Bshift: Uint8;
    Ashift: Uint8;
    refcount: LongInt;
    next: PSDL_PixelFormat;
  end;
  TSDL_PixelFormat = SDL_PixelFormat;

function SDL_GetPixelFormatName(format: SDL_Char): SDL_Char; cdecl; libsdl2;
function SDL_PixelFormatEnumToMasks(format: Uint32; out bpp: LongInt;
  out Rmask, Gmask, Bmask, Amask: Uint32): SDL_Bool; cdecl; libsdl2;
function SDL_MasksToPixelFormatEnum(bpp: LongInt; Rmask, Gmask, Bmask, Amask: Uint32): Uint32; cdecl; libsdl2;
function SDL_AllocFormat(pixel_format: Uint32): PSDL_PixelFormat; cdecl; libsdl2;
procedure SDL_FreeFormat(format: PSDL_PixelFormat); cdecl; libsdl2;
function SDL_AllocPalette(ncolors: LongInt): PSDL_Palette; cdecl; libsdl2;
function SDL_SetPixelFormatPalette(format: PSDL_PixelFormat; palette: PSDL_Palette): LongInt; cdecl; libsdl2;
function SDL_SetPaletteColors(palette: PSDL_Palette; var colors: TSDL_Color;
  firstcolor, ncolors: LongInt): LongInt; cdecl; libsdl2;
procedure SDL_FreePalette(palette: PSDL_Palette); cdecl; libsdl2;
function SDL_MapRGB(format: PSDL_PixelFormat; r, g, b: Uint8): LongInt; cdecl; libsdl2;
function SDL_MapRGBA(format: PSDL_PixelFormat; r, g, b, a: Uint8): LongInt; cdecl; libsdl2;
procedure SDL_GetRGB(pixel: Uint32; format: PSDL_PixelFormat; out r, g, b: Uint8); cdecl; libsdl2;
procedure SDL_GetRGBA(pixel: Uint32; format: PSDL_PixelFormat; out r, g, b, a: Uint8); cdecl; libsdl2;
procedure SDL_CalculateGammaRamp(gamma: SDL_Float; out ramp: Uint16); cdecl; libsdl2;

{ SDL_rwops.h }

const
  SDL_RWOPS_UNKNOWN  = 0;   { Unknown stream type }
  SDL_RWOPS_WINFILE  = 1;   { Win32 file }
  SDL_RWOPS_STDFILE  = 2;   { Stdio file }
  SDL_RWOPS_JNIFILE  = 3;   { Android asset }
  SDL_RWOPS_MEMORY = 4;    { Memory stream }
  SDL_RWOPS_MEMORY_RO  = 5; { Read-Only memory stream }

  RW_SEEK_SET  = 0; { Seek from the beginning of data }
  RW_SEEK_CUR  = 1; { Seek relative to current read point }
  RW_SEEK_END  = 2; { Seek relative to the end of data }

type
  PSDL_RWops = ^TSDL_RWops;
  SDL_RWops = packed record
    size: function(context: PSDL_RWops): Sint64; cdecl;
    seek: function(context: PSDL_RWops; offset: Sint64; whence: LongInt): Sint64; cdecl;
    read: function(context: PSDL_RWops; ptr: Pointer; size, maxnum: IntPtr): IntPtr; cdecl;
    write: function(context: PSDL_RWops; ptr: Pointer; size, num: IntPtr): IntPtr; cdecl;
    close: function(context: PSDL_RWops): LongInt; cdecl;
    type_: Uint32;
    { platform variant sections not needed }
  end;
  TSDL_RWops = SDL_RWops;

function SDL_RWFromFile(fileName, mode: SDL_Char): PSDL_RWops; cdecl; libsdl2;
function SDL_RWFromMem(mem: Pointer; size: LongWord): PSDL_RWops; cdecl; libsdl2;
function SDL_AllocRW: PSDL_RWops; cdecl; libsdl2;
procedure SDL_FreeRW(area: PSDL_RWops); cdecl; libsdl2;

{ omitted functions SDL_RWFromFP, SDL_RWFromConstMem }

{ SDL_blendmode.h }

{ SDL_BlendMode }

const
  SDL_BLENDMODE_NONE = $00000000;  { No blending }
  SDL_BLENDMODE_BLEND = $00000001; { dst = (src * A) + (dst * (1-A)) }
  SDL_BLENDMODE_ADD = $00000002;   { dst = (src * A) + dst }
  SDL_BLENDMODE_MOD = $00000004;   { dst = src * dst }

{ SDL_surface.h }

const
  SDL_SWSURFACE       = 0;
  SDL_PREALLOC        = $00000001;
  SDL_RLEACCEL        = $00000002;
  SDL_DONTFREE        = $00000004;

type
  SDL_Surface = packed record
    flags: Uint32; { Read-only }
    format: PSDL_PixelFormat; { Read-only }
    w, h: LongInt; { Read-only }
    pitch: LongInt; { Read-only }
    pixels: Pointer; { Read-write }
    userdata: Pointer; { Read-write application data associated with the surface }
    locked: LongInt; { Read-only }
    lock_data: Pointer; { Read-only }
    clip_rect: TSDL_Rect; { Read-only clipping information }
    map: Pointer; { Private info for fast blit mapping to other surfaces }
    refcount: LongInt; { Read-mostly reference count -- used when freeing surface }
  end;
  TSDL_Surface = SDL_Surface;
  PSDL_Surface = ^TSDL_Surface;

  TSDL_Blit = function(src: PSDL_Surface; var srcrect: TSDL_Rect;
    dst: PSDL_Surface; var dstrect: TSDL_Rect): LongInt; cdecl;

function SDL_CreateRGBSurface(flags: Uint32; width, height, depth: LongInt;
  Rmask, Gmask, Bmask, Amask: Uint32): PSDL_Surface; cdecl; libsdl2;
function SDL_CreateRGBSurfaceFrom(pixels: Pointer; width, height, depth: LongInt;
  Rmask, Gmask, Bmask, Amask: Uint32): PSDL_Surface; cdecl; libsdl2;
procedure SDL_FreeSurface(surface: PSDL_Surface); cdecl; libsdl2;
function SDL_SetSurfacePalette(surface: PSDL_Surface; palette: PSDL_Palette): LongInt; cdecl; libsdl2;
function SDL_LockSurface(surface: PSDL_Surface): LongInt; cdecl; libsdl2;
procedure SDL_UnlockSurface(surface: PSDL_Surface); cdecl; libsdl2;
function SDL_LoadBMP_RW(src: PSDL_RWops; freesrc: LongInt): PSDL_Surface; cdecl; libsdl2;
function SDL_SaveBMP_RW(surface: PSDL_Surface; dst: PSDL_RWops; freedst: LongInt): LongInt; cdecl; libsdl2;
function SDL_SetSurfaceRLE(surface: PSDL_Surface; flag: LongInt): LongInt; cdecl; libsdl2;
function SDL_SetColorKey(surface: PSDL_Surface; flag: LongInt; key: Uint32): LongInt; cdecl; libsdl2;
function SDL_GetColorKey(surface: PSDL_Surface; out key: Uint32): LongInt; cdecl; libsdl2;
function SDL_SetSurfaceColorMod(surface: PSDL_Surface; r, g, b: Uint8): LongInt; cdecl; libsdl2;
function SDL_GetSurfaceColorMod(surface: PSDL_Surface; out r, g, b: Uint8): LongInt; cdecl; libsdl2;
function SDL_SetSurfaceAlphaMod(surface: PSDL_Surface; alpha: Uint8): LongInt; cdecl; libsdl2;
function SDL_GetSurfaceAlphaMod(surface: PSDL_Surface; out alpha: Uint8): LongInt; cdecl; libsdl2;
function SDL_SetSurfaceBlendMode(surface: PSDL_Surface; blendMode: LongInt): LongInt; cdecl; libsdl2;
function SDL_GetSurfaceBlendMode(surface: PSDL_Surface; out blendMode: LongInt): LongInt; cdecl; libsdl2;
function SDL_SetSurfaceClipRect(surface: PSDL_Surface; constref rect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_GetSurfaceClipRect(surface: PSDL_Surface; out rect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_ConvertSurface(src: PSDL_Surface; fmt: PSDL_PixelFormat; flags: Uint32): PSDL_Surface; cdecl; libsdl2;
function SDL_ConvertSurfaceFormat(src: PSDL_Surface; pixel_format: Uint32; flags: Uint32): PSDL_Surface; cdecl; libsdl2;
function SDL_ConvertPixels(width, height: LongInt; src_format: Uint32; src: Pointer;
  src_pitch: LongInt; dst_format: Uint32; dst: Pointer; dst_pitch: LongInt): LongInt; cdecl; libsdl2;
function SDL_FillRect(dst: PSDL_Surface; constref rect: TSDL_Rect; color: Uint32): LongInt; cdecl; libsdl2;
function SDL_FillRects(dst: PSDL_Surface; var rects: TSDL_Rect; count: LongInt; color: Uint32): LongInt; cdecl; libsdl2;
{ These functions might need to be reviewed as SDL_surface.h currently mixes
  const and var rect arguments. I suspect the final version may fix this }
function SDL_UpperBlit(src: PSDL_Surface; constref srcrect: TSDL_Rect; dst: PSDL_Surface; var dstrect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_LowerBlit(src: PSDL_Surface; var srcrect: TSDL_Rect; dst: PSDL_Surface; var dstrect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_SoftStretch(src: PSDL_Surface; constref srcrect: TSDL_Rect; dst: PSDL_Surface; constref dstrect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_UpperBlitScaled(src: PSDL_Surface; var srcrect: TSDL_Rect; dst: PSDL_Surface; var dstrect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_LowerBlitScaled(src: PSDL_Surface; var srcrect: TSDL_Rect; dst: PSDL_Surface; var dstrect: TSDL_Rect): LongInt; cdecl; libsdl2;

{ SDL_video.h }

{ SDL_WindowFlags }

const
  SDL_WINDOW_FULLSCREEN = $00000001;         { fullscreen window }
  SDL_WINDOW_OPENGL = $00000002;             { window usable with OpenGL context }
  SDL_WINDOW_SHOWN = $00000004;              { window is visible }
  SDL_WINDOW_HIDDEN = $00000008;             { window is not visible }
  SDL_WINDOW_BORDERLESS = $00000010;         { no window decoration }
  SDL_WINDOW_RESIZABLE = $00000020;          { window can be resized }
  SDL_WINDOW_MINIMIZED = $00000040;          { window is minimized }
  SDL_WINDOW_MAXIMIZED = $00000080;          { window is maximized }
  SDL_WINDOW_INPUT_GRABBED = $00000100;      { window has grabbed input focus }
  SDL_WINDOW_INPUT_FOCUS = $00000200;        { window has input focus }
  SDL_WINDOW_MOUSE_FOCUS = $00000400;        { window has mouse focus }
  SDL_WINDOW_FULLSCREEN_DESKTOP = SDL_WINDOW_FULLSCREEN or $00001000;
  SDL_WINDOW_FOREIGN = $00000800;            { window not created by SDL }

  SDL_WINDOWPOS_UNDEFINED = LongInt($1FFF0000);
  SDL_WINDOWPOS_CENTERED = LongInt($2FFF0000);

{ SDL_WindowEventID }

  SDL_WINDOWEVENT_NONE = 0;
  SDL_WINDOWEVENT_SHOWN = SDL_WINDOWEVENT_NONE + 1;
  SDL_WINDOWEVENT_HIDDEN = SDL_WINDOWEVENT_SHOWN + 1;
  SDL_WINDOWEVENT_EXPOSED = SDL_WINDOWEVENT_HIDDEN + 1;
  SDL_WINDOWEVENT_MOVED = SDL_WINDOWEVENT_EXPOSED + 1;
  SDL_WINDOWEVENT_RESIZED = SDL_WINDOWEVENT_MOVED + 1;
  SDL_WINDOWEVENT_SIZE_CHANGED = SDL_WINDOWEVENT_RESIZED + 1;
  SDL_WINDOWEVENT_MINIMIZED = SDL_WINDOWEVENT_SIZE_CHANGED + 1;
  SDL_WINDOWEVENT_MAXIMIZED = SDL_WINDOWEVENT_MINIMIZED + 1;
  SDL_WINDOWEVENT_RESTORED = SDL_WINDOWEVENT_MAXIMIZED + 1;
  SDL_WINDOWEVENT_ENTER = SDL_WINDOWEVENT_RESTORED + 1;
  SDL_WINDOWEVENT_LEAVE = SDL_WINDOWEVENT_ENTER + 1;
  SDL_WINDOWEVENT_FOCUS_GAINED = SDL_WINDOWEVENT_LEAVE + 1;
  SDL_WINDOWEVENT_FOCUS_LOST = SDL_WINDOWEVENT_FOCUS_GAINED + 1;
  SDL_WINDOWEVENT_CLOSE = SDL_WINDOWEVENT_FOCUS_LOST + 1;

{ SDL_GLattr }

  SDL_GL_RED_SIZE = 0;
  SDL_GL_GREEN_SIZE = SDL_GL_RED_SIZE + 1;
  SDL_GL_BLUE_SIZE = SDL_GL_GREEN_SIZE + 1;
  SDL_GL_ALPHA_SIZE = SDL_GL_BLUE_SIZE + 1;
  SDL_GL_BUFFER_SIZE = SDL_GL_ALPHA_SIZE + 1;
  SDL_GL_DOUBLEBUFFER = SDL_GL_BUFFER_SIZE + 1;
  SDL_GL_DEPTH_SIZE = SDL_GL_DOUBLEBUFFER + 1;
  SDL_GL_STENCIL_SIZE = SDL_GL_DEPTH_SIZE + 1;
  SDL_GL_ACCUM_RED_SIZE = SDL_GL_STENCIL_SIZE + 1;
  SDL_GL_ACCUM_GREEN_SIZE = SDL_GL_ACCUM_RED_SIZE + 1;
  SDL_GL_ACCUM_BLUE_SIZE = SDL_GL_ACCUM_GREEN_SIZE + 1;
  SDL_GL_ACCUM_ALPHA_SIZE = SDL_GL_ACCUM_BLUE_SIZE + 1;
  SDL_GL_STEREO = SDL_GL_ACCUM_ALPHA_SIZE + 1;
  SDL_GL_MULTISAMPLEBUFFERS = SDL_GL_STEREO + 1;
  SDL_GL_MULTISAMPLESAMPLES = SDL_GL_MULTISAMPLEBUFFERS + 1;
  SDL_GL_ACCELERATED_VISUAL = SDL_GL_MULTISAMPLESAMPLES + 1;
  SDL_GL_RETAINED_BACKING = SDL_GL_ACCELERATED_VISUAL + 1;
  SDL_GL_CONTEXT_MAJOR_VERSION = SDL_GL_RETAINED_BACKING + 1;
  SDL_GL_CONTEXT_MINOR_VERSION = SDL_GL_CONTEXT_MAJOR_VERSION + 1;
  SDL_GL_CONTEXT_EGL = SDL_GL_CONTEXT_MINOR_VERSION + 1;
  SDL_GL_CONTEXT_FLAGS = SDL_GL_CONTEXT_EGL + 1;
  SDL_GL_CONTEXT_PROFILE_MASK = SDL_GL_CONTEXT_FLAGS + 1;
  SDL_GL_SHARE_WITH_CURRENT_CONTEXT = SDL_GL_CONTEXT_PROFILE_MASK + 1;

{ SDL_GLprofile }

  SDL_GL_CONTEXT_PROFILE_CORE           = $0001;
  SDL_GL_CONTEXT_PROFILE_COMPATIBILITY  = $0002;
  SDL_GL_CONTEXT_PROFILE_ES             = $0004;

{ SDL_GLcontextFlag }

  SDL_GL_CONTEXT_DEBUG_FLAG              = $0001;
  SDL_GL_CONTEXT_FORWARD_COMPATIBLE_FLAG = $0002;
  SDL_GL_CONTEXT_ROBUST_ACCESS_FLAG      = $0004;
  SDL_GL_CONTEXT_RESET_ISOLATION_FLAG    = $0008;

type
  SDL_DisplayMode = packed record
    format: Uint32;                { pixel format }
    w: LongInt;                    { width }
    h: LongInt;                    { height }
    refresh_rate: LongInt;         { refresh rate (or zero for unspecified) }
    driverdata: Pointer;           { driver-specific data, initialize to 0 }
  end;
  TSDL_DisplayMode = SDL_DisplayMode;
  PSDL_DisplayMode = ^TSDL_DisplayMode;

function SDL_GetNumVideoDrivers: LongInt; cdecl; libsdl2;
function SDL_GetVideoDriver(index: LongInt): SDL_Char; cdecl; libsdl2;
function SDL_VideoInit(driver_name: SDL_Char): LongInt; cdecl; libsdl2;
procedure SDL_VideoQuit; cdecl; libsdl2;
function SDL_GetCurrentVideoDriver: SDL_Char; cdecl; libsdl2;
function SDL_GetNumVideoDisplays: LongInt; cdecl; libsdl2;
function SDL_GetDisplayName(displayIndex: LongInt): SDL_Char; cdecl; libsdl2;
function SDL_GetDisplayBounds(displayIndex: LongInt; out rect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_GetNumDisplayModes(displayIndex: LongInt): LongInt; cdecl; libsdl2;
function SDL_GetDisplayMode(displayIndex, modeIndex: LongInt; out mode: TSDL_DisplayMode): LongInt; cdecl; libsdl2;
function SDL_GetDesktopDisplayMode(displayIndex: LongInt; out mode: TSDL_DisplayMode): LongInt; cdecl; libsdl2;
function SDL_GetCurrentDisplayMode(displayIndex: LongInt; out mode: TSDL_DisplayMode): LongInt; cdecl; libsdl2;
function SDL_GetClosestDisplayMode(displayIndex: LongInt; constref mode: TSDL_DisplayMode;
  out closest: TSDL_DisplayMode): LongInt; cdecl; libsdl2;
function SDL_GetWindowDisplayIndex(window: PSDL_Window): LongInt; cdecl; libsdl2;
function SDL_SetWindowDisplayMode(window: PSDL_Window; constref mode: TSDL_DisplayMode): LongInt; cdecl; libsdl2;
function SDL_GetWindowDisplayMode(window: PSDL_Window; out mode: TSDL_DisplayMode): LongInt; cdecl; libsdl2;
function SDL_GetWindowPixelFormat(window: PSDL_Window): Uint32; cdecl; libsdl2;
function SDL_CreateWindow(title: SDL_Char; x, y, w, h: LongInt; flags: Uint32): PSDL_Window; cdecl; libsdl2;
function SDL_CreateWindowFrom(data: Pointer): PSDL_Window; cdecl; libsdl2;
function SDL_GetWindowID(window: PSDL_Window): Uint32; cdecl; libsdl2;
function SDL_GetWindowFromID(id: Uint32): PSDL_Window; cdecl; libsdl2;
function SDL_GetWindowFlags(window: PSDL_Window): Uint32; cdecl; libsdl2;
procedure SDL_SetWindowTitle(window: PSDL_Window; title: SDL_Char); cdecl; libsdl2;
function SDL_GetWindowTitle(window: PSDL_Window): SDL_Char; cdecl; libsdl2;
procedure SDL_SetWindowIcon(window: PSDL_Window; icon: PSDL_Surface); cdecl; libsdl2;
function SDL_SetWindowData(window: PSDL_Window; name: SDL_Char; userdata: Pointer): Pointer; cdecl; libsdl2;
function SDL_GetWindowData(window: PSDL_Window; name: SDL_Char): Pointer; cdecl; libsdl2;
procedure SDL_SetWindowPosition(window: PSDL_Window; x, y: LongInt); cdecl; libsdl2;
procedure SDL_GetWindowPosition(window: PSDL_Window; out x, y: LongInt); cdecl; libsdl2;
procedure SDL_SetWindowSize(window: PSDL_Window; w, h: LongInt); cdecl; libsdl2;
procedure SDL_GetWindowSize(window: PSDL_Window; out w, h: LongInt); cdecl; libsdl2;
procedure SDL_SetWindowMinimumSize(window: PSDL_Window; w, h: LongInt); cdecl; libsdl2;
procedure SDL_GetWindowMinimumSize(window: PSDL_Window; out w, h: LongInt); cdecl; libsdl2;
procedure SDL_SetWindowMaximumSize(window: PSDL_Window; w, h: LongInt); cdecl; libsdl2;
procedure SDL_GetWindowMaximumSize(window: PSDL_Window; out w, h: LongInt); cdecl; libsdl2;
procedure SDL_SetWindowBordered(window: PSDL_Window; bordered: SDL_Bool); cdecl; libsdl2;
procedure SDL_ShowWindow(window: PSDL_Window); cdecl; libsdl2;
procedure SDL_HideWindow(window: PSDL_Window); cdecl; libsdl2;
procedure SDL_RaiseWindow(window: PSDL_Window); cdecl; libsdl2;
procedure SDL_MaximizeWindow(window: PSDL_Window); cdecl; libsdl2;
procedure SDL_MinimizeWindow(window: PSDL_Window); cdecl; libsdl2;
procedure SDL_RestoreWindow(window: PSDL_Window); cdecl; libsdl2;
function SDL_SetWindowFullscreen(window: PSDL_Window; fullscreen: SDL_Bool): LongInt; cdecl; libsdl2;
function SDL_GetWindowSurface(window: PSDL_Window): PSDL_Surface; cdecl; libsdl2;
function SDL_UpdateWindowSurface(window: PSDL_Window): LongInt; cdecl; libsdl2;
function SDL_UpdateWindowSurfaceRects(window: PSDL_Window; var rects: TSDL_Rect; numrects: LongInt): LongInt; cdecl; libsdl2;
procedure SDL_SetWindowGrab(window: PSDL_Window; grabbed: SDL_Bool); cdecl; libsdl2;
function SDL_GetWindowGrab(window: PSDL_Window): SDL_Bool; cdecl; libsdl2;
procedure SDL_SetWindowBrightness(window: PSDL_Window; brightness: SDL_Float); cdecl; libsdl2;
function SDL_GetWindowBrightness(window: PSDL_Window): SDL_Float; cdecl; libsdl2;
function SDL_SetWindowGammaRamp(window: PSDL_Window; var red, green, blue: Uint16): SDL_Float; cdecl; libsdl2;
function SDL_GetWindowGammaRamp(window: PSDL_Window; out red, green, blue: Uint16): SDL_Float; cdecl; libsdl2;
procedure SDL_DestroyWindow(window: PSDL_Window); cdecl; libsdl2;
function SDL_IsScreenSaverEnabled: SDL_Bool; cdecl; libsdl2;
procedure SDL_EnableScreenSaver; cdecl; libsdl2;
procedure SDL_DisableScreenSaver; cdecl; libsdl2;

{ OpenGL support }

function SDL_GL_LoadLibrary(path: SDL_Char): LongInt; cdecl; libsdl2;
function SDL_GL_GetProcAddress(proc: SDL_Char): Pointer; cdecl; libsdl2;
procedure SDL_GL_UnloadLibrary; cdecl; libsdl2;
function SDL_GL_ExtensionSupported(extension: SDL_Char): SDL_Bool; cdecl; libsdl2;
function SDL_GL_SetAttribute(attr, value: LongInt): LongInt; cdecl; libsdl2;
function SDL_GL_GetAttribute(attr: LongInt; out value: LongInt): LongInt; cdecl; libsdl2;
function SDL_GL_CreateContext(window: PSDL_Window): PSDL_GLContext; cdecl; libsdl2;
function SDL_GL_MakeCurrent(window: PSDL_Window; context: PSDL_GLContext): LongInt; cdecl; libsdl2;
function SDL_GL_SetSwapInterval(interval: LongInt): LongInt; cdecl; libsdl2;
function SDL_GL_GetSwapInterval: LongInt; cdecl; libsdl2;
procedure SDL_GL_SwapWindow(window: PSDL_Window); cdecl; libsdl2;
procedure SDL_GL_DeleteContext(context: PSDL_GLContext); cdecl; libsdl2;

{ SDL_shape.h }

const
  SDL_NONSHAPEABLE_WINDOW = -1;
  SDL_INVALID_SHAPE_ARGUMENT = -2;
  SDL_WINDOW_LACKS_SHAPE = -3;

{ WindowShapeMode }

  ShapeModeDefault = 0;
  ShapeModeBinarizeAlpha = ShapeModeDefault + 1;
  ShapeModeReverseBinarizeAlpha = ShapeModeBinarizeAlpha + 1;
  ShapeModeColorKey = ShapeModeReverseBinarizeAlpha + 1;

type
  SDL_WindowShapeParams = packed record
    case Integer of
      0: (binarizationCutoff: Uint8);
      1: (colorKey: TSDL_Color);
  end;
  TSDL_WindowShapeParams = SDL_WindowShapeParams;
  PSDL_WindowShapeParams = ^TSDL_WindowShapeParams;

  SDL_WindowShapeMode = packed record
    mode: Uint32;
    parameters: TSDL_WindowShapeParams;
  end;
  TSDL_WindowShapeMode = SDL_WindowShapeMode;
  PSDL_WindowShapeMode = ^TSDL_WindowShapeMode;

function SDL_CreateShapedWindow(title: SDL_Char; x, y, w, h: LongWord; flags: Uint32): PSDL_Window; cdecl; libsdl2;
function SDL_IsShapedWindow(window: PSDL_Window): SDL_Bool; cdecl; libsdl2;
function SDL_SetWindowShape(window: PSDL_Window; shape: PSDL_Surface; var mode: TSDL_WindowShapeMode): LongInt; cdecl; libsdl2;
function SDL_GetShapedWindowMode(window: PSDL_Window; var mode: TSDL_WindowShapeMode): LongInt; cdecl; libsdl2;

{ SDL_render.h }

{ SDL_RendererFlags }

const
  SDL_RENDERER_SOFTWARE = $00000001;
  SDL_RENDERER_ACCELERATED = $00000002;
  SDL_RENDERER_PRESENTVSYNC = $00000004;
  SDL_RENDERER_TARGETTEXTURE = $00000008;

{ SDL_TextureAccess }

  SDL_TEXTUREACCESS_STATIC = 0;
  SDL_TEXTUREACCESS_STREAMING = SDL_TEXTUREACCESS_STATIC + 1;
  SDL_TEXTUREACCESS_TARGET = SDL_TEXTUREACCESS_STREAMING + 1;

{ SDL_TextureModulate }

  SDL_TEXTUREMODULATE_NONE = $00000000;
  SDL_TEXTUREMODULATE_COLOR = $00000001;
  SDL_TEXTUREMODULATE_ALPHA = $00000002;

{ SDL_RendererFlip }

  SDL_FLIP_NONE = $00000000;
  SDL_FLIP_HORIZONTAL = $00000001;
  SDL_FLIP_VERTICAL = $00000002;

type
  SDL_RendererInfo = packed record
    name: SDL_Char;
    flags: Uint32;
    num_texture_formats: array[0..15] of Uint32;
    texture_formats: Uint32;
    max_texture_width: LongInt;
    max_texture_height: LongInt;
  end;
  TSDL_RendererInfo = SDL_RendererInfo;
  PSDL_RendererInfo = ^TSDL_RendererInfo;

  PSDL_Renderer = Pointer;
  PSDL_Texture = Pointer;

function SDL_GetNumRenderDrivers: LongInt; cdecl; libsdl2;
function SDL_GetRenderDriverInfo(index: LongInt; var info: TSDL_RendererInfo): LongInt; cdecl; libsdl2;
function SDL_CreateWindowAndRenderer(width, height: LongInt; window_flags: Uint32;
  out window: PSDL_Window; out renderer: PSDL_Renderer): LongInt; cdecl; libsdl2;
function SDL_CreateRenderer(window: PSDL_Window; index: LongInt; flags: Uint32): PSDL_Renderer; cdecl; libsdl2;
function SDL_CreateSoftwareRenderer(surface: PSDL_Surface): PSDL_Renderer; cdecl; libsdl2;
function SDL_GetRenderer(window: PSDL_Window): PSDL_Renderer; cdecl; libsdl2;
function SDL_GetRendererInfo(renderer: PSDL_Renderer; out info: TSDL_RendererInfo): LongInt; cdecl; libsdl2;
function SDL_CreateTexture(renderer: PSDL_Renderer; format: Uint32; access, w, h: LongInt): PSDL_Texture; cdecl; libsdl2;
function SDL_CreateTextureFromSurface(renderer: PSDL_Renderer; surface: PSDL_Surface): PSDL_Texture; cdecl; libsdl2;
function SDL_QueryTexture(texture: PSDL_Texture; format: Uint32; out access, w, h: LongInt): LongInt; cdecl; libsdl2;
function SDL_SetTextureColorMod(texture: PSDL_Texture; r, g, b: Uint8): LongInt; cdecl; libsdl2;
function SDL_GetTextureColorMod(texture: PSDL_Texture; out r, g, b: Uint8): LongInt; cdecl; libsdl2;
function SDL_SetTextureAlphaMod(texture: PSDL_Texture; alpha: Uint8): LongInt; cdecl; libsdl2;
function SDL_GetTextureAlphaMod(texture: PSDL_Texture; out alpha: Uint8): LongInt; cdecl; libsdl2;
function SDL_SetTextureBlendMode(texture: PSDL_Texture; blendMode: LongInt): LongInt; cdecl; libsdl2;
function SDL_GetTextureBlendMode(texture: PSDL_Texture; out blendMode: LongInt): LongInt; cdecl; libsdl2;
function SDL_UpdateTexture(texture: PSDL_Texture; constref rect: TSDL_Rect; pixels: Pointer; pitch: LongInt): LongInt; cdecl; libsdl2;
function SDL_LockTexture(texture: PSDL_Texture; constref rect: TSDL_Rect; out pixels: Pointer; out pitch: LongInt): LongInt; cdecl; libsdl2;
procedure SDL_UnlockTexture(texture: PSDL_Texture); cdecl; libsdl2;
function SDL_RenderTargetSupported(texture: PSDL_Texture): SDL_Bool; cdecl; libsdl2;
function SDL_SetRenderTarget(renderer: PSDL_Renderer; texture: PSDL_Texture): LongInt; cdecl; libsdl2;
function SDL_GetRenderTarget(renderer: PSDL_Renderer): PSDL_Texture; cdecl; libsdl2;
function SDL_RenderSetLogicalSize(renderer: PSDL_Renderer; w, h: LongInt): LongInt; cdecl; libsdl2;
procedure SDL_RenderGetLogicalSize(renderer: PSDL_Renderer; out w, h: LongInt); cdecl; libsdl2;
function SDL_RenderSetViewport(renderer: PSDL_Renderer; constref rect: TSDL_Rect): LongInt; cdecl; libsdl2;
procedure SDL_RenderGetViewport(renderer: PSDL_Renderer; out rect: TSDL_Rect); cdecl; libsdl2;
function SDL_RenderSetScale(renderer: PSDL_Renderer; scaleX, scaleY: SDL_Float): LongInt; cdecl; libsdl2;
procedure SDL_RenderGetScale(renderer: PSDL_Renderer; out scaleX, scaleY: SDL_Float); cdecl; libsdl2;
function SDL_SetRenderDrawColor(renderer: PSDL_Renderer; r, g, b, a: Uint8): LongInt; cdecl; libsdl2;
function SDL_GetRenderDrawColor(renderer: PSDL_Renderer; out r, g, b, a: Uint8): LongInt; cdecl; libsdl2;
function SDL_SetRenderDrawBlendMode(renderer: PSDL_Renderer; blendMode: LongInt): LongInt; cdecl; libsdl2;
function SDL_GetRenderDrawBlendMode(renderer: PSDL_Renderer; out blendMode: LongInt): LongInt; cdecl; libsdl2;
function SDL_RenderClear(renderer: PSDL_Renderer): LongInt; cdecl; libsdl2;
function SDL_RenderDrawPoint(renderer: PSDL_Renderer; x, y: LongInt): LongInt; cdecl; libsdl2;
function SDL_RenderDrawPoints(renderer: PSDL_Renderer; var points: TSDL_Point; count: LongInt): LongInt; cdecl; libsdl2;
function SDL_RenderDrawLine(renderer: PSDL_Renderer; x1, y1, x2, y2: LongInt): LongInt; cdecl; libsdl2;
function SDL_RenderDrawLines(renderer: PSDL_Renderer; var points: TSDL_Point; count: LongInt): LongInt; cdecl; libsdl2;
function SDL_RenderDrawRect(renderer: PSDL_Renderer; constref rect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_RenderDrawRects(renderer: PSDL_Renderer; var rect: TSDL_Rect; count: LongInt): LongInt; cdecl; libsdl2;
function SDL_RenderFillRect(renderer: PSDL_Renderer; constref rect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_RenderFillRects(renderer: PSDL_Renderer; var rect: TSDL_Rect; count: LongInt): LongInt; cdecl; libsdl2;
function SDL_RenderCopy(renderer: PSDL_Renderer; texture: PSDL_Texture; constref srcrect, dstrect: TSDL_Rect): LongInt; cdecl; libsdl2;
function SDL_RenderCopyEx(renderer: PSDL_Renderer; texture: PSDL_Texture;
  constref srcrect, dstrect: TSDL_Rect; angle: SDL_Double; constref center: TSDL_Point;
  flip: Uint32): LongInt; cdecl; libsdl2;
function SDL_RenderReadPixels(renderer: PSDL_Renderer; constref rect: TSDL_Rect;
  format: Uint32; pixels: Pointer; pitch: LongInt): LongInt; cdecl; libsdl2;
procedure SDL_RenderPresent(renderer: PSDL_Renderer); cdecl; libsdl2;
procedure SDL_DestroyTexture(texture: PSDL_Texture); cdecl; libsdl2;
procedure SDL_DestroyRenderer(renderer: PSDL_Renderer); cdecl; libsdl2;
function SDL_GL_BindTexture(texture: PSDL_Texture; out texw, texh: SDL_Float): LongInt; cdecl; libsdl2;
function SDL_GL_UnbindTexture(texture: PSDL_Texture): LongInt; cdecl; libsdl2;

{ SDL_messagebox.h }

{ SDL_MessageBoxFlags }

const
  SDL_MESSAGEBOX_ERROR        = $00000010;
  SDL_MESSAGEBOX_WARNING      = $00000020;
  SDL_MESSAGEBOX_INFORMATION  = $00000040;

{ SDL_MessageBoxButtonFlags }

  SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT = $00000001;
  SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT = $00000002;

{ SDL_MessageBoxColorType }

  SDL_MESSAGEBOX_COLOR_BACKGROUND = 0;
  SDL_MESSAGEBOX_COLOR_TEXT = SDL_MESSAGEBOX_COLOR_BACKGROUND + 1;
  SDL_MESSAGEBOX_COLOR_BUTTON_BORDER = SDL_MESSAGEBOX_COLOR_TEXT + 1;
  SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND = SDL_MESSAGEBOX_COLOR_BUTTON_BORDER + 1;
  SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED = SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND + 1;
  SDL_MESSAGEBOX_COLOR_MAX = SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED + 1;

type
  SDL_MessageBoxButtonData = packed record
    flags: Uint32;
    buttonid: LongInt;
    text: SDL_Char;
  end;
  TSDL_MessageBoxButtonData = SDL_MessageBoxButtonData;
  PSDL_MessageBoxButtonData = ^TSDL_MessageBoxButtonData;

  SDL_MessageBoxColor =  packed record
    r, g, b: Uint8;
  end;
  TSDL_MessageBoxColor = SDL_MessageBoxColor;
  PSDL_MessageBoxColor = ^TSDL_MessageBoxColor;

  SDL_MessageBoxColorScheme = packed record
    colors: packed array[0..SDL_MESSAGEBOX_COLOR_MAX - 1] of SDL_MessageBoxColor;
  end;
  TSDL_MessageBoxColorScheme = SDL_MessageBoxColorScheme;
  PSDL_MessageBoxColorScheme = ^TSDL_MessageBoxColorScheme;

  SDL_MessageBoxData = packed record
    flags: Uint32;
    a: Uint32;
    window: PSDL_Window;
    title: SDL_Char;
    message: SDL_Char;
    numbuttons: LongInt;
    b: Uint32;
    buttons: PSDL_MessageBoxButtonData;
    colorScheme: PSDL_MessageBoxColorScheme;
  end;
  TSDL_MessageBoxData = SDL_MessageBoxData;
  PSDL_MessageBoxData = ^SDL_MessageBoxData;

function SDL_ShowMessageBox(constref messageboxdata: TSDL_MessageBoxData; out buttonid: LongInt): LongInt; cdecl; libsdl2;
function SDL_ShowSimpleMessageBox(flags: Uint32; title, message: SDL_Char; window: PSDL_Window): LongInt; cdecl; libsdl2;

{ SDL_keycode.h }

{ SDL_Scancode }

const
  SDL_SCANCODE_UNKNOWN = 0;
  SDL_SCANCODE_A = 4;
  SDL_SCANCODE_B = 5;
  SDL_SCANCODE_C = 6;
  SDL_SCANCODE_D = 7;
  SDL_SCANCODE_E = 8;
  SDL_SCANCODE_F = 9;
  SDL_SCANCODE_G = 10;
  SDL_SCANCODE_H = 11;
  SDL_SCANCODE_I = 12;
  SDL_SCANCODE_J = 13;
  SDL_SCANCODE_K = 14;
  SDL_SCANCODE_L = 15;
  SDL_SCANCODE_M = 16;
  SDL_SCANCODE_N = 17;
  SDL_SCANCODE_O = 18;
  SDL_SCANCODE_P = 19;
  SDL_SCANCODE_Q = 20;
  SDL_SCANCODE_R = 21;
  SDL_SCANCODE_S = 22;
  SDL_SCANCODE_T = 23;
  SDL_SCANCODE_U = 24;
  SDL_SCANCODE_V = 25;
  SDL_SCANCODE_W = 26;
  SDL_SCANCODE_X = 27;
  SDL_SCANCODE_Y = 28;
  SDL_SCANCODE_Z = 29;
  SDL_SCANCODE_1 = 30;
  SDL_SCANCODE_2 = 31;
  SDL_SCANCODE_3 = 32;
  SDL_SCANCODE_4 = 33;
  SDL_SCANCODE_5 = 34;
  SDL_SCANCODE_6 = 35;
  SDL_SCANCODE_7 = 36;
  SDL_SCANCODE_8 = 37;
  SDL_SCANCODE_9 = 38;
  SDL_SCANCODE_0 = 39;
  SDL_SCANCODE_RETURN = 40;
  SDL_SCANCODE_ESCAPE = 41;
  SDL_SCANCODE_BACKSPACE = 42;
  SDL_SCANCODE_TAB = 43;
  SDL_SCANCODE_SPACE = 44;
  SDL_SCANCODE_MINUS = 45;
  SDL_SCANCODE_EQUALS = 46;
  SDL_SCANCODE_LEFTBRACKET = 47;
  SDL_SCANCODE_RIGHTBRACKET = 48;
  SDL_SCANCODE_BACKSLASH = 49;
  SDL_SCANCODE_NONUSHASH = 50;
  SDL_SCANCODE_SEMICOLON = 51;
  SDL_SCANCODE_APOSTROPHE = 52;
  SDL_SCANCODE_GRAVE = 53;
  SDL_SCANCODE_COMMA = 54;
  SDL_SCANCODE_PERIOD = 55;
  SDL_SCANCODE_SLASH = 56;
  SDL_SCANCODE_CAPSLOCK = 57;
  SDL_SCANCODE_F1 = 58;
  SDL_SCANCODE_F2 = 59;
  SDL_SCANCODE_F3 = 60;
  SDL_SCANCODE_F4 = 61;
  SDL_SCANCODE_F5 = 62;
  SDL_SCANCODE_F6 = 63;
  SDL_SCANCODE_F7 = 64;
  SDL_SCANCODE_F8 = 65;
  SDL_SCANCODE_F9 = 66;
  SDL_SCANCODE_F10 = 67;
  SDL_SCANCODE_F11 = 68;
  SDL_SCANCODE_F12 = 69;
  SDL_SCANCODE_PRINTSCREEN = 70;
  SDL_SCANCODE_SCROLLLOCK = 71;
  SDL_SCANCODE_PAUSE = 72;
  SDL_SCANCODE_INSERT = 73;
  SDL_SCANCODE_HOME = 74;
  SDL_SCANCODE_PAGEUP = 75;
  SDL_SCANCODE_DELETE = 76;
  SDL_SCANCODE_END = 77;
  SDL_SCANCODE_PAGEDOWN = 78;
  SDL_SCANCODE_RIGHT = 79;
  SDL_SCANCODE_LEFT = 80;
  SDL_SCANCODE_DOWN = 81;
  SDL_SCANCODE_UP = 82;
  SDL_SCANCODE_NUMLOCKCLEAR = 83;
  SDL_SCANCODE_KP_DIVIDE = 84;
  SDL_SCANCODE_KP_MULTIPLY = 85;
  SDL_SCANCODE_KP_MINUS = 86;
  SDL_SCANCODE_KP_PLUS = 87;
  SDL_SCANCODE_KP_ENTER = 88;
  SDL_SCANCODE_KP_1 = 89;
  SDL_SCANCODE_KP_2 = 90;
  SDL_SCANCODE_KP_3 = 91;
  SDL_SCANCODE_KP_4 = 92;
  SDL_SCANCODE_KP_5 = 93;
  SDL_SCANCODE_KP_6 = 94;
  SDL_SCANCODE_KP_7 = 95;
  SDL_SCANCODE_KP_8 = 96;
  SDL_SCANCODE_KP_9 = 97;
  SDL_SCANCODE_KP_0 = 98;
  SDL_SCANCODE_KP_PERIOD = 99;
  SDL_SCANCODE_NONUSBACKSLASH = 100;
  SDL_SCANCODE_APPLICATION = 101;
  SDL_SCANCODE_POWER = 102;
  SDL_SCANCODE_KP_EQUALS = 103;
  SDL_SCANCODE_F13 = 104;
  SDL_SCANCODE_F14 = 105;
  SDL_SCANCODE_F15 = 106;
  SDL_SCANCODE_F16 = 107;
  SDL_SCANCODE_F17 = 108;
  SDL_SCANCODE_F18 = 109;
  SDL_SCANCODE_F19 = 110;
  SDL_SCANCODE_F20 = 111;
  SDL_SCANCODE_F21 = 112;
  SDL_SCANCODE_F22 = 113;
  SDL_SCANCODE_F23 = 114;
  SDL_SCANCODE_F24 = 115;
  SDL_SCANCODE_EXECUTE = 116;
  SDL_SCANCODE_HELP = 117;
  SDL_SCANCODE_MENU = 118;
  SDL_SCANCODE_SELECT = 119;
  SDL_SCANCODE_STOP = 120;
  SDL_SCANCODE_AGAIN = 121;
  SDL_SCANCODE_UNDO = 122;
  SDL_SCANCODE_CUT = 123;
  SDL_SCANCODE_COPY = 124;
  SDL_SCANCODE_PASTE = 125;
  SDL_SCANCODE_FIND = 126;
  SDL_SCANCODE_MUTE = 127;
  SDL_SCANCODE_VOLUMEUP = 128;
  SDL_SCANCODE_VOLUMEDOWN = 129;
  SDL_SCANCODE_KP_COMMA = 133;
  SDL_SCANCODE_KP_EQUALSAS400 = 134;
  SDL_SCANCODE_INTERNATIONAL1 = 135;
  SDL_SCANCODE_INTERNATIONAL2 = 136;
  SDL_SCANCODE_INTERNATIONAL3 = 137;
  SDL_SCANCODE_INTERNATIONAL4 = 138;
  SDL_SCANCODE_INTERNATIONAL5 = 139;
  SDL_SCANCODE_INTERNATIONAL6 = 140;
  SDL_SCANCODE_INTERNATIONAL7 = 141;
  SDL_SCANCODE_INTERNATIONAL8 = 142;
  SDL_SCANCODE_INTERNATIONAL9 = 143;
  SDL_SCANCODE_LANG1 = 144;
  SDL_SCANCODE_LANG2 = 145;
  SDL_SCANCODE_LANG3 = 146;
  SDL_SCANCODE_LANG4 = 147;
  SDL_SCANCODE_LANG5 = 148;
  SDL_SCANCODE_LANG6 = 149;
  SDL_SCANCODE_LANG7 = 150;
  SDL_SCANCODE_LANG8 = 151;
  SDL_SCANCODE_LANG9 = 152;
  SDL_SCANCODE_ALTERASE = 153;
  SDL_SCANCODE_SYSREQ = 154;
  SDL_SCANCODE_CANCEL = 155;
  SDL_SCANCODE_CLEAR = 156;
  SDL_SCANCODE_PRIOR = 157;
  SDL_SCANCODE_RETURN2 = 158;
  SDL_SCANCODE_SEPARATOR = 159;
  SDL_SCANCODE_OUT = 160;
  SDL_SCANCODE_OPER = 161;
  SDL_SCANCODE_CLEARAGAIN = 162;
  SDL_SCANCODE_CRSEL = 163;
  SDL_SCANCODE_EXSEL = 164;
  SDL_SCANCODE_KP_00 = 176;
  SDL_SCANCODE_KP_000 = 177;
  SDL_SCANCODE_THOUSANDSSEPARATOR = 178;
  SDL_SCANCODE_DECIMALSEPARATOR = 179;
  SDL_SCANCODE_CURRENCYUNIT = 180;
  SDL_SCANCODE_CURRENCYSUBUNIT = 181;
  SDL_SCANCODE_KP_LEFTPAREN = 182;
  SDL_SCANCODE_KP_RIGHTPAREN = 183;
  SDL_SCANCODE_KP_LEFTBRACE = 184;
  SDL_SCANCODE_KP_RIGHTBRACE = 185;
  SDL_SCANCODE_KP_TAB = 186;
  SDL_SCANCODE_KP_BACKSPACE = 187;
  SDL_SCANCODE_KP_A = 188;
  SDL_SCANCODE_KP_B = 189;
  SDL_SCANCODE_KP_C = 190;
  SDL_SCANCODE_KP_D = 191;
  SDL_SCANCODE_KP_E = 192;
  SDL_SCANCODE_KP_F = 193;
  SDL_SCANCODE_KP_XOR = 194;
  SDL_SCANCODE_KP_POWER = 195;
  SDL_SCANCODE_KP_PERCENT = 196;
  SDL_SCANCODE_KP_LESS = 197;
  SDL_SCANCODE_KP_GREATER = 198;
  SDL_SCANCODE_KP_AMPERSAND = 199;
  SDL_SCANCODE_KP_DBLAMPERSAND = 200;
  SDL_SCANCODE_KP_VERTICALBAR = 201;
  SDL_SCANCODE_KP_DBLVERTICALBAR = 202;
  SDL_SCANCODE_KP_COLON = 203;
  SDL_SCANCODE_KP_HASH = 204;
  SDL_SCANCODE_KP_SPACE = 205;
  SDL_SCANCODE_KP_AT = 206;
  SDL_SCANCODE_KP_EXCLAM = 207;
  SDL_SCANCODE_KP_MEMSTORE = 208;
  SDL_SCANCODE_KP_MEMRECALL = 209;
  SDL_SCANCODE_KP_MEMCLEAR = 210;
  SDL_SCANCODE_KP_MEMADD = 211;
  SDL_SCANCODE_KP_MEMSUBTRACT = 212;
  SDL_SCANCODE_KP_MEMMULTIPLY = 213;
  SDL_SCANCODE_KP_MEMDIVIDE = 214;
  SDL_SCANCODE_KP_PLUSMINUS = 215;
  SDL_SCANCODE_KP_CLEAR = 216;
  SDL_SCANCODE_KP_CLEARENTRY = 217;
  SDL_SCANCODE_KP_BINARY = 218;
  SDL_SCANCODE_KP_OCTAL = 219;
  SDL_SCANCODE_KP_DECIMAL = 220;
  SDL_SCANCODE_KP_HEXADECIMAL = 221;
  SDL_SCANCODE_LCTRL = 224;
  SDL_SCANCODE_LSHIFT = 225;
  SDL_SCANCODE_LALT = 226;
  SDL_SCANCODE_LGUI = 227;
  SDL_SCANCODE_RCTRL = 228;
  SDL_SCANCODE_RSHIFT = 229;
  SDL_SCANCODE_RALT = 230;
  SDL_SCANCODE_RGUI = 231;
  SDL_SCANCODE_MODE = 257;
  SDL_SCANCODE_AUDIONEXT = 258;
  SDL_SCANCODE_AUDIOPREV = 259;
  SDL_SCANCODE_AUDIOSTOP = 260;
  SDL_SCANCODE_AUDIOPLAY = 261;
  SDL_SCANCODE_AUDIOMUTE = 262;
  SDL_SCANCODE_MEDIASELECT = 263;
  SDL_SCANCODE_WWW = 264;
  SDL_SCANCODE_MAIL = 265;
  SDL_SCANCODE_CALCULATOR = 266;
  SDL_SCANCODE_COMPUTER = 267;
  SDL_SCANCODE_AC_SEARCH = 268;
  SDL_SCANCODE_AC_HOME = 269;
  SDL_SCANCODE_AC_BACK = 270;
  SDL_SCANCODE_AC_FORWARD = 271;
  SDL_SCANCODE_AC_STOP = 272;
  SDL_SCANCODE_AC_REFRESH = 273;
  SDL_SCANCODE_AC_BOOKMARKS = 274;
  SDL_SCANCODE_BRIGHTNESSDOWN = 275;
  SDL_SCANCODE_BRIGHTNESSUP = 276;
  SDL_SCANCODE_DISPLAYSWITCH = 277;
  SDL_SCANCODE_KBDILLUMTOGGLE = 278;
  SDL_SCANCODE_KBDILLUMDOWN = 279;
  SDL_SCANCODE_KBDILLUMUP = 280;
  SDL_SCANCODE_EJECT = 281;
  SDL_SCANCODE_SLEEP = 282;
  SDL_SCANCODE_APP1 = 283;
  SDL_SCANCODE_APP2 = 284;
  SDL_NUM_SCANCODES = 512;

{ SDL_Keycode }

  SDLK_RETURN = $D;
  SDLK_ESCAPE = $1B;
  SDLK_BACKSPACE = $8;
  SDLK_TAB = $9;
  SDLK_SPACE = Ord(' ');
  SDLK_EXCLAIM = Ord('!');
  SDLK_QUOTEDBL = Ord('"');
  SDLK_HASH = Ord('#');
  SDLK_DOLLAR = Ord('$');
  SDLK_PERCENT = Ord('%');
  SDLK_AMPERSAND = Ord('&');
  SDLK_QUOTE = Ord('''');
  SDLK_LEFTPAREN = Ord('(');
  SDLK_RIGHTPAREN = Ord(')');
  SDLK_ASTERISK = Ord('*');
  SDLK_PLUS = Ord('+');
  SDLK_COMMA = Ord(',');
  SDLK_MINUS = Ord('-');
  SDLK_PERIOD = Ord('.');
  SDLK_SLASH = Ord('/');
  SDLK_0 = Ord('0');
  SDLK_1 = Ord('1');
  SDLK_2 = Ord('2');
  SDLK_3 = Ord('3');
  SDLK_4 = Ord('4');
  SDLK_5 = Ord('5');
  SDLK_6 = Ord('6');
  SDLK_7 = Ord('7');
  SDLK_8 = Ord('8');
  SDLK_9 = Ord('9');
  SDLK_COLON = Ord(':');
  SDLK_SEMICOLON = Ord(';');
  SDLK_LESS = Ord('<');
  SDLK_EQUALS = Ord('=');
  SDLK_GREATER = Ord('>');
  SDLK_QUESTION = Ord('?');
  SDLK_AT = Ord('@');
  SDLK_LEFTBRACKET = Ord('[');
  SDLK_BACKSLASH = Ord('\');
  SDLK_RIGHTBRACKET = Ord(']');
  SDLK_CARET = Ord('^');
  SDLK_UNDERSCORE = Ord('_');
  SDLK_BACKQUOTE = Ord('`');
  SDLK_a = Ord('a');
  SDLK_b = Ord('b');
  SDLK_c = Ord('c');
  SDLK_d = Ord('d');
  SDLK_e = Ord('e');
  SDLK_f = Ord('f');
  SDLK_g = Ord('g');
  SDLK_h = Ord('h');
  SDLK_i = Ord('i');
  SDLK_j = Ord('j');
  SDLK_k = Ord('k');
  SDLK_l = Ord('l');
  SDLK_m = Ord('m');
  SDLK_n = Ord('n');
  SDLK_o = Ord('o');
  SDLK_p = Ord('p');
  SDLK_q = Ord('q');
  SDLK_r = Ord('r');
  SDLK_s = Ord('s');
  SDLK_t = Ord('t');
  SDLK_u = Ord('u');
  SDLK_v = Ord('v');
  SDLK_w = Ord('w');
  SDLK_x = Ord('x');
  SDLK_y = Ord('y');
  SDLK_z = Ord('z');
  SDLK_CAPSLOCK = $40000039;
  SDLK_F1 = $4000003A;
  SDLK_F2 = $4000003B;
  SDLK_F3 = $4000003C;
  SDLK_F4 = $4000003D;
  SDLK_F5 = $4000003E;
  SDLK_F6 = $4000003F;
  SDLK_F7 = $40000040;
  SDLK_F8 = $40000041;
  SDLK_F9 = $40000042;
  SDLK_F10 = $40000043;
  SDLK_F11 = $40000044;
  SDLK_F12 = $40000045;
  SDLK_PRINTSCREEN = $40000046;
  SDLK_SCROLLLOCK = $40000047;
  SDLK_PAUSE = $40000048;
  SDLK_INSERT = $40000049;
  SDLK_HOME = $4000004A;
  SDLK_PAGEUP = $4000004B;
  SDLK_DELETE = $7F;
  SDLK_END = $4000004D;
  SDLK_PAGEDOWN = $4000004E;
  SDLK_RIGHT = $4000004F;
  SDLK_LEFT = $40000050;
  SDLK_DOWN = $40000051;
  SDLK_UP = $40000052;
  SDLK_NUMLOCKCLEAR = $40000053;
  SDLK_KP_DIVIDE = $40000054;
  SDLK_KP_MULTIPLY = $40000055;
  SDLK_KP_MINUS = $40000056;
  SDLK_KP_PLUS = $40000057;
  SDLK_KP_ENTER = $40000058;
  SDLK_KP_1 = $40000059;
  SDLK_KP_2 = $4000005A;
  SDLK_KP_3 = $4000005B;
  SDLK_KP_4 = $4000005C;
  SDLK_KP_5 = $4000005D;
  SDLK_KP_6 = $4000005E;
  SDLK_KP_7 = $4000005F;
  SDLK_KP_8 = $40000060;
  SDLK_KP_9 = $40000061;
  SDLK_KP_0 = $40000062;
  SDLK_KP_PERIOD = $40000063;
  SDLK_APPLICATION = $40000065;
  SDLK_POWER = $40000066;
  SDLK_KP_EQUALS = $40000067;
  SDLK_F13 = $40000068;
  SDLK_F14 = $40000069;
  SDLK_F15 = $4000006A;
  SDLK_F16 = $4000006B;
  SDLK_F17 = $4000006C;
  SDLK_F18 = $4000006D;
  SDLK_F19 = $4000006E;
  SDLK_F20 = $4000006F;
  SDLK_F21 = $40000070;
  SDLK_F22 = $40000071;
  SDLK_F23 = $40000072;
  SDLK_F24 = $40000073;
  SDLK_EXECUTE = $40000074;
  SDLK_HELP = $40000075;
  SDLK_MENU = $40000076;
  SDLK_SELECT = $40000077;
  SDLK_STOP = $40000078;
  SDLK_AGAIN = $40000079;
  SDLK_UNDO = $4000007A;
  SDLK_CUT = $4000007B;
  SDLK_COPY = $4000007C;
  SDLK_PASTE = $4000007D;
  SDLK_FIND = $4000007E;
  SDLK_MUTE = $4000007F;
  SDLK_VOLUMEUP = $40000080;
  SDLK_VOLUMEDOWN = $40000081;
  SDLK_KP_COMMA = $40000085;
  SDLK_KP_EQUALSAS400 = $40000086;
  SDLK_ALTERASE = $40000099;
  SDLK_SYSREQ = $4000009A;
  SDLK_CANCEL = $4000009B;
  SDLK_CLEAR = $4000009C;
  SDLK_PRIOR = $4000009D;
  SDLK_RETURN2 = $4000009E;
  SDLK_SEPARATOR = $4000009F;
  SDLK_OUT = $400000A0;
  SDLK_OPER = $400000A1;
  SDLK_CLEARAGAIN = $400000A2;
  SDLK_CRSEL = $400000A3;
  SDLK_EXSEL = $400000A4;
  SDLK_KP_00 = $400000B0;
  SDLK_KP_000 = $400000B1;
  SDLK_THOUSANDSSEPARATOR = $400000B2;
  SDLK_DECIMALSEPARATOR = $400000B3;
  SDLK_CURRENCYUNIT = $400000B4;
  SDLK_CURRENCYSUBUNIT = $400000B5;
  SDLK_KP_LEFTPAREN = $400000B6;
  SDLK_KP_RIGHTPAREN = $400000B7;
  SDLK_KP_LEFTBRACE = $400000B8;
  SDLK_KP_RIGHTBRACE = $400000B9;
  SDLK_KP_TAB = $400000BA;
  SDLK_KP_BACKSPACE = $400000BB;
  SDLK_KP_A = $400000BC;
  SDLK_KP_B = $400000BD;
  SDLK_KP_C = $400000BE;
  SDLK_KP_D = $400000BF;
  SDLK_KP_E = $400000C0;
  SDLK_KP_F = $400000C1;
  SDLK_KP_XOR = $400000C2;
  SDLK_KP_POWER = $400000C3;
  SDLK_KP_PERCENT = $400000C4;
  SDLK_KP_LESS = $400000C5;
  SDLK_KP_GREATER = $400000C6;
  SDLK_KP_AMPERSAND = $400000C7;
  SDLK_KP_DBLAMPERSAND = $400000C8;
  SDLK_KP_VERTICALBAR = $400000C9;
  SDLK_KP_DBLVERTICALBAR = $400000CA;
  SDLK_KP_COLON = $400000CB;
  SDLK_KP_HASH = $400000CC;
  SDLK_KP_SPACE = $400000CD;
  SDLK_KP_AT = $400000CE;
  SDLK_KP_EXCLAM = $400000CF;
  SDLK_KP_MEMSTORE = $400000D0;
  SDLK_KP_MEMRECALL = $400000D1;
  SDLK_KP_MEMCLEAR = $400000D2;
  SDLK_KP_MEMADD = $400000D3;
  SDLK_KP_MEMSUBTRACT = $400000D4;
  SDLK_KP_MEMMULTIPLY = $400000D5;
  SDLK_KP_MEMDIVIDE = $400000D6;
  SDLK_KP_PLUSMINUS = $400000D7;
  SDLK_KP_CLEAR = $400000D8;
  SDLK_KP_CLEARENTRY = $400000D9;
  SDLK_KP_BINARY = $400000DA;
  SDLK_KP_OCTAL = $400000DB;
  SDLK_KP_DECIMAL = $400000DC;
  SDLK_KP_HEXADECIMAL = $400000DD;
  SDLK_LCTRL = $400000E0;
  SDLK_LSHIFT = $400000E1;
  SDLK_LALT = $400000E2;
  SDLK_LGUI = $400000E3;
  SDLK_RCTRL = $400000E4;
  SDLK_RSHIFT = $400000E5;
  SDLK_RALT = $400000E6;
  SDLK_RGUI = $400000E7;
  SDLK_MODE = $40000101;
  SDLK_AUDIONEXT = $40000102;
  SDLK_AUDIOPREV = $40000103;
  SDLK_AUDIOSTOP = $40000104;
  SDLK_AUDIOPLAY = $40000105;
  SDLK_AUDIOMUTE = $40000106;
  SDLK_MEDIASELECT = $40000107;
  SDLK_WWW = $40000108;
  SDLK_MAIL = $40000109;
  SDLK_CALCULATOR = $4000010A;
  SDLK_COMPUTER = $4000010B;
  SDLK_AC_SEARCH = $4000010C;
  SDLK_AC_HOME = $4000010D;
  SDLK_AC_BACK = $4000010E;
  SDLK_AC_FORWARD = $4000010F;
  SDLK_AC_STOP = $40000110;
  SDLK_AC_REFRESH = $40000111;
  SDLK_AC_BOOKMARKS = $40000112;
  SDLK_BRIGHTNESSDOWN = $40000113;
  SDLK_BRIGHTNESSUP = $40000114;
  SDLK_DISPLAYSWITCH = $40000115;
  SDLK_KBDILLUMTOGGLE = $40000116;
  SDLK_KBDILLUMDOWN = $40000117;
  SDLK_KBDILLUMUP = $40000118;
  SDLK_EJECT = $40000119;
  SDLK_SLEEP = $4000011A;

{ SDL_Keymod }

  KMOD_NONE = $0000;
  KMOD_LSHIFT = $0001;
  KMOD_RSHIFT = $0002;
  KMOD_LCTRL = $0040;
  KMOD_RCTRL = $0080;
  KMOD_LALT = $0100;
  KMOD_RALT = $0200;
  KMOD_LGUI = $0400;
  KMOD_RGUI = $0800;
  KMOD_NUM = $1000;
  KMOD_CAPS = $2000;
  KMOD_MODE = $4000;
  KMOD_RESERVED = $8000;

  KMOD_CTRL = KMOD_LCTRL or KMOD_RCTRL;
  KMOD_SHIFT = KMOD_LSHIFT or KMOD_RSHIFT;
  KMOD_ALT = KMOD_LALT or KMOD_RALT;
  KMOD_GUI = KMOD_LGUI or KMOD_RGUI;

{ SDL_keyboard.h }

type
  SDL_Keysym = record
    scancode: Uint32; { SDL_Scancode }
    sym: Uint32; { SDL_Keycode }
    modifiers: Uint16;
    unicode: Uint32;
  end;
  TSDL_Keysym = SDL_Keysym;
  PSDL_Keysym = ^TSDL_Keysym;

function SDL_GetKeyboardFocus: PSDL_Window; cdecl; libsdl2;
{ Returns global array of 0 or 1 bytes indexed using SDL_Scancode }
function SDL_GetKeyboardState(numkeys: PLongInt): PUint8; cdecl; libsdl2;
{ SDL_Keymod (modstate) }
function SDL_GetModState: Uint32; cdecl; libsdl2;
procedure SDL_SetModState(modstate: Uint32); cdecl; libsdl2;
{ Translate between SDL_Keycode (key) <-> SDL_Scancode (scancode) values }
function SDL_GetKeyFromScancode(scancode: Uint32): Uint32; cdecl; libsdl2;
function SDL_GetScancodeFromKey(key: Uint32): Uint32; cdecl; libsdl2;
function SDL_GetScancodeName(scancode: Uint32): SDL_Char; cdecl; libsdl2;
function SDL_GetScancodeFromName(name: SDL_Char): Uint32; cdecl; libsdl2;
function SDL_GetKeyName(key: Uint32): SDL_Char; cdecl; libsdl2;
function SDL_GetKeyFromName(name: SDL_Char): Uint32; cdecl; libsdl2;
procedure SDL_StartTextInput; cdecl; libsdl2;
function SDL_IsTextInputActive: SDL_Bool; cdecl; libsdl2;
procedure SDL_StopTextInput; cdecl; libsdl2;
procedure SDL_SetTextInputRect(constref rect: TSDL_Rect); cdecl; libsdl2;
function SDL_HasScreenKeyboardSupport: SDL_Bool; cdecl; libsdl2;
function SDL_IsScreenKeyboardShown(window: PSDL_Window): SDL_Bool; cdecl; libsdl2;

{ SDL_mouse.h }

{SDL_SystemCursor }

const
  SDL_SYSTEM_CURSOR_ARROW = 0;
  SDL_SYSTEM_CURSOR_IBEAM = SDL_SYSTEM_CURSOR_ARROW + 1;
  SDL_SYSTEM_CURSOR_WAIT = SDL_SYSTEM_CURSOR_IBEAM + 1;
  SDL_SYSTEM_CURSOR_CROSSHAIR = SDL_SYSTEM_CURSOR_WAIT + 1;
  SDL_SYSTEM_CURSOR_WAITARROW = SDL_SYSTEM_CURSOR_CROSSHAIR + 1;
  SDL_SYSTEM_CURSOR_SIZENWSE = SDL_SYSTEM_CURSOR_WAITARROW + 1;
  SDL_SYSTEM_CURSOR_SIZENESW = SDL_SYSTEM_CURSOR_SIZENWSE + 1;
  SDL_SYSTEM_CURSOR_SIZEWE = SDL_SYSTEM_CURSOR_SIZENESW + 1;
  SDL_SYSTEM_CURSOR_SIZENS = SDL_SYSTEM_CURSOR_SIZEWE + 1;
  SDL_SYSTEM_CURSOR_SIZEALL = SDL_SYSTEM_CURSOR_SIZENS + 1;
  SDL_SYSTEM_CURSOR_NO = SDL_SYSTEM_CURSOR_SIZEALL + 1;
  SDL_SYSTEM_CURSOR_HAND = SDL_SYSTEM_CURSOR_NO + 1;
  SDL_NUM_SYSTEM_CURSORS = SDL_SYSTEM_CURSOR_HAND + 1;

  SDL_BUTTON_LEFT = 1;
  SDL_BUTTON_MIDDLE = 2;
  SDL_BUTTON_RIGHT = 3;
  SDL_BUTTON_X1 = 4;
  SDL_BUTTON_X2 = 5;

type
  PSDL_Cursor = Pointer;

function SDL_GetMouseFocus: PSDL_Window; cdecl; libsdl2;
function SDL_GetMouseState(out x, y: LongInt): Uint32; cdecl; libsdl2;
function SDL_GetRelativeMouseState(out x, y: LongInt): Uint32; cdecl; libsdl2;
procedure SDL_WarpMouseInWindow(window: PSDL_Window; x, y: LongInt); cdecl; libsdl2;
function SDL_SetRelativeMouseMode(enabled: SDL_Bool): LongInt; cdecl; libsdl2;
function SDL_GetRelativeMouseMode: SDL_Bool; cdecl; libsdl2;
function SDL_CreateCursor(data, mask: PUint8; w, h, hot_x, hot_y: LongInt): PSDL_Cursor; cdecl; libsdl2;
function SDL_CreateColorCursor(surface: PSDL_Surface; hot_x, hot_y: LongInt): PSDL_Cursor; cdecl; libsdl2;
function SDL_CreateSystemCursor(id: Uint32): PSDL_Cursor; cdecl; libsdl2;
procedure SDL_SetCursor(cursor: PSDL_Cursor); cdecl; libsdl2;
function SDL_GetCursor: PSDL_Cursor; cdecl; libsdl2;
procedure SDL_FreeCursor(cursor: PSDL_Cursor); cdecl; libsdl2;
function SDL_ShowCursor(toggle: LongInt): LongInt; cdecl; libsdl2;

{ SDL_joystick.h }

const
  SDL_HAT_CENTERED = $00;
  SDL_HAT_UP = $01;
  SDL_HAT_RIGHT = $02;
  SDL_HAT_DOWN = $04;
  SDL_HAT_LEFT = $08;
  SDL_HAT_RIGHTUP = SDL_HAT_RIGHT or SDL_HAT_UP;
  SDL_HAT_RIGHTDOWN = SDL_HAT_RIGHT or SDL_HAT_DOWN;
  SDL_HAT_LEFTUP = SDL_HAT_LEFT or SDL_HAT_UP;
  SDL_HAT_LEFTDOWN = SDL_HAT_LEFT or SDL_HAT_DOWN;

type
  PSDL_Joystick = Pointer;

  SDL_JoystickGUID = packed record
    data: array[0..15] of Uint8;
  end;
  TSDL_JoystickGUID = SDL_JoystickGUID;
  PSDL_JoystickGUID = ^TSDL_JoystickGUID;

  SDL_JoystickID = Sint32;
  TSDL_JoystickID = SDL_JoystickID;

function SDL_NumJoysticks: LongInt; cdecl; libsdl2;
function SDL_JoystickNameForIndex(device_index: LongInt): SDL_Char; cdecl; libsdl2;
function SDL_JoystickOpen(device_index: LongInt): PSDL_Joystick; cdecl; libsdl2;
function SDL_JoystickName(joystick: PSDL_Joystick): SDL_Char; cdecl; libsdl2;
function SDL_JoystickGetDeviceGUID(device_index: LongInt): TSDL_JoystickGUID; cdecl; libsdl2;
function SDL_JoystickGetGUID(joystick: PSDL_Joystick): TSDL_JoystickGUID; cdecl; libsdl2;
procedure SDL_JoystickGetGUIDString(guid: TSDL_JoystickGUID; pszGUID: SDL_Char; cbGUID: LongInt); cdecl; libsdl2;
function SDL_JoystickGetGUIDFromString(pszGUID: SDL_Char): TSDL_JoystickGUID; cdecl; libsdl2;
function SDL_JoystickGetAttached(joystick: PSDL_Joystick): SDL_Bool; cdecl; libsdl2;
function SDL_JoystickInstanceID(joystick: PSDL_Joystick): TSDL_JoystickID; cdecl; libsdl2;
function SDL_JoystickNumAxes(joystick: PSDL_Joystick): LongInt; cdecl; libsdl2;
function SDL_JoystickNumBalls(joystick: PSDL_Joystick): LongInt; cdecl; libsdl2;
function SDL_JoystickNumHats(joystick: PSDL_Joystick): LongInt; cdecl; libsdl2;
function SDL_JoystickNumButtons(joystick: PSDL_Joystick): LongInt; cdecl; libsdl2;
procedure SDL_JoystickUpdate; cdecl; libsdl2;
function SDL_JoystickEventState(state: LongInt): LongInt; cdecl; libsdl2;
function SDL_JoystickGetAxis(joystick: PSDL_Joystick; axis: LongInt): Sint16; cdecl; libsdl2;
function SDL_JoystickGetHat(joystick: PSDL_Joystick; hat: LongInt): Uint8; cdecl; libsdl2;
function SDL_JoystickGetBall(joystick: PSDL_Joystick; ball: LongInt; out dx, dy: LongInt): LongInt; cdecl; libsdl2;
function SDL_JoystickGetButton(joystick: PSDL_Joystick; button: LongInt): Uint8; cdecl; libsdl2;
procedure SDL_JoystickClose(joystick: PSDL_Joystick); cdecl; libsdl2;

{ SDL_gamecontroller.h }

{ SDL_GameControllerBindType }

const
  SDL_CONTROLLER_BINDTYPE_NONE = 0;
  SDL_CONTROLLER_BINDTYPE_BUTTON = SDL_CONTROLLER_BINDTYPE_NONE + 1;
  SDL_CONTROLLER_BINDTYPE_AXIS = SDL_CONTROLLER_BINDTYPE_BUTTON + 1;
  SDL_CONTROLLER_BINDTYPE_HAT = SDL_CONTROLLER_BINDTYPE_AXIS + 1;

{ SDL_GameControllerAxis }

  SDL_CONTROLLER_AXIS_INVALID = -1;
  SDL_CONTROLLER_AXIS_LEFTX = SDL_CONTROLLER_AXIS_INVALID + 1;
  SDL_CONTROLLER_AXIS_LEFTY = SDL_CONTROLLER_AXIS_LEFTX + 1;
  SDL_CONTROLLER_AXIS_RIGHTX = SDL_CONTROLLER_AXIS_LEFTY + 1;
  SDL_CONTROLLER_AXIS_RIGHTY = SDL_CONTROLLER_AXIS_RIGHTX + 1;
  SDL_CONTROLLER_AXIS_TRIGGERLEFT = SDL_CONTROLLER_AXIS_RIGHTY + 1;
  SDL_CONTROLLER_AXIS_TRIGGERRIGHT = SDL_CONTROLLER_AXIS_TRIGGERLEFT + 1;
  SDL_CONTROLLER_AXIS_MAX = SDL_CONTROLLER_AXIS_TRIGGERRIGHT + 1;

{ SDL_GameControllerButton }

  SDL_CONTROLLER_BUTTON_INVALID = -1;
  SDL_CONTROLLER_BUTTON_A = SDL_CONTROLLER_BUTTON_INVALID + 1;
  SDL_CONTROLLER_BUTTON_B = SDL_CONTROLLER_BUTTON_A + 1;
  SDL_CONTROLLER_BUTTON_X = SDL_CONTROLLER_BUTTON_B + 1;
  SDL_CONTROLLER_BUTTON_Y = SDL_CONTROLLER_BUTTON_X + 1;
  SDL_CONTROLLER_BUTTON_BACK = SDL_CONTROLLER_BUTTON_Y + 1;
  SDL_CONTROLLER_BUTTON_GUIDE = SDL_CONTROLLER_BUTTON_BACK + 1;
  SDL_CONTROLLER_BUTTON_START = SDL_CONTROLLER_BUTTON_GUIDE + 1;
  SDL_CONTROLLER_BUTTON_LEFTSTICK = SDL_CONTROLLER_BUTTON_START + 1;
  SDL_CONTROLLER_BUTTON_RIGHTSTICK = SDL_CONTROLLER_BUTTON_LEFTSTICK + 1;
  SDL_CONTROLLER_BUTTON_LEFTSHOULDER = SDL_CONTROLLER_BUTTON_RIGHTSTICK + 1;
  SDL_CONTROLLER_BUTTON_RIGHTSHOULDER = SDL_CONTROLLER_BUTTON_LEFTSHOULDER + 1;
  SDL_CONTROLLER_BUTTON_DPAD_UP = SDL_CONTROLLER_BUTTON_RIGHTSHOULDER + 1;
  SDL_CONTROLLER_BUTTON_DPAD_DOWN = SDL_CONTROLLER_BUTTON_DPAD_UP + 1;
  SDL_CONTROLLER_BUTTON_DPAD_LEFT = SDL_CONTROLLER_BUTTON_DPAD_DOWN + 1;
  SDL_CONTROLLER_BUTTON_DPAD_RIGHT = SDL_CONTROLLER_BUTTON_DPAD_LEFT + 1;
  SDL_CONTROLLER_BUTTON_MAX = SDL_CONTROLLER_BUTTON_DPAD_RIGHT + 1;

type
  PSDL_GameController = Pointer;

  SDL_GameControllerButtonBind = record
    bindType: Uint32;
    case Integer of
      1: (button: LongInt);
      2: (axis: LongInt);
      3: (
        value: record
          hat: LongInt;
          hat_mask: LongInt;
        end);
  end;
  TSDL_GameControllerButtonBind = SDL_GameControllerButtonBind;
  PSDL_GameControllerButtonBind = ^TSDL_GameControllerButtonBind;

function SDL_GameControllerAddMapping(mapping: SDL_Char): LongInt; cdecl; libsdl2;
function SDL_GameControllerMappingForGUID(guid: TSDL_JoystickGUID): SDL_Char; cdecl; libsdl2;
function SDL_GameControllerMapping(gamecontroller: PSDL_GameController): SDL_Char; cdecl; libsdl2;
function SDL_IsGameController(joystick_index: LongInt): SDL_Bool; cdecl; libsdl2;
function SDL_GameControllerNameForIndex(joystick_index: LongInt): SDL_Char; cdecl; libsdl2;
function SDL_GameControllerOpen(joystick_index: LongInt): PSDL_GameController; cdecl; libsdl2;
function SDL_GameControllerName(gamecontroller: PSDL_GameController): SDL_Char; cdecl; libsdl2;
function SDL_GameControllerGetAttached(gamecontroller: PSDL_GameController): SDL_Bool; cdecl; libsdl2;
function SDL_GameControllerGetJoystick(gamecontroller: PSDL_GameController): PSDL_Joystick; cdecl; libsdl2;
function SDL_GameControllerEventState(state: LongInt): LongInt; cdecl; libsdl2;
procedure SDL_GameControllerUpdate; cdecl; libsdl2;
function SDL_GameControllerGetAxisFromString(pchString: SDL_Char): Uint32; cdecl; libsdl2;
function SDL_GameControllerGetStringForAxis(axis: Uint32): SDL_Char; cdecl; libsdl2;
function SDL_GameControllerGetBindForAxis(gamecontroller: PSDL_GameController; axis: Uint32): Uint32; cdecl; libsdl2;
function SDL_GameControllerGetAxis(gamecontroller: PSDL_GameController; axis: Uint32): Sint16; cdecl; libsdl2;
function SDL_GameControllerGetButtonFromString(pchString: SDL_Char): Uint32; cdecl; libsdl2;
function SDL_GameControllerGetStringForButton(button: Uint32): SDL_Char; cdecl; libsdl2;
function SDL_GameControllerGetBindForButton(gamecontroller: PSDL_GameController; button: Uint32): Uint32; cdecl; libsdl2;
function SDL_GameControllerGetButton(gamecontroller: PSDL_GameController; button: Uint32): Uint8; cdecl; libsdl2;
procedure SDL_GameControllerClose(gamecontroller: PSDL_GameController); cdecl; libsdl2;

{ SDL_haptic.h }

const
  SDL_HAPTIC_CONSTANT = 1 shl 0;
  SDL_HAPTIC_SINE = 1 shl 1;
  SDL_HAPTIC_SQUARE = 1 shl 2;
  SDL_HAPTIC_TRIANGLE = 1 shl 3;
  SDL_HAPTIC_SAWTOOTHUP = 1 shl 4;
  SDL_HAPTIC_SAWTOOTHDOWN = 1 shl 5;
  SDL_HAPTIC_RAMP = 1 shl 6;
  SDL_HAPTIC_SPRING = 1 shl 7;
  SDL_HAPTIC_DAMPER = 1 shl 8;
  SDL_HAPTIC_INERTIA = 1 shl 9;
  SDL_HAPTIC_FRICTION = 1 shl 10;
  SDL_HAPTIC_CUSTOM = 1 shl 11;
  SDL_HAPTIC_GAIN = 1 shl 12;
  SDL_HAPTIC_AUTOCENTER = 1 shl 13;
  SDL_HAPTIC_STATUS = 1 shl 14;
  SDL_HAPTIC_PAUSE = 1 shl 15;

  SDL_HAPTIC_POLAR = 0;
  SDL_HAPTIC_CARTESIAN = 1;
  SDL_HAPTIC_SPHERICAL = 2;
  SDL_HAPTIC_INFINITY = 4294967295;

type
  PSDL_Haptic = Pointer;

{TODO: review or test the packing/alignment of these haptic records }

  SDL_HapticDirection = packed record
    type_: Uint8;
    paddding: array[0..2] of Uint8; // ? not sure if this is needed
    dir: array[0..2] of Sint32;
  end;
  TSDL_HapticDirection = SDL_HapticDirection;
  PSDL_HapticDirection = ^TSDL_HapticDirection;

  SDL_HapticConstant = packed record
    type_: Uint16;
    direction: SDL_HapticDirection;
    length: Uint32;
    delay: Uint16;
    button: Uint16;
    interval: Uint16;
    level: Sint16;
    attack_length: Uint16;
    attack_level: Uint16;
    fade_length: Uint16;
    fade_level: Uint16;
  end;
  TSDL_HapticConstant = SDL_HapticConstant;
  PSDL_HapticConstant = ^TSDL_HapticConstant;

  SDL_HapticPeriodic = packed record
    type_: Uint16;
    direction: SDL_HapticDirection;
    length: Uint32;
    delay: Uint16;
    button: Uint16;
    interval: Uint16;
    period: Uint16;
    magnitude: Sint16;
    offset: Sint16;
    phase: Uint16;
    attack_length: Uint16;
    attack_level: Uint16;
    fade_length: Uint16;
    fade_level: Uint16;
  end;
  TSDL_HapticPeriodic = SDL_HapticPeriodic;
  PSDL_HapticPeriodic = ^TSDL_HapticPeriodic;

  SDL_HapticCondition = packed record
    type_: Uint16;
    direction: TSDL_HapticDirection;
    length: Uint32;
    delay: Uint16;
    button: Uint16;
    interval: Uint16;
    right_sat: array[0..2] of Uint16;
    left_sat: array[0..2] of Uint16;
    right_coeff: array[0..2] of Sint16;
    left_coeff: array[0..2] of Sint16;
    deadband: array[0..2] of Uint16;
    center: array[0..2] of Sint16;
  end;
  TSDL_HapticCondition = SDL_HapticCondition;
  PSDL_HapticCondition = ^TSDL_HapticCondition;

  SDL_HapticRamp = record
    type_: Uint16;
    direction: TSDL_HapticDirection;
    length: Uint32;
    delay: Uint16;
    button: Uint16;
    interval: Uint16;
    start: Sint16;
    end_: Sint16;
    attack_length: Uint16;
    attack_level: Uint16;
    fade_length: Uint16;
    fade_level: Uint16;
  end;
  TSDL_HapticRamp = SDL_HapticRamp;
  PSDL_HapticRamp = ^TSDL_HapticRamp;

  SDL_HapticCustom = record
    type_: Uint16;
    direction: SDL_HapticDirection;
    length: Uint32;
    delay: Uint16;
    button: Uint16;
    interval: Uint16;
    channels: Uint8;
    period: Uint16;
    samples: Uint16;
    data: PUint16;
    attack_length: Uint16;
    attack_level: Uint16;
    fade_length: Uint16;
    fade_level: Uint16;
  end;
  TSDL_HapticCustom = SDL_HapticCustom;
  PSDL_HapticCustom = ^TSDL_HapticCustom;

  SDL_HapticEffect = packed record
    case Integer of
      0: (type_: Uint16);
      1: (constant: TSDL_HapticConstant);
      2: (periodic: TSDL_HapticPeriodic);
      3: (condition: TSDL_HapticCondition);
      4: (ramp: TSDL_HapticRamp);
      5: (custom: TSDL_HapticCustom);
  end;
  TSDL_HapticEffect = SDL_HapticEffect;
  PSDL_HapticEffect = ^TSDL_HapticEffect;

function SDL_NumHaptics: LongInt; cdecl; libsdl2;
function SDL_HapticName(device_index: LongInt): SDL_Char; cdecl; libsdl2;
function SDL_HapticOpen(device_index: LongInt): PSDL_Haptic; cdecl; libsdl2;
function SDL_HapticOpened(device_index: LongInt): LongInt; cdecl; libsdl2;
function SDL_HapticIndex(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_MouseIsHaptic: LongInt; cdecl; libsdl2;
function SDL_HapticOpenFromMouse: PSDL_Haptic; cdecl; libsdl2;
function SDL_JoystickIsHaptic(joystick: PSDL_Joystick): LongInt; cdecl; libsdl2;
function SDL_HapticOpenFromJoystick(joystick: PSDL_Joystick): PSDL_Haptic; cdecl; libsdl2;
procedure SDL_HapticClose(haptic: PSDL_Haptic); cdecl; libsdl2;
function SDL_HapticNumEffects(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_HapticNumEffectsPlaying(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_HapticQuery(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_HapticNumAxes(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_HapticEffectSupported(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): LongInt; cdecl; libsdl2;
function SDL_HapticNewEffect(haptic: PSDL_Haptic; effect: PSDL_HapticEffect): LongInt; cdecl; libsdl2;
function SDL_HapticUpdateEffect(haptic: PSDL_Haptic; effect: LongInt; data: PSDL_HapticEffect): LongInt; cdecl; libsdl2;
function SDL_HapticRunEffect(haptic: PSDL_Haptic; effect: LongInt; iterations: Uint32): LongInt; cdecl; libsdl2;
function SDL_HapticStopEffect(haptic: PSDL_Haptic; effect: LongInt): LongInt; cdecl; libsdl2;
procedure SDL_HapticDestroyEffect(haptic: PSDL_Haptic; effect: LongInt); cdecl; libsdl2;
function SDL_HapticGetEffectStatus(haptic: PSDL_Haptic; effect: LongInt): LongInt; cdecl; libsdl2;
function SDL_HapticSetGain(haptic: PSDL_Haptic; gain: LongInt): LongInt; cdecl; libsdl2;
function SDL_HapticSetAutocenter(haptic: PSDL_Haptic; autocenter: LongInt): LongInt; cdecl; libsdl2;
function SDL_HapticPause(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_HapticUnpause(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_HapticStopAll(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_HapticRumbleSupported(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_HapticRumbleInit(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;
function SDL_HapticRumblePlay(haptic: PSDL_Haptic; strength: SDL_Float; length: Uint32): LongInt; cdecl; libsdl2;
function SDL_HapticRumbleStop(haptic: PSDL_Haptic): LongInt; cdecl; libsdl2;

{ SDL_touch.h }

type
  SDL_TouchID = Sint64;
  TSDL_TouchID = SDL_TouchID;
  SDL_FingerID = Sint64;
  TSDL_FingerID = SDL_FingerID;

  SDL_Finger = packed record
    id: TSDL_FingerID;
    x: SDL_Float;
    y: SDL_Float;
    pressure: SDL_Float;
  end;
  TSDL_Finger = SDL_Finger;
  PSDL_Finger = ^TSDL_Finger;

function SDL_GetNumTouchDevices: LongInt; cdecl; libsdl2;
function SDL_GetTouchDevice(index: LongInt): TSDL_TouchID; cdecl; libsdl2;
function SDL_GetNumTouchFingers(touchID: TSDL_TouchID): LongInt; cdecl; libsdl2;
function SDL_GetTouchFinger(touchID: TSDL_TouchID; index: LongInt): PSDL_Finger; cdecl; libsdl2;

{ SDL_gesture.h }

type
  SDL_GestureID = Sint64;
  TSDL_GestureID = SDL_GestureID;

function SDL_RecordGesture(touchId: TSDL_TouchID): LongInt; cdecl; libsdl2;
{ src should proably be const but the header doesn't specify that }
function SDL_SaveAllDollarTemplates(var src: TSDL_RWops): LongInt; cdecl; libsdl2;
function SDL_SaveDollarTemplate(touchId: TSDL_TouchID; var src: TSDL_RWops): LongInt; cdecl; libsdl2;
function SDL_LoadDollarTemplates(touchId: TSDL_TouchID; out src: TSDL_RWops): LongInt; cdecl; libsdl2;

{ SDL_events.h }

const
  SDL_RELEASED = 0;
  SDL_PRESSED = 1;

  SDL_QUERY = -1;
  SDL_IGNORE = 0;
  SDL_DISABLE = 0;
  SDL_ENABLE =1;

{ SDL_EventType }

  { Unused (do not remove) }
  SDL_FIRST_EVENT = 0;
  { Application events }
  SDL_QUIT_EVENT = $100;
  { Window events }
  SDL_WINDOW_EVENT = $200;
  SDL_SYSWM_EVENT = SDL_WINDOW_EVENT + 1;
  { Keyboard events }
  SDL_KEYDOWN = $300;
  SDL_KEYUP = SDL_KEYDOWN + 1;
  SDL_TEXTEDITING = SDL_KEYUP + 1;
  SDL_TEXTINPUT = SDL_TEXTEDITING + 1;
  { Mouse events }
  SDL_MOUSEMOTION = $400;
  SDL_MOUSEBUTTONDOWN = SDL_MOUSEMOTION + 1;
  SDL_MOUSEBUTTONUP = SDL_MOUSEBUTTONDOWN + 1;
  SDL_MOUSEWHEEL = SDL_MOUSEBUTTONUP + 1;
  { Joystick events }
  SDL_JOYAXISMOTION = $600;
  SDL_JOYBALLMOTION = SDL_JOYAXISMOTION + 1;
  SDL_JOYHATMOTION = SDL_JOYBALLMOTION + 1;
  SDL_JOYBUTTONDOWN = SDL_JOYHATMOTION + 1;
  SDL_JOYBUTTONUP = SDL_JOYBUTTONDOWN + 1;
  SDL_JOYDEVICEADDED = SDL_JOYBUTTONUP + 1;
  SDL_JOYDEVICEREMOVED = SDL_JOYDEVICEADDED + 1;
  { Game controller events }
  SDL_CONTROLLERAXISMOTION = $650;
  SDL_CONTROLLERBUTTONDOWN = SDL_CONTROLLERAXISMOTION + 1;
  SDL_CONTROLLERBUTTONUP = SDL_CONTROLLERBUTTONDOWN + 1;
  SDL_CONTROLLERDEVICEADDED = SDL_CONTROLLERBUTTONUP + 1;
  SDL_CONTROLLERDEVICEREMOVED = SDL_CONTROLLERDEVICEADDED + 1;
  SDL_CONTROLLERDEVICEREMAPPED = SDL_CONTROLLERDEVICEREMOVED + 1;
  { Touch events }
  SDL_FINGERDOWN = $700;
  SDL_FINGERUP = SDL_FINGERDOWN + 1;
  SDL_FINGERMOTION = SDL_FINGERUP + 1;
  { Gesture events }
  SDL_DOLLARGESTURE = $800;
  SDL_DOLLARRECORD = SDL_DOLLARGESTURE + 1;
  SDL_MULTIGESTURE = SDL_DOLLARRECORD + 1;
  { Clipboard events }
  SDL_CLIPBOARDUPDATE = $900;
  { Drag and drop events }
  SDL_DROPFILE = $1000;
  { Events SDL_USEREVENT through SDL_LASTEVENT are for your us
    and should be allocated with SDL_RegisterEvents }
  SDL_USER_EVENT = $8000;
  SDL_LAST_EVENT = $FFFF;

{ SDL_eventaction }

  SDL_ADDEVENT = 0;
  SDL_PEEKEVENT = 1;
  SDL_GETEVENT = 2;

{ Event types }

type
  SDL_GenericEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
  end;
  TSDL_GenericEvent = SDL_GenericEvent;
  PSDL_GenericEvent = ^TSDL_GenericEvent;

  SDL_WindowEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    windowID: Uint32;
    event: Uint8;
    padding1: Uint8;
    padding2: Uint8;
    padding3: Uint8;
    data1: LongInt;
    data2: LongInt;
  end;
  TSDL_WindowEvent = SDL_WindowEvent;
  PSDL_WindowEvent = ^TSDL_WindowEvent;

  SDL_KeyboardEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    windowID: Uint32;
    state: Uint8;
    repeat_: Uint8;
    padding2: Uint8;
    padding3: Uint8;
    keysym: TSDL_Keysym;
  end;
  TSDL_KeyboardEvent = SDL_KeyboardEvent;
  PSDL_KeyboardEvent = ^TSDL_KeyboardEvent;

  SDL_TextEditingEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    windowID: Uint32;
    text: array[0..31] of AnsiChar;
    start: LongInt;
    length: LongInt;
  end;
  TSDL_TextEditingEvent = SDL_TextEditingEvent;
  PSDL_TextEditingEvent = ^TSDL_TextEditingEvent;

  SDL_TextInputEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    windowID: Uint32;
    text: array [0..31] of AnsiChar;
  end;
  TSDL_TextInputEvent = SDL_TextInputEvent;
  PSDL_TextInputEvent = ^TSDL_TextInputEvent;

  SDL_MouseMotionEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    windowID: Uint32;
    which: Uint32;
    state: Uint8;
    padding1: Uint8;
    padding2: Uint8;
    padding3: Uint8;
    x: LongInt;
    y: LongInt;
    xrel: LongInt;
    yrel: LongInt;
  end;
  TSDL_MouseMotionEvent = SDL_MouseMotionEvent;
  PSDL_MouseMotionEvent = ^TSDL_MouseMotionEvent;

  SDL_MouseButtonEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    windowID: Uint32;
    which: Uint32;
    button: Uint8;
    state: Uint8;
    padding1: Uint8;
    padding2: Uint8;
    x: LongInt;
    y: LongInt;
  end;
  TSDL_MouseButtonEvent = SDL_MouseButtonEvent;
  PSDL_MouseButtonEvent = ^TSDL_MouseButtonEvent;

  SDL_MouseWheelEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    windowID: Uint32;
    which: Uint32;
    x: LongInt;
    y: LongInt;
  end;
  TSDL_MouseWheelEvent = SDL_MouseWheelEvent;
  PSDL_MouseWheelEvent = ^TSDL_MouseWheelEvent;

  SDL_JoyAxisEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    which: TSDL_JoystickID;
    axis: Uint8;
    padding1: Uint8;
    padding2: Uint8;
    padding3: Uint8;
    value: Sint16;
    padding4: Uint16;
  end;
  TSDL_JoyAxisEvent = SDL_JoyAxisEvent;
  PSDL_JoyAxisEvent = ^TSDL_JoyAxisEvent;

  SDL_JoyBallEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    which: TSDL_JoystickID;
    ball: Uint8;
    padding1: Uint8;
    padding2: Uint8;
    padding3: Uint8;
    xrel: Sint16;
    yrel: Sint16;
  end;
  TSDL_JoyBallEvent = SDL_JoyBallEvent;
  PSDL_JoyBallEvent = ^TSDL_JoyBallEvent;

  SDL_JoyHatEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    which: TSDL_JoystickID;
    hat: Uint8;
    value: Uint8;
    padding1: Uint8;
    padding2: Uint8;
  end;
  TSDL_JoyHatEvent = SDL_JoyHatEvent;
  PSDL_JoyHatEvent = ^TSDL_JoyHatEvent;

  SDL_JoyButtonEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    which: TSDL_JoystickID;
    button: Uint8;
    state: Uint8;
    padding1: Uint8;
    padding2: Uint8;
  end;
  TSDL_JoyButtonEvent = SDL_JoyButtonEvent;
  PSDL_JoyButtonEvent = ^TSDL_JoyButtonEvent;

  SDL_JoyDeviceEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    which: TSDL_JoystickID;
  end;
  TSDL_JoyDeviceEvent = SDL_JoyDeviceEvent;
  PSDL_JoyDeviceEvent = ^TSDL_JoyDeviceEvent;

  SDL_ControllerAxisEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    which: TSDL_JoystickID;
    axis: Uint8;
    padding1: Uint8;
    padding2: Uint8;
    padding3: Uint8;
    value: Sint16;
    padding4: Uint16;
  end;
  TSDL_ControllerAxisEvent = SDL_ControllerAxisEvent;
  PSDL_ControllerAxisEvent = ^TSDL_ControllerAxisEvent;

  SDL_ControllerButtonEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    which: TSDL_JoystickID;
    button: Uint8;
    state: Uint8;
    padding1: Uint8;
    padding2: Uint8;
  end;
  TSDL_ControllerButtonEvent = SDL_ControllerButtonEvent;
  PSDL_ControllerButtonEvent = ^TSDL_ControllerButtonEvent;

  SDL_ControllerDeviceEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    which: TSDL_JoystickID;
  end;
  TSDL_ControllerDeviceEvent = SDL_ControllerDeviceEvent;
  PSDL_ControllerDeviceEvent = ^TSDL_ControllerDeviceEvent;

  SDL_TouchFingerEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    touchId: SDL_TouchID;
    fingerId: SDL_FingerID;
    x: SDL_Float;
    y: SDL_Float;
    dx: SDL_Float;
    dy: SDL_Float;
    pressure: SDL_Float;
  end;
  TSDL_TouchFingerEvent = SDL_TouchFingerEvent;
  PSDL_TouchFingerEvent = ^TSDL_TouchFingerEvent;

  SDL_MultiGestureEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    touchId: TSDL_TouchID;
    dTheta: SDL_Float;
    dDist: SDL_Float;
    x: SDL_Float;
    y: SDL_Float;
    numFingers: Uint16;
    padding: Uint16;
  end;
  TSDL_MultiGestureEvent = SDL_MultiGestureEvent;
  PSDL_MultiGestureEvent = ^TSDL_MultiGestureEvent;

  SDL_DollarGestureEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    touchId: SDL_TouchID;
    gestureId: TSDL_GestureID;
    numFingers: Uint32;
    error: SDL_Float;
    x: SDL_Float;
    y: SDL_Float;
  end;
  TSDL_DollarGestureEvent = SDL_DollarGestureEvent;
  PSDL_DollarGestureEvent = ^TSDL_DollarGestureEvent;

  SDL_DropEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    _file: SDL_Char;
  end;
  TSDL_DropEvent = SDL_DropEvent;
  PSDL_DropEvent = ^TSDL_DropEvent;

  SDL_QuitEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
  end;
  TSDL_QuitEvent = SDL_QuitEvent;
  PSDL_QuitEvent = ^TSDL_QuitEvent;

  SDL_UserEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    windowID: Uint32;
    code: LongInt;
    data1: Pointer;
    data2: Pointer;
  end;
  TSDL_UserEvent = SDL_UserEvent;
  PSDL_UserEvent = ^TSDL_UserEvent;

  SDL_SysWMEvent = packed record
    type_: Uint32;
    timestamp: Uint32;
    msg: Pointer;
  end;
  TSDL_SysWMEvent = SDL_SysWMEvent;
  PSDL_SysWMEvent = ^TSDL_SysWMEvent;

  SDL_Event = record
    case Integer of
      0: (type_: Uint32; timestamp: Uint32);
      1: (generic_: TSDL_GenericEvent);
      2: (window: TSDL_WindowEvent);
      3: (key: TSDL_KeyboardEvent);
      4: (edit: TSDL_TextEditingEvent);
      5: (text: TSDL_TextInputEvent);
      6: (motion: TSDL_MouseMotionEvent);
      7: (button: TSDL_MouseButtonEvent);
      8: (wheel: TSDL_MouseWheelEvent);
      9: (jaxis: TSDL_JoyAxisEvent);
      10: (jball: TSDL_JoyBallEvent);
      11: (jhat: TSDL_JoyHatEvent);
      12: (jbutton: TSDL_JoyButtonEvent);
      13: (jdevice: TSDL_JoyDeviceEvent);
      14: (caxis: TSDL_ControllerAxisEvent);
      15: (cbutton: TSDL_ControllerButtonEvent);
      16: (cdevice: TSDL_ControllerDeviceEvent);
      17: (quit: TSDL_QuitEvent);
      18: (user: TSDL_UserEvent);
      19: (syswm: TSDL_SysWMEvent);
      20: (tfinger: TSDL_TouchFingerEvent);
      21: (mgesture: TSDL_MultiGestureEvent);
      22: (dgesture: TSDL_DollarGestureEvent);
      23: (drop: TSDL_DropEvent);
      24: (padding: array[0..55] of Uint8);
  end;
  TSDL_Event = SDL_Event;
  PSDL_Event = ^TSDL_Event;

  TSDL_EventFilter = function(userdata: Pointer; constref event: TSDL_Event): LongInt; cdecl;

procedure SDL_PumpEvents; cdecl; libsdl2;
function SDL_PeepEvents(events: PSDL_Event; numevents: LongInt; action, minType, maxType: Uint32): LongInt; cdecl; libsdl2;
function SDL_HasEvent(type_: Uint32): SDL_Bool; cdecl; libsdl2;
function SDL_HasEvents(minType, maxType: Uint32): SDL_Bool; cdecl; libsdl2;
procedure SDL_FlushEvent(type_: Uint32); cdecl; libsdl2;
procedure SDL_FlushEvents(minType, maxType: Uint32); cdecl; libsdl2;
function SDL_PollEvent(out event: TSDL_Event): LongInt; cdecl; libsdl2;
function SDL_WaitEventTimeout(out event: TSDL_Event; timeout: LongInt): LongInt; cdecl; libsdl2;
function SDL_PushEvent(constref event: TSDL_Event): LongInt; cdecl; libsdl2;
procedure SDL_SetEventFilter(filter: TSDL_EventFilter; userdata: Pointer); cdecl; libsdl2;
function SDL_GetEventFilter(out filter: TSDL_EventFilter; out userdata: Pointer): SDL_Bool; cdecl; libsdl2;
procedure SDL_AddEventWatch(filter: TSDL_EventFilter; userdata: Pointer); cdecl; libsdl2;
procedure SDL_DelEventWatch(filter: TSDL_EventFilter; userdata: Pointer); cdecl; libsdl2;
procedure SDL_FilterEvents(filter: TSDL_EventFilter; userdata: Pointer); cdecl; libsdl2;
function SDL_EventState(type_: Uint32; state: LongInt): Uint8; cdecl; libsdl2;
function SDL_RegisterEvents(numevents: LongInt): Uint32; cdecl; libsdl2;

{ SDL_audio.h }

const
  SDL_AUDIO_MASK_BITSIZE       = $FF;
  SDL_AUDIO_MASK_DATATYPE      = 1 shl 8;
  SDL_AUDIO_MASK_ENDIAN        = 1 shl 12;
  SDL_AUDIO_MASK_SIGNED        = 1 shl 15;

  AUDIO_U8  = $0008;
  AUDIO_S8  = $8008;
  AUDIO_U16LSB  = $0010;
  AUDIO_S16LSB  = $8010;
  AUDIO_U16MSB  = $1010;
  AUDIO_S16MSB  = $9010;
  AUDIO_U16  = AUDIO_U16LSB;
  AUDIO_S16  = AUDIO_S16LSB;

  AUDIO_S32LSB  = $8020;
  AUDIO_S32MSB  = $9020;
  AUDIO_S32  = AUDIO_S32LSB;

  AUDIO_F32LSB  = $8120;
  AUDIO_F32MSB  = $9120;
  AUDIO_F32  = AUDIO_F32LSB;

  AUDIO_CHAN_MONO = 1;
  AUDIO_CHAN_STEREO = 2;

  AUDIO_SAMPLE_SMALL = 512;
  AUDIO_SAMPLE_MEDIUM = 1024;
  AUDIO_SAMPLE_LARGE = 2048;

  AUDIO_FREQ_FM_QAULITY = 22050;
  AUDIO_FREQ_CD_QAULITY = 44100;

  SDL_AUDIO_ALLOW_FREQUENCY_CHANGE    = $00000001;
  SDL_AUDIO_ALLOW_FORMAT_CHANGE       = $00000002;
  SDL_AUDIO_ALLOW_CHANNELS_CHANGE     = $00000004;
  SDL_AUDIO_ALLOW_ANY_CHANGE          = SDL_AUDIO_ALLOW_FREQUENCY_CHANGE or
      SDL_AUDIO_ALLOW_FORMAT_CHANGE  or SDL_AUDIO_ALLOW_CHANNELS_CHANGE;

  SDL_MIX_MAXVOLUME = 128;

{ SDL_AudioStatus }

  SDL_AUDIO_STOPPED = 0;
  SDL_AUDIO_PLAYING = SDL_AUDIO_STOPPED + 1;
  SDL_AUDIO_PAUSED = SDL_AUDIO_PLAYING + 1;

type
  SDL_AudioDeviceID = Uint32;
  TSDL_AudioDeviceID = SDL_AudioDeviceID;
  PSDL_AudioDeviceID = ^TSDL_AudioDeviceID;

  TSDL_AudioCallback = procedure(userdata: Pointer; stream: PUint8; len: LongInt); cdecl;

  SDL_AudioSpec = record
    freq: LongInt;
    format: Uint16;
    channels: Uint8;
    silence: Uint8;
    samples: Uint16;
    padding: Uint16;
    size: Uint32;
    callback: TSDL_AudioCallback;
    userdata: Pointer;
  end;
  TSDL_AudioSpec = SDL_AudioSpec;
  PSDL_AudioSpec = ^TSDL_AudioSpec;

  PSDL_AudioCVT = ^TSDL_AudioCVT;

  TSDL_AudioFilter = procedure(cvt: PSDL_AudioCVT; format: Uint16); cdecl;

  SDL_AudioCVT = record
    needed: LongInt;
    src_format: Uint16;
    dst_format: Uint16;
    rate_incr: double;
    buf: PUint8;
    len: LongInt;
    len_cvt: LongInt;
    len_mult: Uint32;
    len_ratio: SDL_Double;
    filters: array[0..9] of TSDL_AudioFilter;
    filter_index: LongInt;
  end;
  TSDL_AudioCVT = SDL_AudioCVT;

function SDL_GetNumAudioDrivers: LongInt; cdecl; libsdl2;
function SDL_GetAudioDriver(index: LongInt): SDL_Char; cdecl; libsdl2;
function SDL_AudioInit(driver_name: SDL_Char): LongInt; cdecl; libsdl2;
procedure SDL_SDL_AudioQuit; cdecl; libsdl2;
function SDL_GetCurrentAudioDriver: SDL_Char; cdecl; libsdl2;
function SDL_OpenAudio(desired, obtained: PSDL_AudioSpec): LongInt; cdecl; libsdl2;
function SDL_GetNumAudioDevices(iscapture: LongInt): LongInt; cdecl; libsdl2;
function SDL_GetAudioDeviceName(index, iscapture: LongInt): SDL_Char; cdecl; libsdl2;
function SDL_OpenAudioDevice(device: SDL_Char; iscapture: LongInt;
  desired, obtained: PSDL_AudioSpec; allowed_changes: LongInt): TSDL_AudioDeviceID; cdecl; libsdl2;
function SDL_GetAudioStatus: Uint32; cdecl; libsdl2;
function SDL_GetAudioDeviceStatus(dev: TSDL_AudioDeviceID): Uint32; cdecl; libsdl2;
procedure SDL_PauseAudio(pause_on: LongInt); cdecl; libsdl2;
procedure SDL_PauseAudioDevice(dev: TSDL_AudioDeviceID; pause_on: LongInt); cdecl; libsdl2;
function SDL_LoadWAV_RW(src: PSDL_RWops; freesrc: LongInt; out spec: TSDL_AudioSpec;
  out audio_buf: PUint8; out audio_len: Uint32): PSDL_AudioSpec; cdecl; libsdl2;
procedure SDL_FreeWAV(audio_buf: PUint8); cdecl; libsdl2;
function SDL_BuildAudioCVT(out cvt: TSDL_AudioCVT; src_format: UInt16;
  src_channels: UInt8; src_rate: LongInt; dst_format: UInt16; dst_channels: UInt8;
  dst_rate: LongInt): LongInt; cdecl; libsdl2;
function SDL_ConvertAudio(var cvt: TSDL_AudioCVT): LongInt; cdecl; libsdl2;
procedure SDL_MixAudio(dst, src: PUint8; len: Uint32; volume: LongInt); cdecl; libsdl2;
procedure SDL_MixAudioFormat(dst, src: PUint8; format: Uint16; len: Uint32; volume: LongInt); cdecl; libsdl2;
procedure SDL_LockAudio; cdecl; libsdl2;
procedure SDL_LockAudioDevice(dev: TSDL_AudioDeviceID); cdecl; libsdl2;
procedure SDL_UnlockAudio; cdecl; libsdl2;
procedure SDL_UnlockAudioDevice(dev: TSDL_AudioDeviceID); cdecl; libsdl2;
procedure SDL_CloseAudio; cdecl; libsdl2;
procedure SDL_CloseAudioDevice(dev: TSDL_AudioDeviceID); cdecl; libsdl2;
function SDL_AudioDeviceConnected(dev: TSDL_AudioDeviceID): LongInt; cdecl; libsdl2;

{ SDL_hints.h }

const
  SDL_HINT_FRAMEBUFFER_ACCELERATION = 'SDL_FRAMEBUFFER_ACCELERATION';
  SDL_HINT_RENDER_DRIVER = 'SDL_RENDER_DRIVER';
  SDL_HINT_RENDER_OPENGL_SHADERS = 'SDL_RENDER_OPENGL_SHADERS';
  SDL_HINT_RENDER_SCALE_QUALITY = 'SDL_RENDER_SCALE_QUALITY';
  SDL_HINT_RENDER_VSYNC = 'SDL_RENDER_VSYNC';
  SDL_HINT_VIDEO_X11_XVIDMODE = 'SDL_VIDEO_X11_XVIDMODE';
  SDL_HINT_VIDEO_X11_XINERAMA = 'SDL_VIDEO_X11_XINERAMA';
  SDL_HINT_VIDEO_X11_XRANDR = 'SDL_VIDEO_X11_XRANDR';
  SDL_HINT_GRAB_KEYBOARD = 'SDL_GRAB_KEYBOARD';
  SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS = 'SDL_VIDEO_MINIMIZE_ON_FOCUS_LOSS';
  SDL_HINT_IDLE_TIMER_DISABLED = 'SDL_IOS_IDLE_TIMER_DISABLED';
  SDL_HINT_ORIENTATIONS = 'SDL_IOS_ORIENTATIONS';
  SDL_HINT_XINPUT_ENABLED = 'SDL_XINPUT_ENABLED';
  SDL_HINT_GAMECONTROLLERCONFIG = 'SDL_GAMECONTROLLERCONFIG';
  SDL_HINT_ALLOW_TOPMOST = 'SDL_ALLOW_TOPMOST';

{ SDL_HintPriority }

  SDL_HINT_DEFAULT = 0;
  SDL_HINT_NORMAL = SDL_HINT_DEFAULT + 1;
  SDL_HINT_OVERRIDE = SDL_HINT_NORMAL + 1;

function SDL_SetHintWithPriority(name, value: SDL_Char; priority: Uint32): SDL_Bool; cdecl; libsdl2;
function SDL_SetHint(name, value: SDL_Char): SDL_Bool; cdecl; libsdl2;
function SDL_GetHint(name: SDL_Char): SDL_Char; cdecl; libsdl2;
procedure SDL_ClearHints; cdecl; libsdl2;

{ SDL_cpuinfo.h }

const
  SDL_CACHELINE_SIZE = 128;

function SDL_GetCPUCount: LongInt; cdecl; libsdl2;
function SDL_GetCPUCacheLineSize: LongInt; cdecl; libsdl2;
function SDL_HasRDTSC: SDL_Bool; cdecl; libsdl2;
function SDL_HasAltiVec: SDL_Bool; cdecl; libsdl2;
function SDL_HasMMX: SDL_Bool; cdecl; libsdl2;
function SDL_Has3DNow: SDL_Bool; cdecl; libsdl2;
function SDL_HasSSE: SDL_Bool; cdecl; libsdl2;
function SDL_HasSSE2: SDL_Bool; cdecl; libsdl2;
function SDL_HasSSE3: SDL_Bool; cdecl; libsdl2;
function SDL_HasSSE42: SDL_Bool; cdecl; libsdl2;

{ SDL_clipboard.h }

function SDL_SetClipboardText(text: SDL_Char): LongInt; cdecl; libsdl2;
function SDL_GetClipboardText: SDL_Char; cdecl; libsdl2;
function SDL_HasClipboardText: SDL_Bool; cdecl; libsdl2;

{ SDL_timer.h }

type
  TSDL_TimerCallback = function(interval: Uint32; param: Pointer): Uint32; cdecl;

function SDL_GetTicks: Uint32; cdecl; libsdl2;
function SDL_GetPerformanceCounter: Uint64; cdecl; libsdl2;
function SDL_GetPerformanceFrequency: Uint64; cdecl; libsdl2;
procedure SDL_Delay(ms: Uint32); cdecl; libsdl2;
function SDL_AddTimer(LongInt: Uint32; callback: TSDL_TimerCallback;
  param: Pointer): LongInt; cdecl; libsdl2;
function SDL_RemoveTimer(id: LongInt): SDL_Bool; cdecl; libsdl2;

{ SDL_loadso.h }

function SDL_LoadObject(sofile: SDL_Char): HModule; cdecl; libsdl2;
function SDL_LoadFunction(handle: HModule; name: SDL_Char): Pointer; cdecl; libsdl2;
procedure SDL_UnloadObject(handle: HModule); cdecl; libsdl2;

{ SDL_mutex.h }

const
  SDL_MUTEX_TIMEDOUT = 1;
  SDL_MUTEX_MAXWAIT = High(LongWord);

type
  PSDL_Mutex = Pointer;
  PSDL_Sem = Pointer;
  PSDL_Cond = Pointer;

function SDL_CreateMutex: PSDL_Mutex; cdecl; libsdl2;
function SDL_LockMutex(mutex: PSDL_Mutex): LongInt; cdecl; libsdl2;
function SDL_TryLockMutex(mutex: PSDL_Mutex): LongInt; cdecl; libsdl2;
function SDL_UnlockMutex(mutex: PSDL_Mutex): LongInt; cdecl; libsdl2;
procedure SDL_DestroyMutex(mutex: PSDL_Mutex); cdecl; libsdl2;

function SDL_CreateSemaphore(initial_value: Uint32): PSDL_Sem; cdecl; libsdl2;
procedure SDL_DestroySemaphore(sem: PSDL_Sem); cdecl; libsdl2;
function SDL_SemWait(sem: PSDL_Sem): LongInt; cdecl; libsdl2;
function SDL_SemTryWait(sem: PSDL_Sem): LongInt; cdecl; libsdl2;
function SDL_SemWaitTimeout(sem: PSDL_Sem; ms: Uint32): LongInt; cdecl; libsdl2;
function SDL_SemPost(sem: PSDL_Sem): LongInt; cdecl; libsdl2;
function SDL_SemValue(sem: PSDL_Sem): Uint32; cdecl; libsdl2;

function SDL_CreateCond: PSDL_Cond; cdecl; libsdl2;
procedure SDL_DestroyCond(cond: PSDL_Cond); cdecl; libsdl2;
function SDL_CondSignal(cond: PSDL_Cond): LongInt; cdecl; libsdl2;
function SDL_CondBroadcast(cond: PSDL_Cond): LongInt; cdecl; libsdl2;
function SDL_CondWait(cond: PSDL_Cond; mutex: PSDL_Mutex): LongInt; cdecl; libsdl2;
function SDL_CondWaitTimeout(cond: PSDL_Cond; mutex: PSDL_Mutex; ms: Uint32): LongInt; cdecl; libsdl2;

{ SDL_thread.h }

type
  PSDL_Thread = Pointer;

{ SDL_ThreadPriority }

const
  SDL_THREAD_PRIORITY_LOW = 0;
  SDL_THREAD_PRIORITY_NORMAL = SDL_THREAD_PRIORITY_LOW + 1;
  SDL_THREAD_PRIORITY_HIGH = SDL_THREAD_PRIORITY_NORMAL + 1;

type
  TSDL_ThreadFunction = function(data: Pointer): LongInt; cdecl;

{ Make sure SDL is built with SDL_PASSED_BEGINTHREAD_ENDTHREAD undefined }

function SDL_CreateThread(fn: TSDL_ThreadFunction; name: SDL_Char; data: Pointer): PSDL_Thread; cdecl; libsdl2;
function SDL_GetThreadName(thread: PSDL_Thread): SDL_Char; cdecl; libsdl2;
function SDL_ThreadID: SDL_Long; cdecl; libsdl2;
function SDL_GetThreadID(thread: PSDL_Thread): SDL_Long; cdecl; libsdl2;
function SDL_SetThreadPriority(priority: Uint32): LongInt; cdecl; libsdl2;
procedure SDL_WaitThread(thread: PSDL_Thread; out status: LongInt); cdecl; libsdl2;

{ SDL_atomic.h }

// The SDL atomic API uses might use GCC atomics or plaftorm code and cannot reliably be translated

{ SDL_power.h }

{ SDL_PowerState }

const
  SDL_POWERSTATE_UNKNOWN = 0;
  SDL_POWERSTATE_ON_BATTERY = SDL_POWERSTATE_UNKNOWN + 1;
  SDL_POWERSTATE_NO_BATTERY = SDL_POWERSTATE_ON_BATTERY + 1;
  SDL_POWERSTATE_CHARGING = SDL_POWERSTATE_NO_BATTERY + 1;
  SDL_POWERSTATE_CHARGED = SDL_POWERSTATE_CHARGING + 1;

function SDL_GetPowerInfo(out secs, pct: LongInt): Uint32; cdecl; libsdl2;

{ Macros from all headers }

procedure SDL_QuitRequested;
function SDL_RectEmpty(constref A: TSDL_Rect): Boolean;
function SDL_RectEquals(constref A, B: TSDL_Rect): Boolean;
function SDL_Button(B: Uint32): LongWord;
function SDL_LoadWAV(filename: PChar; out spec: TSDL_AudioSpec; out audio_buf: PUInt8;
  out audio_len: UInt32): PSDL_AudioSpec;
function SDL_RWSize(ctx: PSDL_RWops): Sint64;
function SDL_RWSeek(ctx: PSDL_RWops; offset: Sint64; whence: LongInt): Sint64;
function SDL_RWTell(ctx: PSDL_RWops): Sint64;
function SDL_RWRead(ctx: PSDL_RWops; ptr: Pointer; size, maxnum: IntPtr): Sint64;
function SDL_RWWrite(ctx: PSDL_RWops; ptr: Pointer; size, num: IntPtr): Sint64;
function SDL_RWClose(ctx: PSDL_RWops): LongInt;

{ SDL_image.h }

{ IMG_InitFlags }

const
  IMG_INIT_JPG = $00000001;
  IMG_INIT_PNG = $00000002;
  IMG_INIT_TIF = $00000004;
  IMG_INIT_WEBP = $0000000;

implementation

{ Macros from all headers }

procedure SDL_QuitRequested;
begin
  repeat
    SDL_PumpEvents;
  until SDL_PeepEvents(nil, 0, SDL_PEEKEVENT, SDL_QUIT_EVENT, SDL_QUIT_EVENT) > 0;
end;

function SDL_RectEmpty(constref A: TSDL_Rect): Boolean;
begin
  Result := (A.w <= 0) or (A.h <= 0);
end;

function SDL_RectEquals(constref A, B: TSDL_Rect): Boolean;
begin
  Result := (A.x = B.x) and (A.y = B.y) and (A.w = B.w) and (A.h = B.h);
end;

function SDL_Button(B: Uint32): LongWord;
begin
  Result := 1 shl (B - 1);
end;

function SDL_LoadWAV(filename: PChar; out spec: TSDL_AudioSpec; out audio_buf: PUInt8;
  out audio_len: UInt32): PSDL_AudioSpec;
var
  Ops: PSDL_RWops;
begin
  Ops := SDL_RWFromFile(fileName, 'rb');
  if Ops = nil then
    Exit(nil);
  Result := SDL_LoadWAV_RW(Ops, 1, spec, audio_buf, audio_len);
end;

function SDL_RWSize(ctx: PSDL_RWops): Sint64;
begin
  Result := ctx.size(ctx);
end;

function SDL_RWSeek(ctx: PSDL_RWops; offset: Sint64; whence: LongInt): Sint64;
begin
  Result := ctx.seek(ctx, offset, whence);
end;

function SDL_RWTell(ctx: PSDL_RWops): Sint64;
begin
  Result := ctx.seek(ctx, 0, RW_SEEK_CUR);
end;

function SDL_RWRead(ctx: PSDL_RWops; ptr: Pointer; size, maxnum: IntPtr): Sint64;
begin
  Result := ctx.read(ctx, ptr, size, maxnum);
end;

function SDL_RWWrite(ctx: PSDL_RWops; ptr: Pointer; size, num: IntPtr): Sint64;
begin
  Result := ctx.write(ctx, ptr, size, num);
end;

function SDL_RWClose(ctx: PSDL_RWops): LongInt;
begin
  Result := ctx.close(ctx);
end;

function SDL_PIXELFLAG(X: Uint32): Uint32;
begin
  Result := (X shr 28) and $0F;
end;

function SDL_PIXELTYPE(X: Uint32): Uint32;
begin
  Result := (X shr 24) and $0F;
end;

function SDL_PIXELORDER(X: Uint32): Uint32;
begin
  Result := (X shr 20) and $0F;
end;

function SDL_PIXELLAYOUT(X: Uint32): Uint32;
begin
  Result := (X shr 16) and $0F;
end;

function SDL_BITSPERPIXEL(X: Uint32): Uint32;
begin
  Result := (X shr 8) and $FF;
end;

end.
