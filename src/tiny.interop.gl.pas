unit Tiny.Interop.GL;

{$i tiny.inc}

interface

uses
  Tiny.Types;

{ LoadGL must be called prior to any functions in this unit }

function LoadGL(GetProc: TGetProcAddress): Boolean;

type
  GLvoid = Pointer;
  GLenum = DWord;
  GLboolean = Byte;
  GLbitfield = DWord;
  GLbyte = ShortInt;
  GLshort = SmallInt;
  GLint = LongInt;
  GLsizei = LongInt;
  GLubyte = Byte;
  GLushort = Word;
  GLuint = LongWord;
  GLfloat = Single;
  GLclampf = Single;
  GLfixed = LongInt;
  GLintptr = PtrInt;
  GLsizeiptr = PtrInt;
  GLchar = Char;
  PGLchar = PChar;
  PGLvoid = ^GLvoid;
  PGLenum = ^GLenum;
  PGLboolean = ^GLboolean;
  PGLbitfield = ^GLbitfield;
  PGLbyte = ^GLbyte;
  PGLshort = ^GLshort;
  PGLint = ^GLint;
  PGLsizei = ^GLsizei;
  PGLubyte = ^GLubyte;
  PGLushort = ^GLushort;
  PGLuint = ^GLuint;
  PGLfloat = ^GLfloat;
  PGLclampf = ^GLclampf;
  PGLfixed = ^GLfixed;
  PGLintptr = ^GLintptr;
  PGLsizeiptr = ^GLsizeiptr;

const
  GL_ES_VERSION_2_0 = 1;
  { ClearBufferMask }
  GL_DEPTH_BUFFER_BIT = $00000100;
  GL_STENCIL_BUFFER_BIT = $00000400;
  GL_COLOR_BUFFER_BIT = $00004000;
  { Boolean }
  GL_FALSE = 0;
  GL_TRUE = 1;
  { BeginMode }
  GL_POINTS = $0000;
  GL_LINES = $0001;
  GL_LINE_LOOP = $0002;
  GL_LINE_STRIP = $0003;
  GL_TRIANGLES = $0004;
  GL_TRIANGLE_STRIP = $0005;
  GL_TRIANGLE_FAN = $0006;
  { AlphaFunction (not supported in ES20) }
  { GL_NEVER }
  { GL_LESS }
  { GL_EQUAL }
  { GL_LEQUAL }
  { GL_GREATER }
  { GL_NOTEQUAL }
  { GL_GEQUAL }
  { GL_ALWAYS }
  { BlendingFactorDest }
  GL_ZERO = 0;
  GL_ONE = 1;
  GL_SRC_COLOR = $0300;
  GL_ONE_MINUS_SRC_COLOR = $0301;
  GL_SRC_ALPHA = $0302;
  GL_ONE_MINUS_SRC_ALPHA = $0303;
  GL_DST_ALPHA = $0304;
  GL_ONE_MINUS_DST_ALPHA = $0305;
  { BlendingFactorSrc }
  { GL_ZERO }
  { GL_ONE }
  GL_DST_COLOR = $0306;
  GL_ONE_MINUS_DST_COLOR = $0307;
  GL_SRC_ALPHA_SATURATE = $0308;
  { GL_SRC_ALPHA }
  { GL_ONE_MINUS_SRC_ALPHA }
  { GL_DST_ALPHA }
  { GL_ONE_MINUS_DST_ALPHA }
  { BlendEquationSeparate }
  GL_FUNC_ADD = $8006;
  GL_BLEND_EQUATION = $8009;
  { Same as BLEND_EQUATION }
  GL_BLEND_EQUATION_RGB = $8009;
  GL_BLEND_EQUATION_ALPHA = $883D;
  { BlendSubtract }
  GL_FUNC_SUBTRACT = $800A;
  GL_FUNC_REVERSE_SUBTRACT = $800B;
  { Separate Blend Functions }
  GL_BLEND_DST_RGB = $80C8;
  GL_BLEND_SRC_RGB = $80C9;
  GL_BLEND_DST_ALPHA = $80CA;
  GL_BLEND_SRC_ALPHA = $80CB;
  GL_CONSTANT_COLOR = $8001;
  GL_ONE_MINUS_CONSTANT_COLOR = $8002;
  GL_CONSTANT_ALPHA = $8003;
  GL_ONE_MINUS_CONSTANT_ALPHA = $8004;
  GL_BLEND_COLOR = $8005;
  { Buffer Objects }
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;
  GL_ARRAY_BUFFER_BINDING = $8894;
  GL_ELEMENT_ARRAY_BUFFER_BINDING = $8895;
  GL_STREAM_DRAW = $88E0;
  GL_STATIC_DRAW = $88E4;
  GL_DYNAMIC_DRAW = $88E8;
  GL_BUFFER_SIZE = $8764;
  GL_BUFFER_USAGE = $8765;
  GL_CURRENT_VERTEX_ATTRIB = $8626;
  { CullFaceMode }
  GL_FRONT = $0404;
  GL_BACK = $0405;
  GL_FRONT_AND_BACK = $0408;
  { DepthFunction }
  { GL_NEVER }
  { GL_LESS }
  { GL_EQUAL }
  { GL_LEQUAL }
  { GL_GREATER }
  { GL_NOTEQUAL }
  { GL_GEQUAL }
  { GL_ALWAYS }
  { EnableCap }
  GL_TEXTURE_2D = $0DE1;
  GL_CULL_FACE = $0B44;
  GL_BLEND = $0BE2;
  GL_DITHER = $0BD0;
  GL_STENCIL_TEST = $0B90;
  GL_DEPTH_TEST = $0B71;
  GL_SCISSOR_TEST = $0C11;
  GL_POLYGON_OFFSET_FILL = $8037;
  GL_SAMPLE_ALPHA_TO_COVERAGE = $809E;
  GL_SAMPLE_COVERAGE = $80A0;
  { ErrorCode }
  GL_NO_ERROR = 0;
  GL_INVALID_ENUM = $0500;
  GL_INVALID_VALUE = $0501;
  GL_INVALID_OPERATION = $0502;
  GL_OUT_OF_MEMORY = $0505;
  { FrontFaceDirection }
  GL_CW = $0900;
  GL_CCW = $0901;
  { GetPName }
  GL_LINE_WIDTH = $0B21;
  GL_ALIASED_POINT_SIZE_RANGE = $846D;
  GL_ALIASED_LINE_WIDTH_RANGE = $846E;
  GL_CULL_FACE_MODE = $0B45;
  GL_FRONT_FACE = $0B46;
  GL_DEPTH_RANGE = $0B70;
  GL_DEPTH_WRITEMASK = $0B72;
  GL_DEPTH_CLEAR_VALUE = $0B73;
  GL_DEPTH_FUNC = $0B74;
  GL_STENCIL_CLEAR_VALUE = $0B91;
  GL_STENCIL_FUNC = $0B92;
  GL_STENCIL_FAIL = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS = $0B96;
  GL_STENCIL_REF = $0B97;
  GL_STENCIL_VALUE_MASK = $0B93;
  GL_STENCIL_WRITEMASK = $0B98;
  GL_STENCIL_BACK_FUNC = $8800;
  GL_STENCIL_BACK_FAIL = $8801;
  GL_STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
  GL_STENCIL_BACK_PASS_DEPTH_PASS = $8803;
  GL_STENCIL_BACK_REF = $8CA3;
  GL_STENCIL_BACK_VALUE_MASK = $8CA4;
  GL_STENCIL_BACK_WRITEMASK = $8CA5;
  GL_VIEWPORT = $0BA2;
  GL_SCISSOR_BOX = $0C10;
  { GL_SCISSOR_TEST }
  GL_COLOR_CLEAR_VALUE = $0C22;
  GL_COLOR_WRITEMASK = $0C23;
  GL_UNPACK_ALIGNMENT = $0CF5;
  GL_PACK_ALIGNMENT = $0D05;
  GL_MAX_TEXTURE_SIZE = $0D33;
  GL_MAX_VIEWPORT_DIMS = $0D3A;
  GL_SUBPIXEL_BITS = $0D50;
  GL_RED_BITS = $0D52;
  GL_GREEN_BITS = $0D53;
  GL_BLUE_BITS = $0D54;
  GL_ALPHA_BITS = $0D55;
  GL_DEPTH_BITS = $0D56;
  GL_STENCIL_BITS = $0D57;
  GL_POLYGON_OFFSET_UNITS = $2A00;
  { GL_POLYGON_OFFSET_FILL }
  GL_POLYGON_OFFSET_FACTOR = $8038;
  GL_TEXTURE_BINDING_2D = $8069;
  GL_SAMPLE_BUFFERS = $80A8;
  GL_SAMPLES = $80A9;
  GL_SAMPLE_COVERAGE_VALUE = $80AA;
  GL_SAMPLE_COVERAGE_INVERT = $80AB;
  { GetTextureParameter }
  { GL_TEXTURE_MAG_FILTER }
  { GL_TEXTURE_MIN_FILTER }
  { GL_TEXTURE_WRAP_S }
  { GL_TEXTURE_WRAP_T }
  GL_NUM_COMPRESSED_TEXTURE_FORMATS = $86A2;
  GL_COMPRESSED_TEXTURE_FORMATS = $86A3;
  { HintMode }
  GL_DONT_CARE = $1100;
  GL_FASTEST = $1101;
  GL_NICEST = $1102;
  { HintTarget }
  GL_GENERATE_MIPMAP_HINT = $8192;
  { DataType }
  GL_BYTE = $1400;
  GL_UNSIGNED_BYTE = $1401;
  GL_SHORT = $1402;
  GL_UNSIGNED_SHORT = $1403;
  GL_INT = $1404;
  GL_UNSIGNED_INT = $1405;
  GL_FLOAT = $1406;
  GL_FIXED = $140C;
  { PixelFormat }
  GL_DEPTH_COMPONENT = $1902;
  GL_ALPHA = $1906;
  GL_RGB = $1907;
  GL_RGBA = $1908;
  GL_LUMINANCE = $1909;
  GL_LUMINANCE_ALPHA = $190A;
  { PixelType }
  { GL_UNSIGNED_BYTE }
  GL_UNSIGNED_SHORT_4_4_4_4 = $8033;
  GL_UNSIGNED_SHORT_5_5_5_1 = $8034;
  GL_UNSIGNED_SHORT_5_6_5 = $8363;
  { Shaders }
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
  GL_MAX_VERTEX_ATTRIBS = $8869;
  GL_MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
  GL_MAX_VARYING_VECTORS = $8DFC;
  GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
  GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
  GL_MAX_TEXTURE_IMAGE_UNITS = $8872;
  GL_MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;
  GL_SHADER_TYPE = $8B4F;
  GL_DELETE_STATUS = $8B80;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;
  GL_ATTACHED_SHADERS = $8B85;
  GL_ACTIVE_UNIFORMS = $8B86;
  GL_ACTIVE_UNIFORM_MAX_LENGTH = $8B87;
  GL_ACTIVE_ATTRIBUTES = $8B89;
  GL_ACTIVE_ATTRIBUTE_MAX_LENGTH = $8B8A;
  GL_CURRENT_PROGRAM = $8B8D;
  { StencilFunction }
  GL_NEVER = $0200;
  GL_LESS = $0201;
  GL_EQUAL = $0202;
  GL_LEQUAL = $0203;
  GL_GREATER = $0204;
  GL_NOTEQUAL = $0205;
  GL_GEQUAL = $0206;
  GL_ALWAYS = $0207;
  { StencilOp }
  { GL_ZERO }
  GL_KEEP = $1E00;
  GL_REPLACE = $1E01;
  GL_INCR = $1E02;
  GL_DECR = $1E03;
  GL_INVERT = $150A;
  GL_INCR_WRAP = $8507;
  GL_DECR_WRAP = $8508;
  { StringName }
  GL_VENDOR = $1F00;
  GL_RENDERER = $1F01;
  GL_VERSION = $1F02;
  GL_EXTENSIONS = $1F03;
  GL_SHADING_LANGUAGE_VERSION = $8B8C;
  { TextureMagFilter }
  GL_NEAREST = $2600;
  GL_LINEAR = $2601;
  { TextureMinFilter }
  { GL_NEAREST }
  { GL_LINEAR }
  GL_NEAREST_MIPMAP_NEAREST = $2700;
  GL_LINEAR_MIPMAP_NEAREST = $2701;
  GL_NEAREST_MIPMAP_LINEAR = $2702;
  GL_LINEAR_MIPMAP_LINEAR = $2703;
  { TextureParameterName }
  GL_TEXTURE_MAG_FILTER = $2800;
  GL_TEXTURE_MIN_FILTER = $2801;
  GL_TEXTURE_WRAP_S = $2802;
  GL_TEXTURE_WRAP_T = $2803;
  { TextureTarget }
  { GL_TEXTURE_2D }
  GL_TEXTURE = $1702;
  GL_TEXTURE_CUBE_MAP = $8513;
  GL_TEXTURE_BINDING_CUBE_MAP = $8514;
  GL_TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
  GL_TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
  GL_TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
  GL_MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
  { TextureUnit }
  GL_TEXTURE0 = $84C0;
  GL_TEXTURE1 = $84C1;
  GL_TEXTURE2 = $84C2;
  GL_TEXTURE3 = $84C3;
  GL_TEXTURE4 = $84C4;
  GL_TEXTURE5 = $84C5;
  GL_TEXTURE6 = $84C6;
  GL_TEXTURE7 = $84C7;
  GL_TEXTURE8 = $84C8;
  GL_TEXTURE9 = $84C9;
  GL_TEXTURE10 = $84CA;
  GL_TEXTURE11 = $84CB;
  GL_TEXTURE12 = $84CC;
  GL_TEXTURE13 = $84CD;
  GL_TEXTURE14 = $84CE;
  GL_TEXTURE15 = $84CF;
  GL_TEXTURE16 = $84D0;
  GL_TEXTURE17 = $84D1;
  GL_TEXTURE18 = $84D2;
  GL_TEXTURE19 = $84D3;
  GL_TEXTURE20 = $84D4;
  GL_TEXTURE21 = $84D5;
  GL_TEXTURE22 = $84D6;
  GL_TEXTURE23 = $84D7;
  GL_TEXTURE24 = $84D8;
  GL_TEXTURE25 = $84D9;
  GL_TEXTURE26 = $84DA;
  GL_TEXTURE27 = $84DB;
  GL_TEXTURE28 = $84DC;
  GL_TEXTURE29 = $84DD;
  GL_TEXTURE30 = $84DE;
  GL_TEXTURE31 = $84DF;
  GL_ACTIVE_TEXTURE = $84E0;
  { TextureWrapMode }
  GL_REPEAT = $2901;
  GL_CLAMP_TO_EDGE = $812F;
  GL_MIRRORED_REPEAT = $8370;
  { Uniform Types }
  GL_FLOAT_VEC2 = $8B50;
  GL_FLOAT_VEC3 = $8B51;
  GL_FLOAT_VEC4 = $8B52;
  GL_INT_VEC2 = $8B53;
  GL_INT_VEC3 = $8B54;
  GL_INT_VEC4 = $8B55;
  GL_BOOL = $8B56;
  GL_BOOL_VEC2 = $8B57;
  GL_BOOL_VEC3 = $8B58;
  GL_BOOL_VEC4 = $8B59;
  GL_FLOAT_MAT2 = $8B5A;
  GL_FLOAT_MAT3 = $8B5B;
  GL_FLOAT_MAT4 = $8B5C;
  GL_SAMPLER_2D = $8B5E;
  GL_SAMPLER_CUBE = $8B60;
  { Vertex Arrays }
  GL_VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
  GL_VERTEX_ATTRIB_ARRAY_SIZE = $8623;
  GL_VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
  GL_VERTEX_ATTRIB_ARRAY_TYPE = $8625;
  GL_VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
  GL_VERTEX_ATTRIB_ARRAY_Pointer = $8645;
  GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
  { Read Format }
  GL_IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
  GL_IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;
  { Shader Source }
  GL_COMPILE_STATUS = $8B81;
  GL_INFO_LOG_LENGTH = $8B84;
  GL_SHADER_SOURCE_LENGTH = $8B88;
  GL_SHADER_COMPILER = $8DFA;
  { Shader Binary }
  GL_SHADER_BINARY_FORMATS = $8DF8;
  GL_NUM_SHADER_BINARY_FORMATS = $8DF9;
  { Shader Precision-Specified Types }
  GL_LOW_FLOAT = $8DF0;
  GL_MEDIUM_FLOAT = $8DF1;
  GL_HIGH_FLOAT = $8DF2;
  GL_LOW_INT = $8DF3;
  GL_MEDIUM_INT = $8DF4;
  GL_HIGH_INT = $8DF5;
  { Framebuffer Object. }
  GL_FRAMEBUFFER = $8D40;
  GL_RENDERBUFFER = $8D41;
  GL_RGBA4 = $8056;
  GL_RGB5_A1 = $8057;
  GL_RGB565 = $8D62;
  GL_DEPTH_COMPONENT16 = $81A5;
  GL_STENCIL_INDEX = $1901;
  GL_STENCIL_INDEX8 = $8D48;
  GL_RENDERBUFFER_WIDTH = $8D42;
  GL_RENDERBUFFER_HEIGHT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT = $8D44;
  GL_RENDERBUFFER_RED_SIZE = $8D50;
  GL_RENDERBUFFER_GREEN_SIZE = $8D51;
  GL_RENDERBUFFER_BLUE_SIZE = $8D52;
  GL_RENDERBUFFER_ALPHA_SIZE = $8D53;
  GL_RENDERBUFFER_DEPTH_SIZE = $8D54;
  GL_RENDERBUFFER_STENCIL_SIZE = $8D55;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
  GL_COLOR_ATTACHMENT0 = $8CE0;
  GL_DEPTH_ATTACHMENT = $8D00;
  GL_STENCIL_ATTACHMENT = $8D20;
  GL_NONE = 0;
  GL_FRAMEBUFFER_COMPLETE = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
  GL_FRAMEBUFFER_UNSUPPORTED = $8CDD;
  GL_FRAMEBUFFER_BINDING = $8CA6;
  GL_RENDERBUFFER_BINDING = $8CA7;
  GL_MAX_RENDERBUFFER_SIZE = $84E8;
  GL_INVALID_FRAMEBUFFER_OPERATION = $0506;

var
  glActiveTexture: procedure(texture: GLenum); apicall;
  glAttachShader: procedure(_program: GLuint; shader: GLuint); apicall;
  glBindAttribLocation: procedure(_program: GLuint; index: GLuint; name: pchar); apicall;
  glBindBuffer: procedure(target: GLenum; buffer: GLuint); apicall;
  glBindFramebuffer: procedure(target: GLenum; framebuffer: GLuint); apicall;
  glBindRenderbuffer: procedure(target: GLenum; renderbuffer: GLuint); apicall;
  glBindTexture: procedure(target: GLenum; texture: GLuint); apicall;
  glBlendColor: procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); apicall;
  glBlendEquation: procedure(mode: GLenum); apicall;
  glBlendEquationSeparate: procedure(modeRGB: GLenum; modeAlpha: GLenum); apicall;
  glBlendFunc: procedure(sfactor: GLenum; dfactor: GLenum); apicall;
  glBlendFuncSeparate: procedure(srcRGB: GLenum; dstRGB: GLenum; srcAlpha: GLenum; dstAlpha: GLenum); apicall;
  glBufferData: procedure(target: GLenum; size: GLsizeiptr; data: Pointer; usage: GLenum); apicall;
  glBufferSubData: procedure(target: GLenum; offset: GLintptr; size: GLsizeiptr; data: Pointer); apicall;
  glCheckFramebufferStatus: function(target: GLenum): GLenum; apicall;
  glClear: procedure(mask: GLbitfield); apicall;
  glClearColor: procedure(red: GLclampf; green: GLclampf; blue: GLclampf; alpha: GLclampf); apicall;
  glClearDepthf: procedure(depth: GLclampf); apicall;
  glClearStencil: procedure(s: GLint); apicall;
  glColorMask: procedure(red: GLboolean; green: GLboolean; blue: GLboolean; alpha: GLboolean); apicall;
  glCompileShader: procedure(shader: GLuint); apicall;
  glCompressedTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; imageSize: GLsizei; data: Pointer); apicall;
  glCompressedTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; imageSize: GLsizei; data: Pointer); apicall;
  glCopyTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; x: GLint; y: GLint; width: GLsizei; height: GLsizei; border: GLint); apicall;
  glCopyTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glCreateProgram: function: GLuint; apicall;
  glCreateShader: function(_type: GLenum): GLuint; apicall;
  glCullFace: procedure(mode: GLenum); apicall;
  glDeleteBuffers: procedure(n: GLsizei; buffers: pGLuint); apicall;
  glDeleteFramebuffers: procedure(n: GLsizei; framebuffers: pGLuint); apicall;
  glDeleteProgram: procedure(_program: GLuint); apicall;
  glDeleteRenderbuffers: procedure(n: GLsizei; renderbuffers: pGLuint); apicall;
  glDeleteShader: procedure(shader: GLuint); apicall;
  glDeleteTextures: procedure(n: GLsizei; textures: pGLuint); apicall;
  glDepthFunc: procedure(func: GLenum); apicall;
  glDepthMask: procedure(flag: GLboolean); apicall;
  glDepthRangef: procedure(zNear: GLclampf; zFar: GLclampf); apicall;
  glDetachShader: procedure(_program: GLuint; shader: GLuint); apicall;
  glDisable: procedure(cap: GLenum); apicall;
  glDisableVertexAttribArray: procedure(index: GLuint); apicall;
  glDrawArrays: procedure(mode: GLenum; first: GLint; count: GLsizei); apicall;
  glDrawElements: procedure(mode: GLenum; count: GLsizei; _type: GLenum; indices: Pointer); apicall;
  glEnable: procedure(cap: GLenum); apicall;
  glEnableVertexAttribArray: procedure(index: GLuint); apicall;
  glFinish: procedure; apicall;
  glFlush: procedure; apicall;
  glFramebufferRenderbuffer: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); apicall;
  glFramebufferTexture2D: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); apicall;
  glFrontFace: procedure(mode: GLenum); apicall;
  glGenBuffers: procedure(n: GLsizei; buffers: pGLuint); apicall;
  glGenerateMipmap: procedure(target: GLenum); apicall;
  glGenFramebuffers: procedure(n: GLsizei; framebuffers: pGLuint); apicall;
  glGenRenderbuffers: procedure(n: GLsizei; renderbuffers: pGLuint); apicall;
  glGenTextures: procedure(n: GLsizei; textures: pGLuint); apicall;
  glGetActiveAttrib: procedure(_program: GLuint; index: GLuint; bufsize: GLsizei; length: pGLsizei; size: pGLint; _type: pGLenum; name: pchar); apicall;
  glGetActiveUniform: procedure(_program: GLuint; index: GLuint; bufsize: GLsizei; length: pGLsizei; size: pGLint; _type: pGLenum; name: pchar); apicall;
  glGetAttachedShaders: procedure(_program: GLuint; maxcount: GLsizei; count: pGLsizei; shaders: pGLuint); apicall;
  glGetAttribLocation: function(_program: GLuint; name: pchar): GLint; apicall;
  glGetBooleanv: procedure(pname: GLenum; params: pGLboolean); apicall;
  glGetBufferParameteriv: procedure(target: GLenum; pname: GLenum; params: pGLint); apicall;
  glGetError: function: GLenum; apicall;
  glGetFloatv: procedure(pname: GLenum; params: pGLfloat); apicall;
  glGetFramebufferAttachmentParameteriv: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: pGLint); apicall;
  glGetIntegerv: procedure(pname: GLenum; params: pGLint); apicall;
  glGetProgramiv: procedure(_program: GLuint; pname: GLenum; params: pGLint); apicall;
  glGetProgramInfoLog: procedure(_program: GLuint; bufsize: GLsizei; length: pGLsizei; infolog: pchar); apicall;
  glGetRenderbufferParameteriv: procedure(target: GLenum; pname: GLenum; params: pGLint); apicall;
  glGetShaderiv: procedure(shader: GLuint; pname: GLenum; params: pGLint); apicall;
  glGetShaderInfoLog: procedure(shader: GLuint; bufsize: GLsizei; length: pGLsizei; infolog: pchar); apicall;
  glGetShaderPrecisionFormat: procedure(shadertype: GLenum; precisiontype: GLenum; range: pGLint; precision: pGLint); apicall;
  glGetShaderSource: procedure(shader: GLuint; bufsize: GLsizei; length: pGLsizei; source: pchar); apicall;
  glGetString: function(name: GLenum): PGLchar; apicall;
  glGetTexParameterfv: procedure(target: GLenum; pname: GLenum; params: pGLfloat); apicall;
  glGetTexParameteriv: procedure(target: GLenum; pname: GLenum; params: pGLint); apicall;
  glGetUniformfv: procedure(_program: GLuint; location: GLint; params: pGLfloat); apicall;
  glGetUniformiv: procedure(_program: GLuint; location: GLint; params: pGLint); apicall;
  glGetUniformLocation: function(_program: GLuint; name: pchar): GLint; apicall;
  glGetVertexAttribfv: procedure(index: GLuint; pname: GLenum; params: pGLfloat); apicall;
  glGetVertexAttribiv: procedure(index: GLuint; pname: GLenum; params: pGLint); apicall;
  glGetVertexAttribPointerv: procedure(index: GLuint; pname: GLenum; Pointer: PPointer); apicall;
  glHint: procedure(target: GLenum; mode: GLenum); apicall;
  glIsBuffer: function(buffer: GLuint): GLboolean; apicall;
  glIsEnabled: function(cap: GLenum): GLboolean; apicall;
  glIsFramebuffer: function(framebuffer: GLuint): GLboolean; apicall;
  glIsProgram: function(_program: GLuint): GLboolean; apicall;
  glIsRenderbuffer: function(renderbuffer: GLuint): GLboolean; apicall;
  glIsShader: function(shader: GLuint): GLboolean; apicall;
  glIsTexture: function(texture: GLuint): GLboolean; apicall;
  glLineWidth: procedure(width: GLfloat); apicall;
  glLinkProgram: procedure(_program: GLuint); apicall;
  glPixelStorei: procedure(pname: GLenum; param: GLint); apicall;
  glPolygonOffset: procedure(factor: GLfloat; units: GLfloat); apicall;
  glReadPixels: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei; format: GLenum;  _type: GLenum; pixels: Pointer); apicall;
  glReleaseShaderCompiler: procedure; apicall;
  glRenderbufferStorage: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); apicall;
  glSampleCoverage: procedure(value: GLclampf; invert: GLboolean); apicall;
  glScissor: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;
  glShaderBinary: procedure(n: GLsizei; shaders: pGLuint; binaryformat: GLenum; binary: Pointer; length: GLsizei); apicall;
  glShaderSource: procedure(shader: GLuint; count: GLsizei; _string: Ppchar; length: pGLint); apicall;
  glStencilFunc: procedure(func: GLenum; ref: GLint; mask: GLuint); apicall;
  glStencilFuncSeparate: procedure(face: GLenum; func: GLenum; ref: GLint; mask: GLuint); apicall;
  glStencilMask: procedure(mask: GLuint); apicall;
  glStencilMaskSeparate: procedure(face: GLenum; mask: GLuint); apicall;
  glStencilOp: procedure(fail: GLenum; zfail: GLenum; zpass: GLenum); apicall;
  glStencilOpSeparate: procedure(face: GLenum; fail: GLenum; zfail: GLenum; zpass: GLenum); apicall;
  glTexImage2D: procedure(target: GLenum; level: GLint; internalformat: GLenum; width: GLsizei; height: GLsizei; border: GLint; format: GLenum; _type: GLenum; pixels: Pointer); apicall;
  glTexParameterf: procedure(target: GLenum; pname: GLenum; param: GLfloat); apicall;
  glTexParameterfv: procedure(target: GLenum; pname: GLenum; params: pGLfloat); apicall;
  glTexParameteri: procedure(target: GLenum; pname: GLenum; param: GLint); apicall;
  glTexParameteriv: procedure(target: GLenum; pname: GLenum; params: pGLint); apicall;
  glTexSubImage2D: procedure(target: GLenum; level: GLint; xoffset: GLint; yoffset: GLint; width: GLsizei; height: GLsizei; format: GLenum; _type: GLenum; pixels: Pointer); apicall;
  glUniform1f: procedure(location: GLint; x: GLfloat); apicall;
  glUniform1fv: procedure(location: GLint; count: GLsizei; v: pGLfloat); apicall;
  glUniform1i: procedure(location: GLint; x: GLint); apicall;
  glUniform1iv: procedure(location: GLint; count: GLsizei; v: pGLint); apicall;
  glUniform2f: procedure(location: GLint; x: GLfloat; y: GLfloat); apicall;
  glUniform2fv: procedure(location: GLint; count: GLsizei; v: pGLfloat); apicall;
  glUniform2i: procedure(location: GLint; x: GLint; y: GLint); apicall;
  glUniform2iv: procedure(location: GLint; count: GLsizei; v: pGLint); apicall;
  glUniform3f: procedure(location: GLint; x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glUniform3fv: procedure(location: GLint; count: GLsizei; v: pGLfloat); apicall;
  glUniform3i: procedure(location: GLint; x: GLint; y: GLint; z: GLint); apicall;
  glUniform3iv: procedure(location: GLint; count: GLsizei; v: pGLint); apicall;
  glUniform4f: procedure(location: GLint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); apicall;
  glUniform4fv: procedure(location: GLint; count: GLsizei; v: pGLfloat); apicall;
  glUniform4i: procedure(location: GLint; x: GLint; y: GLint; z: GLint; w: GLint); apicall;
  glUniform4iv: procedure(location: GLint; count: GLsizei; v: pGLint); apicall;
  glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: pGLfloat); apicall;
  glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: pGLfloat); apicall;
  glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; value: pGLfloat); apicall;
  glUseProgram: procedure(_program: GLuint); apicall;
  glValidateProgram: procedure(_program: GLuint); apicall;
  glVertexAttrib1f: procedure(index: GLuint; x: GLfloat); apicall;
  glVertexAttrib1fv: procedure(index: GLuint; values: pGLfloat); apicall;
  glVertexAttrib2f: procedure(index: GLuint; x: GLfloat; y: GLfloat); apicall;
  glVertexAttrib2fv: procedure(index: GLuint; values: pGLfloat); apicall;
  glVertexAttrib3f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); apicall;
  glVertexAttrib3fv: procedure(index: GLuint; values: pGLfloat); apicall;
  glVertexAttrib4f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); apicall;
  glVertexAttrib4fv: procedure(index: GLuint; values: pGLfloat); apicall;
  glVertexAttribPointer: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; offset: GLvoid); apicall;
  glViewport: procedure(x: GLint; y: GLint; width: GLsizei; height: GLsizei); apicall;

implementation

var
  Loaded: Boolean;
  Success: Boolean;

function LoadGL(GetProc: TGetProcAddress): Boolean;

  procedure Load(var P: Pointer; Name: PChar);
  begin
    P := GetProc(Name);
    Success := Success and (P <> nil);
  end;

begin
  Result := Success;
  if Loaded then
    Exit;
  Loaded := True;
  Success := True;
  Load(@glActiveTexture, 'glActiveTexture');
  Load(@glAttachShader, 'glAttachShader');
  Load(@glBindAttribLocation, 'glBindAttribLocation');
  Load(@glBindBuffer, 'glBindBuffer');
  Load(@glBindFramebuffer, 'glBindFramebuffer');
  Load(@glBindRenderbuffer, 'glBindRenderbuffer');
  Load(@glBindTexture, 'glBindTexture');
  Load(@glBlendColor, 'glBlendColor');
  Load(@glBlendEquation, 'glBlendEquation');
  Load(@glBlendEquationSeparate, 'glBlendEquationSeparate');
  Load(@glBlendFunc, 'glBlendFunc');
  Load(@glBlendFuncSeparate, 'glBlendFuncSeparate');
  Load(@glBufferData, 'glBufferData');
  Load(@glBufferSubData, 'glBufferSubData');
  Load(@glCheckFramebufferStatus, 'glCheckFramebufferStatus');
  Load(@glClear, 'glClear');
  Load(@glClearColor, 'glClearColor');
  Load(@glClearDepthf, 'glClearDepthf');
  Load(@glClearStencil, 'glClearStencil');
  Load(@glColorMask, 'glColorMask');
  Load(@glCompileShader, 'glCompileShader');
  Load(@glCompressedTexImage2D, 'glCompressedTexImage2D');
  Load(@glCompressedTexSubImage2D, 'glCompressedTexSubImage2D');
  Load(@glCopyTexImage2D, 'glCopyTexImage2D');
  Load(@glCopyTexSubImage2D, 'glCopyTexSubImage2D');
  Load(@glCreateProgram, 'glCreateProgram');
  Load(@glCreateShader, 'glCreateShader');
  Load(@glCullFace, 'glCullFace');
  Load(@glDeleteBuffers, 'glDeleteBuffers');
  Load(@glDeleteFramebuffers, 'glDeleteFramebuffers');
  Load(@glDeleteProgram, 'glDeleteProgram');
  Load(@glDeleteRenderbuffers, 'glDeleteRenderbuffers');
  Load(@glDeleteShader, 'glDeleteShader');
  Load(@glDeleteTextures, 'glDeleteTextures');
  Load(@glDepthFunc, 'glDepthFunc');
  Load(@glDepthMask, 'glDepthMask');
  Load(@glDepthRangef, 'glDepthRangef');
  Load(@glDetachShader, 'glDetachShader');
  Load(@glDisable, 'glDisable');
  Load(@glDisableVertexAttribArray, 'glDisableVertexAttribArray');
  Load(@glDrawArrays, 'glDrawArrays');
  Load(@glDrawElements, 'glDrawElements');
  Load(@glEnable, 'glEnable');
  Load(@glEnableVertexAttribArray, 'glEnableVertexAttribArray');
  Load(@glFinish, 'glFinish');
  Load(@glFlush, 'glFlush');
  Load(@glFramebufferRenderbuffer, 'glFramebufferRenderbuffer');
  Load(@glFramebufferTexture2D, 'glFramebufferTexture2D');
  Load(@glFrontFace, 'glFrontFace');
  Load(@glGenBuffers, 'glGenBuffers');
  Load(@glGenerateMipmap, 'glGenerateMipmap');
  Load(@glGenFramebuffers, 'glGenFramebuffers');
  Load(@glGenRenderbuffers, 'glGenRenderbuffers');
  Load(@glGenTextures, 'glGenTextures');
  Load(@glGetActiveAttrib, 'glGetActiveAttrib');
  Load(@glGetActiveUniform, 'glGetActiveUniform');
  Load(@glGetAttachedShaders, 'glGetAttachedShaders');
  Load(@glGetAttribLocation, 'glGetAttribLocation');
  Load(@glGetBooleanv, 'glGetBooleanv');
  Load(@glGetBufferParameteriv, 'glGetBufferParameteriv');
  Load(@glGetError, 'glGetError');
  Load(@glGetFloatv, 'glGetFloatv');
  Load(@glGetFramebufferAttachmentParameteriv, 'glGetFramebufferAttachmentParameteriv');
  Load(@glGetIntegerv, 'glGetIntegerv');
  Load(@glGetProgramiv, 'glGetProgramiv');
  Load(@glGetProgramInfoLog, 'glGetProgramInfoLog');
  Load(@glGetRenderbufferParameteriv, 'glGetRenderbufferParameteriv');
  Load(@glGetShaderiv, 'glGetShaderiv');
  Load(@glGetShaderInfoLog, 'glGetShaderInfoLog');
  Load(@glGetShaderPrecisionFormat, 'glGetShaderPrecisionFormat');
  Load(@glGetShaderSource, 'glGetShaderSource');
  Load(@glGetString, 'glGetString');
  Load(@glGetTexParameterfv, 'glGetTexParameterfv');
  Load(@glGetTexParameteriv, 'glGetTexParameteriv');
  Load(@glGetUniformfv, 'glGetUniformfv');
  Load(@glGetUniformiv, 'glGetUniformiv');
  Load(@glGetUniformLocation, 'glGetUniformLocation');
  Load(@glGetVertexAttribfv, 'glGetVertexAttribfv');
  Load(@glGetVertexAttribiv, 'glGetVertexAttribiv');
  Load(@glGetVertexAttribPointerv, 'glGetVertexAttribPointerv');
  Load(@glHint, 'glHint');
  Load(@glIsBuffer, 'glIsBuffer');
  Load(@glIsEnabled, 'glIsEnabled');
  Load(@glIsFramebuffer, 'glIsFramebuffer');
  Load(@glIsProgram, 'glIsProgram');
  Load(@glIsRenderbuffer, 'glIsRenderbuffer');
  Load(@glIsShader, 'glIsShader');
  Load(@glIsTexture, 'glIsTexture');
  Load(@glLineWidth, 'glLineWidth');
  Load(@glLinkProgram, 'glLinkProgram');
  Load(@glPixelStorei, 'glPixelStorei');
  Load(@glPolygonOffset, 'glPolygonOffset');
  Load(@glReadPixels, 'glReadPixels');
  Load(@glReleaseShaderCompiler, 'glReleaseShaderCompiler');
  Load(@glRenderbufferStorage, 'glRenderbufferStorage');
  Load(@glSampleCoverage, 'glSampleCoverage');
  Load(@glScissor, 'glScissor');
  Load(@glShaderBinary, 'glShaderBinary');
  Load(@glShaderSource, 'glShaderSource');
  Load(@glStencilFunc, 'glStencilFunc');
  Load(@glStencilFuncSeparate, 'glStencilFuncSeparate');
  Load(@glStencilMask, 'glStencilMask');
  Load(@glStencilMaskSeparate, 'glStencilMaskSeparate');
  Load(@glStencilOp, 'glStencilOp');
  Load(@glStencilOpSeparate, 'glStencilOpSeparate');
  Load(@glTexImage2D, 'glTexImage2D');
  Load(@glTexParameterf, 'glTexParameterf');
  Load(@glTexParameterfv, 'glTexParameterfv');
  Load(@glTexParameteri, 'glTexParameteri');
  Load(@glTexParameteriv, 'glTexParameteriv');
  Load(@glTexSubImage2D, 'glTexSubImage2D');
  Load(@glUniform1f, 'glUniform1f');
  Load(@glUniform1fv, 'glUniform1fv');
  Load(@glUniform1i, 'glUniform1i');
  Load(@glUniform1iv, 'glUniform1iv');
  Load(@glUniform2f, 'glUniform2f');
  Load(@glUniform2fv, 'glUniform2fv');
  Load(@glUniform2i, 'glUniform2i');
  Load(@glUniform2iv, 'glUniform2iv');
  Load(@glUniform3f, 'glUniform3f');
  Load(@glUniform3fv, 'glUniform3fv');
  Load(@glUniform3i, 'glUniform3i');
  Load(@glUniform3iv, 'glUniform3iv');
  Load(@glUniform4f, 'glUniform4f');
  Load(@glUniform4fv, 'glUniform4fv');
  Load(@glUniform4i, 'glUniform4i');
  Load(@glUniform4iv, 'glUniform4iv');
  Load(@glUniformMatrix2fv, 'glUniformMatrix2fv');
  Load(@glUniformMatrix3fv, 'glUniformMatrix3fv');
  Load(@glUniformMatrix4fv, 'glUniformMatrix4fv');
  Load(@glUseProgram, 'glUseProgram');
  Load(@glValidateProgram, 'glValidateProgram');
  Load(@glVertexAttrib1f, 'glVertexAttrib1f');
  Load(@glVertexAttrib1fv, 'glVertexAttrib1fv');
  Load(@glVertexAttrib2f, 'glVertexAttrib2f');
  Load(@glVertexAttrib2fv, 'glVertexAttrib2fv');
  Load(@glVertexAttrib3f, 'glVertexAttrib3f');
  Load(@glVertexAttrib3fv, 'glVertexAttrib3fv');
  Load(@glVertexAttrib4f, 'glVertexAttrib4f');
  Load(@glVertexAttrib4fv, 'glVertexAttrib4fv');
  Load(@glVertexAttribPointer, 'glVertexAttribPointer');
  Load(@glViewport, 'glViewport');
  Result :=  Success;
end;

end.

