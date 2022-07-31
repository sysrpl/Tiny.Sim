unit Tiny.Interop.NanoVG;

{$i tiny.inc}
{$define libnanovg := external}

interface

uses
  Tiny.Types;

{ LoadNanoVG must be called prior to using a NVGcontext }

function LoadNanoVG(GetProc: TGetProcAddress): Boolean;

const
  {$ifdef gles2}
  SNanoVGBackend = 'GLESv2';
  {$else}
  SNanoVGBackend = 'GL3';
  {$endif}

// NanoVG is small antialiased vector graphics rendering library for OpenGL.
// It has lean API modeled after HTML5 canvas API. It is aimed to be a practical
// and fun toolset for building scalable user interfaces and visualizations.

type
  Float = Single;
  PFloat = ^Float;

  NVGcontext = ^NVGcontextStruct;
  NVGcontextStruct = record end;

// Create flags
type
  NVGCreateFlags = Integer;

const
	// Flag indicating if geometry based anti-aliasing is used (may not be needed when using MSAA).
  NVG_ANTIALIAS = 1;
	// Flag indicating if strokes should be drawn using stencil buffer. The rendering will be a little
	// slower, but path overlaps (i.e. self-intersecting or sharp turns) will be drawn just once.
	NVG_STENCIL_STROKES = 1 shl 1;
	// Flag indicating that additional debug checks are done.
	NVG_DEBUG = 1 shl 2;

{$ifdef gles2}
function nvgCreateGLES2(flags: NVGCreateFlags): NVGcontext; cdecl; libnanovg;
procedure nvgDeleteGLES2(ctx: NVGcontext); cdecl; libnanovg;
{$elseif defined(gl3)}
function nvgCreateGL3(flags: NVGCreateFlags): NVGcontext; cdecl; libnanovg;
procedure nvgDeleteGL3(ctx: NVGcontext); cdecl; libnanovg;
{$endif}

type
  NVGcolor = record
  case Integer of
    0: (rgba: array[0..3] of Float);
  	1: (r, g, b, a: Float);
  end;

  NVxform = array[0..5] of Float;

  NVGpaint = record
  	xform: NVxform;
  	extent: array[0..1] of Float;
  	radius: Float;
  	feather: Float;
  	innerColor: NVGcolor;
  	outerColor: NVGcolor;
  	image: Integer;
  end;

  NVGwinding = (
    // Winding for solid shapes
  	NVG_CCW = 1,
    // Winding for holes
  	NVG_CW = 2
  );

  NVGsolidity = (
    // CCW
  	NVG_SOLID = 1,
    // CW
  	NVG_HOLE = 2
  );

  NVGlineCapJoin = (
  	NVG_BUTT,
  	NVG_ROUND,
  	NVG_SQUARE,
  	NVG_BEVEL,
  	NVG_MITER
  );

  NVGalign = Integer;

// Horizontal align
const
  // Default, align text horizontally to left.
  NVG_ALIGN_LEFT = 1 shl 0;
  // Align text horizontally to center.
  NVG_ALIGN_CENTER = 1 shl 1;
  // Align text horizontally to right.
  NVG_ALIGN_RIGHT = 1 shl 2;

// Vertical align
const
  // Align text vertically to top.
  NVG_ALIGN_TOP = 1 shl 3;
  // Align text vertically to middle.
  NVG_ALIGN_MIDDLE = 1 shl 4;
  // Align text vertically to bottom.
  NVG_ALIGN_BOTTOM = 1 shl 5;
  // Default, align text vertically to baseline.
  NVG_ALIGN_BASELINE = 1 shl 6;

type
  NVGblendFactor = (
  	NVG_ZERO = 1 shl 0,
  	NVG_ONE = 1 shl 1,
  	NVG_SRC_COLOR = 1 shl 2,
  	NVG_ONE_MINUS_SRC_COLOR = 1 shl 3,
  	NVG_DST_COLOR = 1 shl 4,
  	NVG_ONE_MINUS_DST_COLOR = 1 shl 5,
  	NVG_SRC_ALPHA = 1 shl 6,
  	NVG_ONE_MINUS_SRC_ALPHA = 1 shl 7,
  	NVG_DST_ALPHA = 1 shl 8,
  	NVG_ONE_MINUS_DST_ALPHA = 1 shl 9,
  	NVG_SRC_ALPHA_SATURATE = 1 shl 10
  );

  NVGcompositeOperation = (
  	NVG_SOURCE_OVER,
  	NVG_SOURCE_IN,
  	NVG_SOURCE_OUT,
  	NVG_ATOP,
  	NVG_DESTINATION_OVER,
  	NVG_DESTINATION_IN,
  	NVG_DESTINATION_OUT,
  	NVG_DESTINATION_ATOP,
  	NVG_LIGHTER,
  	NVG_COPY,
  	NVG_XOR
  );

  NVGcompositeOperationState = record
  	srcRGB: Integer;
  	dstRGB: Integer;
  	srcAlpha: Integer;
  	dstAlpha: Integer;
  end;

  NVGglyphPosition = record
    // Position of the glyph in the input string.
  	str: PChar;
    // The x-coordinate of the logical glyph position.
  	x: Float;
    // The bounds of the glyph shape.
  	minx, maxx: Float;
  end;
  PNVGglyphPosition = ^NVGglyphPosition;

  NVGtextRow = record
    // Pointer to the input text where the row starts.
  	start: PChar;
    // Pointer to the input text where the row ends (one past the last character).
  	finish: PChar;
    // Pointer to the beginning of the next row.
  	next: PChar;
    // Logical width of the row.
  	width: Float;
    // Actual bounds of the row. Logical with and bounds can differ because of kerning and some parts over extending.
  	minx, maxx: Float;
  end;
  PNVGtextRow = ^NVGtextRow;

  NVGimageFlags = Integer;

const
  // Generate mipmaps during creation of the image.
  NVG_IMAGE_GENERATE_MIPMAPS = 1 shl 0;
  // Repeat image in X direction.
  NVG_IMAGE_REPEATX = 1 shl 1;
  // Repeat image in Y direction.
  NVG_IMAGE_REPEATY = 1 shl 2;
  // Flips (inverses) image in Y direction when rendered.
  NVG_IMAGE_FLIPY = 1 shl 3;
  // Image data has premultiplied alpha.
  NVG_IMAGE_PREMULTIPLIED = 1 shl 4;
  // Image interpolation is Nearest instead Linear
  NVG_IMAGE_NEAREST = 1 shl 5;

// Begin drawing a new frame
// Calls to nanovg drawing API should be wrapped in nvgBeginFrame() & nvgEndFrame()
// nvgBeginFrame() defines the size of the window to render to in relation currently
// set viewport (i.e. glViewport on GL backends). Device pixel ration allows to
// control the rendering on Hi-DPI devices.
// For example, GLFW returns two dimension for an opened window: window size and
// frame buffer size. In that case you would set windowWidth/Height to the window size
// devicePixelRatio to: frameBufferWidth / windowWidth.
procedure nvgBeginFrame(ctx: NVGcontext; windowWidth: Float; windowHeight: Float; devicePixelRatio: Float); cdecl; libnanovg;
// Cancels drawing the current frame.
procedure nvgCancelFrame(ctx: NVGcontext); cdecl; libnanovg;
// Ends drawing flushing remaining render state.
procedure nvgEndFrame(ctx: NVGcontext); cdecl; libnanovg;

//
// Composite operation
//
// The composite operations in NanoVG are modeled after HTML Canvas API, and
// the blend func is based on OpenGL (see corresponding manuals for more info).
// The colors in the blending state have premultiplied alpha.

// Sets the composite operation. The op parameter should be one of NVGcompositeOperation.
procedure nvgGlobalCompositeOperation(ctx: NVGcontext; op: NVGcompositeOperation); cdecl; libnanovg;
// Sets the composite operation with custom pixel arithmetic.
// The parameters should be one of NVGblendFactor.
procedure nvgGlobalCompositeBlendFunc(ctx: NVGcontext; sfactor, dfactor: NVGblendFactor); cdecl; libnanovg;
// Sets the composite operation with custom pixel arithmetic for RGB and alpha components separately.
// The parameters should be one of NVGblendFactor.
procedure nvgGlobalCompositeBlendFuncSeparate(ctx: NVGcontext; srcRGB, dstRGB, srcAlpha, dstAlpha: NVGblendFactor); cdecl; libnanovg;

//
// Color utils
//
// Colors in NanoVG are stored as unsigned Integers in ABGR format.

// Returns a color value from red, green, blue values. Alpha will be set to 255 (1.0f).
function nvgRGB(r, g, b: Byte): NVGcolor; cdecl; libnanovg;
// Returns a color value from red, green, blue values. Alpha will be set to 1.0f.
function nvgRGBf(r, g, b: Float): NVGcolor; cdecl; libnanovg;
// Returns a color value from red, green, blue and alpha values.
function nvgRGBA(r, g, b, a: Byte): NVGcolor; cdecl; libnanovg;
// Returns a color value from red, green, blue and alpha values.
function nvgRGBAf(r, g, b, a: Float): NVGcolor; cdecl; libnanovg;
// Linearly Integererpolates from color c0 to c1, and returns resulting color value.
function nvgLerpRGBA(c0, c1: NVGcolor; u: Float): NVGcolor; cdecl; libnanovg;
// Sets transparency of a color value.
function nvgTransRGBA(c0: NVGcolor; a: Byte): NVGcolor; cdecl; libnanovg;
// Sets transparency of a color value.
function nvgTransRGBAf(c0: NVGcolor; a: Float): NVGcolor; cdecl; libnanovg;
// Returns color value specified by hue, saturation and lightness.
// HSL values are all in range [0..1], alpha will be set to 255.
function nvgHSL(h, s, l: Float): NVGcolor; cdecl; libnanovg;
// Returns color value specified by hue, saturation and lightness and alpha.
// HSL values are all in range [0..1], alpha in range [0..255]
function nvgHSLA(h, s, l: Float; a: Byte): NVGcolor; cdecl; libnanovg;

//
// State Handling
//
// NanoVG contains state which represents how paths will be rendered.
// The state contains transform, fill and stroke styles, text and font styles,
// and scissor clipping.

// Pushes and saves the current render state into a state stack.
// A matching nvgRestore() must be used to restore the state.
procedure nvgSave(ctx: NVGcontext); cdecl; libnanovg;
// Pops and restores current render state.
procedure nvgRestore(ctx: NVGcontext); cdecl; libnanovg;
// Resets current render state to default values. Does not affect the render state stack.
procedure nvgReset(ctx: NVGcontext); cdecl; libnanovg;

//
// Render styles
//
// Fill and stroke render style can be either a solid color or a paintwhich is a gradient or a pattern.
// Solid color is simply defined as a color value, different kinds of Paints can be created
// using nvgLinearGradient(), nvgBoxGradient(), nvgRadialGradient() and nvgImagePattern().
//
// Current render style can be saved and restored using nvgSave() and nvgRestore().

// Sets whether to draw antialias for nvgStroke() and nvgFill(). It's enabled by default.
procedure nvgShapeAntiAlias(ctx: NVGcontext; enabled: Integer); cdecl; libnanovg;
// Sets current stroke style to a solid color.
procedure nvgStrokeColor(ctx: NVGcontext; color: NVGcolor); cdecl; libnanovg;
// Sets current stroke style to a paint, which can be a one of the gradients or a pattern.
procedure nvgStrokePaint(ctx: NVGcontext; paint: NVGpaint); cdecl; libnanovg;
// Sets current fill style to a solid color.
procedure nvgFillColor(ctx: NVGcontext; color: NVGcolor); cdecl; libnanovg;
// Sets current fill style to a paint, which can be a one of the gradients or a pattern.
procedure nvgFillPaint(ctx: NVGcontext; paint: NVGpaint); cdecl; libnanovg;
// Sets the miter limit of the stroke style.
// Miter limit controls when a sharp corner is beveled.
procedure nvgMiterLimit(ctx: NVGcontext; limit: Float); cdecl; libnanovg;
// Sets the stroke width of the stroke style.
procedure nvgStrokeWidth(ctx: NVGcontext; size: Float); cdecl; libnanovg;
// Sets how the end of the line (cap) is drawn,
// Can be one of: NVG_BUTT (default), NVG_ROUND, NVG_SQUARE.
procedure nvgLineCap(ctx: NVGcontext; cap: NVGlineCapJoin); cdecl; libnanovg;
// Sets how sharp path corners are drawn.
// Can be one of NVG_MITER (default), NVG_ROUND, NVG_BEVEL.
procedure nvgLineJoin(ctx: NVGcontext; join: NVGlineCapJoin); cdecl; libnanovg;
// Sets the transparency applied to all rendered shapes.
// Already transparent paths will get proportionally more transparent as well.
procedure nvgGlobalAlpha(ctx: NVGcontext; alpha: Float); cdecl; libnanovg;

//
// Transforms
//
// The paths, gradients, patterns and scissor region are transformed by an transformation
// matrix at the time when they are passed to the API.
// The current transformation matrix is a affine matrix:
//   [sx kx ky]
//   [sy tx ty]
//   [ 0  0  1]
// Where: sx,sy define scaling, kx,ky skewing, and tx,ty translation.
// The last row is assumed to be 0,0,1 and is not stored.
//
// Apart from nvgResetTransform(), each transformation function first creates
// specific transformation matrix and pre-multiplies the current transformation by it.
//
// Current coordinate system (transformation) can be saved and restored using nvgSave() and nvgRestore().

// Resets current transform to a identity matrix.
procedure nvgResetTransform(ctx: NVGcontext); cdecl; libnanovg;
// Premultiplies current coordinate system by specified matrix.
// The parameters are Integererpreted as matrix as follows:
//   [a c e]
//   [b d f]
//   [0 0 1]
procedure nvgTransform(ctx: NVGcontext; a, b, c, d, e, f: Float); cdecl; libnanovg;
// Translates current coordinate system.
procedure nvgTranslate(ctx: NVGcontext; x, y: Float); cdecl; libnanovg;
// Rotates current coordinate system. Angle is specified in radians.
procedure nvgRotate(ctx: NVGcontext; angle: Float); cdecl; libnanovg;
// Skews the current coordinate system along X axis. Angle is specified in radians.
procedure nvgSkewX(ctx: NVGcontext; angle: Float); cdecl; libnanovg;
// Skews the current coordinate system along Y axis. Angle is specified in radians.
procedure nvgSkewY(ctx: NVGcontext; angle: Float); cdecl; libnanovg;
// Scales the current coordinate system.
procedure nvgScale(ctx: NVGcontext; x, y: Float); cdecl; libnanovg;
// Stores the top part (a-f) of the current transformation matrix in to the specified buffer.
//   [a c e]
//   [b d f]
//   [0 0 1]
// There should be space for 6 Floats in the return buffer for the values a-f.
procedure nvgCurrentTransform(ctx: NVGcontext; xform: PFloat); cdecl; libnanovg;

// The following functions can be used to make calculations on 2x3 transformation matrices.
// A 2x3 matrix is represented as Float[6].

// Sets the transform to identity matrix.
procedure nvgTransformIdentity(dst: PFloat); cdecl; libnanovg;
// Sets the transform to translation matrix matrix.
procedure nvgTransformTranslate(dst: PFloat; tx, ty: Float); cdecl; libnanovg;
// Sets the transform to scale matrix.
procedure nvgTransformScale(dst: PFloat; sx, sy: Float); cdecl; libnanovg;
// Sets the transform to rotate matrix. Angle is specified in radians.
procedure nvgTransformRotate(dst: PFloat; a: Float); cdecl; libnanovg;
// Sets the transform to skew-x matrix. Angle is specified in radians.
procedure nvgTransformSkewX(dst: PFloat; a: Float); cdecl; libnanovg;
// Sets the transform to skew-y matrix. Angle is specified in radians.
procedure nvgTransformSkewY(dst: PFloat; a: Float); cdecl; libnanovg;
// Sets the transform to the result of multiplication of two transforms, of A = A*B.
procedure nvgTransformMultiply(dst, src: PFloat); cdecl; libnanovg;
// Sets the transform to the result of multiplication of two transforms, of A = B*A.
procedure nvgTransformPremultiply(dst, src: PFloat); cdecl; libnanovg;
// Sets the destination to inverse of specified transform.
// Returns 1 if the inverse could be calculated, else 0.
function nvgTransformInverse(dst, src: PFloat): Integer; cdecl; libnanovg;
// Transform a point by given transform
procedure nvgTransformPoint(out dstx, dsty: Float; xform: PFloat; srcx, srcy: Float); cdecl; libnanovg;

// Converts degrees to radians and vice versa.
function nvgDegToRad(deg: Float): Float; cdecl; libnanovg;
function nvgRadToDeg(rad: Float): Float; cdecl; libnanovg;

//
// Images
//
// NanoVG allows you to load jpg, png, psd, tga, pic and gif files to be used for rendering.
// In addition you can upload your own image. The image loading is provided by stb_image.
// The parameter imageFlags is combination of flags defined in NVGimageFlags.

// Creates image by loading it from the disk from specified file name.
// Returns handle to the image.
function nvgCreateImage(ctx: NVGcontext; filename: PChar; imageFlags: Integer): Integer; cdecl; libnanovg;
// Creates image by loading it from the specified chunk of memory.
// Returns handle to the image.
function nvgCreateImageMem(ctx: NVGcontext; imageFlags: Integer; data: Pointer; ndata: LongWord): Integer; cdecl; libnanovg;
// Creates image from specified image data.
// Returns handle to the image.
function nvgCreateImageRGBA(ctx: NVGcontext; w: Integer; h: Integer; imageFlags: Integer; data: Pointer): Integer; cdecl; libnanovg;
// Updates image data specified by image handle.
procedure nvgUpdateImage(ctx: NVGcontext; image: Integer; data: Pointer); cdecl; libnanovg;
// Returns the dimensions of a created image.
procedure nvgImageSize(ctx: NVGcontext; image: Integer; out w, h: LongWord); cdecl; libnanovg;
// Deletes created image.
procedure nvgDeleteImage(ctx: NVGcontext; image: Integer); cdecl; libnanovg;

//
// Paints
//
// NanoVG supports four types of paints: linear gradient, box gradient, radial gradient and image pattern.
// These can be used as paints for strokes and fills.

// Creates and returns a linear gradient. Parameters (sx,sy)-(ex,ey) specify the start and end coordinates
// of the linear gradient, icol specifies the start color and ocol the end color.
// The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
function nvgLinearGradient(ctx: NVGcontext; sx, sy, ex, ey: Float; icol, ocol: NVGcolor): NVGpaint; cdecl; libnanovg;
// Creates and returns a box gradient. Box gradient is a feathered rounded rectangle, it is useful for rendering
// drop shadows or highlights for boxes. Parameters (x,y) define the top-left corner of the rectangle,
// (w,h) define the size of the rectangle, r defines the corner radius, and f feather. Feather defines how blurry
// the border of the rectangle is. Parameter icol specifies the inner color and ocol the outer color of the gradient.
// The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
function nvgBoxGradient(ctx: NVGcontext; x, y, w, h, r, f: Float; icol, ocol: NVGcolor): NVGpaint; cdecl; libnanovg;
// Creates and returns a radial gradient. Parameters (cx,cy) specify the center, inr and outr specify
// the inner and outer radius of the gradient, icol specifies the start color and ocol the end color.
// The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
function nvgRadialGradient(ctx: NVGcontext; cx, cy, inr, outr: Float; icol, ocol: NVGcolor): NVGpaint; cdecl; libnanovg;
// Creates and returns an image pattern. Parameters (ox,oy) specify the left-top location of the image pattern,
// (ex,ey) the size of one image, angle rotation around the top-left corner, image is handle to the image to render.
// The gradient is transformed by the current transform when it is passed to nvgFillPaint() or nvgStrokePaint().
function nvgImagePattern(ctx: NVGcontext; ox, oy, ex, ey, angle: Float; image: Integer; alpha: Float): NVGpaint; cdecl; libnanovg;

//
// Scissoring
//
// Scissoring allows you to clip the rendering into a rectangle. This is useful for various
// user Integererface cases like rendering a text edit or a timeline.

// Sets the current scissor rectangle.
// The scissor rectangle is transformed by the current transform.
procedure nvgScissor(ctx: NVGcontext; x, y, w, h: Float); cdecl; libnanovg;
// Intersects current scissor rectangle with the specified rectangle.
// The scissor rectangle is transformed by the current transform.
// Note: in case the rotation of previous scissor rect differs from
// the current one, the Integerersection will be done between the specified
// rectangle and the previous scissor rectangle transformed in the current
// transform space. The resulting shape is always rectangle.
procedure nvgIntersectScissor(ctx: NVGcontext; x, y, w, h: Float); cdecl; libnanovg;
// Reset and disables scissoring.
procedure nvgResetScissor(ctx: NVGcontext); cdecl; libnanovg;

//
// Paths
//
// Drawing a new shape starts with nvgBeginPath(), it clears all the currently defined paths.
// Then you define one or more paths and sub-paths which describe the shape. The are functions
// to draw common shapes like rectangles and circles, and lower level step-by-step functions,
// which allow to define a path curve by curve.
//
// NanoVG uses even-odd fill rule to draw the shapes. Solid shapes should have counter clockwise
// winding and holes should have counter clockwise order. To specify winding of a path you can
// call nvgPathWinding(). This is useful especially for the common shapes, which are drawn CCW.
//
// Finally you can fill the path using current fill style by calling nvgFill(), and stroke it
// with current stroke style by calling nvgStroke().
//
// The curve segments and sub-paths are transformed by the current transform.

// Clears the current path and sub-paths.
procedure nvgBeginPath(ctx: NVGcontext); cdecl; libnanovg;
// Starts new sub-path with specified pointas first point.
procedure nvgMoveTo(ctx: NVGcontext; x, y: Float); cdecl; libnanovg;
// Adds line segment from the last point the path to the specified point.
procedure nvgLineTo(ctx: NVGcontext; x, y: Float); cdecl; libnanovg;
// Adds cubic bezier segment from last point the path via two control points to the specified point.
procedure nvgBezierTo(ctx: NVGcontext; c1x, c1y, c2x, c2y, x, y: Float); cdecl; libnanovg;
// Adds quadratic bezier segment from last point the path via a control point the specified point.
procedure nvgQuadTo(ctx: NVGcontext; cx, cy, x, y: Float); cdecl; libnanovg;
// Adds an arc segment at the corner defined by the last path point, and two specified points.
procedure nvgArcTo(ctx: NVGcontext; x1, y1, x2, y2, radius: Float); cdecl; libnanovg;
// Closes current sub-path with a line segment.
procedure nvgClosePath(ctx: NVGcontext); cdecl; libnanovg;
// Sets the current sub-path winding, see NVGwinding and NVGsolidity.
procedure nvgPathWinding(ctx: NVGcontext; dir: NVGwinding); cdecl; libnanovg;

// Creates new circle arc shaped sub-path. The arc center is at cx,cy, the arc radius is r,
// and the arc is drawn from angle a0 to a1, and swept in direction dir (NVG_CCW, or NVG_CW).
// Angles are specified in radians.
procedure nvgArc(ctx: NVGcontext; cx, cy, r, a0, a1: Float; dir: NVGwinding); cdecl; libnanovg;
// Creates new rectangle shaped sub-path.
procedure nvgRect(ctx: NVGcontext; x, y, w, h: Float); cdecl; libnanovg;
// Creates new rounded rectangle shaped sub-path.
procedure nvgRoundedRect(ctx: NVGcontext; x, y, w, h, r: Float); cdecl; libnanovg;
// Creates new rounded rectangle shaped sub-path with varying radii for each corner.
procedure nvgRoundedRectVarying(ctx: NVGcontext; x, y, w, h, radTopLeft, radTopRight, radBottomRight, radBottomLeft: Float); cdecl; libnanovg;
// Creates new ellipse shaped sub-path.
procedure nvgEllipse(ctx: NVGcontext; cx, cy, rx, ry: Float); cdecl; libnanovg;
// Creates new circle shaped sub-path.
procedure nvgCircle(ctx: NVGcontext; cx, cy, r: Float); cdecl; libnanovg;
// Fills the current path with current fill style.
procedure nvgFill(ctx: NVGcontext); cdecl; libnanovg;
// Fills the current path with current stroke style.
procedure nvgStroke(ctx: NVGcontext); cdecl; libnanovg;

//
// Text
//
// NanoVG allows you to load .ttf files and use the font to render text.
//
// The appearance of the text can be defined by setting the current text style
// and by specifying the fill color. Common text and font settings such as
// font size, letter spacing and text align are supported. Font blur allows you
// to create simple text effects such as drop shadows.
//
// At render time the font face can be set based on the font handles or name.
//
// Font measure functions return values in local space, the calculations are
// carried in the same resolution as the final rendering. This is done because
// the text glyph positions are snapped to the nearest pixels sharp rendering.
//
// The local space means that values are not rotated or scale as per the current
// transformation. For example if you set font size to 12, which would mean that
// line height is 16, then regardless of the current scaling and rotation, the
// returned line height is always 16. Some measures may vary because of the scaling
// since aforementioned pixel snapping.
//
// While this may sound a little odd, the setup allows you to always render the
// same way regardless of scaling. I.e. following works regardless of scaling:
//
//		PChar txt = "Text me up.";
//		nvgTextBounds(vg, x,y, txt, NULL, bounds);
//		nvgBeginPath(vg);
//		nvgRoundedRect(vg, bounds[0],bounds[1], bounds[2]-bounds[0], bounds[3]-bounds[1]);
//		nvgFill(vg);
//
// Note: currently only solid color fill is supported for text.

// Creates font by loading it from the disk from specified file name.
// Returns handle to the font.
function nvgCreateFont(ctx: NVGcontext; name, filename: PChar): Integer; cdecl; libnanovg;
// fontIndex specifies which font face to load from a .ttf/.ttc file.
function nvgCreateFontAtIndex(ctx: NVGcontext; name, filename: PChar; fontIndex: Integer): Integer; cdecl; libnanovg;
// Creates font by loading it from the specified memory chunk.
// Returns handle to the font.
function nvgCreateFontMem(ctx: NVGcontext; name: PChar; data: Pointer; ndata, freeData: LongWord): Integer; cdecl; libnanovg;
// fontIndex specifies which font face to load from a .ttf/.ttc file.
function nvgCreateFontMemAtIndex(ctx: NVGcontext; name: PChar; data: Pointer; ndata, freeData, fontIndex: Integer): Integer; cdecl; libnanovg;
// Finds a loaded font of specified name, and returns handle to it, or -1 if the font is not found.
function nvgFindFont(ctx: NVGcontext; name: PChar): Integer; cdecl; libnanovg;
// Adds a fallback font by handle.
function nvgAddFallbackFontId(ctx: NVGcontext; baseFont: Integer; fallbackFont: Integer): Integer; cdecl; libnanovg;
// Adds a fallback font by name.
function nvgAddFallbackFont(ctx: NVGcontext; baseFont: PChar; fallbackFont: PChar): Integer; cdecl; libnanovg;
// Resets fallback fonts by handle.
procedure nvgResetFallbackFontsId(ctx: NVGcontext; baseFont: Integer); cdecl; libnanovg;
// Resets fallback fonts by name.
procedure nvgResetFallbackFonts(ctx: NVGcontext; baseFont: PChar); cdecl; libnanovg;
// Sets the font size of current text style.
procedure nvgFontSize(ctx: NVGcontext; size: Float); cdecl; libnanovg;
// Sets the blur of current text style.
procedure nvgFontBlur(ctx: NVGcontext; blur: Float); cdecl; libnanovg;
// Sets the letter spacing of current text style.
procedure nvgTextLetterSpacing(ctx: NVGcontext; spacing: Float); cdecl; libnanovg;
// Sets the proportional line height of current text style. The line height is specified as multiple of font size.
procedure nvgTextLineHeight(ctx: NVGcontext; lineHeight: Float); cdecl; libnanovg;
// Sets the text align of current style: text; see NVGalign for options.
procedure nvgTextAlign(ctx: NVGcontext; align: NVGalign); cdecl; libnanovg;
// Sets the font face based on specified id of current text style.
procedure nvgFontFaceId(ctx: NVGcontext; font: Integer); cdecl; libnanovg;
// Sets the font face based on specified name of current text style.
procedure nvgFontFace(ctx: NVGcontext; font: PChar); cdecl; libnanovg;
// Draws text string at specified location. If end is specified only the sub-string up to the end is drawn.
function nvgText(ctx: NVGcontext; x, y: Float; text, cut: PChar): Float; cdecl; libnanovg;
// Draws multi-line text string at specified location wrapped at the specified width.
// If end is specified only the sub-string up to the end is drawn.
// White space is stripped at the beginning of the rows, the text is split at word
// boundaries or when new-line characters are encountered.
// Words longer than the max width are slit at nearest character (i.e. no hyphenation).
procedure nvgTextBox(ctx: NVGcontext; x, y, breakRowWidth: Float; text, cut: PChar); cdecl; libnanovg;
// Measures the specified text string. Parameter bounds should be a pointer to Float[4],
// if the bounding box of the text should be returned. The bounds value are [xmin,ymin, xmax,ymax]
// Returns the horizontal advance of the measured text (i.e. where the next character should drawn).
// Measured values are returned in local coordinate space.
function nvgTextBounds(ctx: NVGcontext; x, y: Float; text, cut: PChar; bounds: PFloat): Float; cdecl; libnanovg;// Measures the specified multi-text string. Parameter bounds should be a pointer to Float[4],
// if the bounding box of the text should be returned. The bounds value are [xmin,ymin, xmax,ymax]
// Measured values are returned in local coordinate space.
procedure nvgTextBoxBounds(ctx: NVGcontext; x, y, breakRowWidth: Float; text, cut: PChar; bounds: PFloat); cdecl; libnanovg;
// Calculates the glyph x positions of the specified text. If end is specified only the sub-string will be used.
// Measured values are returned in local coordinate space.
function nvgTextGlyphPositions(ctx: NVGcontext; x, y: Float; text, cut: PChar; positions: PNVGglyphPosition; maxPositions: Integer): Integer; cdecl; libnanovg;// Returns the vertical metrics based on the current text style.
// Measured values are returned in local coordinate space.
procedure nvgTextMetrics(ctx: NVGcontext; out ascender, descender, lineh: Float); cdecl; libnanovg;// Breaks the specified text into lines. If end is specified only the sub-string will be used.
// White space is stripped at the beginning of the rows, the text is split at word boundaries or when new-line characters are encountered.
// Words longer than the max width are slit at nearest character (i.e. no hyphenation).
function nvgTextBreakLines(ctx: NVGcontext; text, cut: PChar; breakRowWidth: Float; rows: PNVGtextRow; maxRows: Integer): Integer; cdecl; libnanovg;

// NanoSVG is a simple stupid single-header-file SVG parse. The output of the parser is a list of cubic bezier shapes.
//
// The library suits well for anything from rendering scalable icons in your editor application to prototyping a game.
//
// NanoSVG supports a wide range of SVG features, but something may be missing, feel free to create a pull request!
//
// The shapes in the SVG images are transformed by the viewBox and converted to specified units.
// That is, you should get the same looking data as your designed in your favorite app.
//
// NanoSVG can return the paths in few different units. For example if you want to render an image, you may choose
// to get the paths in pixels, or if you are feeding the data into a CNC-cutter, you may want to use millimeters.
//
// The units passed to NanoSVG should be one of: 'px', 'pt', 'pc' 'mm', 'cm', or 'in'.
// DPI (dots-per-inch) controls how the unit conversion is done.
//
// If you don't know or care about the units stuff, "px" and 96 should get you going.

(* Example Usage:
	// Load SVG
	NSVGimage* image;
	image = nsvgParseFromFile("test.svg", "px", 96);
	printf("size: %f x %f\n", image->width, image->height);
	// Use...
	for (NSVGshape *shape = image->shapes; shape != NULL; shape = shape->next) {
		for (NSVGpath *path = shape->paths; path != NULL; path = path->next) {
			for (int i = 0; i < path->npts-1; i += 3) {
				float* p = &path->pts[i*2];
				drawCubicBez(p[0],p[1], p[2],p[3], p[4],p[5], p[6],p[7]);
			}
		}
	}
	// Delete
	nsvgDelete(image);
*)

{ NSVGpaintType }

const
  NSVG_PAINT_NONE = 0;
  NSVG_PAINT_COLOR = 1;
  NSVG_PAINT_LINEAR_GRADIENT = 2;
  NSVG_PAINT_RADIAL_GRADIENT = 3;

type
  NSVGspreadType = (
  	NSVG_SPREAD_PAD = 0,
  	NSVG_SPREAD_REFLECT = 1,
  	NSVG_SPREAD_REPEAT = 2
  );

  NSVGlineJoin = (
  	NSVG_JOIN_MITER = 0,
  	NSVG_JOIN_ROUND = 1,
  	NSVG_JOIN_BEVEL = 2
  );

  NSVGlineCap = (
  	NSVG_CAP_BUTT = 0,
  	NSVG_CAP_ROUND = 1,
  	NSVG_CAP_SQUARE = 2
  );

  NSVGfillRule = (
  	NSVG_FILLRULE_NONZERO = 0,
  	NSVG_FILLRULE_EVENODD = 1
  );

  NSVGflags = (
    NSVG_FLAGS_NONE = 0,
  	NSVG_FLAGS_VISIBLE = 1
  );

  NSVGgradientStop = record
  	color: LongWord;
  	offset: Float;
  end;

  NSVGgradient = ^NSVGgradientStruct;
  NSVGgradientStruct = record
  	xform: array[0..5] of Float;
  	spread: Byte;
  	fx, fy: Float;
  	nstops: Integer;
  	stops: array[0..1] of NSVGgradientStop;
  end;

  NSVGpaint = record
  	kind: Byte;
    case Integer of
      0: (color: LongWord);
      1: (gradient: NSVGgradient);
  end;

  NSVGpath = ^NSVGpathStruct;
  NSVGpathStruct = record
    // Cubic bezier points: x0,y0, [cpx1,cpx1,cpx2,cpy2,x1,y1], ...
  	pts: PFloat;
    // Total number of bezier points.
  	npts: Integer;
    // Flag indicating if shapes should be treated as closed.
  	closed: Byte;
    // Tight bounding box of the shape [minx,miny,maxx,maxy].
  	bounds: array[0..3] of Float;
    // Pointer to next path, or NULL if last element.
  	next: NSVGpath;
  end;

  NSVGshape = ^NSVGshapeStruct;
  NSVGshapeStruct = record
    // Optional 'id' attr of the shape or its group
  	id: array [0..63] of Char;
    // Fill paint
  	fill: NSVGpaint;
    // Stroke paint
  	stroke: NSVGpaint;
    // Opacity of the shape.
  	opacity: Float;
    // Stroke width (scaled).
  	strokeWidth: Float;
    // Stroke dash offset (scaled).
  	strokeDashOffset: Float;
    // Stroke dash array (scaled).
  	strokeDashArray: array[0..7] of Float;
    // Number of dash values in dash array.
  	strokeDashCount: Byte;
    // Stroke join type.
  	strokeLineJoin: Byte;
    // Stroke cap type.
  	strokeLineCap: Byte;
    // Miter limit
  	miterLimit: Float;
    // Fill rule, see NSVGfillRule.
  	fillRule: Byte;
    // Logical or of NSVG_FLAGS_* flags
  	flags: Byte;
    // Unused
    //b0, b1: Byte;
    // Tight bounding box of the shape [minx,miny,maxx,maxy].
  	bounds: array[0..3] of Float;
    // Linked list of paths in the image.
  	paths: NSVGpath;
    // Pointer to next shape, or NULL if last element.
  	next: NSVGshape;
  end;

  NSVGimage = ^NSVGimageStruct;
  NSVGimageStruct = record
    // Width of the image.
  	width: Float;
    // Height of the image.
  	height: Float;
    // Linked list of shapes in the image.
  	shapes: NSVGshape;
  end;

// Parses SVG file from a file, returns SVG image as paths.
function nsvgParseFromFile(filename: PChar; units: PChar; dpi: Float): NSVGimage; cdecl; libnanovg;
// Parses SVG file from a null terminated string, returns SVG image as paths.
// Important note: changes the string.
function nsvgParse(input: PChar; units: PChar; dpi: Float): NSVGimage; cdecl; libnanovg;
// Duplicates a path.
function nsvgDuplicatePath(path: NSVGpath): NSVGpath; cdecl; libnanovg;
// Deletes an image.
procedure nsvgDelete(image: NSVGimage); cdecl; libnanovg;

(* Example Usage:
	// Load SVG
	NSVGimage* image;
	image = nsvgParseFromFile("test.svg", "px", 96);

	// Create rasterizer (can be used to render multiple images).
	struct NSVGrasterizer* rast = nsvgCreateRasterizer();
	// Allocate memory for image
	unsigned char* img = malloc(w*h*4);
	// Rasterize
	nsvgRasterize(rast, image, 0,0,1, img, w, h, w*4);
*)

type
  NSVGrasterizer = ^NSVGrasterizerStruct;
  NSVGrasterizerStruct = record end;

// Allocated rasterizer context.
function nsvgCreateRasterizer: NSVGrasterizer; cdecl; libnanovg;
// Rasterizes SVG image, returns RGBA image (non-premultiplied alpha)
//   r - pointer to rasterizer context
//   image - pointer to image to rasterize
//   tx,ty - image offset (applied after scaling)
//   scale - image scale
//   dst - pointer to destination image data, 4 bytes per pixel (RGBA)
//   w - width of the image to render
//   h - height of the image to render
//   stride - number of bytes per scaleline in the destination buffer
procedure nsvgRasterize(r: NSVGrasterizer; image: NSVGimage; tx, ty, scale: Float;
  dst: Pointer; w, h, stride: Integer); cdecl; libnanovg;
// Deletes rasterizer context.
procedure nsvgDeleteRasterizer(r: NSVGrasterizer); cdecl; libnanovg;

type
  NVGLUframebuffer = ^NVGLUframebufferStruct;
  NVGLUframebufferStruct = record
    ctx: NVGcontext;
    fbo: LongWord;
    rbo: LongWord;
    texture: LongInt;
    image: Integer;
  end;

// Helper function to create GL frame buffer to render to.
procedure nvgluBindFramebuffer(fb: NVGLUframebuffer); cdecl; libnanovg;
function nvgluCreateFramebuffer(ctx: NVGcontext; w, h: LongWord; imageFlags: Integer): NVGLUframebuffer; cdecl; libnanovg;
procedure nvgluDeleteFramebuffer(fb: NVGLUframebuffer); cdecl; libnanovg;

implementation

{$ifdef darwin}
  {$ifdef gles2}
    {$linklib ../libs/libnanovg-gles2-darwin.a}
  {$else}
    {$linklib ../libs/libnanovg-gl3-darwin.a}
  {$endif}
{$endif}
{$ifdef linux}
  {$ifdef raspberry}
    {$linklib ../libs/libnanovg-gles2-raspberry.a}
  {$else}
    {$ifdef gles2}
      {$linklib ../libs/libnanovg-gles2-linux.a}
    {$else}
      {$linklib ../libs/libnanovg-gl3-linux.a}
    {$endif}
  {$endif}
{$endif}
{$ifdef windows}
  {$ifdef gles2}
    {$link ../libs/nanovg-gles2-win.o}
  {$else}
    {$link ../libs/nanovg-gl3-win.o}
  {$endif}
{$endif}

function nvgLoadProc(name: PChar; proc: Pointer): Pointer; cdecl; libnanovg;

var
  Loaded: Boolean;
  Success: Boolean;

function LoadNanoVG(GetProc: TGetProcAddress): Boolean;
const
  Procs: array of PChar = [
    'glGetError',
    'glGetUniformLocation',
    'glCreateProgram',
    'glCreateShader',
    'glGetUniformBlockIndex',
    'glActiveTexture',
    'glAttachShader',
    'glBindAttribLocation',
    'glBindBuffer',
    'glBindBufferRange',
    'glBindTexture',
    'glBindVertexArray',
    'glBlendFuncSeparate',
    'glBufferData',
    'glColorMask',
    'glCompileShader',
    'glCullFace',
    'glDeleteBuffers',
    'glDeleteProgram',
    'glDeleteShader',
    'glDeleteTextures',
    'glDeleteVertexArrays',
    'glDisable',
    'glDisableVertexAttribArray',
    'glDrawArrays',
    'glEnable',
    'glEnableVertexAttribArray',
    'glFinish',
    'glFrontFace',
    'glGenBuffers',
    'glGenTextures',
    'glGenVertexArrays',
    'glGenerateMipmap',
    'glGetIntegerv',
    'glGetProgramInfoLog',
    'glGetProgramiv',
    'glGetShaderInfoLog',
    'glGetShaderiv',
    'glLinkProgram',
    'glPixelStorei',
    'glShaderSource',
    'glStencilFunc',
    'glStencilMask',
    'glStencilOp',
    'glStencilOpSeparate',
    'glTexImage2D',
    'glTexParameteri',
    'glTexSubImage2D',
    'glUniform1f',
    'glUniform1fv',
    'glUniform1i',
    'glUniform1iv',
    'glUniform2f',
    'glUniform2fv',
    'glUniform2i',
    'glUniform2iv',
    'glUniform3f',
    'glUniform3fv',
    'glUniform3i',
    'glUniform3iv',
    'glUniform4f',
    'glUniform4fv',
    'glUniform4i',
    'glUniform4iv',
    'glUniformBlockBinding',
    'glUniformMatrix2fv',
    'glUniformMatrix3fv',
    'glUniformMatrix4fv',
    'glUseProgram',
    'glVertexAttribPointer',
    'glBindFramebuffer',
    'glBindRenderbuffer',
    'glCheckFramebufferStatus',
    'glDeleteFramebuffers',
    'glDeleteRenderbuffers',
    'glFramebufferRenderbuffer',
    'glFramebufferTexture2D',
    'glGenFramebuffers',
    'glGenRenderbuffers',
    'glRenderbufferStorage'];
var
  I: Integer;
begin
  Result := Success;
  if Loaded then
    Exit;
  Loaded := True;
  Success := True;
  for I := Low(Procs) to High(Procs) do
    Success := Success and (nvgLoadProc(Procs[I], GetProc(Procs[I])) <> nil);
  Result := Success;
end;

end.

