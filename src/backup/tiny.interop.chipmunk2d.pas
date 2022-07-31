unit Tiny.Interop.Chipmunk2D;

{$i tiny.inc}

interface

uses
  Tiny.Types;

// From chipmunk_types.h

const
  CP_INFINITY = 1e1000;
  CP_EPSILON = 1e-8;
  CP_PI = PI;
  cpTrue = 1;
  cpFalse = 0;

type
  cpHashValue = UIntPtr;
  PcpHashValue = ^cpHashValue;
  cpCollisionID = LongWord;
  PcpCollisionID = ^cpCollisionID;
  cpBool = Byte;
  PcpBool = ^cpBool;
  cpDataPointer = Pointer;
  { Type used for cpSpace.collision_type. }
  cpCollisionType = Pointer;
  { Type used for cpShape.group. }
  cpGroup = Pointer;
  { Type used for cpShapeFilter category and mask. }
  cpBitmask = LongWord;
  PcpBitmask = ^cpBitmask;
  { Type used for various timestamps. }
  cpTimestamp = LongWord;
  PcpTimestamp = ^cpTimestamp;

{ 2D vector type. }

  {TPointD = record
  public
    x, y: Double;
    class function Create: TPointD; overload; static; inline;
    class function Create(const x, y: Double): TPointD; overload; static; inline;
    class operator Negative(const a: TPointD): TPointD; inline;
    class operator Equal(const a, b: TPointD): Boolean; inline;
    class operator NotEqual(const a, b: TPointD): Boolean; inline;
    class operator Add(const a, b: TPointD): TPointD; inline;
    class operator Subtract(const a, b: TPointD): TPointD; inline;
    class operator Multiply(const a, b: TPointD): TPointD; inline;
    class operator Multiply(a: Double; const b: TPointD): TPointD; inline;
    class operator Multiply(const a: TPointD; b: Double): TPointD; inline;
    class operator Divide(a: Double; const b: TPointD): TPointD; inline;
    function Rotate(angle: Double): TPointD;
    function Distance: Double; overload;
    function Distance(const a: TPointD): Double; overload;
    function PointAtDistance(const a: TPointD; distance: Double): TPointD;
    function PointAtMix(const a: TPointD; percent: Double): TPointD;
    function Normalize: TPointD;
    function Normal(const a: TPointD; scale: Double = 1): TPointD;
    function NormalAtDistance(const a: TPointD; distance: Double; scale: Double = 1): TPointD;
    function NormalAtMix(const a: TPointD; percent: Double; scale: Double = 1): TPointD;
  end;}

const
  cpVZero: TPointD = (X: 0; Y: 0);

function cpV(x, y: Double): TPointD; inline;
function cpClamp(a, min, max: Double): Double; inline;

{ Axis-aligned 2D bounding box type (left, bottom, right, top). }

type
  PcpBB = ^cpBB;
  cpBB = record
    l, b, r, t: Double;
  end;

(*
static inline Boolean cpveql(const TPointD v1, const TPointD v2)
  {
  	return (v1.x == v2.x && v1.y == v2.y);
  }

  /// Add two vectors
  static inline TPointD cpvadd(const TPointD v1, const TPointD v2)
  {
  	return cpv(v1.x + v2.x, v1.y + v2.y);
  }

  /// Subtract two vectors.
  static inline TPointD cpvsub(const TPointD v1, const TPointD v2)
  {
  	return cpv(v1.x - v2.x, v1.y - v2.y);
  }

  /// Negate a vector.
  static inline TPointD cpvneg(const TPointD v)
  {
  	return cpv(-v.x, -v.y);
  }

  /// Scalar multiplication.
  static inline TPointD cpvmult(const TPointD v, const Double s)
  {
  	return cpv(v.x*s, v.y*s);
  }

  /// Vector dot product.
  static inline Double cpvdot(const TPointD v1, const TPointD v2)
  {
  	return v1.x*v2.x + v1.y*v2.y;
  }

  /// 2D vector cross product analog.
  /// The cross product of 2D vectors results in a 3D vector with only a z component.
  /// This function returns the magnitude of the z value.
  static inline Double cpvcross(const TPointD v1, const TPointD v2)
  {
  	return v1.x*v2.y - v1.y*v2.x;
  }

  /// Returns a perpendicular vector. (90 degree rotation)
  static inline TPointD cpvperp(const TPointD v)
  {
  	return cpv(-v.y, v.x);
  }

  /// Returns a perpendicular vector. (-90 degree rotation)
  static inline TPointD cpvrperp(const TPointD v)
  {
  	return cpv(v.y, -v.x);
  }

  /// Returns the vector projection of v1 onto v2.
  static inline TPointD cpvproject(const TPointD v1, const TPointD v2)
  {
  	return cpvmult(v2, cpvdot(v1, v2)/cpvdot(v2, v2));
  }

  /// Returns the unit length vector for the given angle (in radians).
  static inline TPointD cpvforangle(const Double a)
  {
  	return cpv(cpfcos(a), cpfsin(a));
  }

  /// Returns the angular direction v is pointing in (in radians).
  static inline Double cpvtoangle(const TPointD v)
  {
  	return cpfatan2(v.y, v.x);
  }

  /// Uses complex number multiplication to rotate v1 by v2. Scaling will occur if v1 is not a unit vector.
  static inline TPointD cpvrotate(const TPointD v1, const TPointD v2)
  {
  	return cpv(v1.x*v2.x - v1.y*v2.y, v1.x*v2.y + v1.y*v2.x);
  }

  /// Inverse of cpvrotate().
  static inline TPointD cpvunrotate(const TPointD v1, const TPointD v2)
  {
  	return cpv(v1.x*v2.x + v1.y*v2.y, v1.y*v2.x - v1.x*v2.y);
  }

  /// Returns the squared length of v. Faster than cpvlength() when you only need to compare lengths.
  static inline Double cpvlengthsq(const TPointD v)
  {
  	return cpvdot(v, v);
  }

  /// Returns the length of v.
  static inline Double cpvlength(const TPointD v)
  {
  	return cpfsqrt(cpvdot(v, v));
  }

  /// Linearly interpolate between v1 and v2.
  static inline TPointD cpvlerp(const TPointD v1, const TPointD v2, const Double t)
  {
  	return cpvadd(cpvmult(v1, 1.0f - t), cpvmult(v2, t));
  }

  /// Returns a normalized copy of v.
  static inline TPointD cpvnormalize(const TPointD v)
  {
  	// Neat trick I saw somewhere to avoid div/0.
  	return cpvmult(v, 1.0f/(cpvlength(v) + CPFLOAT_MIN));
  }

  /// Spherical linearly interpolate between v1 and v2.
  static inline TPointD
  cpvslerp(const TPointD v1, const TPointD v2, const Double t)
  {
  	Double dot = cpvdot(cpvnormalize(v1), cpvnormalize(v2));
  	Double omega = cpfacos(cpfclamp(dot, -1.0f, 1.0f));

  	if(omega < 1e-3){
  		// If the angle between two vectors is very small, lerp instead to avoid precision issues.
  		return cpvlerp(v1, v2, t);
  	} else {
  		Double denom = 1.0f/cpfsin(omega);
  		return cpvadd(cpvmult(v1, cpfsin((1.0f - t)*omega)*denom), cpvmult(v2, cpfsin(t*omega)*denom));
  	}
  }

  /// Spherical linearly interpolate between v1 towards v2 by no more than angle a radians
  static inline TPointD
  cpvslerpconst(const TPointD v1, const TPointD v2, const Double a)
  {
  	Double dot = cpvdot(cpvnormalize(v1), cpvnormalize(v2));
  	Double omega = cpfacos(cpfclamp(dot, -1.0f, 1.0f));

  	return cpvslerp(v1, v2, cpfmin(a, omega)/omega);
  }

  /// Clamp v to length len.
  static inline TPointD cpvclamp(const TPointD v, const Double len)
  {
  	return (cpvdot(v,v) > len*len) ? cpvmult(cpvnormalize(v), len): v;
  }

  /// Linearly interpolate between v1 towards v2 by distance d.
  static inline TPointD cpvlerpconst(TPointD v1, TPointD v2, Double d)
  {
  	return cpvadd(v1, cpvclamp(cpvsub(v2, v1), d));
  }

  /// Returns the distance between v1 and v2.
  static inline Double cpvdist(const TPointD v1, const TPointD v2)
  {
  	return cpvlength(cpvsub(v1, v2));
  }

  /// Returns the squared distance between v1 and v2. Faster than cpvdist() when you only need to compare distances.
  static inline Double cpvdistsq(const TPointD v1, const TPointD v2)
  {
  	return cpvlengthsq(cpvsub(v1, v2));
  }

  /// Returns true if the distance between v1 and v2 is less than dist.
  static inline cpBool cpvnear(const TPointD v1, const TPointD v2, const Double dist)
  {
  	return cpvdistsq(v1, v2) < dist*dist;
  }

  /// @}

  /// @defgroup cpMat2x2 cpMat2x2
  /// 2x2 matrix type used for tensors and such.
  /// @{

  // NUKE
  static inline cpMat2x2
  cpMat2x2New(Double a, Double b, Double c, Double d)
  {
  	cpMat2x2 m = {a, b, c, d};
  	return m;
  }

  static inline TPointD
  cpMat2x2Transform(cpMat2x2 m, TPointD v)
  {
  	return cpv(v.x*m.a + v.y*m.b, v.x*m.c + v.y*m.d);
  }



  /// Return the max of two Doubles.
  static inline Double cpfmax(Double a, Double b)
  {
  	return (a > b) ? a: b;
  }

  /// Return the min of two Doubles.
  static inline Double cpfmin(Double a, Double b)
  {
  	return (a < b) ? a: b;
  }

  /// Return the absolute value of a Double.
  static inline Double cpfabs(Double f)
  {
  	return (f < 0) ? -f: f;
  }

  /// Clamp @c f to be between @c min and @c max.
  static inline Double cpfclamp(Double f, Double min, Double max)
  {
  	return cpfmin(cpfmax(f, min), max);
  }

  /// Clamp @c f to be between 0 and 1.
  static inline Double cpfclamp01(Double f)
  {
  	return cpfmax(0.0f, cpfmin(f, 1.0f));
  }


  /// Linearly interpolate (or extrapolate) between @c f1 and @c f2 by @c t percent.
  static inline Double cpflerp(Double f1, Double f2, Double t)
  {
  	return f1*(1.0f - t) + f2*t;
  }

  /// Linearly interpolate from @c f1 to @c f2 by no more than @c d.
  static inline Double cpflerpconst(Double f1, Double f2, Double d)
  {
  	return f1 + cpfclamp(f2 - f1, -d, d);
  }

  /// Returns the closest point on the line segment ab, to the point p.
  static inline TPointD
  cpClosetPointOnSegment(const TPointD p, const TPointD a, const TPointD b)
  {
  	TPointD delta = cpvsub(a, b);
  	Double t = cpfclamp01(cpvdot(delta, cpvsub(p, b))/cpvlengthsq(delta));
  	return cpvadd(b, cpvmult(delta, t));
  }

  /// Convenience constructor for cpBB structs.
  static inline cpBB cpBBNew(const Double l, const Double b, const Double r, const Double t)
  {
  	cpBB bb = {l, b, r, t};
  	return bb;
  }

  /// Constructs a cpBB centered on a point with the given extents (half sizes).
  static inline cpBB
  cpBBNewForExtents(const TPointD c, const Double hw, const Double hh)
  {
  	return cpBBNew(c.x - hw, c.y - hh, c.x + hw, c.y + hh);
  }

  /// Constructs a cpBB for a circle with the given position and radius.
  static inline cpBB cpBBNewForCircle(const TPointD p, const Double r)
  {
  	return cpBBNewForExtents(p, r, r);
  }

  /// Returns true if @c a and @c b intersect.
  static inline cpBool cpBBIntersects(const cpBB a, const cpBB b)
  {
  	return (a.l <= b.r && b.l <= a.r && a.b <= b.t && b.b <= a.t);
  }

  /// Returns true if @c other lies completely within @c bb.
  static inline cpBool cpBBContainsBB(const cpBB bb, const cpBB other)
  {
  	return (bb.l <= other.l && bb.r >= other.r && bb.b <= other.b && bb.t >= other.t);
  }

  /// Returns true if @c bb contains @c v.
  static inline cpBool cpBBContainsVect(const cpBB bb, const TPointD v)
  {
  	return (bb.l <= v.x && bb.r >= v.x && bb.b <= v.y && bb.t >= v.y);
  }

  /// Returns a bounding box that holds both bounding boxes.
  static inline cpBB cpBBMerge(const cpBB a, const cpBB b){
  	return cpBBNew(
  		cpfmin(a.l, b.l),
  		cpfmin(a.b, b.b),
  		cpfmax(a.r, b.r),
  		cpfmax(a.t, b.t)
  	);
  }

  /// Returns a bounding box that holds both @c bb and @c v.
  static inline cpBB cpBBExpand(const cpBB bb, const TPointD v){
  	return cpBBNew(
  		cpfmin(bb.l, v.x),
  		cpfmin(bb.b, v.y),
  		cpfmax(bb.r, v.x),
  		cpfmax(bb.t, v.y)
  	);
  }

  /// Returns the center of a bounding box.
  static inline TPointD
  cpBBCenter(cpBB bb)
  {
  	return cpvlerp(cpv(bb.l, bb.b), cpv(bb.r, bb.t), 0.5f);
  }

  /// Returns the area of the bounding box.
  static inline Double cpBBArea(cpBB bb)
  {
  	return (bb.r - bb.l)*(bb.t - bb.b);
  }

  /// Merges @c a and @c b and returns the area of the merged bounding box.
  static inline Double cpBBMergedArea(cpBB a, cpBB b)
  {
  	return (cpfmax(a.r, b.r) - cpfmin(a.l, b.l))*(cpfmax(a.t, b.t) - cpfmin(a.b, b.b));
  }

  /// Returns the fraction along the segment query the cpBB is hit. Returns INFINITY if it doesn't hit.
  static inline Double cpBBSegmentQuery(cpBB bb, TPointD a, TPointD b)
  {
  	TPointD delta = cpvsub(b, a);
  	Double tmin = -INFINITY, tmax = INFINITY;

  	if(delta.x == 0.0f){
  		if(a.x < bb.l || bb.r < a.x) return INFINITY;
  	} else {
  		Double t1 = (bb.l - a.x)/delta.x;
  		Double t2 = (bb.r - a.x)/delta.x;
  		tmin = cpfmax(tmin, cpfmin(t1, t2));
  		tmax = cpfmin(tmax, cpfmax(t1, t2));
  	}

  	if(delta.y == 0.0f){
  		if(a.y < bb.b || bb.t < a.y) return INFINITY;
  	} else {
  		Double t1 = (bb.b - a.y)/delta.y;
  		Double t2 = (bb.t - a.y)/delta.y;
  		tmin = cpfmax(tmin, cpfmin(t1, t2));
  		tmax = cpfmin(tmax, cpfmax(t1, t2));
  	}

  	if(tmin <= tmax && 0.0f <= tmax && tmin <= 1.0f){
  		return cpfmax(tmin, 0.0f);
  	} else {
  		return INFINITY;
  	}
  }

  /// Return true if the bounding box intersects the line segment with ends @c a and @c b.
  static inline cpBool cpBBIntersectsSegment(cpBB bb, TPointD a, TPointD b)
  {
  	return (cpBBSegmentQuery(bb, a, b) != INFINITY);
  }

  /// Clamp a vector to a bounding box.
  static inline TPointD
  cpBBClampVect(const cpBB bb, const TPointD v)
  {
  	return cpv(cpfclamp(v.x, bb.l, bb.r), cpfclamp(v.y, bb.b, bb.t));
  }

  /// Wrap a vector to a bounding box.
  static inline TPointD
  cpBBWrapVect(const cpBB bb, const TPointD v)
  {
  	Double dx = cpfabs(bb.r - bb.l);
  	Double modx = cpfmod(v.x - bb.l, dx);
  	Double x = (modx > 0.0f) ? modx: modx + dx;

  	Double dy = cpfabs(bb.t - bb.b);
  	Double mody = cpfmod(v.y - bb.b, dy);
  	Double y = (mody > 0.0f) ? mody: mody + dy;

  	return cpv(x + bb.l, y + bb.b);
  }

  /// Returns a bounding box offseted by @c v.
  static inline cpBB
  cpBBOffset(const cpBB bb, const TPointD v)
  {
  	return cpBBNew(
  		bb.l + v.x,
  		bb.b + v.y,
  		bb.r + v.x,
  		bb.t + v.y
  	);
  }                 *)

{ Column major affine transform. }

type
  PcpTransform = ^cpTransform;
  cpTransform = record
    a, b, c, d, tx, ty: Double;
  end;

{ Nuke this matrix }

  PcpMat2x2 = ^cpMat2x2;
  cpMat2x2 = record
    a, b, c, d: Double;
  end;

// From chipmunk_structs.h

  cpArrayData = ^cpArrayDataStrct;
  cpArrayDataStrct = array[0..High(Integer)] of Pointer;

  cpArray = ^cpArrayStrct;
  cpArrayStrct = record
    num: LongInt;
    max: LongInt;
    arr: cpArrayData;
  end;

  cpContactBufferHeader = ^cpContactBufferHeaderStruct;
  cpContactBufferHeaderStruct = record end;

// From cpSpatialIndex.h

type
  { Spatial index bounding box callback function type.
   The spatial index calls this function and passes you a pointer to an object you added
   when it needs to get the bounding box associated with that object. }
  cpSpatialIndexBBFunc = function(obj: Pointer): cpBB; cdecl;
  { Spatial index/object iterator callback function type. }
  cpSpatialIndexIteratorFunc = procedure(obj: Pointer; data: Pointer); cdecl;
  { Spatial query callback function type. }
  cpSpatialIndexQueryFunc = function(obj1: Pointer; obj2: Pointer; id: cpCollisionID; data: Pointer): cpCollisionID; cdecl;
  { Spatial segment query callback function type. }
  cpSpatialIndexSegmentQueryFunc = function(obj1: Pointer; obj2: Pointer; data: Pointer): Double; cdecl;
  { In these function the index arguemtn is a cpSpatialIndex }
  cpSpatialIndexDestroyImpl = procedure(index: Pointer); cdecl;
  cpSpatialIndexCountImpl = function(index: Pointer): LongInt; cdecl;
  cpSpatialIndexEachImpl = procedure(index: Pointer; func: cpSpatialIndexIteratorFunc; data: Pointer); cdecl;
  cpSpatialIndexContainsImpl = function(index: Pointer; obj: Pointer; hashid: cpHashValue): cpBool; cdecl;
  cpSpatialIndexInsertImpl = procedure(index: Pointer; obj: Pointer; hashid: cpHashValue); cdecl;
  cpSpatialIndexRemoveImpl = procedure(index: Pointer; obj: Pointer; hashid: cpHashValue); cdecl;
  cpSpatialIndexReindexImpl = procedure(index: Pointer); cdecl;
  cpSpatialIndexReindexObjectImpl = procedure(index: Pointer; obj: Pointer; hashid: cpHashValue); cdecl;
  cpSpatialIndexReindexQueryImpl = procedure(index: Pointer; func: cpSpatialIndexQueryFunc; data: Pointer); cdecl;
  cpSpatialIndexQueryImpl = procedure(index: Pointer; obj: Pointer; bb: cpBB; func: cpSpatialIndexQueryFunc; data: Pointer); cdecl;
  cpSpatialIndexSegmentQueryImpl = procedure(index: Pointer; obj: Pointer; a: TPointD; b: TPointD; t_exit: Double;
    func: cpSpatialIndexSegmentQueryFunc; data: Pointer); cdecl;

  cpSpatialIndexClass = ^cpSpatialIndexClassStruct;
  cpSpatialIndexClassStruct = record
    destroy: cpSpatialIndexDestroyImpl;
    count: cpSpatialIndexCountImpl;
    each: cpSpatialIndexEachImpl;
    contains: cpSpatialIndexContainsImpl;
    insert: cpSpatialIndexInsertImpl;
    remove: cpSpatialIndexRemoveImpl;
    reindex: cpSpatialIndexReindexImpl;
    reindexObject: cpSpatialIndexReindexObjectImpl;
    reindexQuery: cpSpatialIndexReindexQueryImpl;
    query: cpSpatialIndexQueryImpl;
    segmentQuery: cpSpatialIndexSegmentQueryImpl;
  end;

  cpSpatialIndex = ^cpSpatialIndexStruct;
  cpSpatialIndexStruct = record
    klass: cpSpatialIndexClass;
    bbfunc: cpSpatialIndexBBFunc;
    staticIndex: cpSpatialIndex;
    dynamicIndex: cpSpatialIndex;
  end;

{ Destroy a spatial index. }
procedure cpSpatialIndexDestroy(index: cpSpatialIndex); inline;
{ Get the number of objects in the spatial index. }
function cpSpatialIndexCount(index: cpSpatialIndex): Integer; inline;
{ Iterate the objects in the spatial index. @c func will be called once for each object. }
procedure cpSpatialIndexEach(index: cpSpatialIndex; func: cpSpatialIndexIteratorFunc; data: Pointer); inline;
{ Returns true if the spatial index contains the given object.
 Most spatial indexes use hashed storage, so you must provide a hash value too. }
function cpSpatialIndexContains(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue): cpBool; inline;
{ Add an object to a spatial index.
  Most spatial indexes use hashed storage, so you must provide a hash value too.
  Remove an object from a spatial index.
  Most spatial indexes use hashed storage, so you must provide a hash value too. }
procedure cpSpatialIndexRemove(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue); inline;
{ Perform a full reindex of a spatial index. }
procedure cpSpatialIndexReindex(index: cpSpatialIndex); inline;
{ Reindex a single object in the spatial index. }
procedure cpSpatialIndexReindexObject(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue); inline;
{ Perform a rectangle query against the spatial index, calling @c func for each potential match. }
procedure cpSpatialIndexQuery(index: cpSpatialIndex; obj: Pointer; bb: cpBB; func: cpSpatialIndexQueryFunc; data: Pointer); inline;
{ Perform a segment query against the spatial index, calling @c func for each potential match. }
procedure cpSpatialIndexSegmentQuery(index: cpSpatialIndex; obj: Pointer; a, b: TPointD; t_exit: Double; func: cpSpatialIndexSegmentQueryFunc; data: Pointer); inline;
{ Simultaneously reindex and find all colliding objects.
  @c func will be called once for each potentially overlapping pair of objects found.
  If the spatial index was initialized with a static index, it will collide it's objects against that as well. }
procedure cpSpatialIndexReindexQuery(index: cpSpatialIndex; func: cpSpatialIndexQueryFunc; data: Pointer); inline;

type
  cpSpaceHash = ^cpSpaceHashStruct;
  cpSpaceHashStruct = record end;

  cpHashSet = ^cpHashSetStruct;
  cpHashSetStruct = record end;

{ Allocate a spatial hash. }
function cpSpaceHashAlloc: cpSpaceHash; cdecl; external;
{ Initialize a spatial hash.  }
function cpSpaceHashInit(hash: cpSpaceHash; celldim: Double; numcells: LongInt; bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external;
{ Allocate and initialize a spatial hash. }
function cpSpaceHashNew(celldim: Double; cells: LongInt; bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external;
{ Change the cell dimensions and table size of the spatial hash to tune it.
  The cell dimensions should roughly match the average size of your objects
  and the table size should be ~10 larger than the number of objects inserted.
  Some trial and error is required to find the optimum numbers for efficiency. }
procedure cpSpaceHashResize(hash: cpSpaceHash; celldim: Double; numcells: LongInt); cdecl; external;

type
  cpBBTree = ^cpBBTreeStruct;
  cpBBTreeStruct = record end;

{ Allocate a bounding box tree. }
function cpBBTreeAlloc: cpBBTree; cdecl; external;
{ Initialize a bounding box tree. }
function cpBBTreeInit(tree: cpBBTree; bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external;
{ Allocate and initialize a bounding box tree. }
function cpBBTreeNew(bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external;
{ Perform a static top down optimization of the tree. }
procedure cpBBTreeOptimize(index: cpSpatialIndex); cdecl; external;

type
  { Bounding box tree velocity callback function. }
  { This function should return an estimate for the object's velocity. }
  cpBBTreeVelocityFunc = function(obj: Pointer): TPointD; cdecl;

  { Set the velocity function for the bounding box tree to enable temporal coherence. }
  procedure cpBBTreeSetVelocityFunc(index: cpSpatialIndex; func: cpBBTreeVelocityFunc); cdecl; external;

type
  cpSweep1D = ^cpSweep1DStruct;
  cpSweep1DStruct = record end;

{ Allocate a 1D sort and sweep broadphase. }
function cpSweep1DAlloc: cpSweep1D; cdecl; external;
{ Initialize a 1D sort and sweep broadphase. }
function cpSweep1DInit(sweep: cpSweep1D; bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external;
{ Allocate and initialize a 1D sort and sweep broadphase. }
function cpSweep1DNew(bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external;
{ Destroy and free a spatial index. }
procedure cpSpatialIndexFree(index: cpSpatialIndex); cdecl; external;
{ Collide the objects in @c dynamicIndex against the objects in @c staticIndex using the query callback function. }
procedure cpSpatialIndexCollideStatic(dynamicIndex: cpSpatialIndex; staticIndex: cpSpatialIndex; func: cpSpatialIndexQueryFunc; data: Pointer); cdecl; external;

type
  cpSpace = ^cpSpaceStruct;
  cpSpaceStruct = record
    iterations: LongInt;
    gravity: TPointD;
    damping: Double;
    idleSpeedThreshold: Double;
    sleepTimeThreshold: Double;
    collisionSlop: Double;
    collisionBias: Double;
    collisionPersistence: cpTimestamp;
    userData: cpDataPointer;
    stamp: cpTimestamp;
    curr_dt: Double;
    dynamicBodies: cpArray;
    staticBodies: cpArray;
    rousedBodies: cpArray;
    sleepingComponents: cpArray;
    shapeIDCounter: cpHashValue;
    staticShapes: cpSpatialIndex;
    dynamicShapes: cpSpatialIndex;
    constraints: cpArray;
    arbiters: cpArray;
    contactBuffersHead: cpContactBufferHeader;
    cachedArbiters: cpHashSet;
    pooledArbiters: cpArray;
    allocatedBuffers: cpArray;
    locked: LongWord;
    usesWildcards: cpBool;
    collisionHandlers: cpHashSet;
    defaultHandler: Pointer {cpCollisionHandlerStruct};
    skipPostStep: cpBool;
    postStepCallbacks: cpArray;
    staticBody: Pointer {cpBody};
    _staticBody: Pointer {cpBodyStruct};
  end;

  { Rigid body velocity update function type. Param body is of type PcpBody }
  cpBodyVelocityFunc = procedure(body: Pointer; gravity: TPointD; damping: Double; dt: Double); cdecl;
  { Rigid body position update function type. Param body is of type PcpBody }
  cpBodyPositionFunc = procedure(body: Pointer; dt: Double); cdecl;

{ Integration functions
  mass and it's inverse
  moment of inertia and it's inverse
  center of gravity
  position, velocity, force
  Angle, angular velocity, torque (radians)
  "pseudo-velocities" used for eliminating overlap.
  Erin Catto has some papers that talk about what these are. }

  cpBody = ^cpBodyStruct;
  cpBodyStruct = record
    velocity_func: cpBodyVelocityFunc;
    position_func: cpBodyPositionFunc;
    m: Double;
    m_inv: Double;
    i: Double;
    i_inv: Double;
    cog: TPointD;
    p: TPointD;
    v: TPointD;
    f: TPointD;
    a: Double;
    w: Double;
    t: Double;
    transform: cpTransform;
    userData: cpDataPointer;
    v_bias: TPointD;
    w_bias: Double;
    space: cpSpace;
    shapeList: Pointer {cpShape};
    arbiterList: Pointer {cpArbiter};
    constraintList: Pointer {cpConstraint};
    sleeping: record
      root: cpBody;
      next: cpBody;
      idleTime: Double;
    end;
  end;

{ Arbiter is active and its the first collision.
  Arbiter is active and its not the first collision.
  Collision has been explicitly ignored.
  Either by returning false from a begin collision handler or calling cpArbiterIgnore().
  Collison is no longer active. A space will cache an arbiter for up to cpSpace.collisionPersistence more steps.
  Collison arbiter is invalid because one of the shapes was removed. }

  cpArbiterState =  LongInt;

const
  CP_ARBITER_STATE_FIRST_COLLISION = 0;
  CP_ARBITER_STATE_NORMAL = 1;
  CP_ARBITER_STATE_IGNORE = 2;
  CP_ARBITER_STATE_CACHED = 3;
  CP_ARBITER_STATE_INVALIDATED = 4;

type
  cpContact = ^cpContactStruct;
  cpContactStruct = record
    r1: TPointD;
    r2: TPointD;
    nMass: Double;
    tMass: Double;
    bounce: Double;
    jnAcc: Double;
    jtAcc: Double;
    jBias: Double;
    bias: Double;
    hash: cpHashValue;
  end;

  cpCollisionInfo = ^cpCollisionInfoStruct;
  cpCollisionInfoStruct = record
    a: Pointer {cpShape};
    b: Pointer {cpShape};
    id: cpCollisionID;
    n: TPointD;
    count: LongInt;
    arr: cpContact;
  end;

  cpArbiterThread = ^cpArbiterThreadStruct;
  cpArbiterThreadStruct = record
    next: Pointer {cpArbiter};
    prev: Pointer {cpArbiter};
  end;

  cpArbiter = ^cpArbiterStruct;
  cpArbiterStruct = record
    e: Double;
    u: Double;
    surface_vr: TPointD;
    data: cpDataPointer;
    a: Pointer {cpShape};
    b: Pointer {cpShape};
    body_a: cpBody;
    body_b: cpBody;
    thread_a: cpArbiterThreadStruct;
    thread_b: cpArbiterThreadStruct;
    count: LongInt;
    contacts: cpContact;
    n: TPointD;
    handler: Pointer {cpCollisionHandler};
    handlerA: Pointer {cpCollisionHandler};
    handlerB: Pointer {cpCollisionHandler};
    swapped: cpBool;
    stamp: cpTimestamp;
    state: cpArbiterState;
  end;

  cpShapeFilter = ^cpShapeFilterStruct;
  cpShapeFilterStruct = record
    group: cpGroup;
    categories: cpBitmask;
    mask: cpBitmask;
  end;

  cpShapeMassInfo = ^cpShapeMassInfoStruct;
  cpShapeMassInfoStruct = record
    m: Double;
    i: Double;
    cog: TPointD;
    area: Double;
  end;

  cpShapeType =  LongInt;

const
  CP_CIRCLE_SHAPE = 0;
  CP_SEGMENT_SHAPE = 1;
  CP_POLY_SHAPE = 2;
  CP_NUM_SHAPES = 3;

type
  cpShape = ^cpShapeStruct;
  cpShapeStruct = record
    klass: Pointer {cpShapeClass};
    space: cpSpace;
    body: cpBody;
    massInfo: cpShapeMassInfoStruct;
    bb: cpBB;
    sensor: cpBool;
    e: Double;
    u: Double;
    surfaceV: TPointD;
    userData: cpDataPointer;
    _type: cpCollisionType;
    filter: cpShapeFilterStruct;
    next: cpShape;
    prev: cpShape;
    hashid: cpHashValue;
  end;

  cpShapeCacheDataImpl = function(shape: cpShape; transform: cpTransform): cpBB; cdecl;
  cpShapeDestroyImpl = procedure(shape: cpShape); cdecl;
  cpShapePointQueryImpl = procedure(shape: cpShape; p: TPointD; info: Pointer {cpPointQueryInfo}); cdecl;
  cpShapeSegmentQueryImpl = procedure(shape: cpShape; a: TPointD; b: TPointD; radius: Double; info: Pointer {cpSegmentQueryInfo}); cdecl;

  cpShapeClass = ^cpShapeClassStruct;
  cpShapeClassStruct = record
    _type: cpShapeType;
    cacheData: cpShapeCacheDataImpl;
    destroy: cpShapeDestroyImpl;
    pointQuery: cpShapePointQueryImpl;
    segmentQuery: cpShapeSegmentQueryImpl;
  end;

  cpCircleShape = ^cpCircleShapeStruct;
  cpCircleShapeStruct = record
    shape: cpShapeStruct;
    c: TPointD;
    tc: TPointD;
    r: Double;
  end;

  cpSegmentShape = ^cpSegmentShapeStruct;
  cpSegmentShapeStruct = record
    shape: cpShapeStruct;
    a: TPointD;
    b: TPointD;
    n: TPointD;
    ta: TPointD;
    tb: TPointD;
    tn: TPointD;
    r: Double;
    a_tangent: TPointD;
    b_tangent: TPointD;
  end;

  cpSplittingPlane = ^cpSplittingPlaneStruct;
  cpSplittingPlaneStruct = record
    v0: TPointD;
    n: TPointD;
  end;

const
  CP_POLY_SHAPE_INLINE_ALLOC = 6;

{ The untransformed planes are appended at the end of the transformed planes.
  Allocate a small number of splitting planes internally for simple poly. }

type
  cpPolyShape = ^cpPolyShapeStruct;
  cpPolyShapeStruct = record
    shape: cpShapeStruct;
    r: Double;
    count: LongInt;
    planes: cpSplittingPlane;
    _planes: array[0..(2 * CP_POLY_SHAPE_INLINE_ALLOC) - 1] of cpSplittingPlaneStruct;
  end;

type
  cpJointType =  LongWord;

const
	CP_JOINT_PIN = 0;
	CP_JOINT_SLIDE = 1;
	CP_JOINT_PIVOT = 2;
	CP_JOINT_GROOVE = 3;
	CP_JOINT_DAMPED_SPRING = 4;
	CP_JOINT_DAMPED_ROTARY_SPRING = 5;
	CP_JOINT_ROTARY_LIMIT = 6;
	CP_JOINT_RATCHET = 7;
	CP_JOINT_GEAR = 8;
	CP_JOINT_SIMPLE_MOTOR = 9;

type
  cpConstraintPreStepImpl = procedure(constraint: Pointer {cpConstraint}; dt: Double); cdecl;
  cpConstraintApplyCachedImpulseImpl = procedure(constraint: Pointer {cpConstraint}; dt_coef: Double); cdecl;
  cpConstraintApplyImpulseImpl = procedure(constraint: Pointer {cpConstraint}; dt: Double); cdecl;
  cpConstraintGetImpulseImpl = function(constraint: Pointer {cpConstraint}): Double; cdecl;

  cpConstraintClass = ^cpConstraintClassStruct;
  cpConstraintClassStruct = record
    preStep: cpConstraintPreStepImpl;
    applyCachedImpulse: cpConstraintApplyCachedImpulseImpl;
    applyImpulse: cpConstraintApplyImpulseImpl;
    getImpulse: cpConstraintGetImpulseImpl;
  end;

  cpConstraint = ^cpConstraintStruct;
  cpConstraintStruct = record
    klass: cpConstraintClass;
    space: cpSpace;
    a: cpBody;
    b: cpBody;
    next_a: cpConstraint;
    next_b: cpConstraint;
    _type: cpJointType;
    maxForce: Double;
    errorBias: Double;
    maxBias: Double;
    collideBodies: cpBool;
    preSolve: Pointer {cpConstraintPreSolveFunc};
    postSolve: Pointer {cpConstraintPostSolveFunc};
    userData: cpDataPointer;
  end;

  cpPinJoint = ^cpPinJointStruct;
  cpPinJointStruct = record
    constraint: cpConstraintStruct;
    anchorA: TPointD;
    anchorB: TPointD;
    dist: Double;
    r1: TPointD;
    r2: TPointD;
    n: TPointD;
    nMass: Double;
    jnAcc: Double;
    bias: Double;
  end;

  cpSlideJoint = ^cpSlideJointStruct;
  cpSlideJointStruct = record
    constraint: cpConstraintStruct;
    anchorA: TPointD;
    anchorB: TPointD;
    min: Double;
    max: Double;
    r1: TPointD;
    r2: TPointD;
    n: TPointD;
    nMass: Double;
    jnAcc: Double;
    bias: Double;
  end;

  cpPivotJoint = ^cpPivotJointStruct;
  cpPivotJointStruct = record
    constraint: cpConstraintStruct;
    anchorA: TPointD;
    anchorB: TPointD;
    r1: TPointD;
    r2: TPointD;
    k: cpMat2x2;
    jAcc: TPointD;
    bias: TPointD;
  end;

  cpGrooveJoint = ^cpGrooveJointStruct;
  cpGrooveJointStruct = record
    constraint: cpConstraintStruct;
    grv_n: TPointD;
    grv_a: TPointD;
    grv_b: TPointD;
    anchorB: TPointD;
    grv_tn: TPointD;
    clamp: Double;
    r1: TPointD;
    r2: TPointD;
    k: cpMat2x2;
    jAcc: TPointD;
    bias: TPointD;
  end;

  cpDampedSpring = ^cpDampedSpringStruct;
  cpDampedSpringStruct = record
    constraint: cpConstraintStruct;
    anchorA: TPointD;
    anchorB: TPointD;
    restLength: Double;
    stiffness: Double;
    damping: Double;
    springForceFunc: Pointer {cpDampedSpringForceFunc};
    target_vrn: Double;
    v_coef: Double;
    r1: TPointD;
    r2: TPointD;
    nMass: Double;
    n: TPointD;
    jAcc: Double;
  end;

  cpDampedRotarySpring = ^cpDampedRotarySpringStruct;
  cpDampedRotarySpringStruct = record
    constraint: cpConstraintStruct;
    restAngle: Double;
    stiffness: Double;
    damping: Double;
    springTorqueFunc: Pointer {cpDampedRotarySpringTorqueFunc};
    target_wrn: Double;
    w_coef: Double;
    iSum: Double;
    jAcc: Double;
  end;

  cpRotaryLimitJoint = ^cpRotaryLimitJointStruct;
  cpRotaryLimitJointStruct = record
    constraint: cpConstraintStruct;
    min: Double;
    max: Double;
    iSum: Double;
    bias: Double;
    jAcc: Double;
  end;

  cpRatchetJoint = ^cpRatchetJointStruct;
  cpRatchetJointStruct = record
    constraint: cpConstraintStruct;
    angle: Double;
    phase: Double;
    ratchet: Double;
    iSum: Double;
    bias: Double;
    jAcc: Double;
  end;

  cpGearJoint = ^cpGearJointStruct;
  cpGearJointStruct = record
    constraint: cpConstraintStruct;
    phase: Double;
    ratio: Double;
    ratio_inv: Double;
    iSum: Double;
    bias: Double;
    jAcc: Double;
  end;

  cpSimpleMotor = ^cpSimpleMotorStruct;
  cpSimpleMotorStruct = record
    constraint: cpConstraintStruct;
    rate: Double;
    iSum: Double;
    jAcc: Double;
  end;

  cpSpaceArbiterApplyImpulseFunc = procedure(arb: cpArbiter); cdecl;

  cpPostStepCallback = ^cpPostStepCallbackStruct;
  cpPostStepCallbackStruct = record
    func: Pointer {cpPostStepFunc};
    key: Pointer;
    data: Pointer;
  end;

// From chipmunk.h

{ Calculate the moment of inertia for a circle.
  @c r1 and @c r2 are the inner and outer diameters. A solid circle has an inner diameter of 0. }
function cpMomentForCircle(m: Double; r1: Double; r2: Double; offset: TPointD): Double; cdecl; external;
{ Calculate area of a hollow circle.
  @c r1 and @c r2 are the inner and outer diameters. A solid circle has an inner diameter of 0. }
function cpAreaForCircle(r1: Double; r2: Double): Double; cdecl; external;
{ Calculate the moment of inertia for a line segment.
  Beveling radius is not supported. }
function cpMomentForSegment(m: Double; a: TPointD; b: TPointD; radius: Double): Double; cdecl; external;
{ Calculate the area of a fattened (capsule shaped) line segment. }
function cpAreaForSegment(a: TPointD; b: TPointD; radius: Double): Double; cdecl; external;
{ Calculate the moment of inertia for a solid polygon shape assuming it's center of gravity is at it's centroid. The offset is added to each vertex. }
function cpMomentForPoly(m: Double; count: LongInt; verts: PPointD; offset: TPointD; radius: Double): Double; cdecl; external;
{ Calculate the signed area of a polygon. A Clockwise winding gives positive area.
  This is probably backwards from what you expect, but matches Chipmunk's the winding for poly shapes. }
function cpAreaForPoly(count: LongInt; verts: PPointD; radius: Double): Double; cdecl; external;
{ Calculate the natural centroid of a polygon. }
function cpCentroidForPoly(count: LongInt; verts: PPointD): TPointD; cdecl; external;
{ Calculate the moment of inertia for a solid box. }
function cpMomentForBox(m: Double; width: Double; height: Double): Double; cdecl; external;
{ Calculate the moment of inertia for a solid box. }
function cpMomentForBox2(m: Double; box: cpBB): Double; cdecl; external;
{ Calculate the convex hull of a given set of points. Returns the count of points in the hull.
  @c result must be a pointer to a @c TPointD array with at least @c count elements. If @c verts == @c result, then @c verts will be reduced inplace.
  @c first is an optional pointer to an integer to store where the first vertex in the hull came from (i.e. verts[first] == result[0])
  @c tol is the allowed amount to shrink the hull when simplifying it. A tolerance of 0.0 creates an exact hull. }
function cpConvexHull(count: LongInt; verts: PPointD; result: PPointD; first: PLongInt; tol: Double): LongInt; cdecl; external;

// From chipmunk_unsafe.h

{ Set the radius of a circle shape. }
procedure cpCircleShapeSetRadius(shape: cpShape; radius: Double); cdecl; external;
{ Set the offset of a circle shape. }
procedure cpCircleShapeSetOffset(shape: cpShape; offset: TPointD); cdecl; external;
{ Set the endpoints of a segment shape. }
procedure cpSegmentShapeSetEndpoints(shape: cpShape; a: TPointD; b: TPointD); cdecl; external;
{ Set the radius of a segment shape. }
procedure cpSegmentShapeSetRadius(shape: cpShape; radius: Double); cdecl; external;
{ Set the vertexes of a poly shape. }
procedure cpPolyShapeSetVerts(shape: cpShape; count: LongInt; verts: PPointD; transform: cpTransform); cdecl; external;
procedure cpPolyShapeSetVertsRaw(shape: cpShape; count: LongInt; verts: PPointD); cdecl; external;
{ Set the radius of a poly shape. }
procedure cpPolyShapeSetRadius(shape: cpShape; radius: Double); cdecl; external;

// From cpArbiter.h

{ The cpArbiter struct tracks pairs of colliding shapes.
  They are also used in conjuction with collision handler callbacks
  allowing you to retrieve information on the collision or change it.
  A unique arbiter value is used for each pair of colliding objects. It persists until the shapes separate. }

const
  CP_MAX_CONTACTS_PER_ARBITER = 2;

{ Get the restitution (elasticity) that will be applied to the pair of colliding objects. }
function cpArbiterGetRestitution(arb: cpArbiter): Double; cdecl; external;
{ Override the restitution (elasticity) that will be applied to the pair of colliding objects. }
procedure cpArbiterSetRestitution(arb: cpArbiter; restitution: Double); cdecl; external;
{ Get the friction coefficient that will be applied to the pair of colliding objects. }
function cpArbiterGetFriction(arb: cpArbiter): Double; cdecl; external;
{ Override the friction coefficient that will be applied to the pair of colliding objects. }
procedure cpArbiterSetFriction(arb: cpArbiter; friction: Double); cdecl; external;
{ Get the relative surface velocity of the two shapes in contact. }
function cpArbiterGetSurfaceVelocity(arb: cpArbiter): TPointD; cdecl; external;
{ Override the relative surface velocity of the two shapes in contact.
  By default this is calculated to be the difference of the two surface velocities clamped to the tangent plane. }
procedure cpArbiterSetSurfaceVelocity(arb: cpArbiter; vr: TPointD); cdecl; external;
{ Get the user data pointer associated with this pair of colliding objects. }
function cpArbiterGetUserData(arb: cpArbiter): cpDataPointer; cdecl; external;
{ Set a user data point associated with this pair of colliding objects.
  If you need to perform any cleanup for this pointer, you must do it yourself, in the separate callback for instance. }
procedure cpArbiterSetUserData(arb: cpArbiter; userData: cpDataPointer); cdecl; external;
{ Calculate the total impulse including the friction that was applied by this arbiter.
 This function should only be called from a post-solve, post-step or cpBodyEachArbiter callback. }
function cpArbiterTotalImpulse(arb: cpArbiter): TPointD; cdecl; external;
{ Calculate the amount of energy lost in a collision including static, but not dynamic friction.
  This function should only be called from a post-solve, post-step or cpBodyEachArbiter callback. }
function cpArbiterTotalKE(arb: cpArbiter): Double; cdecl; external;
{ Mark a collision pair to be ignored until the two objects separate.
  Pre-solve and post-solve callbacks will not be called, but the separate callback will be called. }
function cpArbiterIgnore(arb: cpArbiter): cpBool; cdecl; external;
{ Return the colliding shapes involved for this arbiter.
  The order of their cpSpace.collision_type values will match
  the order set when the collision handler was registered. }
procedure cpArbiterGetShapes(arb: cpArbiter; out a, b: cpShape); cdecl; external;
{ Return the colliding bodies involved for this arbiter.
  The order of the cpSpace.collision_type the bodies are associated with values will match
  the order set when the collision handler was registered. }
procedure cpArbiterGetBodies(arb: cpArbiter; out a, b: cpBody); cdecl; external;

{ A struct that wraps up the important collision data for an arbiter.
  The number of contact points in the set.
  The normal of the collision.
  The array of contact points.
  The position of the contact on the surface of each shape.
  Penetration distance of the two shapes. Overlapping means it will be negative.
  This value is calculated as cpvdot(cpvsub(point2, point1), normal) and is
  ignored by cpArbiterSetContactPointSet(). }

type
  cpContactPointSet = ^cpContactPointSetStruct;
  cpContactPointSetStruct = record
      count: LongInt;
      normal: TPointD;
      points: array[0..(CP_MAX_CONTACTS_PER_ARBITER) - 1] of record
        pointA: TPointD;
        pointB: TPointD;
        distance: Double;
    end;
  end;

{ Return a contact set from an arbiter. }
function cpArbiterGetContactPointSet(arb: cpArbiter): cpContactPointSetStruct; cdecl; external;
{ Replace the contact point set for an arbiter.
  This can be a very powerful feature, but use it with caution! }
procedure cpArbiterSetContactPointSet(arb: cpArbiter; pointSet: cpContactPointSet); cdecl; external;
{ Returns true if this is the first step a pair of objects started colliding. }
function cpArbiterIsFirstContact(arb: cpArbiter): cpBool; cdecl; external;
{ Returns true if the separate callback is due to a shape being removed from the space. }
function cpArbiterIsRemoval(arb: cpArbiter): cpBool; cdecl; external;
{ Get the number of contact points for this arbiter. }
function cpArbiterGetCount(arb: cpArbiter): LongInt; cdecl; external;
{ Get the normal of the collision. }
function cpArbiterGetNormal(arb: cpArbiter): TPointD; cdecl; external;
{ Get the position of the @c ith contact point on the surface of the first shape. }
function cpArbiterGetPointA(arb: cpArbiter; i: LongInt): TPointD; cdecl; external;
{ Get the position of the @c ith contact point on the surface of the second shape. }
function cpArbiterGetPointB(arb: cpArbiter; i: LongInt): TPointD; cdecl; external;
{ Get the depth of the @c ith contact point. }
function cpArbiterGetDepth(arb: cpArbiter; i: LongInt): Double; cdecl; external;
{ If you want a custom callback to invoke the wildcard callback for the first collision type, you must call this function explicitly.
  You must decide how to handle the wildcard's return value since it may disagree with the other wildcard handler's return value or your own. }
function cpArbiterCallWildcardBeginA(arb: cpArbiter; space: cpSpace): cpBool; cdecl; external;
{ If you want a custom callback to invoke the wildcard callback for the second collision type, you must call this function explicitly.
  You must decide how to handle the wildcard's return value since it may disagree with the other wildcard handler's return value or your own. }
function cpArbiterCallWildcardBeginB(arb: cpArbiter; space: cpSpace): cpBool; cdecl; external;
{ If you want a custom callback to invoke the wildcard callback for the first collision type, you must call this function explicitly.
  You must decide how to handle the wildcard's return value since it may disagree with the other wildcard handler's return value or your own. }
function cpArbiterCallWildcardPreSolveA(arb: cpArbiter; space: cpSpace): cpBool; cdecl; external;
{ If you want a custom callback to invoke the wildcard callback for the second collision type, you must call this function explicitly.
  You must decide how to handle the wildcard's return value since it may disagree with the other wildcard handler's return value or your own. }
function cpArbiterCallWildcardPreSolveB(arb: cpArbiter; space: cpSpace): cpBool; cdecl; external;
{ If you want a custom callback to invoke the wildcard callback for the first collision type, you must call this function explicitly. }
procedure cpArbiterCallWildcardPostSolveA(arb: cpArbiter; space: cpSpace); cdecl; external;
{ If you want a custom callback to invoke the wildcard callback for the second collision type, you must call this function explicitly. }
procedure cpArbiterCallWildcardPostSolveB(arb: cpArbiter; space: cpSpace); cdecl; external;
{ If you want a custom callback to invoke the wildcard callback for the first collision type, you must call this function explicitly. }
procedure cpArbiterCallWildcardSeparateA(arb: cpArbiter; space: cpSpace); cdecl; external;
{ If you want a custom callback to invoke the wildcard callback for the second collision type, you must call this function explicitly. }
procedure cpArbiterCallWildcardSeparateB(arb: cpArbiter; space: cpSpace); cdecl; external;

// From cpBody.h

{ Chipmunk's rigid body type. Rigid bodies hold the physical properties of an object like
 it's mass, and position and velocity of it's center of gravity. They don't have an shape on their own.
 They are given a shape by creating collision shapes (cpShape) that point to the body.
 A dynamic body is one that is affected by gravity, forces, and collisions.
 This is the default body type.
 A kinematic body is an infinite mass, user controlled body that is not affected by gravity, forces or collisions.
 Instead the body only moves based on it's velocity.
 Dynamic bodies collide normally with kinematic bodies, though the kinematic body will be unaffected.
 Collisions between two kinematic bodies, or a kinematic body and a static body produce collision callbacks, but no collision response.
 A static body is a body that never (or rarely) moves. If you move a static body, you must call one of the cpSpaceReindex*() functions.
 Chipmunk uses this information to optimize the collision detection.
 Static bodies do not produce collision callbacks when colliding with other static bodies. }

type
  PcpBodyType = ^cpBodyType;
  cpBodyType =  LongInt;

const
  CP_BODY_TYPE_DYNAMIC = 0;
  CP_BODY_TYPE_KINEMATIC = 1;
  CP_BODY_TYPE_STATIC = 2;

{ Allocate a cpBody. }
function cpBodyAlloc: cpBody; cdecl; external;
{ Initialize a cpBody. }
function cpBodyInit(body: cpBody; mass: Double; moment: Double): cpBody; cdecl; external;
{ Allocate and initialize a cpBody. }
function cpBodyNew(mass: Double; moment: Double): cpBody; cdecl; external;
{ Allocate and initialize a cpBody, and set it as a kinematic body. }
function cpBodyNewKinematic: cpBody; cdecl; external;
{ Allocate and initialize a cpBody, and set it as a static body. }
function cpBodyNewStatic: cpBody; cdecl; external;
{ Destroy a cpBody. }
procedure cpBodyDestroy(body: cpBody); cdecl; external;
{ Destroy and free a cpBody. }
procedure cpBodyFree(body: cpBody); cdecl; external;
{ Wake up a sleeping or idle body. }
procedure cpBodyActivate(body: cpBody); cdecl; external;
{ Wake up any sleeping or idle bodies touching a static body. }
procedure cpBodyActivateStatic(body: cpBody; filter: cpShape); cdecl; external;
{ Force a body to fall asleep immediately. }
procedure cpBodySleep(body: cpBody); cdecl; external;
{ Force a body to fall asleep immediately along with other bodies in a group. }
procedure cpBodySleepWithGroup(body: cpBody; group: cpBody); cdecl; external;
{ Returns true if the body is sleeping. }
function cpBodyIsSleeping(body: cpBody): cpBool; cdecl; external;
{ Get the type of the body. }
function cpBodyGetType(body: cpBody): cpBodyType; cdecl; external;
{ Set the type of the body. }
procedure cpBodySetType(body: cpBody; _type: cpBodyType); cdecl; external;
{ Get the space this body is added to. }
function cpBodyGetSpace(body: cpBody): cpSpace; cdecl; external;
{ Get the mass of the body. }
function cpBodyGetMass(body: cpBody): Double; cdecl; external;
{ Set the mass of the body. }
procedure cpBodySetMass(body: cpBody; m: Double); cdecl; external;
{ Get the moment of inertia of the body. }
function cpBodyGetMoment(body: cpBody): Double; cdecl; external;
{ Set the moment of inertia of the body. }
procedure cpBodySetMoment(body: cpBody; i: Double); cdecl; external;
{ Set the position of a body. }
function cpBodyGetPosition(body: cpBody): TPointD; cdecl; external;
{ Set the position of the body. }
procedure cpBodySetPosition(body: cpBody; pos: TPointD); cdecl; external;
{ Get the offset of the center of gravity in body local coordinates. }
function cpBodyGetCenterOfGravity(body: cpBody): TPointD; cdecl; external;
{ Set the offset of the center of gravity in body local coordinates. }
procedure cpBodySetCenterOfGravity(body: cpBody; cog: TPointD); cdecl; external;
{ Get the velocity of the body. }
function cpBodyGetVelocity(body: cpBody): TPointD; cdecl; external;
{ Set the velocity of the body. }
procedure cpBodySetVelocity(body: cpBody; velocity: TPointD); cdecl; external;
{ Get the force applied to the body for the next time step. }
function cpBodyGetForce(body: cpBody): TPointD; cdecl; external;
{ Set the force applied to the body for the next time step. }
procedure cpBodySetForce(body: cpBody; force: TPointD); cdecl; external;
{ Get the angle of the body. }
function cpBodyGetAngle(body: cpBody): Double; cdecl; external;
{ Set the angle of a body. }
procedure cpBodySetAngle(body: cpBody; a: Double); cdecl; external;
{ Get the angular velocity of the body. }
function cpBodyGetAngularVelocity(body: cpBody): Double; cdecl; external;
{ Set the angular velocity of the body. }
procedure cpBodySetAngularVelocity(body: cpBody; angularVelocity: Double); cdecl; external;
{ Get the torque applied to the body for the next time step. }
function cpBodyGetTorque(body: cpBody): Double; cdecl; external;
{ Set the torque applied to the body for the next time step. }
procedure cpBodySetTorque(body: cpBody; torque: Double); cdecl; external;
{ Get the rotation vector of the body. (The x basis vector of it's transform.) }
function cpBodyGetRotation(body: cpBody): TPointD; cdecl; external;
{ Get the user data pointer assigned to the body. }
function cpBodyGetUserData(body: cpBody): cpDataPointer; cdecl; external;
{ Set the user data pointer assigned to the body. }
procedure cpBodySetUserData(body: cpBody; userData: cpDataPointer); cdecl; external;
{ Set the callback used to update a body's velocity. }
procedure cpBodySetVelocityUpdateFunc(body: cpBody; velocityFunc: cpBodyVelocityFunc); cdecl; external;
{ Set the callback used to update a body's position. }
{ NOTE: It's not generally recommended to override this unless you call the default position update function. }
procedure cpBodySetPositionUpdateFunc(body: cpBody; positionFunc: cpBodyPositionFunc); cdecl; external;
{ Default velocity integration function. }
procedure cpBodyUpdateVelocity(body: cpBody; gravity: TPointD; damping: Double; dt: Double); cdecl; external;
{ Default position integration function. }
procedure cpBodyUpdatePosition(body: cpBody; dt: Double); cdecl; external;
{ Convert body relative/local coordinates to absolute/world coordinates. }
function cpBodyLocalToWorld(body: cpBody; point: TPointD): TPointD; cdecl; external;
{ Convert body absolute/world coordinates to  relative/local coordinates. }
function cpBodyWorldToLocal(body: cpBody; point: TPointD): TPointD; cdecl; external;
{ Apply a force to a body. Both the force and point are expressed in world coordinates. }
procedure cpBodyApplyForceAtWorldPoint(body: cpBody; force: TPointD; point: TPointD); cdecl; external;
{ Apply a force to a body. Both the force and point are expressed in body local coordinates. }
procedure cpBodyApplyForceAtLocalPoint(body: cpBody; force: TPointD; point: TPointD); cdecl; external;
{ Apply an impulse to a body. Both the impulse and point are expressed in world coordinates. }
procedure cpBodyApplyImpulseAtWorldPoint(body: cpBody; impulse: TPointD; point: TPointD); cdecl; external;
{ Apply an impulse to a body. Both the impulse and point are expressed in body local coordinates. }
procedure cpBodyApplyImpulseAtLocalPoint(body: cpBody; impulse: TPointD; point: TPointD); cdecl; external;
{ Get the velocity on a body (in world units) at a point on the body in world coordinates. }
function cpBodyGetVelocityAtWorldPoint(body: cpBody; point: TPointD): TPointD; cdecl; external;
{ Get the velocity on a body (in world units) at a point on the body in local coordinates. }
function cpBodyGetVelocityAtLocalPoint(body: cpBody; point: TPointD): TPointD; cdecl; external;
{ Get the amount of kinetic energy contained by the body. }
function cpBodyKineticEnergy(body: cpBody): Double; cdecl; external;

type
  { Body/shape iterator callback function type. }
  cpBodyShapeIteratorFunc = procedure(body: cpBody; shape: cpShape; data: Pointer); cdecl;
  { Body/constraint iterator callback function type. }
  cpBodyConstraintIteratorFunc = procedure(body: cpBody; constraint: cpConstraint; data: Pointer); cdecl;
  { Body/arbiter iterator callback function type. }
  cpBodyArbiterIteratorFunc = procedure(body: cpBody; arbiter: cpArbiter; data: Pointer); cdecl;

{ Call @c func once for each shape attached to @c body and added to the space. }
procedure cpBodyEachShape(body: cpBody; func: cpBodyShapeIteratorFunc; data: Pointer); cdecl; external;
{ Call @c func once for each constraint attached to @c body and added to the space. }
procedure cpBodyEachConstraint(body: cpBody; func: cpBodyConstraintIteratorFunc; data: Pointer); cdecl; external;
{ Call @c func once for each arbiter that is currently active on the body. }
procedure cpBodyEachArbiter(body: cpBody; func: cpBodyArbiterIteratorFunc; data: Pointer); cdecl; external;

// From cpBody.h

type
  { Callback function type that gets called before solving a joint. }
  cpConstraintPreSolveFunc = procedure(constraint: cpConstraint; space: cpSpace); cdecl;
  { Callback function type that gets called after solving a joint. }
  cpConstraintPostSolveFunc = procedure(constraint: cpConstraint; space: cpSpace); cdecl;

{ Destroy a constraint. }
procedure cpConstraintDestroy(constraint: cpConstraint); cdecl; external;
{ Destroy and free a constraint. }
procedure cpConstraintFree(constraint: cpConstraint); cdecl; external;
{ Get the cpSpace this constraint is added to. }
function cpConstraintGetSpace(constraint: cpConstraint): cpSpace; cdecl; external;
{ Get the first body the constraint is attached to. }
function cpConstraintGetBodyA(constraint: cpConstraint): cpBody; cdecl; external;
{ Get the second body the constraint is attached to. }
function cpConstraintGetBodyB(constraint: cpConstraint): cpBody; cdecl; external;
{ Get the joint type associated with the constraint. }
function cpConstraintGetJointType(constraint: cpConstraint): cpJointType; cdecl; external;
{ Get the maximum force that this constraint is allowed to use. }
function cpConstraintGetMaxForce(constraint: cpConstraint): Double; cdecl; external;
{ Set the maximum force that this constraint is allowed to use. (defaults to INFINITY) }
procedure cpConstraintSetMaxForce(constraint: cpConstraint; maxForce: Double); cdecl; external;
{ Get rate at which joint error is corrected. }
function cpConstraintGetErrorBias(constraint: cpConstraint): Double; cdecl; external;
{ Set rate at which joint error is corrected.
  Defaults to pow(1.0 - 0.1, 60.0) meaning that it will
  correct 10% of the error every 1/60th of a second. }
procedure cpConstraintSetErrorBias(constraint: cpConstraint; errorBias: Double); cdecl; external;
{ Get the maximum rate at which joint error is corrected. }
function cpConstraintGetMaxBias(constraint: cpConstraint): Double; cdecl; external;
{ Set the maximum rate at which joint error is corrected. (defaults to INFINITY) }
procedure cpConstraintSetMaxBias(constraint: cpConstraint; maxBias: Double); cdecl; external;
{ Get if the two bodies connected by the constraint are allowed to collide or not. }
function cpConstraintGetCollideBodies(constraint: cpConstraint): cpBool; cdecl; external;
{ Set if the two bodies connected by the constraint are allowed to collide or not. (defaults to cpFalse) }
procedure cpConstraintSetCollideBodies(constraint: cpConstraint; collideBodies: cpBool); cdecl; external;
{ Get the pre-solve function that is called before the solver runs. }
function cpConstraintGetPreSolveFunc(constraint: cpConstraint): cpConstraintPreSolveFunc; cdecl; external;
{ Set the pre-solve function that is called before the solver runs. }
procedure cpConstraintSetPreSolveFunc(constraint: cpConstraint; preSolveFunc: cpConstraintPreSolveFunc); cdecl; external;
{ Get the post-solve function that is called before the solver runs. }
function cpConstraintGetPostSolveFunc(constraint: cpConstraint): cpConstraintPostSolveFunc; cdecl; external;
{ Set the post-solve function that is called before the solver runs. }
procedure cpConstraintSetPostSolveFunc(constraint: cpConstraint; postSolveFunc: cpConstraintPostSolveFunc); cdecl; external;
{ Get the user definable data pointer for this constraint }
function cpConstraintGetUserData(constraint: cpConstraint): cpDataPointer; cdecl; external;
{ Set the user definable data pointer for this constraint }
procedure cpConstraintSetUserData(constraint: cpConstraint; userData: cpDataPointer); cdecl; external;
{ Get the last impulse applied by this constraint. }
function cpConstraintGetImpulse(constraint: cpConstraint): Double; cdecl; external;

// From cpDampedRotarySpring.h

{ Check if a constraint is a damped rotary spring. }
function cpConstraintIsDampedRotarySpring(constraint: cpConstraint): cpBool; cdecl; external;

type
  { Function type used for damped rotary spring force callbacks. }
  cpDampedRotarySpringTorqueFunc = function(spring: cpConstraint; relativeAngle: Double): Double; cdecl;

{ Allocate a damped rotary spring. }
function cpDampedRotarySpringAlloc: cpDampedRotarySpring; cdecl; external;
{ Initialize a damped rotary spring. }
function cpDampedRotarySpringInit(joint: cpDampedRotarySpring; a: cpBody; b: cpBody; restAngle: Double; stiffness: Double;
  damping: Double): cpDampedRotarySpring; cdecl; external;
{ Allocate and initialize a damped rotary spring. }
function cpDampedRotarySpringNew(a: cpBody; b: cpBody; restAngle: Double; stiffness: Double; damping: Double): cpConstraint; cdecl; external;
{ Get the rest length of the spring. }
function cpDampedRotarySpringGetRestAngle(constraint: cpConstraint): Double; cdecl; external;
{ Set the rest length of the spring. }
procedure cpDampedRotarySpringSetRestAngle(constraint: cpConstraint; restAngle: Double); cdecl; external;
{ Get the stiffness of the spring in force/distance. }
function cpDampedRotarySpringGetStiffness(constraint: cpConstraint): Double; cdecl; external;
{ Set the stiffness of the spring in force/distance. }
procedure cpDampedRotarySpringSetStiffness(constraint: cpConstraint; stiffness: Double); cdecl; external;
{ Get the damping of the spring. }
function cpDampedRotarySpringGetDamping(constraint: cpConstraint): Double; cdecl; external;
{ Set the damping of the spring. }
procedure cpDampedRotarySpringSetDamping(constraint: cpConstraint; damping: Double); cdecl; external;
{ Get the damping of the spring. }
function cpDampedRotarySpringGetSpringTorqueFunc(constraint: cpConstraint): cpDampedRotarySpringTorqueFunc; cdecl; external;
{ Set the damping of the spring. }
procedure cpDampedRotarySpringSetSpringTorqueFunc(constraint: cpConstraint; springTorqueFunc: cpDampedRotarySpringTorqueFunc); cdecl; external;

// From cpDampedSpring.h

{ Check if a constraint is a damped spring. }
function cpConstraintIsDampedSpring(constraint: cpConstraint): cpBool; cdecl; external;

type
  { Function type used for damped spring force callbacks. }
  cpDampedSpringForceFunc = function(spring: cpConstraint; dist: Double): Double; cdecl;

{ Allocate a damped spring. }
function cpDampedSpringAlloc: cpDampedSpring; cdecl; external;
{ Initialize a damped spring. }
function cpDampedSpringInit(joint: cpDampedSpring; a: cpBody; b: cpBody; anchorA: TPointD; anchorB: TPointD;
  restLength: Double; stiffness: Double; damping: Double): cpDampedSpring; cdecl; external;
{ Allocate and initialize a damped spring. }
function cpDampedSpringNew(a: cpBody; b: cpBody; anchorA: TPointD; anchorB: TPointD; restLength: Double;
  stiffness: Double; damping: Double): cpConstraint; cdecl; external;
{ Get the location of the first anchor relative to the first body. }
function cpDampedSpringGetAnchorA(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the location of the first anchor relative to the first body. }
procedure cpDampedSpringSetAnchorA(constraint: cpConstraint; anchorA: TPointD); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpDampedSpringGetAnchorB(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpDampedSpringSetAnchorB(constraint: cpConstraint; anchorB: TPointD); cdecl; external;
{ Get the rest length of the spring. }
function cpDampedSpringGetRestLength(constraint: cpConstraint): Double; cdecl; external;
{ Set the rest length of the spring. }
procedure cpDampedSpringSetRestLength(constraint: cpConstraint; restLength: Double); cdecl; external;
{ Get the stiffness of the spring in force/distance. }
function cpDampedSpringGetStiffness(constraint: cpConstraint): Double; cdecl; external;
{ Set the stiffness of the spring in force/distance. }
procedure cpDampedSpringSetStiffness(constraint: cpConstraint; stiffness: Double); cdecl; external;
{ Get the damping of the spring. }
function cpDampedSpringGetDamping(constraint: cpConstraint): Double; cdecl; external;
{ Set the damping of the spring. }
procedure cpDampedSpringSetDamping(constraint: cpConstraint; damping: Double); cdecl; external;
{ Get the damping of the spring. }
function cpDampedSpringGetSpringForceFunc(constraint: cpConstraint): cpDampedSpringForceFunc; cdecl; external;
{ Set the damping of the spring. }
procedure cpDampedSpringSetSpringForceFunc(constraint: cpConstraint; springForceFunc: cpDampedSpringForceFunc); cdecl; external;

// From cpGearJoint.h

{ Check if a constraint is a gear join. }
function cpConstraintIsGearJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a gear joint. }
function cpGearJointAlloc: cpGearJoint; cdecl; external;
{ Initialize a gear joint. }
function cpGearJointInit(joint: cpGearJoint; a: cpBody; b: cpBody; phase: Double; ratio: Double): cpGearJoint; cdecl; external;
{ Allocate and initialize a gear joint. }
function cpGearJointNew(a: cpBody; b: cpBody; phase: Double; ratio: Double): cpConstraint; cdecl; external;
{ Get the phase offset of the gears. }
function cpGearJointGetPhase(constraint: cpConstraint): Double; cdecl; external;
{ Set the phase offset of the gears. }
procedure cpGearJointSetPhase(constraint: cpConstraint; phase: Double); cdecl; external;
{ Get the angular distance of each ratchet. }
function cpGearJointGetRatio(constraint: cpConstraint): Double; cdecl; external;
{ Set the ratio of a gear joint. }
procedure cpGearJointSetRatio(constraint: cpConstraint; ratio: Double); cdecl; external;

// From cpGrooveJoint.h

{ Check if a constraint is a groove joint. }
function cpConstraintIsGrooveJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a groove joint. }
function cpGrooveJointAlloc: cpGrooveJoint; cdecl; external;
{ Initialize a groove joint. }
function cpGrooveJointInit(joint: cpGrooveJoint; a: cpBody; b: cpBody; groove_a: TPointD; groove_b: TPointD;
  anchorB: TPointD): cpGrooveJoint; cdecl; external;
{ Allocate and initialize a groove joint. }
function cpGrooveJointNew(a: cpBody; b: cpBody; groove_a: TPointD; groove_b: TPointD; anchorB: TPointD): cpConstraint; cdecl; external;
{ Get the first endpoint of the groove relative to the first body. }
function cpGrooveJointGetGrooveA(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the first endpoint of the groove relative to the first body. }
procedure cpGrooveJointSetGrooveA(constraint: cpConstraint; grooveA: TPointD); cdecl; external;
{ Get the first endpoint of the groove relative to the first body. }
function cpGrooveJointGetGrooveB(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the first endpoint of the groove relative to the first body. }
procedure cpGrooveJointSetGrooveB(constraint: cpConstraint; grooveB: TPointD); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpGrooveJointGetAnchorB(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpGrooveJointSetAnchorB(constraint: cpConstraint; anchorB: TPointD); cdecl; external;

// From cpHastySpace.h

type
  cpHastySpace = ^cpHastySpaceStruct;
  cpHastySpaceStruct = record end;

{ Create a new hasty space.
  On ARM platforms that support NEON, this will enable the vectorized solver.
  cpHastySpace also supports multiple threads, but runs single threaded by default for determinism. }

function cpHastySpaceNew: cpSpace; cdecl; external;
procedure cpHastySpaceFree(space: cpSpace); cdecl; external;

{ Set the number of threads to use for the solver.
  Currently Chipmunk is limited to 2 threads as using more generally provides very minimal performance gains.
  Passing 0 as the thread count on iOS or OS X will cause Chipmunk to automatically detect the number of threads it should use.
  On other platforms passing 0 for the thread count will set 1 thread. }
procedure cpHastySpaceSetThreads(space: cpSpace; threads: LongWord); cdecl; external;
{ Returns the number of threads the solver is using to run. }
function cpHastySpaceGetThreads(space: cpSpace): LongWord; cdecl; external;
{ When stepping a hasty space, you must use this function. }
procedure cpHastySpaceStep(space: cpSpace; dt: Double); cdecl; external;

// From cpMarch.h

type
  { Function type used as a callback from the marching squares algorithm to sample an image function.
   It passes you the point to sample and your context pointer, and you return the density. }
  cpMarchSampleFunc = function(point: TPointD; data: Pointer): Double; cdecl;
  { Function type used as a callback from the marching squares algorithm to output a line segment.
   It passes you the two endpoints and your context pointer. }
  cpMarchSegmentFunc = procedure(v0: TPointD; v1: TPointD; data: Pointer); cdecl;

{ Trace an anti-aliased contour of an image along a particular threshold.
  The given number of samples will be taken and spread across the bounding box area using the sampling function and context.
  The segment function will be called for each segment detected that lies along the density contour for @c threshold. }
procedure cpMarchSoft(bb: cpBB; x_samples: LongWord; y_samples: LongWord; threshold: Double; segment: cpMarchSegmentFunc;
  segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer); cdecl; external;
{ Trace an aliased curve of an image along a particular threshold.
  The given number of samples will be taken and spread across the bounding box area using the sampling function and context.
  The segment function will be called for each segment detected that lies along the density contour for @c threshold. }
procedure cpMarchHard(bb: cpBB; x_samples: LongWord; y_samples: LongWord; threshold: Double; segment: cpMarchSegmentFunc;
  segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer); cdecl; external;

// From cpPinJoint.h

{ Check if a constraint is a pin joint. }
function cpConstraintIsPinJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a pin joint. }
function cpPinJointAlloc: cpPinJoint; cdecl; external;
{ Initialize a pin joint. }
function cpPinJointInit(joint: cpPinJoint; a: cpBody; b: cpBody; anchorA: TPointD; anchorB: TPointD): cpPinJoint; cdecl; external;
{ Allocate and initialize a pin joint. }
function cpPinJointNew(a: cpBody; b: cpBody; anchorA: TPointD; anchorB: TPointD): cpConstraint; cdecl; external;
{ Get the location of the first anchor relative to the first body. }
function cpPinJointGetAnchorA(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the location of the first anchor relative to the first body. }
procedure cpPinJointSetAnchorA(constraint: cpConstraint; anchorA: TPointD); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpPinJointGetAnchorB(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpPinJointSetAnchorB(constraint: cpConstraint; anchorB: TPointD); cdecl; external;
{ Get the distance the joint will maintain between the two anchors. }
function cpPinJointGetDist(constraint: cpConstraint): Double; cdecl; external;
{ Set the distance the joint will maintain between the two anchors. }
procedure cpPinJointSetDist(constraint: cpConstraint; dist: Double); cdecl; external;

// From cpPivotJoint.h

{ Check if a constraint is a pivot joint. }
function cpConstraintIsPivotJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a pivot joint }
function cpPivotJointAlloc: cpPivotJoint; cdecl; external;
{ Initialize a pivot joint. }
function cpPivotJointInit(joint: cpPivotJoint; a: cpBody; b: cpBody; anchorA: TPointD; anchorB: TPointD): cpPivotJoint; cdecl; external;
{ Allocate and initialize a pivot joint. }
function cpPivotJointNew(a: cpBody; b: cpBody; pivot: TPointD): cpConstraint; cdecl; external;
{ Allocate and initialize a pivot joint with specific anchors. }
function cpPivotJointNew2(a: cpBody; b: cpBody; anchorA: TPointD; anchorB: TPointD): cpConstraint; cdecl; external;
{ Get the location of the first anchor relative to the first body. }
function cpPivotJointGetAnchorA(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the location of the first anchor relative to the first body. }
procedure cpPivotJointSetAnchorA(constraint: cpConstraint; anchorA: TPointD); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpPivotJointGetAnchorB(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpPivotJointSetAnchorB(constraint: cpConstraint; anchorB: TPointD); cdecl; external;

// From cpPolyline.h

{ Polylines are just arrays of vertexes.
  They are looped if the first vertex is equal to the last.
 cpPolyline structs are intended to be passed by value and destroyed when you are done with them. }

type
  cpPolyline = ^cpPolylineStruct;
  cpPolylineStruct = record
    count: LongInt;
    capacity: LongInt;
    verts: PPointD;
  end;

{ Destroy and free a polyline instance. }
procedure cpPolylineFree(line: cpPolyline); cdecl; external;
{ Returns true if the first vertex is equal to the last. }
function cpPolylineIsClosed(line: cpPolyline): cpBool; cdecl; external;
{ Returns a copy of a polyline simplified by using the Douglas-Peucker algorithm.
  This works very well on smooth or gently curved shapes, but not well on straight edged or angular shapes. }
function cpPolylineSimplifyCurves(line: cpPolyline; tol: Double): cpPolyline; cdecl; external;
{ Returns a copy of a polyline simplified by discarding "flat" vertexes.
  This works well on straight edged or angular shapes, not as well on smooth shapes.}
function cpPolylineSimplifyVertexes(line: cpPolyline; tol: Double): cpPolyline; cdecl; external;
{ Get the convex hull of a polyline as a looped polyline. }
function cpPolylineToConvexHull(line: cpPolyline; tol: Double): cpPolyline; cdecl; external;
{ Polyline sets are collections of polylines, generally built by cpMarchSoft() or cpMarchHard(). }

type
  cpPolylineSet = ^cpPolylineSetStruct;
  cpPolylineSetStruct = record
    count: LongInt;
    capacity: LongInt;
    lines: ^cpPolyline;
  end;

{ Allocate a new polyline set. }
function cpPolylineSetAlloc: cpPolylineSet; cdecl; external;
{ Initialize a new polyline set. }
function cpPolylineSetInit(polySet: cpPolylineSet): cpPolylineSet; cdecl; external;
{ Allocate and initialize a polyline set. }
function cpPolylineSetNew: cpPolylineSet; cdecl; external;
{ Destroy a polyline set. }
procedure cpPolylineSetDestroy(polySet: cpPolylineSet; freePolylines: cpBool); cdecl; external;
{ Destroy and free a polyline set. }
procedure cpPolylineSetFree(polySet: cpPolylineSet; freePolylines: cpBool); cdecl; external;
{ Add a line segment to a polyline set.
  A segment will either start a new polyline, join two others, or add to or loop an existing polyline.
  This is mostly intended to be used as a callback directly from cpMarchSoft() or cpMarchHard(). }
procedure cpPolylineSetCollectSegment(v0: TPointD; v1: TPointD; lines: cpPolylineSet); cdecl; external;
{ Get an approximate convex decomposition from a polyline.
  Returns a cpPolylineSet of convex hulls that match the original shape to within 'tol'.
  NOTE: If the input is a self intersecting polygon, the output might end up overly simplified. }
function cpPolylineConvexDecomposition(line: cpPolyline; tol: Double): cpPolylineSet; cdecl; external;

// From cpPolyShape.h

{ Allocate a polygon shape. }
function cpPolyShapeAlloc: cpPolyShape; cdecl; external;
{ Initialize a polygon shape with rounded corners.
  A convex hull will be created from the vertexes. }
function cpPolyShapeInit(poly: cpPolyShape; body: cpBody; count: LongInt; verts: PPointD; transform: cpTransform;
  radius: Double): cpPolyShape; cdecl; external;
{ Initialize a polygon shape with rounded corners.
 The vertexes must be convex with a counter-clockwise winding. }
function cpPolyShapeInitRaw(poly: cpPolyShape; body: cpBody; count: LongInt; verts: PPointD; radius: Double): cpPolyShape; cdecl; external;
{ Allocate and initialize a polygon shape with rounded corners.
  A convex hull will be created from the vertexes. }
function cpPolyShapeNew(body: cpBody; count: LongInt; verts: PPointD; transform: cpTransform; radius: Double): cpShape; cdecl; external;
{ Allocate and initialize a polygon shape with rounded corners.
  The vertexes must be convex with a counter-clockwise winding. }
function cpPolyShapeNewRaw(body: cpBody; count: LongInt; verts: PPointD; radius: Double): cpShape; cdecl; external;
{ Initialize a box shaped polygon shape with rounded corners. }
function cpBoxShapeInit(poly: cpPolyShape; body: cpBody; width: Double; height: Double; radius: Double): cpPolyShape; cdecl; external;
{ Initialize an offset box shaped polygon shape with rounded corners. }
function cpBoxShapeInit2(poly: cpPolyShape; body: cpBody; box: cpBB; radius: Double): cpPolyShape; cdecl; external;
{ Allocate and initialize a box shaped polygon shape. }
function cpBoxShapeNew(body: cpBody; width: Double; height: Double; radius: Double): cpShape; cdecl; external;
{ Allocate and initialize an offset box shaped polygon shape. }
function cpBoxShapeNew2(body: cpBody; box: cpBB; radius: Double): cpShape; cdecl; external;
{ Get the number of verts in a polygon shape. }
function cpPolyShapeGetCount(shape: cpShape): LongInt; cdecl; external;
{ Get the @c ith vertex of a polygon shape. }
function cpPolyShapeGetVert(shape: cpShape; index: LongInt): TPointD; cdecl; external;
{ Get the radius of a polygon shape. }
function cpPolyShapeGetRadius(shape: cpShape): Double; cdecl; external;

// From cpRatchetJoint.h

{ Check if a constraint is a ratchet joint. }
function cpConstraintIsRatchetJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a ratchet joint. }
function cpRatchetJointAlloc: cpRatchetJoint; cdecl; external;
{ Initialize a ratched joint. }
function cpRatchetJointInit(joint: cpRatchetJoint; a: cpBody; b: cpBody; phase: Double; ratchet: Double): cpRatchetJoint; cdecl; external;
{ Allocate and initialize a ratchet joint. }
function cpRatchetJointNew(a: cpBody; b: cpBody; phase: Double; ratchet: Double): cpConstraint; cdecl; external;
{ Get the angle of the current ratchet tooth. }
function cpRatchetJointGetAngle(constraint: cpConstraint): Double; cdecl; external;
{ Set the angle of the current ratchet tooth. }
procedure cpRatchetJointSetAngle(constraint: cpConstraint; angle: Double); cdecl; external;
{ Get the phase offset of the ratchet. }
function cpRatchetJointGetPhase(constraint: cpConstraint): Double; cdecl; external;
{ Get the phase offset of the ratchet. }
procedure cpRatchetJointSetPhase(constraint: cpConstraint; phase: Double); cdecl; external;
{ Get the angular distance of each ratchet. }
function cpRatchetJointGetRatchet(constraint: cpConstraint): Double; cdecl; external;
{ Set the angular distance of each ratchet. }
procedure cpRatchetJointSetRatchet(constraint: cpConstraint; ratchet: Double); cdecl; external;

// From cpRotaryLimitJoint.h

{ Check if a constraint is a rotary limit joint. }
function cpConstraintIsRotaryLimitJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a damped rotary limit joint. }
function cpRotaryLimitJointAlloc: cpRotaryLimitJoint; cdecl; external;
{ Initialize a damped rotary limit joint. }
function cpRotaryLimitJointInit(joint: cpRotaryLimitJoint; a: cpBody; b: cpBody; min: Double; max: Double): cpRotaryLimitJoint; cdecl; external;
{ Allocate and initialize a damped rotary limit joint. }
function cpRotaryLimitJointNew(a: cpBody; b: cpBody; min: Double; max: Double): cpConstraint; cdecl; external;
{ Get the minimum distance the joint will maintain between the two anchors. }
function cpRotaryLimitJointGetMin(constraint: cpConstraint): Double; cdecl; external;
{ Set the minimum distance the joint will maintain between the two anchors. }
procedure cpRotaryLimitJointSetMin(constraint: cpConstraint; min: Double); cdecl; external;
{ Get the maximum distance the joint will maintain between the two anchors. }
function cpRotaryLimitJointGetMax(constraint: cpConstraint): Double; cdecl; external;
{ Set the maximum distance the joint will maintain between the two anchors. }
procedure cpRotaryLimitJointSetMax(constraint: cpConstraint; max: Double); cdecl; external;

// From cpShape.h

{ Point query info struct.
  The nearest shape, NULL if no shape was within range.
  The closest point on the shape's surface. (in world space coordinates)
  The distance to the point. The distance is negative if the point is inside the shape.
  The gradient of the signed distance function.
  The value should be similar to info.p/info.d, but accurate even for very small values of info.d. }

type
  cpPointQueryInfo = ^cpPointQueryInfoStruct;
  cpPointQueryInfoStruct = record
    shape: cpShape;
    point: TPointD;
    distance: Double;
    gradient: TPointD;
  end;

{ Segment query info struct.
  The shape that was hit, or NULL if no collision occured.
  The point of impact.
  The normal of the surface hit.
  The normalized distance along the query segment in the range [0, 1]. }

  cpSegmentQueryInfo = ^cpSegmentQueryInfoStruct;
  cpSegmentQueryInfoStruct = record
    shape: cpShape;
    point: TPointD;
    normal: TPointD;
    alpha: Double;
  end;

{ Fast collision filtering type that is used to determine if two objects collide before calling collision or query callbacks.
  Two objects with the same non-zero group value do not collide.
  This is generally used to group objects in a composite object together to disable self collisions.
  A bitmask of user definable categories that this object belongs to.
  The category/mask combinations of both objects in a collision must agree for a collision to occur.
  A bitmask of user definable category types that this object object collides with.
  The category/mask combinations of both objects in a collision must agree for a collision to occur. }

{ Destroy a shape. }
procedure cpShapeDestroy(shape: cpShape); cdecl; external;
{ Destroy and Free a shape. }
procedure cpShapeFree(shape: cpShape); cdecl; external;
{ Update, cache and return the bounding box of a shape based on the body it's attached to. }
function cpShapeCacheBB(shape: cpShape): cpBB; cdecl; external;
{ Update, cache and return the bounding box of a shape with an explicit transformation. }
function cpShapeUpdate(shape: cpShape; transform: cpTransform): cpBB; cdecl; external;
{ Perform a nearest point query. It finds the closest point on the surface of shape to a specific point.
  The value returned is the distance between the points. A negative distance means the point is inside the shape. }
function cpShapePointQuery(shape: cpShape; p: TPointD; out info: cpPointQueryInfoStruct): Double; cdecl; external;
{ Perform a segment query against a shape. @c info must be a pointer to a valid cpSegmentQueryInfo structure. }
function cpShapeSegmentQuery(shape: cpShape; a, b: TPointD; radius: Double; out info: cpSegmentQueryInfoStruct): cpBool; cdecl; external;
{ Return contact information about two shapes. }
function cpShapesCollide(a, b: cpShape): cpContactPointSetStruct; cdecl; external;
{ The cpSpace this body is added to. }
function cpShapeGetSpace(shape: cpShape): cpSpace; cdecl; external;
{ The cpBody this shape is connected to. }
function cpShapeGetBody(shape: cpShape): cpBody; cdecl; external;
{ Set the cpBody this shape is connected to.
  Can only be used if the shape is not currently added to a space. }
procedure cpShapeSetBody(shape: cpShape; body: cpBody); cdecl; external;
{ Get the mass of the shape if you are having Chipmunk calculate mass properties for you. }
function cpShapeGetMass(shape: cpShape): Double; cdecl; external;
{ Set the mass of this shape to have Chipmunk calculate mass properties for you. }
procedure cpShapeSetMass(shape: cpShape; mass: Double); cdecl; external;
{ Get the density of the shape if you are having Chipmunk calculate mass properties for you. }
function cpShapeGetDensity(shape: cpShape): Double; cdecl; external;
{ Set the density  of this shape to have Chipmunk calculate mass properties for you. }
procedure cpShapeSetDensity(shape: cpShape; density: Double); cdecl; external;
{ Get the calculated moment of inertia for this shape. }
function cpShapeGetMoment(shape: cpShape): Double; cdecl; external;
{ Get the calculated area of this shape. }
function cpShapeGetArea(shape: cpShape): Double; cdecl; external;
{ Get the centroid of this shape. }
function cpShapeGetCenterOfGravity(shape: cpShape): TPointD; cdecl; external;
{ Get the bounding box that contains the shape given it's current position and angle. }
function cpShapeGetBB(shape: cpShape): cpBB; cdecl; external;
{ Get if the shape is set to be a sensor or not. }
function cpShapeGetSensor(shape: cpShape): cpBool; cdecl; external;
{ Set if the shape is a sensor or not. }
procedure cpShapeSetSensor(shape: cpShape; sensor: cpBool); cdecl; external;
{ Get the elasticity of this shape. }
function cpShapeGetElasticity(shape: cpShape): Double; cdecl; external;
{ Set the elasticity of this shape. }
procedure cpShapeSetElasticity(shape: cpShape; elasticity: Double); cdecl; external;
{ Get the friction of this shape. }
function cpShapeGetFriction(shape: cpShape): Double; cdecl; external;
{ Set the friction of this shape. }
procedure cpShapeSetFriction(shape: cpShape; friction: Double); cdecl; external;
{ Get the surface velocity of this shape. }
function cpShapeGetSurfaceVelocity(shape: cpShape): TPointD; cdecl; external;
{ Set the surface velocity of this shape. }
procedure cpShapeSetSurfaceVelocity(shape: cpShape; surfaceVelocity: TPointD); cdecl; external;
{ Get the user definable data pointer of this shape. }
function cpShapeGetUserData(shape: cpShape): cpDataPointer; cdecl; external;
{ Set the user definable data pointer of this shape. }
procedure cpShapeSetUserData(shape: cpShape; userData: cpDataPointer); cdecl; external;
{ Set the collision type of this shape. }
function cpShapeGetCollisionType(shape: cpShape): cpCollisionType; cdecl; external;
{ Get the collision type of this shape. }
procedure cpShapeSetCollisionType(shape: cpShape; collisionType: cpCollisionType); cdecl; external;
{ Get the collision filtering parameters of this shape. }
function cpShapeGetFilter(shape: cpShape): cpShapeFilterStruct; cdecl; external;
{ Set the collision filtering parameters of this shape. }
procedure cpShapeSetFilter(shape: cpShape; filter: cpShapeFilterStruct); cdecl; external;

{ Allocate a circle shape. }
function cpCircleShapeAlloc: cpCircleShape; cdecl; external;
{ Initialize a circle shape. }
function cpCircleShapeInit(circle: cpCircleShape; body: cpBody; radius: Double; offset: TPointD): cpCircleShape; cdecl; external;
{ Allocate and initialize a circle shape. }
function cpCircleShapeNew(body: cpBody; radius: Double; offset: TPointD): cpShape; cdecl; external;
{ Get the offset of a circle shape. }
function cpCircleShapeGetOffset(shape: cpShape): TPointD; cdecl; external;
{ Get the radius of a circle shape. }
function cpCircleShapeGetRadius(shape: cpShape): Double; cdecl; external;

{ Allocate a segment shape. }
function cpSegmentShapeAlloc: cpSegmentShape; cdecl; external;
{ Initialize a segment shape. }
function cpSegmentShapeInit(seg: cpSegmentShape; body: cpBody; a: TPointD; b: TPointD; radius: Double): cpSegmentShape; cdecl; external;
{ Allocate and initialize a segment shape. }
function cpSegmentShapeNew(body: cpBody; a: TPointD; b: TPointD; radius: Double): cpShape; cdecl; external;
{ Let Chipmunk know about the geometry of adjacent segments to avoid colliding with endcaps. }
procedure cpSegmentShapeSetNeighbors(shape: cpSegmentShape; prev: TPointD; next: TPointD); cdecl; external;
{ Get the first endpoint of a segment shape. }
function cpSegmentShapeGetA(shape: cpShape): TPointD; cdecl; external;
{ Get the second endpoint of a segment shape. }
function cpSegmentShapeGetB(shape: cpShape): TPointD; cdecl; external;
{ Get the normal of a segment shape. }
function cpSegmentShapeGetNormal(shape: cpShape): TPointD; cdecl; external;
{ Get the first endpoint of a segment shape. }
function cpSegmentShapeGetRadius(shape: cpShape): Double; cdecl; external;

// From cpSimpleMotor.h

{ Check if a constraint is a simple motor. }
function cpConstraintIsSimpleMotor(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a simple motor. }
function cpSimpleMotorAlloc: cpSimpleMotor; cdecl; external;
{ initialize a simple motor. }
function cpSimpleMotorInit(joint: cpSimpleMotor; a: cpBody; b: cpBody; rate: Double): cpSimpleMotor; cdecl; external;
{ Allocate and initialize a simple motor. }
function cpSimpleMotorNew(a: cpBody; b: cpBody; rate: Double): cpConstraint; cdecl; external;
{ Get the rate of the motor. }
function cpSimpleMotorGetRate(constraint: cpConstraint): Double; cdecl; external;
{ Set the rate of the motor. }
procedure cpSimpleMotorSetRate(constraint: cpConstraint; rate: Double); cdecl; external;

// From cpSlideJoint.h

{ Check if a constraint is a slide joint. }
function cpConstraintIsSlideJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a slide joint. }
function cpSlideJointAlloc: cpSlideJoint; cdecl; external;
{ Initialize a slide joint. }
function cpSlideJointInit(joint: cpSlideJoint; a: cpBody; b: cpBody; anchorA: TPointD; anchorB: TPointD;
  min: Double; max: Double): cpSlideJoint; cdecl; external;
{ Allocate and initialize a slide joint. }
function cpSlideJointNew(a: cpBody; b: cpBody; anchorA: TPointD; anchorB: TPointD; min: Double;
  max: Double): cpConstraint; cdecl; external;
{ Get the location of the first anchor relative to the first body. }
function cpSlideJointGetAnchorA(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the location of the first anchor relative to the first body. }
procedure cpSlideJointSetAnchorA(constraint: cpConstraint; anchorA: TPointD); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpSlideJointGetAnchorB(constraint: cpConstraint): TPointD; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpSlideJointSetAnchorB(constraint: cpConstraint; anchorB: TPointD); cdecl; external;
{ Get the minimum distance the joint will maintain between the two anchors. }
function cpSlideJointGetMin(constraint: cpConstraint): Double; cdecl; external;
{ Set the minimum distance the joint will maintain between the two anchors. }
procedure cpSlideJointSetMin(constraint: cpConstraint; min: Double); cdecl; external;
{ Get the maximum distance the joint will maintain between the two anchors. }
function cpSlideJointGetMax(constraint: cpConstraint): Double; cdecl; external;
{ Set the maximum distance the joint will maintain between the two anchors. }
procedure cpSlideJointSetMax(constraint: cpConstraint; max: Double); cdecl; external;

// From cpSpace.h

{ Collision begin event function callback type.
  Returning false from a begin callback causes the collision to be ignored until
  the separate callback is called when the objects stop colliding. }

type
  cpCollisionBeginFunc = function(arb: cpArbiter; space: cpSpace; userData: cpDataPointer): cpBool; cdecl;
{ Collision pre-solve event function callback type. }
{ Returning false from a pre-step callback causes the collision to be ignored until the next step. }
  cpCollisionPreSolveFunc = function(arb: cpArbiter; space: cpSpace; userData: cpDataPointer): cpBool; cdecl;
{ Collision post-solve event function callback type. }
  cpCollisionPostSolveFunc = procedure(arb: cpArbiter; space: cpSpace; userData: cpDataPointer); cdecl;
{ Collision separate event function callback type. }
  cpCollisionSeparateFunc = procedure(arb: cpArbiter; space: cpSpace; userData: cpDataPointer); cdecl;

{ Struct that holds function callback pointers to configure custom collision handling.
  Collision handlers have a pair of types; when a collision occurs between two shapes that have these types, the collision handler functions are triggered.
  Collision type identifier of the first shape that this handler recognizes.
  In the collision handler callback, the shape with this type will be the first argument. Read only.
  Collision type identifier of the second shape that this handler recognizes.
  In the collision handler callback, the shape with this type will be the second argument. Read only.
  This function is called when two shapes with types that match this collision handler begin colliding.
  This function is called each step when two shapes with types that match this collision handler are colliding.
  It's called before the collision solver runs so that you can affect a collision's outcome.
  This function is called each step when two shapes with types that match this collision handler are colliding.
  It's called after the collision solver runs so that you can read back information about the collision to trigger events in your game.
  This function is called when two shapes with types that match this collision handler stop colliding.
  This is a user definable context pointer that is passed to all of the collision handler functions. }

  cpCollisionHandler = ^cpCollisionHandlerStruct;
  cpCollisionHandlerStruct = record
    typeA: cpCollisionType;
    typeB: cpCollisionType;
    beginFunc: cpCollisionBeginFunc;
    preSolveFunc: cpCollisionPreSolveFunc;
    postSolveFunc: cpCollisionPostSolveFunc;
    separateFunc: cpCollisionSeparateFunc;
    userData: cpDataPointer;
  end;

{ Allocate a cpSpace. }
function cpSpaceAlloc: cpSpace; cdecl; external;
{ Initialize a cpSpace. }
function cpSpaceInit(space: cpSpace): cpSpace; cdecl; external;
{ Allocate and initialize a cpSpace. }
function cpSpaceNew: cpSpace; cdecl; external;
{ Destroy a cpSpace. }
procedure cpSpaceDestroy(space: cpSpace); cdecl; external;
{ Destroy and free a cpSpace. }
procedure cpSpaceFree(space: cpSpace); cdecl; external;
{ Number of iterations to use in the impulse solver to solve contacts and other constraints. }
function cpSpaceGetIterations(space: cpSpace): LongInt; cdecl; external;
procedure cpSpaceSetIterations(space: cpSpace; iterations: LongInt); cdecl; external;
{ Gravity to pass to rigid bodies when integrating velocity. }
function cpSpaceGetGravity(space: cpSpace): TPointD; cdecl; external;
procedure cpSpaceSetGravity(space: cpSpace; gravity: TPointD); cdecl; external;
{ Damping rate expressed as the fraction of velocity bodies retain each second.
  A value of 0.9 would mean that each body's velocity will drop 10% per second.
  The default value is 1.0, meaning no damping is applied.
  @note This damping value is different than those of cpDampedSpring and cpDampedRotarySpring. }
function cpSpaceGetDamping(space: cpSpace): Double; cdecl; external;
procedure cpSpaceSetDamping(space: cpSpace; damping: Double); cdecl; external;
{ Speed threshold for a body to be considered idle.
  The default value of 0 means to let the space guess a good threshold based on gravity. }
function cpSpaceGetIdleSpeedThreshold(space: cpSpace): Double; cdecl; external;
procedure cpSpaceSetIdleSpeedThreshold(space: cpSpace; idleSpeedThreshold: Double); cdecl; external;
{ Time a group of bodies must remain idle in order to fall asleep.
  Enabling sleeping also implicitly enables the the contact graph.
  The default value of INFINITY disables the sleeping algorithm. }
function cpSpaceGetSleepTimeThreshold(space: cpSpace): Double; cdecl; external;
procedure cpSpaceSetSleepTimeThreshold(space: cpSpace; sleepTimeThreshold: Double); cdecl; external;
{ Amount of encouraged penetration between colliding shapes.
  Used to reduce oscillating contacts and keep the collision cache warm.
  Defaults to 0.1. If you have poor simulation quality,
  increase this number as much as possible without allowing visible amounts of overlap. }
function cpSpaceGetCollisionSlop(space: cpSpace): Double; cdecl; external;
procedure cpSpaceSetCollisionSlop(space: cpSpace; collisionSlop: Double); cdecl; external;
{ Determines how fast overlapping shapes are pushed apart.
  Expressed as a fraction of the error remaining after each second.
  Defaults to pow(1.0 - 0.1, 60.0) meaning that Chipmunk fixes 10% of overlap each frame at 60Hz. }
function cpSpaceGetCollisionBias(space: cpSpace): Double; cdecl; external;
procedure cpSpaceSetCollisionBias(space: cpSpace; collisionBias: Double); cdecl; external;
{ Number of frames that contact information should persist.
  Defaults to 3. There is probably never a reason to change this value. }
function cpSpaceGetCollisionPersistence(space: cpSpace): cpTimestamp; cdecl; external;
procedure cpSpaceSetCollisionPersistence(space: cpSpace; collisionPersistence: cpTimestamp); cdecl; external;
{ User definable data pointer.
  Generally this points to your game's controller or game state
  class so you can access it when given a cpSpace reference in a callback. }
function cpSpaceGetUserData(space: cpSpace): cpDataPointer; cdecl; external;
procedure cpSpaceSetUserData(space: cpSpace; userData: cpDataPointer); cdecl; external;
{ The Space provided static body for a given cpSpace.
  This is merely provided for convenience and you are not required to use it. }
function cpSpaceGetStaticBody(space: cpSpace): cpBody; cdecl; external;
{ Returns the current (or most recent) time step used with the given space.
  Useful from callbacks if your time step is not a compile-time global. }
function cpSpaceGetCurrentTimeStep(space: cpSpace): Double; cdecl; external;
{ returns true from inside a callback when objects cannot be added/removed. }
function cpSpaceIsLocked(space: cpSpace): cpBool; cdecl; external;
{ Create or return the existing collision handler that is called for all collisions that are not handled by a more specific collision handler. }
function cpSpaceAddDefaultCollisionHandler(space: cpSpace): cpCollisionHandler; cdecl; external;
{ Create or return the existing collision handler for the specified pair of collision types.
  If wildcard handlers are used with either of the collision types, it's the responibility of the custom handler to invoke the wildcard handlers. }
function cpSpaceAddCollisionHandler(space: cpSpace; a: cpCollisionType; b: cpCollisionType): cpCollisionHandler; cdecl; external;
{ Create or return the existing wildcard collision handler for the specified type. }
function cpSpaceAddWildcardHandler(space: cpSpace; _type: cpCollisionType): cpCollisionHandler; cdecl; external;
{ Add a collision shape to the simulation.
  If the shape is attached to a static body, it will be added as a static shape. }
function cpSpaceAddShape(space: cpSpace; shape: cpShape): cpShape; cdecl; external;
{ Add a rigid body to the simulation. }
function cpSpaceAddBody(space: cpSpace; body: cpBody): cpBody; cdecl; external;
{ Add a constraint to the simulation. }
function cpSpaceAddConstraint(space: cpSpace; constraint: cpConstraint): cpConstraint; cdecl; external;
{ Remove a collision shape from the simulation. }
procedure cpSpaceRemoveShape(space: cpSpace; shape: cpShape); cdecl; external;
{ Remove a rigid body from the simulation. }
procedure cpSpaceRemoveBody(space: cpSpace; body: cpBody); cdecl; external;
{ Remove a constraint from the simulation. }
procedure cpSpaceRemoveConstraint(space: cpSpace; constraint: cpConstraint); cdecl; external;
{ Test if a collision shape has been added to the space. }
function cpSpaceContainsShape(space: cpSpace; shape: cpShape): cpBool; cdecl; external;
{ Test if a rigid body has been added to the space. }
function cpSpaceContainsBody(space: cpSpace; body: cpBody): cpBool; cdecl; external;
{ Test if a constraint has been added to the space. }
function cpSpaceContainsConstraint(space: cpSpace; constraint: cpConstraint): cpBool; cdecl; external;

type
  { Post Step callback function type. }
  cpPostStepFunc = procedure(space: cpSpace; key: Pointer; data: Pointer); cdecl;

{ Schedule a post-step callback to be called when cpSpaceStep() finishes.
  You can only register one callback per unique value for @c key.
  Returns true only if @c key has never been scheduled before.
  It's possible to pass @c NULL for @c func if you only want to mark @c key as being used. }
function cpSpaceAddPostStepCallback(space: cpSpace; func: cpPostStepFunc; key: Pointer; data: Pointer): cpBool; cdecl; external;

type
  { Nearest point query callback function type. }
  cpSpacePointQueryFunc = procedure(shape: cpShape; point: TPointD; distance: Double; gradient: TPointD; data: Pointer); cdecl;

{ Query the space at a point and call @c func for each shape found. }
procedure cpSpacePointQuery(space: cpSpace; point: TPointD; maxDistance: Double; filter: cpShapeFilterStruct; func: cpSpacePointQueryFunc;
  data: Pointer); cdecl; external;
{ Query the space at a point and return the nearest shape found. Returns NULL if no shapes were found. }
function cpSpacePointQueryNearest(space: cpSpace; point: TPointD; maxDistance: Double; filter: cpShapeFilterStruct; outInfo: cpPointQueryInfo): cpShape; cdecl; external;

type
  { Segment query callback function type. }
  cpSpaceSegmentQueryFunc = procedure(shape: cpShape; point: TPointD; normal: TPointD; alpha: Double; data: Pointer); cdecl;

{ Perform a directed line segment query (like a raycast) against the space calling @c func for each shape intersected. }
procedure cpSpaceSegmentQuery(space: cpSpace; start: TPointD; finish: TPointD; radius: Double; filter: cpShapeFilterStruct;
  func: cpSpaceSegmentQueryFunc; data: Pointer); cdecl; external;
{ Perform a directed line segment query (like a raycast) against the space and return the first shape hit. Returns NULL if no shapes were hit. }
function cpSpaceSegmentQueryFirst(space: cpSpace; start: TPointD; finish: TPointD; radius: Double; filter: cpShapeFilterStruct;
  outInfo: cpSegmentQueryInfo): cpShape; cdecl; external;

type
  { Rectangle Query callback function type. }
  cpSpaceBBQueryFunc = procedure(shape: cpShape; data: Pointer); cdecl;

{ Perform a fast rectangle query on the space calling @c func for each shape found.
  Only the shape's bounding boxes are checked for overlap, not their full shape. }
procedure cpSpaceBBQuery(space: cpSpace; bb: cpBB; filter: cpShapeFilterStruct; func: cpSpaceBBQueryFunc; data: Pointer); cdecl; external;

type
  { Shape query callback function type. }
  cpSpaceShapeQueryFunc = procedure(shape: cpShape; points: cpContactPointSet; data: Pointer); cdecl;

{ Query a space for any shapes overlapping the given shape and call @c func for each shape found. }
function cpSpaceShapeQuery(space: cpSpace; shape: cpShape; func: cpSpaceShapeQueryFunc; data: Pointer): cpBool; cdecl; external;

type
  { Space/body iterator callback function type. }
  cpSpaceBodyIteratorFunc = procedure(body: cpBody; data: Pointer); cdecl;
  { Space/body iterator callback function type. }
  cpSpaceShapeIteratorFunc = procedure(shape: cpShape; data: Pointer); cdecl;
  { Space/constraint iterator callback function type. }
  cpSpaceConstraintIteratorFunc = procedure(constraint: cpConstraint; data: Pointer); cdecl;

{ Call @c func for each body in the space. }
procedure cpSpaceEachBody(space: cpSpace; func: cpSpaceBodyIteratorFunc; data: Pointer); cdecl; external;
{ Call @c func for each shape in the space. }
procedure cpSpaceEachShape(space: cpSpace; func: cpSpaceShapeIteratorFunc; data: Pointer); cdecl; external;
{ Call @c func for each shape in the space. }
procedure cpSpaceEachConstraint(space: cpSpace; func: cpSpaceConstraintIteratorFunc; data: Pointer); cdecl; external;
{ Update the collision detection info for the static shapes in the space. }
procedure cpSpaceReindexStatic(space: cpSpace); cdecl; external;
{ Update the collision detection data for a specific shape in the space. }
procedure cpSpaceReindexShape(space: cpSpace; shape: cpShape); cdecl; external;
{ Update the collision detection data for all shapes attached to a body. }
procedure cpSpaceReindexShapesForBody(space: cpSpace; body: cpBody); cdecl; external;
{ Switch the space to use a spatial has as it's spatial index. }
procedure cpSpaceUseSpatialHash(space: cpSpace; dim: Double; count: LongInt); cdecl; external;
{ Step the space forward in time by @c dt. }
procedure cpSpaceStep(space: cpSpace; dt: Double); cdecl; external;

{ Color type to use with the space debug drawing API. }

type
  cpSpaceDebugColor = ^cpSpaceDebugColorStruct;
  cpSpaceDebugColorStruct = record
    r: single;
    g: single;
    b: single;
    a: single;
  end;

  { Callback type for a function that draws a filled, stroked circle. }
  cpSpaceDebugDrawCircleImpl = procedure(pos: TPointD; angle: Double; radius: Double; outlineColor: cpSpaceDebugColorStruct; fillColor: cpSpaceDebugColorStruct;
    data: cpDataPointer); cdecl;
  { Callback type for a function that draws a line segment. }
  cpSpaceDebugDrawSegmentImpl = procedure(a: TPointD; b: TPointD; color: cpSpaceDebugColorStruct; data: cpDataPointer); cdecl;
  { Callback type for a function that draws a thick line segment. }
  cpSpaceDebugDrawFatSegmentImpl = procedure(a: TPointD; b: TPointD; radius: Double; outlineColor: cpSpaceDebugColorStruct; fillColor: cpSpaceDebugColorStruct;
    data: cpDataPointer); cdecl;
  { Callback type for a function that draws a convex polygon. }
  cpSpaceDebugDrawPolygonImpl = procedure(count: LongInt; verts: PPointD; radius: Double; outlineColor: cpSpaceDebugColorStruct; fillColor: cpSpaceDebugColorStruct;
    data: cpDataPointer); cdecl;
  { Callback type for a function that draws a dot. }
  cpSpaceDebugDrawDotImpl = procedure(size: Double; pos: TPointD; color: cpSpaceDebugColorStruct; data: cpDataPointer); cdecl;
  { Callback type for a function that returns a color for a given shape. This gives you an opportunity to color shapes based on how they are used in your engine. }
  cpSpaceDebugDrawColorForShapeImpl = function(shape: cpShape; data: cpDataPointer): cpSpaceDebugColorStruct; cdecl;

  PcpSpaceDebugDrawFlags = ^cpSpaceDebugDrawFlags;
  cpSpaceDebugDrawFlags =  LongInt;

const
  CP_SPACE_DEBUG_DRAW_SHAPES = 1 shl 0;
  CP_SPACE_DEBUG_DRAW_CONSTRAINTS = 1 shl 1;
  CP_SPACE_DEBUG_DRAW_COLLISION_POINTS = 1 shl 2;

{ Struct used with cpSpaceDebugDraw() containing drawing callbacks and other drawing settings.
  Function that will be invoked to draw circles.
  Function that will be invoked to draw line segments.
  Function that will be invoked to draw thick line segments.
  Function that will be invoked to draw convex polygons.
  Function that will be invoked to draw dots.
  Flags that request which things to draw (collision shapes, constraints, contact points).
  Outline color passed to the drawing function.
  Function that decides what fill color to draw shapes using.
  Color passed to drawing functions for constraints.
  Color passed to drawing functions for collision points.
  User defined context pointer passed to all of the callback functions as the 'data' argument. }

type
  cpSpaceDebugDrawOptions = ^cpSpaceDebugDrawOptionsStruct;
  cpSpaceDebugDrawOptionsStruct = record
    drawCircle: cpSpaceDebugDrawCircleImpl;
    drawSegment: cpSpaceDebugDrawSegmentImpl;
    drawFatSegment: cpSpaceDebugDrawFatSegmentImpl;
    drawPolygon: cpSpaceDebugDrawPolygonImpl;
    drawDot: cpSpaceDebugDrawDotImpl;
    flags: cpSpaceDebugDrawFlags;
    shapeOutlineColor: cpSpaceDebugColorStruct;
    colorForShape: cpSpaceDebugDrawColorForShapeImpl;
    constraintColor: cpSpaceDebugColorStruct;
    collisionPointColor: cpSpaceDebugColorStruct;
    data: cpDataPointer;
  end;

{ Debug draw the current state of the space using the supplied drawing options. }
procedure cpSpaceDebugDraw(space: cpSpace; options: cpSpaceDebugDrawOptions); cdecl; external;

{ From other places }

type
  freeFunc = procedure (arr: Pointer); cdecl;
  cpHashSetEqlFunc = function (ptr: Pointer; elt: Pointer): cpBool; cdecl;
  PcpHashSetTransFunc = ^cpHashSetTransFunc;
  cpHashSetTransFunc = function (ptr: Pointer; data: Pointer): Pointer; cdecl;
  cpHashSetIteratorFunc = procedure (elt: Pointer; data: Pointer); cdecl;
  cpHashSetFilterFunc = function (elt: Pointer; data: Pointer): cpBool; cdecl;

function cpArrayNew(size: LongInt): cpArray; cdecl; external;
procedure cpArrayFree(arr: cpArray); cdecl; external;
procedure cpArrayPush(arr: cpArray; imte: Pointer); cdecl; external;
function cpArrayPop(arr: cpArray): Pointer; cdecl; external;
procedure cpArrayDeleteObj(arr: cpArray; obj: Pointer); cdecl; external;
function cpArrayContains(arr: cpArray; ptr: Pointer): cpBool; cdecl; external;
procedure cpArrayFreeEach(arr: cpArray; func: freeFunc); cdecl; external;
function cpHashSetNew(size: LongInt; eqlFunc: cpHashSetEqlFunc): cpHashSet; cdecl; external;
procedure cpHashSetSetDefaultValue(hashSet: cpHashSet; default_value: Pointer); cdecl; external;
procedure cpHashSetFree(hashSet: cpHashSet); cdecl; external;
function cpHashSetCount(hashSet: cpHashSet): LongInt; cdecl; external;
function cpHashSetInsert(hashSet: cpHashSet; hash: cpHashValue; ptr: Pointer; trans: cpHashSetTransFunc; data: Pointer): Pointer; cdecl; external;
function cpHashSetRemove(hashSet: cpHashSet; hash: cpHashValue; ptr: Pointer): Pointer; cdecl; external;
function cpHashSetFind(hashSet: cpHashSet; hash: cpHashValue; ptr: Pointer): Pointer; cdecl; external;
procedure cpHashSetEach(hashSet: cpHashSet; func: cpHashSetIteratorFunc; data: Pointer); cdecl; external;
procedure cpHashSetFilter(hashSet: cpHashSet; func: cpHashSetFilterFunc; data: Pointer); cdecl; external;
procedure cpBodyAddShape(body: cpBody; shape: cpShape); cdecl; external;
procedure cpBodyRemoveShape(body: cpBody; shape: cpShape); cdecl; external;
procedure cpBodyAccumulateMassFromShapes(body: cpBody); cdecl; external;
procedure cpBodyRemoveConstraint(body: cpBody; constraint: cpConstraint); cdecl; external;
function cpSpatialIndexInit(index: cpSpatialIndex; klass: cpSpatialIndexClass; bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external;
function cpArbiterInit(arb: cpArbiter; a: cpShape; b: cpShape): cpArbiter; cdecl; external;
procedure cpArbiterUnthread(arb: cpArbiter); cdecl; external;
procedure cpArbiterUpdate(arb: cpArbiter; info: cpCollisionInfo; space: cpSpace); cdecl; external;
procedure cpArbiterPreStep(arb: cpArbiter; dt: Double; bias: Double; slop: Double); cdecl; external;
procedure cpArbiterApplyCachedImpulse(arb: cpArbiter; dt_coef: Double); cdecl; external;
procedure cpArbiterApplyImpulse(arb: cpArbiter); cdecl; external;
function cpShapeInit(shape: cpShape; klass: cpShapeClass; body: cpBody; massInfo: cpShapeMassInfoStruct): cpShape; cdecl; external;
function cpCollide(a: cpShape; b: cpShape; id: cpCollisionID; contacts: cpContact): cpCollisionInfoStruct; cdecl; external;
procedure cpLoopIndexes(verts: PPointD; count: LongInt; out start, finish: LongInt); cdecl; external;
procedure cpConstraintInit(constraint: cpConstraint; klass: cpConstraintClass; a: cpBody; b: cpBody); cdecl; external;
procedure cpSpaceSetStaticBody(space: cpSpace; body: cpBody); cdecl; external;
procedure cpSpaceProcessComponents(space: cpSpace; dt: Double); cdecl; external;
procedure cpSpacePushFreshContactBuffer(space: cpSpace); cdecl; external;
function cpContactBufferGetArray(space: cpSpace): cpContact; cdecl; external;
procedure cpSpacePushContacts(space: cpSpace; count: LongInt); cdecl; external;
function cpSpaceGetPostStepCallback(space: cpSpace; key: Pointer): cpPostStepCallback; cdecl; external;
function cpSpaceArbiterSetFilter(arb: cpArbiter; space: cpSpace): cpBool; cdecl; external;
procedure cpSpaceFilterArbiters(space: cpSpace; body: cpBody; filter: cpShape); cdecl; external;
procedure cpSpaceActivateBody(space: cpSpace; body: cpBody); cdecl; external;
procedure cpSpaceLock(space: cpSpace); cdecl; external;
procedure cpSpaceUnlock(space: cpSpace; runPostStep: cpBool); cdecl; external;

{ Simple extentsions }

procedure cpBodyDestroyChildren(body: cpBody);
procedure cpSpaceDestroyChildren(space: cpSpace);

implementation

{$ifdef linux}
  {$linklib c}
  {$linklib m}
  {$linklib ../libs/chipmunk-linux}
{$endif}

{ TPointD }

procedure SinCos(x: Double; out s, c: Double);
begin
  s := Sin(x);
  c := Cos(x);
end;

function cpV(x, y: Double): TPointD;
begin
  Result.X := x; Result.Y := y;
end;

function cpClamp(a, min, max: Double): Double;
begin
  if a < min then Result := min else if a > max then Result := max else Result := a;
end;

{class function TPointD.Create: TPointD;
begin
  Result.x := 0; Result.y := 0;
end;

class function TPointD.Create(const x, y: Double): TPointD;
begin
  Result.x := x; Result.y := y;
end;

class operator TPointD.Negative(const a: TPointD): TPointD;
begin
  Result.x := -a.x; Result.y := -a.y;
end;

class operator TPointD.Equal(const a, b: TPointD): Boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y);
end;

class operator TPointD.NotEqual(const a, b: TPointD): Boolean;
begin
  Result :=  (a.x <> b.x) or (a.y <> b.y);
end;

class operator TPointD.Add(const a, b: TPointD): TPointD;
begin
  Result.x := a.x + b.x; Result.y := a.y + b.y;
end;

class operator TPointD.Subtract(const a, b: TPointD): TPointD;
begin
  Result.x := a.x - b.x; Result.y := a.y - b.y;
end;

class operator TPointD.Multiply(const a, b: TPointD): TPointD;
begin
  Result.x := a.x * b.x; Result.y := a.y * b.y;
end;

class operator TPointD.Multiply(a: Double; const b: TPointD): TPointD;
begin
  Result.x := b.x * a; Result.y := b.y * a;
end;

class operator TPointD.Multiply(const a: TPointD; b: Double): TPointD;
begin
  Result.x := a.x * b; Result.y := a.y * b;
end;

class operator TPointD.Divide(a: Double; const b: TPointD): TPointD;
begin
  Result.x := a / b.x / a; Result.y := a / b.y;
end;

function TPointD.Rotate(angle: Double): TPointD;
var
  s, c: Double;
begin
  if Angle = 0 then
    Exit(Self);
  SinCos(angle, s, c);
  Result.x := Self.x * C - Self.y * s;
  Result.y := Self.x * s + Self.y * c;
end;

function TPointD.Distance: Double;
begin
  Result := Sqrt(x * x + y * y);
end;

function TPointD.Distance(const a: TPointD): Double;
var
  x1, y1: Double;
begin
  x1 := a.x - x; y1 := a.y - y;
  Result := Sqrt(x1 * x1 + y1 * y1);
end;

function TPointD.PointAtDistance(const a: TPointD; distance: Double): TPointD;
begin
  Result := distance * (a - Self).Normalize + Self;
end;

function TPointD.PointAtMix(const a: TPointD; percent: Double): TPointD;
var
  inv: Double;
begin
  inv := 1 - percent;
  Result.x := a.x * percent + x * inv;
  Result.y := a.y * percent + y * inv;
end;

function TPointD.Normalize: TPointD;
var
  d: Double;
begin
  d := Distance + CP_EPSILON;
  d := 1 / d;
  Result.x := x * d;
  Result.y := y * d;
end;

function TPointD.Normal(const a: TPointD; scale: Double = 1): TPointD;
var
  t: Double;
begin
  Result := (Self - a).Normalize * scale;
  t := -Result.x;
  Result.x := Result.y;
  Result.y := t;
end;

function TPointD.NormalAtDistance(const a: TPointD; distance: Double; scale: Double = 1): TPointD;
begin
  Result := Normal(a, scale) + Normalize * distance;
end;

function TPointD.NormalAtMix(const a: TPointD; percent: Double; scale: Double = 1): TPointD;
begin
  Result := Normal(a, scale) + PointAtMix(a, percent);
end;}

procedure cpSpatialIndexDestroy(index: cpSpatialIndex);
begin
	if index.klass <> nil then
    index.klass.destroy(index);
end;

function cpSpatialIndexCount(index: cpSpatialIndex): Integer;
begin
  Result := index.klass.count(index);
end;

procedure cpSpatialIndexEach(index: cpSpatialIndex; func: cpSpatialIndexIteratorFunc; data: Pointer);
begin
  index.klass.each(index, func, data);
end;

function cpSpatialIndexContains(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue): cpBool;
begin
  Result := index.klass.contains(index, obj, hashid);
end;

procedure cpSpatialIndexInsert(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue);
begin
  index.klass.insert(index, obj, hashid);
end;

procedure cpSpatialIndexRemove(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue);
begin
  index.klass.remove(index, obj, hashid);
end;

procedure cpSpatialIndexReindex(index: cpSpatialIndex);
begin
  index.klass.reindex(index);
end;

procedure cpSpatialIndexReindexObject(index: cpSpatialIndex; obj: Pointer; hashid: cpHashValue);
begin
  index.klass.reindexObject(index, obj, hashid);
end;

procedure cpSpatialIndexQuery(index: cpSpatialIndex; obj: Pointer; bb: cpBB; func: cpSpatialIndexQueryFunc; data: Pointer);
begin
  index.klass.query(index, obj, bb, func, data);
end;

procedure cpSpatialIndexSegmentQuery(index: cpSpatialIndex; obj: Pointer; a, b: TPointD; t_exit: Double; func: cpSpatialIndexSegmentQueryFunc; data: Pointer);
begin
  index.klass.segmentQuery(index, obj, a, b, t_exit, func, data);
end;

procedure cpSpatialIndexReindexQuery(index: cpSpatialIndex; func: cpSpatialIndexQueryFunc; data: Pointer);
begin
  index.klass.reindexQuery(index, func, data);
end;

procedure WakeEachShape(shape: cpShape; data: Pointer); cdecl;
var
  B: cpBody;
begin
  B := cpShapeGetBody(shape);
  if B = nil then
    Exit;
  if cpBodyIsSleeping(B) <> cpFalse then
    cpBodyActivate(B);
end;

procedure DestroyEachShape(shape: cpShape; data: Pointer); cdecl;
begin
  if data <> nil then
    cpSpaceRemoveShape(cpSpace(data), shape);
  cpShapeFree(shape);
end;

procedure DestroyEachConstraint(constraint: cpConstraint; data: Pointer); cdecl;
begin
  cpConstraintFree(constraint);
end;

procedure DestroyEachBody(body: cpBody; data: Pointer); cdecl;
begin
  cpBodyFree(body);
end;

procedure cpBodyDestroyChildren(body: cpBody);
begin
  // cpBodyEachShape(body, WakeEachShape, nil);
  // cpBodyEachShape(body, DestroyEachShape, cpBodyGetSpace(body)));
end;

procedure cpSpaceDestroyChildren(space: cpSpace);
begin
  cpSpaceEachShape(space, WakeEachShape, nil);
  cpSpaceEachShape(space, DestroyEachShape, nil);
  cpSpaceEachConstraint(space, DestroyEachConstraint, nil);
  cpSpaceEachBody(space, DestroyEachBody, nil);
end;

end.
