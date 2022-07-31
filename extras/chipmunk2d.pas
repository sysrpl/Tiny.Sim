unit Chipmunk2D;

{$i options.inc}

interface

// From chipmunk_types.h

const
  CP_INFINITY = 1e1000;
  CP_EPSILON = 1e-8;
  CP_PI = PI;
  cpTrue = 1;
  cpFalse = 0;

type
  cpFloat = Double;
  PcpFloat = ^cpFloat;
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

type
  PcpVect = ^cpVect;
  cpVect = record
  public
    x, y: cpFloat;
    class function Create: cpVect; overload; static; inline;
    class function Create(const x, y: cpFloat): cpVect; overload; static; inline;
    class operator Negative(const a: cpVect): cpVect; inline;
    class operator Equal(const a, b: cpVect): Boolean; inline;
    class operator NotEqual(const a, b: cpVect): Boolean; inline;
    class operator Add(const a, b: cpVect): cpVect; inline;
    class operator Subtract(const a, b: cpVect): cpVect; inline;
    class operator Multiply(const a, b: cpVect): cpVect; inline;
    class operator Multiply(a: cpFloat; const b: cpVect): cpVect; inline;
    class operator Multiply(const a: cpVect; b: cpFloat): cpVect; inline;
    class operator Divide(a: cpFloat; const b: cpVect): cpVect; inline;
    function Rotate(angle: cpFloat): cpVect;
    function Distance: cpFloat; overload;
    function Distance(const a: cpVect): cpFloat; overload;
    function PointAtDistance(const a: cpVect; distance: cpFloat): cpVect;
    function PointAtMix(const a: cpVect; percent: cpFloat): cpVect;
    function Normalize: cpVect;
    function Normal(const a: cpVect; scale: cpFloat = 1): cpVect;
    function NormalAtDistance(const a: cpVect; distance: cpFloat; scale: cpFloat = 1): cpVect;
    function NormalAtMix(const a: cpVect; percent: cpFloat; scale: cpFloat = 1): cpVect;
  end;

const
  cpVZero: cpVect = (x: 0; y: 0);

function cpV(x, y: cpFloat): cpVect; inline;
function cpClamp(a, min, max: cpFloat): cpFloat; inline;

{ Axis-aligned 2D bounding box type (left, bottom, right, top). }

type
  PcpBB = ^cpBB;
  cpBB = record
    l, b, r, t: cpFloat;
  end;

(*
static inline Boolean cpveql(const cpVect v1, const cpVect v2)
  {
  	return (v1.x == v2.x && v1.y == v2.y);
  }

  /// Add two vectors
  static inline cpVect cpvadd(const cpVect v1, const cpVect v2)
  {
  	return cpv(v1.x + v2.x, v1.y + v2.y);
  }

  /// Subtract two vectors.
  static inline cpVect cpvsub(const cpVect v1, const cpVect v2)
  {
  	return cpv(v1.x - v2.x, v1.y - v2.y);
  }

  /// Negate a vector.
  static inline cpVect cpvneg(const cpVect v)
  {
  	return cpv(-v.x, -v.y);
  }

  /// Scalar multiplication.
  static inline cpVect cpvmult(const cpVect v, const cpFloat s)
  {
  	return cpv(v.x*s, v.y*s);
  }

  /// Vector dot product.
  static inline cpFloat cpvdot(const cpVect v1, const cpVect v2)
  {
  	return v1.x*v2.x + v1.y*v2.y;
  }

  /// 2D vector cross product analog.
  /// The cross product of 2D vectors results in a 3D vector with only a z component.
  /// This function returns the magnitude of the z value.
  static inline cpFloat cpvcross(const cpVect v1, const cpVect v2)
  {
  	return v1.x*v2.y - v1.y*v2.x;
  }

  /// Returns a perpendicular vector. (90 degree rotation)
  static inline cpVect cpvperp(const cpVect v)
  {
  	return cpv(-v.y, v.x);
  }

  /// Returns a perpendicular vector. (-90 degree rotation)
  static inline cpVect cpvrperp(const cpVect v)
  {
  	return cpv(v.y, -v.x);
  }

  /// Returns the vector projection of v1 onto v2.
  static inline cpVect cpvproject(const cpVect v1, const cpVect v2)
  {
  	return cpvmult(v2, cpvdot(v1, v2)/cpvdot(v2, v2));
  }

  /// Returns the unit length vector for the given angle (in radians).
  static inline cpVect cpvforangle(const cpFloat a)
  {
  	return cpv(cpfcos(a), cpfsin(a));
  }

  /// Returns the angular direction v is pointing in (in radians).
  static inline cpFloat cpvtoangle(const cpVect v)
  {
  	return cpfatan2(v.y, v.x);
  }

  /// Uses complex number multiplication to rotate v1 by v2. Scaling will occur if v1 is not a unit vector.
  static inline cpVect cpvrotate(const cpVect v1, const cpVect v2)
  {
  	return cpv(v1.x*v2.x - v1.y*v2.y, v1.x*v2.y + v1.y*v2.x);
  }

  /// Inverse of cpvrotate().
  static inline cpVect cpvunrotate(const cpVect v1, const cpVect v2)
  {
  	return cpv(v1.x*v2.x + v1.y*v2.y, v1.y*v2.x - v1.x*v2.y);
  }

  /// Returns the squared length of v. Faster than cpvlength() when you only need to compare lengths.
  static inline cpFloat cpvlengthsq(const cpVect v)
  {
  	return cpvdot(v, v);
  }

  /// Returns the length of v.
  static inline cpFloat cpvlength(const cpVect v)
  {
  	return cpfsqrt(cpvdot(v, v));
  }

  /// Linearly interpolate between v1 and v2.
  static inline cpVect cpvlerp(const cpVect v1, const cpVect v2, const cpFloat t)
  {
  	return cpvadd(cpvmult(v1, 1.0f - t), cpvmult(v2, t));
  }

  /// Returns a normalized copy of v.
  static inline cpVect cpvnormalize(const cpVect v)
  {
  	// Neat trick I saw somewhere to avoid div/0.
  	return cpvmult(v, 1.0f/(cpvlength(v) + CPFLOAT_MIN));
  }

  /// Spherical linearly interpolate between v1 and v2.
  static inline cpVect
  cpvslerp(const cpVect v1, const cpVect v2, const cpFloat t)
  {
  	cpFloat dot = cpvdot(cpvnormalize(v1), cpvnormalize(v2));
  	cpFloat omega = cpfacos(cpfclamp(dot, -1.0f, 1.0f));

  	if(omega < 1e-3){
  		// If the angle between two vectors is very small, lerp instead to avoid precision issues.
  		return cpvlerp(v1, v2, t);
  	} else {
  		cpFloat denom = 1.0f/cpfsin(omega);
  		return cpvadd(cpvmult(v1, cpfsin((1.0f - t)*omega)*denom), cpvmult(v2, cpfsin(t*omega)*denom));
  	}
  }

  /// Spherical linearly interpolate between v1 towards v2 by no more than angle a radians
  static inline cpVect
  cpvslerpconst(const cpVect v1, const cpVect v2, const cpFloat a)
  {
  	cpFloat dot = cpvdot(cpvnormalize(v1), cpvnormalize(v2));
  	cpFloat omega = cpfacos(cpfclamp(dot, -1.0f, 1.0f));

  	return cpvslerp(v1, v2, cpfmin(a, omega)/omega);
  }

  /// Clamp v to length len.
  static inline cpVect cpvclamp(const cpVect v, const cpFloat len)
  {
  	return (cpvdot(v,v) > len*len) ? cpvmult(cpvnormalize(v), len): v;
  }

  /// Linearly interpolate between v1 towards v2 by distance d.
  static inline cpVect cpvlerpconst(cpVect v1, cpVect v2, cpFloat d)
  {
  	return cpvadd(v1, cpvclamp(cpvsub(v2, v1), d));
  }

  /// Returns the distance between v1 and v2.
  static inline cpFloat cpvdist(const cpVect v1, const cpVect v2)
  {
  	return cpvlength(cpvsub(v1, v2));
  }

  /// Returns the squared distance between v1 and v2. Faster than cpvdist() when you only need to compare distances.
  static inline cpFloat cpvdistsq(const cpVect v1, const cpVect v2)
  {
  	return cpvlengthsq(cpvsub(v1, v2));
  }

  /// Returns true if the distance between v1 and v2 is less than dist.
  static inline cpBool cpvnear(const cpVect v1, const cpVect v2, const cpFloat dist)
  {
  	return cpvdistsq(v1, v2) < dist*dist;
  }

  /// @}

  /// @defgroup cpMat2x2 cpMat2x2
  /// 2x2 matrix type used for tensors and such.
  /// @{

  // NUKE
  static inline cpMat2x2
  cpMat2x2New(cpFloat a, cpFloat b, cpFloat c, cpFloat d)
  {
  	cpMat2x2 m = {a, b, c, d};
  	return m;
  }

  static inline cpVect
  cpMat2x2Transform(cpMat2x2 m, cpVect v)
  {
  	return cpv(v.x*m.a + v.y*m.b, v.x*m.c + v.y*m.d);
  }



  /// Return the max of two cpFloats.
  static inline cpFloat cpfmax(cpFloat a, cpFloat b)
  {
  	return (a > b) ? a: b;
  }

  /// Return the min of two cpFloats.
  static inline cpFloat cpfmin(cpFloat a, cpFloat b)
  {
  	return (a < b) ? a: b;
  }

  /// Return the absolute value of a cpFloat.
  static inline cpFloat cpfabs(cpFloat f)
  {
  	return (f < 0) ? -f: f;
  }

  /// Clamp @c f to be between @c min and @c max.
  static inline cpFloat cpfclamp(cpFloat f, cpFloat min, cpFloat max)
  {
  	return cpfmin(cpfmax(f, min), max);
  }

  /// Clamp @c f to be between 0 and 1.
  static inline cpFloat cpfclamp01(cpFloat f)
  {
  	return cpfmax(0.0f, cpfmin(f, 1.0f));
  }


  /// Linearly interpolate (or extrapolate) between @c f1 and @c f2 by @c t percent.
  static inline cpFloat cpflerp(cpFloat f1, cpFloat f2, cpFloat t)
  {
  	return f1*(1.0f - t) + f2*t;
  }

  /// Linearly interpolate from @c f1 to @c f2 by no more than @c d.
  static inline cpFloat cpflerpconst(cpFloat f1, cpFloat f2, cpFloat d)
  {
  	return f1 + cpfclamp(f2 - f1, -d, d);
  }

  /// Returns the closest point on the line segment ab, to the point p.
  static inline cpVect
  cpClosetPointOnSegment(const cpVect p, const cpVect a, const cpVect b)
  {
  	cpVect delta = cpvsub(a, b);
  	cpFloat t = cpfclamp01(cpvdot(delta, cpvsub(p, b))/cpvlengthsq(delta));
  	return cpvadd(b, cpvmult(delta, t));
  }

  /// Convenience constructor for cpBB structs.
  static inline cpBB cpBBNew(const cpFloat l, const cpFloat b, const cpFloat r, const cpFloat t)
  {
  	cpBB bb = {l, b, r, t};
  	return bb;
  }

  /// Constructs a cpBB centered on a point with the given extents (half sizes).
  static inline cpBB
  cpBBNewForExtents(const cpVect c, const cpFloat hw, const cpFloat hh)
  {
  	return cpBBNew(c.x - hw, c.y - hh, c.x + hw, c.y + hh);
  }

  /// Constructs a cpBB for a circle with the given position and radius.
  static inline cpBB cpBBNewForCircle(const cpVect p, const cpFloat r)
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
  static inline cpBool cpBBContainsVect(const cpBB bb, const cpVect v)
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
  static inline cpBB cpBBExpand(const cpBB bb, const cpVect v){
  	return cpBBNew(
  		cpfmin(bb.l, v.x),
  		cpfmin(bb.b, v.y),
  		cpfmax(bb.r, v.x),
  		cpfmax(bb.t, v.y)
  	);
  }

  /// Returns the center of a bounding box.
  static inline cpVect
  cpBBCenter(cpBB bb)
  {
  	return cpvlerp(cpv(bb.l, bb.b), cpv(bb.r, bb.t), 0.5f);
  }

  /// Returns the area of the bounding box.
  static inline cpFloat cpBBArea(cpBB bb)
  {
  	return (bb.r - bb.l)*(bb.t - bb.b);
  }

  /// Merges @c a and @c b and returns the area of the merged bounding box.
  static inline cpFloat cpBBMergedArea(cpBB a, cpBB b)
  {
  	return (cpfmax(a.r, b.r) - cpfmin(a.l, b.l))*(cpfmax(a.t, b.t) - cpfmin(a.b, b.b));
  }

  /// Returns the fraction along the segment query the cpBB is hit. Returns INFINITY if it doesn't hit.
  static inline cpFloat cpBBSegmentQuery(cpBB bb, cpVect a, cpVect b)
  {
  	cpVect delta = cpvsub(b, a);
  	cpFloat tmin = -INFINITY, tmax = INFINITY;

  	if(delta.x == 0.0f){
  		if(a.x < bb.l || bb.r < a.x) return INFINITY;
  	} else {
  		cpFloat t1 = (bb.l - a.x)/delta.x;
  		cpFloat t2 = (bb.r - a.x)/delta.x;
  		tmin = cpfmax(tmin, cpfmin(t1, t2));
  		tmax = cpfmin(tmax, cpfmax(t1, t2));
  	}

  	if(delta.y == 0.0f){
  		if(a.y < bb.b || bb.t < a.y) return INFINITY;
  	} else {
  		cpFloat t1 = (bb.b - a.y)/delta.y;
  		cpFloat t2 = (bb.t - a.y)/delta.y;
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
  static inline cpBool cpBBIntersectsSegment(cpBB bb, cpVect a, cpVect b)
  {
  	return (cpBBSegmentQuery(bb, a, b) != INFINITY);
  }

  /// Clamp a vector to a bounding box.
  static inline cpVect
  cpBBClampVect(const cpBB bb, const cpVect v)
  {
  	return cpv(cpfclamp(v.x, bb.l, bb.r), cpfclamp(v.y, bb.b, bb.t));
  }

  /// Wrap a vector to a bounding box.
  static inline cpVect
  cpBBWrapVect(const cpBB bb, const cpVect v)
  {
  	cpFloat dx = cpfabs(bb.r - bb.l);
  	cpFloat modx = cpfmod(v.x - bb.l, dx);
  	cpFloat x = (modx > 0.0f) ? modx: modx + dx;

  	cpFloat dy = cpfabs(bb.t - bb.b);
  	cpFloat mody = cpfmod(v.y - bb.b, dy);
  	cpFloat y = (mody > 0.0f) ? mody: mody + dy;

  	return cpv(x + bb.l, y + bb.b);
  }

  /// Returns a bounding box offseted by @c v.
  static inline cpBB
  cpBBOffset(const cpBB bb, const cpVect v)
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
    a, b, c, d, tx, ty: cpFloat;
  end;

{ Nuke this matrix }

  PcpMat2x2 = ^cpMat2x2;
  cpMat2x2 = record
    a, b, c, d: cpFloat;
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
  cpSpatialIndexSegmentQueryFunc = function(obj1: Pointer; obj2: Pointer; data: Pointer): cpFloat; cdecl;
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
  cpSpatialIndexSegmentQueryImpl = procedure(index: Pointer; obj: Pointer; a: cpVect; b: cpVect; t_exit: cpFloat;
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
procedure cpSpatialIndexSegmentQuery(index: cpSpatialIndex; obj: Pointer; a, b: cpVect; t_exit: cpFloat; func: cpSpatialIndexSegmentQueryFunc; data: Pointer); inline;
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
function cpSpaceHashInit(hash: cpSpaceHash; celldim: cpFloat; numcells: LongInt; bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external;
{ Allocate and initialize a spatial hash. }
function cpSpaceHashNew(celldim: cpFloat; cells: LongInt; bbfunc: cpSpatialIndexBBFunc; staticIndex: cpSpatialIndex): cpSpatialIndex; cdecl; external;
{ Change the cell dimensions and table size of the spatial hash to tune it.
  The cell dimensions should roughly match the average size of your objects
  and the table size should be ~10 larger than the number of objects inserted.
  Some trial and error is required to find the optimum numbers for efficiency. }
procedure cpSpaceHashResize(hash: cpSpaceHash; celldim: cpFloat; numcells: LongInt); cdecl; external;

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
  cpBBTreeVelocityFunc = function(obj: Pointer): cpVect; cdecl;

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
    gravity: cpVect;
    damping: cpFloat;
    idleSpeedThreshold: cpFloat;
    sleepTimeThreshold: cpFloat;
    collisionSlop: cpFloat;
    collisionBias: cpFloat;
    collisionPersistence: cpTimestamp;
    userData: cpDataPointer;
    stamp: cpTimestamp;
    curr_dt: cpFloat;
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
  cpBodyVelocityFunc = procedure(body: Pointer; gravity: cpVect; damping: cpFloat; dt: cpFloat); cdecl;
  { Rigid body position update function type. Param body is of type PcpBody }
  cpBodyPositionFunc = procedure(body: Pointer; dt: cpFloat); cdecl;

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
    m: cpFloat;
    m_inv: cpFloat;
    i: cpFloat;
    i_inv: cpFloat;
    cog: cpVect;
    p: cpVect;
    v: cpVect;
    f: cpVect;
    a: cpFloat;
    w: cpFloat;
    t: cpFloat;
    transform: cpTransform;
    userData: cpDataPointer;
    v_bias: cpVect;
    w_bias: cpFloat;
    space: cpSpace;
    shapeList: Pointer {cpShape};
    arbiterList: Pointer {cpArbiter};
    constraintList: Pointer {cpConstraint};
    sleeping: record
      root: cpBody;
      next: cpBody;
      idleTime: cpFloat;
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
    r1: cpVect;
    r2: cpVect;
    nMass: cpFloat;
    tMass: cpFloat;
    bounce: cpFloat;
    jnAcc: cpFloat;
    jtAcc: cpFloat;
    jBias: cpFloat;
    bias: cpFloat;
    hash: cpHashValue;
  end;

  cpCollisionInfo = ^cpCollisionInfoStruct;
  cpCollisionInfoStruct = record
    a: Pointer {cpShape};
    b: Pointer {cpShape};
    id: cpCollisionID;
    n: cpVect;
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
    e: cpFloat;
    u: cpFloat;
    surface_vr: cpVect;
    data: cpDataPointer;
    a: Pointer {cpShape};
    b: Pointer {cpShape};
    body_a: cpBody;
    body_b: cpBody;
    thread_a: cpArbiterThreadStruct;
    thread_b: cpArbiterThreadStruct;
    count: LongInt;
    contacts: cpContact;
    n: cpVect;
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
    m: cpFloat;
    i: cpFloat;
    cog: cpVect;
    area: cpFloat;
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
    e: cpFloat;
    u: cpFloat;
    surfaceV: cpVect;
    userData: cpDataPointer;
    _type: cpCollisionType;
    filter: cpShapeFilterStruct;
    next: cpShape;
    prev: cpShape;
    hashid: cpHashValue;
  end;

  cpShapeCacheDataImpl = function(shape: cpShape; transform: cpTransform): cpBB; cdecl;
  cpShapeDestroyImpl = procedure(shape: cpShape); cdecl;
  cpShapePointQueryImpl = procedure(shape: cpShape; p: cpVect; info: Pointer {cpPointQueryInfo}); cdecl;
  cpShapeSegmentQueryImpl = procedure(shape: cpShape; a: cpVect; b: cpVect; radius: cpFloat; info: Pointer {cpSegmentQueryInfo}); cdecl;

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
    c: cpVect;
    tc: cpVect;
    r: cpFloat;
  end;

  cpSegmentShape = ^cpSegmentShapeStruct;
  cpSegmentShapeStruct = record
    shape: cpShapeStruct;
    a: cpVect;
    b: cpVect;
    n: cpVect;
    ta: cpVect;
    tb: cpVect;
    tn: cpVect;
    r: cpFloat;
    a_tangent: cpVect;
    b_tangent: cpVect;
  end;

  cpSplittingPlane = ^cpSplittingPlaneStruct;
  cpSplittingPlaneStruct = record
    v0: cpVect;
    n: cpVect;
  end;

const
  CP_POLY_SHAPE_INLINE_ALLOC = 6;

{ The untransformed planes are appended at the end of the transformed planes.
  Allocate a small number of splitting planes internally for simple poly. }

type
  cpPolyShape = ^cpPolyShapeStruct;
  cpPolyShapeStruct = record
    shape: cpShapeStruct;
    r: cpFloat;
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
  cpConstraintPreStepImpl = procedure(constraint: Pointer {cpConstraint}; dt: cpFloat); cdecl;
  cpConstraintApplyCachedImpulseImpl = procedure(constraint: Pointer {cpConstraint}; dt_coef: cpFloat); cdecl;
  cpConstraintApplyImpulseImpl = procedure(constraint: Pointer {cpConstraint}; dt: cpFloat); cdecl;
  cpConstraintGetImpulseImpl = function(constraint: Pointer {cpConstraint}): cpFloat; cdecl;

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
    maxForce: cpFloat;
    errorBias: cpFloat;
    maxBias: cpFloat;
    collideBodies: cpBool;
    preSolve: Pointer {cpConstraintPreSolveFunc};
    postSolve: Pointer {cpConstraintPostSolveFunc};
    userData: cpDataPointer;
  end;

  cpPinJoint = ^cpPinJointStruct;
  cpPinJointStruct = record
    constraint: cpConstraintStruct;
    anchorA: cpVect;
    anchorB: cpVect;
    dist: cpFloat;
    r1: cpVect;
    r2: cpVect;
    n: cpVect;
    nMass: cpFloat;
    jnAcc: cpFloat;
    bias: cpFloat;
  end;

  cpSlideJoint = ^cpSlideJointStruct;
  cpSlideJointStruct = record
    constraint: cpConstraintStruct;
    anchorA: cpVect;
    anchorB: cpVect;
    min: cpFloat;
    max: cpFloat;
    r1: cpVect;
    r2: cpVect;
    n: cpVect;
    nMass: cpFloat;
    jnAcc: cpFloat;
    bias: cpFloat;
  end;

  cpPivotJoint = ^cpPivotJointStruct;
  cpPivotJointStruct = record
    constraint: cpConstraintStruct;
    anchorA: cpVect;
    anchorB: cpVect;
    r1: cpVect;
    r2: cpVect;
    k: cpMat2x2;
    jAcc: cpVect;
    bias: cpVect;
  end;

  cpGrooveJoint = ^cpGrooveJointStruct;
  cpGrooveJointStruct = record
    constraint: cpConstraintStruct;
    grv_n: cpVect;
    grv_a: cpVect;
    grv_b: cpVect;
    anchorB: cpVect;
    grv_tn: cpVect;
    clamp: cpFloat;
    r1: cpVect;
    r2: cpVect;
    k: cpMat2x2;
    jAcc: cpVect;
    bias: cpVect;
  end;

  cpDampedSpring = ^cpDampedSpringStruct;
  cpDampedSpringStruct = record
    constraint: cpConstraintStruct;
    anchorA: cpVect;
    anchorB: cpVect;
    restLength: cpFloat;
    stiffness: cpFloat;
    damping: cpFloat;
    springForceFunc: Pointer {cpDampedSpringForceFunc};
    target_vrn: cpFloat;
    v_coef: cpFloat;
    r1: cpVect;
    r2: cpVect;
    nMass: cpFloat;
    n: cpVect;
    jAcc: cpFloat;
  end;

  cpDampedRotarySpring = ^cpDampedRotarySpringStruct;
  cpDampedRotarySpringStruct = record
    constraint: cpConstraintStruct;
    restAngle: cpFloat;
    stiffness: cpFloat;
    damping: cpFloat;
    springTorqueFunc: Pointer {cpDampedRotarySpringTorqueFunc};
    target_wrn: cpFloat;
    w_coef: cpFloat;
    iSum: cpFloat;
    jAcc: cpFloat;
  end;

  cpRotaryLimitJoint = ^cpRotaryLimitJointStruct;
  cpRotaryLimitJointStruct = record
    constraint: cpConstraintStruct;
    min: cpFloat;
    max: cpFloat;
    iSum: cpFloat;
    bias: cpFloat;
    jAcc: cpFloat;
  end;

  cpRatchetJoint = ^cpRatchetJointStruct;
  cpRatchetJointStruct = record
    constraint: cpConstraintStruct;
    angle: cpFloat;
    phase: cpFloat;
    ratchet: cpFloat;
    iSum: cpFloat;
    bias: cpFloat;
    jAcc: cpFloat;
  end;

  cpGearJoint = ^cpGearJointStruct;
  cpGearJointStruct = record
    constraint: cpConstraintStruct;
    phase: cpFloat;
    ratio: cpFloat;
    ratio_inv: cpFloat;
    iSum: cpFloat;
    bias: cpFloat;
    jAcc: cpFloat;
  end;

  cpSimpleMotor = ^cpSimpleMotorStruct;
  cpSimpleMotorStruct = record
    constraint: cpConstraintStruct;
    rate: cpFloat;
    iSum: cpFloat;
    jAcc: cpFloat;
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
function cpMomentForCircle(m: cpFloat; r1: cpFloat; r2: cpFloat; offset: cpVect): cpFloat; cdecl; external;
{ Calculate area of a hollow circle.
  @c r1 and @c r2 are the inner and outer diameters. A solid circle has an inner diameter of 0. }
function cpAreaForCircle(r1: cpFloat; r2: cpFloat): cpFloat; cdecl; external;
{ Calculate the moment of inertia for a line segment.
  Beveling radius is not supported. }
function cpMomentForSegment(m: cpFloat; a: cpVect; b: cpVect; radius: cpFloat): cpFloat; cdecl; external;
{ Calculate the area of a fattened (capsule shaped) line segment. }
function cpAreaForSegment(a: cpVect; b: cpVect; radius: cpFloat): cpFloat; cdecl; external;
{ Calculate the moment of inertia for a solid polygon shape assuming it's center of gravity is at it's centroid. The offset is added to each vertex. }
function cpMomentForPoly(m: cpFloat; count: LongInt; verts: PcpVect; offset: cpVect; radius: cpFloat): cpFloat; cdecl; external;
{ Calculate the signed area of a polygon. A Clockwise winding gives positive area.
  This is probably backwards from what you expect, but matches Chipmunk's the winding for poly shapes. }
function cpAreaForPoly(count: LongInt; verts: PcpVect; radius: cpFloat): cpFloat; cdecl; external;
{ Calculate the natural centroid of a polygon. }
function cpCentroidForPoly(count: LongInt; verts: PcpVect): cpVect; cdecl; external;
{ Calculate the moment of inertia for a solid box. }
function cpMomentForBox(m: cpFloat; width: cpFloat; height: cpFloat): cpFloat; cdecl; external;
{ Calculate the moment of inertia for a solid box. }
function cpMomentForBox2(m: cpFloat; box: cpBB): cpFloat; cdecl; external;
{ Calculate the convex hull of a given set of points. Returns the count of points in the hull.
  @c result must be a pointer to a @c cpVect array with at least @c count elements. If @c verts == @c result, then @c verts will be reduced inplace.
  @c first is an optional pointer to an integer to store where the first vertex in the hull came from (i.e. verts[first] == result[0])
  @c tol is the allowed amount to shrink the hull when simplifying it. A tolerance of 0.0 creates an exact hull. }
function cpConvexHull(count: LongInt; verts: PcpVect; result: PcpVect; first: PLongInt; tol: cpFloat): LongInt; cdecl; external;

// From chipmunk_unsafe.h

{ Set the radius of a circle shape. }
procedure cpCircleShapeSetRadius(shape: cpShape; radius: cpFloat); cdecl; external;
{ Set the offset of a circle shape. }
procedure cpCircleShapeSetOffset(shape: cpShape; offset: cpVect); cdecl; external;
{ Set the endpoints of a segment shape. }
procedure cpSegmentShapeSetEndpoints(shape: cpShape; a: cpVect; b: cpVect); cdecl; external;
{ Set the radius of a segment shape. }
procedure cpSegmentShapeSetRadius(shape: cpShape; radius: cpFloat); cdecl; external;
{ Set the vertexes of a poly shape. }
procedure cpPolyShapeSetVerts(shape: cpShape; count: LongInt; verts: PcpVect; transform: cpTransform); cdecl; external;
procedure cpPolyShapeSetVertsRaw(shape: cpShape; count: LongInt; verts: PcpVect); cdecl; external;
{ Set the radius of a poly shape. }
procedure cpPolyShapeSetRadius(shape: cpShape; radius: cpFloat); cdecl; external;

// From cpArbiter.h

{ The cpArbiter struct tracks pairs of colliding shapes.
  They are also used in conjuction with collision handler callbacks
  allowing you to retrieve information on the collision or change it.
  A unique arbiter value is used for each pair of colliding objects. It persists until the shapes separate. }

const
  CP_MAX_CONTACTS_PER_ARBITER = 2;

{ Get the restitution (elasticity) that will be applied to the pair of colliding objects. }
function cpArbiterGetRestitution(arb: cpArbiter): cpFloat; cdecl; external;
{ Override the restitution (elasticity) that will be applied to the pair of colliding objects. }
procedure cpArbiterSetRestitution(arb: cpArbiter; restitution: cpFloat); cdecl; external;
{ Get the friction coefficient that will be applied to the pair of colliding objects. }
function cpArbiterGetFriction(arb: cpArbiter): cpFloat; cdecl; external;
{ Override the friction coefficient that will be applied to the pair of colliding objects. }
procedure cpArbiterSetFriction(arb: cpArbiter; friction: cpFloat); cdecl; external;
{ Get the relative surface velocity of the two shapes in contact. }
function cpArbiterGetSurfaceVelocity(arb: cpArbiter): cpVect; cdecl; external;
{ Override the relative surface velocity of the two shapes in contact.
  By default this is calculated to be the difference of the two surface velocities clamped to the tangent plane. }
procedure cpArbiterSetSurfaceVelocity(arb: cpArbiter; vr: cpVect); cdecl; external;
{ Get the user data pointer associated with this pair of colliding objects. }
function cpArbiterGetUserData(arb: cpArbiter): cpDataPointer; cdecl; external;
{ Set a user data point associated with this pair of colliding objects.
  If you need to perform any cleanup for this pointer, you must do it yourself, in the separate callback for instance. }
procedure cpArbiterSetUserData(arb: cpArbiter; userData: cpDataPointer); cdecl; external;
{ Calculate the total impulse including the friction that was applied by this arbiter.
 This function should only be called from a post-solve, post-step or cpBodyEachArbiter callback. }
function cpArbiterTotalImpulse(arb: cpArbiter): cpVect; cdecl; external;
{ Calculate the amount of energy lost in a collision including static, but not dynamic friction.
  This function should only be called from a post-solve, post-step or cpBodyEachArbiter callback. }
function cpArbiterTotalKE(arb: cpArbiter): cpFloat; cdecl; external;
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
      normal: cpVect;
      points: array[0..(CP_MAX_CONTACTS_PER_ARBITER) - 1] of record
        pointA: cpVect;
        pointB: cpVect;
        distance: cpFloat;
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
function cpArbiterGetNormal(arb: cpArbiter): cpVect; cdecl; external;
{ Get the position of the @c ith contact point on the surface of the first shape. }
function cpArbiterGetPointA(arb: cpArbiter; i: LongInt): cpVect; cdecl; external;
{ Get the position of the @c ith contact point on the surface of the second shape. }
function cpArbiterGetPointB(arb: cpArbiter; i: LongInt): cpVect; cdecl; external;
{ Get the depth of the @c ith contact point. }
function cpArbiterGetDepth(arb: cpArbiter; i: LongInt): cpFloat; cdecl; external;
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
function cpBodyInit(body: cpBody; mass: cpFloat; moment: cpFloat): cpBody; cdecl; external;
{ Allocate and initialize a cpBody. }
function cpBodyNew(mass: cpFloat; moment: cpFloat): cpBody; cdecl; external;
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
function cpBodyGetMass(body: cpBody): cpFloat; cdecl; external;
{ Set the mass of the body. }
procedure cpBodySetMass(body: cpBody; m: cpFloat); cdecl; external;
{ Get the moment of inertia of the body. }
function cpBodyGetMoment(body: cpBody): cpFloat; cdecl; external;
{ Set the moment of inertia of the body. }
procedure cpBodySetMoment(body: cpBody; i: cpFloat); cdecl; external;
{ Set the position of a body. }
function cpBodyGetPosition(body: cpBody): cpVect; cdecl; external;
{ Set the position of the body. }
procedure cpBodySetPosition(body: cpBody; pos: cpVect); cdecl; external;
{ Get the offset of the center of gravity in body local coordinates. }
function cpBodyGetCenterOfGravity(body: cpBody): cpVect; cdecl; external;
{ Set the offset of the center of gravity in body local coordinates. }
procedure cpBodySetCenterOfGravity(body: cpBody; cog: cpVect); cdecl; external;
{ Get the velocity of the body. }
function cpBodyGetVelocity(body: cpBody): cpVect; cdecl; external;
{ Set the velocity of the body. }
procedure cpBodySetVelocity(body: cpBody; velocity: cpVect); cdecl; external;
{ Get the force applied to the body for the next time step. }
function cpBodyGetForce(body: cpBody): cpVect; cdecl; external;
{ Set the force applied to the body for the next time step. }
procedure cpBodySetForce(body: cpBody; force: cpVect); cdecl; external;
{ Get the angle of the body. }
function cpBodyGetAngle(body: cpBody): cpFloat; cdecl; external;
{ Set the angle of a body. }
procedure cpBodySetAngle(body: cpBody; a: cpFloat); cdecl; external;
{ Get the angular velocity of the body. }
function cpBodyGetAngularVelocity(body: cpBody): cpFloat; cdecl; external;
{ Set the angular velocity of the body. }
procedure cpBodySetAngularVelocity(body: cpBody; angularVelocity: cpFloat); cdecl; external;
{ Get the torque applied to the body for the next time step. }
function cpBodyGetTorque(body: cpBody): cpFloat; cdecl; external;
{ Set the torque applied to the body for the next time step. }
procedure cpBodySetTorque(body: cpBody; torque: cpFloat); cdecl; external;
{ Get the rotation vector of the body. (The x basis vector of it's transform.) }
function cpBodyGetRotation(body: cpBody): cpVect; cdecl; external;
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
procedure cpBodyUpdateVelocity(body: cpBody; gravity: cpVect; damping: cpFloat; dt: cpFloat); cdecl; external;
{ Default position integration function. }
procedure cpBodyUpdatePosition(body: cpBody; dt: cpFloat); cdecl; external;
{ Convert body relative/local coordinates to absolute/world coordinates. }
function cpBodyLocalToWorld(body: cpBody; point: cpVect): cpVect; cdecl; external;
{ Convert body absolute/world coordinates to  relative/local coordinates. }
function cpBodyWorldToLocal(body: cpBody; point: cpVect): cpVect; cdecl; external;
{ Apply a force to a body. Both the force and point are expressed in world coordinates. }
procedure cpBodyApplyForceAtWorldPoint(body: cpBody; force: cpVect; point: cpVect); cdecl; external;
{ Apply a force to a body. Both the force and point are expressed in body local coordinates. }
procedure cpBodyApplyForceAtLocalPoint(body: cpBody; force: cpVect; point: cpVect); cdecl; external;
{ Apply an impulse to a body. Both the impulse and point are expressed in world coordinates. }
procedure cpBodyApplyImpulseAtWorldPoint(body: cpBody; impulse: cpVect; point: cpVect); cdecl; external;
{ Apply an impulse to a body. Both the impulse and point are expressed in body local coordinates. }
procedure cpBodyApplyImpulseAtLocalPoint(body: cpBody; impulse: cpVect; point: cpVect); cdecl; external;
{ Get the velocity on a body (in world units) at a point on the body in world coordinates. }
function cpBodyGetVelocityAtWorldPoint(body: cpBody; point: cpVect): cpVect; cdecl; external;
{ Get the velocity on a body (in world units) at a point on the body in local coordinates. }
function cpBodyGetVelocityAtLocalPoint(body: cpBody; point: cpVect): cpVect; cdecl; external;
{ Get the amount of kinetic energy contained by the body. }
function cpBodyKineticEnergy(body: cpBody): cpFloat; cdecl; external;

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
function cpConstraintGetMaxForce(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the maximum force that this constraint is allowed to use. (defaults to INFINITY) }
procedure cpConstraintSetMaxForce(constraint: cpConstraint; maxForce: cpFloat); cdecl; external;
{ Get rate at which joint error is corrected. }
function cpConstraintGetErrorBias(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set rate at which joint error is corrected.
  Defaults to pow(1.0 - 0.1, 60.0) meaning that it will
  correct 10% of the error every 1/60th of a second. }
procedure cpConstraintSetErrorBias(constraint: cpConstraint; errorBias: cpFloat); cdecl; external;
{ Get the maximum rate at which joint error is corrected. }
function cpConstraintGetMaxBias(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the maximum rate at which joint error is corrected. (defaults to INFINITY) }
procedure cpConstraintSetMaxBias(constraint: cpConstraint; maxBias: cpFloat); cdecl; external;
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
function cpConstraintGetImpulse(constraint: cpConstraint): cpFloat; cdecl; external;

// From cpDampedRotarySpring.h

{ Check if a constraint is a damped rotary spring. }
function cpConstraintIsDampedRotarySpring(constraint: cpConstraint): cpBool; cdecl; external;

type
  { Function type used for damped rotary spring force callbacks. }
  cpDampedRotarySpringTorqueFunc = function(spring: cpConstraint; relativeAngle: cpFloat): cpFloat; cdecl;

{ Allocate a damped rotary spring. }
function cpDampedRotarySpringAlloc: cpDampedRotarySpring; cdecl; external;
{ Initialize a damped rotary spring. }
function cpDampedRotarySpringInit(joint: cpDampedRotarySpring; a: cpBody; b: cpBody; restAngle: cpFloat; stiffness: cpFloat;
  damping: cpFloat): cpDampedRotarySpring; cdecl; external;
{ Allocate and initialize a damped rotary spring. }
function cpDampedRotarySpringNew(a: cpBody; b: cpBody; restAngle: cpFloat; stiffness: cpFloat; damping: cpFloat): cpConstraint; cdecl; external;
{ Get the rest length of the spring. }
function cpDampedRotarySpringGetRestAngle(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the rest length of the spring. }
procedure cpDampedRotarySpringSetRestAngle(constraint: cpConstraint; restAngle: cpFloat); cdecl; external;
{ Get the stiffness of the spring in force/distance. }
function cpDampedRotarySpringGetStiffness(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the stiffness of the spring in force/distance. }
procedure cpDampedRotarySpringSetStiffness(constraint: cpConstraint; stiffness: cpFloat); cdecl; external;
{ Get the damping of the spring. }
function cpDampedRotarySpringGetDamping(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the damping of the spring. }
procedure cpDampedRotarySpringSetDamping(constraint: cpConstraint; damping: cpFloat); cdecl; external;
{ Get the damping of the spring. }
function cpDampedRotarySpringGetSpringTorqueFunc(constraint: cpConstraint): cpDampedRotarySpringTorqueFunc; cdecl; external;
{ Set the damping of the spring. }
procedure cpDampedRotarySpringSetSpringTorqueFunc(constraint: cpConstraint; springTorqueFunc: cpDampedRotarySpringTorqueFunc); cdecl; external;

// From cpDampedSpring.h

{ Check if a constraint is a damped spring. }
function cpConstraintIsDampedSpring(constraint: cpConstraint): cpBool; cdecl; external;

type
  { Function type used for damped spring force callbacks. }
  cpDampedSpringForceFunc = function(spring: cpConstraint; dist: cpFloat): cpFloat; cdecl;

{ Allocate a damped spring. }
function cpDampedSpringAlloc: cpDampedSpring; cdecl; external;
{ Initialize a damped spring. }
function cpDampedSpringInit(joint: cpDampedSpring; a: cpBody; b: cpBody; anchorA: cpVect; anchorB: cpVect;
  restLength: cpFloat; stiffness: cpFloat; damping: cpFloat): cpDampedSpring; cdecl; external;
{ Allocate and initialize a damped spring. }
function cpDampedSpringNew(a: cpBody; b: cpBody; anchorA: cpVect; anchorB: cpVect; restLength: cpFloat;
  stiffness: cpFloat; damping: cpFloat): cpConstraint; cdecl; external;
{ Get the location of the first anchor relative to the first body. }
function cpDampedSpringGetAnchorA(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the location of the first anchor relative to the first body. }
procedure cpDampedSpringSetAnchorA(constraint: cpConstraint; anchorA: cpVect); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpDampedSpringGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpDampedSpringSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external;
{ Get the rest length of the spring. }
function cpDampedSpringGetRestLength(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the rest length of the spring. }
procedure cpDampedSpringSetRestLength(constraint: cpConstraint; restLength: cpFloat); cdecl; external;
{ Get the stiffness of the spring in force/distance. }
function cpDampedSpringGetStiffness(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the stiffness of the spring in force/distance. }
procedure cpDampedSpringSetStiffness(constraint: cpConstraint; stiffness: cpFloat); cdecl; external;
{ Get the damping of the spring. }
function cpDampedSpringGetDamping(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the damping of the spring. }
procedure cpDampedSpringSetDamping(constraint: cpConstraint; damping: cpFloat); cdecl; external;
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
function cpGearJointInit(joint: cpGearJoint; a: cpBody; b: cpBody; phase: cpFloat; ratio: cpFloat): cpGearJoint; cdecl; external;
{ Allocate and initialize a gear joint. }
function cpGearJointNew(a: cpBody; b: cpBody; phase: cpFloat; ratio: cpFloat): cpConstraint; cdecl; external;
{ Get the phase offset of the gears. }
function cpGearJointGetPhase(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the phase offset of the gears. }
procedure cpGearJointSetPhase(constraint: cpConstraint; phase: cpFloat); cdecl; external;
{ Get the angular distance of each ratchet. }
function cpGearJointGetRatio(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the ratio of a gear joint. }
procedure cpGearJointSetRatio(constraint: cpConstraint; ratio: cpFloat); cdecl; external;

// From cpGrooveJoint.h

{ Check if a constraint is a groove joint. }
function cpConstraintIsGrooveJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a groove joint. }
function cpGrooveJointAlloc: cpGrooveJoint; cdecl; external;
{ Initialize a groove joint. }
function cpGrooveJointInit(joint: cpGrooveJoint; a: cpBody; b: cpBody; groove_a: cpVect; groove_b: cpVect;
  anchorB: cpVect): cpGrooveJoint; cdecl; external;
{ Allocate and initialize a groove joint. }
function cpGrooveJointNew(a: cpBody; b: cpBody; groove_a: cpVect; groove_b: cpVect; anchorB: cpVect): cpConstraint; cdecl; external;
{ Get the first endpoint of the groove relative to the first body. }
function cpGrooveJointGetGrooveA(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the first endpoint of the groove relative to the first body. }
procedure cpGrooveJointSetGrooveA(constraint: cpConstraint; grooveA: cpVect); cdecl; external;
{ Get the first endpoint of the groove relative to the first body. }
function cpGrooveJointGetGrooveB(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the first endpoint of the groove relative to the first body. }
procedure cpGrooveJointSetGrooveB(constraint: cpConstraint; grooveB: cpVect); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpGrooveJointGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpGrooveJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external;

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
procedure cpHastySpaceStep(space: cpSpace; dt: cpFloat); cdecl; external;

// From cpMarch.h

type
  { Function type used as a callback from the marching squares algorithm to sample an image function.
   It passes you the point to sample and your context pointer, and you return the density. }
  cpMarchSampleFunc = function(point: cpVect; data: Pointer): cpFloat; cdecl;
  { Function type used as a callback from the marching squares algorithm to output a line segment.
   It passes you the two endpoints and your context pointer. }
  cpMarchSegmentFunc = procedure(v0: cpVect; v1: cpVect; data: Pointer); cdecl;

{ Trace an anti-aliased contour of an image along a particular threshold.
  The given number of samples will be taken and spread across the bounding box area using the sampling function and context.
  The segment function will be called for each segment detected that lies along the density contour for @c threshold. }
procedure cpMarchSoft(bb: cpBB; x_samples: LongWord; y_samples: LongWord; threshold: cpFloat; segment: cpMarchSegmentFunc;
  segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer); cdecl; external;
{ Trace an aliased curve of an image along a particular threshold.
  The given number of samples will be taken and spread across the bounding box area using the sampling function and context.
  The segment function will be called for each segment detected that lies along the density contour for @c threshold. }
procedure cpMarchHard(bb: cpBB; x_samples: LongWord; y_samples: LongWord; threshold: cpFloat; segment: cpMarchSegmentFunc;
  segment_data: Pointer; sample: cpMarchSampleFunc; sample_data: Pointer); cdecl; external;

// From cpPinJoint.h

{ Check if a constraint is a pin joint. }
function cpConstraintIsPinJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a pin joint. }
function cpPinJointAlloc: cpPinJoint; cdecl; external;
{ Initialize a pin joint. }
function cpPinJointInit(joint: cpPinJoint; a: cpBody; b: cpBody; anchorA: cpVect; anchorB: cpVect): cpPinJoint; cdecl; external;
{ Allocate and initialize a pin joint. }
function cpPinJointNew(a: cpBody; b: cpBody; anchorA: cpVect; anchorB: cpVect): cpConstraint; cdecl; external;
{ Get the location of the first anchor relative to the first body. }
function cpPinJointGetAnchorA(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the location of the first anchor relative to the first body. }
procedure cpPinJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpPinJointGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpPinJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external;
{ Get the distance the joint will maintain between the two anchors. }
function cpPinJointGetDist(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the distance the joint will maintain between the two anchors. }
procedure cpPinJointSetDist(constraint: cpConstraint; dist: cpFloat); cdecl; external;

// From cpPivotJoint.h

{ Check if a constraint is a pivot joint. }
function cpConstraintIsPivotJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a pivot joint }
function cpPivotJointAlloc: cpPivotJoint; cdecl; external;
{ Initialize a pivot joint. }
function cpPivotJointInit(joint: cpPivotJoint; a: cpBody; b: cpBody; anchorA: cpVect; anchorB: cpVect): cpPivotJoint; cdecl; external;
{ Allocate and initialize a pivot joint. }
function cpPivotJointNew(a: cpBody; b: cpBody; pivot: cpVect): cpConstraint; cdecl; external;
{ Allocate and initialize a pivot joint with specific anchors. }
function cpPivotJointNew2(a: cpBody; b: cpBody; anchorA: cpVect; anchorB: cpVect): cpConstraint; cdecl; external;
{ Get the location of the first anchor relative to the first body. }
function cpPivotJointGetAnchorA(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the location of the first anchor relative to the first body. }
procedure cpPivotJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpPivotJointGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpPivotJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external;

// From cpPolyline.h

{ Polylines are just arrays of vertexes.
  They are looped if the first vertex is equal to the last.
 cpPolyline structs are intended to be passed by value and destroyed when you are done with them. }

type
  cpPolyline = ^cpPolylineStruct;
  cpPolylineStruct = record
    count: LongInt;
    capacity: LongInt;
    verts: PcpVect;
  end;

{ Destroy and free a polyline instance. }
procedure cpPolylineFree(line: cpPolyline); cdecl; external;
{ Returns true if the first vertex is equal to the last. }
function cpPolylineIsClosed(line: cpPolyline): cpBool; cdecl; external;
{ Returns a copy of a polyline simplified by using the Douglas-Peucker algorithm.
  This works very well on smooth or gently curved shapes, but not well on straight edged or angular shapes. }
function cpPolylineSimplifyCurves(line: cpPolyline; tol: cpFloat): cpPolyline; cdecl; external;
{ Returns a copy of a polyline simplified by discarding "flat" vertexes.
  This works well on straight edged or angular shapes, not as well on smooth shapes.}
function cpPolylineSimplifyVertexes(line: cpPolyline; tol: cpFloat): cpPolyline; cdecl; external;
{ Get the convex hull of a polyline as a looped polyline. }
function cpPolylineToConvexHull(line: cpPolyline; tol: cpFloat): cpPolyline; cdecl; external;
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
procedure cpPolylineSetCollectSegment(v0: cpVect; v1: cpVect; lines: cpPolylineSet); cdecl; external;
{ Get an approximate convex decomposition from a polyline.
  Returns a cpPolylineSet of convex hulls that match the original shape to within 'tol'.
  NOTE: If the input is a self intersecting polygon, the output might end up overly simplified. }
function cpPolylineConvexDecomposition(line: cpPolyline; tol: cpFloat): cpPolylineSet; cdecl; external;

// From cpPolyShape.h

{ Allocate a polygon shape. }
function cpPolyShapeAlloc: cpPolyShape; cdecl; external;
{ Initialize a polygon shape with rounded corners.
  A convex hull will be created from the vertexes. }
function cpPolyShapeInit(poly: cpPolyShape; body: cpBody; count: LongInt; verts: PcpVect; transform: cpTransform;
  radius: cpFloat): cpPolyShape; cdecl; external;
{ Initialize a polygon shape with rounded corners.
 The vertexes must be convex with a counter-clockwise winding. }
function cpPolyShapeInitRaw(poly: cpPolyShape; body: cpBody; count: LongInt; verts: PcpVect; radius: cpFloat): cpPolyShape; cdecl; external;
{ Allocate and initialize a polygon shape with rounded corners.
  A convex hull will be created from the vertexes. }
function cpPolyShapeNew(body: cpBody; count: LongInt; verts: PcpVect; transform: cpTransform; radius: cpFloat): cpShape; cdecl; external;
{ Allocate and initialize a polygon shape with rounded corners.
  The vertexes must be convex with a counter-clockwise winding. }
function cpPolyShapeNewRaw(body: cpBody; count: LongInt; verts: PcpVect; radius: cpFloat): cpShape; cdecl; external;
{ Initialize a box shaped polygon shape with rounded corners. }
function cpBoxShapeInit(poly: cpPolyShape; body: cpBody; width: cpFloat; height: cpFloat; radius: cpFloat): cpPolyShape; cdecl; external;
{ Initialize an offset box shaped polygon shape with rounded corners. }
function cpBoxShapeInit2(poly: cpPolyShape; body: cpBody; box: cpBB; radius: cpFloat): cpPolyShape; cdecl; external;
{ Allocate and initialize a box shaped polygon shape. }
function cpBoxShapeNew(body: cpBody; width: cpFloat; height: cpFloat; radius: cpFloat): cpShape; cdecl; external;
{ Allocate and initialize an offset box shaped polygon shape. }
function cpBoxShapeNew2(body: cpBody; box: cpBB; radius: cpFloat): cpShape; cdecl; external;
{ Get the number of verts in a polygon shape. }
function cpPolyShapeGetCount(shape: cpShape): LongInt; cdecl; external;
{ Get the @c ith vertex of a polygon shape. }
function cpPolyShapeGetVert(shape: cpShape; index: LongInt): cpVect; cdecl; external;
{ Get the radius of a polygon shape. }
function cpPolyShapeGetRadius(shape: cpShape): cpFloat; cdecl; external;

// From cpRatchetJoint.h

{ Check if a constraint is a ratchet joint. }
function cpConstraintIsRatchetJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a ratchet joint. }
function cpRatchetJointAlloc: cpRatchetJoint; cdecl; external;
{ Initialize a ratched joint. }
function cpRatchetJointInit(joint: cpRatchetJoint; a: cpBody; b: cpBody; phase: cpFloat; ratchet: cpFloat): cpRatchetJoint; cdecl; external;
{ Allocate and initialize a ratchet joint. }
function cpRatchetJointNew(a: cpBody; b: cpBody; phase: cpFloat; ratchet: cpFloat): cpConstraint; cdecl; external;
{ Get the angle of the current ratchet tooth. }
function cpRatchetJointGetAngle(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the angle of the current ratchet tooth. }
procedure cpRatchetJointSetAngle(constraint: cpConstraint; angle: cpFloat); cdecl; external;
{ Get the phase offset of the ratchet. }
function cpRatchetJointGetPhase(constraint: cpConstraint): cpFloat; cdecl; external;
{ Get the phase offset of the ratchet. }
procedure cpRatchetJointSetPhase(constraint: cpConstraint; phase: cpFloat); cdecl; external;
{ Get the angular distance of each ratchet. }
function cpRatchetJointGetRatchet(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the angular distance of each ratchet. }
procedure cpRatchetJointSetRatchet(constraint: cpConstraint; ratchet: cpFloat); cdecl; external;

// From cpRotaryLimitJoint.h

{ Check if a constraint is a rotary limit joint. }
function cpConstraintIsRotaryLimitJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a damped rotary limit joint. }
function cpRotaryLimitJointAlloc: cpRotaryLimitJoint; cdecl; external;
{ Initialize a damped rotary limit joint. }
function cpRotaryLimitJointInit(joint: cpRotaryLimitJoint; a: cpBody; b: cpBody; min: cpFloat; max: cpFloat): cpRotaryLimitJoint; cdecl; external;
{ Allocate and initialize a damped rotary limit joint. }
function cpRotaryLimitJointNew(a: cpBody; b: cpBody; min: cpFloat; max: cpFloat): cpConstraint; cdecl; external;
{ Get the minimum distance the joint will maintain between the two anchors. }
function cpRotaryLimitJointGetMin(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the minimum distance the joint will maintain between the two anchors. }
procedure cpRotaryLimitJointSetMin(constraint: cpConstraint; min: cpFloat); cdecl; external;
{ Get the maximum distance the joint will maintain between the two anchors. }
function cpRotaryLimitJointGetMax(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the maximum distance the joint will maintain between the two anchors. }
procedure cpRotaryLimitJointSetMax(constraint: cpConstraint; max: cpFloat); cdecl; external;

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
    point: cpVect;
    distance: cpFloat;
    gradient: cpVect;
  end;

{ Segment query info struct.
  The shape that was hit, or NULL if no collision occured.
  The point of impact.
  The normal of the surface hit.
  The normalized distance along the query segment in the range [0, 1]. }

  cpSegmentQueryInfo = ^cpSegmentQueryInfoStruct;
  cpSegmentQueryInfoStruct = record
    shape: cpShape;
    point: cpVect;
    normal: cpVect;
    alpha: cpFloat;
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
function cpShapePointQuery(shape: cpShape; p: cpVect; out info: cpPointQueryInfoStruct): cpFloat; cdecl; external;
{ Perform a segment query against a shape. @c info must be a pointer to a valid cpSegmentQueryInfo structure. }
function cpShapeSegmentQuery(shape: cpShape; a, b: cpVect; radius: cpFloat; out info: cpSegmentQueryInfoStruct): cpBool; cdecl; external;
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
function cpShapeGetMass(shape: cpShape): cpFloat; cdecl; external;
{ Set the mass of this shape to have Chipmunk calculate mass properties for you. }
procedure cpShapeSetMass(shape: cpShape; mass: cpFloat); cdecl; external;
{ Get the density of the shape if you are having Chipmunk calculate mass properties for you. }
function cpShapeGetDensity(shape: cpShape): cpFloat; cdecl; external;
{ Set the density  of this shape to have Chipmunk calculate mass properties for you. }
procedure cpShapeSetDensity(shape: cpShape; density: cpFloat); cdecl; external;
{ Get the calculated moment of inertia for this shape. }
function cpShapeGetMoment(shape: cpShape): cpFloat; cdecl; external;
{ Get the calculated area of this shape. }
function cpShapeGetArea(shape: cpShape): cpFloat; cdecl; external;
{ Get the centroid of this shape. }
function cpShapeGetCenterOfGravity(shape: cpShape): cpVect; cdecl; external;
{ Get the bounding box that contains the shape given it's current position and angle. }
function cpShapeGetBB(shape: cpShape): cpBB; cdecl; external;
{ Get if the shape is set to be a sensor or not. }
function cpShapeGetSensor(shape: cpShape): cpBool; cdecl; external;
{ Set if the shape is a sensor or not. }
procedure cpShapeSetSensor(shape: cpShape; sensor: cpBool); cdecl; external;
{ Get the elasticity of this shape. }
function cpShapeGetElasticity(shape: cpShape): cpFloat; cdecl; external;
{ Set the elasticity of this shape. }
procedure cpShapeSetElasticity(shape: cpShape; elasticity: cpFloat); cdecl; external;
{ Get the friction of this shape. }
function cpShapeGetFriction(shape: cpShape): cpFloat; cdecl; external;
{ Set the friction of this shape. }
procedure cpShapeSetFriction(shape: cpShape; friction: cpFloat); cdecl; external;
{ Get the surface velocity of this shape. }
function cpShapeGetSurfaceVelocity(shape: cpShape): cpVect; cdecl; external;
{ Set the surface velocity of this shape. }
procedure cpShapeSetSurfaceVelocity(shape: cpShape; surfaceVelocity: cpVect); cdecl; external;
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
function cpCircleShapeInit(circle: cpCircleShape; body: cpBody; radius: cpFloat; offset: cpVect): cpCircleShape; cdecl; external;
{ Allocate and initialize a circle shape. }
function cpCircleShapeNew(body: cpBody; radius: cpFloat; offset: cpVect): cpShape; cdecl; external;
{ Get the offset of a circle shape. }
function cpCircleShapeGetOffset(shape: cpShape): cpVect; cdecl; external;
{ Get the radius of a circle shape. }
function cpCircleShapeGetRadius(shape: cpShape): cpFloat; cdecl; external;

{ Allocate a segment shape. }
function cpSegmentShapeAlloc: cpSegmentShape; cdecl; external;
{ Initialize a segment shape. }
function cpSegmentShapeInit(seg: cpSegmentShape; body: cpBody; a: cpVect; b: cpVect; radius: cpFloat): cpSegmentShape; cdecl; external;
{ Allocate and initialize a segment shape. }
function cpSegmentShapeNew(body: cpBody; a: cpVect; b: cpVect; radius: cpFloat): cpShape; cdecl; external;
{ Let Chipmunk know about the geometry of adjacent segments to avoid colliding with endcaps. }
procedure cpSegmentShapeSetNeighbors(shape: cpShape; prev: cpVect; next: cpVect); cdecl; external;
{ Get the first endpoint of a segment shape. }
function cpSegmentShapeGetA(shape: cpShape): cpVect; cdecl; external;
{ Get the second endpoint of a segment shape. }
function cpSegmentShapeGetB(shape: cpShape): cpVect; cdecl; external;
{ Get the normal of a segment shape. }
function cpSegmentShapeGetNormal(shape: cpShape): cpVect; cdecl; external;
{ Get the first endpoint of a segment shape. }
function cpSegmentShapeGetRadius(shape: cpShape): cpFloat; cdecl; external;

// From cpSimpleMotor.h

{ Check if a constraint is a simple motor. }
function cpConstraintIsSimpleMotor(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a simple motor. }
function cpSimpleMotorAlloc: cpSimpleMotor; cdecl; external;
{ initialize a simple motor. }
function cpSimpleMotorInit(joint: cpSimpleMotor; a: cpBody; b: cpBody; rate: cpFloat): cpSimpleMotor; cdecl; external;
{ Allocate and initialize a simple motor. }
function cpSimpleMotorNew(a: cpBody; b: cpBody; rate: cpFloat): cpConstraint; cdecl; external;
{ Get the rate of the motor. }
function cpSimpleMotorGetRate(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the rate of the motor. }
procedure cpSimpleMotorSetRate(constraint: cpConstraint; rate: cpFloat); cdecl; external;

// From cpSlideJoint.h

{ Check if a constraint is a slide joint. }
function cpConstraintIsSlideJoint(constraint: cpConstraint): cpBool; cdecl; external;
{ Allocate a slide joint. }
function cpSlideJointAlloc: cpSlideJoint; cdecl; external;
{ Initialize a slide joint. }
function cpSlideJointInit(joint: cpSlideJoint; a: cpBody; b: cpBody; anchorA: cpVect; anchorB: cpVect;
  min: cpFloat; max: cpFloat): cpSlideJoint; cdecl; external;
{ Allocate and initialize a slide joint. }
function cpSlideJointNew(a: cpBody; b: cpBody; anchorA: cpVect; anchorB: cpVect; min: cpFloat;
  max: cpFloat): cpConstraint; cdecl; external;
{ Get the location of the first anchor relative to the first body. }
function cpSlideJointGetAnchorA(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the location of the first anchor relative to the first body. }
procedure cpSlideJointSetAnchorA(constraint: cpConstraint; anchorA: cpVect); cdecl; external;
{ Get the location of the second anchor relative to the second body. }
function cpSlideJointGetAnchorB(constraint: cpConstraint): cpVect; cdecl; external;
{ Set the location of the second anchor relative to the second body. }
procedure cpSlideJointSetAnchorB(constraint: cpConstraint; anchorB: cpVect); cdecl; external;
{ Get the minimum distance the joint will maintain between the two anchors. }
function cpSlideJointGetMin(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the minimum distance the joint will maintain between the two anchors. }
procedure cpSlideJointSetMin(constraint: cpConstraint; min: cpFloat); cdecl; external;
{ Get the maximum distance the joint will maintain between the two anchors. }
function cpSlideJointGetMax(constraint: cpConstraint): cpFloat; cdecl; external;
{ Set the maximum distance the joint will maintain between the two anchors. }
procedure cpSlideJointSetMax(constraint: cpConstraint; max: cpFloat); cdecl; external;

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
function cpSpaceGetGravity(space: cpSpace): cpVect; cdecl; external;
procedure cpSpaceSetGravity(space: cpSpace; gravity: cpVect); cdecl; external;
{ Damping rate expressed as the fraction of velocity bodies retain each second.
  A value of 0.9 would mean that each body's velocity will drop 10% per second.
  The default value is 1.0, meaning no damping is applied.
  @note This damping value is different than those of cpDampedSpring and cpDampedRotarySpring. }
function cpSpaceGetDamping(space: cpSpace): cpFloat; cdecl; external;
procedure cpSpaceSetDamping(space: cpSpace; damping: cpFloat); cdecl; external;
{ Speed threshold for a body to be considered idle.
  The default value of 0 means to let the space guess a good threshold based on gravity. }
function cpSpaceGetIdleSpeedThreshold(space: cpSpace): cpFloat; cdecl; external;
procedure cpSpaceSetIdleSpeedThreshold(space: cpSpace; idleSpeedThreshold: cpFloat); cdecl; external;
{ Time a group of bodies must remain idle in order to fall asleep.
  Enabling sleeping also implicitly enables the the contact graph.
  The default value of INFINITY disables the sleeping algorithm. }
function cpSpaceGetSleepTimeThreshold(space: cpSpace): cpFloat; cdecl; external;
procedure cpSpaceSetSleepTimeThreshold(space: cpSpace; sleepTimeThreshold: cpFloat); cdecl; external;
{ Amount of encouraged penetration between colliding shapes.
  Used to reduce oscillating contacts and keep the collision cache warm.
  Defaults to 0.1. If you have poor simulation quality,
  increase this number as much as possible without allowing visible amounts of overlap. }
function cpSpaceGetCollisionSlop(space: cpSpace): cpFloat; cdecl; external;
procedure cpSpaceSetCollisionSlop(space: cpSpace; collisionSlop: cpFloat); cdecl; external;
{ Determines how fast overlapping shapes are pushed apart.
  Expressed as a fraction of the error remaining after each second.
  Defaults to pow(1.0 - 0.1, 60.0) meaning that Chipmunk fixes 10% of overlap each frame at 60Hz. }
function cpSpaceGetCollisionBias(space: cpSpace): cpFloat; cdecl; external;
procedure cpSpaceSetCollisionBias(space: cpSpace; collisionBias: cpFloat); cdecl; external;
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
function cpSpaceGetCurrentTimeStep(space: cpSpace): cpFloat; cdecl; external;
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
  cpSpacePointQueryFunc = procedure(shape: cpShape; point: cpVect; distance: cpFloat; gradient: cpVect; data: Pointer); cdecl;

{ Query the space at a point and call @c func for each shape found. }
procedure cpSpacePointQuery(space: cpSpace; point: cpVect; maxDistance: cpFloat; filter: cpShapeFilterStruct; func: cpSpacePointQueryFunc;
  data: Pointer); cdecl; external;
{ Query the space at a point and return the nearest shape found. Returns NULL if no shapes were found. }
function cpSpacePointQueryNearest(space: cpSpace; point: cpVect; maxDistance: cpFloat; filter: cpShapeFilterStruct; outInfo: cpPointQueryInfo): cpShape; cdecl; external;

type
  { Segment query callback function type. }
  cpSpaceSegmentQueryFunc = procedure(shape: cpShape; point: cpVect; normal: cpVect; alpha: cpFloat; data: Pointer); cdecl;

{ Perform a directed line segment query (like a raycast) against the space calling @c func for each shape intersected. }
procedure cpSpaceSegmentQuery(space: cpSpace; start: cpVect; finish: cpVect; radius: cpFloat; filter: cpShapeFilterStruct;
  func: cpSpaceSegmentQueryFunc; data: Pointer); cdecl; external;
{ Perform a directed line segment query (like a raycast) against the space and return the first shape hit. Returns NULL if no shapes were hit. }
function cpSpaceSegmentQueryFirst(space: cpSpace; start: cpVect; finish: cpVect; radius: cpFloat; filter: cpShapeFilterStruct;
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
procedure cpSpaceUseSpatialHash(space: cpSpace; dim: cpFloat; count: LongInt); cdecl; external;
{ Step the space forward in time by @c dt. }
procedure cpSpaceStep(space: cpSpace; dt: cpFloat); cdecl; external;

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
  cpSpaceDebugDrawCircleImpl = procedure(pos: cpVect; angle: cpFloat; radius: cpFloat; outlineColor: cpSpaceDebugColorStruct; fillColor: cpSpaceDebugColorStruct;
    data: cpDataPointer); cdecl;
  { Callback type for a function that draws a line segment. }
  cpSpaceDebugDrawSegmentImpl = procedure(a: cpVect; b: cpVect; color: cpSpaceDebugColorStruct; data: cpDataPointer); cdecl;
  { Callback type for a function that draws a thick line segment. }
  cpSpaceDebugDrawFatSegmentImpl = procedure(a: cpVect; b: cpVect; radius: cpFloat; outlineColor: cpSpaceDebugColorStruct; fillColor: cpSpaceDebugColorStruct;
    data: cpDataPointer); cdecl;
  { Callback type for a function that draws a convex polygon. }
  cpSpaceDebugDrawPolygonImpl = procedure(count: LongInt; verts: PcpVect; radius: cpFloat; outlineColor: cpSpaceDebugColorStruct; fillColor: cpSpaceDebugColorStruct;
    data: cpDataPointer); cdecl;
  { Callback type for a function that draws a dot. }
  cpSpaceDebugDrawDotImpl = procedure(size: cpFloat; pos: cpVect; color: cpSpaceDebugColorStruct; data: cpDataPointer); cdecl;
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
procedure cpArbiterPreStep(arb: cpArbiter; dt: cpFloat; bias: cpFloat; slop: cpFloat); cdecl; external;
procedure cpArbiterApplyCachedImpulse(arb: cpArbiter; dt_coef: cpFloat); cdecl; external;
procedure cpArbiterApplyImpulse(arb: cpArbiter); cdecl; external;
function cpShapeInit(shape: cpShape; klass: cpShapeClass; body: cpBody; massInfo: cpShapeMassInfoStruct): cpShape; cdecl; external;
function cpCollide(a: cpShape; b: cpShape; id: cpCollisionID; contacts: cpContact): cpCollisionInfoStruct; cdecl; external;
procedure cpLoopIndexes(verts: PcpVect; count: LongInt; out start, finish: LongInt); cdecl; external;
procedure cpConstraintInit(constraint: cpConstraint; klass: cpConstraintClass; a: cpBody; b: cpBody); cdecl; external;
procedure cpSpaceSetStaticBody(space: cpSpace; body: cpBody); cdecl; external;
procedure cpSpaceProcessComponents(space: cpSpace; dt: cpFloat); cdecl; external;
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
  {$linklib chipmunk-linux}
{$endif}

{ cpVect }

procedure SinCos(x: cpFloat; out s, c: cpFloat);
begin
  s := Sin(x);
  c := Cos(x);
end;

function cpV(x, y: cpFloat): cpVect;
begin
  Result.x := x; Result.y := y;
end;

function cpClamp(a, min, max: cpFloat): cpFloat;
begin
  if a < min then Result := min else if a > max then Result := max else Result := a;
end;

class function cpVect.Create: cpVect;
begin
  Result.x := 0; Result.y := 0;
end;

class function cpVect.Create(const x, y: cpFloat): cpVect;
begin
  Result.x := x; Result.y := y;
end;

class operator cpVect.Negative(const a: cpVect): cpVect;
begin
  Result.x := -a.x; Result.y := -a.y;
end;

class operator cpVect.Equal(const a, b: cpVect): Boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y);
end;

class operator cpVect.NotEqual(const a, b: cpVect): Boolean;
begin
  Result :=  (a.x <> b.x) or (a.y <> b.y);
end;

class operator cpVect.Add(const a, b: cpVect): cpVect;
begin
  Result.x := a.x + b.x; Result.y := a.y + b.y;
end;

class operator cpVect.Subtract(const a, b: cpVect): cpVect;
begin
  Result.x := a.x - b.x; Result.y := a.y - b.y;
end;

class operator cpVect.Multiply(const a, b: cpVect): cpVect;
begin
  Result.x := a.x * b.x; Result.y := a.y * b.y;
end;

class operator cpVect.Multiply(a: cpFloat; const b: cpVect): cpVect;
begin
  Result.x := b.x * a; Result.y := b.y * a;
end;

class operator cpVect.Multiply(const a: cpVect; b: cpFloat): cpVect;
begin
  Result.x := a.x * b; Result.y := a.y * b;
end;

class operator cpVect.Divide(a: cpFloat; const b: cpVect): cpVect;
begin
  Result.x := a / b.x / a; Result.y := a / b.y;
end;

function cpVect.Rotate(angle: cpFloat): cpVect;
var
  s, c: cpFloat;
begin
  if Angle = 0 then
    Exit(Self);
  SinCos(angle, s, c);
  Result.x := Self.x * C - Self.y * s;
  Result.y := Self.x * s + Self.y * c;
end;

function cpVect.Distance: cpFloat;
begin
  Result := Sqrt(x * x + y * y);
end;

function cpVect.Distance(const a: cpVect): cpFloat;
var
  x1, y1: cpFloat;
begin
  x1 := a.x - x; y1 := a.y - y;
  Result := Sqrt(x1 * x1 + y1 * y1);
end;

function cpVect.PointAtDistance(const a: cpVect; distance: cpFloat): cpVect;
begin
  Result := distance * (a - Self).Normalize + Self;
end;

function cpVect.PointAtMix(const a: cpVect; percent: cpFloat): cpVect;
var
  inv: cpFloat;
begin
  inv := 1 - percent;
  Result.x := a.x * percent + x * inv;
  Result.y := a.y * percent + y * inv;
end;

function cpVect.Normalize: cpVect;
var
  d: cpFloat;
begin
  d := Distance + CP_EPSILON;
  d := 1 / d;
  Result.x := x * d;
  Result.y := y * d;
end;

function cpVect.Normal(const a: cpVect; scale: cpFloat = 1): cpVect;
var
  t: cpFloat;
begin
  Result := (Self - a).Normalize * scale;
  t := -Result.x;
  Result.x := Result.y;
  Result.y := t;
end;

function cpVect.NormalAtDistance(const a: cpVect; distance: cpFloat; scale: cpFloat = 1): cpVect;
begin
  Result := Normal(a, scale) + Normalize * distance;
end;

function cpVect.NormalAtMix(const a: cpVect; percent: cpFloat; scale: cpFloat = 1): cpVect;
begin
  Result := Normal(a, scale) + PointAtMix(a, percent);
end;

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

procedure cpSpatialIndexSegmentQuery(index: cpSpatialIndex; obj: Pointer; a, b: cpVect; t_exit: cpFloat; func: cpSpatialIndexSegmentQueryFunc; data: Pointer);
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
