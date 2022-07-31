unit Tiny.Constants;

{$i tiny.inc}

interface

const
  SNoneError = 'None';
  SUnknownError = 'Unknown error code %d';
  SAbstractError = 'Abstract error';
  SOutOfMemory = 'Out of memory';
  SInvalidPointer = 'Invalid pointer operation';
  SDivByZero = 'Division by zero';
  SRangeError = 'Range check error';
  SRangeMethodError = 'Index exceeds bounds range in method %s.%s';
  SIntOverflow = 'Arithmetic overflow';
  SInvalidOp = 'Invalid floating point operation';
  SZeroDivide = 'Floating point division by zero';
  SOverflow = 'Floating point overflow';
  SUnderflow = 'Floating point underflow';
  SObjectCheck = 'Object is nil';
  SInvalidCast = 'Invalid type cast';
  SBusInvalid = 'Bus error or misaligned data access';
  SAccessViolation = 'Access violation';
  SPrivInstruction = 'Privileged instruction';
  SControlBreak = 'Control break detected';
  SStackOverflow = 'Stack overflow';
  SVarTypeCast = 'Invalid variant type cast';
  SVarInvalidOp = 'Invalid variant operation';
  SVarDispatch = 'No variant method call dispatch';
  SVarArrayCreate = 'Variant array cannot be created';
  SVarNotArray = 'Variant does not contain an array';
  SVarArrayBounds = 'Variant array bounds error';
  SAssertionFailed= 'Assertion failed';
  SAssertionError = '%s (%s, line %d)';
  SStreamCancelledError = 'An attempt to read or write was made to a cancelled stream';
  SExternalException = 'External exception';
  SIntfCast = 'Invalid interface cast';
  SSafeCallError = 'Exception in safecall method';
  SQuitSignal = 'Quit signal received';
  SCodesetConversionError = 'Codeset conversion failed';
  SConvertNoneError = 'Unable to convert';
  SNoThreadSupport = 'Thread support was not configured properly';
  SNoWideStringSupport = 'WideString support was not configured properly';
  SIOFileHandleError = 'Unable to create file handle';
  SConvertError = 'Unable to convert %s to %s';
  SAssetWriteError = 'Asset streams are read only';
  SAssetNotExists = 'Asset could not be located';
  SListDuplicateError = 'List duplicate detected';
  SSynchronizeError = 'An error occured with thread syncrhonziation';
  SSelfReferenceError = 'A self referencing collection was detected in method %s.%s';
  SSDLFunctionFailed = 'SDL function %s failed with reason "%s"';
  SNoAssetFolder = 'Asset folder not found. Undefine examples in bare.inc and create a local asset folder.';
  SLibraryLoadFailed = 'The library %s failed load';
  SContextInvalid = 'The context requested is not in a valid state';
  SContextUnknown = 'A context define or asset does not exist';
  SUnknownReason = 'Unknown reason';
  SVertexBufferUnderflow = 'A vertex buffer was created with no vertices';
  SVertexBufferOverflow = 'A vertex buffer ran out of available space';

implementation

end.

