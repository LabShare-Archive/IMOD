#ifndef HVEMTYPES_H
#define HVEMTYPES_H
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#ifndef NO_X_INCLUDES
#include <X11/Xmd.h>
#endif

#include <sys/types.h>

#ifndef NO_X_INCLUDES
typedef BOOL     BOOLEAN;
#endif
typedef char           CHAR;
typedef unsigned char  UCHAR;
typedef float          FLOAT;
typedef double         DOUBLE;

typedef unsigned char  UBYTE;   /* unsigned  8 bit int. */
typedef short          SHORT;   /* signed   16 bit int. */
typedef unsigned short USHORT;  /* unsigned 16 bit int. */
typedef int            INT;     /* signed   32 bit int. */
typedef unsigned int   UINT;    /* unsigned 32 bit int. */

typedef unsigned char bdBYTE;
typedef short         bdSHORT;
typedef int           bdINT;
typedef unsigned int  bdUINT;
typedef float         bdFLOAT;
typedef double        bdDOUBLE;

/* typedef int64_t;   bdLONG  */
/* typedef u_int64_t; bdULONG */

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef X 
#define X 0 
#endif

#ifndef Y
#define Y 1
#endif

#ifndef Z
#define Z 2
#endif

#include <limits.h>

#ifndef FLT_MAX 
#define FLT_MAX         3.40282347E+38F
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif
#ifndef SEEK_END
#define SEEK_END 2
#endif

#endif /* hvemtypes.h */
