#ifndef HVEMTYPES_H
#define HVEMTYPES_H
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.1  2002/11/30 07:24:00  mast
    add ability to exclude X11 definitions for Qt use

*/


#include <sys/types.h>

/* Read the definitions of the b3d... data types of defined bit size */
#include "imodconfig.h"

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
