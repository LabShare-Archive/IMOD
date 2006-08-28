/*   xcramp.h -  declarations for xcramp.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */
/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.1  2003/02/10 20:41:56  mast
Merge Qt source

Revision 1.1.2.2  2003/01/29 01:45:56  mast
changes for color index mode

Revision 1.1.2.1  2003/01/26 23:26:40  mast
Qt version

Revision 3.1  2002/12/01 15:39:50  mast
Declare extern C if c++

*/

#ifndef XCRAMP_H
#define XCRAMP_H

class QGLColormap;

typedef struct xbldrcoloramp
{
  int  depth;    /* The depth of colormap.     */
  int  rgba;
  QGLColormap *qCmapPtr;
  float         scale;    /* rampsize/255                 */
  int    rampsize;        /* The number of colors allocated.          */
  int    rampbase;    /* The number of first color.               */
  int    blacklevel;      /* The grey scale black level, default 0    */
  int    whitelevel;      /* The grey scale white level, default 255. */
  int    reverse;         /* Flag for reverse contrast                */
  int    falsecolor;      /* Use color instead of grey scale ramp.    */
  int    noflevels;       /* Number of levels available for storage.  */
  int    clevel;          /* current level index in use.              */
  int    *blacks;         /* Array of black levels.                   */
  int    *whites;         /* Array of white levels.                   */

  unsigned int   ramp[256];
} Cramp;


Cramp *xcramp_allinit(int depth, QGLColormap *qCmapPtr,  int low,  int high);
int    xcramp_level(Cramp *xcramp, int black, int white);
int    xcramp_falsecolor(Cramp *xcramp, int flag);
int    xcramp_reverse(Cramp *xcramp, int flag);
int    xcramp_ramp(Cramp *cr);
void   xcramp_mapfalsecolor(int gray, int *red, int *green, int *blue);
int    xcramp_readfalsemap(char *filename);
void   xcramp_copyfalsemap(unsigned char *inmap);
void   xcramp_restorefalsemap();
void   xcramp_setlevels(Cramp *xcramp, int black, int white);
void   xcramp_getlevels(Cramp *xcramp, int *black, int *white);

int  xcrampStoreInit(Cramp *cramp, int size);
int  xcrampSelectIndex(Cramp *cramp, int index);

#endif /* XCRAMP_H */



