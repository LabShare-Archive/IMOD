/*   xcramp.h -  declarations for xcramp.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
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
  int    minlevel;        /* Minimum value for ramp */
  int    maxlevel;        /* Maximum value for ramp */
  int    mapsize;         /* Number of values being mapped */

  unsigned int  *ramp;
  unsigned char bramp[256];
  unsigned short *cmap;
} Cramp;


Cramp *xcramp_allinit(int depth, QGLColormap *qCmapPtr, int low, int high, int ushort);
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



