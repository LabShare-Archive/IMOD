/*   psf.c  -  Fortran interface for postscript fuctions. 
 *
 *   Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.3  2005/01/21 18:19:28  mast
Allow IMOD_PS_FONT to be set to set the font

Revision 3.2  2004/11/05 18:53:39  mast
Include local files with quotes, not brackets

Revision 3.1  2003/10/29 04:31:14  mast
switch to calling f2cString

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* DNM 8/17/00: add this to get the flags in as needed */
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77STRING
typedef struct
{
  unsigned short length;
  char dum1;
  char dum2;
  char *string;
} FString;
#endif

#ifdef F77FUNCAP
#define psopen PSOPEN
#define psclose PSCLOSE
#define point  POINT
#define frstpt FRSTPT
#define vector VECTOR
#define frame  FRAME
#define wtstr  WTSTR
#define pscircle PSCIRCLE
#define pslinewidth PSLINEWIDTH
#define pstriangle PSTRIANGLE
#define psquadrangle PSQUADRANGLE
#else
#define psopen psopen_
#define psclose psclose_
#define point  point_
#define frstpt frstpt_
#define vector vector_
#define frame  frame_
#define wtstr  wtstr_
#define pscircle pscircle_
#define pslinewidth pslinewidth_
#define pstriangle pstriangle_
#define psquadrangle psquadrangle_
#endif

#include "ps.h"

static PS *ps = NULL;
static char defaultFont[] = "Helvetica";
static int lastsize = 0;

int psopen(
#ifdef F77STRING
           FString *f77str,
#else
           char *filename,
#endif

           float *lm, float *bm, float *dpi

#ifndef F77STRING
           , int filename_size
#endif
           )
{
#ifdef F77STRING
  int  filename_size  = f77str->length;
  char *filename      = f77str->string;
#endif
  double ddpi, dlm, dbm;
  char *fname = f2cString(filename, filename_size);
  if (!fname) {
    fprintf(stderr, "libps: error getting memory\n");
    return(-1);
  }
  ddpi = *dpi; dlm = *lm; dbm = *bm;
  ps = PSopen(fname, ddpi, dlm, dbm);

  /* DNM 3/23/01: set font size to zero so a font specification will be
     output to every new file */
  lastsize = 0;
  if (!ps)
    return(-1);
  free(fname);
  return(0);
}

void pslinewidth(float *width)
{
  double dw = *width;
  PSsetLineWidth(ps, dw);
}

void point(float *ix, float *iy)
{
  double x = *ix;
  double y = *iy;
  PSdrawPoint(ps, x, y);
}

void frstpt(float *ix, float *iy)
{
  double x = *ix;
  double y = *iy;
  PSsetPoint(ps, x, y);
}

void vector(float *ix, float *iy)
{
  double x = *ix;
  double y = *iy;
  PSdrawVector(ps, x, y);
}

void pscircle(float *ix, float *iy, float *irad, int *fill)
{
  double x = *ix;
  double y = *iy;
  double rad = *irad;
  PSdrawCircle(ps, x, y, rad, *fill);
}

void pstriangle(float *ix, float *iy, int *fill)
{
  double x[3], y[3];
  int i;
  for (i = 0; i < 3; i++) {
    x[i] = ix[i];
    y[i] = iy[i];
  }
  PSdrawTriangle(ps, x, y, *fill);
}

void psquadrangle(float *ix, float *iy, int *fill)
{
  double x[4], y[4];
  int i;
  for (i = 0; i < 4; i++) {
    x[i] = ix[i];
    y[i] = iy[i];
  }
  PSdrawQuadrangle(ps, x, y, *fill);
}

void frame(){PSpage(ps);}

void wtstr(float *ix, float *iy,
#ifdef F77STRING
           FString *f77str,
#else
           char *text,
#endif
           int *jsize, int *jor, int *jctr

#ifndef F77STRING
           , int text_size
#endif
           )

{
  float x = *ix;
  float y = *iy;
#ifdef F77STRING
  int  text_size  = f77str->length;
  char *text      = f77str->string;
#endif

  char *useFont = getenv("IMOD_PS_FONT");
  char *ctext = (char *)malloc(text_size + 1);
  if (!useFont) 
    useFont = defaultFont;
  memcpy(ctext, text, text_size);
  ctext[text_size] = 0x00;

  if(lastsize != *jsize){
    lastsize = *jsize;
    PSsetFont(ps, useFont, lastsize);
  }

  PSdrawText(ps, ctext, x, y, *jor, *jctr);

  /* DNM 3/23/01: plug the leak? */
  free(ctext);
}

void psclose(){PSclose(ps);}

