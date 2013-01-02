/*   psf.c  -  Fortran interface for postscript fuctions. 
 *
 *   Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *   $Id$
 */                                                                           

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* DNM 8/17/00: add this to get the flags in as needed */
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define psopen PSOPEN
#define psclose PSCLOSE
#define pspoint  PSPOINT
#define psfirstpoint PSFIRSTPOINT
#define psvector PSVECTOR
#define psframe  PSFRAME
#define pswritetext  PSWRITETEXT
#define pscircle PSCIRCLE
#define pslinewidth PSLINEWIDTH
#define pstriangle PSTRIANGLE
#define psquadrangle PSQUADRANGLE
#define pssetcolor PSSETCOLOR
#else
#define psopen psopen_
#define psclose psclose_
#define pspoint  pspoint_
#define psfirstpoint psfirstpoint_
#define psvector psvector_
#define psframe  psframe_
#define pswritetext  pswritetext_
#define pscircle pscircle_
#define pslinewidth pslinewidth_
#define pstriangle pstriangle_
#define psquadrangle psquadrangle_
#define pssetcolor pssetcolor_
#endif

#include "ps.h"

static PS *ps = NULL;
static char defaultFont[] = "Helvetica";
static int lastsize = 0;

int psopen(char *filename, float *lm, float *bm, float *dpi, int filename_size)
{
  double ddpi, dlm, dbm;
  char *fname = f2cString(filename, filename_size);
  if (!fname) {
    fprintf(stderr, "psopen: error getting memory\n");
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
  if (ps)
    PSsetLineWidth(ps, dw);
}

void pssetcolor(int *red, int *green, int *blue)
{
  if (ps)
    PSsetColor(ps, *red, *green, *blue);
}

void pspoint(float *ix, float *iy)
{
  double x = *ix;
  double y = *iy;
  if (ps)
    PSdrawPoint(ps, x, y);
}

void psfirstpoint(float *ix, float *iy)
{
  double x = *ix;
  double y = *iy;
  if (ps)
    PSsetPoint(ps, x, y);
}

void psvector(float *ix, float *iy)
{
  double x = *ix;
  double y = *iy;
  if (ps)
    PSdrawVector(ps, x, y);
}

void pscircle(float *ix, float *iy, float *irad, int *fill)
{
  double x = *ix;
  double y = *iy;
  double rad = *irad;
  if (ps)
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
  if (ps)
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
  if (ps)
    PSdrawQuadrangle(ps, x, y, *fill);
}

void psframe()
{
  if (ps)
    PSpage(ps);
}

void pswritetext(float *ix, float *iy, char *text, int *jsize, int *jor, int *jctr, 
           int text_size)

{
  float x = *ix;
  float y = *iy;

  char *useFont = getenv("IMOD_PS_FONT");
  char *ctext = (char *)malloc(text_size + 1);
  if (!useFont) 
    useFont = defaultFont;
  memcpy(ctext, text, text_size);
  ctext[text_size] = 0x00;

  if(lastsize != *jsize){
    lastsize = *jsize;
    if (ps)
      PSsetFont(ps, useFont, lastsize);
  }

  if (ps)
    PSdrawText(ps, ctext, x, y, *jor, *jctr);

  /* DNM 3/23/01: plug the leak? */
  free(ctext);
}

void psclose()
{
  if (ps)
    PSclose(ps);
}

