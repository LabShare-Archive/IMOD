/*   colormap.c - 
 *                      
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */                                                                           
/*
  
$Id$
  
$Log$
Revision 1.1  2007/09/20 02:43:08  mast
Moved to new library

Revision 1.1  2006/08/27 23:44:57  mast
Added to library

*/

#include <stdlib.h>
#include <stdio.h>
#include "b3dutil.h"

/* DOC_SECTION COLORRAMP */
/* DOC_CODE Standard color ramps */
/* The standard color ramp specification used in 3dmod */
static int standardRampData[] =
  { 
    15,
    255,    0,  255,  -616,
    179,    0,  255,  -569,
    120,   40,  255,  -528,
    60,   96,  255,  -469,
    0,  175,  177,  -400,
    0,  191,  143,  -383,
    0,  207,   78,  -361,
    90,  255,   60,  -305,
    191,  255,    0,  -259,
    239,  255,    0,  -240,
    255,  255,    0,  -229,
    255,  175,    0,  -162,
    255,  105,    0,   -83,
    255,   45,   55,   -20,
    255,    0,   90,    0,
  };

/* An inverted color ramp specification, used in model view for mesh normals */
static int invertedRampData[] =
  { 
    15,
    255,    0,   90,    0,
    255,   45,   55,   20,
    255,  105,    0,   83,
    255,  175,    0,  162,
    255,  255,    0,  229,
    239,  255,    0,  240,
    191,  255,    0,  259,
    90,  255,   60,  305,
    0,  207,   78,  361,
    0,  191,  143,  383,
    0,  175,  177,  400,
    60,   96,  255,  469,
    120,   40,  255,  528,
    179,    0,  255,  569,
    255,    0,  255,  616,
  };
/* END_CODE */
/* END_SECTION */

/*!
 * Returns a pointer to the color ramp specification for the standard false
 * color table used in 3dmod.
 */
int *cmapStandardRamp()
{
  return &standardRampData[0];
}

/*!
 * Returns a pointer to a color ramp specification for an false
 * color table inverted from the one used in 3dmod.
 */
int *cmapInvertedRamp()
{
  return &invertedRampData[0];
}

/*!
 * Converts a color ramp specification in [rampData] to a full 256-color table
 * in [table].  The first value in the [rampData] array is the number of color
 * specifications; each set of 4 numbers after that is a red, green, and blue 
 * value (range 0-255) and a relative position along the color table.
 */
int cmapConvertRamp(int *rampData, unsigned char table[3][256])
{

  int nline;
  int *inramp;
  int i,l;
  float tabscl,terpfc,tabpos;
  int indtab;
  nline = *rampData;
  rampData++;
  inramp = (int *)malloc(sizeof(int) * nline * 4);
  if (!inramp)
    return 1;

  for(i = 0, l = 0; i < nline; i++, l+=4){
    inramp[l] = *rampData; rampData++;
    inramp[l+1] = *rampData; rampData++;
    inramp[l+2] = *rampData; rampData++;
    inramp[l+3] = *rampData; rampData++;
  }

  tabscl = (inramp[(nline * 4) - 1] - inramp[3])/255.0;
  indtab = 0;
  for(i = 0; i < 256; i++){
    tabpos = i * tabscl + inramp[3];
    if (tabpos > inramp[((indtab+1) * 4) + 3]){
      indtab++;
      if (indtab > nline - 2)
        indtab--;
    }

    terpfc = (tabpos - inramp[(indtab * 4) + 3])/
      (inramp[((indtab+1) * 4) + 3] - inramp[(indtab * 4) + 3]);

    table[0][i] = (unsigned char)((1 - terpfc) * inramp[(indtab * 4)] +
      terpfc * inramp [((indtab+1) * 4)]);
    table[1][i] = (unsigned char)((1 - terpfc) * inramp[(indtab * 4) + 1] +
      terpfc * inramp [((indtab+1) * 4) + 1]);
    table[2][i] = (unsigned char)((1 - terpfc) * inramp[(indtab * 4) + 2] +
      terpfc * inramp [((indtab+1) * 4) + 2]);
  }
  free(inramp);
  return 0;
}

/*!
 * Reads a color map specification from the file specified by [filename] and 
 * converts it to a complete color table in [table].  The file starts with the
 * number of lines of color data.  If there are 256 lines they must be 
 * red, green, blue triplets (range 0-255); otherwise each line has a red,
 * green, and blue followed by a relative position in the color table.
 * Returns 1 for error opening a file, 2 for errors reading the file, 3 for
 * an invalid number of lines, or 4 for a memory allocation error.
 */
int cmapReadConvert(char *filename, unsigned char table[3][256])
{
  char line[128];
  int err = 0;
  int ind, i, nlines, red, green, blue;
  int *rampData = NULL;
  FILE *fin = fopen(filename, "r");
  
  if (!fin) {
    b3dError(stderr, "cmapReadConvert: error opening file %s\n", filename);
    return 1;
  }
  
  /* Get line with number of entries */
  if (fgetline(fin, line, 128) <= 0) {
    err = 2;
  } else {
    
    nlines = atoi(line);
    if (nlines <=0 || nlines > 256) {
      b3dError(stderr, "cmapReadConvert: invalid number of lines (%d) in %s\n",
               nlines, filename);
      err = 3;
    }
  }
  
  /* If < 256, get array for ramp */
  if (!err && nlines < 256) {
    rampData = (int *)malloc((1 + nlines * 4) * sizeof(int));
    if (!rampData) {
      err = 4;
    } else {

      ind = 1;
      rampData[0] = nlines;
    }
  }

  /* Read the lines and do the appropriate thing with them */
  for (i = 0; !err && i < nlines; i++) {
    if (fgetline(fin, line, 128) <= 0) {
      err = 2;
    } else if (nlines < 256) {
      sscanf(line, "%d%*c%d%*c%d%*c%d", &rampData[ind], &rampData[ind + 1], 
             &rampData[ind + 2], &rampData[ind + 3]);
      ind += 4;
    } else {
      sscanf(line, "%d%*c%d%*c%d", &red, &green, &blue);
      table[0][i] = (unsigned char)red;
      table[1][i] = (unsigned char)green;
      table[2][i] = (unsigned char)blue;
    }
  }

  /* Convert the ramp */
  if (!err && nlines < 256) {
    err = cmapConvertRamp(rampData, table);
    if (err)
      err = 4;
  }
  
  fclose(fin);
  if (rampData)
    free(rampData);
  if (err == 2)
    b3dError(stderr, "cmapReadConvert: error reading file %s\n", filename);
  else if (err == 4)
    b3dError(stderr, "cmapReadConvert: memory allocation error");
  return err;
}
