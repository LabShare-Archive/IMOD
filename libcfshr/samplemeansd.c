/* 
 * samplemeansd.c : estimate mean and Sd of image from sample of pixels
 *
 * $Id$
 * Log at end
 */

#include <stdlib.h>
#include <math.h>
#include "imodconfig.h"
#include "b3dutil.h"

#define BYTE 0
#define SIGNED_BYTE 1
#define UNSIGNED_SHORT 2
#define SIGNED_SHORT 3
#define FLOAT 6
#define INTEGER 7
#define RGB   8
#define RGBA  9

#ifdef F77FUNCAP
#define samplemeansd SAMPLEMEANSD
#else
#define samplemeansd samplemeansd_
#endif

/*!
 * Estimates mean and SD of a sample of an image.  Returns nonzero for errors.
 * ^ [image] = array of pointers to each line of data 
 * ^ [type] = data type, 0 for bytes, 2 for unsigned shorts, 3 for signed shorts, 6 for 
 * floats, 7 for 4-byte integers, 8 for RGB triples of bytes or 9 for RGBA bytes;
 * RGB values are weighted with NTSC weighting
 * ^ [nx], [ny] = X and Y dimensions of image
 * ^ [sample] = fraction of pixels to sample
 * ^ [ixStart], [iyStart] = starting X and Y index to include
 * ^ [nxUse], [nyUse] = number of pixels in X and Y to include
 * ^ [mean], [sd] = returned values of mean and sd
*/
int sampleMeanSD(unsigned char **image, int type, int nx, int ny, 
                 float sample, int ixStart, int iyStart, int nxUse, int nyUse,
                 float *mean, float *sd)
{
  int nSample;

  /* pointers for data of different types */
  char **bytep;
  unsigned char **ubytep;
  unsigned short int **ushortp;
  short int **shortp;
  float **floatp;
  int **intp;

  int ixUse, iyUse, dxSample;
  int i, j, nchan = type == RGBA ? 4 : 3;
  int val;
  unsigned int uval;
  double sum, sumsq, nPixUse;
  float fval;
  int nsum;
  
  if (!image)
    return (-1);

  /* get the number of points to sample, and sampling interval */
  nPixUse = ((double)nxUse) * nyUse;
  nSample = (int)(sample * nPixUse);
  if (nSample >= nPixUse) 
    nSample = (int)nPixUse;
  if (nxUse < 2 || nyUse < 2 || nSample < 5)
    return(1);

  dxSample = (int)(nPixUse / nSample);
  if (dxSample == 0)
    dxSample = 1;
  if (dxSample > 5 && nPixUse < 2.e9 && (dxSample % 2 == ((int)nPixUse % 2)))
    dxSample--;
  nSample = (int)(nPixUse / dxSample);

  /* get pointer based on type of data */
  switch (type) {
  case BYTE :
  case RGBA :
  case RGB :
    ubytep = image;
    break;

  case SIGNED_BYTE :
    bytep = (char **) image;
    break;

  case SIGNED_SHORT :
    shortp = (short int **)image;
    break;

  case UNSIGNED_SHORT :
    ushortp = (unsigned short int **)image;
    break;

  case FLOAT :
    floatp = (float **)image;
    break;
  case INTEGER :
    intp = (int **)image;
    break;
  default :
    return(2);
  }

  sum = 0.;
  sumsq = 0.;
  if (dxSample == 1 && ixStart == 0 && iyStart == 0) {

    for (j = 0; j < nyUse; j++) {
      switch (type) {
      case BYTE:
        for (i = 0; i < nxUse; i++) {
          uval = ubytep[j][i];
          sum += uval;
          sumsq += uval * uval;
        }
        break;

      case RGBA:
      case RGB:
        for (i = 0; i < nxUse; i++) {
          fval = 0.3 * ubytep[j][nchan*i] + 0.59 * ubytep[j][nchan*i+1] + 
            0.11 * ubytep[j][nchan*i+2];
          sum += fval;
          sumsq += fval * fval;
        }
        break;

      case SIGNED_BYTE :
        for (i = 0; i < nxUse; i++) {
          val = bytep[j][i];
          sum += val;
          sumsq += val * val;
        }
        break;

      case SIGNED_SHORT :
        for (i = 0; i < nxUse; i++) {
          val = shortp[j][i];
          sum += val;
          sumsq += val * val;
        } 
        break;

      case UNSIGNED_SHORT :
        for (i = 0; i < nxUse; i++) {
          uval = ushortp[j][i];
          sum += uval;
          sumsq += uval * uval;
        }
        break;

      case FLOAT :
        for (i = 0; i < nxUse; i++) {
          fval = floatp[j][i];
          sum += fval;
          sumsq += fval * fval;
        }
        break;

      case INTEGER :
        for (i = 0; i < nxUse; i++) {
          fval = intp[j][i];
          sum += fval;
          sumsq += fval * fval;
        }
        break;

      }
    }
    nsum = (int)nPixUse;
    
  } else {
    ixUse = 0;
    iyUse = 0;

    for (j = 0; j < nSample; j++) {

      /* get the value */
      switch (type) {
      case BYTE :
        fval = ubytep[iyUse + iyStart][ixUse + ixStart];
        break;

      case RGBA:
      case RGB:
        fval = 0.3 * ubytep[iyUse + iyStart][nchan*(ixUse+ixStart)] +
          0.59 * ubytep[iyUse + iyStart][nchan*(ixUse+ixStart)+1] +
          0.11 * ubytep[iyUse + iyStart][nchan*(ixUse+ixStart)+2];
        break;
    
      case SIGNED_BYTE :
        fval = bytep[iyUse + iyStart][ixUse + ixStart];
        break;
    
      case SIGNED_SHORT :
        fval = shortp[iyUse + iyStart][ixUse + ixStart];
        break;

      case UNSIGNED_SHORT :
        fval = ushortp[iyUse + iyStart][ixUse + ixStart];
        break;

      case FLOAT :
        fval = floatp[iyUse + iyStart][ixUse + ixStart];
        break;

      case INTEGER :
        fval = intp[iyUse + iyStart][ixUse + ixStart];
        break;

      }
      sum += fval;
      sumsq += fval * fval;

      /* move indexes to next spot in use area */
      ixUse += dxSample;
      while (ixUse >= nxUse) {
        ixUse -= nxUse;
        iyUse++;
      }

    }
    nsum = nSample;
  }

    
  sum /= nsum;
  if (nsum > 1) {
    sumsq = (sumsq - nsum * sum * sum)/(nsum - 1.);
    if (sumsq < 0.0)
      sumsq = 0.0;
    sumsq = sqrt(sumsq);
  } else
    sumsq = 0.;

  *mean = (float)sum;
  *sd = (float)sumsq;

  return(0);
}

/*!
 * Fortran wrapper for @sampleMeanSD with a floating point array in [image]
 */
int samplemeansd(float *image, int *nx, int *ny, float *sample, int *ixStart,
                 int *iyStart, int *nxUse, int *nyUse, float *mean, float *sd)
{
  int i;
  unsigned char **lines = makeLinePointers(image, *nx, *ny, 4);
  if (!lines)
    return -1;
  i = sampleMeanSD(lines, FLOAT, *nx, *ny, *sample, *ixStart, *iyStart, *nxUse,
                   *nyUse, mean, sd);
  free(lines);
  return i;
}

/*

$Log$
Revision 1.5  2011/02/14 23:59:03  mast
Fixed byte case and call to new line pointer routine

Revision 1.4  2011/02/12 04:32:06  mast
Added support for RGB

Revision 1.3  2010/02/26 16:56:57  mast
Added fortran wrapper

Revision 1.2  2008/10/02 02:05:12  mast
Clean up warning for SerialEM

Revision 1.1  2007/09/20 02:43:08  mast
Moved to new library

Revision 3.2  2006/10/02 15:32:32  mast
Fixed for > 2 Gpixel image

Revision 3.1  2006/06/26 15:44:06  mast
*** empty log message ***

Revision 3.3  2003/09/18 00:48:55  mast
Changed to take starting coordinates and image subset size

Revision 3.2  2003/09/16 02:59:10  mast
Changed to access image data via line pointers

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/

