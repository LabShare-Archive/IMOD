/* sampleMeanSD: function to estimate mean and SD of a sample of an image
   image = pointer to start of image
   type = data type, one of the values defined below
   nx, ny = X and Y dimensions of image
   sample = fraction of pixels to sample
   matt = fraction of image to ignore on each edge
   mean, sd = returned values of mean and sd
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#define BYTE 0
#define SIGNED_BYTE 1
#define UNSIGNED_SHORT 2
#define SIGNED_SHORT 3
#define FLOAT 6

int sampleMeanSD(unsigned char *image, int type, int nx, int ny, float sample, 
		 float matt, float *mean, float *sd)
{
  int nxUse, nyUse, nxMatt, nyMatt;
  int nLo, nHi, nSample, nPixUse;

  /* pointers for data of different types */
  char *bytep;
  unsigned char *ubytep;
  unsigned short int *ushortp;
  short int *shortp;
  float *floatp;

  int ixUse, iyUse, dxSample;
  int i, j, offset;
  int val;
  unsigned int uval;
  double sum, sumsq;
  float fval;
  int nsum;
  
  if (!image)
       return (-1);

  /* get the area that will be used and offsets into it */  
  nxMatt = nx * matt;
  nxUse = nx - nxMatt;
  nyMatt = ny * matt;
  nyUse = ny - nyMatt;
  nPixUse = nxUse * nyUse;

  /* get the number of points to sample, and sampling interval */
  nSample = sample * nPixUse;
  if (nSample >= nPixUse) nSample = nPixUse;
  if (nxUse < 2 || nyUse < 2 || nSample < 5) return(1);

  dxSample = nPixUse / nSample;
  if (dxSample == 0) dxSample = 1;
  if (dxSample > 5 && (dxSample % 2 == nPixUse % 2)) dxSample--;
  nSample = nPixUse / dxSample;


/* get pointer based on type of data */
  switch (type) {
    case BYTE :
      ubytep = image;
      break;

    case SIGNED_BYTE :
      bytep = (char *) image;
      break;

    case SIGNED_SHORT :
      shortp = (short int *)image;
      break;

    case UNSIGNED_SHORT :
      ushortp = (unsigned short int *)image;
      break;

    case FLOAT :
      floatp = (float *)image;
      break;

  }

  sum = 0.;
  sumsq = 0.;
  if (dxSample == 1 && nxMatt == 0 && nyMatt == 0) {
    switch (type) {
      case BYTE:
	for (i = 0; i < nPixUse; i++) {
	  uval = *ubytep++;
	  sum += uval;
	  sumsq += uval * uval;
	}
	break;

      case SIGNED_BYTE :
	for (i = 0; i < nPixUse; i++) {
	  val = *bytep++;
	  sum += val;
	  sumsq += val * val;
	}
	break;

      case SIGNED_SHORT :
	for (i = 0; i < nPixUse; i++) {
	  val = *shortp++;
	  sum += val;
	  sumsq += val * val;
	}	
	break;

      case UNSIGNED_SHORT :
	for (i = 0; i < nPixUse; i++) {
	  uval = *ushortp++;
	  sum += uval;
	  sumsq += uval * uval;
	}
	break;

      case FLOAT :
	for (i = 0; i < nPixUse; i++) {
	  fval = *floatp++;
	  sum += fval;
	  sumsq += fval * fval;
	}
	break;

    }

    nsum = nPixUse;

  } else {
    ixUse = 0;
    iyUse = 0;

    for (j = 0; j < nSample; j++) {

      /* get offset in actual image, and move indexes to next spot in use area */
      offset = nx * (iyUse + nyMatt) + ixUse + nxMatt;
      ixUse += dxSample;
      while (ixUse >= nxUse) {
	ixUse -= nxUse;
	iyUse++;
      }

      /* get the value */
      switch (type) {
        case BYTE :
	  fval = *(ubytep + offset);
	  break;
	
        case SIGNED_BYTE :
	  fval = *(bytep + offset);
	  break;
	
	case SIGNED_SHORT :
	  fval = *(shortp + offset);
	  break;

	case UNSIGNED_SHORT :
	  fval = *(ushortp + offset);
	  break;

	case FLOAT :
	  fval = *(floatp + offset);
	  break;

	default :
	  return(2);
      }
      sum += fval;
      sumsq += fval * fval;
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

  *mean = sum;
  *sd = sumsq;

  return(0);
}

