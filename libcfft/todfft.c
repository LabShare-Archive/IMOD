/*
  C*TODFFT.FOR********************************************************
  C
  C   TWO-DIMENSIONAL FOURIER TRANSFORM SUBROUTINE FOR IMAGE
  C   PROCESSING. DOES BOTH FORWARD & INVERSE TRANSFORMS
  C   USES LYNN TENEYCK'S MIXED-RADIX ROUTINES
  C   THUS THE ONLY RESTRICTION IS THAT THE IMAGE SIZE BE
  C   AN EVEN NUMBER AND HAVE NO PRIME FACTORS LARGER THAN 19!!
  C
  C   IDIR =  0   FORWARD  TRANSFORM :  exp(+2PIirs)
  C   IDIR =  1   INVERSE TRANSFORM  :  exp(-2PIirs)
  C   IDIR = -1   INVERSE TRANSFORM BUT NO COMPLEX CONJUGATE
  C
  C   DATA SET UP AS NY ROWS OF NX NUMBERS
  C   NOTE NX,NY ALWAYS REFER TO THE REAL-SPACE IMAGE SIZE
  C
  C   NX,NY IMAGE IS TRANSFORMED IN-PLACE INTO NY STRIPS OF
  C   NX/2 + 1 COMPLEX FOURIER COEFFICIENTS
  C   THE ORIGIN IS LOCATED AT THE FIRST POINT!!!
  C
  C   ARRAY MUST BE DIMENSIONED TO ALLOW FOR EXTRA STRIP OF COMPLEX
  C   NUMBERS ON OUTPUT.
  C   THUS FOR A 300X400 TRANSFORM, ARRAY MUST BE DIMENSIONED:
  C   REAL ARRAY(302,400)
  C
  C   A NORMALIZATION FACTOR IS APPLIED DURING BOTH  TRANSFORMATIONS
  C
  C   VERSION 1.00    OCT 11 1981     DAA
  C   VERSION 1.02    APR 19 1982     DAA
  C   VERSION 1.03    NOV 23 1982     DAA
*/
/*  $Author$
    
$Date$

$Revision$

$Log$
*/

#include "cfft.h"
#include <stdio.h>
#include <math.h>

void todfft(float *array, int *nxp, int *nyp, int *idirp)
{
  int idim[6];   /* Make it 6 so that indexes 1 to 5 work */
  int nxo2, nx2, nxt, nxt1, j, nxp2, nxm1, index, iy;
  float onevol;
  int nx = *nxp;
  int ny = *nyp;
  int idir = *idirp;

  nxo2 = nx/2;
  if (2*nxo2 != nx) {
    printf(" todfft: nx= %d must be even!!!\n", nx);
    exit(1);
  }
  nxp2 = nx + 2;
  nxt = nxp2*ny;
  nxt1 = nxt - 1;
  onevol = sqrt((double)(1.0/(nx*ny)));
  if (idir == 0) {
    /*
      c********      forward transforms come here       ******************
      c
      c
      c  set up for first dimension of transform
    */
    idim[1] = nxp2*ny;
    idim[2] = 2;
    idim[3] = idim[1];
    idim[4] = idim[1];
    idim[5] = nxp2;

    realft(array, &(array[1]), nxo2, idim);
    /*
      c  set up for second dimension of transform
    */
    idim[2] = nxp2;
    idim[4] = idim[2];
    idim[5] = 2;

    cmplft(array, &(array[1]), ny, idim);
    /*
      c   take complex conjugate (to make proper forward transform)& scale by 1/volume
    */
    for (j = 0; j < nxt1; j += 2) {
      array[j] = array[j]*onevol;
      array[j+1] = -array[j+1]*onevol;
    }

    return;
  }
  /*
    c**********        inverse transform     *******************
    c
    c
    c  set up for first dimension of transform
  */
  nxm1 = nx - 1;
  idim[1] = nxp2*ny;
  idim[2] = nxp2;
  idim[3] = idim[1];
  idim[4] = idim[2];
  idim[5] = 2;
  /*
    c   take complex conjugate to do inverse & scale by 1/volume
  */
  if (idir != 1) {
    for (j = 0; j < nxt1; j += 2) {
      array[j] = array[j]*onevol;
      array[j+1] = -array[j+1]*onevol;
    }
  } else {
    /*
      c   idir = 1 just scale by 1/volume (for standard inverse transform)
    */
    for (j = 0; j < nxt1; j += 2) {
      array[j] = array[j]*onevol;
      array[j+1] = array[j+1]*onevol;
    }
  }

  cmplft(array, &(array[1]), ny, idim);
  /*
    c  set up for second dimension of transform
  */
  idim[2] = 2;
  idim[4] = idim[1];
  idim[5] = nxp2;
  /*
    c    change data storage mode complex conjugate done by hermft
  */
  index = 1;
  for (iy = 0; iy < ny; iy++) {
    array[index] = array[nxm1 + index];
    index = index + nxp2;
  }

  hermft(array, &(array[1]), nxo2, idim);

  return;
}
