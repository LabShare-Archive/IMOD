/*  IMOD VERSION 2.02
 *
 *  addtomfft.c -- Add two tomograms together in fourier space.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

#include <limits.h>
#include <stdlib.h>
#include <math.h>
#include <mrcc.h>

/*#define TEST_WEDGE  */


int main(int argc, char **argv)
{
     struct MRCheader ah, bh;
     FILE *afin, *bfin, *fout;
     Islice *aslice, *bslice, *cslice;
     Ival   aval, bval, cval;
     float alow = -60.0f, ahigh = 60.0f;
     float blow = -60.0f, bhigh = 60.0f;
     float a, b;
     float maxa, mina, maxb, minb;
     float rpd = 0.017453292;
     float zox, zoy;
     float xaspect, yaspect;
     int wedgea, wedgeb;
     int ox, oy, oz;
     int cx, cy, cz;
     int i, j, k;
     int flip = TRUE;
     int rscale = FALSE;
     float r1, r2, a1, a2;
     float s1 = 0.5f, s2 = 0.5f;

     if (argc < 4){
	  fprintf(stderr, "%s: usage\n", argv[0]);
	  fprintf(stderr, 
		  "%s [options] [x fft file] [y fft file] [output file]\n", 
		  argv[0]);
	  fprintf(stderr, "Options: -x #,# low,high angles for x filter.\n");
	  fprintf(stderr, "         -y #,# low,high angles for y filter.\n");
	  fprintf(stderr, "         -f     Tomogram is flipped. z = microsope z\n");
	  fprintf(stderr, "         -r weight two tomograms by r.\n");
	  fprintf(stderr, "\nBoth fft files must be aligned to one another.\n"
		  "The y fft file should have the tilt axis vertical, and\n"
		  "the x fft file should have the tilt axis horizontal.\n\n"
		  );
	  
	  exit(3);
     }
     

     for (i = 1; i < argc; i++){
	  if (argv[i][0] == '-')
	       switch (argv[i][1]){
		    
		  case 'x':
		    sscanf(argv[++i], "%f%*c%f", &alow, &ahigh);
		    printf("x range %g to %g\n", alow, ahigh);
		    break;
		    
		  case 'y':
		    sscanf(argv[++i], "%f%*c%f", &blow, &bhigh);
		    printf("y range %g to %g\n", blow, bhigh);
		    break;
		    
		  case 'f':
		    flip = FALSE;
		    break;

		  case 'r':
		    rscale = TRUE;
		    break;

		  default:
		    break;
	       }
	  else
	       break;
     }

     afin = fopen(argv[i++], "rb");
     if (afin == NULL){
	  fprintf(stderr, "Error opening %s.\n", argv[1]);
	  exit(3);
     }
     bfin = fopen(argv[i++], "rb");
     if (bfin == NULL){
	  fprintf(stderr, "Error opening %s.\n", argv[1]);
	  exit(3);
     }
     fout = fopen(argv[i], "wb");
     if (fout == NULL){
	  fprintf(stderr, "Error opening %s.\n", argv[2]);
	  exit(3);
     }


     if (mrc_head_read(afin, &ah)){
	  fprintf(stderr, "Can't Read Input File Header.\n");
	  exit(3);
     }
     if (mrc_head_read(bfin, &bh)){
	  fprintf(stderr, "Can't Read Input File Header.\n");
	  exit(3);
     }

     if ((ah.nx != bh.nx) || (ah.ny != bh.ny) || (ah.nz != bh.nz)
	 || (ah.mode != bh.mode) || (ah.mode != 4)){
	  fprintf(stderr, 
		  "Input files must both be complex mode and same size.\n");
	  exit(3);
     }

     ox = 0;
     
     if (flip){
	  oz = ah.ny / 2;
	  oy = ah.nz / 2;
	  xaspect = (float)(ah.nx * 2) / (float)ah.ny;
	  yaspect = (float)ah.nz / (float)ah.ny;
     }else{
	  oy = ah.ny / 2;
	  oz = ah.nz / 2;
	  xaspect = (float)(ah.nx * 2) / (float)ah.nz;
	  yaspect = (float)ah.ny / (float)ah.nz;
     }
     
     mrc_head_label(&ah, "tomadd: Averaged fft");
     mrc_head_write(fout, &ah);
     
     cslice = sliceCreate(ah.nx, ah.ny, ah.mode);

     mina = tanf(alow  * rpd);
     maxa = tanf(ahigh * rpd);
     minb = tanf(blow  * rpd);
     maxb = tanf(bhigh * rpd);
/*     printf(" a (%g, %g) , b (%g, %g)\n", mina, maxa, minb, maxb); */
     ah.fp = afin;
     bh.fp = bfin;
     for(k = 0; k < ah.nz; k++){
	  aslice = sliceReadMRC(&ah, k, 'z');
	  bslice = sliceReadMRC(&bh, k, 'z');
	  for(j = 0; j < ah.ny; j++)
	       for(i = 0; i < ah.nx; i++){
		    if (flip){
			 cx = i - ox;
			 cz = j - oz;
			 cy = k - oy;
		    }else{
			 cx = i - ox;
			 cy = j - oy;
			 cz = k - oz;
		    }
		    cy /= yaspect;
		    cx /= xaspect;
/*		    printf("(%d, %d, %d)\n", cx, cy, cz); */
		    /* inside good data */
		    wedgea = TRUE;
		    wedgeb = TRUE;

		    if (cx)
			 zox = (float)cz/ (float)cx;
		    else{
			 if (cz > 0)
			      zox = FLT_MAX;
			 else
			      zox = - FLT_MAX;
		    }

		    if (cy)
			 zoy = (float)cz/ (float)cy;
		    else{
			 if (cz > 0)
			      zoy = FLT_MAX;
			 else 
			      zoy = - FLT_MAX;
		    }

/*		    printf("zox = %g, zoy = %g\n", zox, zoy); */

		    r1 = sqrtf((cz * cz) + (cx * cx));
		    r2 = sqrtf((cz * cz) + (cy * cy));
		    
		    if (rscale){
			 if (0.0f != (r1 + r2)){
			      s1 = r2 / (r1 + r2);
			      s2 = r1 / (r1 + r2);
			 }else
			      s1 = s2 = 0.5f;
		    }

		    if ((zoy < minb) || (zoy > maxb))
			 wedgeb = FALSE;

		    /* x-values are reversed, from tomogram. */
		    if ((zox > (-mina)) || (zox < (-maxa) ))
			 wedgea = FALSE;

		    sliceGetVal(aslice, i, j, aval);
		    sliceGetVal(bslice, i, j, bval);

		    if ((!wedgea) && (wedgeb)){
			 cval[0] = bval[0];
			 cval[1] = bval[1];
#ifdef TEST_WEDGE
			 cval[0] = 0.25;
			 cval[1] = 0.25;
#endif

		    }else if ((wedgea) && (!wedgeb)){
			 cval[0] = aval[0];
			 cval[1] = aval[1];
#ifdef TEST_WEDGE
			 cval[0] = 0.5;
			 cval[1] = 0.5;
		    }else if ((!wedgea) && (!wedgeb)){
			 cval[0] = 0.0;
			 cval[1] = 0.0;
		    }else if ((wedgea) && (wedgeb)){
			 cval[0] = 1.0;
			 cval[1] = 1.0;
		    }
#else			 

		    }else{
			 cval[0] = (aval[0] * s1) + (bval[0] * s2);
			 cval[1] = (aval[1] * s1) + (bval[1] * s2);
		    }
#endif

		    slicePutVal(cslice, i, j, cval);
	       }
	  
	  if (!fwrite(cslice->data.b, sizeof(float), ah.nx * ah.ny * 2, fout)){
	       fprintf(stderr, "Error writing output file\n");
	       exit(3);
	  }
	       
	  sliceFree(aslice);
	  sliceFree(bslice);
     }

     sliceFree(cslice);
     fclose(afin);
     fclose(bfin);
     fclose(fout);
     exit(0);
}
