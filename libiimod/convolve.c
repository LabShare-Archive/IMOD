/*  IMOD VERSION 2.02
 *
 *  $Id$
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

#include <math.h>
#include <mrcc.h>


Islice *sliceByteEdgeSobel(Islice *sin)
{
     Islice *sout = sliceCreate(sin->xsize, sin->ysize,  SLICE_MODE_FLOAT);
     float val;
     int imax = sin->xsize - 1;
     int jmax = sin->ysize - 1;
     int i, j, x, y;
     float range;
     float n, m, k;

     for(i = 1; i < imax; i++)
	  for(j = 1; j < jmax; j++){
	       n = sliceGetPixelMagnitude(sin, i-1, j+1) +
		    2 * sliceGetPixelMagnitude(sin, i, j+1) +
			 sliceGetPixelMagnitude(sin, i+1, j+1) +
			      sliceGetPixelMagnitude(sin, i-1, j-1) +
				   2 * sliceGetPixelMagnitude(sin, i, j-1) +
					sliceGetPixelMagnitude(sin, i+1, j-1);

	       m = sliceGetPixelMagnitude(sin, i+1, j-1) +
		    2 * sliceGetPixelMagnitude(sin, i+1, j) +
			 sliceGetPixelMagnitude(sin, i+1, j+1) +
			      sliceGetPixelMagnitude(sin, i-1, j-1) +
				   2 * sliceGetPixelMagnitude(sin, i-1, j) +
					sliceGetPixelMagnitude(sin, i-1, j+1);
	       val = abs(n) + abs(m);
	       slicePutFloatVal(sout, i, j, val);
	  }

     sliceNewMode(sout,  SLICE_MODE_BYTE);
}
