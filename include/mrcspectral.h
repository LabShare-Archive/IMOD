/*  IMOD VERSION 2.02
 *
 *  mrcspectral.h -- Spectal functions for mrcc library.
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

#ifndef MRCSPECTRAL_H
#define MRCSPECTRAL_H
#include "mrcslice.h"

#ifdef __cplusplus
extern "C" {
#endif

/* returns total padding needed for size to be power of 2 */
int mrc_pow2pad(int size);

int mrc_3dfft(struct MRCvolume *v, int direction);
struct MRCslice *mrc_fft  (struct MRCslice *si, int direction);
struct MRCslice *mrc_fftx (struct MRCslice *si, int direction, int doy);
struct MRCslice *mrc_corr (struct MRCslice *s1, struct MRCslice *s2);

void  mrc_1dfft(float *data, int nn, int isign);
int corr_conj3d(struct MRCvolume *v1, struct MRCvolume *v2);
int corr_conj(float *g, float *h, int size);
int mrc_bandpass_filter(struct MRCslice *sin, double low, double high);

#ifdef __cplusplus
}
#endif
#endif  /* mrcspectral.h */


