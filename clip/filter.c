/*  IMOD VERSION 2.02
 *
 *  filter.c -- filter in freq. space for clip.
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
#ifndef NOFFTLIB
#include "mrcc.h"
#include "clip.h"

/* future: get 3d filter working
 *         add noise filter using by subtracting a noise file.
 */

int clip_3dfilter(struct MRCheader *hin, struct MRCheader *hout,
                  struct Grap_options *opt)
{
  if ((hin) && (hout) && (opt))
    printf("clip: 3d filter... (non-functional).\n");

  return(0);
}

int clip_bandpass_filter(struct MRCheader *hin, struct MRCheader *hout,
                         struct Grap_options *opt)
{
  Islice *slice;
  int z,k;
     
  if (opt->dim == 3)
    return(clip_3dfilter(hin, hout, opt));


  opt->ocanchmode = FALSE;
  if ((hin->mode != MRC_MODE_COMPLEX_FLOAT) && 
      (hin->mode != MRC_MODE_COMPLEX_SHORT)){
    opt->mode = MRC_MODE_FLOAT;
  }
  else{
    opt->mode = MRC_MODE_COMPLEX_FLOAT;
    opt->ocanresize = FALSE;
  }
  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  show_status("Doing bandpass filter...\n");

  for(k = 0; k < opt->nofsecs; k++,z++){
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy, 
                          (int)opt->cx, (int)opt->cy); 
    if ((hin->mode != MRC_MODE_COMPLEX_FLOAT) &&
        (hin->mode != MRC_MODE_COMPLEX_SHORT)){
      slice_fft(slice);
      mrc_bandpass_filter(slice, opt->high, opt->low);
      slice_fft(slice);
      sliceResizeIn(slice, opt->ox, opt->oy);
    }else{
      sliceComplexFloat(slice);
      mrc_bandpass_filter(slice, opt->high, opt->low);
    }
      
    if (mrc_write_slice((void *)slice->data.b, hout->fp, hout, z, 'z'))
      return -1;

    sliceMMM(slice);

    if (slice->min < hout->amin)
      hout->amin = slice->min;
    if (slice->max > hout->amax)
      hout->amax = slice->max;
           
    hout->amean += slice->mean;
    sliceFree(slice);
  }
  if (opt->nofsecs)
    hout->amean /= opt->nofsecs;
  return mrc_head_write(hout->fp, hout);
}

#endif
