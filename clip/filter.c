/*
 *  filter.c -- filter in freq. space for clip.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

$Date$

$Revision$

$Log$

*/

#include "mrcc.h"
#include "clip.h"

/* future: get 3d filter working
 *         add noise filter using by subtracting a noise file.
 */

int clip_3dfilter(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  if ((hin) && (hout) && (opt))
    printf("clip: 3d filter... (non-functional).\n");

  return(0);
}

int clip_bandpass_filter(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *slice;
  int z,k;
     
  /* 1/16/05: Allow input dimension interpretations of 3d, forget 3d filter */
  /*  if (opt->dim == 3)
      return(clip_3dfilter(hin, hout, opt)); */

  if ((hin->mode != MRC_MODE_COMPLEX_FLOAT) && 
      (hin->mode != MRC_MODE_COMPLEX_SHORT)) {

    /* Allow output mode to be set, otherwise set to float */
    if (opt->mode == IP_DEFAULT)
      opt->mode = MRC_MODE_FLOAT;
  } else {
    opt->ocanchmode = FALSE;  /* Currently a no-op */
    opt->mode = MRC_MODE_COMPLEX_FLOAT;
    opt->ix = opt->iy = opt->cx = opt->cy = IP_DEFAULT;
    opt->ocanresize = FALSE;
    if (opt->add2file && (hout->mode != opt->mode || hout->nx != hin->nx ||
                          hout->ny != hin->ny)) {
      show_error("clip filter - cannot append to output file of different "
                 "size or mode");
      return -1;
    }
  }
  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  mrc_head_label(hout, "clip: fourier filter");
  show_status("Doing bandpass filter...\n");

  for (k = 0; k < opt->nofsecs; k++) {
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy, 
                          (int)opt->cx, (int)opt->cy); 
    if ((hin->mode != MRC_MODE_COMPLEX_FLOAT) &&
        (hin->mode != MRC_MODE_COMPLEX_SHORT)) {
      slice_fft(slice);
      mrc_bandpass_filter(slice, opt->high, opt->low);
      slice_fft(slice);
    } else {
      sliceComplexFloat(slice);
      mrc_bandpass_filter(slice, opt->high, opt->low);
    }
      
    if (clipWriteSlice(slice, hout, opt, k, &z, 1))
      return -1;
  }
  return set_mrc_coords(opt);  
}
