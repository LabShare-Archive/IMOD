/*  IMOD VERSION 2.50
 *
 *  clip -- Command Line Image Proccesing
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#include <stdlib.h>
#include <string.h>
#include "mrcfiles.h"
#include "mrcslice.h"
#include "mrcspectral.h"
#include "clip.h"
#include "imodconfig.h"

void usage(void)
{
     char *name = "clip";

     fprintf(stderr, 
	     "%s: Command Line Image Processing. %s, %s %s\n",
	     VERSION_NAME,
	     name, __DATE__, __TIME__);
     fprintf(stderr, 
	     "Copyright (C) 1995-2001 Boulder Laboratory for 3-Dimensional");
     fprintf(stderr,"Fine Structure,\n Regents of the Univ. of Colorado\n");
     fprintf(stderr, "----------------------------------------------------\n");

     fprintf(stderr, "%s usage:\n", name);
     fprintf(stderr, 
	     "%s [process] [options] [input files...] [output file]\n", name);
     fprintf(stderr, "\nprocess:\n");
/*     fprintf(stderr, "\tadd         - add images together.(future)\n"); */
     fprintf(stderr, "\taverage     - average files together.\n");
     fprintf(stderr, "\tbrighten    - Increase or decrease brightness.\n");
     fprintf(stderr, "\tcolor       - add false color.\n");
     fprintf(stderr, "\tcontrast    - Increase or decrease contrast.\n");
#ifndef NOFFTLIB
     fprintf(stderr, "\tcorrelation - do a auto/cross correlation.\n");
#endif
     fprintf(stderr, "\tedge        - make an edge map.\n");
     fprintf(stderr, "\tinfo        - print information to stdout.\n");
#ifndef NOFFTLIB
     fprintf(stderr, "\tfft         - do a fft or inverse fft transform.\n");
     fprintf(stderr, "\tfilter      - do a bandpass filter.\n");
#endif
     fprintf(stderr, "\tflip        - rotate image by 90 deg. steps.\n");
/*     fprintf(stderr, "\tpeak        - find peaks in image.\n"); */
     fprintf(stderr, "\tresize      - cut out and/or pad image data.\n");
     fprintf(stderr, "\trotation    - rotate image\n");
     fprintf(stderr, "\tshadow      - Increase or decrease image shadows.\n");
     fprintf(stderr, "\tsharpen     - Sharpen/blur image.\n");
     fprintf(stderr, "\tstats       - Print some stats on image file.\n");
     fprintf(stderr, "\ttranslate   - translate image.\n");
     fprintf(stderr, "\tzoom        - magnifiy image.\n");
     fprintf(stderr, "\noptions:\n");
     fprintf(stderr, "\t[-v]  view output data.\n");
     fprintf(stderr, "\t[-3d] or [-2d] treat image as 3d (default) or 2d.\n");
     fprintf(stderr, "\t[-s] Switch, [-n #] Amount; Depends on function.\n");
     fprintf(stderr, "\t[-h #] [-l #] values for high/low pass filters.\n");
     fprintf(stderr, "\t[-r #] [-g #] [-b #] red, green, blue values.\n");
     fprintf(stderr, "\t[-x #,#] [-y #,#] [-z #,#] input variables.\n");
     fprintf(stderr, "\t[-cx #]  [-cy #]  [-cz #]  center coords.\n");
     fprintf(stderr, "\t[-ix #]  [-iy #]  [-iz #]  input sizes.\n");
     fprintf(stderr, "\t[-ox #]  [-oy #]  [-oz #]  output sizes.\n");
     fprintf(stderr, "\t[-a] Append output to file.\n");
     fprintf(stderr, "\t[-ov #] Overwrite output starting at section #\n");
     fprintf(stderr, "\t[-m (mode#)] output data mode.\n");
     fprintf(stderr, "\t[-p (pad#)] pad empty data value.\n");
     fprintf(stderr, "\n");
}

void show_error(char *reason)
{
     fprintf(stderr, "clip: %s\n", reason);
}

void show_status(char *info)
{
     printf("%s", info);
     fflush(stdout);
}

void default_options(struct Grap_options *opt)
{
     opt->hin = opt->hin2 = opt->hout = NULL;
     opt->x  = IP_DEFAULT; opt->x2 = IP_DEFAULT;
     opt->y  = IP_DEFAULT; opt->y2 = IP_DEFAULT;
     opt->z  = IP_DEFAULT; opt->z2 = IP_DEFAULT;
     opt->ix = IP_DEFAULT; opt->iy = IP_DEFAULT; 
     opt->iz = IP_DEFAULT; opt->iz2 = IP_DEFAULT;
     opt->ox = IP_DEFAULT; opt->oy = IP_DEFAULT; opt->oz = IP_DEFAULT;
     opt->cx = IP_DEFAULT; opt->cy = IP_DEFAULT; opt->cz = IP_DEFAULT;
     opt->red = IP_DEFAULT; opt->green = IP_DEFAULT; opt->blue = IP_DEFAULT;
     opt->high = 0; opt->low = 1.0;
     opt->thresh = IP_DEFAULT;
     opt->weight = IP_DEFAULT;
     opt->pad    = IP_DEFAULT;
     opt->dim = 3;
     opt->add2file = IP_APPEND_FALSE;
     opt->sano = FALSE;
     opt->val = IP_DEFAULT;
     opt->mode = IP_DEFAULT;
     opt->nofsecs = IP_DEFAULT;
     opt->ocanresize = TRUE;
     opt->ocanchmode = TRUE;
     opt->ofname = NULL;
}



int main( int argc, char *argv[] )
{
     struct MRCheader hin, hin2, hout;
     struct Grap_options opt;

     int process = IP_NONE;  /* command to run.             */
     int view    = FALSE;    /* view file at end?           */
     int procout = TRUE;     /* will process write output?. */
     int i;

     char viewcmd[1024];
     char *backupstr;

     if (argc < 3){
	  usage();
	  exit(-1);
     }

     opt.command = argv[1];

     /* Find which process to run */
     if (!strncasecmp( argv[1], "add", 3))
	  process = IP_ADD;
     if ((!strncasecmp( argv[1], "avg", 3)) || 
	 (!strncasecmp( argv[1], "average", 3)) )
	  process = IP_AVERAGE;
     if (!strncasecmp( argv[1], "brightness", 3))
	  process = IP_BRIGHTNESS;
     if (!strncasecmp( argv[1], "color", 3))
	  process = IP_COLOR;
     if (!strncasecmp( argv[1], "contrast", 3))
	  process = IP_CONTRAST;

#ifndef NOFFTLIB
     if (!strncasecmp( argv[1], "correlation", 3))
	  process = IP_CORRELATE; 
#endif

     if (!strncasecmp( argv[1], "edge", 3))
	  process = IP_EDGE;
     if (!strncasecmp( argv[1], "info", 3)){
	  process = IP_INFO;
	  procout = FALSE;
     }

#ifndef NOFFTLIB
     if (!strncasecmp( argv[1], "fft", 3))
	  process = IP_FFT;
     if (!strncasecmp( argv[1], "filter", 3))
	  process = IP_FILTER;
#endif

     if (!strncasecmp( argv[1], "flip", 4))
	  process = IP_FLIP;
     if (!strncasecmp( argv[1], "peak", 3)){
	  process = IP_PEAK;
	  procout = FALSE;
     }
     if (!strncasecmp( argv[1], "resize", 3))
	  process = IP_RESIZE;
     if (!strncasecmp( argv[1], "rotate", 3))
	  process = IP_ROTATE;
     if (!strncasecmp( argv[1], "shadow", 4))
	  process = IP_SHADOW;
     if (!strncasecmp( argv[1], "sharpen", 4))
	  process = IP_SHARPEN;
     if (!strncasecmp( argv[1], "stat", 3)){
	  process = IP_STAT;
	  procout = FALSE;
     }
     if (!strncasecmp( argv[1], "translate", 2))
	  process = IP_TRANSLATE;
     if (!strncasecmp( argv[1], "zoom", 3))
	  process = IP_ZOOM;


     if (!strncasecmp( argv[1], "project", 3)){
	  process = IP_PROJECT;
     }

     if (process == IP_NONE){
	  usage();
	  exit(1);
     }

     default_options(&opt);
     opt.pname = argv[0];

     /* get options */
     for (i = 2; i < argc; i++)
	  if (argv[i][0] == '-')
	       switch (argv[i][1]){
		    
		  case 'a':
		    opt.add2file = IP_APPEND_ADD;  break;

		  case '3':
		    opt.dim = 3; break;

		  case '2':
		    opt.dim = 2; break;

		  case 's':
		    opt.sano = TRUE; break;		    
		    
		  case 'n':
		    sscanf(argv[++i], "%f", &(opt.val)); break;
		    
		  case 'm':
		    opt.mode = sliceMode(argv[++i]); break;

		  case 'v':
		    view = TRUE; break;

		  case 'p':  /* Pad option */
		    opt.pad = (float)atof(argv[++i]); break;

		  case 't':
		    sscanf(argv[++i], "%f", &(opt.thresh)); break;

		  case 'w':
		    sscanf(argv[++i], "%f", &(opt.weight)); break;

		  case 'r':
		    sscanf(argv[++i], "%f", &(opt.red)); break;
		  case 'g':
		    sscanf(argv[++i], "%f", &(opt.green)); break;
		  case 'b':
		    sscanf(argv[++i], "%f", &(opt.blue)); break;

		  case 'l':
		    sscanf(argv[++i], "%f", &(opt.low)); break;
		  case 'h':
		    sscanf(argv[++i], "%f", &(opt.high)); break;

		  case 'x': case 'X':
		    sscanf(argv[++i], "%f%*c%f", &(opt.x), &(opt.x2)); break;
		  case 'y': case 'Y':
		    sscanf(argv[++i], "%f%*c%f", &(opt.y), &(opt.y2)); break;
		  case 'z': case 'Z':
		    sscanf(argv[++i], "%f%*c%f", &(opt.z), &(opt.z2)); break;

		  case 'o': case 'O':
		    switch (argv[i][2]){
		       case 0x00:
		       case ' ':
		       case 'v':
			 opt.add2file = IP_APPEND_OVERWRITE;
			 sscanf(argv[++i], "%d", &(opt.isec));
			 break;
		       case 'x': case 'X':
			 sscanf(argv[++i], "%d", &(opt.ox)); break;
		       case 'y': case 'Y':
			 sscanf(argv[++i], "%d", &(opt.oy)); break;
		       case 'z': case 'Z':
			 sscanf(argv[++i], "%d", &(opt.oz)); break;
		       default:
			 fprintf(stderr, "%s: invalid option %s.\n",
				 argv[0], argv[i]);
			 exit(-1);
		    }
		    break;
		    
		  case 'i': case 'I':
		    switch (argv[i][2]){
		       case 'x': case 'X':
			 sscanf(argv[++i], "%d", &(opt.ix)); break;
		       case 'y': case 'Y':
			 sscanf(argv[++i], "%d", &(opt.iy)); break;
		       case 'z': case 'Z':
			 sscanf(argv[++i], "%d%*c%d", &(opt.iz),&opt.iz2);
			 opt.secs = clipMakeSecList(argv[i], &opt.nofsecs);
			 break;
		       default:
			 fprintf(stderr, "%s: invalid option %s.\n",
				 argv[0], argv[i]);
			 exit(-1);
		    }
		    break;

		  case 'c': case 'C':
		    switch (argv[i][2]){
		       case 'x': case 'X':
			 sscanf(argv[++i], "%g", &(opt.cx)); break;
		       case 'y': case 'Y':
			 sscanf(argv[++i], "%g", &(opt.cy)); break;
		       case 'z': case 'Z':
			 sscanf(argv[++i], "%g", &(opt.cz)); break;
		       default:
			 fprintf(stderr, "%s: invalid option %s.\n",
				 argv[0], argv[i]);
			 exit(-1);
		    }
		    break;

		  default:
		    fprintf(stderr, "%s: invalid option %s.\n",
			    argv[0], argv[i]);
		    exit(-1);
	       }
	  else
	       break;


     opt.fnames  = &(argv[i]);

     if (!procout){
	  /* check for at least one input file */
	  if ((argc - 1) < i){
	       usage();
	       exit(-1);
	  }
	  opt.infiles = argc - i;
     }else{
	  /* check for at least one input and one output file name. */
	  if ((argc - 2) < i){
	       usage();
	       exit(-1);
	  }
	  opt.infiles = argc - i - 1;
     }

     /* check and load files:
      * Always at least one input file.
      */
     hin.fp = fopen(argv[i], "r");
     hin.pathname = argv[i];
     if (!hin.fp){
	  fprintf(stderr, "Error opening %s\n", argv[i]);
	  return(-1);
     }
     if (mrc_head_read(hin.fp, &hin)){
	  fprintf(stderr, "Error reading %s\n", argv[i]);
	  return(-1);
     }

     /* Set output header default same as first input file. */
     hout = hin;
     
     hout.headerSize = 1024;
     hout.creatid    = 1000;

     /* Load additional input files. */
     i++;
     if (opt.infiles > 1){
	  hin2.fp = fopen(argv[i], "r");
	  hin2.pathname = argv[i];
	  if (!hin2.fp){
	       fprintf(stderr, "Error opening %s\n", argv[i]);
	       return(-1);
	  }
	  if (mrc_head_read(hin2.fp, &hin2)){
	       if (process == IP_INFO){
		    mrc_head_print(&hin);
		    printf("**********************************************\n");
		    printf("WARNING: This file is not a readable MRC file.\n");
		    printf("**********************************************\n");
	       }
	       fprintf(stderr, "Error reading %s\n", argv[i]);
	       return(-1);
	  }
	  i++;
     }


     /* Setup output file if needed. */
     if (procout){
	  opt.ofname = argv[argc - 1];
	  if (opt.add2file){
	       hout.fp = fopen(argv[argc - 1], "r+");
	       if (!hout.fp){
		    fprintf(stderr, "Error finding %s\n", argv[argc - 1]);
		    return(-1);
	       }

	       if (mrc_head_read(hout.fp, &hout)){
		    fprintf(stderr, "Error reading %s\n", argv[i]);
		    return(-1);
	       }

	       /* DNM 3/21/01: Give explicit error message here if file is
		  swapped.  6/26/02: it should be OK now */
	       /* if (hout.swapped) {
		    fprintf(stderr, "clip error: Attempting to write to a "
			    "byte-swapped file, %s\n", argv[i]);
		    return(-1);
		    } */

	  }else{
#ifndef __vms	       
	       backupstr = (char *)malloc(sizeof(char) * 
					  (strlen(argv[argc - 1]) + 2));
	       sprintf(backupstr, "%s~", argv[argc - 1]);
	       rename(argv[argc - 1], backupstr);
	       free(backupstr);
#endif

	       hout.fp = fopen(argv[argc - 1], "w+");

	       /* DNM: 3/21/01: if it's a new output file, mark header as not
		  swapped */
	       hout.swapped = 0;
	       if (!hout.fp){
		    fprintf(stderr, "Error opening %s\n", argv[argc - 1]);
		    return(-1);
	       }
	  }
     }

     opt.hin = &hin;
     opt.hout = &hout;

     /* run the selected process */
     switch(process){
	case IP_ADD:
	  puts("add: (future)");
	  break;
	case IP_AVERAGE:
	  grap_average(&hin, &hin2, &hout, &opt);
	  break;
	case IP_BRIGHTNESS:
	  clip_brightness(&hin, &hout, &opt);
	  break;
	case IP_COLOR:
	  grap_color(&hin, &hout, &opt);
	  break;
	case IP_CONTRAST:
	  clip_contrast(&hin, &hout, &opt);
	  break;
#ifndef NOFFTLIB
	case IP_CORRELATE:
	  grap_corr(&hin, &hin2, &hout, &opt);
	  break;
#endif
	case IP_EDGE:
	  clipEdge(&hin, &hout, &opt);
	  break;
	case IP_INFO:
	  mrc_head_print(&hin);
	  break;
#ifndef NOFFTLIB
	case IP_FFT:
	  clip_fft(&hin, &hout, &opt);
	  break;
	case IP_FILTER:
	  clip_bandpass_filter(&hin, &hout, &opt);
	  break;
#endif
	case IP_FLIP:
	  grap_flip(&hin, &hout, &opt);
	  break;
	case IP_PEAK:
	  puts("peak: future function\n");
	  break;
	case IP_RESIZE:
	  grap_resize(&hin, &hout, &opt);
	  break;
	case IP_ROTATE:
	  grap_rotate(&hin, &hout, &opt);
	  break;
	case IP_SHADOW:
	  clip_shadow(&hin, &hout, &opt);
	  break;
	case IP_SHARPEN:
	  clip_sharpen(&hin, &hout, &opt);
	  break;
	case IP_STAT:
	  grap_stat(&hin, &opt);
	  break;
	case IP_TRANSLATE:
	  grap_trans(&hin, &hout, &opt);
	  break;
	case IP_ZOOM:
	  if (grap_zoom(&hin, &hout, &opt))
	       fprintf(stderr, "%s: error doing zoom.\n", argv[0]);
	  break;

	default:
	  fprintf(stderr, "%s error: no process selected.\n", argv[0]);
     }

     fclose (hin.fp);
     if (procout)
	  fclose (hout.fp);

#ifdef __sgi
     if (view){
	  sprintf(viewcmd, "mrcv %s", argv[argc - 1]);
	  system(viewcmd);
     }
#endif
     return(0);
}
     
/*
 * Make a list of sections for use with the 
 * -2d and -z option. commands can be like
 * -z 0,4-10 or -z 2,3,4-8,12,19
 */
int *clipMakeSecList(char *clst, int *nofsecs)
{
     int len;
     int i;
     int *secs;
     int *tsecs;
     int range, top, r;

     secs = (int *)malloc(sizeof(int));
     len = strlen(clst);
     secs[0] = atoi(clst);
     *nofsecs = 1;
     
     for(i = 0; i < len; i++){
	  switch(clst[i]){
	       case ',':
	       (*nofsecs)++;
	       tsecs = (int *)realloc(secs, sizeof(int) * (*nofsecs));
	       secs = tsecs;
	       secs[*nofsecs - 1] = atoi(&(clst[++i]));
	       break;
	       
	       case '-':
	       top = atoi(&(clst[++i]));
	       if (top < secs[*nofsecs - 1]){
		    range = top;
		    top = secs[*nofsecs - 1];
		    secs[*nofsecs - 1] = range;
	       }
	       if (top == secs[*nofsecs - 1])
		    continue;
	       range = top - secs[*nofsecs - 1];
	       tsecs = (int *)realloc(secs, sizeof(int) * (*nofsecs + range));
	       secs = tsecs;
	       for (r = 0; r < range; r++)
		    secs[*nofsecs + r] = secs[*nofsecs - 1] + r + 1;
	       *nofsecs += range;
	       break;
	       
	     default:
	       break;
	  }
     }
     return(secs);
}








