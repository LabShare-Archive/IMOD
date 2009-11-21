/*
 *  clip -- Command Line Image Proccesing
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */

#include <stdlib.h>
#include <string.h>
#include "mrcfiles.h"
#include "mrcslice.h"
#include "clip.h"
#include "imodconfig.h"
#include "b3dutil.h"

void usage(void)
{
  char *name = "clip";

  fprintf(stderr, 
          "%s: Command Line Image Processing. %s, %s %s\n",
          name,
          VERSION_NAME, __DATE__, __TIME__);
  imodCopyright();
  fprintf(stderr, "----------------------------------------------------\n");

  fprintf(stderr, "%s usage:\n", name);
  fprintf(stderr, 
          "%s [process] [options] [input files...] [output file]\n", name);
  fprintf(stderr, "\nprocess:\n");
  /*     fprintf(stderr, "\tadd         - Add images together.(future)\n"); */
  fprintf(stderr, "\taverage     - Average files together.\n");
  fprintf(stderr, "\tbrightness  - Increase or decrease brightness.\n");
  fprintf(stderr, "\tcolor       - Add false color.\n");
  fprintf(stderr, "\tcontrast    - Increase or decrease contrast.\n");
  fprintf(stderr, "\tcorrelation - Do a auto/cross correlation.\n");
  fprintf(stderr, "\tdiffusion   - Do 2-D anisotropic diffusion on slices.\n");
  fprintf(stderr, "\tgradient    - Compute gradient as in 3dmod.\n");
  fprintf(stderr, "\tgraham      - Apply Graham filter as in 3dmod.\n");
  fprintf(stderr, "\tinfo        - Print information to stdout.\n");
  fprintf(stderr, "\tfft         - Do a fft or inverse fft transform.\n");
  fprintf(stderr, "\tfilter      - Do a bandpass filter.\n");
  fprintf(stderr, "\tflip..      - Flip image about various axes.\n");
  /*     fprintf(stderr, "\tpeak        - Find peaks in image.\n"); */
  fprintf(stderr, "\tjoinrgb     - Join 3 byte files into an RGB file.\n");
  fprintf(stderr, "\tlaplacian   - Apply Laplacian filter as in 3dmod.\n");
  fprintf(stderr, "\tmedian      - Do median filtering.\n");
  fprintf(stderr, "\tprewitt     - Apply Prewitt filter as in 3dmod.\n");
  fprintf(stderr, "\tresize      - Cut out and/or pad image data.\n");
  fprintf(stderr, "\trotx        - Rotate volume by -90 about X axis.\n");
  /* fprintf(stderr, "\trotation    - Rotate image\n"); */
  fprintf(stderr, "\tshadow      - Increase or decrease image shadows.\n");
  fprintf(stderr, "\tsharpen     - Sharpen image as in 3dmod.\n");
  fprintf(stderr, "\tsmooth      - Smooth image as in 3dmod.\n");
  fprintf(stderr, "\tsobel       - Apply Sobel filter as in 3dmod.\n");
  fprintf(stderr, "\tsplitrgb    - Split RGB image file into 3 byte files.\n");
  fprintf(stderr, "\tstats       - Print some stats on image file.\n");
  /* fprintf(stderr, "\ttranslate   - translate image.\n");
  fprintf(stderr, "\tzoom        - magnify image.\n"); */
  fprintf(stderr, "\noptions:\n");
  fprintf(stderr, "\t[-v]  view output data.\n");
  fprintf(stderr, "\t[-3d] or [-2d] treat image as 3d (default) or 2d.\n");
  fprintf(stderr, "\t[-s] Switch, [-n #] Amount; Depends on function.\n");
  fprintf(stderr, "\t[-n #] [-l #] Iterations and Gaussian sigma for smoothing.\n");
  fprintf(stderr, "\t[-h #] [-l #] values for high/low pass filters.\n");
  fprintf(stderr, "\t[-cc #] [-l #] [-k #] values for anisotropic diffusion.\n");
  fprintf(stderr, "\t[-r #] [-g #] [-b #] red, green, blue values.\n");
  /*  fprintf(stderr, "\t[-x #,#] [-y #,#] [-z #,#] input variables.\n"); */
  fprintf(stderr, "\t[-cx #]  [-cy #]  [-cz #]  center coords.\n");
  fprintf(stderr, "\t[-ix #]  [-iy #]  [-iz #]  input sizes.\n");
  fprintf(stderr, "\t[-ox #]  [-oy #]  [-oz #]  output sizes.\n");
  fprintf(stderr, "\t[-a] Append output to file.\n");
  fprintf(stderr, "\t[-ov #] Overwrite output starting at section #\n");
  fprintf(stderr, "\t[-m (mode#)] output data mode.\n");
  fprintf(stderr, "\t[-p (pad#)] pad empty data value.\n");
  fprintf(stderr, "\t[-1] Number Z values from 1 instead of 0.\n");
  fprintf(stderr, "\n");
}

/* 11/4/04: switch to standard out and standard format to some extent */
void show_error(char *reason)
{
  printf("ERROR: %s\n", reason);
}

void show_warning(char *reason)
{
  printf("WARNING: %s\n", reason);
}

void show_status(char *info)
{
  printf("%s", info);
  fflush(stdout);
}

void default_options(ClipOptions *opt)
{
  opt->hin = opt->hin2 = opt->hout = NULL;
  opt->x  = IP_DEFAULT; opt->x2 = IP_DEFAULT;
  opt->y  = IP_DEFAULT; opt->y2 = IP_DEFAULT;
  opt->z  = IP_DEFAULT; opt->z2 = IP_DEFAULT;
  opt->ix = IP_DEFAULT; opt->iy = IP_DEFAULT; 
  opt->iz = IP_DEFAULT; opt->iz2 = IP_DEFAULT;
  opt->ox = IP_DEFAULT; opt->oy = IP_DEFAULT; opt->oz = IP_DEFAULT;
  opt->cx = IP_DEFAULT; opt->cy = IP_DEFAULT; opt->cz = IP_DEFAULT;
  opt->outBefore = opt->outAfter = IP_DEFAULT;
  opt->red = IP_DEFAULT; opt->green = IP_DEFAULT; opt->blue = IP_DEFAULT;
  opt->high = 0; opt->low = IP_DEFAULT;
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
  opt->fromOne = FALSE;
  opt->ofname = NULL;
}



int main( int argc, char *argv[] )
{
  MrcHeader hin, hin2, hout;
  ClipOptions opt;

  int process = IP_NONE;  /* command to run.             */
  int view    = FALSE;    /* view file at end?           */
  int procout = TRUE;     /* will process write output?. */
  int i, j;
  int retval = 0;

  char viewcmd[1024];
  char *backupstr;
  char *progname = imodProgName(argv[0]);

  if (argc < 3){
    usage();
    exit(3);
  }

  opt.command = argv[1];

  /* Find which process to run */
  /*if (!strncmp( argv[1], "add", 3))
    process = IP_ADD; */
  if ((!strncmp( argv[1], "avg", 3)) || 
      (!strncmp( argv[1], "average", 3)) )
    process = IP_AVERAGE;
  if (!strncmp( argv[1], "brightness", 3))
    process = IP_BRIGHTNESS;
  if (!strncmp( argv[1], "color", 3))
    process = IP_COLOR;
  if (!strncmp( argv[1], "contrast", 3))
    process = IP_CONTRAST;

  if (!strncmp( argv[1], "correlation", 3))
    process = IP_CORRELATE; 
  if (!strncmp( argv[1], "diffusion", 3))
    process = IP_DIFFUSION; 

  if (!strncmp( argv[1], "info", 3)){
    process = IP_INFO;
    procout = FALSE;
  }

  if (!strncmp( argv[1], "fft", 3))
    process = IP_FFT;
  if (!strncmp( argv[1], "filter", 3))
    process = IP_FILTER;

  if (!strncmp( argv[1], "flip", 4))
    process = IP_FLIP;
  if (!strncmp( argv[1], "rotx", 4))
    process = IP_FLIP;
  if (!strncmp( argv[1], "gradient", 4))
    process = IP_GRADIENT;
  if (!strncmp( argv[1], "graham", 4))
    process = IP_GRAHAM;
  if (!strncmp( argv[1], "laplacian", 2))
    process = IP_LAPLACIAN;
  if (!strncmp( argv[1], "median", 2))
    process = IP_MEDIAN;
  /*  if (!strncmp( argv[1], "peak", 3)){
    process = IP_PEAK;
    procout = FALSE;
    } */
  if (!strncmp( argv[1], "prewitt", 2))
    process = IP_PREWITT;
  if (!strncmp( argv[1], "resize", 3))
    process = IP_RESIZE;
  /* if (!strncmp( argv[1], "rotate", 3))
     process = IP_ROTATE; */
  if (!strncmp( argv[1], "shadow", 4))
    process = IP_SHADOW;
  if (!strncmp( argv[1], "sharpen", 4))
    process = IP_SHARPEN;
  if (!strncmp( argv[1], "smooth", 2))
    process = IP_SMOOTH;
  if (!strncmp( argv[1], "sobel", 2))
    process = IP_SOBEL;
  if (!strncmp( argv[1], "stat", 3)){
    process = IP_STAT;
    procout = FALSE;
  }
  /* if (!strncmp( argv[1], "translate", 2))
    process = IP_TRANSLATE;
  if (!strncmp( argv[1], "zoom", 3))
     process = IP_ZOOM;
  if (!strncmp( argv[1], "project", 3)){
    process = IP_PROJECT;
    } */

  if (!strncmp( argv[1], "splitrgb", 3)){
    process = IP_SPLITRGB;
  }
  if (!strncmp( argv[1], "joinrgb", 3)){
    process = IP_JOINRGB;
  }

  if (process == IP_NONE){
    usage();
    exit(1);
  }

  default_options(&opt);
  opt.pname = progname;

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

      case '1':
        opt.fromOne = TRUE; break;

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

      case 'k':
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
                  progname, argv[i]);
          exit(3);
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
                  progname, argv[i]);
          exit(3);
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
        case 'c': case 'C':
          sscanf(argv[++i], "%f", &(opt.thresh)); break;
        default:
          fprintf(stderr, "%s: invalid option %s.\n",
                  progname, argv[i]);
          exit(3);
        }
        break;

      default:
        fprintf(stderr, "%s: invalid option %s.\n",
			    progname, argv[i]);
        exit(3);
      }
    else
      break;


  opt.fnames  = &(argv[i]);

  if (!procout){
    /* check for at least one input file */
    if ((argc - 1) < i){
      usage();
      exit(3);
    }
    opt.infiles = argc - i;
  }else{
    /* check for at least one input and one output file name. */
    if ((argc - 2) < i){
      usage();
      exit(3);
    }
    opt.infiles = argc - i - 1;
  }

  /* check and load files:
   * Always at least one input file.
   */
  hin.fp = fopen(argv[i], "rb");
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
     
  /* DNM 4/2/04: in addition to setting header size, need to zero out "next" */
  hout.headerSize = 1024;
  hout.sectionSkip = 0;
  hout.next = 0;
  hout.creatid    = 1000;

  /* Load additional input files. */
  i++;
  if (opt.infiles > 1){
    hin2.fp = fopen(argv[i], "rb");
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
      hout.fp = fopen(argv[argc - 1], "rb+");
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

    } else if (process != IP_SPLITRGB) {

      /* DNm 10/20/03: switch to calling routine for backup file */
      imodBackupFile(argv[argc - 1]);

      hout.fp = fopen(argv[argc - 1], "wb+");
        
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

  /* Massage the Z values if numbered from one */
  if (opt.fromOne) {
    if (opt.add2file == IP_APPEND_OVERWRITE)
      opt.isec--;
    if (opt.nofsecs != IP_DEFAULT)
      for (j = 0; j < opt.nofsecs; j++)
        opt.secs[j]--;
    if (opt.cz != IP_DEFAULT)
        opt.cz--;
    if (opt.dim == 2) {
      if (opt.iz != IP_DEFAULT)
        opt.iz--;
      if (opt.iz2 != IP_DEFAULT)
        opt.iz2--;
    }
  }

  /* run the selected process */
  switch(process){
  case IP_ADD:
    puts("add: (future)");
    break;
  case IP_AVERAGE:
    retval = grap_average(&hin, &hin2, &hout, &opt);
    break;
  case IP_BRIGHTNESS:
  case IP_CONTRAST:
  case IP_SHADOW:
  case IP_RESIZE:
    retval = clip_scaling(&hin, &hout, &opt, process);
    break;
  case IP_COLOR:
    retval = grap_color(&hin, &hout, &opt);
    break;
  case IP_CORRELATE:
    retval = grap_corr(&hin, &hin2, &hout, &opt);
    break;
  case IP_DIFFUSION:
    retval = clipDiffusion(&hin, &hout, &opt);
    break;
  case IP_GRADIENT:
  case IP_GRAHAM:
  case IP_PREWITT:
  case IP_SOBEL:
    retval = clipEdge(&hin, &hout, &opt, process);
    break;
  case IP_INFO:
    retval = mrc_head_print(&hin);
    break;
  case IP_FFT:
    retval = clip_fft(&hin, &hout, &opt);
    break;
  case IP_FILTER:
    /* opt.dim = 2; */
    retval = clip_bandpass_filter(&hin, &hout, &opt);
    break;
  case IP_FLIP:
    retval = grap_flip(&hin, &hout, &opt);
    break;
  case IP_JOINRGB:
    retval = clip_joinrgb(&hin, &hin2, &hout, &opt);
    break;
  case IP_LAPLACIAN:
  case IP_SMOOTH:
  case IP_SHARPEN:
    retval = clip_convolve(&hin, &hout, &opt, process);
    break;
  case IP_MEDIAN:
    retval = clipMedian(&hin, &hout, &opt);
    break;
  case IP_PEAK:
    retval = puts("peak: future function\n");
    break;
  case IP_ROTATE:
    retval = grap_rotate(&hin, &hout, &opt);
    break;
  case IP_SPLITRGB:
    retval = clip_splitrgb(&hin, &opt);
    break;
  case IP_STAT:
    retval = grap_stat(&hin, &opt);
    break;
  case IP_TRANSLATE:
    retval = grap_trans(&hin, &hout, &opt);
    break;
  case IP_ZOOM:
    if (grap_zoom(&hin, &hout, &opt)) {
      fprintf(stderr, "%s: error doing zoom.\n", progname);
      retval = 1;
    }
    break;

  default:
    fprintf(stderr, "%s error: no process selected.\n", progname);
    retval = 1;
  }

  if (retval)
    exit(retval);
  fclose (hin.fp);
  if (procout && process != IP_SPLITRGB)
    fclose (hout.fp);

  if (view){
    sprintf(viewcmd, "3dmod %s", argv[argc - 1]);
    system(viewcmd);
  }
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

/*
$Log$
Revision 3.21  2008/07/04 21:30:42  mast
Fixed splitrgb the right way this time

Revision 3.20  2008/01/10 05:19:38  mast
Fixed double close of file with splitrgb

Revision 3.19  2007/11/23 01:05:39  mast
Added usage line for smoothing options

Revision 3.18  2007/06/13 17:04:43  sueh
bug# 1019 Setting hout.sectionSkip in main.

Revision 3.17  2007/02/04 21:19:48  mast
Eliminated mrcspectral includes

Revision 3.16  2006/06/23 17:12:02  mast
Added rotx option

Revision 3.15  2005/01/28 05:42:22  mast
Set low to IP_DEFAULT so default would work for diffusion

Revision 3.14  2005/01/27 05:55:17  mast
Added anisotropic diffusion option

Revision 3.13  2005/01/17 17:07:15  mast
Removed rotate, translate, zoom; used new typedefs

Revision 3.12  2005/01/07 23:54:25  mast
Fix brightness listing in usage

Revision 3.11  2005/01/07 20:06:52  mast
Added various filters including simple 3dmod ones and median filter

Revision 3.10  2004/11/04 17:04:21  mast
Switched to producing and using non-mirrored FFTs

Revision 3.9  2004/07/07 19:25:29  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.8  2004/04/22 19:07:56  mast
Zeroed out number of extra header bytes in output file to fixe problems
with headers that have extra data that is not being copied.

Revision 3.7  2004/01/16 18:09:02  mast
Added splitrgb and joinrgb options

Revision 3.6  2003/10/24 03:09:26  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.5  2002/11/05 23:25:26  mast
*** empty log message ***

Revision 3.4  2002/11/05 23:24:46  mast
Change to call copyright function

Revision 3.3  2002/07/31 20:11:27  mast
Changed copyright to use defined variable

Revision 3.2  2002/06/26 16:47:22  mast
Allowed writing to swapped files

*/
