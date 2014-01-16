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
 */

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "iimage.h"
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
  fprintf(stderr, "%s [process] [options] [input files...] [output file]\n", name);
  fprintf(stderr, "\nprocess:\n");
  fprintf(stderr, "\tadd         - Add images together.\n");
  fprintf(stderr, "\taverage     - Average files together.\n");
  fprintf(stderr, "\tbrightness  - Increase or decrease brightness.\n");
  fprintf(stderr, "\tcolor       - Add false color.\n");
  fprintf(stderr, "\tcontrast    - Increase or decrease contrast.\n");
  fprintf(stderr, "\tcorrelation - Do a auto/cross correlation.\n");
  fprintf(stderr, "\tdiffusion   - Do 2-D anisotropic diffusion on slices.\n");
  fprintf(stderr, "\tdivide      - Divide one image volume by another.\n");
  fprintf(stderr, "\tgradient    - Compute gradient as in 3dmod.\n");
  fprintf(stderr, "\tgraham      - Apply Graham filter as in 3dmod.\n");
  fprintf(stderr, "\thistogram   - Print histogram of values.\n");
  fprintf(stderr, "\tinfo        - Print information to stdout.\n");
  fprintf(stderr, "\tfft         - Do a fft or inverse fft transform.\n");
  fprintf(stderr, "\tfilter      - Do a bandpass filter.\n");
  fprintf(stderr, "\tflip..      - Flip image about various axes.\n");
  /*     fprintf(stderr, "\tpeak        - Find peaks in image.\n"); */
  fprintf(stderr, "\tjoinrgb     - Join 3 byte files into an RGB file.\n");
  fprintf(stderr, "\tlaplacian   - Apply Laplacian filter as in 3dmod.\n");
  fprintf(stderr, "\tmedian      - Do median filtering.\n");
  fprintf(stderr, "\tmultiply    - Multiple one image volume by another.\n");
  fprintf(stderr, "\tprewitt     - Apply Prewitt filter as in 3dmod.\n");
  fprintf(stderr, "\tquadrant    - Correct quadrant disparities from 4-port camera.\n");
  fprintf(stderr, "\tresize      - Cut out and/or pad image data.\n");
  fprintf(stderr, "\trotx        - Rotate volume by -90 about X axis.\n");
  /* fprintf(stderr, "\trotation    - Rotate image\n"); */
  fprintf(stderr, "\tshadow      - Increase or decrease image shadows.\n");
  fprintf(stderr, "\tsharpen     - Sharpen image as in 3dmod.\n");
  fprintf(stderr, "\tsmooth      - Smooth image as in 3dmod.\n");
  fprintf(stderr, "\tsobel       - Apply Sobel filter as in 3dmod.\n");
  fprintf(stderr, "\tsplitrgb    - Split RGB image file into 3 byte files.\n");
  fprintf(stderr, "\tstandev     - Compute standard deviation for averaged images.\n");
  fprintf(stderr, "\tstats       - Print some stats on image file.\n");
  fprintf(stderr, "\tsubtract    - Subtract one image volume from another.\n");
  fprintf(stderr, "\ttruncate    - Limit image values at low or high end.\n");
  fprintf(stderr, "\tunwrap      - Undo a wraparound of integer intensity values.\n");
  fprintf(stderr, "\tunpack      - Unpack 4-bit data into bytes [& multiply by gain "
          "ref].\n");
  fprintf(stderr, "\tvariance    - Compute variance for averaged images.\n");
  /* fprintf(stderr, "\ttranslate   - translate image.\n");
  fprintf(stderr, "\tzoom        - magnify image.\n"); */
  fprintf(stderr, "\noptions:\n");
  fprintf(stderr, "\t[-v]  view output data.\n");
  fprintf(stderr, "\t[-3d] or [-2d] treat image as 3d (default) or 2d.\n");
  fprintf(stderr, "\t[-s] Switch, [-n #] Amount; Depends on function.\n");
  fprintf(stderr, "\t[-n #] [-l #] Iterations and Gaussian sigma for smoothing.\n");
  fprintf(stderr, "\t[-h #] [-l #] values for high/low pass filters or truncation.\n");
  fprintf(stderr, "\t[-cc #] [-l #] [-k #] values for anisotropic diffusion.\n");
  fprintf(stderr, "\t[-r #] [-g #] [-b #] red, green, blue values.\n");
  fprintf(stderr, "\t[-x #,#]  [-y #,#]  starting and ending input coords.\n");
  fprintf(stderr, "\t[-cx #]  [-cy #]  [-cz #]  center coords.\n");
  fprintf(stderr, "\t[-ix #]  [-iy #]  [-iz #]  input sizes.\n");
  fprintf(stderr, "\t[-ox #]  [-oy #]  [-oz #]  output sizes.\n");
  fprintf(stderr, "\t[-a] Append output to file.\n");
  fprintf(stderr, "\t[-ov #] Overwrite output starting at section #\n");
  fprintf(stderr, "\t[-m (mode#)] Output data mode.\n");
  fprintf(stderr, "\t[-p (pad#)] Pad empty data value.\n");
  fprintf(stderr, "\t[-1] Number Z values from 1 instead of 0.\n");
  fprintf(stderr, "\t[-P file] Name of piece list file for stats on a montage.\n");
  fprintf(stderr, "\t[-O #,#]  Overlaps in X and Y in displayed montage.\n");
  fprintf(stderr, "\n");
}

/* 11/4/04: switch to standard out and standard format to some extent */
void show_error(const char *format, ...)
{
  char errorMess[512];
  va_list args;
  va_start(args, format);
  vsprintf(errorMess, format, args);
  printf("ERROR: %s\n", errorMess);
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
  opt->high = IP_DEFAULT; opt->low = IP_DEFAULT;
  opt->thresh = IP_DEFAULT;
  opt->weight = IP_DEFAULT;
  opt->pad    = IP_DEFAULT;
  opt->process = IP_NONE;
  opt->dim = 3;
  opt->add2file = IP_APPEND_FALSE;
  opt->sano = FALSE;
  opt->val = IP_DEFAULT;
  opt->mode = IP_DEFAULT;
  opt->nofsecs = IP_DEFAULT;
  opt->secs = NULL;
  opt->ocanresize = TRUE;
  opt->ocanchmode = TRUE;
  opt->fromOne = FALSE;
  opt->ofname = NULL;
  opt->plname = NULL;
  opt->newXoverlap = IP_DEFAULT;
  opt->newYoverlap = IP_DEFAULT;
}



int main( int argc, char *argv[] )
{
  MrcHeader hin, hin2, hout;
  ClipOptions opt;

  int process = IP_NONE;  /* command to run.             */
  int view    = FALSE;    /* view file at end?           */
  int procout = TRUE;     /* will process write output?. */
  int needtwo = FALSE;    /* Does process need two input files? */
  int i, j;
  int retval = 0;

  char viewcmd[1024];
  char *progname = imodProgName(argv[0]);

  if (argc < 3){
    usage();
    exit(3);
  }

  opt.command = argv[1];

  /* Find which process to run */
  if (!strncmp( argv[1], "add", 3)) {
    process = IP_ADD;
    needtwo = TRUE;
  }
  if ((!strncmp( argv[1], "avg", 3)) || 
      (!strncmp( argv[1], "average", 3)) )
    process = IP_AVERAGE;
  if (!strncmp( argv[1], "standev", 4))
    process = IP_STANDEV;
  if (!strncmp( argv[1], "variance", 3))
    process = IP_VARIANCE;
  if (!strncmp( argv[1], "multiply", 3)) {
    process = IP_MULTIPLY;
    needtwo = TRUE;
  }
  if (!strncmp( argv[1], "subtract", 3)) {
    process = IP_SUBTRACT;
    needtwo = TRUE;
  }
  if (!strncmp( argv[1], "divide", 3)) {
    process = IP_DIVIDE;
    needtwo = TRUE;
  }
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
  if (!strncmp( argv[1], "histogram", 2)) {
    process = IP_HISTOGRAM;
    procout = FALSE;
  }
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
  if (!strncmp( argv[1], "stat", 4)){
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

  if (!strncmp( argv[1], "truncate", 3))
    process = IP_TRUNCATE;
  if (!strncmp( argv[1], "unwrap", 3))
    process = IP_UNWRAP;
  if (!strncmp( argv[1], "quadrant", 2)) {
    process = IP_QUADRANT;
    opt.dim = 2;
  } 
  if (!strncmp( argv[1], "unpack", 3))
    process = IP_UNPACK;
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
  opt.process = process;
  opt.pname = progname;

  /* get options */
  for (i = 2; i < argc; i++)
    if (argv[i][0] == '-')
      switch (argv[i][1]){
		    
      case 'a':
        opt.add2file = IP_APPEND_ADD;  break;

      case '3':
        if (process != IP_QUADRANT)
          opt.dim = 3;
        break;

      case '2':
        opt.dim = 2; break;

      case '1':
        opt.fromOne = TRUE; break;

      case 's':
        opt.sano = TRUE; break;		    
		    
      case 'n':
        sscanf(argv[++i], "%f", &(opt.val)); break;
		    
      case 'm':
        opt.mode = sliceMode(argv[++i]); 
        if (opt.mode == SLICE_MODE_UNDEFINED) {
          fprintf(stderr, "%s: invalid mode entry %s.\n", progname, argv[i]);
          exit(3);
        }
        if (opt.mode == SLICE_MODE_SBYTE || opt.mode == SLICE_MODE_UBYTE) {
          overrideWriteBytes(opt.mode == SLICE_MODE_SBYTE ? 1 : 0);
          opt.mode = 0;
        }
        break;

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
        sscanf(argv[++i], "%d%*c%d", &(opt.x), &(opt.x2)); break;
      case 'y': case 'Y':
        sscanf(argv[++i], "%d%*c%d", &(opt.y), &(opt.y2)); break;
        /*case 'z': case 'Z':
          sscanf(argv[++i], "%f%*c%f", &(opt.z), &(opt.z2)); break; */

      case 'o':
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
          fprintf(stderr, "%s: invalid option %s.\n", progname, argv[i]);
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
          fprintf(stderr, "%s: invalid option %s.\n", progname, argv[i]);
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
          fprintf(stderr, "%s: invalid option %s.\n", progname, argv[i]);
          exit(3);
        }
        break;

      case 'P':
        opt.plname = strdup(argv[++i]);
        break;

      case 'O':
        sscanf(argv[++i], "%d%*c%d", &opt.newXoverlap, &opt.newYoverlap);
        break;

      default:
        fprintf(stderr, "%s: invalid option %s.\n", progname, argv[i]);
        exit(3);
      }
    else
      break;


  opt.fnames  = &(argv[i]);

  if (!procout) {
    /* check for at least one input file */
    if ((argc - 1) < i){
      usage();
      exit(3);
    }
    opt.infiles = argc - i;
  } else {
    /* check for at least one input and one output file name. */
    if ((argc - 2) < i){
      usage();
      exit(3);
    }
    opt.infiles = argc - i - 1;
  }

  if (opt.x != IP_DEFAULT) {
    if (opt.cx != IP_DEFAULT || opt.ix != IP_DEFAULT) {
      fprintf(stderr, "You cannot use -x together with -cx or -ix\n");
      exit(1);
    }
  }
  if (opt.y != IP_DEFAULT) {
    if (opt.cy != IP_DEFAULT || opt.iy != IP_DEFAULT) {
      fprintf(stderr, "You cannot use -y together with -cy or -iy\n");
      exit(1);
    }
  }


  /* check and load files:
   * Always at least one input file.
   */
  hin.fp = iiFOpen(argv[i], "rb");
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

  /* 7/20/11: Consolidate all changes for output header into this call; eliminate 
   * setting swapped to 0 below; this header is replaced if appending
   * There is also a new header made in lots of places, not sure how much this is used */
  mrcInitOutputHeader(&hout);

  /* Load additional input files. */
  i++;
  if (opt.infiles > 1){
    hin2.fp = iiFOpen(argv[i], "rb");
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


  /* Setup output file if needed and there are enough - otherwise let the process give 
     the error message . */
  if (procout && (!needtwo || opt.infiles > 1)){
    opt.ofname = argv[argc - 1];
    if (opt.add2file){
      hout.fp = iiFOpen(argv[argc - 1], "rb+");
      if (!hout.fp){
        fprintf(stderr, "Error finding %s\n", argv[argc - 1]);
        return(-1);
      }

      if (mrc_head_read(hout.fp, &hout)){
        fprintf(stderr, "Error reading %s\n", argv[i]);
        return(-1);
      }

    } else if (process != IP_SPLITRGB) {

      /* DNm 10/20/03: switch to calling routine for backup file */
      if (!getenv("IMOD_NO_IMAGE_BACKUP"))
        imodBackupFile(argv[argc - 1]);

      if (process == IP_FFT && hin.mode != MRC_MODE_COMPLEX_FLOAT && 
          b3dOutputFileType() == IIFILE_TIFF) {
        printf("WARNING: clip - Writing an MRC file; TIFF files cannot contain FFTs\n");
        overrideOutputType(IIFILE_MRC);
      }

      hout.fp = iiFOpen(argv[argc - 1], "wb+");
        
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
  case IP_AVERAGE:
  case IP_STANDEV:
  case IP_VARIANCE:
  case IP_ADD:
  case IP_SUBTRACT:
    retval = clip_average(&hin, &hin2, &hout, &opt);
    break;
  case IP_MULTIPLY:
  case IP_DIVIDE:
    retval = clip_multdiv(&hin, &hin2, &hout, &opt);
    break;
  case IP_UNPACK:
    retval = clipUnpack(&hin, &hin2, &hout, &opt);
    break;
  case IP_BRIGHTNESS:
  case IP_CONTRAST:
  case IP_SHADOW:
  case IP_RESIZE:
  case IP_TRUNCATE:
  case IP_UNWRAP:
    retval = clip_scaling(&hin, &hout, &opt);
    break;
  case IP_COLOR:
    retval = clip_color(&hin, &hout, &opt);
    break;
  case IP_QUADRANT:
    retval = clip_quadrant(&hin, &hout, &opt);
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
    retval = clipEdge(&hin, &hout, &opt);
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
    retval = clip_flip(&hin, &hout, &opt);
    break;
  case IP_HISTOGRAM:
    retval = clipHistogram(&hin, &opt);
    break;
  case IP_JOINRGB:
    retval = clip_joinrgb(&hin, &hin2, &hout, &opt);
    break;
  case IP_LAPLACIAN:
  case IP_SMOOTH:
  case IP_SHARPEN:
    retval = clip_convolve(&hin, &hout, &opt);
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
    retval = clip_stat(&hin, &opt);
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
  iiFClose (hin.fp);
  if (procout && process != IP_SPLITRGB)
    iiFClose (hout.fp);

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

