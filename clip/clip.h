/*
 *  clip.h -- Header file for command line image proccessing.
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#ifndef CLIP_H
#define CLIP_H

/* processes */
enum {IP_NONE = 0, IP_ADD, IP_AVERAGE, IP_VARIANCE, IP_STANDEV, IP_BRIGHTNESS, IP_COLOR, 
      IP_CONTRAST, IP_CORRELATE, IP_DIFFUSION, IP_FFT, IP_FILTER, IP_FLIP,
      IP_GRADIENT, IP_SUBTRACT, IP_MULTIPLY, IP_DIVIDE,
      IP_GRAHAM, IP_INFO, IP_JOINRGB, IP_LAPLACIAN, IP_MEDIAN, IP_PEAK,
      IP_PREWITT, IP_UNWRAP, IP_QUADRANT, IP_UNPACK, IP_HISTOGRAM, 
      IP_PROJECT, IP_RESIZE, IP_ROTATE, IP_SHADOW, IP_SHARPEN, IP_SMOOTH,
      IP_SOBEL, IP_SPLITRGB, IP_STAT, IP_TRANSLATE, IP_ZOOM, IP_TRUNCATE};


#define IP_DEFAULT -99999

#define IP_APPEND_FALSE     0
#define IP_APPEND_OVERWRITE 1
#define IP_APPEND_ADD       2

typedef struct Grap_options
{     
  char  *pname;   /* Program name */
  char  *command; /* process command */
  MrcHeader *hin;
  MrcHeader *hin2;
  MrcHeader *hout;
  int process;    /* The process */
  int x,  y,  z;
  int x2, y2, z2;
  int   ix, iy, iz, iz2;
  int   ox, oy, oz;
  float cx, cy, cz;
  float high,  low;
  float red, green, blue;
  float thresh;
  float weight;
  float pad;
  int   mode;
  int   dim;
  int   infiles;
  char  **fnames;
  int   sano;
  int   add2file;
  int   isec;
  float val;
  int   nofsecs;  /* Number of sections in section list. */
  int   *secs;    /* Section list for 2-D */
  int   outBefore;  /* Number of blank sections to output before and after */
  int   outAfter;

  int   ocanresize; /* output size can be changed. */
  int   ocanchmode;  /* output mode can be changed. */
  int   fromOne;     /* Number Z from 1 not 0 */
  char *ofname;
  char *plname;
  int   newXoverlap, newYoverlap;
} ClipOptions;


/* clip.c */
void usage(void);
void show_error(const char *format, ...);
void show_warning(char *reason);
void show_status(char *info);
void default_options(ClipOptions *opt);
int *clipMakeSecList(char *clst, int *nofsecs);

/* clip_proc.c */
int clip_scaling(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clipEdge(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clip_flip(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clip_color(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clip_quadrant(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clip2d_color(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clip_average(MrcHeader *h1, MrcHeader *h2, MrcHeader *hout, ClipOptions *opt);
int clip_multdiv(MrcHeader *h1, MrcHeader *h2, MrcHeader *hout, ClipOptions *opt);
int clipUnpack(MrcHeader *h1, MrcHeader *h2, MrcHeader *hout, ClipOptions *opt);
int clip_joinrgb(MrcHeader *h1, MrcHeader *h2, MrcHeader *hout,
                 ClipOptions *opt);
int clip_splitrgb(MrcHeader *h1, ClipOptions *opt);
int clip2d_average(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clip_parxyz(Istack *v,
		int xmax, int ymax, int zmax,
		float *rx, float *ry, float *rz);
int clip_stat3d(Istack *v);
int clip_get_stat3d(Istack *v,
		    float *rmin, float *rmax, float *rmean,
		    int *rx, int *ry, int *rz);
int clip_stat(MrcHeader *hin, ClipOptions *opt);
int clipHistogram(MrcHeader *hin, ClipOptions *opt);
int clip_convolve(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clipMedian(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clipDiffusion(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int write_vol(Islice **vol, MrcHeader *hout);
int free_vol(Islice **vol, int z);

/* clip_transform.c */
int grap_rotate(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int grap_2dtrans(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int grap_trans(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int grap_zoom(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);

/* clip_io.c */
void set_input_options(ClipOptions *opt, MrcHeader *hin);
int set_output_options(ClipOptions *opt, MrcHeader *hout);
int set_options(ClipOptions *opt, MrcHeader *hin, MrcHeader *hout);
int clipWriteSlice(Islice *slice, MrcHeader *hout, ClipOptions *opt, int kSec, 
                   int *zWrite, int freeSlice);
Istack *grap_volume_read(MrcHeader *hin, ClipOptions *opt);
int grap_volume_write(Istack *v,  MrcHeader *hout, ClipOptions *opt);
int grap_volume_free(Istack *v);
int mrc_head_print(MrcHeader *data);
int set_mrc_coords(ClipOptions *opt);

/* fft.c */
int slice_fft(Islice *slice);
int clip_fftvol3(Istack *v, int idir);
int clip_fftvol(Istack *v);
int clip_3dfft(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clip_fft(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);

void mrcToDFFT(float buf[], int nx, int ny, int idir);

/* filter.c */
int clip_3dfilter(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
int clip_bandpass_filter(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt);
     

/* correlation.c */
void corr_getmax(Islice *is, int sa, int xm, int ym, float *x, float *y);
Islice *clip_slice_corr(Islice *s1, Islice *s2);
int clip_corr3d(MrcHeader *hin1, MrcHeader *hin2, MrcHeader *hout,
                ClipOptions *opt);
int grap_3dcorr(MrcHeader *hin1, MrcHeader *hin2, MrcHeader *hout,
                ClipOptions *opt);
int grap_corr(MrcHeader *hin1, MrcHeader *hin2, MrcHeader *hout,
              ClipOptions *opt);
int padfloat_volume(Istack *v, float pad);
int clip_cor_scalevol(Istack *v);
double parabolic_fit(double *outX, double *outY, double i[3][3]);
     
#endif /* clip.h */
