/*
 *  $Id$
 *
 *  Author: James Kremer email: kremer@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

#ifndef MRCSLICE_H
#define MRCSLICE_H
#include "mrcfiles.h"

/* The following modes are supported by the MRC file format. */
#define SLICE_MODE_BYTE          0   /* type unsigned char                   */
#define SLICE_MODE_SHORT         1   /* type short                           */
#define SLICE_MODE_FLOAT         2   /* type float                           */
#define SLICE_MODE_COMPLEX_SHORT 3   /* 2 channels of short                  */
#define SLICE_MODE_COMPLEX_FLOAT 4   /* 2 channels of floats                 */
#define SLICE_MODE_RGB           16  /* 3 channels of bytes                  */

#define SLICE_MODE_USHORT        98  /* unsigned short                       */
#define SLICE_MODE_MAX           99  /* float data with max channels.        */
#define SLICE_MODE_UNDEFINED     -1  /* float data with max channels.        */

#define SLICE_MAX_CSIZE          3   /* max channels.                        */
#define SLICE_MAX_DSIZE          4   /* max data size of pixel in bytes.     */


typedef float Ival[SLICE_MAX_CSIZE];

union MRCdata
{
  unsigned char *b;
  short         *s;
  float         *f;
};

union VOLdata
{
  unsigned char **b;
  short         **s;
  float         **f;
};

typedef struct MRCslice
{
  union MRCdata data;
  b3dInt32 xsize;                  /* size of data.           */
  b3dInt32 ysize;
  b3dInt32 mode;                   /* type of storage         */
  b3dInt32 csize;                  /* number of data channels */
  b3dInt32 dsize;                  /* data size for pixel     */
  float  min, max, mean;
  b3dInt32 index;                  /* index of current value  */
  float  cval[SLICE_MAX_CSIZE];  /* value of current index  */
}Islice;

typedef struct MRCvolume
{
  struct MRCslice **vol;
  b3dInt32           zsize;
}Istack;

typedef struct BL3DFSvolume
{
  union VOLdata data;
  b3dInt32 xsize;
  b3dInt32 ysize;
  b3dInt32 zsize;
  b3dInt32 tsize;
  b3dInt32 mode;
  b3dInt32 csize;
  b3dInt32 dsize;
  b3dInt32 index;
  float cval[SLICE_MAX_CSIZE]; 
}Ivol;


#ifdef __cplusplus
extern "C" {
#endif

  /*****************************************************************************/
  /* library functions                                                         */
  int     sliceInit   (Islice *s, int xsize, int ysize, int mode, void *data);
  Islice *sliceCreate (int xsize, int ysize, int mode);
  void    sliceFree   (Islice *s);
  int     sliceMode   (char *mst);
  void    sliceClear  (Islice *sl, Ival val);
  int     sliceGetVal (Islice *s, int x, int y, Ival val);
  float   sliceGetPixelMagnitude(Islice *s, int x, int y);
  float   sliceGetValMagnitude(Ival val, int mode);
  int     slicePutVal (Islice *s, int x, int y, Ival val);
  int     sliceNewMode(Islice *s, int mode);
  void    sliceQuadInterpolate(Islice *sl, double x, double y, Ival val);
  Islice *sliceGradient(Islice *sin);
  Islice *sliceBox(Islice *sl, int llx, int lly, int urx, int ury);
  int     sliceBoxIn(Islice *sl, int llx, int lly, int urx, int ury);
  int     sliceResizeIn(Islice *sl, int x, int y);

  int sliceWriteMRCfile(char *filename, Islice *slice);
  Islice *sliceReadMRC(struct MRCheader *hin, int sno, char axis);
  Islice *sliceReadSubm(struct MRCheader *hin, int sno, char axis,
                        int s1, int s2, int c1, int c2);
  void sliceMMM(Islice *slice);
  int sliceMirror(Islice *s, char axis);
  int sliceAddConst(Islice *slice, Ival c);

  int sliceFloat(Islice *slice);
  int sliceComplexFloat(Islice *slice);
  int mrc_slice_wrap(struct MRCslice *s);
  int sliceWrapFFTLines(Islice *s);
  int sliceReduceMirroredFFT(Islice *s);


  /***************************************************************************/
  /* internal/test functions */
  int mrc_slice_init(struct MRCslice *s, int xsize, int ysize, 
                     int mode, void *data);
  int mrc_slice_free(struct MRCslice *s);
  struct MRCslice *mrc_slice_create(int xsize, int ysize, int mode);

  int mrc_slice_getval(struct MRCslice *s, int x, int y, float *val);
  int mrc_slice_putval(struct MRCslice *s, int x, int y, float *val);
  int mrc_slice_calcmmm(struct MRCslice *s);
  struct MRCslice *mrc_slice_real(struct MRCslice *sin);

  struct MRCslice *mrc_slice_translate(struct MRCslice *sin,
                                       double dx, double dy,
                                       int xsize, int ysize);
  struct MRCslice *mrc_slice_zoom(struct MRCslice *sin,
                                  double xz, double yz, 
                                  int xsize, int ysize,
                                  double cx, double cy);
  int mrc_slice_zooms(struct MRCslice *sin, struct MRCslice *sout,
                      double xz, double yz,
                      int xsize, int ysize,
                      double cx,    double cy);
  struct MRCslice *mrc_slice_rotate(struct MRCslice *slin, double angle,
                                    int xsize, int ysize,
                                    double cx, double cy);
  int mrc_slice_rotates(struct MRCslice *slin, struct MRCslice *sout,
                        double angle, int xsize, int ysize,
                        double cx, double cy);
  float mrc_slice_getmagnitude(struct MRCslice *s, int x, int y);
  struct MRCslice *mrc_slice_resize(struct MRCslice *slin, int nx, int ny);
  struct MRCslice *mrc_slice_getvol(struct MRCvolume *v, int sno, char axis);
  int mrc_slice_putvol(struct MRCvolume *v, struct MRCslice *s, 
                       int sno, char axis);
  struct MRCslice *slice_mat_filter(struct MRCslice *sin, float *mat, int dim);
  int mrc_slice_lie_img(struct MRCslice *sin, 
                        struct MRCslice *mask, double alpha);     
  int mrc_slice_interpolate(struct MRCslice *sin,
                            double x, double y, float *val);
  int mrc_slice_lie(struct MRCslice *sin, double in, double alpha);
  int mrc_slice_valscale(struct MRCslice *s, double scale);

  void mrc_slice_mat_getimat(struct MRCslice *sin, int x, int y, int dim, 
                             float *mat);
  float mrc_slice_mat_mult(float *m1, float *m2, int dim);
  int mrc_vol_wrap(struct MRCvolume *v);
  int sliceGetXSize(Islice *slice);
  int sliceGetYSize(Islice *slice);
  int sliceMultConst(Islice *slice, Ival c);





#ifdef __cplusplus
}
#endif


/* macro functions */
/* no error checking. */
#define sliceGetComplexFloatVal(s,x,y,val) memcpy((val), ((s)->data.f + ( 2 * ((x) + ((y) * (s)->xsize)))), 8)
#define slicePutComplexFloatVal(s,x,y,val) memcpy(((s)->data.f + ( 2 * ((x) + ((y) * (s)->xsize)))), (val), 8)
#define slicePutFloatVal(s,x,y,val) ((s)->data.f[ (x) + ((y) + ((s)->xsize))]=(val))



#endif /* islice.h */
