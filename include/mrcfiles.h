/*  IMOD VERSION 2.50
 *
 *  mrcfiles.h
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
    Revision 3.6  2004/01/05 17:26:17  mast
    Renamed imin/imax to outmin/outmax; changed mrcRead... from void to int
    for error returns, and eliminated mode-specific calls

    Revision 3.5  2003/11/18 19:20:51  mast
    changes for 2GB problem on Windows

    Revision 3.4  2003/02/21 22:18:06  mast
    implement new b3d types

    Revision 3.3  2002/07/31 17:39:04  mast
    *** empty log message ***

    Revision 3.2  2002/07/31 17:29:29  mast
    Redefine header entries to comply with MRC image2000 standard
    Add declaration for mrc_set_cmap_stamp

    Revision 3.1  2002/06/26 16:53:13  mast
    Added prototype for mrc_swap_header

*/

#ifndef MRCFILES_H
#define MRCFILES_H

#include <stdio.h>
#include <hvemtypes.h>
#include <imodconfig.h>

#ifndef FALSE
#define FALSE       0           /*false for boolean*/
#endif
#ifndef TRUE
#define TRUE        1           /*true for boolean*/
#endif

#ifndef SEEK_SET
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2
#endif



#define MRC_IDTYPE_MONO   0
#define MRC_IDTYPE_TILT   1
#define MRC_IDTYPE_TILTS  2
#define MRC_IDTYPE_LINA   3
#define MRC_IDTYPE_LINS   4 

#define MRC_SCALE_LINEAR  1
#define MRC_SCALE_POWER   2
#define MRC_SCALE_LOG     3
#define MRC_SCALE_BKG     4

#define MRC_MODE_BYTE          0
#define MRC_MODE_SHORT         1
#define MRC_MODE_FLOAT         2
#define MRC_MODE_COMPLEX_SHORT 3
#define MRC_MODE_COMPLEX_FLOAT 4
#define MRC_MODE_RGB           16
#define MRC_MODE_COMPRESS_RGB  17

#define MRC_WINDOW_DATASIZE 1
#define MRC_WINDOW_FULL     2
#define MRC_WINDOW_NTSC     3

#define MRC_RAMP_LIN 1
#define MRC_RAMP_EXP 2
#define MRC_RAMP_LOG 3


#define MRC_LABEL_SIZE         80
#define MRC_NEXTRA             16
#define MRC_NLABELS            10
#define MRC_HEADER_SIZE        1024   /* Length of Header is 1024 Bytes. */
#define MRC_MAXCSIZE           3


typedef struct  /*complex floating number*/
{
  b3dFloat a;
  b3dFloat b;

} ComplexFloat;

typedef struct  /*complex short number*/
{
  b3dInt16 a;
  b3dInt16 b;

} ComplexShort;



struct MRCheader
{
  b3dInt32   nx;         /*  # of Columns                  */
  b3dInt32   ny;         /*  # of Rows                     */
  b3dInt32   nz;         /*  # of Sections.                */
  b3dInt32   mode;       /*  given by #define MRC_MODE...  */

  b3dInt32   nxstart;    /*  Starting point of sub image.  */
  b3dInt32   nystart;
  b3dInt32   nzstart;

  b3dInt32   mx;         /* Number of rows to read.        */
  b3dInt32   my;
  b3dInt32   mz;

  b3dFloat   xlen;       /* length of x element in um.     */
  b3dFloat   ylen;       /* get scale = xlen/nx ...        */
  b3dFloat   zlen;

  b3dFloat   alpha;      /* cell angles, ignore */
  b3dFloat   beta;
  b3dFloat   gamma;

  b3dInt32   mapc;       /* map coloumn 1=x,2=y,3=z.       */
  b3dInt32   mapr;       /* map row     1=x,2=y,3=z.       */
  b3dInt32   maps;       /* map section 1=x,2=y,3=z.       */

  b3dFloat   amin;
  b3dFloat   amax;
  b3dFloat   amean;
  
  b3dInt16   ispg;       /* image type */
  b3dInt16   nsymbt;     /* space group number */


  /* 64 bytes */

  b3dInt32   next;
  b3dInt16   creatid;  /* Creator id, hvem = 1000, DeltaVision = -16224 */

  
  b3dByte    blank[30];
  
  b3dInt16   nint;
  b3dInt16   nreal;
  b3dInt16   sub;
  b3dInt16   zfac;

  b3dFloat   min2;
  b3dFloat   max2;
  b3dFloat   min3;
  b3dFloat   max3;
  b3dFloat   min4;
  b3dFloat   max4;

  /*  UINT   extra[MRC_NEXTRA];*/

  /* HVEM extra data */
  /* DNM 3/16/01: divide idtype into two shorts */
  b3dInt16   idtype;
  b3dInt16   lens;
  b3dInt16   nd1;     /* Devide by 100 to get float value. */
  b3dInt16   nd2;
  b3dInt16   vd1;
  b3dInt16   vd2;
  b3dFloat   tiltangles[6];  /* 0,1,2 = original:  3,4,5 = current */

#ifdef OLD_STYLE_HEADER
  /* before 2.6.20 */
  /* DNM 3/16/01: redefine the last three floats as wavelength numbers */
  b3dInt16   nwave;   /* # of wavelengths and values */
  b3dInt16   wave1;
  b3dInt16   wave2;
  b3dInt16   wave3;
  b3dInt16   wave4;
  b3dInt16   wave5;
  b3dFloat   zorg;           /* origin */
  
  b3dFloat   xorg;
  b3dFloat   yorg;
#else
  /* MRC 2000 standard */
  b3dFloat   xorg;
  b3dFloat   yorg;
  b3dFloat   zorg;
  b3dByte    cmap[4];
  b3dByte    stamp[4];
  b3dFloat   rms;
#endif

  b3dInt32 nlabl;
  b3dByte  labels[MRC_NLABELS][MRC_LABEL_SIZE + 1];

  /* Internal data not stored in file header */
  b3dUByte *symops;
  FILE   *fp;
  int    pos;
  struct LoadInfo *li;
  int    headerSize;
  int    swapped;

  char *pathname;
  char *filedesc;
  char *userData;
};


/* to get from index to model coords -> scale -> subtract org -> rotate */
/* rotate by current tilt angles ? */

/* Used for loading a subsection of a 3-D data file. */
struct LoadInfo
{

  /* Sub area to load. */
  int xmin;      
  int xmax;
  int ymin;
  int ymax;
  int zmin;
  int zmax;
  
  int zinc;      /* read only every 'zinc' section, future */
     
  int ramp;      /* Contrast ramp type. */
  int scale;
  int black;     /* Change contrast values. */
  int white;
  
  int imaginary; /* For complex data. if true view i data, 
                    else real data.  */
  
  int axis;      /* 1=x, 2=y, 3=z , 0 = default */
  
  float slope;
  float offset;
  float smin,smax; /* Scale to min and max values of input */
  int contig;    /* Load idata in contigous memory */
  
  int outmin, outmax; /* clamp values to outmin and outmax after scaling. */
  
  int   plist;         /* Size of piece list.         */
  float opx, opy, opz; /* origin of pieces.           */
  float px, py, pz;    /* size of final pieced image. */
  int   pdz; 
  /* number of zsections that have data. */
  int *pcoords;      /* The piece list.             */

};


struct TiltInfo
{

  float *tilt;
  float  axis_x;
  float  axis_y;
  float  axis_z;
  float  scale_x;
  float  scale_y;
  float  scale_z;
  float  alpha;
  float  beta;
  float  gamma;

};


#ifdef __cplusplus
extern "C" {
#endif


/******************************** Header functions **************************/
int mrc_head_read (FILE *fin,  struct MRCheader *hdata);
int mrc_head_write(FILE *fout, struct MRCheader *hdata);
int mrc_head_label(struct MRCheader *hdata, char *label);
int mrc_head_new  (struct MRCheader *hdata, int x, int y, int z, int mode);
int mrc_byte_mmm  (struct MRCheader *hdata, unsigned char **idata);
int mrc_head_label_cp(struct MRCheader *hin, struct MRCheader *hout);

void mrc_get_scale(struct MRCheader *h, float *xs, float *ys, float *zs);
void mrc_set_scale(struct MRCheader *h, double x, double y, double z);
void mrc_coord_cp(struct MRCheader *hout, struct MRCheader *hin);
	  

/************************* Write image data functions ************************/
int mrc_write_byte (FILE *fout, struct MRCheader *hdata, unsigned char **data);
int mrc_write_idata(FILE *fout, struct MRCheader *hdata, void *data[]);
int mrc_data_new   (FILE *fout, struct MRCheader *hdata);
int mrc_write_slice(void *buf, FILE *fout, struct MRCheader *hdata, 
		    int slice, char axis);

/************************ Read image data functions **************************/
void *mrc_read_image (FILE *fin, struct MRCheader *hdata, int z);
float mrc_read_point (FILE *fin, struct MRCheader *hdata, int x, int y, int z);
void *mrc_mread_slice(FILE *fin, struct MRCheader *hdata,
		      int slice, char axis);
int mrc_read_slice(void *buf, FILE *fin, struct MRCheader *hdata, 
		   int slice, char axis);

  unsigned char **mrcGetDataMemory(struct LoadInfo *li, int xysize, int zsize,
                                   int pixsize);
  void mrcFreeDataMemory(unsigned char **idata, int contig, int zsize);
  float mrcGetComplexScale();
  float mrcComplexSminSmax(float inMin, float inMax, float *outMin, 
                           float *outMax);

unsigned char **read_mrc_byte(FILE *fin, struct MRCheader *hdata, 
			      struct LoadInfo *li);
unsigned char **mrc_read_byte(FILE *fin, struct MRCheader *hdata, 
			      struct LoadInfo *li,
			      void (*func)(char *));

int mrcReadSectionByte(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int z);
int mrcReadZByte(struct MRCheader *hdata, struct LoadInfo *li,
		  unsigned char *buf, int z);
int mrcReadYByte(struct MRCheader *hdata, struct LoadInfo *li,
		  unsigned char *buf, int y);
int mrcReadZ(struct MRCheader *hdata, struct LoadInfo *li,
	      unsigned char *buf, int cz);
int mrcReadY(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int cy);
int mrcReadSection(struct MRCheader *hdata, struct LoadInfo *li,
		    unsigned char *buf, int z);



/* misc stdio functions */
int  loadtilts(struct TiltInfo *ti, struct MRCheader *hdata);
int  getloadinfo(struct MRCheader *hdata,  struct LoadInfo *li); 
int  mrc_init_li(struct LoadInfo *li, struct MRCheader *hd);
int  mrc_plist_li(struct LoadInfo *li, struct MRCheader *hdata, char *fname);
int  mrc_plist_load(struct LoadInfo *li, struct MRCheader *hdata, FILE *fin);
int  mrc_plist_proc(struct LoadInfo *li, int nx, int ny, int nz);
int  mrc_plist_create(struct LoadInfo *li, int nx, int ny, int nz, int nfx, 
		      int nfy, int ovx, int ovy);
int  iiPlistLoadF(FILE *fin, struct LoadInfo *li, int nx, int ny, int nz);
int  iiPlistLoad(char *filename, struct LoadInfo *li, int nx, int ny, int nz);
void mrc_liso(struct MRCheader *hdata, struct LoadInfo *li);
int mrc_fix_li(struct LoadInfo *li, int nx, int ny, int nz);
int get_loadinfo(struct MRCheader *hdata, struct LoadInfo *li);
unsigned char *get_byte_map(float slope, float offset, int outmin, int outmax);
unsigned char *get_short_map(float slope, float offset, int outmin, int outmax,
			     int ramptype, int swapbytes, int signedint);


/************************ Internal functions *********************************/
int getfilename(char *name, char *prompt);
void mrc_default_status(char *string);
int mrc_getdcsize(int mode, int *dsize, int *csize);
int fgetline(FILE *fp, char s[],int limit);
void mrc_swap_shorts(short int *data, int amt);
void mrc_swap_longs(int *data, int amt);
void mrc_swap_floats(float *data, int amt);
int mrc_big_seek(FILE *fp, int base, int size1, int size2, int flag);
void mrc_swap_header(struct MRCheader *hdata);
void mrc_set_cmap_stamp(struct MRCheader *hdata);

int b3dFseek(FILE *fp, int offset, int flag);
size_t b3dFread(void *buf, size_t size, size_t count, FILE *fp);
size_t b3dFwrite(void *buf, size_t size, size_t count, FILE *fp);

#ifdef __cplusplus
}
#endif
#endif
