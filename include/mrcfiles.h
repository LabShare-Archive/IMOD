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
  FLOAT a;
  FLOAT b;

} ComplexFloat;

typedef struct  /*complex short number*/
{
  short a;
  short b;

} ComplexShort;



struct MRCheader
{
  int     nx;         /*  # of Columns                  */
  int     ny;         /*  # of Rows                     */
  int     nz;         /*  # of Sections.                */
  int     mode;       /*  given by #define MRC_MODE...  */

  int     nxstart;    /*  Starting point of sub image.  */
  int     nystart;
  int     nzstart;

  int     mx;         /* Number of rows to read.        */
  int     my;
  int     mz;

  FLOAT   xlen;       /* length of x element in um.     */
  FLOAT   ylen;       /* get scale = xlen/nx ...        */
  FLOAT   zlen;

  FLOAT   alpha;      /* cell angles, ignore */
  FLOAT   beta;
  FLOAT   gamma;

  int     mapc;       /* map coloumn 1=x,2=y,3=z.       */
  int     mapr;       /* map row     1=x,2=y,3=z.       */
  int     maps;       /* map section 1=x,2=y,3=z.       */

  FLOAT   amin;
  FLOAT   amax;
  FLOAT   amean;
  
  short     ispg;       /* image type */
  short     nsymbt;     /* space group number */


  /* 64 bytes */

  int     next;
  short   creatid;  /* Creator id, hvem = 1000, DeltaVision = -16224 */

  
  char    blank[30];
  
  short   nint;
  short   nreal;
  short   sub;
  short   zfac;

  float   min2;
  float   max2;
  float   min3;
  float   max3;
  float   min4;
  float   max4;

  /*  UINT   extra[MRC_NEXTRA];*/

    /* HVEM extra data */
     /* DNM 3/16/01: divide idtype into two shorts */
    short   idtype;
    short   lens;
    short   nd1;     /* Devide by 100 to get float value. */
    short   nd2;
    short   vd1;
    short   vd2;
    FLOAT   tiltangles[6];  /* 0,1,2 = original:  3,4,5 = current */

#ifdef OLD_STYLE_HEADER
     /* before 2.6.20 */
     /* DNM 3/16/01: redefine the last three floats as wavelength numbers */
    short   nwave;   /* # of wavelengths and values */
    short   wave1;
    short   wave2;
    short   wave3;
    short   wave4;
    short   wave5;
     FLOAT   zorg;           /* origin */

     FLOAT   xorg;
     FLOAT   yorg;
#else
   /* MRC 2000 standard */
     FLOAT   xorg;
     FLOAT   yorg;
     FLOAT   zorg;
     char    cmap[4];
     char    stamp[4];
     FLOAT   rms;
#endif

  int   nlabl;
  char  labels[MRC_NLABELS][MRC_LABEL_SIZE + 1];

  UCHAR  *symops;
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
     float smin,smax; /* Scale to min and max values */
     int contig;    /* Load idata in contigous memory */

     int imin, imax; /* clamp values to imin and imax after scaling. */

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

unsigned char **read_mrc_byte(FILE *fin, struct MRCheader *hdata, 
			      struct LoadInfo *li);
unsigned char **mrc_read_byte(FILE *fin, struct MRCheader *hdata, 
			      struct LoadInfo *li,
			      void (*func)(char *));

void mrcReadSectionByte(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int z);
void mrcReadZByte_Byte(struct MRCheader *hdata, struct LoadInfo *li,
		       unsigned char *buf, int cz);
void mrcReadYByte_Byte(struct MRCheader *hdata, struct LoadInfo *li,
		       unsigned char *buf, int cy);
void mrcReadZByte_Short(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int cz);
void mrcReadYByte_Short(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int cy);
void mrcReadZByte_Float(struct MRCheader *hdata, struct LoadInfo *li,
			       int dsize,
			       unsigned char *buf, int cz);
void mrcReadYByte_Float(struct MRCheader *hdata, struct LoadInfo *li,
			       int dsize,
			       unsigned char *buf, int cy);
void mrcReadZByte_RGB(struct MRCheader *hdata, struct LoadInfo *li,
		       unsigned char *buf, int cz);
void mrcReadZByte(struct MRCheader *hdata, struct LoadInfo *li,
		  unsigned char *buf, int z);
void mrcReadYByte(struct MRCheader *hdata, struct LoadInfo *li,
		  unsigned char *buf, int y);
void mrcReadZ(struct MRCheader *hdata, struct LoadInfo *li,
	      unsigned char *buf, int cz);
void mrcReadY(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int cy);
void mrcReadSection(struct MRCheader *hdata, struct LoadInfo *li,
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
unsigned char *get_byte_map(float slope, float offset, int imin, int imax);
unsigned char *get_short_map(float slope, float offset, int imin, int imax,
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

#ifdef __cplusplus
}
#endif
#endif
