/*  IMOD VERSION 2.02
 *
 *  mrc.h - Guideline for mrc header info structure.
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


/*
 *  int and float data types are 32 bits.
 *
 */

#ifndef MRC_H
#define MRC_H

#include <stdio.h>

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
  float a;
  float b;

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
  float   xlen;       /* length of x element in um.     */
  float   ylen;       /* get scale = xlen/nx ...        */
  float   zlen;
  float   alpha;      /* cell angles, ignore */
  float   beta;
  float   gamma;
  int     mapc;       /* map coloumn 1=x,2=y,3=z.       */
  int     mapr;       /* map row     1=x,2=y,3=z.       */
  int     maps;       /* map section 1=x,2=y,3=z.       */
  float   amin;
  float   amax;
  float   amean;
  int     ispg;       /* image type */
  int     nsymbt;     
  unsigned int extra[MRC_NEXTRA];

    /* HVEM extra data */
    long    idtype;  
    short   nd1;     /* Devide by 100 to get float value. */
    short   nd2;
    short   vd1;
    short   vd2;
    float   tiltangles[9];  /* 0,1,2 = original:  3,4,5 = current */
    float   zorg;           /* origin */

  float   xorg;
  float   yorg;
  int   nlabl;
  char  labels[MRC_NLABELS][MRC_LABEL_SIZE + 1];
};






