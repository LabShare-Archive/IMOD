/*****************************************************************************
 *                                                                           *
 *   FILE: recfile.h                                                         *
 *                                                                           *
 *   PURPOSE: Read HVEM3D rec files from an IBM PC.                          *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0 Oct 1995					             *
 *									     *
 *       James Kremer  kremer@boulder.colorado.edu			     *
 *****************************************************************************
 *   Copyright (C) 1995 by Boulder Laboratory for 3-Dimensional Fine         *
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
#define HEADSZ          80
#define DIRHDSZ 122
#define DIRRECSZ        11
#define DATAHDSZ 60
#define DATARECSZ 100

#define XDATA 0
#define YDATA 1
#define ZDATA 2

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

typedef short Data[3];

typedef struct dirhdst {
     char    name[HEADSZ];

     short   lastdir;   /* number of imod contours */
     short   lplane;
     short   lobject;
     char    ltype;     /* number of imod objects */

     int    nxtbyte;
     char    dirfull;
     char    speed;
     char    falign;
     short   maxdir;
     short   extra2;
     short   extra3;
     short   extra4;
     float   edmag;
     float   mag;      /* in kx      */
     float   secthick; /* in microns */
     short   xcent;
     short   ycent;
     short   zcent;
     short   ndel;
     
     char    centered;
     char    aligned;
     float   unitsize; /* tablet pixel size in mm */
} HvemHead;

typedef struct{

     int  del;
     short npts;
     char  type;
     char  subtype;
     short color;
     short fcolor;
     float area;
     float perim;
     Data  *points;
     
} HvemContour;

HvemHead *hvem3D_read_head(FILE *fin);
void hvem3D_free_head(HvemHead *head);
HvemContour *hvem3D_read_contour(FILE *fin, int index);
void hvem3D_free_contour(HvemContour *cont);
