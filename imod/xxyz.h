/*  IMOD VERSION 2.02
 *
 *  xxyz.h -- Include file for xyz.c, the XYZ Window.
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

#ifndef XXYZ_H
#define XXYZ_H

#define XYZ_BSIZE 6
#define XYZ_STRINGSIZE 128

struct xxyzwin
{
     struct ViewInfo *vi;   /* Image Data information.              */
     Widget dialog;         /* The top widget of the xyz window     */
     Widget glw;            /* The drawing widget of the xyz window */
     XID    context;
     
     unsigned char *fdata;  /* tmp data storage nx by ny            */
     B3dCIImage *xydata;    /* Draw buffer for Z slices.            */
     B3dCIImage *xzdata;    /* Draw buffer for Y slices.            */
     B3dCIImage *yzdata;    /* Draw buffer for X slices.            */

     int winx, winy;         /* Size of xyz window.                  */
     int overclear;          /* Clear overlay planes if set.         */
     int exposed;
     float zoom;

     int lx, ly, lz;
};


/* Functions */
#ifdef __cplusplus
extern "C" {
#endif

int xxyz_open( struct ViewInfo *vi);  /* open the xxyz window             */
int xyz_draw(struct ViewInfo *vi);    /* force update of the xxyz window. */
int xyz_draw_showslice(struct ViewInfo *vi);

#ifdef __cplusplus
}
#endif

#endif /* xxyz.h */

