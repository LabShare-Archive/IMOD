/*  IMOD VERSION 2.02
 *
 *  colormap.h
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

/* Store multiple colormaps */
#define CMAP_LEVEL_ARRAY_SIZE 4
#define CMAP_RAMPSIZE 256

struct Cmap_Levels{
    int black;
    int white;
  };



/* Public Functions */

extern void cmap_init(int black, int white, int rampbase, int rampmax);
extern void cmap_brightness(int val);
extern void cmap_contrast(int val);
extern void cmap_blacklevel(int val);
extern void cmap_whitelevel(int val);
extern void cmap_getlevels(int *black, int *white);
extern void cmap_toggle_falsecolor(void);
extern void cmap_toggle_reverse(void);
extern int  cmap_index(int index, int *black, int *white);
void cmap_get_levels(int *black, int *white);

/* Private internal Functions */
extern void adjustcmap_pf(int *low, int *high, int rampbase);
extern void cmap_falsecolor(int gray, short *red, short *green, short *blue);


/* Private Global Variables */
extern int    Cmap_Rampbase;
extern int    Cmap_Falsecolor;
extern int    Cmap_Reverse;
extern int    Cmap_CurrentMap;
extern struct Cmap_Levels Cmap_Levels_Array[];

