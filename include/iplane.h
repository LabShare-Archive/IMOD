/*  IMOD VERSION 2.02
 *
 *  iplane.h -- Image model plane header.
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

#ifndef _IPLANE_H_
#define _IPLANE_H_

#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Create a new Plane and initalize. */
Iplane *imodPlanesNew(int size);

/* Delete plane(s) created with imodPlanesNew() */
void    imodPlaneDelete(Iplane *plane);

/* Initalize and edit plane functions. */
void    imodPlaneInit(Iplane *plane);
void    imodPlaneRotate(Iplane *plane, double angle, Ipoint *pnt);
void    imodPlaneAxisRotate(Iplane *plane, double angle, int axis);
void    imodPlaneTranslate(Iplane *plane, Ipoint *pnt);
void    imodPlaneSetPN(Iplane *plane, Ipoint *pnt, Ipoint *nor);

/* Clipping plane functions. */
int     imodPlaneClip(Iplane *plane, Ipoint *pnt);
int     imodPlanesClip(Iplane *plane, int nplanes, Ipoint *pnt);
void    imodClipsInitialize(IclipPlanes *clips);
void    imodClipsFixCount(IclipPlanes *clips, b3dUInt32 flags);
int     imodClipsRead(IclipPlanes *clips, FILE *fin);
void    imodPlaneSetFromClips(IclipPlanes *objClips, IclipPlanes *glbClips,
                              Iplane *plane, int maxPlanes, int *nPlanes);

#ifdef __cplusplus
}
#endif
#endif
