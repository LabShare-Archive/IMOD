/*  IMOD VERSION 2.02
 *
 *  iplane.c -- graphic plane library elements.
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

#include "imodel.h"

Iplane *imodPlanesNew(int size)
{
     Iplane *pl;
     int p;

     if (size <= 0)
	  return(NULL);
     
     pl = (Iplane *)malloc(sizeof(Iplane) * size);
     if (pl){
	  for(p = 0; p < size; p++)
	       imodPlaneInit(&(pl[p]));
     }
     return(pl);
}

void imodPlaneDelete(Iplane *plane)
{
     if (plane)
	  free(plane);
}

void imodPlaneInit(Iplane *plane)
{
     /* default plane z <= 0 */
     plane->a = plane->b = plane->d = 0.0f;
     plane->c = -1.0f;
}

void imodPlaneAxisRotate(Iplane *plane, double angle, int axis)
{
     Ipoint rpt;
     Imat *mat = imodMatNew(3);
     
     imodMatRot(mat, angle, axis);
     imodMatTransform(mat, (Ipoint *)plane, &rpt);
     imodMatDelete(mat);
     plane->a = rpt.x;
     plane->b = rpt.y;
     plane->c = rpt.z;
     return;
}

/* rotate plane by data in point x = alpha, y = beta, z = gamma. */
void imodPlaneRotate(Iplane *plane, double angle, Ipoint *pnt)
{
     Ipoint rpt;
     Imat *mat = imodMatNew(3);

     imodMatRotateVector(mat, angle, pnt);
     imodMatTransform(mat, (Ipoint *)plane, &rpt);
     imodMatDelete(mat);
     plane->a = rpt.x;
     plane->b = rpt.y;
     plane->c = rpt.z;
     return;
}

/* Translate plane by given point (x, y, z) */
void imodPlaneTranslate(Iplane *plane, Ipoint *pnt)
{
     plane->a += pnt->x;
     plane->b += pnt->y;
     plane->c += pnt->z;
     return;
}

/* set plane parameters from a point on a plane and a normal */
void imodPlaneSetPN(Iplane *plane, Ipoint *pnt, Ipoint *nor)
{
     plane->a = nor->x;
     plane->b = nor->y;
     plane->c = nor->z;
     plane->d = (pnt->x * nor->x) + (pnt->y * nor->y) + (pnt->z * nor->z);
     return;
}

/* Returns True if point is in the half space defined by clipping plane. */
/* Returns False if point needs to be clipped.                           */
int imodPlaneClip(Iplane *plane, Ipoint *pnt)
{
     if ( ((plane->a * pnt->x) + (plane->b * pnt->y) + 
	 (plane->c * pnt->z) + plane->d) >= 0)
	  return(1);
     else
	  return(0);
}

/* Check a series of planes */
int imodPlanesClip(Iplane *plane, int nplanes, Ipoint *pnt)
{
     int pn;
     for(pn = 0; pn < nplanes; pn++){
	  if (!imodPlaneClip(&(plane[pn]), pnt))
	       return(0);
     }
     return(1);
}


/* Initialize all clip planes and other parameters of the clip plane set */
void   imodClipsInitialize(IclipPlanes *clips)
{
  int i;
  clips->count       = 0;
  clips->flags = 0;
  clips->trans = 0;
  clips->plane = 0;
  for (i = 0; i < IMOD_CLIPSIZE; i++) {
    clips->normal[i].x = clips->normal[i].y = 0.0f;
    clips->normal[i].z = -1.0f;
    clips->point[i].x = clips->point[i].y = clips->point[i].z = 0.0f;
  }
}

/* Read all the clip planes of a set based on the size of the chunk */
int imodClipsRead(IclipPlanes *clips, FILE *fin)
{
  int size, nread;
  size = imodGetInt(fin);
  nread = (size - SIZE_CLIP) / 24 + 1;

  imodGetBytes(fin, (unsigned char *)&clips->count, 4);
  imodGetFloats(fin, (float *)&clips->normal[0], 3 * nread);
  imodGetFloats(fin, (float *)&clips->point[0], 3 * nread);
  return(ferror(fin));
}

/* Fix the count of clip planes for old files or for a count set to zero
   when written in a new file.
   If clip is 0 and the first plane is not in initialized state,
   turn off first flag and set clip 1; otherwise, if old model
   and clip is nonzero, set the first flag and make sure clip is 1 */
void imodClipsFixCount(IclipPlanes *clips, b3dUInt32 flags)
{
  if (!clips->count && 
      (clips->point[0].x || clips->point[0].y || 
       clips->point[0].z || clips->normal[0].x || 
       clips->normal[0].y || clips->normal[0].z != -1.f)) {
    clips->flags &= 254;
    clips->count = 1;
  } else if (clips->count && !(flags & IMODF_MULTIPLE_CLIP)) {
    clips->flags |= 1;
    clips->count = 1;
  }
}

/* Set plane parameters from clipping plane sets
   Either objClips or glbClips can be null; i.e. one set of planes can be used
   Set nPlanes to zero before first call */
void imodPlaneSetFromClips(IclipPlanes *objClips, IclipPlanes *glbClips,
                           Iplane *plane, int maxPlanes, int *nPlanes)
{
  int i, doGlobal = 1;
  if (objClips) {
    if (objClips->flags & (1 << 7))
      doGlobal = 0;
    for (i = 0; i < objClips->count; i++)
      if ((objClips->flags & (1 << i)) && *nPlanes < maxPlanes) {
        imodPlaneSetPN(&plane[*nPlanes], &objClips->point[i], 
                       &objClips->normal[i]);
        (*nPlanes)++;
      }
  }

  if (doGlobal && glbClips) {
    for (i = 0; i < glbClips->count; i++)
      if ((glbClips->flags & (1 << i)) && *nPlanes < maxPlanes) {
        imodPlaneSetPN(&plane[*nPlanes], &glbClips->point[i], 
                       &glbClips->normal[i]);
        (*nPlanes)++;
      }
  }
}
