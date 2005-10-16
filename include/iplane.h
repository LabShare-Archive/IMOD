/*
 *  iplane.h -- Image model plane header.
 *
 *  Authors: James Kremer, David Mastronarde   email: mast@colorado.edu
 */

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
  void imodClipsTrans(IclipPlanes *clips, Imat *mat, Imat *mat2);

#ifdef __cplusplus
}
#endif
#endif
