/*  IMOD VERSION 2.50
 *
 *  imod_edit.c -- Funtions for handeling model structures.
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
    Revision 3.1  2002/01/28 16:45:25  mast
    Removed imod_nearest function, which was used only by xyz window and did
    not work

*/

#include <math.h>
#include "imod.h"
#include "xzap.h"

int imod_distance( float *x, float *y, struct Mod_Point *pnt);

/* moves point by adding x,y, and z to current model point. */
/* used by information window.                              */
/* returns the index of the current point.                  */
int imod_movepoint(int x, int y, int z)
{
     struct Mod_Object *obj = NULL;
     struct Mod_Contour *cont = NULL;
     int index = Model->cindex.point;

     obj = imodel_object_get(Model);
     if (obj == NULL)
	  return(0);
     cont = imodContourGet(Model);
     if (cont == NULL)
	  return(0);
     
     if (  (obj->flags & IMOD_OBJFLAG_OPEN)
	 ||(obj->flags & IMOD_OBJFLAG_WILD)
	 ||(obj->flags & IMOD_OBJFLAG_SCAT)){

       cont->pts[index].x += x;
       cont->pts[index].y += y;
       cont->pts[index].z += z;
     }
     
     return(Model->cindex.point);
}

/* sets the current graphics position to the same as the current model
 * position. 
 *
 * Sets current time also.
 */

int imod_setxyzmouse()
{
  return(ivwRedraw(App->cvi));
}

int ivwRedraw(ImodView *vw)
{
     Imod *imod = ivwGetModel(vw);
     Iobj  *obj = NULL;
     Icont *cont = NULL;
     int   index;

     if ( (index = imod->cindex.point) < 0){
	  imodDraw(vw, IMOD_DRAW_MOD);
	  return(1);
     }

     cont = imodContourGet(imod);
     if (cont == NULL){
	  imodDraw(vw, IMOD_DRAW_MOD);
	  return(1);
     }
     if ((cont->pts == NULL) || (cont->psize <= index)){
	  imodDraw(vw, IMOD_DRAW_MOD);
	  return(1);
     }

     obj = imodObjectGet(imod);
     if (iobjFlagTime(obj))
	  ivwSetTime(vw, cont->type);

     ivwSetLocationPoint(vw, &(cont->pts[index]));

     return(0);
}


/* DNM 1/23/02: deleted  imod_nearest, was used only by xyz window */


/* DNM 6/17/01: pass the selection size as a parameter so that windows can
   make it zoom-dependent */
int imod_obj_nearest(struct Mod_Object *obj, 
		     struct Mod_Index *index,
		     struct Mod_Point *pnt,
		     float selsize)
{
    
    struct Mod_Contour *cont;
    int i, pindex;
    int distance = -1;
    int temp_distance;
    int ctime;
    int cz = (int)(pnt->z + 0.5f);
    int twod = 0;
    
    /* Don't report points not in our time. DNM - unless time is 0*/
    ivwGetTime(App->cvi, &ctime);
    
    /* Ignore Z value if 2d image. */
    twod = (!(App->cvi->dim & 4));
    
    for (i = 0; i < obj->contsize; i++){
	
	cont = &(obj->cont[i]);
	if ((ctime) && (obj->flags & IMOD_OBJFLAG_TIME) && (cont->type) &&
	    (cont->type != ctime)) continue;
	
	for(pindex = 0; pindex < cont->psize; pindex++){
	    
	    if ((twod) || ( cz == ((int)(cont->pts[pindex].z + 0.5f))))
		
		if ((  (pnt->x - cont->pts[pindex].x) < selsize)
		    && (  (cont->pts[pindex].x - pnt->x) < selsize)
		    && (  (pnt->y - cont->pts[pindex].y) < selsize) 
		    && (  (cont->pts[pindex].y - pnt->y) < selsize))
		    {
			
			temp_distance = imod_distance( &(cont->pts[pindex].x),
						      &(cont->pts[pindex].y),
						      pnt);
			
			if (distance == -1){
			    distance = temp_distance;
			    index->contour = i;
			    index->point   = pindex;
			}
			
			if (distance > temp_distance){
			    distance = temp_distance;
			    index->contour = i;
			    index->point   = pindex;
			}
		    }
	}
    }
    return(distance);
}


int imod_distance( float *x, float *y, struct Mod_Point *pnt)
{

     double distance;
     int retval;

     distance = ((*x - pnt->x) * (*x - pnt->x)) + 
	  ((*y - pnt->y) * (*y - pnt->y));
     
     distance = sqrt(distance);

     retval = (int)(distance + 0.5);
     
     return(retval);
}

/* This is called when moving all contours in an object */
void imod_contour_move(int ob)
{

     int oldob;
     int oldco;
     int oldpt;
     int co;
     struct Mod_Object *obj;  
     struct Mod_Contour *cont;
     struct Mod_Contour *ocont;
     
     oldob =  Model->cindex.object;
     oldco =  Model->cindex.contour;
     oldpt =  Model->cindex.point;


     ocont = imodContourGet(Model);
     if (!ocont)
	  return;
     
     if (ob == oldob)
	  return;
     
     if (ob > Model->objsize)
	  return;
     
     if (ob < 0)
	  return;
     
     obj = &(Model->obj[ob]);

     /* DNM: switch to this Add and Remove method to avoid problems with
	labels */
     imodObjectAddContour(obj, ocont);

     obj = &(Model->obj[oldob]);
     imodObjectRemoveContour(obj, oldco);
     /* DNM 3/29/01: delete old code. */
     return;
}


