/*  IMOD VERSION 2.30
 *
 *  iobj.c -- Funtions for handeling model object structures.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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
    Revision 3.1  2003/02/21 22:22:02  mast
    Use new b3d types

*/

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <float.h>

#include <imodel.h>


Iobj *imodObjectNew()
{
     Iobj *obj;
     obj = imodObjectsNew(1);
     if (!obj)
	  return(NULL);
     return(obj);
}

/* returns an array of objects initialized to default values */
Iobj *imodObjectsNew(int size)
{
     Iobj *obj;
     int ob;

     obj = (Iobj *)malloc(size * sizeof(Iobj));
     if (!obj)
	  return(NULL);

     for(ob = 0; ob < size; ob++){
	  imodObjectDefault(&(obj[ob]));
     }
     return(obj);
}

void imodObjectDefault(Iobj *obj)
{
     int i;

     obj->cont      = NULL;
     obj->mesh      = NULL;
     obj->vcont     = NULL;
     obj->vmesh     = NULL;
     for(i = 0; i < IOBJ_STRSIZE; i++)
	  obj->name[i] = 0x00;
     for(i = 0; i < IOBJ_EXSIZE; i++)
	  obj->extra[i] = 0;
     obj->contsize   = 0;
     obj->flags      = 0;
     obj->axis       = 0;
     obj->drawmode   = 1;
     obj->red        = 0.5f;
     obj->green      = 0.5f;
     obj->blue       = 0.5f;
     obj->pdrawsize  = 0;
     obj->symbol     = IOBJ_SYM_NONE;
     obj->symsize    = 3;
     obj->linewidth2 = 1;
     obj->linewidth  = 1;
     obj->linesty    = 0;
     obj->symflags   = 0;
     obj->sympad     = 0;
     obj->trans      = 0;
     obj->meshsize   = 0;
     obj->surfsize   = 0;
     
     obj->clip       = 0;
     obj->clip_flags = 0;
     obj->clip_trans = 0;
     obj->clip_plane = 0;
     obj->clip_normal.x = obj->clip_normal.y = 0.0f;
     obj->clip_normal.z = -1.0f;
     obj->clip_point.x = obj->clip_point.y = obj->clip_point.z = 0.0f;

     obj->ambient   = 102;
     obj->diffuse   = 255;
     obj->specular  = 127;
     obj->shininess = 4;
     obj->mat1 = 0;
     obj->mat1b1 = 0;
     obj->mat1b2 = 0;
     obj->mat1b3 = 0;
     obj->mat2 = 0;
     obj->mat3 = 0;
     obj->mat3b1 = 0;
     obj->mat3b2 = 0;
     obj->mat3b3 = 0;
     obj->store = NULL;
     return;
}

int imodObjectDelete(Iobj *obj)
{
     return(imodObjectsDelete(obj, 1));
}

int imodObjectsDelete(Iobj *obj, int size)
{
     int ob;

     if (size < 1)
	  return(-1);
     if (!obj)
	  return(-1);

     for(ob = 0; ob < size; ob++){
	  if (obj[ob].contsize)
	       imodContoursDelete(obj[ob].cont, obj[ob].contsize);
	  if (obj[ob].meshsize)
	       imodMeshesDelete(obj[ob].mesh, obj[ob].meshsize);
     }
     free(obj);
     return(0);
}

int imodObjectCopy(Iobj *from, Iobj *to)
{
     if (!from)
	  return(-1);
     if (!to)
	  return(-1);
     memcpy(to, from, sizeof(Iobj));
     return(0);
}

/* Converts from runtime to file format */
int imodObjectVirtOut(Iobj *obj)
{
     if (!obj)
	  return(-1);
     if (!iobjVirtual(obj->flags))
	  return(-1);
     if ((!obj->vcont) && (!obj->vmesh)){
	  obj->flags &= ~IMOD_OBJFLAG_VIRT;
	  return(-1);
     }

     obj->contsize++;
     obj->cont = (Icont *)realloc(obj->cont, sizeof(Icont) * obj->contsize);
     obj->cont[obj->contsize-1].pts   = NULL;
     obj->cont[obj->contsize-1].psize = 0;
     obj->cont[obj->contsize-1].flags = 0;
     obj->cont[obj->contsize-1].type  = 0;
     obj->cont[obj->contsize-1].surf  = 0;
     if (obj->vcont){
	  imodContourCopy(obj->vcont, &(obj->cont[obj->contsize - 1]));
	  free(obj->vcont);
     }
     if (!obj->vmesh)
	  obj->vmesh = imodMeshNew();

     obj->mesh = imodel_mesh_add(obj->vmesh, obj->mesh, &(obj->meshsize));
     free(obj->vmesh);

     return(0);
}

/* Converts from file to runtime format */
int imodObjectVirtIn(Iobj *obj)
{

     if (!obj)
	  return(-1);
     if (!iobjVirtual(obj->flags))
	  return(-1);
     
     if (!obj->contsize){
	  obj->vcont = NULL;
	  obj->flags &= ~IMOD_OBJFLAG_VIRT;
	  return(-1);
     }
     obj->vcont = imodContourNew();
     if (!obj->vcont)
	  return(-1);
     imodContourCopy(&(obj->cont[obj->contsize - 1]), obj->vcont);
     obj->contsize--;
     if (!obj->contsize)
	  free(obj->cont);
     else
	  obj->cont = (Icont *)realloc(obj->cont, 
				       sizeof(Icont) * obj->contsize);

     if (!obj->meshsize){
	  obj->vmesh = NULL; /* use contour instead in 3-d drawing. */
/*	  obj->flags &= ~IMOD_OBJFLAG_VIRT; */
/*	  imodContourDelete(obj->vcont); */
/*	  obj->vcont = NULL; */
	  return(1);
     }
     obj->vmesh = imodMeshNew();
     if (!obj->vmesh)
	  return(-1);
     imodMeshCopy(&(obj->mesh[obj->meshsize - 1]), obj->vmesh);
     obj->meshsize--;
     if (!obj->meshsize)
	  free(obj->mesh);
     else
	  obj->mesh = (Imesh *)realloc(obj->mesh,
				       sizeof(Imesh) * obj->meshsize);
     return(0);
}


/*****************************************************************************/
/* FUNCTION: imodel_object_get                                               */
/* Returns a pointer to the selected object index.                           */
/*****************************************************************************/
Iobj *imodObjectGet(struct Mod_Model  *imod)
{
     if (imod->cindex.object < 0)
	  return( (Iobj *)NULL);
     return( &(imod->obj[imod->cindex.object]));
}

Iobj *imodObjectGetFirst(Imod *imod)
{
     int ob, co, pt;

     if (!imod) return(NULL);
     imodGetIndex(imod, &ob, &co, &pt);
     imodSetIndex(imod, 0, co, pt);
     return(imodObjectGet(imod));
}

Iobj *imodObjectGetNext(Imod *imod)
{
     int ob, co, pt;

     if (!imod) return(NULL);
     imodGetIndex(imod, &ob, &co, &pt);
     ob++;
     if (ob >= imod->objsize)
	  return(NULL);
     imodSetIndex(imod, ob, co, pt);
     return(imodObjectGet(imod));
}

Icont *imodObjectGetContour(Iobj *inObject, int inIndex)
{
    if (inObject == NULL) return(NULL);
    if (inIndex < 0) return(NULL);
    if (inObject->cont == NULL) return(NULL);
    if (inIndex >= inObject->contsize) return(NULL);
    return(&inObject->cont[inIndex]);
}

Imesh *imodObjectGetMesh(Iobj *inObject, int inIndex)
{
    if (inObject == NULL) return(NULL);
    if (inIndex < 0) return(NULL);
    if (inObject->mesh == NULL) return(NULL);
    if (inIndex >= inObject->meshsize) return(NULL);
    return(&inObject->mesh[inIndex]);
}

int imodObjectAddMesh(Iobj *inObject, Imesh *inMesh)
{
     if (!inObject) return(-1);
     if (!inMesh) return(-1);
     inObject->mesh = imodel_mesh_add
	(inMesh, inObject->mesh, &(inObject->meshsize));
    return(inObject->meshsize - 1);
/*    return(-1);*/
}

/****************************************************************************
 * FUNCTION: imodObjectSort                                                   
 * Sorts the contours in an object by their z value.
 * Returns Non zero for error.                                               
 *   Sept 1996. added time value as key to sort.
 *              Empty contours no longer need to be deleted.
 */

int imodObjectSort(Iobj *obj)
{
     Icont *cont;
     Icont  tcont;
     int i, j, sz, z, sindex;
     int st, t;

     if (obj == NULL)
	  return(-1);
     if (obj->flags & IMOD_OBJFLAG_OPEN)
	  return(-1);
     cont = obj->cont;

     if (cont == NULL)
	  return(-1);

     /* Delete Empty contours. */
     /*
     for (i = 0; i < obj->contsize; i++){
	       if (cont[i].psize)
		    continue;
	       obj->contsize--;
	       sindex = obj->contsize;
	       tcont        = cont[i];
	       cont[i]      = cont[sindex];
	       cont[sindex] = tcont;
	  }
     */

     for (i = 0; i < obj->contsize - 1; i++){
	  sindex = i;
	  if (cont[i].psize)
	       sz = cont[i].pts[0].z;
	  else
	       sz = INT_MAX;
	  st = cont[i].type;

	  for (j = i + 1; j < obj->contsize; j++){
	       if (cont[j].psize)
		    z = cont[j].pts[0].z;
	       else
		    z = INT_MAX;
	       t = cont[j].type;

	       if (iobjFlagTime(obj)){
		    if (st == t){
			 if (sz > z){
			      sz = z;
			      sindex = j;
			 }
		    }else{
			 if (st > t){
			      st = t;
			      sindex = j;
			      sz = z;
			 }
		    }
		    
	       }else{
		    if (sz > z){
			 sz = z;
			 sindex = j;
		    }
	       }
	  }
	  tcont        = cont[i];
	  cont[i]      = cont[sindex];
	  cont[sindex] = tcont;

     }
     return(0);
}


/*****************************************************************************/
/* OBSOLETE */
struct Mod_Object *imodel_object_get(struct Mod_Model *mod)
{return(imodObjectGet(mod));}

/* returns volume of object. */
float imodObjectVolume(Iobj *obj)
{
     float ca = 0.0f;
     int co;

     if (!iobjClose(obj->flags))
	  return(0);

     for(co = 0; co < obj->contsize; co++)
	  ca += imodContourArea(&(obj->cont[co]));
     return(ca);
}


/* testing, internal only */
int imodel_object_centroid(struct Mod_Object *obj, struct Mod_Point *rcp)
{
     struct Mod_Contour *cont;
     struct Mod_Point cpt;
     double weight, tweight = 0;
     int co;

     rcp->x = 0.0f;
     rcp->y = 0.0f;
     rcp->z = 0.0f;

     for(co = 0; co < obj->contsize; co++){
	  cont = &(obj->cont[co]);
	  imodel_contour_centroid(cont, &cpt, &weight);
	  tweight += weight;
	  rcp->x += cpt.x;
	  rcp->y += cpt.y;
	  rcp->z += cpt.z;  /* z-scale is done outside of function */
     }
     rcp->x /= tweight;
     rcp->y /= tweight;
     rcp->z /= tweight;
     return(0);
}

/* add contour to object data. */
int imodObjectAddContour(Iobj *obj, Icont *ncont)
{
     Icont *cont;

     if (!obj)
	  return(-1);
     if (!ncont)
	  return(-1);

     if (obj->contsize == 0)
	  cont = (struct Mod_Contour *)
	       malloc(sizeof(struct Mod_Contour));
     else
	  cont = (struct Mod_Contour *)
	       realloc(obj->cont, 
		       sizeof(Icont) * (obj->contsize + 1));

     if (!cont)
	  return(-1);

     ++obj->contsize;
     obj->cont = cont;
     cont = &(obj->cont[obj->contsize -1]);
     imodContourCopy(ncont, cont);
     
     return(obj->contsize - 1);
}

/* Removes contour from object list without deleting contour data */
int imodObjectRemoveContour(Iobj *obj, int index)
{
     int co;

     if ((!obj) || (index < 0))
	  return(1);
     if (index >= obj->contsize)
	  return(1);

     obj->contsize--;
     if (obj->contsize > 0)
	  for(co = index; co < obj->contsize; co++){
	       imodContourCopy(&(obj->cont[co+1]), &(obj->cont[co]));
	  }
     return(0);
}

Iobj *imodObjectClip(Iobj *obj, Iplane *plane, int planes)
{
     Iobj *robj = imodObjectNew();
     Icont *cc, *cont;
     Ipoint ipnt;
     int pt, lpt, ppt, tpt;
     int co;
     int laststate;

     if ((planes <= 0) || (!plane) || (!obj))
	  return(NULL);

     robj = obj;

     robj->cont = NULL;
     robj->contsize = 0;

     for(co = 0; co < obj->contsize; co++){
	  laststate = 0;
	  cont = &(obj->cont[co]);
	  cc = imodContourNew();
	  lpt = cont->psize;
	  if (iobjClose(obj->flags))
	       lpt++;

	  for(pt = 0; pt < lpt; pt++){
	       ppt = tpt; /* previous point index gets old */
	       tpt = pt;  /* current this point */
	       if (tpt == cont->psize)
		    tpt = 0;

	       if(imodPlanesClip(plane, planes, &(cont->pts[tpt]))){
		    imodPointAppend(cc, &(cont->pts[tpt]));
		    if (laststate == 2){
			 if (cc->psize){
			      imodObjectAddContour(robj, cc);
			      cc = imodContourNew();
			 }
			 imodPointPlaneEdge
			      (&ipnt, plane, planes, 
			       &(cont->pts[tpt]), &(cont->pts[ppt]));
			 imodPointAppend(cc, &ipnt);
		    }
		    laststate = 1;
		    continue;
	       }
	       if (laststate == 1){
		    imodPointPlaneEdge
			 (&ipnt, plane, planes, 
			  &(cont->pts[tpt]), &(cont->pts[ppt]));
		    imodPointAppend(cc, &ipnt);
	       }
	       laststate = 2;
	  }
	  imodObjectAddContour(robj, cc);
     }
     robj->flags |= IMOD_OBJFLAG_OPEN;
     return(robj);
}

int imodObjectGetBBox(Iobj *obj, Ipoint *ll, Ipoint *ur)
{
     Ipoint min;
     Ipoint max;
     Icont *cont;
     int co;

     min.x = min.y = min.z = FLT_MAX;
     max.x = max.y = max.z = - FLT_MAX;

     if ((!obj) || (!ll) || (!ur))
	  return(-1);
     if (!obj->contsize)
	  return(1);

     *ll = min;
     *ur = max;
     for(co = 0; co < obj->contsize; co++){
	  if (imodContourGetBBox(&obj->cont[co], &min, &max))
	       continue;

	  if (min.x < ll->x) ll->x = min.x;
	  if (min.y < ll->y) ll->y = min.y;
	  if (min.z < ll->z) ll->z = min.z;

	  if (max.x > ur->x) ur->x = max.x;
	  if (max.y > ur->y) ur->y = max.y;
	  if (max.z > ur->z) ur->z = max.z;
	  
     }

     return(0);
}


void  imodObjectGetColor(Iobj *inObject,
			 float *outRed, float *outGreen, float *outBlue)
{
     *outRed   = inObject->red;
     *outGreen = inObject->green;
     *outBlue  = inObject->blue;
}

void  imodObjectSetColor(Iobj *inObject,
                         float inRed, float inGreen, float inBlue)
{
  inObject->red = inRed;
  inObject->green = inGreen;
  inObject->blue = inBlue;
}

int   imodObjectGetMaxContour(Iobj *inObject)
{
     if (!inObject) return(0);
     return(inObject->contsize);
}

char *imodObjectGetName(Iobj *inObject)
{
     if (!inObject) return(0);
     return(inObject->name);
}

int imodObjectSetName(Iobj *obj, char *inName)
{
    int i, len, retval = 0;
    for(i = 0; i < IOBJ_STRSIZE; i++)
          obj->name[i] = 0x00;
    len = strlen(inName);
    if (len > (IOBJ_STRSIZE - 1)){
	len = IOBJ_STRSIZE - 1;
	retval++;
    }
    for(i = 0; i < len; i++)
	obj->name[i] = inName[i];
    return(retval);
}


int   imodObjectGetValue(Iobj *inObject, int inValueType)
{
     switch(inValueType){

	case IobjMaxContour:
	  return(inObject->contsize);
	case IobjLineWidth:
	  return(inObject->linewidth);
	case IobjPointSize:
	  return(inObject->pdrawsize);
	case IobjMaxMesh:
	  return(inObject->meshsize);
	case IobjMaxSurface:
	  return(inObject->surfsize);
	  

	case IobjFlagClosed:
	  return(iobjClose(inObject->flags));

	case IobjFlagConnected:
	  return(!(iobjScat(inObject->flags)));

	case IobjFlagFilled:
	  return(iobjFill(inObject->flags));

	case IobjFlagDraw:
	  return(!(iobjDraw(inObject->flags)));

	case IobjFlagMesh:
	  return(iobjMesh(inObject->flags));

	case IobjFlagLine:
	  return(!(iobjLine(inObject->flags)));

	case IobjFlagTime:
	  return(iobjFlagTime(inObject));

	default:
	  return(0);
     }
}

static void setObjFlag(Iobj *inObject, b3dUInt32 flag, int state)
{
    if (!inObject) return;
    if (state){
	 inObject->flags |= flag;
    }else{
	 inObject->flags &= ~flag;
    }
}

void  imodObjectSetValue(Iobj *inObject, int inValueType, int inValue)
{
     switch(inValueType){
	case IobjLineWidth:
	  inObject->linewidth = inValue;
	  return;

	case IobjPointSize:
	  inObject->pdrawsize = inValue;
	  return;

	case IobjFlagClosed:
	  setObjFlag(inObject, IMOD_OBJFLAG_OPEN, !inValue);
	  return;

	case IobjFlagConnected:
	  setObjFlag(inObject, IMOD_OBJFLAG_SCAT, !inValue);
	  return;

	case IobjFlagFilled:
	  setObjFlag(inObject, IMOD_OBJFLAG_FILL, inValue);
	  return;

	case IobjFlagDraw:
	  setObjFlag(inObject, IMOD_OBJFLAG_OFF, !inValue);
	  return;

	case IobjFlagMesh:
	  setObjFlag(inObject, IMOD_OBJFLAG_MESH, inValue);
	  return;

	case IobjFlagLine:
	  setObjFlag(inObject, IMOD_OBJFLAG_LINE, !inValue);
	  return;

     }
}

