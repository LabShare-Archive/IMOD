/* Model draw, used only by the slicer window */

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 23:06:04  mast
conversion to cpp

Revision 3.1.2.1  2003/01/06 15:38:42  mast
Add sphere drawing

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/
#include "imod.h"
#include "imod_display.h"

#include <GL/gl.h>
#include <GL/glu.h>

void imodDrawModel(Imod *imod);
void imodDrawContourLines(Icont *cont, GLenum mode);
void imodDrawObjectSymbols(Iobj *obj);
void imodDrawSpheres(Iobj *obj);

void imodDrawModel(Imod *imod)
{
  Iobj  *obj;
  Icont *cont;
  int ob, co;

  if (imod->drawmode <= 0)
    return;

  for(ob = 0; ob < imod->objsize; ob++){
    obj = &imod->obj[ob];
    if (iobjOff(imod->obj[ob].flags))
      continue;

    imodSetObjectColor(ob);
    glLineWidth((GLfloat)obj->linewidth2);

    if (iobjScat(obj->flags))
      imodDrawSpheres(obj);
    else {
      
      /* Draw all of the objects lines first. */
      for(co = 0; co < obj->contsize; co++){
	cont = &obj->cont[co];
	if (!cont->psize)
	  continue;
	
	/* todo: ghost contour ? */
	/* check ghostmode and surface ghostmode. */
	
	if (iobjOpen(obj->flags)){
	  imodDrawContourLines(cont, GL_LINE_STRIP);
	  continue;
	}
	if (cont->flags & ICONT_OPEN)
	  imodDrawContourLines(cont, GL_LINE_STRIP);
	else
	  imodDrawContourLines(cont, GL_LINE_LOOP);
      }
    }
    imodDrawObjectSymbols(obj);
  }
     
  /* draw ends of current contour. */
  /* draw current point symbol.    */
     
  return;
}

void imodDrawContourLines(Icont *cont, GLenum mode)
{
  Ipoint *point;
  int pt, lpt;
     
  point = cont->pts;
  lpt = cont->psize;
#ifdef LINE_LOOP_HACK
  glBegin(GL_LINE_STRIP);
#else
  glBegin(mode);
#endif

  for(pt = 0; pt < lpt; pt++, point++)
    glVertex3fv((float *)point);
#ifdef LINE_LOOP_HACK
  if (mode == GL_LINE_LOOP)
    glVertex3fv((float *)cont->pts);
#endif
  glEnd();
  return;
}

void imodDrawObjectSymbols(Iobj *obj)
{
  static GLUquadricObj *qobj = NULL;
  Icont  *cont;
  Ipoint *point;
  Ipoint vert;
  unsigned int co, pt, lpt;
  int mode;
  double inner, outer;
  int slices, loops;

  if (obj->symflags  & IOBJ_SYMF_FILL)
    mode = GL_POLYGON;
  else
    mode = GL_LINE_LOOP;

  if (!qobj)
    qobj = gluNewQuadric();

  switch(obj->symbol){

  case IOBJ_SYM_CIRCLE:
    outer = obj->symsize;
    if (obj->symflags  & IOBJ_SYMF_FILL)
      inner = 0.0;
    else
      inner = outer - obj->linewidth2;
    slices = (int)(outer + 4);
    loops = 1;
    for(co = 0; co < obj->contsize; co++)
      for(pt = 0,lpt = obj->cont[co].psize,
	    point = obj->cont[co].pts; pt < lpt; pt++, point++){
	glPushMatrix();
	glTranslatef(point->x, point->y, point->z);
	gluDisk(qobj, inner, outer, slices, loops);
	glPopMatrix();
      }
    break;

  case IOBJ_SYM_SQUARE:
    for(co = 0; co < obj->contsize; co++)
      for(pt = 0,lpt = obj->cont[co].psize,
	    point = obj->cont[co].pts; pt < lpt; pt++, point++){
	glBegin(mode);
	vert = *point;
	vert.x -= (obj->symsize/2);
	vert.y -= (obj->symsize/2);
	glVertex3fv((float *)&vert);
	vert.x += obj->symsize;
	glVertex3fv((float *)&vert);
	vert.y += obj->symsize;
	glVertex3fv((float *)&vert);
	vert.x -= obj->symsize;
	glVertex3fv((float *)&vert);
	glEnd();
      }
    break;

  case IOBJ_SYM_TRIANGLE:
    for(co = 0; co < obj->contsize; co++)
      for(pt = 0,lpt = obj->cont[co].psize,
	    point = obj->cont[co].pts; pt < lpt; pt++, point++){
	glBegin(mode);
	vert = *point;
	vert.y += obj->symsize;
	glVertex3fv((float *)&vert);
	vert.x += obj->symsize;
	vert.y -= (obj->symsize + (obj->symsize/2));
	glVertex3fv((float *)&vert);
	vert.x -= 2 * obj->symsize;
	glVertex3fv((float *)&vert);
	glEnd();
      }
    break;

  case IOBJ_SYM_STAR:
    break;

  case IOBJ_SYM_NONE:
    glBegin(GL_POINTS);
    for(co = 0; co < obj->contsize; co++)
      for(pt = 0,lpt = obj->cont[co].psize, 
	    point = obj->cont[co].pts; pt < lpt; pt++, point++)
	glVertex3fv((float *)point);
    glEnd();
    break;

  default:
    break;

  }

  /* todo: draw ends */

  return;
}

void imodDrawSpheres(Iobj *obj)
{
  static GLUquadricObj *qobj = NULL;
  Icont *cont;
  Ipoint *point;
  unsigned int pt, co;
  int stepRes;
  GLdouble drawsize;
  GLuint listIndex;

  if (!qobj)
    qobj = gluNewQuadric();

  /* The default is GLU_FILL, and it makes it dotty at the equator */
  /* But GLU_LINE does not give a circle on every slice */
  gluQuadricDrawStyle(qobj, GLU_FILL);

  /* Make a display list for the default size */
  drawsize = obj->pdrawsize;
  if (drawsize) {
    stepRes = drawsize < 5 ? drawsize + 4 : 8;
    listIndex = glGenLists(1);
    glNewList(listIndex, GL_COMPILE);
    gluSphere(qobj, drawsize , stepRes * 2, stepRes);
    glEndList();
  }

  for(co = 0; co < obj->contsize; co++){
    cont = &obj->cont[co];
    if (!cont->psize)
      continue;
    for(pt = 0, point = cont->pts; pt < cont->psize; pt++, point++){
      drawsize = imodPointGetSize(obj, cont, pt);
      if (!drawsize)
	continue;
      glPushMatrix();
      glTranslatef(point->x, point->y, point->z);

      /* Use the display list if default; otherwise individual size */
      if (drawsize == obj->pdrawsize)
	glCallList(listIndex);
      else {
	stepRes = (int)(drawsize < 5 ? drawsize + 4 : 8);
	gluSphere(qobj, drawsize , stepRes * 2, stepRes);
      }
      glPopMatrix();
    }
  }
  if (obj->pdrawsize)
    glDeleteLists(listIndex, 1);
}

void imodDrawSymbol(Ipoint *point, 
		    unsigned char sym,
		    unsigned char size,
		    unsigned char flags)
{
  switch(sym){
          
  case IOBJ_SYM_CIRCLE:
    break;
          
  case IOBJ_SYM_SQUARE:
    break;
          
  case IOBJ_SYM_TRIANGLE:
    break;
          
  case IOBJ_SYM_STAR:
    break;

  case IOBJ_SYM_NONE:
    glBegin(GL_POINTS);
    glVertex3fv((float *)point);
    glEnd();
    break;
          

  default:
    break;
  }
     
  return;
}
