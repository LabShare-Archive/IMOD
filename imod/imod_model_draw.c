#include "imod.h"

#ifdef DRAW_OpenGL

#include <GL/gl.h>
#include <GL/glu.h>

void imodDrawModel(Imod *imod);
void imodDrawContourLines(Icont *cont, GLenum mode);
void imodDrawObjectSymbols(Iobj *obj);

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

	  /* Draw all of the objects lines first. */
	  imodSetObjectColor(ob);
	  glLineWidth((GLfloat)obj->linewidth2);
	  for(co = 0; co < obj->contsize; co++){
	       cont = &obj->cont[co];
	       if (!cont->psize)
		    continue;
	       
	       /* todo: ghost contour ? */
	       /* check ghostmode and surface ghostmode. */
	       
	       if (iobjScat(obj->flags))
		    continue;
	       if (iobjOpen(obj->flags)){
		    imodDrawContourLines(cont, GL_LINE_STRIP);
		    continue;
	       }
	       if (cont->flags & ICONT_OPEN)
		    imodDrawContourLines(cont, GL_LINE_STRIP);
	       else
		    imodDrawContourLines(cont, GL_LINE_LOOP);
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
	  slices = outer + 4;
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

     /* todo: draw out sphere radius for scat contours. ? */

     return;
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

#else
static void dummy(void){return;}
#endif
