/*  imodv_light.c -- OpenGL lighting functions for imodv.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$
Log at end
*/

#include <math.h>
#include <qgl.h>
#include "imodv.h"
#include "imodv_light.h"

float Imodv_light_position[4];
float Imodv_light_att[3];
float Imodv_light_dist;
int   Imodv_light_current;
int   Imodv_light_vec = 0;

static void light_update(void);
void light_moveby(int x, int y);

void imodvSetLight(Iview *vw)
{
  if (vw->world & VIEW_WORLD_LIGHT)
    Imodv->lighting = 1;
  else
    Imodv->lighting = 0;
  light_moveby(0,0); 
}


void light_getparam(int param, float *outValue)
{

  switch(param){
  case 1: 
    *outValue = Imodv_light_position[0]; 
    break;
  case 2: 
    *outValue = Imodv_light_position[1]; 
    break;
  case 3: 
    *outValue = Imodv_light_position[2]; 
    break;
  case 4: *outValue = Imodv_light_att[0]; break;
  case 5: *outValue = Imodv_light_att[1]; break;
  case 6: *outValue = Imodv_light_att[2]; break;
  case 7: *outValue = Imodv_light_dist; break;
  }
}

void light_setparam(int param, double value)
{
  switch(param){
  case 1:
    Imodv_light_position[0] = value;
    break;
  case 2:
    Imodv_light_position[1] = value;
    break;
  case 3:
    Imodv_light_position[2] = value;
    break;
        
  case 4:
    Imodv_light_att[0] = value;
    glLightf(GL_LIGHT0, GL_CONSTANT_ATTENUATION, Imodv_light_att[0]);
    return;

  case 5:
    Imodv_light_att[1] = value;
    glLightf(GL_LIGHT0, GL_LINEAR_ATTENUATION, Imodv_light_att[1]);
    return;

  case 6:
    Imodv_light_att[2] = value;
    glLightf(GL_LIGHT0, GL_QUADRATIC_ATTENUATION, Imodv_light_att[2]);
    return;

  case 7:
    Imodv_light_dist = value;
    break;
  }
  light_update();
}

static void light_update(void)
{
  float lightpos[4];
  float ldist;

  glLightf(GL_LIGHT0 + Imodv_light_current,
           GL_CONSTANT_ATTENUATION, Imodv_light_att[0]);
  glLightf(GL_LIGHT0 + Imodv_light_current, 
           GL_LINEAR_ATTENUATION, Imodv_light_att[1]);
  glLightf(GL_LIGHT0 + Imodv_light_current, 
           GL_QUADRATIC_ATTENUATION, Imodv_light_att[2]);

  if (Imodv_light_dist == 0.0f){
    Imodv_light_position[3] = lightpos[3] = 0.0;
    glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 0);
    ldist = 1.0f;
    lightpos[0] = Imodv_light_position[0] * ldist;
    lightpos[1] = Imodv_light_position[1] * ldist;
    lightpos[2] = Imodv_light_position[2] * ldist;
    glLightfv(GL_LIGHT0 + Imodv_light_current, GL_POSITION, lightpos);
  }else{
    Imodv_light_position[3] = lightpos[3] = 1.0f;
    glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 1);
    ldist = Imodv_light_dist;
    lightpos[0] = Imodv_light_position[0];
    lightpos[1] = Imodv_light_position[1];
    lightpos[2] = Imodv_light_position[2];
    glLightfv(GL_LIGHT0 + Imodv_light_current, GL_POSITION, lightpos);
  }

}


void light_init(void)
{
  static int first = 1;
    
  GLfloat amb[] = { 0.1, 0.1, 0.1, 1.0};
  GLfloat ambm[] = { 0.7, 0.7, 0.7, 1.0};
    
  if (first){
    Imodv_light_dist   = 0.0f;
    Imodv_light_att[0] = 1.0f;
    Imodv_light_att[1] = 0.0f;
    Imodv_light_att[2] = 0.0f;
    Imodv_light_current = 0;
    Imodv_light_position[0] = Imodv_light_position[1] = 
      Imodv_light_position[3] = 0;
    Imodv_light_position[2] = 1.0f;
    first = 0;
  }

  imodvSetLight(Imodv->imod->view);
  glLightfv(GL_LIGHT0, GL_AMBIENT, amb);
  /*     glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 1); */
  /* 2.0b6: changed light to be infinite. */
  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 0); 

  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ambm);
  return;
}

void light_moveby(int x, int y)
{
  int lx = (int)(Imodv->imod->view->lightx * 10.0f);
  int ly = (int)(Imodv->imod->view->lighty * 10.0f);

  lx += x;
  ly += y;
  light_move(&lx, &ly);
  Imodv->imod->view->lightx = (float)lx * 0.1f;
  Imodv->imod->view->lighty = (float)ly * 0.1f;
}

/* 
 *  Move the light!
 *
 * x should be the address of Imodv->lightx and
 * y should be the address of Imodv->lighty.
 */
void light_move(int *x, int *y)
{
  int n = 0;
  float xn,yn,zn;
  double xa, ya;
  int lim = 899;
  float lightpos[4];
  float ldist = Imodv_light_dist;

  if (Imodv_light_dist > 0.0){
    Imodv_light_position[0] = *x;
    Imodv_light_position[1] = *y;
    light_update();
    return;
  }

  if (*x > lim)  *x = lim;
  if (*x < -lim) *x = -lim;
  if (*y > lim)  *y = lim;
  if (*y < -lim) *y = -lim;

  xa = (double)*x * 0.1;
  ya = (double)*y * 0.1;
  xa *= 0.017453293;
  ya *= 0.017453293;

  xn = (float)sin(xa);
  yn = (float)sin(ya);
  zn = 1.0;
     
  Imodv_light_position[0] = xn;
  Imodv_light_position[1] = yn;
  Imodv_light_position[2] = zn;
  light_update();
  return;
}


/* Adjust light color. */
void light_adjust(Iobj *obj, float r, float g, float b, int trans)
{
  GLfloat red   = r;
  GLfloat green = g;
  GLfloat blue  = b;
  GLfloat alpha = (1.0 - (float)trans / 100.0f);
  GLfloat params[4];
  GLfloat spec, amb, diffuse, shine;
  GLenum face = GL_FRONT_AND_BACK;
  /*    if (!(obj->flags & IMOD_OBJFLAG_LIGHT)) return;*/

  spec    = obj->specular / 255.0f;
  amb     = obj->ambient  / 255.0f;
  diffuse = obj->diffuse / 255.0f;
  shine   = ((255 - obj->shininess) / 50.0f ) + 1.0f;
    
  params[0] = (float)red   * amb;
  params[1] = (float)green * amb;
  params[2] = (float)blue  * amb;
  params[3] = alpha;
  glMaterialfv(face, GL_AMBIENT, params);

  params[0] = (float)red * diffuse;
  params[1] = (float)green * diffuse;
  params[2] = (float)blue * diffuse;
  params[3] = alpha;
  glMaterialfv(face, GL_DIFFUSE, params);
    
  params[0] = spec + (red);
  params[1] = spec + (green);
  params[2] = spec + (blue);
  params[3] = alpha;
  glMaterialfv(face, GL_SPECULAR, params);

  return;
}

void light_on(struct Mod_Object *obj)
{
  Iview *vw = Imodv->imod->view;
  GLfloat red   = obj->red;
  GLfloat green = obj->green;
  GLfloat blue  = obj->blue;
  GLfloat alpha = (1.0 - (float)obj->trans / 100.0f);
  unsigned char *ub;

  GLfloat params[4];
  GLfloat spec, amb, diffuse, shine;
  /* could make this conditional on both-side lighting flag set if there
     are problems */
  GLenum face = GL_FRONT_AND_BACK;

  if (obj->flags & IMOD_OBJFLAG_FCOLOR){
    ub = ( unsigned char * )&(obj->mat1);
    red   = (float)ub[0] / 255.0f;
    green = (float)ub[1] / 255.0f;
    blue  = (float)ub[2] / 255.0f;
  }

  spec    = obj->specular / 255.0f;
  amb     = obj->ambient  / 255.0f;
  diffuse = obj->diffuse / 255.0f;
  shine   = ((255 - obj->shininess) / 50.0f ) + 1.0f;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
  params[0] = (float)red   * amb;
  params[1] = (float)green * amb;
  params[2] = (float)blue  * amb;
  params[3] = alpha;
  glMaterialfv(face, GL_AMBIENT, params);
     
  params[0] = (float)red * diffuse;
  params[1] = (float)green * diffuse;
  params[2] = (float)blue * diffuse;
  params[3] = alpha;
  glMaterialfv(face, GL_DIFFUSE, params);

  params[0] = spec + (red);
  params[1] = spec + (green);
  params[2] = spec + (blue);
  params[3] = alpha;
  glMaterialfv(face, GL_SPECULAR, params);

  /* DNM 9/3/02: this was glMateriali but did not seem to matter */
  glMaterialf(face, GL_SHININESS, shine);

  /*    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
        glEnable(GL_COLOR_MATERIAL);
  */
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
     
  /*     glLightfv(GL_LIGHT0, GL_POSITION, Imodv_light_position); */

  /*
    glTranslatef(vw->trans.x, vw->trans.y, 
    (vw->trans.z * Imodv->imod->zscale));
  */
  glScalef(vw->scale.x, vw->scale.y, 
           vw->scale.z * Imodv->imod->zscale);
     

  light_update();
  glPopMatrix();

}
    

void light_off(void)
{
  glDisable(GL_LIGHT0);
  glDisable(GL_LIGHTING);
  glDisable(GL_COLOR_MATERIAL);
}


#ifdef IMODV_LIGHT_TEST_NORMAL
int imod_light_normal( struct Mod_Point *n,  struct Mod_Point *p1,
                       struct Mod_Point *p2,  struct Mod_Point *p3, double z);


int test_normal()
{

  struct Mod_Point p,q,r,n;

  p.x = 4; p.y = -3; p.z = 1;
  q.x = 6; q.y = -4; q.z = 7;
  r.x = 1; r.y = 2;  r.z = 2;

  imod_light_normal(&n, &p, &q, &r, 1.0);

  imodPrintStderr("glNormal3fv(%g)\n", n.x, n.y, n.z); 
  return(0);
}



int imod_light_normal( struct Mod_Point *n,
                       struct Mod_Point *p1,
                       struct Mod_Point *p2,
                       struct Mod_Point *p3, double z)
{
  /* find the normal for the plane with the points p1,p2,p3 */
  double dist;
     
  struct Mod_Point v1, v2;


  v1.x = p3->x - p2->x;
  v1.y = p3->y - p2->y;
  v1.z = ((p3->z * z) - (p2->z * z));

  v2.x = p1->x - p2->x;
  v2.y = p1->y - p2->y;
  v2.z = ((p1->z * z) - (p2->z * z));

  n->x = (v1.z * v2.y) - (v1.y * v2.z);
  n->y = (v1.x * v2.z) - (v1.z * v2.x);
  n->z = (v1.y * v2.x) - (v1.x * v2.y);


  /* now normalize n ; x^2 + y^2 + z^2 = 1 */

  dist = (n->x * n->x) + (n->y * n->y) + (n->z * n->z);
  dist = sqrt(dist);
  if (dist == 0.0){
    n->x = 0;
    n->y = 0;
    n->z = -1;
  }
  else{
    dist = 1/dist;

    n->x *= dist;
    n->y *= dist;
    n->z *= dist;
  }
  return(0);
}

#endif /* IMODV_LIGHT_TEST_NORMAL */
/*
$Log$
Revision 4.5  2005/06/20 22:20:28  mast
Pass transparency to light_adjust

Revision 4.4  2004/09/21 20:18:51  mast
Moved clipping function to imodv_ogl

Revision 4.3  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.2  2003/02/27 17:28:22  mast
Had to include qgl.h instead of GL/gl.h under windows

Revision 4.1  2003/02/10 20:29:01  mast
autox.cpp

Revision 1.1.2.3  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.2  2002/12/17 18:33:19  mast
using new includes for imodv compoennts

Revision 1.1.2.1  2002/12/15 21:14:02  mast
conversion to cpp

Revision 3.2  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.1  2002/09/03 19:37:54  mast
Changed shininess call from glMateriali to glMaterialf

*/
