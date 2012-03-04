/*  mv_light.cpp -- OpenGL lighting functions for imodv.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <math.h>
#include <qgl.h>
#include "imodv.h"
#include "mv_light.h"

float Imodv_light_position[4];
float Imodv_light_att[3];
float Imodv_light_dist;
int   Imodv_light_current;
int   Imodv_light_vec = 0;

static void light_update(void);

// Set up the light in its current position
void imodvSetLight(Iview *vw)
{
  light_moveby(vw, 0,0); 
}

// Get light parameters (unused)
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

// Set light parameters (unused)
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

// Update the light position and properties
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

// Initialize the lighting (when GL widget is initialized)
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

  Imodv->lighting = (Imodv->imod->view->world & VIEW_WORLD_LIGHT) ? 1 : 0;
  glLightfv(GL_LIGHT0, GL_AMBIENT, amb);
  /*     glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 1); */
  /* 2.0b6: changed light to be infinite. */
  glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 0); 

  glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ambm);
  return;
}

/*
 * Move the light by the given amount
 */
void light_moveby(Iview *vw, int x, int y)
{
  int lx = (int)(vw->lightx * 10.0f);
  int ly = (int)(vw->lighty * 10.0f);

  // 4/3/07: incoming data no longer scaled by 10 so that it can be scaled
  // by 3 instead for greater sensitivity
  lx += 2 * x;
  ly += 2 * y;
  light_move(&lx, &ly);
  vw->lightx = (float)lx * 0.1f;
  vw->lighty = (float)ly * 0.1f;
}

/* 
 *  Set the light position
 */
void light_move(int *x, int *y)
{
  float xn,yn,zn;
  double xa, ya;
  int lim = 800;

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

  /* 4/3/07: The behavior of the light by setting normals with the original
     method (sin(xa), sin(ya)) gave very little range.  Rotating a normal by
     angles gave huge variation in sensitivity with position.  Fitting a curve
     to the ratios (xn/zn) found at detectable even steps in rotation gave the
     formula used here.  Books and man pages don't help about what is going on
     with this position. */
  xa = (double)*x * 0.1;
  ya = (double)*y * 0.1;
  zn = 1.0;
  xn = (float)(pow(10., fabs(0.011 * xa) + 0.845099) - 7.);
  yn = (float)(pow(10., fabs(0.011 * ya) + 0.845098) - 7.);
  if (xa < 0.)
    xn = -xn;
  if (ya > 0.)
    yn = -yn;
  // printf("angles %.1f %.1f Normal %.4f %.4f %.4f\n", xa, ya, xn, yn, zn);
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

/*
 * Set up lighting for an object from color and material properties
 */
void light_on(Iobj *obj, int modind)
{
  Iview *vw = Imodv->mod[modind]->view;
  GLfloat red   = obj->red;
  GLfloat green = obj->green;
  GLfloat blue  = obj->blue;
  GLfloat alpha = (1.0 - (float)obj->trans / 100.0f);

  GLfloat params[4];
  GLfloat spec, amb, diffuse, shine;
  /* could make this conditional on both-side lighting flag set if there
     are problems */
  GLenum face = GL_FRONT_AND_BACK;

  if (obj->flags & IMOD_OBJFLAG_FCOLOR){
    red   = (float)obj->fillred / 255.0f;
    green = (float)obj->fillgreen / 255.0f;
    blue  = (float)obj->fillblue / 255.0f;
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
           vw->scale.z * Imodv->mod[modind]->zscale);
     

  light_update();
  glPopMatrix();

}

// Turn off lighting
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
