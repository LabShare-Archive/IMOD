/*  imodel_to.c --  Converts imodel files to other formats.
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.4  2005/09/11 19:12:17  mast
Changes for new mesh style

Revision 3.3  2004/10/13 05:45:29  mast
Fixed bug in writing mesh to nff file

Revision 3.2  2004/09/10 21:33:46  mast
Eliminated long variables

*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "imodel.h"

int imod_mesh_to_synu(struct Mod_Object *obj, int no, double zscale);

int imod_to_wmod(struct Mod_Model *mod, FILE *fout, const char *filename)
{

  int object_count = 1;
  int point_count = 0;
  int ob, co, i;
  int display_switch = 247;
  int blank, blen;

  struct Mod_Object  *obj;
  struct Mod_Contour *cont;

  int  cnum = 0;  /* Total number of contours. */
  int  pnum = 0;  /* Total number of points.   */


  /* find all contours and points. */
  for (ob = 0; ob < mod->objsize; ob++){
    obj = &(mod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      cnum++;
      pnum += cont->psize;
    }
  }



  fprintf(fout, " Model file name........................%s", filename);
  blen = strlen(filename);
  blen = 31 - blen;
  if (blen < 0){
    for(blank = 0; blank < blen; blank++)
      fprintf(fout, " ");
  }
  fprintf(fout, "\n");
  fprintf(fout, " max # of object....................... %4d\n", 2 * cnum);
  fprintf(fout, " # of node............................. %4d\n", 2 * pnum);
  fprintf(fout, " # of object........................... %4d\n", cnum);
  fprintf(fout, "  Object sequence : \n");



  for (ob = 0; ob < mod->objsize; ob++, display_switch){
    obj = &(mod->obj[ob]);
    if (display_switch > 255)
      display_switch = 247;
      

    for(co = 0; co < obj->contsize; co++){
 
      cont = &(obj->cont[co]);
      fprintf(fout, "  Object #: %11d\n", object_count++);
      fprintf(fout, " # of point: %11d\n", cont->psize);
      fprintf(fout, " Display switch:1  %d\n", display_switch);
      fprintf(fout, 
              "     #    X       Y       Z      Mark    Label \n");
      ++point_count;
      for (i = 0; i < cont->psize; i++){
        fprintf(fout, "%7d", point_count++);
        fprintf(fout, " %7.2f", cont->pts[i].x); 
        fprintf(fout, " %7.2f", cont->pts[i].y);
        fprintf(fout, " %7.2f   0\n", cont->pts[i].z);
      }
    }
  }


  fprintf(fout, "\n  END\n");

  return(0);
}

int imod_to_nff(struct Mod_Model *mod, FILE *fout)
{
     
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  int objnum;
  int contnum;
  int i, j;
  int useMesh;
    
  float xo = 0, yo = 0, zo = 0;  /* x,y,z offsets. */
  float xs = 1, ys = 1, zs = 1;  /* x,y,z scale.   */
  int listInc, vertBase, normAdd;
    
  xo = mod->xoffset;
    
  /* Loop through objects. */
  for (objnum = 0; objnum < mod->objsize; objnum++){
    
    /* Set color for each object. */
    obj = &(mod->obj[objnum]);
    
    fprintf(fout, "# object with %d contours.\n", obj->contsize);
    fprintf(fout, "f %g %g %g 0 0 0 0 0\n",
            obj->red, obj->green, obj->blue);
    
    useMesh = imodObjectGetValue(obj, IobjFlagMesh);
    
    if (useMesh){
      int i, mi, mm;
      Imesh *mesh;
      for(mi = 0; mi < obj->meshsize; mi++){
        mesh = &obj->mesh[mi];
        
        for(i  = 0; i < mesh->lsize; i++){
          switch(mesh->list[i]){
          case IMOD_MESH_BGNPOLY:
          case IMOD_MESH_BGNBIGPOLY:
            while(mesh->list[++i] != IMOD_MESH_ENDPOLY);
            break;
         
          case IMOD_MESH_BGNPOLYNORM:
          case IMOD_MESH_BGNPOLYNORM2:
            imodMeshPolyNormFactors(mesh->list[i++], &listInc, &vertBase,
                                    &normAdd);
            while (mesh->list[i] != IMOD_MESH_ENDPOLY) {
              fprintf(fout, "pp 3\n");
              for (j = 0; j < 3; j++) {
                fprintf(fout, "%g %g %g %g %g %g\n",
                        mesh->vert[mesh->list[i+vertBase]].x* mod->xscale,
                        mesh->vert[mesh->list[i+vertBase]].y* mod->yscale,
                        mesh->vert[mesh->list[i+vertBase]].z* mod->zscale,
                        mesh->vert[mesh->list[i] + normAdd].x * mod->xscale,
                        mesh->vert[mesh->list[i] + normAdd].y * mod->yscale,
                        mesh->vert[mesh->list[i] + normAdd].z * mod->zscale);
                i+=listInc;
              }
            }
            break;
          }
        }
      }
    }else{
      /* Loop through contours. */
      for(contnum = 0; contnum < obj->contsize; contnum++){
        
        cont = &(obj->cont[contnum]);
        
        /* Loop through points. */
        
        if (iobjScat(obj->flags)){ 
          for (i = 0; i < cont->psize; i++)
            fprintf(fout, "s %g %g %g %d\n",
                    (cont->pts[i].x + mod->xoffset) * mod->xscale,
                    (cont->pts[i].y + mod->yoffset) * mod->yscale,
                    (cont->pts[i].z + mod->zoffset) *mod->zscale,
                    obj->pdrawsize);
            
        }else if (!(obj->flags & IMOD_OBJFLAG_OPEN)){
          fprintf(fout, "p %d\n", cont->psize);
          for (i = 0; i < cont->psize; i++)
            
            fprintf(fout, "%g %g %g\n",
                    (cont->pts[i].x + mod->xoffset) * mod->xscale,
                    (cont->pts[i].y + mod->yoffset) * mod->yscale,
                    (cont->pts[i].z + mod->zoffset) *mod->zscale);
        }else{
            
          if (cont->psize == 1){
            fprintf(fout, "p %d\n", 3);
            for (i = 0; i < 3; i++)
              fprintf(fout, "%g %g %g\n",
                      (cont->pts[0].x + mod->xoffset) * 
                      mod->xscale,
                      (cont->pts[0].y + mod->yoffset) * 
                      mod->yscale,
                      (cont->pts[0].z + mod->zoffset) *
                      mod->zscale);
          }
            
          if (cont->psize == 2){
            fprintf(fout, "p %d\n", 3);
            fprintf(fout, "%g %g %g\n",
                    (cont->pts[0].x + mod->xoffset) * mod->xscale,
                    (cont->pts[0].y + mod->yoffset) * mod->yscale,
                    (cont->pts[0].z + mod->zoffset) *mod->zscale);
            
            fprintf(fout, "%g %g %g\n",
                    (cont->pts[1].x + mod->xoffset) * mod->xscale,
                    (cont->pts[1].y + mod->yoffset) * mod->yscale,
                    (cont->pts[1].z + mod->zoffset) *mod->zscale);
            
            fprintf(fout, "%g %g %g\n",
                    (cont->pts[0].x + mod->xoffset) * mod->xscale,
                    (cont->pts[0].y + mod->yoffset) * mod->yscale,
                    (cont->pts[0].z + mod->zoffset) *mod->zscale);
            
          }
            
          if (cont->psize > 2){
            fprintf(fout, "p %d\n", (cont->psize * 2) - 2);
            for (i = 0; i < cont->psize; i++)
                
              fprintf(fout, "%g %g %g\n",
                      (cont->pts[i].x + mod->xoffset) * 
                      mod->xscale,
                      (cont->pts[i].y + mod->yoffset) *
                      mod->yscale,
                      (cont->pts[i].z + mod->zoffset) 
                      *mod->zscale);
            
            for (i = cont->psize - 2; i > 0; i--)
              fprintf(fout, "%g %g %g\n",
                      (cont->pts[i].x + mod->xoffset) *
                      mod->xscale,
                      (cont->pts[i].y + mod->yoffset) * 
                      mod->yscale,
                      (cont->pts[i].z + mod->zoffset) *
                      mod->zscale);
          }
        }
        
      }
    }
  }
  return(0);
}
    


int imod_to_synu(struct Mod_Model *mod)
{
  FILE *fout  = NULL;
  FILE *fview = NULL;
  struct Mod_Contour *cont;
  int ob, co, pt, i;
  int v,vertices, edges;
  char viewdata[] = "Viewdata";
  char *vdata = 
    "visible=no rep=l trans=no cull=off depthcue=yes shademode=f";
  char *mdata = 
    "visible=no rep=l trans=yes cull=off depthcue=yes shademode=s";
  char filename[32];
  char line[128];

     
  for(ob = 0; ob < mod->objsize; ob++)
    imodObjectSort((struct Mod_Object *) &(mod->obj[ob]) );

  for(ob = 0; ob < mod->objsize; ob++){
    sprintf(filename, "type%d.cont", ob);
    fout = fopen(filename, "w");
    if (fout == NULL)
      continue;

    for(co = 0; co < mod->obj[ob].contsize; co++){
           
      cont = &(mod->obj[ob].cont[co]);
      vertices = cont->psize;
      v = vertices;
      edges = (2*v)-2;
      
      if (vertices == 1)
        vertices = 2;
      if (!edges)
        edges = 2;
           
      if (vertices == 0)
        continue;

      fprintf(fout, "#synu\n#Imod object %d\n", ob);
      if (mod->obj[ob].flags & IMOD_OBJFLAG_OPEN){

        fprintf(fout, "polygonmesh 4l %dl %dl 1l\n%d\n%d\n1\n0\n",
                vertices, edges, vertices, edges);
            
        if (v > 1){
          for(pt = 0; pt < v; pt++)
            fprintf(fout, "%5.2f %5.2f %5.2f\n", 
                    cont->pts[pt].x, 
                    cont->pts[pt].y, 
                    cont->pts[pt].z* mod->zscale);
          for(pt = 0; pt < v; pt++)
            fprintf(fout, "%d\n", pt);
          for(pt = v-2 ; pt > 0; pt--)
            fprintf(fout, "%d\n", pt);
          fprintf(fout, "%d\n", (2 * v) - 3);
        }else{
          fprintf(fout, "%5.2f %5.2f %5.2f\n", 
                  cont->pts[0].x,
                  cont->pts[0].y,
                  cont->pts[0].z * mod->zscale);
          fprintf(fout, "%5.2f %5.2f %5.2f\n", 
                  cont->pts[0].x,
                  cont->pts[0].y,
                  cont->pts[0].z * mod->zscale);
          fprintf(fout, "0\n1\n1\n");
        }
            
      }else{
            
        fprintf(fout, 
                "polygonmesh 4l %dl %dl 1l\n%d\n%d\n1\n0\n",
                v,v,v,v);
        for(pt = 0; pt < v; pt++)
          fprintf(fout, "%5.2f %5.2f %5.2f\n",
                  cont->pts[pt].x,
                  cont->pts[pt].y,
                  cont->pts[pt].z * mod->zscale);
        for(pt = 0; pt < v; pt++)
          fprintf(fout, "%d\n", pt);
        fprintf(fout, "%d\n", --pt);
      }
    }

    fclose(fout);

    if (mod->obj[ob].meshsize)
      imod_mesh_to_synu(&(mod->obj[ob]), ob, (double)mod->zscale);
  }

  fview = fopen(viewdata, "w");
     
  if (fview){
    for (ob = 0; ob < mod->objsize; ob++){

      fprintf(fview, "type%d.cont %s ", ob, vdata);
      fprintf(fview, "color=%1.2f,%1.2f,%1.2f,%1.2f\n", 
              mod->obj[ob].red,
              mod->obj[ob].green,
              mod->obj[ob].blue,
              mod->obj[ob].trans/255.0);

      if (mod->obj[ob].meshsize){
        fprintf(fview, "type%d.mesh %s ", ob, mdata);
        fprintf(fview, "color=%1.2f,%1.2f,%1.2f,%1.2f\n",
                mod->obj[ob].red,
                mod->obj[ob].green,
                mod->obj[ob].blue,
                mod->obj[ob].trans/255.0);
      }
    }
    fclose(fview);
  }


  return(0);
}


int imod_mesh_to_synu(struct Mod_Object *obj, int no, double zscale)
{
  FILE *fout;
  char filename[128];
  struct Mod_Mesh *mesh;
  int i, v, l, e, p;
  int v1, v2, v3, tv, vc;
  float hiz, lowz;
  int direction;
  int no_of_verts;
  int no_of_edges;
  int no_of_polys;

  sprintf(filename, "type%d.mesh", no);
  fout = fopen(filename, "w");
  if (fout == NULL)
    return(-1);

  for (i = 0; i < obj->meshsize; i ++){

    mesh = &(obj->mesh[i]);
    if ((!mesh->vsize) || (!mesh->lsize))
      continue;
    no_of_verts = mesh->vsize;
    no_of_polys = no_of_verts;
    no_of_edges = no_of_polys * 3;


    fprintf(fout, "#synu\n#Imod object %d\n#mesh %d\n", no, i);

    fprintf(fout, "polygonmesh 4l %dl %dl %dl\n%d\n%d\n%d\n0\n",
            no_of_verts, no_of_edges, no_of_polys,
            no_of_verts, no_of_edges, no_of_polys);

    /* vertices list */
    for (v = 0; v <  no_of_verts; v++)
      fprintf(fout, "%5.2f %5.2f %5.2f\n",
              mesh->vert[v].x,
              mesh->vert[v].y,
              mesh->vert[v].z * zscale);
      
    /* edge list */
      
    v1 = mesh->list[0];
    v2 = mesh->list[1];
    lowz = mesh->vert[0].z;
    hiz  = mesh->vert[mesh->vsize - 1].z;
    if (lowz > hiz){
      lowz = hiz;
      hiz  = mesh->vert[0].z;
    }

    for (e = 0, l = 2; e < no_of_edges; l++){
      if (mesh->list[l] == IMOD_MESH_SWAP){
        tv = v1;
        v1 = v2;
        v2 = tv;
      }else {
        e+=3;
        if (l < mesh->lsize){
          v3 = mesh->list[l];
          direction = 1;
          if (mesh->vert[v1].z == mesh->vert[v2].z){
            vc = v1 - v2;
            if (vc < 0)
              vc *= -1;
            vc -= 1;
            if (mesh->vert[v1].z == lowz){ 
              if (v2 > v1)
                direction *= -1;
            }else{
              if (v1 > v2)
                direction *= -1;
            }
          }
          if (mesh->vert[v2].z ==  mesh->vert[v3].z){
            vc = v3 - v2;
            if (vc < 0)
              vc *= -1;
            vc -= 1;
            if (mesh->vert[v1].z == hiz){
              if ( v2 > v3)
                direction *= -1;
            }
            else
              if (v3 > v2)
                direction *= -1;
          }
          if (mesh->vert[v3].z == mesh->vert[v1].z){
            vc = v1 - v3;
            if (vc < 0)
              vc *= -1;
            vc -= 1;
            if (mesh->vert[v1].z == hiz){
              if ( v1 > v3)
                direction *= -1;
            }
            else
              if (v3 > v1)
                direction *= -1;
          }
             
          if (vc)
            direction *= -1;
                   
          if (direction > 0)
            fprintf(fout, "%d\n%d\n%d\n", 
                    v3, v2, v1);
          else
            fprintf(fout, "%d\n%d\n%d\n", 
                    v1, v2, v3);
          v1 = v2;
          v2 = mesh->list[l];
        }else
          fprintf(fout, "0\n1\n2\n");
      }
    }

    /* polygon list:  2, 5, 8, 11, ... */
    for (p = 0; p < no_of_polys; p++)
      fprintf(fout, "%d\n", (p * 3) + 2);
  }
  return(0);
}



/* The Renderman (R) Interface Procedures and RIB Protocol are:
 * Copyright 1988,1989, Pixar.
 * All rights reseved.
 * RenderMan (R) is a registered trademark of Pixar.
 */

int imod_to_RIB(Imod *imod, FILE *fout);
static int pRIB_mesh(FILE *fout, Imesh *mesh, double zscale);
static int pRIB_scat(FILE *fout, Iobj *obj, double z);
static int pRIB_tubes(FILE *fout, Iobj *obj, double z);

static int prib_tube(FILE *fout, Ipoint *p1, Ipoint *p2, 
                     int slices, int linewidth, double z);



int imod_to_RIB(Imod *imod, FILE *fout)
{
  Iview *vw;
  Iobj *obj;
  Ipoint maxp, minp;
  int ob, co, pt;
  int m;
  Ipoint imageMax = {0., 0., 0.};

  char image_filename[] = "imod2RIB.tif";
  float opac = 1.0f;
  float cscale = 1.0f; /* camera scale */
  float fovytan = 1.0f, cdist;

  float xoffset, yoffset, zoffset;
  float lxn = -0.5f, lyn = 0.5f, lzn = -1.0f;
  float fovy = 33.0f; /* field of view of the camera, 0 means ortho */
  float czoom = imod->xscale; /* camera zoom factor. */
  float r = 1.0f; /* raduis of bounding sphere. */

  /* calculate camera scale */
  if (!imod->cview){
    imodViewModelDefault(imod, imod->view, &imageMax);
  }
  vw = &(imod->view[imod->cview]);

  r = vw->rad;
  cscale = (1/r) * 0.75;
  fovytan = tan((((double)vw->fovy ) * 0.0087266463));
  if (fovytan)
    cdist = r/fovytan;
  else{
    cdist = r;
    fovytan = cscale;
  }

  cdist /= 0.75;

  fprintf(fout, "#RenderMan RIB-Structure 1.0\n");
  fprintf(fout, "#Created\n");
  fprintf(fout, "#by IMOD\n\n");

  fprintf(fout, "#The Renderman (R) Interface Procedures and RIB Protocol are:\n");
  fprintf(fout, "#Copyright 1988,1989, Pixar.\n");
  fprintf(fout, "#All rights reseved.\n");
  fprintf(fout, "#RenderMan (R) is a registered trademark of Pixar.\n");
  fprintf(fout, "#\n\n");


  /*     fprintf(fout, "#FrameAspectRatio 1\n"); */
  /*     fprintf(fout, "#Format 128 128 1\n"); */
  /*     fprintf(fout, "#Display \"%s\" \"file\" \"rgb\"\n", image_filename); */

  if (vw->fovy){
    fprintf(fout, "Projection \"perspective\"  \"fov\" %g\n", vw->fovy);
    cscale = 1.0;
  }

  /* Position light for Model */
  fprintf(fout, "LightSource \"distantlight\" 1 \"from\" [%g %g %g] ",
          lxn, lyn, lzn);
  fprintf(fout, "\"to\" [0 0 0] \"intensity\" 1\n");

  fprintf(fout, "\nWorldBegin\n");
  fprintf(fout, "Translate 0 0 %g\n", cdist);
  /*     fprintf(fout, "FrameBegin 1\n"); */
  fprintf(fout, "Scale %g %g -%g\n", cscale, cscale, cscale);

  fprintf(fout, "Rotate %g 1 0 0\n",  vw->rot.x);
  fprintf(fout, "Rotate %g 0 1 0\n",  vw->rot.y);
  fprintf(fout, "Rotate %g 0 0 1\n",  vw->rot.z);
  /*     fprintf(fout, "Scale 1 1 %g\n", imod->zscale); */

  fprintf(fout, "Translate %g %g %g\n",
          vw->trans.x, vw->trans.y, vw->trans.z);

  /* Draw each object */
  for(ob = 0; ob < imod->objsize; ob++){
    obj = &imod->obj[ob];
    fprintf(fout, "\n# Object %d\n", ob + 1);
    if (obj->name)
      fprintf(fout, "# %s\n", obj->name);
    if (iobjOff(obj->flags)){
      fprintf(fout, "#Turned off, no rendering.\n");
      continue;
    }
    fprintf(fout, "AttributeBegin\n");

    fprintf(fout, "Color [ %g %g %g  ]\n", 
            obj->red, obj->green, obj->blue);

    opac = 100 - obj->trans;
    opac = opac * 0.01;
    fprintf(fout, "Opacity %g %g %g\n", opac, opac, opac);

    fprintf(fout, "LightSource \"ambientlight\" 1 \"intensity\" %g\n", 
            (float)(obj->ambient/256.0));

    fprintf(fout, "Surface \"plastic\" \"Ks\" %g\n",
            (float)((float)obj->shininess * 0.0078125f));

    if (iobjClose(obj->flags)){
      for(m = 0; m < obj->meshsize; m++)
        pRIB_mesh(fout, &obj->mesh[m], (double)imod->zscale);
    }
    if (iobjScat(obj->flags)){
      pRIB_scat(fout, obj, (double)imod->zscale);
    }
    if (iobjOpen(obj->flags)){
      pRIB_tubes(fout, obj, (double)imod->zscale);
    }

    fprintf(fout, "AttributeEnd\n");
  }
  fprintf(fout, "WorldEnd\n");
  /*     fprintf(fout, "FrameEnd 1\n"); */
  return(0);
}


static int pRIB_mesh(FILE *fout, Imesh *mesh, double zscale)
{
  Ipoint cndat;
  Ipoint *cnormal;
  unsigned int i, lsize;
  int first, j;
  Ipoint norm[3];
  Ipoint vert[3];
  float z = zscale;
  int listInc, vertBase, normAdd;

  if (!mesh)
    return -1;
  if (!mesh->lsize)
    return -1;
  lsize = mesh->lsize;

  cndat.x = cndat.y = 0.0f;
  cndat.z = 1.0f;
  cnormal = &cndat;

  for(i  = 0; i < lsize; i++){
    switch(mesh->list[i]){

    case IMOD_MESH_BGNPOLY:
      fprintf(fout, "Polygon \"P\" [");
      while(mesh->list[++i] != IMOD_MESH_ENDPOLY){
        fprintf(fout, " %g %g %g ",
                mesh->vert[mesh->list[i]].x,
                mesh->vert[mesh->list[i]].y,
                mesh->vert[mesh->list[i]].z * z);
      }
      fprintf(fout, "]\n");
      break;

    case IMOD_MESH_BGNBIGPOLY:
      /* todo: draw concave poly */
      while(mesh->list[++i] != IMOD_MESH_ENDPOLY);
      break;

    case IMOD_MESH_BGNPOLYNORM:
    case IMOD_MESH_BGNPOLYNORM2:
      imodMeshPolyNormFactors(mesh->list[i++], &listInc, &vertBase, &normAdd);
      while (mesh->list[i] != IMOD_MESH_ENDPOLY){
        for (j = 0; j < 3; j++) {
          norm[j] = mesh->vert[mesh->list[i + vertBase]];
          vert[j] = mesh->vert[mesh->list[i] + normAdd];
          i += listInc;
        }

        fprintf(fout, "Polygon \"P\" [");
        fprintf(fout, " %g %g %g ", 
                vert[0].x, vert[0].y, vert[0].z * z);
        fprintf(fout, " %g %g %g ", 
                vert[1].x, vert[1].y, vert[1].z * z);
        fprintf(fout, " %g %g %g ", 
                vert[2].x, vert[2].y, vert[2].z * z);
        fprintf(fout, "] \"N\" [");
        fprintf(fout, " %g %g %g ", 
                norm[0].x, norm[0].y, norm[0].z * z);
        fprintf(fout, " %g %g %g ", 
                norm[1].x, norm[1].y, norm[1].z * z);
        fprintf(fout, " %g %g %g ", 
                norm[2].x, norm[2].y, norm[2].z * z);
        fprintf(fout, "]\n");
      }
      break;

    case IMOD_MESH_SWAP:
      break;
    case IMOD_MESH_BGNTRI:
      while(mesh->list[++i] != IMOD_MESH_ENDTRI);
      break;
    case IMOD_MESH_NORMAL:
      i++;
      cnormal = &mesh->vert[mesh->list[i]];
      break;
    case IMOD_MESH_END:
      i = lsize;
      break;

    default:
      break;
           
           
    }
  }

  return(0);
}



static int pRIB_scat(FILE *fout, Iobj *obj, double z)
{
  int co, pt;
  Icont *cont;
  float ssize = obj->pdrawsize * 0.5f;

  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    if (!cont->psize)
      continue;
    for(pt = 0; pt < cont->psize; pt++){
      fprintf(fout, "TransformBegin\n");
      fprintf(fout, "Translate %g %g %g\n",
              cont->pts[pt].x, cont->pts[pt].y, cont->pts[pt].z * z);
      fprintf(fout, "Scale %g %g %g\n", ssize, ssize, ssize);
      fprintf(fout, "Sphere 1 -1 1 360\n");
      fprintf(fout, "TransformEnd\n");
    }
  }
  return(0);
}


static int pRIB_tubes(FILE *fout, Iobj *obj, double z)
{
  int co, pt, lpt;
  Icont *cont;

  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    lpt = cont->psize - 1;
    if (lpt <= 0)
      continue;
      
    for(pt = 0; pt < lpt; pt++){
      prib_tube(fout, &cont->pts[pt], &cont->pts[pt+1], 
                8, obj->linewidth, z);
    }
  }
  return(0);
}

static int prib_tube(FILE *fout, Ipoint *p1, Ipoint *p2, 
                     int slices, int linewidth, double z)
{
  Ipoint norm, offset[2];
  Imat *mat = imodMatNew(2);
  double astep;
  int sl;

  if (!mat) return(-1);
  norm.x = 0.0f; norm.y = linewidth;
  offset[0] = norm;
  astep = 360 / slices;

  for(sl = 0; sl < slices; sl++){
    imodMatRot(mat, astep, 0);
    imodMatTransform(mat, &norm, &offset[1]);

    fprintf(fout, "Polygon \"P\" ");
    fprintf(fout, "[ %g %g %g  %g %g %g  %g %g %g  %g %g %g ]\n",
            p2->x + offset[0].x, p2->y + offset[0].y, p2->z * z,
            p2->x + offset[1].x, p2->y + offset[1].y, p2->z * z,
            p1->x + offset[1].x, p1->y + offset[1].y, p1->z * z,
            p1->x + offset[0].x, p1->y + offset[0].y, p1->z * z);
    offset[0] = offset[1];
  }
  imodMatDelete(mat);
  return(0);
}
