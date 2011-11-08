/*
 *  imod2vrml - convert IMOD model to VRML version 1
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */


#include <stdio.h>
#include <string.h>
#include <math.h>
#include "imodel.h"

#define VRML_REDUNDANT 1

static int tablevel;
static int lowres = 0;

/* Function Prototypes */
int imod_to_vrml(Imod *imod, int flags, FILE *fout);
static void printCamera(Imod *imod, FILE *fout);
static void printInfo(Imod *imod, FILE *fout);
static void printLight( Imod *imod, FILE *fout);

static void printObject(Imod *imod, int ob, int flags, FILE *fout);
static void printContours(Imod *imod, Iobj *obj, FILE *fout);
static void printFilledContours(Imod *imod, Iobj *obj, FILE *fout);
static void printScatContours(Imod *imod, Iobj *obj, FILE *fout);
static void printMesh(Imod *imod, int ob, int flags, FILE *fout);

static void usage(int error)
{
  printf("\nConverts an imod model to the Virtual Reality Modeling "
         "Language format.\n");
  printf("Usage: imod2vrml [-l | -s] <imod model file> <output_file.wrl>\n");
  printf("       The -l option selects low-resolution meshes, if any.\n");
  printf("       The -s option outputs separate point/normal list for "
         "each polygon.\n");
  exit(error);
}

int main( int argc, char *argv[])
{
  int i;
  FILE *fout;
  Imod *imod;
  int flags = 0;
  char *progname = imodProgName(argv[0]);

  if (argc < 2) {
    imodVersion(progname);
    imodCopyright();
    usage(0);
  }

  for (i = 1; i < argc; i++){
    if (argv[i][0] == '-')
      switch (argv[i][1]){
      case 'l':
        lowres = 1;
        break;

      case 's':
        flags |= VRML_REDUNDANT;
        break;

      default:
        usage(-1);
        break;

      }
    else
      break;
  }

  if (argc - i != 2)
    usage(-1);

  fout = fopen( argv[i + 1] , "w");
  if (fout == NULL){
    printf("Couldn't open output file %s.\n", argv[i + 1]);
    exit(10);
  }
     
  imod = imodRead(argv[i]);
  if (!imod){
    fprintf(stderr, "%s: Error reading imod model %s\n", argv[0], 
            argv[i]);
    exit(3);
  }
     
  imod_to_vrml(imod, flags, fout);

  fclose(fout);
  exit(0);
}

int imod_to_vrml(Imod *imod, int flags, FILE *fout)
{
  int ob;

  fprintf(fout, "#VRML V1.0 ascii\n");
  fprintf(fout, "\nSeparator {\n");
  {
    printInfo(imod,fout); 
    printLight(imod,fout);
    printCamera(imod,fout);
    for(ob = 0; ob < imod->objsize; ob++)
      printObject(imod, ob,flags, fout);
  }
  fprintf(fout, "}\n");
  return 0;
}


static void printCamera(Imod *imod, FILE *fout)
{
  float xpos, ypos, zpos;
  float xo, yo, zo;
  float fd;
  double r;
  Ipoint maxp, minp;
  float ator = 0.0174533;

  fd = xo = yo = zo = 0.0f;
  imodel_maxpt(imod, &maxp);
  imodel_minpt(imod, &minp);

  maxp.z *= imod->zscale;
  minp.z *= imod->zscale;
  r = ((maxp.x - minp.x) * (maxp.x - minp.x)) +
    ((maxp.y - minp.y) * (maxp.y - minp.y)) +
    ((maxp.z - minp.z) * (maxp.z - minp.z));
  r = sqrt(r);
  xpos = ((maxp.x - minp.x)*0.5)+minp.x;
  ypos = ((maxp.y - minp.y)*0.5)+minp.y;
  zpos = ((maxp.z - minp.z)*0.5)+minp.z;
  fd = r;

  fprintf(fout, "\tDEF Viewer Info { string \"examiner\" }\n");

  fprintf(fout, "\tTranslation { \n\t\ttranslation %g %g %g\n\t}\n",
          -xpos, -ypos, -zpos);

  fprintf(fout, "\tPerspectiveCamera {\n");
  {
    fprintf(fout, "\t\tposition 0 0 %g\n", r);
    fprintf(fout, "\t\torientation %g %g %g 0\n", xo, yo, zo);
    fprintf(fout, "\t\tfocalDistance %g\n", fd);
  }
  fprintf(fout, "\t}\n");
             
  if (imod->cview) {
    fprintf(fout, "\tRotation {\n\t\trotation  1 0 0  %g\n\t}\n",
            ator * imod->view[imod->cview].rot.x);
    fprintf(fout, "\tRotation {\n\t\trotation  0 1 0  %g\n\t}\n",
            ator * imod->view[imod->cview].rot.y);
    fprintf(fout, "\tRotation {\n\t\trotation  0 0 1  %g\n\t}\n",
            ator * imod->view[imod->cview].rot.z);
  }    

  return;
}

static void printLight( Imod *imod, FILE *fout)
{

  fprintf(fout, "\tDirectionalLight {\n"
          "\t\tdirection 1 -1 -1  \n\t}\n");
     
}

static void printInfo(Imod *imod, FILE *fout)
{
  fprintf(fout, "\tSeparator {\n");
  {
    fprintf(fout, "\t\tInfo {\n");
    fprintf(fout, "\t\t\tstring \"Created by Imod\"\n");
    fprintf(fout, "\t\t}\n");
  }
  fprintf(fout, "\t}\n");
  return;
}

static void printMaterial(Iobj *obj, int *lastuse, int usefill, FILE *fout)
{
  float diffuse = ((int)obj->diffuse) / 255.;
  float specular = ((int)obj->specular) / 255.;
  float shininess = ((int)obj->shininess) / 255.;
  float red = obj->red;
  float green = obj->green;
  float blue = obj->blue;
  if (*lastuse == usefill)
    return;

  if (usefill) {
    red = (float)obj->fillred / 255.;
    green = (float)obj->fillgreen / 255.;
    blue = (float)obj->fillblue / 255.;
  }
  *lastuse = usefill;

  /* DMN 1/18/01: VRMLview gave artifacts with shininess at 0 */
  if (shininess < 0.01)
    shininess= 0.01;
     
  /* material definition. */
  fprintf(fout, "\tMaterial{\n");
  /* DNM 1/18/01: in VRMLview ambient color did not affect surface 
     display, did affect line display, so no need to figure out if
     displaying lines or surfaces and scaling by obj->ambient */
  fprintf(fout, "\t\tambientColor %g %g %g\n",
          red, green, blue);
  fprintf(fout, "\t\tdiffuseColor %g %g %g\n",
          red * diffuse, green * diffuse, blue * diffuse);
  fprintf(fout, "\t\tspecularColor %g %g %g\n",
          red * specular, green * specular, blue * specular);
          
  /*      if (!iobjLight(flags))
          fprintf(fout, "\t\temissiveColor %g %g %g\n",
          red, green, blue); */

  /* DNM 1/18/01: this needed to be 0's for proper display */
  fprintf(fout, "\t\temissiveColor 0 0 0\n");
     
  fprintf(fout, "\t\tshininess %g\n", shininess);
  fprintf(fout, "\t\ttransparency %g\n", obj->trans/100.0f);
  fprintf(fout, "\t} #Material\n");
}


static void printObject(Imod *imod, int ob, int flags, FILE *fout)
{
  int co;
  Iobj *obj = &imod->obj[ob];
  int lastuse = -1;
  int hasSpheres = (iobjScat(obj->flags) || obj->pdrawsize) ? 1 : 0;
  if (iobjOff(obj->flags))
    return;

  /* See if object has spheres to draw */
  for (co = 0; co < obj->contsize && !hasSpheres; co++)
    if (obj->cont[co].sizes)
      hasSpheres = 1;

  fprintf(fout, "DEF Object%dData Separator {\n", ob);

  /* Call scattered point routine for all objects with point sizes 
     for some reason this needs to be first */
  if (hasSpheres) {
    printMaterial(obj, &lastuse, obj->flags & IMOD_OBJFLAG_FCOLOR, fout);
    printScatContours(imod, obj, fout);
  }
  
  /* Call routines for drawing various kinds of non-scattered points */
  if (iobjMesh(obj->flags))
    printMesh(imod, ob, flags, fout);
  else if (iobjFill(obj->flags) && iobjClose(obj->flags))
    printFilledContours(imod, obj, fout);
  else if (!iobjScat(obj->flags)) {
    printMaterial(obj, &lastuse, 0, fout);
    printContours(imod, obj, fout);
  }

  fprintf(fout, "} #Object%dData\n", ob);
  return;
}


static void printScatContours(Imod *imod, Iobj *obj, FILE *fout)
{
  Icont *cont;
  int hasSpheres = (iobjScat(obj->flags) || obj->pdrawsize) ? 1 : 0;
  int co, pt;
  float zscale = imod->zscale;
  float size;

  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    
    /* Skip the contour if it is empty or if object is not scattered and there 
       are no sizes */
    if (!cont || !cont->psize || (!hasSpheres && !cont->sizes))
      continue;
    for (pt = 0; pt < cont->psize; pt++) {
      size = imodPointGetSize(obj, cont, pt);

      /* Draw point only if size is nonzero */
      if (size > 0.)
        fprintf(fout, "\tDEF PntDat Separator { "
                "Translation { translation %g %g %g}"
                "Sphere { radius %f } }\n",
                cont->pts[pt].x,
                cont->pts[pt].y,
                cont->pts[pt].z * zscale, size);
    }
  }
}

/* Draws open or closed lines if object not filled. */
static void printContours(Imod *imod, Iobj *obj, FILE *fout)
{
  Icont *cont;
  int co, pt;
  float zscale = imod->zscale;

  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    if (!cont )
      continue;
    if (!cont->psize)
      continue;

    fprintf(fout, "\tDEF ContourData Coordinate3 {\n");
    fprintf(fout, "\t\tpoint [\n");
    for(pt = 0; pt < cont->psize; pt++)
      fprintf(fout, "\t\t%g %g %g%c\n",
              cont->pts[pt].x,
              cont->pts[pt].y,
              cont->pts[pt].z * zscale,
              (pt == (cont->psize-1))?']':',');
    fprintf(fout, "\t}\n");
    if (iobjLine(obj->flags)) {
      fprintf(fout, "\tIndexedLineSet {\n");
      fprintf(fout, "\t\tcoordIndex [");
      for(pt = 0; pt < cont->psize; pt++) {
        fprintf(fout, "%d,", pt);
        if ((pt % 10) == 9)
          fprintf(fout, "\n");
      }
      if (iobjClose(obj->flags))
        fprintf(fout, "0");
      fprintf(fout, "]\n\t}\n");
    } else {
      fprintf(fout, "\tPointSet {\n\t\tstartIndex 0\n"
              "\t\tnumPoints -1\n\t}\n");
    }            
  }
  return;
}

static void printFilledContours(Imod *imod, Iobj *obj, FILE *fout)
{
  Icont *cont;
  int co, pt;
  float zscale = imod->zscale;
  int lastuse = -1;
     
  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    if (!cont )
      continue;
    if (cont->psize < 3)
      continue;

    printMaterial(obj, &lastuse, obj->flags & IMOD_OBJFLAG_FCOLOR, fout);
          
    fprintf(fout, "\tDEF ContourData Coordinate3 {\n");
    fprintf(fout, "\t\tpoint [\n");
    for(pt = 0; pt < cont->psize; pt++)
      fprintf(fout, "\t\t%g %g %g%c\n",
              cont->pts[pt].x,
              cont->pts[pt].y,
              cont->pts[pt].z * zscale,
              (pt == (cont->psize-1))?']':',');
    fprintf(fout, "\t}\n");
    fprintf(fout, "\tIndexedFaceSet {\n");
    fprintf(fout, "\t\tcoordIndex [");
    for(pt = 0; pt < cont->psize; pt++) {
      fprintf(fout, "%d,", pt);
      if (pt % 10 == 9)
        fprintf(fout, "\n");
    }
    if (iobjClose(obj->flags))
      fprintf(fout, "0");
    fprintf(fout, "]\n\t}\n");
    if (iobjLine(obj->flags)) {
      printMaterial(obj, &lastuse, 0, fout);
      fprintf(fout, "\tIndexedLineSet {\n");
      fprintf(fout, "\t\tcoordIndex [");
      for(pt = 0; pt < cont->psize; pt++) {
        fprintf(fout, "%d,", pt);
        if (pt % 10 == 9)
          fprintf(fout, "\n");
      }
      fprintf(fout, "0");
      fprintf(fout, "]\n\t}\n");
    }
  }
}



static void printMesh(Imod *imod, int ob, int flags, FILE *fout)
{
  Iobj *obj = &imod->obj[ob];
  Imesh *mesh;
  int me, i, j;
  float zscale = imod->zscale;
  int *ilist, *mapping;
  int lsize;
  int index, ind, poly;
  Ipoint norm;
  int lastuse = -1;
  int resol;
  int listInc, vertBase, normAdd;

  imodMeshNearestRes(obj->mesh, obj->meshsize, lowres, &resol);

  if (iobjFill(obj->flags))
    fprintf(fout, "\tNormalBinding { value PER_VERTEX }\n");
  else
    fprintf(fout, "\tNormalBinding { value OVERALL }\n");
         
  /* Everywhere below, get rid of extraneous commas and tabs, limit
     resolution to 5 digits for coordinates, 3 digits for normals */
  for(me = 0; me < obj->meshsize; me++){
    if (imeshResol(obj->mesh[me].flag) != resol)
      continue;
    poly = 0;
    mesh = &obj->mesh[me];
    ilist = (int *)malloc(sizeof(int) * mesh->lsize);
    if (!ilist) {
      printf("Error allocating memory\n");
      exit(1);
    }
    if (flags & VRML_REDUNDANT) {
      mapping = (int *)malloc(sizeof(int) * mesh->vsize);
      if (!mapping) {
        printf("Error allocating memory\n");
        exit(1);
      }
    }


    /* Output coordinates unless doing redundant output */
    lsize = 0;
    if (!(flags & VRML_REDUNDANT) || 
        !(iobjFill(obj->flags) || iobjLine(obj->flags))) {
      fprintf(fout, "\tDEF Obj%dMesh%dData  Coordinate3 {\n",
              ob, me);
      fprintf(fout, "\t\tpoint [\n");
      for(i = 0; i < mesh->vsize; i+=2){
        fprintf(fout, "%.5g %.5g %.5g%c\n",
                mesh->vert[i].x,
                mesh->vert[i].y,
                mesh->vert[i].z * zscale,
                (i >= (mesh->vsize-2))?']':',');
      }
      fprintf(fout, "\t}\n");
    }
        
    /* Output normals if doing fill and not doing redundant output */
    if (!(flags & VRML_REDUNDANT) && iobjFill(obj->flags)) {
      fprintf(fout, "\tDEF Obj%dMesh%dNData Normal {\n",
              ob, me);
      fprintf(fout, "\t\tvector [\n");
      for(i = 1; i < mesh->vsize; i+=2){
        norm = mesh->vert[i];
        imodPointNormalize(&norm);
        fprintf(fout, "%.3f %.3f %.3f%c\n",
                norm.x, norm.y, norm.z,
                (i >= (mesh->vsize-1))?']':',');
      }
      fprintf(fout, "\t}\n");
    }

    if (iobjFill(obj->flags) || iobjLine(obj->flags)) {
      for(i = 0; i < mesh->lsize; i++){
        if (mesh->list[i] == IMOD_MESH_END)
          break;
               
        if (imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase, 
                                    &normAdd)) {
          
          /* Copy the indices to the new index array numbered from 0 */
          lsize = 0;
          i++;
          while (mesh->list[i] != IMOD_MESH_ENDPOLY) {
            ilist[lsize++] = mesh->list[i + vertBase]/2;
            i += listInc;
            ilist[lsize++] = mesh->list[i + vertBase]/2;
            i += listInc;
            ilist[lsize++] = mesh->list[i + vertBase]/2;
            i += listInc;
          }

          if (!lsize)
            continue;

          /* If doing redundant output, output the points and convert the 
             indexes */
          poly++;
          if (flags & VRML_REDUNDANT) {
            fprintf(fout, "\tDEF Obj%dMesh%dPoly%dData  Coordinate3 {\n",
                    ob, me, poly);
            fprintf(fout, "\t\tpoint [\n");

            index = 0;
            for (j = 0; j < mesh->vsize; j++)
              mapping[j] = -1;
            for (ind = 0; ind < lsize; ind++) {
              j = 2 * ilist[ind];

              /* If mapping doesn't exit yet, output the point */
              if (mapping[j] < 0) {
                mapping[j] = index++;
                if (index > 1)
                  fprintf(fout,",\n");
                fprintf(fout, "%.5g %.5g %.5g",
                        mesh->vert[j].x,
                        mesh->vert[j].y,
                        mesh->vert[j].z * zscale);
              }
            }
            fprintf(fout, "]\n\t}\n");

            /* If doing fill, output normals next, recreate mapping */
            if (iobjFill(obj->flags)) {
              fprintf(fout, "\tDEF Obj%dMesh%dPoly%dNData Normal {\n",
                      ob, me, poly);
              fprintf(fout, "\t\tvector [\n");

              index = 0;
              for (j = 0; j < mesh->vsize; j++)
                mapping[j] = -1;
              for (ind = 0; ind < lsize; ind++) {
                j = 2 * ilist[ind];
                if (mapping[j] < 0) {
                  mapping[j] = index++;
                  if (index > 1)
                    fprintf(fout,",\n");
                  norm = mesh->vert[j + 1];
                  imodPointNormalize(&norm);
                  fprintf(fout, "%.3f %.3f %.3f",
                          norm.x, norm.y, norm.z);
                }
              }
              fprintf(fout, "]\n\t}\n");
            }

            /* Then replace indexes with ones in the polygon-specific set */
            for (ind = 0; ind < lsize; ind++) {
              j = 2 * ilist[ind];
              ilist[ind] = mapping[j];
            }
          }

          /* If showing surface, put out face set */
          if (iobjFill(obj->flags)){
            printMaterial(obj, &lastuse,
                          obj->flags & IMOD_OBJFLAG_FCOLOR, 
                          fout);
            fprintf(fout, "\tIndexedFaceSet {\n");
            fprintf(fout, "\t\tcoordIndex [");
            for(index = 0; index < lsize / 3; index++){
              ind = 3 * index;
              fprintf(fout, "%d,%d,%d,-1%c\n", 
                      ilist[ind],
                      ilist[ind + 1], ilist[ind + 2],
                      (index >= (lsize/3-1))?']':',');
            }
            fprintf(fout,"\t}\n");
          }

          /* If showing lines, put out line set */
          if (iobjLine(obj->flags)){
            printMaterial(obj, &lastuse, 0, fout);
            fprintf(fout, "\tIndexedLineSet {\n");
            fprintf(fout, "\t\tcoordIndex [");
            for(index = 0; index < lsize / 3; index++){
              ind = 3 * index;
              fprintf(fout, "%d,%d,%d,%d,-1%c\n",
                      ilist[ind],
                      ilist[ind + 1], ilist[ind + 2],
                      ilist[ind],
                      (index >= (lsize/3-1))?']':',');
            }
            fprintf(fout, "\t}\n");
          }
                         
        } 
      }
    } else {
      /* If showing neither, define a point set */
      printMaterial(obj, &lastuse, 0, fout);
      fprintf(fout, "\tPointSet {\n\t\tstartIndex 0\n"
              "\t\tnumPoints -1\n\t}\n");
    }
    if (ilist) 
      free(ilist);
    if ((flags & VRML_REDUNDANT) && mapping)
      free(mapping);
  }
}

/*
$Log$
Revision 3.7  2007/04/11 21:22:15  mast
Added option to output points/normals for each polygon

Revision 3.6  2006/08/31 23:13:15  mast
Changed mat1 to real names

Revision 3.5  2005/09/11 19:22:11  mast
Changes for new style of mesh

Revision 3.4  2005/02/03 17:25:09  mast
Prevented incorrect output for empty polygons

Revision 3.3  2005/01/26 22:28:40  mast
Made all points with sizes be displayed as spheres

Revision 3.2  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.1  2002/12/23 21:37:14  mast
fixed exit status

*/
