/*
 *  imod2meta.c -- convert an imod model to the QuickDraw 3D meta format.
 *
 *  Author: James Kremer email: James.Kremer@netherworld.com
 */

/*****************************************************************************
 *   Copyright (C) 1996 by James Kremer                                      *
 *   All Rights reserved.                                                    *
 *                                                                           *
 *   You are allowed to make copies and modifications to this file.          *
 *                                                                           *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include "imodel.h"


static float Zscale = 1.0f;
int LinesOnly = 0;
int ObjectOnly = -1;

static void printAttributes(FILE *f, Iobj *obj)
{
  fprintf(f, "\tContainer (\n");
  fprintf(f, "\t\tAttributeSet ( )\n");
  fprintf(f, "\t\tDiffuseColor ( %g %g %g )\n",
          obj->red, obj->green, obj->blue);

  /*     fprintf(f, "\tFillStyle ( Edges )\n"); */
  /* 0 = opac, 1 = invisable.
     trans = obj->trans*0.001;
     fprintf(f, "\tTransparencyColor( %g %g %g )\n",
     trans, trans, trans);
  */

  fprintf(f, "\t)\n");
}

static void printContourLines(FILE *f, Icont *cont, int style)
{
  int pt;
  int psize = cont->psize;

  if (style) psize++;
  fprintf(f, "\tPolyline ( %d\n", psize);

  for(pt = 0; pt < cont->psize; pt++){
    fprintf(f, "\t\t%g %g %g\n",
            cont->pts[pt].x, 
            cont->pts[pt].y,
            Zscale * cont->pts[pt].z);
  }
  if (style)
    fprintf(f, "\t\t%g %g %g\n",
            cont->pts[0].x, 
            cont->pts[0].y,
            Zscale * cont->pts[0].z);
  fprintf(f, "\t)\n");
}

static void printContourPoly(FILE *f, Icont *cont)
{
  int pt;

  fprintf(f, "\tGeneralPolygon (\n\t\t1\n\t\t%d\n",
          cont->psize);
  for(pt = 0; pt < cont->psize; pt++){
    fprintf(f, "\t\t%g %g %g\n",
            cont->pts[pt].x, 
            cont->pts[pt].y,
            Zscale * cont->pts[pt].z);
  }
  fprintf(f, "\t)\n");
}

static int printMesh(FILE *f, Iobj *obj, Imesh *mesh)
{
  int i;
  int nfaces = 0;
  int nverts = 0;


     

  fprintf(f, "Container (\n");
  fprintf(f, "\tMesh (\n");


     
     
  fprintf(f, "\t\t%d\n", mesh->vsize);
  for (i = 0; i < mesh->vsize; i++){
    fprintf(f, "\t\t%g %g %g\n",
            mesh->vert[i].x, mesh->vert[i].y,mesh->vert[i].z);
  }
  fprintf(f, "\t\t%d\n", nfaces);
  fprintf(f, "\t\t0\n");
  for(i = 0; i < nfaces; i++){
      
  }

  printAttributes(f, obj);

#ifdef USE_NORMALS
  fprintf(f, "\tContainer (\n");
  fprintf(f, "\tAttributeVertexSet ( )\n");
  fprintf(f, "\tNormal(\n");
  for (i = 0; i < nverts; i++){
      
  }
  fprintf(f, "\t)\n");
  fprintf(f, "\t)\n");
#endif

  fprintf(f, "\t)\n");
  fprintf(f, ")\n");
  return 0;
}

static int printContourScat(FILE *f, Iobj *obj, Icont *c, int inPsize)
{
  int pt;
  float psize = (float)inPsize * 0.1f;

  for(pt = 0; pt < c->psize; pt++){
    fprintf(f, "Container (\n" 
            "\tEllipsoid ( 0 0 %g %g 0 0 0 %g 0 %g %g %g )\n",
            psize, psize, psize, 
            c->pts[pt].x,
            c->pts[pt].y,
            Zscale * c->pts[pt].z);
    printAttributes(f, obj);
    fprintf(f, ")\n");
  }
  return(0);
}


static int printObject(FILE *f, Imod *m, int ob)
{
  Iobj *obj = &m->obj[ob];
  Icont *cont;
  int co;
  int ec = 0; /* error code. */
     
  for(co = 0; co < obj->contsize; co++){
    cont = &obj->cont[co];
    if (!cont->psize) return -1;

    if (iobjClose(obj->flags)){
      fprintf(f, "Container (\n");
      if ((iobjFill(obj->flags)) && (!LinesOnly))
        printContourPoly(f, cont);
      else
        printContourLines(f, &obj->cont[co], 1);
      printAttributes(f, obj);
      fprintf(f, ")\n");
      continue;
    }

    if (iobjScat(obj->flags)){
      printContourScat(f , obj, cont, obj->pdrawsize*5);
      continue;
    }

    fprintf(f, "Container (\n");
    printContourLines(f, &obj->cont[co], 0);
    printAttributes(f, obj);
    fprintf(f, ")\n");
  }
  return(ec);
}


static int printModel(FILE *f, Imod *m)
{
  int    ob;     /* object index. */
  int    ec = 0; /* error code.   */
  Ipoint min, max, mean;
  Zscale = m->zscale;

  /* DNM: comment out item with no format field */
  fprintf(f, "3DMetafile (1 0 Stream Label0> )\n"/*, m->objsize*/);
  fprintf(f, "\n#Created by imod2meta\n\n");
  fprintf(f, "BeginGroup ( DisplayGroup( ) )\n");

  imodel_minpt(m, &min);
  imodel_maxpt(m, &max);
  mean.x = (max.x + min.x) * 0.5f;
  mean.y = (max.y + min.y) * 0.5f;
  mean.z = (max.z + min.z) * 0.5f;
  fprintf(f, "Translate ( %g %g %g )\n",
          -mean.x, -mean.y, -Zscale*mean.z);


  /* put camera, renderer, lights here */
  if (ObjectOnly < 0){
    for(ob = 0; ob < m->objsize; ob++)
      ec = printObject(f, m, ob);
  }else{
    printf("object %d\n", ObjectOnly);
    ec = printObject(f, m, ObjectOnly);
  }

  fprintf(f, "EndGroup ( )\n");
     
  return(ec);
}

static void usage(void)
{
  printf("imod2meta - Test version 0.02\n");
  printf("Convert an imod model file to the QuickDraw 3D "
         "meta file format.\n");
  printf("imod2meta usage:\n");
  printf("imod2meta [file_in.imod] [file_out.meta]\n");
     
}



int main(int argc, char **argv)
{
     
  int i;
  FILE *fout;
  Imod *imod;

  extern double meshDiameterSize;
  int ch;
     
  if (argc < 2){
    usage();
    exit(1);
  }

  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
      case 'o':
        ObjectOnly = atoi(argv[++i]);
        break;
      case 'l':
        LinesOnly = 1;
        break;
      case 'm':
        puts("mesh");
        break;

      case '?':
        usage();
        exit(1);
        break;
      default:
        exit(1);
        break;
        fprintf(stderr, "imod2meta: unknown option %s\n", argv[i]);
      }
    } else
      break;
  }
     

  if (i >= (argc -1)){
    usage();
    exit(1);
  }
     
  imod = imodRead(argv[i]);
  if (!imod){
    fprintf(stderr, "Imod2meta: Error reading imod model. (%s)\n",
            argv[i]);
    exit(3);
  }
  i = argc-1;
  fout = fopen( argv[i] , "w");
  if (fout == NULL){
    printf("Couldn't open output file %s.\n", argv[i]);
    exit(10);
  }

  printModel(fout, imod);
     
  fclose(fout);
  exit(0);
}
