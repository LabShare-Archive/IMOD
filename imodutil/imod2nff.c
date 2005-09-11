/*  UNSUPPORTED
 *
 *  imod2nff.c -- Convert imod model files to the  nff file format.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <stdio.h>
#include <stdlib.h>
#include "imodel.h"

int WriteNFF(struct Mod_Model *mod, FILE *fout);
static int usage(void)
{
  printf("imod2nff, Version 2.0b1 %s %s\n", __DATE__,__TIME__);
  printf("IMOD2NFF: Usage,  "
         "imod2nff [option] <infile.imod> <outfile.nff>\n");
  printf("\tOptions: -m Force output of mesh data.\n");
  exit(1);
}

main( int argc, char *argv[])
{
  Imod *imod;
  FILE *fout  = NULL;
  int i, ch;
  char *options = "m";
  int forceMesh = 0;

  extern char *optarg;
  extern int optind;

  if (argc < 3) usage();

  while ((ch = getopt(argc, argv, options)) != EOF)
    switch (ch){
         
    case 'm':
      forceMesh = 1;
      break;

    case '?':
    default:
      usage();
      break;

    }
  i = optind;
  imod = imodRead(argv[i]);
  if (!imod){
    fprintf(stderr, "Imod2NFF: Error reading model %s.\n",
            argv[i]);
    exit(3);
  }
  i++;
     
  fout = fopen(argv[i], "w");
  if (fout == NULL){
    printf("Couldn't open output file %s.\n", argv[i]);
    exit(10);
  }

  if (forceMesh){
    int ob;
    for(ob = 0; ob < imod->objsize; ob++){
      if (imod->obj[ob].meshsize)
        imod->obj[ob].flags |= IMOD_OBJFLAG_MESH ;
    }
  }
     
  WriteNFF(imod, fout);
     
  fclose(fout);
}


int WriteNFF(struct Mod_Model *mod, FILE *fout)
{
#ifdef USE_LIBRARY
  return(imod_to_nff(mod, fout));
#endif
     
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  int objnum;
  int contnum;
  int i, j;
  int useMesh;
  float r;
  float xo = 0, yo = 0, zo = 0;  /* x,y,z offsets. */
  float xs = 1, ys = 1, zs = 1;  /* x,y,z scale.   */
  int listInc, vertBase, normAdd;
    
  xo = mod->xoffset;
    
  /* Loop through objects. */
  for (objnum = 0; objnum < mod->objsize; objnum++){
    
    /* Set color for each object. */
    obj = &(mod->obj[objnum]);
    
    fprintf(fout, "# object %d with %d contours.\n", 
            objnum, obj->contsize);
    fprintf(fout, "f %g %g %g 0 0 0 0 0\n",
            obj->red, obj->green, obj->blue);
    
    useMesh = imodObjectGetValue(obj, IobjFlagMesh);
    
    if (useMesh){
      int i, mi;
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
          r = obj->pdrawsize;
          r *= 0.5f;
          for (i = 0; i < cont->psize; i++)
            fprintf(fout, "s %g %g %g %g\n",
                    (cont->pts[i].x + mod->xoffset) * mod->xscale,
                    (cont->pts[i].y + mod->yoffset) * mod->yscale,
                    (cont->pts[i].z + mod->zoffset) *mod->zscale,
                    r);
            
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

