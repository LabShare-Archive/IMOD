/*****************************************************************************
 *                                                                           *
 *   FILE: imodold.c                                                         *
 *                                                                           *
 *   PURPOSE: Convert an IMOD model file to the old IMOD model format.       *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0 Oct, 1995                                               *
 *									     *
 *       James Kremer  kremer@boulder.colorado.edu			     *
 *****************************************************************************
 *   Copyright (C) 1995 by Boulder Laboratory for 3-Dimensional Fine         *
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
#include <imodel.h>

static int old_imodel_write(struct Mod_Model *mod, FILE *fout);
static int imodel_01write_object(struct Mod_Object *obj, FILE *fout);
static int imodel_01write_contour(struct Mod_Contour *cont, FILE *fout);

int main(int argc, char *argv[])
{
     FILE *fin, *fout;
     Imod *imod;

     if (argc != 3){
	  fprintf(stderr, "%s: usage\n", argv[0]);
	  fprintf(stderr, "%s <input model> <output model>\n", argv[0]);
	  exit(-1);
     }

     imod = imodRead(argv[1]);
     if (!imod){
	  fprintf(stderr, "%s: error reading model %s\n",
		  argv[0], argv[1]);
	  exit(1);
     }

     fout = fopen(argv[2], "w");
     if (!fout){
	  fprintf(stderr, "%s: error opening output file %s\n",
		  argv[0], argv[2]);
	  exit(2);
     }

     if (old_imodel_write(imod, fout)){
	  fprintf(stderr, "%s: error writting model to file.\n", argv[0]);
	  exit(10);
     }
     exit(0);
}


static int old_imodel_write(struct Mod_Model *mod, FILE *fout)
{
     int i,tmp;
     long id;
     int count;
     int extra[3] = {0,0,0};
     rewind(fout);
     
     id = ID_IMOD;
     count = fwrite( &(id), sizeof(long), 1, fout);

     id = IMOD_01;
     if (!fwrite( &(id), sizeof(long), 1, fout))
	  return(-1);

     fwrite( mod->name, 1, IMOD_STRSIZE, fout);
     fwrite( &(mod->xmax), sizeof(int), 3, fout);
     fwrite( &(mod->objsize), sizeof(mod->objsize), 1, fout);
     fwrite( &(mod->flags), sizeof(int), 1, fout);
     fwrite( &(mod->drawmode), sizeof(int), 1, fout);
     fwrite( &(mod->mousemode), sizeof(int), 1, fout);

     fwrite( &(mod->blacklevel), sizeof(int), 1, fout);
     fwrite( &(mod->whitelevel), sizeof(int), 1, fout);
     
     fwrite( &(mod->xoffset), sizeof(float), 1, fout);
     fwrite( &(mod->yoffset), sizeof(float), 1, fout);
     fwrite( &(mod->zoffset), sizeof(float), 1, fout);
     
     fwrite( &(mod->xscale), sizeof(float), 1, fout);
     fwrite( &(mod->yscale), sizeof(float), 1, fout);
     fwrite( &(mod->zscale), sizeof(float), 1, fout);
     
     fwrite( &(mod->cindex.object), sizeof(int), 1, fout);
     fwrite( &(mod->cindex.contour), sizeof(int), 1, fout);
     fwrite( &(mod->cindex.point), sizeof(int), 1, fout);
     fwrite( &(mod->res), sizeof(int), 1, fout);
     fwrite( &(mod->thresh), sizeof(int), 1, fout);
     fwrite( &(mod->pixsize), sizeof(float), 1, fout);
     fwrite( &(mod->units), sizeof(int), 1, fout);
     fwrite( &(mod->csum), sizeof(long), 1, fout);
     fwrite( extra, sizeof(int), 3, fout);
     
     for(i = 0; i < mod->objsize; i++)
	  if (imodel_01write_object( &(mod->obj[i]), fout))
	       return(-1);
     fflush(fout);
     return(0);
}


static int imodel_01write_object(struct Mod_Object *obj, FILE *fout)
{
     int i,tmp;
     long id;

     id = ID_OBJT;
     fwrite( &(id), sizeof(long), 1, fout);
     
     fwrite( obj->name, 1, IMOD_STRSIZE, fout);
     fwrite( &(obj->contsize), sizeof(obj->contsize), 1, fout);
     fwrite( &(obj->flags), sizeof(long), 1, fout);
     fwrite( &(obj->axis), sizeof(int), 1, fout);
     fwrite( &(obj->drawmode), sizeof(int), 1, fout);
     fwrite( &(obj->red), sizeof(float), 1, fout);
     fwrite( &(obj->green), sizeof(float), 1, fout);
     fwrite( &(obj->blue), sizeof(float), 1, fout);
     
     fwrite( &(obj->pdrawsize), sizeof(int), 1, fout);
     tmp = obj->linewidth;
     fwrite( &tmp, sizeof(int), 1, fout);
     tmp = obj->trans;
     fwrite( &tmp, sizeof(int), 1, fout);
     fwrite( &(obj->meshsize), sizeof(int), 1, fout);
     
     for(i = 0; i < obj->contsize; i++)
	  if (imodel_01write_contour( &(obj->cont[i]), fout))
	       return(-1);
     return(0);
}

static int imodel_01write_contour(struct Mod_Contour *cont, FILE *fout)
{
     int i = 0;
     long id;
     
     id = ID_CONT;
     fwrite( &(id), sizeof(long), 1, fout);
     fwrite( &(cont->psize), sizeof(cont->psize), 1, fout);
     fwrite( &(cont->flags), sizeof(long), 1, fout);
     fwrite( &(cont->type), sizeof(int), 1, fout);
     /* Surface undefined in version 01. set to 0 */
     fwrite( &(i), sizeof(int), 1, fout);
     
     id = ID_PNTS;
     fwrite( &(id), sizeof(long), 1, fout);
     
     for(i = 0; i < cont->psize; i++){
	  if (!fwrite( &(cont->pts[i].x), sizeof(float), 1, fout))
	       return(-1);
	  if (!fwrite( &(cont->pts[i].y), sizeof(float), 1, fout))
	       return(-1);
	  if (!fwrite( &(cont->pts[i].z), sizeof(float), 1, fout))
	       return(-1);
     }
     return(0);
}
