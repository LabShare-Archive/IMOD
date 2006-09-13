/*****************************************************************************/
/*                                                                           */
/*                      University of Colorado                               */
/*          ---------------------------------------------------              */
/*          Boulder Laboratory for 3-Dimensional Fine Structure              */
/*          ---------------------------------------------------              */
/*                                                                           */
/*   FILE: imodclean.c                                                       */
/*                                                                           */
/*   PURPOSE:  Gets rid of empty arrays and out of bounds data.              */
/*             Makes closed contours have same z value.                      */
/*                                                                           */
/*   AUTHOR: James Kremer Oct  1994                                          */
/*                                                                           */
/*****************************************************************************/
#include <stdio.h>
#include "imodel.h"

int cleanzero(Imod *imod);

main( int argc, char *argv[])
{
     FILE               *fin, *fout;
     struct Mod_Model   model;
     int i;
     
     if (argc != 3){
	  fprintf(stderr, "%s: usage %s <infile> <outfile>\n",
		  argv[0], argv[0]);
	  exit(3);
     }

     i = 1;
     fin = fopen(argv[i], "rb");
     if (!fin){
	  fprintf(stderr, "%s: Error, couldn't open file %s\n",
		  argv[0], argv[i]);
	  exit(3);
     }

     imodDefault(&model);
     model.file = fin;
     if (imodReadFile(&model)){
	  fprintf(stderr, "%s: Error reading imod model. (%s)\n",
		  argv[0], argv[i]);
	  perror("Model read");
	  fprintf(stderr, "Output model may be corrupt.\n");
     }
     
     
     i = 2;
     fout = fopen(argv[i], "wb");
     if (!fout){
	  fprintf(stderr, "%s: Error, couldn't open file %s\n",
		  argv[0], argv[i]);
	  exit(3);
     }

     imodel_model_clean(&model, 0);

     cleanzero(&model);


     imodWrite(&model, fout);

     fclose(fin);
     fclose(fout);
}


int cleanzero(Imod *imod)
{
    Iobj *obj;
    Icont *cont;
    Ipoint *pnt;
    int ob,co,pt;

    for(ob = 0 ; ob < imod->objsize; ob++){
	obj = &imod->obj[ob];
	for(co = 0; co < obj->contsize; co++){
	    cont = &obj->cont[co];
	    for(pt = 0; pt < cont->psize; pt++){
		pnt = &cont->pts[pt];

		if ((pnt.x < 0.001f) && (pnt.x > -0.001f))
		    pnt.x = 0.0f;
		if ((pnt.y < 0.001f) && (pnt.y > -0.001f))
		    pnt.y = 0.0f;
		if ((pnt.z < 0.001f) && (pnt.z > -0.001f))
		    pnt.z = 0.0f;
		    
	    }
	}
    }
}




