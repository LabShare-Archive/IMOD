/*
 *  rec2imod.c -- Convert HVEM3D rec files from an IBM PC to IMOD model files.
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *          David Mastronarde, version 1.1, mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

#include <stdio.h>
#include <stdlib.h>
#include "imodel.h"
#include "recfile.h"

static void usage(void)
{
     fprintf(stderr, "rec2imod version 1.1 usage:\n");
     fprintf(stderr, "rec2imod [options] [hvem filename] [imod filename]\n");
     fprintf(stderr, "\toptions:\n");
     fprintf(stderr, "\t\t-m [float] - magnification in kx.\n");
     fprintf(stderr, "\t\t-t [float] - thickness of each section in um.\n");
     fprintf(stderr, "\t\t-p [float] - pixel size in mm. (default 0.1)\n");
     fprintf(stderr, "\t\t-z [float] - zscale (used only if no mag and thickness entered)\n");
     fprintf(stderr, "\t\t-s [float] - scaling factor, to reduce dimensions of data\n");
     return;
}

main (int argc, char **argv)
{
     FILE *fin, *fout;
     Imod *imod;
     Iobj *obj;
     Ipoint point;
     Icont *cont;
     HvemHead *head;
     HvemContour *hvemcont;
     int i,pt;
     int tmp;
     int maxcont = -1;
     float scaling = 1.0;
     int ob, co;
     short tlist[256];

     float Xscale = 1.0;
     float Yscale = 1.0;
     float Zscale = 1.0;
     float unitsize = 0.1;
     float mag = 0.0;
     float section = 0.0;
     float facx, facy;
     
     if (argc < 2){
	  usage();
	  exit(3);
     }

     for (i = 1; i < argc ; i++){
	  if (argv[i][0] == '-'){
	       switch (argv[i][1]){
		
		  case 'm': /* magnification 1000x */
		    mag = atof(argv[++i]);
		    break;

		  case 't':  /* thickness of section */
		    section = atof(argv[++i]);
		    break;

		  case 'p': /* pixel size in mm */
		    unitsize = atof(argv[++i]);
		    break;
    
		  case 'x':
		    Xscale = atof(argv[++i]);
		    break;
		    
		  case 'y':
		    Yscale = atof(argv[++i]);
		    break;
		    
		  case 'z':
		    Zscale = atof(argv[++i]);
		    break;
		    
		  case 'c':
		    maxcont = atof(argv[++i]);
		    break;
		    
		  case 's':
		    scaling = atof(argv[++i]);
		    break;
		    
		  default:
		    break;
	       }
	  }else
	       break;
     }

     if (i > (argc - 2)){
	  usage();
	  exit(1);
     }

     fin = fopen(argv[i++], "r");
     if (!fin){
	  fprintf(stderr, "Couldn't open %s\n", argv[i]);
	  exit(3);
     }
     
     fout = fopen(argv[i], "wb");
     if (!fout){
	  fprintf(stderr, "Couldn't open %s\n", argv[i]);
	  exit(3);
     }

     head = hvem3D_read_head(fin);
     head->unitsize = unitsize;


/*     if (!head->ltype){
	  fprintf(stderr, "Error, No types found.\n");
	  exit(3);
     }
*/

     imod = imodNew();
     for(i = 0; i < 256; i++)
	  tlist[i] = -1;

     if(maxcont > 0) head->lastdir = maxcont;
     for(i = 0; i < head->lastdir; i++){

	  printf("\rcontour %d of %d          \r", i+1, head->lastdir);
	  fflush(stdout);

	  hvemcont = hvem3D_read_contour(fin, i); 
	  
	  if (!hvemcont)
	       continue;

	  if (hvemcont->del){
	       hvem3D_free_contour(hvemcont);
	       continue;
	  }

	  if (tlist[hvemcont->type] < 0){
	       imodNewObject(imod);
	       tlist[hvemcont->type] = imod->objsize - 1;
	       sprintf(imod->obj[imod->objsize - 1].name, 
		       "Type %d data.", i+1);
	  }
	  imod->cindex.object = tlist[hvemcont->type];

	  imodNewContour(imod);
	  cont = imodContourGet(imod);
	  if (!cont)
	       continue;
	  cont->surf = 0;
	  cont->psize = hvemcont->npts;
	  cont->pts = (Ipoint *)malloc(sizeof(Ipoint) * cont->psize);

	  for(pt = 0; pt < hvemcont->npts; pt++){
	       cont->pts[pt].x  = hvemcont->points[pt][XDATA];
	       cont->pts[pt].y  = hvemcont->points[pt][YDATA];
	       cont->pts[pt].z  = hvemcont->points[pt][ZDATA];
	  }
/*	  for(pt = 0; pt < hvemcont->npts; pt++){
	       Ipoint *p = &cont->pts[pt];
	       p->x  = hvemcont->points[pt][XDATA];
	       p->y  = hvemcont->points[pt][YDATA];
	       p->z  = hvemcont->points[pt][ZDATA];

	       p->x /= 10.f;
	       p->y /= 10.f;
	       
	       tmp = (int)p->x;
	       p->x = tmp;
	       tmp = (int)p->y;
	       p->y = tmp;

	  }
	  if (hvemcont->npts > 2) {
	    imodContourUnique(cont);
	    imodContourUnique(cont);
	    imodContourStrip(cont); 
	  }
*/
	  hvem3D_free_contour(hvemcont);

     }
     printf("\ndone\n");
     hvem3D_free_head(head);

     imod->units   = IMOD_UNIT_UM;
     if (mag != 0.0)
	  head->mag = mag;
     if (section != 0.0)
	  head->secthick = section;

     if (head->mag)
	  imod->pixsize = scaling * head->unitsize / head->mag;
     if (imod->pixsize)
	  imod->zscale  = head->secthick / imod->pixsize;
     if (imod->zscale == 0.0)
          imod->zscale = Zscale;

     imod->xscale = Xscale;
     imod->yscale = Yscale;


     /* shift data down to 0,0,0 and scale by the given factor */
     imodel_minpt(imod, &point);
     for (ob = 0; ob < imod->objsize; ob++){
	  obj = &(imod->obj[ob]);
	  for(co = 0; co < obj->contsize; co++){
	       cont = &(obj->cont[co]);
	       for (pt = 0; pt < cont->psize; pt++){
		 cont->pts[pt].x = scaling * (cont->pts[pt].x - point.x);
		 cont->pts[pt].y = scaling * (cont->pts[pt].y - point.y);
		 cont->pts[pt].z -= point.z;
	       }
	   }
     }


     imodel_maxpt(imod, &point);
     imod->xmax = point.x;
     imod->ymax = point.y;
     imod->zmax = point.z + 1;

     printf("Maximum X, Y and Z are %d, %d, and %d\n",imod->xmax, 
	    imod->ymax, imod->zmax);
     if (scaling == 1.0) {
       if (point.x > 1500 || point.y > 1500) {
	 facx = 1280./point.x;
	 facy = 1024./point.y;
	 if (facx > facy)
	   facx = facy;
	 printf ("For better results with imodmesh, it is recommended that\n"
		 "you rerun this program with a scaling (-s) of about %.2f,\n"
		 "then run reducecont with a tolerance of about 0.2\n", facx);
       }
     }

     imodWrite(imod, fout);
     imodDelete(imod);

     exit(0);
     return 0;
}


