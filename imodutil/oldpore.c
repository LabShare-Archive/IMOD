/*  IMOD VERSION 2.02
 *
 *  $Id$
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

#include <imodel.h>


main(int argc, char **argv)
{
     FILE *fout;
     Imod *imod, *omod;
     Iobj *obj, *sobj, *nobj;
     Icont *cont;
     Ipoint cpoint;
     int ob, co, pt, npt;

     if (argc != 3){
	  fprintf(stderr, "%s: usage %s [infile] [outfile]\n", 
		  argv[0], argv[0]);
	  exit(-1);
     }

     omod = imodRead(argv[1]);
     if (!omod){
	  fprintf(stderr, "%s: Error opening %s.\n", argv[0], argv[1]);
	  exit(-1);
     }
     fout = fopen(argv[2], "w");
     if (!fout){
	  fprintf(stderr, "%s: Error opening %s\n", argv[0], argv[2]);
	  imodDelete(omod);
	  exit(-1);
     }
     imod = imodNew();
     if (!imod){
	  fprintf(stderr, "%s: Error getting model\n", argv[0]);
	  imodDelete(omod);
	  fclose(fout);
	  exit(-1);
     }

     imod->zscale = omod->zscale;
     imod->units  = omod->units;
     imod->pixsize = omod->pixsize;
     imodNewObject(imod);
     sobj = imodObjectGet(imod);
     sobj->flags |= IMOD_OBJFLAG_SCAT;
     sobj->pdrawsize = 10;
     imodNewContour(imod);
     cont = imodContourGet(imod);


     for(ob = 0; ob < omod->objsize; ob++){
	  obj = &(omod->obj[ob]);
	  if (obj->contsize > 3){
	       imodNewObject(imod);
	       nobj = imodObjectGet(imod);
	       imodObjectCopy(obj, nobj);
	       nobj->cont = imodContoursNew(obj->contsize);
	       for(co = 0; co < obj->contsize; co++){
		    imodContourCopy(&(obj->cont[co]), &(nobj->cont[co]));
		    nobj->cont[co].pts = (Ipoint *)malloc
			 (sizeof(Ipoint) * obj->cont[co].psize);
		    memcpy(nobj->cont[co].pts, obj->cont[co].pts, 
			   sizeof(Ipoint) * obj->cont[co].psize);
	       }
	  }else{
	       cpoint.x = cpoint.y = cpoint.z = 0.0f;
	       npt = 0;
	       for(co = 0; co < obj->contsize; co++){
		    for(pt = 0; pt < obj->cont[co].psize; pt++, npt++){
			 cpoint.x += obj->cont[co].pts[pt].x;
			 cpoint.y += obj->cont[co].pts[pt].y;
			 cpoint.z += obj->cont[co].pts[pt].z;
		    }
	       }
	       if (npt){
		    cpoint.x /= (float)npt;
		    cpoint.y /= (float)npt;
		    cpoint.z /= (float)npt;
		    imodPointAppend(cont, &cpoint);
	       }
	  }
     }

     imodWrite(imod, fout);
     fclose(fout);
     imodDelete(imod);
     imodDelete(omod);
}
