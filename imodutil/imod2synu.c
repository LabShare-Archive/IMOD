/*  UNSUPPORTED
 *
 *  imod2synu.c -- Convert an imod model file to the SYNU format
 *                 used by UCSD.
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
#include <string.h>

#include "imodel.h"

main( int argc, char *argv[])
{
     int i;
     int mk_mesh = 0;
     FILE *fin;
     Imod *imod;
     double zscale = -99.9;

     if (argc < 2){
	  printf("imod2synu version 1.0: usage,\n");
	  printf("imod2synu [-s] [-z #] [imod filename]\n");
	  printf("\t-s makes skins\n");
	  exit(1);
     }
     
     i = 1;
     
     for (i = 1; i < argc; i++){
	  if (argv[i][0] == '-'){
	       switch (argv[i][1]){
		  case 's':
		    mk_mesh = 1;
		    break;

		  case 'z':
		    i++;
		    zscale = atof(argv[i]);
		    break;

		  default:
		    fprintf(stderr, "%s: invalid option\n", argv[0]);
		    exit(3);
		    break;
	       }
	  }else
	       break;
     }

     

     imod = imodRead(argv[i]);
     if (!imod){
	  printf("Imod2synu: Couldn't read model file %s.\n", argv[i]);
	  exit(10);
     }

     if (zscale != -99.9)
	  imod->zscale = (float)zscale;

     if (mk_mesh)
	  imodel_skin_model(imod);
     
     printf("Creating synu files...");
     fflush(stdout);
     imod_to_synu( imod );
     printf(" Done!\n");
     exit(0);
}
