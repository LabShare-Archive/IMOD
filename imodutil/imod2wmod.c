/*  UNSUPPORTED
 *
 *  imod2wmod.c -- Convert an imod model file to the VMS wimp format.
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
     FILE *fout;
     Imod *imod;

     if (argc < 2){
	  printf("imod2wmod usage:\n");
	  printf("imod2wmod [imod filename] [wmod filename]\n");
	  exit(1);
     }

     fout = fopen( argv[2] , "w");
     if (fout == NULL){
	  printf("Couldn't open output file %s.\n", argv[2]);
	  exit(10);
     }
     
     imod = imodRead(argv[1]);
     if (!imod){
	  fprintf(stderr, "Imod2wmod: Error reading imod model.\n");
	  exit(3);
     }
     
     imod_to_wmod(&model, fout, argv[2]);

     fclose(fout);
}
