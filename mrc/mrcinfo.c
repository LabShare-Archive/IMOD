/*  UNSUPPORTED
 *
 *  mrcinfo.c -- Output header info to standard out.
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
#include "mrcfiles.h"

main( int argc, char *argv[] )
{
     int    i = 0;
     int    error;
     FILE   *fin;
     struct MRCheader hdata;
     struct MRCheader *data;
     float  vd1, vd2, nd1, nd2;

     fprintf(stderr, 
	     "%s version 1.0 Copyright (C)1994 Boulder Laboratory for\n", 
	     argv[0]);
     fprintf(stderr,
	     "3-Dimensional Fine Structure, University of Colorado.\n");

     if (argc < 2){
	  fprintf(stderr,"Usage: %s <image file>\n",argv[0]) ;
	  exit(3) ;
     }

     data = &hdata;          
     fin = fopen(argv[1], "rb");
     
     if (fin == NULL){
	  fprintf(stderr, "Error opening %s.\n", argv[1]);
	  exit(3);
     }
     
     error = mrc_head_read(fin, &hdata);
     if (error < 0){
	  fprintf(stderr, "Can't Read Input File Header.\n");
	  exit(3);
     }
     if (error > 0)
	  fprintf(stderr, "Warning: File is not in MRC format.\n\n");

     /* Output info */
     printf("MRCinfo: Info on image file %s\n", argv[1]);
     switch(data->mode)
	  {
	     case MRC_MODE_BYTE :
		  printf("mode = Byte\n") ;
		  break ;
		case MRC_MODE_SHORT :
		     printf("mode = Short\n") ;
		  break ;
		case MRC_MODE_FLOAT :
		     printf("mode = Float\n") ;
		  break ;
		case MRC_MODE_COMPLEX_SHORT :
		     printf("mode = Complex Short\n") ;
		  break ;
		case MRC_MODE_COMPLEX_FLOAT :
		     printf("mode = Complex Float\n") ;
		  break ;
		case MRC_MODE_RGB:
		  printf("mode = rgb byte\n");
		  break;
		default:
		  printf("mode is unknown.\n");
		  break;
	     }

     printf("Image size    =  ( %d, %d, %d)\n", 
	    data->nx, data->ny, data->nz);

     printf("minimum value = %g\n",data->amin) ;
     printf("maximum value = %g\n",data->amax) ;
     printf("mean value    = %g\n",data->amean) ;

     printf("Start reading image at ( %d, %d, %d).\n",
	    data->nxstart,
	    data->nystart,
	    data->nzstart) ;
    
     printf("Read length   =  ( %d, %d, %d).\n",
	    data->mx, data->my, data->mz);

     printf("Size of voxel is ( %g x %g x %g ) um.\n",
	    data->xlen, data->ylen, data->zlen) ;

     printf("Rotation      =  ( %g, %g, %g).\n", 
	    data->alpha, data->beta, data->gamma) ;

     printf("Col, rows, sections  = axis (%d , %d, %d)\n",
	    data->mapc, data->mapr, data->maps) ;

     printf("Angles (%g, %g, %g) --> (%g, %g, %g)\n",
	    data->tiltangles[0], data->tiltangles[1], data->tiltangles[2],
	    data->tiltangles[3], data->tiltangles[4], data->tiltangles[5]);
     
     if (data->ispg)
	  printf("ispg =\t\t%d\n",data->ispg) ;
     if (data->idtype){
	  printf("idtype =\t%d\n", data->idtype);

	  nd1 = data->nd1;
	  nd2 = data->nd2;
	  vd1 = data->vd1;
	  vd2 = data->vd2;
	  
	  
	  printf("nd1 =\t\t%g\n", (nd1));
	  printf("nd2 =\t\t%g\n", (nd2));
	  printf("vd1 =\t\t%g\n", (vd1 / 100.0));
	  printf("vd2 =\t\t%g\n", (vd2 / 100.0));
     }

     printf("orgin = ( %g, %g, %g)\n",
	    data->xorg, data->yorg, data->zorg);

     if (data->nlabl > MRC_NLABELS){
	  printf("There are to many labels.\n\n");
	  exit(0);
     }

     printf("Thare are %d labels.\n\n",data->nlabl) ;

     for (i = 0; i < data->nlabl; i++)
	  printf("%s",data->labels[i]) ;
     
     exit(0);
}





