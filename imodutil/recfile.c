/*****************************************************************************
 *                                                                           *
 *   FILE: recfile.c                                                         *
 *                                                                           *
 *   PURPOSE: Read HVEM3D rec files from an IBM PC.                          *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0 Oct 1995					             *
 *       James Kremer							     *
 *       Version 1.1 June 1998					     	     *
 *	 David Mastronarde,  email mast@colorado.edu			     *
 *									     *
 *****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine         *
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
#include "recfile.h"

static char read_char(FILE *fin)
{
     unsigned char buf;
     fread(&buf,1,1,fin);
     return(buf);
}

static short read_short(FILE *fin)
{
     unsigned char buf[2];

     fread(buf,2,1,fin);
#ifdef __vms
     return(buf[0]<<8)+(buf[1]<<0);
#else
     return (buf[1]<<8)+(buf[0]<<0);
#endif
}

static int read_long(FILE *fin)
{
     unsigned char buf[4];
     int ldat;

     fread(buf,4,1,fin);
#ifdef __vms
     return (buf[0]<<24)+(buf[1]<<16)+(buf[2]<<8)+(buf[3]<<0);
#else
     return (buf[3]<<24)+(buf[2]<<16)+(buf[1]<<8)+(buf[0]<<0);
#endif
}

/* DNM: this doesn't work */
static float read_float(FILE *fin)
{
     unsigned char buf[4];
     
     fread(buf,4,1,fin);
#ifdef __vms
     return (buf[0]<<24)+(buf[1]<<16)+(buf[2]<<8)+(buf[3]<<0);
#else
     return (buf[3]<<24)+(buf[2]<<16)+(buf[1]<<8)+(buf[0]<<0);
#endif
}

void hvem3D_free_head(HvemHead *head)
{
     if (!head) return;
     free(head);
     return;
}

HvemHead *hvem3D_read_head(FILE *fin)
{
     HvemHead *head;
     char ch;

     head = (HvemHead *)malloc(sizeof(HvemHead));
     rewind(fin);

     fread(head->name, 1, HEADSZ, fin);
     head->lastdir = read_short(fin);
     head->lplane   = read_short(fin);
     head->lobject  = read_short(fin);
     head->ltype    = read_char(fin);

     printf("Rec file:\n");
     puts(head->name);
     printf("dir = %d, plane = %d, object = %d, type = %d\n",
	    head->lastdir, head->lplane, head->lobject, head->ltype);

     fseek(fin, 6, SEEK_CUR);
     head->aligned  = read_char(fin);
     /*     ch = read_char(fin); */
     
     fseek(fin, 12, SEEK_CUR);

/*     head->mag = read_float(fin);
     head->secthick = read_float(fin); */
     head->xcent = read_short(fin);
     head->ycent = read_short(fin);
     head->zcent = read_short(fin);
     fseek(fin, 2, SEEK_CUR);
     head->lastdir++;

     printf(/*mag %g, secthick %f, */"center(%d, %d, %d)\n", 
	 /*   head->mag, head->secthick, */
	    head->xcent, head->ycent, head->zcent);

     return (head);
}


void hvem3D_free_contour(HvemContour *cont)
{
     if (!cont)
	  return;
     if (cont->npts)
	  free(cont->points);
     free(cont);
     return;
}

HvemContour *hvem3D_read_contour(FILE *fin, int index)
{
     HvemContour *cont;
     char   type,nrec;
     int strtrec, inc;
     int addr, nrecs;
     short  pln, objindex, npts;
     float  area, perim;
     int    n,x,i,j;
     int   cdelete;


     cont = (HvemContour *)malloc(sizeof(HvemContour));

     if (!cont){
	  fprintf(stderr, "hvem3D_read_contour: cont malloc error.\n");
	  return(NULL);
     }
	  

     inc = DIRHDSZ + index*DIRRECSZ;
     if (fseek(fin,inc, SEEK_SET)){
	  free(cont);
	  return(NULL);
     }

     
     pln = read_short(fin);
     fseek(fin, 2, SEEK_CUR);
     type = read_char(fin);
     fseek(fin, 1, SEEK_CUR);
     nrec = read_char(fin);
     strtrec = read_long(fin);
     if(nrec == 0 || strtrec == 0 || fseek(fin, strtrec, SEEK_SET) != 0) {
       free(cont);
       return(NULL);
     }
     

     fseek(fin, 2, SEEK_CUR);
     npts = read_short(fin);
/*     area = read_float(fin);
     perim = read_float(fin); */
     fseek(fin, 48, SEEK_CUR);

     if (pln < 0) {
	  free(cont);
	  return(NULL);
     }else{
	  cont->del = FALSE;
     }

     cont->type = type;
     cont->npts = npts;
/*     cont->area = area;
     cont->perim = perim; */
     cont->area = 1.0;
     cont->perim = 1.0;

     if (cont->npts <= 0 || (npts + DATARECSZ -1)/DATARECSZ != nrec){
	  fprintf(stderr, "\nInconsistent contour # %d, %d points, but"
		  " %d records\n", index + 1, npts, nrec);

	  free(cont);

	  return(NULL);
     }
     
     cont->points = (Data *)malloc( npts * sizeof(Data));
     if (!cont->points){
	  fprintf(stderr, "\nhvem3D_read_contour: error getting point data.\n");
	  free(cont);
	  exit(1);
	  return(NULL);
     }

     for(i = 0; i < npts; i++){
	  cont->points[i][XDATA] = read_short(fin);
	  cont->points[i][ZDATA] = pln;
     }
     
     inc = strtrec + DATAHDSZ + (nrec * (DATARECSZ<<1));
     fseek(fin,inc,SEEK_SET); 
     for(i = 0; i < npts; i++){
	  cont->points[i][YDATA] = read_short(fin);
     }
     return(cont);
}

