/*  IMOD VERSION 2.50
 *
 *  midas_io.c -- File input/output routines.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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

/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#include <stdlib.h>
#include <mrcc.h>
#include <imodel.h>
#include "midas.h"

static struct MRCheader ImageHeader;
static struct LoadInfo loadinfo;


/* Load the image to be aligned.
 */
int load_image(struct Midas_view *vw, char *filename)
{
     struct MRCheader *hin = &ImageHeader;
     int k;
     float smin, smax;

     hin->fp = fopen(filename, "r");
     if (!hin->fp){
	  midas_error("Couldn't open", filename, 0);
	  return(1);
     }

     if (mrc_head_read(hin->fp, hin)){
	  midas_error("Couldn't read", filename, 0);
	  return(1);
     }

     smin = hin->amin;
     smax = hin->amax;
     if (vw->sminin || vw->smaxin) {
	  smin= vw->sminin;
	  smax= vw->smaxin;
     }

     vw->li = &loadinfo;
     mrc_init_li(vw->li, NULL);
     vw->li->xmin = 0;
     vw->li->xmax = hin->nx - 1;
     vw->li->ymin = 0;
     vw->li->ymax = hin->ny - 1;
     if (smin == smax)
	  smax = smin + 1.;
     vw->li->slope = 255./(smax - smin);
     vw->li->offset = - smin * vw->li->slope;

     vw->hin = hin;
     return(0);
}

/* Load image to have other images compared to.*/
int load_refimage(struct Midas_view *vw, char *filename)
{
     struct MRCheader hin;
     struct LoadInfo li;
     float smin, smax;

     hin.fp = fopen(filename, "r");
     if (!hin.fp) {
	  fprintf(stderr, "Error opening reference image %s\n",
		  filename);
	  return 1;
     }
     if (mrc_head_read(hin.fp, &hin)) {
	  fprintf(stderr, "Error reading reference image %s\n",
		  filename);
	  return 1;
     }


     vw->refzsize = hin.nz;
     if (hin.nx != vw->xsize || hin.ny != vw->ysize){
	  fprintf(stderr, "Error: size of reference image in %s does not \n"
		  "match size of images being aligned.\n", filename);
	  return 1;
     }

     if (vw->xsec < 0 || vw->xsec >= hin.nz) {
	  if (vw->xsec < 0)
	       vw->xsec = 0;
	  if (vw->xsec >= hin.nz)
	       vw->xsec = hin.nz - 1;
	  fprintf(stderr, "Warning: specified section for reference image "
		  "was out of bounds;\n using section %d instead\n", vw->xsec);
     }

     li = *vw->li;
     smin = hin.amin;
     smax = hin.amax;
     if (smin == smax)
	  smax = smin + 1.;
     li.slope = 255./(smax - smin);
     li.offset = - smin * li.slope;
     vw->ref->mean = hin.amean * li.slope + li.offset;

     mrcReadZByte(&hin, &li, vw->ref->data.b, vw->xsec);

     fclose(hin.fp);
     return 0;
}

int save_view(struct Midas_view *vw, char *filename)
{

     struct MRCheader header = *VW->hin;
     struct MRCheader *hout = &header;
     struct Midas_transform *tr;
     struct MRCslice *s, *orgSlice;
     int k;

     s = mrc_slice_create(vw->xsize, vw->ysize, MRC_MODE_BYTE);
     if (!s) {
	  midas_error("Error creating slice. ","Out of memory", 0);
	  return(-1);
     }

     /* Open up file to save. */
     hout->fp = fopen(filename, "w");
     if (!hout->fp){
	  midas_error("Couldn't open", filename, 0);
	  return(-1);
     }
     hout->headerSize = 1024;
     hout->creatid = 1000;
     hout->next = 0;
     hout->mode = 0;

     if (VW->hin->mode)
	  mrc_head_label(hout, "Midas: Adjusted contrast, converted to bytes");
     else
	  mrc_head_label(hout, "Midas: Adjusted contrast");
     hout->swapped = 0;
     mrc_head_write(hout->fp, hout);

     for (k = 0; k < vw->zsize; k++){
	  orgSlice = getRawSlice(vw, k);
	  fill_rgb(orgSlice->data.b, (unsigned long *)s->data.b, vw->xysize,
		   -1, &vw->tr[k]);
	  mrc_write_slice((void *)s->data.b, hout->fp, hout, k, 'z');
     }

     /* Clean up and restore. */
     mrc_slice_free(s);
     fclose(hout->fp);
     return(0);
}

/* 
 * Load a transformation file, either xf or xg format.
 */
int load_transforms(struct Midas_view *vw, char *filename)
{
     FILE *fin;
     int k, ixy, nedgex, nedgey;
     float xc, yc, dx, dy;
     char line[256];

     fin = fopen(filename, "r");
     if (!fin)
	  return(-1);

     if (vw->xtype == XTYPE_MONT) {
	  if (!imodFgetline(fin, line,255)) {
	       fprintf(stderr,"Error reading displacement file.\n");
	       return(-2);
	  }
	  sscanf(line, "%d%*c%d%*c",&nedgex, &nedgey);
	  if (nedgex != vw->nedge[0] && nedgey != vw->nedge[1]) {
	       fprintf(stderr,"Wrong number of edges in displacement file.\n");
	       return(-3);
	  }
	  for (ixy = 0; ixy < 2; ixy++)
	       for (k = 0 ; k < vw->nedge[ixy]; k++){
		    if (!imodFgetline(fin, line,255)) {
			 fprintf(stderr,"Error reading displacement file.\n");
			 return(-2);
		    }
		    sscanf(line, "%f%*c%f%*c", &(vw->edgedx[k * 2 + ixy]),
			   &(vw->edgedy[k * 2 + ixy]));
	       }
	  set_mont_pieces(vw);

     } else {		 
		    
	  xc = (float)vw->xsize * 0.5f;
	  yc = (float)vw->ysize * 0.5f;
	  
	  for (k = 0 ; k < vw->zsize; k++){
	       if (!imodFgetline(fin, line, 255))
		    continue;
	       
	       sscanf(line, "%f%*c%f%*c%f%*c%f%*c%f%*c%f%*c",
		      &(vw->tr[k].mat[0]), &(vw->tr[k].mat[3]),
		      &(vw->tr[k].mat[1]), &(vw->tr[k].mat[4]),
		      &dx, &dy);
	       vw->tr[k].mat[2] = 0.0;
	       vw->tr[k].mat[5] = 0.0;
	       vw->tr[k].mat[8] = 1.0;
	       
	       /* DNM: change xc * m3 to yc *m3, yc * m1 to xc * m1 to match
		  transformation applied on output */
	       vw->tr[k].mat[6] = dx + xc - (xc * vw->tr[k].mat[0]) 
	       - (yc * vw->tr[k].mat[3]);
	       vw->tr[k].mat[7] = dy + yc - (xc * vw->tr[k].mat[1])
		    - (yc * vw->tr[k].mat[4]);
	  }
     }
	
     /* flush the cache of any transformed images */
     flush_xformed(vw);

     fclose(fin);

     backup_current_mat();
     fill_viewdata(vw);
     return(0);
}

/* Write a xf or xg transformation file.
 */
int write_transforms(struct Midas_view *vw, char *filename)
{
     FILE *fout;
     int k, ixy;
     float dx, dy, xc, yc;

     fout = fopen(filename, "w");
     if (!fout){
	  midas_error("Couldn't open", filename, 0);
	  return(-1);
     }

     if (vw->xtype == XTYPE_MONT) {
	  fprintf(fout, "%7d %7d\n", vw->nedge[0], vw->nedge[1]);
	  for (ixy = 0; ixy < 2; ixy++)
	       for (k = 0 ; k < vw->nedge[ixy]; k++)
		    fprintf(fout, "%8.3f %8.3f\n", (vw->edgedx[k * 2 + ixy]),
		    (vw->edgedy[k * 2 + ixy]));
     } else {

	  xc = (float)vw->xsize * 0.5f;
	  yc = (float)vw->ysize * 0.5f;
	  
	  for(k = 0; k < vw->zsize; k++){
	       dx = vw->tr[k].mat[6] + (xc * vw->tr[k].mat[0]) 
		    + (yc * vw->tr[k].mat[3]) - xc;
	       dy = vw->tr[k].mat[7] + (xc * vw->tr[k].mat[1]) 
		    + (yc * vw->tr[k].mat[4]) - yc;
	       fprintf(fout, "%12.7f%12.7f%12.7f%12.7f%12.3f%12.3f\n",
		       vw->tr[k].mat[0],
		       vw->tr[k].mat[3],
		       vw->tr[k].mat[1],
		       vw->tr[k].mat[4],
		       dx, dy);
	       
	  }
     }
     fclose(fout);
     return(0);
}
