/*  IMOD VERSION 2.50
 *
 *  file_io.cpp -- File input/output routines.
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
Revision 3.6  2004/08/04 22:35:13  mast
Changed unsigned long to b3dUInt32 for 64-bit use

Revision 3.5  2004/07/12 18:42:43  mast
Changes for chunk alignment

Revision 3.4  2003/12/17 21:44:19  mast
Changes to implement global rotations

Revision 3.3  2003/11/01 16:43:10  mast
changed to put out virtually all error messages to a window

*/

#include <stdlib.h>
#include "midas.h"
#include "b3dutil.h"
#include <qfile.h>
#include "dia_qtutils.h"

static struct MRCheader ImageHeader;
static struct LoadInfo loadinfo;


/* Load the image to be aligned.
 */
int load_image(struct Midas_view *vw, char *filename)
{
  struct MRCheader *hin = &ImageHeader;
  float smin, smax;
  QString qstr;

  hin->fp = fopen(filename, "rb");
  if (!hin->fp){
    midas_error("Couldn't open", filename, 0);
    return(1);
  }

  if (mrc_head_read(hin->fp, hin)){
    qstr.sprintf("Could not read %s", filename);
    midas_error(b3dGetError(), (char *)qstr.latin1(), 0);
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
  QString qstr;

  hin.fp = fopen(filename, "rb");
  if (!hin.fp) {
    midas_error("Error opening reference image ", filename, 0);
    return 1;
  }
  if (mrc_head_read(hin.fp, &hin)) {
    qstr.sprintf("Error reading reference image %s\n", filename);
    midas_error(b3dGetError(), (char *)qstr.latin1(), 0);
    return 1;
  }


  vw->refzsize = hin.nz;
  if (hin.nx != vw->xsize || hin.ny != vw->ysize){
    qstr.sprintf("Error: size of reference image in %s does not \n"
	    "match size of images being aligned.\n", filename);
    midas_error("", (char *)qstr.latin1(), 0);
    return 1;
  }

  if (vw->xsec < 0 || vw->xsec >= hin.nz) {
    if (vw->xsec < 0)
      vw->xsec = 0;
    if (vw->xsec >= hin.nz)
      vw->xsec = hin.nz - 1;
    qstr.sprintf("Warning: specified section for reference image "
	    "was out of bounds;\n using section %d instead\n", vw->xsec + 1);
    dia_err((char *)qstr.latin1());
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
  struct MRCslice *s, *orgSlice;
  int k;

  s = mrc_slice_create(vw->xsize, vw->ysize, MRC_MODE_BYTE);
  if (!s) {
    midas_error("Error creating slice. ","Out of memory", 0);
    return(-1);
  }

  /* Open up file to save. */
  hout->fp = fopen(filename, "wb");
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
    vw->midasGL->fill_rgb(orgSlice->data.b, (b3dUInt32 *)s->data.b,
			  vw->xysize, -1, &vw->tr[k]);
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
  int i, k, ixy, nedgex, nedgey, numRead;
  QString str = filename;
  QString qline;

  QFile file(str);
  if (!file.open(IO_ReadOnly | IO_Translate))
    return(-1);

  QTextStream stream(&file);
  stream.setf(QTextStream::dec);

  if (vw->xtype == XTYPE_MONT) {
    qline = stream.readLine();
    if (qline.isEmpty()) {
      midas_error("Error reading displacement file.", "", 0);
      return(-2);
    }
    sscanf(qline.latin1(), "%d%*c%d%*c",&nedgex, &nedgey);
    if (nedgex != vw->nedge[0] && nedgey != vw->nedge[1]) {
      midas_error("Wrong number of edges in displacement file.", "", 0);
      return(-3);
    }
    for (ixy = 0; ixy < 2; ixy++)
      for (k = 0 ; k < vw->nedge[ixy]; k++){
	qline = stream.readLine();
	if (qline.isEmpty()) {
          midas_error("Error reading displacement file.", "", 0);
	  return(-2);
	}
	sscanf(qline.latin1(), "%f%*c%f%*c", &(vw->edgedx[k * 2 + ixy]),
	       &(vw->edgedy[k * 2 + ixy]));
      }
    set_mont_pieces(vw);

  } else {		 

    // Regular transforms: set up number to read (less if chunks)
    numRead = vw->numChunks ? vw->numChunks : vw->zsize;
		    
    // Make unit transforms
    for (k = 0 ; k < numRead; k++)
      tramat_idmat(vw->tr[k].mat);

    for (k = 0 ; k < numRead; k++) {

      // Read a line; skip blank lines but quit loop on end of file
      while (1) {
	qline = stream.readLine();
	if (!qline.isEmpty() || qline.isNull())
	  break;
      }
      if (qline.isNull())
	break;

      sscanf(qline.latin1(), "%f%*c%f%*c%f%*c%f%*c%f%*c%f%*c",
	     &(vw->tr[k].mat[0]), &(vw->tr[k].mat[3]),
	     &(vw->tr[k].mat[1]), &(vw->tr[k].mat[4]),
	     &(vw->tr[k].mat[6]), &(vw->tr[k].mat[7]));
      vw->tr[k].mat[2] = 0.0;
      vw->tr[k].mat[5] = 0.0;
      vw->tr[k].mat[8] = 1.0;
	       
      /* DNM 12/16/03: no longer have to convert dx,dy for origin-centered 
         transforms */
    }
    
    if (vw->rotMode)
      rotate_all_transforms(vw, vw->globalRot);
  }
	
  // Copy transforms up for chunk mode
  for (i = vw->numChunks - 1; i >= 0; i--) {
    for (k = vw->chunk[i + 1].start - 1; k >= vw->chunk[i].start; k--)
      if (k != i)
        tramat_copy(vw->tr[i].mat, vw->tr[k].mat);
  }

  /* flush the cache of any transformed images */
  flush_xformed(vw);

  file.close();

  vw->midasSlots->backup_current_mat();
  vw->midasGL->fill_viewdata(vw);
  return(0);
}

/* Write a xf or xg transformation file.
 */
int write_transforms(struct Midas_view *vw, char *filename)
{
  int k, ixy, numWrite, i;
  float mat[9];
  QString str = filename;

  QFile file(str);
  if (!file.open(IO_WriteOnly | IO_Translate)) {
    midas_error("Couldn't open", filename, 0);
    return(-1);
  }
  QTextStream stream(&file);

  if (vw->xtype == XTYPE_MONT) {
    str.sprintf("%7d %7d\n", vw->nedge[0], vw->nedge[1]);
    stream << str;
    for (ixy = 0; ixy < 2; ixy++)
      for (k = 0 ; k < vw->nedge[ixy]; k++) {
	str.sprintf("%8.3f %8.3f\n", (vw->edgedx[k * 2 + ixy]),
		(vw->edgedy[k * 2 + ixy]));
	stream << str;
      }

  } else {

    numWrite = vw->numChunks ? vw->numChunks : vw->zsize;
    for (i = 0; i < numWrite; i++) {
      k = vw->numChunks ? vw->chunk[i].start : i;
      tramat_copy(vw->tr[k].mat, mat);
      if (vw->rotMode)
        rotate_transform(mat, -vw->globalRot);
      str.sprintf("%12.7f%12.7f%12.7f%12.7f%12.3f%12.3f\n",
                  mat[0], mat[3], mat[1], mat[4], mat[6], mat[7]);
	stream << str;
    }


  }
  file.close();
  return(0);
}
