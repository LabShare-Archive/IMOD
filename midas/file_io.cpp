/*
 *  file_io.cpp -- File input/output routines.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */

#include <stdlib.h>
#include "midas.h"
#include "b3dutil.h"
#include <qfile.h>
#include <QTextStream>
#include "dia_qtutils.h"

static struct MRCheader ImageHeader;
static struct LoadInfo loadinfo;


/* Load the image to be aligned.
 */
int load_image(MidasView *vw, char *filename)
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
    midas_error(b3dGetError(), LATIN1(qstr), 0);
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
int load_refimage(MidasView *vw, char *filename)
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
    midas_error(b3dGetError(), LATIN1(qstr), 0);
    return 1;
  }


  vw->refzsize = hin.nz;
  if (hin.nx != vw->xsize || hin.ny != vw->ysize){
    qstr.sprintf("Error: size of reference image in %s does not \n"
	    "match size of images being aligned.\n", filename);
    midas_error("", LATIN1(qstr), 0);
    return 1;
  }

  if (vw->xsec < 0 || vw->xsec >= hin.nz) {
    if (vw->xsec < 0)
      vw->xsec = 0;
    if (vw->xsec >= hin.nz)
      vw->xsec = hin.nz - 1;
    qstr.sprintf("Warning: specified section for reference image "
	    "was out of bounds;\n using section %d instead\n", vw->xsec + 1);
    dia_err(LATIN1(qstr));
  }

  li = *vw->li;
  smin = hin.amin;
  smax = hin.amax;
  if (smin == smax)
    smax = smin + 1.;
  li.slope = 255./(smax - smin);
  li.offset = - smin * li.slope;
  vw->ref->mean = hin.amean * li.slope + li.offset;

  midasReadZByte(vw, &hin, &li, vw->ref->data.b, vw->xsec);

  fclose(hin.fp);
  return 0;
}

int midasReadZByte(MidasView *vw, MRCheader *hin, LoadInfo *li, 
                   unsigned char *data, int sec)
{
  int err, nxb, nyb;
  if (vw->binning == 1) {
    err = mrcReadZByte(hin, li, data, sec);
  } else {
    err = mrcReadZByte(hin, li, vw->unbinnedBuf, sec);
    if (err)
      return err;
    err = reduceByBinning(vw->unbinnedBuf, SLICE_MODE_BYTE, hin->nx, hin->ny,
                    vw->binning, data, 1, &nxb, &nyb);
  }
  return err;
}

int save_view(MidasView *vw, char *filename)
{

  struct MRCheader header = *VW->hin;
  struct MRCheader *hout = &header;
  struct MRCslice *s, *orgSlice;
  int k;

  s = sliceCreate(vw->xsize, vw->ysize, MRC_MODE_BYTE);
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
  sliceFree(s);
  fclose(hout->fp);
  return(0);
}


/* 
 * Load a transformation file, either xf or xg format.
 */
int load_transforms(MidasView *vw, char *filename)
{
  int i, k, ixy, nedgex, nedgey, numRead;
  QString str = filename;
  QString qline;

  QFile file(str);
  if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
    return(-1);

  QTextStream stream(&file);
  stream.setIntegerBase(10);

  if (vw->xtype == XTYPE_MONT) {
    qline = stream.readLine();
    if (qline.isEmpty()) {
      midas_error("Error reading displacement file.", "", 0);
      return(-2);
    }
    sscanf(LATIN1(qline), "%d%*c%d%*c",&nedgex, &nedgey);
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
        i = 0;
	sscanf(LATIN1(qline), "%f%*c%f%*c%d", &(vw->edgedx[k * 2 + ixy]),
	       &(vw->edgedy[k * 2 + ixy]), &i);
        vw->skippedEdge[k * 2 + ixy] = i;
        if (i)
          vw->anySkipped = 1;
        vw->edgedx[k * 2 + ixy] /= vw->binning;
        vw->edgedy[k * 2 + ixy] /= vw->binning;
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

      sscanf(LATIN1(qline), "%f%*c%f%*c%f%*c%f%*c%f%*c%f%*c",
	     &(vw->tr[k].mat[0]), &(vw->tr[k].mat[3]),
	     &(vw->tr[k].mat[1]), &(vw->tr[k].mat[4]),
	     &(vw->tr[k].mat[6]), &(vw->tr[k].mat[7]));
      vw->tr[k].mat[2] = 0.0;
      vw->tr[k].mat[5] = 0.0;
      vw->tr[k].mat[8] = 1.0;
      vw->tr[k].mat[6] /= vw->binning;
      vw->tr[k].mat[7] /= vw->binning;
	       
      /* DNM 12/16/03: no longer have to convert dx,dy for origin-centered 
         transforms */
    }
    
    // Rotate then stretch transforms when coming in
    if (vw->rotMode)
      rotate_all_transforms(vw, vw->globalRot);
    if (vw->cosStretch > 0)
      stretch_all_transforms(vw, 0);
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
int write_transforms(MidasView *vw, char *filename)
{
  int k, ixy, numWrite, i;
  float mat[9], dxout, dyout;
  QString str = filename;

  QFile file(str);
  if (!file.open(QIODevice::WriteOnly | QIODevice::Text)) {
    midas_error("Couldn't open", filename, 0);
    return(-1);
  }
  QTextStream stream(&file);

  if (vw->xtype == XTYPE_MONT) {
    str.sprintf("%7d %7d\n", vw->nedge[0], vw->nedge[1]);
    stream << str;
    for (ixy = 0; ixy < 2; ixy++)
      for (k = 0 ; k < vw->nedge[ixy]; k++) {
        dxout = vw->edgedx[k*2 + ixy] * vw->binning;
        dyout = vw->edgedy[k*2 + ixy] * vw->binning;
        if (vw->anySkipped)
          str.sprintf("%9.3f %9.3f  %d\n", dxout, dyout,
                      vw->skippedEdge[k * 2 + ixy]);
        else
          str.sprintf("%9.3f %9.3f\n", dxout, dyout);
	stream << str;
      }

  } else {

    numWrite = vw->numChunks ? vw->numChunks : vw->zsize;
    for (i = 0; i < numWrite; i++) {
      k = vw->numChunks ? vw->chunk[i].start : i;
      tramat_copy(vw->tr[k].mat, mat);

      // Destretch and back-rotate transform on output
      if (vw->cosStretch)
        stretch_transform(vw, mat, i, 1);
      if (vw->rotMode)
        rotate_transform(mat, -vw->globalRot);

      str.sprintf("%12.7f%12.7f%12.7f%12.7f%12.3f%12.3f\n", mat[0], mat[3], 
                  mat[1], mat[4], mat[6] * vw->binning, mat[7] * vw->binning);
	stream << str;
    }


  }
  file.close();
  return(0);
}

/*
 * Load tilt angles
 */
void load_angles(MidasView *vw)
{
  QString str = vw->tiltname;
  int num;
  float lastAngle;

  QFile file(str);
  if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
    midas_error("Error opening or reading tilt angle file", vw->tiltname, 1);

  QTextStream stream(&file);
  stream.setIntegerBase(10);
  vw->tiltAngles = (float *)malloc(vw->zsize * sizeof(float));
  if (!vw->tiltAngles)
    midas_error("Error getting memory for tilt angles", "", 1);

  for (num = 0; num < vw->zsize; num++) {
    str = stream.readLine();
    if (str.isNull())
      break;
    if (str.isEmpty()) {
      num--;
      continue;
    }
    vw->tiltAngles[num] = atof(LATIN1(str));
  }
  if (!num)
    midas_error("No tilt angles found in the file",  vw->tiltname, 1);
  lastAngle =  vw->tiltAngles[num-1];
  for (; num < vw->zsize; num++)
    vw->tiltAngles[num] = lastAngle;
}

/*
$Log$
Revision 3.11  2009/01/15 16:30:19  mast
Qt 4 port

Revision 3.10  2008/10/13 04:36:23  mast
Added cosine stretching

Revision 3.9  2007/02/04 21:11:33  mast
Function name changes from mrcslice cleanup

Revision 3.8  2005/11/08 02:37:25  mast
Added a space to ecd output

Revision 3.7  2004/11/05 18:53:22  mast
Include local files with quotes, not brackets

Revision 3.6  2004/08/04 22:35:13  mast
Changed unsigned long to b3dUInt32 for 64-bit use

Revision 3.5  2004/07/12 18:42:43  mast
Changes for chunk alignment

Revision 3.4  2003/12/17 21:44:19  mast
Changes to implement global rotations

Revision 3.3  2003/11/01 16:43:10  mast
changed to put out virtually all error messages to a window

*/

