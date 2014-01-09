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
#include <math.h>
#include "midas.h"
#include "b3dutil.h"
#include <qfile.h>
#include <QTextStream>
#include "dia_qtutils.h"

static MrcHeader ImageHeader;
static struct LoadInfo loadinfo;


/* Load the image to be aligned.
 */
int load_image(MidasView *vw, char *filename)
{
  MrcHeader *hin = &ImageHeader;
  float smin, smax;
  QString qstr;

  hin->fp = iiFOpen(filename, "rb");
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
  MrcHeader hin;
  struct LoadInfo li;
  float smin, smax;
  QString qstr;

  hin.fp = iiFOpen(filename, "rb");
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

  iiFClose(hin.fp);
  return 0;
}

int midasReadZByte(MidasView *vw, MrcHeader *hin, LoadInfo *li, 
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

  MrcHeader header = *VW->hin;
  MrcHeader *hout = &header;
  struct MRCslice *s, *orgSlice;
  int k;

  s = sliceCreate(vw->xsize, vw->ysize, MRC_MODE_BYTE);
  if (!s) {
    midas_error("Error creating slice. ","Out of memory", 0);
    return(-1);
  }

  /* Open up file to save. */
  hout->fp = iiFOpen(filename, "wb");
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
  iiFClose(hout->fp);
  return(0);
}


/* 
 * Load a transformation file, either xf or xg format or warping or edge displacements
 */
int load_transforms(MidasView *vw, char *filename)
{
  int i, k, ixy, nedgex, nedgey, numRead, nxMax, nyMax, prodMax, nxGrid, nyGrid;
  int warpVersion, warpFlags, ix, iy, iz, err;
  int warpnx, warpny, warpnz, warpbin, newNx, newNy;
  float pixelSize, xInterval, yInterval, mat[9], xscale, yscale, zscale, xStart, yStart;
  float warpScale, warpXoffset, warpYoffset;
  int warpFile = -1, contFile = 1;
  float *xControl = NULL, *yControl = NULL, *dxGrid = NULL, *dyGrid = NULL;
  QString str = filename;
  QString qline;

  // Set up number of simple transforms to read (less if chunks)
  numRead = vw->numChunks ? vw->numChunks : vw->zsize;

  // For all but montage transforms, try to load the file as warping first
  if (vw->xtype != XTYPE_MONT) {
    warpFile = readWarpFile(filename, &warpnx, &warpny, &warpnz, 
                               &warpbin, &pixelSize, &warpVersion, &warpFlags);
    if (warpFile < 0) {

      // An error occurred: return unless it is a simple xf file
      if (warpFile != -3 || warpVersion != 0) {
        qline.sprintf("Error %d (version value %d) reading the transform file as a "
                      "warping file.", warpFile, warpVersion);
        midas_error(LATIN1(qline),
                    "It does not appear to be a simple transform file", 0);
        return(-4);
      }
    } else {

      // It was a warping file; it is an error if that is not allowed
      err = 0;
      if (!vw->warpingOK) {
        midas_error("The transform file is a warping file.", "This is not allowed "
                    "with global alignments or rotating all images by an angle.", 0);
        err = -5;
      }

      if (!(warpFlags & WARP_INVERSE)) {
        midas_error("The warping file does not appear to have inverse warpings.",
                    "Forward warpings are not supported yet.", 0);
        err = -6;
      }

      if (!err) {
        mrc_get_scale(vw->hin, &xscale, &yscale, &zscale);
        warpScale = (xscale / pixelSize) * vw->binning;
        newNx = B3DNINT(vw->xsize * warpScale);
        newNy = B3DNINT(vw->ysize * warpScale);
        warpXoffset = (newNx - warpnx) / 2.;
        warpYoffset = (newNy - warpny) / 2.;
      }
        
      // If the file is a grid, we need to make a new file that is control points
      // Also make a new file if offset is big enough
      if (!err && (!(warpFlags & WARP_CONTROL_PTS) || warpXoffset || warpYoffset)) {
        if (warpFlags & WARP_CONTROL_PTS)
          getNumWarpPoints(-1, &prodMax);
        else
          getWarpGridSize(-1, &nxMax, &nyMax, &prodMax);
          
        contFile = newWarpFile(newNx, newNy, warpbin, pixelSize,
                               WARP_CONTROL_PTS | WARP_INVERSE);
        if (contFile < 0) {
          err = -7;
        } else {

          // Get arrays
          xControl = B3DMALLOC(float, prodMax);
          yControl = B3DMALLOC(float, prodMax);
          dxGrid = B3DMALLOC(float, prodMax);
          dyGrid = B3DMALLOC(float, prodMax);
          if (!xControl || !yControl || !dxGrid || !dyGrid)
            err = -7;
        }

        // loop on sections in file, get the warping and linear transform
        for (iz = 0; iz < warpnz && !err; iz++) {
          setCurrentWarpFile(warpFile);
          getLinearTransform(iz, mat, 3);
          if (warpFlags & WARP_CONTROL_PTS) {
            if (getNumWarpPoints(iz, &prodMax) ||
                getWarpPoints(iz, xControl, yControl, dxGrid, dyGrid)) {
              err = -7;
              break;
            }
          } else {
            if (getWarpGrid(iz, &nxGrid, &nyGrid, &xStart, &yStart, &xInterval,
                            &yInterval, dxGrid, dyGrid, 0)) {
              err = -7;
              break;
            }
            i = 0;
            
            // Make control points
            for (iy = 0; iy < nyGrid; iy++) {
              for (ix = 0; ix < nxGrid; ix++) {
                xControl[i] = xStart + ix * xInterval;
                yControl[i] = yStart + iy * yInterval;
                i++;
              }
            }
            prodMax = i;
          }          

          // Shift points by offset
          for (i = 0; i < prodMax; i++) {
            xControl[i] += warpXoffset;
            yControl[i] += warpYoffset;
          }
          
          // Save it as control points to new file
          setCurrentWarpFile(contFile);
          if (setLinearTransform(iz, mat, 3) ||
              setWarpPoints(iz, prodMax, xControl, yControl, dxGrid, dyGrid))
            err = -7;
        }

        // Clear out the grid file and replace with new file
        if (!err) {
          clearWarpFile(warpFile);
          warpFile = contFile;
        }
      }

      // If error, clean up and return
      if (err) {
        B3DFREE(xControl);
        B3DFREE(yControl);
        B3DFREE(dxGrid);
        B3DFREE(dyGrid);
        if (contFile >= 0)
          clearWarpFile(contFile);
        clearWarpFile(warpFile);
        if (err == -7)
          midas_error("Error trying to convert a warp grid to control points", "", 0);
        return err;
      }

      // Save Z size, set up scaling
      vw->warpNz = warpnz;
      vw->warpScale = warpScale;

      // Initialize transforms and load them for all sections
      setCurrentWarpFile(warpFile);
      for (k = 0 ; k < numRead; k++) {
        tramat_idmat(vw->tr[k].mat);
        getLinearTransform(k, vw->tr[k].mat, 3);
        vw->tr[k].mat[6] /= vw->warpScale;
        vw->tr[k].mat[7] /= vw->warpScale;
      }
    }
  }

  if (warpFile < 0) {

    // Open file and stream
    QFile file(str);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
      return(-1);

    QTextStream stream(&file);
    stream.setIntegerBase(10);
    
    if (vw->xtype == XTYPE_MONT) {
      
      // For edge displacements
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
          
          // Eat blank lines, needed thanks to blendmont bug
          while (qline.isEmpty() && !stream.atEnd()) {
            qline = stream.readLine();
          }
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
    file.close();
  }
	
  // Copy transforms up for chunk mode
  for (i = vw->numChunks - 1; i >= 0; i--) {
    for (k = vw->chunk[i + 1].start - 1; k >= vw->chunk[i].start; k--)
      if (k != i)
        tramat_copy(vw->tr[i].mat, vw->tr[k].mat);
  }

  if (vw->curWarpFile >= 0)
    clearWarpFile(vw->curWarpFile);
  vw->curWarpFile = warpFile;

  /* flush the cache of any transformed images */
  flush_xformed(vw);

  vw->midasSlots->backup_current_mat();
  vw->midasGL->fill_viewdata(vw);
  return(0);
}

/* Write a xf or xg transformation file.
 */
int write_transforms(MidasView *vw, char *filename)
{
  int k, ixy, numWrite, i, nControl;
  bool anyWarp = false;
  float mat[9], dxout, dyout;
  QString str = filename;
  float *xControl, *yControl, *xVector, *yVector;

  numWrite = vw->numChunks ? vw->numChunks : vw->zsize;

  // For warp file, see if there are any non-zero control points anywhere
  if (vw->curWarpFile >= 0) {
    for (k = 0; k < vw->warpNz && !anyWarp; k++) {
      if (getNumWarpPoints(k, &nControl))
        break;
      if (getWarpPointArrays(k, &xControl, &yControl, &xVector, &yVector))
        break;
      for (i = 0; i < nControl; i++) {
        if (xVector[i] != 0. || yVector[i] != 0.) {
          anyWarp = true;
          //printf("%d %d %f %f\n", k, i, xVector[i], yVector[i]);
          break;
        }
      }
    }

    // If there are, extend the transforms if necessary and write them.  Skip backup
    if (anyWarp) {
      if (vw->warpNz < numWrite) {
        tramat_idmat(mat);
        if (setLinearTransform(numWrite - 1, mat, 3)) {
          midas_error("Error extending warp storage to final section", 
                      "Warp file not saved" , 0);
          return 1;
        }
        vw->warpNz = numWrite;
      }
      for (i = 0; i < numWrite; i++) {
        k = vw->numChunks ? vw->chunk[i].start : i;
        tramat_copy(vw->tr[k].mat, mat);
        mat[6] *= vw->warpScale;
        mat[7] *= vw->warpScale;
        setLinearTransform(i, mat, 3);
      }
      return (writeWarpFile(LATIN1(str), 1));
    }
  }

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
Revision 3.13  2011/03/03 19:51:20  mast
Made it accept blank lines in ecd file

Revision 3.12  2010/06/29 22:30:02  mast
Changes for binning option

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

