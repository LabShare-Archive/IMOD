/*  iirawimage.cpp - Raw image reading module; opens RawImageForm for info
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#include <qlabel.h>
#include <qapplication.h>
#include "imod.h"
#include "b3dutil.h"
#include "iirawimage.h"
#include "form_rawimage.h"

// Resident info structure
static RawImageInfo info = {0, 64, 64, 1, false, 0, true, 0., 255., false};

/*
 * Routines to modify the structure from command line
 */
void iiRawSetSize(int nx, int ny, int nz)
{
  info.nx = B3DMAX(1, nx);
  info.ny = B3DMAX(1, ny);
  info.nz = B3DMAX(1, nz);
  info.allMatch = true;
}

int iiRawSetMode(int mode)
{
  switch (mode) {
  case MRC_MODE_BYTE:
    info.type = 0;
    break;
  case MRC_MODE_SHORT:
    info.type = 1;
    break;
  case MRC_MODE_FLOAT:
    info.type = 2;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    info.type = 3;
    break;
  case MRC_MODE_RGB:
    info.type = 4;
    break;
  default:
    return 1;
  }
  return 0;
}

void iiRawSetHeaderSize(int size)
{
  info.headerSize = B3DMAX(0, size);
}

void iiRawSetSwap()
{
  info.swapBytes = true;
}

void iiRawSetScale(float smin, float smax)
{
  info.amin = smin;
  info.amax = smax;
  info.scanMinMax = false;
}

/*
 * The routine to check and setup a file
 */
int iiRawCheck(ImodImageFile *inFile)
{
  Islice *slice;
  int z, ind, csize, dsize;
  float amin, amax;
  struct MRCheader *hdr;
  QString str;
  QLabel *splash = NULL;

  if (!inFile)
    return -1;
  if (!inFile->fp)
    return 1;

  str = inFile->filename;
  ind = str.findRev('/');
  if (ind < 0)
    ind = str.findRev('\\');
  if (ind >= 0)
    str = str.right(str.length() - 1 - ind);

  // Unless the all match flag has been set, get the form to set info
  if (!info.allMatch) {
    RawImageForm *form = new RawImageForm(NULL, "raw image form", true);
    form->setIcon(*(App->iconPixmap));
    form->load(str, &info);
    if (form->exec() == QDialog::Rejected)
      return 1;
    form->unload(&info);
    delete form;
  }

  // Get an MRC header; set sizes into that header and the iifile header
  hdr = (struct MRCheader *)malloc(sizeof(struct MRCheader));
  if (!hdr)
    return 2;
  inFile->nx   = hdr->nx = info.nx;
  inFile->ny   = hdr->ny = info.ny;
  inFile->nz   = hdr->nz = info.nz;
  inFile->file = IIFILE_RAW;
  hdr->swapped = info.swapBytes ? 1 : 0;
  hdr->headerSize = info.headerSize;
  hdr->fp = inFile->fp;

  // Set flags for type of data
  switch(info.type) {
  case 0:
    hdr->mode = MRC_MODE_BYTE;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_UBYTE;
    break;
  case 1:
    hdr->mode = MRC_MODE_SHORT;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_SHORT;
    break;
  case 2:
    hdr->mode = MRC_MODE_FLOAT;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_FLOAT;
    break;
  case 3:
    hdr->mode = MRC_MODE_COMPLEX_FLOAT;
    inFile->format = IIFORMAT_COMPLEX;
    inFile->type   = IITYPE_FLOAT;
    break;
  case 4:
    hdr->mode = MRC_MODE_RGB;
    inFile->format = IIFORMAT_RGB;
    inFile->type   = IITYPE_UBYTE;
    break;
  }

  amin = info.amin;
  amax = info.amax;

  // Scan through file to find min and max if flag set and not RGB
  if (hdr->mode != MRC_MODE_RGB && info.scanMinMax) {

    // Show splash label if > 1 MB
    mrc_getdcsize(hdr->mode, &dsize, &csize);
    if ((double)hdr->nx * hdr->ny * hdr->nz * dsize * csize > 1.e6) {
      splash = new QLabel("Scanning " + str + " for min/max", NULL);
      QSize hint = splash->sizeHint();
      splash->move(QApplication::desktop()->width() / 2 - hint.width() / 2, 
                   QApplication::desktop()->height() / 2 - hint.height() / 2);
      splash->show();
      qApp->processEvents();
    }

    // Loop on slices
    for (z = 0; z < hdr->nz; z++) {
      slice = sliceReadMRC(hdr, z, 'z');
      if (!slice)
        return 1;
      sliceMMM(slice);
      if (z) {
        amin = B3DMIN(amin, slice->min);
        amax = B3DMAX(amax, slice->max);
      } else {
        amin = slice->min;
        amax = slice->max;
      }
      sliceFree(slice);
    }
    if (splash)
      delete splash;
  }

  inFile->smin = inFile->amin = hdr->amin = amin;
  inFile->smax = inFile->amax = hdr->amax = amax;
  inFile->amean = hdr->amean = (amax + amin) / 2.;

  // Set the header and the access routines; just use the MRC routines
  inFile->header = (char *)hdr;
  inFile->readSection = iiMRCreadSection;
  inFile->readSectionByte = iiMRCreadSectionByte;
  inFile->cleanUp = iiRawDelete;
  return 0;
}

void iiRawDelete(ImodImageFile *inFile)
{
  if (inFile->header)
    free(inFile->header);
}
