/*  iirawimage.cpp - Raw image reading module; opens RawImageForm for info
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <qlabel.h>
#include <qapplication.h>
#include <QDesktopWidget>
#include <qdatetime.h>
#include "imod.h"
#include "b3dutil.h"
#include "iirawimage.h"
#include "form_rawimage.h"

// Resident info structure
static RawImageInfo info = {0, 64, 64, 1, 0, 0, 0., 255., 1, 0, 0, 0, 0.};

/*
 * Routines to modify the structure from command line
 */
void iiRawSetSize(int nx, int ny, int nz)
{
  info.nx = B3DMAX(1, nx);
  info.ny = B3DMAX(1, ny);
  info.nz = B3DMAX(1, nz);
  info.allMatch = 1;
}

int iiRawSetMode(int mode)
{
  switch (mode) {
  case -1:
    info.type = RAW_MODE_SBYTE;
    break;
  case MRC_MODE_BYTE:
    info.type = RAW_MODE_BYTE;
    break;
  case MRC_MODE_SHORT:
    info.type = RAW_MODE_SHORT;
    break;
  case MRC_MODE_USHORT:
    info.type = RAW_MODE_USHORT;
    break;
  case MRC_MODE_FLOAT:
    info.type = RAW_MODE_FLOAT;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    info.type = RAW_MODE_COMPLEX_FLOAT;
    break;
  case MRC_MODE_RGB:
    info.type = RAW_MODE_RGB;
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
  info.swapBytes = 1;
}

void iiRawSetInverted()
{
  info.yInverted = 1;
}

void iiRawSetScale(float smin, float smax)
{
  info.amin = smin;
  info.amax = smax;
  info.scanMinMax = 0;
}

/*
 * The routine to check and setup a file
 */
int iiRawCheck(ImodImageFile *inFile)
{
  int ind, err;
  QString str;

  if (!inFile)
    return IIERR_BAD_CALL;
  if (!inFile->fp)
    return IIERR_BAD_CALL;

  str = inFile->filename;
  ind = str.lastIndexOf('/');
  if (ind < 0)
    ind = str.lastIndexOf('\\');
  if (ind >= 0)
    str = str.right(str.length() - 1 - ind);

  // Unless the all match flag has been set, get the form to set info
  if (!info.allMatch) {
    RawImageForm *form = new RawImageForm(NULL, true, Qt::Window);
    form->setWindowIcon(*(App->iconPixmap));
    form->load(str, &info);
    if (form->exec() == QDialog::Rejected)
      return IIERR_NOT_FORMAT;
    form->unload(&info);
    delete form;
  }

  if ((err = iiSetupRawHeaders(inFile, &info)))
    return err;
  return (iiRawScan(inFile));
}

/*
 * To scan the file for min and max if necessary
 */
int iiRawScan(ImodImageFile *inFile)
{
  Islice *slice;
  int z, ind, csize, dsize;
  MrcHeader *hdr = (MrcHeader *)inFile->header;
  QString str;
  QLabel *splash = NULL;
  float amin, amax, fval;
  short int sval;
  unsigned short usval;

  if (!inFile)
    return -1;
  if (!inFile->fp)
    return 1;

  str = inFile->filename;
  ind = str.lastIndexOf('/');
  if (ind < 0)
    ind = str.lastIndexOf('\\');
  if (ind >= 0)
    str = str.right(str.length() - 1 - ind);

  amin = info.amin;
  amax = info.amax;

  // Don't bother to scan bytes, 1:1 scaling is fine
  if (hdr->mode == MRC_MODE_BYTE && info.scanMinMax) {
    amin = 0;
    amax = 255;
    
  }

  // Scan through file to find min and max if flag set and not RGB
  if (hdr->mode != MRC_MODE_RGB  && hdr->mode != MRC_MODE_BYTE &&
      info.scanMinMax) {

    // Show splash label if > 1 MB
    mrc_getdcsize(hdr->mode, &dsize, &csize);
    if ((double)hdr->nx * hdr->ny * hdr->nz * dsize * csize > 1.e6) {
      splash = new QLabel("Scanning " + str + " for min/max", NULL);
      qApp->flush();
      QSize hint = splash->sizeHint();
      splash->move(QApplication::desktop()->width() / 2 - hint.width() / 2, 
                   QApplication::desktop()->height() / 2 - hint.height() / 2);
      splash->show();
      qApp->flush();
      qApp->syncX();
      qApp->processEvents();
    }

    // Loop on slices.  Believe it or not, it's not that much faster to
    // do the min/max here than in sliceMMM
    amin = 1.e37;
    amax = -amin;
    for (z = 0; z < hdr->nz; z++) {
      slice = sliceReadMRC(hdr, z, 'z');
      if (!slice)
        return IIERR_IO_ERROR;
      switch (hdr->mode) {
      case MRC_MODE_SHORT:
        for (ind = 0; ind < hdr->nx * hdr->ny; ind++) {
          sval = slice->data.s[ind];
          if (amin > sval)
            amin = sval;
          if (amax < sval)
            amax = sval;
        }
        break;

      case MRC_MODE_USHORT:
        for (ind = 0; ind < hdr->nx * hdr->ny; ind++) {
          usval = slice->data.us[ind];
          if (amin > usval)
            amin = usval;
          if (amax < usval)
            amax = usval;
        }
        break;

      case MRC_MODE_FLOAT:
        for (ind = 0; ind < hdr->nx * hdr->ny; ind++) {
          fval = slice->data.f[ind];
          if (amin > fval)
            amin = fval;
          if (amax < fval)
            amax = fval;
        }
        break;

      default:
        sliceMMM(slice);
        if (z) {
          amin = B3DMIN(amin, slice->min);
          amax = B3DMAX(amax, slice->max);
        } else {
          amin = slice->min;
          amax = slice->max;
        }
      }
      sliceFree(slice);
    }
    if (splash)
      delete splash;
  }

  inFile->smin = inFile->amin = hdr->amin = amin;
  inFile->smax = inFile->amax = hdr->amax = amax;
  inFile->amean = hdr->amean = (amax + amin) / 2.;

  return 0;
}
