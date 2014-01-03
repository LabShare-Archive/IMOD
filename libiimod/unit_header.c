/******************************************************************************
 *  unit_header.c - the header-manipulating routines for unit-based image file access
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  Adapted from the blockio.c module originally from Baylor
 *
 *  $Id$
 *****************************************************************************/
 
#include <stdio.h>
#include <string.h> 
#include <stdlib.h>
#include <math.h>
#include "iiunit.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define iiuretbasichead IIURETBASICHEAD
#define iiucreateheader IIUCREATEHEADER
#define icrhdr ICRHDR
#define iiuwriteheader IIUWRITEHEADER
#define iwrhdr IWRHDR
#define iiuwriteheaderstr IIUWRITEHEADERSTR
#define iwrhdrc IWRHDRC
#define iiutransheader IIUTRANSHEADER
#define itrhdr ITRHDR
#define iiuretcell IIURETCELL
#define irtcel IRTCEL
#define iiualtcell IIUALTCELL
#define ialcel IALCEL
#define iiuretdatatype IIURETDATATYPE
#define irtdat IRTDAT
#define ialdat IALDAT
#define iiualtdatatype IIUALTDATATYPE
#define iiuretsize IIURETSIZE
#define irtsiz IRTSIZ
#define iiualtsize IIUALTSIZE
#define ialsiz IALSIZ
#define iiuretsample IIURETSAMPLE
#define irtsam IRTSAM
#define iiualtsample IIUALTSAMPLE
#define ialsam IALSAM
#define iiuretaxismap IIURETAXISMAP
#define irtmap IRTMAP
#define iiualtaxismap IIUALTAXISMAP
#define ialmap IALMAP
#define iiuretspacegroup IIURETSPACEGROUP
#define iiuretimodflags IIURETIMODFLAGS
#define irtimodflags IRTIMODFLAGS
#define iiualtimodflags IIUALTIMODFLAGS
#define ialimodflags IALIMODFLAGS
#define iiualtsigned IIUALTSIGNED
#define ialsigned IALSIGNED
#define iiuretorigin IIURETORIGIN
#define irtorg IRTORG
#define iiualtorigin IIUALTORIGIN
#define ialorg IALORG
#define ialmod IALMOD
#define iiualtmode IIUALTMODE
#define iiuretrms IIURETRMS
#define irtrms IRTRMS
#define iiualtrms IIUALTRMS
#define ialrms IALRMS
#define iiurettilt IIURETTILT
#define irttlt IRTTLT
#define iiualttilt IIUALTTILT
#define ialtlt IALTLT
#define iiurettiltorig IIURETTILTORIG
#define irttlt_orig IRTTLT_ORIG
#define iiualttiltorig IIUALTTILTORIG
#define ialtlt_orig IALTLT_ORIG
#define iiualttiltrot IIUALTTILTROT
#define ialtlt_rot IALTLT_ROT
#define iiuretdelta IIURETDELTA
#define irtdel IRTDEL
#define iiualtdelta IIUALTDELTA
#define ialdel IALDEL
#define iiuretlabels IIURETLABELS
#define irtlab IRTLAB
#define iiualtlabels IIUALTLABELS
#define iallab IALLAB
#define iiutranslabels IIUTRANSLABELS
#define itrlab ITRLAB
#define iiuretnumextended IIURETNUMEXTENDED
#define irtnbsym IRTNBSYM
#define iiualtnumextended IIUALTNUMEXTENDED
#define ialnbsym IALNBSYM
#define iiuretextendedtype IIURETEXTENDEDTYPE
#define irtsymtyp IRTSYMTYP
#define iiualtextendedtype IIUALTEXTENDEDTYPE
#define ialsymtyp IALSYMTYP
#define iiuretextendeddata IIURETEXTENDEDDATA
#define irtsym IRTSYM
#define iiualtextendeddata IIUALTEXTENDEDDATA
#define irtsym IRTSYM
#define iiutransextendeddata IIUTRANSEXTENDEDDATA
#define itrextra ITREXTRA

#else
#define iiuretbasichead iiuretbasichead_
#define iiucreateheader iiucreateheader_
#define icrhdr icrhdr_
#define iiuwriteheader iiuwriteheader_
#define iwrhdr iwrhdr_
#define iiuwriteheaderstr iiuwriteheaderstr_
#define iwrhdrc iwrhdrc_
#define iiutransheader iiutransheader_
#define itrhdr itrhdr_
#define iiuretcell iiuretcell_
#define irtcel irtcel_
#define iiualtcell iiualtcell_
#define ialcel ialcel_
#define iiuretdatatype iiuretdatatype_
#define irtdat irtdat_
#define ialdat ialdat_
#define iiualtdatatype iiualtdatatype_
#define iiuretsize iiuretsize_
#define irtsiz irtsiz_
#define iiualtsize iiualtsize_
#define ialsiz ialsiz_
#define iiuretsample iiuretsample_
#define irtsam irtsam_
#define iiualtsample iiualtsample_
#define ialsam ialsam_
#define iiuretaxismap iiuretaxismap_
#define irtmap irtmap_
#define iiualtaxismap iiualtaxismap_
#define ialmap ialmap_
#define iiuretspacegroup iiuretspacegroup_
#define iiuretimodflags iiuretimodflags_
#define irtimodflags irtimodflags_
#define iiualtimodflags iiualtimodflags_
#define ialimodflags ialimodflags_
#define iiualtsigned iiualtsigned_
#define ialsigned ialsigned_
#define iiuretorigin iiuretorigin_
#define irtorg irtorg_
#define iiualtorigin iiualtorigin_
#define ialorg ialorg_
#define ialmod ialmod_
#define iiualtmode iiualtmode_
#define iiuretrms iiuretrms_
#define irtrms irtrms_
#define iiualtrms iiualtrms_
#define ialrms ialrms_
#define iiurettilt iiurettilt_
#define irttlt irttlt_
#define iiualttilt iiualttilt_
#define ialtlt ialtlt_
#define iiurettiltorig iiurettiltorig_
#define iiualttiltorig iiualttiltorig_
#define iiuretdelta iiuretdelta_
#define irtdel irtdel_
#define iiualtdelta iiualtdelta_
#define ialdel ialdel_
#define iiuretlabels iiuretlabels_
#define irtlab irtlab_
#define iiualtlabels iiualtlabels_
#define iallab iallab_
#define iiutranslabels iiutranslabels_
#define itrlab itrlab_
#define iiuretnumextended iiuretnumextended_
#define irtnbsym irtnbsym_
#define iiualtnumextended iiualtnumextended_
#define ialnbsym ialnbsym_
#define iiuretextendedtype iiuretextendedtype_
#define irtsymtyp irtsymtyp_
#define iiualtextendedtype iiualtextendedtype_
#define ialsymtyp ialsymtyp_
#define iiuretextendeddata iiuretextendeddata_
#define irtsym irtsym_
#define iiualtextendeddata iiualtextendeddata_
#define ialsym ialsym_
#define iiutransextendeddata iiutransextendeddata_
#define itrextra itrextra_

#ifdef G77__HACK
#define irttlt_orig irttlt_orig__
#define ialtlt_orig ialtlt_orig__
#define ialtlt_rot ialtlt_rot__
#else
#define irttlt_orig irttlt_orig_
#define ialtlt_orig ialtlt_orig_
#define ialtlt_rot ialtlt_rot_
#endif
#endif

/*!
 * Returns some header values for unit [iunit]: image dimensions in [nxyz], sampling in 
 * [mxyz], data mode in [mode], and minimum, maximum, and mean density in [dmin], [dmax],
 * and [dmean].  Fortran wrapper iiuRetBasicHead.
 */
void iiuRetBasicHead(int iunit, int *nxyz, int *mxyz, int *mode, float *dmin, float *dmax,
                     float *dmean)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetBasicHead", 1, 0);
  nxyz[0] = hdr->nx;
  nxyz[1] = hdr->ny;
  nxyz[2] = hdr->nz;
  mxyz[0] = hdr->mx;
  mxyz[1] = hdr->my;
  mxyz[2] = hdr->mz;
  *mode =  hdr->mode;
  *dmin =  hdr->amin;
  *dmax =  hdr->amax;
  *dmean =  hdr->amean;
}

void iiuretbasichead(int *iunit, int *nxyz, int *mxyz, int *mode, float *dmin,
                     float *dmax, float *dmean)
{ iiuRetBasicHead(*iunit, nxyz, mxyz, mode, dmin, dmax, dmean); }

/*!
 * Creates a new header, i.e., initializes the header, for unit [iunit] with the give
 * image size in [nxyz], sample size in [mxyz], file mode in [mode], and [numLabels]
 * titles in [labels].  Fortran wrappers iiuCreateHeader and icrhdr.
 */
void iiuCreateHeader(int iunit, int *nxyz, int *mxyz, int mode, int *labels, 
                     int numLabels)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuCreateHeader", 1, 2);
  int nxyzst[3] = {0, 0, 0};
  hdr->mode = mode;
  iiuAltSize(iunit, nxyz, nxyzst);
  iiuAltSample(iunit, mxyz);
  hdr->xlen = mxyz[0];
  hdr->xlen = mxyz[1];
  hdr->zlen = mxyz[2];
  iiuAltLabels(iunit, labels, numLabels);
  iiuSyncWithMrcHeader(iunit);
}

void iiucreateheader(int *iunit, int *nxyz, int *mxyz, int *mode, int *labels,
                     int *numLabels)
{ iiuCreateHeader(*iunit, nxyz, mxyz, *mode, labels, *numLabels);}
void icrhdr(int *iunit, int *nxyz, int *mxyz, int *mode, int *labels, int *numLabels)
{ iiuCreateHeader(*iunit, nxyz, mxyz, *mode, labels, *numLabels);}

/*!
 * Writes the header for unit [iunit] to file with the minimum, maximum and mean
 * density values in [dmin], [dmax], and [dmean].  A single title can be supplied in
 * an 80-byte array, [label], and will be treated according to the value of [labFlag]: ^
 *  0:  [label] becomes the only title ^
 *  1:  [label] is added at the end of existing titles if there is room ^
 *  2:  [label] is added as the first title and others are shifted up ^
 * -1 or anything else: do not add a title ^
 * Returns -1 for various internal errors or 1 for an error writing the header.
 * Fortran wrappers iiuWriteHeader and iwrhdr (a void).
 */
int iiuWriteHeader(int iunit, int *label, int labFlag, float dmin, float dmax,
                    float dmean)
{
  int doExit = iiuGetExitOnError();
  int i;
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuWriteHeader", doExit, 2);
  if (!hdr)
    return -1;
  if (labFlag == 0) {
    iiuAltLabels(iunit, label, 1);
  } else if (labFlag == 1 && hdr->nlabl < MRC_NLABELS) {
    memcpy(hdr->labels[hdr->nlabl], label, MRC_LABEL_SIZE);
    fixTitlePadding(hdr->labels[hdr->nlabl++]);
  } else if (labFlag == 2) {
    hdr->nlabl = B3DMIN(hdr->nlabl + 1, MRC_NLABELS);
    for (i = hdr->nlabl - 1; i > 0; i--)
      memcpy(hdr->labels[i], hdr->labels[i - 1], MRC_LABEL_SIZE);
    memcpy(hdr->labels[0], label, MRC_LABEL_SIZE);
    fixTitlePadding(hdr->labels[0]);
  }

  hdr->amin = dmin;
  hdr->amax = dmax;
  hdr->amean = dmean;
  if (mrc_head_write(hdr->fp, hdr)) {
    if (doExit)
      exit(1);
    return 1;
  }
  return 0;
}

int iiuwriteheader(int *iunit, int *label, int *labFlag, float *dmin, float *dmax,
                    float *dmean)
{ return iiuWriteHeader(*iunit, label, *labFlag, *dmin, *dmax, *dmean);}
void iwrhdr(int *iunit, int *label, int *labFlag, float *dmin, float *dmax, float *dmean)
{ iiuWriteHeader(*iunit, label, *labFlag, *dmin, *dmax, *dmean);}

/*!
 * Writes a header just as @iiuWriteHeader does, but with [label] being a string instead
 * of an 80-byte array.  Fortran wrappers iiuWriteHeaderStr and iwrhdrc (a void).
 */
int iiuWriteHeaderStr(int iunit, const char *labelStr, int labFlag, float dmin,
                       float dmax, float dmean)
{
  int label[MRC_LABEL_SIZE / 4];
  strncpy((char *)label, labelStr, MRC_LABEL_SIZE);
  return iiuWriteHeader(iunit, label, labFlag, dmin, dmax, dmean); 
}

int iiuwriteheaderstr(int *iunit, char *labelStr, int *labFlag, float *dmin,
                       float *dmax, float *dmean, int labelLen)
{
  int err;
  char *label = f2cString(labelStr, labelLen);
  iiuMemoryError(label, "ERROR: iiuwriteheaderstr - Allocating C string");
  err = iiuWriteHeaderStr(*iunit, label, *labFlag, *dmin, *dmax, *dmean);
  free(label);
  return err;
}

void iwrhdrc(int *iunit, char *labelStr, int *labFlag, float *dmin, float *dmax,
             float *dmean, int labelLen)
{ iiuwriteheaderstr(iunit, labelStr, labFlag, dmin, dmax, dmean, labelLen);}

/*!
 * Transfers all header information from unit [iunit] to [intoUnit], including the 
 * extended header if any.  Returns -1 for internal errors or 1 for an error writing the
 * extended header.  Fortran wrappers iiuTransHeader and itrhdr (a void).
 */
int iiuTransHeader(int intoUnit, int iunit)
{
  MrcHeader *ihdr = iiuMrcHeader(iunit, "iiuTransHeader", iiuGetExitOnError(), 0);
  MrcHeader *jhdr = iiuMrcHeader(intoUnit, "iiuTransHeader", iiuGetExitOnError(), 2);
  if (!ihdr || !jhdr)
    return -1;
  *jhdr = *ihdr;
  mrcInitOutputHeader(jhdr);
  iiuSyncWithMrcHeader(intoUnit);
  return iiuTransExtendedData(intoUnit, iunit);
}

int iiutransheader(int *iunit, int *junit) {return iiuTransHeader(*iunit, *junit);}
void itrhdr(int *iunit, int *junit) {iiuTransHeader(*iunit, *junit);}

/*!
 * Return the cell size {xlen}, {ylen}, {zlen} and cell angles {alpha}, {beta}, and 
 * {gamma}into [cell] for unit [iunit].  Fortran wrappers iiuRetCell and irtcel.  
 */
void iiuRetCell(int iunit, float *cell)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetCell", 1, 0);
  cell[0] = hdr->xlen;
  cell[1] = hdr->ylen;
  cell[2] = hdr->zlen;
  cell[3] = hdr->alpha;
  cell[4] = hdr->beta;
  cell[5] = hdr->gamma;
}

void iiuretcell(int *iunit, float *cell) {iiuRetCell(*iunit, cell);}
void irtcel(int *iunit, float *cell) {iiuRetCell(*iunit, cell);}

/*!
 * Sets the cell size {xlen}, {ylen}, {zlen} and cell angles {alpha}, {beta}, and {gamma}
 * to values in [cell] for unit [iunit].  Fortran wrappers iiuAltCell and ialcel.  
 */
void iiuAltCell(int iunit, float *cell)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltCell", 1, 0);
  hdr->xlen = cell[0];
  hdr->ylen = cell[1];
  hdr->zlen = cell[2];
  hdr->alpha = cell[3];
  hdr->beta = cell[4];
  hdr->gamma = cell[5];
}

void iiualtcell(int *iunit, float *cell) {iiuAltCell(*iunit, cell);}
void ialcel(int *iunit, float *cell) {iiuAltCell(*iunit, cell);}

/*!
 * Returns data type information for unit [iunit].  The original description of its 
 * meaning is: ^
 * itype = 0       normal mono data ^
 * itype = 1       tilt set        N1 = # angles stored per section
 * if n1=0 then n2=axis, v1=start ang, v2=delang ^
 * itype = 2       stereo pairs stored L,R V1= angle V2 = angle R ^
 * itype = 3       avg mono N1 = number sects avg, N2 = offset per sect ^
 * itype = 4       avg stereo N1 = number sects avg, N2 = offset per sect
 * V1,V2 = angls L,R ^
 * Fortran wrappers iiuRetDataType and irtdat.
 */
void iiuRetDataType(int iunit, int *itype, int *lensNum, int *n1, int *n2, float *v1, 
                    float *v2)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetDataType", 1, 0);
  *itype = hdr->idtype;
  *lensNum = hdr->lens;
  *n1 = hdr->nd1;
  *n2 = hdr->nd2;
  *v1 = 0.01 * hdr->vd1;
  *v2 = 0.01 * hdr->vd2;
}

void iiuretdatatype(int *iunit, int *itype, int *lensNum, int *n1, int *n2, float *v1, 
                    float *v2)
{ iiuRetDataType(*iunit, itype, lensNum, n1, n2, v1, v2);}
void irtdat(int *iunit, int *itype, int *lensNum, int *n1, int *n2, float *v1, float *v2)
{ iiuRetDataType(*iunit, itype, lensNum, n1, n2, v1, v2);}

/*!
 * Sets data type information for unit [iunit], as described in @@iiuRetDataType@.
 * Fortran wrappers iiuAltDataType and ialdat.
 */
void iiuAltDataType(int iunit, int itype, int lensNum, int n1, int n2, float v1, float v2)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetDataType", 1, 0);
  hdr->idtype = itype;
  hdr->lens = lensNum;
  hdr->nd1 = n1;
  hdr->nd2 = n2;
  hdr->vd1 = B3DNINT(v1 * 100.);
  hdr->vd2 = B3DNINT(v2 * 100.);
}

void ialdat(int *iunit, int *itype, int *lensNum, int *n1, int *n2, float *v1, float *v2)
{ iiuAltDataType(*iunit, *itype, *lensNum, *n1, *n2, *v1, *v2);}
void iiualtdatatype(int *iunit, int *itype, int *lensNum, int *n1, int *n2, float *v1, 
                    float *v2)
{ iiuAltDataType(*iunit, *itype, *lensNum, *n1, *n2, *v1, *v2);}

/*!
 * Returns the image dimensions {nx}, {ny}, {nz} into [nxyz], the sample size {mx}, {my},
 * {mz} into [mxyz], and starting coordinates into [nxyzst] for unit [iunit].  Fortran 
 * wrappers iiuRetSize and irtsiz.
 */
void iiuRetSize(int iunit, int *nxyz, int *mxyz, int *nxyzst)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetSize", 1, 0);
  nxyz[0] = hdr->nx;
  nxyz[1] = hdr->ny;
  nxyz[2] = hdr->nz;
  mxyz[0] = hdr->mx;
  mxyz[1] = hdr->my;
  mxyz[2] = hdr->mz;
  nxyzst[0] = hdr->nxstart;
  nxyzst[1] = hdr->nystart;
  nxyzst[2] = hdr->nzstart;
}

void iiuretsize(int *iunit, int *nxyz, int *mxyz, int *nxyzst)
{ iiuRetSize(*iunit, nxyz, mxyz, nxyzst);}
void irtsiz(int *iunit, int *nxyz, int *mxyz, int *nxyzst)
{ iiuRetSize(*iunit, nxyz, mxyz, nxyzst);}


/*!
 * Sets the image dimensions {nx}, {ny}, {nz} to values in [nxyz] and starting indexes to 
 * values in [nxyzst] for unit [iunit].  Fortran wrappers iiuAltSize and ialsiz.
 */
void iiuAltSize(int iunit, int *nxyz, int *nxyzst)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltSize", 1, 0);
  hdr->nx = nxyz[0];
  hdr->ny = nxyz[1];
  hdr->nz = nxyz[2];
  hdr->nxstart = nxyzst[0];
  hdr->nystart = nxyzst[1];
  hdr->nzstart = nxyzst[2];
  iiuSyncWithMrcHeader(iunit);
}

void iiualtsize(int *iunit, int *nxyz, int *nxyzst) {iiuAltSize(*iunit, nxyz, nxyzst);}
void ialsiz(int *iunit, int *nxyz, int *nxyzst) {iiuAltSize(*iunit, nxyz, nxyzst);}

/*!
 * Returns the sample size {mx}, {my}, {mz} into [mxyz] for unit [iunit].  Fortran 
 * wrappers iiuRetSample and irtsam.
 */
void iiuRetSample(int iunit, int *mxyz)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetSample", 1, 0);
  mxyz[0] = hdr->mx;
  mxyz[1] = hdr->my;
  mxyz[2] = hdr->mz;
}

void iiuretsample(int *iunit, int *mxyz) {iiuRetSample(*iunit, mxyz);}
void irtsam(int *iunit, int *mxyz) {iiuRetSample(*iunit, mxyz);}

/*!
 * Sets the sample size {mx}, {my}, {mz} to [mxyz] for unit [iunit].  Fortran wrappers
 * iiuAltSample and ialsam.
 */
void iiuAltSample(int iunit, int *mxyz)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltSample", 1, 0);
  hdr->mx = mxyz[0];
  hdr->my = mxyz[1];
  hdr->mz = mxyz[2];
}

void iiualtsample(int *iunit, int *mxyz) {iiuAltSample(*iunit, mxyz);}
void ialsam(int *iunit, int *mxyz) {iiuAltSample(*iunit, mxyz);}

/*!
 * Returns the (unsupported) axis map values {mapc}, {mapr}, {maps} into [mapcrs] for
 * unit [iunit].  Fortran wrappers iiuRetAxisMap and irtmap.
 */
void iiuRetAxisMap(int iunit, int *mapcrs)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetAxisMap", 1, 0);
  mapcrs[0] = hdr->mapc;
  mapcrs[1] = hdr->mapr;
  mapcrs[2] = hdr->maps;
}

void iiuretaxismap(int *iunit, int *mxyz) {iiuRetAxisMap(*iunit, mxyz);}
void irtmap(int *iunit, int *mxyz) {iiuRetAxisMap(*iunit, mxyz);}

/*!
 * Sets the (unsupported) axis map values {mapc}, {mapr}, {maps} from values in [mapcrs]
 * for unit [iunit].  Fortran wrappers iiuAltAxisMap and ialmap.
 */
void iiuAltAxisMap(int iunit, int *mapcrs)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltAxisMap", 1, 0);
  hdr->mapc = mapcrs[0];
  hdr->mapr = mapcrs[1];
  hdr->maps = mapcrs[2];
}

void iiualtaxismap(int *iunit, int *mxyz) {iiuAltAxisMap(*iunit, mxyz);}
void ialmap(int *iunit, int *mxyz) {iiuAltAxisMap(*iunit, mxyz);}

/*!
 * Returns the {imodFlags} component in [iflags] and 1 if the file has the IMOD stamp in 
 * [ifImod] for unit [iunit].  Fortran wrappers iiuRetImodFlags and irtImodFlags.
 */
void iiuRetImodFlags(int iunit, int *iflags, int *ifImod)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetImodFlags", 1, 0);
  *iflags = hdr->imodFlags;
  *ifImod = hdr->imodStamp == IMOD_MRC_STAMP ? 1 : 0;
}

void iiuretimodflags(int *iunit, int *iflags, int *ifImod)
{ iiuRetImodFlags(*iunit, iflags, ifImod);}
void irtimodflags(int *iunit, int *iflags, int *ifImod)
{ iiuRetImodFlags(*iunit, iflags, ifImod);}

/*
 * Sets the {imodFlags} component to [iflags] for unit [iunit].  Fortran wrappers 
 * iiuAltImodFlags and ialimodflags
 */
void iiuAltImodFlags(int iunit, int iflags)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltImodFlags", 1, 0);
  hdr->imodFlags = iflags;
} 

void iiualtimodflags(int *iunit, int *iflags) {iiuAltImodFlags(*iunit, *iflags);}
void ialimodflags(int *iunit, int *iflags) {iiuAltImodFlags(*iunit, *iflags);}

/*
 * Sets flag for whether to write bytes as signed (transient {byteSigned} member) to 
 * value of [iflags] for unit [iunit].  Fortran wrappers iiuAltSigned and ialsigned.
 */
void iiuAltSigned(int iunit, int iflags)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltSigned", 1, 0);
  hdr->bytesSigned = iflags;
} 

void iiualtsigned(int *iunit, int *iflags) {iiuAltSigned(*iunit, *iflags);}
void ialsigned(int *iunit, int *iflags) {iiuAltSigned(*iunit, *iflags);}

/*!
 * Returns the X, Y, and Z origin values into [xorig], [yorig], and [zorig] for unit
 * [iunit].  Fortran wrappers iiuRetOrigin and irtorg.
 */
void iiuRetOrigin(int iunit, float *xorig, float *yorig, float *zorig)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetOrigin", 1, 0);
  *xorig = hdr->xorg;
  *yorig = hdr->yorg;
  *zorig = hdr->zorg;
}

void iiuretorigin(int *iunit, float *xorig, float *yorig, float *zorig)
{ iiuRetOrigin(*iunit, xorig, yorig, zorig);}
void irtorg(int *iunit, float *xorig, float *yorig, float *zorig)
{ iiuRetOrigin(*iunit, xorig, yorig, zorig);}

/*!
 * Sets the X, Y, and Z origin values from [xorig], [yorig], and [zorig] for unit
 * [iunit].  Fortran wrappers iiuAltOrigin and ialorg.
 */
void iiuAltOrigin(int iunit, float xorig, float yorig, float zorig)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltOrigin", 1, 0);
  hdr->xorg = xorig;
  hdr->yorg = yorig;
  hdr->zorg = zorig;
}

void iiualtorigin(int *iunit, float *xorig, float *yorig, float *zorig)
{ iiuAltOrigin(*iunit, *xorig, *yorig, *zorig);}
void ialorg(int *iunit, float *xorig, float *yorig, float *zorig)
{ iiuAltOrigin(*iunit, *xorig, *yorig, *zorig);}

/*!
 * Sets file mode for unit [iunit] to [mode].  Fortran wrappers iiuAltMode and ialmod.
 */
void iiuAltMode(int iunit, int mode)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltMode", 1, 0);
  hdr->mode = mode;
}

void ialmod(int *iunit, int *mode) {iiuAltMode(*iunit, *mode);}
void iiualtmode(int *iunit, int *mode) {iiuAltMode(*iunit, *mode);}

/*!
 * Returns the RMS value, if set, into [rmsVal] for unit [iunit].  Fortran wrappers 
 * iiuRetRMS and irtrms.
 */
void iiuRetRMS(int iunit, float *rmsVal)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetRMS", 1, 0);
  *rmsVal = hdr->rms;
}

void iiuretrms(int *iunit, float *rmsVal) {iiuRetRMS(*iunit, rmsVal);}
void irtrms(int *iunit, float *rmsVal) {iiuRetRMS(*iunit, rmsVal);}

/*!
 * Sets the RMS value for unit [iunit] to [rmsVal].  Fortran wrappers iiuAltRMS and 
 * ialrms.
 */
void iiuAltRMS(int iunit, float rmsVal)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltRMS", 1, 0);
  hdr->rms = rmsVal;
}

void iiualtrms(int *iunit, float *rmsVal) {iiuAltRMS(*iunit, *rmsVal);}
void ialrms(int *iunit, float *rmsVal) {iiuAltRMS(*iunit, *rmsVal);}

/*!
 * Returns current tilt angles (items 3, 4, 5 in {tiltangles}) into [tilt] for unit
 * [iunit].  Fortran wrappers iiuRetTilt and irttlt.
 */
void iiuRetTilt(int iunit, float *tilt)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetTilt", 1, 0);
  tilt[0] = hdr->tiltangles[3];
  tilt[1] = hdr->tiltangles[4];
  tilt[2] = hdr->tiltangles[5];
}

void iiurettilt(int *iunit, float *tilt) {iiuRetTilt(*iunit, tilt);}
void irttlt(int *iunit, float *tilt) {iiuRetTilt(*iunit, tilt);}

/*!
 * Set current tilt angles (items 3, 4, 5 in {tiltangles}) from values in [tilt] for unit
 * [iunit].  Fortran wrappers iiuAltTilt and ialtlt.
 */
void iiuAltTilt(int iunit, float *tilt)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltTilt", 1, 0);
  hdr->tiltangles[3] = tilt[0];
  hdr->tiltangles[4] = tilt[1];
  hdr->tiltangles[5] = tilt[2];
}

void iiualttilt(int *iunit, float *tilt) {iiuAltTilt(*iunit, tilt);}
void ialtlt(int *iunit, float *tilt) {iiuAltTilt(*iunit, tilt);}

/*!
 * Returns original tilt angles (items 0, 1, 2 in {tiltangles}) into [tilt] for unit
 * [iunit].  Fortran wrappers iiuRetTiltOrig and irttlt_orig.
 */
void iiuRetTiltOrig(int iunit, float *tilt)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetTiltOrig", 1, 0);
  tilt[0] = hdr->tiltangles[0];
  tilt[1] = hdr->tiltangles[1];
  tilt[2] = hdr->tiltangles[2];
}

void iiurettiltorig(int *iunit, float *tilt) {iiuRetTiltOrig(*iunit, tilt);}
void irttlt_orig(int *iunit, float *tilt) {iiuRetTiltOrig(*iunit, tilt);}

/*!
 * Set current tilt angles (items 3, 4, 5 in {tiltangles}) from values in [tilt] for unit
 * [iunit].  Fortran wrappers iiuAltTiltOrig and ialtlt_orig.
 */
void iiuAltTiltOrig(int iunit, float *tilt)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltTiltOrig", 1, 0);
  hdr->tiltangles[0] = tilt[0];
  hdr->tiltangles[1] = tilt[1];
  hdr->tiltangles[2] = tilt[2];
}

void iiualttiltorig(int *iunit, float *tilt) {iiuAltTiltOrig(*iunit, tilt);}
void ialtlt_orig(int *iunit, float *tilt) {iiuAltTiltOrig(*iunit, tilt);}

/*!
 * Rotate the current tilt angles by the X, Y, Z angles in [tilt] for unit
 * [iunit].  Fortran wrappers iiuAltTiltRot and ialtlt_rot.
 */
void iiuAltTiltRot(int iunit, float *tilt)
{
  float amat1[9], amat2[9], amat3[9];
  int k, l, m;
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltTiltRot", 1, 0);

  /* Convert new and old angles to matrices, then multiply */
  anglesToMatrix(tilt, amat1, 3);
  anglesToMatrix(&hdr->tiltangles[3], amat2, 3);
  for (k = 0; k < 3; k++) {
    for (l = 0; l < 3; l++) {
      amat3[l + 3 * k] = 0;
      for (m = 0; m < 3; m++)
        amat3[l + 3 * k] += amat1[l + 3 * m] * amat2[m + 3 * k];
    }
  }

  /* Convert back to angles */
  icalc_angles(&hdr->tiltangles[3], amat3);
}

void iiualttiltrot(int *iunit, float *tilt) {iiuAltTiltRot(*iunit, tilt);}
void ialtlt_rot(int *iunit, float *tilt) {iiuAltTiltRot(*iunit, tilt);}

/*!
 * Returns the pixel size in X, Y and Z in Angstroms into [delta] for unit [iunit].
 * This is defined as the cell size divided by the sample size, or 1 if the latter is
 * not positive.  Fortran wrappers iiuRetDelta and irtdel.
 */
void iiuRetDelta(int iunit, float *delta)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetDelta", 1, 0);
  delta[0] = hdr->mx > 0 ? hdr->xlen / hdr->mx : 1.;
  delta[1] = hdr->my > 0 ? hdr->ylen / hdr->my : 1.;
  delta[2] = hdr->mz > 0 ? hdr->zlen / hdr->mz : 1.;
}

void iiuretdelta(int *iunit, float *delta) {iiuRetDelta(*iunit, delta);}
void irtdel(int *iunit, float *delta) {iiuRetDelta(*iunit, delta);}

/*!
 * Sets pixel size in X, Y, Z in Angstroms to the values in [delta] for unit [iunit] by
 * changing the cell sizes.  Fortran wrappers iiuAltDelta and ialdel.
 */
void iiuAltDelta(int iunit, float *delta)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltDelta", 1, 0);
  hdr->mx = B3DMAX(1, hdr->mx);
  hdr->xlen = delta[0] * hdr->mx;
  hdr->my = B3DMAX(1, hdr->my);
  hdr->ylen = delta[0] * hdr->my;
  hdr->mz = B3DMAX(1, hdr->mz);
  hdr->zlen = delta[0] * hdr->mz;
}

void iiualtdelta(int *iunit, float *delta) {iiuAltDelta(*iunit, delta);}
void ialdel(int *iunit, float *delta) {iiuAltDelta(*iunit, delta);}

/*!
 * Returns the space group entry, {ispg}, into [ispg] for unit [iunit].  Fortran
 * wrapper iiuRetSpaceGroup.
 */
void iiuRetSpaceGroup(int iunit, int *ispg)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetSpaceGroup", 1, 0);
  *ispg = hdr->ispg;
}

void iiuretspacegroup(int *iunit, int *ispg) {iiuRetSpaceGroup(*iunit, ispg);}

/*!
 * Returns number of labels for unit [iunit] in [numLabels] and returns the 80 characters
 * of that number of label contiguously into [labels].  Fortran wrappers iiuRetLabels
 * and irtlab.
 */
void iiuRetLabels(int iunit, int *labels, int *numLabels)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetLabels", 1, 0);
  int i;
  *numLabels = hdr->nlabl;
  for (i = 0; i < hdr->nlabl; i++)
    memcpy(labels + 20 * i, hdr->labels[i], MRC_LABEL_SIZE);
}

void iiuretlabels(int *iunit, int *labels, int *numLabels)
{ iiuRetLabels(*iunit, labels, numLabels);}
void irtlab(int *iunit, int *labels, int *numLabels)
{ iiuRetLabels(*iunit, labels, numLabels);}

/*!
 * Sets the labels for unit [iunit] from the array in [labels], which should have the
 * 80 characters for [numLabels] labels packed contiguously.  The labels may be 
 * null-terminated; the null and following bytes will be filled with spaces.
 * Fortran wrappers iiuAltLabels and iallab.
 */
void iiuAltLabels(int iunit, int *labels, int numLabels)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltLabels", 1, 0);
  int i;
  hdr->nlabl = B3DMIN(MRC_NLABELS, numLabels);
  for (i = 0; i < hdr->nlabl; i++) {
    memcpy(hdr->labels[i], labels + 20 * i, MRC_LABEL_SIZE);
    fixTitlePadding(hdr->labels[i]);
  }
  for (i = hdr->nlabl; i < MRC_NLABELS; i++)
    memset(hdr->labels[i], 0, MRC_LABEL_SIZE + 1);
}

void iiualtlabels(int *iunit, int *labels, int *numLabels)
{ iiuAltLabels(*iunit, labels, *numLabels);}
void iallab(int *iunit, int *labels, int *numLabels)
{ iiuAltLabels(*iunit, labels, *numLabels);}

/*!
 * Transfers labels from unit [iunit] to [intoUnit]. Fortran wrappers iiuTransLabels and
 * itrlab.
 */
void iiuTransLabels(int intoUnit, int iunit)
{
  MrcHeader *ihdr = iiuMrcHeader(iunit, "iiuTransLabels", 1, 0);
  MrcHeader *jhdr = iiuMrcHeader(intoUnit, "iiuTransLabels", 1, 0);
  mrc_head_label_cp(ihdr, jhdr);
}

void iiutranslabels(int *iunit, int *junit) {iiuTransLabels(*iunit, *junit);}
void itrlab(int *iunit, int *junit) {iiuTransLabels(*iunit, *junit);}

/*!
 * Returns the number of bytes in the extended header of unit [iuit] into [numBytes].
 * Fortran wrappers iiuRetNumExtended and irtnbsym.
 */
void iiuRetNumExtended(int iunit, int *numBytes)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuRetNumExtended", 1, 0);
  *numBytes = hdr->next;
}

void iiuretnumextended(int *iunit, int *numBytes) {iiuRetNumExtended(*iunit, numBytes);}
void irtnbsym(int *iunit, int *numBytes) {iiuRetNumExtended(*iunit, numBytes);}

/*!
 * Sets the number of bytes in the extended header of unit [iuit] ito [numBytes].
 * Fortran wrappers iiuAltNumExtended and irtnbsym.
 */
void iiuAltNumExtended(int iunit, int numBytes)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiuAltNumExtended", 1, 0);
  hdr->next = numBytes;
  hdr->headerSize = 1024 + numBytes;
}

void iiualtnumextended(int *iunit, int *numBytes) {iiuAltNumExtended(*iunit, *numBytes);}
void ialnbsym(int *iunit, int *numBytes) {iiuAltNumExtended(*iunit, *numBytes);}

/*!
 * Returns the elements {nint} and {nreal} describing the extended header data type and
 * size per section for unit [iunit].  Fortran wrappers iiuRetExtendedType and irtsymtyp.
 */
void iiuRetExtendedType(int iunit, int *nintOrFlags, int *nrealOrBytes)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiRetExtendedType", 1, 0);
  *nintOrFlags = hdr->nint;
  *nrealOrBytes = hdr->nreal;
}

void iiuretextendedtype(int *iunit, int *nintOrFlags, int *nrealOrBytes)
{ iiuRetExtendedType(*iunit, nintOrFlags, nrealOrBytes);}
void irtsymtyp(int *iunit, int *nintOrFlags, int *nrealOrBytes)
{ iiuRetExtendedType(*iunit, nintOrFlags, nrealOrBytes);}

/*!
 * Sets the elements {nint} and {nreal} unit [iunit].  Fortran wrappers 
 * iiuAltExtendedType and ialsymtyp.
 */
void iiuAltExtendedType(int iunit, int nintOrFlags, int nrealOrBytes)
{
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiAltExtendedType", 1, 0);
  hdr->nint = nintOrFlags;
  hdr->nreal = nrealOrBytes;
}

void iiualtextendedtype(int *iunit, int *nintOrFlags, int *nrealOrBytes)
{ iiuAltExtendedType(*iunit, *nintOrFlags, *nrealOrBytes);}
void ialsymtyp(int *iunit, int *nintOrFlags, int *nrealOrBytes)
{ iiuAltExtendedType(*iunit, *nintOrFlags, *nrealOrBytes);}

/*!
 * Returns the extended header data for unit [iunit] into array [extra] with byte 
 * swapping if needed and returns the number of bytes into [numBytes].  Returns -1 for 
 * internal errors or 1 for a read error.  Fortran wrappers
 * iiuRetExtendedData and irtsym (a void).
 */
int iiuRetExtendedData(int iunit, int *numBytes, int *extra)
{
  int doExit = iiuGetExitOnError();
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiRetExtendedData", doExit, 1);
  int i, indConv;
  if (!hdr)
    return -1;
  *numBytes = hdr->next;
  if (!hdr->next)
    return 0;
  b3dFseek(hdr->fp, 1024, SEEK_SET);
  if (b3dFread(extra, 1, hdr->next, hdr->fp) != hdr->next) {
    fprintf(stdout, "\nERROR: iiRetExtendedData - Reading %d bytes of extended header "
            "data for unit %d.\n", hdr->next, iunit);
    if (doExit)
      exit(1);
    return 1;
  }

  /* Swap data as shorts or as ints and floats */
  if (hdr->swapped) {
    if (extraIsNbytesAndFlags(hdr->nint, hdr->nreal) || (!hdr->nint && !hdr->nreal)) {
      mrc_swap_shorts((b3dInt16 *)extra, hdr->next / 2);
    } else {
      indConv = 0;
      for (i = 0; i < hdr->next / (4 * (hdr->nint + hdr->nreal)); i++) {
        if (hdr->nint > 0)
          mrc_swap_longs(&extra[indConv], hdr->nint);
        indConv += hdr->nint;
        if (hdr->nreal > 0)
          mrc_swap_floats((b3dFloat *)&extra[indConv], hdr->nreal);
        indConv += hdr->nreal;
      }
    }
  }
  return 0;
}

int iiuretextendeddata(int *iunit, int *numBytes, int *extra)
{ return iiuRetExtendedData(*iunit, numBytes, extra);}
void irtsym(int *iunit, int *numBytes, int *extra)
{ iiuRetExtendedData(*iunit, numBytes, extra);}

/*!
 * Set the extended header data for unit [iunit] to be [numBytes] bytes with the contents 
 * of array [extra].  Returns negative values for internal errors or 2 for a write error.
 * Fortran wrappers iiuAltExtendedData and ialsym (a void).
 */
int iiuAltExtendedData(int iunit, int numBytes, int *extra)
{
  int doExit = iiuGetExitOnError();
  MrcHeader *hdr = iiuMrcHeader(iunit, "iiAltExtendedData", doExit, 2);
  if (!hdr)
    return -1;
  if (hdr->swapped) {
    fprintf(stdout, "\nERROR: iiuAltExtendedData - Cannot write extra header data to a"
            " byte-swapped file (unit %d).\n", iunit);
    if (doExit)
      exit(1);
    return -2;
  }
  hdr->next = numBytes;
  hdr->headerSize = 1024 + numBytes;
  b3dFseek(hdr->fp, 1024, SEEK_SET);
  if (b3dFwrite(extra, 1, numBytes, hdr->fp) != numBytes) {
    fprintf(stdout, "\nERROR: iiAltExtendedData - Writing %d bytes of extended header "
            "data for unit %d.\n", numBytes, iunit);
    if (doExit)
      exit(1);
    return 2;
  }
  return 0;
}

int iiualtextendeddata(int *iunit, int *numBytes, int *extra)
{ return iiuAltExtendedData(*iunit, *numBytes, extra);}
void ialsym(int *iunit, int *numBytes, int *extra)
{ iiuAltExtendedData(*iunit, *numBytes, extra);}

/*!
 * Transfers extended header data from unit [iunit] to [intoUnit], which is not allowed to
 * be a byte-swapped file.  Returns negative values for internal errors, 1 for a read 
 * error, or 2 for a write error.  Fortran wrappers iiuTransExtendedData and 
 * itrextra (a void).
 */
int iiuTransExtendedData(int intoUnit, int iunit)
{
  int *extra;
  int numBytes, err;
  int doExit = iiuGetExitOnError();
  MrcHeader *ihdr = iiuMrcHeader(iunit, "iiuTransExtendedData", doExit, 1);
  MrcHeader *jhdr = iiuMrcHeader(intoUnit, "iiuTransExtendedData", doExit, 2);
  if (!ihdr || !jhdr)
    return -1;
  if (!ihdr->next) {
    iiuAltNumExtended(intoUnit, 0);
    return 0;
  }
  extra = B3DMALLOC(int, (ihdr->next + 3) / 4);
  iiuMemoryError(extra, "ERROR: iiuTransExtendedData - Allocating buffer for data");
  jhdr->nint = ihdr->nint;
  jhdr->nreal = ihdr->nreal;
  err = iiuRetExtendedData(iunit, &numBytes, extra);
  if (err)
    return 1;
  return iiuAltExtendedData(intoUnit, numBytes, extra);
}

int iiutransextendeddata(int *iunit, int *junit)
{ return iiuTransExtendedData(*iunit, *junit);}
void itrextra(int *iunit, int *junit) {iiuTransExtendedData(*iunit, *junit);}
