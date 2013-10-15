/*
 *  transforms.cpp -- Transformation functions used in midas.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <math.h>
#include "midas.h"
#include "imodel.h"
#include <qfile.h>
#include <QTextStream>
#include "dia_qtutils.h"
#include "b3dutil.h"
#include "sliceproc.h"
#include "cfft.h"

static void checklist(int *xpclist, int npclist, int nxframe, int *minxpiece,
                      int *nxpieces, int *nxoverlap);
static void solve_for_shifts(MidasView *vw, float *b, int *ivarpc, int *indvar,
                             int nvar, int leavind);
static int lowerEdgeIfIncluded(int ipc, int ixy);
static int upperEdgeIfIncluded(int ipc, int ixy);
static int checkPathList(MidasView *vw, int ipc, int jpc, int &numOnList);
static int piecesNotConnected(MidasView *vw, int leavind);

int new_view(MidasView *vw)
{
  int i;
  vw->xsize = 0;
  vw->ysize = 0;
  vw->zsize = 0;
  vw->xysize = 0;
  vw->binning = 1;
  vw->cz = 0;
  vw->ref = NULL;
  vw->showref = 0;
  vw->tr = NULL;
  vw->sangle = 0;
  vw->sdatSize = 0;
  vw->sdat = NULL;
  vw->id   = NULL;
  vw->zoom = 1.;     
  vw->truezoom = 1.;
  vw->zoomind = 5;
  vw->mousemoving = 0;
  vw->useFixed = 0;
     
  vw->xtrans = 0;
  vw->ytrans = 0;
  vw->vmode = MIDAS_VIEW_COLOR;
  vw->fastInterp = TRUE;

  vw->width = 0;
  vw->height= 0;

  vw->xtype = XTYPE_XF;
  vw->xname = NULL;
  vw->oname = NULL;
  vw->changed = 0;
  vw->didsave = 0;
  vw->refname = NULL;
  vw->xsec  = 0;
  vw->plname = NULL;
  vw->refSpin = NULL;
  vw->wXedge = NULL;
  vw->wMeanerr = NULL;
  vw->curWarpFile = -1;
  vw->editWarps = false;
  vw->drawVectors = false;
  vw->numWarpBackup = 0;
  vw->maxWarpBackup = 0;
  vw->backupXcontrol = NULL;
  vw->backupYcontrol = NULL;
  vw->backupXvector = NULL;
  vw->backupYvector = NULL;
  vw->gridSize = 0;
  vw->gridDx = NULL;
  vw->gridDy = NULL;
  vw->lastWarpedZ = -1;
  vw->lastGridDx = NULL;
  vw->lastGridDy = NULL;
  vw->lastGridSize = 0;
  vw->cachein = 0;
  vw->sminin = 0;
  vw->smaxin = 0;
  vw->rotMode = 0;
  vw->globalRot = 0.;
  vw->cosStretch = 0;
  vw->tiltname = NULL;
  vw->tiltOffset = 0.;
  vw->numChunks = 0;
  vw->cachesize = 0;
  vw->cache = NULL;
  vw->usecount = 0;
  vw->quiet = 0;

  vw->exposed = 0;
  vw->blackstate = 0;
  vw->whitestate = 255;
  vw->reversemap = 0;
  vw->applytoone = 0;
  vw->difftoggle = NULL;
  vw->keepsecdiff = 1;
  vw->mouseXonly = 0;
  vw->anySkipped = 0;
  vw->robustFit = 0;
  vw->robustCrit = 1.;
  vw->excludeSkipped = 0;
  vw->centerXory = -1;
  vw->numTopErr = 6;
  vw->skipErr = 0;
  vw->ctrlPressed = 0;
  vw->shiftPressed = 0;
  vw->midasWindow = NULL;
  vw->imageForChannel[0] = 1;
  vw->imageForChannel[1] = 2;
  vw->imageForChannel[2] = 1;
  for (i = 0; i < 3; i++) {
    vw->incindex[i] = 4;
    vw->increment[i] = vw->midasSlots->getIncrement(4, i);
  }
  for (i = 0; i < 5; i++)
    vw->paramstate[i] = -999.;
  return(0);
}

/* Sets the current image to the given filename.
 */
int load_view(MidasView *vw, char *fname)
{
  int k, ix, ixy, iy, ind, ipclo, ipchi, ned;
  QString str;
  int maxedges, nsect, nxypc, maxpieces, nalong, ncross;

  if (load_image(vw, fname))
    return(-1);

  vw->cz = 1;
  vw->refz = 0;
  ix = B3DMIN(vw->hin->nx, vw->hin->ny);
  vw->binning = B3DMIN(ix, vw->binning);
  vw->zsize = vw->hin->nz;
  
  // Adjust X and Y size for the binning.  Note that montage parameters like
  // piece coordinates and overlap are never adjusted for binning, so 
  // computations with them need to access the original size from hin
  vw->xsize = vw->hin->nx / vw->binning;
  vw->ysize = vw->hin->ny / vw->binning;
  vw->xysize = vw->xsize * vw->ysize;
  vw->xcenter = 0.5 * vw->xsize;
  vw->ycenter = 0.5 * vw->ysize;
  ix = B3DMAX(vw->xsize, vw->ysize);
  vw->corrBoxSize = B3DMAX(32, 16 * B3DNINT(ix / 80.));
  vw->corrShiftLimit = B3DMAX(2, 2 * B3DNINT(ix / 20.));

  /* Maximum cache size given memory allowed */
  vw->cachesize = 1000000 * MAX_CACHE_MBYTES / vw->xysize;

  /* input value overrides this completely */
  if (vw->cachein)
    vw->cachesize = vw->cachein;

  /* But limit by number of sections */
  if (vw->cachesize > vw->zsize)
    vw->cachesize = vw->zsize;

  /* And require at least 4 spots */
  if (vw->cachesize < 4)
    vw->cachesize = 4;

  vw->cache = (Midas_cache *)malloc(vw->cachesize * sizeof(Midas_cache));
  if (vw->binning > 1) {
    vw->unbinnedBuf = (unsigned char *)malloc(vw->hin->nx * vw->hin->ny);
    if (!vw->unbinnedBuf)
      midas_error("Out of memory! ", "Exiting", 3);
  }
  for(k = 0; k < vw->cachesize; k++){
    vw->cache[k].sec = sliceCreate(vw->xsize, vw->ysize, 
                                        MRC_MODE_BYTE);
    if (!vw->cache[k].sec)
      midas_error("Out of memory! ", "Exiting", 3);
    vw->cache[k].zval = -1;
    vw->cache[k].xformed = -1;
    vw->cache[k].used = -1;
    vw->cache[k].sec->mean = vw->hin->amean * vw->li->slope + 
      vw->li->offset;
  }

  vw->tr = (Midas_transform *)malloc((vw->zsize + 1) * sizeof(Midas_transform));
  for (k = 0 ; k <= vw->zsize; k++){
    vw->tr[k].black = 0;
    vw->tr[k].white = 255;
    tramat_idmat(vw->tr[k].mat);
  }

  vw->id = (b3dUInt32 *)malloc(vw->xysize * sizeof(b3dUInt32));

  /* Create and load slice for reference image now */
  if (vw->refname) {
    vw->ref = sliceCreate(vw->xsize, vw->ysize, MRC_MODE_BYTE);
    if (!vw->ref)
      midas_error("Out of memory! ", "Exiting", 3);
    if (load_refimage(vw, vw->refname))
      return(1);
    vw->refz = vw->xsec;
    vw->keepsecdiff = 0;
    vw->cz = 0;
  }

  /* Analyze chunk list and set up view sections for each pair */
  if (vw->numChunks) {
    if (vw->chunk[vw->numChunks].start < vw->zsize) {
      vw->chunk[vw->numChunks].start = vw->zsize;
      dia_puts("The total number of sections in the chunk list is less than "
               "the number of sections in the image.  The extra sections are "
               "assumed to be in the last chunk");
    } else if (vw->chunk[vw->numChunks].start > vw->zsize) {
      if (vw->chunk[vw->numChunks - 1].start >= vw->zsize)
        midas_error("The total number of sections in the chunk list is too "
                    "large for the number of sections in the file", "", 1);
      vw->chunk[vw->numChunks].start = vw->zsize;
      dia_puts("The total number of sections in the chunk list is more than "
               "the number of sections in the image.  The extra sections are "
               "assumed to be in the last chunk");
    }
    vw->chunk[vw->numChunks - 1].size = vw->chunk[vw->numChunks].start -
      vw->chunk[vw->numChunks - 1].start;

    for (k = 0; k < vw->numChunks; k++) {
      vw->chunk[k].curSec = vw->chunk[k].start;
      vw->chunk[k].refSec = vw->chunk[k + 1].start - 1;
      /*printf("%d %d %d %d %d\n", k, vw->chunk[k].size, vw->chunk[k].start,
        vw->chunk[k].curSec, vw->chunk[k].refSec); */
    }
    vw->cz = vw->chunk[1].curSec;
    vw->refz = vw->chunk[0].refSec;
    vw->curChunk = 1;
    vw->keepsecdiff = 0;
  }

  /* Now get pieces and analyze them if montage */
  if (vw->plname) {
    str = vw->plname;
    QFile file(str);
    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
      midas_error("Error opening ", vw->plname, 3);

    QTextStream stream(&file);
    stream.setIntegerBase(10);

    vw->xpclist = (int *)malloc(vw->zsize * sizeof(int));
    vw->ypclist = (int *)malloc(vw->zsize * sizeof(int));
    vw->zpclist = (int *)malloc(vw->zsize * sizeof(int));
    vw->edgeupper = (int *)malloc(2 * vw->zsize * sizeof(int));
    vw->edgelower = (int *)malloc(2 * vw->zsize * sizeof(int));
    vw->fbs_indvar = (int *)malloc(2 * vw->zsize * sizeof(int));
    if (!vw->xpclist || !vw->ypclist || !vw->zpclist || !vw->edgeupper
        || !vw->edgelower || !vw->fbs_indvar)
      midas_error("Error getting memory for piece list.", "", 3);

    vw->minzpiece = 1000000;
    vw->maxzpiece = -1000000;
    for (k = 0; k < vw->zsize; k++) {
      str = stream.readLine();
      if (str.isEmpty()) {
        str.sprintf("Error reading piece list after %d lines\n" , k);
        midas_error(LATIN1(str), "", 3);
      }

      sscanf(LATIN1(str), "%d %d %d", &(vw->xpclist[k]), 
                       &(vw->ypclist[k]), &(vw->zpclist[k]));
      if (vw->minzpiece > vw->zpclist[k])
        vw->minzpiece = vw->zpclist[k];
      if (vw->maxzpiece < vw->zpclist[k])
        vw->maxzpiece = vw->zpclist[k];
    }

    checklist(vw->xpclist, vw->zsize, vw->hin->nx, &vw->minxpiece, 
              &vw->nxpieces, &vw->nxoverlap);
    checklist(vw->ypclist, vw->zsize, vw->hin->ny, &vw->minypiece, 
              &vw->nypieces, &vw->nyoverlap);
    if (vw->nxpieces < 0 || vw->nypieces < 0)
      midas_error("Piece list does not have regular array "
                  "of coordinates.", "", 3);

    vw->maxedge[0] = vw->nypieces * (vw->nxpieces - 1);
    vw->maxedge[1] = vw->nxpieces * (vw->nypieces - 1);
    maxedges = vw->maxedge[0];
    if (maxedges < vw->maxedge[1])
      maxedges = vw->maxedge[1];
    nsect = (vw->maxzpiece + 1 - vw->minzpiece);
    maxedges *= nsect;
    nxypc = vw->nxpieces * vw->nypieces;
    maxpieces = nxypc * nsect;
    vw->pieceupper = (int *)malloc(2 * maxedges * sizeof(int));
    vw->piecelower = (int *)malloc(2 * maxedges * sizeof(int));
    vw->edgedx = (float *)malloc(2 * maxedges * sizeof(float));
    vw->edgedy = (float *)malloc(2 * maxedges * sizeof(float));
    vw->skippedEdge = (int *)malloc(2 * maxedges * sizeof(int));
    vw->montmap = (int *)malloc(maxpieces * sizeof(int));
    vw->fbs_work = (int *)malloc((nxypc * 41 / 2 + 1) * sizeof(int));
    vw->fbs_b = (float *)malloc(2 * nxypc * sizeof(float));
    vw->fbs_ivarpc = (int *)malloc(nxypc * sizeof(int));
    vw->pathList = (int *)malloc(nxypc * sizeof(int));
    vw->leaveType = (unsigned char *)malloc(vw->zsize * sizeof(unsigned char));
    if (!vw->montmap || !vw->pieceupper || !vw->piecelower ||
        !vw->edgedx || !vw->edgedy || !vw->fbs_work || !vw->fbs_b ||
        !vw->fbs_ivarpc || !vw->skippedEdge || !vw->pathList || !vw->leaveType)
      midas_error("Error getting memory for piece analysis.", "", 3);

    /*get edge indexes for pieces and piece indexes for edges
      first build a vw->montmap of all pieces present */
    for (k = 0; k < maxpieces; k++)
      vw->montmap[k] = -1;

    for (k = 0; k < vw->zsize; k++) {
      vw->montmap[(vw->xpclist[k] - vw->minxpiece) / 
                  (vw->hin->nx - vw->nxoverlap) +
                  vw->nxpieces * (vw->ypclist[k] - vw->minypiece) /
                  (vw->hin->ny - vw->nyoverlap) +
                  nxypc * (vw->zpclist[k] - vw->minzpiece)] = k;
      vw->edgelower[2 * k] = -1;
      vw->edgeupper[2 * k] = -1;
      vw->edgelower[2 * k + 1] = -1;
      vw->edgeupper[2 * k + 1] = -1;
    }
          
    /* look at all the edges in turn, add to list if pieces on both 
       sides */

    nalong = vw->nypieces;
    ncross = vw->nxpieces;
    for (ixy = 0; ixy < 2; ixy++) {
      ned=0;
      for (k = 0; k < nsect; k++) {
        for (iy = 0; iy < nalong; iy++) {
          for (ix = 1; ix < ncross; ix++) {
            if (!ixy) {
              ind = ix + iy * vw->nxpieces + k * nxypc;
              ipclo=vw->montmap[ind - 1];
            } else { 
              ind = iy + ix * vw->nxpieces + k * nxypc;
              ipclo=vw->montmap[ind - vw->nxpieces];
            }
            ipchi=vw->montmap[ind];

            if(ipclo >= 0 && ipchi >= 0) {
              vw->piecelower[2 * ned + ixy] = ipclo;
              vw->pieceupper[2 * ned + ixy] = ipchi;
              vw->edgelower[2 * ipchi + ixy] = ned;
              vw->edgeupper[2 * ipclo + ixy] = ned;
              ned++;
            }
          }
        }
      }
      vw->nedge[ixy] = ned;
      nalong = vw->nxpieces;
      ncross = vw->nypieces;
    }
          
    if (!vw->nedge[0] && !vw->nedge[1])
      midas_error("There are no edges or no montage "
              "according to these piece coordinates.", "", 3);
          
    vw->xory = 0;
    vw->montcz = nearest_section(vw, vw->minzpiece, 0);
    vw->midasSlots->manage_xory(vw);
    vw->curedge = nearest_edge(vw, vw->montcz, vw->xory, 1, 0, 
                               &vw->edgeind);

    // Initialize the edge dxy to zero then load them if there is a file
    for (k = 0; k < 2 * maxedges; k++) {
      vw->edgedx[k] = vw->edgedy[k] = 0.;
      vw->skippedEdge[k] = 0;
    }

    set_mont_pieces(vw);
    vw->incindex[2]--;
    vw->increment[2] = vw->midasSlots->getIncrement(vw->incindex[2], 2);

    if (vw->didsave == -1) {
      if (load_transforms(vw, vw->xname))
        exit(3);
      vw->didsave = 0;
    }
  }

  // Load tilt angles
  if (vw->cosStretch)
    load_angles(vw);

  vw->midasGL->fill_viewdata(vw);

  if (vw->didsave)
    load_transforms(vw, vw->xname);
  vw->didsave = 0;

  if (vw->oname) {
    if (vw->xname)
      free(vw->xname);
    vw->xname = vw->oname;
  }

  vw->midasSlots->backup_current_mat();
  return(0);
}


Islice *getRawSlice(MidasView *vw, int zval)
{
  int k;
  int minuse = vw->usecount + 1;
  int oldest = 0;

  // Return the reference slice for zval out of range
  if (zval >= vw->zsize)
    return vw->ref;

  for (k = 0; k < vw->cachesize; k++) {
    /* If it's a match, mark as used and return */
    if (vw->cache[k].zval == zval && !vw->cache[k].xformed) {
      vw->cache[k].used = vw->usecount++;
      return (vw->cache[k].sec);
    }

    /* keep track of oldest slice encountered */
    if (vw->cache[k].used < minuse) {
      minuse = vw->cache[k].used;
      oldest = k;
    }
  }

  /* Allocate oldest spot to this slice and read it in */
  vw->cache[oldest].zval = zval;
  vw->cache[oldest].xformed = 0;
  vw->cache[oldest].used = vw->usecount++;
  vw->cache[oldest].nControl = 0;
  midasReadZByte(vw, vw->hin, vw->li, vw->cache[oldest].sec->data.b, zval);
  return (vw->cache[oldest].sec);
}

static Islice *getXformSlice(MidasView *vw, int zval, int shiftOK,
                             int *xformed)
{
  int k, i, izwarp = -1, nControl;
  int minuse = vw->usecount + 1;
  int oldest = 0;
  Islice *orgSlice;
  float mat[9]; 
  float rmat[9];
  int found = -1;
  int matchshift = 0;
  int match2x2 = 0;
  int matchwarp = 0;
  double angle, lastAng, stretch;
  float *xControl, *yControl, *xVector, *yVector;
  int nxGrid, nyGrid;
  float xInterval, yInterval, meanSDs[8], sem, xStart, yStart;
  *xformed = 1;

  /* Get the transformation that needs to be applied */
  tramat_copy(vw->tr[zval].mat, mat);
  if (vw->rotMode) {
    tramat_idmat(rmat);
    tramat_rot(rmat, vw->globalRot);
    if (zval == vw->cz || vw->xtype == XTYPE_XG) {
      if (vw->cosStretch > 0 && zval) {
        lastAng = (vw->tiltAngles[vw->refz]-vw->tiltOffset)*RADIANS_PER_DEGREE;
        angle = (vw->tiltAngles[zval] - vw->tiltOffset) * RADIANS_PER_DEGREE;
        stretch = cos(lastAng) / cos(angle);
        tramat_scale(rmat, stretch, 1.);
      }
      tramat_multiply(rmat, vw->tr[zval].mat, mat);
    } else
      tramat_copy(rmat, mat);
  }

  // Set index of warping section as flag for warping
  izwarp = VW->numChunks ? VW->curChunk : VW->cz;
  if (VW->curWarpFile < 0 || izwarp >= VW->warpNz || getNumWarpPoints(izwarp, &nControl)
      || nControl <= 3)
    izwarp = -1;

  // If warping, get all the factors needed to match cache values
  if (izwarp >= 0) {
    if (gridSizeFromSpacing(izwarp, -1., -1., 1) || 
        getGridParameters(izwarp, &nxGrid, &nyGrid, &xStart, &yStart, &xInterval, 
                          &yInterval) || getWarpPointArrays(izwarp, &xControl, &yControl,
                                                            &xVector, &yVector)) {
      izwarp = -1;
    } else {
      avgSD(xControl, nControl, &meanSDs[0], &meanSDs[1], &sem);
      avgSD(yControl, nControl, &meanSDs[2], &meanSDs[3], &sem);
      avgSD(xVector, nControl, &meanSDs[4], &meanSDs[5], &sem);
      avgSD(yVector, nControl, &meanSDs[6], &meanSDs[7], &sem);
    }
  }

  /* search cache for transformed slice.  */
  for (k = 0; k < vw->cachesize; k++) {
    if (vw->cache[k].zval == zval && vw->cache[k].xformed) {
      vw->cache[k].used = vw->usecount++;
      //printf("%d %d %d %d\n", k, vw->cache[k].zval, zval, vw->cache[k].xformed); 

      /* If slice is in cache, check for match of 2x2 matrix and shift terms */
      if (fabs((double)(vw->cache[k].mat[0] - mat[0])) < 0.00001 &&
          fabs((double)(vw->cache[k].mat[1] - mat[1])) < 0.00001 &&
          fabs((double)(vw->cache[k].mat[3] - mat[3])) < 0.00001 &&
          fabs((double)(vw->cache[k].mat[4] - mat[4])) < 0.00001)
        match2x2 = 1;
      if (fabs((double)(vw->cache[k].mat[6] - mat[6])) < 0.0001 &&
          fabs((double)(vw->cache[k].mat[7] - mat[7])) < 0.0001)
        matchshift = 1;

      // Check for match of warping parameters
      //printf("%d %d\n", vw->cache[k].nControl, nControl);
      if (match2x2 && matchshift && izwarp >= 0 && vw->cache[k].nControl == nControl &&
          vw->cache[k].nxGrid == nxGrid && vw->cache[k].nyGrid == nyGrid && 
          vw->cache[k].xStart == xStart && vw->cache[k].yStart == yStart && 
          vw->cache[k].xInterval == xInterval && vw->cache[k].yInterval == yInterval) {
        matchwarp = 1;
        for (i = 0; i < 8; i++)
          if (vw->cache[k].meanSDs[i] != meanSDs[i])
            matchwarp = 0;
      }
          
      /* All match, return slice as transformed */
      if (match2x2 && matchshift && 
          (matchwarp || (izwarp < 0 && !vw->cache[k].nControl))) 
        return (vw->cache[k].sec);

      /* Shift mismatch and this is OK, copy matrix and return, mark as not
         transformed */
      if (match2x2 && shiftOK && izwarp < 0 && !vw->cache[k].nControl) {
        *xformed = 0;
        tramat_copy(mat, vw->cache[k].mat);
        return (vw->cache[k].sec);
      }

      /* Otherwise break out with slice found, get original slice */
      orgSlice = getRawSlice(vw, zval);
      found = k;
      break;
    }
  }

  /* If need to set up a new slice in cache */
  if (found < 0) {

    /* If it's a unit transformation, just return the raw slice and mark
       as already transformed */
    if (mat[0] == 1.0 && mat[4] == 1.0 && mat[1] == 0.0 && mat[3] == 0.0 &&
        fabs((double)mat[6]) < 0.0001 && fabs((double)mat[7]) < 0.0001 && izwarp < 0) {
      *xformed = -1;
      return (getRawSlice(vw, zval));
    }

    /* Make sure the original slice is in cache before looking for oldest */
    orgSlice = getRawSlice(vw, zval);

    /* Find oldest slice */
    for (k = 0; k < vw->cachesize; k++) {
      if (vw->cache[k].used < minuse) {
        minuse = vw->cache[k].used;
        oldest = k;
      }
    }

    /* Allocate oldest spot to this slice and mark grid as not reusable */
    vw->cache[oldest].zval = zval;
    vw->cache[oldest].xformed = 1;
    vw->cache[oldest].used = vw->usecount++;
    found = oldest;
    VW->lastWarpedZ = -1;
  }

  // Transform raw slice and set the warping parameters
  midas_transform(zval, orgSlice, vw->cache[found].sec, mat, izwarp);
  tramat_copy(mat, vw->cache[found].mat);
  if (izwarp < 0)
    vw->cache[found].nControl = 0;
  else {
    vw->cache[found].nControl = nControl;
    vw->cache[found].nxGrid = nxGrid;
    vw->cache[found].nyGrid = nyGrid;
    vw->cache[found].xStart = xStart;
    vw->cache[found].yStart = yStart;
    vw->cache[found].xInterval = xInterval;
    vw->cache[found].yInterval = yInterval;
    for (i = 0; i < 8; i++)
      vw->cache[found].meanSDs[i] = meanSDs[i];
  }
  return (vw->cache[found].sec);
}

/* 
 * Get a slice from the image data.
 */
Islice *midasGetSlice(MidasView *vw, int sliceType)
{
  int xformed;
  switch(sliceType){
  case MIDAS_SLICE_CURRENT:
    return(getXformSlice(vw, vw->cz, 0, &xformed));
  case MIDAS_SLICE_PREVIOUS:
    return(getXformSlice(vw, vw->refz, 0, &xformed));
  case MIDAS_SLICE_REFERENCE:
    return(vw->ref);
  case MIDAS_SLICE_OCURRENT:
    return(getRawSlice(vw, vw->cz));
  case MIDAS_SLICE_OPREVIOUS:
    return(getRawSlice(vw, vw->refz));
  }
  return(NULL);
}

/*
 * Get the image data point from appropriate previous image for the mode
 */
unsigned char *midasGetPrevImage(MidasView *vw)
{
  int refz = vw->refz;

  Islice *prevSlice;
  unsigned char *prevImageData = NULL;

  /* set previous image data pointer. */
  switch(vw->xtype){
  case XTYPE_XF:
  case XTYPE_MONT:
    prevSlice = midasGetSlice(vw, vw->rotMode ? MIDAS_SLICE_PREVIOUS : 
                              MIDAS_SLICE_OPREVIOUS);
    if (prevSlice)
      prevImageData = prevSlice->data.b;
    break;
  case XTYPE_XREF:
    if (vw->ref)
      prevImageData = vw->ref->data.b;
    refz = vw->zsize;
    break;
  case XTYPE_XG:
  default:
    prevSlice = midasGetSlice(vw, MIDAS_SLICE_PREVIOUS);
    if (prevSlice)
      prevImageData = prevSlice->data.b;
    break;
  }
  return prevImageData;
}


/* flush the cache of any transformed images */
void flush_xformed(MidasView *vw)
{
  int k;
  for (k = 0; k < vw->cachesize; k++)
    if (vw->cache[k].xformed) {
      vw->cache[k].zval = -1;
      vw->cache[k].used = -1;
    }
}

void midasGetSize(MidasView *vw, int *xs, int *ys)
{
  if (!vw){
    *xs = *ys = 0;
  }
  *xs = vw->xsize;
  *ys = vw->ysize;
  return;
}


/* Faster then generic transformation.
 * Translate the image by xt and yt.
 */
int translate_slice(MidasView *vw, int xt, int yt)
{
  unsigned char *tptr, *cptr;
  int i, j, yofs;
  int xfd;
  unsigned char mean;
  int xsize, ysize, xysize;
  Islice *curSlice = getXformSlice(vw, MIDAS_SLICE_CURRENT, 1, &xfd);

  if (!curSlice) return(-1);

  /* If slice is already transformed, return now */
  if (xfd)
    return 0;

  midasGetSize(vw, &xsize, &ysize);
  xysize = xsize * ysize;

  /* protect against big shifts */
  if (xt >= xsize)
    xt = xsize - 1;
  if (xt <= -xsize)
    xt = 1 - xsize;

  if (yt >= ysize)
    yt = ysize - 1;
  if (yt <= -ysize)
    yt = 1 - ysize;

  mean = (unsigned char)curSlice->mean;
  cptr = curSlice->data.b;
  tptr = cptr;
     
  yofs = xsize * yt;
  if (yt > 0){
    cptr += xysize - 1;
    tptr = cptr - yofs;
    for(i = xysize; i > yofs; i--, cptr--, tptr--)
      *cptr = *tptr;
    cptr = curSlice->data.b;
    for (i = 0; i < yofs; i++, cptr++)
      *cptr = mean;

  }
  if (yt < 0){
    yofs = -yofs;
    tptr += yofs;
    for(i = yofs; i < xysize; i++, cptr++, tptr++)
      *cptr = *tptr;
    for (i = 0; i < yofs; i++, cptr++)
      *cptr = mean;
  }
     
  cptr = curSlice->data.b;
  tptr = cptr;

  if (xt > 0){
    for(j = 0; j <  ysize; j++) {
      yofs = j * xsize;
      for(i = xsize - 1; i >= xt; i--)
        cptr[i + yofs] = cptr[i + yofs - xt];
      for (i = 0; i < xt; i++)
        cptr[i + yofs] = mean;
    }
  }
  if (xt < 0){

    for(j = 0; j < ysize; j++) {
      yofs = j * xsize;
      for(i = 0; i < xsize + xt; i++)
        cptr[i + yofs] = cptr[i + yofs - xt] ;
      for (i = xsize + xt; i < xsize; i++)
        cptr[i + yofs] = mean;
    }
  }
  return(0);
}

/*
 * Transform a slice by the given transformation, possibly including warping
 */
int midas_transform(int zval, Islice *slin, Islice *sout, float *trmat, int izwarp)
{
  int i, j, k, l, index;
  int ix, iy, ixgrid, iygrid, indy[2], indx[2], ixlim[2], iylim[2];
  int xsize, ysize;
  float *mat;
  float xbase, ybase;
  float xdx, xdy, ydx, ydy;
  float x, y, xline, yline;
  unsigned char umean;
  int nxLim, nyLim, box, timeOutput = 0;
  int ixStart, ixEnd, nxGrid, nyGrid, ixgStart, ixgEnd, iygStart, iygEnd;
  float xRight, xLeft, xEnd, xInterval, yInterval, xstep, ystep, xStart, yStart;
  float fx, fy, gridfy, xbox, ybox, xmap[2][2], ymap[2][2], xlim[2], ylim[2];
  bool allIn;
  unsigned char *buf;
  float xcen = VW->xsize / 2.;
  float ycen = VW->ysize / 2.;
  double wallstart, arrStart;
  const char *messages[3] = {"Error setting up grid from control points", 
                             "Failed to get memory for warping grid",
                             "Failed to get interpolated warping grid"};

  mat = tramat_inverse(trmat);
  if (!mat)
    return(-1);
  xdx = mat[0];
  xdy = mat[1];
  ydx = mat[3];
  ydy = mat[4];
  xbase = mat[6] + xcen - xcen * xdx - ycen * ydx;
  ybase = mat[7] + ycen - xcen * xdy - ycen * ydy;

  xsize = slin->xsize;
  ysize = slin->ysize;
  umean = (unsigned char)slin->mean;
  wallstart = wallTime();

  // If warping, get the grid
  if (izwarp >= 0) {
    index = fillWarpingGrid(izwarp, &nxGrid, &nyGrid, &xStart, &yStart, &xInterval,
                           &yInterval);
    if (timeOutput)
      printf("fillWarpingGrid after %.3f\n", 1000. * (wallTime()- wallstart));
    if (index) {
      midas_error(messages[index - 1], "No warping was done", 0);
      izwarp = -1;
    } else {
      // convert the grid into loaded image coordinates
      xStart /= VW->warpScale;
      yStart /= VW->warpScale;
      xInterval /= VW->warpScale;
      yInterval /= VW->warpScale;
      for (i = 0; i < nxGrid * nyGrid; i++) {
        VW->gridDx[i] /= VW->warpScale;
        VW->gridDy[i] /= VW->warpScale;
      }
    }
    if (timeOutput)
      printf("scaled grid after %.3f\n", 1000. * (wallTime()- wallstart));
  }
  
  // All set to do warping
  if (izwarp >= 0) {

    // Evaluate whether only some of the image needs to be transformed
    ixgStart = 0;
    ixgEnd = nxGrid;
    iygStart = 0;
    iygEnd = nyGrid;
    if (zval == VW->lastWarpedZ && nxGrid == VW->lastNxGrid && nyGrid == VW->lastNyGrid &&
        fabs((double)(xStart - VW->lastXstart)) < 1.e-3 && 
        fabs((double)(yStart - VW->lastYstart)) < 1.e-3 && 
        fabs((double)(xInterval - VW->lastXinterv)) < 1.e-3 && 
        fabs((double)(yInterval - VW->lastYinterv)) < 1.e-3 && 
        mat[0] == VW->lastMat[0] && mat[1] == VW->lastMat[1] && 
        mat[3] == VW->lastMat[3] && mat[4] == VW->lastMat[4] && 
        mat[6] == VW->lastMat[6] && mat[7] == VW->lastMat[7]) {
      
      // Find domain of grid mismatch and set limits to do intervals outside edge of that
      ixgStart = nxGrid;
      ixgEnd = 0;
      iygStart = nyGrid;
      iygEnd = 0;
      for (iy = 0; iy < nyGrid; iy++) {
        for (ix = 0; ix < nxGrid; ix++) {
          i = ix + iy * nxGrid;
          if (VW->gridDx[i] != VW->lastGridDx[i] || VW->gridDy[i] != VW->lastGridDy[i]) {
            //if (fabs((double)VW->gridDx[i] - VW->lastGridDx[i]) > 1.e-3 || fabs((double)VW->gridDy[i] - VW->lastGridDy[i]) > 1.e-3) {
            ixgStart = B3DMIN(ixgStart, ix);
            ixgEnd = B3DMAX(ixgEnd, ix + 1);
            iygStart = B3DMIN(iygStart, iy);
            iygEnd = B3DMAX(iygEnd, iy + 1);
          }
        }
      }
    }
    arrStart = wallTime();
    if (timeOutput)
      printStderr("total grid time %.3f\n", 1000. * (wallTime()- wallstart));

#pragma omp parallel for                                                \
  shared(xdx, xdy, ydx, ydy, VW, xbase, ybase, umean, nxLim, nyLim, xsize, ysize) \
  shared(yStart, yInterval, xStart, xInterval, ixgStart, ixgEnd, iygStart, iygEnd) \
  shared(nyGrid, nxGrid)                                                        \
  private(j, x, y, buf, i, ix, iy, index) \
  private(fx, fy, iygrid, ixgrid, indx, indy, xlim, ylim, ixlim, iylim, allIn) \
  private(xmap, ymap, xbox, ybox, gridfy, xstep, ystep)
    for (iygrid = iygStart; iygrid <= iygEnd; iygrid++) {

      // Get indexes of grid points in Y on low and high side of block, and y coordinates
      indy[0] = B3DMAX(0, iygrid - 1);
      indy[1] = B3DMIN(nyGrid - 1, iygrid);
      ylim[0] = yStart + yInterval * (iygrid - 1);
      ylim[1] = yStart + yInterval * iygrid;
      iylim[0] = (int)ceil((double)ylim[0]);
      if (iygrid == 0) {
        iylim[0] = 0;
        ylim[0] = B3DMIN(0., ylim[0]);
      }
      iylim[1] = (int)ceil((double)ylim[1]) - 1;
      if (iygrid == nyGrid) {
        iylim[1] = ysize - 1;
        ylim[1] = B3DMAX(ysize - 0.999, ylim[1]);
      }

      // Loop on X blocks, get indexes and limiting coordinates in X
      for (ixgrid = ixgStart; ixgrid <= ixgEnd; ixgrid++) {
        indx[0] = B3DMAX(0, ixgrid - 1);
        indx[1] = B3DMIN(nxGrid - 1, ixgrid);
        xlim[0] = xStart + xInterval * (ixgrid - 1);
        xlim[1] = xStart + xInterval * ixgrid;
        ixlim[0] = (int)ceil((double)xlim[0]);
        if (ixgrid == 0) {
          ixlim[0] = 0;
          xlim[0] = B3DMIN(0., xlim[0]);
        }
        ixlim[1] = (int)ceil((double)xlim[1]) - 1;
        if (ixgrid == nxGrid) {
          ixlim[1] = xsize - 1;
          xlim[1] = B3DMAX(xsize - 0.999, xlim[1]);
        }
      
        // Evaluate mapping of each corner point and see if inside 
        allIn = true;
        //printf("block %d %d:", ixgrid, iygrid);
        for (iy = 0; iy < 2; iy++) {
          for (ix = 0; ix < 2; ix++) {
            index = indx[ix] + nxGrid * indy[iy];
            x = xlim[ix] + VW->gridDx[index];
            y = ylim[iy] + VW->gridDy[index];
            //printf("  %.1f, %.1f", VW->gridDx[index], VW->gridDy[index]);
            xmap[ix][iy] = x * xdx + y * ydx + xbase;
            ymap[ix][iy] = x * xdy + y * ydy + ybase;
            if (xmap[ix][iy] < 0. || xmap[ix][iy] >= xsize - 1 || ymap[ix][iy] < 0. || 
                ymap[ix][iy] >= ysize - 1)
              allIn = false;
          }
        }
        //puts("");
        xbox = xlim[1] - xlim[0];
        ybox = ylim[1] - ylim[0];

        // Loop on lines, set up start coordinate and steps
        for (j = iylim[0]; j <= iylim[1]; j++) {
          gridfy = (j - ylim[0]) / ybox;
          xstep = ((1. - gridfy) * (xmap[1][0] - xmap[0][0]) + 
                   gridfy * (xmap[1][1] - xmap[0][1])) / xbox;
          ystep = ((1. - gridfy) * (ymap[1][0] - ymap[0][0]) + 
                   gridfy * (ymap[1][1] - ymap[0][1])) / xbox;
          x = (1. - gridfy) * xmap[0][0] + gridfy * xmap[0][1] + 
            xstep * (ixlim[0] - xlim[0]);
          y = (1. - gridfy) * ymap[0][0] + gridfy * ymap[0][1] +
            ystep * (ixlim[0] - xlim[0]);

          // Loop across lines in different cases
          buf = &sout->data.b[ixlim[0] + j * xsize];
          if (VW->fastInterp) {

            /* nearest neighbor, add 0.5 for nearest int */
            x += 0.5;
            y += 0.5;
            if (allIn) {
              for (i = ixlim[0]; i <= ixlim[1]; i++, y += ystep, x += xstep){
                ix = (int)x; 
                iy = (int)y;
                index = ix + (iy * xsize);
                *buf++ = slin->data.b[index];
              }
            } else {
              for (i = ixlim[0]; i <= ixlim[1]; i++, y += ystep, x += xstep){
                ix = (int)x; 
                iy = (int)y;
                if (ix >= 0 && ix < xsize && iy >= 0 && iy < ysize) {
                  index = ix + (iy * xsize);
                  *buf++ = slin->data.b[index];
                } else
                  *buf++ = umean;
              }
            }

          } else {

            // Linear interpolation
            if (allIn) {
              for (i = ixlim[0]; i <= ixlim[1]; i++, y += ystep, x += xstep){
                ix = (int)x; 
                iy = (int)y;
                fx = x - ix;
                fy = y - iy;
                index = ix + (iy * xsize);
                *buf++ = (unsigned char)((1. - fy) * ((1. - fx) *slin->data.b[index] +
                                                      fx * slin->data.b[index + 1]) +
                                         fy * ((1. - fx) *slin->data.b[index + xsize] +
                                               fx * slin->data.b[index + xsize + 1]));
              }
            } else {
              for (i = ixlim[0]; i <= ixlim[1]; i++, y += ystep, x += xstep){
                ix = (int)x; 
                iy = (int)y;
                fx = x - ix;
                fy = y - iy;
                if (ix >= 0 && ix < xsize - 1  && iy >= 0 && iy < ysize - 1) {
                  index = ix + (iy * xsize);
                  *buf++ = (unsigned char)((1. - fy) * ((1. - fx) *slin->data.b[index] +
                                                        fx * slin->data.b[index + 1]) +
                                           fy * ((1. - fx) *slin->data.b[index + xsize] +
                                                 fx * slin->data.b[index + xsize + 1]));
                } else
                  *buf++ = umean;
              }
            }  
          }
        }
      }
    }
    // End of parallel for

    // Record parameters of this grid and copy the grid
    VW->lastWarpedZ = zval;
    tramat_copy(mat, VW->lastMat);
    VW->lastNxGrid = nxGrid;
    VW->lastNyGrid = nyGrid;
    VW->lastXstart = xStart;
    VW->lastYstart = yStart;
    VW->lastXinterv = xInterval;
    VW->lastYinterv = yInterval;
    if (VW->lastGridSize < nxGrid * nyGrid) {
      B3DFREE(VW->lastGridDx);
      B3DFREE(VW->lastGridDy);
      VW->lastGridSize = nxGrid * nyGrid;
      VW->lastGridDx = B3DMALLOC(float, VW->lastGridSize);
      VW->lastGridDy = B3DMALLOC(float, VW->lastGridSize);
      if (!VW->lastGridDx || !VW->lastGridDy) {
        VW->lastGridSize = 0;
        VW->lastWarpedZ = -1;
      }
    }
    if (VW->lastWarpedZ >= 0) {
      memcpy(VW->lastGridDx, VW->gridDx, nxGrid * nyGrid * sizeof(float));
      memcpy(VW->lastGridDy, VW->gridDy, nxGrid * nyGrid * sizeof(float));
    }
    if (timeOutput)
      printStderr("array fill time %.3f    limits %d %d %d %d\n",
             1000. * (wallTime()- arrStart), ixgStart, ixgEnd, iygStart, iygEnd);

  } else {

    // REGULAR TRANSFORMATION
    nxLim = xsize - 1;
    nyLim = ysize - 1;
    if (!VW->fastInterp) {
      nxLim = xsize - 2;
      nyLim = ysize - 2;
    }

#pragma omp parallel for                                                \
  shared(xdx, xdy, ydx, ydy, VW, xbase, ybase, umean, nxLim, nyLim, xsize, ysize) \
  private(j, x, y, xStart, xEnd, xLeft, xRight, ixEnd, ixStart, k, buf, i, ix, iy) \
  private(fx, fy, l, xline, yline, index)
    for (j = 0; j < ysize; j++) {
      xline = xbase + j * ydx;
      yline = ybase + j * ydy;

      /* compute constrained, safe coordinates to use */
      xStart = 0;
      xEnd = xsize - 1;

      /* get intersection with left and right sides unless vertical */
      if (xdx > 1.e-10 || xdx < -1.e-10) {
        xLeft = -xline / xdx;
        xRight = (nxLim - 0.5 - xline) / xdx;
        if (xLeft < xRight) {
          xStart = B3DMAX(xStart, xLeft);
          xEnd = B3DMIN(xEnd, xRight);
        } else {
          xStart = B3DMAX(xStart, xRight);
          xEnd = B3DMIN(xEnd, xLeft);
        }
      } else if (xline < 0 || xline >= nxLim - 0.5) {
        /* if vertical and outside limits, set up for fill */
        xStart = nxLim;
        xEnd = 1;
      }

      /* get intersection with bottom and top unless horizontal */
      if (xdy > 1.e-10 || xdy < -1.e-10) {
        xLeft = -yline / xdy;
        xRight = (nyLim - 0.5 - yline) / xdy;
        if (xLeft < xRight) {
          xStart = B3DMAX(xStart, xLeft);
          xEnd = B3DMIN(xEnd, xRight);
        } else {
          xStart = B3DMAX(xStart, xRight);
          xEnd = B3DMIN(xEnd, xLeft);
        }
      } else if (yline < 0 || yline >= nyLim - 0.5) {
        xStart = nxLim;
        xEnd = 1;
      }

      /* Limit these values before truncating because they can be > 2147... */
      xStart = B3DMIN(xStart, xsize + 10.);
      xEnd = B3DMAX(xEnd, -10.);

      /* truncate ending down, starting up */
      ixEnd = (int)xEnd;
      ixStart = nxLim + 1 - (int)(nxLim + 1. - xStart);

      /* If they are crossed, set up so fill does whole extent */
      if (ixStart > ixEnd) {
        ixStart = nxLim / 2;
        ixEnd = ixStart - 1;
      }

      if (ixEnd > nxLim)
        ixEnd = nxLim;

      /* Do the left and right fills */
      buf = &sout->data.b[j * xsize];
      for (i = 0; i < ixStart; i++)
        *buf++ = umean;
      buf = &sout->data.b[ixEnd + 1 + j * xsize];
      for (i = ixEnd + 1; i < xsize; i++)
        *buf++ = umean;

      if (VW->fastInterp) {

        /* nearest neighbor, add 0.5 for nearest int */
        xline += 0.5;
        yline += 0.5;
        buf = &sout->data.b[ixStart + j * xsize];
        for (i = ixStart; i <= ixEnd; i++) {
          ix = (int)(xline + i * xdx); 
          iy = (int)(yline + i * xdy);
          index = ix + (iy * xsize);
          *buf++ = slin->data.b[index];
        }
      } else {
        
        /* bilinear interpolation */
        buf = &sout->data.b[ixStart + j * xsize];
        for (i = ixStart; i <= ixEnd; i++) {
          x = xline + i * xdx; 
          y = yline + i * xdy;
          ix = (int)x;
          iy = (int)y;
          fx = x - ix;
          fy = y - iy;
          index = ix + (iy * xsize);
          *buf++ = (unsigned char)((1. - fy) * ((1. - fx) *slin->data.b[index] +
                                                fx * slin->data.b[index + 1]) +
                                   fy * ((1. - fx) *slin->data.b[index + xsize] +
                                         fx * slin->data.b[index + xsize + 1]));
        }
      }

    }
    // End of parallel for

    VW->lastWarpedZ = -1;
  }

  tramat_free(mat);
  if (timeOutput)
    printStderr("Total transform time %.3f\n", 1000. * (wallTime()- wallstart));
  return(0);
}

/*
 * Get a full warping grid with standardized spacing parameters
 */
int fillWarpingGrid(int iz, int *nxGrid, int *nyGrid, float *xStart, float *yStart,
                   float *xInterval, float *yInterval)
{
  int gridProd;
  //double wallstart = wallTime();

  // set up the grid size from point spacing
  if (gridSizeFromSpacing(iz, -1., -1., 1))
    return 1;
  // printf("spacing time %.3f\n", 1000. * (wallTime()- wallstart));

  // get the size and make sure array is big enough
  getWarpGridSize(iz, nxGrid, nyGrid, &gridProd);
  if (VW->gridSize < gridProd) {
    B3DFREE(VW->gridDx);
    B3DFREE(VW->gridDy);
    VW->gridDx = B3DMALLOC(float, gridProd);
    VW->gridDy = B3DMALLOC(float, gridProd);
    VW->gridSize = gridProd;
    if (!VW->gridDx || !VW->gridDy) {
      B3DFREE(VW->gridDx);
      B3DFREE(VW->gridDy);
      VW->gridSize = 0;
      return 2;
    }
  }

  // Get the warp grid
  if (getWarpGrid(iz, nxGrid, nyGrid, xStart, yStart, xInterval, yInterval, VW->gridDx,
                  VW->gridDy, *nxGrid))
    return 3;
  /*for (int iy = 0; iy < *nyGrid; iy++) {
    for (int ix = 0; ix < *nxGrid; ix++)
      printf("   %.1f,%1.f", VW->gridDx[ix + iy * *nxGrid], VW->gridDy[ix+iy * *nxGrid]);
    printf("\n");
    }*/
      
  return 0;
}


/*
 * Matrix calculation functions.  
 * 6/9/11: modified to use new libcfshr functions instead of functions that
 * generically handled 3x3 matrices
 */

float *tramat_create(void)
{
  float *mat;

  mat = (float *)malloc(9 * sizeof(float));
  if (!mat)
    return(NULL);
  tramat_idmat(mat);
  return(mat);
}                    

void tramat_free(float *mat)
{
  if (mat)
    free(mat);
}

int tramat_idmat(float *mat)
{
  xfUnit(mat, 1., 3);
  return 0;
}

int tramat_copy(float *fmat, float *tomat)
{
  xfCopy(fmat, 3, tomat, 3);
  return 0;
}

int tramat_multiply(float *m2, float *m1, float *out)
{
  xfMult(m2, m1, out, 3);
  return 0;
}

float *tramat_inverse(float *mat)
{
  float *imat;

  imat = tramat_create();
  xfInvert(mat, imat, 3);
  return(imat);
}

int tramat_translate(float *mat, double x, double y)
{
  mat[6] += x;
  mat[7] += y;
  return(0);
}

int tramat_scale(float *mat, double x, double y)
{
  float *smat, *omat;

  smat = tramat_create();
  omat = tramat_create();

  smat[0] = (float)x;
  smat[4] = (float)y;
  tramat_multiply(mat, smat, omat);
  tramat_copy(omat, mat);
  tramat_free(omat);
  tramat_free(smat);
  return 0;
}

int tramat_rot(float *mat, double angle)
{
  double cosa, sina;
  float *rmat, *omat;

  rmat = tramat_create();
  omat = tramat_create();

  angle *= 0.017453293;
  cosa = cos(angle);
  sina = sin(angle);

  rmat[0] = (float)cosa;
  rmat[1] = (float)sina;
  rmat[3] = (float)-sina;
  rmat[4] = (float)cosa;

  tramat_multiply(mat, rmat, omat);
  tramat_copy(omat, mat);
  tramat_free(omat);
  tramat_free(rmat);
  return 0;
}


/* This now needs a center defined */
/*int tramat_getxy(float *mat, float *x, float *y)
{
  float ox = *x, oy = *y;

  *x = (mat[0] * ox) + (mat[3] * oy) + mat[6]; 
  *y = (mat[1] * ox) + (mat[4] * oy) + mat[7]; 
  return 0;
} */

#ifdef TRAMAT_TEST
static int tramat_testin(float *mat, float *imat)
{
  float *idmat;
  int error = 0;

  idmat = tramat_create();
  tramat_multiply(mat, imat, idmat);

  if (idmat[0] != 1)
    error++;
  if (idmat[1] != 0)
    error++;
  if (idmat[2] != 0)
    error++;
  if (idmat[3] != 0)
    error++;
  if (idmat[4] != 1)
    error++;
  if (idmat[5] != 0)
    error++;
  if (idmat[6] != 0)
    error++;
  if (idmat[7] != 0)
    error++;
  if (idmat[8] != 1)
    error++;

  free(idmat);

  if (error){
    fprintf(stderr, "matrix inversion error %d\n", error);
          
    fprintf(stderr, "matrix = %g %g %g\n", mat[0], mat[1], mat[2]);
    fprintf(stderr, "         %g %g %g\n", mat[3], mat[4], mat[5]); 
    fprintf(stderr, "         %g %g %g\n", mat[6], mat[7], mat[8]); 

    fprintf(stderr, "inv mat = %g %g %g\n", imat[0], imat[1], imat[2]);
    fprintf(stderr, "          %g %g %g\n", imat[3], imat[4], imat[5]);
    fprintf(stderr, "          %g %g %g\n", imat[6], imat[7], imat[8]);

    fprintf(stderr, "id mat = %g %g %g\n", idmat[0], idmat[1], idmat[2]);
    fprintf(stderr, "         %g %g %g\n", idmat[3], idmat[4], idmat[5]);
    fprintf(stderr, "         %g %g %g\n", idmat[6], idmat[7], idmat[8]);
          
  }
  return(error);
}
#endif

/* Rotate a transform to new global rotation */
void rotate_transform(float *mat, double angle)
{
  float rmat[9], pmat[9];
  tramat_idmat(rmat);
  tramat_rot(rmat, -angle);
  tramat_multiply(rmat, mat, pmat);
  tramat_rot(pmat, angle); 
  tramat_copy(pmat, mat);
}

void rotate_all_transforms(MidasView *vw, double angle)
{
  int i;
  for (i = 0; i < vw->zsize; i++)
    rotate_transform(vw->tr[i].mat, angle);
}

/*
 * Apply stretch to X shift of transform, or take it away 
 */
void stretch_transform(MidasView *vw, float *mat, int index, 
                       int destretch)
{
  double angle, lastAngle, stretch;
  if (!index)
    return;
  lastAngle = (vw->tiltAngles[index-1] - vw->tiltOffset) * RADIANS_PER_DEGREE;
  angle = (vw->tiltAngles[index] - vw->tiltOffset) * RADIANS_PER_DEGREE;
  stretch = cos(lastAngle) / cos(angle);
  if (destretch)
    mat[6] /= stretch;
  else
    mat[6] *= stretch;
}

void stretch_all_transforms(MidasView *vw, int destretch)
{
  int i;
  printf("%stretching all\n", destretch? "Des" : "S");
  for (i = 0; i < vw->zsize; i++)
    stretch_transform(vw, vw->tr[i].mat, i, destretch);
}

void transform_model(const char *infname, const char *outfname, MidasView *vw)
{
  struct Mod_Model *model;
  int k;
     
  model = imodRead(infname);
  if (!model){
    dia_err("Error reading model.");
    return;
  }
  if (imodOpenFile(outfname, "wb", model)){
    dia_err("Error opening output.");
    return;
  }
     
  for(k = 0; k < vw->zsize; k++){
    imodel_transform_slice(model, vw->tr[k].mat, k);
  }
  imodWriteFile(model);
  imodCloseFile(model);
  dia_puts("Finished transforming model.");
  return;
}

/*
 * If there are between 1 and 3 control points, find the equivalent transformation,
 * add it to the current linear transform, and reduce the vectors to 0
 */
void reduceControlPoints(MidasView *vw)
{
  int iz = vw->numChunks ? vw->curChunk : vw->cz;
  float xCont[3], yCont[3], xVect[3], yVect[3];
  float *curmat = VW->tr[VW->cz].mat;
  float mat[9], tmat[9], dist, denom, dx, dy, dxv, dyv, mag, distmax;
  float samePtCrit = 3.;
  float colinearCrit = 3.;
  double theta;
  bool allZero = false;
  float *xControl, *yControl, *xVector, *yVector;
  int i, j, ifar, nControl;

  if (vw->curWarpFile < 0 || getNumWarpPoints(iz, &nControl))
    return;
  if (!nControl || nControl > 3 || getWarpPointArrays(iz, &xControl, &yControl, 
                                                 &xVector, &yVector))
    return;

  // Return if they are all zero to avoid numeric drift
  for (i = 0; i < nControl; i++)
    if (xVector[i] || yVector[i])
      break;
  if (i >= nControl)
    allZero = true;

  // Scale and center the coordinates
  tramat_idmat(mat);
  for (i = 0; i < nControl; i++) {
    xVect[i] = xVector[i] / vw->warpScale;
    yVect[i] = yVector[i] / vw->warpScale;
    xCont[i] = xControl[i] / vw->warpScale - vw->xsize / 2.;
    yCont[i] = yControl[i] / vw->warpScale - vw->ysize / 2.;
    //printf("%d %f %f %f %f\n", i, xCont[i], yCont[i], xVect[i], yVect[i]);
  }

  switch (nControl) {
  case 1:       // 1 POINT
    if (allZero)
      return;
    curmat[6] -= xVector[0];
    curmat[7] -= yVector[0];
    break;

  case 2:       // 2 POINTS
    dxv = xCont[0] + xVect[0] - (xCont[1] + xVect[1]);
    dyv = yCont[0] + yVect[0] - (yCont[1] + yVect[1]);
    dx = xCont[0] - xCont[1];
    dy = yCont[0] - yCont[1];
    denom = (float)sqrt((double)(dxv * dxv + dyv * dyv));
    dist = (float)sqrt((double)(dx * dx + dy * dy));
    //printf("%f %f %f %f %f %f\n", dx, dy, dist, dxv, dyv, denom);
    if (denom < samePtCrit || dist < samePtCrit) {
      removeWarpPoint(iz, 0);
      vw->curControl = 0;
      reduceControlPoints(vw);
      return;
    }
    if (allZero)
      return;
    mag = dist / denom;
    theta = atan2(dy, dx) - atan2(dyv, dxv);
    //printf("mag %f  theta %f\n", mag, theta);
    mat[0] = mag * cos(theta);
    mat[3] = -mag * sin(theta);
    mat[1] = -mat[3];
    mat[4] = mat[0];
    mat[6] = xCont[0] - mat[0] * (xCont[0] + xVect[0]) - mat[3] * (yCont[0] + yVect[0]);
    mat[7] = yCont[0] - mat[1] * (xCont[0] + xVect[0]) - mat[4] * (yCont[0] + yVect[0]);
    tramat_multiply(curmat, mat, tmat);
    tramat_copy(tmat, curmat);
    break;

  case 3:       // 3 POINTS
    
    // This is actually 2x the area of the triangle
    denom = determ3(xCont[0] + xVect[0], yCont[0] + yVect[0], 1.,
                    xCont[1] + xVect[1], yCont[1] + yVect[1], 1.,
                    xCont[2] + xVect[2], yCont[2] + yVect[2], 1.);
    //printf("denom %f\n", denom);
    if (fabs((double)denom) < colinearCrit) {

      // if collinear, find farthest pair of points
      distmax = -1.;
      for (i = 0; i < 3; i++) {
        j = (i + 1) % 3;
        dx = xCont[i] + xVect[i] - (xCont[j] + xVect[j]);
        dy = yCont[i] + yVect[i] - (yCont[j] + yVect[j]);
        dist = dx * dx + dy * dy;
        if (dist > distmax) {
          distmax = dist;
          ifar = i;
        }
      }
      
      // remove the other point then call this routine with remaining two
      removeWarpPoint(iz, (ifar + 2) %3);
      vw->curControl = B3DMIN(1, B3DMAX(0, vw->curControl));
      reduceControlPoints(vw);
      return;
    }
    if (allZero)
      return;

    // Find the transformation and modify current one
    mat[0] = determ3(xCont[0], yCont[0] + yVect[0], 1.,
                     xCont[1], yCont[1] + yVect[1], 1.,
                     xCont[2], yCont[2] + yVect[2], 1.) / denom;
    mat[3] = determ3(xCont[0] + xVect[0], xCont[0], 1.,
                     xCont[1] + xVect[1], xCont[1], 1.,
                     xCont[2] + xVect[2], xCont[2], 1.) / denom;
    mat[6] = determ3(xCont[0] + xVect[0], yCont[0] + yVect[0], xCont[0],
                     xCont[1] + xVect[1], yCont[1] + yVect[1], xCont[1],
                     xCont[2] + xVect[2], yCont[2] + yVect[2], xCont[2]) / denom;
    mat[1] = determ3(yCont[0], yCont[0] + yVect[0], 1.,
                     yCont[1], yCont[1] + yVect[1], 1.,
                     yCont[2], yCont[2] + yVect[2], 1.) / denom;
    mat[4] = determ3(xCont[0] + xVect[0], yCont[0], 1.,
                     xCont[1] + xVect[1], yCont[1], 1.,
                     xCont[2] + xVect[2], yCont[2], 1.) / denom;
    mat[7] = determ3(xCont[0] + xVect[0], yCont[0] + yVect[0], yCont[0],
                     xCont[1] + xVect[1], yCont[1] + yVect[1], yCont[1],
                     xCont[2] + xVect[2], yCont[2] + yVect[2], yCont[2]) / denom;
    tramat_multiply(curmat, mat, tmat);
    tramat_copy(tmat, curmat);
    break;
  }

  // Zero the vectors
  for (i = 0; i < nControl; i++)
    xVector[i] = yVector[i] = 0.;

  vw->midasSlots->synchronizeChunk(vw->cz);
  vw->midasSlots->update_parameters();
  vw->changed = 1;
}

/* Modify control points if underlying transform changes.  This was great for testing
   but pointless for users to do */
void adjustControlPoints(MidasView *vw)
{
  int iz = vw->numChunks ? vw->curChunk : vw->cz;
  float *xControl, *yControl, *xVector, *yVector;
  float *oldMat, *newMat, denom, ee, ff, xt, yt, xcen, ycen, xcvcen, ycvcen;
  int nControl, i;
  if (iz >= 0)
    return;
  if (vw->curWarpFile < 0 || iz >= vw->warpNz || getNumWarpPoints(iz, &nControl) ||
      nControl < 4 || getWarpPointArrays(iz, &xControl, &yControl, &xVector, &yVector))
    return;

  // Take inverses of old and new matrices; the old matrix was saved by call to 
  // getChangeLimits
  newMat = tramat_inverse(vw->tr[vw->cz].mat);
  oldMat = tramat_inverse(vw->oldMat);
  denom = newMat[0] * newMat[4] - newMat[1] * newMat[3];
  xcen = vw->xsize / 2.;
  ycen = vw->ysize / 2.;
  for (i = 0; i < nControl; i++) {

    // Solve for vector that gives same final position
    xcvcen = xControl[i] + xVector[i] - xcen;
    ycvcen = yControl[i] + yVector[i] - ycen;
    ee = oldMat[0] * xcvcen + oldMat[3] * ycvcen + oldMat[6] - 
      (newMat[0] * (xControl[i] -xcen) + newMat[3] * (yControl[i] - ycen) + newMat[6]);
    ff = oldMat[1] * xcvcen + oldMat[4] * ycvcen + oldMat[7] - 
      (newMat[1] * (xControl[i] -xcen) + newMat[4] * (yControl[i] - ycen) + newMat[7]);
    xt = xVector[i];
    yt = yVector[i];
    xVector[i] = ee * newMat[4] - ff * newMat[3];
    yVector[i] = newMat[0] * ff - newMat[1] * ee;
    printf("%d  %.1f, %.1f  to  %.1f, %.1f\n", i, xt, yt, xVector[i], yVector[i]);
  }
  free(newMat);
  free(oldMat);
}


///////////////////////////////////////////////////
// MONTAGE RELATED ROUTINES

/* Check the list of piece coordinates for regularity and determine the 
   the number of pieces in the dimension and the overlap */
static void checklist(int *xpclist, int npclist, int nxframe, int *minxpiece,
                      int *nxpieces, int *nxoverlap)
{
  int minxdiff=100000;
  int maxxpiece=-100000;
  int i, ixdiff, ixpc;

  /* get min and max of piece coordinates */

  *minxpiece=100000;
  /* DNM 1/15/02: in all three loops, change starting counter from 1 to 0 */
  for (i = 0; i < npclist; i++) {
    if (*minxpiece > xpclist[i])
      *minxpiece = xpclist[i];
    if (maxxpiece < xpclist[i])
      maxxpiece = xpclist[i];
  }


  /* get difference from minimum; keep track of minimum distance */

  for (i = 0; i < npclist; i++) {
    ixdiff = xpclist[i] - *minxpiece;
    if (ixdiff > 0 && minxdiff > ixdiff) 
      minxdiff = ixdiff;
  }

  /*  now check and make sure all differences are multiples of minimum, but
      if there were no non-zero differences, return 1 piece, 0 overlap */

  if (minxdiff == 100000) {
    *nxpieces = 1;
    *nxoverlap = 0;
  } else {
    *nxpieces = -1;
    if (minxdiff > nxframe)
      return;
    for (i = 0; i < npclist; i++) {

      ixdiff = xpclist[i] - *minxpiece;
      if(ixdiff % minxdiff) {
        *nxpieces = -1;
        return;
      }
      ixpc = ixdiff / minxdiff + 1;
      if (*nxpieces < ixpc)
        *nxpieces = ixpc;
    }
    *nxoverlap = nxframe - minxdiff;
  }
}

int includedEdge(int mapind, int xory)
{
  int ipc = VW->montmap[mapind];
  int ned = -1;
  if (ipc >= 0)
    ned = VW->edgeupper[2 * ipc + xory];
  if (ned >= 0 && !(VW->excludeSkipped && VW->skippedEdge[2 * ned + xory]))
    return 1;
  return 0;
}

int nearest_edge(MidasView *vw, int z, int xory, int edgeno, 
                 int direction, int *edgeind)
{
  int base = (z - vw->minzpiece) * vw->nxpieces * vw->nypieces;
  int edge, xmult, ymult, nacross, ind;
  int maxbelow = -1;
  int minabove = vw->maxedge[xory] + 1;

  /* Set up  depending on direction */
  if (xory) {
    nacross = vw->nypieces - 1;
    ymult = 1;
    xmult = vw->nxpieces;
  } else {
    nacross = vw->nxpieces - 1;
    xmult = 1;
    ymult = vw->nxpieces;
  }
     
          
  for (edge = 1; edge <= vw->maxedge[xory]; edge++) {
    ind = ((edge - 1) % nacross) * xmult + 
      ((edge - 1) / nacross) * ymult + base;
    if (includedEdge(ind, xory)) {

      /* If there is an edge, update maxbelow if below the target 
         and keep going; set both max and min if on target and break,
         or set the minabove and break if past the target */
      if (edge < edgeno)
        maxbelow = edge;
      else if (edge == edgeno) {
        maxbelow = edge;
        minabove = edge;
        break;
      } else {
        minabove = edge;
        break;
      }
    }
  }

  if (maxbelow < 0 && minabove > vw->maxedge[xory])
    return 0;

  /* if direction -, use the max below target if one was found;
     if direction +, use the min above target if one was found;
     otherwise, use the one that is closest to target */
  if (direction < 0) {
    if (maxbelow < 0)
      return 0;
    edge = maxbelow;
  } else if (direction > 0) {
    if (minabove > vw->maxedge[xory])
      return 0;
    edge = minabove;
  } else {
    if (maxbelow < 0)
      edge = minabove;
    else if (minabove > vw->maxedge[xory])
      edge = maxbelow;
    else {
      edge = maxbelow;
      if (edgeno - maxbelow > minabove - edgeno)
        edge = minabove;
    }
  }
  ind = ((edge - 1) % nacross) * xmult + 
    ((edge - 1) / nacross) * ymult + base;
  *edgeind = vw->edgeupper[xory + 2 * vw->montmap[ind]];
  return edge;
}
     
int nearest_section(MidasView *vw, int sect, int direction)
{
  int edge, ind;
  int maxbelow = vw->minzpiece - 1;
  int minabove = vw->maxzpiece + 1;
  int z;

  for (z = vw->minzpiece; z <= vw->maxzpiece; z++) {
    edge = nearest_edge(vw, z, vw->xory, 1, 0, &ind);
    if (!edge)
      edge = nearest_edge(vw, z, 1 - vw->xory, 1, 0, &ind);
    if (edge) {
      if (z < sect)
        maxbelow = z;
      else if (z == sect) {
        maxbelow = z;
        minabove = z;
        break;
      } else {
        minabove = z;
        break;
      }
    }
  }


  /* if direction -, use the max below target if one was found;
     if direction +, use the min above target if one was found;
     otherwise, use the one that is closest to target if two were found,
     or the one if one was found */
  if (direction < 0) {
    return maxbelow;
  }
  if (direction > 0) {
    if (minabove > vw->maxzpiece)
      return vw->minzpiece - 1;
    return minabove;
  }
  if (maxbelow < vw->minzpiece)
    return minabove;
  if (minabove > vw->maxzpiece)
    return maxbelow;
  if (sect - maxbelow > minabove - sect)
    return minabove;
  return maxbelow;
}
               
/* Sets up current and reference piece # and maintains the transforms
   properly for the displacements of the current edge */
void set_mont_pieces(MidasView *vw)
{
  int ind = vw->edgeind * 2 + vw->xory;
  vw->cz = vw->pieceupper[ind];
  vw->refz = vw->piecelower[ind];
  vw->tr[vw->cz].mat[6] = vw->edgedx[ind];
  vw->tr[vw->cz].mat[7] = vw->edgedy[ind];
  if (vw->xory)
    vw->tr[vw->cz].mat[7] += vw->ysize - 
      (float)(vw->nyoverlap - vw->hin->ny % vw->binning) / vw->binning;
  else
    vw->tr[vw->cz].mat[6] += vw->xsize - 
      (float)(vw->nxoverlap - vw->hin->nx % vw->binning) / vw->binning;
}

static int lowerEdgeIfIncluded(int ipc, int ixy)
{
  int ind = VW->edgelower[2 * ipc + ixy];
  if (ind >= 0 && !(VW->excludeSkipped && VW->skippedEdge[2 * ind + ixy]))
    return ind;
  return -1;
}

static int upperEdgeIfIncluded(int ipc, int ixy)
{
  int ind = VW->edgeupper[2 * ipc + ixy];
  if (ind >= 0 && !(VW->excludeSkipped && VW->skippedEdge[2 * ind + ixy]))
    return ind;
  return -1;
}

// Check if a piece is already assigned a type, add to list if not,
// return 1 if the new piece has different type from old one
static int checkPathList(MidasView *vw, int oldpc, int newpc, int &numOnList)
{
      // If this piece already has a type and it is different from the one
      // one we are looking at, then there is path
  if (vw->leaveType[newpc]) {
    if (vw->leaveType[newpc] != vw->leaveType[oldpc])
      return 1;
  } else {

    // Otherwise, assign type and add to look list
    vw->leaveType[newpc] = vw->leaveType[oldpc];
    vw->pathList[numOnList++] = newpc;
  }
  return 0;
}

// Determine if two pieces on either side of edge being left out are not 
// connected by any other path
static int piecesNotConnected(MidasView *vw, int leavind)
{
  int i, edge, ipc, nlist = 2;
  int lookInd = 0;
  vw->pathList[0] = vw->piecelower[leavind];
  vw->pathList[1] = vw->pieceupper[leavind];
  for (i = 0; i < vw->zsize; i++)
    vw->leaveType[i] = 0;
  vw->leaveType[vw->pathList[0]] = 1;
  vw->leaveType[vw->pathList[1]] = 2;
  while (lookInd < nlist) {
    ipc = vw->pathList[lookInd];

    // Check each edge, if any connects to a piece already assigned the other
    // type, then they are connected
    edge = lowerEdgeIfIncluded(ipc, 0);
    if (edge >= 0 && 2 * edge != leavind && 
        checkPathList(vw, ipc, vw->piecelower[2 * edge], nlist))
      return 0;
    edge = lowerEdgeIfIncluded(ipc, 1);
    if (edge >= 0 && 2 * edge + 1 != leavind && 
        checkPathList(vw, ipc, vw->piecelower[2*edge + 1], nlist))
      return 0;
    edge = upperEdgeIfIncluded(ipc, 0);
    if (edge >= 0 && 2 * edge != leavind && 
        checkPathList(vw, ipc, vw->pieceupper[2 * edge], nlist))
      return 0;
    edge = upperEdgeIfIncluded(ipc, 1);
    if (edge >= 0 && 2 * edge + 1 != leavind && 
        checkPathList(vw, ipc, vw->pieceupper[2*edge + 1], nlist))
      return 0;
    lookInd++;
  }
  return 1;
}

#define MAX_GLOBAL_VARS 25
#define LOCAL_BORDER 3
void find_best_shifts(MidasView *vw, int leaveout, int ntoperr,
                      float *meanerr, float *amax, int *indmax,
                      float *curerrx, float *curerry, int localonly)
{
  float *b = vw->fbs_b;
  int *indvar = vw->fbs_indvar;
  int *ivarpc = vw->fbs_ivarpc;
  int leavind, i, j, ivar, ipclo, ipc, nvar, ind, elx, ely, eux, euy;
  int nsum, edge, ixy;
  float adist, asum, adx, ady;

  /*  Unlike in bsubs.f the data coming in are the shifts to bring 
      the upper piece into alignment with the lower piece. */

  // Forget it if there is no connection except this edge
  leavind = -1;
  *curerrx = *curerry = *meanerr = 0.;
  if (leaveout) {
    leavind = vw->edgeind * 2 + vw->xory;
    if (piecesNotConnected(vw, leavind))
      return;
  }

  /* build list of variables */
  nvar=0;
  for (ipc = 0; ipc < vw->zsize; ipc++) {
    if(vw->zpclist[ipc] == vw->montcz) {
      indvar[ipc] = -1;
      ind = ipc * 2;
      elx = lowerEdgeIfIncluded(ipc, 0);
      ely = lowerEdgeIfIncluded(ipc, 1);
      eux = upperEdgeIfIncluded(ipc, 0);
      euy = upperEdgeIfIncluded(ipc, 1);
      if ((elx > -1 && elx * 2 != leavind) ||
          (ely > -1 && ely * 2 + 1 != leavind) ||
          (eux > -1 && eux * 2 != leavind) ||
          (euy > -1 && euy * 2 + 1 != leavind)) {
        ivarpc[nvar] = ipc;
        indvar[ipc] = nvar++;
      }
    }
  }

  /* Set up for zero errors if not enough variables */
  for (i = 0; i < ntoperr; i++) {
    amax[i]=0.;
    indmax[i] = -1;
  }
  if (nvar < 2)
    return;

  if (nvar > MAX_GLOBAL_VARS * MAX_GLOBAL_VARS) {
    //double wallstart = wallTime();
    find_local_errors(vw, leaveout, ntoperr, meanerr, amax, indmax,
                      curerrx, curerry, localonly);
    //printf("local time %.3f\n", wallTime() - wallstart);
    return;
  }

  solve_for_shifts(vw, b, ivarpc, indvar, nvar, leavind);


  nsum=0;
  asum=0.;
  for (ivar = 0; ivar < nvar; ivar++) {
    ipc=ivarpc[ivar];

    for (ixy = 0; ixy < 2; ixy++) {
      edge = lowerEdgeIfIncluded(ipc, ixy);
      ind = edge * 2 + ixy;
      if (edge > -1) {
        ipclo = vw->piecelower[ind];
        adx = b[ivar * 2] - b[indvar[ipclo] * 2] - vw->edgedx[ind];
        ady = b[ivar * 2 + 1] - b[indvar[ipclo] * 2 + 1] - vw->edgedy[ind];
        adist = sqrt((double)(adx * adx + ady * ady));
        if (ind == vw->edgeind * 2 + vw->xory) {
          *curerrx = adx;
          *curerry = ady;
        }
        if (ind != leavind) {
          asum += adist;
          nsum++;
          for (i = 0; i < ntoperr; i++) {
            if (adist > amax[i]) {
              for (j = ntoperr - 1; j > i; j--) {
                amax[j] = amax[j - 1];
                indmax[j] = indmax[j - 1];
              }
              amax[i] = adist;
              indmax[i] = ind;
              break;
            }
          }
        }
      }
    }
  }

  if (nsum)
    asum /= nsum;
  *meanerr = asum;
}

/* #include <sys/types.h>
   #include <time.h> */

/* When there are too many variables to solve all at once in a reasonable
   time, this routine computes errors only in smaller patches */
void find_local_errors(MidasView *vw, int leaveout, int ntoperr,
                       float *meanerr, float *amax, int *indmax,
                       float *curerrx, float *curerry, int localonly)
{
  float *b = vw->fbs_b;
  int *indvar = vw->fbs_indvar;
  int *ivarpc = vw->fbs_ivarpc;
  int leavind, i, j, ivar, ipclo, ipc, nvar, ind, elx, ely, eux, euy;
  int nsum, edge, ixy, localind;
  float adist, adx, ady;
  double asum;
  int limvar = vw->nxpieces * vw->nypieces;
  int ix, iy, ixst, ixnd, ixstin, ixndin, iyst, iynd, iystin, iyndin;
  int nlong, nshort, ndivShort, ndivLong, divSizeShort, divSizeLong;
  int basesize, ndivX, ndivY, divSizeX, divSizeY, divX, divY, doarea;
  int shortsize;
     
  /* clock_t timeval1, timeval2;
     float elapsed;
     timeval1 = clock(); */

  nsum=0;
  asum=0.;

  /* COmpute only in local area around edge if either the localonly or the
     leaveout flag is set */
  localind = -1;
  if (localonly)
    localind = vw->edgeind * 2 + vw->xory;

  leavind = -1;
  if (leaveout) {
    leavind = vw->edgeind * 2 + vw->xory;
    localind = leavind;
  }

  /* determine how to chop the area into domains */
  if (vw->nxpieces < vw->nypieces) {
    nshort = vw->nxpieces;
    nlong = vw->nypieces;
  } else {
    nshort = vw->nypieces;
    nlong = vw->nxpieces;
  }

  /* if short size is small enough, don't cut it up; otherwise divide */
  /* use length minus one to account for overlap of one each time */
  ndivShort = 1;
  divSizeShort = nshort - 1;
  shortsize = nshort;
  if (nshort > MAX_GLOBAL_VARS) {
    basesize = MAX_GLOBAL_VARS - 2 * LOCAL_BORDER - 1;
    ndivShort = (nshort + basesize - 2) / basesize;
    divSizeShort = (nshort - 1) / ndivShort;
    shortsize = divSizeShort + 2 * LOCAL_BORDER + 1;
    if ((nshort - 1) % ndivShort)
      shortsize++;
  }

  /*   printf ("%d %d %d %d\n", basesize, ndivShort, divSizeShort, shortsize); */

  /* Now long size can have bigger divisions if short side is small */
  basesize = (MAX_GLOBAL_VARS * MAX_GLOBAL_VARS) / shortsize -
    2 * LOCAL_BORDER - 1;
  ndivLong = (nlong + basesize - 2) / basesize;
  divSizeLong = (nlong - 1) / ndivLong;
  /*   printf ("%d %d %d\n", basesize, ndivLong, divSizeLong); */

  ndivX = vw->nxpieces < vw->nypieces ? ndivShort : ndivLong;
  ndivY = vw->nxpieces > vw->nypieces ? ndivShort : ndivLong;
  divSizeX = vw->nxpieces < vw->nypieces ? divSizeShort : divSizeLong;
  divSizeY = vw->nxpieces > vw->nypieces ? divSizeShort : divSizeLong;

  /* Loop on the domains; set starting and ending pieces in X and Y
     Get outer pieces and inner ones that bound the edges whose errors
     will be computed */
  iystin = 0;
  for (divY = 0; divY < ndivY; divY++) {
    iyndin  = iystin + divSizeY;
    if (divY < (vw->nypieces - 1) % ndivY)
      iyndin++;
    iyst = iystin - LOCAL_BORDER;
    if (iyst < 0) 
      iyst = 0;
    iynd = iyndin + LOCAL_BORDER;
    if (iynd >= vw->nypieces)
      iynd = vw->nypieces - 1;

    ixstin = 0;
    for (divX = 0; divX < ndivX; divX++) {
      ixndin = ixstin + divSizeX;
      if (divX < (vw->nxpieces - 1) % ndivX)
        ixndin++;
      ixst = ixstin - LOCAL_BORDER;
      if (ixst < 0) 
        ixst = 0;
      ixnd = ixndin + LOCAL_BORDER;
      if (ixnd >= vw->nxpieces)
        ixnd = vw->nxpieces - 1;
               

      /* loop on the pieces, adding them as variables */
      nvar=0;
      doarea = 0;
      if (localind < 0)
        doarea = 1;
      for (ipc = 0; ipc < vw->zsize; ipc++)
        if(vw->zpclist[ipc] == vw->montcz)
          indvar[ipc] = -1;
      for (iy = iyst; iy <= iynd; iy++) {
        for (ix = ixst; ix <= ixnd; ix++) {
          ipc = vw->montmap[ix + iy * vw->nxpieces + vw->montcz * limvar];
          if (ipc < 0)
            continue;
          
          ind = ipc * 2;
          elx = lowerEdgeIfIncluded(ipc, 0);
          ely = lowerEdgeIfIncluded(ipc, 1);
          eux = upperEdgeIfIncluded(ipc, 0);
          euy = upperEdgeIfIncluded(ipc, 1);

          /* include a piece if one of its edges is included
             in the area being analyzed */
          if ((ix > ixst && elx > -1 && elx * 2 != leavind) ||
              (iy > iyst && ely > -1 && ely*2 + 1 != leavind) ||
              (ix < ixnd && eux > -1 && eux * 2 != leavind) ||
              (iy < iynd && euy > -1 && euy*2 + 1 != leavind)) {
            ivarpc[nvar] = ipc;
            indvar[ipc] = nvar++;
          }

          /* determine if one of these edges is the current
             edge and is included in area whose errors are
             being computed */
          if (localind > -1 &&
              ((ix > ixstin && elx > -1 && elx*2 == localind) ||
              (iy > iystin && ely > -1 && ely * 2 + 1 == localind) ||
              (ix < ixndin && eux > -1 && eux* 2 == localind) ||
               (iy < iyndin && euy > -1 && euy * 2 + 1 == localind)))
            doarea = 1;
        }
      }
      /*printf("X: %d %d %d %d %d   Y: %d %d %d %d %d   leave %d %d\n",
        divX, ixst, ixnd, ixstin, ixndin, divY, iyst, iynd, iystin, iyndin, leavind, doarea); */
      if (nvar >= 2 && doarea) {
        solve_for_shifts(vw, b, ivarpc, indvar, nvar, leavind);

        /* evaluate errors for ones in the inner area */
        for (iy = iystin; iy <= iyndin; iy++) {
          for (ix = ixstin; ix <= ixndin; ix++) {
            ipc = vw->montmap[ix + iy * vw->nxpieces + vw->montcz * limvar];
            if (ipc < 0 || indvar[ipc] < 0)
              continue;
            ivar = indvar[ipc];

            for (ixy = 0; ixy < 2; ixy++) {
              edge = lowerEdgeIfIncluded(ipc, ixy);
              ind = edge * 2 + ixy;
              /* evaluate if edge exists and, for an X edge, the piece is past
                 the first in X and is either not the last local piece in
                 Y or the very last piece in Y */
              if (edge > -1 && 
                  ((!ixy && ix > ixstin &&
                    (iy < iyndin || iynd == vw->nypieces - 1))
                   || (ixy && iy > iystin &&
                       (ix < ixndin || ixnd == vw->nxpieces -1)))){
                ipclo = vw->piecelower[ind];
                adx = b[ivar * 2] - b[indvar[ipclo]* 2] - vw->edgedx[ind];
                ady = b[ivar * 2 + 1] - 
                  b[indvar[ipclo] * 2 + 1] - vw->edgedy[ind];
                adist = sqrt((double)(adx * adx + ady * ady));
                if (ind == vw->edgeind * 2 + vw->xory){
                  *curerrx = adx;
                  *curerry = ady;
                }
                if (ind != leavind) {
                  asum += adist;
                  nsum++;
                  for (i = 0; i < ntoperr; i++) {
                    if (adist > amax[i]) {
                      for (j = ntoperr - 1; j > i; j--) {
                        amax[j] = amax[j - 1];
                        indmax[j] = indmax[j - 1];
                      }
                      amax[i] = adist;
                      indmax[i] = ind;
                      break;
                    }
                  }
                }
              }
            }
          }
        }

      }
      ixstin = ixndin;
    }
    iystin = iyndin;
  }
  if (nsum)
    asum /= nsum;
  *meanerr = asum;

  /*  printf("nsum = %d\n", nsum); */

  /* timeval2 = clock();
     elapsed = (float)(timeval2 - timeval1) / CLOCKS_PER_SEC;
     printf("%.3f\n", elapsed); */
}

/* Determine shifts for the given set of variables */
static void solve_for_shifts(MidasView *vw, float *b, 
                             int *ivarpc, int *indvar, int nvar,
                             int leavind)
{
  int numIter;
  int maxIter = 20 + nvar;
  int numAvgForTest = 7;
  int intervalForTest = 50;
  float critMaxMove = 5.e-4f;
  float critMoveDiff = 5.e-6f;
  float wErrMean, wErrMax;

  if (nvar < 2)
    return;

  if (findPieceShifts(ivarpc, nvar, indvar, vw->xpclist, vw->ypclist,
                      vw->edgedx, vw->edgedy, -1, vw->piecelower,
                      vw->pieceupper, vw->skippedEdge, 1, b, 1,
                      vw->edgelower, vw->edgeupper, 1, vw->fbs_work, 0,leavind,
                      vw->excludeSkipped ? 1 : 3, 
                      vw->robustFit * vw->robustCrit, critMaxMove, 
                      critMoveDiff, maxIter,numAvgForTest,intervalForTest,
                      &numIter, &wErrMean, &wErrMax))
    printf("Error calling findPieceShifts\n");
  return;
}

static void xcorrRange(int size, float shift, int center, int border, int box,
                       int &prev0, int &prev1)
{
  int ishift = B3DNINT(shift);
  int boxlo = center - box / 2;
  int boxhi = boxlo + box - 1;
  prev0 = border + B3DMAX(0, ishift);
  
  prev1 = size - 1 - border - B3DMIN(0, ishift);
  prev0 = B3DMAX(boxlo, prev0);
  prev1 = B3DMIN(boxhi, prev1);
}

#define MAX_PEAKS 16
void crossCorrelate(MidasView *vw)
{
  float padFrac = 0.2f;
  float radius1 = -0.005, radius2 = 0.2f, sigma1 = 0.025f, sigma2 = 0.05f;
  int nsmooth = 6;
  int maxdim = B3DMAX(vw->xsize, vw->ysize);
  int border = B3DMIN(16, maxdim / 64);
  float xpeak[MAX_PEAKS], ypeak[MAX_PEAKS], peaks[MAX_PEAKS];
  float ctf[8193], ctfDelta;
  float *mat = vw->tr[vw->cz].mat;
  float dx, dy, diff1, diff2, xtrans = mat[6], ytrans = mat[7];
  int ix0, ix1, iy0, iy1, i, nxsmooth, nysmooth, minpad;
  int nxuse, nyuse, nxpad, nypad;
  float *array, *brray, *arfilt;
  float xcenter = vw->xcenter;
  float ycenter = vw->ycenter;
  float xPeakAdjust =0., yPeakAdjust = 0.;
  int ixAdjust = 0, iyAdjust = 0;
  int curSliceType = MIDAS_SLICE_CURRENT;
  int ipsave, nsum, imax, halfBox = vw->corrBoxSize / 2;
  unsigned char *curImageData, *prevImageData;
  float *xControl, *yControl, *xVector, *yVector;
  Islice *curSlice;
  double ccc, cccmax;

  // Use control point if there is a current one
  if (vw->editWarps && vw->curControl >= 0) {
    i = vw->numChunks ? vw->curChunk : vw->cz;
    if (getWarpPointArrays(i, &xControl, &yControl, &xVector, &yVector)) {
      midas_error("Error getting control point position", "", 0);
      return;
    }
    xcenter = xControl[vw->curControl] / vw->warpScale;
    ycenter = yControl[vw->curControl] / vw->warpScale;
    xtrans -= xVector[vw->curControl] / vw->warpScale;
    ytrans -= yVector[vw->curControl] / vw->warpScale;
    
    // Determine the average movement of the corners of the correlation box
    dx = halfBox * mat[0] + halfBox * mat[3] - halfBox;
    dy = halfBox * mat[1] + halfBox * mat[4] - halfBox;
    diff1 = (float)sqrt((double)dx * dx + dy * dy);
    dx = halfBox * mat[0] - halfBox * mat[3] - halfBox;
    dy = halfBox * mat[1] - halfBox * mat[4] + halfBox;
    diff2 = (float)sqrt((double)dx * dx + dy * dy);

    // If the movement is low enough, go to the original image instead of the transformed
    // one and adjust the extracted box and peak positions accordingly
    // This avoids having to iterate the correlation
    if (diff1 + diff2 <= 6) {
      curSliceType = MIDAS_SLICE_OCURRENT;
      ixAdjust = -B3DNINT(xtrans);
      iyAdjust = -B3DNINT(ytrans);
      xPeakAdjust = -xtrans - ixAdjust;
      yPeakAdjust = -ytrans - iyAdjust;
    }
    /*printf("total shift %f %f  adjust %d %d  %f %f\n", xtrans, ytrans, ixAdjust, 
      iyAdjust, xPeakAdjust, yPeakAdjust);*/
  }

  xcorrRange(vw->xsize, xtrans, xcenter, border, vw->corrBoxSize, ix0, ix1);
  xcorrRange(vw->ysize, ytrans, ycenter, border, vw->corrBoxSize, iy0, iy1);
  nxuse = ix1 + 1 - ix0;
  nyuse = iy1 + 1 - iy0;
  if (nxuse < 32 || nyuse < 32) {
    midas_error("The box size or overlap is too small to",
                "correlate the images at this shift", 0);
    return;
  }

  // Tried using taperinpad alone and it fails in the usual way on edges
  // So use the current approach in blendmont of smoothing and tapering outside
  // filtering low frequencies and looking at real-space correlations 
  // coefficient at  multiple locations
  // Make padding at least twice as big as desired smoothing
  minpad = B3DMAX(nxuse * padFrac, 2 * nsmooth);
  nxpad = niceFrame(nxuse + 2 * minpad, 2, 19);
  minpad = B3DMAX(nyuse * padFrac, 2 * nsmooth);
  nypad = niceFrame(nyuse + 2 * minpad, 2, 19);
  nxsmooth = nxuse + 2 * nsmooth;
  nysmooth = nyuse + 2 * nsmooth;
  array = (float *)malloc(((nxpad + 2) * nypad) * sizeof(float));
  brray = (float *)malloc(((nxpad + 2) * nypad) * sizeof(float));
  arfilt = (float *)malloc(((nxpad + 2) * nypad) * sizeof(float));
  if (!array || !brray || !arfilt) {
    midas_error("Error getting memory for correlation arrays", "", 0);
    B3DFREE(array);
    B3DFREE(brray);
    B3DFREE(arfilt);
    return;
  }
  ipsave = vw->fastInterp;
  if (vw->fastInterp) {
    vw->fastInterp = 0;
    flush_xformed(vw);
  }
  curSlice = midasGetSlice(vw, curSliceType);
  curImageData = curSlice->data.b;
  prevImageData = midasGetPrevImage(vw);
  vw->fastInterp = ipsave;
  sliceTaperInPad(prevImageData, SLICE_MODE_BYTE, vw->xsize, ix0, ix1, iy0,
                  iy1, array, nxuse, nxuse, nyuse, 0, 0);
  sliceSmoothOutPad(array, SLICE_MODE_FLOAT, nxuse, nyuse, array, nxsmooth, 
                    nxsmooth, nysmooth);
  sliceTaperOutPad(array, SLICE_MODE_FLOAT, nxsmooth, nysmooth, array, 
                   nxpad+2, nxpad, nypad, 0, 0.);
  sliceTaperInPad(curImageData, SLICE_MODE_BYTE, vw->xsize, ix0 + ixAdjust, 
                  ix1 + ixAdjust, iy0 + iyAdjust, iy1 + iyAdjust, brray, 
                  nxuse, nxuse, nyuse, 0, 0);
  sliceSmoothOutPad(brray, SLICE_MODE_FLOAT, nxuse, nyuse, brray, nxsmooth, 
                    nxsmooth, nysmooth);
  sliceTaperOutPad(brray, SLICE_MODE_FLOAT, nxsmooth, nysmooth, brray, 
                   nxpad+2, nxpad, nypad, 0, 0.);

  todfftc(array, nxpad, nypad, 0);
  todfftc(brray, nxpad, nypad, 0);

  // Double filter the correlation so it corresponds to the filtered real-space
  // correlation
  XCorrSetCTF(sigma1, sigma2, radius1, radius2, ctf, nxpad, nypad,
              &ctfDelta);
  XCorrFilterPart(array, array, nxpad, nypad, ctf, ctfDelta);
  XCorrFilterPart(brray, brray, nxpad, nypad, ctf, ctfDelta);
  memcpy(arfilt, array, (nxpad + 2) * nypad * sizeof(float));
  conjugateProduct(array, brray, nxpad, nypad);
  todfftc(array, nxpad, nypad, 1);
  todfftc(brray, nxpad, nypad, 1);
  todfftc(arfilt, nxpad, nypad, 1);
  XCorrPeakFind(array, nxpad+2, nypad, xpeak, ypeak, peaks, MAX_PEAKS);

  // For each peak within range, evaluate the CCC and find one with highest CCC
  cccmax = -10.;
  for (i = 0; i < MAX_PEAKS; i++) {
    if (peaks[i] > -1.e29 && fabs((double)xpeak[i]) <= vw->corrShiftLimit && 
        fabs((double)ypeak[i]) <= vw->corrShiftLimit) {
      ccc = XCorrCCCoefficient(arfilt, brray, nxpad+2, nxpad, nypad, xpeak[i], 
                               ypeak[i], (nxpad - nxuse) / 2, 
                               (nypad - nyuse) / 2, &nsum);
      //printf("%.2f %.2f %g %.5f %d\n", xpeak[i], ypeak[i], peaks[i], ccc, nsum);
      if (ccc > cccmax) {
        cccmax = ccc;
        imax = i;
      }
    }
  }
  free(array);
  free(brray);
  free(arfilt);
  if (cccmax > -1.) {
    /* printf("Peak %d at %.2f,%.2f   ccc %.4f  raw ratio to first %f\n",imax+1,
       xpeak[imax], ypeak[imax], cccmax, peaks[0] / peaks[imax]);  */
    vw->drawCorrBox = 1;
    vw->midasSlots->translate(xpeak[imax] + xPeakAdjust, ypeak[imax] + yPeakAdjust);
    return;
  }

  // No peak found, draw box in red
  vw->drawCorrBox = 3;
  vw->midasGL->draw();
}
