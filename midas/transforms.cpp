/*  IMOD VERSION 2.50
 *
 *  transforms.cpp -- Transformation functions used in midas.
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
Revision 3.1  2003/02/10 20:49:58  mast
Merge Qt source

Revision 1.1.2.5  2003/01/26 23:20:33  mast
using new library

Revision 1.1.2.4  2002/12/06 20:45:40  mast
Forgot to initialize midasWindow to NULL

Revision 1.1.2.3  2002/12/06 19:59:52  mast
Implement QTextStream for reading piece list

Revision 1.1.2.2  2002/12/06 05:12:15  mast
Protect quick translate against big shifts

Revision 1.1.2.1  2002/12/05 03:13:02  mast
New Qt version

Revision 3.3  2002/08/19 04:54:47  mast
Added declaration for solve_for_shifts

Revision 3.2  2002/08/19 04:50:03  mast
Made it do a series of local solutions for displacement errors in
montage fixing mode when there are many pieces.

Revision 3.1  2002/01/16 00:29:14  mast
Fixed a problem in montage fixing mode when there was only one section,
an error in Fortran to C translation

*/

#include <stdlib.h>
#include <math.h>
#include "midas.h"
#include <imodel.h>
#include <qfile.h>
#include "dia_qtutils.h"

static void checklist(int *xpclist, int npclist, int nxframe, int *minxpiece,
		      int *nxpieces, int *nxoverlap);
static void solve_for_shifts(struct Midas_view *vw, float *a, float *b,
			     int *ivarpc, int *indvar, int nvar, int limvar,
			     int leavind);

int gaussj(float *a, int n, int np, float *b, int m, int mp);

int new_view(struct Midas_view *vw)
{
  int i;
  vw->xsize = 0;
  vw->ysize = 0;
  vw->zsize = 0;
  vw->xysize = 0;
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
     
  vw->xtrans = 0;
  vw->ytrans = 0;
  vw->vmode = MIDAS_VIEW_COLOR;
  vw->fastip = TRUE;

  vw->width = 0;
  vw->height= 0;

  vw->xtype = XTYPE_XF;
  vw->xname = NULL;
  vw->changed = 0;
  vw->didsave = 0;
  vw->refname = NULL;
  vw->xsec  = 0;
  vw->plname = NULL;
  vw->reftext = NULL;
  vw->wXedge = NULL;
  vw->wMeanerr = NULL;

  vw->cachein = 0;
  vw->sminin = 0;
  vw->smaxin = 0;
  vw->boxsize = INITIAL_BOX_SIZE;
  vw->cachesize = 0;
  vw->cache = NULL;
  vw->usecount = 0;

  vw->exposed = 0;
  vw->blackstate = 0;
  vw->whitestate = 255;
  vw->reversemap = 0;
  vw->applytoone = 0;
  vw->difftoggle = NULL;
  vw->keepsecdiff = 1;
  vw->midasWindow = NULL;
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
int load_view(struct Midas_view *vw, char *fname)
{
  int k, ix, ixy, iy, ind, ipclo, ipchi, ned;
  QString str;
  int maxedges, nsect, nxypc, maxpieces, nalong, ncross;

  if (load_image(vw, fname))
    return(-1);

  vw->cz = 1;
  vw->refz = 0;

  vw->zsize = vw->hin->nz;
  vw->xsize = vw->hin->nx;
  vw->ysize = vw->hin->ny;
  vw->xysize = vw->xsize * vw->ysize;
  vw->xcenter = 0.5 * vw->xsize;
  vw->ycenter = 0.5 * vw->ysize;

  if (vw->boxsize < 0) {
    vw->boxsize = (int)((sqrt((double)vw->xysize) - 1.) / 128. + 1.);
    if (vw->boxsize > 8)
      vw->boxsize = 8;
  }

  /* Fix the box size that might have been entered */
  if (vw->boxsize > vw->xsize / 4)
    vw->boxsize = vw->xsize / 4;
  if (vw->boxsize > vw->ysize / 4)
    vw->boxsize = vw->ysize / 4;

  if (vw->boxsize == 0)
    vw->fastip = 0;
  if (vw->boxsize < 1)
    vw->boxsize = 1;
	  

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

  vw->cache = (struct Midas_cache *)malloc(vw->cachesize * 
					   sizeof(struct Midas_cache));

  for(k = 0; k < vw->cachesize; k++){
    vw->cache[k].sec = mrc_slice_create(vw->xsize, vw->ysize, 
					MRC_MODE_BYTE);
    if (!vw->cache[k].sec)
      midas_error("Out of memory! ", "Exiting", -1);
    vw->cache[k].zval = -1;
    vw->cache[k].xformed = -1;
    vw->cache[k].used = -1;
    vw->cache[k].sec->mean = vw->hin->amean * vw->li->slope + 
      vw->li->offset;
  }

  vw->tr = (struct Midas_transform *)
    malloc((vw->zsize + 1) * sizeof(struct Midas_transform));
  for (k = 0 ; k <= vw->zsize; k++){
    vw->tr[k].black = 0;
    vw->tr[k].white = 255;
    vw->tr[k].mat[0] = 1.0;
    vw->tr[k].mat[1] = 0.0;
    vw->tr[k].mat[2] = 0.0;
    vw->tr[k].mat[3] = 0.0;
    vw->tr[k].mat[4] = 1.0;
    vw->tr[k].mat[5] = 0.0;
    vw->tr[k].mat[6] = 0.0;
    vw->tr[k].mat[7] = 0.0;
    vw->tr[k].mat[8] = 1.0;
  }

  vw->id = (unsigned long *)malloc(vw->xysize * sizeof(unsigned long));

  /* Create and load slice for reference image now */
  if (vw->refname) {
    vw->ref = mrc_slice_create(vw->xsize, vw->ysize, MRC_MODE_BYTE);
    if (!vw->ref)
      midas_error("Out of memory! ", "Exiting", -1);
    if (load_refimage(vw, vw->refname))
      return(1);
    vw->refz = vw->xsec;
    vw->keepsecdiff = 0;
    vw->cz = 0;
  }

  /* Now get pieces and analyze them if montage */
  if (vw->plname) {
    str = vw->plname;
    QFile file(str);
    if (!file.open(IO_ReadOnly | IO_Translate)) {
      fprintf(stderr, "Error opening %s.\n", vw->plname);
      exit(-1);
    }
    QTextStream stream(&file);
    stream.setf(QTextStream::dec);

    vw->xpclist = (int *)malloc(vw->zsize * sizeof(int));
    vw->ypclist = (int *)malloc(vw->zsize * sizeof(int));
    vw->zpclist = (int *)malloc(vw->zsize * sizeof(int));
    vw->edgeupper = (int *)malloc(2 * vw->zsize * sizeof(int));
    vw->edgelower = (int *)malloc(2 * vw->zsize * sizeof(int));
    vw->fbs_indvar = (int *)malloc(2 * vw->zsize * sizeof(int));
    if (!vw->xpclist || !vw->ypclist || !vw->zpclist || !vw->edgeupper
	|| !vw->edgelower || !vw->fbs_indvar) {
      fprintf(stderr, "Error getting memory for piece list.\n");
      exit(-1);
    }

    vw->minzpiece = 1000000;
    vw->maxzpiece = -1000000;
    for (k = 0; k < vw->zsize; k++) {
      str = stream.readLine();
      if (str.isEmpty()) {
	fprintf(stderr, "Error reading piece list after %d lines\n"
		, k);
	exit(-1);
      }
      sscanf(str.latin1(), "%d %d %d", &(vw->xpclist[k]), 
		       &(vw->ypclist[k]), &(vw->zpclist[k]));
      if (vw->minzpiece > vw->zpclist[k])
	vw->minzpiece = vw->zpclist[k];
      if (vw->maxzpiece < vw->zpclist[k])
	vw->maxzpiece = vw->zpclist[k];
    }

    checklist(vw->xpclist, vw->zsize, vw->xsize, &vw->minxpiece, 
	      &vw->nxpieces, &vw->nxoverlap);
    checklist(vw->ypclist, vw->zsize, vw->ysize, &vw->minypiece, 
	      &vw->nypieces, &vw->nyoverlap);
    if (vw->nxpieces < 0 || vw->nypieces < 0) {
      fprintf(stderr, "Piece list does not have regular array "
	      "of coordinates.\n");
      exit(-1);
    }

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
    vw->montmap = (int *)malloc(maxpieces * sizeof(int));
    vw->fbs_a = (float *)malloc(nxypc * nxypc * sizeof(float));
    vw->fbs_b = (float *)malloc(2 * nxypc * sizeof(float));
    vw->fbs_ivarpc = (int *)malloc(nxypc * sizeof(int));
    if (!vw->montmap || !vw->pieceupper || !vw->piecelower ||
	!vw->edgedx || !vw->edgedy || !vw->fbs_a || !vw->fbs_b ||
	!vw->fbs_ivarpc) {
      fprintf(stderr, "Error getting memory for piece analysis.\n");
      exit(-1);
    }

    /*get edge indexes for pieces and piece indexes for edges
      first build a vw->montmap of all pieces present */
    for (k = 0; k < maxpieces; k++)
      vw->montmap[k] = -1;

    for (k = 0; k < vw->zsize; k++) {
      vw->montmap[(vw->xpclist[k] - vw->minxpiece) / 
		  (vw->xsize - vw->nxoverlap) +
		  vw->nxpieces * (vw->ypclist[k] - vw->minypiece) /
		  (vw->ysize - vw->nyoverlap) +
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
	  
    if (!vw->nedge[0] && !vw->nedge[1]) {
      fprintf(stderr, "There are no edges or no montage "
	      "according to these piece coordinates.\n");
      exit(-1);
    }
	  
    vw->xory = 0;
    vw->montcz = nearest_section(vw, vw->minzpiece, 0);
    vw->midasSlots->manage_xory(vw);
    vw->curedge = nearest_edge(vw, vw->montcz, vw->xory, 1, 0, 
			       &vw->edgeind);
    set_mont_pieces(vw);
    vw->incindex[2]--;
    vw->increment[2] = vw->midasSlots->getIncrement(vw->incindex[2], 2);
    if (load_transforms(vw, vw->xname))
      exit(-1);
    vw->didsave = 0;
  }

  vw->midasGL->fill_viewdata(vw);

  if (vw->didsave)
    load_transforms(vw, vw->xname);
  vw->didsave = 0;

  vw->midasSlots->backup_current_mat();
  return(0);
}


Islice *getRawSlice(struct Midas_view *vw, int zval)
{
  int k;
  int minuse = vw->usecount + 1;
  int oldest = 0;

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
  mrcReadZByte(vw->hin, vw->li, vw->cache[oldest].sec->data.b, zval);
  return (vw->cache[oldest].sec);
}

static Islice *getXformSlice(struct Midas_view *vw, int zval, int *xformed)
{
  int k;
  int minuse = vw->usecount + 1;
  int oldest = 0;
  Islice *orgSlice;
  float *mat = vw->tr[zval].mat;
  *xformed = 1;

  /* search cache for transformed slice.  Do this first so that the
     transformed slice will always get shifted by fast translations */
  for (k = 0; k < vw->cachesize; k++) {
    /* If it's a match, mark as not transformed and return */
    if (vw->cache[k].zval == zval && vw->cache[k].xformed) {
      vw->cache[k].used = vw->usecount++;
      *xformed = 0;
      return (vw->cache[k].sec);
    }
  }

  /* If it's a unit transformation, just return the raw slice and mark
     as already transformed */
  if (mat[0] == 1.0 && mat[4] == 1.0 && mat[1] == 0.0 && mat[3] == 0.0 &&
      mat[6] < 0.0001 && mat[6] > -0.0001 && mat[7] < 0.0001 && 
      mat[7] > -0.0001) {
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

  /* Allocate oldest spot to this slice and transform raw slice */
  vw->cache[oldest].zval = zval;
  vw->cache[oldest].xformed = 1;
  vw->cache[oldest].used = vw->usecount++;
  midas_transform(orgSlice, vw->cache[oldest].sec, &vw->tr[zval]);
  return (vw->cache[oldest].sec);
}

/* 
 * Get a slice from the image data.
 */
Islice *midasGetSlice(struct Midas_view *vw, int sliceType, int *xformed)
{
  *xformed = 0;
  switch(sliceType){
  case MIDAS_SLICE_CURRENT:
    return(getXformSlice(vw, vw->cz, xformed));
  case MIDAS_SLICE_PREVIOUS:
    return(getXformSlice(vw, vw->refz, xformed));
  case MIDAS_SLICE_REFERENCE:
    return(vw->ref);
  case MIDAS_SLICE_OCURRENT:
    return(getRawSlice(vw, vw->cz));
  case MIDAS_SLICE_OPREVIOUS:
    return(getRawSlice(vw, vw->refz));
  }
  return(NULL);
}

/* flush the cache of any transformed images */
void flush_xformed(struct Midas_view *vw)
{
  int k;
  for (k = 0; k < vw->cachesize; k++)
    if (vw->cache[k].xformed) {
      vw->cache[k].zval = -1;
      vw->cache[k].used = -1;
    }
}

struct Midas_transform *midasGetTrans(struct Midas_view *vw)
{
  return(&vw->tr[vw->cz]);
}

void midasGetSize(struct Midas_view *vw, int *xs, int *ys)
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
int translate_slice(struct Midas_view *vw, int xt, int yt)
{
  unsigned char *tptr, *cptr;
  int i, j, yofs;
  int xfd;
  unsigned char mean;
  int xsize, ysize, xysize;
  Islice *curSlice = midasGetSlice(vw, MIDAS_SLICE_CURRENT, &xfd);

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

int midas_transform(struct MRCslice *slin, 
		    struct MRCslice *sout,
		    struct Midas_transform *tr)
{
  int i, j, k, l, index;
  int ix, iy;
  int xsize, ysize;
  float *mat;
  float ox = 0, oy = 0;
  float xdx = 1, xdy = 0, ydx = 0, ydy = 1;
  float x, y;
  unsigned char umean;
  int nxa, nya, box;
  int ixst, ixnd;
  float xrt, xlft, xst, xnd;
  float fx, fy;
  unsigned char *buf;

  mat = tramat_inverse(tr->mat);
  if (!mat)
    return(-1);
  tramat_getxy(mat, &ox, &oy);
  tramat_getxy(mat, &xdx, &xdy);
  tramat_getxy(mat, &ydx, &ydy);
  xdx -= ox;
  xdy -= oy;
  ydx -= ox;
  ydy -= oy;

  xsize = slin->xsize;
  ysize = slin->ysize;
  umean = (unsigned char)slin->mean;

  box = VW->boxsize;
  nxa = xsize - box;
  nya = ysize - box;
  if (!VW->fastip) {
    box = 1;
    nxa = xsize - 2;
    nya = ysize - 2;
  }

  for(j = 0, y = oy, x = ox; j < ysize + 1 - box; 
      j += box, y += ydy * box, x += ydx * box){
    oy = y; ox = x;

    /* compute constrained, safe coordinates to use */
    xst = 0;
    xnd = xsize - 1;

    /* get intersection with left and right sides unless vertical */
    if (xdx > 1.e-10 || xdx < -1.e-10) {
      xlft = -x / xdx;
      xrt = (nxa - 0.5 - x) / xdx;
      if (xlft < xrt) {
	if (xst < xlft)
	  xst = xlft;
	if (xnd > xrt)
	  xnd = xrt;
      } else {
	if (xst < xrt)
	  xst = xrt;
	if (xnd > xlft)
	  xnd = xlft;
      }
    } else if (x < 0 || x >= nxa - 0.5) {
      /* if vertical and outside limits, set up for fill */
      xst = nxa;
      xnd = 1;
    }

    /* get intersection with bottom and top unless horizontal */
    if (xdy > 1.e-10 || xdy < -1.e-10) {
      xlft = -y / xdy;
      xrt = (nya - 0.5 - y) / xdy;
      if (xlft < xrt) {
	if (xst < xlft)
	  xst = xlft;
	if (xnd > xrt)
	  xnd = xrt;
      } else {
	if (xst < xrt)
	  xst = xrt;
	if (xnd > xlft)
	  xnd = xlft;
      }
    } else if (y < 0 || y >= nya - 0.5) {
      xst = nxa;
      xnd = 1;
    }

    /* truncate ending down, starting up */
    ixnd = (int)xnd;
    ixst = nxa + 1 - (int)(nxa + 1. - xst);

    /* If they are crossed, set up so fill does whole extent */
    if (ixst > ixnd) {
      ixst = nxa / 2;
      ixnd = ixst - 1;
    } else
      /* otherwise, make sure it's an even box multiple */
      ixnd = ixst + box * ((ixnd - ixst) / box);

    /* Do the left and right fills */
    for (k = j; k < j + box; k++) {
      buf = &sout->data.b[k * xsize];
      for (i = 0; i < ixst; i++)
	*buf++ = umean;
      buf = &sout->data.b[ixnd + 1 + k * xsize];
      for (i = ixnd + 1; i < xsize; i++)
	*buf++ = umean;
    }

    /* displace to starting point */
    x += ixst * xdx;
    y += ixst * xdy;

    if (VW->fastip && box < 2) {

      /* nearest neighbor, no box copies, add 0.5 for nearest int */
      x += 0.5;
      y += 0.5;
      buf = &sout->data.b[ixst + j * xsize];
      for(i = ixst; i <= ixnd; i++, y += xdy, x += xdx){
	ix = (int)x; iy = (int)y;
	index = ix + (iy * xsize);
	*buf++ = slin->data.b[index];
      }

    } else if (VW->fastip) {

      /* Box copies */
      x += 0.5;
      y += 0.5;
      for(i = ixst; i <= ixnd; i += box, y += xdy * box, 
	    x += xdx * box){
	ix = (int)x; iy = (int)y;
	for (k = j; k < j + box; k++) {
	  index = ix + ((iy + k - j) * xsize);
	  buf = &sout->data.b[i + k * xsize];
	  for (l = 0; l < box; l++)
	    *buf++ = slin->data.b[index++];
	}
      }
    } else {
      /* bilinear interpolation */
      buf = &sout->data.b[ixst + j * xsize];
      for(i = ixst; i <= ixnd; i++, y += xdy, x += xdx){
	ix = (int)x; iy = (int)y;
	fx = x - ix;
	fy = y - iy;
	index = ix + (iy * xsize);
	*buf++ = (unsigned char)((1. - fy) * ((1. - fx) *slin->data.b[index] +
					      fx * slin->data.b[index + 1]) +
				 fy * ((1. - fx) *slin->data.b[index + xsize] +
				       fx * slin->data.b[index + xsize + 1]));
      }
    }

    y = oy; x = ox;
  }

  /* fill any top lines left undone */
  for (j = ysize + 1 - box; j < ysize; j++) {
    buf = &sout->data.b[j * xsize];
    for (i = 0; i < xsize; i++)
      *buf++ = umean;
  }

  tramat_free(mat);
  return(0);
}

/*
 * Matrix calculation functions.  
 * To do: use the IMOD library imat functions instead.
 *
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
  int i;

  for(i = 0; i < 9; i++)
    mat[i] = 0.0;
  mat[0] = 1.0;
  mat[4] = 1.0;
  mat[8] = 1.0;
  return 0;
}

int tramat_copy(float *fmat, float *tomat)
{
  int i;

  for(i = 0; i < 9; i++)
    tomat[i] = fmat[i];
  return 0;
}

int tramat_multiply(float *m2, float *m1, float *out)
{
  out[0] = (m1[0] * m2[0]) + (m1[3] * m2[1]) + (m1[6] * m2[2]);
  out[1] = (m1[1] * m2[0]) + (m1[4] * m2[1]) + (m1[7] * m2[2]);
  out[2] = (m1[2] * m2[0]) + (m1[5] * m2[1]) + (m1[8] * m2[2]);
  out[3] = (m1[0] * m2[3]) + (m1[3] * m2[4]) + (m1[6] * m2[5]);
  out[4] = (m1[1] * m2[3]) + (m1[4] * m2[4]) + (m1[7] * m2[5]);
  out[5] = (m1[2] * m2[3]) + (m1[5] * m2[4]) + (m1[8] * m2[5]);
  out[6] = (m1[0] * m2[6]) + (m1[3] * m2[7]) + (m1[6] * m2[8]);
  out[7] = (m1[1] * m2[6]) + (m1[4] * m2[7]) + (m1[7] * m2[8]);
  out[8] = (m1[2] * m2[6]) + (m1[5] * m2[7]) + (m1[8] * m2[8]);
  return 0;
}

int tramat_translate(float *mat, double x, double y)
{
  /*     float *tmat, *omat; */

  mat[6] += x;
  mat[7] += y;
  return(0);

  /*
    tmat = tramat_create();
    omat = tramat_create();

    tmat[6] += x;
    tmat[7] += y;
    tramat_multiply(mat, tmat, omat);
    tramat_copy(omat, mat);
    tramat_free(omat);
    tramat_free(tmat);
  */
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


int tramat_getxy(float *mat, float *x, float *y)
{
  float ox = *x, oy = *y;

  *x = (mat[0] * ox) + (mat[3] * oy) + mat[6]; 
  *y = (mat[1] * ox) + (mat[4] * oy) + mat[7]; 
  return 0;
}


int tramat_testin(float *mat, float *imat)
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


float *tramat_oinverse(float *mat)
{
  /* solve equation     */
  /* mat * imat = idmat */

  float *imat;
  float cbod, ebod;
  imat = tramat_create();

  if (mat[4] == 0){
    imat[0] = 0;
    imat[1] = 1 / mat[3];
  }else{
    cbod = (mat[3] * mat[1])/mat[4];
    imat[0] = 1.0 / (mat[0] - cbod);
    imat[1] =  - (mat[1] * imat[0]) / mat[4];
  }

  if (mat[3] == 0){
    imat[3] = 0;
    imat[4] = 1 / mat[4];
  }else{
    imat[3] = 1.0 / (mat[1] - ((mat[0] * mat[4])/mat[3]));
    imat[4] =  - (mat[0] * imat[3]) / mat[3];
  }

  if (mat[0] == 0){
    imat[7] = - (mat[6] / mat[3]);
    imat[6] = - (mat[7] / mat[1]);
  }else if (mat[3] == 0){
    imat[7] = - (mat[7] / mat[4]);
    imat[6] = - (mat[6] / mat[0]);
  }else{
    ebod = (mat[6] * mat[1])/mat[4];
    imat[7] = (ebod - mat[7]) / (mat[4] - cbod);
    imat[6] = -(mat[6] + (mat[3] * imat[7])) / mat[4];
  }
  return(imat);
}


float *tramat_inverse(float *mat)
{
  float *imat;
  float mval;
  int i;

  imat = tramat_create();
  mval = mat[8] * ( (mat[0] * mat[4]) - (mat[1] * mat[3]) );

  imat[0] =  mat[4] * mat[8]; 
  imat[1] = -mat[1] * mat[8];
  imat[2] =  0.0;
  imat[3] = -mat[3] * mat[8];
  imat[4] =  mat[0] * mat[8];
  imat[5] =  0.0;
  imat[6] = (mat[3] * mat[7]) - (mat[4] * mat[6]);
  imat[7] = (mat[1] * mat[6]) - (mat[0] * mat[7]);
  imat[8] = (mat[0] * mat[4]) - (mat[1] * mat[3]);

  for(i = 0; i < 9; i++)
    imat[i] /= mval;

  /*     tramat_testin(mat, imat); */
  return(imat);
}


void transform_model(char *infname, char *outfname, struct Midas_view *vw)
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

int nearest_edge(struct Midas_view *vw, int z, int xory, int edgeno, 
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
    if (vw->montmap[ind] >= 0 && vw->montmap[ind + xmult] >= 0) {

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
     
int nearest_section(struct Midas_view *vw, int sect, int direction)
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
void set_mont_pieces(struct Midas_view *vw)
{
  int ind = vw->edgeind * 2 + vw->xory;
  vw->cz = vw->pieceupper[ind];
  vw->refz = vw->piecelower[ind];
  vw->tr[vw->cz].mat[6] = vw->edgedx[ind] + vw->xpclist[vw->cz] - 
    vw->xpclist[vw->refz];
  vw->tr[vw->cz].mat[7] = vw->edgedy[ind] + vw->ypclist[vw->cz] - 
    vw->ypclist[vw->refz];
}

#define MAX_GAUSSJ_VARS 9
#define LOCAL_BORDER 2
void find_best_shifts(struct Midas_view *vw, int leaveout, int ntoperr,
		      float *meanerr, float *amax, int *indmax,
		      float *curerrx, float *curerry, int localonly)
{
  float *a = vw->fbs_a;
  float *b = vw->fbs_b;
  int *indvar = vw->fbs_indvar;
  int *ivarpc = vw->fbs_ivarpc;
  int leavind, i, j, ivar, ipclo, ipc, nvar, ind, elx, ely, eux, euy;
  int nsum, edge, ixy;
  float adist, asum, adx, ady;
  int limvar = vw->nxpieces * vw->nypieces;

  /*  Unlike in bsubs.f the data coming in are the shifts to bring 
      the upper piece into alignment with the lower piece. */
     
  /* build list of variables */
  leavind = -1;
  if (leaveout)
    leavind = vw->edgeind * 2 + vw->xory;
  nvar=0;
  for (ipc = 0; ipc < vw->zsize; ipc++) {
    if(vw->zpclist[ipc] == vw->montcz) {
      indvar[ipc] = -1;
      ind = ipc * 2;
      elx = vw->edgelower[ind];
      ely = vw->edgelower[ind + 1];
      eux = vw->edgeupper[ind];
      euy = vw->edgeupper[ind + 1];
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
  *curerrx = *curerry = *meanerr = 0.;
  if (nvar < 2)
    return;

  if (nvar > MAX_GAUSSJ_VARS * MAX_GAUSSJ_VARS) {
    find_local_errors(vw, leaveout, ntoperr, meanerr, amax, indmax,
		      curerrx, curerry, localonly);
    return;
  }

  solve_for_shifts(vw, a, b, ivarpc, indvar, nvar, limvar, leavind);


  nsum=0;
  asum=0.;
  for (ivar = 0; ivar < nvar; ivar++) {
    ipc=ivarpc[ivar];

    for (ixy = 0; ixy < 2; ixy++) {
      edge = vw->edgelower[ipc * 2 + ixy];
      ind = edge * 2 + ixy;
      if (edge > -1) {
	ipclo = vw->piecelower[ind];
	adx = b[ivar * 2] - b[indvar[ipclo] * 2] - vw->edgedx[ind];
	ady = b[ivar * 2 + 1] - b[indvar[ipclo] * 2 + 1] -
	  vw->edgedy[ind];
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
void find_local_errors(struct Midas_view *vw, int leaveout, int ntoperr,
		       float *meanerr, float *amax, int *indmax,
		       float *curerrx, float *curerry, int localonly)
{
  float *a = vw->fbs_a;
  float *b = vw->fbs_b;
  int *indvar = vw->fbs_indvar;
  int *ivarpc = vw->fbs_ivarpc;
  int leavind, i, j, ivar, ipclo, ipc, nvar, ind, elx, ely, eux, euy;
  int nsum, edge, ixy, localind;
  float adist, asum, adx, ady;
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
  if (nshort > MAX_GAUSSJ_VARS) {
    basesize = MAX_GAUSSJ_VARS - 2 * LOCAL_BORDER - 1;
    ndivShort = (nshort + basesize - 2) / basesize;
    divSizeShort = (nshort - 1) / ndivShort;
    shortsize = divSizeShort + 2 * LOCAL_BORDER + 1;
    if ((nshort - 1) % ndivShort)
      shortsize++;
  }

  /*   printf ("%d %d %d %d\n", basesize, ndivShort, divSizeShort, shortsize); */

  /* Now long size can have bigger divisions if short side is small */
  basesize = (MAX_GAUSSJ_VARS * MAX_GAUSSJ_VARS) / shortsize -  
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
	  ipc = vw->montmap[ix + iy * vw->nxpieces + 
			    vw->montcz * limvar];

	  ind = ipc * 2;
	  elx = vw->edgelower[ind];
	  ely = vw->edgelower[ind + 1];
	  eux = vw->edgeupper[ind];
	  euy = vw->edgeupper[ind + 1];
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
	      (ix > ixstin && elx > -1 && elx*2 == localind) ||
	      (iy > iystin && ely > -1 && 
	       ely * 2 + 1 == localind) ||
	      (ix < ixndin && eux > -1 && eux* 2 == localind) ||
	      (iy < iyndin && euy > -1 && 
	       euy * 2 + 1 == localind))
	    doarea = 1;
	}
      }
      /*  printf("X: %d %d %d %d %d   Y: %d %d %d %d %d   leave %d %d\n",
	  divX, ixst, ixnd, ixstin, ixndin, divY, iyst, iynd, iystin, iyndin, leavind, doarea); */
      if (nvar >= 2 && doarea) {
	solve_for_shifts(vw, a, b, ivarpc, indvar, nvar, limvar, 
			 leavind);

	/* evaluate errors for ones in the inner area */
	for (iy = iystin; iy <= iyndin; iy++) {
	  for (ix = ixstin; ix <= ixndin; ix++) {
	    ipc = vw->montmap[ix + iy * vw->nxpieces + 
			      vw->montcz * limvar];
	    ivar = indvar[ipc];
	    if (ivar < 0)
	      continue;

	    for (ixy = 0; ixy < 2; ixy++) {
	      edge = vw->edgelower[ipc * 2 + ixy];
	      ind = edge * 2 + ixy;
	      /* evaluate if edge exists and, for an
		 X edge, the piece is past the first in X
		 and is either not the last local piece in
		 Y or the very last piece in Y */
	      if (edge > -1 && 
		  ((!ixy && ix > ixstin &&
		    (iy < iyndin || 
		     iynd == vw->nypieces - 1))
		   || (ixy && iy > iystin &&
		       (ix < ixndin || 
			ixnd == vw->nxpieces -1)))){
		ipclo = vw->piecelower[ind];
		adx = b[ivar * 2] - b[indvar[ipclo]* 2]
		  - vw->edgedx[ind];
		ady = b[ivar * 2 + 1] - 
		  b[indvar[ipclo] * 2 + 1] -
		  vw->edgedy[ind];
		adist = sqrt((double)
			     (adx * adx + ady * ady));
		if (ind == vw->edgeind * 2 + vw->xory){
		  *curerrx = adx;
		  *curerry = ady;
		}
		if (ind != leavind) {
		  asum += adist;
		  nsum++;
		  for (i = 0; i < ntoperr; i++) {
		    if (adist > amax[i]) {
		      for (j = ntoperr - 1;
			   j > i; j--) {
			amax[j] = 
			  amax[j - 1];
			indmax[j] = 
			  indmax[j - 1];
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

/* Set up equations to determine shifts for the given set of variables */
static void solve_for_shifts(struct Midas_view *vw, float *a, float *b, 
			     int *ivarpc, int *indvar, int nvar, int limvar,
			     int leavind)
{
  int ivar, ipc, ind,i;
  int neighpc, neighvar, edge, m, ixy;
  float xsum, ysum;

  if (nvar < 2)
    return;

  /*  build matrix of simultaneous equations for minimization solution */
  for (ivar = 0; ivar < nvar - 1; ivar++) {
    ipc = ivarpc[ivar];
    for (m = 0; m < nvar -1; m++) {
      a[ivar * limvar + m] = 0.;
      b[ivar * 2] = 0;
      b[ivar * 2 + 1] = 0.;
    }

    for (ixy = 0; ixy < 2; ixy++) {

      edge = vw->edgelower[ipc * 2 + ixy];
      if(edge > -1 && edge * 2 + ixy != leavind) {
	ind = edge * 2 + ixy;
	neighpc = vw->piecelower[ind];
	neighvar = indvar[neighpc];
	if (neighvar >= 0) {
	  a[ivar * limvar + ivar]++;
		    
	  /* for a regular neighbor, enter a -1 in its term; 
	     but for the last variable being eliminated, enter
	     a +1 for ALL other variables instead */
	  if (neighvar != nvar - 1)
	    a[ivar * limvar + neighvar]--;
	  else {
	    for (m = 0 ; m < nvar-1; m++)
	      a[ivar * limvar + m]++;
	  }

	  /* when this piece is an upper piece, subtract 
	     displacements from (add shifts to) constant term */
	  b[ivar * 2] += vw->edgedx[ind];
	  b[ivar * 2 + 1] += vw->edgedy[ind];
	}
      }

      edge = vw->edgeupper[ipc * 2 + ixy];
      if(edge > -1 && edge * 2 + ixy != leavind) {
	ind = edge * 2 + ixy;
	neighpc = vw->pieceupper[ind];
	neighvar = indvar[neighpc];
	if (neighvar >= 0) {
	  a[ivar * limvar + ivar]++;
	  if (neighvar != nvar - 1)
	    a[ivar * limvar + neighvar]--;
	  else {
	    for (m=0 ; m < nvar-1; m++)
	      a[ivar * limvar + m]++;
	  }
		    
	  /* when a lower piece, add displacements to (subtract
	     shifts from) constant terms */
	  b[ivar * 2] -= vw->edgedx[ind];
	  b[ivar * 2 + 1] -= vw->edgedy[ind];
	}
      }
    }
  }

  /* solve the equations, take the b values as dx and dy; compute the
     sum to get the shift for the final piece */

  /*   for (i = 0; i < nvar; i++)
       printf("%5d",ivarpc[i]);
       printf("\n");
       for (j = 0; j < (nvar -1); j++) {
       for (i = 0; i < (nvar - 1); i++) 
       printf("%7.1f", a[j + i * limvar]);
       printf("\n");
       }
       for (j = 0; j < 2; j++) {
       for (i = 0; i < (nvar - 1); i++)
       printf("%9.2f", b[i * 2 + j]);
       printf("\n");
       } */
	  
  gaussj(a, nvar - 1, limvar, b, 2, 2);

  xsum=0.;
  ysum=0.;
  for (i = 0; i < nvar-1; i++) {
    xsum += b[i * 2];
    ysum += b[i * 2 + 1];
  }
  b[(nvar - 1) * 2] = -xsum;
  b[(nvar - 1) * 2 + 1] = -ysum;

}

