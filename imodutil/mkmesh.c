/*  IMOD VERSION 2.50
 *
 *  mkmesh.c -- mesh making fuctions.
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
Revision 3.7  2004/09/27 21:58:33  mast
Prevented polygons from being combined on second run of imeshReMeshNormal

Revision 3.6  2004/09/10 21:34:01  mast
Eliminated long variables

Revision 3.5.6.1  2004/09/10 19:27:52  mast
Eliminate long variables

Revision 3.5  2003/08/26 03:47:57  mast
Added ability to cap ends of tubes, made a function to add a cap mesh, made
the number of segments in a tube mesh be variable, and
renamed jcont and mkcont to indicate they are specific to tubes.

Revision 3.4  2003/02/27 16:57:57  mast
change to b3dX,Y,Z

Revision 3.3  2003/02/07 02:36:59  mast
Compute zmin correctly so it will work with negative z's; add some tests
for empty contours in early setup of contour lists

Revision 3.2  2002/04/09 19:07:15  mast
Fixed a bug in scan_points_to_segments found by Lambert Zipj.

Revision 3.1  2002/01/13 17:37:02  mast
Made it work with inside-out donuts (annulus to horseshoe transitions)

*/
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include <time.h>
#include "mkmesh.h"

double meshDiameterSize = 0.0;

typedef struct Nest_struct
{
  int co;        /* contour number */
  int level;     /* Level in from outside-most */
  int ninside;   /* Number inside */
  int *inside;   /* Numbers of contours inside */
  int noutside;  /* Number outside */
  int *outside;  /* Numbers of contours outside */
  Icont *inscan; /* Scan contour of object interior, for odd levels */
}Nesting;

static void report_time(char *string);
static int    imeshDefaultCallback(int inStatus);

static Imesh *imeshContours(Icont *bc, Icont *tc, Ipoint *scale);
static Imesh *imeshContoursCost(Icont *bc, Icont *tc, Ipoint *scale, 
				int inside, int openobj);

static Imesh *imeshContourCap(Icont *cont, int side, int inside, 
			      Ipoint *scale);
static Imesh *makeCapMesh(Icont *cont, Ipoint *cm, int meshdir);

static int mesh_open_tube_obj(Iobj *obj, Ipoint *scale, unsigned int flags);
static Imesh *joinTubeCont(Icont *c1, Icont *c2, Ipoint *norm);
static int mkTubeCont(Icont *cont, Ipoint *loc, Ipoint *n, Ipoint *scale,
		  float tubeDiameter, int slices);

static int inside_cont(Icont *cont, Ipoint pt);
static int eliminate_overlap(Icont *c1, Icont *c2);
static void cost_from_area_matrices(float *up, float *down, float *cost,
				    char *path, int bdim, int tdim,
				    int bsize, int sb, int st, float curmin);
static void build_area_matrices(Icont *bc, int bdir,
				Icont *tc, int tdir, Ipoint *scale,
				float *up, float *down);
static Icont *connect_orphans(Iobj *obj, Icont *cout, int *list, int *used,
			      int *justinl, int njustin, int *inlist, int joindir,
			      int oddeven, int surf, int time, Ipoint *scale,
			      Icont **scancont, Ipoint *pmin, Ipoint *pmax);
static float evaluate_break(Icont *cout, int *list, int *used,
			    int *justinl, int njustin, Icont **scancont,
			    Ipoint *pmin, Ipoint *pmax, int st1, int st2,
			    float curmin, float pathlen, float fullarea,
			    int joindir, int areaonly, float *innerarea);
static void add_whole_nest(int nind, Iobj *obj, Nesting *nests, int *nestind,
			   int flag, int *tlist, int *ntop);
static Icont *join_all_contours(Iobj *obj, int *list, int ncont, 
				int nsamelevel, int fill, int *olist, 
				int nother, int nothersame, Icont *other);
static int chunkMeshAddIndex(Imesh *mesh, int index, int *maxlist);
static void subtract_scan_contours(Icont *cs1, Icont *cs2);
static int mesh_contours(Iobj *obj, Icont *bcont, Icont *tcont, int surf,
			 int time, Ipoint *scale, int inside);
static int break_contour_inout(Icont *cin, int st1, int st2,  int fill,
			       Icont **cout1, Icont **cout2);

static int fastmesh = 0;

static int    imeshDefaultCallback(int inStatus)
{
  int dummy = inStatus;
  return(0);
}

static clock_t timeval1, timeval2;

static void report_time(char *string)
{
  char *dummy = string;
  float elapsed;
  timeval2 = clock();
  elapsed = (float)(timeval2 - timeval1) / CLOCKS_PER_SEC;
  /* printf("%s: %.3f\n",string, elapsed); */
  timeval1 = timeval2;
}
	  

static int getnextz(int *zlist, int zlsize, int cz)
{
  int z;

  for(z = 0; z < zlsize - 1; z++){
    if (cz == zlist[z])
      return(zlist[z+1]);
  }
  /* DNM: this will work the same as when not connecting skips */
  if (cz == zlist[zlsize - 1])
    return (zlist[zlsize - 1] + 1);
  return(-2);
}


static int getContZValue(Icont *cont)
{
  int p;
  float z=0.0f;
  if ((!cont) || (!cont->psize))
    return(-1);
  for(p = 0; p < cont->psize; p++)
    z+=cont->pts[p].z;
  z/=(float)cont->psize;
  z+=0.5f;
  p = z;
  return(p);
}

static int mesh_open_tube_obj(Iobj *obj, Ipoint *scale, unsigned int flags)
{
  Imesh *nmesh;
  Icont *cont;
  Icont *clst;
  Ipoint nrot;
  int co, pt, npt, ppt, slices;
  float tubeDiameter = obj->linewidth;

  /* DNM 8/25/03: Set tube diameter here instead of in mkTubeCont and make 
     number of segments variable within limits */
  if (meshDiameterSize > 0.0) 
    tubeDiameter = meshDiameterSize;

  slices = tubeDiameter / 2;
  if (slices < 12) 
    slices = 12;
  if (slices > 50)
    slices = 50;

  obj->meshsize = 0;
  obj->mesh = NULL;
     
  for(co = 0; co < obj->contsize; co++){
    cont = &obj->cont[co];
    if (cont->psize < 2)
      continue;
    clst = imodContoursNew(cont->psize);
	  
    for(pt = 0; pt < cont->psize; pt++){

      /* DNM: simplify and include scale in normal calculation */
	       
      ppt = pt - 1;
      if (ppt < 0)
	ppt = 0;
      npt = pt + 1;
      if (npt == cont->psize)
	npt = pt;

      nrot.x = scale->x * (cont->pts[npt].x - cont->pts[ppt].x);
      nrot.y = scale->y * (cont->pts[npt].y - cont->pts[ppt].y);
      nrot.z = scale->z * (cont->pts[npt].z - cont->pts[ppt].z);

      imodPointNormalize(&nrot);
      mkTubeCont(&clst[pt], &cont->pts[pt], &nrot, scale,
	     tubeDiameter, slices);

    }
	  
    for(pt = 0; pt < cont->psize - 1; pt++){
      nrot.x = scale->x * (cont->pts[pt+1].x - cont->pts[pt].x);
      nrot.y = scale->y * (cont->pts[pt+1].y - cont->pts[pt].y);
      nrot.z = scale->z * (cont->pts[pt+1].z - cont->pts[pt].z);

      imodPointNormalize(&nrot);
      nmesh = joinTubeCont(&clst[pt], &clst[pt+1], &nrot);
      if (nmesh){
	obj->mesh = imodel_mesh_add(nmesh, obj->mesh, &(obj->meshsize));
	free(nmesh);
      }
    }

    /* DNM 8/25/02: cap if flag is set*/
    if (flags & IMESH_CAP_TUBE) {
      nmesh = makeCapMesh(&clst[0], &cont->pts[0], 1);
      if (nmesh){
	obj->mesh = imodel_mesh_add(nmesh, obj->mesh, &(obj->meshsize));
	free(nmesh);
      }
      nmesh = makeCapMesh(&clst[cont->psize - 1], &cont->pts[cont->psize - 1], 
			  0);
      if (nmesh){
	obj->mesh = imodel_mesh_add(nmesh, obj->mesh, &(obj->meshsize));
	free(nmesh);
      }
    }
	  
    imodContoursDelete(clst, cont->psize);
  }
     
    
  obj->mesh = imeshReMeshNormal(obj->mesh, &(obj->meshsize), scale, 0);
  if (!obj->mesh)
    obj->meshsize = 0;
     
  return(0);
}

static float segment_separation(float l1, float u1, float l2, float u2)
{
  int minlen;
  minlen = u1 - l1;
  if (u2 - l2 < minlen)
    minlen = u2 - l2;
  if ((l1 >= l2 && u1 <= u2) || (l2 >= l1 && u2 <= u1))
    return (0.);
  if (l1 > u2)
    return (minlen + l1 - u2);
  if (l2 > u1)
    return (minlen + l2 - u1);
	  
  if (u1 > u2) {
    minlen = u1 - u2;
    if (l1 - l2 < minlen)
      minlen = l1 - l2;
  } else {
    minlen = u2 - u1;
    if (l2 - l1 < minlen)
      minlen = l2 - l1;
  }
  return (minlen);
}

/* connect open contours like a surface. */
static int mesh_open_obj(Iobj *obj, Ipoint *scale, int incz, 
			 unsigned int flags,
			 int zmin, int zmax, int *contz, int *zlist, 
			 int zlsize, int *numatz, int **contatz, Ipoint *pmin, 
			 Ipoint *pmax)
{
  Imesh *nmesh = NULL;
  Icont *cont, *econt;
  int co,eco, nextz, iz, nextiz, i, j, k, indmin, needmat, matsize;
  float *sepmat;
  float minsep, xlap, ylap;
  int inside = 0;

  if (obj->flags & IMOD_OBJFLAG_OUT)
    inside = 1;

  fastmesh = 1;

  co = (obj->contsize + zmax - zmin) / (zmax + 1 - zmin);
  matsize = co * co;
  sepmat = (float *)malloc(matsize * sizeof(float));
     
  for (iz = 0; iz < zmax - zmin; iz++) {
    if (flags & IMESH_MK_SKIP){
      nextz = getnextz(zlist, zlsize, iz + zmin);
    }else{
      nextz = iz + zmin + incz;
    }
    nextiz = nextz - zmin;
    needmat = numatz[iz] * numatz[nextiz];
    if (needmat > matsize) {
      sepmat = (float *)realloc(sepmat, needmat * sizeof(float));
      matsize = needmat;
    }

    for (i = 0; i < numatz[iz]; i++) {
      co = contatz[iz][i];
      cont = &(obj->cont[co]);
      for (j = 0; j < numatz[nextiz]; j++) {
	eco = contatz[nextiz][j];
	econt = &(obj->cont[eco]);
	sepmat[i + j * numatz[iz]] = -1.;
	if (!cont->psize)
	  continue;
	if (!econt->psize)
	  continue;
	if ((flags & IMESH_MK_SURF) && (cont->surf != econt->surf))
	  continue;
	if ((flags & IMESH_MK_TIME) && (cont->type != econt->type))
	  continue;

	/* for a valid pair, compute a separation factor */
	xlap = segment_separation(pmin[co].x, pmax[co].x,
				  pmin[eco].x, pmax[eco].x);
	ylap = segment_separation(pmin[co].y, pmax[co].y,
				  pmin[eco].y, pmax[eco].y);
	if (ylap > xlap)
	  xlap = ylap;
	sepmat[i + j * numatz[iz]] = xlap;
      }
    }

    for (;;) {

      /* Loop: find pair with minimum separation and mesh them */
      minsep = 1.e30;
      for (i = 0; i < needmat; i++)
	if (sepmat[i] >= 0. && sepmat[i] < minsep) {
	  minsep = sepmat[i];
	  indmin = i;
	}

      /* Done if no more pairs available */
      if (minsep > 1.e20)
	break;
      i = indmin % numatz[iz];
      j = indmin / numatz[iz];
      co = contatz[iz][i];
      cont = &(obj->cont[co]);
      eco = contatz[nextiz][j];
      econt = &(obj->cont[eco]);
      cont->flags |= ICONT_OPEN;
      econt->flags |= ICONT_OPEN;
      nmesh = imeshContoursCost(cont, econt, scale, inside, 1);
      if (nmesh){
	nmesh->pad = cont->surf;
	if (!(flags & IMESH_MK_SURF))
	  nmesh->pad = 0;
	nmesh->type = cont->type;
	if (!(flags & IMESH_MK_TIME))
	  nmesh->type = 0;
	obj->mesh = imodel_mesh_add
	  (nmesh, obj->mesh, &(obj->meshsize));
	free(nmesh);
      }

      /* mark all pairs involving these two conts as unavailable */
      for (k = 0; k < numatz[nextiz]; k++)
	sepmat[i + k * numatz[iz]] = -1.;
      for (k = 0; k < numatz[iz]; k++)
	sepmat[k + j * numatz[iz]] = -1.;
    }
  }

  if (flags & IMESH_MK_NORM){
    obj->mesh = imeshReMeshNormal(obj->mesh, &(obj->meshsize), scale, 0);
    if (!obj->mesh)
      obj->meshsize = 0;
  } 

  for (i = 0; i < zmax + 1 - zmin; i++) {
    if (numatz[i])
      free (contatz[i]);
  }
  free(sepmat);
  free(numatz);
  free(contatz);
  free(contz);
  free(zlist);
  free(pmin);
  free(pmax);
  return(0);
}

/* make a contour for tube mesh. */
static int mkTubeCont(Icont *cont, Ipoint *loc, Ipoint *n, Ipoint *scale,
		  float tubeDiameter, int slices)
{
  Ipoint spt, tpt, cpt;
  Imat *mat = imodMatNew(3);
  Imat *rmat = imodMatNew(3);
  double astep = 360.0 / (double)slices;
  double a = 0.0, b = 0.0;
  int sl;

  Ipoint rscale;

  /*     printf("mkcont: norm = %g %g %g\n",
	 n->x, n->y, n->z);
  */

  rscale.x = 1.0f/scale->x;
  rscale.y = 1.0f/scale->y;
  rscale.z = 1.0f/scale->z;


  b = acos((double)n->z);
  b *= 57.29578;

  /* DNM: modify method of getting rotation matrix to end up in correct
     quadrant, using atan2 */

  imodMatRot(mat, b, b3dY);
  a = atan2((double)n->y, (double)n->x);
  a *= 57.29578;
  imodMatRot(mat, a, b3dZ);


  spt.x = 0.0f;
  spt.y = tubeDiameter * 0.5f;
  spt.z = 0.0f;

  imodMatScale(mat, &rscale);  /* DNM: Move this outside the loop */
  for(sl = 0; sl < slices; sl++){
    imodMatRot(rmat, astep, b3dZ);
    imodMatTransform(rmat, &spt, &tpt);
    imodMatTransform(mat, &tpt, &cpt);

    cpt.x += loc->x;
    cpt.y += loc->y;
    cpt.z += loc->z;
    imodPointAppend(cont, &cpt);
  }
  imodMatDelete(mat);
  imodMatDelete(rmat);
  return(0);
}

/* Find point at which back-transformed circle reaches its top, and return 0
   for ccw or 1 for cw circle */

static int circle_top_and_direction(Icont *cont, Imat *mat, int *ptop)
{
  int pt, pt2;
  int reverse = 0;
  float ytop, xtop;
  Ipoint tpt;

  /* Find pt at which Y reaches a maximum */
  for (pt = 0; pt <= cont->psize; pt++) {
    pt2 = pt;
    if (pt2 == cont->psize)
      pt2 = 0;
    imodMatTransform(mat, &cont->pts[pt2], &tpt);
    if (pt == 0 || tpt.y > ytop) {
      ytop = tpt.y;
      xtop = tpt.x;
      *ptop = pt2;
    }
    /* if the LAST point was a maximum, determine whether there's
       a reversal based on change in X */
    if (pt == *ptop + 1)
      reverse = tpt.x > xtop ? 1 : 0;
  }
  return reverse;
}
	       
/* join two tube contours together. */
static Imesh *joinTubeCont(Icont *c1, Icont *c2, Ipoint *norm)
{
  int pt, mpt, npt, pt2, pt1, idir2;
  Imat *mat = imodMatNew(3);
  Imesh *mesh = imodMeshNew();
  Ipoint tpt, lpt;
  double a, b;
  int invert1 = 0, invert2 = 0, reverse1 = 0, reverse2 = 0;

  mpt = c1->psize;

  /* Get matrix that rotates this central normal to Z axis */
  b = acos((double)norm->z);
  b *= 57.29578;
  a = atan2((double)norm->y, (double)norm->x);
  a *= 57.29578;
  imodMatRot(mat, -a, b3dZ);
  imodMatRot(mat, -b, b3dY);


  /* Start at top point of each back-transformed circle */

  reverse1 = circle_top_and_direction(c1, mat, &pt1);
  reverse2 = circle_top_and_direction(c2, mat, &pt2);

  /* and go backwards for second contour if reverse flag doesn't match */

  idir2 = (reverse1 == reverse2) ? 1 : -1;

  for (pt = 0 ; pt < mpt; pt++){

    imodMeshAddVert(mesh, &c1->pts[pt1]);
    imodMeshAddVert(mesh, &c2->pts[pt2]);

    pt1++;
    if (pt1 == mpt)
      pt1 = 0;

    pt2 += idir2;
    if (pt2 == mpt)
      pt2 = 0;
    if (pt2 < 0)
      pt2 = mpt - 1;
	  
    npt = pt+1;
    if (npt == mpt)
      npt = 0;

    imodMeshAddIndex(mesh, IMOD_MESH_BGNPOLY);

    imodMeshAddIndex(mesh, (pt*2) + 1);
    imodMeshAddIndex(mesh, pt*2);
    imodMeshAddIndex(mesh, npt*2);
    imodMeshAddIndex(mesh, IMOD_MESH_ENDPOLY);

    imodMeshAddIndex(mesh, IMOD_MESH_BGNPOLY);
    imodMeshAddIndex(mesh, (pt*2) + 1);
    imodMeshAddIndex(mesh, npt*2);
    imodMeshAddIndex(mesh, (npt*2)+1);
    imodMeshAddIndex(mesh, IMOD_MESH_ENDPOLY);

  }
  imodMeshAddIndex(mesh, IMOD_MESH_END);

  imodMatDelete(mat);

  return(mesh);
}



/*
 * Build the matrices of areas for up and down triangles
 */
static void build_area_matrices(Icont *bc, int bdir,
				Icont *tc, int tdir, Ipoint *scale,
				float *up, float *down)
{
  int k, l, i, j, ni, nj, lbase, bsize, tsize, tdim, bdim;
  Ipoint *bpt, *tpt;

  bsize = bc->psize;
  tsize = tc->psize;
  bpt = bc->pts;
  tpt = tc->pts;
  bdim = bsize;
  tdim = tsize;
  if ((bc->flags & ICONT_OPEN) && (tc->flags & ICONT_OPEN)) {
    bdim--;
    tdim--;
  }

  i = 0;
  if (bdir < 0)
    i = bsize - 1;
  j = 0;
  if (tdir < 0)
    j = tsize - 1;

  for (l = 0; l < tsize; l++) {
    nj = j + tdir;
    if (nj == tsize)
      nj = 0;
    if (nj < 0)
      nj = tsize - 1;
    lbase = l * bsize;
    for (k = 0; k < bsize; k++) {
      ni = i + bdir;
      if (ni == bsize)
	ni = 0;
      if (ni < 0)
	ni = bsize - 1;
      if (k == bdim)
	up[k + lbase] = 0.;
      else
	up[k + lbase] = imodPointAreaScale(&tpt[j], &bpt[i], 
					   &bpt[ni], scale);
      if (l == tdim)
	down[k + lbase] = 0.;
      else
	down[k + lbase] = imodPointAreaScale(&tpt[j], &tpt[nj], 
					     &bpt[i], scale);
      i = ni;
    }
    j = nj;
  }
}

/*
 * Compute the minimum area path to every possible connection, thus allowing
 * one to follow a minimum area path from ending to starting connection
 */
static void cost_from_area_matrices(float *up, float *down, float *cost,
				    char *path, int bdim, int tdim,
				    int bsize, int sb, int st, float curmin)
{
  int i, j, k, l, jl, il, ind, lbase;
  float rowmin, costup, costdown;

  cost[0] = 0;
  j = st;
  if (j == tdim)
    j = 0;
  rowmin = 0.0;
  for (l = 0; l <= tdim; l ++) {
    lbase = l * (bdim + 1);
    i = sb;
    if (i == bdim)
      i = 0;
    for (k = 0; k <= bdim; k++) {
      ind = k + lbase;
      if (!k) {

	/* If in first column, add area of down triangles */
	if (l) {
	  cost[ind] = cost[ind - (bdim + 1)] + 
	    down[i + jl * bsize];
	  path[ind] = 0;
	  rowmin = cost[ind];
	}
      } else {
	if (!l) {

	  /* If in first row, add area of up triangles */
	  cost[ind] = cost[ind - 1] + up[il + j * bsize];
	  path[ind] = 1;
	} else {

	  /* Otherwise figure out which is smaller and add area
	     and save path direction for that */
	  costdown = cost[ind - (bdim + 1)] + 
	    down[i + jl * bsize];
	  costup = cost[ind - 1] + up[il + j * bsize];

	  if (costdown < costup) {
	    cost[ind] = costdown;
	    path[ind] = 0;
	  } else {
	    cost[ind] = costup;
	    path[ind] = 1;
	  }
	  /* keep track of minimum along the row */
	  if (cost[ind] < rowmin)
	    rowmin = cost[ind];
	}
      }

      /* adjust row index into area matrices */
      il = i;
      i++;
      if (i == bdim)
	i = 0;
    }

    /* adjust column index into area matrices */
    jl = j;
    j++;
    if (j == tdim)
      j = 0;

    /* If there is a current minimum and row min exceeds it, abort */
    if (curmin >= 0.0 && rowmin > curmin) {
      cost[bdim + tdim * (bdim + 1)] = rowmin;
      return;
    }
  }
}

static void chunkAddTriangle(Imesh *mesh, int i1, int i2, int i3, int *maxsize,
			     int inside)
{
  int o2 = i2;
  int o3 = i3;
  if (inside) {
    o2 = i3;
    o3 = i2;
  }
  chunkMeshAddIndex(mesh, IMOD_MESH_BGNPOLY, maxsize);
  chunkMeshAddIndex(mesh, i1, maxsize);
  chunkMeshAddIndex(mesh, o2, maxsize);
  chunkMeshAddIndex(mesh, o3, maxsize);
  chunkMeshAddIndex(mesh, IMOD_MESH_ENDPOLY, maxsize);
}

/* Create a mesh between two contours using a minimum area
 * cost analysis.
 */
static Imesh *imeshContoursCost(Icont *bc, Icont *tc, Ipoint *scale, 
				int inside, int openobj)
{
  Imesh *mesh;
  int i, j, pt, jo;
  int tdim, bdim;
  int bsize, tsize, csize, totind;
  int dofast;
  int bsi, tsi, li, lj, step, iskip, jskip, bothsame;
  int maxsize;
  float mincost, ccost;
  float *up, *down, *cost;
  char *path;

  /* Index [0] is bottom contour, Index [1] is top contour. */
  int si[2];                /* start index */
  int direction[2];        /* contour direction. */
     
  /* Check input data. */
  if ((bc == NULL) || (!bc->psize) || (tc == NULL) || (!tc->psize))
    return(NULL);

  /* Init internal data. */
  mesh = imodMeshNew();
  if (!mesh) return(NULL);
  maxsize = 0;

  tsize = tc->psize;
  tdim = tsize;
  bsize = bc->psize;
  bdim = bsize;
  if (tc->flags & ICONT_OPEN && bc->flags & ICONT_OPEN) {
    tdim--;
    bdim--;
  }
  csize = (tdim + 1) * (bdim + 1);
  totind = csize - 1;

  up = (float *)malloc(sizeof(float) * bsize * tsize);
  down = (float *)malloc(sizeof(float) * tsize * bsize);
  cost = (float *)malloc(sizeof(float) * csize);
  path = (char *)malloc(sizeof(char) * csize);
  if (!down || !up || !cost || !path) {
    free(mesh);
    if (up) free(up);
    if (down) free(down);
    if (cost) free(cost);
    if (path) free(path);
    return(NULL);
  }

     
  /* Find direction of each contour.
   * If contours aren't flat in a Z plane, they must already be
   * in the correct orientation.
   */

  direction[0] = imodContZDirection(bc);
  direction[1] = imodContZDirection(tc);
  if (!direction[0]) direction[0] = 1;
  if (!direction[1]) direction[1] = 1;


  si[0] = si[1] = 0;
  dofast = 1;
  bothsame = 1;

  /* Set starting index for both contours. */
  {
    Ipoint minp, maxp;
    int ti, mti = 0;
    float mdist = imodPointDistance(bc->pts, tc->pts);
    float dist;

    if ((bc->flags & ICONT_OPEN)){
      si[0] = 0;

      if ((tc->flags & ICONT_OPEN)){
	float dista, distb;
	si[1] = 0;
	dofast = fastmesh;
	/* If the object type is open contour, then ignore the
	   computed contour directions; set directions to +, and
	   invert the second one if that makes them match up 
	   better */
	if (openobj) {
	  direction[1] = direction[0] = 1;
	  dista = imodPointDistance(bc->pts, &tc->pts[0]);
	  distb = imodPointDistance(bc->pts, 
				    &tc->pts[tc->psize-1]);
	  if (distb < dista){
	    si[1] = tc->psize-1;
	    direction[1] = -1;
	  }
	}
      }else {
	si[1] = imodContourNearest(tc, bc->pts);
	bothsame = 0;
      }
	       
    }else{
	       
      if ((tc->flags & ICONT_OPEN)){
	si[1] = 0;
	si[0] = imodContourNearest(bc, tc->pts);
	bothsame = 0;
      }else{
	dofast = fastmesh;
		    
	/* Try to have all mesh start at about the same place
	 * so it looks better with fake transperentcy.
	 */
	imodel_contour_mm(bc, &maxp, &minp);
	si[0] = imodContourNearest(bc, &minp);
		    
	/* Now find a similar point in the top contour. */
		    
	imodel_contour_mm(tc, &maxp, &minp);
	si[1] = imodContourNearest(tc,  &minp); 

	/* DNM: this won't work any better than going for corner
	   points: basically, need to do two passes to have a good
	   starting connector  */
	/*   mti = 0;
	     mdist = imodPoint3DScaleDistance
	     (&bc->pts[si[0]], tc->pts, scale);
		    
	     for(ti = 0; ti < tc->psize; ti++){
	     dist = imodPoint3DScaleDistance
	     (&bc->pts[si[0]], &tc->pts[ti], scale);
	     if (dist < mdist){
	     mti = ti;
	     mdist = dist;
	     }
	     }
	     si[1] = mti;
	*/
      }
    }
  }
     
  /*
   * Build the matrices of areas for up and down triangles
   */
  build_area_matrices(bc, direction[0], tc, direction[1], scale, up, down);

  /* flip starting indexes if directions are reversed */
  bsi = si[0];
  if (direction[0] < 0)
    bsi = bsize - 1 - bsi;
  tsi = si[1];
  if (direction[1] < 0)
    tsi = tsize - 1 - tsi;


  /* Get the cost and path matrices for these starting points */

  cost_from_area_matrices(up, down, cost, path, bdim, tdim,
			  bsize, bsi, tsi, -1.0);
       
  if (dofast) {

    if (!(bc->flags & ICONT_OPEN) && 
	!(tc->flags & ICONT_OPEN)) {

      /* If both closed contours, go halfway around to find a
	 new starting point, and use that instead */
      i = bdim;
      j = tdim;
      for (step = 0; step < (bdim + tdim) / 2; step++) {
	if (path[i + j * (bdim + 1)])
	  i--;
	else
	  j--;
      }
      bsi = (bsi + i) % bsize;
      tsi = (tsi + j) % tsize;
      cost_from_area_matrices(up, down, cost, path, bdim, tdim,
			      bsize, bsi, tsi, -1.0);
    } 
  } else {
       
    /* Time-consuming: find starting point that gives minimum  area */
     
    mincost = cost[totind];

    if (tc->flags & ICONT_OPEN  || bc->flags & ICONT_OPEN){
      /* For open contours, just do reverse direction */

      build_area_matrices(bc, direction[0], tc, -direction[1],
			  scale, up, down);
      cost_from_area_matrices(up, down, cost, path, bdim, tdim,
			      bsize, bsi, tsi, mincost);

      /* If it's better, flip direction; otherwise rebuild areas */
      if (cost[totind] < mincost)
	direction[1] *= -1;
      else
	build_area_matrices(bc, direction[0], tc, direction[1],
			    scale, up, down);
    } else {
	  
      for(i = 0; i < tc->psize; i++){
	cost_from_area_matrices(up, down, cost, path, bdim, tdim,
				bsize, bsi, i, mincost);
		 
	ccost = cost[totind];
		    
	if (ccost < mincost){
	  mincost = ccost;
	  tsi = i;
	  /*		 printf ("* "); */
	}
	/*	     printf("%d %d %g\n",si[0], i, ccost); */
      }
    }

    /* Redo path from final starting indices and direction */
    cost_from_area_matrices(up, down, cost, path, bdim, tdim,
			    bsize, bsi, tsi, -1.0);
  }

  /* get back the starting indexes in terms of unreversed data, unless 
     they're both open contours, where we want to start at the high end */
  if (!((bc->flags & ICONT_OPEN) && (tc->flags & ICONT_OPEN))) {
    if (direction[0] < 0)
      bsi = bsize - 1 - bsi;
    if (direction[1] < 0)
      tsi = tsize - 1 - tsi;
  }
  si[0] = bsi;
  si[1] = tsi;

  free(cost);
  free(up);
  free(down);

  /*
   * copy contour points to mesh vert array
   */
  mesh->vsize = bc->psize + tc->psize + 2;
  mesh->vert = (Ipoint *)malloc(mesh->vsize * sizeof(Ipoint));
  if (mesh->vert == NULL){
    free(mesh);
    free(path);
    return(NULL);
  }
  /* copy bottom contour data to mesh array. */
  pt = si[0];
  for(i = 0; i < bsize; i++){
    mesh->vert[i] = bc->pts[pt];
    pt += direction[0];
    if (pt == bsize) {
      pt = 0;
      iskip = i;
    } else if (pt < 0) {
      pt = bsize - 1;
      iskip = i;
    }
  }
  mesh->vert[bsize] = bc->pts[si[0]];
     
  /* copy top contour data to mesh vert array. */
  for (i = bsize + 1, pt = si[1]; i < mesh->vsize - 1; i++){
    mesh->vert[i] = tc->pts[pt];
    pt += direction[1];
    if (pt == tsize) {
      pt = 0;
      jskip = i - (bsize + 1);
    } else if (pt < 0) {
      pt = tsize - 1;
      jskip = i - (bsize + 1);
    }
  }
  mesh->vert[mesh->vsize - 1] = tc->pts[si[1]];

  /*     if (bothsame)
	 jskip = iskip = -1; */
     
  /* The connection loop.             */
  /* Now make the mesh by following the path. */
  i = bdim;
  j = tdim;
  jo = bsize + 1;

  while( (i) || (j) ){

    if (path[i + j * (bdim + 1)]){
      li = i - 1;
      if (li < 0)
	li = bsize - 1;
      if (!(bc->flags & ICONT_OPEN) || li != iskip)
	chunkAddTriangle(mesh, j+jo, li, i, &maxsize, inside);
      i--;
    }else{
      lj = j - 1;
      if (lj < 0)
	lj = tsize - 1;
      if (!(tc->flags & ICONT_OPEN) || lj != jskip)
	chunkAddTriangle(mesh, lj+jo, i, j+jo, &maxsize, inside);
      j--;
    }
	  
  }

  free(path);

  imodMeshAddIndex(mesh, IMOD_MESH_END);
  return(mesh);
}
static void dump_lists(char *message, int *blist, int nb, int *tlist, int nt,
		       int *blook, int *tlook)
{
  int i;
  printf("%s:\nbottom: ", message);
  for (i = 0; i < nb; i++)
    if (blook)
      printf(" %d", blook[blist[i]]+1);
    else
      printf(" %d", blist[i]+1);
  printf("\ntop: ");
  for (i = 0; i < nt; i++)
    if (tlook)
      printf(" %d", tlook[tlist[i]]+1);
    else
      printf(" %d", tlist[i]+1);
  printf("\n");
}



#define CONNECT_TOP     ICONT_CONNECT_TOP
#define CONNECT_BOTTOM  ICONT_CONNECT_BOTTOM
#define CONNECT_BOTH    (ICONT_CONNECT_TOP & ICONT_CONNECT_BOTTOM)

#define connectTop(f)    ((f) & CONNECT_TOP)
#define connectBottom(f) ((f) & CONNECT_BOTTOM)
#define connectBoth(f)   ((connectTop(f)) && (connectBottom(f)))

static int numwarn = 0;

int SkinObject
(Iobj *obj,      /* The object to be skinned.            */
 Ipoint *scale,  /* Scaling used for normal calculation. */
 double overlap, /* Overlap percentage.                  */
 int cap,        /* cap ends of surface.                 */
 int *cap_skip_zlist,  /* List of Z values to not cap */
 int cap_skip_nz,      /* Number of Z values to not cap */
 int incz,             /* increment in z values */
 unsigned int flags,
 int skipPasses,      /* Number of passes for skipped sections */
 int (*inCB)(int))
{
  int co, tco, m;
  Imesh *nmesh = NULL;
  Icont *cont, *tcont, *bcont, *tempcont, *jcont;
  Icont **scancont;
  int *czl, *nzl, zmin,zmax;
  int i, j, czs, nzs;
  int found_data;
  int *zlist;
  int zlsize = 0;
  int nextz, prevz, z;
  int direction;
  int notop = 0, nobot = 0, nocon = 0;
  int foundInside, nInside;
  int cz;

  int (*cb)(int);
  int status;

  int *blist, *tlist, *contz;
  Ipoint *pmin, *pmax;
  int nbot, ntop, lind, inl;
  int *numatz;
  int **contatz;
  int indz, lis, kis;
  int numnests = 0;
  Icont *econt;  /* The contour that will be tested    */
  int eco;       /* for being eaten all up.            */
  float frac1, frac2;
  Nesting *nest, *nests;
  int *nestind;
  int inco, outco, level, more, ready, nind, oind, zpass;

  /* start the timer if doing reports */
  timeval1 = clock();

  /* Check that the input object is skinable.
   */
  if (obj == NULL)
    return(-1);
  if (obj->flags & IMOD_OBJFLAG_SCAT)
    return(-1);     

  if (!iobjClose(obj->flags) && (flags & IMESH_MK_TUBE) )
    return(mesh_open_tube_obj(obj, scale, flags));

  if (flags & IMESH_MK_FAST)
    fastmesh = 0;
  else
    fastmesh = 1;

  if (!obj->contsize)
    return(-1);
  obj->mesh = NULL;
  obj->meshsize = 0;
     
  /* Find min and max z values.
   * Clear the type value used to store connection information.
   */
  contz = (int *)malloc(obj->contsize * sizeof(int));
  if (!contz)
    return(-1);
  zmin = INT_MAX;
  zmax = INT_MIN;
  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    cz = getContZValue(cont);
    contz[co] = cz;
    cont->flags &= ~CONNECT_BOTH;
    if (cont->psize){
      if (cz < zmin)
	zmin = cz;
      if (cz > zmax)
	zmax = cz;
    }
  }

  /* get list of z sections to connect for skip */
  zlist = (int *)malloc((zmax - zmin + 1) * sizeof(int));
  numatz = (int *)malloc((zmax - zmin + 1 + incz) * sizeof(int));
  contatz = (int **)malloc((zmax - zmin + 1) * sizeof(int *));
  if (!zlist || !numatz || !contatz)
    return(-1);
  for (z = 1; z <= incz; z++)
    numatz[zmax + z - zmin] = 0;
  for(z = zmin; z <= zmax; z++){
    numatz[z - zmin] = 0;
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      cz = contz[co];
      if (cont->psize && cz == z){
	zlist[zlsize++] = z;
	break;
      }
    }
  }

  /* Get number of contours at each z and tables of each */
  for(co = 0; co < obj->contsize; co++)
    if (obj->cont[co].psize)
      numatz[contz[co] - zmin]++;
    

  for (i = 0; i < zmax + 1 - zmin; i++) {
    contatz[i] = (int *)malloc(numatz[i] * sizeof(int));
    if (!contatz[i])
      return(-1);
    numatz[i] = 0;
  }
     			     
  for(co = 0; co < obj->contsize; co++) {
    if (obj->cont[co].psize) {
      i = contz[co] - zmin;
      contatz[i][numatz[i]++] = co;
    }
  }

  /* Allocate space for lists of min's max's, and found contours */

  pmin = (Ipoint *)malloc(obj->contsize * sizeof(Ipoint));
  pmax = (Ipoint *)malloc(obj->contsize * sizeof(Ipoint));
  if (!pmin || !pmax)
    return(-1);

  for(co = 0; co < obj->contsize; co++)
    if (obj->cont[co].psize)
      imodel_contour_mm(&(obj->cont[co]), &(pmax[co]), &(pmin[co]));

  if (!iobjClose(obj->flags))
    return(mesh_open_obj(obj, scale, incz, flags, zmin, zmax, contz, 
			 zlist, zlsize, numatz, contatz, pmin, pmax));

  blist = (int *)malloc(obj->contsize * sizeof(int));
  tlist = (int *)malloc(obj->contsize * sizeof(int));
  if (!blist || !tlist)
    return(-1);

  /* Tell callback function we are just getting started. */
  if (inCB) cb = inCB; else cb = imeshDefaultCallback;
  status = (*cb)(1);
  if (status) return(status);

  /*
   * Prescan contours for faster rendering.
   */
  scancont = (Icont **)malloc(obj->contsize * sizeof (Icont *)); 
  if (!scancont) return(-1);
  for(co = 0; co < obj->contsize; co++)
    scancont[co] = imodel_contour_scan(&(obj->cont[co]));

  report_time("Got scan contours");
     
  /* Get array for pointers to inside/outside information */
  nestind = (int *)malloc(obj->contsize * sizeof(int));
  if (!nestind)
    return(-1);
  for(co = 0; co < obj->contsize; co++)
    nestind[co] = -1;


  /*
   *  Mark all contours with no data as connected.
   *  Build lists of inside and outside contours.
   */
     
  for (indz = 0; indz < zmax + 1 - zmin; indz++) {
    for (kis = 0; kis < numatz[indz] - 1; kis++) {
      co = contatz[indz][kis];
      cont = &(obj->cont[co]);
      if (!cont->psize)
	cont->flags |= CONNECT_BOTH;
      if (cont->flags & CONNECT_BOTH)
	continue;
      for (lis = kis + 1; lis < numatz[indz]; lis++) {
	eco = contatz[indz][lis];
	econt = &(obj->cont[eco]);
	if (!econt->psize)
	  econt->flags |= CONNECT_BOTH;
	if (econt->flags & CONNECT_BOTH)
	  continue;
	if ((flags & IMESH_MK_TIME) && (cont->type != econt->type))
	  continue;
	imodel_overlap_fractions(scancont[co], pmin[co], pmax[co],
				 scancont[eco], pmin[eco],
				 pmax[eco], &frac1, &frac2);
	if (frac1 > 0.99 || frac2 > 0.99) {
	  if (frac2 > frac1) {
	    inco = eco;
	    outco = co;
	  } else {
	    inco = co;
	    outco = eco;
	  }
	  /* add outside one to the inside's lists;
	     create them new list if necessary */
	  nind = nestind[inco];
	  if (nind < 0) {
	    if (numnests)
	      nests = (Nesting *)realloc(nests, 
					 (numnests + 1 )* sizeof(Nesting));
	    else
	      nests = (Nesting *)malloc(sizeof(Nesting));
	    nind = numnests++;
	    nestind[inco] = nind;
	    nest = &nests[nind];
	    nest->co = inco;
	    nest->level = 0;
	    nest->ninside = 0;
	    nest->noutside = 0;
	  } else
	    nest = &nests[nind];

	  if (nest->noutside)
	    nest->outside = (int *)realloc(nest->outside,
					   (nest->noutside + 1) * sizeof(int));
	  else
	    nest->outside = (int *)malloc(sizeof(int));
	  nest->outside[nest->noutside++] = outco;
			 
	  /* now add inside one to outside's list */
	  nind = nestind[outco];
	  if (nind < 0) {
	    if (numnests)
	      nests = (Nesting *)realloc(nests,
					 (numnests + 1 )*sizeof(Nesting));
	    else
	      nests = (Nesting *)malloc(sizeof(Nesting));
	    nind = numnests++;
	    nestind[outco] = nind;
	    nest = &nests[nind];
	    nest->co = outco;
	    nest->level = 0;
	    nest->ninside = 0;
	    nest->noutside = 0;
	  } else
	    nest = &nests[nind];

	  if (nest->ninside)
	    nest->inside = (int *)realloc(nest->inside,
					  (nest->ninside + 1) * sizeof(int));
	  else
	    nest->inside = (int *)malloc(sizeof(int));
	  nest->inside[nest->ninside++] = inco;
			   
	} else if (frac1 > 0.1 || frac2 > 0.1) {
	  printf("WARNING: Contours %d and %d overlap by "
		 "%.3f and %.3f\n", co + 1, eco + 1,
		 frac1, frac2);
	  if (!numwarn)
	    printf("To find these contours in Imod, you may "
		   "first have to remove empty contours\n "
		   "with the menu command "
		   "Edit-Object-Clean\n");
	  numwarn = 1;
	}
      }
    }
  }

  /* Analyze inside and outside contours to determine level */
  level = 1;
  do {
    more = 0;
    for (nind = 0; nind < numnests; nind++) {
      if (nests[nind].level)
	continue;
      ready = 1;
      /* if the only contours outside have level assigned but lower
	 than the current level, then this contour can be assigned to
	 the current level */
      for (i = 0; i < nests[nind].noutside; i++) {
	oind = nestind[nests[nind].outside[i]];
	if (nests[oind].level == 0 || nests[oind].level >= level) {
	  more = 1;
	  ready = 0;
	  break;
	}
      }
      if (ready) 
	nests[nind].level = level;
    }
    level++;
  } while(more);

  /* Strip the inside and outside lists down to adjacent levels only */
  /*     for (nind = 0; nind < numnests; nind++) {
	 keep = 0;
	 for (i = 0; i < nests[nind].noutside; i++) {
	 oind = nestind[nests[nind].outside[i]];
	 if (nests[nind].level + 1 == nests[oind].level)
	 nests[nind].outside[keep++] = oind;
	 }
	 nests[nind].noutside = keep;
	 keep = 0;
	 for (i = 0; i < nests[nind].ninside; i++) {
	 oind = nestind[nests[nind].inside[i]];
	 if (nests[nind].level - 1 == nests[oind].level)
	 nests[nind].inside[keep++] = oind;
	 }
	 nests[nind].ninside = keep;
	 }
  */

  /* Compose a scan contour excluding inner areas for each outer contour
     at an odd level */
  /* DNM 12/18/01: Need to include even levels also now */
  for (nind = 0; nind < numnests; nind++) {
    nest = &nests[nind];	
    if (/*(nest->level % 2) == 1 && */ nest->ninside > 0) {
      nest->inscan = imodContourDup(scancont[nest->co]);
      for (i = 0; i < nest->ninside; i++)
	subtract_scan_contours(nest->inscan, 
			       scancont[nest->inside[i]]);
    }
  }

     

  report_time("Marked contours");

  status = (*cb)(25); /* quarter of the way done. */     
  /*status = (*cb)(1); ? */
  if (status) return(status);
     
  /* Do multiple passes for connections farther apart in Z */
  for (zpass = 1; zpass <= skipPasses; zpass++) {

    /* go through all contours and find connections */
    for (co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (!cont->psize)
	continue;
	  
      /* use type to tell if already connected. 
       * type = 0 (contour is not connected to any other contour. 
       * type = 1 connected on top.            
       * type = 2 connected on bottom.         
       * type = 3 connected on top and bottom. 
       *  connect from top of current, bottom contour to other
       *  contours on top.
       */
      if (connectTop(cont->flags))
	continue;

      /*  Set the connection on top and define the top contour
       *  as empty.  We are going to find a top contour (tcont) to connect
       *  to this contour (cont) if we can.
       */
      cont->flags |=  CONNECT_TOP;

      /* The current contour is now the bottom contour (bcont)    */
      bcont = cont;

      /* define z-sections that connections will be made to; step farther
	 in Z for each pass */
      nextz = contz[co];
      for (inl = 0; inl < zpass; inl++) {
	if (flags & IMESH_MK_SKIP){
	  nextz = getnextz(zlist, zlsize, nextz);
	}else{
	  nextz += incz;
	}
	if (nextz > zlist[zlsize - 1])
	  break;
      }
      prevz = contz[co];
      found_data = 1;

      nbot = 1;
      ntop = 0;
      blist[0] = co;
      foundInside = 0;
      if (nestind[co] >= 0) {
					
	/* If inside/outside, find outermost */
	foundInside = 1;
	add_whole_nest(nestind[co], obj, nests, nestind, CONNECT_TOP,
		       blist, &nbot);
      }
	  
      /* find all contours that link to current contour.
       * If we find a contour we have to look again, because
       * we may have to join contours to the one we just found.
       */

      while(found_data){
	found_data = 0;
	       
	for (lis = 0; lis < numatz[nextz - zmin]; lis++) {
	  tco = contatz[nextz - zmin][lis];
		    
	  /* The current contour we are going to try to join. */
	  jcont = &(obj->cont[tco]);
		    
	  /* can this be a top contour? 
	   * if so add it to top list.
	   */
	  /* Skip contours that are already connected */
	  if (!jcont->psize)
	    continue;
	  if (connectBottom(jcont->flags))
	    continue;
	  if (flags & IMESH_MK_SURF)
	    if (bcont->surf != jcont->surf)
	      continue;
	  if (flags & IMESH_MK_TIME)
	    if (bcont->type != jcont->type)
	      continue;
			 
	  /* See if it overlaps any contour in bottom list */
	  for (inl = 0; inl < nbot; inl++) {
	    lind = blist[inl];
	    if (imodel_scans_overlap(scancont[lind],
				     pmin[lind], pmax[lind],
				     scancont[tco],
				     pmin[tco], pmax[tco])) {
	      if (overlap != 0.0){
		/* contours must overlap by given fraction*/
		imodel_overlap_fractions(scancont[lind],
					 pmin[lind], pmax[lind],
					 scancont[tco],
					 pmin[tco], pmax[tco],
					 &frac1, &frac2);
		if (frac1 <= overlap && frac2 <= overlap)
		  continue;
	      }
	      found_data = 1;
	      jcont->flags |= CONNECT_BOTTOM;
	      tlist[ntop++] = tco;
	      if (nestind[tco] >= 0) {

		/* If inside/outside, find outermost */
		foundInside = 1;
		add_whole_nest(nestind[tco], obj, nests, 
			       nestind, CONNECT_BOTTOM,
			       tlist, &ntop);
	      }
	      break;
	    }
	  }
	}

	/* if any new top contours were found, need to scan for more
	   bottom contours */
	if (!found_data)
	  break;
	for (lis = 0; lis < numatz[prevz - zmin]; lis++) {
	  tco = contatz[prevz - zmin][lis];
		    
	  /* The current contour we are going to try to join. */
	  jcont = &(obj->cont[tco]);
		    
	  /* can this be an addition to the bottom?
	   * if so add to list on bottom
	   */
	  /* Skip contours that are already connected */
	  if (!jcont->psize)
	    continue;
	  if (connectTop(jcont->flags))
	    continue;
				   
	  if (flags & IMESH_MK_SURF)
	    if (bcont->surf != jcont->surf)
	      continue;
	  if (flags & IMESH_MK_TIME)
	    if (bcont->type != jcont->type)
	      continue;

	  /* See if it overlaps any contour in top list */
	  for (inl = 0; inl < ntop; inl++) {
	    lind = tlist[inl];
	    if (imodel_scans_overlap(scancont[lind],
				     pmin[lind], pmax[lind],
				     scancont[tco],
				     pmin[tco], pmax[tco])) {

	      if (overlap != 0.0){
		/* contours must overlap by given fraction*/
		imodel_overlap_fractions(scancont[lind],
					 pmin[lind], pmax[lind],
					 scancont[tco],
					 pmin[tco], pmax[tco],
					 &frac1, &frac2);
		if (frac1 <= overlap && frac2 <= overlap)
		  continue;
	      }
	      found_data = 1;
	      jcont->flags |= CONNECT_TOP;
	      blist[nbot++] = tco;
	      if (nestind[tco] >= 0) {
				   
		/* If inside/outside, find outermost */
		foundInside = 1;
		add_whole_nest(nestind[tco], obj, 
			       nests, nestind,
			       CONNECT_TOP, blist,
			       &nbot);
	      }
	      break;
	    }
	  }
	}
      }
	  
      if (ntop && !foundInside){
	bcont = join_all_contours(obj, blist, nbot, nbot, 1, tlist,
				  ntop, ntop, NULL);
	tcont = join_all_contours(obj, tlist, ntop, ntop, -1, blist,
				  nbot, nbot, NULL);
	if (mesh_contours(obj, bcont, tcont, cont->surf, cont->type, 
			  scale, 0))
	  return (-1);
      } else if (ntop) {
	/* There are inside contours to resolve */
	/* declarations for this big section */
	float *maxfrac, *minfrac;
	int *tinlist, *binlist, *tjustinl, *bjustinl;
	int *toutlist, *boutlist, *tused, *bused, *tlevels, *blevels;
	Icont *boutcont, *toutcont;
	Nesting *bnest;
	float fracmax;
	int imax, jmax, tlevel, blevel, nbjustin, ntjustin, found;
	int ntopout, nbotout, ntopin, nbotin, istr, icur, k;
	int ntopsame, nbotsame, oddeven;
	Ipoint bpmin, bpmax, tpmin, tpmax; 

	minfrac = (float *)malloc(ntop * nbot * sizeof(float));
	maxfrac = (float *)malloc(ntop * nbot * sizeof(float));
	tinlist = (int *)malloc(ntop * sizeof(int));
	binlist = (int *)malloc(nbot * sizeof(int));
	tjustinl = (int *)malloc(ntop * sizeof(int));
	bjustinl = (int *)malloc(nbot * sizeof(int));
	toutlist = (int *)malloc(ntop * sizeof(int));
	boutlist = (int *)malloc(nbot * sizeof(int));
	tused = (int *)malloc(ntop * sizeof(int));
	bused = (int *)malloc(nbot * sizeof(int));
	tlevels = (int *)malloc(ntop * sizeof(int));
	blevels = (int *)malloc(nbot * sizeof(int));

	if (!minfrac || !tinlist || !binlist || !tjustinl || !bjustinl
	    || !toutlist || !boutlist || !tused || !bused || !tlevels
	    || !blevels || !maxfrac) {
	  fprintf(stderr, "Fatal Error: "
		  "Not enough memory to get in/out arrays.\n");
	  return(-1);
	}
		   
	/* dump_lists("initial", blist, nbot, tlist, ntop, NULL, NULL);*/
	/* Make tables of overlap fractions between top and bottom */
	for (i = 0; i < ntop; i++) {
	  tused[i] = 0;
	  tlevels[i] = 1;
	  tcont = scancont[tlist[i]];
	  tpmin = pmin[tlist[i]];
	  tpmax = pmax[tlist[i]];
	  if (nestind[tlist[i]] >= 0) {
	    nest = &nests[nestind[tlist[i]]];
	    tlevels[i] = nest->level;
	    /* DNM 12/18/01: Need to include even levels also */
	    if (/*nest->level % 2 &&*/ nest->ninside) {
	      tcont = nest->inscan;
	      imodel_contour_mm(tcont, &tpmax, &tpmin);
	    }
	  }
	  for (j = 0; j < nbot; j++) {
	    bused[j] = 0;
	    blevels[j] = 1;
	    bcont = scancont[blist[j]];
	    bpmin = pmin[blist[j]];
	    bpmax = pmax[blist[j]];
	    if (nestind[blist[j]] >= 0) {
	      bnest = &nests[nestind[blist[j]]];
	      blevels[j] = bnest->level;
	      if (/*bnest->level % 2 &&*/ bnest->ninside) {
		bcont = bnest->inscan;
		imodel_contour_mm(bcont, &bpmax, &bpmin);
	      }
	    }
	    imodel_overlap_fractions(bcont, bpmin, bpmax,
				     tcont, tpmin, tpmax,
				     &frac1, &frac2);
	    if (frac1 > frac2) {
	      maxfrac[i * nbot + j] = frac1;
	      minfrac[i * nbot + j] = frac2;
	    } else {
	      maxfrac[i * nbot + j] = frac2;
	      minfrac[i * nbot + j] = frac1;
	    }
	  }
	}

	/* Loop, starting with the maximum overlap pair of odd levels */
	oddeven = 1;
	for (;;) {
	  fracmax = -1.;
	  for (i = 0; i < ntop; i++) {
	    if (!tused[i] && (tlevels[i] % 2 == oddeven))
	      for (j = 0; j < nbot; j++) {
		if (!bused[j] && 
		    (blevels[j] % 2 == oddeven)) {
		  frac1 = minfrac[i * nbot + j];
		  if (frac1 > fracmax) {
		    fracmax = frac1;
		    imax = i;
		    jmax = j;
		  }
		}
	      }
	  }

	  /* If there is no overlap, switch to doing even levels and
	     restart, or break out if evens are done also */
	  if (fracmax <= overlap) {
	    if (oddeven) {
	      oddeven = 0;
	      continue;
	    } else
	      break;
	  }

	  /* Start new lists with best pair, take them off original
	     lists */
	  tlevel = tlevels[imax];
	  blevel = blevels[jmax];
	  ntopout = 1;
	  nbotout = 1;
	  toutlist[0] = imax;
	  boutlist[0] = jmax;
	  tused[imax] = 1;
	  bused[jmax] = 1;
			 
	  do {
	    found_data = 0;
	    /* check every available top contour of same level
	       against the bottom list, add to top list if
	       overlap */
	    for (i = 0; i < ntop; i++) {
	      if (!tused[i] && tlevels[i] == tlevel) {
		for (j = 0; j < nbotout; j++) {
		  if (maxfrac[i * nbot + boutlist[j]]
		      > overlap) {
		    toutlist[ntopout++] = i;
		    tused[i] = 1;
		    found_data = 1;
		    break;
		  }
		}
	      }
	    }
	    /* Do the same for bottom contours against top list */
	    for (j = 0; j < nbot; j++) {
	      if (!bused[j] && blevels[j] == blevel) {
		for (i = 0; i < ntopout; i++) {
		  if (maxfrac[toutlist[i] * nbot + j] 
		      > overlap) {
		    boutlist[nbotout++] = j;
		    bused[j] = 1;
		    found_data = 1;
		    break;
		  }
		}
	      }
	    }
	  } while(found_data);

	  /*dump_lists("outside", boutlist, nbotout, toutlist, ntopout,blist, tlist);*/
	  /* Next, build lists of contours just inside 
	     this set of contours */
	  nbjustin = 0;
	  ntjustin = 0;
	  for (i = 0; i < ntop; i++) {
	    if (!tused[i] && tlevels[i] == tlevel + 1) {
	      nest = &nests[nestind[tlist[i]]];
	      for (k = 0; k < nest->noutside; k++) {
		found = 0;
		for (j = 0; j < ntopout; j++) {
		  if (nest->outside[k] == 
		      tlist[toutlist[j]]) {
		    found = 1;
		    tjustinl[ntjustin++] = i;
		    break;
		  }
		}
		if (found)
		  break;
	      }
	    }
	  }
	  for (i = 0; i < nbot; i++) {
	    if (!bused[i] && blevels[i] == blevel + 1) {
	      nest = &nests[nestind[blist[i]]];
	      for (k = 0; k < nest->noutside; k++) {
		found = 0;
		for (j = 0; j < nbotout; j++) {
		  if (nest->outside[k] == 
		      blist[boutlist[j]]) {
		    found = 1;
		    bjustinl[nbjustin++] = i;
		    break;
		  }
		}
		if (found)
		  break;
	      }
	    }
	  }
	  /*dump_lists("just inside", bjustinl, nbjustin, tjustinl, 
	    ntjustin, blist, tlist);*/
		    
	  /* Now iteratively find overlapping sets of inside conts */
	  for (istr = 0; istr < nbjustin; istr++) {
	    if (!bused[bjustinl[istr]]) {
	      ntopin = 0;
	      nbotin = 1;
	      binlist[0] = bjustinl[istr];
	      bused[bjustinl[istr]] = 1;
	      do {
		found_data = 0;
		/* check on top list of just in contours
		   for ones that overlap list of overlapping
		   bottom conts */
		for (j = 0; j < ntjustin; j++) {
		  icur = tjustinl[j];
		  if (!tused[icur])
		    for (k = 0; k < nbotin; k++) {
		      if (maxfrac[icur * nbot + 
				  binlist[k]] > 
			  overlap) {
			found_data = 1;
			tinlist[ntopin++] =icur;
			tused[icur] = 1;
			break;
		      }
		    }
		}
		/* check on bottom list of just in contours
		   for ones that overlap list of overlapping
		   top conts */
		for (j = 0; j < nbjustin; j++) {
		  icur = bjustinl[j];
		  if (!bused[icur])
		    for (k = 0; k < ntopin; k++) {
		      if (maxfrac[tinlist[k] * 
				  nbot + icur] > 
			  overlap) {
			found_data = 1;
			binlist[nbotin++]= icur;
			bused[icur] = 1;
			break;
		      }
		    }
		}
	      } while(found_data);

	      /*dump_lists("inside", binlist, nbotin, tinlist, 
		ntopin, blist, tlist);*/
	      /* If got top contours, process and mesh the
		 set of top and bottoms */
	      if (ntopin) {
		nbotsame = nbotin;
		ntopsame = ntopin;
		/* but first, look for contours just inside
		   these and add them to lists if they
		   overlap the interior area of the other
		   contours or ones already added to list */

		do {
		  found_data = 0;
		  for (i = 0; i < ntop; i++) {
		    if (tused[i] ||
			tlevels[i] != tlevel + 2)
		      continue;

		    /* see if it's inside */
		    nest = &nests[nestind[tlist[i]]];
		    found = 0;
		    for (k = 0; k < nest->noutside;
			 k++)
		      for (j = 0; j < ntopin; j++) 
			if (nest->outside[k] == 
			    tlist[tinlist[j]]) 
			  found = 1;
		    if (!found)
		      continue;

		    /* see if overlap outsides */
		    found = 0;
		    for (k = 0; k < nbotout; k++)
		      if (maxfrac[i * nbot + 
				  boutlist[k]] > 
			  overlap) 
			found = 1;

		    /* see if overlap added ones */
		    for (k = nbotsame; k < nbotin;
			 k++)
		      if (maxfrac[i * nbot + 
				  binlist[k]] > 
			  overlap)
			found = 1;
		    if (found) {
		      found_data = 1;
		      tinlist[ntopin++] =i;
		      tused[i] = 1;
		    }

		  }

		  for (i = 0; i < nbot; i++) {
		    if (bused[i] ||
			blevels[i] != blevel + 2)
		      continue;

		    /* see if it's inside */
		    nest = &nests[nestind[blist[i]]];
		    found = 0;
		    for (k = 0; k < nest->noutside;
			 k++)
		      for (j = 0; j < nbotin; j++) 
			if (nest->outside[k] == 
			    blist[binlist[j]]) 
			  found = 1;
		    if (!found)
		      continue;

		    /* see if overlap outsides */
		    found = 0;
		    for (k = 0; k < ntopout; k++)
		      if (maxfrac[toutlist[k] *
				  nbot + i] >
			  overlap) 
			found = 1;

		    /* see if overlap added ones */
		    for (k = ntopsame; k < ntopin;
			 k++)
		      if (maxfrac[tinlist[k] * 
				  nbot +  i] > 
			  overlap)
			found = 1;
		    if (found) {
		      found_data = 1;
		      binlist[nbotin++] =i;
		      bused[i] = 1;
		    }
		  }

		} while(found_data);

		/*dump_lists("inside enhanced", binlist, 
		  nbotin, tinlist, ntopin, blist, tlist); */
		for (i = 0; i < ntopin; i++)
		  tinlist[i] = tlist[tinlist[i]];
		for (i = 0; i < nbotin; i++)
		  binlist[i] = blist[binlist[i]];

		bcont = join_all_contours(obj, binlist,
					  nbotin, nbotsame,
					  1, tinlist,
					  ntopin, ntopsame,
					  NULL);
		tcont = join_all_contours(obj, tinlist,
					  ntopin, ntopsame,
					  -1, binlist, 
					  nbotin, nbotsame,
					  NULL);
		if (mesh_contours(obj, bcont, tcont, 
				  cont->surf, cont->type,
				  scale, oddeven))
		  return (-1);
	      } else
		bused[bjustinl[istr]] = 0;
	    }
	  }

	  /* There may now be orphan inside contours left.  At this
	     point, convert the outside lists and join into single
	     outside contours */
	  for (i = 0; i < ntopout; i++)
	    toutlist[i] = tlist[toutlist[i]];
	  for (i = 0; i < nbotout; i++)
	    boutlist[i] = blist[boutlist[i]];

	  boutcont = join_all_contours(obj, boutlist, nbotout, 
				       nbotout, 1, toutlist, ntopout,
				       ntopout, NULL);
	  toutcont = join_all_contours(obj, toutlist, ntopout,
				       ntopout, -1, boutlist,
				       nbotout, nbotout, NULL);
	  if (!boutcont || !toutcont)
	    return (-1);

	  boutcont = connect_orphans(obj, boutcont, tlist, tused, 
				     tjustinl, ntjustin, tinlist,
				     -1, oddeven,
				     cont->surf, cont->type, 
				     scale, scancont, pmin, pmax);
	  toutcont = connect_orphans(obj, toutcont, blist, bused,
				     bjustinl, nbjustin, binlist, 
				     1, oddeven, cont->surf,
				     cont->type,
				     scale, scancont, pmin, pmax);
	  /* finally ready to mesh the outside contours, possibly
	     changed by orphan handling */
	  if (mesh_contours(obj, boutcont, toutcont, cont->surf,
			    cont->type, scale, 1 - oddeven))
	    return (-1);

	  /* All of this continues until no more overlapping outside
	     contours are found at any level */
	}

	/* Need to unmark connections for any that weren't used */
	for (i = 0; i < nbot; i++)
	  if (bused[i] <= 0)
	    obj->cont[blist[i]].flags &= ~CONNECT_TOP;
	for (i = 0; i < ntop; i++)
	  if (tused[i] <= 0)
	    obj->cont[tlist[i]].flags &= ~CONNECT_BOTTOM;


	/* clean up inside-outside arrays */
	free(minfrac);
	free(maxfrac);
	free(tinlist);
	free(binlist);
	free(tjustinl);
	free(bjustinl);
	free(toutlist);
	free(boutlist);
	free(tused);
	free(bused);
	free(blevels);
	free(tlevels);

      } else {
	/* We didn't find a contour to connect to - remove marks. */
	for (i = 0; i < nbot; i++)
	  obj->cont[blist[i]].flags &= ~CONNECT_TOP;
      }
    }
  }   /* end of zpass loop */
     
  /* force connection of stray contours to nearest contour */
  if (flags & IMESH_MK_STRAY)
    for (co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (!cont->psize)
	continue;
      if (connectTop(cont->flags))
	continue;

      tcont = NULL;
      bcont = imodContourDup(cont);
      if (!bcont){
	fprintf(stderr, "Fatal Error: couldn't get contour.\n");
	return(-1);
      }
	       
      /* define z-sections that connections will be made to. */
      if (flags & IMESH_MK_SKIP){
	nextz = getnextz(zlist, zlsize, contz[co]);
      }else{
	nextz = contz[co] + incz;
      }
      prevz = contz[co];

      tcont = NULL;
      for (lis = 0; lis < numatz[nextz - zmin]; lis++) {
	tco = contatz[nextz - zmin][lis];
	jcont = &(obj->cont[tco]);
	if (!jcont->psize)
	  continue;
	if (jcont->flags & CONNECT_BOTTOM)
	  continue;
		    
	/* if (contz[tco] != nextz)
	   continue; */
		    
	if (flags & IMESH_MK_SURF)
	  if (bcont->surf != jcont->surf)
	    continue;
	if (flags & IMESH_MK_TIME)
	  if (bcont->type != jcont->type)
	    continue;

	if (tcont){
	  /* todo: grab the nearer contour */
	}else{
	  tcont = jcont;
	}
      }
      if (tcont){
	int intmp = 0;
	if (obj->flags & IMOD_OBJFLAG_OUT)
	  intmp = 1 - intmp;
	if (bcont->psize == 1) imodPointAppend(bcont, bcont->pts);
	if (tcont->psize == 1) imodPointAppend(tcont, tcont->pts);
	nmesh = imeshContoursCost(bcont, tcont, scale, intmp, 0);
	if (nmesh){
	  obj->mesh = imodel_mesh_add
	    (nmesh, obj->mesh, &(obj->meshsize));
	  free(nmesh);
	}
	bcont->flags |= CONNECT_TOP;
	tcont->flags |= CONNECT_BOTTOM;
      }
	       
      imodContourDelete(bcont);
    }
     

  report_time("Connected contours");
  /*
   * Clean up scan conversions.
   */
  for(co = 0; co < obj->contsize; co++){
    if (scancont[co])
      imodContourDelete(scancont[co]);
  }
  free(scancont);

  status = (*cb)(50); /* half way done. */     
  status = (*cb)(1);
  if (status) return(status);     


  /*     printf(".");fflush(stdout); */
  if (cap){
    int skipz;
    int inside;
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      inside = 0;

      /* if the contour has two or less points then the
       * surface must already be capped. Only cap if contour
       * isn't connected on top or bottom.
       */
      if ((cont->psize > 2) && (!connectBoth(cont->flags))){

	/* But don't cap if the thickness is nil, this was already
	   meshed as a complex cap */
	if (imodContourArea(cont) / imodContourLength(cont, 1)
	    < 0.01)
	  continue;
		
	if (nestind[co] >= 0)
	  if (nests[nestind[co]].level % 2 == 0)
	    inside = 1;

	if (obj->flags & IMOD_OBJFLAG_OUT)
	  inside = 1 - inside;

	/* Skip capping if the Z value is in list to exclude */
	if (connectBottom(cont->flags) || cont->pts->z == zmax)
	  direction = 1;
	else
	  direction = -1;
	skipz = 0;
	for (i = 0; i < cap_skip_nz; i++)
	  if (floor((double)(cont->pts->z + direction + 0.5)) ==
	      cap_skip_zlist[i]) {
	    skipz = 1;
	    break;
	  }
	if (skipz)
	  continue;

	if ((cap == IMESH_CAP_ALL) || (cap == IMESH_CAP_ALL_FLAT)){

	  if (connectBottom(cont->flags))
	    direction = 1;
	  if (connectTop(cont->flags))
	    direction = -1;
	  if (cap == IMESH_CAP_ALL_FLAT)
	    nmesh = imeshContourCap(cont, direction, inside,
				    scale);
	  else
	    nmesh = imeshContourCap(cont, direction, inside,
				    scale);
	  if (nmesh){
	    obj->mesh = imodel_mesh_add
	      (nmesh, obj->mesh, &(obj->meshsize));
	    free(nmesh);
	  }
	  continue;
	}

	if (cont->pts->z == zmin){
	  nmesh = imeshContourCap(cont, -1, inside, scale); 
	  if (nmesh){
	    obj->mesh = imodel_mesh_add
	      (nmesh, obj->mesh, &(obj->meshsize));
	    free(nmesh);
	  }
	}
	if (cont->pts->z == zmax){
	  nmesh = imeshContourCap(cont, 1, inside, scale); 
	  if (nmesh){
	    obj->mesh = imodel_mesh_add
	      (nmesh, obj->mesh, &(obj->meshsize));
	    free(nmesh);
	  }
	}
      }
    }

  }
	  
	       
  report_time("Capped");
  for(m = 0; m < obj->meshsize; m++) {
    if (!(flags & IMESH_MK_SURF))
      obj->mesh[m].pad = 0;
    if (!(flags & IMESH_MK_TIME))
      obj->mesh[m].type = 0;
  }
     
  if (flags & IMESH_MK_NORM){
    obj->mesh = imeshReMeshNormal(obj->mesh, &(obj->meshsize), scale,
				  0); 
    if (!obj->mesh)
      obj->meshsize = 0;
  }	  

  for (nind = 0; nind < numnests; nind++) {
    nest = &nests[nind];
    if (/*(nest->level % 2) == 1 &&*/ nest->ninside > 0)
      imodContourDelete(nest->inscan);
    if (nest->ninside)
      free(nest->inside);
    if (nest->noutside)
      free(nest->outside);
  }
  if(numnests)
    free(nests);
  free(nestind);

  for (i = 0; i < zmax + 1 - zmin; i++) {
    if (numatz[i])
      free (contatz[i]);
  }
  free(numatz);
  free(contatz);
  free(contz);
  free(zlist);
  free(tlist);
  free(blist);
  free(pmin);
  free(pmax);
  return(0);
}

static void add_whole_nest(int nind, Iobj *obj, Nesting *nests, int *nestind,
			   int flag, int *tlist, int *ntop)
{
  int i, oind;
  Icont *jcont;
  Nesting *nest = &nests[nind];
  for (i = 0; i < nest->noutside; i++) {
    oind = nestind[nest->outside[i]];
    if (nests[oind].level == 1) {
      nind = oind;
      nest = &nests[nind];
      break;
    }
  }

  /* Add it to list if not on it */
  jcont = &obj->cont[nest->co];
  if (!(jcont->flags & flag)) {
    jcont->flags |= flag;
    tlist[(*ntop)++] = nest->co;
  }

  /* Add all inside contours to list */
  for (i = 0; i < nest->ninside; i++) {
    jcont = &obj->cont[nest->inside[i]];
    if (!(jcont->flags & flag)) {
      jcont->flags |= flag;
      tlist[(*ntop)++] = nest->inside[i];
    }
  }
}

/* Routine to connect orphan contours */
/* DNM 12/18/01: Add the oddeven argument, although it has not been verified 
   that it works for even ones */

#define NSECT 38
#define MAXTEST 100
#define NSEARCH  13
static Icont *connect_orphans(Iobj *obj, Icont *cout, int *list, int *used,
			      int *justinl, int njustin, int *inlist, int joindir,
			      int oddeven, int surf, int time, Ipoint *scale,
			      Icont **scancont, Ipoint *pmin, Ipoint *pmax)
{
  Ipoint pminout, pmaxout, pminin, pmaxin, incen;
  Icont *outscan, *inscan, *c1, *c2, *cinmin, *coutmin, *cont;
  Ipoint *pts;
  int co, inl, st1, st2, ninlist, i, j, pt, next, st1min, st2min;
  float frac1, frac2, fullarea, inarea, area1, area2;
  float dist, ratio, minratio, sectfac, ang, totlen, minpath, curpath;
  int found_data, sect, ncand, nincrease, sweepdir, nextcand, norder;
  int outnearpt[NSECT], outcand[NSECT], indorder[NSECT * NSECT];
  float infardist[NSECT], outneardist[NSECT], pathlen[NSECT * NSECT];
  float forlen[NSECT], nextlen;
  float pi = 3.1415927;
  double ddx, ddy, ddist;
  int st1test[MAXTEST], st2test[MAXTEST];
  float testratio [MAXTEST];
  int ntest, nexttest, outsize, dstdir, found, indmin;
  int dst1[NSEARCH] = {0, 1, 1, 1, 2, 2, 0, 2, -1, 0, -1, -1, 1}; 
  int dst2[NSEARCH] = {1, 0, 1, 2, 1, 2, 2, 0, 0, -1, -1, 1, -1};
  int ymid, nmidlines, atmidline;
  int icand1, icand2, iscan, instart, inend, nforward, scandir;
  int inmatch[NSECT];
  float seglen, innerarea, maxarea;
  int nscan = 2;

  found_data = 0;
  for (inl = 0; inl < njustin; inl++)
    if (!used[justinl[inl]])
      found_data = 1;
  if (!found_data)
    return cout;

  outscan = imodel_contour_scan(cout);
  if (!outscan)
    return NULL;
  imodel_contour_mm(cout, &pmaxout, &pminout);

  /* go through and mark ones that will get capped first */
  for (inl = 0; inl < njustin; inl++) {
    /* if it is an orphan, get the overlap fractions with the outer
       contour.  Large fraction means it will get capped; small fraction
       means we need to create a contour around a hole if possible */
    if (!used[justinl[inl]]) {
      co = list[justinl[inl]];
      imodel_overlap_fractions(scancont[co], pmin[co], pmax[co],
			       outscan, pminout, pmaxout,
			       &frac1, &frac2);
      if (frac1 > 0.8)    /* What to set this to as capping thresh? */
	used[justinl[inl]] = -1;
    }
  }
  imodContourDelete(outscan);
       
  for (inl = 0; inl < njustin; inl++) {
    if (used[justinl[inl]])
      continue;
    fullarea = imodContourArea(cout);
    totlen = imodContourLength(cout, 1);

    /* start with whole inner contour, then iterate on a segment */
    co = list[justinl[inl]];
    cont = &obj->cont[co];
    instart = 0;
    inend = 0;
    for (iscan = 0; iscan < nscan; iscan++) {
      pt = instart; 
      ddist = 0;
      pts = cont->pts;
      do {
	next = (pt + 1) % cont->psize;
	ddx = pts[next].x - pts[pt].x;
	ddy = pts[next].y - pts[pt].y;
	ddist += sqrt(ddx * ddx + ddy * ddy);
	pt = next;
      } while (next != inend);
      seglen = ddist;

      /* go to equally spaced points around the segment of the inner
	 contour and make a list of nearest points in the outer 
	 contour */
      ddist = 0.;
      nextlen = seglen / (NSECT - 2);
      ncand = 1;
      nforward = 0;
      pt = instart;
      inmatch[0] = instart;
      outcand[0] = imodContourNearest(cout, &pts[pt]);
      do {
	next = (pt + 1) % cont->psize;
	ddx = pts[next].x - pts[pt].x;
	ddy = pts[next].y - pts[pt].y;
	ddist += sqrt(ddx * ddx + ddy * ddy);
	if (ddist > nextlen || next == inend) {
	  nextlen += seglen / (NSECT - 2);
	  inmatch[ncand] = next;
	  outcand[ncand++] = imodContourNearest(cout, 
						&pts[next]);
	  if (outcand[ncand - 1] > outcand[ncand - 2])
	    nforward++;
	  if (ncand == NSECT)
	    break;
	}
	pt = next;
      } while (next != inend);
      scandir = (nforward > ncand / 2.) ? 1 : -1;
	  
      /* order the candidates and eliminate duplicates */
      for (i = 0; i < ncand - 1; i++)
	for (j = i + 1; j < ncand; j++)
	  if (outcand[i] > outcand[j]) {
	    pt = outcand[i];
	    outcand[i] = outcand[j];
	    outcand[j] = pt;
	    pt = inmatch[i];
	    inmatch[i] = inmatch[j];
	    inmatch[j] = pt;
	  }
      j = 1;
      for (i = 1; i < ncand; i++)
	if (outcand[j - 1] != outcand[i]) {
	  inmatch[j] = inmatch[i];
	  outcand[j++] = outcand[i];
	}
      ncand = j;
      if (ncand < 2)
	break;

      /* get forward distances */
      pts = cout->pts;
      for (i = 0; i < ncand - 1; i++) {
	ddist = 0.;
	for (pt = outcand[i]; pt < outcand[i + 1]; pt++) 
	  ddist += imodPointDistance(&pts[pt], &pts[pt + 1]);
	forlen[i] = ddist;
      }
	  

      /* compute minimum path length between each pair of
	 candidates */
      norder = 0;
      for (i = 0; i < ncand - 1; i++) {
	ddist = 0.;
	for (j = i + 1; j < ncand; j++) {
	  ddist += forlen[j - 1];
	  if (ddist < totlen / 2.) {
	    pathlen[i * ncand + j] = ddist;
	    pathlen[j * ncand + i] = 1.;
	  } else {
	    pathlen[i * ncand + j] = totlen - ddist;
	    pathlen[j * ncand + i] = -1.;
	  }
	  indorder[norder++] = i * ncand + j;
	}
      }

      /* order by decreasing separation */
      for (i = 0; i < norder - 1; i++)
	for (j = i; j < norder; j++)
	  if (pathlen[indorder[i]] < pathlen[indorder[j]]) {
	    pt = indorder[i];
	    indorder[i] = indorder[j];
	    indorder[j] = pt;
	  }

      /* run through the pairs of candidates finding one with a 
	 maximum inner area; if inner area is equal to current 
	 maximum, then minimize ratio as well */
      maxarea = -1.;
      for (i = 0; i < norder; i++) {
	st1 = outcand[indorder[i] / ncand];
	st2 = outcand[indorder[i] % ncand];
	ratio = evaluate_break(cout, list, used,
			       justinl, njustin, scancont,
			       pmin, pmax, st1, st2, maxarea,
			       pathlen[indorder[i]], fullarea,
			       joindir, 1, &innerarea);
	if (ratio > 1.e25)
	  return NULL;
	if (innerarea > maxarea) {
	  minratio = ratio;
	  maxarea = innerarea;
	  indmin = indorder[i];
	} else if (innerarea == maxarea && ratio < minratio) {
	  minratio = ratio;
	  indmin = indorder[i];
	}
      }

      /* set up for another scan between these points */
      icand1 = indmin / ncand;
      icand2 = indmin % ncand;
      if (scandir * pathlen[icand2 * ncand + icand1] > 0) {
	instart = inmatch[icand2];
	inend = inmatch[icand1];
      } else {
	instart = inmatch[icand1];
	inend = inmatch[icand2];
      }
    }
    if (ncand < 2)
      continue;

    /* Set up for search for somewhat local minimum */
    minpath = pathlen[indmin];
    st1min = outcand[icand1];
    st2min = outcand[icand2];
    st1test[0] = st1min;
    st2test[0] = st2min;
    testratio[0] = minratio;
    ntest = 1;
    nexttest = 1;
    outsize = cout->psize;
    sweepdir = -pathlen[icand2 * ncand + icand1];
    do {
      found_data = 0;
      for (i = 0; i < NSEARCH; i++) {
	curpath = minpath;
	st1 = (st1min + dst1[i] * sweepdir + outsize) % outsize;
	dstdir = dst1[i] > 0 ? 1 : -1;
	pt = st1;
	for (j = 0; j < dstdir * dst1[i]; j++) {
	  next = (pt + dstdir * sweepdir + outsize) % outsize;
	  curpath += dstdir * imodPointDistance
	    (&pts[pt], &pts[next]);
	  pt = next;
	}
	st2 = (st2min - dst2[i] * sweepdir + outsize) % 
	  outsize;
	dstdir = dst2[i] > 0 ? 1 : -1;
	pt = st2min;
	for (j = 0; j < dstdir * dst2[i]; j++) {
	  next = (pt + dstdir * sweepdir + outsize) % outsize;
	  curpath += dstdir * imodPointDistance
	    (&pts[pt], &pts[next]);
	  pt = next;
	}

	if (curpath <= 0.)
	  pt = next;

	/* search on the test list */
	found = 0;
	for (j = 0; j < ntest; j++)
	  if (st1test[j] == st1 && st2test[j] == st2) {
	    found = 1;
	    ratio = testratio[j];
	    break;
	  }
	if (!found) {
	  ratio = evaluate_break(cout, list, used,
				 justinl, njustin, scancont,
				 pmin, pmax, st1, st2,
				 minratio, curpath, fullarea,
				 joindir, 0, &innerarea);
	  if (ratio > 1.e25)
	    return NULL;
	  st1test[nexttest] = st1;
	  st2test[nexttest] = st2;
	  testratio[nexttest++] = ratio;
	  if (ntest < nexttest)
	    ntest = nexttest;
	  if (nexttest == MAXTEST)
	    nexttest = 0;
	}
	if (ratio < minratio) {
	  minratio = ratio;
	  st1min = st1;
	  st2min = st2;
	  found_data = 1;
	  break;
	}
      }
    } while(found_data);

    if (minratio < 1.e20) {
      if(break_contour_inout(cout, st1min, st2min, -joindir, &c1,
			     &c2))
	return NULL;
      area1 = imodContourArea(c1);
      area2 = imodContourArea(c2);
      if (area2 < area1) {
	cinmin = c2;
	coutmin = c1;
      } else {
	cinmin = c1;
	coutmin = c2;
      }

      inscan = imodel_contour_scan(cinmin);
      if (!inscan)
	return NULL;
      imodel_contour_mm(cinmin, &pmaxin, &pminin);

      /* build list of ones that overlap new inner contour */
      ninlist = 0;
      for (i = 0; i < njustin; i++) {
	if (used[justinl[i]])
	  continue;
	co = list[justinl[i]];
	if (imodel_scans_overlap(scancont[co], pmin[co], pmax[co],
				 inscan, pminin, pmaxin)) {
	  inlist[ninlist++] = co;
	  used[justinl[i]] = 1;
	}
      }

      /* Join these contours, mesh to the created inside one, delete
	 them both and the scan; replace the outer contour with the
	 new one */
      c1 = join_all_contours(obj, inlist, ninlist, ninlist, joindir, 
			     NULL, 0, 0, cinmin);
      if (joindir < 0) {
	c2 = c1;
	c1 = cinmin;
	cinmin = c2;
      }
      if (mesh_contours(obj, c1, cinmin, surf, time, scale, oddeven))
	return NULL;
      imodContourDelete(cout);
      cout = coutmin;
      imodContourDelete(inscan);
    }
  }
  return cout;
}


/* cross_cont tests whether contour cout is crossed by the line segment between
   x1s,y1s and x1e, y1e.  It omits segments containing points st1 and st2 */

static int cross_cont(Icont *cout, float x1s, float y1s, float x1e, float y1e,
		      int st1, int st2)
{
  float dx1, dy1, x2s, x2e, y2s, y2e, dx2, dy2, dxs, dys, tnum, unum, den;
  int pt, next;

  dx1 = x1e - x1s;
  dy1 = y1e - y1s;
  for (pt = 0; pt < cout->psize; pt++) {
    next = (pt + 1) % cout->psize;
    if (pt == st1 || pt == st2 || next == st1 || 
	next == st2)
      continue;
    x2s = cout->pts[pt].x;
    x2e = cout->pts[next].x;
    y2s = cout->pts[pt].y;
    y2e = cout->pts[next].y;
    dx2 = x2s - x2e;
    dy2 = y2s - y2e;
    dxs = x2s - x1s;
    dys = y2s - y1s;
    den = dx1 * dy2 - dy1 * dx2;
    tnum = dxs * dy2 - dys * dx2;
    unum = dx1 * dys - dy1 * dxs;
    if (den < 0) {
      if (tnum <= 0. && unum <= 0. && tnum >= den && unum >= den)
	return 1;
    } else {
      if (tnum >= 0. && unum >= 0. && tnum <= den && unum <= den)
	return 1;
    }
  }
  return 0;
}

static float evaluate_break(Icont *cout, int *list, int *used,
			    int *justinl, int njustin, Icont **scancont,
			    Ipoint *pmin, Ipoint *pmax, int st1, int st2,
			    float curmin, float pathlen, float fullarea,
			    int joindir, int areaonly, float *innerarea)
{
  Ipoint pminin, pmaxin;
  Icont *inscan, *c1, *c2, *cintst, *couttst;
  int co, cross, ninlist, i, pt, next;
  float frac1, frac2, areasum, inarea, area1, area2;
  float x1s, x1e, x2s, x2e, y1s, y1e, y2s, y2e, dx1, dy1, dx2, dy2;
  float dxs, dys, den, tnum, unum, dist, ratio, maxarea;
  double areapow = 2.0;

  /* does the biggest possible area, implied by distamce and pathlen,
     not enough to give a lower ratio? */
  dist = imodPointDistance(&cout->pts[st1], &cout->pts[st2]);
  maxarea = (dist + pathlen) * (dist + pathlen) / 12.5664;
  ratio = 1.e20;
  if (maxarea > 0)
    ratio = dist / pow((double)maxarea, areapow);
  if ((!areaonly && ratio > curmin) || (areaonly && maxarea < curmin)) {
    *innerarea = maxarea;
    return (ratio);
  }

  /* second test: does connector cross contour? */
  x1s = cout->pts[st1].x;
  x1e = cout->pts[st2].x;
  y1s = cout->pts[st1].y;
  y1e = cout->pts[st2].y;

  *innerarea = 0.;
  if(cross_cont(cout, x1s, y1s, x1e, y1e, st1, st2))
    return (1.e20);

  /* Make two new contours, require that one be bigger than
     the original; take the other as test inner one */
  if(break_contour_inout(cout, st1, st2, -joindir, &c1, &c2))
    return (1.e30);
  area1 = imodContourArea(c1);
  area2 = imodContourArea(c2);
  if (area1 > fullarea && area2 < area1) {
    cintst = c2;
    couttst = c1;
    inarea = area2;
  } else if (area2 > fullarea) {
    cintst = c1;
    couttst = c2;
    inarea = area1;
  } else {
    imodContourDelete(c1);
    imodContourDelete(c2);
    return (1.e20);
  }

  ratio = 1.e20;
  if (inarea > 0.)
    ratio = dist / pow((double)inarea, areapow);
  if ((!areaonly && ratio > curmin) || (areaonly && inarea < curmin)) {
    *innerarea = inarea;
    return (ratio);
  }

  /* scan convert the inner one for testing overlaps */
  inscan = imodel_contour_scan(cintst);
  if (!inscan)
    return (1.e30);
  imodel_contour_mm(cintst, &pmaxin, &pminin);

  /* sum the area of orphans overlapping this contour */
  areasum = 0;
  for (i = 0; i < njustin; i++) {
    if (used[justinl[i]])
      continue;
    co = list[justinl[i]];
    imodel_overlap_fractions(scancont[co], pmin[co],
			     pmax[co], inscan, pminin,
			     pmaxin, &frac1, &frac2);
    areasum += frac2 * inarea;
  }
		    
  ratio = 1.e20;
  if (areasum > 0.)
    ratio = dist / pow((double)areasum, areapow);
  *innerarea = areasum;
  imodContourDelete(c1);
  imodContourDelete(c2);
  imodContourDelete(inscan);
  return (ratio);
}




/*
 * Creates a cap mesh on a contour.
 * scale is passed to imodMeshContours.
 * side has two values: 1 cap on top, -1 cap on bottom.
 *
 */
static Imesh *imeshContourCap(Icont *cont, int side, int inside, Ipoint *scale)
{
  int     pt;
  int     meshdir = 0;
  /* IMOD_CONTOUR_CLOCKWISE, IMOD_CONTOUR_COUNTER_CLOCKWISE */
  int     direction; 
  int     failedcm = 0;

  Ipoint  cm, ll, ur;
  Imesh  *m;
  Icont *ncont, *scont;
  double rotation;
  float ratio, length, width, dely, nexty;

  if (imodContourCenterOfMass(cont, &cm)) {
    /* If the center of mass computation fails, take midpoint of the
       bounding box */
    imodel_contour_mm(cont, &ll, &ur);
    cm.x = (ll.x + ur.x) / 2.;
    cm.y = (ll.y + ur.y) / 2.;
    cm.z = (ll.z + ur.z) / 2.;
    failedcm = 1;
  }
  direction = imodContZDirection(cont);
  if (side == 1)
    cm.z += 0.5f;
  else
    cm.z -= 0.5f;

  /* evaluate elongation and need for a complex cap.
     Find the long axis with a certain precision, limit the ratio to
     half the length so that delta y is at least 2 */
  rotation = 3.14159 / 2. - imodContourLongAxis(cont, 5., &ratio, &length);
  if (ratio > length / 2.)
    ratio = length / 2.;
  if (ratio >= 2.0 && cont->psize > 4 && !failedcm) {
    ncont = imodContourDup(cont);
    if (!ncont)
      return NULL;
    imodContourRotateZ(ncont, rotation);
    scont = imodel_contour_scan(ncont);
    if (!scont)
      return NULL;
    imodContourDelete(ncont);
    ncont = imodContourNew();
    dely = length / ratio;
    nexty = scont->pts->y + dely / 2.;

    /* find the midpoint of each scan line at the next desired Y */
    for (pt = 0; pt < scont->psize; pt += 2)
      if (scont->pts[pt].y > nexty) {
	cm.x = (scont->pts[pt].x + scont->pts[pt+1].x) / 2.;
	cm.y = nexty;
	nexty += dely;
	imodPointAppend(ncont, &cm);
      }

    /* loop the contour back at the end to make complex cap */
    for (pt = ncont->psize - 2; pt > 0; pt--)
      imodPointAppend(ncont, &ncont->pts[pt]);

    imodContourRotateZ(ncont, -rotation);

    if (side == 1)
      m = imeshContoursCost(cont, ncont, scale, inside, 0);
    else
      m = imeshContoursCost(ncont, cont, scale, inside, 0);

    imodContourDelete(ncont);
    imodContourDelete(scont);
    return (m);
  }


  if (( (side < 0) && (direction == IMOD_CONTOUR_COUNTER_CLOCKWISE)) ||
      ( (side > 0) && (direction == IMOD_CONTOUR_CLOCKWISE)))
    meshdir = 1;
  if (inside)
    meshdir = 1 - meshdir;

  /* ccw on top , cw on bottom (unless inside!)*/

  return(makeCapMesh(cont, &cm, meshdir));
}

/* make a cap mesh connecting a contour to a point in the given direction */
static Imesh *makeCapMesh(Icont *cont, Ipoint *cm, int meshdir)
{
  int pt, npt;
  Imesh *m = imodMeshNew();
  if (!m) 
    return(m);

  if (meshdir){
    imodMeshAddVert(m, cm);
	  
    for(pt = 0; pt < cont->psize; pt++){
      npt = pt+1;
      if (npt == cont->psize) npt = 0;
      imodMeshAddVert(m, &cont->pts[pt]);
      imodMeshAddIndex(m, IMOD_MESH_BGNPOLY);
      imodMeshAddIndex(m, 0);
      imodMeshAddIndex(m, npt+1);
      imodMeshAddIndex(m, pt+1);
      imodMeshAddIndex(m, IMOD_MESH_ENDPOLY);
    }
  }else{
    imodMeshAddVert(m, cm);
	  
    for(pt = 0; pt < cont->psize; pt++){
      npt = pt+1;
      if (npt == cont->psize) npt = 0;
      imodMeshAddVert(m, &cont->pts[pt]);
	       
      imodMeshAddIndex(m, IMOD_MESH_BGNPOLY);
      imodMeshAddIndex(m, pt+1);
      imodMeshAddIndex(m, npt+1);
      imodMeshAddIndex(m, 0);
      imodMeshAddIndex(m, IMOD_MESH_ENDPOLY);
    }
  }

  imodMeshAddIndex(m, IMOD_MESH_END);
  return(m);
}


void imesh_normal(Ipoint *n, Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *sp)
{
  double dist, sdist;
  Ipoint v1, v2;
     
  v1.x = p3->x - p2->x;
  v1.y = p3->y - p2->y;
  v1.z = p3->z - p2->z;

  v2.x = p1->x - p2->x;
  v2.y = p1->y - p2->y;
  v2.z = p1->z - p2->z;
  if (sp){
    v1.x *= sp->x;
    v1.y *= sp->y;
    v1.z *= sp->z;
    v2.x *= sp->x;
    v2.y *= sp->y;
    v2.z *= sp->z;
  }

  n->x = (v1.y * v2.z) - (v1.z * v2.y);
  n->y = (v1.z * v2.x) - (v1.x * v2.z);
  n->z = (v1.x * v2.y) - (v1.y * v2.x);

  /* now normalize n ; x^2 + y^2 + z^2 = 1 */
     
  sdist = (n->x * n->x) + (n->y * n->y) + (n->z * n->z);
  dist = sqrt(sdist);
  if (dist == 0.0){
    n->x = 0;
    n->y = 0;
    n->z = -1;
  }
  else{
    dist = 1/dist;
    n->x *= dist;
    n->y *= dist;
    n->z *= dist;
  }
  return;     
}


static int ptcompare(const void *v1, const void *v2)
{
  Ipoint *pt1 = (Ipoint *)v1;
  Ipoint *pt2 = (Ipoint *)v2;
  if (pt1->x < pt2->x)
    return -1;
  if (pt1->x > pt2->x)
    return 1;
  if (pt1->y == pt2->y)
    return 0;
  if (pt1->y > pt2->y)
    return 1;
  return -1;
}

/* Remake meshes into one mesh to store data more efficiently.
 * and calculate normals for all verticies.
 * The normals are scaled by the values in the point scale.
 * DNM: modified to take both POLY meshes and POLYNORM meshes
 * WARNING: THIS FUNCTION WILL DELETE THE INPUT MESH BEFORE RETURNING
 * Note: the division by X for sorting of points was implemented before qsort
 * was used.  It made a huge difference then but only a small one with qsort.
 */

#define XDIV 10
Imesh *imeshReMeshNormal(Imesh *meshes, int *size, Ipoint *scale, int resol)
{
  Imesh *nm;
  Imesh *mesh, *remesh = NULL;
  int m,i,l,v, pt, dpt;
  Icont *nlist;
  Ipoint *v1, *v2, *v3;
  int     v1i, v2i, v3i;
  Ipoint npt;
  int lsize;
  int ptdup;
  int *olist, olsize;
  int surf, surfsize = 0;
  int time, timesize = 0;
  int meshsize = 0;
  unsigned int msize = *size;
  int linc, l1, l2;
  char *gotit;
  int ivt, indvert;
  int zmin, zmax, intz, dup;
  int *numatz, *cumatz;
  Ipoint **vecatz;
  Ipoint *iptz;
  float vecx, vecy;
  float xmin, xmax, delx;
  int indx, tablesize, maxlist;
  int ptmp[3];
    
  if (!msize)
    return NULL;

  for(m = 0; m < msize; m++) {
    /* count only the meshes where the resolution value matches */
    if (imeshResol(meshes[m].flag) == resol) {
      if (meshes[m].pad > surfsize)
	surfsize = meshes[m].pad;
      if (meshes[m].type > timesize)
	timesize = meshes[m].type;
    } else {
      /* otherwise, move the mesh to the output mesh */
      nm = &meshes[m];
      remesh = imodel_mesh_add(nm, remesh, &meshsize);
      nm->vert = NULL;
      nm->list = NULL;
      nm->vsize = 0;
      nm->lsize = 0;
    }
  }
  surfsize++;
  timesize++;
	
  report_time("Starting ReMesh");
    
  /*    lsize = 0;
	l1 = 0;
	l2 = 0;
	for(m = 0; m < msize; m++) {
	lsize += meshes[m].vsize * 12 + meshes[m].lsize * 4 + 32;
	l1 += meshes[m].vsize;
	l2 += meshes[m].lsize;
	}
	printf("Incoming mesh vertices = %d, indexes = %d, memory = %d\n",
	l1, l2, lsize); */

  for(surf = 0; surf < surfsize; surf++){
    if (surfsize > 1){
      printf("\rnormals surface %d", surf);
      fflush(stdout);
    }
    for(time = 0; time < timesize; time++){
      nm = imodMeshNew();
      if (!nm){
	*size = 0;
	return(NULL);
      }

      /* DNM 6/20/01: in the xcourse of adding time support, had it transfer
	 surface number to the mesh also */
      maxlist = 0;
      nm->pad = surf;
      nm->type = time;
	
      /* Find min and max Z and X values */
      zmax = -10000000;
      zmin = 10000000;
      xmax = -1.e30;
      xmin = 1.e30;
      for(m = 0; m < *size; m++){
	if (meshes[m].pad == surf && meshes[m].type == time) {
	  mesh = &(meshes[m]);
	  for(l = 0; l < mesh->lsize; l++) {
	    if (mesh->list[l] == IMOD_MESH_BGNPOLY || 
		mesh->list[l] == IMOD_MESH_BGNPOLYNORM){
	      linc = 1;
	      if (mesh->list[l++] == IMOD_MESH_BGNPOLYNORM)
		linc = 2;
	      while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
		l += (linc - 1);
		for (ivt = 0; ivt < 3; ivt++) {
		  intz = mesh->vert[mesh->list[l]].z;
		  if (intz > zmax)
		    zmax = intz;
		  if (intz < zmin)
		    zmin = intz;
		  vecx = mesh->vert[mesh->list[l]].x;
		  if (vecx > xmax)
		    xmax = vecx;
		  if (vecx < xmin)
		    xmin = vecx;
		  l += linc;				     
		}
		l -= (linc - 1);
	      }
	    }
	  }
	}
      }

      if (zmax < zmin)
	continue;

      tablesize = XDIV * (zmax + 1 - zmin);
      delx = (xmax + 1. - xmin) / XDIV;
      /* Count the number of each Z and fractional X value */
      numatz = (int *) malloc(sizeof(int) * tablesize);
      cumatz = (int *) malloc(sizeof(int) * (tablesize + 1));
      vecatz = (Ipoint **) malloc(sizeof(Ipoint*) * tablesize);

      if (!cumatz || !numatz || !vecatz) {
	*size = 0;
	return(NULL);
      }
      for (l = 0; l < tablesize; l++)
	numatz[l] = 0;
      for(m = 0; m < *size; m++){
	if (meshes[m].pad == surf && meshes[m].type == time) {
	  mesh = &(meshes[m]);
	  for(l = 0; l < mesh->lsize; l++) {
	    if (mesh->list[l] == IMOD_MESH_BGNPOLY || 
		mesh->list[l] == IMOD_MESH_BGNPOLYNORM){
	      linc = 1;
	      if (mesh->list[l++] == IMOD_MESH_BGNPOLYNORM)
		linc = 2;
	      while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
		l += (linc - 1);
		for (ivt = 0; ivt < 3; ivt++) {
		  intz = mesh->vert[mesh->list[l]].z;
		  vecx = mesh->vert[mesh->list[l]].x;
		  indx = (vecx - xmin) / delx;
		  numatz[indx + XDIV *(intz - zmin)]++;
		  l += linc;
		}
		l -= (linc - 1);
	      }
	    }
	  }
	}
      }

      /* allocate arrays for the points, rezero the counters */
      for (l = 0; l < tablesize; l++) {
	if (numatz[l]) {
	  vecatz[l] = (Ipoint *)malloc(sizeof(Ipoint) * numatz[l]);
	  if (!vecatz[l]) {
	    *size = 0;
	    return(NULL);
	  }
	  numatz[l] = 0;
	}
      }



      /* Put points into arrays, eliminating duplicates as we go */
      for(m = 0; m < *size; m++){
	mesh = &(meshes[m]);
	if (mesh->pad == surf && mesh->type == time && mesh->vsize > 0) {
	  gotit = (char *)malloc(mesh->vsize);
	  if (!gotit) {
	    *size = 0;
	    return(NULL);
	  }
	  for (l = 0; l < mesh->vsize; l++)
	    gotit[l] = 0;

	  for(l = 0; l < mesh->lsize; l++) {
	    if (mesh->list[l] == IMOD_MESH_BGNPOLY || 
		mesh->list[l] == IMOD_MESH_BGNPOLYNORM){
	      linc = 1;
	      if (mesh->list[l++] == IMOD_MESH_BGNPOLYNORM)
		linc = 2;
	      while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
		l += (linc - 1);
		for (ivt = 0; ivt < 3; ivt++) {
		  indvert = mesh->list[l];
		  if (!gotit[indvert]) {
		    gotit[indvert] = 1;
		    intz = mesh->vert[indvert].z;
		    vecx = mesh->vert[indvert].x;
		    vecy = mesh->vert[indvert].y;
		    indx = (vecx - xmin) / delx;
		    intz = indx + XDIV * (intz - zmin);
		    pt = 0;
		    dup = 0;
		    iptz = vecatz[intz];
		    /* Take out this search and use
		       qsort below */
		    /*  while (pt < numatz[intz] && !dup) {
			if (iptz[pt].x  == vecx &&
			iptz[pt].y  == vecy)
			dup = 1;
			pt++;
			}
			if (!dup) { */
		    iptz[numatz[intz]] = 
		      mesh->vert[indvert];
		    numatz[intz]++;
		    /* } */
		  }
		  l += linc;
		}
		l -= (linc - 1);
	      }
	    }
	  }
	  free (gotit);
	}
      }

      /* Sort the vertices by X/Y, eliminate duplicates */
      for (l = 0; l < tablesize; l++)
	if (numatz[l]) {
	  iptz = vecatz[l];
	  qsort(iptz, numatz[l], sizeof(Ipoint), ptcompare);
	  l2 = 1;
	  for (l1 = 1; l1 < numatz[l]; l1++) {
	    if (ptcompare(&iptz[l2 - 1], &iptz[l1]))
	      iptz[l2++] = iptz[l1];
	  }
	  numatz[l] = l2;
	}

      cumatz[0] = 0;
      for (l = 0; l < tablesize; l++)
	cumatz[l + 1] = cumatz[l] + 2 * numatz[l];
	
      /* copy points to mesh data, zero out normal space also */
      nm->vert = (Ipoint *)malloc( sizeof(Ipoint) * cumatz[tablesize]);
      nm->vsize = cumatz[tablesize];
      iptz = nm->vert;
      for (l = 0; l < tablesize; l++) {
	for (pt = 0; pt < numatz[l]; pt++) {
	  *iptz++ = vecatz[l][pt];
	  iptz->x = 0.;
	  iptz->y = 0.;
	  iptz->z = 0.;
	  iptz++;
	}
      }

      /*	printf ("vertices & normals %d\n",nm->vsize); */
      /*	printf("Finding Indices\n");  */
      report_time("Built tables");

      /* add normals to mesh */
      for(m = 0; m < msize; m++){
	if (meshes[m].pad != surf || meshes[m].type != time)
	  continue;
	mesh = &(meshes[m]);
	if (!mesh->lsize)
	  continue;
	chunkMeshAddIndex(nm, IMOD_MESH_BGNPOLYNORM, &maxlist);
        linc = 0;
	for(l = 0; l < mesh->lsize; l++) {
	  if (mesh->list[l] == IMOD_MESH_BGNPOLY || 
	      mesh->list[l] == IMOD_MESH_BGNPOLYNORM){

            /* Preserve polynorms polygons: start a new polygon if this is a 
               polynorm or if the last one was - use linc to indicate this */
            if ((linc && mesh->list[l] == IMOD_MESH_BGNPOLYNORM) || linc == 2) {
              chunkMeshAddIndex(nm, IMOD_MESH_ENDPOLY, &maxlist);
              chunkMeshAddIndex(nm, IMOD_MESH_BGNPOLYNORM, &maxlist);
            }
	    linc = 1;
	    if (mesh->list[l++] == IMOD_MESH_BGNPOLYNORM)
	      linc = 2;
	    while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
	      l += (linc - 1);
	      for (ivt = 0; ivt < 3; ivt++) {
		intz = mesh->vert[mesh->list[l]].z;
		vecx = mesh->vert[mesh->list[l]].x;
		/*  vecy = mesh->vert[mesh->list[l]].y; */
		indx = (vecx - xmin) / delx;
		intz = indx + XDIV * (intz - zmin);
		/* take this out; use bsearch instead */
		/* for (pt = cumatz[intz]; 
		   pt < cumatz[intz] + 2 * numatz[intz];
		   pt += 2) {
		   if (nm->vert[pt].x == vecx &&
		   nm->vert[pt].y == vecy)
		   break;
		   } */
				
		iptz = (Ipoint *)bsearch
		  (&mesh->vert[mesh->list[l]],
		   &nm->vert[cumatz[intz]],
		   numatz[intz], 2 * sizeof(Ipoint),
		   ptcompare);
		if (iptz)
		  pt = iptz - nm->vert;
		else {
		  printf("Failed to find vertex in reduced"
			 "list\n");
		  pt = cumatz[intz];
		}
		ptmp[ivt]=pt;
		l += linc;
	      }

	      /* Save only if it is truly a triangle */
	      if (ptmp[1] != ptmp[2] && ptmp[0] != ptmp[2] &&
		  ptmp[0] != ptmp[1]) {
		chunkMeshAddIndex(nm, ptmp[0] + 1, &maxlist);
		chunkMeshAddIndex(nm, ptmp[0], &maxlist);
		chunkMeshAddIndex(nm, ptmp[1] + 1, &maxlist);
		chunkMeshAddIndex(nm, ptmp[1], &maxlist);
		chunkMeshAddIndex(nm, ptmp[2] + 1, &maxlist);
		chunkMeshAddIndex(nm, ptmp[2], &maxlist);
	      }
	      l -= (linc - 1);
	    }
	  }
	}
	chunkMeshAddIndex(nm, IMOD_MESH_ENDPOLY, &maxlist);
      }
	
      imodMeshAddIndex(nm, IMOD_MESH_END);

      /*	printf ("indices %d\n",nm->lsize); */
      /*	printf("Calculating Normals\n"); */
      report_time("Found Indexes");
	
      /* calculate normals for all points. */
      mesh = nm;
      for(l = 0; l < mesh->lsize; l++) {
	if (mesh->list[l] == IMOD_MESH_BGNPOLYNORM){
	  l++;
	  while (mesh->list[l] != IMOD_MESH_ENDPOLY) {
	    imesh_normal(&npt, 
			 &(mesh->vert[mesh->list[l+5]]),
			 &(mesh->vert[mesh->list[l+1]]), 
			 &(mesh->vert[mesh->list[l+3]]), 
			 scale);
	    mesh->vert[mesh->list[l]].x += npt.x;
	    mesh->vert[mesh->list[l]].y += npt.y;
	    mesh->vert[mesh->list[l]].z += npt.z;
	    mesh->vert[mesh->list[l+2]].x += npt.x;
	    mesh->vert[mesh->list[l+2]].y += npt.y;
	    mesh->vert[mesh->list[l+2]].z += npt.z;
	    mesh->vert[mesh->list[l+4]].x += npt.x;
	    mesh->vert[mesh->list[l+4]].y += npt.y;
	    mesh->vert[mesh->list[l+4]].z += npt.z;
	    l += 6;
	  }
	}
      }
	
      for (l = 0; l < tablesize; l++)
	if (numatz[l])
	  free(vecatz[l]);
      free(numatz);
      free(vecatz);
      free(cumatz);

	
      if (nm){
	nm->flag |= (resol << IMESH_FLAG_RES_SHIFT);
	if ((nm->vsize) && (nm->lsize))
	  remesh = imodel_mesh_add
	    (nm, remesh, &meshsize);
	    
	free(nm);
      }
    }
  }
  if (surfsize > 1)
    puts(" ");
  *size = meshsize;

  imodMeshesDelete(meshes, msize);
  report_time("Calculated Normals");

  return(remesh);
}





/* DNM: functions to eliminate overlap between contours, which screw things
   up when one needs to add a point and join them */

static int inside_cont(Icont *cont, Ipoint pt)
{
  Ipoint *pts = cont->pts;
  int rstrad, lstrad;

  int nrcross=0;
  int nlcross=0;
  int np = cont->psize;
  float x = pt.x;
  float y = pt.y;
  float xp, yp, xc, yc, xcross;
  int j, jl;

  yp = pts[np - 1].y;
  j = 0;
  while(j < np) {
    if(yp < y) {
	      
      /* if last point below y, search for first that is not below */
	       
      while(j < np && pts[j].y < y) 
	j++;

    } else if (yp > y) {

      /* or if last point above y, search for first that is not 
	 above */

      while(j < np && pts[j].y > y)
	j++;
    }

    if (j < np) {
      jl=j-1;
      if(jl < 0)
	jl=np - 1;
      xp=pts[jl].x;
      yp=pts[jl].y;
      xc=pts[j].x;
      yc=pts[j].y;
	    
      /*  return if point is a vertex */

      if (x == xc && y == yc)
	return 1;
	    
      /* does edge straddle the ray to the right or the left? */

      rstrad=(yc > y) != (yp > y);
      lstrad=(yc < y) != (yp < y);
      if(lstrad || rstrad) {
	      
	/* if so, compute the crossing of the ray, add up crossings */

	xcross=xp+(y-yp)*(xc-xp)/(yc-yp);
	if(rstrad && (xcross > x))
	  nrcross++;
	if(lstrad && (xcross < x))
	  nlcross++;
      }
      yp=yc;
    }
    j++;
  }
  
  /*  if left and right crossings don't match, it's on an edge
      otherwise, inside iff crossings are odd */
  if(nrcross % 2 !=  nlcross % 2)
    return 1;
  return((nrcross % 2) > 0);
}

static void interpolate_point(Ipoint pt1, Ipoint pt2, float frac, Ipoint *pt3)
{
  pt3->x = frac * pt2.x + (1 - frac) * pt1.x;
  pt3->y = frac * pt2.y + (1 - frac) * pt1.y;
  pt3->z = frac * pt2.z + (1 - frac) * pt1.z;
}

static void backoff_overlap(Icont *c1, Icont *c2)
{
  Ipoint pt3, ptbefore, ptafter;
  int firstout, lastout, firstin, lastin;
  int i, before, after;
     
  /* find first point outside */
  for (firstout = 0; firstout < c1->psize; firstout++) {
    if (!inside_cont(c2, c1->pts[firstout]))
      break;
  }

  /* give up if never got out */
  if (firstout == c1->psize)
    return;

  /* now find last point out */
  lastout = firstout;
  firstin = lastout + 1;
  for (i = 0; i < c1->psize; i++) {
    if (firstin == c1->psize)
      firstin = 0;
    if (inside_cont(c2, c1->pts[firstin]))
      break;
    lastout = firstin;
    firstin++;
  }

  /* give up if never got in */
  if (i == c1->psize)
    return;

  /* find first interpolated point that's outside and go 1 more tenth out*/
  before = 0;
  for (i = 9; i > 1; i--) {
    interpolate_point(c1->pts[lastout], c1->pts[firstin], 0.1 * i, &pt3);
    if (!inside_cont(c2, pt3)) {
      interpolate_point(c1->pts[lastout], c1->pts[firstin], 
			0.1 * (i - 1), &ptbefore);
      /*	       printf ("before frac %d\n", i-1); */
      before = 1;
      break;
    }
  }

  /* Now find first point out from here */
  lastin = firstin;
  firstout = lastin + 1;
  for (i = 0; i < c1->psize; i++) {
    if (firstout == c1->psize)
      firstout = 0;
    if (!inside_cont(c2, c1->pts[firstout]))
      break;
    lastin = firstout;
    firstout++;
  }

  /* find first interpolated point that's outside and go 1 more tenth out*/
  after = 0;
  for (i = 9; i > 1; i--) {
    interpolate_point(c1->pts[firstout], c1->pts[lastin], 0.1 * i, &pt3);
    if (!inside_cont(c2, pt3)) {
      interpolate_point(c1->pts[firstout], c1->pts[lastin], 
			0.1 * (i - 1), &ptafter);
      /*	       printf ("after frac %d\n", i-1); */
      after = 1;
      break;
    }
  }
  /*     printf ("adding at %d\n", lastout + 1); */

  /* Add the two points after the last one out */
  if (before) {
    lastout++;
    imodPointAdd(c1, &ptbefore, lastout);
  }
  if (after) {
    lastout++;
    imodPointAdd(c1, &ptafter, lastout);
  }

  /* Just delete all points inside */
  for (i = 0; i < c1->psize; i++) {
    if (inside_cont(c2, c1->pts[i])) {
      imodPointDelete(c1, i);
      /*	       printf ("deleting %d\n", i); */
      i--;
    }
  }
}



static int eliminate_overlap(Icont *c1, Icont *c2)
{
  Icont *c1cp;
  int i;
  for (i = 0; i < 2; i++) {
    if(!imodel_contour_overlap(c1, c2))
      return i;
    c1cp = imodContourDup(c1);
    backoff_overlap(c1, c2);
    backoff_overlap(c2, c1cp);
    imodContourDelete(c1cp);
  }
  return 1;
}

#define CHECK_DIVISIONS 0
#define CHECK_MIN_FRACTION 0.45
#define CHECK_MIN_DISTANCE 2.
#define INSIDE_MAX_DISTANCE 0.1
#define INSIDE_MAX_FRACTION 0.1
static int check_legal_joiner(Ipoint pt1, Ipoint pt2, Icont *c1, Icont *c2,
			      int samelevel, Iobj *obj, int *olist,
			      int nother, int nothersame, Icont *other)
{
  Ipoint mid;
  int i, j, retval, inside;
  float len, minfrac, frac, xs1, xs2, ys1, ys2;
  int ncheck = CHECK_DIVISIONS;
  len = sqrt((double)(pt1.x - pt2.x) * (pt1.x - pt2.x) +
	     (pt1.y - pt2.y) * (pt1.y - pt2.y));

  /* check that a connector trimmed by 0.1 pixel or 0.1 of length does not
     cross either contour */
  if (len > 1.e-5) {
    frac = INSIDE_MAX_DISTANCE / len;
    if (frac > INSIDE_MAX_FRACTION)
      frac = INSIDE_MAX_FRACTION;
    xs1 = (1. - frac) * pt1.x + frac * pt2.x;
    ys1 = (1. - frac) * pt1.y + frac * pt2.y;

    xs2 = (1. - frac) * pt2.x + frac * pt1.x;
    ys2 = (1. - frac) * pt2.y + frac * pt1.y;
    if(cross_cont(c1, xs1, ys1, xs2, ys2, -1, -1))
      return 0;
    if(cross_cont(c2, xs1, ys1, xs2, ys2, -1, -1))
      return 0;
  }

  /* check one or more points in middle of connector to verify proper
     relation to contours on next slice */
  minfrac = 0.45;
  if (len > 1.e-5)
    minfrac = CHECK_MIN_DISTANCE / len;
  if (minfrac < CHECK_MIN_FRACTION)
    minfrac = CHECK_MIN_FRACTION;
  if (minfrac > 0.5)
    minfrac = 0.5;

  for (j = 0; j <= CHECK_DIVISIONS; j++) {
    /* If contours are at same level, midpoint should be inside the
       basic contours of other level and outside any extra inner 
       contours.  Finding the point inside any one contour is conclusive
       on legality or illegality, respectively */
    retval = 1;
    if (!samelevel)
      retval = 0;

    frac = 0.5;
    if (ncheck > 0)
      frac = minfrac + j * (1. - 2 * minfrac) / ncheck;
    mid.x = (1. - frac) * pt1.x + frac * pt2.x;
    mid.y = (1. - frac) * pt1.y + frac * pt2.y;
    if (other)
      if (inside_cont(other, mid))
	return retval;
    for (i = 0; i < nothersame; i++)
      if (inside_cont(&obj->cont[olist[i]], mid))
	return retval;
    retval = 1 - retval;
    for (i = nothersame; i < nother; i++)
      if (inside_cont(&obj->cont[olist[i]], mid))
	return retval;
  }
  return (retval);
}

static void segment_mm(Icont *cont, int ptst, int ptnd, float *xmin, 
		       float *xmax, float *ymin, float *ymax)
{
  int pt = ptst - 1;
  Ipoint *pts = cont->pts;
  *xmin = pts[ptst].x;
  *ymin = pts[ptst].y;
  *xmax = *xmin;
  *ymax = *ymin;
  do {
    pt = (pt + 1) % cont->psize;
    if (*xmin > pts[pt].x)
      *xmin = pts[pt].x;
    if (*xmax < pts[pt].x)
      *xmax = pts[pt].x;
    if (*ymin > pts[pt].y)
      *ymin = pts[pt].y;
    if (*ymax < pts[pt].y)
      *ymax = pts[pt].y;
  } while (pt != ptnd);
}



#define SEGSCAN_DIV 30
static void scan_points_to_segments(Icont *c1, Icont *c2, double *legalmin,
				    double *dsqrmin, int *pt1, int *pt2, 
				    float *tbest, Ipoint *close,
				    int docheck, int samelevel, Iobj *obj,
				    int *olist, int nother, int nothersame,
				    Icont *other)
{
  float dx, dy, xs, ys, xe, ye, x0, y0, dsqr, t, sep;
  int pta, ptb, next, legal, i, j, ind;
  int ndiv1, ndiv2, nperdiv1, nperdiv2, ptst, ptnd, ptand;
  Ipoint *pts1, *pts2;
  Ipoint cltmp;
  float xmin1[SEGSCAN_DIV], xmax1[SEGSCAN_DIV];
  float ymin1[SEGSCAN_DIV], ymax1[SEGSCAN_DIV];
  float xmin2[SEGSCAN_DIV], xmax2[SEGSCAN_DIV];
  float ymin2[SEGSCAN_DIV], ymax2[SEGSCAN_DIV];
  float bbsep[SEGSCAN_DIV * SEGSCAN_DIV];
  int indorder[SEGSCAN_DIV * SEGSCAN_DIV];

  /* divide each contour into segments and get the mins and maxes of each */
  ndiv1 = SEGSCAN_DIV;
  if (c1->psize < ndiv1)
    ndiv1 = c1->psize;
  nperdiv1 = (c1->psize + ndiv1 - 1) / ndiv1;
  ndiv1 = (c1->psize + nperdiv1 - 1) / nperdiv1;
  for (j = 0; j < ndiv1; j++) {
    ptst = nperdiv1 * j;
    ptnd = ptst + nperdiv1 - 1;
    if (ptnd >= c1->psize)
      ptnd = c1->psize - 1;
    segment_mm(c1, ptst, ptnd, &xmin1[j], &xmax1[j], &ymin1[j],
	       &ymax1[j]);
  }

  ndiv2 = SEGSCAN_DIV;
  if (c2->psize < ndiv2)
    ndiv2 = c2->psize;
  nperdiv2 = (c2->psize + ndiv2 - 1) / ndiv2;
  ndiv2 = (c2->psize + nperdiv2 - 1) / nperdiv2;
  for (j = 0; j < ndiv2; j++) {
    ptst = nperdiv2 * j;
    ptnd = ptst + nperdiv2;
    if (ptnd >= c2->psize)
      ptnd = 0;
    segment_mm(c2, ptst, ptnd, &xmin2[j], &xmax2[j], &ymin2[j],
	       &ymax2[j]);
  }

  /* measure separations between divisions, remember to square them! */
  for (i = 0; i < ndiv1; i++)
    for (j = 0; j < ndiv2; j++) {
      ind = i * ndiv2 + j;
      sep = 0;
      if (sep < xmin1[i] - xmax2[j])
	sep = xmin1[i] - xmax2[j];
      if (sep < xmin2[j] - xmax1[i])
	sep = xmin2[j] - xmax1[i];
      if (sep < ymin1[i] - ymax2[j])
	sep = ymin1[i] - ymax2[j];
      if (sep < ymin2[j] - ymax1[i])
	sep = ymin2[j] - ymax1[i];
      bbsep[ind] = sep * sep;
      indorder[ind] = ind;
    }

  /* order by increasing separation */
  for (i = 0; i < ndiv1 * ndiv2 - 1; i++)
    for (j = i + 1; j < ndiv1 * ndiv2; j++)
      if (bbsep[indorder[i]] > bbsep[indorder[j]]) {
	ind = indorder[i];
	indorder[i] = indorder[j];
	indorder[j] = ind;
      }

  pts1 = c1->pts;
  pts2 = c2->pts;

  /* loop on the divisions in that order, skip ones above current minimum */
  for (i = 0; i < ndiv1 * ndiv2; i++) {
    if (bbsep[indorder[i]] >= *legalmin)
      continue;
    ind = indorder[i] / ndiv2;
    pta = ind * nperdiv1;
    ptand = pta + nperdiv1;
    if (ptand > c1->psize)
      ptand = c1->psize;
    ind = indorder[i] % ndiv2;
    ptst = ind * nperdiv2;
    ptnd = ptst + nperdiv2;    /* DNM fixed 4/9/02, ptb -> ptst */
    if (ptnd > c2->psize)
      ptnd = c2->psize;
    for (; pta < ptand; pta++) {
      x0 = pts1[pta].x;
      y0 = pts1[pta].y;
      xs = pts2[ptst].x;
      ys = pts2[ptst].y;
      for (ptb = ptst; ptb < ptnd; ptb++) {

	/* determine closest approach between point in c1 and
	   segment in c2 */
	next = (ptb + 1) % c2->psize;
	xe = pts2[next].x;
	ye = pts2[next].y;
	dx = xe - xs;
	dy = ye - ys;
	t = 0.;
	if (dx != 0. || dy != 0.) {
	  t = ((x0 - xs) * dx + (y0 - ys) * dy) /
	    (dx * dx + dy * dy);
	  if (t < 0.)
	    t = 0.;
	  if (t > 1.)
	    t  = 1.;
	}
	cltmp.x = xs + t * dx;
	cltmp.y = ys + t * dy;
	dx = cltmp.x - x0;
	dy = cltmp.y - y0;
	dsqr = dx * dx + dy * dy;
	if (dsqr < *legalmin) {

	  /* if less than the current min, check legality if
	     required on this call */
	  legal = 1;
	  if (docheck)
	    legal = check_legal_joiner(pts1[pta], cltmp, c1,
				       c2, samelevel, obj,
				       olist, nother,
				       nothersame, other);
	  /* if still legal, save new min */
	  if (legal) {
	    *legalmin = dsqr;
	    *pt1 = pta;
	    *pt2 = ptb;
	    *tbest = t;
	    cltmp.z = pts2[ptb].z;
	    *close = cltmp;
			     
	  }
	}
	if (*legalmin > 1.e20 && dsqr < *dsqrmin) {

	  /* or, if no legal min found yet, keep track of
	     nonlegal minimum as a fallback */
	  *dsqrmin = dsqr;
	  *pt1 = pta;
	  *pt2 = ptb;
	  *tbest = t;
	  cltmp.z = pts2[ptb].z;
	  *close = cltmp;
	}
	xs = xe;
	ys = ye;
      }
    }
  }
}

static int find_closest_contour(Icont *tcont, Icont *onecont, Iobj *obj, 
				int *list, int ncont,
				int *used, int *pt1min, int *pt2min, 
				int *firstbest, float *tbest, Ipoint *close,
				int *olist, int nother, int nothersame, 
				Icont *other, int nsamelevel)
{
  Icont *jcont;
  Ipoint testpt1, testpt2, closet;
  int jmin, samelevel, j;
  double legalmin, distmin, lastmin;
  int pt1mint, pt2mint;

  /* first pass: find contour with absolute closest approach regardless
     of legality of connector */
  legalmin = 1.e30;
  lastmin = 1.e30;
  distmin = 1.e30;
  if (onecont)
    ncont = 2;
  for (j = 1; j < ncont; j++) {
    jcont = onecont;
    if (!onecont) {
      if (used[j])
	continue;
      jcont = &(obj->cont[list[j]]);
    }

    /* first measure points of tcont to segments of jcont; then 
       vice versa */
    /* Have to use temporary variables for pt1min, pt2min, and close
       because on the PC, scan_points_to_segments may find a new minimum
       and change these values without this calling routine detecting a
       change in legalmin.  Only when a change is detected out here are
       the actual pt1min, pt2min and close modified */
    scan_points_to_segments(tcont, jcont, &legalmin, &distmin,
			    &pt1mint, &pt2mint, tbest, &closet,
			    0, 0, obj, olist, nother, nothersame, other);
    if (legalmin < lastmin) {
      lastmin = legalmin;
      *firstbest = 1;
      *pt1min = pt1mint;
      *pt2min = pt2mint;
      *close = closet;
      jmin = j;
      testpt1 = tcont->pts[*pt1min];
      testpt2 = *close;
    }
    scan_points_to_segments(jcont, tcont, &legalmin, &distmin,
			    &pt2mint, &pt1mint, tbest, &closet,
			    0, 0, obj, olist, nother, nothersame, other);
    if (legalmin < lastmin) {
      lastmin = legalmin;
      *firstbest = 0;
      *pt1min = pt1mint;
      *pt2min = pt2mint;
      *close = closet;
      jmin = j;
      testpt2 = jcont->pts[*pt2min];
      testpt1 = *close;
    }
  }

  samelevel = 1;
  jcont = onecont;
  if (!onecont) {
    jcont = &(obj->cont[list[jmin]]);
    if (jmin >= nsamelevel)
      samelevel = 0;
  }
  if(!check_legal_joiner(testpt1, testpt2, tcont, jcont, samelevel,
			 obj, olist, nother, nothersame, other)) {

    /* if first pass didn't give legal connector, then find closest
       legal connector by brute force, checking each new candidate
       closest distance */
    distmin = 1.e30;
    legalmin = 1.e30;
    lastmin = 1.e30;
    for (j = 1; j < ncont; j++) {
      samelevel = 1;
      jcont = onecont;
      if (!onecont) {
	if (used[j])
	  continue;
	jcont = &(obj->cont[list[j]]);
	if (j >= nsamelevel)
	  samelevel = 0;
      }

      scan_points_to_segments(tcont, jcont, &legalmin, &distmin,
			      &pt1mint, &pt2mint, tbest, &closet,
			      1, samelevel, obj, olist, nother,
			      nothersame, other);
      if (legalmin < lastmin || 
	  (legalmin > 1.e20 && distmin < lastmin)) {
	lastmin = legalmin;
	if (legalmin > 1.e20)
	  lastmin = distmin;
	*firstbest = 1;
	*pt1min = pt1mint;
	*pt2min = pt2mint;
	*close = closet;
	jmin = j;
      }
      scan_points_to_segments(jcont, tcont, &legalmin, &distmin,
			      &pt2mint, &pt1mint, tbest, &closet,
			      1, samelevel, obj, olist, nother,
			      nothersame, other);
      if (legalmin < lastmin || 
	  (legalmin > 1.e20 && distmin < lastmin)) {
	lastmin = legalmin;
	if (legalmin > 1.e20)
	  lastmin = distmin;
	*firstbest = 0;
	*pt1min = pt1mint;
	*pt2min = pt2mint;
	*close = closet;
	jmin = j;
      }
    }
  }

  return(jmin);
}


static Icont *join_all_contours(Iobj *obj, int *list, int ncont, 
				int nsamelevel, int fill, int *olist, 
				int nother, int nothersame, Icont *other)
{
  Icont *tcont, *jcont;
  Icont *tempcont;
  int i, j, jmin, tptmin, jptmin, firstbest, counterdir;
  int *used;
  float tbest;
  Ipoint close;

  /* start with first contour */
  tcont = imodContourDup(&(obj->cont[list[0]]));
  if (!tcont)
    return(NULL);
  if (ncont < 2)
    return(tcont);

  used = (int *)malloc(ncont * sizeof(int));
  if (!used)
    return (NULL);
  for (i = 0; i < ncont; i++)
    used[i] = 0;

  /* loop to connect one more contour each time */
  for (i = 1; i < ncont ; i++) {

    jmin = find_closest_contour(tcont, NULL, obj, list, ncont, used,
				&tptmin, &jptmin, &firstbest, &tbest, 
				&close, olist, nother, nothersame,
				other, nsamelevel);

    jcont = imodContourDup(&(obj->cont[list[jmin]]));
    if (!jcont)
      return(NULL);
    used[jmin] = 1;

    /* eliminate overlap for contours on the basic level.
       If eliminating overlap actually moved some points, reevaluate
       closest points */
    if (jmin < nsamelevel)
      if (eliminate_overlap(tcont, jcont)) 
	find_closest_contour(tcont, jcont, obj, list, ncont, used,
			     &tptmin, &jptmin, &firstbest, &tbest,
			     &close, olist, nother, nothersame,
			     other, nsamelevel);

    /* Add the connector point if it is inside a segment */
    if (tbest > 0. && tbest < 1.) {
      if (firstbest) {
	jptmin++;
	imodPointAdd(jcont, &close, jptmin);
      } else {
	tptmin++;
	imodPointAdd(tcont, &close, tptmin);
      }
    }	 

    counterdir = 0;
    if (jmin >= nsamelevel)
      counterdir = 1;

    tempcont = tcont;
    tcont = imodContourJoin(tempcont, jcont, tptmin, jptmin, fill, 
			    counterdir);
    if (!tcont)
      return(NULL);
    imodContourDelete(tempcont);
    imodContourDelete(jcont);
  }
  /*     for (i = 0; i < tcont->psize; i++)
	 printf ("%.1f %.1f %.1f\n", tcont->pts[i].x, tcont->pts[i].y,
	 tcont->pts[i].z); */


  free(used);
  return (tcont);
}

/* DNM: routine to add mesh index and reallocate new memory in chunks.
   Frequent reallocs are very expensive */

#define CHUNKSIZE 8192

static int chunkMeshAddIndex(Imesh *mesh, int index, int *maxlist)
{
  int *tmp;
     
  if (mesh->lsize >= *maxlist) {
    if ((mesh->lsize > 0) && (mesh->list)) {
      *maxlist += CHUNKSIZE;
      tmp = (int *)realloc(mesh->list, *maxlist * sizeof(int));
    } else {
      *maxlist = CHUNKSIZE;
      tmp = (int *)malloc(*maxlist * sizeof(int));
      mesh->lsize = 0;
    }
    if (tmp == NULL)
      return(-1);
     
    mesh->list = tmp;
  }
  mesh->list[mesh->lsize] = index;
  mesh->lsize++;
  return(0);
}

static void subtract_scan_contours(Icont *cs1, Icont *cs2)
{
  int i, j, jstrt;
  float e1, s1, e2, s2;

  if (!cs1) return;
  if (!cs2) return;

  jstrt = 0;
  for(i = 0; i < cs1->psize - 1; i+=2)
    for(j = jstrt; j < cs2->psize - 1; j+=2){
      if (cs1->pts[i].y == cs2->pts[j].y){

	s1 = cs1->pts[i].x;
	e1 = cs1->pts[i+1].x;
	s2 = cs2->pts[j].x;
	e2 = cs2->pts[j+1].x;
		    
	if (s1 < e2 && s2 < e1) {
	  if (s1 < s2 && e1 <= e2)
	    cs1->pts[i+1].x = s2;   /* truncate right end */
	  else if ( s1 >= s2 && e1 > e2)
	    cs1->pts[i].x = e2;     /* truncate left end */
	  else if (s1 >= s2 && e1 < e2) {
	    /* wipe out the line completely */
	    imodPointDelete(cs1, i);
	    imodPointDelete(cs1, i);
	    i -= 2;
	    break;
	  } else {
	    /* split the line in two */
	    imodPointAdd(cs1, &cs2->pts[j], i + 1);
	    imodPointAdd(cs1, &cs2->pts[j+1], i + 2);
	  }
	}

      } else if (cs1->pts[i].y > cs2->pts[j].y)
	jstrt = j;
      else
	break;
    }
}     

static int mesh_contours(Iobj *obj, Icont *bcont, Icont *tcont, int surf,
			 int time, Ipoint *scale, int inside)
{
  Imesh *nmesh;
  if (!tcont || !bcont){
    fprintf(stderr, "Fatal Error: "
	    "Not enough memory to get new contour.\n");
    return(-1);
  }

  if (bcont->psize == 1) imodPointAppend(bcont, bcont->pts);
  if (tcont->psize == 1) imodPointAppend(tcont, tcont->pts);

  if (obj->flags & IMOD_OBJFLAG_OUT)
    inside = 1 - inside;

  nmesh = imeshContoursCost(bcont, tcont, scale, inside, 0);
  if (nmesh){
    nmesh->pad = surf;
    nmesh->type = time;
    obj->mesh = imodel_mesh_add
      (nmesh, obj->mesh, &(obj->meshsize));
    free(nmesh);
  }
  imodContourDelete(tcont);
  imodContourDelete(bcont);
  return 0;
}


/*********************************************************/
/* Break a contour into two contours at st1 and st2      */
/* fill =  1, fill in the line connecting contours up.   */
/* fill = -1, fill in the line connecting contours down. */
/* fill =  0, dont fill.                                 */
static int break_contour_inout(Icont *cin, int st1, int st2,  int fill,
			       Icont **cout1, Icont **cout2)
{
  Ipoint point;
  Icont *c1, *c2;
  int pt, pto;
  int reversed = 0;
  if (st2 < st1) {
    reversed = st1;
    st1 = st2;
    st2 = reversed;
    reversed = 1;
  }

  if (!cin)
    return 1;

  /* set up new contour and fill point. */
  c1 = imodContourNew();
  c2 = imodContourNew();
  if (!c1 || !c2)
    return 1;

  /* transfer an open flag to the c1 contour, which will acquire the
     endpoints and opening of cin */
  if (cin->flags & ICONT_OPEN)
    c1->flags |= ICONT_OPEN;

  point.x = (cin->pts[st1].x + cin->pts[st2].x) * 0.5f;
  point.y = (cin->pts[st1].y + cin->pts[st2].y) * 0.5f;
  point.z = (cin->pts[st1].z + cin->pts[st2].z) * 0.5f;
  if (fill == 1)
    point.z += 0.75f;
  if (fill == -1)
    point.z -= 0.75f;

  c1->psize = st1 + 1 + cin->psize - st2;
  c2->psize = st2 + 1 - st1;
  if (fill) {
    c1->psize++;
    c2->psize++;
  }
  c1->pts = (Ipoint *)malloc(c1->psize * sizeof(Ipoint));
  c2->pts = (Ipoint *)malloc(c2->psize * sizeof(Ipoint));

  /* add points to new contours */
  pto = 0;
  for(pt = 0; pt <= st1; pt++)
    c1->pts[pto++] = cin->pts[pt];
  if (fill)
    c1->pts[pto++] = point;
  for(pt = st2; pt < cin->psize; pt++)
    c1->pts[pto++] = cin->pts[pt];
  pto = 0;
  if (fill)
    c2->pts[pto++] = point;
  for(pt = st1; pt <= st2; pt++)
    c2->pts[pto++] = cin->pts[pt];
  *cout1 = c1;
  *cout2 = c2;
  if (reversed) {
    *cout1 = c2;
    *cout2 = c1;
  }
  return 0;
}
