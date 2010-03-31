/*
 *  imodinfo.c --  Prints info about imod files to std out.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"
#include "b3dutil.h"

#define MINFO_SPECIAL  99
#define MINFO_STANDARD 1
#define MINFO_CHART    2
#define MINFO_DIST     3
#define MINFO_ASCII    5
#define MINFO_LENGTH   6
#define MINFO_SURFACE  7
#define MINFO_OBJECT   8
#define MINFO_POINTS   9
#define MINFO_RATIOS   10

static double info_contour_length(Icont *cont, int objflags,
                                  double pixsize, double zscale);
static double info_contour_surface_area(Icont *cont, int objflags,
                                        double pixsize, double zscale);
static double info_contour_area(Icont *cont, int objflags,
                                double pixsize, double zscale);
static double info_contour_vol(Icont *cont, int objflags,
                               double pixsize, double zscale);
static void imodinfo_points(Imod *imod, int subarea, Ipoint ptmin,
                            Ipoint ptmax, int useclip, int verbose);
static void imodinfo_ratios(Imod *imod);
static void imodinfo_object(struct Mod_Model *imod, int scaninside, 
                            int subarea, Ipoint min, Ipoint max,
                            int useclip);
static void imodinfo_full_object_report(Imod *imod, int ob, int scaninside, 
                                        int subarea, Ipoint ptmin,
                                        Ipoint ptmax, int useclip);
static int contour_stats(Icont *cont, int flags, double pixsize, 
                         double zscale);
static Iobj *imodinfo_ObjectClip(Iobj *obj, Iplane *plane, int planes);

static void print_units(int units);
static void imodinfo_special(Imod *imod, char *fname);
static void imodinfo_print_model(Imod *model, int verbose, int scaninside, 
                                 int subarea, Ipoint ptmin, Ipoint ptmax,
                                 int useclip);
static void imodinfo_objndist(struct Mod_Model *imod, int bins);
static void imodinfo_length(Imod *imod);
static float imeshSurfaceSubarea(Imesh *mesh, Ipoint *scale, Ipoint min,
                                 Ipoint max, int doclip, Iplane *plane,
                                 int nPlanes);
static double scan_contour_area(struct Mod_Contour *cont);
static void trim_scan_contour(Icont *cont, Ipoint min, Ipoint max, int doclip,
                              Iplane *plane, int nPlanes);
static void imodinfo_object(struct Mod_Model *imod, int scaninside, 
                            int subarea, Ipoint min, Ipoint max,
                            int useclip);
static void computeObjectAreaVol(Imod *model, Iobj *obj, int scaninside, 
                                 int subarea, Ipoint min, Ipoint max,
                                 int useclip, double *surf, double *vol,
                                 double *msurf, double *mvol, double *inmvol,
                                 Ipoint *cent);
static float contourVolumeFactor(Iobj *obj, Icont *cont, Ipoint min, 
                                 Ipoint max);
static float scanned_volume(Iobj *obj, int subarea, Ipoint ptmin, Ipoint ptmax,
                            int doclip, Iplane *plane, int nPlanes, 
                            double *meshVol);
static int contourSubareaByScan(Icont *cont, Ipoint ptmin, Ipoint ptmax,
                                int doclip, Iplane *plane, int nPlanes, 
                                int makeScan, Icont **scancont, Ipoint *pmin,
                                Ipoint *pmax, float *area);
static double clippedTriangleArea(Imesh *mesh, int i, float zs, Ipoint min,
                                  Ipoint max, int doclip, Iplane *plane,
                                  int nPlanes, int listInc, int vertBase);
static void imodinfo_surface(Imod *imod, int scaninside, Ipoint min,
                             Ipoint max, int useclip);
static int imodinfo_objdist(Imod *imod, int bins);


static void imodinfo_usage(char *name)
{
  printf("usage: %s [options] <imod filename>\n", name);
  printf("options:\n");

  printf("\t-a\tPrint ascii readable version of imod file.\n");
  printf("\t-c\tPrint centroids of closed objects.\n");
  printf("\t-l\tPrint lengths of contours in column output.\n");
  printf("\t-s\tPrint surface information.\n");
  printf("\t-p\tPrint point size information.\n");
  printf("\t-r\tPrint ratio of length to area for closed contours.\n");
  printf("\t-o list\tPrint full report on list of objects.\n");
  printf("\t-i\tAnalyze for inside contours and adjust volume.\n");
  printf("\t-x min,max   Compute volume, mesh area, and point count and sizes\n");
  printf("\t-y min,max         between min and max in X (-x), Y (-y), or Z (-z).\n");
  printf("\t-z min,max   \n");
  printf("\t-t 1/-1\tApply clipping plane in normal (1) or inverted (-1) orientation\n");
  printf("\t-v\tBe verbose on model output.\n");
  printf("\t-vv\tBe more verbose on model output (prints points).\n");
  printf("\t-h\tHush - no contour or point data in standard or point output.\n");
  printf("\t-f filename  Write output to file.\n");
  return;
}

static FILE *fout;
static int Debug = FALSE;

int main( int argc, char *argv[])
{
  int    i;
  int    verbose = 0;
  int    scaninside = FALSE;
  int    subarea = FALSE;
  int    useclip = 0;
  int    ob;
  int    mode = MINFO_STANDARD;
  int    hush = FALSE;
  FILE   *fin;
  Imod   *model;
  int bins = 0;
  int *list;
  int nlist;
  int errcode;
  Ipoint ptmin = {-1.e30, -1.e30, -1.e30};
  Ipoint ptmax = {1.e30, 1.e30, 1.e30};
  char *progname = imodProgName(argv[0]);

     
  if (argc == 1){
    imodVersion(progname);
    imodCopyright();
    imodinfo_usage(progname);
    exit(1);
  }
  fout = stdout;

  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
               
      case 'b':
        bins = atoi(argv[++i]);
        break;
               
      case 'l':
        mode = MINFO_LENGTH;
        break;
               
      case 'f':
        fout = fopen(argv[++i], "w");
        if (!fout) {
          fprintf(stderr, "%s: Error opening output file %s\n",
                  progname, argv[++i]);
          exit(1);
        }
        break;
                    
      case 'a':
        mode = MINFO_ASCII;
        break;

      case 's':
        mode = MINFO_SURFACE;
        break;

      case 'p':
        mode = MINFO_POINTS;
        break;

      case 'r':
        mode = MINFO_RATIOS;
        break;

      case 'v':
        verbose++;
        if (argv[i][2] == 'v')
          verbose++;
        break;

      case 'o':
        list = parselist(argv[++i], &nlist);
        if (!list) {
          fprintf(stderr, "%s: error parsing list %s\n", progname, argv[i]);
          exit (2);
        }
        mode = MINFO_OBJECT;
        break;

      case 'c':
        mode = MINFO_CHART;
        break;
               
      case 'n':
        mode = MINFO_DIST;
        break;
               
      case 'i':
        scaninside = TRUE;
        break;
               
      case 'h':
        hush = TRUE;
        break;
               
      case 'D':
        Debug = TRUE;
        break;
               
      case 't':
        sscanf(argv[++i], "%d", &useclip);
        scaninside = TRUE;
        break;
               
      case 'x':
        sscanf(argv[++i], "%f%*c%f", &ptmin.x, &ptmax.x);
        scaninside = TRUE;
        subarea = TRUE;
        break;

      case 'y':
        sscanf(argv[++i], "%f%*c%f", &ptmin.y, &ptmax.y);
        scaninside = TRUE;
        subarea = TRUE;
        break;

      case 'z':
        sscanf(argv[++i], "%f%*c%f", &ptmin.z, &ptmax.z);
        subarea = TRUE;
        break;

      default:
        fprintf(stderr, "%s: unknown option %s\n", progname, argv[i]);
        imodinfo_usage(progname);
        exit(2);
        break;

      }
    } else
      break;
  }
  /* DNM: took out section that was #ifdef OLDOPT, it's irrelevant */
      
  if (i >= argc) {
    imodinfo_usage(progname);
    exit(2);
  }

  if (hush && !verbose)
    verbose = -1;
  for (; i < argc; i++){
    fin = fopen(argv[i], "rb");
    if (!fin){
      fprintf(stderr, "%s: Error, couldn't read file %s\n",
              progname, argv[i]);
      exit(3);
    }

    model = imodNew();
    if (!model) {
      fprintf(stderr, "%s: Error allocating model structure\n", progname);
      exit(3);
    }
    model->file = fin;
    errcode = imodReadFile(model);
    if (errcode){
      fprintf(stderr, "%s: Error (%d) reading imod model. (%s)\n", 
              progname, errcode, argv[i]);
      /*             exit(3); */

    }

    fprintf(fout, "# MODEL %s\n", argv[i]);
    fprintf(fout, "# NAME  %s\n", model->name);
    fprintf(fout, "# PIX SCALE:  x = %g\n", model->xscale);
    fprintf(fout, "#             y = %g\n", model->yscale);
    fprintf(fout, "#             z = %g\n", model->zscale);
    fprintf(fout, "# PIX SIZE      = %g\n", model->pixsize);
    fprintf(fout, "# UNITS: ");
          
    print_units(model->units);

    if (model->refImage){
      fprintf(fout, "\n# Model to Image index coords:\n");
      fprintf(fout, "#      SCALE  = ( %g, %g, %g)\n",
              model->refImage->cscale.x, 
              model->refImage->cscale.y,
              model->refImage->cscale.z);
      fprintf(fout, "#      OFFSET = ( %g, %g, %g)\n",
              model->refImage->ctrans.x, 
              model->refImage->ctrans.y,
              model->refImage->ctrans.z);
      fprintf(fout, "#      ANGLES = ( %g, %g, %g)\n",
              model->refImage->crot.x, 
              model->refImage->crot.y,
              model->refImage->crot.z);

    }

          

    fprintf(fout, "\n\n");
          
    switch(mode){

    case MINFO_ASCII:
      /* DNM 12/7/04: fixed to just assign fout to file */
      model->file = fout;
      imodWriteAscii(model);
      break;

    case MINFO_SPECIAL:
      if (fin){
        fclose(fin);
        fin = NULL;
      }
      imodinfo_special(model, argv[i]);
      break;

    case MINFO_STANDARD:
      imodinfo_print_model(model, verbose, scaninside, subarea,
                           ptmin, ptmax, useclip);
      break;

    case MINFO_SURFACE:
      imodinfo_surface(model, scaninside, ptmin, ptmax, useclip);
      break;

    case MINFO_POINTS:
      imodinfo_points(model, subarea, ptmin, ptmax, useclip, verbose);
      break;

    case MINFO_RATIOS:
      imodinfo_ratios(model);
      break;

    case MINFO_CHART:
      imodinfo_object(model, scaninside, subarea, ptmin, ptmax, useclip);
      break;
    case MINFO_DIST:
      imodinfo_objndist(model, bins);
      break;
    case MINFO_LENGTH:
      imodinfo_length(model);
      break;
    case MINFO_OBJECT:
      for (ob = 0; ob < nlist; ob++) {
        if (ob)
          fprintf(fout, "\n");
        imodinfo_full_object_report(model, list[ob], scaninside, subarea,
                           ptmin, ptmax, useclip);
      }
      break;
    default:
      break;
    }
    if (fin)
      fclose(fin);
    fprintf(fout, "\n\n\n\n");
    imodDelete(model);
  }
  exit(0);
}

/*
 * The default model output.
 * Inputs:  model - the model structure
 *          verbose  - flag indicating verbose level
 *          scaninside - flag that scan contours are needed to find inside
 *                       contours and deal with subsets and clipping
 *          subarea    - flag that a subarea is specified
 *          min, max   - points with limits of subarea
 *          useclip    - flag to use clipping planes, if any, to find region to
 *                        include (1) or exclude (-1)
 */
static void imodinfo_print_model(Imod *model, int verbose, int scaninside,
                                 int subarea, Ipoint min, Ipoint max,
                                 int useclip)
{
  double tsa, tvol;
  double dist;
  double sa, msa, inmvol;
  double mvol;
  int ob, co, pt, npt, coz;
  Iobj  *obj;
  Icont *cont;
  Ipoint mscale, p1;
  Iplane plane[2 * IMOD_CLIPSIZE];
  int doclip, goodside, nPlanes;
  float volFac;
  Iview *view = &model->view[model->cview];

  if (!model->objsize){
    fprintf(fout, "Model has no objects!!!\n");
    return;
  }

  for (ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    tsa = 0; tvol = 0.; mvol = 0.; inmvol = 0.; msa = 0.;
    nPlanes = 0;
    /* Get clipping planes in 4 parameter form; c is already
       multiplied by zscale so no need to 
       multiply z coordinates by zscale for tests*/
    imodPlaneSetFromClips(&obj->clips, &view->clips, plane, 2 * IMOD_CLIPSIZE,
                          &nPlanes);
    doclip = nPlanes ? useclip : 0;

    fprintf(fout, "OBJECT %d\n", ob + 1);
    fprintf(fout, "NAME:  %s\n", obj->name);
    fprintf(fout, "       %d contours\n", obj->contsize);
    if (obj->flags & IMOD_OBJFLAG_OFF)
      fprintf(fout, "       object drawing is turned off\n");
    if (obj->flags & IMOD_OBJFLAG_SCAT)
      fprintf(fout, "       object uses scattered points.\n");
    else if (obj->flags & IMOD_OBJFLAG_OPEN)
      fprintf(fout, "       object uses open contours.\n");
    else
      fprintf(fout, "       object uses closed contours.\n");
    if (obj->flags & IMOD_OBJFLAG_OUT)
      fprintf(fout, "       contours in object are inside out.\n");
    fprintf(fout, "       color (red, green, blue) = (%g, %g, %g)\n",
            obj->red, obj->green, obj->blue);
    fprintf(fout, "\n");

    if (scaninside && !(obj->flags & IMOD_OBJFLAG_OPEN)) {
      tvol = model->pixsize * model->pixsize * 
        scanned_volume(obj, subarea, min, max, doclip, plane, nPlanes, &mvol);
    } else {
      for(co = 0; co < obj->contsize; co++){
        cont = &(obj->cont[co]);
        npt = cont->psize;
               
        /* DNM: scattered points in subarea, or clipping plane 
           active: count points inside */
        if ((subarea || doclip) && 
            (obj->flags & IMOD_OBJFLAG_SCAT)) {
          npt = 0;
          for (pt = 0; pt < cont->psize; pt++) {
            p1 = cont->pts[pt];
            if (doclip) {
              goodside = imodPlanesClip(plane, nPlanes, &p1);
              if ((goodside && doclip < 0) || 
                  (!goodside && doclip > 0))
                continue;
            }                            
            if (p1.x >= min.x && p1.x <= max.x &&
                p1.y >= min.y && p1.y <= max.y &&
                p1.z >= min.z && p1.z <= max.z)
              npt++;
          }
        }
        if (verbose >= 0) 
          fprintf(fout, "\tCONTOUR #%d,%d,%d  %d points", 
                  co + 1, ob + 1,  cont->surf, npt);
        if (verbose > 1){
          fprintf(fout, "\n\t");
          imodLabelPrint(cont->label, stdout);
        }
        if (!cont->psize) {
          if (verbose >= 0)
            fprintf(fout, "\n");
          continue;
        }
                    
        /* DNM: skip if doing subarea and contour outside limits */
        if (subarea && !(obj->flags & IMOD_OBJFLAG_OPEN)) {
          coz = imodContourZValue(cont);
          if (coz < min.z || coz > max.z) {
            if (verbose >= 0)
              fprintf(fout, "\n");
            
            /* But see if need to add contour to mesh volume */
            if (!(obj->flags & IMOD_OBJFLAG_OPEN)) {
              volFac = contourVolumeFactor(obj, cont, min, max);
              if (volFac > 0.)
                mvol += volFac * imodContourArea(cont) * model->pixsize *
                  model->pixsize;
            }
            continue;
          }
        }
                    
        /* DNM: modified to give zscale correct lengths in all 
           cases */
        if (verbose <= 0){
          dist = info_contour_length(cont, obj->flags, 
                                     (double)model->pixsize,
                                     (double)model->zscale);
          if (!(obj->flags & IMOD_OBJFLAG_OPEN)){
            if (verbose >= 0)
              fprintf(fout, ", length = %g, ", dist);
            sa = imodContourArea(cont);
            sa *= model->pixsize * model->pixsize;
            if (verbose >= 0)
              fprintf(fout, " area = %g\n", sa);
            tsa += dist;
            tvol += sa;
            mvol += sa * contourVolumeFactor(obj, cont, min, max);
          }else{
            if (verbose >= 0)
              fprintf(fout, "\tlength = %g %s\n", dist, 
                      imodUnits(model)); 
          }
        }else{
          fprintf(fout, ".\n");
        }
                    
        if (verbose > 1){
          fprintf(fout, "\t\tx\ty\tz\n");
          for(pt = 0; pt < cont->psize; pt++)
            fprintf(fout, "\t\t%g\t%g\t%g\n",
                    cont->pts[pt].x,
                    cont->pts[pt].y,
                    cont->pts[pt].z);
        }
        if (verbose > 0)
          contour_stats(cont, obj->flags, 
                        (double)model->pixsize,
                        (double)model->zscale);
      }
    }

    if (obj->meshsize){

      int mesh, resol;
      mscale.x = model->xscale;
      mscale.y = model->yscale;
      mscale.z = model->zscale;
      imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);
      for(mesh = 0; mesh < obj->meshsize; mesh++)
        if (imeshResol(obj->mesh[mesh].flag) == resol) {
          if (subarea || doclip)
            msa += imeshSurfaceSubarea 
              (&obj->mesh[mesh], &mscale, min, max,
               doclip, plane, nPlanes);
          else
            msa += imeshSurfaceArea (&obj->mesh[mesh],
                                     &mscale);
          inmvol += imeshVolume(&obj->mesh[mesh], &mscale, NULL);
        }
      msa *= model->pixsize * model->pixsize; 
      inmvol *= model->pixsize * model->pixsize * model->pixsize;
    }
    if (mvol > 0.0){
      mvol *= model->zscale * model->pixsize;
      fprintf(fout, "\tTotal contour volume = %g\n", (float)mvol);
    } else if (tvol > 0.0){
      tvol *= model->zscale * model->pixsize;
      fprintf(fout, "\n\tTotal cylinder volume = %g\n", (float)tvol);
    }
    if (inmvol > 0.)
      fprintf(fout, "\tTotal volume inside mesh = %g\n", (float)inmvol);

    if (msa > 0.)
      fprintf(fout, "\tTotal mesh surface area = %g\n", (float)msa);
    else if (tsa > 0.0){
      tsa *= (model->zscale * model->pixsize);
      fprintf(fout, "\tTotal cylinder surface area = %g\n", (float)tsa);
    }
          
    fprintf(fout, "\n");
  }
  return;
}


/* prints volume and surface area, for each surface. Arguments defined as 
   above*/

static void imodinfo_surface(Imod *imod, int scaninside, Ipoint min,
                             Ipoint max, int useclip)
{
  Iobj *obj;
  Icont *cont;
  Icont *scan;
  Ipoint pmin, pmax;
  int ob, co, pt, i, coz, skipz;
  float tvol, volFac;
  double *sa = NULL;
  double *vol = NULL;
  double *mvol = NULL;
  int *nofc = NULL;
  unsigned int maxsurf;
  Iplane plane[2 * IMOD_CLIPSIZE];
  int nPlanes = 0;
  int doclip;
  int listInc, vertBase, normAdd;
  Iview *view = &imod->view[imod->cview];


  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    imodPlaneSetFromClips(&obj->clips, &view->clips, plane, 2 * IMOD_CLIPSIZE,
                          &nPlanes);
    doclip = nPlanes ? useclip : 0;
    maxsurf = 0;
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (cont->surf > maxsurf)
        maxsurf = cont->surf;
    }
    obj->surfsize = maxsurf;
    sa = (double *)malloc((obj->surfsize + 1) * sizeof(double));
    if (!sa)
      return;
    vol = (double *)malloc((obj->surfsize + 1) * sizeof(double));
    if (!vol){
      free(sa);
      return;
    }
    mvol = (double *)malloc((obj->surfsize + 1) * sizeof(double));
    if (!mvol){
      free(sa);
      free(vol);
      return;
    }
    nofc = (int *)malloc((obj->surfsize + 1) * sizeof(int));
    if (!nofc){
      free(vol);
      free(mvol);
      free(sa);
      return;
    }

    for(i = 0; i <= obj->surfsize; i++){
      sa[i] = vol[i] = mvol[i] = 0.0;
      nofc[i] = 0;
    }
      
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (!cont->psize)
        continue;
      i = cont->surf;

      coz = imodContourZValue(cont);
      skipz = 0;
      if (coz < min.z || coz > max.z)
        skipz = 1;

      volFac = contourVolumeFactor(obj, cont, min, max);

      /* Compute subarea if there is a volume factor or this is a Z within
         range; add to mesh volume always, to cylinder vol if within range */
      if (((volFac > 0. && !(scaninside || doclip)) || !skipz) &&
          contourSubareaByScan(cont, min, max, doclip, plane, nPlanes, 0, 
                               &scan, &pmin, &pmax, &tvol)) {
        tvol *= imod->pixsize * imod->pixsize * imod->pixsize * imod->zscale;
        if (volFac)
          mvol[i] += tvol * volFac;
        if (!skipz) {
          nofc[i]++;
          vol[i] += tvol;
        }
      }

      if (!(scaninside || doclip || skipz))
        sa[i] += info_contour_surface_area(cont, obj->flags,
                                           (double)imod->pixsize,
                                           (double)imod->zscale);
    }
          

    fprintf(fout, "\n#Object %d data, %s\n", ob + 1, obj->name);
    if (obj->meshsize){

      int me, found, psurf, resol;
      double *msa;
      double psa;
      Imesh *mesh;
      Ipoint *p1, *p2, *p3;
      float xx, yy, zz;
      float zs = imod->zscale;

      msa = (double *)malloc((obj->surfsize + 1) * sizeof(double));
      if (!msa) {
        free(nofc);
        free(vol);
        free(mvol);
        free(sa);
        return;
      }
      for(i = 0; i <= obj->surfsize; i++)
        msa[i] = 0;

      imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);

      for(me = 0; me < obj->meshsize; me++) {
        mesh = &obj->mesh[me];
        if (!mesh || !mesh->lsize || 
            imeshResol(mesh->flag) != resol)
          continue;
        for (i = 0; i < mesh->lsize; i++) {
          if (imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase, 
                                      &normAdd)) {
            i++;

            /* Get a total area for this polygon and find
               the surface whose contours it matches */
            psa = 0.;
            found = 0;
            while(mesh->list[i] != IMOD_MESH_ENDPOLY){

              psa += clippedTriangleArea(mesh, i, zs, min, max, doclip,
                                         plane, nPlanes, listInc, vertBase);
              p1 = &(mesh->vert[mesh->list[i + vertBase]]);
              i+=listInc;
              p2 = &(mesh->vert[mesh->list[i + vertBase]]);
              i+=listInc;
              p3 = &(mesh->vert[mesh->list[i + vertBase]]);
              i+=listInc;
                                   
              if (!found) {
                /* Scan contours to find match */
                for(co = 0; co < obj->contsize; co++){
                  cont = &(obj->cont[co]);
                  if (!cont->psize)
                    continue;
                  for (pt = 0; pt < cont->psize;
                       pt++) {
                    xx = cont->pts[pt].x;
                    yy = cont->pts[pt].y;
                    zz = cont->pts[pt].z;
                    if (zz != p1->z && zz != p2->z && zz != p3->z)
                      break;
                    if ((xx == p1->x && yy == p1->y && zz == p1->z) ||
                        (xx == p2->x && yy == p2->y && zz == p2->z) ||
                        (xx == p3->x && yy == p3->y && zz == p3->z)) {
                      found = 1;
                      psurf = cont->surf;
                      break;
                    }
                  }
                  if (found)
                    break;
                }
              }
            }
            if (found)
              msa[psurf] += psa * imod->pixsize * imod->pixsize;
          }
        }
      }               
      fprintf(fout, "#Surface : Contours,  Cyl. Volume,  "
              "Mesh Volume,  Cyl. Surface,  Mesh Surface\n");
      for(i = 0; i <= obj->surfsize; i++)
        fprintf(fout, "%7d   %8d   %12.6g  %12.6g  %12.6g  %12.6g\n"
                , i, nofc[i], vol[i], mvol[i], sa[i], msa[i]);
      free(msa);
    } else {
          
      fprintf(fout, "#Surface : Contours,  Cyl. Volume,  Cyl. Surface\n");
      for(i = 0; i <= obj->surfsize; i++)
        fprintf(fout, "%7d   %8d   %12.6g  %12.6g\n", 
                i, nofc[i], vol[i], sa[i]);
    }

    free(sa);
    free(vol);
    free(mvol);
    free(nofc);
  }
  return;
}

/* prints size information for every point, for each contour with sizes or for
   all contours in scattered point objects.  Arguments defined as above */

static void imodinfo_points(Imod *imod, int subarea, Ipoint min, Ipoint max,
                            int useclip, int verbose)
{
  Iobj *obj;
  Icont *cont;
  int ob, co, pt;
  int objheader;
  double rsum, rsqsum, rcubsum;
  float rad, area, volume;
  int nsum;
  float pi = 3.14159;
  int skip, goodside;
  Ipoint *p1;
  Iplane plane[2 * IMOD_CLIPSIZE];
  int nPlanes;
  Iview *view = &imod->view[imod->cview];

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    nPlanes = 0;
    imodPlaneSetFromClips(&obj->clips, &view->clips, plane, 2 * IMOD_CLIPSIZE,
                          &nPlanes);
    objheader = 0;
    rsum = rsqsum = rcubsum = 0.0;
    nsum = 0;

    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (cont->sizes || iobjScat(obj->flags)) {
        if (!objheader) {
          objheader = 1;
          fprintf(fout, "\n#Object %d data, %s\n", ob + 1,
                  obj->name);
        }
        if (verbose >= 0) {
          fprintf(fout, "\tCONTOUR #%d,%d,%d  %d points", 
                  co + 1, ob + 1,  cont->surf, cont->psize);
          if (subarea || (useclip && nPlanes))
            fprintf(fout, " total, before constraints");
          fprintf(fout,"\n");
        }

        for (pt = 0; pt < cont->psize; pt++) {
          /* If doing subarea, skip points that are outside */
          skip = subarea;
          p1 = &cont->pts[pt];
          if (skip) {
            if (p1->x >= min.x && p1->x <= max.x &&
                p1->y >= min.y && p1->y <= max.y &&
                p1->z >= min.z && p1->z <= max.z)
              skip = FALSE;
          }

          /* if using clipping plane, skip points that don't
             pass the test */
          if (!skip && useclip && nPlanes) {
            goodside = imodPlanesClip(plane, nPlanes, p1);
            if ((goodside && useclip < 0) || 
                (!goodside && useclip > 0))
              skip = TRUE;
          }

          if (!skip) {
            rad = imodPointGetSize(obj, cont, pt) *
              imod->pixsize;
            if (verbose >= 0)
              fprintf(fout,"  %11.6g\n", rad);
            rsum += rad;
            rsqsum += rad * rad;
            rcubsum += rad * rad * rad;
            nsum++;
          }                      
        } 
      }
    }
    if (nsum > 0) {
      rad = rsum / nsum;
      area = 4. * pi * rsqsum;
      volume = 4. * pi * rcubsum / 3.;
      fprintf(fout, "\n\tMean radius = %g for %d points.\n"
              "\tImplied total surface area = %g; total volume = %g\n",
              rad, nsum, area, volume);
    }
  }
  return;
}

/* prints area/length ratio for each contour for closed contours */

static void imodinfo_ratios(Imod *model)
{
  Iobj *obj;
  Icont *cont;
  int ob, co;
  double ratio, sa, dist;
     
  for (ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    if (!((obj->flags & IMOD_OBJFLAG_SCAT) || 
          (obj->flags & IMOD_OBJFLAG_OPEN))) {
      fprintf(fout, "OBJECT %d\n", ob + 1);
      fprintf(fout, "NAME:  %s\n", obj->name);

      for(co = 0; co < obj->contsize; co++){
        cont = &(obj->cont[co]);
        if (cont->psize <=2)
          continue;

        dist = info_contour_length(cont, obj->flags, 
                                   (double)model->pixsize,
                                   (double)model->zscale);
        sa = imodContourArea(cont);
        sa *= model->pixsize * model->pixsize;
        ratio = sa / dist;
        fprintf(fout, "%d %g\n", ob + 1, ratio);
      }
    }
  }
  return;
}


/* Prints full report on an object, whose object number (numbered from 1) is
   ob.  Other arguments defined as above */
static void imodinfo_full_object_report(Imod *imod, int ob, int scaninside, 
                                 int subarea, Ipoint ptmin, Ipoint ptmax,
                                 int useclip)
{
  Ipoint min, max, cent;
  int i, co;
  Iobj *obj;
  int contours_with_data = 0;
  double surf, vol, msurf, mvol, inmvol;
  int numClips = 0;
  Iview *view = &imod->view[imod->cview];
  char *units = imodUnits(imod);

  if (ob < 1)
    return;
  if (ob > imod->objsize)
    return;

  obj = &(imod->obj[ob-1]);
  fprintf(fout, "Object # %d:\n", ob);
  fprintf(fout, "%s\n", obj->name);
     
  for(co = 0; co < obj->contsize; co++)
    if (obj->cont[co].psize)
      contours_with_data++;
  fprintf(fout, "\tNumber of Contours = %d\n", obj->contsize);
  fprintf(fout, "\tNumber of Contours with Data = %d\n", contours_with_data);
  fprintf(fout, "\tNumber of Meshes   = %d\n", obj->meshsize);
  fprintf(fout, "\tNumber of Surfaces = %d\n", obj->surfsize);

  fprintf(fout, "\tColor  = (Red, Green, Blue, Alpha) = (%g, %g, %g, %g)\n",
          obj->red, obj->green, obj->blue, (float)(obj->trans * 0.01f));
  fprintf(fout, "\tAmbient Light  = %d\n", obj->ambient);
  fprintf(fout, "\tDiffuse Light  = %d\n", obj->diffuse);
  fprintf(fout, "\tSpecular Light = %d\n", obj->specular);
  fprintf(fout, "\tShininess      = %d\n", obj->shininess);

  imodObjectGetBBox(obj, &min, &max);
  fprintf(fout, "\n\tBounding Box   = { (%g, %g, %g), (%g, %g, %g)}\n",
          min.x, min.y, min.z, max.x, max.y, max.z);

  min.x = min.y = min.z = -1.e30;
  max.x = max.y = max.z = 1.e30;

  computeObjectAreaVol(imod, obj, 0, 0, min, max, 0,
                       &surf, &vol, &msurf, &mvol, &inmvol, &cent);
  fprintf(fout, "\tCenter         = (%g, %g, %g)\n",cent.x, cent.y, cent.z);
  if (mvol > 0.)
    fprintf(fout, "\tContour Volume          = %g %s^3\n", mvol, units);
  else
    fprintf(fout, "\tCylinder Volume         = %g %s^3\n", vol, units);
  if (inmvol > 0.)
    fprintf(fout, "\tVolume Inside Mesh      = %g %s^3\n", inmvol, units);
  if (msurf > 0.)
    fprintf(fout, "\tMesh Surface Area       = %g %s^2\n", msurf, units);
  else
    fprintf(fout, "\tCylinder Surface Area   = %g %s^2\n", surf, units);
  

  for (i = 0; i < obj->clips.count; i++) {
    if (obj->clips.flags & (1 << i)) {
      /* DNM 8/3/01: divide normal.z by zscale for consistency with imod 
         and SDA output */
      fprintf(fout, "\tClip %d Normal    = (%g, %g, %g)\n", i,
              obj->clips.normal[i].x,
              obj->clips.normal[i].y,
              obj->clips.normal[i].z / imod->zscale);
      fprintf(fout, "\tClip %d Point     = (%g, %g, %g)\n", i,
              obj->clips.point[i].x,
              obj->clips.point[i].y,
              obj->clips.point[i].z);
      numClips++;
    }
  }

  /* Add up view planes if they apply */
  if (!(obj->clips.flags & (1 << 7))) {
    for (i = 0; i < view->clips.count; i++)
      if (view->clips.flags & (1 << i))
        numClips++;
  }

  if (subarea || (useclip && numClips)) {
    fprintf(fout, "    Clipped and/or subsetted values:\n");
    computeObjectAreaVol(imod, obj, scaninside, subarea, ptmin, ptmax, useclip,
                         &surf, &vol, &msurf, &mvol, &inmvol, &cent);
    if (mvol > 0.)
      fprintf(fout, "\tContour Volume          = %g %s^3\n", mvol, units);
    else
      fprintf(fout, "\tCylinder Volume         = %g %s^3\n", vol, units);
    if (msurf > 0.)
      fprintf(fout, "\tMesh Surface Area       = %g %s^2\n", msurf, units);
    else if (surf > 0.)
      fprintf(fout, "\tCylinder Surface Area   = %g %s^2\n", surf, units);
  }
  return;
}

/* Prints summary of volume, area, and centroid for each object. 
   Arguments defined as above */

static void imodinfo_object(Imod *imod, int scaninside, 
                                 int subarea, Ipoint min, Ipoint max,
                                 int useclip)
{
  int ob;
  Iobj *obj;
  Ipoint  cent;
  double surf, vol, msurf, mvol, inmvol;


  fprintf(fout, "#Obj     Cyl. Vol      Cont Vol   Vol Inside Mesh   Mesh Surf"
          "                Center\n");
  fprintf(fout, "#-----------------------------------------------------------"
          "---------------------------------\n");

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    computeObjectAreaVol(imod, obj, scaninside, subarea, min, max, useclip,
                         &surf, &vol, &msurf, &mvol, &inmvol, &cent);

    if (!(iobjClose(obj->flags))){
      surf = vol = msurf = mvol = 0.0;
    }

    if (obj->contsize)
      fprintf(fout, "%4d   %12.6g  %12.6g  %12.6g  %12.6g  %9.2f %9.2f %9.2f\n"
              , ob + 1, vol, mvol, inmvol, msurf, cent.x, cent.y, cent.z);
    else
      fprintf(fout, "%4d           0           0           0           0"
              "        x        x        x\n", ob + 1);
  }
  return;
}

/* 
 * Computes cylinder and mesh volumes and surface areas for an object.
 * Other than arguments specified above, 
 * Inputs:  obj   - pointer to object 
 * Outputs: surf  - pointer to cylinder surface area
 *          vol   - pointer to cylinder volume
 *          msurf - pointer to mesh surface area
 *          mvol  - pointer to mesh volume
 *          inmvol - pointer to volume inside mesh
 *          cent  - pointer to point to receive centroid
 */
static void computeObjectAreaVol(Imod *model, Iobj *obj, int scaninside, 
                                 int subarea, Ipoint min, Ipoint max,
                                 int useclip, double *surf, double *vol,
                                 double *msurf, double *mvol, double *inmvol,
                                 Ipoint *cent)
{
  Ipoint ccent, mscale;
  Icont *cont;
  Iplane plane[2 * IMOD_CLIPSIZE];
  int nPlanes = 0;
  Iview *view = &model->view[model->cview];
  int doclip;
  int mesh, resol, co, coz;
  double tweight, weight;
  double zscale = model->zscale;
  double pixsize = model->pixsize;
  float tvol;

  *vol = *mvol = *surf = *msurf = *inmvol = 0.;
  cent->x = 0.0f;
  cent->y = 0.0f;
  cent->z = 0.0f;
  tweight = 0;
  imodPlaneSetFromClips(&obj->clips, &view->clips, plane, 2 * IMOD_CLIPSIZE,
                          &nPlanes);
  doclip = nPlanes ? useclip : 0;
  if (scaninside && !(obj->flags & IMOD_OBJFLAG_OPEN)) {
    *vol = pow(pixsize, 3) * zscale *
      scanned_volume(obj, subarea, min, max, doclip, plane, nPlanes, mvol);
    *mvol *= pow(pixsize, 3) * zscale;
  } else {
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      tvol = info_contour_vol(cont, obj->flags, pixsize, zscale);
      *mvol += tvol * contourVolumeFactor(obj, cont, min, max);
      if (subarea && !(obj->flags & IMOD_OBJFLAG_OPEN)) {
        coz = imodContourZValue(cont);
        if (coz < min.z || coz > max.z)
          continue;
      }
      
      *vol += tvol;
      *surf += info_contour_length(cont, obj->flags, pixsize, zscale);

      /* 2/24/09: Compute differently for open contours, so set a flag 
         for an open contour object */
      setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 
                      obj->flags & IMOD_OBJFLAG_OPEN);
      imodel_contour_centroid(cont, &ccent, &weight);
      setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 0);
      tweight += weight;
      cent->x += ccent.x;
      cent->y += ccent.y;
      cent->z += ccent.z * zscale;
    }
    *surf *= model->pixsize * model->zscale;
    if (tweight){
      cent->x /= (float)tweight;
      cent->y /= (float)tweight;
      cent->z /= (float)tweight;
    }
  }
  
  if (obj->meshsize){

    *msurf = 0.0;
    mscale.x = model->xscale;
    mscale.y = model->yscale;
    mscale.z = model->zscale;
    imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);
    for (mesh = 0; mesh < obj->meshsize; mesh++)
      if (imeshResol(obj->mesh[mesh].flag) == resol) {
        if (subarea || doclip)
          *msurf += imeshSurfaceSubarea(&obj->mesh[mesh], &mscale, min, max,
                                        doclip, plane, nPlanes);
        else
          *msurf+= imeshSurfaceArea (&obj->mesh[mesh], &mscale);
        *inmvol += imeshVolume(&obj->mesh[mesh], &mscale, NULL);
      }
    *msurf *= model->pixsize * model->pixsize; 
    *inmvol *= model->pixsize * model->pixsize * model->pixsize;
  }
}


static void print_units(int units)
{
  switch(units){
  case IMOD_UNIT_PIXEL:
    fprintf(fout, "pixels");
    break;
  case IMOD_UNIT_KILO:
    fprintf(fout, "km");
    break;
  case IMOD_UNIT_METER:
    fprintf(fout, "m");
    break;
  case IMOD_UNIT_CM:
    fprintf(fout, "cm");
    break;
  case IMOD_UNIT_MM:
    fprintf(fout, "mm");
    break;
  case IMOD_UNIT_UM:
    fprintf(fout, "um");
    break;
  case IMOD_UNIT_NM:
    fprintf(fout, "nm");
    break;
  case IMOD_UNIT_ANGSTROM:
    fprintf(fout, "A");
    break;
  case IMOD_UNIT_PM:
    fprintf(fout, "pm");
    break;
  default:
    fprintf(fout, "unknown units");
    break;
  }
  return;
}


/* nearest neighbor distence */
static void imodinfo_objndist(Imod *imod, int bins)
{
  int ob, co, pt, tpt, i;
  Iobj *obj;
  Icont *cont;
  Icont *dcont;
  Ipoint    pnt;
  int d, bin, level;
  double dist, mindist, min, max, binsize, binval, binmin, binmax;
  double pixsize, zscale;
  double *dista;

  if (!bins)
    bins = 20;

  pixsize = imod->pixsize;
  if (pixsize == 0)
    pixsize = 1;
  zscale = imod->zscale;
  if (zscale == 0)
    zscale = 1.0;
          
  dcont = imodContourNew();

  fprintf(fout, "#distance   number\n\n");

  dista = (double *)malloc(imod->objsize * sizeof(double));

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    pnt.x = 0.0f;
    pnt.y = 0.0f;
    pnt.z = 0.0f;
    tpt = 0;
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for(pt = 0; pt < cont->psize; pt++, tpt++){
        pnt.x += cont->pts[pt].x;
        pnt.y += cont->pts[pt].y;
        pnt.z += cont->pts[pt].z * zscale;
      }
    }
    pnt.x /= (float)tpt;
    pnt.y /= (float)tpt;
    pnt.z /= (float)tpt;
    imodPointAdd(dcont, &pnt, dcont->psize);
  }

  mindist = pixsize *
    imodel_point_dist(&(dcont->pts[1]),&(dcont->pts[0]));
  for(i = 0; i < dcont->psize - 1; i++){
    if (i)
      mindist = pixsize *
        imodel_point_dist(&(dcont->pts[i]),&(dcont->pts[0]));
    for(pt = 0; pt < dcont->psize; pt++){
      if (pt == i)
        break;
      dist = pixsize *
        imodel_point_dist(&(dcont->pts[i]),&(dcont->pts[pt]));
      if (dist < mindist) 
        dist++;
    }
    dista[i] = mindist;
  }

  max = dista[0];
  min = dista[0];
  for (d = 1; d < imod->objsize; d++){
    if (dista[d] < min)
      min = dista[d];
    if (dista[d] > max)
      max = dista[d];
  }

  binsize = (max - min) / (double)bins;
  binval = min + (binsize * 0.5);

  for(bin = 0; bin < bins; bin++, binval += binsize){
    level = 0;
    binmin = binval - (binsize * 0.5);
    binmax = binval + (binsize * 0.5);

    for (d = 0; d < imod->objsize; d++)
      if ((dista[d] < binmax) && (dista[d] > binmin))
        level++;
          
    fprintf(fout, "%f\t%d\n", binval, level);     
  }
  return;
}


static int imodinfo_objdist(Imod *imod, int bins)
{

  int ob, co, pt, tpt, i;
  Iobj *obj;
  Icont *cont;
  Icont *dcont;
  Ipoint    pnt;
  int d, bin, level;
  double *dist, min, max, binsize, binval, binmin, binmax;
  double pixsize;
  int   comps = 0;

  if (!bins)
    bins = 100;

  pixsize = imod->pixsize;
  if (pixsize == 0)
    pixsize = 1;

  dcont = imodContourNew();

  for (ob = imod->objsize; ob > 1; ob--)
    comps += ob - 1;

  dist = (double *)malloc(comps * sizeof(double));

  fprintf(fout, "# distance  number.\n\n");


  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    pnt.x = 0.0f;
    pnt.y = 0.0f;
    pnt.z = 0.0f;
    tpt = 0;
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      for(pt = 0; pt < cont->psize; pt++, tpt++){
        pnt.x += cont->pts[pt].x;
        pnt.y += cont->pts[pt].y;
        pnt.z += cont->pts[pt].z;
      }
    }
    pnt.x /= (float)tpt;
    pnt.y /= (float)tpt;
    pnt.z /= (float)tpt;
    imodPointAdd(dcont, &pnt, dcont->psize);
  }

  for(i = 0, d = 0; i < dcont->psize - 1; i++)
    for(pt = i; pt < dcont->psize; pt++, d++){
      dist[d] = pixsize *
        imodel_point_dist(&(dcont->pts[i]),&(dcont->pts[pt]));
    }
     
  max = dist[0];
  min = dist[0];
  for (d = 1; d < comps; d++){
    if (dist[d] < min)
      min = dist[d];
    if (dist[d] > max)
      max = dist[d];
  }

  binsize = (max - min) / (double)bins;
  binval = min + (binsize * 0.5);

  for(bin = 0; bin < bins; bin++, binval += binsize){
    level = 0;
    binmin = binval - (binsize * 0.5);
    binmax = binval + (binsize * 0.5);

    for (d = 0; d < comps; d++)
      if ((dist[d] < binmax) && (dist[d] > binmin))
        level++;
          
    fprintf(fout, "%f\t%d\n", binval, level);     
  }
  return(0);
}

/* Prints statistics for a contour, with no clipping or subset adjustments */
static int contour_stats(Icont *cont, int flags, double pixsize, 
                         double zscale)
{
  float area, cdist, odist;
  float length, width, aspect;
  Ipoint cmass = {0., 0., 0.};
  double orientation;
  int pt, num;
  if (!cont)
    return -1;

  if (iobjScat(flags) || cont->psize < 3){

    for (pt = 0; pt < cont->psize; pt++) {
      cmass.x += cont->pts[pt].x;
      cmass.y += cont->pts[pt].y;
      cmass.z += cont->pts[pt].z;
    }
    num = B3DMAX(1, cont->psize);
    fprintf(fout, "\t\tCenter of Mass     = (%g, %g, %g) in pixel coords.\n",
            cmass.x / num, cmass.y / num, cmass.z / num);
    return 0;
  }

  odist = info_contour_length(cont, (int)(flags | IMOD_OBJFLAG_OPEN),
                              pixsize, zscale);
  cdist = info_contour_length(cont, (int)(flags & ~IMOD_OBJFLAG_OPEN),
                              pixsize, zscale);
  fprintf(fout, "\t\tClosed/Open length = %g / %g\n", cdist, odist);

  area  = imodContourArea(cont);
  area *= pixsize * pixsize;
  fprintf(fout, "\t\tEnclosed Area      = %g\n", area);

  imodContourCenterOfMass(cont, &cmass);
  fprintf(fout, "\t\tCenter of Mass     = (%g, %g, %g) in pixel coords.\n", 
          cmass.x, cmass.y, cmass.z);

  /*     moment = imodContourCenterMoment(cont, &cmass, 1, 1); */
  /*     fprintf(fout, "\t\tMoment of Inertia  = %g \n", moment); */

  fprintf(fout, "\t\tCircle             = %g\n", imodContourCircularity(cont));

  orientation = imodContourLongAxis(cont, 1.0, &aspect, &length);
  orientation /= 0.01745329252; 

  fprintf(fout, "\t\tOrientation        = %g degrees.\n", orientation);

  /*     imodContourRotateZ(cont, -orientation);
         imodContourGetBBox(cont, &ll, &ur);
         length = ur.x - ll.x;
         width  = ur.y - ll.y;
         imodContourRotateZ(cont, orientation); */

  width = (length > 0.) ? aspect / length : 0.;

  if ((length) && (width))
    fprintf(fout, "\t\tEllipse            = %g\n", 
            area/(0.7853981635*length*width*pixsize*pixsize));
     
  fprintf(fout, "\t\tLength X Width     = %g x %g\n", length, width);

  fprintf(fout, "\t\tAspect Ratio       = %g\n", aspect);

  return 0;
}

static void imodinfo_special(Imod *imod, char *fname)
{
  FILE *fout;

  fout = fopen(fname, "w");
  imod->obj[1].contsize = 11;

  imod->cindex.object = 137;
  imod->cindex.contour = 0;
  imod->cindex.point = 0;
  imodDeleteObject(imod, 137);
  imodDeleteObject(imod, 136);

  imodWrite(imod, fout);
  fclose(fout);
  return;
}



/****************************************************************************/

/* Prints length of each contour in model */
static void imodinfo_length(Imod *imod)
{
  int ob,co;
  Iobj *obj;
  Icont *cont;
  double dist;

  fprintf(fout, "# Obj Cont Pnts Length (in %s)\n", imodUnits(imod));
  fprintf(fout, "#------------------------\n");

  for(ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      dist = info_contour_length(cont, obj->flags, 
                                 (double)imod->pixsize,
                                 (double)imod->zscale);
      fprintf(fout, "%3d %3d  %3d  %g\n", ob+1, co+1, cont->psize, dist);
    }
  }
  return;
}


static double pointdist(Ipoint *p1, Ipoint *p2)
{
  double dist;

  dist = ((p1->x - p2->x)*(p1->x - p2->x)) + 
    ((p1->y - p2->y)*(p1->y - p2->y)) + 
    ((p1->z - p2->z)*(p1->z - p2->z));
  return(sqrt(dist));
}

/* COmputes contour length, taking account of open/closed character */
static double info_contour_length(Icont *cont, int objflags,
                                  double pixsize, double zscale)
{
  int pt;
  double dist = 0.0;
  Ipoint p1, p2;

  if (!cont)
    return(0.0);

  if (cont->psize <= 0)
    return(0.0);

  if ((iobjClose(objflags)) || (iobjOpen(objflags))){
    for(pt = 0; pt < cont->psize - 1; pt++){
      p1.x = cont->pts[pt].x;
      p1.y = cont->pts[pt].y;
      p1.z = cont->pts[pt].z * zscale;
      p2.x = cont->pts[pt+1].x;
      p2.y = cont->pts[pt+1].y;
      p2.z = cont->pts[pt+1].z * zscale;
      dist += pointdist(&p1, &p2);
    }

    /* DNM 9/26/04: add test for open contour */
    if (iobjClose(objflags) && !(cont->flags & ICONT_OPEN)) {
      p1.x = cont->pts[pt].x;
      p1.y = cont->pts[pt].y;
      p1.z = cont->pts[pt].z * zscale;
      p2.x = cont->pts[0].x;
      p2.y = cont->pts[0].y;
      p2.z = cont->pts[0].z * zscale;
      dist += pointdist(&p1, &p2);
    }
  }
  dist *= pixsize;
  return(dist);
}

/* Computes contribution of contour length to surface area by adjusting for
   Z thickness */
static double info_contour_surface_area(Icont *cont, int objflags,
                                        double pixsize, double zscale)
{
  double ld;
  if (!cont)
    return(0.0);

  if (cont->psize <= 0)
    return(0.0);

  ld = info_contour_length(cont, objflags, pixsize, zscale);
  return(ld * zscale * pixsize);
}

/* Computes contour area; multiplying by pixel size for closed contours only */
static double info_contour_area(Icont *cont, int objflags,
                                double pixsize, double zscale)
{
  double area;

  if (!cont)
    return(0.0);

  if (cont->psize <= 0)
    return(0.0);

  area = imodContourArea(cont);

  if (iobjClose(objflags)){
    area *= pixsize * pixsize;
  }
  return(area);
}

/* COmputes contribution of contour to cylinder volume */
static double info_contour_vol(Icont *cont, int objflags,
                               double pixsize, double zscale)
{
  double vol = 0.0;
  if (!cont)
    return(0.0);

  if (cont->psize <= 0)
    return(0.0);
  vol = imodContourArea(cont);
  vol *= pixsize * pixsize * pixsize * zscale;
  return(vol);
}

/* Computes the factor by while a contour's area contributes to volume.
   Finds the one or two mesh polygons that contain the contour and
   add half of the spacing between contours in each direction.
   Z limits are applied from min and max inputs to truncate the spacing.
   Returns zero if no meshes are found.*/
static float contourVolumeFactor(Iobj *obj, Icont *cont, Ipoint min, 
                                 Ipoint max)
{
  Imesh *mesh;
  int me, i, pt, found;
  Ipoint *p1, *p2, *p3;
  float volFac = 0.;
  float xx, yy, zz, zmin, zmax, diff;
  int resol;
  int ind1, ind2, ind3, num1, num2, num3, numTri, indStart;
  int listInc, vertBase, normAdd;

  imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);

  /* Loop on points until one is found in a mesh */
  found = 0;
  for (pt = 0; pt < cont->psize && !found; pt++) {
    xx = cont->pts[pt].x;
    yy = cont->pts[pt].y;
    zz = cont->pts[pt].z;

    for (me = 0; me < obj->meshsize; me++) {
      mesh = &obj->mesh[me];
      if (!mesh || !mesh->lsize || 
          imeshResol(mesh->flag) != resol)
        continue;
      for (i = 0; i < mesh->lsize; i++){
        if (imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase, 
                                    &normAdd)) {
          i++;
          indStart = i;
          while (mesh->list[i] != IMOD_MESH_ENDPOLY){
          
            p1 = &(mesh->vert[mesh->list[i + vertBase]]);
            i+=listInc;
            p2 = &(mesh->vert[mesh->list[i + vertBase]]);
            i+=listInc;
            p3 = &(mesh->vert[mesh->list[i + vertBase]]);
            i+=listInc;

            /* Find an exactly matching point */
            if ((xx == p1->x && yy == p1->y && zz == p1->z) ||
                (xx == p2->x && yy == p2->y && zz == p2->z) ||
                (xx == p3->x && yy == p3->y && zz == p3->z)) {

              /* get the maximum difference between the points */
              if (p1->z < zz || p2->z < zz || p3->z < zz) {
                zmax = zz;
                zmin = p1->z;
                if (zmin > p2->z)
                  zmin = p2->z;
                if (zmin > p3->z)
                  zmin = p3->z;
                  
              } else {
                zmin = zz;
                zmax = p1->z;
                if (zmax < p2->z)
                  zmax = p2->z;
                if (zmax < p3->z)
                  zmax = p3->z;
              }
              if (zmax >= max.z && min.z >= zmin)
                diff = max.z - min.z;
              else if (zmin < min.z)
                diff = zmax - min.z;
              else if (zmax > max.z)
                diff = max.z - zmin;
              else
                diff = zmax - zmin;
              if (Debug)
                printf("%f %f %f %f %f\n", zz, p1->z, p2->z, p3->z, diff);

              /* Go through the whole polygon checking for cap */
              i = indStart;
              ind1 = mesh->list[i + vertBase];
              ind2 = mesh->list[i + listInc + vertBase];
              ind3 = mesh->list[i + 2 * listInc + vertBase];
              num1 = 0;
              num2 = 0;
              num3 = 0;
              numTri = 0;
              while (mesh->list[i] != IMOD_MESH_ENDPOLY){
                if (mesh->list[i + vertBase] == ind1)
                  num1++;
                i += listInc;
                if (mesh->list[i + vertBase] == ind2)
                  num2++;
                i += listInc;
                if (mesh->list[i + vertBase] == ind3)
                  num3++;
                numTri++;
                i += listInc;
              }
              /* printf("Triangles: %d  shared indices: %d %d %d\n", numTri, 
                     num1, num2, num3); */

              /* If diff is still positive after clipping by Z, add half for
               ordinary case or one-third for cap case */
              if (diff > 0.) {
                if (num1 == numTri || num2 == numTri || num3 == numTri)
                  volFac += diff / 3.f;
                else
                  volFac += diff / 2.f;
              }

              /* If found two meshes, done */
              if (found) {
                if (Debug)
                  printf("Found 2 polygons at z = %f, factor %f\n",zz, volFac);
                return volFac;
              }
              found++;
              break;
            }
          }
        }
      }
    }
  }
  if (Debug)
    printf("Found %d polygons at z = %f, factor %f\n",found, zz, volFac);
  return volFac;
}

/* Now unused with new consistent computations in full report */
static Iobj *imodinfo_ObjectClip(Iobj *obj, Iplane *plane, int planes)
{
  Iobj *robj = imodObjectNew();
  Icont *cc, *cont;
  Ipoint cpnt;
  int pt, lpt, ppt, tpt;
  int co;
  int laststate;
  float clp;

  if ((planes <= 0) || (!plane) || (!obj))
    return(NULL);

  *robj = *obj;

  robj->cont = NULL;
  robj->contsize = 0;

  for(co = 0; co < obj->contsize; co++){

    laststate = 0;
    cont = &(obj->cont[co]);
    cc = imodContourNew();
    lpt = cont->psize;
    if (iobjClose(obj->flags))
      lpt++;

    for(pt = 0; pt < lpt; pt++){
      ppt = tpt; /* previous point index gets old */
      tpt = pt;  /* current this point */
      if (tpt == cont->psize)
        tpt = 0;
               
      cpnt = cont->pts[tpt];
      /*      clp =  ( ((plane->a * cpnt.x) + (plane->b * cpnt.y) + 
                (plane->c * cpnt.z ) + plane->d));


                if (clp >= 0.0f){ */
      if (imodPlanesClip(plane, planes, &cpnt)) {
        imodPointAppend(cc, &cpnt);
      }

#ifdef FANCY_CLIP
      if (laststate == 2){
        imodPointPlaneEdge
          (&ipnt, plane, planes, 
           &(cont->pts[tpt]), &(cont->pts[ppt]));
        imodPointAppend(cc, &ipnt);
        fprintf(fout, "|");
      }else{
        imodPointAppend(cc, &(cont->pts[tpt]));
        fprintf(fout, "a");
      }
      laststate = 1; /* Last Point appended */
      continue;
      /* } */

      /* if last point was added */
      if (laststate == 1){
        fprintf(fout, "|");
        imodPointPlaneEdge
          (&ipnt, plane, planes, 
           &(cont->pts[tpt]), &(cont->pts[ppt]));
        imodPointAppend(cc, &ipnt);
      }
      laststate = 2; /* Last point was clipped. */
      fprintf(fout, "X") ;
      /* } */

#endif
    }
    /*        fprintf(fout, "co %d: %d %d\n", co, cont->psize, cc->psize); */
    if (cc->psize > 0) {
      imodObjectAddContour(robj, cc);
      free(cc);
    } else
      imodContourDelete(cc);
  }
  robj->flags |= IMOD_OBJFLAG_OPEN;
  return(robj);
}

/* 
 * Returns area of mesh in square pixels, as constrained by the subset min and 
 * max limits and possible clipping planes.  Works only for POLYNORM meshes. 
 * Inputs:  mesh -  pointer to mesh
 *          scale - pointer to point with scale factors (for Z scaling)
 *          min, max  - points with limits of subarea
 *          doclip    - flag to use clipping planes to find region to
 *                        include (1) or exclude (-1); it is 0 if no planes
 *          plane     - pointer to array of Iplanes
 *          nPlanes   - number of clipping planes
 */
static float imeshSurfaceSubarea(Imesh *mesh, Ipoint *scale, Ipoint min,
                                 Ipoint max, int doclip, Iplane *plane,
                                 int nPlanes)
{
  int i;
  int listInc, vertBase, normAdd;
  double tsa = 0.0f;
  float zs = 1.0f;

  if ((!mesh) || (!mesh->lsize))
    return(0.0f);
  if (scale)
    zs = scale->z;

  for(i = 0; i < mesh->lsize; i++){
    switch(mesh->list[i]){
    case IMOD_MESH_BGNPOLYNORM:
    case IMOD_MESH_BGNPOLYNORM2:
      imodMeshPolyNormFactors(mesh->list[i], &listInc, &vertBase, &normAdd);
      i++;
      while(mesh->list[i] != IMOD_MESH_ENDPOLY){
        tsa += clippedTriangleArea(mesh, i, zs, min, max, doclip, plane, 
                                   nPlanes, listInc, vertBase);
        i += 3 * listInc;
      }
      break;
               
    case IMOD_MESH_END:
      return((float)tsa);
               
    }
          
  }
  return((float)tsa);
}


/* Return the area of the next mesh triangle, possibly clipped by subarea
   limits and clipping planes.  Arguments are as above, except zs is Z scale */
static double clippedTriangleArea(Imesh *mesh, int i, float zs, Ipoint min,
                                  Ipoint max, int doclip, Iplane *plane,
                                  int nPlanes, int listInc, int vertBase)
{
  int inside1, inside2, inside3;
     
  Ipoint *p1, *p2, *p3;
  Ipoint n, n1, n2;
  float clipfrac;

  p1 = &(mesh->vert[mesh->list[i + vertBase]]);
  p2 = &(mesh->vert[mesh->list[i + listInc + vertBase]]);
  p3 = &(mesh->vert[mesh->list[i + 2 * listInc + vertBase]]);
                    
  /* Count the number of vertices that are inside the defined volume
     first evaluate their relation to the clipping plane */
  inside1 = 1;
  inside2 = 1;
  inside3 = 1;
    
  if (doclip) {
    inside1 = imodPlanesClip(plane, nPlanes, p1);
    inside2 = imodPlanesClip(plane, nPlanes, p2);
    inside3 = imodPlanesClip(plane, nPlanes, p3);
    if (doclip < 0) {
      inside1 = 1 - inside1;
      inside2 = 1 - inside2;
      inside3 = 1 - inside3;
    }
  }
    
  /* Now see whether the vertices fit in the bounding box */
  if (p1->x < min.x || p1->x >= max.x ||
      p1->y < min.y || p1->y >= max.y ||
      p1->z < min.z || p1->z >= max.z)
    inside1 = 0;
  if (p2->x < min.x || p2->x >= max.x ||
      p2->y < min.y || p2->y >= max.y ||
      p2->z < min.z || p2->z >= max.z)
    inside2 = 0;
  if (p3->x < min.x || p3->x >= max.x ||
      p3->y < min.y || p3->y >= max.y ||
      p3->z < min.z || p3->z >= max.z)
    inside3 = 0;
    
  /*  just take a fraction equal to the fraction of vertices inside */
  clipfrac = (inside1 + inside2 + inside3) / 3.;
  if (clipfrac) {
    n1.x = p1->x - p2->x;
    n1.y = p1->y - p2->y;
    n1.z = (p1->z - p2->z) * zs;
    n2.x = p3->x - p2->x;
    n2.y = p3->y - p2->y;
    n2.z = (p3->z - p2->z) * zs;
    imodPointCross(&n1, &n2, &n);
      
    return clipfrac * sqrt((double)(n.x*n.x + n.y*n.y + n.z*n.z)) * 0.5;
  }
  return 0.;
}

/* Computes the cylinder and mesh volume with adjustments for inside contours,
   subset areas, or clipping planes.  Arguments as above; mesh volume returned
   in meshVol */
static float scanned_volume(Iobj *obj, int subarea, Ipoint ptmin, Ipoint ptmax,
                            int doclip, Iplane *plane, int nPlanes, 
                            double *meshVol)
{
  int co;
  Icont *cont;
  Icont **scancont;
  int zmin,zmax;
  int indz;
  int nummax, inbox;

  int *contz;
  Ipoint *pmin, *pmax;
  int *numatz;
  int **contatz;
  int *zlist;
  int  kis, zlsize;
  int numnests;
  int eco;
  Nesting *nests;
  int *nestind;
  int numwarn = -1;
  int level;
  float *areas;
  float *volFacs;
  double tvol = 0.;

  *meshVol = 0.;
  if (!obj->contsize)
    return(0.);
     
  if (imodContourMakeZTables(obj, 1, 0, &contz, &zlist, &numatz, &contatz, 
                             &zmin, &zmax, &zlsize, &nummax))
    return -1;

  /* Allocate space for lists of min's max's, and scan contours */
  pmin = (Ipoint *)malloc(nummax * sizeof(Ipoint));
  pmax = (Ipoint *)malloc(nummax * sizeof(Ipoint));
  scancont = (Icont **)malloc(nummax * sizeof (Icont *)); 
  areas = (float *)malloc(nummax * sizeof(float));
  volFacs = (float *)malloc(nummax * sizeof(float));

  /* Get array for index to inside/outside information */
  nestind = (int *)malloc(nummax * sizeof(int));
  if (!pmin || !pmax || !scancont || !areas || !nestind || !volFacs)
    return -1.;

  for (indz = 0; indz < zmax + 1 - zmin; indz++) {
    if (indz + zmin < ptmin.z || indz + zmin > ptmax.z)
      continue;
    /* printf ("Measuring at z = %d\n", indz + zmin); */
    inbox = 0;
    numnests = 0;
    for (kis = 0; kis < numatz[indz]; kis++) {
      co = contatz[indz][kis];
      cont = &(obj->cont[co]);

      if (contourSubareaByScan(cont, ptmin, ptmax, doclip, plane, nPlanes, 1,
                               &scancont[inbox], &pmin[inbox], 
                               &pmax[inbox], &areas[inbox])) {
        volFacs[inbox] = contourVolumeFactor(obj, cont, ptmin, ptmax);
        nestind[inbox++] = -1;
        if (Debug)
          printf ("contour %d  area %f  volume factor %f\n", co + 1,
                  areas[inbox-1], volFacs[inbox-1]);
      }
    }

    /* Look for overlapping contours as in imodmesh */
    for (co = 0; co < inbox - 1; co++) {
      for (eco = co + 1; eco < inbox; eco++) {
        if (imodContourCheckNesting(co, eco, scancont, pmin, pmax, &nests,
                                    nestind, &numnests, &numwarn))
          return -1;
      }
    }

    /* Analyze inside and outside contours to determine level */
    imodContourNestLevels(nests, nestind, numnests);
          
    /* now add up areas of non-nested and odd levels, minus even levels */
    for (co = 0; co < inbox; co++) {
      level = 1;
      if (nestind[co] >= 0)
        level = nests[nestind[co]].level;
      if (level % 2) {
        /* printf ("adding %f at level %d\n", areas[co], level); */
        tvol += areas[co];
        *meshVol += volFacs[co] * areas[co];
      } else {
        /* printf ("subtracting %f at level %d\n", areas[co], level); */
        tvol -= areas[co];
        *meshVol -= volFacs[co] * areas[co];
      }
    }

    /* clean up inside the nests */
    imodContourFreeNests(nests, numnests);

    /* clean up scan conversions */
    for (co = 0; co < inbox; co++)
      imodContourDelete(scancont[co]);
  }

  /* clean up everything else */
  free(nestind);

  imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);
  free(scancont);
  free(pmin);
  free(pmax);
  free(areas);
  free(volFacs);
  return ((float)tvol);
}
  
/*
 * Computes the area of a contour clipped by subarea and clipping planes, 
 * using a scan contour to implement the clipping.  Returns 1 if the contour
 * has non-zero area.  Arguments doclip, plane, nPlanes are as above.  Other
 * inputs:  ptmin, ptmax - points with limits of subarea
 *          makeScan     - Make a scan contour even if one is not needed and
 *                          do not delete a created scan contour
 * outputs: scancont     - pointer to address of created scan contour
 *          pmin, pmax   - pointers to points for bounding box of contour
 *          area         - pointer to computed area
 */
static int contourSubareaByScan(Icont *cont, Ipoint ptmin, Ipoint ptmax,
                                int doclip, Iplane *plane, int nPlanes, 
                                int makeScan, Icont **scancont, Ipoint *pmin,
                                Ipoint *pmax, float *area)
{
  Ipoint tmpmax, tmpmin, corner;
  int clipsum;
  float frac1, frac2;
  *scancont = NULL;
  *area = 0.;
  
  if (!cont->psize)
    return 0;

  /* Get limits and test in X and Y */
  imodContourGetBBox(cont, &tmpmin, &tmpmax);
  if (tmpmin.x > ptmax.x || tmpmax.x < ptmin.x ||
      tmpmin.y > ptmax.y || tmpmax.y < ptmin.y)
    return 0;

  if (doclip) {
    /* Evaluate the corners of the bounding box: if all on 
       wrong side of clip plane, skip */
    corner = tmpmin;
    clipsum = imodPlanesClip(plane, nPlanes, &corner);
    corner.y = tmpmax.y;
    clipsum += imodPlanesClip(plane, nPlanes, &corner);
    corner.x = tmpmax.x;
    clipsum += imodPlanesClip(plane, nPlanes, &corner);
    corner.y = tmpmin.y;
    clipsum += imodPlanesClip(plane, nPlanes, &corner);
    if ((clipsum == 0 && doclip > 0) || 
        (clipsum == 4 && doclip < 0))
      return 0;
  }

  /* Start with true area of untrimmed contour */
  *area = imodContourArea(cont);

  /* If contour is not wholly inside the box, or is not all on 
     the good side of the clip plane, need to trim scan
     contour down */
  if (tmpmin.x < ptmin.x || tmpmax.x > ptmax.x ||
      tmpmin.y < ptmin.y || tmpmax.y > ptmax.y || 
      (doclip && (clipsum > 0 && clipsum < 4))) {
    (*scancont) = imodel_contour_scan(cont);

    /* need to copy the z coordinate over! */
    if ((*scancont)->psize)
      (*scancont)->pts->z = cont->pts->z;

    frac1 = scan_contour_area((*scancont));
    trim_scan_contour((*scancont), ptmin, ptmax, doclip,
                      plane, nPlanes);
    if(!(*scancont)->psize) {
      /* If contour now empty, delete and skip it */
      imodContourDelete((*scancont));
      return 0;
    }
    imodContourGetBBox((*scancont), &tmpmin, &tmpmax);

    /* Adjust true area down by ratio of trimmed to original
       scan-contour area */
    frac2 = scan_contour_area((*scancont));
    if (frac1 && (frac2 < frac1)) 
      *area *= frac2 / frac1;
  }

  /* Make scan contour if necessary and requested; delete if not requested */
  if (makeScan && !(*scancont)) {
    (*scancont) = imodel_contour_scan(cont);
    if ((*scancont)->psize)
      (*scancont)->pts->z = cont->pts->z;
  } else if (!makeScan && *scancont)
    imodContourDelete((*scancont));

  *pmin = tmpmin;
  *pmax = tmpmax;
  return 1;
}

/* Compute area of scan contour.  Slight mod of library function to assume 
   it's already a scan cont, and to add up and return doubles not ints */
static double scan_contour_area(Icont *cont)
{
  double pix = 0.;
  int i, j;
  int xmin, xmax;
  int bgnpt,endpt;
  int scanline;

  if (!cont)
    return(0.);

  if (cont->psize < 2)
    return(0.);

  for(i = 0, scanline = 0; i < cont->psize - 1; i++, scanline++){
    bgnpt = i;
    while (cont->pts[i].y == cont->pts[i+1].y){
      ++i;
      if (i == cont->psize){
        i--;
        break;
      }
    }
    endpt = i;
          
    /* check for odd amount of scans, shouldn't happen! */
    if (! ( (endpt-bgnpt)% 2)){
      continue;
      /*             printf(" (Error scan line %d,%d)\n", scanline, y);  */
    }
    else{
      for(j = bgnpt; j < endpt; j++){
        xmin = cont->pts[j].x;
        xmax = cont->pts[j+1].x;
        if (xmin >= cont->surf)
          pix += xmax - xmin;
        j++;
      }
    }
  }
  return(pix);
}

/* Trims a scan contour based on subset area and clipping planes. 
   Arguments as above */
static void trim_scan_contour(Icont *cont, Ipoint min, Ipoint max, int doclip,
                              Iplane *plane, int nPlanes)
{
  int i, ipl;
  Ipoint *pts;
  Ipoint tmin, tmax;
  float yline, ylast, crit, pmag, contz;
  Iplane *pl;

  if (!cont)
    return;
  if (cont->psize < 2)
    return;
  pts = cont->pts;
  contz = pts->z;

  /* DNM 9/26/04: just loop on multiple planes.  Fix probable bug; take max
   of clipping crit and overall min, min of clipping crit and overall max */
  for (ipl = 0; ipl < (doclip ? nPlanes : 1); ipl++) {
    pl = &plane[ipl];
    tmin = min;
    tmax = max;
    ylast = 1.e20;
    if (doclip) {
      pmag = sqrt((double)(pl->a * pl->a + pl->b * pl->b + pl->c * pl->c));

      /* If a is small, compute y limit; but if b is small also, skip
         checking for these limits.  In either case set doclip to 0 */
      if (pl->a < 1.e-10 * pmag && pl->a > -1.e-10 * pmag) {
        if (pl->b > 1.e-10 * pmag || pl->b < -1.e-10 * pmag) {
          crit = -(pl->c * contz + pl->d) / pl->b;
          if ((doclip > 0 && pl->b > 0.) ||
              (doclip < 0 && pl->b < 0.)) {
            if (tmin.y < crit)
              tmin.y = crit;
            /*printf("%f  tmin.y  %f\n", contz, crit);*/
          } else {
            if (tmax.y > crit)
              tmax.y = crit;
            /*printf("%f  tmax.y  %f\n", contz, crit);*/
          }
        }
        doclip = 0;
      }
    }

    for (i = 0; i < cont->psize; i++) {
      yline = pts[i].y;
      if (yline != ylast && doclip) {
        /* If its a new line and clip needs to be checked, compute the
           limit in X for this Y and assign it to min or max */
        tmin = min;
        tmax = max;
        ylast = yline;
        crit = -(pl->b * yline + pl->c * contz + pl->d) / pl->a;
        if ((doclip > 0 && pl->a > 0.) ||
            (doclip < 0 && pl->a < 0.)) {
          if (tmin.x < crit)
            tmin.x = crit;
          /*printf ("%f %d %f tmin.x %f\n", contz, i, yline, crit);*/
        } else {
          if (tmax.x > crit)
            tmax.x = crit;
          /*printf ("%f %d %f tmax.x %f\n", contz, i, yline, crit); */
        }

      }

      if (yline == pts[i + 1].y) {
        if (yline < tmin.y || yline > tmax.y || pts[i].x > tmax.x ||
            pts[i + 1].x < tmin.x) {
          /* If line is out of bounds in y or x, delete 2 points */
          imodPointDelete(cont, i);
          imodPointDelete(cont, i);
          i--;
        } else {
          /* otherwise, check and truncate the left and right ends
             of the scan line */
          if (pts[i].x < tmin.x)
            pts[i].x = tmin.x;
          if (pts[i + 1].x > tmax.x)
            pts[i + 1].x = tmax.x;
          i++;
        }
      }
    }
  }
}

/*

$Log$
Revision 3.21  2009/02/24 18:03:00  mast
Made object centroid be done differently for open object

Revision 3.20  2008/11/14 06:11:09  mast
Added volume in mesh report, renamed mesh volume to contour volume, made
it drop the cylinder reports if the mesh exists.

Revision 3.19  2008/06/19 19:18:37  mast
Fixed -vv option

Revision 3.18  2008/04/04 21:21:29  mast
Free contour after adding to object

Revision 3.17  2006/09/13 02:37:25  mast
Switch to allocating and freeing model

Revision 3.16  2006/06/26 14:48:49  mast
Added b3dutil include for parselist

Revision 3.15  2005/09/11 19:22:11  mast
Changes for new style of mesh

Revision 3.14  2005/04/04 22:41:32  mast
Fixed problem with argument order to imdContourGetBBox

Revision 3.13  2005/03/20 19:56:05  mast
Eliminating duplicate functions

Revision 3.12  2005/01/29 20:28:57  mast
Pulled out routines for making Z tables and doing nested contour
analysis; fixed bug in clipping plane volume measurement

Revision 3.11  2004/12/06 22:39:54  mast
Fixed use of -f with ascii output of model

Revision 3.10  2004/11/05 19:05:29  mast
Include local files with quotes, not brackets

Revision 3.9  2004/09/28 22:20:31  mast
Overhauled to add mesh volume computation that takes account of spacing
between connected contours; to handle multiple clipping planes; and
to give consistent results with inside contours, clipping and subset
volumes for main output, full object, surface, and centroid outputs.

Revision 3.8  2004/09/21 20:34:25  mast
First changes to deal with new clipping plane structure

Revision 3.7  2004/09/10 21:34:01  mast
Eliminated long variables

Revision 3.5.4.1  2004/07/07 19:26:21  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.5  2003/10/26 14:46:41  mast
fixed problem in eliminating getopt

Revision 3.4  2003/10/24 03:05:23  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.3  2003/10/02 05:38:25  mast
Fixed point info output so it does subareas correctly for scattered points or 
points with sizes; and made hush suppress individual point data

Revision 3.2  2003/02/07 00:18:06  mast
Divided all mesh triangles that cross boundaries with -x, -y, -z options
so that no area is lost

Revision 3.1  2002/12/23 21:40:50  mast
fixed exit status

*/

