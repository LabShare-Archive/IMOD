/*  IMOD VERSION 2.41
 *
 *  imodinfo.c --  Prints info about imod files to std out.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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
Revision 3.7  2004/09/10 21:34:01  mast
Eliminated long variables

Revision 3.5.4.1  2004/07/07 19:26:21  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.5  2003/10/26 14:46:41  mast
fixed problem in eliminating getopt

Revision 3.4  2003/10/24 03:05:23  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.3  2003/10/02 05:38:25  mast
Fixed point info output so it does subareas correctly for scattered points or points
with sizes; and made hush suppress individual point data

Revision 3.2  2003/02/07 00:18:06  mast
Divided all mesh triangles that cross boundaries with -x, -y, -z options
so that no area is lost

Revision 3.1  2002/12/23 21:40:50  mast
fixed exit status

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <imodel.h>
#include <math.h>

#define MINFO_SPECIAL  99
#define MINFO_STANDARD 1
#define MINFO_CHART    2
#define MINFO_DIST     3
#define MINFO_INTERACTIVE 4
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
static void imodinfo_surface(Imod *imod);
static void imodinfo_points(Imod *imod, int subarea, Ipoint ptmin,
                            Ipoint ptmax, int useclip, int verbose);
static void imodinfo_ratios(Imod *imod);
static int contour_stats(Icont *cont, int flags, double pixsize, 
                         double zscale);
static Iobj *imodinfo_ObjectClip(Iobj *obj, Iplane *plane, int planes);

static void print_units(int units);
static void imodinfo_special(Imod *imod, char *fname);
static void imodinfo_print_model(Imod *model, int verbose, int scaninside, 
                                 int subarea, Ipoint ptmin, Ipoint ptmax,
                                 int useclip);
static void imodinfo_object(struct Mod_Model *imod);
static void imodinfo_objndist(struct Mod_Model *imod, int bins);
static void imodinfo_length(Imod *imod);
static void imodinfo_interactive(Imod *imod);
static void imodinfo_full_object_report(Imod *imod, int ob);
float imeshSurfaceSubarea(Imesh *mesh, Ipoint *scale, Ipoint min, Ipoint max,
                          int doclip, Iplane *plane);
static int getContZValue(Icont *cont);
static float scanned_volume(Iobj *obj, int subarea, Ipoint ptmin, 
                            Ipoint ptmax, int doclip, Iplane *plane);
static double scan_contour_area(struct Mod_Contour *cont);
static void trim_scan_contour(Icont *cont, Ipoint min, Ipoint max, int doclip,
                              Iplane *plane);


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
  printf("\t-o #\tPrint full report on given object #.\n");
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

int main( int argc, char *argv[])
{
  int                i,j,c;
  int                verbose = 0;
  int                scaninside = FALSE;
  int                subarea = FALSE;
  int                useclip = 0;
  int                ob, co, pt;
  int                mode = MINFO_STANDARD;
  int                hush = FALSE;
  FILE               *fin;
  struct Mod_Model   model;
  struct Mod_Object  *obj;
  struct Mod_Contour *cont;
  int bins = 0;
  int sa,inlo, inhi;
  double dist;
  double tsa, tvol;
  char *afname = NULL;
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
        break;

      case 'o':
        ob = atoi(argv[++i]);
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
               
      case 't':
        sscanf(argv[++i], "%d", &useclip);
        scaninside = TRUE;
        break;
               
      case 'x':
        sscanf(argv[++i], "%d%*c%d", &inlo, &inhi);
        ptmin.x = inlo;
        ptmax.x = inhi;
        scaninside = TRUE;
        subarea = TRUE;
        break;

      case 'y':
        sscanf(argv[++i], "%d%*c%d", &inlo, &inhi);
        ptmin.y = inlo;
        ptmax.y = inhi;
        scaninside = TRUE;
        subarea = TRUE;
        break;

      case 'z':
        sscanf(argv[++i], "%d%*c%d", &inlo, &inhi);
        ptmin.z = inlo;
        ptmax.z = inhi;
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
    model.file = fin;
    errcode = imodReadFile(&model);
    if (errcode){
      fprintf(stderr, "%s: Error (%d) reading imod model. (%s)\n", 
              progname, errcode, argv[i]);
      /*             exit(3); */

    }

    fprintf(fout, "# MODEL %s\n", argv[i]);
    fprintf(fout, "# NAME  %s\n", model.name);
    fprintf(fout, "# PIX SCALE:  x = %g\n", model.xscale);
    fprintf(fout, "#             y = %g\n", model.yscale);
    fprintf(fout, "#             z = %g\n", model.zscale);
    fprintf(fout, "# PIX SIZE      = %g\n", model.pixsize);
    fprintf(fout, "# UNITS: ");
          
    print_units(model.units);

    if (model.refImage){
      fprintf(fout, "\n# Model to Image index coords:\n");
      fprintf(fout, "#      SCALE  = ( %g, %g, %g)\n",
              model.refImage->cscale.x, 
              model.refImage->cscale.y,
              model.refImage->cscale.z);
      fprintf(fout, "#      OFFSET = ( %g, %g, %g)\n",
              model.refImage->ctrans.x, 
              model.refImage->ctrans.y,
              model.refImage->ctrans.z);
      fprintf(fout, "#      ANGLES = ( %g, %g, %g)\n",
              model.refImage->crot.x, 
              model.refImage->crot.y,
              model.refImage->crot.z);

    }

          

    fprintf(fout, "\n\n");
          
    switch(mode){

    case MINFO_ASCII:
      if (afname){
        model.file = fopen(afname, "w");
      }else{
        model.file = stdout;
      }
      imodWriteAscii(&model);
      break;

    case MINFO_SPECIAL:
      if (fin){
        fclose(fin);
        fin = NULL;
      }
      imodinfo_special(&model, argv[i]);
      break;

    case MINFO_STANDARD:
      imodinfo_print_model(&model, verbose, scaninside, subarea,
                           ptmin, ptmax, useclip);
      break;

    case MINFO_SURFACE:
      imodinfo_surface(&model);
      break;

    case MINFO_POINTS:
      imodinfo_points(&model, subarea, ptmin, ptmax, useclip, verbose);
      break;

    case MINFO_RATIOS:
      imodinfo_ratios(&model);
      break;

    case MINFO_CHART:
      imodinfo_object(&model);
      break;
    case MINFO_DIST:
      imodinfo_objndist(&model, bins);
      break;
    case MINFO_LENGTH:
      imodinfo_length(&model);
      break;
    case MINFO_INTERACTIVE:
      imodinfo_interactive(&model);
      break;
    case MINFO_OBJECT:
      imodinfo_full_object_report(&model, ob);
      break;
    default:
      break;
    }
    if (fin)
      fclose(fin);
    fprintf(fout, "\n\n\n\n");
  }
  exit(0);
}

static void imodinfo_print_model(Imod *model, int verbose, int scaninside,
                                 int subarea, Ipoint min, Ipoint max,
                                 int useclip)
{
  double tsa, tvol;
  double dist;
  double sa;
  int ob, co, pt, i, npt, coz;
  struct Mod_Object  *obj;
  struct Mod_Contour *cont;
  Ipoint mscale, p1;
  Iplane plane;
  int doclip, goodside;

  if (!model->objsize){
    fprintf(fout, "Model has no objects!!!\n");
    return;
  }

  for (ob = 0; ob < model->objsize; ob++){
    obj = &(model->obj[ob]);
    tsa = 0; tvol = 0;
    doclip = 0;
    if (useclip && obj->clips.count && (obj->clips.flags & 1)) {
      /* Get clipping plane in 4 parameter form; c is already
         multiplied by zscale so no need to 
         multiply z coordinates by zscale for tests*/
      imodPlaneSetPN(&plane, &(obj->clips.point[0]), &(obj->clips.normal[0]));
      doclip = useclip;
    }

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
        scanned_volume(obj, subarea, min, max, doclip, &plane);
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
              goodside = imodPlaneClip(&plane, &p1);
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
          coz = getContZValue(cont);
          if (coz < min.z || coz > max.z) {
            if (verbose >= 0)
              fprintf(fout, "\n");
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
    if (tvol > 0.0){
      tvol *= model->zscale * model->pixsize;
      fprintf(fout, "\n\tTotal volume = %g\n", tvol);
    }
    if (tsa > 0.0){
      tsa *= (model->zscale * model->pixsize);
      fprintf(fout, "\tTotal contour cylinder surface area = %g\n", tsa);
    }
    if (obj->meshsize){

      int mesh, resol;
      tsa = 0.0;
      mscale.x = model->xscale;
      mscale.y = model->yscale;
      mscale.z = model->zscale;
      imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);
      for(mesh = 0; mesh < obj->meshsize; mesh++)
        if (imeshResol(obj->mesh[mesh].flag) == resol) {
          if (subarea || doclip)
            tsa += imeshSurfaceSubarea 
              (&obj->mesh[mesh], &mscale, min, max,
               doclip, &plane);
          else
            tsa += imeshSurfaceArea (&obj->mesh[mesh],
                                     &mscale);
        }
      tsa *= model->pixsize * model->pixsize; 
      fprintf(fout, "\tTotal mesh surface area = %g\n", tsa);
    }
          
    fprintf(fout, "\n");
  }
  return;
}


/* prints volume and surface area, for each surface. */

static void imodinfo_surface(Imod *imod)
{
  Iobj *obj;
  Icont *cont;
  int ob, co, pt, i;
  float tvol, tsa;
  double *sa = NULL;
  double *vol = NULL;
  unsigned int *nofc = NULL;
  unsigned int maxsurf;


  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
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
    nofc = (unsigned int *)malloc((obj->surfsize + 1) 
                                   * sizeof(unsigned int));
    if (!nofc){
      free(vol);
      free(sa);
      return;
    }

    for(i = 0; i <= obj->surfsize; i++){
      sa[i] = vol[i] = 0.0;
      nofc[i] = 0;
    }
      
    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      if (!cont->psize)
        continue;
      i = cont->surf;
      tvol = info_contour_vol(cont, obj->flags,
                              (double)imod->pixsize,
                              (double)imod->zscale);
      tsa = info_contour_surface_area
        (cont, obj->flags,
         (double)imod->pixsize,
         (double)imod->zscale);
      nofc[i]++;
      vol[i] += tvol;
      sa[i] += tsa;
    }
          
    fprintf(fout, "\n#Object %d data, %s\n", ob + 1, obj->name);
    if (obj->meshsize){

      int me, found, psurf, resol;
      double *msa;
      double psa;
      Imesh *mesh;
      Ipoint *p1, *p2, *p3;
      Ipoint n, n1, n2;
      float xx, yy, zz;
      float zs = imod->zscale;

      msa = (double *)malloc((obj->surfsize + 1) * sizeof(double));
      if (!msa) {
        free(nofc);
        free(vol);
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
        for(i = 0; i < mesh->lsize; i++){
          if (mesh->list[i] == IMOD_MESH_BGNPOLYNORM) {
            i++;

            /* Get a total area for this polygon and find
               the surface whose contours it matches */
            psa = 0.;
            found = 0;
            while(mesh->list[i] != IMOD_MESH_ENDPOLY){

              n.x = n.y = n.z = 0.0f;
              p1 = &(mesh->vert[mesh->list[++i]]);
              i+=2;
              p2 = &(mesh->vert[mesh->list[i]]);
              i+=2;
              p3 = &(mesh->vert[mesh->list[i++]]);
                                   
              n1.x = p1->x - p2->x;
              n1.y = p1->y - p2->y;
              n1.z = (p1->z - p2->z) * zs;
              n2.x = p3->x - p2->x;
              n2.y = p3->y - p2->y;
              n2.z = (p3->z - p2->z) * zs;
              imodPointCross(&n1, &n2, &n);

              psa += sqrt(n.x*n.x + n.y*n.y + n.z*n.z) 
                * 0.5;
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
                    if (zz != p1->z && zz != 
                        p2->z && zz != p3->z)
                      break;
                    if ((xx == p1->x && yy ==
                         p1->y && zz == p1->z) ||
                        (xx == p2->x && yy ==
                         p2->y && zz == p2->z) ||
                        (xx == p3->x && yy ==
                         p3->y && zz == p3->z)) {
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
              msa[psurf] += psa * 
                imod->pixsize * imod->pixsize;
          }
        }
      }               
      fprintf(fout, "#Surface : Contours,     Volume, "
              "Cyl. Surf. Area, Mesh Surf. Area\n");
      for(i = 0; i <= obj->surfsize; i++)
        fprintf(fout, "%7d   %8d  %11.6g      %11.6g      %11.6g\n"
                , i, nofc[i], vol[i], sa[i], msa[i]);
      free(msa);
    } else {
          
      fprintf(fout, "#Surface : Contours,     Volume, Surf. Area\n");
      for(i = 0; i <= obj->surfsize; i++)
        fprintf(fout, "%7d   %8d  %11.6g  %11.6g\n", 
                i, nofc[i], vol[i], sa[i]);
    }

    free(sa);
    free(vol);
    free(nofc);
  }
  return;
}

/* prints size information for every point, for each contour with sizes or for all
   contours in scattered point objects */

static void imodinfo_points(Imod *imod, int subarea, Ipoint min, Ipoint max,
                            int useclip, int verbose)
{
  Iobj *obj;
  Icont *cont;
  int ob, co, pt;
  int objheader;
  double rsum, rsqsum, rcubsum;
  float rad, area, volume;
  int nsum, clipOK;
  float pi = 3.14159;
  int skip, goodside;
  Ipoint *p1;
  Iplane plane;

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    clipOK = obj->clips.count && (obj->clips.flags & 1) ? 1 : 0;
    objheader = 0;
    rsum = rsqsum = rcubsum = 0.0;
    nsum = 0;

    if (useclip && clipOK) {
      imodPlaneSetPN(&plane, &(obj->clips.point[0]), &(obj->clips.normal[0]));
      /* plane.c *= imod->zscale; */
    }

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
          if (subarea || (useclip && clipOK))
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
          if (!skip && useclip && clipOK) {
            goodside = imodPlaneClip(&plane, p1);
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


static void imodinfo_full_object_report(Imod *imod, int ob)
{
  Ipoint min, max, cent, ccent, mscale;
  int co;
  Icont *cont;
  Iobj *obj;
  Iobj *clip_obj;
  int contours_with_data = 0;
  Iplane plane;
  float surf, vol, tsa;
  double weight, tweight, tpt;
  int mesh, resol;

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

  surf = vol = 0.0f;
  tpt = weight = tweight = 0;
  cent.x = cent.y = cent.z = 0.0f;
  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    vol  += info_contour_vol(cont, obj->flags,
                             (double)imod->pixsize,
                             (double)imod->zscale);
    surf += info_contour_length(cont, obj->flags,
                                (double)imod->pixsize,
                                (double)imod->zscale);
    imodel_contour_centroid(cont, &ccent, &weight);
    tweight += weight;
    cent.x += ccent.x;
    cent.y += ccent.y;
    cent.z += ccent.z * imod->zscale;
  }
  if (tweight){
    cent.x /= (float)tweight;
    cent.y /= (float)tweight;
    cent.z /= (float)tweight;
  }
  mscale.x = imod->xscale;
  mscale.y = imod->yscale;
  mscale.z = imod->zscale;
  fprintf(fout, "\tCenter         = (%g, %g, %g)\n",cent.x, cent.y, cent.z);
  surf *= imod->pixsize * imod->zscale;
  fprintf(fout, "\tCylinder Surface Area   = %g ^2\n", surf);

  /* DNM: add mesh area if it exists */
  if (obj->meshsize){
    tsa = 0.0;
    imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);
    for(mesh = 0; mesh < obj->meshsize; mesh++)
      if (imeshResol(obj->mesh[mesh].flag) == resol) {
        tsa += imeshSurfaceArea (&obj->mesh[mesh],
                                 &mscale);
      }
    tsa *= imod->pixsize * imod->pixsize; 
    fprintf(fout, "\tMesh surface area       = %g ^2\n", tsa);
  }
  fprintf(fout, "\tVolume                  = %g ^3\n", vol);

  if (obj->clips.count && (obj->clips.flags & 1)) {
    /* DNM 8/3/01: divide normal.z by zscale for consistency with imod 
       and SDA output */
    fprintf(fout, "\tClip Normal    = (%g, %g, %g)\n", 
            obj->clips.normal[0].x,
            obj->clips.normal[0].y,
            obj->clips.normal[0].z / imod->zscale);
    fprintf(fout, "\tClip Point     = (%g, %g, %g)\n", 
            obj->clips.point[0].x,
            obj->clips.point[0].y,
            obj->clips.point[0].z);
    imodPlaneSetPN(&plane, &(obj->clips.point[0]), &(obj->clips.normal[0]));
    clip_obj = imodinfo_ObjectClip(obj, &plane, 1);
    if (!clip_obj){
      fprintf(fout, "\tError getting clip data.\n");
      return;
    }
    surf = vol = 0.0f;
    for(co = 0; co < clip_obj->contsize; co++){
      cont = &(clip_obj->cont[co]);
      vol  += info_contour_vol(cont, clip_obj->flags,
                               (double)imod->pixsize,
                               (double)imod->zscale);
    }

    /* DNM 8/4/01: delete cylinder surface area as hopeless because of
       the cut edge; add mesh surface area */
    if (obj->meshsize){
      tsa = 0.0;
      min.x -= 100.;
      min.y -= 100.;
      min.z -= 100.;
      max.x += 100.;
      max.y += 100.;
      max.z += 100.;
      imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);
      for(mesh = 0; mesh < obj->meshsize; mesh++)
        if (imeshResol(obj->mesh[mesh].flag) == resol) {
          tsa += imeshSurfaceSubarea 
            (&obj->mesh[mesh], &mscale, min, max,
             1, &plane);
        }
      tsa *= imod->pixsize * imod->pixsize; 
      fprintf(fout, "\tClipped Mesh surface area = %g ^2\n", tsa);
    }

    fprintf(fout, "\tClipped Volume            = %g ^3\n", vol);
  }else{
    fprintf(fout, "\tNo Clipping Plane.\n");
  }

  return;
}

static void imodinfo_object(struct Mod_Model *imod)
{
  int ob, co, pt, tpt;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  struct Mod_Point   cent, ccent;
  float surf, vol;
  int i;
  float units = 1.0;
  float pixsize, zscale;
  double weight, tweight;
  pixsize = imod->pixsize;
  zscale = imod->zscale;


  fprintf(fout, "#Obj       Surf \tVolume\t\tCenter\n");
  fprintf(fout, "#------------------------------------------------------------\n");

  for (ob = 0; ob < imod->objsize; ob++){
    obj = &(imod->obj[ob]);
    surf = 0.0f;
    vol = 0.0f;
    tpt = 0;
    cent.x = 0.0f;
    cent.y = 0.0f;
    cent.z = 0.0f;
    tweight = 0;

    for(co = 0; co < obj->contsize; co++){
      cont = &(obj->cont[co]);
      vol  += info_contour_vol(cont, obj->flags,
                               (double)imod->pixsize,
                               (double)imod->zscale);
      surf += info_contour_length(cont, obj->flags,
                                  (double)imod->pixsize,
                                  (double)imod->zscale);
      imodel_contour_centroid(cont, &ccent, &weight);
      tweight += weight;
      cent.x += ccent.x;
      cent.y += ccent.y;
      cent.z += ccent.z * zscale;
    }
    surf *= imod->pixsize * imod->zscale;
    if (tweight){
      cent.x /= (float)tweight;
      cent.y /= (float)tweight;
      cent.z /= (float)tweight;
    }

    if (!(iobjClose(obj->flags))){
      surf = vol = 0.0;
    }

    if (obj->contsize)
      fprintf(fout, "%4d  %10.2f  %10.2f \t%11.2f %11.2f %11.2f\n", 
              ob + 1, surf, vol, cent.x, cent.y, cent.z);
    else
      fprintf(fout, "%4d  0     0\t\t       x           x           x\n",
              ob + 1);
  }
  return;
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
static void imodinfo_objndist(struct Mod_Model *imod, int bins)
{
  int ob, co, pt, tpt, i;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  struct Mod_Contour *dcont;
  struct Mod_Point    pnt;
  int d, bin, level;
  double dist, mindist, min, max, binsize, binval, binmin, binmax;
  double pixsize, zscale;
  double *dista;
  int   comps = 0;

  if (!bins)
    bins = 20;

  pixsize = imod->pixsize;
  if (pixsize == 0)
    pixsize = 1;
  zscale = imod->zscale;
  if (zscale == 0)
    zscale = 1.0;
          
  dcont = imodel_contour_create();

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
    imodel_point_add(dcont, &pnt, dcont->psize);
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


int imodinfo_objdist(struct Mod_Model *imod, int bins)
{

  int ob, co, pt, tpt, i;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  struct Mod_Contour *dcont;
  struct Mod_Point    pnt;
  int d, bin, level;
  double *dist, min, max, binsize, binval, binmin, binmax;
  double pixsize;
  int   comps = 0;

  if (!bins)
    bins = 100;

  pixsize = imod->pixsize;
  if (pixsize == 0)
    pixsize = 1;

  dcont = imodel_contour_create();

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
    imodel_point_add(dcont, &pnt, dcont->psize);
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

static void imodinfo_interactive(Imod *imod)
{
  int c;
  int ob, co, pt;
  int done = FALSE;
  ob = imod->cindex.object;
  co = imod->cindex.contour;
  pt = imod->cindex.point;

  while(!done){

    c = getchar();

    switch(c){

    case 'o':
      break;
    case 'O':
      break;

    case 'Q':
    case 'q':
      done = TRUE;
      break;
    }
  }
  return;
}


static int contour_stats(Icont *cont, int flags, double pixsize, 
                         double zscale)
{
  float area, cdist, odist;
  float length, width, aspect;
  Ipoint cmass, ll, ur;
  float moment;
  double orientation;
  if (!cont) return -1;

  if (iobjScat(flags)){
    imodContourCenterOfMass(cont, &cmass);
    fprintf(fout, "\t\tCenter of Mass     = (%g, %g, %g) in pixel coords.\n",
            cmass.x, cmass.y, cmass.z);
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
  imodFreeObject(imod, 137);
  imodFreeObject(imod, 136);

  imodWrite(imod, fout);
  fclose(fout);
  return;
}



/****************************************************************************/

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
    if (iobjClose(objflags)){
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

static double info_contour_area(Icont *cont, int objflags,
                                double pixsize, double zscale)
{
  int pt;
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
     


static Iobj *imodinfo_ObjectClip(Iobj *obj, Iplane *plane, int planes)
{
  Iobj *robj = imodObjectNew();
  Icont *cc, *cont;
  Ipoint ipnt, cpnt;
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
      clp =  ( ((plane->a * cpnt.x) + (plane->b * cpnt.y) + 
                (plane->c * cpnt.z ) + plane->d));


      if (clp >= 0.0f){
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
    if (cc->psize > 0)
      imodObjectAddContour(robj, cc);
    else
      imodContourDelete(cc);
  }
  robj->flags |= IMOD_OBJFLAG_OPEN;
  return(robj);
}

/* Return area of mesh in square pixels, for all triangles wholly inside
   the subset min and max limits.  Works only for POLYNORM meshes. */
float imeshSurfaceSubarea(Imesh *mesh, Ipoint *scale, Ipoint min, Ipoint max,
                          int doclip, Iplane *plane)
{
  int i, inside1, inside2, inside3;
  double tsa = 0.0f;
  float sa = 0.0f;
     
  Ipoint *p1, *p2, *p3, *p;
  Ipoint n, n1, n2;
  float zs = 1.0f;
  int nsum = 0;
  float clipfrac;

  if ((!mesh) || (!mesh->lsize))
    return(0.0f);
  if (scale) zs = scale->z;

  for(i = 0; i < mesh->lsize; i++){
    switch(mesh->list[i]){
    case IMOD_MESH_BGNPOLYNORM:
      i++;
      while(mesh->list[i] != IMOD_MESH_ENDPOLY){
        n.x = n.y = n.z = 0.0f;
        p1 = &(mesh->vert[mesh->list[++i]]);
        i+=2;
        p2 = &(mesh->vert[mesh->list[i]]);
        i+=2;
        p3 = &(mesh->vert[mesh->list[i++]]);
                    
        /* Count the number of vertices that are inside the defined volume
           first evaluate their relation to the clipping plane */
        inside1 = 1;
        inside2 = 1;
        inside3 = 1;
    
        if (doclip) {
          inside1 = imodPlaneClip(plane, p1);
          inside2 = imodPlaneClip(plane, p2);
          inside3 = imodPlaneClip(plane, p3);
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
      
          tsa += clipfrac * sqrt((double)(n.x*n.x + n.y*n.y + 
                                          n.z*n.z)) * 0.5;
          nsum++;
        }
      }
               
      break;
               
    case IMOD_MESH_END:
      return((float)tsa);
               
    }
          
  }
  return((float)tsa);
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
               
static float scanned_volume(Iobj *obj, int subarea, Ipoint ptmin, Ipoint ptmax,
                            int doclip, Iplane *plane)
{
  int co;
  Icont *cont;
  Icont **scancont;
  int zmin,zmax;
  int i, j;
  int z, indz;
  int cz;
  int nummax, inbox, clipsum;
  Ipoint tmpmax, tmpmin, corner;

  int *contz;
  Ipoint *pmin, *pmax;
  int *numatz;
  int **contatz;
  int  kis;
  int numnests;
  int eco;
  float frac1, frac2;
  Nesting *nest, *nests;
  int *nestind;
  int inco, outco, level, more, ready, nind, oind;
  float *areas;
  double tvol = 0.;

  if (!obj->contsize)
    return(0.);
     
  /* Find min and max z values.
   * Clear the type value used to store connection information.
   */
  contz = (int *)malloc(obj->contsize * sizeof(int));
  if (!contz)
    return -1.;
  zmin = INT_MAX;
  zmax = INT_MIN;
  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    if (!cont->psize)
      continue;
    cz = getContZValue(cont);
    contz[co] = cz;
    if (cz < zmin)
      zmin = cz;
    if (cz > zmax)
      zmax = cz;
  }

  numatz = (int *)malloc((zmax - zmin + 2) * sizeof(int));
  contatz = (int **)malloc((zmax - zmin + 2) * sizeof(int *));
  if (!numatz || !contatz)
    return -1.;
  for(z = zmin; z <= zmax; z++)
    numatz[z - zmin] = 0;

  /* Get number of contours at each z and tables of each */
  for(co = 0; co < obj->contsize; co++)
    if (obj->cont[co].psize)
      numatz[contz[co] - zmin]++;

  nummax =0;
  for (i = 0; i < zmax + 1 - zmin; i++) {
    contatz[i] = (int *)malloc(numatz[i] * sizeof(int));
    if (!contatz[i])
      return -1.;
    if (numatz[i] > nummax)
      nummax = numatz[i];
    numatz[i] = 0;
  }
                             
  for(co = 0; co < obj->contsize; co++)
    if (obj->cont[co].psize) {
      i = contz[co] - zmin;
      contatz[i][numatz[i]++] = co;
    }

  /* Allocate space for lists of min's max's, and scan contours */
  pmin = (Ipoint *)malloc(nummax * sizeof(Ipoint));
  pmax = (Ipoint *)malloc(nummax * sizeof(Ipoint));
  scancont = (Icont **)malloc(nummax * sizeof (Icont *)); 
  areas = (float *)malloc(nummax * sizeof(float));

  /* Get array for pointers to inside/outside information */
  nestind = (int *)malloc(nummax * sizeof(int));
  nests = (Nesting *)malloc(nummax * sizeof(Nesting));
  if (!pmin || !pmax || !scancont || !areas || !nestind || !nests)
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
      if (!cont->psize)
        continue;
      imodel_contour_mm(cont, &tmpmax, &tmpmin);
      if (tmpmin.x > ptmax.x || tmpmax.x < ptmin.x ||
          tmpmin.y > ptmax.y || tmpmax.y < ptmin.y)
        continue;

      if (doclip) {
        /* Evaluate the corners of the bounding box: if all on 
           wrong side of clip plane, skip */
        corner = tmpmin;
        clipsum = imodPlaneClip(plane, &corner);
        corner.y = tmpmax.y;
        clipsum += imodPlaneClip(plane, &corner);
        corner.x = tmpmax.x;
        clipsum += imodPlaneClip(plane, &corner);
        corner.y = tmpmin.y;
        clipsum += imodPlaneClip(plane, &corner);
        if ((clipsum == 0 && doclip > 0) || 
            (clipsum == 4 & doclip < 0))
          continue;
      }

      scancont[inbox] = imodel_contour_scan(cont);

      /* need to copy the z coordinate over! */
      if (scancont[inbox]->psize)
        scancont[inbox]->pts->z = cont->pts->z;

      /* Start with true area of untrimmed contour */
      areas[inbox] = imodContourArea(cont);

      /* If contour is not wholly inside the box, or is not all on 
         the good side of the clip plane, need to trim scan
         contour down */
      if (tmpmin.x < ptmin.x || tmpmax.x > ptmax.x ||
          tmpmin.y < ptmin.y || tmpmax.y > ptmax.y || 
          (doclip && (clipsum > 0 && clipsum < 4))) {
        frac1 = scan_contour_area(scancont[inbox]);
        trim_scan_contour(scancont[inbox], ptmin, ptmax, doclip,
                          plane);
        if(!scancont[inbox]->psize) {
          /* If contour now empty, delete and skip it */
          imodContourDelete(scancont[inbox]);
          continue;
        }
        imodel_contour_mm(scancont[inbox], &tmpmax, &tmpmin);

        /* Adjust true area down by ratio of trimmed to original
           scan-contour area */
        frac2 = scan_contour_area(scancont[inbox]);
        if (frac1 && (frac2 < frac1)) 
          areas[inbox] *= frac2 / frac1;
      }

      pmin[inbox] = tmpmin;
      pmax[inbox] = tmpmax;
      nestind[inbox++] = -1;
      /*  printf ("contour %d  area %f\n", co + 1, areas[inbox-1]); */
    }

    /* Look for overlapping contours as in imodmesh */
    for (co = 0; co < inbox - 1; co++) {
      for (eco = co + 1; eco < inbox; eco++) {
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
          /* add outside one to the inside's lists */
          nind = nestind[inco];
          if (nind < 0) {
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
        /* if the only contours outside have level assigned but 
           lower than the current level, then this contour can be 
           assigned to the current level */
        for (i = 0; i < nests[nind].noutside; i++) {
          oind = nestind[nests[nind].outside[i]];
          if (nests[oind].level == 0 || 
              nests[oind].level >= level) {
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
          
    /* now add up areas of non-nested and odd levels, 
       minus even levels */
    for (co = 0; co < inbox; co++) {
      level = 1;
      if (nestind[co] >= 0)
        level = nests[nestind[co]].level;
      if (level % 2) {
        /* printf ("adding %f at level %d\n", areas[co], level); */
        tvol += areas[co];
      } else {
        /* printf ("subtracting %f at level %d\n", areas[co], level); */
        tvol -= areas[co];
      }
    }

    /* clean up inside the nests */
    for (nind = 0; nind < numnests; nind++) {
      nest = &nests[nind];
      if (nest->ninside)
        free(nest->inside);
      if (nest->noutside)
        free(nest->outside);
    }
    /* clean up scan conversions */
    for (co = 0; co < inbox; co++)
      imodContourDelete(scancont[co]);
  }

  /* clean up everything else */
  free(nests);
  free(nestind);

  for (i = 0; i < zmax + 1 - zmin; i++) {
    if (numatz[i])
      free (contatz[i]);
  }
  free(numatz);
  free(contatz);
  free(contz);
  free(scancont);
  free(pmin);
  free(pmax);
  free(areas);
  return ((float)tvol);
}

/* Slight mod of library function to assume it's already a scan cont, and to 
   add up and return doubles not ints */
static double scan_contour_area(struct Mod_Contour *cont)
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

static void trim_scan_contour(Icont *cont, Ipoint min, Ipoint max, int doclip,
                              Iplane *pl)
{
  int i;
  Ipoint *pts;
  Ipoint tmin, tmax;
  float yline, ylast, crit, pmag, contz;

  if (!cont)
    return;
  if (cont->psize < 2)
    return;
  pts = cont->pts;
  contz = pts->z;
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
          tmin.y = crit;
          /* printf("%f  tmin.y  %f\n", contz, crit); */
        } else {
          tmax.y = crit;
          /* printf("%f  tmax.y  %f\n", contz, crit); */
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
        tmin.x = crit;
        /*  printf ("%f %d %f tmin.x %f\n", contz, i, yline, crit); */
      } else {
        tmax.x = crit;
        /* printf ("%f %d %f tmax.x %f\n", contz, i, yline, crit); */
      }

    }

    if (yline == pts[i + 1].y) {
      if (yline < tmin.y || yline > tmax.y || pts[i].x > tmax.x ||
          pts[i + 1].x < tmin.x) {
        /* If line is out of bounds in y or x, delete 2 points */
        imodel_point_delete(cont, i);
        imodel_point_delete(cont, i);
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
