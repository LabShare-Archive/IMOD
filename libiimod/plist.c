#include <stdio.h>
#include "mrcc.h"
#include <stdlib.h>
#include "b3dutil.h"
#include "autodoc.h"

static int plist_load(FILE *fin, IloadInfo *li, int nx, int ny, int nz);

/* load piece list into LoadInfo structure. */
/*!
 * Reads a list of piece coordinates from a text file whose name is in
 * [fname], places it in the @@mrcfiles.html#IloadInfo structure@ [li], and 
 * processes it with @mrc_plist_proc given the image dimensions in [hdata].
 * Returns 1 or -1 for errors.
 */
int mrc_plist_li(IloadInfo *li, MrcHeader *hdata, const char *fname)
{
  FILE *fin;
  int retval;
  if (!fname) 
    return(1);
  fin = fopen(fname, "r");
  if (!fin){
    li->plist = 0;

    /* DNM 11/24/03: took out errno-based report because it wouldn't link in Visual C */
    b3dError(stderr, "ERROR opening piece list file");
    return(-1);
  }
  retval = (mrc_plist_load(li, hdata, fin));
  fclose(fin);
  return(retval);
}

/*!
 * Reads a list of piece coordinates from a text file [fin],
 * places it in the @@mrcfiles.html#IloadInfo structure@ [li], and processes it
 * with @mrc_plist_proc given the image dimensions in [hdata].
 * Returns 1 for errors.
 */
int mrc_plist_load(IloadInfo *li, MrcHeader *hdata, FILE *fin)
{
  return (plist_load(fin, li, hdata->nx, hdata->ny, hdata->nz));
}

/* Reads piece list from a text file, places into li,processes it */
static int plist_load(FILE *fin, IloadInfo *li, int nx, int ny, int nz)
{
  int i, scanret;
  int x, y, z;

  li->plist = nz;

  li->pcoords = (int *)malloc(sizeof(int) * 3 * nz);

  for(i=0; i < nz; i++){
    scanret = fscanf(fin, "%d %d %d", &x, &y, &z);
    if (scanret == 3) {
      /*      printf("mrc_plist_load %d : %d %d %d\n", i,x,y,z); */
      li->pcoords[(i*3)]   = x;
      li->pcoords[(i*3)+1] = y;
      li->pcoords[(i*3)+2] = z; 
    } else {
      li->plist = i;
      if (scanret != EOF)
        b3dError(stderr, "Error reading piece list after %d lines\n"
                 , i);
      break;
    }
  }
  return(mrc_plist_proc(li, nx, ny, nz));
}

/*!
 * Analyzes the piece coordinates in the {pcoord} array in the 
 * @@mrcfiles.html#IloadInfo structure@ [li] based on the image dimensions in
 * [nx], [ny], and [nz].  It fills in the {px}, {py}, {pz}, {opx}, {opy},
 * and {opz} elements in [li], shifts coordinates to start at zero, and 
 * determines number of Z sections with data ({pdz}).  Returns 1 for memory
 * error.
 */
int mrc_plist_proc(IloadInfo *li, int nx, int ny, int nz)
{
  int i;
  int pmin[3];
  int pmax[3];
  int *zlist;

  pmin[0] = li->pcoords[0]; pmax[0] = pmin[0]+nx;
  pmin[1] = li->pcoords[1]; pmax[1] = pmin[1]+ny;
  pmin[2] = li->pcoords[2]; pmax[2] = pmin[2];
  for(i=1; i < li->plist; i++){
    if (pmin[0] > li->pcoords[(i*3)])   pmin[0] = li->pcoords[(i*3)];
    if (pmin[1] > li->pcoords[(i*3)+1]) pmin[1] = li->pcoords[(i*3)+1];
    if (pmin[2] > li->pcoords[(i*3)+2]) pmin[2] = li->pcoords[(i*3)+2];
    if (pmax[0] < (li->pcoords[(i*3)] + nx)) 
      pmax[0] = li->pcoords[(i*3)] + nx;
    if (pmax[1] < (li->pcoords[(i*3)+1] + ny)) 
      pmax[1] = li->pcoords[(i*3)+1] + ny;
    if (pmax[2] < li->pcoords[(i*3)+2]) pmax[2] = li->pcoords[(i*3)+2];
  }

  li->px = pmax[0] - pmin[0];
  li->py = pmax[1] - pmin[1];

  li->pz = pmax[2] - pmin[2] + 1;
  li->opx = pmin[0];
  li->opy = pmin[1];
  li->opz = pmin[2];

  /* shift the X and Y coordinates to zero since that's what the rest of
     the program expects */
  /* DNM 12/19/98: do this to Z as well, eliminate warning because the
     transformations should take care of it */
  for(i = 0; i < li->plist; i++){
    li->pcoords[(i*3)] -= pmin[0];
    li->pcoords[(i*3) + 1] -= pmin[1];
    li->pcoords[(i*3) + 2] -= pmin[2];
  }

  /*     if (pmin[0] || pmin[1])
         fprintf(stderr, "Adjusted X and Y piece coordinates by"
         " %d,%d to start at 0,0\n", -pmin[0], -pmin[1]); */

  /* find number of z sections that contain data. */
  zlist=(int *)malloc(sizeof(int) * ((int)li->pz + 1));
  if (!zlist)
    return 1;
  li->pdz = 0;
  for(i = 0; i < li->pz; i++)
    zlist[i] = 0;
  for(i = 0; i < li->plist; i++)
    zlist[li->pcoords[(i*3)+2]]++;
  for(i = 0; i < li->pz; i++)
    if (zlist[i]) li->pdz++;
  free(zlist);
  return(0);
}

/*!
 * Creates a piece list in [li] for [nz] sections of size [nx] by [ny], with
 * the number of frames in X and Y given by [nfx] and [nfy], and overlap in X
 * and Y given by [ovx] and [ovy].  Processes the list with @@mrc_plist_proc@.
 * Returns 1 for memory error.
 */
int mrc_plist_create(IloadInfo *li, int nx, int ny, int nz,
                     int nfx, int nfy, int ovx, int ovy)
{
  int i;
  int x, y, z;

  li->plist = nz;

  li->pcoords = (int *)malloc(sizeof(int) * 3 * nz);
  if (!li->pcoords)
    return 1;

  x = y = z = 0;
  if (ovx >= nx)
    ovx = nx - 1;
  if (ovy >= ny)
    ovy = ny - 1;

     
  for(i=0; i < nz; i++){
    li->pcoords[(i*3)]   = x * (nx - ovx);
    li->pcoords[(i*3)+1] = y * (ny - ovy);
    li->pcoords[(i*3)+2] = z; 
    x++;
    if (x >= nfx) {
      y++;
      x = 0;
      if (y >= nfy) {
        z++;
        y = 0;
      }
    }
  }
  return(mrc_plist_proc(li, nx, ny, nz));
}

/*!
 * Reads a list of piece coordinates from a text file whose name is in
 * [filename], places it in the @@mrcfiles.html#IloadInfo structure@ [li], and
 * processes it with @mrc_plist_proc given the image dimensions in [nx], [ny],
 * and [nz].  Returns 1 for all kinds of errors.
 */
int iiPlistLoad(const char *filename, IloadInfo *li, int nx, int ny, int nz)
{
  FILE *fin;
  int retval;
  if ((!filename)||(nz < 1)||(ny<1)||(nx<1)) 
    return 1;
  fin = fopen(filename, "r");
  if (!fin)
    return 1;
  retval = iiPlistLoadF(fin, li, nx, ny, nz);
  fclose (fin);
  return retval;
}

/*!
 * Reads a list of piece coordinates from the text file [fin]
 * places it in the @@mrcfiles.html#IloadInfo structure@ [li], and processes it
 * with @mrc_plist_proc given the image dimensions in [nx], [ny], and [nz].
 * Returns 1 for all kinds of errors.
 */
int iiPlistLoadF(FILE *fin, IloadInfo *li, int nx, int ny, int nz)
{
  if ((!fin)||(nz < 1)||(ny<1)||(nx<1)) 
    return 1;
  return (plist_load(fin, li, nx, ny, nz));
}

/*!
 * Reads a list of piece coordinates from an image metadata file if one exists.
 * It looks for the file with the name in [filename], or with the extension 
 * .mdoc added if [addMdoc] is non-zero.  If it finds the file, the file 
 * describes a montage, and there are [nz] sections, it places the coordinates
 * in the @@mrcfiles.html#IloadInfo structure@ [li], and processes it
 * with @mrc_plist_proc given the image dimensions in [nx], [ny], and [nz].
 * Returns -4 for bad arguments, -5 for a memory error, 1 if it is not a 
 * montage or the number of sections does not match, 2 if there are not piece
 * coordinates for every sections, and -3, -2, or -1 for an 
 * error in @@autodoc.html#AdocOpenImageMetadata@.
 */
int iiPlistFromMetadata(const char *filename, int addMdoc, IloadInfo *li, int nx, 
                        int ny, int nz)
{
  int adocIndex, montage, numSect, sectType, i;
  char *sectNames[2] = {"ZValue", "Image"};
  

  if (!filename || nx < 1 || ny < 1 || nz < 1)
    return -4;
  adocIndex = AdocOpenImageMetadata(filename, addMdoc, &montage, &numSect,
                                    &sectType);
  if (adocIndex < 0)
    return adocIndex;

  if (!montage || numSect != nz) {
    AdocClear(adocIndex);
    return 1;
  }

  li->pcoords = (int *)malloc(sizeof(int) * 3 * nz);
  if (!li->pcoords) {
    AdocClear(adocIndex);
    return -5;
  }
  li->plist = nz;

  for (i = 0; i < nz; i++) {
    if (AdocGetThreeIntegers(sectNames[sectType-1], i, "PieceCoordinates",
                             &li->pcoords[i*3], &li->pcoords[i*3+1], 
                             &li->pcoords[i*3+2]))
      break;
  }
  AdocClear(adocIndex);
  if (i < nz) {
    free(li->pcoords);
    li->plist = 0;
    return 2;
  }
  return(mrc_plist_proc(li, nx, ny, nz));
}  
