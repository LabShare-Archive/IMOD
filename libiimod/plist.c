#include <stdio.h>
#include "mrcc.h"
#include <stdlib.h>
#include "b3dutil.h"

static int plist_load(FILE *fin, struct LoadInfo *li, int nx, int ny, int nz);

/* load piece list into LoadInfo structure. */
int mrc_plist_li(struct LoadInfo *li, struct MRCheader *hdata, char *fname)
{
     FILE *fin;
     int retval;
     if (!fname) return(1);
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

int mrc_plist_load(struct LoadInfo *li, struct MRCheader *hdata, FILE *fin)
{
     return (plist_load(fin, li, hdata->nx, hdata->ny, hdata->nz));
}

static int plist_load(FILE *fin, struct LoadInfo *li, int nx, int ny, int nz)
{
     int i, scanret;
     int x, y, z;

     li->plist = nz;

     li->pcoords = (int *)malloc(sizeof(int) * 3 * nz);

     for(i=0; i < nz; i++){
	  scanret = fscanf(fin, "%d %d %d", &x, &y, &z);
	  if (scanret == 3) {
/*	  printf("mrc_plist_load %d : %d %d %d\n", i,x,y,z); */
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

int mrc_plist_proc(struct LoadInfo *li, int nx, int ny, int nz)
{
     int i;
     int pmin[3];
     int pmax[3];

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

     {    /* find number of z sections that contain data. */
	  int *zlist=(int *)malloc(sizeof(int) * ((int)li->pz + 1));
	  li->pdz = 0;
	  for(i = 0; i < li->pz; i++)
	       zlist[i] = 0;
	  for(i = 0; i < li->plist; i++)
	       zlist[li->pcoords[(i*3)+2]]++;
	  for(i = 0; i < li->pz; i++)
	       if (zlist[i]) li->pdz++;
	  free(zlist);
     }
     return(0);
}

/* DNM 2/10/01: create a piece list for the given number of sections, with
   # of frames in x and y given by nfx, nfy, and overlap in x and y by ovx, ovy
*/
int mrc_plist_create(struct LoadInfo *li, int nx, int ny, int nz,
		     int nfx, int nfy, int ovx, int ovy)
{
     int i, scanret;
     int x, y, z;

     li->plist = nz;

     li->pcoords = (int *)malloc(sizeof(int) * 3 * nz);

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

int iiPlistLoad(char *filename, struct LoadInfo *li, int nx, int ny, int nz)
{
     FILE *fin;
     int retval;
     if ((!filename)||(nz < 1)||(ny<1)||(nx<1)) return 1;
     fin = fopen(filename, "r");
     if (!fin) return 1;
     retval = iiPlistLoadF(fin, li, nx, ny, nz);
     fclose (fin);
     return retval;
}

int iiPlistLoadF(FILE *fin, struct LoadInfo *li, int nx, int ny, int nz)
{
    if ((!fin)||(nz < 1)||(ny<1)||(nx<1)) return 1;
    return (plist_load(fin, li, nx, ny, nz));
}
