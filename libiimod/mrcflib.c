#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <mrcc.h>

typedef struct
{
     unsigned short length;
     char dum1;
     char dum2;
     char *string;
} FString;

typedef char *CString;

#ifndef F77FUNCAP
#define IALPRT ialprt_
#define IMOPEN imopen_
#define IRDHDR irdhdr_
#define IMPOSN imposn_
#define CIRDSEC cirdsec_
#define IMCLOSE imclose_
#endif

void IALPRT(int *mode);

#ifdef F77STRING
void IMOPEN(int *istream, FString *filename, FString *mode);
#else
void IMOPEN(int *istream, char *filename, char *mode, int fsize, int msize);
#endif

void IRDHDR(int *istream, long *nxyz, long *mxyz, long *mode,
	    float *min, float *max, float *mean);
void IMPOSN(int *istream, int *x, int *y);
void CIRDSEC(int *istream, float *data, int *iferr);
void IMCLOSE(int *istream);

unsigned char **mrcFlibLoadByte(char *fname,
				struct MRCheader *hdata,
				struct LoadInfo *li)
{
     FString ro;
     FString filename;
     unsigned char **idata;
     float *data;
     long nxyz[3];
     long mxyz[3];
     long mode;
     float min,max,mean;
     int istream = 1;
     int i,j,k;
     int ybgn = 0;
     int estat;
     int csize = 1;
     int nxy;
     int inscale = 0;
     float scale, offset, val, kscale, rpixel, ipixel;
     float nmax, nmin;

     /* set up fortran strings */
     ro.length = 2;
     ro.string = "RO";
     filename.length = strlen(fname);
     filename.string = fname;

     IALPRT(&ybgn);

#ifdef F77STRING
     IMOPEN(&istream, &filename, &ro);
#else
     IMOPEN(&istream, filename.string, ro.string, filename.length, ro.length);
#endif

     IRDHDR(&istream, nxyz, mxyz, &mode, &min, &max, &mean);
     hdata->nx = nxyz[0];
     hdata->ny = nxyz[1];
     hdata->nz = nxyz[2];
     hdata->mode = mode;
     hdata->amax = max;
     hdata->amean = mean;

     idata = (unsigned char **)malloc(sizeof(unsigned char *) *  hdata->nz);
     if (!idata)
	  return(NULL);
     nxy = hdata->nx * hdata->ny;
     if (mode == 3)
	  csize++;
     if (mode == 4)
	  csize++;

     data = (float *)malloc(sizeof(float) * nxy * csize);
     if (!data)
	  return(NULL);


     if ((li->smin != 0.0) && (li->smax != 0.0)){
	  max = li->smax;
	  min = li->smin;
	  inscale = 1;
     }


     if ((max - min) != 0)
	  scale = 255.0 / (max - min);
     else
	  scale = 1.0;

     offset = min * scale;

     fprintf(stderr, "max %g min  %g\n", max, min);

     for(k = 0; k < hdata->nz; k++){

	  IMPOSN(&istream, &k, &ybgn);

	  CIRDSEC(&istream, data, &estat);
	  if (estat)
	       return(NULL);
	  idata[k] = (unsigned char *)malloc(sizeof(unsigned char) * nxy);
	  if (!idata[k])
	       return(NULL);

	  if ((min == max) && (!inscale)){
	       nmax = nmin = data[0];
	       for(i = 0; i < nxy; i++){
		    if(data[i] > nmax)
			 nmax = data[i];
		    if (data[i] < nmin)
			 nmin = data[i];
	       }
	       if ((max - min) != 0)
		    scale = 255.0 / (max - min);
	       else
		    scale = 1.0;
	       offset = min * scale;
	  }

	  switch(mode){
	     case 0:
	       for(i = 0; i < nxy; i++)
		    idata[k][i] = data[i];
	     case 1:
	     case 2:
	     case 9:
	     case 10:
	     case 11:
	     case 12:
	     case 13:
	     case 14:
	     case 15:
	       for(i = 0; i < nxy; i++)
		    idata[k][i] = (data[i] * scale) - offset;
	       break;

	     case 3:
	     case 4:
	       scale = (float)log(1 + (kscale * max));
	       if (scale != 0.0)
		    scale = 255.0f / scale;
	       for(i = 0; i < nxy; i++){
		    rpixel = data[i * 2];
		    ipixel = data[(i * 2) + 1];
		    val = (rpixel * rpixel) + (ipixel * ipixel);
		    val = (float)sqrt((double)val);
		    val = (float)log(1 + (kscale * val));
		    val *= scale;
		    if (val < 0.0)
			 val = 0.0;
		    if (val > 255.0)
			 val = 255.0;
		    idata[k][i] = (unsigned char)(val + 0.5f);
	       }
	       break;

	     default:
	       return(NULL);
	       break;
	  }

     }
     free(data);
     IMCLOSE(&istream);
     return(idata);
}
