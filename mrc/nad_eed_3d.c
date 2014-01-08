/*
 * nad_eed_3d - Nonlinear anisotropic diffusion, edge enhancing, 3D
 *
 * Author: Achilleas Frangakis
 *
 * Copyright Max-Planck-Institut for Biochemistry, Martinsried, Germany
 * Incorporated into IMOD with permission
 * Changes for IMOD are confined to two includes just below, the section on 
 * "Calculating eigenvectors and eigenvalues", diff_tensor(), main() and associated
 * routines at the end
 *
 *  $Id$
 */

/*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include "nrutil.h"
#include "dsyev3.h"
#include "cfsemshare.h"

/* 
Edge enhancing Diffusion
Achilleas Frangakis
*/


void dummies

     (float ***v,        /* image matrix */
      long  nx,          /* size in x direction */
      long  ny,          /* size in y direction */
      long  nz)          /* size in z direction */

/* creates dummy boundaries by periodical continuation */

{
long i, j, k;  /* loop variables */

for (i=1; i<=nx; i++)
    {
    for (j=1; j<=ny; j++)
        {
        v[i][j][0]    = v[i][j][nz];    /* first level of the extended image is */
                                        /* equal to the last level of the       */
                                        /* original image                       */  
        v[i][j][nz+1] = v[i][j][1];
        }
     }

for (j=1; j<=ny; j++)
    {
    for (k=0; k<=nz+1; k++)
        {
        v[0][j][k]    = v[nx][j][k];
        v[nx+1][j][k] = v[1][j][k];
        }
    }

for (k=0; k<=nz+1; k++)
    {
    for (i=0; i<=nx+1; i++)
        {
        v[i][0][k]    = v[i][ny][k];
        v[i][ny+1][k] = v[i][1][k];
        }
    }

return;
}
/* ---------------------------------------------------------------------- */

void analyse

     (float   ***u,         /* image, unchanged */
      long    nx,          /* pixel number in x direction */
      long    ny,          /* pixel number in x direction */
      long    nz,          /* pixel number in x direction */
      float   *min,        /* minimum, output */
      float   *max,        /* maximum, output */
      float   *mean,       /* mean, output */
      float   *vari)       /* variance, output */

/*
 calculates minimum, maximum, mean and variance of an image u
*/

{
long    i, j, k;       /* loop variables */
float   help;       /* auxiliary variable */
double  help2;      /* auxiliary variable */

*min  = u[1][1][1];
*max  = u[1][1][1];
help2 = 0.0;

for (i=1; i<=nx; i++)
 for (j=1; j<=ny; j++)
   for (k=1; k<=nz; k++)
     {
     if (u[i][j][k] < *min) *min = u[i][j][k];
     if (u[i][j][k] > *max) *max = u[i][j][k];
     help2 = help2 + (double)u[i][j][k];
     }
*mean = (float)help2 / (nx * ny * nz);

*vari = 0.0;
for (i=1; i<=nx; i++)
 for (j=1; j<=ny; j++)
  for (k=1; k<=nz; k++)
     {
     help  = u[i][j][k] - *mean;
     *vari = *vari + help * help;
     }
*vari = *vari / (nx * ny * nz);

return;

} /* analyse */


/* ---------------------------------------------------------------------- */

void gauss_conv 

     (float    sigma,      /* standard deviation of Gaussian */
      long     nx,         /* image dimension in x direction */ 
      long     ny,         /* image dimension in y direction */ 
      long     nz,         /* image dimension in z direction */ 
      float    hx,         /* pixel size in x direction */
      float    hy,         /* pixel size in y direction */
      float    hz,         /* pixel size in z direction */
      int    precision,  /* cutoff at precision * sigma */
      float    ***f)       /* input: original image ;  output: smoothed */


/* 
 Gaussian coonvolution. 
*/


{
long    i, j, k, p;              /* loop variables */
int     length;                  /* convolution vector: 0..length */
float   sum;                     /* for summing up */
float   *conv;                   /* convolution vector */
float   *help;                   /* row or column with dummy boundaries */
     

/* ------------------------ diffusion in x direction -------------------- */

/* calculate length of convolution vector */
length = precision + 1;
if (length>nx)
   {
    printf("gauss_conv: sigma too large \n");
    exit(0);
   }


/* allocate storage for convolution vector */
conv=vector (0, length);

/* calculate entries of convolution vector */
for (i=0; i<=length; i++)
    conv[i] = 1 / (sigma * sqrt(2.0 * 3.1415927)) 
              * exp (- (i * i * hx * hx) / (2.0 * sigma * sigma));

/* normalisation */
sum = conv[0];
for (i=1; i<=length; i++)
    sum = sum + 2.0 * conv[i];   
for (i=0; i<=length; i++)
    conv[i] = conv[i] / sum;

/* allocate storage for a row */
help=vector (0, nx+length+length-1);

for (j=1; j<=ny; j++)
    {
    for (k=1; k<=nz; k++)
        {
        /* copy in row vector */
        for (i=1; i<=nx; i++)
            help[i+length-1] = f[i][j][k];
 
            for (p=1; p<=length; p++)
                {
                help[length-p]      = help[nx+length-p];
                help[nx+length-1+p] = help[length+p-1];
                }
 
        /* convolution step */
        for (i=length; i<=nx+length-1; i++)
            {
            /* calculate convolution */
            sum = conv[0] * help[i];
            for (p=1; p<=length; p++)
                sum = sum + conv[p] * (help[i+p] + help[i-p]);
            /* write back */
            f[i-length+1][j][k] = sum;
            }
        } /* for k */
    } /* for j */

/* disallocate storage for a row */
free_vector (help, 0, nx+length+length-1);

/* disallocate convolution vector */
free_vector (conv, 0, length);

/* ------------------------ diffusion in y direction -------------------- */

/* calculate length of convolution vector */
length = precision + 1;
if (length>ny)
   {
    printf("gauss_conv: sigma too large \n");
    exit(0);
   }

/* allocate storage for convolution vector */
conv = vector (0, length);

/* calculate entries of convolution vector */
for (j=0; j<=length; j++)
    conv[j] = 1 / (sigma * sqrt(2.0 * 3.1415927)) 
              * exp (- (j * j * hy * hy) / (2.0 * sigma * sigma));

/* normalization */
sum = conv[0];
for (j=1; j<=length; j++)
    sum = sum + 2.0 * conv[j];
for (j=0; j<=length; j++)
    conv[j] = conv[j] / sum;

/* allocate storage for a row */
help = vector (0 , ny+length+length-1);

for (i=1; i<=nx; i++)
    {
    for (k=1; k<=nz; k++)
        {
        /* copy in column vector */
        for (j=1; j<=ny; j++)
            help[j+length-1] = f[i][j][k];

        /* assign boundary conditions */
               for (p=1; p<=length; p++)
                   {
                   help[length-p]      = help[ny+length-p];
                   help[ny+length-1+p] = help[length+p-1];
                   } 
 
        /* convolution step */
        for (j=length; j<=ny+length-1; j++)
            {
            /* calculate convolution */
            sum = conv[0] * help[j];
            for (p=1; p<=length; p++)
                sum = sum + conv[p] * (help[j+p] + help[j-p]);
            /* write back */
            f[i][j-length+1][k] = sum;
            }
        } /* for k */
    } /* for i */

/* disallocate storage for a row */
free_vector (help,0, ny+length+length-1);

/* disallocate convolution vector */
free_vector (conv, 0, length);

/* ------------------------ diffusion in z direction -------------------- */

/* calculate length of convolution vector */
length = precision + 1;
/*
if (length>=nz)
   {
    printf("gauss_conv: sigma too large \n");
    exit(0);
   }
*/
/* allocate storage for convolution vector */
conv = vector (0 , length);

/* calculate entries of convolution vector */
for (k=0; k<=length; k++)
    conv[k] = 1 / (sigma * sqrt(2.0 * 3.1415927)) 
              * exp (- (k * k * hz * hz) / (2.0 * sigma * sigma));

/* normalization */
sum = conv[0];
for (k=1; k<=length; k++)
    sum = sum + 2.0 * conv[k];
for (k=0; k<=length; k++)
    conv[k] = conv[k] / sum;

/* allocate storage for a row */
help = vector (0 , nz+length+length-1);

for (i=1; i<=nx; i++)
    {
    for (j=1; j<=ny; j++)
        {
        /* copy in column vector */
        for (k=1; k<=nz; k++)
            help[k+length-1] = f[i][j][k];

        /* assign boundary conditions */
               for (p=1; p<=length; p++)
                   {
                   help[length-p]      = help[nz+length-p];
                   help[nz+length-1+p] = help[length+p-1];
                   } 
 
        /* convolution step */
        for (k=length; k<=nz+length-1; k++)
            {
            /* calculate convolution */
            sum = conv[0] * help[k];
            for (p=1; p<=length; p++)
                sum = sum + conv[p] * (help[k+p] + help[k-p]);
            /* write back */
            f[i][j][k-length+1] = sum;
            }
        } /* for j */
    } /* for i */

/* disallocate storage for a row */
free_vector (help, 0, nz+length+length-1);

/* disallocate convolution vector */
free_vector (conv, 0, length);

return;

} /* gauss_conv */


/* --------------------------------------------------------------------- */
/* Definition of the Tensor */
void struct_tensor_eed

     (float    ***v,       /* image !! gets smoothed on exit !! */
      long     nx,         /* image dimension in x direction */
      long     ny,         /* image dimension in y direction */
      long     nz,         /* image dimension in z direction */
      float    hx,         /* pixel size in x direction */
      float    hy,         /* pixel size in y direction */
      float    hz,         /* pixel size in z direction */
      float    sigma,      /* noise scale */
      float    ***dxx,     /* element of structure tensor, output */
      float    ***dxy,     /* element of structure tensor, output */
      float    ***dxz,     /* element of structure tensor, output */
      float    ***dyy,     /* element of structure tensor, output */
      float    ***dyz,     /* element of structure tensor, output */
      float    ***dzz,     /* element of structure tensor, output */
      float    ***grd)  

/*
 Calculates the structure tensor.
*/

{
long    i, j, k;                     /* loop variables */
float   dv_dx, dv_dy, dv_dz;         /* derivatives of v */
float   two_hx, two_hy, two_hz;      /* time savers */

/* ---- smoothing at noise scale, reflecting b.c. ---- */
/*
* if (sigma > 0.0) 
*    gauss_conv (sigma, nx, ny, nz, hx, hy, hz, 2, v);  
*/

/* ---- building tensor product ---- */

two_hx = 2.0 * hx;    /*norming element -depends on Pixel size */
two_hy = 2.0 * hy;    /*norming element -depends on Pixel size */
two_hz = 2.0 * hz;    /*norming element -depends on Pixel size */
dummies (v, nx, ny, nz);

for (i=1; i<=nx; i++)
 for (j=1; j<=ny; j++)
  for (k=1; k<=nz; k++)
      {
      dv_dx = (v[i+1][j][k] - v[i-1][j][k]) / two_hx;
      dv_dy = (v[i][j+1][k] - v[i][j-1][k]) / two_hy;
      dv_dz = (v[i][j][k+1] - v[i][j][k-1]) / two_hz;
      dxx[i][j][k] = dv_dx * dv_dx;
      dxy[i][j][k] = dv_dx * dv_dy; 
      dxz[i][j][k] = dv_dz * dv_dx;
      dyy[i][j][k] = dv_dy * dv_dy; 
      dyz[i][j][k] = dv_dz * dv_dy;
      dzz[i][j][k] = dv_dz * dv_dz;
      grd[i][j][k] = sqrt(dv_dx*dv_dx + dv_dy*dv_dy + dv_dz*dv_dz);
      }

/* Smooth the gradient */

if (sigma > 0.0) 
   {
   gauss_conv (sigma, nx, ny, nz, hx, hy, hz, 2, dxx);
   gauss_conv (sigma, nx, ny, nz, hx, hy, hz, 2, dxy);
   gauss_conv (sigma, nx, ny, nz, hx, hy, hz, 2, dxz);
   gauss_conv (sigma, nx, ny, nz, hx, hy, hz, 2, dyy);
   gauss_conv (sigma, nx, ny, nz, hx, hy, hz, 2, dyz);
   gauss_conv (sigma, nx, ny, nz, hx, hy, hz, 2, dzz);
   }

/*gauss_conv (0.5, nx, ny, nz, hx, hy, hz, 1, grd);*/

return;

} 


/* ------------------------------------------------------------------------ */
/* Calculating eigenvectors and eigenvalues */

/* Changes for IMOD: jacobi() and eigsrt() from Numerical recipes removed, */
/* read_struct_tens() and read_ei() changed to use the vectors from dsyevq3() */

/* Define the struct_tens matrix */

void read_struct_tens
   (float    v_dxx,     /* 11 element of structure tensor */
    float    v_dxy,     /* 12 element of structure tensor */
    float    v_dxz,     /* 13 element of structure tensor */
    float    v_dyy,     /* 22 element of structure tensor */
    float    v_dyz,     /* 23 element of structure tensor */
    float    v_dzz,     /* 33 element of structure tensor */
    double   a[3][3])   /* Structure Tensor in complete matrix form */ 

{
  /* DNM: This loads the upper triangle of matrix */
   a[0][0]=v_dxx;
   a[0][1]=v_dxy;
   a[0][2]=v_dxz;
   a[1][1]=v_dyy;
   a[1][2]=v_dyz;
   a[2][2]=v_dzz;

return;
}

/* read the eigenvalue and eigenvectors */
void read_ei

  (double  *w,      /* Input, eigenvalues in ascending order */
   double  a[3][3], /* Input, corresponding eigenvectors */
   float  *ev11,   /* 1. comp. of 1. eigenvector */ 
   float  *ev12,   /* 2. comp. of 1. eigenvector */ 
   float  *ev13,   /* 3. comp. of 1. eigenvector */
   float  *ev21,   /* 1. comp. of 2. eigenvector */ 
   float  *ev22,   /* 2. comp. of 2. eigenvector */ 
   float  *ev23,   /* 3. comp. of 2. eigenvector */
   float  *ev31,   /* 1. comp. of 3. eigenvector */ 
   float  *ev32,   /* 2. comp. of 3. eigenvector */ 
   float  *ev33,   /* 3. comp. of 3. eigenvector */
   float  *lam1,   /* 1. eigenvalue */ 
   float  *lam2,   /* 2. eigenvalue */ 
   float  *lam3)   /* 3. eigenvalue */ 
    
{
/* Output */
*ev11=a[0][0];
*ev12=a[1][0];
*ev13=a[2][0];
*ev21=a[0][1];
*ev22=a[1][1];
*ev23=a[2][1];
*ev31=a[0][2];
*ev32=a[1][2];
*ev33=a[2][2];

*lam1=w[0];
*lam2=w[1];
*lam3=w[2];

return;
}

/* End Definition of the structure tensor */


/* ----------------------------------------------------------------------- */

void PA_backtrans 

     (float  ev11,   /* 1. comp. of 1. eigenvector */ 
      float  ev12,   /* 2. comp. of 1. eigenvector */ 
      float  ev13,   /* 3. comp. of 1. eigenvector */
      float  ev21,   /* 1. comp. of 2. eigenvector */ 
      float  ev22,   /* 2. comp. of 2. eigenvector */ 
      float  ev23,   /* 3. comp. of 2. eigenvector */
      float  ev31,   /* 1. comp. of 3. eigenvector */ 
      float  ev32,   /* 2. comp. of 3. eigenvector */ 
      float  ev33,   /* 3. comp. of 3. eigenvector */
      float  lam1,   /* 1. eigenvalue */ 
      float  lam2,   /* 2. eigenvalue */ 
      float  lam3,   /* 3. eigenvalue */ 
      float  *a11,   /* coeff. of (3*3)-matrix, output */ 
      float  *a12,   /* coeff. of (3*3)-matrix, output */ 
      float  *a13,   /* coeff. of (3*3)-matrix, output */ 
      float  *a22,   /* coeff. of (3*3)-matrix, output */ 
      float  *a23,   /* coeff. of (3*3)-matrix, output */ 
      float  *a33)   /* coeff. of (3*3)-matrix, output */ 


/*
 Principal axis backtransformation of a symmetric (3*3)-matrix. 
 A = U * diag(lam1, lam2, lam3) * U_transpose with U = (v1 | v2 | v3)     
 v1 = (ev11, ev12, ev13) is first eigenvector
*/

{

*a11 = lam1*ev11*ev11 + lam2*ev21*ev21 + lam3*ev31*ev31;
*a12 = lam1*ev11*ev12 + lam2*ev21*ev22 + lam3*ev31*ev32;
*a13 = lam1*ev11*ev13 + lam2*ev21*ev23 + lam3*ev31*ev33;
*a22 = lam1*ev12*ev12 + lam2*ev22*ev22 + lam3*ev32*ev32;
*a23 = lam1*ev12*ev13 + lam2*ev22*ev23 + lam3*ev32*ev33;
*a33 = lam1*ev13*ev13 + lam2*ev23*ev23 + lam3*ev33*ev33;

return;

} /* PA_backtrans */

/*--------------------------------------------------------------------------*/
void diff_tensor 
     
     (float    lambda,/* edge enhancing diffusion coefficient */
      long     nx,       /* image dimension in x direction */
      long     ny,       /* image dimension in y direction */
      long     nz,       /* image dimension in z direction */
      float    hx,       /* Pixelsize in x direction */
      float    hy,       /* Pixelsize in y direction */
      float    hz,       /* Pixelsize in z direction */
      float    sigma,    /* Noise scale */
      float    ***grd,   /* Gradient of the image */
      float    ***dxx,    /* in: structure tensor el., out: diff. tensor el. */
      float    ***dxy,    /* in: structure tensor el., out: diff. tensor el. */ 
      float    ***dxz,    /* in: structure tensor el., out: diff. tensor el. */ 
      float    ***dyy,    /* in: structure tensor el., out: diff. tensor el. */
      float    ***dyz,    /* in: structure tensor el., out: diff. tensor el. */ 
      float    ***dzz)    /* in: structure tensor el., out: diff. tensor el. */ 
/*
 Calculates the diffusion tensor of CED by means of the structure tensor.
*/
/* Changes for IMOD: eigenvector input and output arrays replaced by a, q, w and 
   dsyevq3 and eigenSort are used from libcfshr. */
{
  long    i, j, k;                   /* loop variables */
  float   *ev11, *ev12, *ev13;       /* specify first eigenvector */
  float   *ev21, *ev22, *ev23;       /* specify second eigenvector */
  float   *ev31, *ev32, *ev33;       /* specify third eigenvector */
  float   *mu1, *mu2, *mu3;          /* eigenvalues of structure tensor */
  float   lam1, lam2, lam3;          /* eigenvalues of diffusion tensor */
  float   ***v;                      /* image */
  double  a[3][3];                   /* Matrix for real tensors */
  double  q[3][3];                   /* Matrix for eigenvectors */
  double  w[3];                      /* Array with unordered eigenvalues */


v=f3tensor(0, nx+1, 0 , ny+1,0, nz+1);
ev11=vector(0,0);ev12=vector(0,0);ev13=vector(0,0);ev21=vector(0,0);ev22=vector(0,0);
ev23=vector(0,0);ev31=vector(0,0);ev32=vector(0,0);ev33=vector(0,0);
mu1=vector(0,0);mu2=vector(0,0);mu3=vector(0,0);


for (i=1; i<=nx; i++)
 for (j=1; j<=ny; j++)
  for (k=1; k<=nz; k++)
     {
        read_struct_tens(dxx[i][j][k], dxy[i][j][k], dxz[i][j][k], dyy[i][j][k], dyz[i][j][k], dzz[i][j][k], a);
        dsyevq3(a, q, w);
        eigenSort(w, &q[0][0], 3, 3, 1, 0);
        read_ei(w, q, ev11, ev12, ev13, ev21, ev22, ev23, ev31, ev32, ev33,  mu1, mu2, mu3);

        if (grd[i][j][k] > 0.0)
         {
         lam1 = 1.0 - exp (-3.31488 / pow(grd[i][j][k]/lambda,16.0)); 
         lam2 = 1.0 - exp (-3.31488 / pow(grd[i][j][k]/lambda,16.0));
         }  /* end if */
         else
         {
         lam1 = 1;
         lam2 = 1;
         }  /* end else */

         lam3 = 1;
         PA_backtrans (*ev11, *ev12, *ev13, *ev21, *ev22, *ev23, *ev31, *ev32, *ev33, lam1, lam2, lam3, &dxx[i][j][k], &dxy[i][j][k], &dxz[i][j][k], &dyy[i][j][k], &dyz[i][j][k], &dzz[i][j][k]); 

     }    /* end for */

free_f3tensor(v, 0, nx+1, 0 , ny+1,0, nz+1);

free_vector(ev11,0,0);free_vector(ev12,0,0);free_vector(ev13,0,0);free_vector(ev21,0,0);free_vector(ev22,0,0);
free_vector(ev23,0,0);free_vector(ev31,0,0);free_vector(ev32,0,0);free_vector(ev33,0,0);
free_vector(mu1,0,0);free_vector(mu2,0,0);free_vector(mu3,0,0);

return;

}  /* diff_tensor */

/*--------------------------------------------------------------------------*/


void eed 

     (float    ht,          /* time step size, 0 < ht <= 0.25 */
      long     nx,          /* image dimension in x direction */ 
      long     ny,          /* image dimension in y direction */ 
      long     nz,          /* image dimension in z direction */ 
      float    hx,          /* pixel size in x direction */
      float    hy,          /* pixel size in y direction */
      float    hz,          /* pixel size in z direction */
      float    sigma,       /* noise scale */
      float    lambda,   /* lamda parameter for eed */         
      float    ***u)        /* input: original image;  output: smoothed */


/* 
 Coherence-enhancing anisotropic diffusion. 
 Explicit discretization.
*/
{
long    i, j, k;                                       /* loop variables */
float   rxx, rxy, rxz, ryy, ryz, rzz;                  /* time savers */
float   wN, wNE, wE, wSE;                              /* weights */
float   wS, wSW, wW, wNW;                              /* weights */
float   wB, wSB, wNB, wEB, wWB;                        /* weights */
float   wF, wSF, wNF, wEF, wWF;                        /* weights */
float   ***f;                                          /* work copy of u */
float   ***grd;
float   ***dxx, ***dxy, ***dxz, ***dyy, ***dyz, ***dzz;
 
/* ---- allocate storage ---- */

f=f3tensor (0,nx+1,0,ny+1,0,nz+1);
grd=f3tensor (0,nx+1,0,ny+1,0,nz+1);
dxx=f3tensor (0,nx+1,0,ny+1,0,nz+1);
dxy=f3tensor (0,nx+1,0,ny+1,0,nz+1);
dxz=f3tensor (0,nx+1,0,ny+1,0,nz+1);
dyy=f3tensor (0,nx+1,0,ny+1,0,nz+1);
dyz=f3tensor (0,nx+1,0,ny+1,0,nz+1);
dzz=f3tensor (0,nx+1,0,ny+1,0,nz+1);


/* ---- copy u into f ---- */

for (i=1; i<=nx; i++)
 for (j=1; j<=ny; j++)
  for (k=1; k<=nz; k++)
     f[i][j][k] = u[i][j][k];

/* ---- calculate entries of structure tensor for eed ---- */

struct_tensor_eed (f, nx, ny, nz, hx, hy, hz, sigma, dxx, dxy, dxz, dyy, dyz, dzz, grd);

/* ---- calculate entries of diffusion tensor ---- */

diff_tensor (lambda, nx, ny, nz, hx, hy, hz, sigma, grd, dxx, dxy, dxz, dyy, dyz, dzz);


/* ---- calculate explicit nonlinear diffusion of u ---- */

rxx  = ht / (2.0 * hx * hx);
ryy  = ht / (2.0 * hy * hy);
rzz  = ht / (2.0 * hz * hz);
rxy  = ht / (4.0 * hx * hy);
rxz  = ht / (4.0 * hx * hz);
ryz  = ht / (4.0 * hy * hz);

dummies (dxx, nx, ny, nz);
dummies (dxy, nx, ny, nz);
dummies (dxz, nx, ny, nz);
dummies (dyy, nx, ny, nz);
dummies (dyz, nx, ny, nz);
dummies (dzz, nx, ny, nz);

/* copy u into f and assign dummy boundaries */
for (i=1; i<=nx; i++)
 for (j=1; j<=ny; j++)
  for (k=1; k<=nz; k++)
     f[i][j][k] = u[i][j][k];
dummies (f, nx, ny, nz);

/* diffuse */
for (i=1; i<=nx; i++)
 for (j=1; j<=ny; j++)
  for (k=1; k<=nz; k++)
     {
 
    /* weights */
     wE  =   rxx * (dxx[i+1][j][k] + dxx[i][j][k]) - rxy * (sqrt(dxy[i+1][j][k] * dxy[i+1][j][k]) + sqrt(dxy[i][j][k] * dxy[i][j][k])) - rxz * (sqrt(dxz[i+1][j][k] * dxz[i+1][j][k]) + sqrt(dxz[i][j][k] * dxz[i][j][k]));
     wW  =   rxx * (dxx[i-1][j][k] + dxx[i][j][k]) - rxy * (sqrt(dxy[i-1][j][k] * dxy[i-1][j][k]) + sqrt(dxy[i][j][k] * dxy[i][j][k])) - rxz * (sqrt(dxz[i-1][j][k] * dxz[i-1][j][k]) + sqrt(dxz[i][j][k] * dxz[i][j][k]));
     wS  =   ryy * (dyy[i][j+1][k] + dyy[i][j][k]) - rxy * (sqrt(dxy[i][j+1][k] * dxy[i][j+1][k]) + sqrt(dxy[i][j][k] * dxy[i][j][k])) - ryz * (sqrt(dyz[i][j+1][k] * dyz[i][j+1][k]) + sqrt(dyz[i][j][k] * dyz[i][j][k]));
     wN  =   ryy * (dyy[i][j-1][k] + dyy[i][j][k]) - rxy * (sqrt(dxy[i][j-1][k] * dxy[i][j-1][k]) + sqrt(dxy[i][j][k] * dxy[i][j][k])) - ryz * (sqrt(dyz[i][j-1][k] * dyz[i][j-1][k]) + sqrt(dyz[i][j][k] * dyz[i][j][k]));
     wB  =   rzz * (dzz[i][j][k-1] + dzz[i][j][k]) - ryz * (sqrt(dyz[i][j][k-1] * dyz[i][j][k-1]) + sqrt(dyz[i][j][k] * dyz[i][j][k])) - rxz * (sqrt(dxz[i][j][k-1] * dxz[i][j][k-1]) + sqrt(dxz[i][j][k] * dxz[i][j][k]));
     wF  =   rzz * (dzz[i][j][k+1] + dzz[i][j][k]) - ryz * (sqrt(dyz[i][j][k+1] * dyz[i][j][k+1]) + sqrt(dyz[i][j][k] * dyz[i][j][k])) - rxz * (sqrt(dxz[i][j][k+1] * dxz[i][j][k+1]) + sqrt(dxz[i][j][k] * dxz[i][j][k]));

     wSE =   rxy * (   dxy[i+1][j+1][k] + dxy[i][j][k] + sqrt(dxy[i+1][j+1][k] * dxy[i+1][j+1][k]) + sqrt(dxy[i][j][k] * dxy[i][j][k]));
     wNW =   rxy * (   dxy[i-1][j-1][k] + dxy[i][j][k] + sqrt(dxy[i-1][j-1][k] * dxy[i-1][j-1][k]) + sqrt(dxy[i][j][k] * dxy[i][j][k]));
     wNE =   rxy * ( - dxy[i+1][j-1][k] - dxy[i][j][k] + sqrt(dxy[i+1][j-1][k] * dxy[i+1][j-1][k]) + sqrt(dxy[i][j][k] * dxy[i][j][k]));
     wSW =   rxy * ( - dxy[i-1][j+1][k] - dxy[i][j][k] + sqrt(dxy[i-1][j+1][k] * dxy[i-1][j+1][k]) + sqrt(dxy[i][j][k] * dxy[i][j][k]));
     wSF =   ryz * (   dyz[i][j+1][k+1] + dyz[i][j][k] + sqrt(dyz[i][j+1][k+1] * dyz[i][j+1][k+1]) + sqrt(dyz[i][j][k] * dyz[i][j][k]));
     wNF =   ryz * ( - dyz[i][j-1][k+1] - dyz[i][j][k] + sqrt(dyz[i][j-1][k+1] * dyz[i][j-1][k+1]) + sqrt(dyz[i][j][k] * dyz[i][j][k]));
     wEF =   rxz * (   dxz[i+1][j][k+1] + dxz[i][j][k] + sqrt(dxz[i+1][j][k+1] * dxz[i+1][j][k+1]) + sqrt(dxz[i][j][k] * dxz[i][j][k]));
     wWF =   rxz * ( - dxz[i-1][j][k+1] - dxz[i][j][k] + sqrt(dxz[i-1][j][k+1] * dxz[i-1][j][k+1]) + sqrt(dxz[i][j][k] * dxz[i][j][k]));
     wSB =   ryz * ( - dyz[i][j+1][k-1] - dyz[i][j][k] + sqrt(dyz[i][j+1][k-1] * dyz[i][j+1][k-1]) + sqrt(dyz[i][j][k] * dyz[i][j][k]));
     wNB =   ryz * (   dyz[i][j-1][k-1] + dyz[i][j][k] + sqrt(dyz[i][j-1][k-1] * dyz[i][j-1][k-1]) + sqrt(dyz[i][j][k] * dyz[i][j][k]));
     wEB =   rxz * ( - dxz[i+1][j][k-1] - dxz[i][j][k] + sqrt(dxz[i+1][j][k-1] * dxz[i+1][j][k-1]) + sqrt(dxz[i][j][k] * dxz[i][j][k]));
     wWB =   rxz * (   dxz[i-1][j][k-1] + dxz[i][j][k] + sqrt(dxz[i-1][j][k-1] * dxz[i-1][j][k-1]) + sqrt(dxz[i][j][k] * dxz[i][j][k]));

/* printf("wSE=%f\n",wSE); */


     /* modify weights to prevent flux across boundaries */
     if (i==1)  /* set western weights zero */
        {
        wSW = 0.0;
        wW  = 0.0;
        wNW = 0.0;
        wWB = 0.0;
        wWF = 0.0; 
        }
     if (i==nx) /* set eastern weights zero */
        {
        wNE = 0.0;
        wE  = 0.0;
        wSE = 0.0;
        wEB = 0.0;
        wEF = 0.0; 
         }
     if (j==1)  /* set northern weights zero */
        {
        wNW = 0.0;
        wN  = 0.0;
        wNE = 0.0;
        wNB = 0.0;
        wNF = 0.0; 
         }
     if (j==ny) /* set southern weights zero */
        {
        wSE = 0.0;
        wS  = 0.0; 
        wSW = 0.0;
        wSB = 0.0;
        wSF = 0.0; 
         } 
     if (k==nz)  /* set forward weights zero */
        {
        wEF = 0.0;
        wF  = 0.0;
        wWF = 0.0;
        wNB = 0.0;
        wSF = 0.0;
        }
     if (k==1) /* set backward weights zero */
        {
        wSB = 0.0;
        wB  = 0.0; 
        wNB = 0.0;
        wEB = 0.0;
        wWB = 0.0; 
        } 

     /* evolution */
     u[i][j][k] = f[i][j][k]  
             + wE  * (f[i+1][j][k]   - f[i][j][k]) 
             + wW  * (f[i-1][j][k]   - f[i][j][k]) 
             + wS  * (f[i][j+1][k]   - f[i][j][k]) 
             + wN  * (f[i][j-1][k]   - f[i][j][k])
             + wB  * (f[i][j][k-1]   - f[i][j][k])
             + wF  * (f[i][j][k+1]   - f[i][j][k])
             + wSE * (f[i+1][j+1][k] - f[i][j][k]) 
             + wNW * (f[i-1][j-1][k] - f[i][j][k]) 
             + wSW * (f[i-1][j+1][k] - f[i][j][k]) 
             + wNE * (f[i+1][j-1][k] - f[i][j][k])
             + wNB * (f[i][j-1][k-1] - f[i][j][k]) 
             + wNF * (f[i][j-1][k+1] - f[i][j][k]) 
             + wEB * (f[i+1][j][k-1] - f[i][j][k]) 
             + wEF * (f[i+1][j][k+1] - f[i][j][k])
             + wWB * (f[i-1][j][k-1] - f[i][j][k]) 
             + wWF * (f[i-1][j][k+1] - f[i][j][k]) 
             + wSB * (f[i][j+1][k-1] - f[i][j][k]) 
             + wSF * (f[i][j+1][k+1] - f[i][j][k]);
            
     } /* for i, j, k  */


/* ---- disallocate storage ---- */

free_f3tensor (f,   0,nx+1,0,ny+1,0,nz+1);
free_f3tensor (grd, 0,nx+1,0,ny+1,0,nz+1);
free_f3tensor (dxx, 0,nx+1,0,ny+1,0,nz+1);
free_f3tensor (dxy, 0,nx+1,0,ny+1,0,nz+1);
free_f3tensor (dxz, 0,nx+1,0,ny+1,0,nz+1);
free_f3tensor (dyy, 0,nx+1,0,ny+1,0,nz+1);
free_f3tensor (dyz, 0,nx+1,0,ny+1,0,nz+1);
free_f3tensor (dzz, 0,nx+1,0,ny+1,0,nz+1);


return;

} /* ced */


/*--------------------------------------------------------------------------*/
// IMOD modifications all below here

#include "b3dutil.h"
#include "parse_params.h"
#include "iimage.h"

void usage(char *progname, float ht, int pmax, float sigma, float lambda)
{
  printf("%s by A. Frangakis and R. Hegerl (adapted for IMOD)\n"
         "Usage: %s [options] <input file> <output file>\nOptions:\n"
         "\t-k #\tK (lambda) value, threshold for gradients (default %.1f)\n"
         "\t-n #\tNumber of iterations (default %d)\n"
         "\t-i list\tList of iterations at which to write output\n"
         "\t-o #\tOutput only the given Z slice (numbered from 1)\n"
         "\t-m #\tMode for output file, 0=byte, 1=int, 2=real, 6=unsigned int"
         "\n"
         "\t-s #\tSigma for smoothing of structure tensor (default %.1f)\n"
         "\t-t #\tTime step (default %.2f)\n"
         "\t-P  \tPrint PID to standard error\n",
         progname, progname, lambda, pmax, sigma, ht);
  exit(1);
}

void testNumericEntry(char *endptr, char *argv, char *option)
{
  if (endptr == argv)
    exitError("Option %s must be followed by a number, not by %s", option, 
              argv);
}
  

#define STRING_MAX 256

int main (int argc, char **argv)
{
  MrcHeader header;
  float  ***u;                   /* image */
  int   i, j, k, p;             /* loop variables */
  int   nx, ny, nz;             /* image size in x, y direction */
  FILE   *fp_infile, *fp_outfile = NULL;    /* input file, output file */
  float  ht = 0.1f;              /* time step size */
  int    pmax = 20;              /* largest iteration number */
  float  sigma = 0.;             /* noise scale */
  float  lambda = 1.;            /* lamda Parameter */
  float  max, min;               /* largest, smallest grey value */
  float  mean;                   /* average grey value */
  float  vari;                   /* variance */
  Islice *sl;
  int sliceMode;
  char *progname = imodProgName(argv[0]);
  char *endptr;
  int nWrite = 0;
  int doWrite, iarg, nzout, kst, knd, writeArg;
  int* writeList;
  char outFile[STRING_MAX];
  int oneSlice = 0;
  int outMode = -1;
  float minout, maxout, sumout;

  struct tm *local;
  time_t t;

  if (argc < 2)
    usage(progname, ht, pmax , sigma, lambda);

  setExitPrefix("ERROR: nad_eed_3d -");
  t = time(NULL);
  local = localtime(&t);
  printf ("\nProgram %s\n", progname);
  printf ("        started at: %s", asctime(local));

  if (argc < 3) {
    printf("ERROR: %s - incorrect number of input arguments\n", progname);
    usage(progname, ht, pmax, sigma, lambda);
    exit(3);
  }

  for (iarg = 1; iarg < argc; iarg++){
    if (argv[iarg][0] == '-'){
      switch (argv[iarg][1]){

      case 'k':
        lambda = strtod(argv[++iarg], &endptr);
        testNumericEntry(endptr, argv[iarg], "-k");
        break;
      case 's':
        sigma = strtod(argv[++iarg], &endptr);
        testNumericEntry(endptr, argv[iarg], "-s");
        break;
      case 'n':
        pmax = strtol(argv[++iarg], &endptr, 10);
        testNumericEntry(endptr, argv[iarg], "-n");
        break;
      case 'o':
        oneSlice = strtol(argv[++iarg], &endptr, 10);
        testNumericEntry(endptr, argv[iarg], "-o");
        break;
      case 'm':
        outMode = strtol(argv[++iarg], &endptr, 10);
        testNumericEntry(endptr, argv[iarg], "-m");
        if (sliceModeIfReal(outMode) < 0)
          exitError("Output mode %d not allowed\n", outMode);
        break;
      case 't':
        ht = strtod(argv[++iarg], &endptr);
        testNumericEntry(endptr, argv[iarg], "-t");
        break;
      case 'i':
        writeList = parselist(argv[++iarg], &nWrite);
        if (!writeList)
          exitError("Bad entry in iteration list");
        writeArg = iarg;
        break;
      case 'P':
        pidToStderr();
        break;
      default:
        exitError("Invalid option %s", argv[iarg]);
        break;
      }

    }else{
      break;
    }
  }

  // Override pmax with maximum to write
  if (nWrite > 0) {
    pmax = 0;
    for (i = 0; i < nWrite; i++)
      pmax = B3DMAX(pmax, writeList[i]);
  }

  if (iarg != argc - 2)
    exitError("Command line should end with input and output files");

  printf("input file:          %s\n", argv[iarg]);
  printf("output file:         %s\n", argv[iarg + 1]);


  /* ---- read input image ---- */
  if ((fp_infile = iiFOpen (argv[iarg], "rb")) == 0)
    exitError("Could not open input file %s", argv[iarg]);

  /* read header */
  if (mrc_head_read(fp_infile, &header))
    exitError("Reading header of input file %s", argv[iarg]);

  // Check if it is the correct data type and set slice type
  sliceMode = sliceModeIfReal(header.mode);
  if (sliceMode < 0)
    exitError("File mode is %d; only byte, short, integer allowed",
              header.mode);

  nx = header.nx;
  ny = header.ny;
  nz = header.nz;
  printf("dimensions:          %d x %d x %d\n", nx, ny, nz);
  printf("K (lambda):          %f\n\n", lambda);
  if (oneSlice < 1 || oneSlice > nz)
    oneSlice = 0;

  /* allocate storage */
  u=f3tensor( 0,nx+1,0,ny+1,0,nz+1);

  /* read image data */
  for (k=1; k<=nz; k++) {
    
    // Create a slice and read into it
    sl = sliceCreate(nx, ny, sliceMode);
    if (!sl)
      exitError("Creating slice for input");
    if (mrc_read_slice(sl->data.b, fp_infile, &header, k - 1, 'Z'))
      exitError("Reading slice %d", k);
    
    // Convert slice to floats
    if (sliceMode != SLICE_MODE_FLOAT)
      if (sliceNewMode(sl, SLICE_MODE_FLOAT) < 0)
        exitError("Converting slice to float");

    // Copy data into array
    for (j=0; j<ny; j++)
      for (i=0; i<nx; i++)
        u[i+1][j+1][k] = sl->data.f[i + j * nx];

    sliceFree(sl);
  }
  iiFClose(fp_infile);

  /* ---- Image ---- */
  analyse (u, nx, ny, nz, &min, &max, &mean, &vari);
  printf("minimum:       %1.10f \n", min);
  printf("maximum:       %1.10f \n", max);
  printf("mean:          %1.10f \n", mean);
  printf("variance:      %1.10f \n\n", vari);

  // Take care of header
  if (oneSlice && nWrite)
    sprintf(outFile, "%s: Z %d, iter %s", progname, oneSlice, 
            argv[writeArg]);
  else
    sprintf(outFile, "%s: Edge-enhancing anisotropic diffusion", progname);
  mrc_head_label(&header, outFile);

  // Adjust output mode if valid entry made (it was tested on arg processing)
  if (outMode >= 0) {
    sliceMode = sliceModeIfReal(outMode);
    header.mode = outMode;
  }

  // Fix things in header for an output file.  You have to set header size
  // not just set next to 0
  mrcInitOutputHeader(&header);

  /* ---- process image ---- */

  nzout = 0;
  minout = 1.e30;
  maxout = -minout;
  sumout = 0;
    
  for (p=1; p<=pmax; p++) {
    /* perform one iteration */
    printf("iteration number: %5d / %d \n", p, pmax);
    
    eed (ht, nx, ny, nz, 1.0, 1.0, 1.0, sigma, lambda, u);
    
    
    /* check minimum, maximum, mean, variance */
    analyse (u, nx, ny, nz, &min, &max, &mean, &vari);
    printf("minimum:       %1.10f \n", min);
    printf("maximum:       %1.10f \n", max);
    printf("mean:          %1.10f \n", mean);
    printf("variance:      %1.10f \n\n", vari);

    // Write data if it is an iteration on list or the last iteration
    doWrite = 0;
    for (i = 0; i < nWrite; i++)
      if (p == writeList[i])
        doWrite = 1;
    if (doWrite || p == pmax) {

      header.amean = mean;
      header.amin = min;
      header.amax = max;
    
      strncpy(outFile, argv[iarg + 1], STRING_MAX - 10);
      outFile[STRING_MAX - 10] = 0;
      if (nWrite && !oneSlice)
        sprintf(&outFile[strlen(outFile)], "-%03d", p);

      /* open output file if not open yet */
      if (!fp_outfile) {
        if (!getenv("IMOD_NO_IMAGE_BACKUP") && imodBackupFile(outFile))
          fprintf(stderr, "WARNING: error renaming existing %s to %s~", 
                  outFile, outFile);

        if ((fp_outfile = iiFOpen (outFile, "wb")) == 0)
          exitError("Could not open output file %s", outFile);
      } 
      
      /* write image data and close file */
      kst = oneSlice ? oneSlice : 1;
      knd = oneSlice ? oneSlice : nz;
      for (k=kst; k<=knd; k++) {
        // Create a slice and copy into it
        sl = sliceCreate(nx, ny, SLICE_MODE_FLOAT);
        if (!sl)
          exitError("Creating slice for output");
        for (j=0; j<ny; j++)
          for (i=0; i<nx; i++)
            sl->data.f[i + j * nx] = u[i+1][j+1][k];
        
        // Convert if necessary and write slice
        if (sliceMode != SLICE_MODE_FLOAT)
          if (sliceNewMode(sl, sliceMode) < 0)
            exitError("Converting slice to short");
        if (mrc_write_slice(sl->data.b, fp_outfile, &header, 
                            oneSlice ? nzout : k - 1, 'Z'))
          exitError("Writing slice %d", k);

        // If doing one slice, accumulate mmm
        if (oneSlice) {
          sliceMMM(sl);
          minout = B3DMIN(minout, sl->min);
          maxout = B3DMAX(maxout, sl->max);
          sumout += sl->mean;
        }
        sliceFree(sl);
      }
      
      nzout++;

      // Close the file if not doing one slice or end of run
      if (!oneSlice || p == pmax) {

        // Adjust size and mmm if did one slice
        if (oneSlice) {
          header.nz = nzout;
          header.mz = (header.mz * nzout) / nz;
          header.zlen = (header.zlen * nzout) / nz;
          header.amin = minout;
          header.amax = maxout;
          header.amean = sumout / nzout;
        }
      
        // Write the MRC header.        
        if (mrc_head_write(fp_outfile, &header))
          exitError("Writing header");
      
        iiFClose(fp_outfile);
        printf("output image %s successfully written\n\n", outFile);
        fp_outfile = NULL;
      }
    }
    fflush(stdout);
  } /* for */
  
  t = time(NULL);
  local = localtime(&t);
  printf ("program finished at: %s\n", asctime(local)); 
  
  /* ---- disallocate storage ---- */
  
  free_f3tensor (u, 0,nx+1,0,ny+1,0,nz+1);
  exit(0);
}

