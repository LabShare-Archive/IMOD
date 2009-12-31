/*
 *  gpubp.cu -- Kernel and C code for CUDA-based backprojection, reprojection
 *               and Fourier filtering
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "cuda_runtime_api.h"
#include "cuda.h"
#include "cufft.h"
#include "imodconfig.h"

#ifdef F77FUNCAP
#define gpuavailable GPUAVAILABLE
#define gpuallocarrays GPUALLOCARRAYS
#define gpuloadproj GPULOADPROJ
#define gpushiftproj GPUSHIFTPROJ
#define gpubpxtilt GPUBPXTILT
#define gpubpnox GPUBPNOX
#define gpubplocal GPUBPLOCAL
#define gpuloadlocals GPULOADLOCALS
#define gpuloadfilter GPULOADFILTER
#define gpufilterlines GPUFILTERLINES
#define gpureproject GPUREPROJECT
#define gpudone GPUDONE
#else
#define gpuavailable gpuavailable_
#define gpuallocarrays gpuallocarrays_
#define gpuloadproj gpuloadproj_
#define gpushiftproj gpushiftproj_
#define gpubpxtilt gpubpxtilt_
#define gpubpnox gpubpnox_
#define gpubplocal gpubplocal_
#define gpuloadlocals gpuloadlocals_
#define gpuloadfilter gpuloadfilter_
#define gpufilterlines gpufilterlines_
#define gpureproject gpureproject_
#define gpudone gpudone_
#endif

#ifdef __cplusplus
extern "C" {
  int gpuavailable(int *nGPU, int *memory);
  int gpuallocarrays(int *width, int *thick, int *nxprj2, int *nviews, 
                     int *nslice, int *nxwarp);
  int gpubpnox(float *slice, float *lines, float *sbeta, float *cbeta,
               int *nxprj,
               float *xcenin, float *xcen, float *ycen, float *edgefill);
  int gpushiftproj(int *numPlanes, int *lsliceStart, int *loadStart);
  int gpuloadproj(float *lines, int *numPlanes, int *lsliceStart, 
                  int *loadStart);
  int gpubpxtilt(float *slice, float *sbeta, float *cbeta, float *salpha,
                 float *calpha, float *xzfac, float *yzfac, int *nxprj,
                 int *nyprj, float *xcenin, float *xcen, float *ycen,
                 int *lslice, float *slicen, float *edgefill);
  int gpubplocal(float *slice, int *lslice, int *nxwarp, int *nywarp,
                 int *ixswarp, int *iyswarp, int *idxwarp, int *idywarp,
                 int *nxprj, float *xcen, float *xcenin, float *delxx,
                 float *ycen, float *slicen, float *edgefill);
  int gpuloadfilter(float *lines);
  int gpuloadlocals(float *packed, int *numWarps);
  int gpufilterlines(float *lines, int *lslice);
  int gpureproject(float *lines, float *sbeta, float *cbeta, float *salpha, 
                   float *calpha, float *xzfac, float *yzfac, float *delz,
                   int *lsStart, int *lsEnd, int *ithick,
                   float *xcen, float *xcenPdelxx, int *minXreproj, 
                   float *xprjOffset, float *ycen, int *minYreproj,
                   float *yprjOffset, float *slicen, int *ifalpha, 
                   float *pmean);
  void gpudone();
}
#endif

static int checkProjLoad(int *numPlanes, int *lsliceStart, int startm1);
static int testReportErr(char *mess);
static int loadBetaInvertCos(float *cbeta, float *sbeta, float *costmp);
static int synchronizeCopySlice(float *slice, int numLines);
static void pflush(const char *format, ...);
static void pflerr(const char *format, ...);



// Offsets to positions in constant array
// For some reason 6 separate arrays did not work for xtilt case
#define DELTA_OFS  720
#define MAX_TABLE (6 * DELTA_OFS)
__constant__ float tables[MAX_TABLE];

#define COSOFS 0
#define SINOFS (1 * DELTA_OFS)
#define CALOFS (2 * DELTA_OFS)
#define SALOFS (3 * DELTA_OFS)
#define XZFOFS (4 * DELTA_OFS)
#define YZFOFS (5 * DELTA_OFS)

// Definitions for accessing the local alignments arrays with texture calls
#define F11IND 0.f
#define F21IND 1.f
#define F12IND 2.f
#define F22IND 3.f
#define F13IND 4.f
#define F23IND 5.f
#define CAIND 6.f
#define SAIND 7.f
#define CBIND 8.f
#define SBIND 9.f
#define XZFIND 10.f
#define YZFIND 11.f


// declare texture reference for 2D float textures
texture<float, 2, cudaReadModeElementType> projtex;
texture<float, 2, cudaReadModeElementType> localtex;

// Static variables for device arrays
static float* devSlice;
static cudaArray* devProj;
static float *xprojf = NULL;
static float *xprojz = NULL;
static float *yprojf = NULL;
static float *yprojz = NULL;
static cudaArray *localData = NULL;
static float *radialFilt = NULL;
static float *devFFT = NULL;

// Other static variables
static cufftHandle forwardPlan = 0, inversePlan = 0;
static int max_gflops_device = -1;
static size_t slicePitch;
static int sliceThick, sliceWidth, numViews, numProjPlanes;
static int lsliceFirst, numLoadedPlanes, nxPlane;
static int copyFilteredOK = 0;
static int *planeLoaded;

/*
 *  SETUP/SHUTDOWN ROUTINES
 */

/*
 * Test whether a GPU is available, either a GPU of the given number if nGPU is
 * > 0, or the one with the best processing rate if nGPU is 0, and return the
 * memory in bytes.  Return value is 1 for success, 0 for failure.
 */
int gpuavailable(int *nGPU, int *memory)
{
  int current_device = 0;
  int device_count = 0;
  float gflops;
  struct cudaDeviceProp device_properties, best_properties;
  float max_gflops = 0;
  *memory = 0;
  cudaGetDeviceCount( &device_count );
  if (*nGPU != 0) {
    if (*nGPU < 0 || *nGPU > device_count)
      return 0;
    current_device = *nGPU - 1;
    device_count = *nGPU;
  }
  for (; current_device < device_count; current_device++) {
    if (cudaGetDeviceProperties( &device_properties, current_device)
        != cudaSuccess) {
      pflerr("Error returned from trying to get properties of GPU device %d",
               current_device);
      return 0;
    }
    gflops = device_properties.multiProcessorCount * 
      device_properties.clockRate;
    if( gflops > max_gflops ) {
      max_gflops = gflops;
      max_gflops_device = current_device;
      best_properties = device_properties;
    }
  }
    
  if (max_gflops_device >= 0) {
    *memory = best_properties.totalGlobalMem;
    return 1;
  }
  return 0;
}

/*
 * Allocate all needed arrays on the GPU.  Allocate a reconstructed slice or
 * reprojected line array of width x thick, an array for nplanes of input data 
 * each with nviews lines of length nxprj2, and local alignment arrays if
 * numWarps > 0.  If numWarp >= 0, also allocate arrays for line filtering.
 */
int gpuallocarrays(int *width, int *thick, int *nxprj2, int *nviews, 
                   int *nplanes, int *numWarps)
{
  size_t pitch1, pitch2, pitch3, pitch4, memTot;

  if (max_gflops_device < 0)
    return 1;
  if (cudaSetDevice(max_gflops_device) != cudaSuccess) {
    pflerr("Error selecting GPU device");
    return 1;
  }

  // Allocate memory for slice on device
  size_t sizetmp = *width * sizeof(float);
  if (cudaMallocPitch((void **)&devSlice, &slicePitch, sizetmp, *thick) != 
      cudaSuccess) {
    pflerr("Failed to allocate slice array on GPU device");
    return 1;
  }

  // Allocate memory for projection lines
  cudaChannelFormatDesc projDesc = cudaCreateChannelDesc
    (32, 0, 0, 0, cudaChannelFormatKindFloat);
  if (cudaMallocArray(&devProj, &projDesc, *nxprj2, *nviews * *nplanes)
      != cudaSuccess) {
    pflerr("Failed to allocate projection array on GPU device");
    cudaFree(devSlice);
    return 1;
  }
  memTot = sizetmp * *thick + 4 * *nxprj2 * *nviews * *nplanes;

  // set texture parameters
  projtex.addressMode[0] = cudaAddressModeClamp;
  projtex.addressMode[1] = cudaAddressModeClamp;
  projtex.filterMode = cudaFilterModeLinear;
  projtex.normalized = false;
  
  // Bind the array to the texture
  if (cudaBindTextureToArray(projtex, devProj, projDesc) != cudaSuccess) {
    pflerr("Failed to bind projection array to texture");
    cudaFree(devSlice);
    cudaFreeArray(devProj);
    return 1;
  }

  if (*nplanes > 1) {
    planeLoaded = (int *)malloc(*nplanes * sizeof(int));
    if (!planeLoaded) {
      pflush("Failed to malloc little array planeLoaded\n");
      gpudone();
      return 1;
    }
  }

  // Get arrays for local proj factors
  if (*numWarps > 0) {
    if (cudaMallocPitch((void **)&xprojf, &pitch1, sizetmp, *nviews) != 
        cudaSuccess ||
        cudaMallocPitch((void **)&xprojz, &pitch2, sizetmp, *nviews) != 
        cudaSuccess ||
        cudaMallocPitch((void **)&yprojf, &pitch3, sizetmp, *nviews) != 
        cudaSuccess ||
        cudaMallocPitch((void **)&yprojz, &pitch4, sizetmp, *nviews) != 
        cudaSuccess  ||
        cudaMallocArray(&localData, &projDesc, *numWarps * *nviews, 12) 
        != cudaSuccess) {
      pflerr("Failed to allocate local factor arrays on GPU device");
      gpudone();
      return 1;
    }
    if (pitch2 != pitch1 || pitch3 != pitch1 || pitch4 != pitch1 || 
        pitch1 != slicePitch) {
      pflush("Array pitches for GPU arrays do NOT match\n");
      gpudone();
      return 1;
    }

    localtex.filterMode = cudaFilterModePoint;
    localtex.normalized = false;
    if (cudaBindTextureToArray(localtex, localData, projDesc) != cudaSuccess) {
      pflerr("Failed to bind local factor arrays to texture");
      gpudone();
      return 1;
    }
    memTot += 4 * sizetmp * *nviews + 48 * *numWarps * *nviews;
  }

  // Get arrays for radial filtering
  if (*numWarps >= 0) {
    sizetmp = *nxprj2 * *nviews * sizeof(float);
    if (cudaMalloc((void **)&devFFT, sizetmp)  != cudaSuccess ||
        cudaMalloc((void **)&radialFilt, sizetmp)  != cudaSuccess) {
      pflerr("Failed to  allocate GPU arrays for radial filtering");
      gpudone();
      return 1;
    }
    memTot += 2 * sizetmp;
  }

  pflush("Allocated %4d MB for arrays on the GPU\n", 
         (memTot + 512*1024)/(1024*1024));
  sliceWidth = *width;
  sliceThick = *thick;
  numViews = *nviews;
  numProjPlanes = *nplanes;
  nxPlane = *nxprj2;
  return 0;
}

// Routine to free all allocated resources
void gpudone()
{
  cudaFree(devSlice);
  cudaFreeArray(devProj);
  cudaFree(xprojf);
  cudaFree(xprojz);
  cudaFree(yprojf);
  cudaFree(yprojz);
  cudaFreeArray(localData);
  cudaFree(devFFT);
  cudaFree(radialFilt);
  if (forwardPlan)
    cufftDestroy(forwardPlan);
  if (inversePlan)
    cufftDestroy(inversePlan);
}

/*
 * ROUTINES FOR LOADING/MAINTAINING STACK OF PLANES ON GPU
 */ 

// Function to shift existing data in preparation for loading new data starting
// in position loadStart (numbered from 1) and with starting slice number
// lsliceStart
int gpushiftproj(int *numPlanes, int *lsliceStart, int *loadStart)
{
  int startm1 = *loadStart - 1;
  int shift, shiftStart, numToShift, todo, dstY, srcY;
  size_t sizetmp = nxPlane * sizeof(float);
  if (startm1 > 0) {
    if (checkProjLoad(numPlanes, lsliceStart, startm1))
      return 1;

    // Copy data down without overlap if it goes into occupied planes
    if (startm1 < numLoadedPlanes) {
      shift = numLoadedPlanes - startm1;
      numToShift = startm1;
      shiftStart = 0;
      while (numToShift > 0) {
        todo = shift;
        if (todo > numToShift)
          todo = numToShift;
        dstY = shiftStart * numViews;
        srcY = dstY + shift * numViews;
        //pflush("Copying down %d\n", todo);
        if (cudaMemcpy2DArrayToArray(devProj, 0, dstY, devProj, 0, srcY,
                                     sizetmp, todo * numViews,
                                     cudaMemcpyDeviceToDevice) != cudaSuccess){
          pflerr("Error copying segment of projection array down");
          numLoadedPlanes = 0;
          return 1;
        }
        numToShift -= todo;
        shiftStart += todo;
      }
    }
  }
  numLoadedPlanes = startm1;
  lsliceFirst = *lsliceStart - startm1;

  /*pflush("Initializing array num %d  first %d  loaded %d\n", numProjPlanes, 
    lsliceFirst, numLoadedPlanes); */
  // Initialize array for keeping track of copied planes, and enable copying
  for (todo = 0; todo < numProjPlanes; todo++)
    planeLoaded[todo] = todo < numLoadedPlanes ? 1 : 0;
  copyFilteredOK = 1;
  return 0;
}

// Function to load numPlanes planes of input data, starting in position
// loadStart (numbered from 1) and with starting slice number lsliceStart
int gpuloadproj(float *lines, int *numPlanes, int *lsliceStart, int *loadStart)
{
  int startm1 = *loadStart - 1;
  int todo, dstY, numCopy = 0;

  if (startm1 > 0 && checkProjLoad(numPlanes, lsliceStart, startm1)) {
    copyFilteredOK = 0;
    return 1;
  }

  // Check for valid load
  if (startm1 + *numPlanes > numProjPlanes) {
    pflush("Trying to load past end of projection array\n");
    copyFilteredOK = 0;
    numLoadedPlanes = 0;
    return 1;
  }
  
  // Find the number to copy by the last plane not already loaded
  if (copyFilteredOK) {
    for (todo = startm1; todo < startm1 + *numPlanes; todo++)
      if (!planeLoaded[todo])
        numCopy = todo + 1 - startm1;
  }
  copyFilteredOK = 0;

  // Finally do the load
  dstY = startm1 * numViews;
  todo = numCopy * numViews * nxPlane * 4;
  //if (numCopy) pflush("Loading %d planes\n", numCopy);
  if (numCopy && cudaMemcpyToArray(devProj, 0, dstY, lines, todo,
                                   cudaMemcpyHostToDevice) != cudaSuccess) {
    pflerr("Failed to copy projection array to device");
    numLoadedPlanes = 0;
    return 1;
  }
  numLoadedPlanes = startm1 + *numPlanes;
  lsliceFirst = *lsliceStart - startm1;
  return 0;
}

// Function to do initial check on parameters in load/shift calls
static int checkProjLoad(int *numPlanes, int *lsliceStart, int startm1)
{
  if (!numLoadedPlanes) {
    pflush("Trying to load into higher planes when none are loaded\n");
    return 1;
  }
  if (lsliceFirst + numLoadedPlanes != *lsliceStart) {
    pflush("Starting slice %d does not match first slice %d + num loaded %d"
            "\n", *lsliceStart, lsliceFirst, numLoadedPlanes);
    numLoadedPlanes = 0;
    return 1;
  }
  if (startm1 > numLoadedPlanes) {
    pflush("Starting plane %d is past number loaded %d\n", startm1+1, 
           numLoadedPlanes);
    numLoadedPlanes = 0;
    return 1;
  }
  return 0;
}

/*
 * ROUTINES FOR RADIAL FILTERING OF INPUT LINES
 */

// Kernel to multiply the FFT by the filter
__global__ void filterFFT(float *FFT, float *filter, int nxprj2, int nviews, 
                          float scale)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;
  if (i < nviews && j < nxprj2)
    FFT[i * nxprj2 + j] *= filter[i * nxprj2 + j] * scale;
}

// Function to load the filter lines into the array and generate plans
int gpuloadfilter(float *lines)
{
  size_t sizetmp = nxPlane * numViews * sizeof(float);
  if (cudaMemcpy(radialFilt, lines, sizetmp, cudaMemcpyHostToDevice) !=
      cudaSuccess) {
    pflerr("Failed to copy radial filters to GPU array");
    gpudone();
    return 1;
  }
  if (cufftPlan1d(&forwardPlan, nxPlane - 2, CUFFT_R2C, numViews) != 
      CUFFT_SUCCESS || cufftPlan1d(&inversePlan, nxPlane - 2, CUFFT_C2R, 
                                   numViews) != CUFFT_SUCCESS) {
    pflush("Failed to generate a plan for CUFFT\n");
    gpudone();
    return 1;
  }
  return 0;
}

// Function to filter the set of input lines
int gpufilterlines(float *lines, int *lslice)
{
  int ind, blockX = 16;
  size_t sizetmp = nxPlane * numViews * sizeof(float);
  float scale = 1.f / (nxPlane - 2);
  cudaError_t err;
  if (cudaMemcpy(devFFT, lines, sizetmp, cudaMemcpyHostToDevice) !=
      cudaSuccess) {
    pflerr("Failed to copy lines to GPU array for radial filtering");
    return 1;
  }
  if (cufftExecR2C(forwardPlan, devFFT, (cufftComplex *)devFFT) != 
      CUFFT_SUCCESS) {
    pflush("Failure in forward FFT on GPU\n");
    return 1;
  }
  
  // Filter!!!
  dim3 blockSize(blockX, 16, 1);
  dim3 gridSize((nxPlane + blockSize.x - 1) / blockSize.x, 
                (numViews + blockSize.y - 1) / blockSize.y, 1);

  filterFFT<<<gridSize, blockSize>>>
    (devFFT, radialFilt, nxPlane, numViews, scale);
  err = cudaGetLastError();
  if (err != cudaSuccess) {
    pflerr("Error executing threads for filtering"); 
    return 1;
  }
  if (cudaThreadSynchronize() != cudaSuccess) {
    pflerr("Error return from synchronizing after filtering");
    return 1;
  }

  if (cufftExecC2R(inversePlan, (cufftComplex *)devFFT, devFFT) != 
      CUFFT_SUCCESS) {
    pflush("Failure in inverse FFT on GPU\n");
    return 1;
  }
  if (cudaMemcpy(lines, devFFT, sizetmp, cudaMemcpyDeviceToHost) !=
      cudaSuccess) {
    pflerr("Failed to copy radial filtered lines back from GPU array");
    return 1;
  }
  
  // If copying is OK and it is a slice in needed range, copy it to proj
  if (copyFilteredOK) {
    ind = *lslice - lsliceFirst;
    if (ind >= 0 && ind < numProjPlanes) {
      //pflush("Copying %d to plane %d\n", *lslice,ind);
      if (cudaMemcpyToArray(devProj, 0, ind * numViews, devFFT, sizetmp,
                            cudaMemcpyDeviceToDevice) == cudaSuccess)
        planeLoaded[ind] = 1;
    }
  }
  return 0;
}

/*
 * ROUTINES FOR SIMPLE BACK-PROJECTION (NO X-AXIS TILT, ETC)
 */

// Kernel for simple back-projection with testing at ends of lines
__global__ void bpNoXtTest(float *slice, int pitch, int jbase, int iwide,
                             int nxprj, int ithick, int nviews, 
                             float xcenin, float xcen, float ycen, 
                             float edgefill)
{
  float cbeta, sbeta, zpart, kproj, xp;
  float sum = 0.;
  int iv;
  int j = blockIdx.x * blockDim.x + threadIdx.x + jbase;
  int i = blockIdx.y * blockDim.y + threadIdx.y;
  if (j < iwide && i < ithick) {
    for (iv = 0; iv < nviews; iv++) {
      cbeta = tables[iv+COSOFS];
      sbeta = tables[iv+SINOFS];
      zpart = (i + 1 - ycen) * sbeta + xcenin;
      xp =  zpart + (j + 1 - xcen) * cbeta - 0.5f;
      //if (i == 150) printf("%d %d %d  %d  %d  %.2f  %.2f\n", iv, j, i, jlft, jrt, zpart + (1 - xcen) * cbeta - 0.5f, zpart + (nxprj - xcen) * cbeta - 0.5f);
      if (xp >= 0.5 && xp <= nxprj - 0.5) {
        kproj = iv + 0.5f;
        sum += tex2D(projtex, xp, kproj);
      } else {
        sum += edgefill;
      }
    }
    slice[i * pitch + j] = sum;
  }
}

// Kernel for simple back-projection with no testing
__global__ void bpNoXtFast(float *slice, int pitch, int jbase, int iwide,
                             int ithick, int nviews, 
                             float xcenin, float xcen, float ycen)
{
  float cbeta, sbeta, zpart, kproj, xp;
  float sum = 0.;
  int iv;
  int j = blockIdx.x * blockDim.x + threadIdx.x + jbase;
  int i = blockIdx.y * blockDim.y + threadIdx.y;
  if (i < ithick) {
    for (iv = 0; iv < nviews; iv++) {
      cbeta = tables[iv+COSOFS];
      sbeta = tables[iv+SINOFS];
      zpart = (i + 1 - ycen) * sbeta + xcenin;
      kproj = iv + 0.5f;
      xp =  zpart + (j + 1 - xcen) * cbeta - 0.5f;
      sum += tex2D(projtex, xp, kproj);
    }
    slice[i * pitch + j] = sum;
  }
}

// Function to run simple backprojection
int gpubpnox(float *slice, float *lines, float *sbeta, float *cbeta,
             int *nxprj, float *xcenin, float *xcen, float *ycen,
             float *edgefill)
{
  size_t sizetmp = sizeof(float) * nxPlane * numViews;
  float cosinv[DELTA_OFS];
  int iv, jlft, jrt, jlftmax, jrtmin, gridLeft, gridFast, gridRight;
  float zpart, xlft, xrt, xlfttmp, xrttmp;
  int i, blockX = 16;

  if (loadBetaInvertCos(cbeta, sbeta, cosinv))
    return 1;

  // Copy projections
  if (cudaMemcpyToArray(devProj, 0, 0, lines, sizetmp, cudaMemcpyHostToDevice)
      != cudaSuccess) {
    pflerr("Failed to copy projection array to device");
    return 1;
  }

  // Find limits of region that needs no testing
  jlftmax = 1;
  jrtmin = sliceWidth;
  for (iv = 0; iv < numViews; iv++) {
    for (i = 0; i <= sliceThick - 1; i += sliceThick - 1) {
      zpart = (i + 1 - *ycen) * sbeta[iv] + *xcenin;
      xlfttmp = (1. - zpart) * cosinv[iv] + *xcen;
      xrttmp = (*nxprj - zpart) * cosinv[iv] + *xcen;
      xlft = fmin(xlfttmp, xrttmp);
      xrt = fmax(xlfttmp, xrttmp);
      jlft = (int)ceilf(xlft);
      jrt = (int)ceilf(xrt) - 1;
      jlftmax = max(jlftmax, jlft);
      jrtmin = min(jrtmin, jrt);
      //printf("%d %f %d %.2f %d  %d  %.2f  %.2f\n", iv, cbet, i, zpart, jlft, jrt, zpart + (1 - *xcen) * cbet - 0.5f, zpart + (*nxprj - *xcen) * cbet - 0.5f);
    }
  }

  // Figure out grid sizes for left test, fast, and right test regions
  dim3 blockSize(blockX, 16, 1);
  dim3 gridSize((sliceWidth + blockSize.x - 1) / blockSize.x, 
                (sliceThick + blockSize.y - 1) / blockSize.y, 1);

  gridLeft = (jlftmax - 1 + blockX - 1) / blockX;
  gridFast = jrtmin / blockX - gridLeft;
  if (gridFast <= 0) {
    gridLeft = gridSize.x;
    gridRight = 0;
  } else
    gridRight = gridSize.x - (gridFast + gridLeft);

  if (gridLeft > 0) {
    gridSize.x = gridLeft;
    bpNoXtTest<<<gridSize, blockSize>>>
      (devSlice, slicePitch / 4, 0, sliceWidth, *nxprj, sliceThick, 
       numViews, *xcenin, *xcen, *ycen, *edgefill);
    if (testReportErr("in left test region of backprojection"))
      return 1;
  }

  if (gridFast > 0) {
    gridSize.x = gridFast;
    bpNoXtFast<<<gridSize, blockSize>>>
      (devSlice, slicePitch / 4, blockX * gridLeft, sliceWidth,
       sliceThick, numViews, *xcenin, *xcen, *ycen);
    if (testReportErr("in no-test region of backprojection"))
      return 1;
  }

  if (gridRight > 0) {
    gridSize.x = gridRight;
    bpNoXtTest<<<gridSize, blockSize>>>
      (devSlice, slicePitch / 4, blockX * (gridLeft + gridFast), sliceWidth, 
       *nxprj, sliceThick, numViews, *xcenin, *xcen, *ycen, *
       edgefill);
    if (testReportErr("in right test region of backprojection"))
      return 1;
  }

  return (synchronizeCopySlice(slice, sliceThick));
    
}

/*
 * ROUTINES FOR BACK-PROJECTION WITH X AXIS TILT AND/OR Z FACTORS
 */

// Kernel for BP with X-axis tilt/Z-factors and testing at ends of lines
__global__ void bpXtiltTest(float *slice, int pitch, int jbase, int iwide,
                            int nxprj, int nyprj, int ithick, int nviews, 
                            float xcenin, float xcen, float ycen, float yy,
                            float slicen, int lsliceBase, float edgefill)
{
  float cbeta, sbeta, zpart, kproj, xp, zz, calpha, salpha, fj, yproj, xx;
  float sum = 0.;
  int iv, jproj;
  int j = blockIdx.x * blockDim.x + threadIdx.x + jbase;
  int i = blockIdx.y * blockDim.y + threadIdx.y;
  if (j < iwide && i < ithick) {
    zz = (i + 1 - ycen);
    xx = (j + 1 - xcen);
    for (iv = 0; iv < nviews; iv++) {
      cbeta = tables[iv+COSOFS];
      sbeta = tables[iv+SINOFS];
      salpha = tables[iv+SALOFS];
      calpha = tables[iv+CALOFS];
      zpart = yy * salpha * sbeta + 
        zz * (calpha * sbeta + tables[iv+XZFOFS]) + xcenin;
      yproj = yy * calpha - zz * (salpha - tables[iv+YZFOFS]) + slicen;
      xp =  zpart + xx * cbeta - 0.5f;
      if (yproj >= 1. && yproj <= nyprj && xp >= 0.5 && xp < nxprj - 0.5) {
        jproj = min((int)yproj, nyprj - 1);
        fj = yproj - jproj;
        kproj = (jproj - lsliceBase) * nviews + iv + 0.5f;
        sum += (1.f - fj) * tex2D(projtex, xp, kproj) + 
          fj * tex2D(projtex, xp, kproj + nviews);
      } else {
        sum += edgefill;
      }
    }
    slice[i * pitch + j] = sum;
  }
}

// Kernel for BP with X-axis tilt/Z-factors and no testing 
__global__ void bpXtiltFast(float *slice, int pitch, int jbase, int iwide,
                            int ithick, int nviews, float xcenin, float xcen, 
                            float ycen, float yy, float slicen, int lsliceBase)
{
  float cbeta, sbeta, zpart, kproj, xp, zz, calpha, salpha, fj, yproj, xx;
  float sum = 0.;
  int iv, jproj;
  int j = blockIdx.x * blockDim.x + threadIdx.x + jbase;
  int i = blockIdx.y * blockDim.y + threadIdx.y;
  if (i < ithick) {
    zz = (i + 1 - ycen);
    xx = (j + 1 - xcen);
    for (iv = 0; iv < nviews; iv++) {
      cbeta = tables[iv+COSOFS];
      sbeta = tables[iv+SINOFS];
      salpha = tables[iv+SALOFS];
      calpha = tables[iv+CALOFS];
      zpart = yy * salpha * sbeta + 
        zz * (calpha * sbeta + tables[iv+XZFOFS]) + xcenin;
      yproj = yy * calpha - zz * (salpha - tables[iv+YZFOFS]) + slicen;
      jproj = (int)yproj;
      fj = yproj - jproj;
      kproj = (jproj - lsliceBase) * nviews + iv + 0.5f;
      xp =  zpart + xx * cbeta - 0.5f;
      sum += (1.f - fj) * tex2D(projtex, xp, kproj) + 
        fj * tex2D(projtex, xp, kproj + nviews);
    }
    slice[i * pitch + j] = sum;
  }
}

// Function to run back-projection with X-axis tilt/Z-factors
int gpubpxtilt(float *slice, float *sbeta, float *cbeta, 
               float *salpha, float *calpha, float *xzfac, float *yzfac,
               int *nxprj, int *nyprj, float *xcenin, float *xcen, float *ycen,
               int *lslice, float *slicen, float *edgefill)
{
  int iv, jlft, jrt, jlftmax, jrtmin, gridLeft, gridFast, gridRight;
  float zpart, xlft, xrt, xlfttmp, xrttmp, yy, zz, yproj;
  int i, ytest, blockX = 16;
  float cosinv[DELTA_OFS];

  if (loadBetaInvertCos(cbeta, sbeta, cosinv))
    return 1;

  // Copy alphas and z factors
  iv = numViews * sizeof(float);
  if (cudaMemcpyToSymbol(tables, calpha, iv, CALOFS*4, cudaMemcpyHostToDevice)
      || cudaMemcpyToSymbol(tables, salpha, iv, SALOFS*4,
                            cudaMemcpyHostToDevice) ||
      cudaMemcpyToSymbol(tables, xzfac, iv, XZFOFS*4, cudaMemcpyHostToDevice)
      || cudaMemcpyToSymbol(tables, yzfac, iv, YZFOFS*4,
                            cudaMemcpyHostToDevice)) {
    pflerr("Failed to copy constant data to GPU");
    return 1;
  }

  // Find limits of region that needs no testing.  Test every angle top & bot
  jlftmax = 1;
  jrtmin = sliceWidth;
  yy = *lslice - *slicen;
  ytest = 0;
  for (iv = 0; iv < numViews; iv++) {
    for (i = 0; i <= sliceThick - 1; i += sliceThick - 1) {
      zz = (i + 1 - *ycen);
      zpart = yy * salpha[iv] * sbeta[iv] + zz * (calpha[iv] * sbeta[iv] +
                                                  xzfac[iv]) + *xcenin;
      yproj = yy * calpha[iv] - zz * (salpha[iv] - yzfac[iv]) + *slicen;
      if (yproj < 1 || yproj > *nyprj - 1)
        ytest = 1;
      xlfttmp = (1. - zpart) * cosinv[iv] + *xcen;
      xrttmp = (*nxprj - zpart) * cosinv[iv] + *xcen;
      xlft = fmin(xlfttmp, xrttmp);
      xrt = fmax(xlfttmp, xrttmp);
      jlft = (int)ceilf(xlft);
      jrt = (int)ceilf(xrt) - 1;
      jlftmax = max(jlftmax, jlft);
      jrtmin = min(jrtmin, jrt);
      //printf("%d %f %d %.2f %d  %d  %.2f  %.2f\n", iv, cbet, i, zpart, jlft, jrt, zpart + (1 - *xcen) * cbet - 0.5f, zpart + (*nxprj - *xcen) * cbet - 0.5f);
    }
  }

  // Figure out grid sizes for left test, fast, and right test regions
  dim3 blockSize(blockX, 16, 1);
  dim3 gridSize((sliceWidth + blockSize.x - 1) / blockSize.x, 
                (sliceThick + blockSize.y - 1) / blockSize.y, 1);

  gridLeft = (jlftmax - 1 + blockX - 1) / blockX;
  gridFast = jrtmin / blockX - gridLeft;
  if (gridFast <= 0 || ytest) {
    gridLeft = gridSize.x;
    gridRight = 0;
    gridFast = 0;
  } else
    gridRight = gridSize.x - (gridFast + gridLeft);

  if (gridLeft > 0) {
    gridSize.x = gridLeft;
    bpXtiltTest<<<gridSize, blockSize>>>
      (devSlice, slicePitch / 4, 0, sliceWidth, *nxprj, *nyprj, sliceThick, 
       numViews, *xcenin, *xcen, *ycen, yy, *slicen, lsliceFirst, *edgefill);
    if (testReportErr("in left test region of backprojection"))
      return 1;
  }

  if (gridFast > 0) {
    gridSize.x = gridFast;
    bpXtiltFast<<<gridSize, blockSize>>>
      (devSlice, slicePitch / 4, blockX * gridLeft, sliceWidth, 
       sliceThick, numViews, *xcenin, *xcen, *ycen, yy, *slicen, lsliceFirst);
    if (testReportErr("in no-test region of backprojection"))
      return 1;
  }

  if (gridRight > 0) {
    gridSize.x = gridRight;
    bpXtiltTest<<<gridSize, blockSize>>>
      (devSlice, slicePitch / 4, blockX * (gridLeft + gridFast), sliceWidth, 
       *nxprj, *nyprj, sliceThick, 
       numViews, *xcenin, *xcen, *ycen, yy, *slicen, lsliceFirst, *edgefill);
    if (testReportErr("in right test region of backprojection"))
      return 1;
  }

  return (synchronizeCopySlice(slice, sliceThick));
}

/*
 * ROUTINES FOR BACK-PROJECTION WITH LOCAL ALIGNMENTS
 */

// Kernel for back-projection using local projection factors, testing as needed
__global__ void bpLocalTest(float *slice, float *xprojf, float *xprojz, 
                            float *yprojf, float *yprojz, int pitch, int iwide,
                            int nxprj, int lsliceLast, int ithick, int nviews,
                            float ycen, int lsliceBase, float edgeFill)
{
  float kproj, xp, zz, fj, yproj;
  float sum = 0.;
  int iv, jproj, ind;
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;
  if (i < ithick && j < iwide) {
    zz = (i + 1 - ycen);
    for (iv = 0; iv < nviews; iv++) {
      ind = iv * pitch + j;
      xp = xprojf[ind] + zz * xprojz[ind] - 0.5f;
      yproj = yprojf[ind] + zz * yprojz[ind];
      if (yproj >= lsliceBase && yproj <= lsliceLast && xp >= 0.5 && 
          xp < nxprj - 0.5) {
        jproj = min((int)yproj, lsliceLast - 1);
        fj = yproj - jproj;
        kproj = (jproj - lsliceBase) * nviews + iv + 0.5f;
        sum += (1.f - fj) * tex2D(projtex, xp, kproj) + 
          fj * tex2D(projtex, xp, kproj + nviews);
      } else {
        sum += edgeFill;
      }
    }
    slice[i * pitch + j] = sum;
  }
}

// Kernel for computing the local projection factors from warping data
__global__ void localProjFactors
(float *xprjf, float *xprjz, float *yprjf, float *yprjz, int pitch, int nviews,
 int iwide, int lslice, int nxwarp, int nywarp, int ixswarp, int iyswarp,
 int idxwarp, int idywarp, float xcen, float xcenin, float xcenPdelxx,
 float slicen)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int iv = blockIdx.y * blockDim.y + threadIdx.y;
  int ind1,ind2,ind3,ind4,ixc,ixt,ixpos,iyt,iypos;
  float fnd1,fnd2,fnd3,fnd4,yzf1,yzf2,yzf3,yzf4;
  float f1,f2,f3,f4,xx,yy,fx,fy;
  float calf,salf,a11,a12,a21,a22,xadd,yadd,xalladd,yalladd;
  float calf2,salf2,a112,a122,a212,a222,xadd2,yadd2;
  float calf3,salf3,a113,a123,a213,a223,xadd3,yadd3;
  float calf4,salf4,a114,a124,a214,a224,xadd4,yadd4;
  float f1x,f2x,f3x,f4x,f1xy,f2xy,f3xy,f4xy;
  float f1y,f2y,f3y,f4y,f1yy,f2yy,f3yy,f4yy;
  float xp1f,xp1z,yp1f,xp2f,xp2z,yp2f,xp3f,xp3z,yp3f,xp4f,xp4z,yp4f;
  float cbeta,sbeta,cbeta2,sbeta2,cbeta3,sbeta3,cbeta4,sbeta4;

  if (j >= iwide || iv >= nviews)
    return;

  // Need to add 1 to j when it is used as a position
  ixc=(int)floor(j-xcen+xcenPdelxx+1.5f);
  ixt=min(max(ixc-ixswarp,0),(nxwarp-1)*idxwarp);
  ixpos=min(ixt/idxwarp+1,nxwarp-1);
  fx=((float)(ixt-(ixpos-1)*idxwarp))/idxwarp;
  iyt=min(max(lslice-iyswarp,0),(nywarp-1)*idywarp);
  iypos=min(iyt/idywarp+1,nywarp-1);
  fy=((float)(iyt-(iypos-1)*idywarp))/idywarp;

  ind1=(nxwarp*(iypos-1)+ixpos-1)*nviews+iv;
  ind2=ind1+nviews;
  ind3=ind1+nxwarp*nviews;
  ind4=ind3+nviews;
  f1=(1.-fy)*(1.-fx);
  f2=(1.-fy)*fx;
  f3=fy*(1.-fx);
  f4=fy*fx;
  fnd1 = ind1;
  fnd2 = ind2;
  fnd3 = ind3;
  fnd4 = ind4;
  
  cbeta=tex2D(localtex,fnd1,CBIND);
  sbeta=tex2D(localtex,fnd1,SBIND);
  calf=tex2D(localtex,fnd1,CAIND);
  salf=tex2D(localtex,fnd1,SAIND);
  a11=tex2D(localtex,fnd1,F11IND);
  a12=tex2D(localtex,fnd1,F12IND);
  a21=tex2D(localtex,fnd1,F21IND);
  a22=tex2D(localtex,fnd1,F22IND);
  xadd=tex2D(localtex,fnd1,F13IND)+xcenin-xcenin*a11-slicen*a12;
  yadd=tex2D(localtex,fnd1,F23IND)+slicen-xcenin*a21-slicen*a22;

  cbeta2=tex2D(localtex,fnd2,CBIND);
  sbeta2=tex2D(localtex,fnd2,SBIND);
  calf2=tex2D(localtex,fnd2,CAIND);
  salf2=tex2D(localtex,fnd2,SAIND);
  a112=tex2D(localtex,fnd2,F11IND);
  a122=tex2D(localtex,fnd2,F12IND);
  a212=tex2D(localtex,fnd2,F21IND);
  a222=tex2D(localtex,fnd2,F22IND);
  xadd2=tex2D(localtex,fnd2,F13IND)+xcenin-xcenin*a112-slicen*a122;
  yadd2=tex2D(localtex,fnd2,F23IND)+slicen-xcenin*a212-slicen*a222;

  cbeta3=tex2D(localtex,fnd3,CBIND);
  sbeta3=tex2D(localtex,fnd3,SBIND);
  calf3=tex2D(localtex,fnd3,CAIND);
  salf3=tex2D(localtex,fnd3,SAIND);
  a113=tex2D(localtex,fnd3,F11IND);
  a123=tex2D(localtex,fnd3,F12IND);
  a213=tex2D(localtex,fnd3,F21IND);
  a223=tex2D(localtex,fnd3,F22IND);
  xadd3=tex2D(localtex,fnd3,F13IND)+xcenin-xcenin*a113-slicen*a123;
  yadd3=tex2D(localtex,fnd3,F23IND)+slicen-xcenin*a213-slicen*a223;

  cbeta4=tex2D(localtex,fnd4,CBIND);
  sbeta4=tex2D(localtex,fnd4,SBIND);
  calf4=tex2D(localtex,fnd4,CAIND);
  salf4=tex2D(localtex,fnd4,SAIND);
  a114=tex2D(localtex,fnd4,F11IND);
  a124=tex2D(localtex,fnd4,F12IND);
  a214=tex2D(localtex,fnd4,F21IND);
  a224=tex2D(localtex,fnd4,F22IND);
  xadd4=tex2D(localtex,fnd4,F13IND)+xcenin-xcenin*a114-slicen*a124;
  yadd4=tex2D(localtex,fnd4,F23IND)+slicen-xcenin*a214-slicen*a224;
       
  f1x=f1*a11;
  f2x=f2*a112;
  f3x=f3*a113;
  f4x=f4*a114;
  f1xy=f1*a12;
  f2xy=f2*a122;
  f3xy=f3*a123;
  f4xy=f4*a124;

  f1y=f1*a21;
  f2y=f2*a212;
  f3y=f3*a213;
  f4y=f4*a214;
  f1yy=f1*a22;
  f2yy=f2*a222;
  f3yy=f3*a223;
  f4yy=f4*a224;

  xalladd=f1*xadd+f2*xadd2+f3*xadd3+f4*xadd4;
  yalladd=f1*yadd+f2*yadd2+f3*yadd3+f4*yadd4;
       
  // Each projection position is a sum of a fixed factor ("..f")
  // and a factor that multiplies z ("..z")
   
  xx=j+1.-xcen;
  yy=lslice-slicen;
  xp1f=xx*cbeta + yy*salf*sbeta + xcenPdelxx;
  xp1z=calf*sbeta + tex2D(localtex,fnd1,XZFIND);
  xp2f=xx*cbeta2 + yy*salf2*sbeta2 + xcenPdelxx;
  xp2z=calf2*sbeta2 + tex2D(localtex,fnd2,XZFIND);
  xp3f=xx*cbeta3 + yy*salf3*sbeta3 + xcenPdelxx;
  xp3z=calf3*sbeta3 + tex2D(localtex,fnd3,XZFIND);
  xp4f=xx*cbeta4 + yy*salf4*sbeta4 + xcenPdelxx;
  xp4z=calf4*sbeta4 + tex2D(localtex,fnd4,XZFIND);

  yp1f=yy*calf + slicen;
  yp2f=yy*calf2 + slicen;
  yp3f=yy*calf3 + slicen;
  yp4f=yy*calf4 + slicen;

  // store the fixed and z-dependent component of the
  // projection coordinates
  yzf1 = tex2D(localtex,fnd1,YZFIND);
  yzf2 = tex2D(localtex,fnd2,YZFIND);
  yzf3 = tex2D(localtex,fnd3,YZFIND);
  yzf4 = tex2D(localtex,fnd4,YZFIND);
  ind1 = pitch * iv + j;
  xprjf[ind1] =f1x*xp1f+f2x*xp2f+f3x*xp3f+f4x*xp4f+
    f1xy*yp1f+f2xy*yp2f+f3xy*yp3f+f4xy*yp4f+xalladd;
  xprjz[ind1] =f1x*xp1z+f2x*xp2z+f3x*xp3z+f4x*xp4z-
    (f1xy*(salf-yzf1)+f2xy*(salf2-yzf2)+ f3xy*(salf3-yzf3)+f4xy*(salf4-yzf4));
  yprjf[ind1] =f1y*xp1f+f2y*xp2f+f3y*xp3f+f4y*xp4f+
    f1yy*yp1f+f2yy*yp2f+f3yy*yp3f+f4yy*yp4f+yalladd;
  yprjz[ind1] =f1y*xp1z+f2y*xp2z+f3y*xp3z+f4y*xp4z-
    (f1yy*(salf-yzf1)+f2yy*(salf2-yzf2)+ f3yy*(salf3-yzf3)+f4yy*(salf4-yzf4));
}

// Function to load the local alignment data
int gpuloadlocals(float *packed, int *numWarps)
{
  size_t sizetmp = sizeof(float) * *numWarps * numViews * 12;
  if (cudaMemcpyToArray(localData, 0, 0, packed, sizetmp,
                        cudaMemcpyHostToDevice) != cudaSuccess) {
    pflerr("Failed to copy local data to GPU array");
    gpudone();
    return 1;
  }
  return 0;
}

// Function to run back-projection with local alignments, first computing the
// the projection factors for all positions and views, then running the 
// back projection kernel
int gpubplocal(float *slice, int *lslice, int *nxwarp, int *nywarp,
               int *ixswarp, int *iyswarp, int *idxwarp, int *idywarp,
               int *nxprj, float *xcen, float *xcenin, float *delxx,
               float *ycen, float *slicen, float *edgefill)
{
  int blockX = 16;

  // Compute the local projection factors
  dim3 blockFac(blockX, 16, 1);
  dim3 gridFac((sliceWidth + blockFac.x - 1) / blockFac.x, 
                (numViews + blockFac.y - 1) / blockFac.y, 1);
  localProjFactors<<<gridFac, blockFac>>>
    (xprojf, xprojz, yprojf, yprojz, slicePitch / 4, numViews, sliceWidth, 
     *lslice, *nxwarp, *nywarp, *ixswarp, *iyswarp, *idxwarp, *idywarp, *xcen,
     *xcenin, *xcenin+*delxx, *slicen);
  if (testReportErr("computing localProjFactors"))
      return 1;

  if (cudaThreadSynchronize() != cudaSuccess) {
    pflerr("Error return from synchronizing after computing local factors");
    return 1;
  }


  // Do the backprojection
  dim3 blockSize(blockX, 16, 1);
  dim3 gridSize((sliceWidth + blockSize.x - 1) / blockSize.x, 
                (sliceThick + blockSize.y - 1) / blockSize.y, 1);

  bpLocalTest<<<gridSize, blockSize>>>
    (devSlice, xprojf, xprojz, yprojf, yprojz, slicePitch / 4, sliceWidth,
     *nxprj, lsliceFirst + numLoadedPlanes - 1, 
     sliceThick, numViews, *ycen, lsliceFirst, *edgefill);
  if (testReportErr("for local backprojection"))
      return 1;

  return (synchronizeCopySlice(slice, sliceThick));
}

/*
 * ROUTINES FOR REPROJECTION
 */

// Kernel to do simple reprojection (no X axis tilt or Z factors)
__global__ void reprojNox(float *lines, int pitch, int iwide, int ithick, 
                          int lsStart, int lsEnd, int lsliceBase, 
                          float xxlim, float xcenAdj, float xcenPdelxx,
                          float xprjOffset, float ycenAdj, float sbeta,
                          float cbetinv, float delz, int numz, float pmean)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;
  int line, kz;
  float zz, sum, frac, zslice, xproj, xx;
  line = i + lsStart;
  sum = 0.;
  if (j >= iwide || line > lsEnd)
    return;
  for (kz = 0; kz < numz; kz++) {
    zz = 1 + kz * delz;
    frac = 1.;
    if (zz > ithick) {
      frac = 1. - (zz - (int)zz);
      zz = ithick;
    }
    zslice = zz - 0.5f;
    zz -= ycenAdj;
    xproj = j + 1 + xprjOffset;

    // the usual -0.5 is incorporated into xcenAdj
    xx = (xproj - (zz  * sbeta + xcenPdelxx)) * cbetinv + xcenAdj;
    if (xx < 0.5f || xx > xxlim) {
      sum += frac * pmean;
    } else {
      zslice += (line - lsliceBase) * ithick;
      sum += frac * tex2D(projtex, xx, zslice);
    }
  }
  lines[pitch * i + j] = sum;
}

// Kernel to do reprojection with X axis tilt and/or Z factors
__global__ void reprojXtilt(float *lines, int pitch, int iwide, int ithick, 
                            int lsStart, int lsEnd, int lsliceBase,
                            int lsliceLast, float xxlim, float xcenAdj,
                            float xcenPdelxx, float xprjOffset, float slicen,
                            float yprjOffset, float ycenAdj, float cbetinv,
                            float calfinv, float salfmyz, float salfsbet,
                            float calsbetpxz, float delz, int numz,
                            float pmean)
{
  int j = blockIdx.x * blockDim.x + threadIdx.x;
  int i = blockIdx.y * blockDim.y + threadIdx.y;
  int line, kz,iys;
  float zz, sum, frac, zslice, yproj, yy, yslice, xproj, xx, fy;
  float ytol = 0.05f;
  line = i + lsStart;
  sum = 0.;
  if (j >= iwide || line > lsEnd)
    return;
  for (kz = 0; kz < numz; kz++) {
    zz = 1 + kz * delz;
    frac = 1.;
    if (zz > ithick) {
      frac = 1. - (zz - (int)zz);
      zz = ithick;
    }
    zslice = zz - 0.5f;
    zz -= ycenAdj;
    yproj = line + yprjOffset;
    yy = (yproj + zz * salfmyz - slicen) * calfinv;
    yslice = yy + slicen - yprjOffset;
    xproj = j + 1 + xprjOffset;

    // the usual -0.5 is incorporated into xcenAdj
    xx = (xproj - (yy * salfsbet + zz * calsbetpxz + xcenPdelxx)) * cbetinv +
      xcenAdj;
    if (xx < 0.5f || xx > xxlim || yslice < lsliceBase - ytol ||
        yslice > lsliceLast + ytol) {
      sum += frac * pmean;
    } else {
      iys = (int)yslice;
      if (iys < lsliceBase) {
        iys = lsliceBase;
        fy = 0.;
      } else if (iys >= lsliceLast) {
        iys = lsliceLast - 1;
        fy = 1.;
      } else {
        fy = yslice - iys;
      }
      zslice += (iys - lsliceBase) * ithick;
      sum += frac * ((1. - fy) * tex2D(projtex, xx, zslice) + 
                     fy * tex2D(projtex, xx, zslice + ithick));
    }
  }
  lines[pitch * i + j] = sum;
}

// Function to run reprojection for all cases except local alignments
int gpureproject(float *lines, float *sbeta, float *cbeta, float *salpha, 
                 float *calpha, float *xzfac, float *yzfac, float *delz,
                 int *lsStart, int *lsEnd, int *ithick,
                 float *xcen, float *xcenPdelxx, int *minXreproj, 
                 float *xprjOffset, float *ycen, int *minYreproj,
                 float *yprjOffset, float *slicen, int *ifalpha, float *pmean)
{ 
  int blockX = 16;
  int numz, numLines = *lsEnd + 1 - *lsStart;
  int lastSlice = lsliceFirst + numLoadedPlanes - 1;
  float znum, xcenAdj, salfsbet, calsbetpxz, ycenAdj, salfmyz, cbetinv,calfinv;
  znum = 1. + (*ithick - 1) / *delz;
  numz = znum;
  if (znum - numz > 0.1)
    numz++;
  xcenAdj = *xcen - (*minXreproj-1) - 0.5;
  salfsbet = *salpha * *sbeta;
  calsbetpxz = *calpha * *sbeta + *xzfac;
  ycenAdj = *ycen + 1 - *minYreproj;
  salfmyz = *salpha - *yzfac;
  cbetinv = 1. / *cbeta;
  calfinv = 1. / *calpha;

  dim3 blockSize(blockX, 16, 1);
  dim3 gridSize((sliceWidth + blockSize.x - 1) / blockSize.x, 
                (numLines + blockSize.y - 1) / blockSize.y, 1);

  if (*ifalpha) {
    reprojXtilt<<<gridSize, blockSize>>>
      (devSlice, slicePitch / 4, sliceWidth, *ithick, *lsStart, *lsEnd, 
       lsliceFirst, lastSlice, nxPlane - 0.5, xcenAdj, *xcenPdelxx,
       *xprjOffset, *slicen, *yprjOffset, ycenAdj, cbetinv, calfinv, salfmyz,
       salfsbet, calsbetpxz, *delz, numz, *pmean);
  } else {
    reprojNox<<<gridSize, blockSize>>>
      (devSlice, slicePitch / 4, sliceWidth, *ithick, *lsStart, *lsEnd, 
       lsliceFirst, nxPlane - 0.5, xcenAdj, *xcenPdelxx, *xprjOffset,
       ycenAdj, *sbeta, cbetinv, *delz, numz, *pmean);
  }
  if (testReportErr("for reprojection"))
    return 1;
  return (synchronizeCopySlice(lines, numLines));
}


/*
 * UTILITY ROUTINES
 */
   
// Load cosine and sine beta into constant array and compute inverse cosine
static int loadBetaInvertCos(float *cbeta, float *sbeta, float *cosinv)
{
  int i, iv;
  float yy;

  // Invert cosines with limit
  for (i = 0; i < numViews; i++) {
    yy = cbeta[i];
    if (fabs(yy) < 0.001f)
      yy = yy >= 0 ? 0.001f : -0.001f;
    cosinv[i] = 1.f / yy;
  }

  // Copy sines/cosines
  iv = numViews * sizeof(float);
  if (cudaMemcpyToSymbol(tables, cbeta, iv, 0, cudaMemcpyHostToDevice) ||
      cudaMemcpyToSymbol(tables, sbeta, iv, SINOFS*4,
                            cudaMemcpyHostToDevice)) {
    pflerr("Failed to copy constant data to GPU");
    return 1;
  }
  return 0;
}

// Synchronize the threads and copy computed data back to caller's array
static int synchronizeCopySlice(float *slice, int numLines)
{
  int sizetmp;
  if (cudaThreadSynchronize() != cudaSuccess) {
    pflerr("Error return from synchronizing after backprojection");
    return 1;
  }

  // Get slice back
  sizetmp = sizeof(float) * sliceWidth;
  if (cudaMemcpy2D(slice, sizetmp, devSlice, slicePitch, sizetmp, numLines, 
                   cudaMemcpyDeviceToHost) != cudaSuccess) {
    pflerr("Error copying slice back to host");
    return 1;
  }
  return 0;
}

// Test for and report error after executing threads           
static int testReportErr(char *mess)
{
  cudaError_t err;
  err = cudaGetLastError();
  if (err != cudaSuccess) {
    pflush("Error executing threads %s: %s\n", mess,
           cudaGetErrorString(err));
    return 1;
  }
  return 0;
}

// Print a message with flushes to get it out before fortran output
static void pflush(const char *format, ...)
{
  char errorMess[512];
  va_list args;
  va_start(args, format);
  vsprintf(errorMess, format, args);
  printf(errorMess);
  fflush(stdout);  
  fflush(stdout);
  va_end(args);
}

// In case of error, find the error string and print it with message
static void pflerr(const char *format, ...)
{
  cudaError_t err;
  char errorMess[512];
  va_list args;
  va_start(args, format);
  vsprintf(errorMess, format, args);
  printf(errorMess);
  err = cudaGetLastError();
  pflush(": %s\n", cudaGetErrorString(err));
  fflush(stdout);  
  fflush(stdout);
  va_end(args);
}

/*

$Log$

*/
