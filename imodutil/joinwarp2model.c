/*
 *
 joinwarp2model.c -- Convert warping control points to nucleus of refining model
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"
#include "b3dutil.h"
#include "parse_params.h"
#include "warpfiles.h"

#define MAX_CHUNKS 1000

int main( int argc, char *argv[])
{
  char *warpName;
  char *outName;
  char *joinName = NULL;
  char *xformName;
  char *origName;
  char *xfptsName;
  char *command;
  char *progname = imodProgName(argv[0]);
  Imod *origMod;
  Imod *xfptsMod;
  FILE *fout, *fin;
  Icont *cont;
  Iobj *obj;
  MrcHeader hdata;
  Ipoint *xfpt;
  int binning = 1;
  int xOffset = 0, yOffset = 0;
  float linearXF[6], inverseXF[6];
  float joinPixel, xformPixel, warpPixel, ydum, zdum, xback, yback;
  int xJoinSize, yJoinSize;
  int chunkSizes[MAX_CHUNKS];
  int nxWarp, nyWarp, nzWarp, version, warpFlags, nxXform, nyXform, nzXform, numOptArgs;
  int ierr, numChunks, ifPixel, ifSize, iz, maxControl, numControl, outlen, numNonOptArgs;
  int co, pt;
  float *xControl, *yControl, *xVector, *yVector;
  const char *noWarpTxt = "There are no warping transforms; no warp point model produced";

  /* Fallbacks from    ../manpages/autodoc2man 2 1 joinwarp2model */
  int numOptions = 9;
  const char *options[] = {
    "input:InputWarpFile:FN:", "output:OutputModelFile:FN:", "joined:JoinedFile:FN:",
    "xform:AppliedTransformFile:FN:", "size:SizeOfJoinInXandY:IP:",
    "pixel:PixelSpacing:F:", "offset:OffsetInXandY:IP:", "binning:BinningOfJoin:I:",
    "chunks:ChunkSizes:IA:"};
  
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        5, 1, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  if (PipGetInOutFile("InputWarpFile", 0, &warpName))
    exitError("No input warping file specified");
  if (PipGetInOutFile("OutputModelFile", 1, &outName))
    exitError("No output model file name specified");
  if (PipGetString("AppliedTransformFile", &xformName))
    exitError("No file of applied warping G transforms specified");
  PipGetString("JoinedFile", &joinName);
  ifPixel = 1 - PipGetFloat("PixelSpacing", &joinPixel);
  ifSize = 1 - PipGetTwoIntegers("SizeOfJoinInXandY", &xJoinSize, &yJoinSize);
  if ((joinName && (ifPixel || ifSize)) || (!joinName && (!ifPixel || !ifSize)))
    exitError("You must enter either the joined image file name or the pixel size and "
              "X/Y size of that file");

  numChunks = 0;
  if (PipGetIntegerArray("ChunkSizes", chunkSizes, &numChunks, MAX_CHUNKS))
    exitError("You must enter the list of chunk sizes in the joined file");
  PipGetTwoIntegers("OffsetInXandY", &xOffset, &yOffset);
  PipGetInteger("BinningOfJoin", &binning);

  /* Turn chunks sizes into cumulative Z's */
  for (iz = 1; iz < numChunks; iz++)
    chunkSizes[iz] += chunkSizes[iz - 1];

  /* Get the warp point file and check its properties */
  ierr = readWarpFile(warpName, &nxWarp, &nyWarp, &nzWarp, &iz, &warpPixel, 
                            &version, &warpFlags);
  if (ierr == -3 && version == 0) {
    printf("%s\n", noWarpTxt);
    exit(0);  /* What to exit as ?? */
  }
  if (ierr)
    exitError("Error %d opening or reading the warp file %s", ierr, warpName);
  if (!(warpFlags & WARP_CONTROL_PTS))
    exitError("The warp file does not contain control points to turn into a model");
  if (nzWarp != numChunks)
    exitError("The number of chunks entered (%d) does not match the number of "
              "transforms in the warp file (%d)", numChunks, nzWarp);

  if (getNumWarpPoints(-1, &maxControl))
    exitError("Getting maximum number of control points");
  
  /* Open the joined file and get the header and sizes and pixel size */
  if (joinName) {
    fin = fopen(joinName, "rb");
    if (!fin)
      exitError("Opening joined image file %s", joinName);
    if (mrc_head_read(fin, &hdata))
      exitError("Reading MRC header from joined image file %s", joinName);
    fclose(fin);
    xJoinSize = hdata.nx;
    yJoinSize = hdata.ny;
    if (hdata.nz != chunkSizes[numChunks - 1])
      exitError("The sum of the chunk sizes (%d) does not match the Z size of the "
                "joined file (d)", chunkSizes[numChunks - 1], hdata.nz);
    mrc_get_scale(&hdata, &joinPixel, &ydum, &zdum);
  }

  /* Get filenames for temp files */
  outlen = strlen(outName) + 20;
  origName = B3DMALLOC(char, outlen);
  xfptsName = B3DMALLOC(char, outlen);
  command = B3DMALLOC(char, 2 * outlen + 100);
  if (!origName || !xfptsName || !command)
    exitError("Allocating string arrays");
  strcpy(origName, outName);
  strcpy(xfptsName, outName);
  strcat(origName, ".origpts");
  strcat(xfptsName, ".xfpts");
  
  /* Set up the model */
  origMod = imodNew();
  if (!origMod)
    exitError("Allocating new model or array for warp points");
  if (imodNewObject(origMod))
    exitError("Adding an object to model");
  obj = &origMod->obj[0];
  obj->flags |= IMOD_OBJFLAG_OPEN;
  obj->pdrawsize = 50;
  obj->red = 1.;
  obj->green = 0.;
  obj->blue = 0.;

  /* Get the warp points for each section into new contours */
  for (iz = 1; iz < nzWarp; iz++) {
    if (getNumWarpPoints(iz, &numControl) || 
        getWarpPointArrays(iz, &xControl, &yControl, &xVector, &yVector) ||
        getLinearTransform(iz, linearXF, 2))
      exitError("Getting warp control points or linear transform for iz = %d", iz);
    xfInvert(linearXF, inverseXF, 2);
    for (pt = 0; pt < numControl; pt++) {

      /* The position on the unaligned image is the control point plus the vector
         times the inverse linear xform */
      xfApply(inverseXF, nxWarp / 2., nyWarp / 2., xControl[pt] + xVector[pt],
              yControl[pt] + yVector[pt], &xback, &yback, 2);
      cont = imodContourNew();
      if (!cont || !imodPointAppendXYZ(cont, xback, yback, (float)iz) || 
          imodObjectAddContour(obj, cont) < 0)
        exitError("Adding point to model object");
      free(cont);
    }
  }
  
  /* Make sure that the applied warp transform file matches the warp point file */
  ierr = readWarpFile(xformName, &nxXform, &nyXform, &nzXform, &iz, &xformPixel,
                      &version, &warpFlags);
  if (ierr < 0)
    exitError("Error # %d opening applied transform file %s", ierr, xformName);
  if (nxXform != nxWarp || nyXform != nyWarp || nyXform != nyWarp || 
      fabs(xformPixel - warpPixel) > 1.e-4 * warpPixel || (warpFlags & WARP_CONTROL_PTS))
    exitError("Applied transform file does appear to be derived from warp point file");
  warpFilesDone();

  /* Set maximum dimensions and pixel size data and write model */
  origMod->xmax = nxWarp;
  origMod->ymax = nyWarp;
  origMod->zmax = nzWarp;
  origMod->refImage = B3DMALLOC(IrefImage, 1);
  if (!origMod->refImage)
    exitError("Adding IrefImage structure to temp model");

  origMod->refImage->cscale.x = warpPixel;
  origMod->refImage->cscale.y = warpPixel;
  origMod->refImage->cscale.z = warpPixel;
  origMod->refImage->ctrans.x = 0;
  origMod->refImage->ctrans.y = 0.;
  origMod->refImage->ctrans.z = 0.;

  fout = fopen(origName, "wb");
  if (!fout)
    exitError("Opening new file for temporary model of original points %s", origName);
  if (imodWrite(origMod, fout))
    exitError("Writing temporary model of original points");
  fclose(fout);
  
  /* Run the xfmodel and get the transformed model */
  sprintf(command, "xfmodel -xform %s %s %s", xformName, origName, xfptsName);
  ierr = system(command);
  if (ierr) {
    printf("This command was run and gave an error:\n%s\n", command);
    exitError("Xfmodel failed with return value %d", ierr);
  }

  xfptsMod = imodRead(xfptsName);
  if (!xfptsMod)
    exitError("Reading in transformed model %s", xfptsName);
  
  /* Loop on points and scale them up and shift them and duplicate onto correct Z's */
  obj = &xfptsMod->obj[0];
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    if (cont->psize != 1 || cont->pts[0].z < 0.9 || cont->pts[0].z > numChunks - 0.9)
      exitError("Contour %d in transformed model %s has wrong number of points or "
                "Z out of range", co + 1, xfptsName);
    xfpt = &obj->cont[co].pts[0];
    iz = B3DNINT(xfpt->z);
    xfpt->x = (xfpt->x - nxWarp / 2.) * warpPixel / joinPixel + xJoinSize / 2. - 
      xOffset / binning;
    xfpt->y = (xfpt->y - nyWarp / 2.) * warpPixel / joinPixel + yJoinSize / 2. - 
      yOffset / binning;
    xfpt->z = chunkSizes[iz - 1] - 1.;
    if (!imodPointAppendXYZ(cont, xfpt->x, xfpt->y, xfpt->z + 1))
      exitError("Adding point to final model");
  }

  /* Set the output size and the image reference information in the model */
  xfptsMod->xmax = xJoinSize;
  xfptsMod->ymax = yJoinSize;
  xfptsMod->zmax = chunkSizes[numChunks - 1];
  if (joinName) {
    if (imodSetRefImage(xfptsMod, &hdata))
      exitError("Adding IrefImage structure to final model");
  } else {
    xfptsMod->refImage = NULL;
  }
  
  fout = fopen(outName, "wb");
  if (!fout)
    exitError("Opening new file for final model %s", outName);
  if (imodWrite(xfptsMod, fout))
    exitError("Writing final model");
  fclose(fout);
  exit(0);
}
