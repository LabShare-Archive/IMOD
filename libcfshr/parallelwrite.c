/*
 * parallelwrite.c  - Functions for direct writing in parallel to one file
 *
 * Copyright (C) 2009 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 */

#include "imodconfig.h"
#include "b3dutil.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"

#ifdef F77FUNCAP
#define parwrtinitializefw PARWRTINITIALIZEFW
#define parwrtfindregion PARWRTFINDREGION
#define parwrtgetregion PARWRTGETREGION
#define parwrtproperties PARWRTPROPERTIES
#define parwrtsetcurrentfw PARWRTSETCURRENTFW
#else
#define parwrtinitializefw parwrtinitializefw_
#define parwrtfindregion parwrtfindregion_
#define parwrtgetregion parwrtgetregion_
#define parwrtproperties parwrtproperties_
#define parwrtsetcurrentfw parwrtsetcurrentfw_
#endif

#define MAX_FILES 5
typedef struct {
  char *file;
  int section[2];
  int startLine[2];
} BoundRegion;

typedef struct {
  BoundRegion *regions;
  int nx, ny, numBoundLines, numFiles, everySec;
} BoundInfo;

static BoundInfo sInfos[MAX_FILES];
static int sNumInfos = 0;
static int sCurInfo = -1;

#define MAXLINE 1024

/*!
 * Initialize parallel writing to an image file by reading in the boundary 
 * info file whose name is [filename], and storing various values in static 
 * variables. [filename] can be NULL or an empty string, in which case the count of
 * parallel writing files is incremented and this info file index is marked as having no 
 * info file.  [nxin] and [nyin] * specify the dimensions of the images.  The format of 
 * the info file is to start with a line with these entries: ^
 * Version  type  nx  numLines  numFiles  ^
 * version = 1 for now  ^
 * type = 0 for chunks in Z, 1 for chunks in Y that require boundaries on each
 * section  ^
 * nx = X dimension of image file  ^
 * numLines = number of lines written at each boundary  ^
 * numFiles = number of boundary files  ^
 * For each boundary file, there are then two lines:  ^
 * Name of boundary file  ^
 * Boundary 1 section and starting line, boundary 2 section and starting line ^
 * Sections and lines are numbered from 0.  For chunks in Z, the section number
 * should be -1 for no boundary (i.e., for boundary 1 of first chunk and 
 * boundary 2 of last chunk).  A line number of -1 indicates that the boundary
 * extends to the end of the section (thus, a setup script does not need to 
 * know the size of the images in Y).  This routine will convert a line number
 * of -1 to the appropriate starting line.  ^
 * For chunks in Y, the section number is
 * ignored (and should be -1) and the line number should be -1 for no boundary.
 * Returns 1 for failure to open file, 2 for error reading file, 3 for 
 * inappropriate values in header line of file, 4 for memory allocation errors, or
 * 5 for trying to open too many boundary info files for the array.
 */
int parWrtInitialize(char *filename, int nxin, int nyin)
{
  FILE *fp;
  int i, version;
  char line[MAXLINE];
  BoundInfo *bi;

  if (sNumInfos >= MAX_FILES)
    return 5;
  bi = &sInfos[sNumInfos];
  if (!filename || !strlen(filename)) {
    bi->regions = NULL;
    sCurInfo = sNumInfos++;
    return 0;
  }
  fp = fopen(filename, "r");
  if (!fp)
    return 1;
  if (fgetline(fp, line, MAXLINE) <= 0)
    return 2;

  /* Format of file is: version #, type, nx, # of bound lines, # of files */
  sscanf(line, "%d %d %d %d %d", &version, &bi->everySec, &bi->nx, &bi->numBoundLines,
         &bi->numFiles);
  if (bi->nx != nxin || bi->numBoundLines <= 0 || bi->numFiles <= 0)
    return 3;
  bi->regions = (BoundRegion *)malloc(bi->numFiles * sizeof(BoundRegion));
  if (!bi->regions)
    return 4;
  for (i = 0; i < bi->numFiles; i++) {
    if (fgetline(fp, line, MAXLINE) <= 0)
      return 2;
    bi->regions[i].file = strdup(line);
    if (!bi->regions[i].file)
      return 4;
    if (fgetline(fp, line, MAXLINE) <= 0)
      return 2;
    sscanf(line, "%d %d %d %d", &bi->regions[i].section[0], 
           &bi->regions[i].startLine[0], &bi->regions[i].section[1], 
           &bi->regions[i].startLine[1]);

    /* Replace a -1 for second start line with actual line # */
    if (bi->regions[i].section[1] >= 0 && bi->regions[i].startLine[1] < 0)
      bi->regions[i].startLine[1] = nyin - bi->numBoundLines;
  }
  fclose(fp);
  bi->ny = nyin;
  sCurInfo = sNumInfos++;
  return 0;
}

/*!
 * Simple Fortran wrapper for @parWrtInitialize, called by the Fortran
 * parWrtInitialize function.
 */
int parwrtinitializefw(char *filename, int *nxin, int *nyin, int strlen)
{
  int retval;
  char *tempstr = f2cString(filename, strlen);
  if (!tempstr)
    return 4;
  retval = parWrtInitialize(tempstr, *nxin, *nyin);
  free(tempstr);
  return retval;
}

/*!
 * Returns properties read from the current boundary info file: [allSec] 1 if chunks
 * are in Y, [linesBound] with the number of lines in each boundary, [nfiles]
 * with the number of boundary files. These values are all zero if there was no boundary 
 * info file at the current index.  Returns 1 for parallel writing not initialized. 
 */
int parWrtProperties(int *allSec, int *linesBound, int *nfiles)
{
  BoundInfo *bi;
  if (!sNumInfos || sCurInfo < 0)
    return 1;
  *linesBound = 0;
  *allSec = *nfiles = 0;
  if (!sInfos[sCurInfo].regions)
    return 0;
  bi = &sInfos[sCurInfo];
  *linesBound = bi->numBoundLines;
  *allSec = bi->everySec;
  *nfiles = bi->numFiles;
  return 0;
}

/*!
 * Fortran wrapper for @parWrtProperties
 */
int parwrtproperties(int *allSec, int *linesBound, int *nfiles)
{
  return parWrtProperties(allSec, linesBound, nfiles);
}

/*!
 * Sets the index of the current boundary info file to [index], numbered from 0.  Returns 
 * 1 for index out of range.
 */
int parWrtSetCurrent(int index)
{
  if (index < 0 || index >= sNumInfos)
    return 1;
  sCurInfo = index;
  return 0;
}

/*!
 * Fortran wrapper for @parWrtSetCurrent, with [index] numbered from 1.
 */
int parwrtsetcurrentfw(int *index)
{
  return parWrtSetCurrent(*index - 1);
}

/*!
 * Finds the parallel writing region that contains [nlWrite] lines starting
 * with line [lineNum] on section [secNum] and returns the boundary file in
 * [filename], and the section and starting line of the first and second
 * boundaries in arrays [sections] and [startLines].  Returns 1 if parallel
 * writing not initialized, or 2 if the region is not found.
 */
int parWrtFindRegion(int secNum, int lineNum, int nlWrite, char **filename, 
                     int *sections, int *startLines)
{
  int i, pastStart, beforeEnd;
  BoundInfo *bi;
  BoundRegion *regions;
  if (!sNumInfos || sCurInfo < 0 || !sInfos[sCurInfo].regions)
    return 1;
  bi = &sInfos[sCurInfo];
  regions = bi->regions;
  for (i = 0; i < bi->numFiles; i++) {
    pastStart = 1;
    beforeEnd = 1;
    if (bi->everySec) {
      if (regions[i].startLine[0] >= 0 &&
          regions[i].startLine[0] > lineNum + nlWrite - 1)
        pastStart = 0;
      if (regions[i].startLine[1] >= 0 &&
          regions[i].startLine[1] + bi->numBoundLines - 1 < lineNum)
        beforeEnd = 0;
    } else {
      if (regions[i].section[0] >= 0 && 
          (regions[i].section[0] > secNum || 
           (regions[i].section[0] == secNum && 
            regions[i].startLine[0] > lineNum + nlWrite - 1)))
        pastStart = 0;
      if (regions[i].section[1] >= 0 && 
          (regions[i].section[1] < secNum ||
           (regions[i].section[1] == secNum &&
            regions[i].startLine[1] + bi->numBoundLines - 1 < lineNum)))
        beforeEnd = 0;
    }
    if (pastStart && beforeEnd) {
      *filename = regions[i].file;
      sections[0] = regions[i].section[0];
      startLines[0] = regions[i].startLine[0];
      sections[1] = regions[i].section[1];
      startLines[1] = regions[i].startLine[1];

      /* Modify boundary lines for Y chunks so that simple test against the
         boundary will work even if there is no boundary */
      if (bi->everySec) {
        if (startLines[0] < 0)
          startLines[0] -= bi->numBoundLines;
        if (startLines[1] < 0)
          startLines[1] = bi->ny + 1;
      }
      return 0;
    }
  }
  return 2;
}

/*!
 * Fortran wrapper for @parWrtFindRegion
 */
int parwrtfindregion(int *secNum, int *lineNum, int *nlWrite, char *filename,
                     int *sections, int *startLines, int strlen)
{
  char *fileptr;
  int retval = parWrtFindRegion(*secNum, *lineNum, *nlWrite, &fileptr,
                                sections, startLines);
  if (retval)
    return retval;
  return c2fString(fileptr, filename, strlen);
}

/*!
 * Fortran-callable function to get the parameters of parallel writing region
 * number [regionNum] (numbered from 1) and return the boundary file in
 * [filename] and the sections and starting lines in arrays [sections] and
 * [startLines].  Returns 2 if writing not initialized, or 1 if the region 
 * number is out of bounds.
 */
int parwrtgetregion(int *regionNum, char *filename, int *sections, 
                    int *startLines, int strlen)
{
  int reg = *regionNum - 1;
  BoundRegion *regions;
  if (!sNumInfos || sCurInfo < 0 || !sInfos[sCurInfo].regions)
    return 2;
  if (reg < 0 || reg >= sInfos[sCurInfo].numFiles)
    return 1;
  regions = sInfos[sCurInfo].regions;
  sections[0] = regions[reg].section[0];
  startLines[0] = regions[reg].startLine[0];
  sections[1] = regions[reg].section[1];
  startLines[1] = regions[reg].startLine[1];
  return c2fString(regions[reg].file, filename, strlen);
}
