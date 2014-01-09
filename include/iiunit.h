/* iiunit.h - include file for using iiu unit-based I/O routines
 * 
 * $Id$ 
 */

#ifndef IIUNIT_H
#define IIUNIT_H

#include "imodconfig.h"
#include "mrcfiles.h"

#ifdef F77FUNCAP
#define ialBrief IALBRIEF
#define iiuRetBrief IIURETBRIEF
#define iiuAltPrint IIUALTPRINT
#define iiuRetPrint IIURETPRINT
#else
#define ialBrief ialbrief_
#define iiuRetBrief iiuretbrief_
#define iiuAltPrint iiualtprint_
#define iiuRetPrint iiuretprint_
#endif

#ifdef __cplusplus
extern "C" {
#endif

  /* Functions in unit_fileio.c */
  int iiuOpen(int iunit, const char *name, const char *attribute);
  void iiuClose(int iunit);
  void iiuSetPosition(int iunit, int section, int line);
  int iiuReadSection(int iunit, char *array);
  int iiuReadSecPart(int iunit, char *array, int nxdim, int indX0, int indX1,
                     int indY0, int indY1);
  int iiuReadLines(int iunit, char *array, int numLines);
  int iiuWriteSection(int iunit, char *array);
  int iiuWriteSubarray(int iunit, char *array, int nxdim, int ixStart, 
                       int iyStart, int iyEnd);
  int iiuWriteLines(int iunit, char *array, int numLines);
  void iiuFileInfo(int iunit, int *fileSize, int *fileType, int *flags);
  void ialBrief(int *val);
  int iiuRetBrief();
  void iiuAltPrint(int *val);
  int iiuRetPrint();
  void iiuAltConvert(int iunit, int val);
  void iiuExitOnError(int doExit, int storeError);
  int iiuGetExitOnError();
  MrcHeader *iiuMrcHeader(int iunit, const char *function, int doExit, int checkRW);
  void iiuSyncWithMrcHeader(int iunit);
  int iiuFileType(int iunit);
  void iiuMemoryError(void *ptr, const char *message);

  /* Functions in unit_header.c */
  void iiuRetBasicHead(int iunit, int *nxyz, int *mxyz, int *mode, float *dmin, 
                       float *dmax, float *dmean);
  void iiuCreateHeader(int iunit, int *nxyz, int *mxyz, int mode, int *labels, 
                       int numLabels);
  int iiuWriteHeader(int iunit, int *label, int labFlag, float dmin, float dmax,
                      float dmean);
  int iiuWriteHeaderStr(int iunit, const char *labelStr, int labFlag, float dmin,
                         float dmax, float dmean);
  int iiuTransHeader(int iunit, int junit);
  void iiuRetCell(int iunit, float *cell);
  void iiuAltCell(int iunit, float *cell);
  void iiuRetDataType(int iunit, int *itype, int *lensNum, int *n1, int *n2, float *v1, 
                    float *v2);
  void iiuAltDataType(int iunit, int itype, int lensNum, int n1, int n2, float v1, 
                      float v2);
  void iiuRetSize(int iunit, int *nxyz, int *mxyz, int *nxyzst);
  void iiuAltSize(int iunit, int *nxyz, int *nxyzst);
  void iiuRetSample(int iunit, int *mxyz);
  void iiuAltSample(int iunit, int *mxyz);
  void iiuRetAxisMap(int iunit, int *mapcrs);
  void iiuAltAxisMap(int iunit, int *mapcrs);
  void iiuRetSpaceGroup(int iunit, int *ispg);
  void iiuRetImodFlags(int iunit, int *iflags, int *ifImod);
  void iiuAltImodFlags(int iunit, int iflags);
  void iiuAltSigned(int iunit, int iflags);
  void iiuRetOrigin(int iunit, float *xorig, float *yorig, float *zorig);
  void iiuAltOrigin(int iunit, float xorig, float yorig, float zorig);
  void iiuAltMode(int iunit, int mode);
  void iiuRetRMS(int iunit, float *rmsVal);
  void iiuAltRMS(int iunit, float rmsVal);
  void iiuRetTilt(int iunit, float *tilt);
  void iiuAltTilt(int iunit, float *tilt);
  void iiuRetTiltOrig(int iunit, float *tilt);
  void iiuAltTiltOrig(int iunit, float *tilt);
  void iiuRetDelta(int iunit, float *delta);
  void iiuAltDelta(int iunit, float *delta);
  void iiuRetLabels(int iunit, int *labels, int *numLabels);
  void iiuAltLabels(int iunit, int *labels, int numLabels);
  void iiuTransLabels(int iunit, int junit);
  void iiuRetNumExtended(int iunit, int *numBytes);
  void iiuAltNumExtended(int iunit, int numBytes);
  void iiuRetExtendedType(int iunit, int *nintOrFlags, int *nrealOrBytes);
  void iiuAltExtendedType(int iunit, int nintOrFlags, int nrealOrBytes);
  int iiuRetExtendedData(int iunit, int *numBytes, int *extra);
  int iiuAltExtendedData(int iunit, int numBytes, int *extra);
  int iiuTransExtendedData(int iunit, int junit);

#ifdef __cplusplus
}
#endif

#endif
