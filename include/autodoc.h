/*
 * autodoc.h - Header for autodoc.c in libimod
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 */                                                                           

#ifndef AUTODOC_H
#define AUTODOC_H
#ifdef __cplusplus
extern "C" {
#endif
int AdocRead(const char *filename);
int AdocOpenImageMetadata(const char *filename, int addMdoc, int *montage,
                          int *numSect, int *sectType);
int AdocNew();
int AdocSetCurrent(int index);
void AdocClear(int index);
void AdocDone();
int AdocWrite(const char *filename);
int AdocAppendSection(const char *filename);
int AdocAddSection(const char *collName, const char *name);
int AdocInsertSection(const char *typeName, int sectInd, const char *name);
int AdocDeleteSection(const char *typeName, int sectInd);
int AdocLookupSection(const char *typeName, const char *name);
int AdocFindInsertIndex(const char *typeName, int nameValue);
int AdocSetKeyValue(const char *collName, int sectInd, const char *key, 
                    const char *value);
int AdocDeleteKeyValue(const char *collName, int sectInd, const char *key);
int AdocGetSectionName(const char *collName, int sectInd, char **string);
int AdocGetNumberOfSections(const char *collName);
int AdocGetNumberOfKeys(const char *typeName, int sectInd);
int AdocGetString(const char *collName, int sectInd, const char *key, char **string);
int AdocGetInteger(const char *collName, int sectInd, const char *key, int *val1);
int AdocGetTwoIntegers(const char *collName, int sectInd, const char *key, int *val1,
                       int *val2);
int AdocGetThreeIntegers(const char *collName, int sectInd, const char *key, int *val1,
                         int *val2, int *val3);
int AdocGetIntegerArray(const char *collName, int sectInd, const char *key, int *array,
                        int *numToGet, int arraySize);
int AdocGetFloat(const char *collName, int sectInd, const char *key, float *val1);
int AdocGetTwoFloats(const char *collName, int sectInd, const char *key, float *val1,
                     float *val2);
int AdocGetThreeFloats(const char *collName, int sectInd, const char *key, float *val1,
                       float *val2, float *val3);
int AdocGetFloatArray(const char *collName, int sectInd, const char *key, float *array,
                      int *numToGet, int arraySize);
int AdocSetInteger(const char *typeName, int sectInd, const char *key, int ival);
int AdocSetTwoIntegers(const char *typeName, int sectInd, const char *key, int ival1, 
                       int ival2);
int AdocSetThreeIntegers(const char *typeName, int sectInd, const char *key, int ival1,
                         int ival2, int ival3);
int AdocSetFloat(const char *typeName, int sectInd, const char *key, float val);
int AdocSetTwoFloats(const char *typeName, int sectInd, const char *key, float val1, 
                     float val2);
int AdocSetThreeFloats(const char *typeName, int sectInd, const char *key, float val1,
                       float val2, float val3);

#ifdef __cplusplus
}
#endif
#endif

/*
  $Log$
  Revision 3.7  2010/08/31 22:05:26  mast
  New function to open image metadata

  Revision 3.6  2010/08/28 05:18:04  mast
  Append function

  Revision 3.5  2010/08/27 20:54:48  mast
  new function

  Revision 3.4  2009/04/13 05:13:45  mast
  New function to clear one autodoc

  Revision 3.3  2007/04/05 20:57:42  mast
  Added set functions for ints and floats

  Revision 3.2  2006/10/17 18:01:26  mast
  Gettingthe Id string right

  Revision 3.1  2006/10/17 18:00:32  mast
  Added to package

*/
