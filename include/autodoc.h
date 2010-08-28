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
int AdocRead(char *filename);
int AdocNew();
int AdocSetCurrent(int index);
void AdocClear(int index);
void AdocDone();
int AdocWrite(char *filename);
int AdocAppendSection(char *filename);
int AdocAddSection(char *collName, char *name);
int AdocSetKeyValue(char *collName, int sectInd, char *key, char *value);
int AdocDeleteKeyValue(char *collName, int sectInd, char *key);
int AdocGetSectionName(char *collName, int sectInd, char **string);
int AdocGetNumberOfSections(char *collName);
int AdocGetNumberOfKeys(char *typeName, int sectInd);
int AdocGetString(char *collName, int sectInd, char *key, char **string);
int AdocGetInteger(char *collName, int sectInd, char *key, int *val1);
int AdocGetTwoIntegers(char *collName, int sectInd, char *key, int *val1,
                       int *val2);
int AdocGetThreeIntegers(char *collName, int sectInd, char *key, int *val1,
                         int *val2, int *val3);
int AdocGetIntegerArray(char *collName, int sectInd, char *key, int *array,
                        int *numToGet, int arraySize);
int AdocGetFloat(char *collName, int sectInd, char *key, float *val1);
int AdocGetTwoFloats(char *collName, int sectInd, char *key, float *val1,
                     float *val2);
int AdocGetThreeFloats(char *collName, int sectInd, char *key, float *val1,
                       float *val2, float *val3);
int AdocGetFloatArray(char *collName, int sectInd, char *key, float *array,
                      int *numToGet, int arraySize);
int AdocSetInteger(char *typeName, int sectInd, char *key, int ival);
int AdocSetTwoIntegers(char *typeName, int sectInd, char *key, int ival1, 
                       int ival2);
int AdocSetThreeIntegers(char *typeName, int sectInd, char *key, int ival1,
                         int ival2, int ival3);
int AdocSetFloat(char *typeName, int sectInd, char *key, float val);
int AdocSetTwoFloats(char *typeName, int sectInd, char *key, float val1, 
                     float val2);
int AdocSetThreeFloats(char *typeName, int sectInd, char *key, float val1,
                       float val2, float val3);

#ifdef __cplusplus
}
#endif
#endif

/*
  $Log$
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
