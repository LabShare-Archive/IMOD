/*
 * autodoc.h - Header for autodoc.c in libimod
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id
 */                                                                           

#ifndef AUTODOC_H
#define AUTODOC_H
#ifdef __cplusplus
extern "C" {
#endif
int AdocRead(char *filename);
int AdocNew();
int AdocSetCurrent(int index);
void AdocDone();
int AdocWrite(char *filename);
int AdocAddSection(char *collName, char *name);
int AdocSetKeyValue(char *collName, int sectInd, char *key, char *value);
int AdocDeleteKeyValue(char *collName, int sectInd, char *key);
int AdocGetSectionName(char *collName, int sectInd, char **string);
int AdocGetNumberOfSections(char *collName);
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
#ifdef __cplusplus
}
#endif
#endif

/*
  $Log$
*/
