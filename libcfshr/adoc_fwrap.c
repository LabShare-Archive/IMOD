/*
 * adoc_fwrap.c - Fortran wrappers for autodoc functions
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 * Log at end of file
 */                                                                           

#include <stdlib.h>
#include <string.h>
#include "imodconfig.h"
#include "b3dutil.h"
#include "autodoc.h"
#include "parse_params.h"

#ifdef F77FUNCAP
#define adocread ADOCREAD
#define adocopenimagemetadata ADOCOPENIMAGEMETADATA
#define adocnew ADOCNEW
#define adocsetcurrent ADOCSETCURRENT
#define adocdone ADOCDONE
#define adocwrite ADOCWRITE
#define adocaddsection ADOCADDSECTION
#define adocsetkeyvalue ADOCSETKEYVALUE
#define adocdeletekeyvalue ADOCDELETEKEYVALUE
#define adocgetsectionname ADOCGETSECTIONNAME
#define adocgetnumberofsections ADOCGETNUMBEROFSECTIONS
#define adocgetstring ADOCGETSTRING
#define adocgetinteger ADOCGETINTEGER
#define adocgettwointegers ADOCGETTWOINTEGERS
#define adocgetthreeintegers ADOCGETTHREEINTEGERS
#define adocgetintegerarray ADOCGETINTEGERARRAY
#define adocgetfloat ADOCGETFLOAT
#define adocgettwofloats ADOCGETTWOFLOATS
#define adocgetthreefloats ADOCGETTHREEFLOATS
#define adocgetfloatarray ADOCGETFLOATARRAY
#define adocsetinteger ADOCSETINTEGER
#define adocsettwointegers ADOCSETTWOINTEGERS
#define adocsetthreeintegers ADOCSETTHREEINTEGERS
#define adocsetfloat ADOCSETFLOAT
#define adocsettwofloats ADOCSETTWOFLOATS
#define adocsetthreefloats ADOCSETTHREEFLOATS
#else
#define adocread adocread_
#define adocopenimagemetadata adocopenimagemetadata_
#define adocnew adocnew_
#define adocsetcurrent adocsetcurrent_
#define adocdone adocdone_
#define adocwrite adocwrite_
#define adocaddsection adocaddsection_
#define adocsetkeyvalue adocsetkeyvalue_
#define adocdeletekeyvalue adocdeletekeyvalue_
#define adocgetsectionname adocgetsectionname_
#define adocgetnumberofsections adocgetnumberofsections_
#define adocgetstring adocgetstring_
#define adocgetinteger adocgetinteger_
#define adocgettwointegers adocgettwointegers_
#define adocgetthreeintegers adocgetthreeintegers_
#define adocgetintegerarray adocgetintegerarray_
#define adocgetfloat adocgetfloat_
#define adocgettwofloats adocgettwofloats_
#define adocgetthreefloats adocgetthreefloats_
#define adocgetfloatarray adocgetfloatarray_
#define adocsetinteger adocsetinteger_
#define adocsettwointegers adocsettwointegers_
#define adocsetthreeintegers adocsetthreeintegers_
#define adocsetfloat adocsetfloat_
#define adocsettwofloats adocsettwofloats_
#define adocsetthreefloats adocsetthreefloats_
#endif

static int twof2cstr(char *collName, char *key, int collSize, int keySize, 
                     char **cStr, char **kStr);
static char *adocf2cstr(char *str, int strSize);

int adocread(char *filename, int nameSize)
{
  char *cStr;
  int err;
  if (!(cStr = adocf2cstr(filename, nameSize)))
    return -1;
  err = AdocRead(cStr);
  free(cStr);
  return (err >= 0 ? err + 1 : err);
}

int adocopenimagemetadata(char *filename, int *addMdoc, int *montage,
                          int *numSect, int *sectType, int nameSize)
{
  char *cStr;
  int err;
  if (!(cStr = adocf2cstr(filename, nameSize)))
    return -1;
  err = AdocOpenImageMetadata(cStr, *addMdoc, montage, numSect, sectType);
  free(cStr);
  return (err >= 0 ? err + 1 : err);
}

int adocnew()
{
  return AdocNew();
}

int adocsetcurrent(int *index)
{
  return AdocSetCurrent(*index - 1);
}

void adocdone()
{
  AdocDone();
}

int adocwrite(char *filename, int nameSize)
{
  char *cStr;
  int err;
  if (!(cStr = adocf2cstr(filename, nameSize)))
    return -1;
  err = AdocWrite(cStr);
  free(cStr);
  return err;
}

int adocaddsection(char *collName, char *name, int collSize, int nameSize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, name, collSize, nameSize, &cStr, &kStr))
    return -1;
  err = AdocAddSection(cStr, kStr);
  free(cStr);
  free(kStr);
  return (err >= 0 ? err + 1 : err);
}

int adocsetkeyvalue(char *collName, int *sectInd, char *key, char *value, 
                    int collSize, int keySize, int valSize)
{
  char *cStr, *kStr, *vStr;
  int err;
  if (!(vStr = adocf2cstr(value, valSize)))
    return -1;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr)) {
    free(vStr);
    return -1;
  }
  if (vStr[0])
    err = AdocSetKeyValue(cStr, *sectInd - 1, kStr, vStr);
  else
    err = AdocSetKeyValue(cStr, *sectInd - 1, kStr, NULL);
  free(cStr);
  free(kStr);
  free(vStr);
  return err;
}

int adocsetinteger(char *collName, int *sectInd, char *key, int *ival,
                   int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocSetInteger(cStr, *sectInd - 1, kStr, *ival);
  free(cStr);
  free(kStr);
  return err;
}

int adocsettwointegers(char *collName, int *sectInd, char *key, int *ival1,
                       int *ival2, int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocSetTwoIntegers(cStr, *sectInd - 1, kStr, *ival1, *ival2);
  free(cStr);
  free(kStr);
  return err;
}

int adocsetthreeintegers(char *collName, int *sectInd, char *key, int *ival1,
                         int *ival2, int *ival3, int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocSetThreeIntegers(cStr, *sectInd - 1, kStr, *ival1, *ival2, *ival3);
  free(cStr);
  free(kStr);
  return err;
}

int adocsetfloat(char *collName, int *sectInd, char *key, float *val,
                 int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocSetFloat(cStr, *sectInd - 1, kStr, *val);
  free(cStr);
  free(kStr);
  return err;
}

int adocsettwofloats(char *collName, int *sectInd, char *key, float *val1,
                     float *val2, int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocSetTwoFloats(cStr, *sectInd - 1, kStr, *val1, *val2);
  free(cStr);
  free(kStr);
  return err;
}

int adocsetthreefloats(char *collName, int *sectInd, char *key, float *val1,
                       float *val2, float *val3, int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocSetThreeFloats(cStr, *sectInd - 1, kStr, *val1, *val2, *val3);
  free(cStr);
  free(kStr);
  return err;
}


int adocdeletekeyvalue(char *collName, int *sectInd, char *key, int collSize, 
                       int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocDeleteKeyValue(cStr, *sectInd - 1, kStr);
  free(cStr);
  free(kStr);
  return err;
}

int adocgetsectionname(char *collName, int *sectInd, char *string, 
                       int collSize, int strSize)
{
  char *strPtr = NULL;
  char *cStr;
  int err;
  if (!(cStr = adocf2cstr(collName, collSize)))
    return -1;
  err = AdocGetSectionName(cStr, *sectInd - 1, &strPtr);
  if (!err && c2fString(strPtr, string, strSize)) {
    PipSetError("In AdocGetSectionName, string is too long for character "
                "variable");
    err = -1;
  }

  if (strPtr)
    free(strPtr);
  free(cStr);
  return err;
}

int adocgetnumberofsections(char *collName, int collSize)
{
  char *cStr;
  int err;
  if (!(cStr = adocf2cstr(collName, collSize)))
    return -1;
  err = AdocGetNumberOfSections(cStr);
  free(cStr);
  return err;
}

int adocgetstring(char *collName, int *sectInd, char *key, char *string,
                  int collSize, int keySize, int strSize)
{
  char *strPtr = NULL;
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocGetString(cStr, *sectInd - 1, kStr, &strPtr);
  if (!err && c2fString(strPtr, string, strSize)) {
    PipSetError("In AdocGetString, string is too long for character variable");
    err = -1;
  }

  if (strPtr)
    free(strPtr);
  free(cStr);
  free(kStr);
  return err;
}

int adocgetinteger(char *collName, int *sectInd, char *key, int *val1,
                   int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocGetInteger(cStr, *sectInd - 1, kStr, val1);
  free(cStr);
  free(kStr);
  return err;
}

int adocgettwointegers(char *collName, int *sectInd, char *key, int *val1,
                       int *val2, int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocGetTwoIntegers(cStr, *sectInd - 1, kStr, val1, val2);
  free(cStr);
  free(kStr);
  return err;
}

int adocgetthreeintegers(char *collName, int *sectInd, char *key, int *val1,
                         int *val2, int *val3, int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocGetThreeIntegers(cStr, *sectInd - 1, kStr, val1, val2, val3);
  free(cStr);
  free(kStr);
  return err;
}

int adocgetintegerarray(char *collName, int *sectInd, char *key, int *array,
                        int *numToGet, int *arraySize, int collSize, 
                        int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocGetIntegerArray(cStr, *sectInd - 1, kStr, array, numToGet, *arraySize);
  free(cStr);
  free(kStr);
  return err;
}

int adocgetfloat(char *collName, int *sectInd, char *key, float *val1,
                 int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocGetFloat(cStr, *sectInd - 1, kStr, val1);
  free(cStr);
  free(kStr);
  return err;
}

int adocgettwofloats(char *collName, int *sectInd, char *key, float *val1,
                     float *val2, int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocGetTwoFloats(cStr, *sectInd - 1, kStr, val1, val2);
  free(cStr);
  free(kStr);
  return err;
}

int adocgetthreefloats(char *collName, int *sectInd, char *key, float *val1,
                       float *val2, float *val3, int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocGetThreeFloats(cStr, *sectInd - 1, kStr, val1, val2, val3);
  free(cStr);
  free(kStr);
  return err;
}

int adocgetfloatarray(char *collName, int *sectInd, char *key, float *array,
                      int *numToGet, int *arraySize, int collSize, int keySize)
{
  char *cStr, *kStr;
  int err;
  if (twof2cstr(collName, key, collSize, keySize, &cStr, &kStr))
    return -1;
  err = AdocGetFloatArray(cStr, *sectInd - 1, kStr, array, numToGet, *arraySize);
  free(cStr);
  free(kStr);
  return err;
}

/* Create C copies of two Fortran strings */
static int twof2cstr(char *collName, char *key, int collSize, int keySize, 
                     char **cStr, char **kStr)
{
  if (!(*cStr = adocf2cstr(collName, collSize)))
    return -1;
  if (!(*kStr = adocf2cstr(key, keySize))) {
    free(*cStr);
    return -1;
  }
  return 0;
}

/* Create a C string with a copy of a Fortran string */
static char *adocf2cstr(char *str, int strSize)
{ 
  char *newStr = f2cString(str, strSize);
  if (!newStr)
    PipSetError("Memory error converting string from Fortran to C");
  return newStr;
}

/*
  $Log$
  Revision 1.1  2007/09/20 02:43:08  mast
  Moved to new library

  Revision 3.2  2007/04/05 20:57:23  mast
  Added set functions for ints and floats

  Revision 3.1  2006/10/17 18:14:52  mast
  Added to package

*/
