/*
 * autodoc.c - A parser and manager for autodoc files
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 */                                                                           

#include "autodoc.h"
#include "parse_params.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <sys/stat.h>
#include <sys/types.h>
#ifndef _WIN32
#include <unistd.h>
#endif
#include "b3dutil.h"

/* A section has a name and holds the key-value pairs */
typedef struct adoc_section {
  char *name;          /* value after delimiter in section header */
  char **keys;         /* Array of strings with keys */
  char **values;       /* Array of strings with values */
  int numKeys;         /* Number of key/value pairs */
  int maxKeys;         /* Current size of array */
  char **comments;     /* List of comments strings */
  int *comIndex;       /* Array of key indexes they occur before */
  int numComments;     /* Number of comments */
} AdocSection;

/* A collection holds sections of the same type and has a name */
typedef struct adoc_collection {
  char *name;             /* section type, before delimiter in header */
  AdocSection *sections;  /* Array of sections */
  int numSections;        /* Number of sections */
  int maxSections;        /* Current size of array */
} AdocCollection;

/* An autodoc has an array of collections and a list of sections in order */
typedef struct adoc_autodoc {
  AdocCollection *collections;  
  int numCollections;
  char **finalComments;
  int numFinalCom;
  int *collList;
  int *sectList;
  int numSections;
  int maxSections;
  int inUse;
} Autodoc;

/* The static variables that can hold multiple autodocs */
static Autodoc *autodocs = NULL;
static int numAutodocs = 0;
static int curAdocInd = -1;
static Autodoc *curAdoc = NULL;

#define GLOBAL_NAME "PreData"
#define OPEN_DELIM "["
#define CLOSE_DELIM "]"

static char defaultDelim[] = "=";
static char *valueDelim = defaultDelim;
static char *newDelim = NULL;

#define BIG_STR_SIZE  10240
#define ERR_STR_SIZE  1024
#define MALLOC_CHUNK 10

static void deleteAdoc(Autodoc *adoc);
static int parseKeyValue(char *line, char *end, char **key, char **value);
static int lookupKey(AdocSection *sect, const char *key);
static int lookupCollection(Autodoc *adoc, const char *name);
static int addKey(AdocSection *sect, const char *key, const char *value);
static int addSection(Autodoc *adoc, int collInd, const char *name);
static int addCollection(Autodoc *adoc, const char *name);
static int addAutodoc();
static AdocSection *getSection(const char *collName, int sectInd);
static int addComments(AdocSection *sect, char **comments, int *numComments,
                       int index);
static int writeFile(FILE *afile, int writeAll);
static int findSectionInAdocList(int collInd, int sectInd);

/*!
 * Reads an autodoc from the file specified by [filename], and returns the 
 * index of the new autodoc (numbered from zero) or -1 for an error.
 */
int AdocRead(const char *filename)
{
  int gotSection = 0;
  int err, i, lineLen, indst, icol, ikey, lastInd, index;
  int badline = 1234;
  AdocSection *curSect;
  AdocCollection *coll;
  char *line, *lineEnd, *key, *value;
  char bigStr[BIG_STR_SIZE];
  char errStr[ERR_STR_SIZE];
  char commentChar = '#';
  char **commentList = NULL;
  int maxComments = 0;
  int numComments = 0;
  FILE *afile;

  valueDelim = defaultDelim;
  afile = fopen(filename, "r");
  if (!afile) {
    PipSetError("Error opening autodoc file");
    return -1;
  }

  /* Create a new adoc, which sets up global collection/section
   and takes care of cleanup if it fails */
  if ((index = addAutodoc()) < 0) {
    fclose(afile);
    return -1;
  }

  AdocSetCurrent(index);
  curSect = &curAdoc->collections[0].sections[0];
  lastInd = -1;

  while (1) {

    /* We cannot allow in-line comments so that value lines can contain 
       anything.  But do allow blank and comment lines */
    lineLen = PipReadNextLine(afile, bigStr, BIG_STR_SIZE, commentChar, 1, 0,
                              &indst);
    /* puts(bigStr);
       printf("%d %d\n", lineLen, indst); */
    if (lineLen == -3)
      break;
    if (lineLen < 0) {
      PipSetError(lineLen == -2 ? "Error reading autodoc file" : 
                  "Line too long in autodoc file");
      err = -1;
      break;
    }
    
    /* First check for comment and add it to list */
    line = &bigStr[indst];
    if (indst >= lineLen || line[0] == commentChar) {
      if (numComments >= maxComments) {
        if (maxComments)
          commentList = (char **)realloc(commentList,
                                         (maxComments + 1) * sizeof(char *));
        else
          commentList = (char **)malloc(sizeof(char *));
        if ((err = PipMemoryError(commentList, "AdocRead")))
          break;
        maxComments++;
      }
      commentList[numComments] = strdup(bigStr);
      if ((err = PipMemoryError(commentList[numComments++], "AdocRead")))
        break;
      continue;
    }

    if (PipStartsWith(line, OPEN_DELIM) && strstr(line, CLOSE_DELIM)) {

      /* If this is a section start, get name - value.  Here there must be
       a value and it is an error if there is none. */
      lineEnd = strstr(line, CLOSE_DELIM);
      err = parseKeyValue(line + strlen(OPEN_DELIM), lineEnd, &key, &value);
      if (!value)
        err = 1;
      if (err) {
        err = err > 0 ? badline : err;
        break;
      }

      /* Lookup the collection under the key and create one if not found */
      icol = lookupCollection(curAdoc, key);
      if (icol < 0) {
        if ((err = addCollection(curAdoc, key)))
          break;
        icol = curAdoc->numCollections - 1;
      }
      coll = &curAdoc->collections[icol];

      /* Add a section to the collection and set it as current one */
      if ((err = addSection(curAdoc, icol, value)))
        break;
      curSect = &coll->sections[coll->numSections - 1];
      gotSection = 1;
      free(key);
      free(value);
      lastInd = -1;

    } else {

      /* Otherwise this is key-value inside a section.  First check for
       continuation line and append to last value. */
      if (lastInd >= 0 && !strstr(line, valueDelim) && 
          curSect->values[lastInd]) {
        ikey = strlen(curSect->values[lastInd]);
        curSect->values[lastInd] = (char *)realloc
          (curSect->values[lastInd], ikey + lineLen - indst + 3);
        if ((err = PipMemoryError(curSect->values[lastInd], "AdocRead")))
          break;

        /* Replace null with space and new null, then append new string */
        curSect->values[lastInd][ikey] = ' ';
        curSect->values[lastInd][ikey + 1] = 0x00;
        strcat(curSect->values[lastInd], line);
        continue;
      }

      /* This should be a key-value pair now */
      lineEnd = line + lineLen - indst;
      if ((err = parseKeyValue(line, lineEnd, &key, &value))) {
        err = err > 0 ? badline : err;
        break;
      }

      /* Handle new key-value delimiter - replace previous new value if any */
      if (!gotSection && !strcmp(key, "KeyValueDelimiter") && value) {
        if (newDelim)
          free(newDelim);
        newDelim = strdup(value);
        if ((err = PipMemoryError(newDelim, "AdocRead")))
          break;
        valueDelim = newDelim;
      }

      /* Handle change of comment character */
      if (!gotSection && !strcmp(key, "CommentCharacter"))
        commentChar = value[0];

      /* Look up the key first to replace an existing value */
      ikey = lookupKey(curSect, key);
      if (ikey >= 0) {
        if (curSect->values[ikey])
          free(curSect->values[ikey]);
        curSect->values[ikey] = value;
        free(key);
        lastInd = ikey;

      } else {

        /* Or just add the key-value */
        if ((err = addKey(curSect, key, value)))
          break;
        free(key);
        if (value)
          free(value);
        lastInd = curSect->numKeys -1;
      }
    }

    /* If there are comments, attach to item just added */
    if (numComments)
      if ((err = addComments(curSect, commentList, &numComments, lastInd)))
        break;
  }

  /* END OF FILE: If error, clean out autodoc, compose message for bad line */
  if (err) {
    deleteAdoc(curAdoc);
    if (err == badline) {
      bigStr[ERR_STR_SIZE - 50] = 0x00;
      sprintf(errStr, "Improperly formatted line in autodoc: %s\n", bigStr);
      PipSetError(errStr);
      err = -1;
    }

    /* Clean out comment list too */ 
    for (i = 0; i < numComments; i++)
      if (commentList[i])
        free(commentList[i]);
    if (commentList)
      free(commentList);
  } else if (commentList) {

    /* If good, transfer any comments to the autodoc */
    if (numComments) {
      curAdoc->finalComments = commentList;
      curAdoc->numFinalCom = numComments;
    } else
      free(commentList);
  }

  fclose(afile);
  return (err ? err : index);
}

/*!
 * Try to read an image metadata file and determine its properties.  It will 
 * look for and try to read the file with name [filename] if [addMdoc] is 0,
 * otherwise it will append '.mdoc' to [filename].  Returns a non-zero in
 * [montage] if the file indicates a montage; the number of sections in
 * [numSect], and 1 for a metadata autodoc or 2 for an image series autodoc
 * in [sectType].  The return value is the index of the autodoc, or -1 for an
 * error opening or reading the file, -2 if the file does not exist, or -3 if
 * it is neither type of metadata file.
 */
int AdocOpenImageMetadata(const char *filename, int addMdoc, int *montage,
                          int *numSect, int *sectType)
{
  struct stat buf;
  char *usename = (char *)filename;
  int series, index;

  /* Attach extension to file if requested */
  if (addMdoc) {
    usename = (char *)malloc(strlen(filename) + 6);
    if (!usename)
      return -1;
    sprintf(usename, "%s.mdoc", filename);
  }

  /* Return -2 if it does not exist, -1 if error reading it */
  if (stat(usename, &buf)) {
    index = -2;
  } else {
    index = AdocRead(usename);
  }
  if (addMdoc)
    free(usename);
  if (index < 0)
    return index;

  /* Determine section type or return -3 if not a metadata file */
  if (!AdocGetString(GLOBAL_NAME, 0, "ImageFile", &usename)) {
    *sectType = 1;
    free(usename);
    *numSect = AdocGetNumberOfSections("ZValue");
  } else if (!AdocGetInteger(GLOBAL_NAME, 0, "ImageSeries", &series) && 
             series) {
    *sectType = 2;
    *numSect = AdocGetNumberOfSections("Image");
  } else {
    AdocClear(index);
    return -3;
  }
 
  *montage = 0;
  AdocGetInteger(GLOBAL_NAME, 0, "Montage", montage);
  return index;
}

/*!
 * Creates a new autodoc in the internal array, makes it the current one, and 
 * returns its index or -1 for an error.
 */
int AdocNew()
{
  int err;
  if ((err = addAutodoc()) < 0)
    return err;
  AdocSetCurrent(err);
  return err;
}  

/*!
 * Makes the autodoc at [index] be the current autodoc.  The index is numbered
 * from zero.  Returns -1 for an index out of bounds.
 */
int AdocSetCurrent(int index)
{
  if (index < 0 || index >= numAutodocs)
    return -1;
  curAdocInd = index;
  curAdoc = &autodocs[curAdocInd];
  return 0;
}

/*!
 * Deletes all data from the autodoc at [index] and marks it as unused.
 */
void AdocClear(int index)
{
  if (index >= 0 && index < numAutodocs)
    deleteAdoc(&autodocs[index]);
}

/*!
 * Deletes all autodocs and returns the module to its initial state. 
 */
void AdocDone()
{
  int i;
  for (i = 0; i < numAutodocs; i++)
    deleteAdoc(&autodocs[i]);
  if (autodocs)
    free(autodocs);
  autodocs = NULL;
  numAutodocs = 0;
  curAdocInd = -1;
  curAdoc = NULL;
}

/*!
 * Writes the current autodoc to the file specified by [filename].  Returns 1
 * for failure to back up a previous file, and -1 for other errors 
 */
int AdocWrite(const char *filename)
{
  int i,backerr;
  FILE *afile;

  if (!curAdoc)
    return -1;
  backerr = imodBackupFile(filename);
  afile = fopen(filename, "w");
  if (!afile)
    return -1;
  if (writeFile(afile, 1))
    return -1;

  for (i = 0; i < curAdoc->numFinalCom; i++)
    fprintf(afile, "%s\n", curAdoc->finalComments[i]);

  fclose(afile);
  return backerr ? 1 : 0;
}

/*!
 * Appends the last section in the current autodoc to the file specified by 
 * [filename].  Returns -1 for errors.
 */
int AdocAppendSection(const char *filename)
{
  FILE *afile;
  if (!curAdoc)
    return -1;
  afile = fopen(filename, "a");
  if (!afile)
    return -1;
  if (writeFile(afile, 0))
    return -1;

  fclose(afile);
  return 0;
}

/* Function to actually write the file or last section only */
static int writeFile(FILE *afile, int writeAll)
{
  int i,j,k, ind, comInd, write, lastBlank;
  AdocCollection *coll;
  AdocSection *sect;

  /* Initialize delimiter, loop on indexes in the autodoc */
  valueDelim = defaultDelim;
  for (ind = 0; ind < curAdoc->numSections; ind++) {
    write = (writeAll || ind == curAdoc->numSections - 1) ? 1 : 0;
    i = curAdoc->collList[ind];
    j = curAdoc->sectList[ind];
    coll = &curAdoc->collections[i];
    sect = &coll->sections[j];

    /* dump comments before section */
    comInd = 0;
    lastBlank = 0;
    while (write && comInd < sect->numComments && sect->comIndex[comInd] == -1)
      if (write) {
        lastBlank = sect->comments[comInd][0] == 0x00 ? 1 : 0;
        fprintf(afile, "%s\n", sect->comments[comInd++]);
      }

    /* Write section name unless we're in global */
    if ((i || j || strcmp(sect->name, GLOBAL_NAME)) && write)
      fprintf(afile, "%s[%s %s %s]\n", lastBlank ? "" : "\n", coll->name, valueDelim,
              sect->name);

    /* Loop on key-values */
    for (k = 0; k < sect->numKeys; k++) {

      /* dump comments associated with this index */
      while (write && comInd < sect->numComments && sect->comIndex[comInd] == k)
        if (write)
          fprintf(afile, "%s\n", sect->comments[comInd++]);

      /* Print key-value pairs with non-null values */
      if (sect->keys[k] && sect->values[k]) {
        if (write)
          fprintf(afile, "%s %s %s\n", sect->keys[k], valueDelim, 
                  sect->values[k]);
        
        /* After a new delimiter is written, need to set delimiter */
        if (!i && !j && !strcmp("KeyValueDelimiter", sect->keys[k])) {
          if (newDelim)
            free(newDelim);
          newDelim = strdup(sect->values[k]);
          if (PipMemoryError(newDelim, "AdocWrite")) {
            fclose(afile);
            return -1;
          }
          valueDelim = newDelim;

        }

        /* Print keys without values too */
      } else if (sect->keys[k] && write) 
        fprintf(afile, "%s %s \n", sect->keys[k], valueDelim);
    }
  }
  return 0;
}

/*!
 * Adds a section of type specified by [typeName] and name given by [name].  
 * Returns the index of the new section in the collection of sections of that
 * type, or -1 for error. 
 */
int AdocAddSection(const char *typeName, const char *name)
{
  AdocCollection *coll;
  int collInd;

  if (!curAdoc || !typeName || !name)
    return -1;
  collInd = lookupCollection(curAdoc, typeName);
  if (collInd < 0) {
    if (addCollection(curAdoc, typeName))
      return -1;
    collInd = curAdoc->numCollections - 1;
  }
  coll = &curAdoc->collections[collInd];
  if (addSection(curAdoc, collInd, name))
    return -1;
  return coll->numSections - 1;
}

/*!
 * Inserts a section with name given by [name] into the collection of sections of type 
 * [typeName], at the index [sectInd].  The collection must exist unless [sectInd] is 0,
 * and [sectInd] must be less than or equal to the number of sections in that collection.
 * Returns -1 for error.
 */
int AdocInsertSection(const char *typeName, int sectInd, const char *name)
{
  AdocCollection *coll;
  int i, collInd, masterInd, numSect = 0;
  AdocSection newSect;
  if (!curAdoc || !typeName || !name)
    return -1;
  collInd = lookupCollection(curAdoc, typeName);
  if (collInd >= 0)
    numSect = curAdoc->collections[collInd].numSections;
  if (sectInd < 0 || sectInd > numSect)
    return -1;
  
  /* Find the index of this section in the master list if it needs to be shuffled */
  if (sectInd < numSect) {
    masterInd = findSectionInAdocList(collInd, sectInd);
    if (masterInd < 0)
      return -1;
  }

  /* Add section to end regardless, then return if that is all that is needed */
  coll = &curAdoc->collections[collInd];
  if (AdocAddSection(typeName, name) < 0)
      return -1;
  if (sectInd == numSect)
    return 0;

  /* Save the new section then move existing sections up and copy new one into place */
  memcpy(&newSect, &coll->sections[coll->numSections - 1], sizeof(AdocSection));
  for (i = coll->numSections - 1; i > sectInd; i--)
    memcpy(&coll->sections[i], &coll->sections[i - 1], sizeof(AdocSection));
  memcpy(&coll->sections[sectInd], &newSect, sizeof(AdocSection));
  
  /* Move the master lists up and decrement any other indices in this collection */
  for (i = curAdoc->numSections - 1; i > masterInd; i--) {
    curAdoc->collList[i] = curAdoc->collList[i - 1];
    curAdoc->sectList[i] = curAdoc->sectList[i - 1];
    if (curAdoc->collList[i] == collInd && curAdoc->sectList[i] >= sectInd)
      curAdoc->sectList[i]++;
  }

  return 0;
}

/*!
 * Deletes the section at index [sectInd] from the collection of sections of type 
 * [typeName].  Returns -1 for error.  (Untested)
 */
int AdocDeleteSection(const char *typeName, int sectInd)
{
  AdocCollection *coll;
  int collInd, i, masterInd;
  if (!curAdoc || !typeName)
    return -1;
  collInd = lookupCollection(curAdoc, typeName);
  if (collInd < 0)
    return -1;
  coll = &curAdoc->collections[collInd];
  if (sectInd < 0 || sectInd >= coll->numSections)
    return -1;

  /* Find the index of this section in the master list */
  masterInd = findSectionInAdocList(collInd, sectInd);
  if (masterInd < 0)
    return -1;

  /* Repack the sections */
  for (i = sectInd + 1; i < coll->numSections; i++)
    memcpy(&coll->sections[i - 1], &coll->sections[i], sizeof(AdocSection));
  coll->numSections--;

  /* Repack the master list and decrement any other indices in this collection */
  for (i = masterInd + 1; i < curAdoc->numSections; i++) {
    if (curAdoc->collList[i] == collInd && curAdoc->sectList[i] > sectInd)
      curAdoc->sectList[i]--;
    curAdoc->collList[i - 1] = curAdoc->collList[i];
    curAdoc->sectList[i - 1] = curAdoc->sectList[i];
  }
  curAdoc->numSections--;
  return 0;
}

/*!
 * Looks up a section of type specified by [typeName] and name given by [name].  
 * Returns the index of that section in the collection of sections of that
 * type, -1 if there is none, or -2 for error.
 */
int AdocLookupSection(const char *typeName, const char *name)
{
  AdocCollection *coll;
  int collInd, sectInd;

  if (!curAdoc || !typeName || !name)
    return -2;
  collInd = lookupCollection(curAdoc, typeName);
  if (collInd < 0)
    return -2;
  coll = &curAdoc->collections[collInd];
  for (sectInd = 0; sectInd < coll->numSections; sectInd++)
    if (!strcmp(coll->sections[sectInd].name, name))
      return sectInd;
  return -1;
}

/*!
 * Looks in the collection of sections of type [typeName], converts their name strings to
 * to integers, and returns the index of the first section whose name is greater than
 * [nameValue], the number of sections if there is no such section, or -1 for error
 * (including if a section exists whose name converts to [nameValue]).  This returned
 * index can thus be used in a call to @AdocInsertSection to maintain the sections in
 * numeric order by name value.
 */
int AdocFindInsertIndex(const char *typeName, int nameValue)
{
  AdocCollection *coll;
  int collInd, sectInd, sectValue;

  if (!curAdoc || !typeName)
    return -1;
  collInd = lookupCollection(curAdoc, typeName);
  if (collInd < 0)
    return -1;
  coll = &curAdoc->collections[collInd];
  for (sectInd = 0; sectInd < coll->numSections; sectInd++) {
    sectValue = atoi(coll->sections[sectInd].name);
    if (nameValue == sectValue)
      return -1;
    if (nameValue < sectValue)
      return sectInd;
  }
  return coll->numSections;
}

/*!
 * Sets a key-value pair to [key] and [value] in the section with index 
 * [sectInd] in the collection of sections of type [typeName].  The section 
 * must already exist.  Replaces an existing value if any.  [value] may be 
 * NULL.  Returns -1 for error.
 */
int AdocSetKeyValue(const char *typeName, int sectInd, const char *key, const char *value)
{
  AdocSection *sect;
  int keyInd;

  if (!(sect = getSection(typeName, sectInd)))
    return -1;
  if (!key || !value)
    return -1;
  keyInd = lookupKey(sect, key);

  /* If key already exists, clear out value and set it again */
  if (keyInd >= 0) {
    if (sect->values[keyInd])
      free(sect->values[keyInd]);
    if (value) {
      sect->values[keyInd] = strdup(value);
      if (PipMemoryError(sect->values[keyInd], "AdocSetKeyValue"))
        return -1;
    } else
      sect->values[keyInd] = NULL;
  } else
    return addKey(sect, key, value);
  return 0;
}

/*!
 * Sets the value of [key] to the integer [ival] in the section with index 
 * [sectInd] in the collection of sections of type [typeName].  The section 
 * must already exist.  Replaces an existing value if any.
 * Returns -1 for error.
 */
int AdocSetInteger(const char *typeName, int sectInd, const char *key, int ival)
{
  char str[30];
  sprintf(str, "%d", ival);
  return(AdocSetKeyValue(typeName, sectInd, key, str));
}

/*!
 * Like @AdocSetInteger, except that the value is set to the two integers
 * [ival1] [ival2].
 */
int AdocSetTwoIntegers(const char *typeName, int sectInd, const char *key, int ival1, 
                       int ival2)
{
  char str[60];
  sprintf(str, "%d %d", ival1, ival2);
  return(AdocSetKeyValue(typeName, sectInd, key, str));
}

/*!
 * Like @AdocSetInteger, except that the value is set to the three integers
 * [ival1] [ival2] [ival3].
 */
int AdocSetThreeIntegers(const char *typeName, int sectInd, const char *key, int ival1,
                         int ival2, int ival3)
{
  char str[90];
  sprintf(str, "%d %d %d", ival1, ival2, ival3);
  return(AdocSetKeyValue(typeName, sectInd, key, str));
}

/*!
 * Sets the value of [key] to the float [val] in the section with index 
 * [sectInd] in the collection of sections of type [typeName].  The section 
 * must already exist.  Replaces an existing value if any.
 * Returns -1 for error.
 */
int AdocSetFloat(const char *typeName, int sectInd, const char *key, float val)
{
  char str[30];
  sprintf(str, "%g", val);
  return(AdocSetKeyValue(typeName, sectInd, key, str));
}

/*!
 * Like @AdocSetFloat, except that the value is set to the two floats
 * [val1] [val2].
 */
int AdocSetTwoFloats(const char *typeName, int sectInd, const char *key, float val1, 
                     float val2)
{
  char str[60];
  sprintf(str, "%g %g", val1, val2);
  return(AdocSetKeyValue(typeName, sectInd, key, str));
}

/*!
 * Like @AdocSetFloat, except that the value is set to the three floats
 * [val1] [val2] [val3].
 */
int AdocSetThreeFloats(const char *typeName, int sectInd, const char *key, float val1,
                       float val2, float val3)
{
  char str[90];
  sprintf(str, "%g %g %g", val1, val2, val3);
  return(AdocSetKeyValue(typeName, sectInd, key, str));
}

/*!
 * Deletes the key-value pair matching [key] in the section with index
 * [sectInd] in the collection of sections of type [typeName].  Clears out
 * both the key and the value.  Returns -1 for error. 
 */  
int AdocDeleteKeyValue(const char *typeName, int sectInd, const char *key)
{
  AdocSection *sect;
  int keyInd;

  if (!(sect = getSection(typeName, sectInd)))
    return -1;
  keyInd = lookupKey(sect, key);
  if (keyInd < 0)
    return -1;
  if (sect->values[keyInd])
    free(sect->values[keyInd]);
  sect->values[keyInd] = NULL;
  free(sect->keys[keyInd]);
  sect->keys[keyInd] = NULL;
  return 0;
}

/*
 * Routines for getting data from or modifying the current autodoc
 */

/*!
 * Gets the name of the section with index [sectInd] in the collection of 
 * sections of type [typeName].  Returns the name in [string].
 * Returns -1 for errors.
 */
int AdocGetSectionName(const char *typeName, int sectInd, char **string)
{
  AdocSection *sect;

  if (!(sect = getSection(typeName, sectInd)))
    return -1;
  *string = strdup(sect->name);
  return (PipMemoryError(*string, "AdocGetSectionName"));
}

/*!
 * Returns the number of sections of type [typeName].  Returns -1 for errors,
 * and 0 if there are no sections of the given type.
 */
int AdocGetNumberOfSections(const char *typeName)
{
  int collInd;
  if (!curAdoc || !typeName)
    return -1;
  collInd = lookupCollection(curAdoc, typeName);
  if (collInd < 0)
    return 0;
  return curAdoc->collections[collInd].numSections;
}

/*!
 * Returns the number of key-value pairs in the section with index [sectInd]
 * in the collection of sections of type [typeName].  Returns -1 for errors.
 */
int AdocGetNumberOfKeys(const char *typeName, int sectInd)
{
  AdocSection *sect;
  if (!(sect = getSection(typeName, sectInd)))
    return -1;
  return sect->numKeys;
}

/*!
 * Gets the value string matching [key] in the section with index [sectInd]
 * in the collection of sections of type [typeName].  Returns a copy of the
 * value in [string]; it should be freed with {free}.  Returns -1 if the key is
 * null, the section does not exist, or for a memory error; returns 1 if the 
 * key does not occur in the given section or if the value is null.  
 */
int AdocGetString(const char *typeName, int sectInd, const char *key, char **string)
{
  AdocSection *sect;
  int keyInd;

  if (!key)
    return -1;
  if (!(sect = getSection(typeName, sectInd)))
    return -1;
  keyInd = lookupKey(sect, key);
  if (keyInd < 0 || !sect->values[keyInd])
    return 1;
  *string = strdup(sect->values[keyInd]);
  return (PipMemoryError(*string, "AdocGetString"));
}

/*!
 * Like @AdocGetString, except that it extracts one integer from the value
 * string and returns its value in [val1].
 */
int AdocGetInteger(const char *typeName, int sectInd, const char *key, int *val1)
{
  int err;
  int num = 1;
  int tmp[1];
  if ((err = AdocGetIntegerArray(typeName, sectInd, key, tmp, &num, 1)) != 0)
    return err;
  *val1 = tmp[0];
  return 0;
}

/*! Like @AdocGetInteger except that it returns a float */
int AdocGetFloat(const char *typeName, int sectInd, const char *key, float *val1)
{
  int err;
  int num = 1;
  float tmp[1];
  if ((err = AdocGetFloatArray(typeName, sectInd, key, tmp, &num, 1)) != 0)
    return err;
  *val1 = tmp[0];
  return 0;
}

/*!
 * Like @AdocGetString, except that it extracts two integers from the value
 * string and returns their values in [val1] and [val2].
 */
int AdocGetTwoIntegers(const char *typeName, int sectInd, const char *key, int *val1,
                         int *val2)
{
  int err;
  int num = 2;
  int tmp[2];
  if ((err = AdocGetIntegerArray(typeName, sectInd, key, tmp, &num, 2)) != 0)
    return err;
  *val1 = tmp[0];
  *val2 = tmp[1];
  return 0;
}

/*! Like @AdocGetTwoIntegers except that it returns floats */
int AdocGetTwoFloats(const char *typeName, int sectInd, const char *key, float *val1,
                         float *val2)
{
  int err;
  int num = 2;
  float tmp[2];
  if ((err = AdocGetFloatArray(typeName, sectInd, key, tmp, &num, 2)) != 0)
    return err;
  *val1 = tmp[0];
  *val2 = tmp[1];
  return 0;
}

/*!
 * Like @AdocGetString, except that it extracts three integers from the value
 * string and returns their values in [val1], [val2], and [val3].
 */
int AdocGetThreeIntegers(const char *typeName, int sectInd, const char *key, int *val1,
                         int *val2, int *val3)
{
  int err;
  int num = 3;
  int tmp[3];
  if ((err = AdocGetIntegerArray(typeName, sectInd, key, tmp, &num, 3)) != 0)
    return err;
  *val1 = tmp[0];
  *val2 = tmp[1];
  *val3 = tmp[2];
  return 0;
}

/*! Like @AdocGetThreeIntegers except that it returns floats */
int AdocGetThreeFloats(const char *typeName, int sectInd, const char *key, float *val1,
                         float *val2, float *val3)
{
  int err;
  int num = 3;
  float tmp[3];
  if ((err = AdocGetFloatArray(typeName, sectInd, key, tmp, &num, 3)) != 0)
    return err;
  *val1 = tmp[0];
  *val2 = tmp[1];
  *val3 = tmp[2];
  return 0;
}

/*!
 * Like @AdocGetString, except that it extracts a set of integers from the
 * value string and returns their values in [array], whose size is given in 
 * [arraySize].  Set [numToGet] to the number of values to get, or 0 to get
 * all the values; in the latter case [numToGet] is returned with the number
 * of values retrieved.  Returns -1 for errors in parsing, too few values on 
 * the line, or not enough space in the array, as well as for failures in 
 * getting the value string.
 */
int AdocGetIntegerArray(const char *typeName, int sectInd, const char *key, int *array,
                        int *numToGet, int arraySize)
{
  char *string;
  int err;
  if ((err = AdocGetString(typeName, sectInd, key, &string)))
    return err;
  err = PipGetLineOfValues(string, string, (void *)array, PIP_INTEGER,
                           numToGet, arraySize);
  free(string);
  return err;
}

/*! Like @AdocGetIntegerArray except that it returns floats */
int AdocGetFloatArray(const char *typeName, int sectInd, const char *key, float *array,
                        int *numToGet, int arraySize)
{
  char *string;
  int err;
  if ((err = AdocGetString(typeName, sectInd, key, &string)))
    return err;
  err = PipGetLineOfValues(string, string, (void *)array, PIP_FLOAT,
                           numToGet, arraySize);
  free(string);
  return err;
}

/*
 * Routines for adding keys, sections, collections, autodocs 
 */

/* Adds a key-value pair to the given section, without checking for 
   duplication */
static int addKey(AdocSection *sect, const char *key, const char *value)
{

  /* First allocate enough memory if needed */
  if (!sect->maxKeys) {
    sect->keys = (char **)malloc(MALLOC_CHUNK * sizeof(char *));
    sect->values = (char **)malloc(MALLOC_CHUNK * sizeof(char *));
    sect->maxKeys = MALLOC_CHUNK;
  } else if (sect->numKeys >= sect->maxKeys) {
    sect->keys = (char **)realloc(sect->keys, (sect->maxKeys + MALLOC_CHUNK) * 
                                  sizeof(char *));
    sect->values = (char **)realloc(sect->values, 
                                    (sect->maxKeys + MALLOC_CHUNK) *
                                    sizeof(char *));
    sect->maxKeys += MALLOC_CHUNK;
  }
  if (!sect->keys || !sect->values) {
    PipMemoryError(NULL, "addKey");
    return -1;
  }

  /* Copy key and value and increment count */
  sect->keys[sect->numKeys] = strdup(key);
  if (value)
    sect->values[sect->numKeys] = strdup(value);
  else
    sect->values[sect->numKeys] = NULL;
  if (! sect->keys[sect->numKeys] || (value && !sect->values[sect->numKeys])) {
    PipMemoryError(NULL, "addKey");
    return -1;
  }
  sect->numKeys++;
  return 0;
}

/* Adds a section of the given name to the collection */
static int addSection(Autodoc *adoc, int collInd, const char *name)
{
  AdocCollection *coll = &adoc->collections[collInd];
  AdocSection *sect;

  /* First allocate enough memory if needed for the sections in the collection
     and for the master lists in the autodoc */
  if (!coll->maxSections) {
    coll->sections = (AdocSection *)malloc(MALLOC_CHUNK * sizeof(AdocSection));
    coll->maxSections = MALLOC_CHUNK;
  } else if (coll->numSections >= coll->maxSections) {
    coll->sections = (AdocSection *)
      realloc(coll->sections, (coll->maxSections + MALLOC_CHUNK)
              * sizeof(AdocSection));
    coll->maxSections += MALLOC_CHUNK;
  }
  if (PipMemoryError(coll->sections, "addSection"))
    return -1;

  if (!adoc->maxSections) {
    adoc->collList = (int *)malloc(MALLOC_CHUNK * sizeof(int));
    adoc->sectList = (int *)malloc(MALLOC_CHUNK * sizeof(int));
    adoc->maxSections = MALLOC_CHUNK;
  } else if (adoc->numSections >= adoc->maxSections) {
    adoc->collList = (int *)realloc
      (adoc->collList, (adoc->maxSections + MALLOC_CHUNK) * sizeof(int));
    adoc->sectList = (int *)realloc
      (adoc->sectList, (adoc->maxSections + MALLOC_CHUNK) * sizeof(int));
    adoc->maxSections += MALLOC_CHUNK;
  }
  if (!adoc->collList || !adoc->sectList) {
    PipMemoryError(NULL, "addSection");
    return -1;
  }

  /* Copy the name and initialize to empty keys */
  sect = &coll->sections[coll->numSections];
  sect->name = strdup(name);
  if (PipMemoryError(sect->name, "addSection"))
    return -1;
  sect->keys = NULL;
  sect->values = NULL;
  sect->numKeys = 0;
  sect->maxKeys = 0;
  sect->comments = NULL;
  sect->comIndex = NULL;
  sect->numComments = 0;

  /* Add the collection and section # to master list */
  adoc->collList[adoc->numSections] = collInd;
  adoc->sectList[adoc->numSections++] = coll->numSections++;
  return 0;
}

/* Adds a collection of the given name to the autodoc */
static int addCollection(Autodoc *adoc, const char *name)
{
  AdocCollection *coll;

  /* Allocate just one at a time when needed */
  if (!adoc->numCollections)
    adoc->collections = (AdocCollection *)malloc(sizeof(AdocCollection));
  else
    adoc->collections = (AdocCollection *)realloc(adoc->collections,
                                                  (adoc->numCollections + 1) *
                                                  sizeof(AdocCollection));
  if (PipMemoryError(adoc->collections, "addCollection"))
    return -1;
  coll = &adoc->collections[adoc->numCollections];
  coll->name = strdup(name);
  if (PipMemoryError(coll->name, "addCollection"))
    return -1;
  coll->numSections = 0;
  coll->maxSections = 0;
  coll->sections = NULL;
  adoc->numCollections++;
  return 0;
}

/* Adds an autodoc to the array.  Initializes it with a global collection and
   section, and clears it out if this fails */
static int addAutodoc()
{
  Autodoc *adoc;
  int index = -1, i;
  
  /* Search for a free autodoc in array */
  for (i = 0; i < numAutodocs; i++) {
    if (!autodocs[i].inUse) {
      index = i;
      break;
    }
  }

  if (index < 0) {

    /* Allocate just one at a time when needed */
    if (!numAutodocs)
      autodocs = (Autodoc *)malloc(sizeof(Autodoc));
    else
      autodocs = (Autodoc *)realloc(autodocs, (numAutodocs + 1) *
                                    sizeof(Autodoc));
    if (PipMemoryError(autodocs, "addAutodoc"))
      return -1;
    index = numAutodocs++;
  }

  /* Initialize collections */
  adoc = &autodocs[index];
  adoc->collections = NULL;
  adoc->numCollections = 0;
  adoc->finalComments = NULL;
  adoc->numFinalCom = 0;
  adoc->collList = NULL;
  adoc->sectList = NULL;
  adoc->numSections = 0;
  adoc->maxSections = 0;
  adoc->inUse = 1;

  /* Add a collection and section for global data */
  if (addCollection(adoc, GLOBAL_NAME))
    return -1;
  if (addSection(adoc, 0, GLOBAL_NAME)) {
    deleteAdoc(adoc);
    return -1;
  }
  return index;
}

/* 
 * Utility routines for freeing, parsing and lookup
 */
/* Frees all data in an autodoc and zero out the top-level items so this can
   be called more than once */
static void deleteAdoc(Autodoc *adoc)
{
  AdocSection *sect;
  AdocCollection *coll;
  int i, j, k;
  for (i = 0; i < adoc->numCollections; i++) {
    coll = &adoc->collections[i];
    for (j = 0; j < coll->numSections; j++) {
      sect = &coll->sections[j];

      /* Clean key/values out of sections */
      for (k = 0; k < sect->numKeys; k++) {
        if (sect->keys[k])
          free(sect->keys[k]);
        if (sect->values[k])
          free(sect->values[k]);
      }
      if (sect->keys)
        free(sect->keys);
      if (sect->values)
        free(sect->values);
      if (sect->name)
        free(sect->name);

      /* Clean comments out of sections */
      for (k = 0; k < sect->numComments; k++)
        if (sect->comments[k])
          free(sect->comments[k]);
      if (sect->comments)
        free(sect->comments);
    }


    /* Free sections */
    if (coll->sections)
      free(coll->sections);
    if (coll->name)
      free(coll->name);
  }

  /* Free collections */
  if (adoc->collections)
    free(adoc->collections);
  adoc->numCollections = 0;
  adoc->collections = NULL;

  /* Free lists of sections */
  if (adoc->collList)
    free(adoc->collList);
  if (adoc->sectList)
    free(adoc->sectList);
  adoc->collList = NULL;
  adoc->sectList = NULL;
  adoc->numSections = 0;
  adoc->maxSections = 0;
  
  /* Free final comments */
  for (i = 0; i < adoc->numFinalCom; i++)
    if (adoc->finalComments[i])
      free(adoc->finalComments[i]);
  if (adoc->finalComments)
    free(adoc->finalComments);
  adoc->finalComments = NULL;
  adoc->numFinalCom = 0;
  adoc->inUse = 0;
}

/* Parses the characters in line up to (not including) end for the construct
   key = value.  Returns an allocated copy of key in key and value in value.
   Returns NULL in value if there is no value.  Returns 1 for other malformed
   lines, and -1 for other errors. */
static int parseKeyValue(char *line, char *end, char **key, char **value)
{
  char *valStart, *keyEnd;
  int keyLen, valLen;

  /* Eat spaces at start and end */
  while (line < end && (*line == ' ' || *line == '\t'))
    line++;
  while (line < end && (*(end - 1) == ' ' || *(end - 1) == '\t'))
    end--;
  if (line == end)
    return 1;
         
  /* Find delimiter.  If it is not there or no text before it, error */
  valStart = strstr(line, valueDelim);
  if (!valStart || valStart == line)
    return 1;
  
  /* Eat spaces after key */
  keyEnd = valStart;
  while (keyEnd > line && (*(keyEnd - 1) == ' ' || *(keyEnd - 1) == '\t'))
    keyEnd--;
  
  /* Eat spaces after the delimiter.  Allow an empty value */
  valStart += strlen(valueDelim);
  while (valStart < end && (*valStart == ' ' || *valStart == '\t'))
    valStart++;

  /* Allocate for strings and copy them */
  keyLen = keyEnd - line;
  *key = (char *)malloc(keyLen + 1);
  if (PipMemoryError(*key, "parseKeyValue"))
    return -1;
  memcpy(*key, line, keyLen);
  (*key)[keyLen] = 0x00;

  valLen = end - valStart;
  *value = NULL;
  if (valLen) {
    *value = (char *)malloc(valLen + 1);
    if (PipMemoryError(*value, "parseKeyValue"))
      return -1;
    memcpy(*value, valStart, valLen);
    (*value)[valLen] = 0x00;
  }
  return 0;
}

/* Looks up a key in a section and returns its index, or -1 if not present */
static int lookupKey(AdocSection *sect, const char *key)
{
  int i;
  if (!key)
    return -1;
  for (i = 0; i < sect->numKeys; i++)
    if (sect->keys[i] && !strcmp(key, sect->keys[i]))
      return i;
  return -1;
}

/* Looks up a collection in the autodoc by name; returns index or -1 if not
   there */
static int lookupCollection(Autodoc *adoc, const char *name)
{
  int i;
  if (!name || !adoc)
    return -1;
  for (i = 0; i < adoc->numCollections; i++)
    if (!strcmp(name, adoc->collections[i].name))
      return i;
  return -1;
}

/* Returns the section in the given collection and with given index, or NULL
   for error */
static AdocSection *getSection(const char *typeName, int sectInd)
{
  AdocCollection *coll;
  int collInd;
  if (!curAdoc || !typeName || sectInd < 0)
    return NULL;
  collInd = lookupCollection(curAdoc, typeName);
  if (collInd < 0)
    return NULL;
  coll = &curAdoc->collections[collInd];
  if (sectInd >= coll->numSections)
    return NULL;
  return (&coll->sections[sectInd]);
}

/* Adds the comments in the list of comments to the section, taking the
   strings without copying and setting numComments to zero */
static int addComments(AdocSection *sect, char **comments, int *numComments,
                       int index)
{
  int i, newNum = sect->numComments + *numComments;
  if (sect->numComments) {
    sect->comments = (char **)realloc(sect->comments, newNum * sizeof(char *));
    sect->comIndex = (int *)realloc(sect->comIndex, newNum * sizeof(int));
  } else {
    sect->comments = (char **)malloc(newNum * sizeof(char *));
    sect->comIndex = (int *)malloc(newNum * sizeof(int));
  }
  if (!sect->comments || !sect->comIndex) {
    PipMemoryError(NULL, "addComments");
    return -1;
  }
  for (i = 0; i < *numComments; i++) {
    sect->comments[sect->numComments] = comments[i];
    sect->comIndex[sect->numComments++] = index;
  }
  *numComments = 0;
  return 0;
}

/* Find the index of a section in the master list */
static int findSectionInAdocList(int collInd, int sectInd)
{
  int i;
  for (i = 0; i < curAdoc->numSections; i++)
    if (curAdoc->collList[i] == collInd && curAdoc->sectList[i] == sectInd)
      return i;
  return -1;
}
