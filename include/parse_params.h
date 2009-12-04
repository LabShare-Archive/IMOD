/*   parse_params.h  -  declarations for C functions in PIP package
 *
 *   Copyright (C) 2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *   $Id$
 */                                                                           

#ifndef PARSE_PARAMS_H
#define PARSE_PARAMS_H
#include <stdio.h>
#define PIP_INTEGER  1
#define PIP_FLOAT    2
#ifdef __cplusplus
extern "C" {
#endif

int PipInitialize(int numOpts);
int PipExitOnError(int useStdErr, char *prefix);
void exitError(char *format, ...);
void setExitPrefix(char *prefix);
int PipAddOption(char *optionString);
int PipNextArg(char *argString);
void PipNumberOfArgs(int *numOptArgs, int *numNonOptArgs);
int PipGetNonOptionArg(int argNo, char **arg);
int PipGetString(char *option, char **string);
int PipGetInteger(char *option, int *val);
int PipGetFloat(char *option, float *val);
int PipGetBoolean(char *option, int *val);
int PipGetTwoIntegers(char *option, int *val1, int *val2);
int PipGetTwoFloats(char *option, float *val1, float *val2);
int PipGetThreeIntegers(char *option, int *val1, int *val2, int *val3);
int PipGetThreeFloats(char *option, float *val1, float *val2, float *val3);
int PipGetIntegerArray(char *option, int *array, int *numToGet, int arraySize);
int PipGetFloatArray(char *option, float *array, int *numToGet, int arraySize);
int PipPrintHelp(char *progName, int useStdErr, int inputFiles,
		 int outputFiles);
int PipGetError(char **errString);
int PipSetError(char *errString);
int PipNumberOfEntries(char *option, int *numEntries);
void PipDone(void);
int PipParseInput(int argc, char *argv[], char *options[], int numOptions,
		  int *numOptArgs, int *numNonOptArgs);
int PipParseEntries(int argc, char *argv[], int *numOptArgs,
                    int *numNonOptArgs);
int PipReadOptionFile(char *progName, int helpLevel, int localDir);
int PipMemoryError(void *ptr, char *routine);
void PipAllowCommaDefaults(int val);
void PipSetManpageOutput(int val);
void PipEnableEntryOutput(int val);
void PipPrintEntries();
void PipSetSpecialFlags(int inCase, int inDone, int inStd, int inLines, 
                        int inAbbrevs);
int PipReadStdinIfSet(void);
int PipStartsWith(char *fullStr, char *subStr);
int PipGetInOutFile(char *option, int nonOptArgNo, char **filename);
void PipReadOrParseOptions(int argc, char *argv[], char *options[], 
                           int numOpts, char *progName, int minArgs, 
                           int numInFiles, int numOutFiles, int *numOptArgs,
                           int *numNonOptArgs, void (headerFunc)(char *));
int PipReadNextLine(FILE *pFile, char *lineStr, int strSize, char comment, 
                    int keepComments, int inLineComments, int *firstNonWhite);
int PipGetLineOfValues(char *option, char *strPtr, void *array, int valType, 
                       int *numToGet, int arraySize);
#ifdef __cplusplus
}
#endif
#endif

/*
$Log$
Revision 3.9  2007/08/03 16:40:04  mast
Fixes for clean compile

Revision 3.8  2007/06/22 04:58:42  mast
Added special flag function

Revision 3.7  2006/10/17 18:38:12  mast
Changed read line function

Revision 3.6  2006/10/16 16:17:48  mast
Made some functions global

Revision 3.5  2006/09/20 23:02:34  mast
Added callback for header usage function

Revision 3.4  2006/06/08 03:10:27  mast
Added higher-level C functions

Revision 3.3  2003/10/08 17:20:39  mast
New functions for autodoc files

Revision 3.2  2003/08/08 16:21:33  mast
Add functions for getting two numbers

Revision 3.1  2003/06/05 00:19:44  mast
*** empty log message ***

*/
