/*   parse_params.h  -  declarations for C functions in PIP package
 *
 *   Copyright (C) 2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.2  2003/08/08 16:21:33  mast
Add functions for getting two numbers

Revision 3.1  2003/06/05 00:19:44  mast
*** empty log message ***

*/
#ifndef PARSE_PARAMS_H
#define PARSE_PARAMS_H
#ifdef __cplusplus
extern "C" {
#endif

int PipInitialize(int numOpts);
int PipExitOnError(int useStdErr, char *prefix);
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
#ifdef __cplusplus
}
#endif
#endif
