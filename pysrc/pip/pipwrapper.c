/*
 * pipwrapper.c - python extension wrapper for the IMOD PIP library
 * June 2005 - Tor Mohling
 */
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

/* Include the Python C API definitions */
#include "Python.h"

/* Include the PIP API */
#include "parse_params.h"

/*
 * conventions for use with python:
 *  - ALL functions return a SINGLE object;  for functions with multiple
 *    return values this object MIGHT be a TUPLE.
 *    - If an error occurs, None is returned instead of the expected
 *      object.  This is no problem if you set PIP to exit on error.  
 *      Otherwise it is important that you grab the return value(s) like so:
 *
 *        retval = PipFunction()
 *        if retval == None:
 *            ERROR
 *        (x,y) = retval  # or whatever..
 *
 *  - All functions return the None object on error and set the
 *    global PipErrno to the status value returned by the real PIP
 *    function
 */

#define PIPMAXARRAY 64
static int PipErrNo = 0;


/*
 * these functions are all straight translations - i.e. no
 * re-arranging of parameters and return-values is necessary
 */

/* int PipInitialize(int numOpts); */
PyObject *pip_PipInitialize(PyObject *self, PyObject *args) 
{
  int numOpts;
  if (!PyArg_ParseTuple(args, "i:PipInitialize", &numOpts))
    return(NULL);

  PipErrNo = PipInitialize(numOpts);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("i", 0);
}

/* int PipExitOnError(int useStdErr, char *prefix); */
PyObject *pip_PipExitOnError(PyObject *self, PyObject *args)
{
  int useStdErr;
  char *prefix;
  if (!PyArg_ParseTuple(args, "is:PipExitOnError", &useStdErr, &prefix))
    return(NULL);

  PipErrNo = PipExitOnError(useStdErr, prefix);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("i", 0);
}

/* int PipAddOption(char *optionString); */
/* IGNORE - Fortran only */

/* int PipNextArg(char *argString); */
/* IGNORE - Fortran only */

/* int PipPrintHelp(char *progName,int useStdErr, int inputFiles, int outputFiles); */
PyObject *pip_PipPrintHelp(PyObject *self, PyObject *args) 
{
  int useStdErr, inputFiles, outputFiles;
  char *progName;
  if (!PyArg_ParseTuple(args, "siii:PipPrintHelp", &progName, &useStdErr, 
                        &inputFiles, &outputFiles)) 
    return(NULL);

  PipErrNo = PipPrintHelp(progName, useStdErr, inputFiles, outputFiles);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("i", 0);
}

/* int PipSetError(char *errString); */
PyObject *pip_PipSetError(PyObject *self, PyObject *args) 
{
  char *errString;
  if (!PyArg_ParseTuple(args, "s:PipSetError", &errString))
    return(NULL);

  PipErrNo = PipSetError(errString);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("i", 0);
}

/* void setExitPrefix(char *prefix); */
PyObject *pip_setExitPrefix(PyObject *self, PyObject *args) 
{
  char *errString;
  if (!PyArg_ParseTuple(args, "s:setExitPrefix", &errString))
    return(NULL);

  setExitPrefix(errString);
  return Py_BuildValue("");
}

/* void exitError(char *format, ...); */
/* exitError(string)  */
PyObject *pip_exitError(PyObject *self, PyObject *args) 
{
  char *errString;
  if (!PyArg_ParseTuple(args, "s:exitError", &errString))
    exitError("Error string not available");
  exitError(errString);
  return Py_BuildValue("");
}



/* void PipDone(void); */
PyObject *pip_PipDone(PyObject *self, PyObject *args) 
{
  if (!PyArg_ParseTuple(args, ":PipDone"))
    return(NULL);

  PipDone();
  return Py_BuildValue("");
}

/* int PipReadOptionFile(char *progName, int helpLevel, int localDir); */
/* NOTE:  FIX helpLevel=1, localDir=0 */
/* sts = PipReadOptionFile(progName) */
PyObject *pip_PipReadOptionFile(PyObject *self, PyObject *args)
{
  char *progName;
  if (!PyArg_ParseTuple(args, "s:PipReadOptionFile", &progName))
    return(NULL);

  PipErrNo = PipReadOptionFile(progName, 1, 0);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("i", 0);
}

/* void PipAllowCommaDefaults(int val); */
/* IGNORE - Fortran only */

/* void PipSetManpageOutput(int val); */
/* IGNORE - only used in special doc-creator program */

/* int PipMemoryError(void *ptr, char *routine); */
/* IGNORE - Fortran only */



/*
 * The remaining functions are all modified from the original C defs;
 * in C they return values in parameters-by-reference which python can't
 * really do.
 */

/* void PipNumberOfArgs(int *numOptArgs, int *numNonOptArgs); */
/*  numOptArgs, numNonOptArgs = PipNumberOfArgs()  */
PyObject *pip_PipNumberOfArgs(PyObject *self, PyObject *args)
{
  int numOptArgs, numNonOptArgs;
  if (!PyArg_ParseTuple(args, ":PipNumberOfArgs"))
    return(NULL);

  PipNumberOfArgs(&numOptArgs, &numNonOptArgs);
  return Py_BuildValue("(ii)", numOptArgs, numNonOptArgs);
}

/* int PipGetNonOptionArg(int argNo, char **arg); */
/*  opt = PipGetNonOptionArg(argNo) */
PyObject *pip_PipGetNonOptionArg(PyObject *self, PyObject *args) {
  int argNo;
  char *arg;
  PyObject *retval;

  if (!PyArg_ParseTuple(args, "i:PipGetNonOptionArg", &argNo))
    return(NULL);

  PipErrNo = PipGetNonOptionArg(argNo, &arg);
  if (PipErrNo < 0)
    return Py_BuildValue("");
 
  retval = Py_BuildValue("s", arg);
  free(arg);
  return retval;
}

/* int PipGetString(char *option, char **string); */
/*  sVal = PipGetString(opt,str)      NOTE: this wrapper may need to free */
PyObject *pip_PipGetString(PyObject *self, PyObject *args) 
{
  char *option;
  char *string;
  char *tstr;
  PyObject *retval;

  if (!PyArg_ParseTuple(args, "ss:PipGetString", &option, &string))
    return(NULL);

  PipErrNo = PipGetString(option, &tstr);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  else if (PipErrNo > 0)
    return Py_BuildValue("s", string);

  retval = Py_BuildValue("s", tstr);
  free(tstr);
  return retval;
}

/* int PipGetBoolean(char *option, int *val); */
/*  bVal = PipGetBoolean(opt,val) */
PyObject *pip_PipGetBoolean(PyObject *self, PyObject *args) 
{
  char *option;
  int val;
  if (!PyArg_ParseTuple(args, "si:PipGetBoolean", &option, &val))
    return(NULL);

  PipErrNo = PipGetBoolean(option, &val);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("i", val);
}


/* int PipGetInteger(char *option, int *val); */
/*  iVal = PipGetInteger(opt,ival1) */
PyObject *pip_PipGetInteger(PyObject *self, PyObject *args)
{
  char *option;
  int ival1;

  if (!PyArg_ParseTuple(args, "si:PipGetInteger", &option, &ival1))
    return(NULL);

  PipErrNo = PipGetInteger(option, &ival1);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("i", ival1);
}

/* int PipGetTwoIntegers(char *option, int *val1, int *val2); */
/*  iVal1, iVal2 = PipGetTwoIntegers(opt,ival1,ival2) */
PyObject *pip_PipGetTwoIntegers(PyObject *self, PyObject *args) 
{
  char *option;
  int ival1, ival2;
  if (!PyArg_ParseTuple(args, "sii:PipGetTwoIntegers", &option, &ival1,
                        &ival2))
    return(NULL);

  PipErrNo = PipGetTwoIntegers(option, &ival1, &ival2);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("(ii)", ival1, ival2);
}

/* int PipGetThreeIntegers(char *option, int *val1, int *val2, int *val3); */
/*  (iVal1, iVal2, iVal3) = PipGetThreeIntegers(opt,ival1,ival2,ival3) */
PyObject *pip_PipGetThreeIntegers(PyObject *self, PyObject *args) 
{
  char *option = "None";
  int ival1, ival2, ival3;
  if (!PyArg_ParseTuple(args, "siii:PipGetThreeIntegers", &option, &ival1, 
                        &ival2, &ival3)) 
    return(NULL);

  PipErrNo = PipGetThreeIntegers(option, &ival1, &ival2, &ival3);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("(iii)", ival1, ival2, ival3);
}

/* int PipGetIntegerArray(char *option, int *array, int *numToGet, int arraySize); */
/*  [ iVal ... ] = PipGetIntegerArray(opt, cnt) */
/*  NOTE WELL - XXX fixed size for max array */
PyObject *pip_PipGetIntegerArray(PyObject *self, PyObject *args) 
{
  int i, cnt;
  char *option;
  int ints[PIPMAXARRAY];
  PyObject *tval = 0;
  PyObject *pyints = 0;
  if (!PyArg_ParseTuple(args, "si:PipGetIntegerArray", &option, &cnt))
    return(NULL);

  PipErrNo = PipGetIntegerArray(option, &ints[0], &cnt, PIPMAXARRAY);
  if (PipErrNo < 0)
    return Py_BuildValue("");

  pyints = PyList_New(cnt);
  for (i=0 ; i<cnt ; i++) {
    tval = PyInt_FromLong((long)ints[i]);
    PyList_SetItem(pyints, i, tval);
  }
  return Py_BuildValue("O", pyints);
}

/* int PipGetFloat(char *option, float *val); */
/*  fVal = PipGetFloat(opt,fval) */
PyObject *pip_PipGetFloat(PyObject *self, PyObject *args) 
{
  char *option;
  float fval1;
  if (!PyArg_ParseTuple(args, "sf:PipGetFloat", &option, &fval1))
    return(NULL);

  PipErrNo = PipGetFloat(option, &fval1);
  if (PipErrNo < 0) 
    return Py_BuildValue("");
  return Py_BuildValue("f", fval1);
}

/* int PipGetTwoFloats(char *option, float *val1, float *val2); */
/*  fVal1, fVal2 = PipGetTwoFloats(opt,fval1,fval2) */
PyObject *pip_PipGetTwoFloats(PyObject *self, PyObject *args) {
  char *option;
  float fval1, fval2;
  if (!PyArg_ParseTuple(args, "sff:PipGetTwoFloats", &option, &fval1, &fval2))
    return(NULL);

  PipErrNo = PipGetTwoFloats(option, &fval1, &fval2);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("(ff)", fval1, fval2);
}

/* int PipGetThreeFloats(char *option, float *val1, float *val2, float *val3); */
/*  fVal1, fVal2, fVal3 = PipGetThreeFloats(opt,fval1,fval2,fval3) */
PyObject *pip_PipGetThreeFloats(PyObject *self, PyObject *args) {
  char *option;
  float fval1, fval2, fval3;
  if (!PyArg_ParseTuple(args, "sfff:PipGetThreeFloats", &option, &fval1, 
                        &fval2, &fval3))
    return(NULL);

  PipErrNo = PipGetThreeFloats(option, &fval1, &fval2, &fval3);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("(fff)", fval1, fval2, fval3);
}

/* int PipGetFloatArray(char *option, float *array, int *numToGet, int arraySize); */
/*  [ fVal ... ] = PipGetFloatArray(opt, cnt) */
/*  NOTE WELL - XXX fixed size for max array */
PyObject *pip_PipGetFloatArray(PyObject *self, PyObject *args) {
  int i, cnt;
  char *option;
  float flts[PIPMAXARRAY];
  PyObject *tval = 0;
  PyObject *pyflts = 0;
  if (!PyArg_ParseTuple(args, "si:PipGetFloatArray", &option, &cnt))
    return(NULL);

  PipErrNo = PipGetFloatArray(option, &flts[0], &cnt, PIPMAXARRAY);
  if (PipErrNo < 0)
    return Py_BuildValue("");

  pyflts = PyList_New(cnt);
  for (i=0 ; i<cnt ; i++) {
    tval = PyFloat_FromDouble((double)flts[i]);
    PyList_SetItem(pyflts, i, tval);
  }
  return Py_BuildValue("O", pyflts);
}

/* int PipGetError(char **errString); */
/*  errStr = PipGetError()   NOTE: this wrapper may need to free */
PyObject *pip_PipGetError(PyObject *self, PyObject *args)
{
  char *errStr;
  PyObject *retval;
  if (!PyArg_ParseTuple(args, ":PipGetError"))
    return(NULL);

  PipErrNo = PipGetError(&errStr);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  retval = Py_BuildValue("s", errStr);
  free(errStr);
  return retval;
}

/* int PipNumberOfEntries(char *option, int *numEntries); */
/*  numEntries = PipNumberOfEntries(opt) */
PyObject *pip_PipNumberOfEntries(PyObject *self, PyObject *args)
{
  int numEntries;
  char *option;
  if (!PyArg_ParseTuple(args, "s:PipNumberOfEntries", &option))
    return(NULL);

  PipErrNo = PipNumberOfEntries(option, &numEntries);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("i", numEntries);
}

/*
 * int PipParseInput(int argc, char *argv[], char *options[], int numOptions,
 *                   int *numOptArgs, int *numNonOptArgs);
 */
/* numOptArgs, numNonOptArgs = PipParseInput(sys.argv, options) */
PyObject *pip_PipParseInput(PyObject *self, PyObject *args)
{
  int i, ac, numOpts, numOptArgs, numNonOptArgs;
  PyObject *argvec = 0;
  PyObject *optvec = 0;
  PyObject *tmparg = 0;
  char **av, **ov;
  if (!PyArg_ParseTuple(args, "O!O!:PipParseEntries", &PyList_Type, &argvec, 
                        &PyList_Type, &optvec))
    return(NULL);

  /* get the length of the arg list and verify >= 0 */
  ac = PyList_Size(argvec);   
  if (ac < 0)
    return(NULL);

  if ((av = malloc((size_t) ac * sizeof(char *))) == NULL) {
    PyErr_NoMemory();
    return(NULL); 
  }

  for (i=0; i<ac; i++)
    av[i] = PyString_AsString(PyList_GetItem(argvec, i));

  /* get the length of the opt list and vfy >= 0 */
  numOpts = PyList_Size(optvec);
  if (numOpts < 0)
    return(NULL);

  if ((ov = malloc((size_t) numOpts * sizeof(char *))) == NULL) {
    PyErr_NoMemory();
    return(NULL); 
  }
 
  for (i=0; i<numOpts; i++) {
    tmparg = PyList_GetItem(optvec, i);
    ov[i] = PyString_AsString(tmparg);
  }
  PipErrNo = PipParseInput(ac, av, ov, numOpts, &numOptArgs, &numNonOptArgs);
  free(av);
  free(ov);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("(ii)", numOptArgs, numNonOptArgs);
}

/*
 * int PipParseEntries(int argc, char *argv[], int *numOptArgs,
 *                     int *numNonOptArgs);
 */
/* numOptArgs, numNonOptArgs = PipParseEntries(sys.argv) */
PyObject *pip_PipParseEntries(PyObject *self, PyObject *args)
{
  int i, ac, numOptArgs, numNonOptArgs;
  PyObject *argvec = 0;
  PyObject *tmparg = 0;
  char **av;
  if (!PyArg_ParseTuple(args, "O!:PipParseEntries", &PyList_Type, &argvec))
    return(NULL);

  /* get the length of the list and verify >= 0 */
  ac = PyList_Size(argvec); 
  if (ac < 0)
    return(NULL);

  if ((av = malloc((size_t) ac * sizeof(char *))) == NULL) {
    PyErr_NoMemory();
    return(NULL); 
  }

  for (i=0; i<ac; i++) {
    tmparg = PyList_GetItem(argvec, i);
    av[i] = PyString_AsString(tmparg);
  }
  PipErrNo = PipParseEntries(ac, av, &numOptArgs, &numNonOptArgs);
  free(av);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("(ii)", numOptArgs, numNonOptArgs);
}

/* void PipReadOrParseOptions(int argc, char *argv[], char *options[], 
   int numOpts, char *progName, 
   int minArgs, int numInFiles, int numOutFiles,
   int *numOptArgs, int *numNonOptArgs, void (headerFunc)(char *)); */
/* numOptArgs, numNonOptArgs = PipReadOrParseOptions(sys.argv, options, 
   progName, minArgs, numInFiles, numOutFiles) */
PyObject *pip_PipReadOrParseOptions(PyObject *self, PyObject *args)
{
  int i, ac, numOpts,  minArgs, numInFiles, numOutFiles, numOptArgs;
  int numNonOptArgs;
  char *progName;
  PyObject *argvec = 0;
  PyObject *optvec = 0;
  PyObject *tmparg = 0;
  char **av, **ov;
  if (!PyArg_ParseTuple(args, "O!O!siii:PipReadOrParseOptions", &PyList_Type, 
                        &argvec, &PyList_Type, &optvec, &progName, &minArgs,
                        &numInFiles, &numOutFiles))
    return(NULL);

  /* get the length of the arg list and verify >= 0 */
  ac = PyList_Size(argvec);   
  if (ac < 0)
    return(NULL);

  if ((av = malloc((size_t) ac * sizeof(char *))) == NULL) {
    PyErr_NoMemory();
    return(NULL); 
  }

  for (i=0; i<ac; i++)
    av[i] = PyString_AsString(PyList_GetItem(argvec, i));

  /* get the length of the opt list and vfy >= 0 */
  numOpts = PyList_Size(optvec);
  if (numOpts < 0)
    return(NULL);

  if ((ov = malloc((size_t) numOpts * sizeof(char *))) == NULL) {
    PyErr_NoMemory();
    return(NULL); 
  }
 
  for (i=0; i<numOpts; i++) {
    tmparg = PyList_GetItem(optvec, i);
    ov[i] = PyString_AsString(tmparg);
  }
  PipReadOrParseOptions(ac, av, ov, numOpts, progName, minArgs,
                                   numInFiles, numOutFiles, &numOptArgs,
                                   &numNonOptArgs, NULL);
  free(av);
  free(ov);
  if (PipErrNo < 0)
    return Py_BuildValue("");
  return Py_BuildValue("(ii)", numOptArgs, numNonOptArgs);
}

/* int PipGetInOutFile(char *option, int nonOptArgNo, char **filename); */
/*  filename = PipGetInOutFile(option, argNo) */
PyObject *pip_PipGetInOutFile(PyObject *self, PyObject *args) {
  int argNo;
  char *arg;
  char *option;
  PyObject *retval;

  if (!PyArg_ParseTuple(args, "si:PipGetInOutFile", &option, &argNo))
    return(NULL);

  PipErrNo = PipGetInOutFile(option, argNo, &arg);
  if (PipErrNo < 0)
    return Py_BuildValue("");
 
  retval = Py_BuildValue("s", arg);
  free(arg);
  return retval;
}


/*
 * method table maps function names to wrapper-functions
 */
static PyMethodDef pipmethods[] = {
  { "PipInitialize",         pip_PipInitialize,         METH_VARARGS },
  { "PipExitOnError",        pip_PipExitOnError,        METH_VARARGS },
  { "PipPrintHelp",          pip_PipPrintHelp,          METH_VARARGS },
  { "PipSetError",           pip_PipSetError,           METH_VARARGS },
  { "setExitPrefix",         pip_setExitPrefix,         METH_VARARGS },
  { "exitError",             pip_exitError,             METH_VARARGS },
  { "PipDone",               pip_PipDone,               METH_VARARGS },
  { "PipReadOptionFile",     pip_PipReadOptionFile,     METH_VARARGS },
  { "PipNumberOfArgs",       pip_PipNumberOfArgs,       METH_VARARGS },
  { "PipGetNonOptionArg",    pip_PipGetNonOptionArg,    METH_VARARGS },
  { "PipGetString",          pip_PipGetString,          METH_VARARGS },
  { "PipGetBoolean",         pip_PipGetBoolean,         METH_VARARGS },
  { "PipGetInteger",         pip_PipGetInteger,         METH_VARARGS },
  { "PipGetTwoIntegers",     pip_PipGetTwoIntegers,     METH_VARARGS },
  { "PipGetThreeIntegers",   pip_PipGetThreeIntegers,   METH_VARARGS },
  { "PiPGetIntegerArray",    pip_PipGetIntegerArray,    METH_VARARGS },
  { "PipGetFloat",           pip_PipGetFloat,           METH_VARARGS },
  { "PipGetTwoFloats",       pip_PipGetTwoFloats,       METH_VARARGS },
  { "PipGetThreeFloats",     pip_PipGetThreeFloats,     METH_VARARGS },
  { "PiPGetFloatArray",      pip_PipGetFloatArray,      METH_VARARGS },
  { "PipGetError",           pip_PipGetError,           METH_VARARGS },
  { "PipNumberOfEntries",    pip_PipNumberOfEntries,    METH_VARARGS },
  { "PipParseInput",         pip_PipParseInput,         METH_VARARGS },
  { "PipParseEntries",       pip_PipParseEntries,       METH_VARARGS },
  { "PipReadOrParseOptions", pip_PipReadOrParseOptions, METH_VARARGS },
  { NULL, NULL }
};

/*
 * Pip module initialization function
 */
PyMODINIT_FUNC
initpip(void) {
  Py_InitModule("pip", pipmethods);
}

