/*
 *  $Id$
 *
 */
#ifndef IMODPLUGIN_H
#define IMODPLUGIN_H

/*
 *  Include all the files needed for imod plugins.
 *  Define NO_PRIVATE_STRUCT to access all model structures directly
 */

#ifndef NO_PRIVATE_STRUCT
struct privateStruct
{
     int privateData;
};

typedef struct privateStruct Imod;    /* The entire main model structure. */
typedef struct privateStruct Iview;   /* The view or camera structure.    */
typedef struct privateStruct Iobj;    /* Object data structure.           */
typedef struct privateStruct Imesh;   /* Mesh data structure.             */ 
typedef struct privateStruct Icont;   /* Contour data structure.          */
typedef struct privateStruct Ilabel;  /* Point label structure.           */
typedef struct privateStruct IrefImage; /* Image reference structure.     */
typedef struct privateStruct Iobjview;  /* Object view structure.         */

#define IMODELP_H
#endif

#define IMOD_NoIndex -1

#include "imodel.h"

#ifndef NO_PRIVATE_STRUCT

/* Define structures needed for imod.h */
typedef struct privateStruct ImodView;
#endif

/* Define macro for import of functions under Windows */
#ifdef _WIN32
#define DLL_EX_IM _declspec(dllimport)
#else
#define DLL_EX_IM
#endif

/* Defines to exclude the private include files */
#ifndef NO_PRIVATE_STRUCT
#define IMODP_H
#define IMODVIEWP_H
#define CONTROLP_H
#define IMODPLUGP_H
#define UNDOREDOP_H
#endif
#include "imod.h"
#include "control.h"
#include "imodplug.h"
#include "undoredo.h"

#endif
