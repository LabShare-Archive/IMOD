/*
 *  $Id$
 *
 */
#ifndef IMODPLUGIN_H
#define IMODPLUGIN_H

/*
 *  Include all the files needed for imod plugins.
 */

struct privateStruct
{
     int privateData;
};

typedef struct { float x, y, z; }    Ipoint;
typedef struct { float a, b, c, d; } Iplane;

typedef struct privateStruct Imod;    /* The entire main model structure. */
typedef struct privateStruct Iview;   /* The view or camera structure.    */
typedef struct privateStruct Iobj;    /* Object data structure.           */
typedef struct privateStruct Imesh;   /* Mesh data structure.             */ 
typedef struct privateStruct Icont;   /* Contour data structure.          */
typedef struct privateStruct Ilabel;  /* Point label structure.           */

#define IMOD_NoIndex -1

#include "model.h"

/* Define structures needed for imod.h */
typedef struct privateStruct ImodView;

/* Define macro for import of functions under Windows */
#ifdef _WIN32
#define DLL_EX_IM _declspec(dllimport)
#else
#define DLL_EX_IM
#endif

/* Defines to exclude the private include files */
#define IMODP_H
#define IMODVIEWP_H
#define CONTROLP_H
#define IMODPLUGP_H
#define UNDOREDOP_H
#include "imod.h"
#include "control.h"
#include "imodplug.h"
#include "undoredo.h"

#endif
