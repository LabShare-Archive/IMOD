/*  IMOD VERSION 2.02
 *
 *  $Id$
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
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
#include "imod.h"
#include "control.h"
#include "imodplug.h"


#endif
