/*****************************************************************************
 *                                                                           *
 *   FILE: diaP.h                                                            *
 *                                                                           *
 *   PURPOSE: Dialog library priviate header file.                           *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0  James Kremer  kremer@beagle.colorado.edu               *
 *                                                                           *
 *****************************************************************************
 *   Copyright (C) 1994,1995 by Boulder Laboratory for 3-Dimensional Fine    *
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

/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#ifndef DIAP_H
#define DIAP_H

#include <dia.h>

extern XtAppContext  Dia_context;
extern char         *Dia_title;
extern Widget        Dia_toplevel;

#ifdef __cplusplus
extern "C" {
#endif

void dia_destroy_shell(Widget widget, XtPointer client, XtPointer call);
void dia_map_cb(Widget w, XtPointer client_data, XtPointer call_data);     

#ifdef __cplusplus
}
#endif

#endif
