/*  IMOD VERSION 2.10
 *
 *  $Id$
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
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
/* imod_io.h */

#ifndef IMOD_IO_H
#define IMOD_IO_H

extern char Statstring[128];
extern char Inputstring[128];

/* Functions */
int imod_model_changed(struct Mod_Model *imodel);
void imod_cleanup_autosave(void);
int reqask(char *prompt);
int imod_autosave(struct Mod_Model *mod);
struct Mod_Model *LoadModel(FILE *mfin, char *filename);
int SaveModel(struct Mod_Model *mod);
int SaveasModel(struct Mod_Model *mod);
int SaveModelQuit(struct Mod_Model *mod);
unsigned char **imod_io_image_load(ImodImageFile *im,
				   struct LoadInfo *li,
				   void (*func)(char *));

#endif
