/*  IMOD VERSION 2.50
 *
 *  $Id$
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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

#ifndef BD_IPROC_H_
#define BD_IPROC_H_


#ifdef __cplusplus
extern "C" {
#endif


#define PROC_BACKGROUND 0
#define PROC_FOREGROUND 255


  typedef struct
  {
    diaDialog     *dia;
    ImodView      *vw;        /* image data to model                       */
    unsigned char *idata;     /* Image data processing buffer.             */
    int           idatasec;   /* data section. */
    int           idatatime;  /* time value of section */
    int           procnum;
    int           modified;   /* flag that section data are modified */
    Widget        frame;
    Widget        curcont;   /* current control widget. */

    int           threshold;
    int           edge;

  } ImodIProc;


  typedef struct
  {
    char *name;         /* Name of index */
    void (*cb)(Widget, XtPointer, XtPointer);       /* callback */
    void (*mkwidget)(Widget, XtPointer, XtPointer);
    XtPointer client;
    Widget control;
  } ImodIProcData;

  int inputIProcOpen(struct ViewInfo *vw);

#ifdef __cplusplus
}
#endif

#endif /* BD_IPROC_H_ */
