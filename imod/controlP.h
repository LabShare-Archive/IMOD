/*   controlP.h  -  declarations for private functions in control.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$
Log at end of file
*/
#ifndef CONTROLP_H
#define CONTROLP_H

/* Each window that shows the view below uses this control 
 * structure to have the view update the window.
 */
typedef struct
{
  void *userData;
  ImodControlProc draw_cb;
  ImodControlProc close_cb;
  ImodControlKey  key_cb;
  int  id;
  int  status;
  
}ImodControl;

/* This structure sits inside of each view and is the
 * master controller.
 */
typedef struct imod_control_list
{ 
  Ilist *      list;
  int          active;
  int          top;
  int          reason;
  int          workID;
}ImodControlList;


/* private control functions. */
void ivwControlListDrawCancel(ImodView *iv);
void ivwControlListDraw(ImodView *iv, int reason);
void ivwControlListDelete(ImodView *iv);
void ivwWorkProc(ImodView *iv);
QRect ivwRestorableGeometry(QWidget *widget);

#endif
/*
$Log$
*/
