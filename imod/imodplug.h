/*   imodplug.h  -  declarations for imodplug.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 1.1.2.3  2003/01/29 01:43:57  mast
    switch back to extern C

    Revision 1.1.2.2  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 1.1.2.1  2003/01/13 01:06:53  mast
    Initial creation

*/
#ifndef IMODPLUG_H
#define IMODPLUG_H

class QPopupMenu;
class QKeyEvent;
#ifndef IMODP_H
typedef struct ViewInfo ImodView;
#endif

/* It looks like these need to be C linkage on the SGI */
extern "C" {

// Formerly in imod.h

/*************************** Setup Functions *********************************/

/*
 * Function must be defined by plugin.
 *
 * Returns the name of the plugin.
 * Bit flags for the type of plugins are returned in the type.
 * Not all of imod's data structures may be initialized at the time of
 * this function call so no initialization should be done.
 */
char *imodPlugInfo(int *type);

/*
 * Generic Plugin interface.
 * IMOD will call this functions on your behalf at 
 * well defined times.
 */
void imodPlugExecuteType(ImodView *inView, int inType, int inReason);

/* Menu execution function for plugins with the IMOD_PLUG_MENU bit set.
 * This function will be called if available, if not defined then. 
 * the following call will be made.
 * imodPlugExecuteType(inView, IMOD_PLUG_MENU, IMOD_REASON_EXECUTE);
 */
void imodPlugExecute(ImodView *vw);

/* Key input callback function to be defined by plugins with the
 * IMOD_PLUG_KEYS bit set.
 * This function can be used to override imod hot key settings.
 * A nonzero return value indicates that the plugin handled the input key.
 * and that no other action should be taken by the imod program.
 * A zero return value indicates that imod should process the key as usual.
 */
int imodPlugKeys(ImodView *vw, QKeyEvent *event);

// Functions from imodP.h
int imodPlugInit(void);
int imodPlugLoaded(int type);
int imodPlugCall(ImodView *vw, int type, int reason);
void imodPlugMenu(QPopupMenu *parent); /* build plugin menu. */
int imodPlugHandleKey(ImodView *vw, QKeyEvent *event);
void imodPlugOpen(int item);

}
#endif
