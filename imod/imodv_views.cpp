/*
 *  imodv_views.cpp -- Edit, store, and access view list in model
 *                     Companion form class is imodvViewsForm in 
 *                     formv_views.ui[.h]
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdio.h>
#include <string.h>
#include "formv_views.h"
#include "imodv.h"
#include "dia_qtutils.h"
#include "imod.h"
#include "imod_input.h"
#include "imodv_gfx.h"
#include "imodv_input.h"
#include "imodv_menu.h"
#include "imodv_control.h"
#include "imodv_light.h"
#include "imodv_views.h"
#include "imodv_depthcue.h"
#include "imodv_objed.h"
#include "control.h"
#include "undoredo.h"


struct imodv_viewed
{
  imodvViewsForm *dia;
  ImodvApp  *a;
};

static struct imodv_viewed viewStruct;
static struct imodv_viewed *ved = &viewStruct;

static void build_list(ImodvApp *a);
static void imodvUpdateView(ImodvApp *a);
static void manage_world_flags(ImodvApp *a, Iview *view);

static int auto_store = 1;

static void imodvUpdateView(ImodvApp *a)
{
  a->wireframe = (a->imod->view->world & VIEW_WORLD_WIREFRAME) ? 1 : 0;
  a->lowres = (a->imod->view->world & VIEW_WORLD_LOWRES) ? 1 : 0;
  a->lighting = (a->imod->view->world & VIEW_WORLD_LIGHT) ? 1 : 0;
  imodvControlSetView(a);
  imodvObjedNewView();
  imodvDepthCueSetWidgets();
  imodvMenuLight(a->imod->view->world & VIEW_WORLD_LIGHT);
  imodvMenuWireframe(a->imod->view->world & VIEW_WORLD_WIREFRAME);
  imodvMenuLowres(a->imod->view->world & VIEW_WORLD_LOWRES);
}

/* Update when a different model is being displayed (setView true) or when
   a model is being restored by undo/redo (setView false) */
void imodvUpdateModel(ImodvApp *a, bool setView)
{
  imodvUpdateView(a);
  if (!ved->dia)
    return;
  ved->dia->removeAllItems();
  build_list(a);
  if (setView)
    imodvViewsGoto(a->imod->cview - 1, false, false);
  ved->dia->selectItem(a->imod->cview - 1, true);
  ved->dia->setAutostore(auto_store);
}

/* set the current application flags into the current view */
static void manage_world_flags(ImodvApp *a, Iview *view)
{
  if (a->lighting)
    view->world |= VIEW_WORLD_LIGHT;
  else
    view->world &= ~VIEW_WORLD_LIGHT;
  if (a->wireframe)
    view->world |= VIEW_WORLD_WIREFRAME;
  else
    view->world &= ~VIEW_WORLD_WIREFRAME;
  if (a->lowres)
    view->world |= VIEW_WORLD_LOWRES;
  else
    view->world &= ~VIEW_WORLD_LOWRES;
}

/* Automatically store the current view if auto_store is set */
void imodvAutoStoreView(ImodvApp *a)
{
  /* This is a common place to complete the set of object views */
  imodObjviewComplete(a->imod);

  if (!auto_store || !a->imod->cview)
    return;

  manage_world_flags(a, a->imod->view);
  imodViewStore(a->imod, a->imod->cview);
}

void imodvViewsHelp()
{
  imodShowHelpPage("modvViewEdit.html");
  return;
}

// Done: tell the box to close
void imodvViewsDone()
{
  ved->dia->close();
}
  
// Closing - do we need to autostore view?
void imodvViewsClosing()
{
  imodvDialogManager.remove((QWidget *)ved->dia);
  ved->dia = NULL;
}

// Open, close, or raise the dialog box
void imodvViewEditDialog(ImodvApp *a, int state)
{
  QString qstr;
  char *window_name;
  static int first = 1;

  if (first){
    ved->dia = NULL;
    first = 0;
  }
  if (!state){
    if (ved->dia)
      ved->dia->close();
    return;
  }
  if (ved->dia){
    ved->dia->raise();
    return;
  }
  ved->a = a;

  ved->dia = new imodvViewsForm(imodvDialogManager.parent(IMODV_DIALOG),
                                Qt::Window);

  // Set title bar
  window_name = imodwEithername("3dmodv Views: ", a->imod->fileName, 1);
  qstr = window_name;
  if (window_name)
    free(window_name);
  if (!qstr.isEmpty())
    ved->dia->setWindowTitle(qstr);

  build_list(a);
  ved->dia->setAutostore(auto_store);
  ved->dia->selectItem(a->imod->cview - 1, true);
  imodvDialogManager.add((QWidget *)ved->dia, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)ved->dia, IMODV_DIALOG);
}

// Save model: call appropriate routine depending on whether imod or standalone
void imodvViewsSave()
{
  if (ved->a->standalone)
    imodvFileSave();
  else
    inputSaveModel(App->cvi);
}

/*
 * The goto view callback.
 * item is numbered from 0 and incremented internally
 */
void imodvViewsGoto(int item, bool draw, bool regChg)
{
  item++;
  if (!ved->a->imod)
    return;

  /* If registering changes, do so before the autostore to capture revertable
     state */
  if (regChg) {
    if (!ved->a->standalone)
      ved->a->vi->undo->modelChange(UndoRedo::ViewChanged);
    imodvFinishChgUnit();
  }

  /* If changing views, store the current view before changing */
  if (item != ved->a->imod->cview)
    imodvAutoStoreView(ved->a);

  ved->a->imod->cview = item;

  imodViewUse(ved->a->imod);
  imodvNewModelAngles(&ved->a->imod->view->rot);
  imodvDrawImodImages(ved->a->linkToSlicer);

  imodvUpdateView(ved->a);
  if (draw) 
    imodvDraw(Imodv);
}


/* 
 * The store view callback.
 * Sets the view in the window to the current view. 
 * item is numbered from 0 and incremented internally
 */

void imodvViewsStore(int item)
{
  item++;
  Iview *view = ved->a->imod->view;

  /* 11/20/04: eliminated autostore if changing views; this is always current
     view */
  imodvRegisterModelChg();
  imodvFinishChgUnit();

  ved->a->imod->cview = item;
     
  manage_world_flags(ved->a, view);

  imodViewStore(ved->a->imod, item);
}

/* Make a new view, the form already found a unique label  */
void imodvViewsNew(const char *label)
{
  int cview;
  Iview *view = ved->a->imod->view;

  if (!ved->a->imod) return;

  imodvRegisterModelChg();
  imodvFinishChgUnit();

  imodvAutoStoreView(ved->a);

  cview = ved->a->imod->cview;
  imodViewModelNew(ved->a->imod);
  view = ved->a->imod->view;
  ved->a->imod->cview = ved->a->imod->viewsize - 1;
    
  if (cview == ved->a->imod->cview) {
    imodError(NULL,"3dmodv: error creating view\n");
    return; /* no view created. */
  }

  manage_world_flags(ved->a, view);

  cview = ved->a->imod->cview;

  strcpy(ved->a->imod->view[cview].label, label);
  imodViewStore(ved->a->imod, cview);

  ved->dia->addItem(ved->a->imod->view[cview].label);
  ved->dia->selectItem(cview - 1, true);
}

/* Delete a view
 * item is numbered from 0 and incremented internally, newCurrent is also
 */
void imodvViewsDelete(int item, int newCurrent)
{
  item++;
  Imod *imod = ved->a->imod;
  int i;

  if (!imod || item <= 0 || imod->viewsize < 2)
    return;

  if (!ved->a->standalone)
    ved->a->vi->undo->modelChange(UndoRedo::ViewChanged);
  imodvFinishChgUnit();

  // Need to free object views of the view being deleted
  if (imod->view[item].objvsize)
    free (imod->view[item].objview);

  // How about deleting the existing view?  No need, if the array grows
  // again the space is reused
  // 7/24/03 discovered it was copying from non-existent view at top
  for (i = item; i < imod->viewsize - 1; i++) {
    imod->view[i] = imod->view[i+1];
  }

  imod->viewsize--;
  imod->cview = (newCurrent >= 0 ? newCurrent : 0) + 1;
  imodvViewsGoto(imod->cview - 1, true, false);

  /* DNM 7/31/03: at least on RH 9, new selection doesn't show after deletion
     unless select another item first */
  if (imod->viewsize > 1)
    ved->dia->selectItem(imod->cview > 1 ? imod->cview - 2 : 1, true);
  ved->dia->selectItem(imod->cview - 1, true);
}

// A new label has been entered
void imodvViewsLabel(const char *label, int item)
{
  imodvRegisterModelChg();
  imodvFinishChgUnit();
  strcpy( ved->a->imod->view[item + 1].label, label);
}

void imodvViewsAutostore(int state)
{
  auto_store = state;
}

// A new initialization routine to make sure there is a real view in
// addition to the working view, and put it into use.
// This should be called whenever a model is loaded or created
void imodvViewsInitialize(Imod *imod)
{
  Ipoint imageMax = {0., 0., 0.};

  // Create view 1 if it does not exist yet, make it current if current is 0
  if (imod->viewsize < 2) {
    imodViewModelNew(imod);
    strcpy(imod->view[1].label, "view 1");
  }
  if (!imod->cview)
    imod->cview = 1;

  // Correct the scaling for the current view if it is 1:
  if (imod->view[imod->cview].rad == 1.)
    imodViewDefaultScale(imod, &imod->view[imod->cview], &imageMax, 1.);

  // Put the current view into use
  imodViewUse(imod);

  // Set the world flags in the Imodv structure
  Imodv->lighting = (imod->view->world & VIEW_WORLD_LIGHT)  ? 1 : 0;
  Imodv->wireframe = (imod->view->world & VIEW_WORLD_WIREFRAME) ? 1 : 0;
  Imodv->lowres = (imod->view->world & VIEW_WORLD_LOWRES) ? 1 : 0;
}

// send the list of views to the form
static void build_list(ImodvApp *a)
{
  int i;

  //  sprintf(a->imod->view->label, "Original Default View");
  for(i = 1; i < a->imod->viewsize; i++)
    ved->dia->addItem(a->imod->view[i].label);
}

/*

    $Log$
    Revision 4.14  2009/01/15 16:33:18  mast
    Qt 4 port

    Revision 4.13  2008/06/10 02:07:04  mast
    Set up lighting flag only when view changes

    Revision 4.12  2007/11/30 06:51:50  mast
    Changes for linking slicer to model view

    Revision 4.11  2007/10/03 19:31:10  sueh
    bug# 1038 Replacing help strings with an .html file.

    Revision 4.10  2005/10/13 20:08:00  mast
    Add bin scaling argument (1)

    Revision 4.9  2004/11/21 06:07:49  mast
    Changes for undo/redo

    Revision 4.8  2003/11/01 18:12:17  mast
    changed to put out virtually all error messages to a window

    Revision 4.7  2003/08/01 00:12:57  mast
    Fix some bugs from the new numbering mismatch between indexes in list box
    and the actual views, free object views when view is deleted

    Revision 4.6  2003/06/27 20:00:07  mast
    Added a function to initialize views and make sure view 1 is always present;
    changed indices, etc. to eliminate the "default view"

    Revision 4.5  2003/04/28 04:01:26  mast
    Fix hotkey text

    Revision 4.4  2003/04/25 03:28:32  mast
    Changes for name change to 3dmod

    Revision 4.3  2003/04/17 18:43:38  mast
    adding parent to window creation

    Revision 4.2  2003/03/04 21:41:29  mast
    Refresh the imod windows when the view changes

    Revision 4.1  2003/02/10 20:29:02  mast
    autox.cpp

    Revision 1.1.2.8  2003/01/18 01:10:17  mast
    add include of dia_qtutils

    Revision 1.1.2.7  2003/01/13 07:21:38  mast
    Changes to use new dialog manager class

    Revision 1.1.2.6  2002/12/30 06:40:24  mast
    Prevent multiple draws, adapt to dialog-widget control

    Revision 1.1.2.5  2002/12/27 17:45:01  mast
    clean up unused variable

*/
