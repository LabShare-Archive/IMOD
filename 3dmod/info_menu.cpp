/*  info_menu.cpp -- Menu slots for the imod information window; part
 *                       of the InfoWindow class declared in info_setup.h
 *                       and constructed in info_setup.cpp
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdio.h>
#include <math.h>
#include <qdir.h>
#include <qaction.h>
#include <qtimer.h>
#include "xxyz.h"
#include "object_edit.h"
#include "pixelview.h"
#include "xgraph.h"
#include "dia_qtutils.h"
#include "cachefill.h"
#include "imod.h"
#include "imodv.h"
#include "mv_views.h"
#include "mv_objed.h"
#include "isosurface.h"
#include "display.h"
#include "xcramp.h"
#include "xzap.h"
#include "imod_edit.h"
#include "info_setup.h"
#include "info_cb.h"
#include "imod_io.h"
#include "imod_input.h"
#include "cont_edit.h"
#include "cont_copy.h"
#include "iproc.h"
#include "imodv.h"
#include "sslice.h"
#include "xtum.h"
#include "moviecon.h"
#include "model_edit.h"
#include "rescale.h"
#include "b3dgfx.h"
#include "autox.h"
#include "locator.h"
#include "finegrain.h"
#include "vertexbuffer.h"
#include "preferences.h"
#include "undoredo.h"
#include "scalebar.h"

static int obj_moveto = 0;

/* function unsuitable libimod because of undo stuff */
static int imodContourBreakByZ(ImodView *vi, Iobj *obj, int ob, int co);

/*
 * THE FILE MENU
 */
void InfoWindow::fileSlot(int item)
{
  int returnValue;
  int limits[4];
  unsigned char **data;
  QString qname;

  if (ImodForbidLevel){
    if (item == FILE_MENU_QUIT)
      imod_quit();
    return;
  }

  switch(item){

    // Prevent saving of model during initial load since its trans is invalid
  case FILE_MENU_NEW: /* New */
    if (App->cvi->doingInitialLoad && imod_model_changed(Model))
      break;
    App->cvi->undo->clearUnits();
    createNewModel(NULL);
    break;
          
  case FILE_MENU_RELOAD:  /* reload - fall through to open */
    if (Imod_filename[0] == 0x00)
      break;

  case FILE_MENU_OPEN: /* open */
    //  forbid input during file dialog; swallow any pending events
    if (App->cvi->doingInitialLoad && imod_model_changed(Model))
      break;
    imod_info_forbid();
    imod_info_input();

    releaseKeyboard();
    if (item == FILE_MENU_OPEN) 
      returnValue = openModel(NULL, false, false);
    else
      returnValue = openModel(Imod_filename, false, true);
    imod_info_enable();

    if(returnValue == IMOD_IO_SUCCESS) {
      wprint("Model loaded.\n");
      App->cvi->undo->clearUnits();
    }
    else if(returnValue == IMOD_IO_SAVE_ERROR) {
      wprint("Error Saving Model. New model not loaded.\n");
    }

    else if(returnValue == IMOD_IO_READ_ERROR) {
      wprint("Error reading model. New model not loaded");
    }
    else if(returnValue != IMOD_IO_SAVE_CANCEL && 
            returnValue != IMOD_IO_READ_CANCEL) {
      wprint("Unknown return code, new model not loaded!!\n");
    }
    break;
      
  case FILE_MENU_SAVE: /* save */
    if (App->cvi->doingInitialLoad)
      break;
    imod_info_forbid();
    imod_info_input();
    releaseKeyboard();
    SaveModel(App->cvi->imod);
    /*         wprint("Error Saving Model."); DNM: it already has message*/
    imod_info_enable();
    break;

  case FILE_MENU_SAVEAS: /* save as */
    if (App->cvi->doingInitialLoad)
      break;
    imod_info_forbid();
    imod_info_input();
    releaseKeyboard();
    SaveasModel(App->cvi->imod);
    imod_info_enable();
    break;

  case FILE_MENU_MOVIEMONT:
    imodMovieConDialog(App->cvi);
    break;

  case FILE_MENU_SNAPDIR:
      imod_info_forbid();
      imod_info_input();
      releaseKeyboard();
      b3dSetSnapDirectory();
      imod_info_enable();
      break;

      // 12/16/10: If preferences is easy to get to, this shouldn't be here
      /* case FILE_MENU_SNAPQUALITY:
      imod_info_forbid();
      imod_info_input();
      releaseKeyboard();
      int quality = ImodPrefs->snapQuality();
      if (diaQInput(&quality, 10, 100, 0, 
                     "Quality factor for JPEG snapshots (%)"))
        ImodPrefs->setSnapQuality(quality);
      imod_info_enable();
      break; */

  case FILE_MENU_SNAPGRAY:
    App->convertSnap = 1 - App->convertSnap;
    mActions[FILE_MENU_SNAPGRAY]->setChecked(App->convertSnap != 0);
    break;

  case FILE_MENU_TIFF:  /* Save raw image to tiff file */
    if (!App->cvi->rgbStore)
      wprint("This option works only with color images.\n");
    else {
      /* DNM 12/2/02: switch to dia_filename so that the forbid/enable can
         be used, and move saving functionality into this case */
      imod_info_forbid();
      imod_info_input();
      releaseKeyboard();
      qname = imodPlugGetSaveName(ImodInfoWin, "TIFF File to save section from memory:");
      imod_info_enable();
      if (qname.isEmpty())
        break;

      data = ivwGetCurrentZSection(App->cvi);
      limits[0] = limits[1] = 0;
      limits[2] = App->cvi->xsize;
      limits[3] = App->cvi->ysize;
      b3dSnapshot_TIF(qname, 3, limits, data, false);
    }
    break;
    
  case FILE_MENU_EXTRACT: /* extract */
    imod_info_forbid();
    imod_info_input();
    releaseKeyboard();
    extract();
    imod_info_enable();
    break;

  case FILE_MENU_SAVEINFO: /* extract */
    wprintWriteFile();
    break;

  case FILE_MENU_QUIT:
    imod_quit();
    break;
       
  default:
    break;
  }

}

/*
 * THE FILE WRITE AS MENU
 */
void InfoWindow::fileWriteSlot(int item)
{
  FILE *fout;
  QString qname;

  if (ImodForbidLevel)
    return;

  /* DNM 12/2/02: add the forbid, change returns to breaks */
  imod_info_forbid();
  imod_info_input();

  switch (item){

  case FWRITE_MENU_IMOD: /* write imod */
    releaseKeyboard();
    qname = imodPlugGetSaveName(ImodInfoWin, "File to save as Imod:");
    if (qname.isEmpty())
      break;
    fout =  fopen(LATIN1(QDir::convertSeparators(qname)), "wb");
    if (!fout)
      break;

    /* DNM 7/24/03: Make sure model is fully up to date before storing */
    if (ImodvClosed)
      Imodv->imod = App->cvi->imod;
    imodvAutoStoreView(Imodv);
    imodWrite(App->cvi->imod, fout);
    fclose(fout);
    break;

  case FWRITE_MENU_WIMP: /* write wimp */
    releaseKeyboard();
    qname = imodPlugGetSaveName(ImodInfoWin, "File to save as Wimp:");
    if (qname.isEmpty())
      break;
    fout =  fopen(LATIN1(QDir::convertSeparators(qname)), "w");
    if (!fout)
      break;
    imod_to_wmod(App->cvi->imod, fout, LATIN1(qname));
    fclose(fout);
    break;

  case FWRITE_MENU_NFF: /* write NFF */
    releaseKeyboard();
    qname = imodPlugGetSaveName(ImodInfoWin, "File to save as NFF:");
    if (qname.isEmpty())
      break;
    fout =  fopen(LATIN1(QDir::convertSeparators(qname)), "w");
    if (!fout)
      break;
    imod_to_nff(App->cvi->imod, fout);
    fclose(fout);
    break;

  case FWRITE_MENU_SYNU: /* write synu */
    imod_to_synu(App->cvi->imod);
    break;

  }
  imod_info_enable();
}

/*
 * THE EDIT MENU
 */
void InfoWindow::editSlot(int item)
{
  switch(item){
    /*case EDIT_MENU_MOVIES:
    imodMovieConDialog(App->cvi);
    break; */
  case EDIT_MENU_GRAIN:
    fineGrainOpen(App->cvi);
    break;
  case EDIT_MENU_ANGLES:
    slicerAnglesOpen();
    break;
  case EDIT_MENU_SCALEBAR:
    scaleBarOpen();
    break;
  case EDIT_MENU_PREFS:
    ImodPrefs->editPrefs();
    break;
  }
}


/*
 * THE EDIT MODEL MENU
 */
void InfoWindow::editModelSlot(int item)
{
  ImodView *vi = App->cvi;
  Imod *imod = vi->imod;
  int ob, co, detach, hasdata, obsave;
  int cosave, ptsave;
  Iobj *obj;

  if (ImodForbidLevel)
    return;

  switch(item){
  case EMODEL_MENU_HEADER: 
    openModelEdit(vi);
    break;
  case EMODEL_MENU_OFFSETS:
    openModelOffset(vi);
    break;
  case EMODEL_MENU_CLEAN:
    /* Clean model means delete empty objects */
    if (dia_ask("Delete all empty objects?")) {
      vbCleanupVBD(imod);
      obsave = imod->cindex.object;
      cosave = vi->imod->cindex.contour;
      ptsave = vi->imod->cindex.point;
      detach = 0;
      for (ob = imod->objsize - 1; ob >= 0; ob--){
        /* Check if object has any contours with points; but don't
           delete the last object or isosurface objects */
        obj = &imod->obj[ob];
        hasdata = (imod->objsize == 1 || obj->meshsize > 0) ? 1 : 0;
        if (!hasdata) {
          for (co = 0; co < (int)obj->contsize; co++){
            if (obj->cont[co].psize) {
              hasdata = 1;
              break;
            }
          }
        }
        if (!hasdata) {
          /* delete object if it is empty */
          vi->undo->objectRemoval(ob);
          imodDeleteObject(imod, ob);
          if (ob < obsave) 
            /*If this object is less than than the current
              one, then decrease current object */
            obsave--;
          else if (ob == obsave)
            /* but if it was current object, detach from 
               contour */
            detach = 1;
        }
      }

      /* Restore current point if possible */
      if (detach) {
        if (obsave >= (int)imod->objsize)
          obsave = imod->objsize - 1;
        cosave = -1;
        ptsave = -1;
      }
      imod->cindex.object = obsave;
      imod->cindex.contour = cosave;
      imod->cindex.point = ptsave;

      /* Take care of object color map and displayed color of current
         object */
      vi->undo->finishUnit();
      imodSelectionListClear(vi);
      imod_cmap(vi->imod);
      imod_info_setobjcolor();
      imodDraw(vi, IMOD_DRAW_RETHINK);
      imodvObjedNewView();
    }
    break;
  }
}

/*
 * THE EDIT OBJECT MENU
 */
void InfoWindow::editObjectSlot(int item)
{
  Iobj *obj;
  Icont *cont;
  int ob,co,pt, coind, obOld, obNew, ndone, num, minOb, maxOb;
  int cosave, ptsave;
  QString qstr;
  const char *objtype;
  ImodView *vi = App->cvi;
  Imod *imod = vi->imod;
  static int lastDelete = 0;
  static int lastCombine = 0;
  static int lastFixz = 0;
  static int lastFlatten = 0;
  int *lastp;
  bool fixz;

  if (ImodForbidLevel)
    return;

  obj = imodObjectGet(imod);
  if (!obj && item != EOBJECT_MENU_NEW)
    return;
     
  switch(item){
  case EOBJECT_MENU_NEW: /* New */
    inputNewObject(vi);
    imod_object_edit();
    imod_info_setobjcolor();
    imodvObjedNewView();
    break;
          
  case EOBJECT_MENU_DELETE: /* Delete */
    num = imodNumSelectedObjects(vi, minOb, maxOb);
    if (!num || !obj)
      break;
    if (lastDelete < 2) {
      if (num > 1) {
        qstr.sprintf("%d objects are selected.\nDelete all selected objects?",
                     num);
        lastDelete = dia_ask_forever(LATIN1(qstr));
      } else
        lastDelete = dia_ask_forever("Delete Object?");
      if (!lastDelete)
        break;
    } else if (num > 1)
      wprint("Deleted %d objects\n", num);

    obOld = imod->cindex.object;
    for (ob = maxOb; ob >= minOb; ob--) {
      if (imodSelectionListQuery(vi, ob, -1) > -2 || ob == obOld) {
        vi->undo->objectRemoval(ob);
        vbCleanupVBD(&imod->obj[ob]);
        imodDeleteObject(imod, ob);
      }
    }
    vi->undo->finishUnit();
    imodSelectionListClear(vi);

    // Most things work with no objects, but who wants to check all the dialogs
    if (!imod->objsize) {
      inputNewObject(vi);
      wprint("\aA new empty object was created because you deleted all "
             "objects\n");
    }
    imodDraw(vi, IMOD_DRAW_MOD);
    imod_cmap(imod);
    imod_info_setobjcolor();
    imodvObjedNewView();
    break;
          
  case EOBJECT_MENU_COLOR: /* Color */
    imod_info_forbid();
    imod_info_input();
    imod_info_enable();
          
    imod_object_color(imod->cindex.object);
    break;

  case EOBJECT_MENU_TYPE: /* type*/
    imod_object_edit();
    imod_draw_window();
    imod_info_setobjcolor();
    break;
          
  case EOBJECT_MENU_MOVE: /* move */
    imod_info_forbid();
    imod_info_input();
    imod_info_enable();
    if (imod->objsize < 2){
      wprint("Must have more than one object to move contours "
             "to a new object\n");
      break;
    }
    if (obj_moveto > (int)imod->objsize)
      obj_moveto = imod->objsize;
    if (obj_moveto < 1)
      obj_moveto = 1;

    if (imod->cindex.object > -1){
      ob = imod->cindex.object;
      pt = (!imod->obj[ob].contsize && imod->obj[ob].meshsize) ? 1 : 0;
      if (!diaQInput(&obj_moveto, 1, imod->objsize, 0, 
                     pt ? "Move all meshes to selected object." :
                     "Move all contours to selected object."))
        break;
      obNew = obj_moveto - 1; 

      if (obNew != ob) {
        if (pt != ((!imod->obj[obNew].contsize && imod->obj[obNew].meshsize) ? 1 : 0)) {
          wprint("\aYou cannot combine isosurface objects (meshes and no contours) "
                 "with regular contour-based objects.\n");
          break;
        }
        vbCleanupVBD(&imod->obj[ob]);
        vbCleanupVBD(&imod->obj[obNew]);
        imodMoveAllContours(vi, obNew);
        imod->cindex.contour = -1;
        imod->cindex.point = -1;
        vi->undo->finishUnit();
        
        // DNM 11/16/04: move these up into conditional block
        imodSelectionListClear(vi);
        imodDraw(vi, IMOD_DRAW_MOD);
      } else
        wprint("\aMust select a different object to move "
               "contours to.\n");
    }
    /* DNM: need to maintain separate object numbers for two functions */
    /*    vi->obj_moveto = obj_moveto; */
    break;

  case EOBJECT_MENU_COMBINE: /* combine */
    num = imodNumSelectedObjects(vi, minOb, maxOb);
    obOld = imod->cindex.object;
    if (num < 2) {
      wprint("\aYou must select contours in more than one object to combine "
             "objects.\n");
      break;
    }
    
    // Check that all objects are compatible
    pt = (!imod->obj[minOb].contsize && imod->obj[minOb].meshsize) ? 1 : 0;
    for (ob = maxOb; ob > minOb; ob--) {
      if (imodSelectionListQuery(vi, ob, -1) > -2 || ob == obOld) {
        if (((!imod->obj[ob].contsize && imod->obj[ob].meshsize) ? 1 : 0) != pt) {
          wprint("\aYou cannot combine isosurface objects (meshes and no contours) "
                 "with regular contour-based objects.\n");
          pt = -1;
          break;
        }
      }
    }
    if (pt < 0)
      break;

    if (lastCombine < 2) {
      qstr.sprintf("Are you sure you want to combine these %d selected "
                   "objects into one?", num);
      lastCombine = dia_ask_forever(LATIN1(qstr));
      if (!lastCombine)
        break;
    } else
      wprint("Combined %d objects\n", num);

    vbCleanupVBD(&imod->obj[minOb]);
    for (ob = maxOb; ob > minOb; ob--) {
      if (imodSelectionListQuery(vi, ob, -1) > -2 || ob == obOld) {
        imod->cindex.object = ob;
        imodMoveAllContours(vi, minOb);
        vi->undo->objectRemoval(ob);
        vbCleanupVBD(&imod->obj[ob]);
        imodDeleteObject(imod, ob);
      }
    }
    imod->cindex.object = ob;
    imod->cindex.contour = -1;
    imod->cindex.point = -1;
    vi->undo->finishUnit();
    imodSelectionListClear(vi);
    imodDraw(vi, IMOD_DRAW_MOD);
    imod_cmap(imod);
    imod_info_setobjcolor();
    imodvObjedNewView();
    break;

  case EOBJECT_MENU_INFO: /* stats: eliminated 3/31/10; run imodinfo 9/28/11 */
    objectInfo();
    break;

  case EOBJECT_MENU_CLEAN: /* Clean: delete empty contours */
    obj = imodObjectGet(imod);
    if (!obj)
      break;
    cosave = imod->cindex.contour;
    ptsave = imod->cindex.point;
    for (co = obj->contsize - 1; co >= 0; co--){
      if (!obj->cont[co].psize) {
        /* delete contour if it is empty */
        imod->cindex.contour = co;
        vi->undo->contourRemoval();
        imodDeleteContour(imod, co);
        if (co < cosave) 
          /* And if this contour is less than than the current
             one, then decrease current contour */
          cosave--;
        else if (co == cosave)
          /* but if it was current contour, detach from it */
          cosave = -1;
      }
    }

    /* Restore current point if possible */
    if (cosave >= 0) {
      imod->cindex.contour = cosave;
      imod->cindex.point = ptsave;
    }

    vi->undo->finishUnit();
    imodSelectionListClear(vi);
    imodDraw(vi, IMOD_DRAW_RETHINK);
    break;

  case EOBJECT_MENU_FIXZ: /* break all contours at z transitions */
  case EOBJECT_MENU_FLATTEN: /* Flatten to nearest integer */
    fixz = item == EOBJECT_MENU_FIXZ;
    imodGetIndex(imod, &ob, &co, &pt);
    if (iobjClose(obj->flags)) 
      objtype = "closed contour";
    else if (iobjScat(obj->flags))
      objtype = "scattered point";
    else
      objtype = "open contour";
    lastp = fixz ? &lastFixz : &lastFlatten;
    if (*lastp < 2) {
      qstr.sprintf("Are you sure you want to %s in object %d, a %s object?", 
                   fixz ? "break all contours into different Z planes" : 
                   "flatten all contours to lie in only one Z plane", 
                   ob + 1, objtype);
    
      *lastp = dia_ask_forever(LATIN1(qstr));
      if (!*lastp)
        break;
    }
    ndone = 0;
    for (coind = obj->contsize - 1; coind >= 0; coind--) {
      cont = &obj->cont[coind];
      if (cont->psize && fixz) 
        ndone += imodContourBreakByZ(vi, obj, ob, coind);
      else if (cont->psize) {
        vi->undo->contourDataChg(ob, coind);
        imodContourFlatten(cont);
        ndone++;
      }
    }
    if (co >= 0)
      imodSetIndex(imod, ob, co, pt);
    if (ndone)
      vi->undo->finishUnit();
    imodSelectionListClear(vi);
    imodDraw(vi, IMOD_DRAW_MOD);
    break;

  case EOBJECT_MENU_RENUMBER: /* Shift object to a new position */
    imod_info_forbid();
    imod_info_input();
    imod_info_enable();
    if (imod->objsize < 2){
      wprint("\aMust have more than one object to renumber objects\n");
      break;
    }
    imodGetIndex(imod, &obOld, &co, &pt);
    if (obOld < 0)
      break;
    obNew = obOld == imod->objsize - 1 ? obOld : imod->objsize;

    if (!diaQInput(&obNew, 1, imod->objsize, 0,
                   "New object number for current object."))
      break;

    obNew--;
    if (obOld == obNew)
      break;
    
    vi->undo->objectMove(obOld, obNew);
    if (imodMoveObject(imod, obOld, obNew)) {
      wprint("\aError getting memory for moving objects\n");
      vi->undo->flushUnit();
      break;
    }
    
    imodSetIndex(imod, obNew, co, pt);
    vi->undo->finishUnit();
    imodSelectionListClear(vi);
    imodDraw(vi, IMOD_DRAW_MOD);
    imodvObjedNewView();
    break;

  default:
    break;
          
  }
     
}

/*
 * THE EDIT SURFACE MENU
 */
void InfoWindow::editSurfaceSlot(int item)
{
  Iobj *obj;
  Icont *cont;

  int ob, co, pt, coNew, numDel, surfDel;
  QString qstr;
  Imod *imod = App->cvi->imod;
  static int lastDelete = 0;

  if (ImodForbidLevel)
    return;

  switch(item){
  case ESURFACE_MENU_NEW: /* new */
    inputNewSurface(App->cvi);
    break;
          
  case ESURFACE_MENU_GOTO: /* go to */
    imodContEditSurf(App->cvi);
    break;
          
  case ESURFACE_MENU_MOVE: /* move */
    imodContEditMoveDialog(App->cvi, 1);
    break;

  case ESURFACE_MENU_DELETE: /* delete */
    cont = imodContourGet(imod);
    obj = imodObjectGet(imod);
    if (!cont)
      break;

    // Count contours at this surface and confirm if > 1
    numDel = 0;
    surfDel = cont->surf;
    for (co = 0; co < obj->contsize; co++)
      if (obj->cont[co].surf == surfDel)
        numDel++;
    if (lastDelete < 2 && numDel > 1) {
      qstr.sprintf("Are you sure you want to delete the %d contours in "
                   "this surface?", numDel);
      lastDelete = dia_ask_forever(LATIN1(qstr));
      if (!lastDelete)
        break;
    } else
      wprint("Deleted %d contours.\n", numDel);

    // Remove contours from end back, adjust current contour when deleted
    imodGetIndex(imod, &ob, &coNew, &pt);
    for (co = obj->contsize - 1; co >= 0; co--) {
      if (obj->cont[co].surf == surfDel) {
        App->cvi->undo->contourRemoval(co);
        imodDeleteContour(imod, co);
        if (coNew && coNew == co)
          coNew--;
      }
    }
    imodSetIndex(imod, ob, coNew, pt);
    App->cvi->undo->finishUnit();
    imodSelectionListClear(App->cvi);
    imod_setxyzmouse();
    break;
  }
}
          
/*
 * THE EDIT CONTOUR MENU
 */
void InfoWindow::editContourSlot(int item)
{
  Iobj *obj;
  Icont *cont;
  int ob,co,pt, ptb, open;
  double dist;
  ImodView *vi = App->cvi;
  Imod *imod = vi->imod;

  if (ImodForbidLevel)
    return;

  cont = imodContourGet(imod);
  obj = imodObjectGet(imod);
     
  switch(item){
  case ECONTOUR_MENU_NEW: /* new */
    inputNewContour(vi);
    break;
          
  case ECONTOUR_MENU_DELETE: /* del */
    inputDeleteContour(vi);
    break;
          
  case ECONTOUR_MENU_MOVE: /* move */
    // 11/4/04: no longer require multiple objects
    imodContEditMoveDialog(vi, 0);
    break;
          
  case ECONTOUR_MENU_SORT: /* sort */
    if (imod->mousemode != IMOD_MMODEL){
      wprint("\aError: Must be in Model mode to Sort.\n");
    } else if (!obj || iobjScat(obj->flags)) {
      wprint("\aError: Scattered point objects cannot be sorted.\n");
    } else {
      vi->undo->clearUnits();
      imodObjectSort(imodObjectGet(imod));
      imod->cindex.contour = -1;
      imod->cindex.point   = -1;
      imodSelectionListClear(vi);
      imod_info_setocp();
    }
    break;

  case ECONTOUR_MENU_AUTO: /* auto */
    autox_open(vi);
    imod_info_setocp();
    break;

  case ECONTOUR_MENU_TYPE: /* surface */
    imodContEditSurf(vi);
    break;


  case ECONTOUR_MENU_INFO: /* Print Stats */
    if (!cont)
      return;
    if (!cont->psize)
      return;
    obj = imodObjectGet(imod);
    if (!obj) break;      
    wprint("Obj: %d, Cont %d\n", 
           imod->cindex.object+1, 
           imod->cindex.contour+1);

    if (iobjClose(obj->flags)){
              
      /* 2.00b7 fix added */
      /* DNM 9/29/06: treat 2-point contours as open and modify messages */
      open = cont->flags & ICONT_OPEN || cont->psize < 3;
      if (!open){
        dist = imodContourArea(cont);
        dist *= imod->pixsize * imod->pixsize * vi->xybin * vi->xybin;
        wprint("2D area = %g square %s\n", dist, imodUnits(imod));
      }

      for(dist = 0.0, pt = 0; pt < (int)cont->psize-1; pt++){
        dist += imodel_point_dist(&(cont->pts[pt]), &(cont->pts[pt+1]));
      }
               
      /* 2.00b7 fix added */
      if (!open)
        dist += imodel_point_dist
          (&(cont->pts[cont->psize-1]), cont->pts);
               
      dist *= imod->pixsize * vi->xybin;
      wprint("2D %s length = %g %s\n", open ? "open" : "closed", dist, 
             imodUnits(imod));
               
    }
    if (iobjOpen(obj->flags)){
      Ipoint scale;
      scale.x = imod->xscale * vi->xybin;
      scale.y = imod->yscale * vi->xybin;
      scale.z = imod->zscale * vi->zbin;
      for(dist = 0.0, pt = 0; pt < (int)cont->psize-1; pt++){
        dist += imodPoint3DScaleDistance
          (&(cont->pts[pt]), &(cont->pts[pt+1]), &scale);
      }

      /* 2.00b7 fix removed */
      /*dist += imodPoint3DScaleDistance
        (&(cont->pts[cont->psize-1]), cont->pts, &scale);
      */

      dist *= imod->pixsize;
      wprint("3D scaled open length = %g %s\n", dist, imodUnits(imod));
    }
    if (iobjScat(obj->flags)){
      wprint("No stats available for scattered contours.\n");
    }
    break;

  case ECONTOUR_MENU_BREAK: /* break a contour into two */
    imodContEditBreakOpen(vi);
    break;

  case ECONTOUR_MENU_FIXZ: /* break a contour at z transitions */
    obj = imodObjectGet(imod);
    if (!obj)
      break;      
    if (!cont)
      break;      
    if (!cont->psize)
      break;      
    if (!iobjPlanar(obj->flags)){
      wprint("\aError: Only closed contours or contours in open objects "
             "with automatic new contours can be broken by Z\n");
      break;
    }
    imodGetIndex(imod, &ob, &co, &pt);
    ptb = imodContourBreakByZ(vi, obj, ob, co);

    // This takes care of point position
    imodSetIndex(imod, ob, co, pt);
    if (ptb)
      vi->undo->finishUnit();
    imodSelectionListClear(vi);
    imodDraw(vi, IMOD_DRAW_MOD);
    break;

  case ECONTOUR_MENU_JOIN: /* join two contour together. */
    imodContEditJoinOpen(vi);
    break;

  case ECONTOUR_MENU_INVERT: /* invert a contour */
    if (!cont)
      break;
    vi->undo->contourDataChg();
    if (!imodel_contour_invert(cont)) {
      imodGetIndex(imod, &ob, &co, &pt);
      pt = (cont->psize - 1) - pt;
      imodSetIndex(imod, ob, co, pt);
      vi->undo->finishUnit();
      imodDraw(vi, IMOD_DRAW_MOD);
    }
    break;

  case ECONTOUR_MENU_COPY: /* duplicate current contour. */
    openContourCopyDialog(vi);
    break;

  case ECONTOUR_MENU_LOOPBACK: /* Loop back to start to make complex cap */
    if (!cont || cont->psize < 3)
      break;
    vi->undo->contourDataChg();
    for (ptb = cont->psize - 2; ptb > 0; ptb--)
      imodPointAppend(cont, &cont->pts[ptb]);
    vi->undo->finishUnit();
    imodDraw(vi, IMOD_DRAW_MOD);
    break;

  case ECONTOUR_MENU_FILLIN: /* fill in a contour at every z level: Open contours only */
    obj = imodObjectGet(imod);
    if (!obj)
      break;      
    if (!cont)
      break;      
    if (!cont->psize)
      break;      
    if (iobjClose(obj->flags) || iobjScat(obj->flags)){
      wprint("\aError: Only Open contours can be filled in by Z\n");
      break;
    }
    imodGetIndex(imod, &ob, &co, &pt);
    for (ptb = 0; ptb < (int)cont->psize - 1; ptb++) {
      int zcur, znext, zfill, first;
      Ipoint newPt;
      Ipoint *cur, *next;
      zcur = (int)(floor(cont->pts[ptb].z + 0.5));
      znext = (int)(floor(cont->pts[ptb + 1].z + 0.5));
      zfill = zcur;
      first = 1;

      /* find points where rounded z differs by more than one */
      if (zcur - znext > 1)
        zfill--;
      else if (znext - zcur > 1)
        zfill++;
      if (zcur != zfill) {
        if (first)
          vi->undo->contourDataChg();
        first = 0;
        cur = &cont->pts[ptb];
        next = &cont->pts[ptb + 1];

        /* insert one point at the next Z; the spacing from that
           one to the next will be assessed on next iteration */
        newPt.z = zfill;
        newPt.x = cur->x + (next->x - cur->x) * (zfill - cur->z) /
          (next->z - cur->z);
        newPt.y = cur->y + (next->y - cur->y) * (zfill - cur->z) /
          (next->z - cur->z);
        imodPointAdd(cont, &newPt, ptb + 1);
        if (ptb < pt)
          pt++;
      }
    }

    imodSetIndex(imod, ob, co, pt);
    vi->undo->finishUnit();
    imodDraw(vi, IMOD_DRAW_MOD);
    break;

  default:
    break;
          
  }
     
}

/*
 * THE EDIT POINT MENU
 */
void InfoWindow::editPointSlot(int item)
{
  ImodView *vi = App->cvi;
  Imod *imod = vi->imod;
  Icont *cont = imodContourGet(imod);
  Ipoint scale;

  if (ImodForbidLevel)
    return;

  switch(item){
  case EPOINT_MENU_DELETE: /* Delete */
    inputDeletePoint(vi);
    break;
          
  case EPOINT_MENU_SORTDIST: /* Sort by distance */
    if (cont && imod->mousemode == IMOD_MMODEL){
      scale.x = imod->xscale * vi->xybin;
      scale.y = imod->yscale * vi->xybin;
      scale.z = imod->zscale * vi->zbin;

      vi->undo->contourDataChg();
      imodContourSort3D(imodContourGet(imod), &scale);
      vi->undo->finishUnit();
      imodDraw(vi, IMOD_DRAW_MOD);
    }
    break;

  case EPOINT_MENU_SORTZ: /* Sort by Z */
    if (imod->mousemode == IMOD_MMODEL){
      if (cont)
        if (cont->psize) {
          vi->undo->contourDataChg();
          imodel_contour_sortz(cont, 0, cont->psize - 1);
          vi->undo->finishUnit();
          imodDraw(vi, IMOD_DRAW_MOD);
        }
    }
    break;
          
  case EPOINT_MENU_DIST: /* dist */{
             
    int    pt   = imod->cindex.point;
    Ipoint *cpt, *ppt;
    float dist2d, dist3d;

    /* DNM: psize must be at least 2, not 0 or 1 */
    if ((!cont) || (cont->psize < 2) || (pt < 0)){
      wprint("\aError: No points selected.\n");
      break;
    }
    cpt = &(cont->pts[pt]);
    if (pt == 0) pt = cont->psize;
    pt--;
    ppt = &(cont->pts[pt]);

    scale.x = imod->xscale * vi->xybin;
    scale.y = imod->yscale * vi->xybin;
    scale.z = imod->zscale * vi->zbin;
             
    dist2d  = imodel_point_dist(cpt, ppt);
    dist3d  = imodPoint3DScaleDistance(cpt, ppt, &scale);

    wprint("\nDistance to previous point :\nX/Y only = %g pixels, %g %s\n",
           dist2d, dist2d * imod->pixsize * vi->xybin,
           imodUnits(imod));
    wprint("In 3D     = %g %spixels, %g %s\n",
           dist3d, vi->xybin * vi->zbin > 1 ? "unbinned " : "",
           dist3d * imod->pixsize, imodUnits(imod));
    break;
  }
  case EPOINT_MENU_VALUE: /* value */
    wprint("Pixel value from file:\n (%g, %g, %g) = %g",
           vi->xmouse + 1, vi->ymouse + 1, vi->zmouse + 1,
           ivwGetFileValue(vi, (int)vi->xmouse,
                           (int)vi->ymouse, (int)vi->zmouse));
    break;

  case EPOINT_MENU_SIZE: /* size */
    imodContEditSurf(vi);
    break;

  default:
    break;
          
  }
     
}

/*
 * THE EDIT IMAGE MENU
 */
void InfoWindow::editImageSlot(int item)
{
  int cmap, cmapold;
  if (ImodForbidLevel)
    return;

  switch(item){
  case EIMAGE_MENU_PROCESS:
    inputIProcOpen(App->cvi);
    break;

  case EIMAGE_MENU_COLORMAP: /* rampbase */
    if (App->rgba)
      wprint("Not needed for TrueColor graphics.\n");
    else if (App->depth <= 8){
      wprint("Not available for 8-bit graphics.\n");
    }else{
      cmap = ((App->cvi->rampbase-RAMPBASE)/RAMP_INTERVAL) + 1;
      cmapold = cmap;
      diaQInput(&cmap, 1, MAXIMUM_RAMPS, 0, "New Colormap #");
      if (cmap == cmapold)
        break;
      Rampbase = App->cvi->rampbase = App->base = 
        (cmap - 1) * RAMP_INTERVAL + RAMPBASE;
      App->objbase  = App->base + 257;

      // Delete old map, get new one, fill map and tell widgets
      delete App->qColormap;
      App->qColormap = new QGLColormap();
      imod_cmap(App->cvi->imod);
      xcramp_setlevels(App->cvi->cramp, App->cvi->black,App->cvi->white);
      imodDraw(App->cvi, IMOD_DRAW_COLORMAP);
      break;
    }
    break;

  case EIMAGE_MENU_RELOAD:
    imodImageScaleDialog(App->cvi);
    break;

  case EIMAGE_MENU_FLIP:
    App->cvi->undo->clearUnits();
    vbCleanupVBD(App->cvi->imod);
    
    /* DNM 12/10/02: if busy loading, this will defer it */
    if (ivwFlip(App->cvi))
      break;
    /* DNM: check wild flag here */
    ivwCheckWildFlag(App->cvi->imod);
    imodDraw(App->cvi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
    break;

  case EIMAGE_MENU_FILLCACHE:
    if (App->cvi->vmSize)
      imodCacheFill(App->cvi);
    else
      wprint("Cache is not active.\n");
    break;

  case EIMAGE_MENU_FILLER:
    if (App->cvi->vmSize)
      imodCacheFillDialog(App->cvi);
    else
      wprint("Cache is not active.\n");
    break;

  default:
    break;
  }

}

/*
 * THE IMAGE MENU
 */
void InfoWindow::imageSlot(int item)
{
  //fprintf(stderr,"Edit image item %d\n", item);

  if (ImodForbidLevel || App->cvi->doingInitialLoad)
    return;
     
  switch(item){

  case IMAGE_MENU_GRAPH: /* graph */
    /* DMN 2/25/01: do not open with fake image */
    if (!App->cvi->fakeImage && !App->cvi->rgbStore)
      xgraphOpen(App->cvi);
    break;

  case IMAGE_MENU_SLICER: /* slice */
    slicerOpen(App->cvi, 0);
    break;
          
  case IMAGE_MENU_LINKSLICE: /* linked slicers */
    setupLinkedSlicers(App->cvi);
    break;
          
  case IMAGE_MENU_TUMBLER: /* tumble */
    if (!App->cvi->rgbStore)
      xtumOpen(App->cvi);
    break;

  case IMAGE_MENU_LOCATOR: /* locator */
    locatorOpen(App->cvi);
    break;

  case IMAGE_MENU_MODV: /* model view */
    imod_autosave(App->cvi->imod);
    imodv_open();
    break;

  case IMAGE_MENU_ZAP:
    imod_zap_open(App->cvi, 0);
    break;

  case IMAGE_MENU_MULTIZ:
    imod_zap_open(App->cvi, 1);
    break;

  case IMAGE_MENU_XYZ:
    xxyz_open(App->cvi);
    break;

  case IMAGE_MENU_PIXEL:
    if (!App->cvi->fakeImage)
      open_pixelview(App->cvi);
    break;

  case IMAGE_MENU_ISOSURFACE:
    if (!App->cvi->fakeImage && !App->cvi->rgbStore) {
      imodv_open();
      imodvIsosurfaceEditDialog(Imodv, 1);
    }

    /* DNM 12/18/02 removed unused zoom command */
          
  default:
    break;
          
  }

}


/*
 * THE HELP MENU
 */
void InfoWindow::helpSlot(int item)
{

  switch (item){
  case HELP_MENU_MAN:
    imodShowHelpPage("../man/3dmod.html#TOP");
    break;
  case HELP_MENU_MENUS:
    imodShowHelpPage("menus.html#TOP");
    break;
  case HELP_MENU_CONTROLS:
    imodShowHelpPage("infowin.html#TOP");
    break;
  case HELP_MENU_HOTKEY:
    imodShowHelpPage("keyboard.html#TOP");
    break;

  case HELP_MENU_ABOUT:
    imod_info_forbid();
    imod_info_input();
    imod_info_enable();
    dia_vasmsg
      ("3dmod Version ",
       VERSION_NAME, "[", __DATE__, __TIME__, "]",
       "written by James Kremer and",
       "David Mastronarde\n",
       "Copyright (C)",COPYRIGHT_YEARS,"by",LAB_NAME1,"\n",LAB_NAME2,
       "& Regents of the University of Colorado\n\n",
       NULL);
    break;

  }
  return;
}

/* imodContourBreakByZ will break contour number co of object obj into 
   contours that are all coplanar in Z.  It returns 1 if the contour was broken
   and 0 if not */
static int imodContourBreakByZ(ImodView *vi, Iobj *obj, int ob, int co)
{
  int zcur, zprev, ptb, first;
  Icont *cont2, *cont;
  cont = &obj->cont[co];
  first = 1;

  // Work from the end of the contour to the beginning looking for Z changes
  for (ptb = cont->psize - 1; ptb > 0; ptb--) {
    zcur = (int)(floor(cont->pts[ptb].z + 0.5));
    zprev = (int)(floor(cont->pts[ptb - 1].z + 0.5));
    if (zcur != zprev) {

      // The first time, mark the contour as being changed
      // Then record each added contour
      if (first)
        vi->undo->contourDataChg(ob, co);
      first = 0;
      vi->undo->contourAddition(ob, obj->contsize);

      // duplicate contour and shift down back end
      cont2 = imodContourBreak(cont, ptb, -1);
      if (!cont2) {
        wprint("\aMemory or other error breaking contour.\n");
        vi->undo->flushUnit();
        return 0;
      }
      imodel_contour_check_wild(cont2);
      imodObjectAddContour(obj, cont2);
      free(cont2);
      cont = &obj->cont[co];

      // Copy any contour-specific properties to new contour
      if (istoreCountContSurfItems(obj->store, co, 0)) {
        vi->undo->objectPropChg();
        if (istoreCopyContSurfItems(obj->store, &obj->store, co,
                                    obj->contsize - 1, 0))
          wprint("\aError copying contour properties\n");
      }
    }
  }
  imodel_contour_check_wild(cont);
  return(1-first);
}
