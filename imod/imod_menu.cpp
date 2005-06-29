/*  imod_menu.cpp -- Menu slots for the imod information window; part
 *                       of the InfoWindow class declared in imod_info.h
 *                       and constructed in imod_info.cpp
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#include <stdio.h>
#include <math.h>
#include <qfiledialog.h>
#include <qdir.h>
#include <qtimer.h>
#include "xxyz.h"
#include "imod_object_edit.h"
#include "pixelview.h"
#include "xgraph.h"
#include "dia_qtutils.h"
#include "imod_cachefill.h"
#include "imod.h"
#include "imodv.h"
#include "imodv_views.h"
#include "imodv_objed.h"
#include "imod_display.h"
#include "xcramp.h"
#include "xzap.h"
#include "imod_edit.h"
#include "imod_info.h"
#include "imod_info_cb.h"
#include "imod_io.h"
#include "imod_input.h"
#include "imod_cont_edit.h"
#include "imod_cont_copy.h"
#include "iproc.h"
#include "imodv.h"
#include "sslice.h"
#include "xtum.h"
#include "imod_moviecon.h"
#include "imod_model_edit.h"
#include "imod_iscale.h"
#include "b3dgfx.h"
#include "autox.h"
#include "finegrain.h"
#include "preferences.h"
#include "undoredo.h"

/****help text data include files*****/
#include "imodhelp.h" 
#include "menus.h"
#include "hotkey.h"


static int obj_moveto = 0;
/* Temporary function - may include in libimod */
static Icont *imodContourBreakByZ(ImodView *vi, Iobj *obj, int ob, int co);

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
    if (item == 6)
      imod_quit();
    return;
  }

  switch(item){

  case FILE_MENU_NEW: /* New */
    App->cvi->undo->clearUnits();
    createNewModel(NULL);
    break;
          
  case FILE_MENU_OPEN: /* open */
    //  forbid input during file dialog; swallow any pending events
    imod_info_forbid();
    imod_info_input();

    releaseKeyboard();
    returnValue = openModel(NULL, false);
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
    imod_info_forbid();
    imod_info_input();
    App->cvi->imod->blacklevel = App->cvi->black;
    App->cvi->imod->whitelevel = App->cvi->white;
    releaseKeyboard();
    if (SaveModel(App->cvi->imod));
    /*         wprint("Error Saving Model."); DNM: it already has message*/
    imod_info_enable();
    break;

  case FILE_MENU_SAVEAS: /* save as */
    imod_info_forbid();
    imod_info_input();
    App->cvi->imod->blacklevel = App->cvi->black;
    App->cvi->imod->whitelevel = App->cvi->white;
    releaseKeyboard();
    SaveasModel(App->cvi->imod);
    imod_info_enable();
    break;
          
  case FILE_MENU_TIFF:  /* Save raw image to tiff file */
    if (!App->cvi->rawImageStore)
      wprint("This option works only with color images.\n");
    else {
      /* DNM 12/2/02: switch to dia_filename so that the forbid/enable can
         be used, and move saving functionality into this case */
      imod_info_forbid();
      imod_info_input();
      releaseKeyboard();
      qname = QFileDialog::getSaveFileName
        (QString::null, QString::null, 0, 0, 
         "TIFF File to save section from memory:");
      imod_info_enable();
      if (qname.isEmpty())
        break;

      data = ivwGetCurrentZSection(App->cvi);
      limits[0] = limits[1] = 0;
      limits[2] = App->cvi->xsize;
      limits[3] = App->cvi->ysize;
      b3dSnapshot_TIF((char *)qname.latin1(), 0, limits, data);
    }
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
    qname = QFileDialog::getSaveFileName(QString::null, QString::null, 0, 0, 
                                         "File to save as Imod:");
    if (qname.isEmpty())
      break;
    fout =  fopen((QDir::convertSeparators(qname)).latin1(), "wb");
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
    qname = QFileDialog::getSaveFileName(QString::null, QString::null, 0, 0, 
                                         "File to save as Wimp:");
    if (qname.isEmpty())
      break;
    fout =  fopen((QDir::convertSeparators(qname)).latin1(), "w");
    if (!fout)
      break;
    imod_to_wmod(App->cvi->imod, fout, (char *)qname.latin1());
    fclose(fout);
    break;

  case FWRITE_MENU_NFF: /* write NFF */
    releaseKeyboard();
    qname = QFileDialog::getSaveFileName(QString::null, QString::null, 0, 0, 
                                         "File to save as NFF:");
    if (qname.isEmpty())
      break;
    fout =  fopen((QDir::convertSeparators(qname)).latin1(), "w");
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
  case EDIT_MENU_MOVIES:
    imodMovieConDialog(App->cvi);
    break;
  case EDIT_MENU_GRAIN:
    fineGrainOpen(App->cvi);
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
      obsave = imod->cindex.object;
      cosave = vi->imod->cindex.contour;
      ptsave = vi->imod->cindex.point;
      detach = 0;
      for (ob = imod->objsize - 1; ob >= 0; ob--){
        /* Check if object has any contours with points; but don't
           delete the last object */
        hasdata = (imod->objsize == 1);
        obj = &imod->obj[ob];
        for (co = 0; co < (int)obj->contsize; co++){
          if (obj->cont[co].psize) {
            hasdata = 1;
            break;
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
  int ob,co,pt, coind, obOld, obNew;
  float vol;
  int cosave, ptsave;
  QString qstr;
  char *objtype;
  ImodView *vi = App->cvi;
  Imod *imod = vi->imod;

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
    break;
          
  case EOBJECT_MENU_DELETE: /* Delete */
    if (!obj || !dia_ask("Delete Object?"))
      break;
    vi->undo->objectRemoval();
    imodDeleteObject(imod, imod->cindex.object);
    vi->undo->finishUnit();
    imodSelectionListClear(vi);
    imodDraw(vi, IMOD_DRAW_MOD);
    imod_cmap(imod);
    imod_info_setobjcolor();
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
      if (!diaQInput(&obj_moveto, 1, imod->objsize, 0,
                     "Move all contours to selected object."))
        break;
      obNew = obj_moveto - 1; 

      /* DNM: need to set contour inside loop because each deletion
         sets it to -1; and need to not increment counter! 
         And need to not do it if it's the same object! */
      if (obNew != imod->cindex.object) {
        if (ilistSize(obj->store)) {
          vi->undo->objectPropChg();
          vi->undo->objectPropChg(obNew);
          for (co = 0; co <= obj->surfsize; co++) {
            istoreCopyContSurfItems(obj->store, &imod->obj[obNew].store, co, 
                                    co, 1);
            istoreDeleteContSurf(obj->store, co, 1);
          }
        }
        for (co = 0; co < (int)obj->contsize; ) {
          imod->cindex.contour = 0;
          imod_contour_move(obNew);
        }
        imod->cindex.contour = -1;
        imod->cindex.point = -1;
        vi->undo->finishUnit();
        
        // DNM 11/16/04: move these up into conditional block
        imodSelectionListClear(vi);
        imodDraw(vi, IMOD_DRAW_MOD);
      } else
        wprint("Must select a different object to move "
               "contours to.\n");
    }
    /* DNM: need to maintain separate object numbers for two functions */
    /*    vi->obj_moveto = obj_moveto; */
    break;

  case EOBJECT_MENU_INFO: /* stats */
    {
      float sa = 0.0f;
      int co,pt;
      Icont *cont;
      Iobj *obj = imodObjectGet(imod);
      if (!obj) break;

      vol = imodObjectVolume(obj);
      /* DNM 1/3/04: do not multiply by zbin because this is just summing 
         over all the contours which will be more frequent if Z binning */
      if (imod->pixsize)
        vol *= imod->pixsize * vi->xybin * imod->pixsize * vi->xybin
          * imod->pixsize;
      vol *= imod->zscale;
      wprint("Object %d: Volume = %g %s^3.\n", 
             imod->cindex.object + 1, vol, 
             imodUnits(imod));

      if (!iobjClose(obj->flags)) break;

      for(co = 0; co < (int)obj->contsize; co++){
        cont = &obj->cont[co];
        /* DNM 2/12/01: added test, because the for loop will
           execute indefinitely on SGI or PC when psize is 0
           because it is an unsigned int */
        if (cont->psize < 2) 
          continue;
        for(pt = 0; pt < (int)cont->psize - 1; pt++){
          sa += imodel_point_dist
            (&(cont->pts[pt]), &(cont->pts[pt+1]));
        }
        if (!imodContourIsOpen(cont)){
          sa += imodel_point_dist
            (cont->pts, &(cont->pts[cont->psize-1]));
        }

      }
      sa *= imod->pixsize * vi->xybin * imod->pixsize * imod->zscale;
      wprint("Object %d: Surface Area = %g%s^2.\n",
             imod->cindex.object + 1,sa,
             imodUnits(imod));
               
    }
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
    imodGetIndex(imod, &ob, &co, &pt);
    if (iobjClose(obj->flags)) 
      objtype = "closed contour";
    else if (iobjScat(obj->flags))
      objtype = "scattered point";
    else
      objtype = "open contour";
    qstr.sprintf("Are you sure you want to break all contours to lie in only"
                 " one Z plane in object %d, a %s object?", ob + 1, objtype);
    if (!dia_ask((char *)(qstr.latin1())))
      break;
    for (coind = obj->contsize - 1; coind >= 0; coind--) {
      cont = &obj->cont[coind];
      if (cont->psize) 
        imodContourBreakByZ(vi, obj, ob, coind);
    }
    if (co >= 0)
      imodSetIndex(imod, ob, co, pt);
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
    qstr.sprintf("Are you sure you want to delete the %d contours in "
                 "this surface?", numDel);
    if (numDel > 1 && !dia_ask((char *)qstr.latin1()))
      break;

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
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  int ob,co,pt, ptb;
  double dist;
  ImodView *vi = App->cvi;
  Imod *imod = vi->imod;

  if (ImodForbidLevel)
    return;

  cont = (struct Mod_Contour *)imodContourGet(imod);
     
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
    if (imod->mousemode == IMOD_MMODEL){
      vi->undo->clearUnits();
      imodObjectSort(imodObjectGet(imod));
      imod->cindex.contour = -1;
      imod->cindex.point   = -1;
      imodSelectionListClear(vi);
      imod_info_setocp();
    }else{
      wprint("\aError: Must be in Model mode to Sort.\n");
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
      if (!(cont->flags & ICONT_OPEN)){
        dist = imodContourArea(cont);
        dist *= imod->pixsize * imod->pixsize * vi->xybin * vi->xybin;
        wprint("2D area = %g square %s\n", dist, imodUnits(imod));
      }

      for(dist = 0.0, pt = 0; pt < (int)cont->psize-1; pt++){
        dist += imodel_point_dist(&(cont->pts[pt]), &(cont->pts[pt+1]));
      }
               
      /* 2.00b7 fix added */
      if (!(cont->flags & ICONT_OPEN))
        dist += imodel_point_dist
          (&(cont->pts[cont->psize-1]), cont->pts);
               
      dist *= imod->pixsize * vi->xybin;
      wprint("2D length = %g %s\n", dist, imodUnits(imod));
               
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
      wprint("3D scale length = %g %s\n", dist, imodUnits(imod));
    }
    if (iobjScat(obj->flags)){
      wprint("No stats available for scattered contours.\n");
    }
    break;

  case ECONTOUR_MENU_BREAK: /* break a contour into two */
    imodContEditBreak(vi);
    break;

  case ECONTOUR_MENU_FIXZ: /* break a contour at z transitions */
    obj = imodObjectGet(imod);
    if (!obj)
      break;      
    if (!cont)
      break;      
    if (!cont->psize)
      break;      
    if (!iobjClose(obj->flags)){
      wprint("\aError: Only Closed contours can be broken by Z\n");
      break;
    }
    imodGetIndex(imod, &ob, &co, &pt);
    imodContourBreakByZ(vi, obj, ob, co);

    // This takes care of point position
    imodSetIndex(imod, ob, co, pt);
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

  if (ImodForbidLevel)
    return;

  switch(item){
  case EPOINT_MENU_DELETE: /* Delete */
    inputDeletePoint(vi);
    break;
          
  case EPOINT_MENU_SORTDIST: /* Sort by distance */
    if (cont && imod->mousemode == IMOD_MMODEL){
      vi->undo->contourDataChg();
      imodel_contour_sort(imodContourGet(imod));
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
    Ipoint scale;
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

    wprint("\nDistance to previous point :\n2D = %g pixels, %g %s\n",
           dist2d, dist2d * imod->pixsize * vi->xybin,
           imodUnits(imod));
    wprint("3D = %g pixels, %g %s\n",
           dist3d, dist3d * imod->pixsize,
           imodUnits(imod));
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
    /* DNM 12/10/02: if busy loading, this will defer it */
    if (ivwFlip(App->cvi))
      break;
    /* DNM: check wild flag here */
    ivwCheckWildFlag(App->cvi->imod);
    imodDraw(App->cvi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
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
  // fprintf(stderr,"Edit image item %d\n", item);

  if (ImodForbidLevel)
    return;

  /* DNM: only model and zap will work with raw (color) data */
  if (App->cvi->rawImageStore && 
      !(item == IMAGE_MENU_ZAP || item == IMAGE_MENU_MODV))
    return;
     
  switch(item){

  case IMAGE_MENU_GRAPH: /* graph */
    /* DMN 2/25/01: do not open with fake image */
    if (!App->cvi->fakeImage)
      xgraphOpen(App->cvi);
    break;

  case IMAGE_MENU_SLICER: /* slice */
    sslice_open(App->cvi);
    /*    cut_open(); */
    break;
          
  case IMAGE_MENU_TUMBLER: /* tumble */
    xtumOpen(App->cvi);
    break;

  case IMAGE_MENU_MODV: /* model view */
    imod_autosave(App->cvi->imod);
    imodv_open();
    break;

  case IMAGE_MENU_ZAP:
    imod_zap_open(App->cvi);
    break;

  case IMAGE_MENU_XYZ:
    xxyz_open(App->cvi);
    break;

  case IMAGE_MENU_PIXEL:
    if (!App->cvi->fakeImage)
      open_pixelview(App->cvi);
    break;

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
    if (imodShowHelpPage("../man/3dmod.html"))
      dia_smsg(Imod_help_text);
    break;
  case HELP_MENU_MENUS:
    dia_smsg(Imod_menus_help);
    break;
  case HELP_MENU_HOTKEY:
    dia_smsg(Imod_hotkey_help);
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
   contours that are all coplanar in Z.  It returns a 
   pointer to the remnant of the original contour */
static Icont *imodContourBreakByZ(ImodView *vi, Iobj *obj, int ob, int co)
{
  int ni, oi, zcur, zprev, ptb, first;
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
        return cont;
      }
      imodel_contour_check_wild(cont2);
      imodObjectAddContour(obj, cont2);
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
  if (!first)
    vi->undo->finishUnit();
  return cont;
}

/*
  $Log$
  Revision 4.24  2005/06/26 19:37:24  mast
  Added fine-grain entries

  Revision 4.23  2005/03/20 19:55:36  mast
  Eliminating duplicate functions

  Revision 4.22  2004/12/24 02:18:31  mast
  Use man page insetad of out-of-date imodhelp

  Revision 4.21  2004/11/20 05:05:27  mast
  Changes for undo/redo capability

  Revision 4.20  2004/11/04 23:30:55  mast
  Changes for rounded button style

  Revision 4.19  2004/11/01 23:26:17  mast
  implemented surface deletion

  Revision 4.18  2004/10/12 15:23:09  mast
  Fixed string when writing as NFF

  Revision 4.17  2004/09/21 20:17:10  mast
  Added menu option to renumber object

  Revision 4.16  2004/06/04 03:17:36  mast
  Added argument to call to open model

  Revision 4.15  2004/01/05 18:38:13  mast
  Adjusted info outputs by binning as appropriate, and defined vi and imod
  in some menus for brevity

  Revision 4.14  2003/11/26 18:17:27  mast
  Disable image menu entries for raw or no images appropriately

  Revision 4.13  2003/09/16 02:53:15  mast
  Changed to pass the memory snapshot as an array of line pointers

  Revision 4.12  2003/07/31 22:12:27  mast
  Autostore views when writing a model as Imod

  Revision 4.11  2003/06/20 19:46:34  mast
  Allowed break by Z to work for any kind of pbject, with better warning

  Revision 4.10  2003/06/19 05:48:17  mast
  Added ability to break all contours in object by Z value

  Revision 4.9  2003/06/04 23:32:25  mast
  Output coordinates numbered from one in point value output

  Revision 4.8  2003/04/25 03:28:32  mast
  Changes for name change to 3dmod

  Revision 4.7  2003/04/18 00:46:53  mast
  Call inputNewContour when making new contour toget times right

  Revision 4.6  2003/04/16 18:46:51  mast
  hide/show changes

  Revision 4.5  2003/03/28 23:51:11  mast
  changes for Mac problems

  Revision 4.4  2003/03/24 17:58:09  mast
  Changes for new preferences capability

  Revision 4.3  2003/02/27 19:29:31  mast
  Use Qt functions to manage filenames

  Revision 4.2  2003/02/12 21:39:51  mast
  Fix problem with getting new object after deleting all objects

  Revision 4.1  2003/02/10 20:29:00  mast
  autox.cpp

  Revision 1.1.2.11  2003/01/29 01:33:15  mast
  changes for colormap switching

  Revision 1.1.2.10  2003/01/27 00:30:07  mast
  Pure Qt version and general cleanup

  Revision 1.1.2.9  2003/01/23 20:07:02  mast
  add include for imod_cont_copy

  Revision 1.1.2.8  2003/01/18 01:15:56  mast
  add include for cache filler

  Revision 1.1.2.7  2003/01/14 21:52:38  mast
  include new movie controller include file

  Revision 1.1.2.6  2003/01/13 01:00:28  mast
  Qt version

  Revision 1.1.2.5  2003/01/10 23:52:17  mast
  Changes for Qt version of tumbler and elimination of tilt window

  Revision 1.1.2.4  2003/01/06 15:52:39  mast
  changes for Qt version of slicer and new object color routines

  Revision 1.1.2.3  2002/12/19 04:37:13  mast
  Cleanup of unused global variables and defines

  Revision 1.1.2.2  2002/12/17 18:40:24  mast
  Changes and new includes with Qt version of imodv

  Revision 1.1.2.1  2002/12/13 06:15:49  mast
  include file changes

  Revision 3.10.2.2  2002/12/11 00:41:00  mast
  Prevent flipping while loading data

  Revision 3.10.2.1  2002/12/05 16:23:52  mast
  No changes - CVS detected as modified in branch

  Revision 3.11  2002/12/03 15:49:07  mast
  consistently set the forbid-level before any potential file dialog to
  prevent multiple file dialogs from appearing; switched memory save to
  using dia_filename so that this would work there as well.

  Revision 3.10  2002/12/01 15:34:41  mast
  Changes to get clean compilation with g++

  Revision 3.9  2002/11/05 23:26:39  mast
  Changed copyright notice to use lab name and years

  Revision 3.8  2002/09/27 20:24:57  rickg
  Moved IO functionality into imod_io
  Move client message functionality into imod_client_message since it was no
  longer dependent upon any code in this model due to the changes to imod_io.

  Revision 3.7  2002/09/19 22:52:54  rickg
  Added MESSAGE_QUIT case for receiving of events.

  Revision 3.6  2002/09/17 18:52:44  mast
  Changed event handler to expect a signature (IMOD), and to give error
  messages about unexpected packets only in debig mode

  Revision 3.5  2002/09/14 18:32:43  mast
  Eliminate unneeded argument from fprintf

  Revision 3.4  2002/09/13 23:44:44  mast
  Changes after testing out most error conditions for messages

  Revision 3.3  2002/09/13 21:09:54  mast
  Added message handler to process external ClientMessage events for opening
  and saving model files.

  Revision 3.2  2002/05/20 15:34:04  mast
  Made time index modeling be the default for a new object (or new model)
  if multiple files are open

  Revision 3.1  2001/12/17 18:45:54  mast
  Added calls for cache filling

*/
