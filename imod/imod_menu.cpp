/*  IMOD VERSION 2.7.9
 *
 *  imod_menu.cpp -- Menu slots for the imod information window; part
 *                       of the InfoWindow class declared in imod_info.h
 *                       and constructed in imod_info.cpp
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Fine    *
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
#include "preferences.h"

/****help text data include files*****/
#include "imodhelp.h" 
#include "menus.h"
#include "hotkey.h"


static int obj_moveto = 0;

/*
 * THE FILE MENU
 */
void InfoWindow::fileSlot(int item)
{
  int returnValue;
  int limits[4];
  unsigned char *data;
  QString qname;

  if (ImodForbidLevel){
    if (item == 6)
      imod_quit();
    return;
  }

  switch(item){

  case FILE_MENU_NEW: /* New */
    createNewModel(NULL);
    break;
	  
  case FILE_MENU_OPEN: /* open */
    //  forbid input during file dialog; swallow any pending events
    imod_info_forbid();
    imod_info_input();

    releaseKeyboard();
    returnValue = openModel(NULL);
    imod_info_enable();

    if(returnValue == IMOD_IO_SUCCESS) {
      wprint("Model loaded.\n");
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
    /*	       wprint("Error Saving Model."); DNM: it already has message*/
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
                                       "File to save as Wimp:");
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
 * THE EDIT MENU: Just movies
 */
void InfoWindow::editSlot(int item)
{
  if (item == EDIT_MENU_MOVIES)
    imodMovieConDialog(App->cvi);
  else if (item == EDIT_MENU_PREFS)
    ImodPrefs->editPrefs();
}


/*
 * THE EDIT MODEL MENU
 */
void InfoWindow::editModelSlot(int item)
{
  Imod *imod = App->cvi->imod;
  int ob, co, detach, hasdata, obsave;
  int cosave, ptsave;
  Iobj *obj;

  if (ImodForbidLevel)
    return;

  switch(item){
    case EMODEL_MENU_HEADER: 
    openModelEdit(App->cvi);
    break;
    case EMODEL_MENU_OFFSETS:
    openModelOffset(App->cvi);
    break;
    case EMODEL_MENU_CLEAN:
    /* Clean model means delete empty objects */
    if (dia_ask("Delete all empty objects?")) {
      obsave = imod->cindex.object;
      cosave = App->cvi->imod->cindex.contour;
      ptsave = App->cvi->imod->cindex.point;
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
          imodFreeObject(imod, ob);
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
      imod_cmap(App->cvi->imod);
      imod_info_setobjcolor();
      imodDraw(App->cvi, IMOD_DRAW_RETHINK);
    }
    break;
  }
}

/*
 * THE EDIT OBJECT MENU
 */
void InfoWindow::editObjectSlot(int item)
{
  struct Mod_Object *obj;
  int co;
  float vol;
  int cosave, ptsave;

  if (ImodForbidLevel)
    return;

  obj = imodel_object_get(App->cvi->imod);
  if (!obj && item != EOBJECT_MENU_NEW)
    return;
     
  switch(item){
  case EOBJECT_MENU_NEW: /* New */
    inputNewObject(App->cvi);
    imod_object_edit();
    imod_info_setobjcolor();
    break;
	  
  case EOBJECT_MENU_DELETE: /* Delete */
    if (obj)
      if (dia_ask("Delete Object?")) {
        imodFreeObject(App->cvi->imod,
                       App->cvi->imod->cindex.object);
      }
    imodDraw(App->cvi, IMOD_DRAW_MOD);
    imod_cmap(App->cvi->imod);
    imod_info_setobjcolor();
    break;
	  
  case EOBJECT_MENU_COLOR: /* Color */
    imod_info_forbid();
    imod_info_input();
    imod_info_enable();
	  
    imod_object_color(App->cvi->imod->cindex.object);
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
    if (App->cvi->imod->objsize < 2){
      wprint("Must have more than one object to move contours "
             "to a new object\n");
      break;
    }
    if (obj_moveto > (int)App->cvi->imod->objsize)
      obj_moveto = App->cvi->imod->objsize;
    if (obj_moveto < 1)
      obj_moveto = 1;

    if (App->cvi->imod->cindex.object > -1){
      if (!diaQInput(&obj_moveto, 1, App->cvi->imod->objsize, 0,
		   "Move all contours to selected object."))
	break;
      /* DNM: need to set contour inside loop because each deletion
         sets it to -1; and need to not increment counter! 
         And need to not do it if it's the same object! */
      if (obj_moveto - 1 != App->cvi->imod->cindex.object) {
        for(co = 0; co < (int)obj->contsize; ) {
          App->cvi->imod->cindex.contour = 0;
          imod_contour_move(obj_moveto - 1);
        }
        App->cvi->imod->cindex.contour = -1;
        App->cvi->imod->cindex.point = -1;
      } else
        wprint("Must select a different object to move "
			   "contours to.\n");
    }
    /* DNM: need to maintain separate object numbers for two functions */
    /*	  App->cvi->obj_moveto = obj_moveto; */
    imodDraw(App->cvi, IMOD_DRAW_MOD);
    break;

  case EOBJECT_MENU_INFO: /* stats */
    {
      float sa = 0.0f;
      int co,pt;
      Icont *cont;
      Iobj *obj = imodObjectGet(App->cvi->imod);
      if (!obj) break;

      vol = imodObjectVolume(obj);
      if (App->cvi->imod->pixsize)
        vol *= App->cvi->imod->pixsize 
          * App->cvi->imod->pixsize 
          * App->cvi->imod->pixsize;
      vol *= App->cvi->imod->zscale;
      wprint("Object %d: Volume = %g %s^3.\n", 
             App->cvi->imod->cindex.object + 1, vol, 
             imodUnits(App->cvi->imod));

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
      sa *= App->cvi->imod->pixsize * App->cvi->imod->pixsize *
        App->cvi->imod->zscale;
      wprint("Object %d: Surface Area = %g%s^2.\n",
             App->cvi->imod->cindex.object + 1,sa,
             imodUnits(App->cvi->imod));
	       
    }
    break;

  case EOBJECT_MENU_CLEAN: /* Clean: delete empty contours */
    obj = imodObjectGet(App->cvi->imod);
    if (!obj)
      break;
    cosave = App->cvi->imod->cindex.contour;
    ptsave = App->cvi->imod->cindex.point;
    for (co = obj->contsize - 1; co >= 0; co--){
      if (!obj->cont[co].psize) {
        /* delete contour if it is empty */
        App->cvi->imod->cindex.contour = co;
        DelContour(App->cvi->imod, co);
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
      App->cvi->imod->cindex.contour = cosave;
      App->cvi->imod->cindex.point = ptsave;
    }

    imodDraw(App->cvi, IMOD_DRAW_RETHINK);
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
    imodContEditMoveDialog(App->cvi);
    break;
  }
}
	  
/*
 * THE EDIT CONTOUR MENU
 */
void InfoWindow::editContourSlot(int item)
{
  struct Mod_Object *obj;
  struct Mod_Contour *cont, *cont2;
  int ob,co,pt, ptb;
  double dist;

  if (ImodForbidLevel)
    return;

  cont = (struct Mod_Contour *)imodContourGet(App->cvi->imod);
     
  switch(item){
  case ECONTOUR_MENU_NEW: /* new */
    NewContour(App->cvi->imod);
    imod_info_setocp();
    break;
	  
  case ECONTOUR_MENU_DELETE: /* del */
    inputDeleteContour(App->cvi);
    break;
	  
  case ECONTOUR_MENU_MOVE: /* move */
    if (App->cvi->imod->objsize < 2){
      wprint("Must have more than one object to move contours "
             "to a new object\n");
      break;
    }
    imodContEditMoveDialog(App->cvi);
    break;
	  
  case ECONTOUR_MENU_SORT: /* sort */
    if (App->cvi->imod->mousemode == IMOD_MMODEL){
      imodObjectSort(imodel_object_get(App->cvi->imod));
      App->cvi->imod->cindex.contour = -1;
      App->cvi->imod->cindex.point   = -1;
      imod_info_setocp();
    }else{
      wprint("\aError: Must be in Model mode to Sort.\n");
    }
    break;

  case ECONTOUR_MENU_AUTO: /* auto */
    autox_open(App->cvi);
    imod_info_setocp();
    break;

  case ECONTOUR_MENU_TYPE: /* surface */
    imodContEditSurf(App->cvi);
    break;


  case ECONTOUR_MENU_INFO: /* Print Stats */
    if (!cont)
      return;
    if (!cont->psize)
      return;
    obj = imodel_object_get(App->cvi->imod);
    if (!obj) break;	  
    wprint("Obj: %d, Cont %d\n", 
           App->cvi->imod->cindex.object+1, 
           App->cvi->imod->cindex.contour+1);

    if (iobjClose(obj->flags)){
	      
      /* 2.00b7 fix added */
      if (!(cont->flags & ICONT_OPEN)){
        dist = imodContourArea(cont);
        dist *= App->cvi->imod->pixsize * App->cvi->imod->pixsize;
        wprint("2D area = %g square %s\n", 
               dist, imodUnits(App->cvi->imod));
      }

      for(dist = 0.0, pt = 0; pt < (int)cont->psize-1; pt++){
        dist += imodel_point_dist(&(cont->pts[pt]),
                                  &(cont->pts[pt+1]));
      }
	       
      /* 2.00b7 fix added */
      if (!(cont->flags & ICONT_OPEN))
        dist += imodel_point_dist
          (&(cont->pts[cont->psize-1]), cont->pts);
	       
      dist *= App->cvi->imod->pixsize;
      wprint("2D length = %g %s\n", dist, imodUnits(App->cvi->imod));
	       
    }
    if (iobjOpen(obj->flags)){
      Ipoint scale;
      scale.x = App->cvi->imod->xscale;
      scale.y = App->cvi->imod->yscale;
      scale.z = App->cvi->imod->zscale;
      for(dist = 0.0, pt = 0; pt < (int)cont->psize-1; pt++){
        dist += imodPoint3DScaleDistance
          (&(cont->pts[pt]), &(cont->pts[pt+1]), &scale);
      }

      /* 2.00b7 fix removed */
      /*dist += imodPoint3DScaleDistance
        (&(cont->pts[cont->psize-1]), cont->pts, &scale);
      */

      dist *= App->cvi->imod->pixsize;
      wprint("3D scale length = %g %s\n", 
             dist, imodUnits(App->cvi->imod));
    }
    if (iobjScat(obj->flags)){
      wprint("No stats available for scattered contours.\n");
    }
    break;

  case ECONTOUR_MENU_BREAK: /* break a contour into two */
    imodContEditBreak(App->cvi);
    break;

  case ECONTOUR_MENU_FIXZ: /* break a contour at z transitions */
    obj = imodel_object_get(App->cvi->imod);
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
    imodGetIndex(App->cvi->imod, &ob, &co, &pt);
    for (ptb = cont->psize - 1; ptb > 0; ptb--) {
      int ni, oi, zcur, zprev;
      zcur = (int)(floor(cont->pts[ptb].z + 0.5));
      zprev = (int)(floor(cont->pts[ptb - 1].z + 0.5));
      if (zcur != zprev) {
        cont2 = imodContourDup(cont);
        ni = 0;
        for (oi = ptb; oi < (int)cont->psize; oi++, ni++) {
          cont2->pts[ni] = cont->pts[oi];
          if (cont->sizes)
            cont2->sizes[ni] = cont->sizes[oi];
        }
        cont2->psize = ni;
        cont->psize = ptb;
        imodel_contour_check_wild(cont2);
        imodObjectAddContour(obj, cont2);
        cont = imodContourGet(App->cvi->imod);
      }
    }
    imodel_contour_check_wild(cont);
    if (pt >= (int)cont->psize)
      pt = cont->psize - 1;
    imodSetIndex(App->cvi->imod, ob, co, pt);
    imodDraw(App->cvi, IMOD_DRAW_MOD);
    break;

  case ECONTOUR_MENU_JOIN: /* join two contour together. */
    imodContEditJoin(App->cvi, 0, 0);
    break;

  case ECONTOUR_MENU_INVERT: /* invert a contour */
    if (!imodel_contour_invert(cont)) {
      imodGetIndex(App->cvi->imod, &ob, &co, &pt);
      pt = (cont->psize - 1) - pt;
      imodSetIndex(App->cvi->imod, ob, co, pt);
      imodDraw(App->cvi, IMOD_DRAW_MOD);
    }
    break;

  case ECONTOUR_MENU_COPY: /* duplicate current contour. */
    openContourCopyDialog(App->cvi);
    break;

  case ECONTOUR_MENU_LOOPBACK: /* Loop back to start to make complex cap */
    for (ptb = cont->psize - 2; ptb > 0; ptb--)
      imodPointAppend(cont, &cont->pts[ptb]);
    break;

  case ECONTOUR_MENU_FILLIN: /* fill in a contour at every z level: Open contours only */
    obj = imodel_object_get(App->cvi->imod);
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
    imodGetIndex(App->cvi->imod, &ob, &co, &pt);
    for (ptb = 0; ptb < (int)cont->psize - 1; ptb++) {
      int zcur, znext, zfill;
      Ipoint newPt;
      Ipoint *cur, *next;
      zcur = (int)(floor(cont->pts[ptb].z + 0.5));
      znext = (int)(floor(cont->pts[ptb + 1].z + 0.5));
      zfill = zcur;

      /* find points where rounded z differs by more than one */
      if (zcur - znext > 1)
        zfill--;
      else if (znext - zcur > 1)
        zfill++;
      if (zcur != zfill) {
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

    imodSetIndex(App->cvi->imod, ob, co, pt);
    imodDraw(App->cvi, IMOD_DRAW_MOD);
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

  if (ImodForbidLevel)
    return;

  switch(item){
  case EPOINT_MENU_DELETE: /* Delete */
    inputDeletePoint(App->cvi);
    break;
	  
  case EPOINT_MENU_SORTDIST: /* Sort by distance */
    if (App->cvi->imod->mousemode == IMOD_MMODEL){
      imodel_contour_sort(imodContourGet(App->cvi->imod));
      imodDraw(App->cvi, IMOD_DRAW_MOD);
    }
    break;

  case EPOINT_MENU_SORTZ: /* Sort by Z */
    if (App->cvi->imod->mousemode == IMOD_MMODEL){
      Icont *cont = imodContourGet(App->cvi->imod);
      if (cont)
        if (cont->psize) {
          imodel_contour_sortz(cont, 0, cont->psize - 1);
          imodDraw(App->cvi, IMOD_DRAW_MOD);
        }
    }
    break;
	  
  case EPOINT_MENU_DIST: /* dist */{
	     
    Icont *cont = imodContourGet(App->cvi->imod);
    int    pt   = App->cvi->imod->cindex.point;
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

    scale.x = App->cvi->imod->xscale;
    scale.y = App->cvi->imod->yscale;
    scale.z = App->cvi->imod->zscale;
	     
    dist2d  = imodel_point_dist(cpt, ppt);
    dist3d  = imodPoint3DScaleDistance(cpt, ppt, &scale);

    wprint("\nDistance to previous point :\n2D = %g pixels, %g %s\n",
           dist2d, dist2d * App->cvi->imod->pixsize,
           imodUnits(App->cvi->imod));
    wprint("3D = %g pixels, %g %s\n",
           dist3d, dist3d * App->cvi->imod->pixsize,
           imodUnits(App->cvi->imod));
    break;
  }
  case EPOINT_MENU_VALUE: /* value */
    wprint("Pixel value from file:\n (%g, %g, %g) = %g",
           App->cvi->xmouse, App->cvi->ymouse, App->cvi->zmouse,
           ivwGetFileValue(App->cvi, (int)App->cvi->xmouse,
                           (int)App->cvi->ymouse, (int)App->cvi->zmouse));
    break;

  case EPOINT_MENU_SIZE: /* size */
    imodContEditSurf(App->cvi);
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
#ifdef __ppc__
  if (mDeferredItem < 0) {
    //  fprintf(stderr, "Starting defer timer item %d\n", item);
    mDeferredItem = item;
    mDeferTimer->start(1, true);
    return;
  }
#endif

  /* DNM: only model and zap will work with raw (color) data */
  if (App->cvi->rawImageStore && !(item == 5 || item == 4))
    return;
     
  switch(item){

  case IMAGE_MENU_GRAPH: /* graph */
    /* DMN 2/25/01: do not open with fake image */
    if (!App->cvi->fakeImage)
      xgraphOpen(App->cvi);
    break;

  case IMAGE_MENU_SLICER: /* slice */
    sslice_open(App->cvi);
    /*	  cut_open(); */
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
      ("Imod Version ",
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

/*
$Log$
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
