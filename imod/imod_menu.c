/*  IMOD VERSION 2.50
 *
 *  imod_menu.c -- Menu callbacks for the imod information window.
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
#include <stdio.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/AtomMgr.h>
#include <dia.h>
#include "imod.h"
#include "imod_info.h"
#include "imod_io.h"
#include "imodel.h"
#include "mrcfiles.h"
#include "options.h"
#include "iproc.h"

/****help text data include files*****/
#include "imodhelp.h" 
#include "menus.h"
#include "hotkey.h"


void ioew_sgicolor_cb(Widget w, XtPointer client, XtPointer call);
void imod_file_cb(Widget w, XtPointer client, XtPointer call);

extern int Imod_debug;

void imod_file_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;
  int returnValue;
  char *filestr = NULL;
  int limits[4];
  unsigned char *data;

  if (ImodForbidLevel){
    if (item == 6)
      imod_quit();
    return;
  }

  switch(item){

  case 0: /* New */
    createNewModel(NULL);
    break;
	  
  case 1: /* open */
    //  forbid input during file dialog; swallow any pending events
    imod_info_forbid();
    imod_info_input();

    returnValue = openModel(filestr);
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
      
  case 2: /* save */
    imod_info_forbid();
    imod_info_input();
    App->cvi->imod->blacklevel = App->cvi->black;
    App->cvi->imod->whitelevel = App->cvi->white;
    if (SaveModel(App->cvi->imod));
    /*	       wprint("Error Saving Model."); DNM: it already has message*/
    imod_info_enable();
    break;

  case 3: /* save as */
    imod_info_forbid();
    imod_info_input();
    App->cvi->imod->blacklevel = App->cvi->black;
    App->cvi->imod->whitelevel = App->cvi->white;
    SaveasModel(App->cvi->imod);
    imod_info_enable();
    break;
	  
  case 4:
    /* File format requester */
    break;

  case 5:  /* Save raw image to tiff file */
    if (!App->cvi->rawImageStore)
      wprint("This option works only with color images.\n");
    else {
      /* DNM 12/2/02: switch to dia_filename so that the forbid/enable can
         be used, and move saving functionality into this case */
      imod_info_forbid();
      imod_info_input();
      filestr = dia_filename("TIFF File to save section from memory");
      imod_info_enable();
      if (!filestr)
        break;
      data = ivwGetCurrentZSection(App->cvi);
      limits[0] = limits[1] = 0;
      limits[2] = App->cvi->xsize;
      limits[3] = App->cvi->ysize;
      b3dSnapshot_TIF(filestr, 0, limits, data);
      XtFree(filestr);
    }
    break;


  case 6:
    imod_info_forbid();
    imod_info_input();
    imod_info_enable();
    dia_vasmsg
      ("Imod Version ",
       VERSION_NAME, "[", __DATE__, __TIME__, "]",
       "originally written by James Kremer and revised by",
       "David Mastronarde\n",
       "Copyright (C)",COPYRIGHT_YEARS,"by",LAB_NAME1,"\n",LAB_NAME2,
       "& Regents of the University of Colorado\n\n",
       NULL);
    break;

  case 7:
    imod_quit();
    break;
       
  case 99:
    dia_print("never");
    break;
       
  default:
    break;
  }

}

void imod_file_write_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;
  FILE *fout;
  char *filename;

  if (ImodForbidLevel)
    return;

  /* DNM 12/2/02: add the forbid, change returns to breaks */
  imod_info_forbid();
  imod_info_input();

  switch (item){

  case 0: /* write imod */
    filename = dia_filename("Write file as Imod");
    if (!filename)
      break;
    fout =  fopen(filename, "w");
    if (!fout){
      free(filename);
      break;
    }
    imodWrite(App->cvi->imod, fout);
    free(filename);
    fclose(fout);
    break;

  case 1: /* write wimp */
    filename = dia_filename("Write file as wimp");
    if (!filename)
      break;
    fout =  fopen(filename, "w");
    if (!fout){
      free(filename);
      break;
    }
    imod_to_wmod(App->cvi->imod, fout, filename);
    free(filename);
    fclose(fout);
    break;

  case 2: /* write NFF */
    filename = dia_filename("Write file as NFF");
    if (!filename)
      break;
    fout =  fopen(filename, "w");
    if (!fout){
      free(filename);
      break;
    }
    imod_to_nff(App->cvi->imod, fout);
    free(filename);
    fclose(fout);
    break;

  case 3: /* write synu */
    imod_to_synu(App->cvi->imod);
    break;

  }
  imod_info_enable();
}


void imod_edit_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;
  if (item == 6)
    imodMovieConDialog(App->cvi);
}

void imod_edit_model_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;
  Imod *imod = App->cvi->imod;
  int ob, co, detach, hasdata, obsave;
  int cosave, ptsave;
  Iobj *obj;

  if (ImodForbidLevel)
    return;

  switch(item){
  case 0: 
    openModelEdit(App->cvi);
    break;
  case 1:
    openModelOffset(App->cvi);
    break;
  case 2:
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
        for (co = 0; co < obj->contsize; co++){
          if (obj->cont[co].psize) {
            hasdata = 1;
            break;
          }
        }
        if (!hasdata) {
          /* delete object if it is empty */
          if (App->rgba)
            free_object_colors(imod, ob, ob);
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
        if (obsave >= imod->objsize)
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

void imod_edit_object_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;
  short red, green, blue;
  struct Mod_Object *obj;
  int ob, co;
  long cob;
  char prompt[32];
  float vol;
  int cosave, ptsave;

  if (ImodForbidLevel)
    return;

  obj = imodel_object_get(App->cvi->imod);
  if (!obj)
    if (item)
      return;
     
  switch(item){
  case 0: /* New */
    inputNewObject(App->cvi);
    imod_object_edit(App->toplevel);
    imod_info_setobjcolor();
    break;
	  
  case 1: /* Delete */
    if (obj)
      if (dia_ask("Delete Object?")) {
        if (App->rgba)
          free_object_colors(App->cvi->imod,
                             App->cvi->imod->cindex.object,
                             App->cvi->imod->cindex.object);
        imodFreeObject(App->cvi->imod,
                       App->cvi->imod->cindex.object);
      }
    imodDraw(App->cvi, IMOD_DRAW_MOD);
    imod_cmap(App->cvi->imod);
    imod_info_setobjcolor();
    break;
	  
  case 2: /* Color */
    imod_info_forbid();
    imod_info_input();
    imod_info_enable();
	  
    red = (short int)(obj->red * 255);
    blue = (short int)(obj->blue * 255);
    green = (short int)(obj->green * 255);
    ob = App->cvi->imod->cindex.object;
    cob = ob;
    sprintf(prompt, "Imod: Object %d color.", App->cvi->imod->cindex.object + 1);
    dia_setcolor(red, green, blue, prompt, ioew_sgicolor_cb, (XtPointer)cob);
    break;

  case 3: /* type*/
    imod_object_edit(App->toplevel);
    imod_draw_window();
    imod_info_setobjcolor();
    break;
	  
  case 4: /* goto */
    if (App->cvi->imod->objsize < 2){
      wprint("Must have more than one object to go "
             "to a different object\n");
      break;
    }
    ob = App->cvi->imod->cindex.object + 1;
    if (ob < 1)
      ob = 1;
    imod_info_forbid();
    imod_info_input();
    App->cvi->imod->cindex.object = dia_int(1, App->cvi->imod->objsize, ob, 0,
                                            "Select Object Number") - 1;
    App->cvi->imod->cindex.contour = -1;
    App->cvi->imod->cindex.point = -1;
    imod_info_enable();	  
    imod_setxyzmouse();
    imod_object_edit_draw();
    break;
	
  case 5: /* move */
    imod_info_forbid();
    imod_info_input();
    imod_info_enable();
    if (App->cvi->imod->objsize < 2){
      wprint("Must have more than one object to move contours "
             "to a new object\n");
      break;
    }
    if (Imod_obj_moveto > App->cvi->imod->objsize)
      Imod_obj_moveto = App->cvi->imod->objsize;
    if (Imod_obj_moveto < 1)
      Imod_obj_moveto = 1;

    if (App->cvi->imod->cindex.object > -1){
      Imod_obj_moveto = dia_int
        (1, App->cvi->imod->objsize, Imod_obj_moveto, 0,
         "Move all contours to selected object.");
      /* DNM: need to set contour inside loop because each deletion
         sets it to -1; and need to not increment counter! 
         And need to not do it if it's the same object! */
      if (Imod_obj_moveto - 1 != App->cvi->imod->cindex.object) {
        for(co = 0; co < obj->contsize; ) {
          App->cvi->imod->cindex.contour = 0;
          imod_contour_move(Imod_obj_moveto - 1);
        }
        App->cvi->imod->cindex.contour = -1;
        App->cvi->imod->cindex.point = -1;
      } else
        wprint("Must select a different object to move "
			   "contours to.\n");
    }
    /* DNM: need to maintain separate object numbers for two functions */
    /*	  App->cvi->obj_moveto = Imod_obj_moveto; */
    imodDraw(App->cvi, IMOD_DRAW_MOD);
    break;

  case 6: /* stats */
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

      for(co = 0; co < obj->contsize; co++){
        cont = &obj->cont[co];
        /* DNM 2/12/01: added test, because the for loop will
           execute indefinitely on SGI or PC when psize is 0
           because it is an unsigned int */
        if (cont->psize < 2) 
          continue;
        for(pt = 0; pt < cont->psize - 1; pt++){
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

  case 7: /* Clean: delete empty contours */
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

void imod_edit_surface_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;

  if (ImodForbidLevel)
    return;

  switch(item){
  case 0: /* new */
    inputNewSurface(App->cvi);
    break;
	  
  case 1: /* go to */
    inputContourSurf(App->cvi);
    break;
	  
  case 2: /* move */
    inputContourMoveDialog(App->cvi);
    break;
  }
}
	  
void imod_edit_contour_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;
     
  struct Mod_Object *obj;
  struct Mod_Contour *cont, *cont2;
  int ob,co,pt, ptb;
  double dist;

  if (ImodForbidLevel)
    return;

  cont = (struct Mod_Contour *)imodContourGet(App->cvi->imod);
     
  switch(item){
  case 0: /* new */
    NewContour(App->cvi->imod);
    imod_info_setocp();
    break;
	  
  case 1: /* del */
    inputDeleteContour(App->cvi);
    break;
	  
  case 2: /* move */
    if (App->cvi->imod->objsize < 2){
      wprint("Must have more than one object to move contours "
             "to a new object\n");
      break;
    }
    inputContourMoveDialog(App->cvi);
    break;
	  
  case 3: /* sort */
    if (App->cvi->imod->mousemode == IMOD_MMODEL){
      imodObjectSort(imodel_object_get(App->cvi->imod));
      App->cvi->imod->cindex.contour = -1;
      App->cvi->imod->cindex.point   = -1;
      imod_info_setocp();
    }else{
      wprint("\aError: Must be in Model mode to Sort.\n");
    }
    break;

  case 4: /* auto */
    autox_open(App->cvi);
    imod_info_setocp();
    break;

  case 5: /* surface */
    inputContourSurf(App->cvi);
    break;

  case 6: /* Goto */
    obj = imodel_object_get(App->cvi->imod);
    if (!obj)
      break;
    if (obj->contsize < 2)
      break;
    co = App->cvi->imod->cindex.contour + 1;
    if (co < 1)
      co = 1;
	  
    imod_info_forbid();
    imod_info_input();
    App->cvi->imod->cindex.contour = dia_int(1, obj->contsize, co, 0,
                                             "Select Contour Number") - 1;
    App->cvi->imod->cindex.point = -1;
    imod_info_enable();
    imod_setxyzmouse();
    break;


  case 7: /* Print Stats */
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

      for(dist = 0.0, pt = 0; pt < cont->psize-1; pt++){
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
      for(dist = 0.0, pt = 0; pt < cont->psize-1; pt++){
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

  case 8: /* break a contour into two */
    inputContourBreak(App->cvi);
    break;

  case 9: /* break a contour at z transitions */
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
        for (oi = ptb; oi < cont->psize; oi++, ni++) {
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
    if (pt >= cont->psize)
      pt = cont->psize - 1;
    imodSetIndex(App->cvi->imod, ob, co, pt);
    imodDraw(App->cvi, IMOD_DRAW_MOD);
    break;

  case 10: /* join two contour together. */
    inputContourJoin(App->cvi, 0, 0);
    break;

  case 11: /* invert a contour */
    if (!imodel_contour_invert(cont)) {
      imodGetIndex(App->cvi->imod, &ob, &co, &pt);
      pt = (cont->psize - 1) - pt;
      imodSetIndex(App->cvi->imod, ob, co, pt);
      imodDraw(App->cvi, IMOD_DRAW_MOD);
    }
    break;

  case 12: /* duplicate current contour. */
    openContourCopyDialog(App->cvi);
    break;

  case 13: /* Loop back to start to make complex cap */
    for (ptb = cont->psize - 2; ptb > 0; ptb--)
      imodPointAppend(cont, &cont->pts[ptb]);
    break;

  case 14: /* fill in a contour at every z level: Open contours only */
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
    for (ptb = 0; ptb < cont->psize - 1; ptb++) {
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

void imod_edit_point_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;
  struct Mod_Contour *cont;
  int pt;

  if (ImodForbidLevel)
    return;

  switch(item){
  case 0: /* Delete */
    inputDeletePoint(App->cvi);
    break;
	  
  case 1: /* Sort by distance */
    if (App->cvi->imod->mousemode == IMOD_MMODEL){
      imodel_contour_sort(imodContourGet(App->cvi->imod));
      imodDraw(App->cvi, IMOD_DRAW_MOD);
    }
    break;

  case 2: /* Sort by Z */
    if (App->cvi->imod->mousemode == IMOD_MMODEL){
      Icont *cont = imodContourGet(App->cvi->imod);
      if (cont)
        if (cont->psize) {
          imodel_contour_sortz(cont, 0, cont->psize - 1);
          imodDraw(App->cvi, IMOD_DRAW_MOD);
        }
    }
    break;
	  
  case 3: /* dist */{
	     
    Icont *cont = imodContourGet(App->cvi->imod);
    int    pt   = App->cvi->imod->cindex.point;
    Ipoint *cpt, *ppt;
    Ipoint scale;
    float dist2d, dist3d;

    /* DNM: psize must be at least 2, not 0 or 1 */
    if ((!cont) || (cont->psize < 2) || (pt < 0)){
      XBell(App->display, 100);
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
  case 4: /* value */
    wprint("Pixel value from file:\n (%g, %g, %g) = %g",
           App->cvi->xmouse, App->cvi->ymouse, App->cvi->zmouse,
           ivwGetFileValue(App->cvi, (int)App->cvi->xmouse,
                           (int)App->cvi->ymouse, (int)App->cvi->zmouse));
    break;

  case 5: /* Go to */
    cont = imodContourGet(App->cvi->imod);
    if (!cont)
      break;
    if (cont->psize < 2)
      break;
    pt = App->cvi->imod->cindex.point + 1;
    if (pt < 0)
      pt = 1;

    App->cvi->imod->cindex.point = dia_int(1, cont->psize, pt, 0,
                                           "Select Point Number") - 1;
    imod_setxyzmouse();
    break;
	  
  case 6: /* size */
    inputContourSurf(App->cvi);
    break;

  default:
    break;
	  
  }
     
}

void imod_edit_image_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;

  if (ImodForbidLevel)
    return;

  switch(item){
  case 0:
    inputIProcOpen(App->cvi);
    break;

  case 1: /* rampbase */
    if (App->rgba)
      wprint("Not needed for TrueColor graphics.\n");
    else if (App->depth <= 8){
      wprint("Not available for 8-bit graphics.\n");
    }else{
      Rampbase = App->cvi->rampbase = App->base =
        ((dia_int(1, 12, ((App->cvi->rampbase-256)/330) + 1, 0,
                  "New Colormap #")-1) * 330) + 256;
      App->objbase  = App->base + 257;
      imod_cmap(App->cvi->imod);
      imodDraw(App->cvi, IMOD_DRAW_IMAGE | IMOD_DRAW_MOD);
	       
#ifdef DRAW_GL
      cmap_setrampbase(Rampbase);
      adjustcmap_pf(&App->cvi->black, &App->cvi->white, Rampbase);
#else
      xcrampNewBase(App->cvi->cramp, Rampbase);
      xcramp_setlevels(App->cvi->cramp, App->cvi->black,App->cvi->white);
#endif
      break;
    }
    break;

  case 2:
    imodImageScaleDialog(App->cvi);
    break;

  case 3:
    ivwFlip(XYZ_vi);
    /* DNM: check wild flag here */
    ivwCheckWildFlag(App->cvi->imod);
    imodDraw(App->cvi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
    break;

  case 4:
    if (App->cvi->vmSize)
      imodCacheFill(App->cvi);
    else
      wprint("Cache is not active.\n");
    break;

  case 5:
    if (App->cvi->vmSize)
      imodCacheFillDialog(App->cvi);
    else
      wprint("Cache is not active.\n");
    break;

  default:
    break;
  }

}

void imod_win_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;

  if (ImodForbidLevel)
    return;

  /* DNM: only model and zap will work with raw (color) data */
  if (App->cvi->rawImageStore && !(item == 5 || item == 4))
    return;
     
  switch(item){

  case 0: /* graph */
    /* DMN 2/25/01: do not open with fake image */
    if (!App->cvi->fakeImage)
      xgraphOpen(App->cvi);
    break;

  case 1: /* slice */
    sslice_open(App->cvi);
    /*	  cut_open(); */
    break;
	  
  case 2: /* tumble */
    imod_tumble_open(App->cvi);
    break;

  case 3: /* tilt */
    tltopen(App->cvi, Tilt_vi);
    break;

  case 4: /* model view */
    imod_autosave(App->cvi->imod);
    imodv_open(App->cvi->imod, Rampbase + IMOD_BASE);
    break;

  case 5:
    imod_zap_open(App->cvi);
    break;

  case 6:
    xxyz_open(App->cvi);
    break;

  case 7:
    open_pixelview(App->cvi);
    break;

  case 10: /* zoom */
    system(IMOD_ZOOM_COMMAND);
    break;
	  
  default:
    break;
	  
  }

}


void imod_help_cb(Widget w, XtPointer client, XtPointer call)
{
  int item = (int)client;

  switch (item){
  case 0:
    dia_smsg(Imod_help_text);
    break;
  case 1:
    dia_smsg(Imod_menus_help);
    break;
  case 2:
    dia_smsg(Imod_hotkey_help);
    break;
  }
  return;
}
