/*  IMOD VERSION 2.20
 *
 *  imod_cachefill.c -- Routines to fill cache
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
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
#include <stdio.h>
#include <stdlib.h>

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/Separator.h>
#include <dia.h>
#include "imod.h"

static struct{
     diaDialog *dia;
     ImodView  *vw;
     int       autofill;
     int       fracfill;
     int       balance;
     int       overlap;
}imodCacheFillData = {0, 0, 0, 0, 0, 0};

static Widget mkWorkArea(ImodView *vw, Widget top);

static void help_cb()
{
     dia_vasmsg
	  ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
	   "Imod Cache Filler \n"
	   "~~~~~~~~~~~~~~~~~~~~~~~~"
	   "\n\n",
	   "The Cache Filler can be used to fill some or all of the image ",
	   "cache in one operation, and can also fill the cache "
	   "automatically when the next image to be displayed is not yet "
	   "loaded into cache.  It is primarily designed to make the loading "
	   "of flipped images from large files much more efficient, but it "
	   "could also be useful when loading unflipped images.\n\n",
	   "The \"Fill\" radio buttons select whether the program will "
	   "fill all, half, or one-quarter of the image cache in the next "
	   "operation.\n\n"
	   "If there are multiple image files loaded (multiple times), then "
	   "there will be a set of radio buttons to set the priority among "
	   "the files.  The choices are:\n",
	   "\tAllocate the amount of cache to be filled equally among all "
	   "times.\n"
	   "\tDivide available cache space equally among the current time "
	   "and the previous and next times; then if any space remains to be "
	   "filled, divide it equally among the remaining times.\n"
	   "\tAllocate as much of the cache as needed to the current time, "
	   "then divide remaining space equally among the rest of the times.\n"
	   "\tFill the cache only for the current time.\n\n"
	   "If the Autofill check box is selected, then the program will "
	   "fill the cache according to the other settings in this dialog "
	   "whenever an image is to be displayed that is not in the cache.\n\n"
	   "With Autofill on, if an image is reached by stepping forward or "
	   "backward in Z, "
	   "then the program will retain some existing sections to provide "
	   "the amount of overlap indicated in the \"Overlap by\" radio "
	   "buttons, namely by half, a quarter, or an eighth of the amount of "
	   "cache to be filled.  For example, if the cache is 64 images, "
	   "it contains sections 36 to 99, "
	   "all of it is to be filled, and you step up from section 99 to 100 "
	   "then sections 68 to 99 will be retained and 100 to 131 will be "
	   "loaded if \"1/2\" is selected; whereas 92 to 99 will be retained "
	   "and 100 to 155 loaded if \"1/8\" is selected.\n\n",
	   "The Fill button will fill the cache according to the current "
	   "settings, and has the same effect as the \"Edit-Image-Fill Cache\""
	   " menu entry.\n\n",
	   "To visualize large image files in flipped mode most conveniently, "
	   "start imod without the -Y option and with the desired cache size "
	   "specified in megabytes (e.g., -C 100M).  Select the Z "
	   "level that you want to see after flipping by clicking at the "
	   "corresponding Y level in the Zap window with the left mouse "
	   "button.  Open the Cache Filler window and select appropriate "
	   "filling options, including Autofill.  Then flip the image.\n\n"
	   "The program will not accept input while loading, but will report "
	   "progress in the Info window with a dot every 10 slices.  When "
	   "loading flipped images, a dot will appear for every 10 of the "
	   "native Z slices that are loaded from the file.\n",
	   NULL);
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     mkWorkArea(imodCacheFillData.vw, w);
}

static void done_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     diaDestroyDialog(dia);

     imodCacheFillData.dia = NULL;
}

static void fracfill_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;

     if (!cbs->set)
	  return;
     imodCacheFillData.fracfill = (int)client;
}

static void balance_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;

     if (!cbs->set)
	  return;
     imodCacheFillData.balance = (int)client;
}


static void autofill_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     imodCacheFillData.autofill = cbs->set;
}

static void overlap_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;

     if (!cbs->set)
	  return;
     imodCacheFillData.overlap = (int)client;
}

static void set_z_limits(int *zstart, int *zend, int nfill, int cz, int zsize,
			 int ovbefore, int ovafter)
{
     int nbefore, nafter;

     if (ovbefore > ovafter) {
	  nbefore = (ovbefore * nfill) / (ovbefore + ovafter);
	  nafter = nfill - nbefore - 1;
     } else {
	  nafter = (ovafter * nfill) / (ovbefore + ovafter);
	  nbefore = nfill - nafter - 1;
     }

     /* Set zstart and zend */
     *zstart = cz - nbefore;
     if (*zstart < 0) {
	  *zstart = 0;
	  *zend = nfill - 1;
	  if (*zend >= zsize)
	       *zend = zsize - 1;
     } else {
	  *zend = cz + nafter;
	  if (*zend >= zsize) {
	       *zend = zsize - 1;
	       *zstart = zsize - nfill;
	       if (*zstart < 0)
		    *zstart = 0;
	  }
     }
}

static void report_cache(ImodView *vw, char *string)
{
     int sl;
     printf("%s\n", string);
     for (sl = 0; sl < vw->vmSize; sl++) {
	  printf (" %3d,%2d,%5d", vw->vmCache[sl].cz, vw->vmCache[sl].ct,
		  vw->vmCache[sl].used);
	  if (sl % 6 == 5 || sl == vw->vmSize - 1)
	       printf("\n");
     }
}

static int fill_cache(ImodView *vw, int cz, int ovbefore, int ovafter)
{
     int filltable[3] = {1, 2, 4};
     int ntimes = vw->nt ? vw->nt : 1;
     int nfill = vw->vmSize / filltable[imodCacheFillData.fracfill];
     int *zstart = (int *)malloc((ntimes + 1) * sizeof(int));
     int *zend = (int *)malloc((ntimes + 1) * sizeof(int));
     int *zstall = (int *)malloc((ntimes + 1) * sizeof(int));
     int *zndall = (int *)malloc((ntimes + 1) * sizeof(int));
     int curtime = vw->nt ? vw->ct : 1;
     int nleft, nbase, nextra, i, nshare, tstart, tend, nadj, nadd;
     int time, ct, z, found, sl, llysave, urysave,nslice, sect, offset;
     int minused, slmin, maxdtime, dtime, tdirlim, tdir; 
     int maxdz, dz, zdirlim, zdir;
     int *loadtbl;
     ivwSlice tmpSlice;
     unsigned char *buf;
     char statstr[64];

     if (!zstart || !zend || !zstall || !zndall)
	  return 1;

     if (!nfill)
	  nfill = 1;

     /* report_cache(vw,"starting"); */
     if (imodCacheFillData.balance >= 2 || !vw->nt) {

	  /* No times, or favor current time */
	  /* Get number of slices before and after current one  for current
	     time */
	  set_z_limits(&zstart[curtime], &zend[curtime], nfill, cz, vw->zsize,
		       ovbefore, ovafter);

	  /* Divide remaining slices among remaining times */
	  if (ntimes > 1) {
	       nleft = nfill - (zend[curtime] + 1 - zstart[curtime]);
	       nbase = nleft / (ntimes - 1);
	       nextra = nleft % (ntimes - 1);
	       for (i = 1; i <= vw->nt; i++) {
		    if (i == curtime)
			 continue;
		    nshare = nbase + (i <= nextra ? 1 : 0);
		    if (imodCacheFillData.balance == 3)
			 nshare = 0;
		    set_z_limits(&zstart[i], &zend[i], nshare, cz, vw->zsize,
				 ovbefore, ovafter);
	       }
	  }

     } else if (imodCacheFillData.balance == 1) {

	  /* Treat adjacent times equally - get starting and ending times,
	     give each one an equal share */
	  tstart = curtime - 1;
	  if (tstart < 1 )
	       tstart = 1;
	  tend = curtime + 1;
	  if (tend > vw->nt)
	       tend = vw->nt;
	  nadj = tend + 1 - tstart;
	  nshare = nfill / nadj;
	  nleft = nfill;
	  for (i = tstart; i <= tend; i++) {
	       set_z_limits(&zstart[i], &zend[i], nshare, cz, vw->zsize,
			    ovbefore, ovafter);
	       nleft -= zend[i] + 1 - zstart[i];
	  }

	  /* Divide remaining slices among remaining times */
	  if (ntimes > nadj) {
	       nbase = nleft / (ntimes - nadj);
	       nextra = nleft % (ntimes - nadj);
	       for (i = 1; i <= vw->nt; i++) {
		    if (i >= tstart && i <= tend)
			 continue;
		    nshare = nbase + (i <= nextra ? 1 : 0);
		    set_z_limits(&zstart[i], &zend[i], nshare, cz, vw->zsize,
				 ovbefore, ovafter);
	       }
	  }
     } else {

	  /* Treat all times equally */
	  nbase = nfill / ntimes;
	  nextra = nfill % ntimes;
	  for (i = 1; i <= vw->nt; i++) {
	       nshare = nbase + (i <= nextra ? 1 : 0);
	       set_z_limits(&zstart[i], &zend[i], nshare, cz, vw->zsize,
			    ovbefore, ovafter);
	  }
     }
     /* for (i=1; i <= ntimes; i++) {
	  printf("time = %d, zs = %d, ze = %d\n", i, zstart[i], zend[i]);
	  } */

     /* Scan through all times, prioritize slices that
	already exist and are needed */
     for (time = 1; time <= ntimes; time++) {
	  ct = time;
	  if (!vw->nt)
	       ct = 0;

	  zstall[time] = zstart[time];
	  zndall[time] = zend[time];
	  for (z = zstart[time]; z <= zend[time]; z++) {
	       found = 0;
	       for (sl = 0; sl < vw->vmSize; sl++) {
		    if (vw->vmCache[sl].ct == ct && 
			vw->vmCache[sl].cz == z) {
			 vw->vmCache[sl].used = vw->vmCount + 1;
			 found = 1;
			 break;
		    } 
	       }
	       
	       /* If flipped, stop if not found; need to load continguous
		  slices only */
	       if (!found && vw->li->axis == 2) {
		    break;
	       }
	  }

	  /* If flipped, go down from the top end also, finding contiguous
	     slices that are loaded */
	  if (vw->li->axis == 2) {
	       zstart[time] = z;
	       for (z = zend[time]; z >= zstart[time]; z--) {
		    found = 0;
		    for (sl = 0; sl < vw->vmSize; sl++) {
			 if (vw->vmCache[sl].ct == ct && 
			     vw->vmCache[sl].cz == z) {
			      vw->vmCache[sl].used = vw->vmCount + 1;
			      found = 1;
			      break;
			 } 
		    }
	       
		    if (!found) {
			 break;
		    }
	       }
	       zend[time] = z;
	  }
     }

     /*  report_cache(vw,"reprioritized");
     for (i=1; i <= ntimes; i++) {
	  printf("time = %d, zs = %d, ze = %d\n", i, zstart[i], zend[i]);
	  } */

     /* Prepare to access multiple files */
     if (vw->nt) {
	  iiClose(&vw->imageList[vw->ct-1]);
	  if (Imod_IFDpath)
	       chdir(Imod_IFDpath);
     }
     
     /* Load the slices that are needed */
     for (time = 1; time <= ntimes; time++) {
	  if (zstart[time] > zend[time]) 
	       continue;

	  ct = 0;
	  if (vw->nt) {
	       wprint("\nReading image file # %3.3d ", time);
	       ct = time;
	       vw->hdr = vw->image = &vw->imageList[time-1];
	       ivwSetScale(vw);
	       iiReopen(vw->image);
	  } else
	       wprint("\nReading image file ");

	  nadd = 0;

	  if (vw->li->axis != 2) {

	       /* For z slices, look for ones that are needed */
	       for (z = zstart[time]; z <= zend[time]; z++) {
		    found = 0;
		    for (sl = 0; sl < vw->vmSize; sl++) {
			 if (vw->vmCache[sl].ct == ct && 
			     vw->vmCache[sl].cz == z) {
			      found = 1;
			      break;
			 } 
		    }
		    if (found)
			 continue;

		    nadd++;
		    if (!(nadd % 10))
			 wprint(".");

		    /* Find oldest slice in cache */
		    minused = vw->vmCount + 1;
		    slmin = 0;
		    for (sl = 0; sl < vw->vmSize; sl++)
			 if (vw->vmCache[sl].used < minused) {
			      minused = vw->vmCache[sl].used;
			      slmin = sl;
			 }

		    /* Load data */
		    ivwReadZ(vw, vw->vmCache[slmin].sec->data.b, z);
		    vw->vmCache[slmin].cz = z;
		    vw->vmCache[slmin].ct = ct;
		    vw->vmCache[slmin].used = vw->vmCount + 1;
		    
		    ivwScaleDepth8(vw, &vw->vmCache[slmin]);
	       }
	  } else {

	       /* For flipped data, save lly, ury and set them for
		  reading in Z slices */
	       llysave = vw->image->lly;
	       urysave = vw->image->ury;
	       vw->image->lly = llysave + zstart[time];
	       vw->image->ury = llysave + zend[time];
	       vw->image->axis = 3;

	       nslice = zend[time] + 1 - zstart[time];
	       buf = (unsigned char *)malloc(vw->xsize * nslice);
	       if (!buf)
		    return 1;
	       loadtbl = (int *)malloc(nslice * sizeof(int));
	       if (!loadtbl)
		    return 1;
	       
	       /* Find oldest slices and enter them into table */
	       for (i = 0; i < nslice; i++) {
		    minused = vw->vmCount + 1;
		    slmin = 0;
		    for (sl = 0; sl < vw->vmSize; sl++)
			 if (vw->vmCache[sl].used < minused) {
			      minused = vw->vmCache[sl].used;
			      slmin = sl;
			 }
		    loadtbl[i] = slmin;
		    vw->vmCache[slmin].used = vw->vmCount + 1;
	       }

	       /* Loop on true Z slices, read in, copy lines to cache slices */
	       for (sect = 0; sect < vw->ysize; sect++) {
		    nadd++;
		    if (!(nadd % 10))
			 wprint(".");
		    iiReadSectionByte(vw->image, (char *)buf, 
				      sect + vw->li->zmin);
		    offset = sect * vw->xsize;
		    for (sl = 0; sl < nslice; sl++)
			 memcpy(vw->vmCache[loadtbl[sl]].sec->data.b + offset, 
				buf + sl * vw->xsize, vw->xsize);
	       }

	       for (i = 0; i < nslice; i++) {
		    sl = loadtbl[i];
		    vw->vmCache[sl].cz = i + zstart[time];
		    vw->vmCache[sl].ct = ct;
		    ivwScaleDepth8(vw, &vw->vmCache[sl]);
	       }

	       free(buf);
	       free(loadtbl);
	       vw->image->lly = llysave;
	       vw->image->ury = urysave;
	       vw->image->axis = 2;
	  }

	  if (vw->nt)
	       iiClose(vw->image);
     }
     wprint("Done!\n");

     /* Restore current image to be open */
     if (vw->nt) {
	  vw->hdr = vw->image = &vw->imageList[vw->ct-1];
	  ivwSetScale(vw);
	  iiReopen(vw->image);
	  if (Imod_IFDpath)
	       chdir(Imod_cwdpath);
     }

     /* Set priorities - move from farthest out times inward to current time */
     maxdtime = ntimes - curtime;
     if (maxdtime < curtime - 1)
	  maxdtime = curtime - 1;

     for (dtime = maxdtime; dtime >= 0; dtime--) {
	  tdirlim = dtime ? -1 : 1;
	  for (tdir = 1; tdir >= tdirlim; tdir -= 2) {
	       time = curtime + dtime * tdir;
	       if (time < 1 || time > ntimes)
		    continue;
	       ct = time;
	       if (!vw->nt)
		    ct = 0;

	       /* Move from farthest out z in to current z */
	       maxdz = zndall[time] - cz;
	       if (maxdz < cz - zstall[time])
		    maxdz = cz - zstall[time];

	       for (dz = maxdz; dz >=0; dz--) {
		    zdirlim = dz ? -1 : 1;
		    for (zdir = 1; zdir >= zdirlim; zdir -= 2) {
			 z = cz + dz * zdir;
			 if (z < zstall[time] || z > zndall[time])
			      continue;

			 /* Look for slice in cache and give it use count */
			 for (sl = 0; sl < vw->vmSize; sl++) {
			      if (vw->vmCache[sl].ct == ct && 
				  vw->vmCache[sl].cz == z) {
				   vw->vmCount++;
				   vw->vmCache[sl].used = vw->vmCount;
				   break;
			      } 
			 }
		    }
	       }
	  }
     }

     /*  report_cache(vw,"loaded"); */
     free(zstart);
     free(zend);
     free(zstall);
     free(zndall);
     return 0;
}

static void fill_cb(Widget w, XtPointer client, XtPointer call)
{
     fill_cache(imodCacheFillData.vw, imodCacheFillData.vw->zmouse, 1, 1);
}

int icfGetAutofill(void)
{
     return imodCacheFillData.autofill;
}

void imodCacheFill(ImodView *vw)
{
     fill_cache(vw, vw->zmouse, 1, 1);
}

unsigned char *icfDoAutofill(ImodView *vw, int cz)
{
     int sl;
     int ifbefore = 0;
     int ifafter = 0;
     int ovbefore = 1;
     int ovafter = 1;
     int ovtable[3] = {1, 3, 7};

     /* Find out if Z before or after the current Z exists in the cache */
     for (sl = 0; sl < vw->vmSize; sl++) {
	  if (vw->vmCache[sl].ct == vw->ct) {
	       if (vw->vmCache[sl].cz == cz - 1)
		    ifbefore = 1;
	       if (vw->vmCache[sl].cz == cz + 1)
		    ifafter = 1;
	  }
     }

     /* Set the overlap factors before or after accordingly */
     if (ifafter && !ifbefore)
	  ovbefore = ovtable[imodCacheFillData.overlap];
     if (!ifafter && ifbefore)
	  ovafter = ovtable[imodCacheFillData.overlap];

     if (fill_cache(vw, cz, ovbefore, ovafter))
	  return NULL;

     for (sl = 0; sl < vw->vmSize; sl++)
	  if (vw->vmCache[sl].ct == vw->ct && vw->vmCache[sl].cz == cz)
	       return (vw->vmCache[sl].sec->data.b);

     return NULL;
}

void imodCacheFillDialog(ImodView *vw)
{
     XtPointer cbd = (XtPointer)vw;
     
     if (imodCacheFillData.dia){
	  XRaiseWindow(App->display, 
		       XtWindow(imodCacheFillData.dia->dialog));
	  return;
     }

     imodCacheFillData.vw = vw;

     imodCacheFillData.dia = diaVaCreateDialog
	  ("Imod: Cache Filler", App->toplevel, App->context,
	   DiaNcontrolButton, "Fill", fill_cb, cbd,
	   DiaNcontrolButton, "Done", done_cb,   cbd,
	   DiaNcontrolButton, "Help", help_cb,   cbd,
	   DiaNworkAreaFunc,  workarea_cb,       cbd,
	   DiaNwindowQuit,    done_cb,           cbd,
	   0);
     return;
}

/****************************************************************************/
/*  Dialog controls.                                                 */

static Widget mkWorkArea(ImodView *vw, Widget top)
{
     Widget frame, col, radio, button, row;
     XmString xsx, xsy, xsz, xsw;

     frame = XtVaCreateWidget
	  ("frame", xmFrameWidgetClass, top, NULL);

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, frame,
	   NULL);


     row = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL, NULL);
     XtVaCreateManagedWidget
       ("Fill:", xmLabelWidgetClass, row, NULL);

     xsx = XmStringCreateSimple("All");
     xsy = XmStringCreateSimple("1/2");
     xsz = XmStringCreateSimple("1/4");

     radio = XmVaCreateSimpleRadioBox (row, "radio_box",
				       imodCacheFillData.fracfill, 
				       fracfill_cb,
				       XmVaRADIOBUTTON, xsx, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsy, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsz, NULL, NULL, NULL,
				       XmNorientation, XmHORIZONTAL, NULL);
     XtManageChild (radio);
     XtManageChild (row);
     XmStringFree(xsx);
     XmStringFree(xsy);
     XmStringFree(xsz);

     if (vw->nt) {
	  xsw = XmStringCreateSimple("Treat all times equally");
	  xsx = XmStringCreateSimple("Treat adjacent times equally");
	  xsy = XmStringCreateSimple("Favor current time");
	  xsz = XmStringCreateSimple("Load only current time");

	  radio = XmVaCreateSimpleRadioBox (col, "radio_box",
				       imodCacheFillData.balance, 
				       balance_cb,
				       XmVaRADIOBUTTON, xsw, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsx, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsy, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsz, NULL, NULL, NULL,
				       XmNorientation, XmVERTICAL, NULL);
	  XtManageChild (radio);
	  XmStringFree(xsw);
	  XmStringFree(xsx);
	  XmStringFree(xsy);
	  XmStringFree(xsz);
     }	  

     button  = XtVaCreateManagedWidget
	  ("Autofill",  xmToggleButtonWidgetClass, col, NULL);
     XtAddCallback(button, XmNvalueChangedCallback, 
		   autofill_cb, NULL);
     if (imodCacheFillData.autofill)
	  XmToggleButtonSetState(button, True, False);
     else
	  XmToggleButtonSetState(button, False, False);

     row = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL, NULL);
     XtVaCreateManagedWidget
       ("Overlap by:", xmLabelWidgetClass, row, NULL);

     xsx = XmStringCreateSimple("1/2");
     xsy = XmStringCreateSimple("1/4");
     xsz = XmStringCreateSimple("1/8");
     radio = XmVaCreateSimpleRadioBox (row, "radio_box",
				       imodCacheFillData.overlap, 
				       overlap_cb,
				       XmVaRADIOBUTTON, xsx, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsy, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsz, NULL, NULL, NULL,
				       XmNorientation, XmHORIZONTAL, NULL);
     XtManageChild (radio);
     XtManageChild (row);
     XmStringFree(xsx);
     XmStringFree(xsy);
     XmStringFree(xsz);

     XtManageChild(col);
     XtManageChild(frame);
     return(frame);
}     

