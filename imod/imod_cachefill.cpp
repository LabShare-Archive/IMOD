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
Log at end of file
*/

#include <stdio.h>
#include <stdlib.h>
#include <qvbuttongroup.h>
#include <qhbuttongroup.h>
#include <qradiobutton.h>
#include <qcheckbox.h>
#include <qlayout.h>
#include <qtooltip.h>
#include <qdir.h>
#include "dia_qtutils.h"

#include "imod_cachefill.h"
#include "imod.h"
#include "imod_info_cb.h"
#include "imod_display.h"
#include "control.h"


static struct{
  ImodCacheFill *dia;
  ImodView  *vw;
  int       autofill;
  int       fracfill;
  int       balance;
  int       overlap;
}imodCacheFillData = {0, 0, 0, 0, 0, 0};

static void set_z_limits(int *zstart, int *zend, int nfill, int cz, int zsize,
                         int ovbefore, int ovafter);
static void report_cache(ImodView *vw, char *string);
static int fill_cache(ImodView *vw, int cz, int ovbefore, int ovafter);


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
  unsigned char *buf;

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
    if (!Imod_IFDpath.isEmpty())
      QDir::setCurrent(Imod_IFDpath);
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

    /* DNM 4/18/03: process events so that text will show up and so that the
       program can be killed */
    imod_info_input();
    if (App->exiting)
      exit(0);
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
	if (!(nadd % 10)) {
	  wprint(".");
          imod_info_input();
          if (App->exiting)
            exit(0);
        }

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
    if (!Imod_IFDpath.isEmpty())
      QDir::setCurrent(Imod_cwdpath);
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
  imodDraw(vw, IMOD_DRAW_IMAGE);
  return 0;
}

int icfGetAutofill(void)
{
  return imodCacheFillData.autofill;
}

void imodCacheFill(ImodView *vw)
{
  fill_cache(vw, (int)vw->zmouse, 1, 1);
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
  if (imodCacheFillData.dia){
    imodCacheFillData.dia->raise();
    return;
  }

  imodCacheFillData.vw = vw;

  imodCacheFillData.dia = new ImodCacheFill
    (imodDialogManager.parent(IMOD_DIALOG), "cache filler");
  imodDialogManager.add((QWidget *)imodCacheFillData.dia, IMOD_DIALOG);
  return;
}

/****************************************************************************/
/*  THE CACHE FILLER CLASS                                                  */
/*                                                                          */
/*   The constructor                                                        */

static char *buttonLabels[] = {"Fill", "Done", "Help"};
static char *buttonTips[] = {"Fill cache based on settings here", 
			     "Close dialog box", "Open help window"};

ImodCacheFill::ImodCacheFill(QWidget *parent, const char *name)
  : DialogFrame(parent, 3, buttonLabels, buttonTips, true, 
		" ", "", name)
{
  // Set up fill fraction radio buttons
  mFillGroup = new QHButtonGroup("Fill fraction", this, "fill group");
  mLayout->addWidget(mFillGroup);
  QToolTip::add(mFillGroup, "Set amount of cache to fill at one time");

  QRadioButton *radio = diaRadioButton("All", mFillGroup);
  radio = diaRadioButton("1/2", mFillGroup);
  radio = diaRadioButton("1/4", mFillGroup);

  mFillGroup->setButton(imodCacheFillData.fracfill);
  connect(mFillGroup, SIGNAL(clicked(int)), this, SLOT(fractionSelected(int)));
  
  // Set up balance radio buttons only if times loaded
  if (imodCacheFillData.vw->nt > 0) {
    mBalanceGroup = new QVButtonGroup("Balance between times", this, 
				      "balance group");
    mLayout->addWidget(mBalanceGroup);
    
    radio = diaRadioButton("Treat all times equally", mBalanceGroup);
    QToolTip::add(radio, "Fill equal number of sections for all times");
    radio = diaRadioButton("Treat adjacent times equally", mBalanceGroup);
    QToolTip::add(radio, "Load more sections for current time and"
		  " two adjacent times");
    radio = diaRadioButton("Favor current time", mBalanceGroup);
    QToolTip::add(radio, "Load more sections for the current time");
    radio = diaRadioButton("Load only current time", mBalanceGroup);
    QToolTip::add(radio, "Load sections only for the current time");
    
    mBalanceGroup->setButton(imodCacheFillData.balance);
    connect(mBalanceGroup, SIGNAL(clicked(int)), this, 
	    SLOT(balanceSelected(int)));
  }

  // The auto checkbox
  mAutoCheck = diaCheckBox("Autofill", this, mLayout);
  QToolTip::add(mAutoCheck, "Fill cache to selected extent whenever "
		"section not in cache is needed");
  mAutoCheck->setChecked(imodCacheFillData.autofill != 0);
  connect(mAutoCheck, SIGNAL(toggled(bool)), this, SLOT(autoToggled(bool)));

  // The overlap radio buttons
  mOverlapGroup = new QHButtonGroup("Overlap fraction", this, "overlap group");
  mLayout->addWidget(mOverlapGroup);
  QToolTip::add(mOverlapGroup, "Set fraction of sections to retain when"
		" filling cache automatically");

  mOverlapRadio[0] = diaRadioButton("1/2", mOverlapGroup);
  mOverlapRadio[1] = diaRadioButton("1/4", mOverlapGroup);
  mOverlapRadio[2] = diaRadioButton("1/8", mOverlapGroup);

  mOverlapGroup->setButton(imodCacheFillData.overlap);
  connect(mOverlapGroup, SIGNAL(clicked(int)), this,
	  SLOT(overlapSelected(int)));

  // Enable the overlap buttons only if autofill is on
  mOverlapGroup->setEnabled(imodCacheFillData.autofill != 0);
  for (int i = 0; i < 3; i++)
    mOverlapRadio[i]->setEnabled(imodCacheFillData.autofill != 0);

  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));

  setCaption(imodCaption("3dmod Cache Filler"));
  show();
}

// Respond to action buttons
void ImodCacheFill::buttonPressed(int which)
{
  switch(which) {
  case 0:
    fill_cache(imodCacheFillData.vw, (int)imodCacheFillData.vw->zmouse, 1, 1);
    break;
  case 1:
    close();
    break;
  case 2:
    dia_vasmsg
      ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
       "3dmod Cache Filler \n"
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
       "start 3dmod without the -Y option and with the desired cache size "
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
    break;
  }
}

// Respond to the radio buttons
void ImodCacheFill::fractionSelected(int which)
{
  imodCacheFillData.fracfill = which;
}

void ImodCacheFill::balanceSelected(int which)
{
  imodCacheFillData.balance = which;
}

void ImodCacheFill::overlapSelected(int which)
{
  imodCacheFillData.overlap = which;
}

void ImodCacheFill::autoToggled(bool state)
{
  imodCacheFillData.autofill = state ? 1 : 0;
  mOverlapGroup->setEnabled(state);
  for (int i = 0; i < 3; i++)
    mOverlapRadio[i]->setEnabled(state);
}

// The dialog is closing: remove from manager
void ImodCacheFill::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)imodCacheFillData.dia);
  imodCacheFillData.dia = NULL;
  e->accept();
}

// CLose on Escape.  Pass keys on.
void ImodCacheFill::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void ImodCacheFill::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

/*
$Log$
Revision 4.4  2003/04/18 20:08:48  mast
Process events while loading to allow messages and exit

Revision 4.3  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.2  2003/02/27 17:40:55  mast
Use Qt routines for directory operations

Revision 4.1  2003/02/10 20:28:59  mast
autox.cpp

Revision 1.1.2.3  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.2  2003/01/23 20:02:57  mast
switch from button pressed to clicked

Revision 1.1.2.1  2003/01/18 01:12:20  mast
qt version

Revision 3.2  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.1  2001/12/17 18:44:38  mast
Initial version of module

*/
