/*  
 *  cachefill.cpp -- Routines to fill cache
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT file for full copyright notice. 
 *
 *  $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <QButtonGroup>
#include <QGroupBox>
#include <qradiobutton.h>
#include <qcheckbox.h>
#include <qlayout.h>
#include <qtooltip.h>
#include <qdir.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QKeyEvent>
#include "dia_qtutils.h"

#include "cachefill.h"
#include "imod.h"
#include "info_cb.h"
#include "pyramidcache.h"
#include "display.h"
#include "control.h"
#include "preferences.h"

static struct{
  ImodCacheFill *dia;
  ImodView  *vi;
  int       autofill;
  int       fracfill;
  int       balance;
  int       overlap;
}imodCacheFillData = {0, 0, 0, 0, 0, 0};


static void set_z_limits(ImodView *vi, int *zstart, int *zend, int nfill,
                         int cz, int ovbefore, int ovafter);
//static void report_cache(ImodView *vi, char *string);
static int fill_cache(ImodView *vi, int cz, int ovbefore, int ovafter, int source);
static void clean_fill(int *zstart, int *zend, int *zstall, int *zndall,
                       int *loadtbl, unsigned char *buf);
static int walk_in_z(ImodView *vi, int cz, int steps);

/* Move the given number of steps in Z, skipping missing sections in a piece
   list and stopping one past the limits 0 and zsize - 1 */
static int walk_in_z(ImodView *vi, int cz, int steps)
{
  int idir = steps > 0 ? 1 : -1;
  while (steps != 0) {
    cz += idir;
    if (cz < 0 || cz >= vi->zsize)
      break;
    if (!ivwPlistBlank(vi, cz))
      steps -= idir;
  }
  return cz;
}

static void set_z_limits(ImodView *vi, int *zstart, int *zend, int nfill,
                         int cz, int ovbefore, int ovafter)
{
  int nbefore, nafter;
  int zsize = vi->zsize;

  /* set the number before and after from overlap factors */
  if (ovbefore > ovafter) {
    nbefore = (ovbefore * nfill) / (ovbefore + ovafter);
    nafter = nfill - nbefore - 1;
  } else {
    nafter = (ovafter * nfill) / (ovbefore + ovafter);
    nbefore = nfill - nafter - 1;
  }

  /* Set zstart and zend */
  *zstart = walk_in_z(vi, cz, -nbefore);
  if (*zstart < 0) {
    *zstart = 0;
    *zend = walk_in_z(vi, 0, nfill - 1);
    if (*zend >= zsize)
      *zend = zsize - 1;
  } else {
    *zend = walk_in_z(vi, cz, nafter);
    if (*zend >= zsize) {
      *zend = zsize - 1;
      *zstart = walk_in_z(vi, zsize - 1, -(nfill - 1));
      if (*zstart < 0)
	*zstart = 0;
    }
  }
}

/*
static void report_cache(ImodView *vi, char *string)
{
  int sl;
  imodPrintStderr("%s\n", string);
  for (sl = 0; sl < vi->vmSize; sl++) {
    imodPrintStderr (" %3d,%2d,%5d", vi->vmCache[sl].cz, vi->vmCache[sl].ct,
	    vi->vmCache[sl].used);
    if (sl % 6 == 5 || sl == vi->vmSize - 1)
      imodPrintStderr("\n");
  }
}
*/

static int fill_cache(ImodView *vi, int cz, int ovbefore, int ovafter, int source)
{
  int filltable[3] = {1, 2, 4};
  int ntimes = vi->numTimes ? vi->numTimes : 1;
  int nfill = vi->vmSize / filltable[imodCacheFillData.fracfill];
  int curtime = vi->numTimes ? vi->curTime : 1;
  int nleft, nbase, nextra, i, nshare, tstart, tend, nadj;
  int time, ct, z, sl, llysave, urysave,nslice, sect, offset;
  int minused, slmin, maxdtime, dtime, tdirlim, tdir, pixSize; 
  int maxdz, dz, zdirlim, zdir, loadAxis;
  int *loadtbl = NULL;
  int *zstart, *zend, *zstall, *zndall;
  unsigned char *buf = NULL;
  QString message;

  if (vi->fullCacheFlipped)
    return 0;
  if (vi->pyrCache) {
    if (vi->loadingImage)
      return 1;
    vi->loadingImage = 1;
    vi->pyrCache->fillCacheForArea(cz, source);
    vi->loadingImage = 0;
    imodDraw(vi, IMOD_DRAW_IMAGE | IMOD_DRAW_MOD);
    return 0;
  }

  zstart = (int *)malloc((ntimes + 1) * sizeof(int));
  zend = (int *)malloc((ntimes + 1) * sizeof(int));
  zstall = (int *)malloc((ntimes + 1) * sizeof(int));
  zndall = (int *)malloc((ntimes + 1) * sizeof(int));
  if (!zstart || !zend || !zstall || !zndall) {
    clean_fill(zstart, zend, zstall, zndall, loadtbl, buf);
    return 1;
  }

  vi->loadingImage = 1;
  loadAxis = vi->li->axis;

  if (!nfill)
    nfill = 1;

  /* report_cache(vi,"starting"); */
  if (imodCacheFillData.balance >= 2 || !vi->numTimes) {

    /* No times, or favor current time */
    /* Get number of slices before and after current one  for current
       time */
    set_z_limits(vi, &zstart[curtime], &zend[curtime], nfill, cz,
                 ovbefore, ovafter);

    /* Divide remaining slices among remaining times */
    if (ntimes > 1) {
      nleft = nfill - (zend[curtime] + 1 - zstart[curtime]);
      nbase = nleft / (ntimes - 1);
      nextra = nleft % (ntimes - 1);
      for (i = 1; i <= vi->numTimes; i++) {
	if (i == curtime)
	  continue;
	nshare = nbase + (i <= nextra ? 1 : 0);
	if (imodCacheFillData.balance == 3)
	  nshare = 0;
	set_z_limits(vi, &zstart[i], &zend[i], nshare, cz, ovbefore, ovafter);
      }
    }

  } else if (imodCacheFillData.balance == 1) {

    /* Treat adjacent times equally - get starting and ending times,
       give each one an equal share */
    tstart = curtime - 1;
    if (tstart < 1 )
      tstart = 1;
    tend = curtime + 1;
    if (tend > vi->numTimes)
      tend = vi->numTimes;
    nadj = tend + 1 - tstart;
    nshare = nfill / nadj;
    nleft = nfill;
    for (i = tstart; i <= tend; i++) {
      set_z_limits(vi, &zstart[i], &zend[i], nshare, cz, ovbefore, ovafter);
      nleft -= zend[i] + 1 - zstart[i];
    }

    /* Divide remaining slices among remaining times */
    if (ntimes > nadj) {
      nbase = nleft / (ntimes - nadj);
      nextra = nleft % (ntimes - nadj);
      for (i = 1; i <= vi->numTimes; i++) {
	if (i >= tstart && i <= tend)
	  continue;
	nshare = nbase + (i <= nextra ? 1 : 0);
	set_z_limits(vi, &zstart[i], &zend[i], nshare, cz, ovbefore, ovafter);
      }
    }
  } else {

    /* Treat all times equally */
    nbase = nfill / ntimes;
    nextra = nfill % ntimes;
    for (i = 1; i <= vi->numTimes; i++) {
      nshare = nbase + (i <= nextra ? 1 : 0);
      set_z_limits(vi, &zstart[i], &zend[i], nshare, cz, ovbefore, ovafter);
    }
  }
  /* for (i=1; i <= ntimes; i++) {
     imodPrintStderr("time = %d, zs = %d, ze = %d\n", i, zstart[i], zend[i]);
     } */

  /* Scan through all times, prioritize slices that
     already exist and are needed */
  for (time = 1; time <= ntimes; time++) {
    ct = time;
    if (!vi->numTimes)
      ct = 0;

    zstall[time] = zstart[time];
    zndall[time] = zend[time];
    for (z = zstart[time]; z <= zend[time]; z++) {

      sl = vi->cacheIndex[z * vi->vmTdim + ct - vi->vmTbase];
      if (sl >= 0) 
        vi->vmCache[sl].used = vi->vmCount + 1;
               
      /* If flipped, stop if not found; need to load continguous
	 slices only */
      else if (loadAxis == 2)
	break;
    }

    /* If flipped, go down from the top end also, finding contiguous
       slices that are loaded */
    if (loadAxis == 2) {
      zstart[time] = z;
      for (z = zend[time]; z >= zstart[time]; z--) {
        sl = vi->cacheIndex[z * vi->vmTdim + ct - vi->vmTbase];
        if (sl >= 0) 
          vi->vmCache[sl].used = vi->vmCount + 1;
        else
	  break;
      }
      zend[time] = z;
    }
  }

  /*  report_cache(vi,"reprioritized");
      for (i=1; i <= ntimes; i++) {
      imodPrintStderr("time = %d, zs = %d, ze = %d\n", i, zstart[i], zend[i]);
      } */

  /* Prepare to access multiple files */
  if (vi->numTimes) {
    iiClose(&vi->imageList[vi->curTime-1]);
    if (!Imod_IFDpath.isEmpty())
      QDir::setCurrent(Imod_IFDpath);
  }
     
  /* Load the slices that are needed */
  for (time = 1; time <= ntimes; time++) {
    if (zstart[time] > zend[time]) 
      continue;

    ct = 0;
    if (vi->numTimes) {
      ct = time;
      vi->hdr = vi->image = &vi->imageList[time-1];
      //ivwSetScale(vi);
      ivwReopen(vi->image);
    }

    if (loadAxis != 2) {

      /* imodPrintStderr("loading %d %d\n", zstart[time], zend[time]); */
      /* For z slices, look for ones that are needed */
      for (z = zstart[time]; z <= zend[time]; z++) {
        sl = vi->cacheIndex[z * vi->vmTdim + ct - vi->vmTbase];
        if (sl >= 0 || ivwPlistBlank(vi, z)) 
	  continue;
        
        /* DNM 4/18/03: process events so that text will show up and so that 
           the program can be killed */
        if (vi->numTimes)
          message.sprintf("Reading image file # %3.3d, Z = %d\r", time, z + 1);
        else
          message.sprintf("Reading image file, Z = %d\r", z + 1);
        imod_imgcnt(LATIN1(message));

	/* Find oldest slice in cache */
	minused = vi->vmCount + 1;
	slmin = 0;
	for (sl = 0; sl < vi->vmSize; sl++)
	  if (vi->vmCache[sl].used < minused) {
	    minused = vi->vmCache[sl].used;
	    slmin = sl;
	  }
        if (vi->vmCache[slmin].cz >= 0 && vi->vmCache[slmin].ct >= vi->vmTbase)
          vi->cacheIndex[vi->vmCache[slmin].cz * vi->vmTdim + 
                         ct - vi->vmTbase] = -1;

	/* Load data */
	ivwReadZ(vi, vi->vmCache[slmin].sec->data.b, z);
	vi->vmCache[slmin].cz = z;
	vi->vmCache[slmin].ct = ct;
	vi->vmCache[slmin].used = vi->vmCount + 1;
        vi->cacheIndex[z * vi->vmTdim + ct - vi->vmTbase] = slmin;
                    
	ivwScaleDepth8(vi, &vi->vmCache[slmin]);
      }
    } else {

      /* For flipped data, save lly, ury and set them for
	 reading in Z slices */
      llysave = vi->image->lly;
      urysave = vi->image->ury;
      vi->image->lly = llysave + zstart[time];
      vi->image->ury = llysave + zend[time];
      vi->image->axis = 3;
      pixSize = ivwGetPixelBytes(vi->rawImageStore);

      nslice = zend[time] + 1 - zstart[time];
      buf = (unsigned char *)malloc(vi->xsize * nslice * pixSize);
      loadtbl = (int *)malloc(nslice * sizeof(int));
      if (!buf || !loadtbl) {
        clean_fill(zstart, zend, zstall, zndall, loadtbl, buf);
        vi->loadingImage = 0;
	return 1;
      }
               
      /* Find oldest slices and enter them into table */
      for (i = 0; i < nslice; i++) {
	minused = vi->vmCount + 1;
	slmin = 0;
	for (sl = 0; sl < vi->vmSize; sl++)
	  if (vi->vmCache[sl].used < minused) {
	    minused = vi->vmCache[sl].used;
	    slmin = sl;
	  }
	loadtbl[i] = slmin;
	vi->vmCache[slmin].used = vi->vmCount + 1;
        if (vi->vmCache[slmin].cz >= 0 && vi->vmCache[slmin].ct >= vi->vmTbase)
          vi->cacheIndex[vi->vmCache[slmin].cz * vi->vmTdim + 
                         ct - vi->vmTbase] = -1;
      }

      /* Loop on true Z slices, read in, copy lines to cache slices */
      for (sect = 0; sect < vi->ysize; sect++) {
        z = sect + vi->li->zmin;
        if (vi->numTimes)
          message.sprintf("Reading image # %3.3d, file Z = %d", time, z);
        else
          message.sprintf("Reading image, file Z = %d", z);
        imod_imgcnt(LATIN1(message));

	ivwReadBinnedSection(vi, (char *)buf, z);
	offset = sect * vi->xsize * pixSize;
	for (sl = 0; sl < nslice; sl++)
	  memcpy(vi->vmCache[loadtbl[sl]].sec->data.b + offset, 
		 buf + sl * vi->xsize * pixSize, vi->xsize * pixSize);
      }

      for (i = 0; i < nslice; i++) {
	sl = loadtbl[i];
	vi->vmCache[sl].cz = i + zstart[time];
	vi->vmCache[sl].ct = ct;
        vi->cacheIndex[i + zstart[time] * vi->vmTdim + ct - vi->vmTbase] = sl;
	ivwScaleDepth8(vi, &vi->vmCache[sl]);
      }

      vi->image->lly = llysave;
      vi->image->ury = urysave;
      vi->image->axis = 2;
    }

    if (vi->numTimes)
      iiClose(vi->image);
  }
  imod_imgcnt("\n");

  /* Restore current image to be open */
  if (vi->numTimes) {
    vi->hdr = vi->image = &vi->imageList[vi->curTime-1];
    //ivwSetScale(vi);
    ivwReopen(vi->image);
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
      if (!vi->numTimes)
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
          sl = vi->cacheIndex[z * vi->vmTdim + ct - vi->vmTbase];
          if (sl >= 0) {
            vi->vmCount++;
            vi->vmCache[sl].used = vi->vmCount;
	  }
	}
      }
    }
  }

  /*  report_cache(vi,"loaded"); */
  clean_fill(zstart, zend, zstall, zndall, loadtbl, buf);
  vi->loadingImage = 0;
  imodDraw(vi, IMOD_DRAW_IMAGE);
  return 0;
}

/* Clean up memory allocations from fill_cache */
static void clean_fill(int *zstart, int *zend, int *zstall, int *zndall,
                      int *loadtbl, unsigned char *buf)
{
  if (zstart)
    free(zstart);
  if (zend)
    free(zend);
  if (zstall)
    free(zstall);
  if (zndall)
    free(zndall);
  if (loadtbl)
    free(loadtbl);
  if (buf)
    free(buf);
}

int icfGetAutofill(void)
{
  return imodCacheFillData.autofill;
}

int imodCacheFill(ImodView *vi, int source)
{
  if (!vi->loadingImage)
    return (fill_cache(vi, (int)vi->zmouse, 1, 1, source));
  return -1;
}

unsigned char *icfDoAutofill(ImodView *vi, int cz)
{
  int sl;
  int ifbefore = 0;
  int ifafter = 0;
  int ovbefore = 1;
  int ovafter = 1;
  int ovtable[3] = {1, 3, 7};

  if (vi->loadingImage)
    return NULL;

  /* Find out if Z before or after the current Z exists in the cache */
  if (cz > 0 && vi->cacheIndex[(cz - 1) * vi->vmTdim + 
                               vi->curTime - vi->vmTbase] >= 0)
    ifbefore = 1;
  if (cz < vi->zsize - 1 && vi->cacheIndex[(cz + 1) * vi->vmTdim + 
                               vi->curTime - vi->vmTbase] >= 0)
    ifafter = 1;

  /* Set the overlap factors before or after accordingly */
  if (ifafter && !ifbefore)
    ovbefore = ovtable[imodCacheFillData.overlap];
  if (!ifafter && ifbefore)
    ovafter = ovtable[imodCacheFillData.overlap];

  if (fill_cache(vi, cz, ovbefore, ovafter, 0))
    return NULL;

  sl = vi->cacheIndex[cz * vi->vmTdim + vi->curTime - vi->vmTbase];
  if (sl >= 0)
    return (vi->vmCache[sl].sec->data.b);

  return NULL;
}

void imodCacheFillDialog(ImodView *vi)
{
  if (imodCacheFillData.dia){
    imodCacheFillData.dia->raise();
    return;
  }

  imodCacheFillData.vi = vi;

  imodCacheFillData.dia = new ImodCacheFill
    (imodDialogManager.parent(IMOD_DIALOG), "cache filler");
  imodDialogManager.add((QWidget *)imodCacheFillData.dia, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)imodCacheFillData.dia, IMOD_DIALOG);
}

/****************************************************************************/
/*  THE CACHE FILLER CLASS                                                  */
/*                                                                          */
/*   The constructor                                                        */

static const char *buttonLabels[] = {"Fill", "Done", "Help"};
static const char *buttonTips[] = {"Fill cache based on settings here", 
			     "Close dialog box", "Open help window"};

ImodCacheFill::ImodCacheFill(QWidget *parent, const char *name)
  : DialogFrame(parent, 3, 1, buttonLabels, buttonTips, true, 
		ImodPrefs->getRoundedStyle(), " ", "", name)
{
  // Set up fill fraction radio buttons
  mFillGroup = new QButtonGroup(this);
  QGroupBox *gbox = new QGroupBox("Fill fraction", this);
  mLayout->addWidget(gbox);
  QHBoxLayout *hLayout = new QHBoxLayout(gbox);
  hLayout->setSpacing(0);
  hLayout->setContentsMargins(5, 2, 5, 5);
  gbox->setToolTip("Set amount of cache to fill at one time");

  QRadioButton *radio = diaRadioButton("All", gbox, mFillGroup, hLayout, 0, 
                                       NULL);
  radio = diaRadioButton("1/2", gbox, mFillGroup, hLayout, 1, NULL);
  radio = diaRadioButton("1/4", gbox, mFillGroup, hLayout, 2, NULL);

  diaSetGroup(mFillGroup, imodCacheFillData.fracfill);
  connect(mFillGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(fractionSelected(int)));
  
  // Set up balance radio buttons only if times loaded
  if (imodCacheFillData.vi->numTimes > 0) {
    mBalanceGroup = new QButtonGroup(this);
    gbox = new QGroupBox("Balance between times", this);
    mLayout->addWidget(gbox);
    QVBoxLayout *vLayout = new QVBoxLayout(gbox);
    vLayout->setSpacing(0);
    vLayout->setContentsMargins(5, 2, 5, 5);
    
    radio = diaRadioButton("Treat all times equally", gbox, mBalanceGroup, 
                           vLayout, 0, 
                           "Fill equal number of sections for all times");
    radio = diaRadioButton("Treat adjacent times equally", gbox, mBalanceGroup,
                           vLayout, 1, "Load more sections for current time"
                           " and two adjacent times");
    radio = diaRadioButton("Favor current time", gbox, mBalanceGroup, 
                           vLayout, 2, 
                           "Load more sections for the current time");
    radio = diaRadioButton("Load only current time", gbox, mBalanceGroup,
                           vLayout, 3,
                           "Load sections only for the current time");
    
    diaSetGroup(mBalanceGroup, imodCacheFillData.balance);
    connect(mBalanceGroup, SIGNAL(buttonClicked(int)), this, 
	    SLOT(balanceSelected(int)));
  }

  // The auto checkbox
  mAutoCheck = diaCheckBox("Autofill", this, mLayout);
  mAutoCheck->setToolTip("Fill cache to selected extent whenever "
		"section not in cache is needed");
  mAutoCheck->setChecked(imodCacheFillData.autofill != 0);
  connect(mAutoCheck, SIGNAL(toggled(bool)), this, SLOT(autoToggled(bool)));

  // The overlap radio buttons
  mOverlapGroup = new QButtonGroup(this);
  mOverlapBox = new QGroupBox("Fill fraction", this);
  mLayout->addWidget(mOverlapBox);
  hLayout = new QHBoxLayout(mOverlapBox);
  hLayout->setSpacing(0);
  hLayout->setContentsMargins(5, 2, 5, 5);
  mOverlapBox->setToolTip("Set fraction of sections to retain when"
                          " filling cache automatically");

  radio = diaRadioButton("1/2", mOverlapBox, mOverlapGroup, hLayout, 0, NULL);
  radio = diaRadioButton("1/4", mOverlapBox, mOverlapGroup, hLayout, 1, NULL);
  radio = diaRadioButton("1/8", mOverlapBox, mOverlapGroup, hLayout, 2, NULL);

  diaSetGroup(mOverlapGroup, imodCacheFillData.overlap);
  connect(mOverlapGroup, SIGNAL(buttonClicked(int)), this,
	  SLOT(overlapSelected(int)));

  // Enable the overlap buttons only if autofill is on
  mOverlapBox->setEnabled(imodCacheFillData.autofill != 0);
  //for (int i = 0; i < 3; i++)
  //mOverlapRadio[i]->setEnabled(imodCacheFillData.autofill != 0);

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));

  setWindowTitle(imodCaption("3dmod Cache Filler"));
}

// Respond to action buttons
void ImodCacheFill::buttonPressed(int which)
{
  switch(which) {
  case 0:
    if (!imodCacheFillData.vi->loadingImage)
      fill_cache(imodCacheFillData.vi, (int)imodCacheFillData.vi->zmouse, 1, 1, 0);
    break;
  case 1:
    close();
    break;
  case 2:
    imodShowHelpPage("cachefill.html#TOP");
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
  mOverlapBox->setEnabled(state);
  //for (int i = 0; i < 3; i++)
  //mOverlapRadio[i]->setEnabled(state);
}

void ImodCacheFill::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
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
  if (utilCloseKey(e))
    close();
  else
    ivwControlKey(0, e);
}

void ImodCacheFill::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}
