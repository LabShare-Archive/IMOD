/*  IMOD VERSION 2.50
 *
 *  imod.c -- Main imod program; Display MRC Images and build Models.
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
Log at the end of file
*/

#include <limits.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <qfiledialog.h>
#include <qapplication.h>
#include <qdir.h>
#include <qimage.h>
#include <qpixmap.h>
#include "xxyz.h"

#include "imod.h" 
#ifndef NO_IMOD_FORK
#include <unistd.h>
#endif
#include "imod_workprocs.h"
#include "imodv.h"
#include "imodv_views.h"
#include "xzap.h"
#include "imod_display.h"
#include "imod_info.h"
#include "imod_info_cb.h"
#include "imod_io.h"
#include "imod_input.h"
#include "imod_cachefill.h"
#include "sslice.h"
#include "control.h"
#include "imodplug.h"
#include "b3dgfx.h"
#include "xcramp.h"
#include "dia_qtutils.h"
#include "imod_client_message.h"
#include "preferences.h"
#include "form_startup.h"
#include "b3dicon.xpm"

/******************************* Globals *************************************/
ImodApp *App;
Imod    *Model;

char   *Imod_imagefile;
QString Imod_IFDpath;
QString Imod_cwdpath;

int    Imod_debug = FALSE;
int    ImodTrans  = TRUE;
int    Rampbase = RAMPBASE;

/*****************************************************************************/

static int loopStarted = 0;

void imod_usage(char *name)
{
  QString qstr; 
  char *progName = imodProgName(name);
  imodVersion(progName);
  imodCopyright();
  qstr.sprintf("Usage: %s [options] <Image files> <model file>\n", progName);
  qstr += "Options: -xyz  Open xyz window first.\n";
  qstr += "         -S    Open slicer window first.\n";
  qstr += "         -V    Open model view window first.\n";
  qstr += "         -Z    Open Zap window (use with -S, -xyz, or -V)\n";
  qstr += "         -x min,max  Load in sub image.\n";
  qstr += "         -y min,max  Load in sub image.\n";
  qstr += "         -z min,max  Load in sub image.\n";
  qstr += "         -s min,max  Scale input to range [min,max].\n";
  qstr += "         -C #  Set # of sections or Mbytes to cache (#M or #m for"
         " Mbytes).\n";
  qstr += "         -F    Fill cache right after starting program.\n";
  qstr += "         -Y    Flip volume to model planes normal to y axis.\n";
  qstr += "         -p <file name>  Load piece list file.\n";
  qstr += "         -P nx,ny  Display images as montage in nx by ny array.\n";
  qstr += "         -o nx,ny  Set x and y overlaps for montage display.\n";
  qstr += "         -f    Load as frames even if image file has piece "
         "coordinates.\n";
  qstr += "         -m    Load model with model coords (override scaling).\n";
  qstr += "         -2    Treat model as 2D only.\n";
  qstr += "         -G    Display RGB-mode MRC file in gray-scale.\n";
  qstr += "         -ci   Display images in color index mode with colormap.\n";
  qstr += "         -h    Print this help message.\n";
  imodPrintInfo(qstr.latin1());
  return;
}


int main( int argc, char *argv[])
{
  ImodApp app;
  struct ViewInfo vi;
  struct LoadInfo li;
  FILE *fin        = NULL;
  FILE *mfin       = NULL;
  char *plistfname = NULL;
  int xyzwinopen   = FALSE;
  int sliceropen   = FALSE;
  int zapOpen      = FALSE;
  int modelViewOpen= FALSE;
  int print_wid    = FALSE;
  int loadinfo     = FALSE;
  int fillCache    = FALSE;
  int new_model_created = FALSE;
  int i      = 0;
  int vers;
  int cmap;
  int cacheSize = 0;
  int namelen;
  int frames = 0;
  int firstfile = 0;
  int lastimage;
  int  pathlen;
  int grayrgbs = 0;
  int nframex = 0;
  int nframey = 0;
  int overx = 0;
  int overy = 0;
  float font_scale = 0.;
  Iobj *obj;
  QString qname;
  int doFork = 1;
  char *cmdLineStyle = NULL;
  int doImodv = 0;
  int nChars;
  QRect infoGeom;

  /* Initialize data. */
  App = &app;
  App->rgba = 1;    /* Set to 1 to force RGB visual */
  App->exiting = 0;

  /*DNM: prescan for debug, ci and style flags before the display_init */
  /* Cancel forking on debug or -W output */
  for (i = 1; i < argc; i++){
    if (!strcmp("-D", argv[i])) {
      Imod_debug = TRUE;
      doFork = 0;
    }
    if (!strcmp("-W", argv[i]))
      doFork = 0;

    if (!strcmp("-ci", argv[i]))
      App->rgba = -1;  /* Set to -1 to force worthless Color index visual */

    if (argv[i][0] == '-' && argv[i][1] == 's' && argv[i][2] == 't'
        && argv[i][3] == 'y' && argv[i][4] == 'l' && argv[i][5] == 'e') {
      if (argv[i][6] == '=')
	cmdLineStyle = strdup(&(argv[i][7]));
      else if (i < argc - 1)
	cmdLineStyle = strdup(argv[i + 1]);
    }
    
    if (!strcmp("-modv", argv[i]) || !strcmp("-view", argv[i]))
      doImodv = 1;
  }

#ifndef NO_IMOD_FORK
  /* Fork now to avoid conflicts */
  if (doFork)
    if (fork())
      exit(0);
#endif

#ifndef _WIN32
  /* if no input files, print help stuff */
  /*  if (argc < 2){
    imod_usage(argv[0]);
    exit(1);
    } */
#endif

  /* Open the Qt application */
  
  QApplication qapp(argc, argv);
  
  /* Set title for error dialogs, and set up to store error strings */
  diaSetTitle("3dmod");
  b3dSetStoreError(1);

  ImodPrefs = new ImodPreferences(cmdLineStyle);

  // Set up the application icon for windows to use
  App->iconPixmap = new QPixmap(QImage(b3dicon));

  /* if no input files, open startup window */
  if (argc < 2) {
    StartupForm *startup = new StartupForm(NULL, "startup dialog", true);
    startup->setIcon(*(App->iconPixmap));
    if (startup->exec() == QDialog::Rejected) {
      imod_usage(argv[0]);
      exit(1);
    }
    
    argv = startup->getArguments(argc);
    /*for (i = 0; i < argc; i++)
      fprintf(stderr, "%s ", argv[i]);
      fprintf(stderr, "\n"); */
    delete startup;
  }

  /* Run the program as imodv? */
  i = strlen(argv[0]);
  if (doImodv || argv[0][i-1] == 'v'){
    imodv_main(argc, argv);
    exit(0);
  }

  imod_display_init(App, argv);
  mrc_init_li(&li, NULL);
  vi.li = &li;

  /*******************/
  /* Initialize Data */
  App->cvi = &vi;
  ivwInit(&vi);
  vi.fp = fin;
  vi.vmSize = cacheSize;
  vi.flippable = 1;

#ifdef __sgi
  /* DNM: Find out how many imods this user is running and set the cmap to
     that number.  Also change the interval from 300 to 330 here and in
     imod_menu.c */
  cmap = system ("exit `\\ps -a | grep 3dmod | wc -l`");
  cmap = WEXITSTATUS(cmap);
  /*     printf("Returned cmap = %d\n", cmap); */
  if (cmap <= 0)
    cmap = 1;
  if (cmap > MAXIMUM_RAMPS)
    cmap = MAXIMUM_RAMPS;
  Rampbase  = RAMPBASE + ((cmap - 1) * RAMP_INTERVAL);
#endif

  App->base = Rampbase;

  /* handle input options. */
  for (i = 1; i < argc; i++){
    if (argv[i][0] == '-'){
      if (firstfile) {
        imodError(NULL, "3dmod: invalid to have argument %s after"
          " first filename\n", argv[i]);
        exit(1);
      }
      switch (argv[i][1]){
        
      case 'c':
        if (argv[i][2] == 'i')
          break;
        cmap = atoi(argv[++i]);
        if ((cmap > 12) || (cmap < 1)){
          imodError(NULL, "3dmod: valid -c range is 1 - 12\n");
          exit(-1);
        }
        Rampbase  = 256 + ((cmap - 1) * 330);
        App->base = Rampbase;
        break;
        
      case 'C':
        /* value ending in m or M is megabytes, store as minus */
        pathlen = strlen(argv[++i]);
        sscanf(argv[i], "%d%*c", &cacheSize);
        /* if (cacheSize < 0)
        cacheSize = 0; */
        if (argv[i][pathlen - 1] == 'M' ||
          argv[i][pathlen - 1] == 'm')
          cacheSize = -cacheSize;
        vi.vmSize = cacheSize;
        break;
        
      case 'F':
        fillCache = TRUE;
        break;

      case 'x':
        if (argv[i][2] == 'y')
          if(argv[i][3] == 'z'){
            xyzwinopen = TRUE;
            break;
          }
          loadinfo = TRUE;
          if (argv[i][2] != 0x00)
            sscanf(argv[i], "-x%d%*c%d", &(li.xmin), &(li.xmax));
          else
            sscanf(argv[++i], "%d%*c%d", &(li.xmin), &(li.xmax));
          break;
          
      case 'y':
        loadinfo = TRUE;
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-y%d%*c%d", &(li.ymin), &(li.ymax));
        else
          sscanf(argv[++i], "%d%*c%d", &(li.ymin), &(li.ymax));
        break;
        
      case 'z':
        loadinfo = TRUE;
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-z%d%*c%d", &(li.zmin), &(li.zmax));
        else
          sscanf(argv[++i], "%d%*c%d", &(li.zmin), &(li.zmax));
        break;
        
      case 's':
        loadinfo = TRUE;
        sscanf(argv[++i], "%f%*c%f", &(li.smin), &(li.smax));
        break;
        
      case 'i':
      case 'D':
        Imod_debug = TRUE;
        break;
        
      case 'm':
        ImodTrans = FALSE;
        break;
        
        /* DNM: better disable this
        case 'X':
        li.axis = 1;
        break;
        */
      case 'Y':
        li.axis = 2;
        break;
        
      case 'h':
        imod_usage(argv[0]);
        exit(1);
        break;
        
      case 'p':
        plistfname = argv[++i];
        break;
        
      case 'f':
        frames = 1;
        break;
        
      case 'G':
        grayrgbs = 1;
        break;
        
      case '2':
        vi.dim &= ~4;
        break;
        
      case 'P':
        sscanf(argv[++i], "%d%*c%d", &nframex, &nframey);
        break;
        
      case 'o':
        sscanf(argv[++i], "%d%*c%d", &overx, &overy);
        break;
        
      case 'S':
        sliceropen = TRUE;
        break;
        
      case 'V':
        modelViewOpen = TRUE;
        break;
        
      case 'Z':
        zapOpen = TRUE;
        break;
        
      case 'W':
        print_wid = TRUE;
        break;
        
        /*      case 'F':
        font_scale = atof(argv[++i]);
        break; */
        
      default:
        break;
        
      }
    } else if (!firstfile)
      firstfile = i;
  }
  
  /* this is for testing big fonts */ 
  if (font_scale > 0.) {
    QFont newFont = QApplication::font();
    float pointSize = newFont.pointSizeFloat();
    if (pointSize > 0) {
      newFont.setPointSizeFloat(pointSize * font_scale);
    } else {
      int pixelSize = newFont.pixelSize();
      newFont.setPixelSize((int)floor(pixelSize * font_scale + 0.5));
    }
    QApplication::setFont(newFont);
  }

  /* Load in all the imod plugins that we can use.*/
  imodPlugInit();

  QDir *curdir = new QDir();
  Model = NULL;

  /* Try to open the last file if there is one */
  if (firstfile) {
    mfin = fopen((QDir::convertSeparators(QString(argv[argc - 1]))).latin1(), 
      "rb");
    if (mfin == NULL) {
      
      /* Fail to open, and it is the only filename, then exit */
      if (firstfile == argc - 1) {
        imodError(NULL, "Couldn't open input file %s.\n", argv[argc - 1]);
        exit(10);
      }
      
      /* But if there are other files, open new model with that name*/
      printf("Model file (%s) not found: opening "
        "new model by that name.\n", argv[argc - 1]);

      Model = imodNew();
      lastimage = argc - 2;
      new_model_created = TRUE;
    } else {
      
    /*
    * Try loading file as a model.
      */
      Model = (struct Mod_Model *)LoadModel(mfin);
      if (Model){
        if (Imod_debug)
          fprintf(stderr, "Loaded model %s\n", argv[argc -1]);
        lastimage = argc - 2;
      } else {
        /* If fail, last file is an image */
        lastimage = argc - 1;
      }
      fclose(mfin);
    }
  }
  
  /* If we have a model and no image files before that, then it's a fake
     image */
  if (lastimage < firstfile && Model){
    vi.fakeImage = 1;
    Imod_imagefile = NULL;
    vi.nt = Model->tmax = imodGetMaxTime(Model);
    ivwCheckWildFlag(Model);
    
  } else if (!firstfile || lastimage == firstfile) {
    
  /* If there are no filenames, or one image file, then treat as image
    file or IFD.  First get filename if none */
    if (!firstfile) {
      vers = imodVersion(NULL);
      imodCopyright();    
      qname = QFileDialog::getOpenFileName(QString::null, QString::null, 0, 0, 
                                           "3dmod: Select Image file to load:");
      if (qname.isEmpty()) {
        imodError(NULL, "3DMOD: file not selected\n");
        exit(-1);
      }
      Imod_imagefile = strdup(qname.latin1());
      
    } else {
      /* Or, just set the image file name */
      Imod_imagefile = strdup
                      ((curdir->cleanDirPath(QString(argv[firstfile]))).latin1());
    }
    
    if (Imod_debug){
      fprintf(stderr, "Loading %s\n", Imod_imagefile);
    }
   
    vi.fp = fin = fopen
      ((QDir::convertSeparators(QString(Imod_imagefile))).latin1(), "r");
    if (fin == NULL){
      imodError(NULL, "Couldn't open input file %s.\n", Imod_imagefile);
      exit(10);
    }
    
    /* A single image file name can be either
    * IMOD image file desc. 
     * or mrc image file.
     */
    /* Note no need to set the current working directory when just
       determining if it's an IFD */

    vi.ifd = imodImageFileDesc(fin);

    if (Imod_debug)
      printf( "Image file type %d\n", vi.ifd);

    /* The file is an image, not an image list */
    if (!vi.ifd){
      
      errno = 0;
      vi.image = iiOpen(Imod_imagefile, "rb");
      if (!vi.image){
        qname = b3dGetError();
        qname += QString("3dmod error: Failed to load input file ") + 
          Imod_imagefile + "\n";
        if (errno) 
          qname += QString("System error: ") + strerror(errno);
        imodError(NULL, qname.latin1());
        exit(-1);
      }
      
      if (vi.image->file == IIFILE_MRC && 
        ((vi.image->format != IIFORMAT_RGB) || grayrgbs)) {
        vi.hdr = vi.image;
        
        if (li.smin == li.smax){
          li.smin = vi.image->imin;
          li.smax = vi.image->imax;
        }
        
        iiSetMM(vi.image, (double)li.smin, (double)li.smax);
        /* Removed alternative code to USEIMODI which seemed to 
        allow plugin reading */
        
      } else {
      /* If it's not an MRC file or has color, call the 
        multiple file handler, set ifd -1 */
        iiClose(vi.image);
        ivwMultipleFiles(&vi, &Imod_imagefile, 0, 0);
        vi.ifd = -1;
        vi.hdr = (ImodImageFile *)ilistItem((Ilist *)vi.imageList, 0);
      }
    }
  } else {

    /* Multiple image files, set ifd -2 */
    ivwMultipleFiles(&vi, argv, firstfile, lastimage);
    vi.ifd = -2;
  }
             
  /* set the model filename, or get a new model with null name */
  if (Model) {
    nChars = strlen((curdir->cleanDirPath(QString(argv[argc - 1]))).latin1());
    if (nChars >= IMOD_FILENAME_SIZE)
      nChars = IMOD_FILENAME_SIZE - 1;
    strncpy(Imod_filename,
      (curdir->cleanDirPath(QString(argv[argc - 1]))).latin1(), nChars);
  } else {
    Model = imodNew();
    Imod_filename[0] = 0x00;
    new_model_created = TRUE;
  }

  /* If new model created, make first object, and 
     if there are multiple image files, set time flag by default */
  if (new_model_created) {
    imodvViewsInitialize(Model);
    imodNewObject(Model);
    obj = imodObjectGet(Model);
    if (vi.nt)
      obj->flags |= IMOD_OBJFLAG_TIME;
  }

  Model->mousemode = IMOD_MMOVIE;
  vi.imod = Model;

  /* DNM: set this now in case image load is interrupted */
  Model->csum = imodChecksum(Model);

  /*********************/
  /* Open Main Window. */
  imod_info_open(); 

  if (Imod_debug)
    puts("info opened");
  imod_color_init(App);
  imod_set_mmode(IMOD_MMOVIE);

  /* Copy filename into model structure */
  namelen = strlen(Imod_filename)+1;
  Model->fileName = (char *)malloc(namelen);
  if (Model->fileName)
    memcpy(Model->fileName, Imod_filename, namelen);

  /* report window before loading data */
  if (print_wid) {
    unsigned int winID = (unsigned int)ImodInfoWin->winId();
    fprintf(stderr, "Window id = %u\n", winID);
    fflush(stderr);    // Needed on Windows
    if (Imod_debug)
      wprint("Window id = %u\n", winID);
  }

  /* Get the clipboard messaging object on heap (doesn't work on stack!) */
  ImodClipboard *clipHandler = new ImodClipboard();

  /********************************************/
  /* Load in image data, set up image buffer. */
  /* change the current directory in case there's an IFD file */
  if (!vi.fakeImage && vi.ifd > 0) {

    Imod_cwdpath = QDir::currentDirPath();

    Imod_IFDpath = QString(Imod_imagefile);
    pathlen = Imod_IFDpath.findRev('/');
    if (pathlen < 0)
      Imod_IFDpath = "";
    else {
      Imod_IFDpath.truncate(pathlen + 1);
      QDir::setCurrent(Imod_IFDpath);
      if (Imod_debug)
        printf("chdir %s\n", Imod_IFDpath.latin1());
    }
  }

  if ((vi.ifd == 0 || vi.ifd == -1) && (!vi.fakeImage)) {
    /* Check for piece list file and read it */
    iiPlistLoad(plistfname, vi.li, 
                vi.hdr->nx, vi.hdr->ny, vi.hdr->nz);

    if (!vi.li->plist && nframex > 0 && nframey > 0)
      mrc_plist_create(vi.li, vi.hdr->nx, vi.hdr->ny, vi.hdr->nz,
                       nframex, nframey, overx, overy);

    /* Or, check for piece coordinates in image header */
    if (!vi.li->plist && !frames)
      iiLoadPCoord(vi.image, vi.li,
                   vi.hdr->nx, vi.hdr->ny, vi.hdr->nz);
          
    if (vi.li->plist) {
    /* If pieces, change loading coordinates by the offset in piece
      coordinates */
      if (li.xmin != -1)
        li.xmin -= (int)li.opx;
      if (li.xmax != -1)
        li.xmax -= (int)li.opx;
      if (li.ymin != -1)
        li.ymin -= (int)li.opy;
      if (li.ymax != -1)
        li.ymax -= (int)li.opy;
      if (li.zmin != -1)
        li.zmin -= (int)li.opz;
      if (li.zmax != -1)
        li.zmax -= (int)li.opz;
      /* nip the -Y flag in the bud to avoid misunderstanding */
      li.axis = 3;
      vi.flippable = 0;
      /* need to fix the coordinates now if not standard MRC */
      if (vi.ifd < 0)
        mrc_fix_li(&li, (int)li.px, (int)li.py, (int)li.pz);
    }

  }

  /* Finish loading/setting up images, reading IFD if necessary */
  errno = 0;
  if (ivwLoadImage(&vi)){
    qname = b3dGetError();
    qname += "3dmod: Fatal Error -- while reading image data.\n";
    if (errno) 
      qname += QString("System error: ") + strerror(errno);
    imodError(NULL, qname.latin1());
    exit(-1);
  }

  if (fillCache && vi.vmSize)
    imodCacheFill(&vi);

  if (!Imod_IFDpath.isEmpty())
    QDir::setCurrent(Imod_cwdpath);

  if (Imod_debug) puts("Read image data OK.");
  if (Imod_imagefile)
    wprint("\nImage %s\n", 
      (QDir::convertSeparators(QString(Imod_imagefile))).latin1());
  else if (vi.fakeImage)
    wprint("\nNo image loaded.\n");

  delete curdir;
  ImodInfoWin->manageMenus();

  /*************************************/
  /* add all work procs and time outs. (DNM: no more for imodv) */

  /* imodv_add_anim(); */
  imod_start_autosave(App->cvi);

  /* Satisfy the lawyers. */
  wprint("3dmod %s Copyright %s\n"
         "BL3DEMC & Regents of the Univ. of Colo.\n", 
         VERSION_NAME, COPYRIGHT_YEARS);
  imod_draw_window();
  xcramp_setlevels(App->cvi->cramp,App->cvi->black,App->cvi->white);

  /*********************************/
  /* Open up default Image Window. */
  if (xyzwinopen && !vi.rawImageStore)
    xxyz_open(&vi);
  if (sliceropen && !vi.rawImageStore)
    sslice_open(&vi);
  if (modelViewOpen)
    imodv_open();
  if (zapOpen || !(xyzwinopen || sliceropen || modelViewOpen))
    imod_zap_open(&vi); 
  if (Imod_debug)  
    puts("initial windows opened");
  if (App->rgba)
    imod_info_setbw(App->cvi->black, App->cvi->white);

  /* Start main application input loop. */
  if (Imod_debug)
    puts("mainloop");
  imodPlugCall(&vi, 0, IMOD_REASON_STARTUP);

  loopStarted = 1;

  return qapp.exec();
}

/* Close everything as gracefully as possible */
void imod_exit(int retcode)
{
  if (ImodPrefs)                     // Save settings first to get zap sizes
    ImodPrefs->saveSettings();
  imodv_close();                     // Imodv and associated dialogs
  ivwControlListDelete(App->cvi);    // Image windows
  imodDialogManager.close();         // Remaining imod dialog windows
  // It did NOT work to use qApp->closeAllWindows after this
  if (!loopStarted)
    exit(retcode);
  QApplication::exit(retcode);
  App->exiting = 1;
}

/* DNM 2/7/02: keep it from sending up another window if one is already up */
void imod_quit(void)
{
  int done, err;

  if (!imod_model_changed(Model)){
    imod_cleanup_autosave();
    imod_exit(0);
    return;
  }

  done = dia_choice("Save model before quitting?",
                    "Yes", "No", "Cancel");

  switch(done){
  case 1:
          
    if ((err = SaveModel(Model))){
      if (err == IMOD_IO_SAVE_CANCEL)
        break;
      wprint("%s\n", imodIOGetErrorString());
      wprint("Model not saved; quit aborted.\n");
      break;
    }else{
      imod_cleanup_autosave();
      imod_exit(0);
      return;
    }
    break;

  case 2:
    imod_cleanup_autosave();
    /* DNM: It used to make one last autosave and quit, but if the user
       says NO, then just clean up and quit! */
    imod_exit(0);
    return;
    break;

  case 3:
    break;

  default:
    break;
  }
  return;
}

/* Appends either the model or file name to the window name, giving
   first priority to the model name if "modelFirst" is set */
char *imodwEithername(char *intro, char *filein, int modelFirst)
{
  char *retString;
  if (modelFirst) {
    retString = imodwGivenName(intro, filein);
    if (!retString)
      retString = imodwfname(intro);

  } else {
    retString = imodwfname(intro);
    if (!retString)
      retString = imodwGivenName(intro, filein);
  }
  return(retString);
}


/* Appends the given name to window name */
char *imodwGivenName(char *intro, char *filein)
{
  char *winame, *filename;
  int i;
     
  filename = filein;

  /* DNM: treat null name and null pointer the same */
  if (!filename || !*filename)
    return(NULL);
  for(i = 0; filein[i]; i++){
    if (filein[i] == '/'){
      filename = &(filein[i]);
      filename++;
    }
  }
  winame = (char *)malloc(strlen(filename) + strlen(intro) + 2);
  if (!winame)
    return(NULL);
  sprintf(winame, "%s %s", intro, filename);
  return(winame);
}

/* Appends image name to window names. */
char *imodwfname(char *intro)
{
  char *filename;
  filename = Imod_imagefile;

  /* DNM 7/21/02: if multiple files, output number of image files */
  if (!filename && App->cvi->nt > 1) {
    filename = (char *)malloc(20 + strlen(intro));
    if (!filename)
      return NULL;
    sprintf(filename, "%s %d image files", intro, App->cvi->nt);
    return(filename);
  }
  return (imodwGivenName(intro, filename));
}

/* Takes an intro without a :, and returns a qstring with intro: filename
   or just intro */
QString imodCaption(char *intro)
{
  QString qstr = intro;
  qstr += ":";
  char *name = imodwfname((char *)qstr.latin1());
  if (name) {
    qstr = name;
    free(name);
  } else
    qstr = intro;
  return qstr;
}

void imodError(FILE *out, const char *format, ...)
{
  char errorMess[512];
  va_list args;
  va_start(args, format);
  
  vsprintf(errorMess, format, args);
#ifdef _WIN32
  out = NULL;
#endif
  if (out)
    fprintf(out, errorMess);
  else
    dia_err(errorMess);
}

void imodPrintInfo(const char *message)
{
#ifdef _WIN32
  dia_puts((char *)message);
#else
  printf(message);
#endif
}


/***********************************************************************
 * Core application plugin lookup functions.
 *
 */

int           imodDepth(void){ return(App->depth); }

void imodDefaultKeys(QKeyEvent *event, ImodView *vw)
{
  inputQDefaultKeys(event, vw);
}

int imodColorValue(int inColor)
{
  int pixel = 0;

  switch(inColor)
    {
    case COLOR_BACKGROUND:
      pixel = App->background; break;
    case COLOR_FOREGROUND:
      pixel = App->foreground; break;
    case COLOR_SELECT:
      pixel = App->select; break;
    case COLOR_SHADOW:
      pixel = App->shadow; break;
    case COLOR_END:
      pixel = App->endpoint; break;
    case COLOR_BEGIN:
      pixel = App->bgnpoint; break;
    case COLOR_POINT:
      pixel = App->curpoint; break;
    case COLOR_GHOST:
      pixel = App->ghost; break;
    case COLOR_MIN:
      pixel = App->cvi->rampbase; break;
    case COLOR_MAX:
      pixel =(App->cvi->rampbase + App->cvi->rampsize);break;
            
    }
  b3dColorIndex(pixel);
  return pixel;
}

/*
$Log$
Revision 4.26  2003/11/01 18:12:16  mast
changed to put out virtually all error messages to a window

Revision 4.25  2003/10/01 05:13:56  mast
Added functions for rationalizing plugin compilation

Revision 4.24  2003/09/24 17:33:09  mast
Remove setting of info window geometry from here - wanted it sooner

Revision 4.23  2003/09/24 15:08:37  mast
Put window ID to info window in debug mode

Revision 4.22  2003/09/24 00:50:11  mast
Switched from keeping track of geometry to keeping track of pos() and
size() when saving and restoring positions and sizes

Revision 4.21  2003/09/17 04:48:43  mast
Added call to set info window geometry and made settings get saved before
exit

Revision 4.20  2003/09/13 04:32:33  mast
Changed to protect the model filename array from overflow

Revision 4.19  2003/06/27 19:24:13  mast
initialize views when start a new model

Revision 4.18  2003/05/18 22:59:00  mast
Create icon pixmap here to be able to set it for startup dialog

Revision 4.17  2003/05/18 22:07:37  mast
changes for new startup dialog

Revision 4.16  2003/05/12 22:07:39  mast
had to flush stderr to get window ID to etomo on Windows

Revision 4.15  2003/04/30 23:54:41  mast
Comment out printing window id to info window

Revision 4.14  2003/04/25 00:13:33  mast
Added cache filling option and make program name changes

Revision 4.13  2003/04/18 20:15:40  mast
Set flag when program is exiting

Revision 4.12  2003/04/17 21:53:05  mast
Fix simplification

Revision 4.11  2003/04/17 21:48:44  mast
simplify -imodv option processing

Revision 4.10  2003/04/11 18:15:59  mast
Fix exiting logic to not exit after calling Qt exit

Revision 4.9  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.8  2003/03/24 17:58:09  mast
Changes for new preferences capability

Revision 4.7  2003/03/12 20:50:32  mast
make starting with no arguments allow file selection in Windows

Revision 4.6  2003/02/28 01:31:08  mast
fixing include fiddles

Revision 4.5  2003/02/27 23:07:13  mast
fiddling with includes some more

Revision 4.4  2003/02/27 19:23:51  mast
Changes for windows version

Revision 4.3  2003/02/22 00:00:44  mast
Open file in binary mode

Revision 4.2  2003/02/20 15:58:57  mast
Add -V and -Z options, rationalize them with -S and -xyz options

Revision 4.1  2003/02/10 20:28:59  mast
autox.cpp

Revision 1.1.2.17  2003/02/04 19:10:16  mast
Set default style to windows everywhere

Revision 1.1.2.16  2003/01/29 17:49:20  mast
Fork at top of program before doing any Qt stuff, and don't fork with -W

Revision 1.1.2.15  2003/01/29 01:31:24  mast
change -rgb to -ci, close windows on exit

Revision 1.1.2.14  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.13  2003/01/23 20:14:09  mast
Add include of imod_io

Revision 1.1.2.12  2003/01/13 01:15:42  mast
changes for Qt version of info window

Revision 1.1.2.11  2003/01/06 15:41:02  mast
Add imodCaption function

Revision 1.1.2.10  2002/12/23 04:52:58  mast
Add option to get different font size

Revision 1.1.2.9  2002/12/19 04:37:13  mast
Cleanup of unused global variables and defines

Revision 1.1.2.8  2002/12/17 18:40:24  mast
Changes and new includes with Qt version of imodv

Revision 1.1.2.7  2002/12/14 17:53:04  mast
*** empty log message ***

Revision 1.1.2.6  2002/12/14 05:40:43  mast
new visual-assessing code

Revision 1.1.2.5  2002/12/13 06:09:09  mast
include file changes

Revision 1.1.2.4  2002/12/09 17:49:19  mast
changes to get Zap as a Qt window

Revision 1.1.2.3  2002/12/07 01:23:23  mast
Improved window title code

Revision 1.1.2.2  2002/12/06 21:58:35  mast
*** empty log message ***

Revision 1.1.2.1  2002/12/05 16:24:46  mast
Open a Qxt application

Revision 3.11  2002/12/03 15:45:08  mast
Call SaveModel instead of SaveModelQuit when quitting, to give user a chance
to set the filename to save to

Revision 3.10  2002/12/01 16:51:34  mast
Changes to eliminate warnings on SGI

Revision 3.9  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.8  2002/09/27 19:46:26  rickg
Reverted LoadModel call due to changes in imod_io
Added error string to SaveModelQuit call
Removed redudant function declarations at begging of file.

Revision 3.7  2002/09/18 22:56:48  rickg
Print out process ID when printing out window ID.

Revision 3.6  2002/09/18 02:51:35  mast
Started event handler right after the fork, so it can receive events during
the image load.

Revision 3.5  2002/09/17 18:40:33  mast
Moved the report to window ID to before fork and data loading

Revision 3.4  2002/09/14 00:13:11  mast
Set declarations and use of event handler right to make SGI compiler happy

Revision 3.3  2002/09/13 21:05:39  mast
Set up event handler for client messages, added option to output window ID

Revision 3.2  2002/07/21 20:28:52  mast
Changed imodwfname to return a string with number of image files when
multiple files are loaded.

Revision 3.1  2002/05/20 15:32:39  mast
Added -S option to open slicer first; made it set a new model so that time
index modeling is the default if multiple files are opened.

*/
