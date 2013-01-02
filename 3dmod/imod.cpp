/*
 *  imod.cpp -- Main 3dmod program; Display MRC Images and build Models.
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

#include <limits.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
#include <locale.h>
#include <qfiledialog.h>
#include <qapplication.h>
#include <qdir.h>
#include <qdatetime.h>
#include <qimage.h>
#include <qpixmap.h>
#include "xxyz.h"

#include "imod.h" 
#ifndef NO_IMOD_FORK
#include <unistd.h>
#endif
#include "workprocs.h"
#include "imodv.h"
#include "mv_menu.h"
#include "mv_views.h"
#include "xzap.h"
#include "display.h"
#include "info_setup.h"
#include "info_cb.h"
#include "imod_io.h"
#include "imod_input.h"
#include "cachefill.h"
#include "sslice.h"
#include "control.h"
#include "imodplug.h"
#include "b3dgfx.h"
#include "xcramp.h"
#include "dia_qtutils.h"
#include "client_message.h"
#include "preferences.h"
#include "form_startup.h"
#include "imod_assistant.h"
#include "iirawimage.h"
#include "b3dicon.xpm"

extern "C" int iiQImageCheck(ImodImageFile *inFile);

/******************************* Globals *************************************/
ImodApp *App;
Imod    *Model = NULL;

char   *Imod_imagefile;
QString Imod_IFDpath;
QString Imod_cwdpath;

int    Imod_debug = FALSE;
int    ImodTrans  = TRUE;
int    Rampbase = RAMPBASE;
ImodAssistant *ImodHelp = NULL;
ImodClipboard *ClipHandler = NULL;

/*****************************************************************************/

static int loopStarted = 0;
static char *debugKeys = NULL;
static char *windowKeys = NULL;

void imod_usage(char *name)
{
  QString qstr; 
  char *progName = imodProgName(name);
  imodVersion(progName);
  imodCopyright();
  qstr.sprintf("Usage: %s [options] <Image files> <model file>\n", progName);
  qstr += "Options:\n";
  qstr += "   -xyz  Open xyz window first.\n";
  qstr += "   -S    Open slicer window first.\n";
  qstr += "   -V    Open model view window first.\n";
  qstr += "   -Z    Open Zap window (use with -S, -xyz, or -V).\n";
  qstr += "   -O    Set options with startup dialog box.\n";
  qstr += "   -x min,max  Load in sub image.\n";
  qstr += "   -y min,max  Load in sub image.\n";
  qstr += "   -z min,max  Load in sub image.\n";
  qstr += "   -s min,max  Scale input to range [min,max] (0,0 for all files the same).\n";
  qstr += "   -I #  Load data as 16-bit integers (# = 1) or as bytes (# = 0).\n";
  qstr += "   -C #  Set # of sections or Mbytes to cache (#M or #m for"
    " Mbytes).\n";
  qstr += "   -F    Fill cache right after starting program.\n";
  qstr += "   -Y    Rotate volume around X to model planes normal to Y axis.\n";
  qstr += "   -B #  Bin images by # in X, Y, and Z.\n";
  qstr += "   -b nxy,nz  Bin images by nxy in X and Y, by nz in Z (nz "
    "default=1).\n";
  qstr += "   -a <file name>  Load tilt angles from file.\n";
  qstr += "   -p <file name>  Load piece list file.\n";
  qstr += "   -pm   Load piece list from .mdoc metadata file\n";
  qstr += "   -P nx,ny  Display images as montage in nx by ny array.\n";
  qstr += "   -o nx,ny  Set X and X overlaps for montage display.\n";
  qstr += "   -f    Load as frames even if image file has piece "
    "coordinates.\n";
  qstr += "   -r nx,ny,nz  Dimensions for raw image files.\n";
  qstr += "   -ri   Raw file is inverted in Y.\n";
  qstr += "   -t #  Mode for raw files: -1/0=byte, 1/6=int, 2=float, "
    "4=complex, 16=RGB,\n";
  qstr += "   -H #  Header size in bytes in raw image files.\n";
  qstr += "   -w    Swap bytes from raw image files.\n";
  qstr += "   -R    Read image data from standard input, not from file."
    "\n";
  qstr += "   -m    Load model with model coords (override scaling).\n";
  qstr += "   -2    Treat model as 2D only.\n";
  qstr += "   -T    Display multiple single-image files as times not "
    "sections.\n";
  qstr += "   -G    Display RGB-mode MRC file in gray-scale.\n";
  qstr += "   -M    Do not mirror FFT data around Y axis.\n";
  qstr += "   -ci   Display images in color index mode with colormap.\n";
  qstr += "   -cm <file name>  Load custom false color table.\n";
  qstr += "   -E <keys>  Open windows specified by key letters.\n";
  qstr += "   -h    Print this help message.\n";
  imodPrintInfo(LATIN1(qstr));
  return;
}


int main( int argc, char *argv[])
{
  ImodApp app;
  ImodView vi;
  IloadInfo li;
  FILE *fin        = NULL;
  FILE *mfin       = NULL;
  char *plistfname = NULL;
  char *anglefname = NULL;
  int xyzwinopen   = FALSE;
  int sliceropen   = FALSE;
  int zapOpen      = FALSE;
  int modelViewOpen= FALSE;
  bool print_wid   = false;
  int fillCache    = FALSE;
  bool newModelCreated = false;
  int i      = 0;
  int nx, ny, nz, mode;
  int minxpiece, numXpieces, xoverlap, minypiece, numYpieces, yoverlap;
  int frames = 0;
  int firstfile = 0;
  int lastimage;
  int  pathlen;
  int nframex = 0;
  int nframey = 0;
  int overx = 0;
  int overy = 0;
  int doStartup = 0;
  int hugeCache = 2000000000;
  QString qname;
  int doFork = 1;
  char *cmdLineStyle = NULL;
  int doImodv = 0;
  int rawSet = 0;
  int overEntered = 0;
  int useMdoc = 0;
  bool useStdin = false;
  bool dataFromStdin = false;
  int argScan;
  QRect infoGeom;
  StartupForm *startup;

  /* Initialize data. */
  App = &app;
  App->rgba = 1;    /* Set to 1 to force RGB visual */
  App->exiting = 0;
  App->closing = 0;
  App->cvi = &vi;
  App->base = Rampbase;
  App->convertSnap = 0;
  App->glInitialized = 0;

  /* Set up fixed indexes */
  App->background   = IMOD_BACKGROUND;
  App->foreground   = IMOD_FOREGROUND;
  App->select       = IMOD_SELECT;
  App->shadow       = IMOD_SHADOW;
  App->endpoint     = IMOD_ENDPOINT;
  App->bgnpoint     = IMOD_BGNPOINT;
  App->curpoint     = IMOD_CURPOINT;
  App->ghost        = IMOD_GHOST;

  /*DNM: prescan for debug, ci and style flags before the display_init */
  /* Cancel forking on debug or -W output, or -L or -R or -h */
  for (i = 1; i < argc; i++){
    if (!strncmp("-D", argv[i], 2)) {
      Imod_debug = TRUE;
      debugKeys = strdup(argv[i] + 2);
      doFork = 0;
    }
    if (!strcmp("-W", argv[i]) || !strcmp("-R", argv[i]) || !strcmp("-h", argv[i]))
      doFork = 0;

    if (!strcmp("-L", argv[i])) {
      doFork = 0;
#if defined(_WIN32) && !defined(QT_THREAD_SUPPORT)
      imodError(NULL, "Error: -L option cannot be used because "
                "3dmod was not built with Qt thread support\n");
      exit(1);
#endif
    }

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

    if (!strcmp("-O", argv[i]))
      doStartup = 1;
    if (argv[i][0] == '-')
      firstfile = i + 1;
  }

#ifndef NO_IMOD_FORK
  /* Fork now to avoid conflicts */
  if (doFork)
    if (fork())
      exit(0);
#endif

  /* Open the Qt application */
  
  QApplication qapp(argc, argv);
  
  /* Set title for error dialogs, and set up to store error strings */
  diaSetTitle("3dmod");
  b3dSetStoreError(1);
  setlocale(LC_NUMERIC, "C");

  ImodPrefs = new ImodPreferences(cmdLineStyle);
  ImodHelp = new ImodAssistant("html", "IMOD.qhc", "3dmod", false, false, "3dmodHelp");

  // Set up the application icon for windows to use
  App->iconPixmap = new QPixmap(QPixmap::fromImage(QImage(b3dicon)));

  /* if no input files, open startup window */
  i = strlen(argv[0]);
  if (argv[0][i-1] == 'v')
    doImodv = 1;
  if (argc < 2 || (doStartup && doImodv)) {
    startup = new StartupForm(NULL, true, Qt::Window);
    startup->setWindowIcon(*(App->iconPixmap));
    if (doImodv) 
      startup->setValues(&vi, argv, firstfile, argc, doImodv, plistfname, 
                         anglefname, xyzwinopen, sliceropen, zapOpen,
                         modelViewOpen, fillCache, ImodTrans, 0, frames,
                         nframex, nframey, overx, overy, overEntered);
    if (startup->exec() == QDialog::Rejected) {
      imod_usage(argv[0]);
      exit(1);
    }
    
    argv = startup->getArguments(argc);
    doImodv = 0;
    /*for (i = 0; i < argc; i++)
      imodPrintStderr("%s ", argv[i]);
      imodPrintStderr("\n"); */
    delete startup;
  }

  /* Run the program as imodv? */
  i = strlen(argv[0]);
  if (doImodv || argv[0][i-1] == 'v'){
    imodv_main(argc, argv);
    exit(0);
  }

  /* 3/14/11: eliminated old code for out how many imods this user is running. */

  /*******************/
  /* Loop once or twice on arguments; initialize Data each time */

  for (argScan = 0; argScan <= doStartup; argScan++) { 
    mrc_init_li(&li, NULL);
    ivwInit(&vi, false);
    vi.li = &li;
    if (ImodPrefs->loadUshorts())
      vi.rawImageStore = MRC_MODE_USHORT;
    plistfname = NULL;
    xyzwinopen   = FALSE;
    sliceropen   = FALSE;
    zapOpen      = FALSE;
    modelViewOpen= FALSE;
    fillCache    = FALSE;
    ImodTrans  = TRUE;
    frames     = 0;
    nframex = 0;
    nframey = 0;
    overx = 0;
    overy = 0;
    firstfile = 0;
    overEntered = 0;

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
          /* 1/5/04: eliminated colormap selection option */
          if (argv[i][2] == 'm') {
            if (xcramp_readfalsemap(argv[++i])) {
              qname = b3dGetError();
              imodError(NULL, LATIN1(qname));
            }
          }
          break;
        
        case 'C':
          /* value ending in m or M is megabytes, store as minus */
          pathlen = strlen(argv[++i]);
          sscanf(argv[i], "%d%*c", &vi.vmSize);
          if (argv[i][pathlen - 1] == 'M' || argv[i][pathlen - 1] == 'm')
            vi.vmSize = -vi.vmSize;
          else if (!vi.vmSize)
            vi.vmSize = hugeCache;
          vi.keepCacheFull = 0;
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
          if (argv[i][2] != 0x00)
            sscanf(argv[i], "-x%d%*c%d", &(li.xmin), &(li.xmax));
          else
            sscanf(argv[++i], "%d%*c%d", &(li.xmin), &(li.xmax));
          break;
          
        case 'y':
          if (argv[i][2] != 0x00)
            sscanf(argv[i], "-y%d%*c%d", &(li.ymin), &(li.ymax));
          else
            sscanf(argv[++i], "%d%*c%d", &(li.ymin), &(li.ymax));
          break;
        
        case 'z':
          if (argv[i][2] != 0x00)
            sscanf(argv[i], "-z%d%*c%d", &(li.zmin), &(li.zmax));
          else
            sscanf(argv[++i], "%d%*c%d", &(li.zmin), &(li.zmax));
          break;
        
        case 's':
          sscanf(argv[++i], "%f%*c%f", &(li.smin), &(li.smax));
          if (li.smin || li.smax)
            iiRawSetScale(li.smin, li.smax);
          else
            vi.equalScaling = 1;
          break;
        
        case 'b':
          sscanf(argv[++i], "%d%*c%d", &(vi.xybin), &(vi.zbin));
          if (!vi.zbin)
            vi.zbin = 1;
          break;

        case 'B':
          sscanf(argv[++i], "%d", &(vi.xybin));
          vi.zbin = vi.xybin;
          break;

        case 'i':
        case 'D':
          Imod_debug = TRUE;
          break;
        
        case 'E':
          windowKeys = strdup(argv[++i]);
          break;

        case 'm':
          ImodTrans = FALSE;
          break;
        
        case 'M':
          vi.li->mirrorFFT = -1;
          break;
        
        case 'Y':
          li.axis = 2;
          break;
        
        case 'h':
          imod_usage(argv[0]);
          exit(1);
          break;
        
        case 'p':
          if (argv[i][2] == 'm') {
            useMdoc = 1;
            break;
          }
          plistfname = argv[++i];
          break;
        
        case 'a':
          anglefname = argv[++i];
          break;
        
        case 'f':
          frames = 1;
          break;
        
        case 'G':
          vi.grayRGBs = 1;
          break;
        
        case '2':
          vi.dim &= ~4;
          break;
        
        case 'P':
          sscanf(argv[++i], "%d%*c%d", &nframex, &nframey);
          break;
        
        case 'o':
          overEntered = 1;
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
        
        case 'T':
          vi.multiFileZ = -1;
          break;
        
        case 'W':
          print_wid = true;
          break;
        
        case 'r':
          if (argv[i][2] == 'i') {
            iiRawSetInverted();
          } else {
            sscanf(argv[++i], "%d,%d,%d", &nx, &ny, &nz);
            iiRawSetSize(nx, ny, nz);
          }
          rawSet = 1;
          break;

        case 't':
          sscanf(argv[++i], "%d", &mode);
          iiRawSetMode(mode);
          rawSet = 1;
          break;

        case 'H':
          sscanf(argv[++i], "%d", &mode);
          iiRawSetHeaderSize(mode);
          rawSet = 1;
          break;

        case 'w':
          iiRawSetSwap();
          rawSet = 1;
          break;
        
        case 'I':
          sscanf(argv[++i], "%d", &mode);
          if (mode)
            vi.rawImageStore = MRC_MODE_USHORT;
          else
            vi.rawImageStore = 0;
          break;

        case 'R':
          dataFromStdin = true;
          break;

        case 'L':
          useStdin = true;
          break;
        
        default:
          break;
        
        }
      } else if (!firstfile)
        firstfile = i;
    }

    /* First time through when doing startup, open startup and give it 
       options */
    if (!argScan && doStartup) {
      startup = new StartupForm(NULL, true, Qt::Window);
      startup->setWindowIcon(*(App->iconPixmap));
      startup->setValues(&vi, argv, firstfile, argc, doImodv, plistfname,
                         anglefname, xyzwinopen, sliceropen, zapOpen,
                         modelViewOpen, fillCache, ImodTrans, vi.li->mirrorFFT,
                         frames, nframex, nframey, overx, overy, overEntered);
      if (startup->exec() == QDialog::Rejected) {
        imod_usage(argv[0]);
        exit(1);
      }
    
      argv = startup->getArguments(argc);
      if (Imod_debug) {
        for (i = 0; i < argc; i++)
          imodPrintStderr("%s ", argv[i]);
        imodPrintStderr("\n"); 
      }
      delete startup;
    }
  }
  
  /* Initialize the display system - defer color ramps until image type is known */
  imod_display_init(App, argv);

  /* Load in all the imod plugins that we can use.*/
  imodPlugInit();

  /* Add check function for raw and QImage formats after plugins so plugins
     can add them first.  But if any raw options were entered, put raw check 
     first.  But if reading from stdin, put MRC check first! */
  if (dataFromStdin) 
    iiInsertCheckFunction(iiMRCCheck, 0);
  iiAddCheckFunction(iiQImageCheck);
  if (rawSet)
    iiInsertCheckFunction(iiRawCheck,0);
  else
    iiAddCheckFunction(iiRawCheck);
  tiffFilterWarnings();

  QDir *curdir = new QDir();

  /* Try to open the last file if there is one */
  if (firstfile) {
    mfin = fopen(LATIN1(QDir::convertSeparators(QString(argv[argc - 1]))),
                 "rb");
    if (mfin == NULL) {
      
      /* Fail to open, and it is the only filename, then exit */
      if (firstfile == argc - 1) {
        imodError(NULL, "Couldn't open input file %s.\n", argv[argc - 1]);
        exit(10);
      }
      
      /* But if there are other files, set up to open new model with that name*/
      imodPrintStderr("Model file (%s) not found: opening "
        "new model by that name.\n", argv[argc - 1]);

      lastimage = argc - 2;
      newModelCreated = true;
    } else {
      
    /*
    * Try loading file as a model.  Turn it on if successful
      */
      Model = LoadModel(mfin);
      if (Model) {
        if (Imod_debug)
          imodPrintStderr("Loaded model %s\n", argv[argc -1]);
        lastimage = argc - 2;
        Model->drawmode = 1;

        // Set this now in case image load is interrupted
        Model->csum = imodChecksum(Model);
      } else {
        /* If fail, last file is an image */
        lastimage = argc - 1;
      }
      fclose(mfin);
    }
  }
  
  if (dataFromStdin) {

    // Data from stdin: check for contradictory options
    if (li.xmin != -1 || li.xmax != -1 || li.ymin != -1 || li.ymax != -1 ||
        li.zmin != -1 || li.zmax != -1) {
      imodError(NULL, "You cannot set subareas when reading data from stdin"
                "\n");
      exit(3);
    }
    if (plistfname || useStdin || rawSet || vi.vmSize) {
      imodError(NULL, "You cannot use -C, -L, -p, or raw options when "
                "reading data from stdin\n");
      exit(3);
    }
    if (firstfile && lastimage >= firstfile) {
      imodError(NULL, "You cannot enter any image files when "
                "reading data from stdin\n");
      exit(3);
    }

    Imod_imagefile = "";
    vi.noReadableImage = 1;
    ivwMultipleFiles(&vi, &Imod_imagefile, 0, 0);

  } else if (lastimage < firstfile && Model) {

    /* If we have a model and no image files before that, then it's a fake image */
    vi.fakeImage = 1;
    Imod_imagefile = NULL;
    vi.nt = Model->tmax = imodGetMaxTime(Model);
    
  } else if (!firstfile || lastimage == firstfile) {
    
  /* If there are no filenames, or one image file, then treat as image
    file or IFD.  First get filename if none */
    if (!firstfile) {
      imodVersion(NULL);
      imodCopyright();    
      qname = QFileDialog::getOpenFileName
        (NULL, "3dmod: Select Image file to load:");
      if (qname.isEmpty()) {
        imodError(NULL, "3DMOD: file not selected\n");
        exit(3);
      }
      Imod_imagefile = strdup(LATIN1(qname));
      
    } else {
      /* Or, just set the image file name */
      Imod_imagefile = strdup
        (LATIN1(curdir->cleanPath(QString(argv[firstfile]))));
    }
    
    if (Imod_debug){
      imodPrintStderr("Loading %s\n", Imod_imagefile);
    }
   
    vi.fp = fin = fopen
      (LATIN1(QDir::convertSeparators(QString(Imod_imagefile))), "r");
    if (fin == NULL){
      imodError(NULL, "Couldn't open input file %s.\n", Imod_imagefile);
      exit(10);
    }
    
    /* A single image file name can be either
    * IMOD image file desc. 
     * or mrc image file.
     */
    vi.ifd = imodImageFileDesc(fin);

    if (Imod_debug)
      imodPrintStderr( "Image file type %d\n", vi.ifd);

    if (vi.ifd) {

      /* The file is an image list */
      if (vi.ifd > 1) {
        imodError(NULL, "3dmod: Image list file version too high.\n");
        exit (11);
      }

      /* take directory path to IFD file as new current directory for reading
         images */
      Imod_cwdpath = QDir::currentPath();

      Imod_IFDpath = QString(Imod_imagefile);
      pathlen = Imod_IFDpath.lastIndexOf('/');
      if (pathlen < 0)
        Imod_IFDpath = "";
      else {
        Imod_IFDpath.truncate(pathlen + 1);
        QDir::setCurrent(Imod_IFDpath);
        if (Imod_debug)
          imodPrintStderr("chdir %s\n", LATIN1(Imod_IFDpath));
      }

      /* Load list of images and reset current directory */
      ivwLoadIMODifd(&vi);
      if (!Imod_IFDpath.isEmpty())
        QDir::setCurrent(Imod_cwdpath);

    } else {

      /* It is a single image file - build list with this image */
      ivwMultipleFiles(&vi, &Imod_imagefile, 0, 0);
    }
  } else {

    /* Multiple image files - build list of images */
    ivwMultipleFiles(&vi, argv, firstfile, lastimage);
  }
             
  /* If one file, use its smin, smax to set li's smin,smax - may not be
     needed but used to happen */
  /*if (!vi.fakeImage && vi.nt <= 1) {
    li.smin = vi.image->smin;
    li.smax = vi.image->smax;
    }*/

  /* Now look for piece coordinates - moved up from below 1/2/04 */
  if (!vi.fakeImage && vi.nt <= 1 && !vi.li->plist && !dataFromStdin) {
    /* Check for piece list file and read it */
    iiPlistLoad(plistfname, vi.li, vi.hdr->nx, vi.hdr->ny, vi.hdr->nz);

    /* Or use the -P specification */
    if (!vi.li->plist && nframex > 0 && nframey > 0)
      mrc_plist_create(vi.li, vi.hdr->nx, vi.hdr->ny, vi.hdr->nz,
                       nframex, nframey, overx, overy);

    /* Or, check for piece coordinates in image header */
    if (!vi.li->plist && !frames && vi.image->file == IIFILE_MRC) {
      ivwReopen(vi.image);
      iiLoadPCoord(vi.image, useMdoc, vi.li, vi.hdr->nx, vi.hdr->ny,
                   vi.hdr->nz);
      iiClose(vi.image);
    }
          
    // If an overlap was entered and there were piece coordinates, adjust the overlap
    if (vi.li->plist && !nframex && !nframey && overEntered) {
      if (checkPieceList(vi.li->pcoords, 3, vi.li->plist, 1, vi.hdr->nx, &minxpiece, 
                         &numXpieces, &xoverlap) || 
          checkPieceList(vi.li->pcoords + 1, 3, vi.li->plist, 1, vi.hdr->ny, &minypiece,
                         &numYpieces, &yoverlap)) {
        imodError(NULL, "3dmod: Piece coordinates are not on a regular grid so overlap"
                  " cannot be adjusted.\n");
        exit (12);
      }
      if (numXpieces > 1)
        adjustPieceOverlap(vi.li->pcoords, 3, vi.li->plist, vi.hdr->nx, minxpiece,
                           xoverlap, overx);
      if (numYpieces > 1)
        adjustPieceOverlap(vi.li->pcoords + 1, 3, vi.li->plist, vi.hdr->ny, minypiece,
                           yoverlap, overy);
      vi.li->px = (vi.hdr->nx - overx) * (numXpieces - 1) + vi.hdr->nx;
      vi.li->py = (vi.hdr->ny - overy) * (numYpieces - 1) + vi.hdr->ny;
    }

    /* DNM 1/2/04: move adjusting of loading coordinates to fix_li call,
       and move that call into list processing */
    /* Only need to say it is not flippable unless cache full */
    if (vi.li->plist) 
      vi.flippable = 0;
  }

  /* set the model filename, or set to get a new model with empty name */
  if (Model || newModelCreated) {
    setImod_filename(LATIN1(curdir->cleanPath(QString(argv[argc - 1]))));
  } else {
    Imod_filename[0] = 0x00;
    newModelCreated = true;
  }

  // Read tilt angles if any
  if (anglefname)
    ivwReadAngleFile(&vi, anglefname);

  /*********************/
  /* Open Main Window. */
  imod_info_open(); 
  if (Imod_debug)
    imodPuts("info opened");

  if (Model && imodDebug('C'))
    wprint("main set checksum %d\n", Model->csum);

  /* report window before loading data */
  if (print_wid) {
    unsigned int winID = (unsigned int)ImodInfoWin->winId();
    imodPrintStderr("Window id = %u\n", winID);
    if (Imod_debug)
      wprint("Window id = %u\n", winID);
  }

  /* Get the clipboard messaging object on heap (doesn't work on stack!) */
  if (print_wid || useStdin)
    ClipHandler = new ImodClipboard(useStdin);
  App->listening = (print_wid ? 1 : 0) + (useStdin ? 2 : 0);

  /********************************************/
  /* Load in image data, set up image buffer. */

  /* If the user did not limit cache and specified Fill cache, then restore
     the flag to keep cache full */
  if (fillCache && vi.vmSize == hugeCache)
    vi.keepCacheFull = 1;

  /* Finish setting up and loading images */
  errno = 0;
  QTime loadTime;
  loadTime.start();
  if (ivwLoadImage(&vi)){
    qname = b3dGetError();
    qname += "3dmod: Fatal Error -- while reading image data.\n";
    if (errno) 
      qname += QString("System error: ") + QString(QString(strerror(errno)));
    imodError(NULL, LATIN1(qname));
    exit(3);
  }

  // Now we can set to middle Z
  if (ImodPrefs->startAtMidZ())
    vi.zmouse = (int)(vi.zsize / 2);

  if (Imod_debug)
    imodPrintStderr("Loading time %.3f\n", loadTime.elapsed() / 1000.);

  /* 11/13/06: remove setting of time flag in new model */

  /* Fill cache if user specified it - loader already filled if keepCacheFull*/
  if (fillCache && vi.vmSize)
    imodCacheFill(&vi);

  if (Imod_debug) 
    imodPuts("Read image data OK.");

  /* DNM 1/1/04: eliminate filename output, it is all over the place */
  if (vi.fakeImage)
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
  /* Open up default Image Windows. */
  if (xyzwinopen)
    xxyz_open(&vi);
  if (sliceropen)
    sslice_open(&vi);
  if (modelViewOpen) {
    imodv_open();
    imodvOpenSelectedWindows(windowKeys);
  }
  if (zapOpen || !(xyzwinopen || sliceropen || modelViewOpen))
    imod_zap_open(&vi, 0); 
  if (Imod_debug)  
    imodPuts("initial windows opened");
  if (App->rgba)
    imod_info_setbw(App->cvi->black, App->cvi->white);

  /* Open up requested dialog windows */
  ImodInfoWin->openSelectedWindows(windowKeys, modelViewOpen);
    
  /* Start main application input loop. */
  if (Imod_debug)
    imodPuts("mainloop");
  imodPlugCall(&vi, 0, IMOD_REASON_STARTUP);

  nx = ImodPrefs->autoConAtStart();
  if (!vi.fakeImage && !vi.rgbStore && 
      (nx > 1 || (nx && (newModelCreated || (Model->flags & IMODF_NEW_TO_3DMOD)))))
    ImodInfoWin->setupAutoContrast();

  loopStarted = 1;
#ifdef Q_OS_MACX
  inputRaiseWindows();
#endif
  return qapp.exec();
}

// Provide information about whether event loop started yet
int imodLoopStarted()
{
  return loopStarted;
}

/* Close everything as gracefully as possible */
void imod_exit(int retcode)
{

  // For Windows, get the clip handler started on disconnecting, then do other
  // stuff, then wait until it gets the disconnect
  if (ClipHandler)
    ClipHandler->startDisconnect();
  if (ImodHelp)
    delete ImodHelp;
  if (ImodPrefs)                     // Tell prefs to get zap sizes
    ImodPrefs->recordZapGeometry();
  zapReportBiggestMultiZ();

  App->closing = 1;
  imodv_close();                     // Imodv and associated dialogs
  ivwControlListDelete(App->cvi);    // Image windows
  imodDialogManager.close();         // Remaining imod dialog windows
  if (ImodPrefs)                     // Now save settings after windows get to 
    ImodPrefs->saveSettings(0);       // specify settings
  if (ClipHandler)
    ClipHandler->waitForDisconnect();
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

  if (imodDebug('T') || (windowKeys && strchr(windowKeys, '2')) ||
      !imod_model_changed(Model)) {
    imod_cleanup_autosave();
    imod_exit(0);
    return;
  }

  done = dia_choice("Save model before quitting?", "Yes", "No", "Cancel");

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

/* Returns true if debugging keys include the given character */
bool imodDebug(char key)
{
  return (Imod_debug && debugKeys && (strchr(debugKeys, key) != NULL));
}
