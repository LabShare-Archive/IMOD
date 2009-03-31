/*
 *  imod.c -- Main 3dmod program; Display MRC Images and build Models.
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

#include <limits.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <errno.h>
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
#include "imod_workprocs.h"
#include "imodv.h"
#include "imodv_menu.h"
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
#include "imod_assistant.h"
#include "iirawimage.h"
#include "b3dicon.xpm"

extern "C" int iiQImageCheck(ImodImageFile *inFile);

/******************************* Globals *************************************/
ImodApp *App;
Imod    *Model;

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
  qstr += "Options: -xyz  Open xyz window first.\n";
  qstr += "         -S    Open slicer window first.\n";
  qstr += "         -V    Open model view window first.\n";
  qstr += "         -Z    Open Zap window (use with -S, -xyz, or -V).\n";
  qstr += "         -O    Set options with startup dialog box.\n";
  qstr += "         -x min,max  Load in sub image.\n";
  qstr += "         -y min,max  Load in sub image.\n";
  qstr += "         -z min,max  Load in sub image.\n";
  qstr += "         -s min,max  Scale input to range [min,max].\n";
  qstr += "         -C #  Set # of sections or Mbytes to cache (#M or #m for"
    " Mbytes).\n";
  qstr += "         -F    Fill cache right after starting program.\n";
  qstr += "         -Y    Flip volume to model planes normal to Y axis.\n";
  qstr += "         -B #  Bin images by # in X, Y, and Z.\n";
  qstr += "         -b nxy,nz  Bin images by nxy in X and Y, by nz in Z (nz "
    "default=1).\n";
  qstr += "         -a <file name>  Load tilt angles from file.\n";
  qstr += "         -p <file name>  Load piece list file.\n";
  qstr += "         -P nx,ny  Display images as montage in nx by ny array.\n";
  qstr += "         -o nx,ny  Set X and X overlaps for montage display.\n";
  qstr += "         -f    Load as frames even if image file has piece "
    "coordinates.\n";
  qstr += "         -r nx,ny,nz  Dimensions for raw image files.\n";
  qstr += "         -t #  Mode for raw files: 0=byte, 1=int; 2=float, "
    "4=complex, 16=RGB.\n";
  qstr += "         -H #  Header size in bytes in raw image files.\n";
  qstr += "         -w    Swap bytes from raw image files.\n";
  qstr += "         -R    Read image data from standard input, not from file."
    "\n";
  qstr += "         -m    Load model with model coords (override scaling).\n";
  qstr += "         -2    Treat model as 2D only.\n";
  qstr += "         -T    Display multiple single-image files as times not "
    "sections.\n";
  qstr += "         -G    Display RGB-mode MRC file in gray-scale.\n";
  qstr += "         -M    Do not mirror FFT data around Y axis.\n";
  qstr += "         -ci   Display images in color index mode with colormap.\n";
  qstr += "         -cm <file name>  Load custom false color table.\n";
  qstr += "         -E <keys>  Open windows specified by key letters.\n";
  qstr += "         -h    Print this help message.\n";
  imodPrintInfo(LATIN1(qstr));
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
  char *anglefname = NULL;
  int xyzwinopen   = FALSE;
  int sliceropen   = FALSE;
  int zapOpen      = FALSE;
  int modelViewOpen= FALSE;
  int print_wid    = FALSE;
  int fillCache    = FALSE;
  int new_model_created = FALSE;
  int i      = 0;
  int cmap;
  int nx, ny, nz, mode;
  int namelen;
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
  Iobj *obj;
  QString qname;
  int doFork = 1;
  char *cmdLineStyle = NULL;
  int doImodv = 0;
  int rawSet = 0;
  int overEntered = 0;
  bool useStdin = false;
  bool dataFromStdin = false;
  int argScan;
  int nChars;
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
  /* Cancel forking on debug or -W output, or -L or -R */
  for (i = 1; i < argc; i++){
    if (!strncmp("-D", argv[i], 2)) {
      Imod_debug = TRUE;
      debugKeys = strdup(argv[i] + 2);
      doFork = 0;
    }
    if (!strcmp("-W", argv[i]) || !strcmp("-R", argv[i]))
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

  ImodPrefs = new ImodPreferences(cmdLineStyle);
  ImodHelp = new ImodAssistant("html/3dmodHelp", "3dmod.adp", "3dmod");

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

  /* DNM: Find out how many imods this user is running.  Used to be used for
   SGI colormaps.  Could be used to offset Info window.  WEXITSTATUS needed
  on Linux/SGI, not needed on Mac */
  /*
  cmap = system ("exit `\\ps -a | grep '[3 ][di]mod$' | wc -l`");
  cmap = WEXITSTATUS(cmap);
  printf("Returned cmap = %d\n", cmap); 
  */

  /*******************/
  /* Loop once or twice on arguments; initialize Data each time */

  for (argScan = 0; argScan <= doStartup; argScan++) { 
    mrc_init_li(&li, NULL);
    ivwInit(&vi, false);
    vi.li = &li;
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
          iiRawSetScale(li.smin, li.smax);
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
          print_wid = TRUE;
          break;
        
        case 'r':
          sscanf(argv[++i], "%d,%d,%d", &nx, &ny, &nz);
          iiRawSetSize(nx, ny, nz);
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
  
  /* Initialize the display system */
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
  Model = NULL;

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
      
      /* But if there are other files, open new model with that name*/
      imodPrintStderr("Model file (%s) not found: opening "
        "new model by that name.\n", argv[argc - 1]);

      Model = imodNew();
      lastimage = argc - 2;
      new_model_created = TRUE;
    } else {
      
    /*
    * Try loading file as a model.  Turn it on if successful
      */
      Model = (struct Mod_Model *)LoadModel(mfin);
      if (Model){
        if (Imod_debug)
          imodPrintStderr("Loaded model %s\n", argv[argc -1]);
        lastimage = argc - 2;
        Model->drawmode = 1;
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

    /* If we have a model and no image files before that, then it's a fake
       image */
    vi.fakeImage = 1;
    Imod_imagefile = NULL;
    vi.nt = Model->tmax = imodGetMaxTime(Model);
    ivwCheckWildFlag(Model);
    
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
  if (!vi.fakeImage && vi.nt <= 1) {
    li.smin = vi.image->smin;
    li.smax = vi.image->smax;
  }

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
      iiReopen(vi.image);
      iiLoadPCoord(vi.image, vi.li, vi.hdr->nx, vi.hdr->ny, vi.hdr->nz);
      iiClose(vi.image);
    }
          
    /* DNM 1/2/04: move adjusting of loading coordinates to fix_li call,
       and move that call into list processing */
    /* Only need to say it is not flippable unless cache full */
    if (vi.li->plist) 
      vi.flippable = 0;
  }

  /* set the model filename, or get a new model with null name */
  if (Model) {
    setImod_filename(LATIN1(curdir->cleanPath(QString(argv[argc - 1]))));
  } else {
    Model = imodNew();
    Imod_filename[0] = 0x00;
    new_model_created = TRUE;
  }

  /* If new model created, initialize views and make first object */
  if (new_model_created)
    initNewModel(Model);

  Model->mousemode = IMOD_MMOVIE;
  vi.imod = Model;

  /* DNM: set this now in case image load is interrupted */
  Model->csum = imodChecksum(Model);

  // Read tilt angles if any
  if (anglefname)
    ivwReadAngleFile(&vi, anglefname);

  /*********************/
  /* Open Main Window. */
  imod_info_open(); 

  if (Imod_debug)
    imodPuts("info opened");
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
  if (xyzwinopen && !vi.rawImageStore)
    xxyz_open(&vi);
  if (sliceropen && !vi.rawImageStore)
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
  ImodInfoWin->openSelectedWindows(windowKeys);
    
  /* Start main application input loop. */
  if (Imod_debug)
    imodPuts("mainloop");
  imodPlugCall(&vi, 0, IMOD_REASON_STARTUP);

  nx = ImodPrefs->autoConAtStart();
  if (!vi.fakeImage && !vi.rawImageStore && 
      (nx > 1 || (nx && new_model_created)))
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
  if (ImodPrefs)                     // Tell prefs to get zap sizes
    ImodPrefs->recordZapGeometry();
  zapReportBiggestMultiZ();

  App->closing = 1;
  imodv_close();                     // Imodv and associated dialogs
  ivwControlListDelete(App->cvi);    // Image windows
  imodDialogManager.close();         // Remaining imod dialog windows
  if (ImodPrefs)                     // Now save settings after windows get to 
    ImodPrefs->saveSettings(0);       // specify settings
  if (ImodHelp)
    delete ImodHelp;
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

/* Returns true if debugging keys include the given character */
bool imodDebug(char key)
{
  return (Imod_debug && debugKeys && (strchr(debugKeys, key) != NULL));
}

/*

$Log$
Revision 4.74  2009/03/30 18:26:03  mast
Raise all windows after starting on the Mac

Revision 4.73  2009/03/22 21:37:26  mast
Set flag when start to close windows for Mac

Revision 4.72  2009/03/22 03:17:48  mast
Just like in imodv, only create clip handler if needed

Revision 4.71  2009/02/27 23:46:55  mast
Set listening flag if started with -W or -L

Revision 4.70  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.69  2008/12/04 06:50:58  mast
Turn model on when loading

Revision 4.68  2008/12/01 15:42:01  mast
Changes for undo/redo and selection in 3dmodv standalone

Revision 4.67  2008/05/27 05:38:30  mast
Added angle file option, middle section and autocontrasting at startup

Revision 4.66  2008/04/02 04:39:40  mast
Fixed test for image files entered with -R

Revision 4.65  2008/04/02 04:23:36  mast
Changes for reading from standard input

Revision 4.64  2008/03/06 00:01:18  mast
Made windowKeys static and used a key to suppress model save query

Revision 4.63  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 4.62  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 4.61  2007/12/04 22:03:03  mast
Chnages for rearrangement of utilities

Revision 4.60  2006/09/21 22:26:26  mast
Go to raw check first if any raw options are entered

Revision 4.59  2006/09/03 21:29:22  mast
Moved check function adding to after plugin loading

Revision 4.58  2006/08/28 05:17:25  mast
Added option to read in custom color map

Revision 4.57  2006/08/16 23:48:15  mast
Added loading time debug output

Revision 4.56  2006/07/03 19:51:51  mast
Request a disconnect of the message handler on exit

Revision 4.55  2006/06/20 17:27:26  mast
Changed test for using -L - threads required on Windows only

Revision 4.54  2006/06/19 05:29:14  mast
Added -L option to use stdin for messages; delete clipboard object on exit

Revision 4.53  2006/01/14 18:15:18  mast
Call new function for model initialization

Revision 4.52  2005/10/14 22:02:50  mast
Call new function to set Imod_filename

Revision 4.51  2004/12/24 02:18:52  mast
Removed absolute argument from call to show help page

Revision 4.50  2004/12/06 04:39:54  mast
Changed call to ImodAssistant constructor

Revision 4.49  2004/12/04 02:10:31  mast
Moved declaration of ImodHelp into here

Revision 4.48  2004/12/02 21:42:23  mast
Changes for raw image loading

Revision 4.47  2004/11/30 03:39:42  mast
Added call to allow reading via QImage

Revision 4.46  2004/11/24 18:29:38  mast
Start the assistant with an adp file to get customized appearance

Revision 4.45  2004/11/22 00:23:22  mast
Added creation and convenience call to new help system

Revision 4.44  2004/11/21 05:58:29  mast
Added call to open imodv windows, and -DT to throw away model

Revision 4.43  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 4.42  2004/11/04 23:52:19  mast
Undoing erroreous checking of test version


Revision 4.40  2004/11/04 17:01:31  mast
Changes for loading FFTs with internal mirroring

Revision 4.39  2004/11/02 20:15:54  mast
Initialized color indices for named colors here

Revision 4.38  2004/07/07 19:25:29  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 4.37  2004/06/23 03:34:54  mast
Change in exiting sequence to allow generic settings to be saved

Revision 4.36  2004/06/05 00:10:24  mast
Prevented accessing vi.image with no image loaded - crashed on FC1

Revision 4.35  2004/05/31 23:27:19  mast
Added functions for printing to standard error, with flush on windows

Revision 4.34  2004/05/28 23:30:13  mast
Add function to report in whether event loop has started

Revision 4.33  2004/03/25 21:06:00  mast
Prevented accessing vi.hdr when no image was loaded

Revision 4.32  2004/01/09 15:54:12  mast
Turn keepcachefull back on if user enter -F and doesn't limit cache

Revision 4.31  2004/01/07 01:53:59  mast
Needed to reopen image file to read piece list from it

Revision 4.30  2004/01/06 16:55:01  mast
Added new option to open startup page, passing options to it

Revision 4.29  2004/01/05 17:21:09  mast
Added binning option and did major cleanup of image file loading

Revision 4.28  2003/12/30 06:36:44  mast
Add option for multifile Z display

Revision 4.27  2003/11/24 16:47:39  mast
needed to cast argument to printInfo to char *

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
