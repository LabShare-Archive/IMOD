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

    $Log$
*/

#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/wait.h>

#include "imod.h" 
#include "imodv.h"

/******************************* Globals *************************************/
ImodApp *App;
Imod    *Model;

struct ViewInfo *XYZ_vi  = NULL;
struct ViewInfo *Tilt_vi = NULL;
struct ViewInfo *Imod_vi = NULL;

struct MRCheader Imod_hdata;
struct TiltInfo Tilts;

char   Tomwind[128];
char   Tltwind[128];   /* Title of Tilt Window.                          */

char   *Imod_imagefile;
FILE   *Imod_Imagefp;
char   *Imod_IFDpath;
char   *Imod_cwdpath;

int    Stereo;          /* Flag to tell if tilt window is in stereo mode. */
int    Modeltouch = FALSE;
int    Ghostmode = FALSE;
int    Imod_debug = FALSE;
int    ImodTrans  = TRUE;
int    Rampbase = RAMPBASE;

extern Widget Imod_widget_float;

/*****************************************************************************/

/* Exit Functions */
void imod_quit(void);
int  SaveModelQuit(Imod *mod);
void imod_cleanup_backup(void);

void imod_usage(char *name)
{
     imodVersion(name);
     imodCopyright();
     printf("%s: Usage, %s [options] <Image files> <model file>\n",
	    name, name);
     printf("Options: -c #  Set # of colormap to use (1-12).\n");
     printf("         -rgb  Display images in 24-bit color, not with colormap.\n");
     printf("         -C #  Set # of sections or Mbytes to cache (#M or #m for Mbytes).\n");
     printf("         -xyz  Open xyz window first.\n");
     printf("         -S    Open slicer window first.\n");
     printf("         -x min,max  Load in sub image.\n");
     printf("         -y min,max  Load in sub image.\n");
     printf("         -z min,max  Load in sub image.\n");
     printf("         -s min,max  Scale input to range [min,max].\n");
     printf("         -Y  Model planes normal to y axis.\n");
     printf("         -Z  Model planes normal to z axis. (Default)\n");
     printf("         -p <file name>  Load piece list file.\n");
     printf("         -P nx,ny  Display images as montage in nx by ny array.\n");
     printf("         -o nx,ny  Set x and y overlaps for montage display.\n");
     printf("         -f  Load as frames even if image file has piece coordinates.\n");
     printf("         -m  Load model with model coords (override scaling).\n");
     printf("         -2  Treat model as 2D only.\n");
     printf("         -G  Display RGB-mode MRC file in gray-scale.\n");
     printf("         -h  Print this help message.\n");
     printf("\n");
     return;
}


int main( int argc, char *argv[])
{
     ImodApp app;
     struct ViewInfo vi;
     struct ViewInfo tiltvi;
     struct LoadInfo li;
     FILE *fin        = NULL;
     FILE *mfin       = NULL;
     char *plistfname = NULL;
     int xyzwinopen   = FALSE;
     int sliceropen   = FALSE;
     int loadinfo     = FALSE;
     int new_model_created = FALSE;
     int i      = 0;
     int vers;
     int flipit = 0;
     int cpid;
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
     Iobj *obj;

     /* Initialize data. */
     App = &app;
     App->rgba = 0;

     /*DNM: prescan for debug and rgb flags before the display_init */
     for (i = 1; i < argc; i++){
	  if (argv[i][0] == '-' && argv[i][1] == 'D')
	       Imod_debug = TRUE;
	  if (argv[i][0] == '-' && argv[i][1] == 'r' && argv[i][2] == 'g'
	      && argv[i][3] == 'b')
	       App->rgba = 1;
     }

/* Run the program as imodv? */
     i = strlen(argv[0]);
     if (argv[0][i-1] == 'v'){
	  imodv_main(argc, argv);
	  exit(0);
     }

     if (argc > 1){
	  i = strcmp("-imodv", argv[1]);
	  if (i) i = strcmp("-view", argv[1]);
	  if (!i){
	       argc--; argv++; argv[0]++;
	       imodv_main(argc, argv);
	       exit(0);
	  }
     }

     /* if no input files, print help stuff */
     if (argc < 2){
	  imod_usage(argv[0]);
	  exit(1);
     }

     imod_display_init(App, argv, &argc);
     mrc_init_li(&li, NULL);
     vi.li = &li;

     /*******************/
     /* Initialize Data */
     XYZ_vi  = App->cvi = &vi;
     Tilt_vi = &tiltvi;
     ivwInit(&vi);
#ifndef USEIMODI
     vi.hdr = &Imod_hdata;
#endif
     vi.fp = fin;
     vi.zap = NULL;
     vi.vmSize = cacheSize;
     vi.flippable = 1;

#ifdef __sgi
     /* DNM: Find out how many imods this user is running and set the cmap to
	that number.  Also change the interval from 300 to 330 here and in
	imod_menu.c */
     cmap = system ("exit `\\ps -a | grep imod | wc -l`");
     cmap = WEXITSTATUS(cmap);
     /*     printf("Returned cmap = %d\n", cmap); */
     if (cmap <= 0)
          cmap = 1;
     if (cmap > 12)
          cmap = 12;
     Rampbase  = 256 + ((cmap - 1) * 330);
#endif

     App->base = Rampbase;

     /* handle input options. */
     for (i = 1; i < argc; i++){
	  if (argv[i][0] == '-'){
	       if (firstfile) {
		    fprintf(stderr, "Imod: invalid to have argument %s after"
			    " first filename\n", argv[i]);
		    exit(1);
	       }
	       switch (argv[i][1]){

		  case 'c':
		    cmap = atoi(argv[++i]);
		    if ((cmap > 12) || (cmap < 1)){
			 fprintf(stderr, "imod: valid -c range is 1 - 12\n");
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
		    flipit = TRUE;
		    li.axis = 2;
		    break;

		  case 'Z':
		    li.axis = 3;
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

		  default:
		    break;

	       }
	  } else if (!firstfile)
	       firstfile = i;
     }

     /* Load in all the imod plugins that we can use.*/
     imodPlugInit();

     Model = NULL;
     mfin = NULL;

     /* Try to open the last file if there is one */
     if (firstfile) {
	  mfin = fopen(argv[argc - 1], "r");
	  if (mfin == NULL) {

	       /* Fail to open, and it is the only filename, then exit */
	       if (firstfile == argc - 1) {
		    printf("Couldn't open input file %s.\n", argv[argc - 1]);
		    exit(10);
	       }

	       /* But if there are other files, open new model with that name*/
	       fprintf(stderr, "Model file (%s) not found: opening "
		       "new model by that name.\n", argv[argc - 1]);
	       /* This creates a new model in Model */
	       imod_open(NULL);
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
	       Imod_imagefile = dia_filename
		    ("IMOD: Enter Image file to LOAD.");
	  
	       if (Imod_imagefile == NULL){
		    fprintf(stderr, "IMOD: file not selected\n");
		    exit(-1);
	       }
	  } else {
	       /* Or, just set the image file name */
	       Imod_imagefile = argv[firstfile];
	  }
	       
	  if (Imod_debug){
	      fprintf(stderr, "Loading %s\n", Imod_imagefile);
	  }

	  vi.fp = fin = fopen(Imod_imagefile, "r");
	  if (fin == NULL){
	       printf("Couldn't open input file %s.\n", Imod_imagefile);
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

	       vi.image = iiOpen(Imod_imagefile, "r");
	       if (!vi.image){
		    fprintf(stderr, "imod error: "
			    "Failed to load input file %s\n",
			    Imod_imagefile);
		    if (errno) perror("image open");
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
		    vi.hdr = ilistItem((Ilist *)vi.imageList, 0);
	       }
	  }
     } else {

	  /* Multiple image files, set ifd -2 */
	  ivwMultipleFiles(&vi, argv, firstfile, lastimage);
	  vi.ifd = -2;
     }
	     
     /* set the model filename, or get a new model with null name */
     if (Model) {
	  sprintf(Imod_filename, "%s", argv[argc - 1]);
     } else {
	       
	  /* This creates a new model in Model */
	  imod_open(NULL);
	  Imod_filename[0] = 0x00;
	  new_model_created = TRUE;
     }

     Model->mousemode = IMOD_MMOVIE;
     vi.imod = Model;

     /* DNM 5/16/02: if multiple image files, set time flag by default */
     if (new_model_created) {
	  obj = imodObjectGet(vi.imod);
	  if (vi.nt)
	       obj->flags |= IMOD_OBJFLAG_TIME;
     }

     /* DNM: set this now in case image load is interrupted */
     Model->csum = imodChecksum(Model);

     /*********************/
     /* Open Main Window. */
     imod_info_open(argc, argv); 
     if (vi.fakeImage)
          XtSetSensitive(Imod_widget_float, False);
     if (Imod_debug)
	  puts("info opened");
     imod_color_init(App);
     imod_set_mmode(IMOD_MMOVIE);

     /* Copy filename into model structure */
     namelen = strlen(Imod_filename)+1;
     Model->fileName = malloc(namelen);
     if (Model->fileName)
	  memcpy(Model->fileName, Imod_filename, namelen);


#ifndef NO_IMOD_FORK
     /* put imod in background if not debug. */
     if (!Imod_debug)
	  if ((cpid = fork()) != 0)
	       exit(0);
#endif


     /********************************************/
     /* Load in image data, set up image buffer. */
     /* change the current directory in case there's an IFD file */
     Imod_IFDpath = NULL;
#ifndef __vms
     if (!vi.fakeImage && vi.ifd > 0) {
	  Imod_cwdpath = getcwd(NULL, -1);

	  pathlen = strlen(Imod_imagefile);
	  while (( pathlen > 0) && 
		 (Imod_imagefile[pathlen-1] != '/'))
	       pathlen--;
	 
	  if (pathlen > 0){
	       Imod_IFDpath = strdup(Imod_imagefile);
	       Imod_IFDpath[pathlen] = 0x00;
	       chdir(Imod_IFDpath);
	       /*  printf("chdir %s\n", Imod_IFDpath); */
	  }
     }
#endif

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
		   li.xmin -= li.opx;
	      if (li.xmax != -1)
		   li.xmax -= li.opx;
	      if (li.ymin != -1)
		   li.ymin -= li.opy;
	      if (li.ymax != -1)
		   li.ymax -= li.opy;
	      if (li.zmin != -1)
		   li.zmin -= li.opz;
	      if (li.zmax != -1)
		   li.zmax -= li.opz;
	      /* nip the -Y flag in the bud to avoid misunderstanding */
	      li.axis = 3;
	      vi.flippable = 0;
	      /* need to fix the coordinates now if not standard MRC */
	      if (vi.ifd < 0)
		   mrc_fix_li(&li, li.px, li.py, li.pz);
	 }

     }

     /* Finish loading/setting up images, reading IFD if necessary */
     if (ivwLoadImage(&vi)){
	 fprintf(stderr, "imod: Fatal Error --" 
		 " while reading image data.\n");
	 perror("imod LoadImage");
	 exit(-1);
     }

     if (Imod_IFDpath)
	     chdir(Imod_cwdpath);

     if (Imod_debug) puts("Read image data OK.");
     if (Imod_imagefile)
	 wprint("\nImage %s\n", Imod_imagefile);
     else
	 wprint("\nNo image loaded.\n");



     /*************************************/
     /* add all work procs and time outs. (DNM: no more for imodv) */

     /* imodv_add_anim(); */
     imod_start_autosave();

     /* Satisfy the lawyers. */
     wprint("Imod %s Copyright %s\n"
	    "BL3DFS & Regents of the Univ. of Colo.\n", 
	    VERSION_NAME, COPYRIGHT_YEARS);
     imod_draw_window();
     xcramp_setlevels(App->cvi->cramp,App->cvi->black,App->cvi->white);

     /*********************************/
     /* Open up default Image Window. */
     if (xyzwinopen && !vi.rawImageStore)
	  xxyz_open(&vi);
     else if (sliceropen && !vi.rawImageStore)
	  sslice_open(&vi);
     else
	  imod_zap_open(&vi); 
     if (Imod_debug)  puts("zap or xyz opened");
     if (App->rgba)
	  imod_info_setbw(App->cvi->black, App->cvi->white);
     /* Start main application input loop. */
     if (Imod_debug) puts("mainloop");
     imodPlugCall(&vi, 0, IMOD_REASON_STARTUP);
     dia_mainloop();
     return 0;
}


void imod_exit(int retcode)
{
     XtDestroyWidget(App->toplevel);
     exit(retcode);
}

/* DNM 2/7/02: keep it from sending up another window if one is already up */
void imod_quit(void)
{
     static int quitting = 0;
     int done;

     if (quitting)
	  return;

     if (!imod_model_changed(Model)){
          imod_cleanup_autosave();
	  imod_exit(0);
     }

     quitting = 1;
     done = dia_choice("Save model before quitting?",
			    "Yes", "No", "Cancel");
     quitting = 0;

     switch(done){
	case 1:
	  
	  if (SaveModelQuit(Model)){
	       wprint("Imod Error: Model not saved.\n");
	       break;
	  }else{
	       imod_cleanup_autosave();
	       imod_exit(0);
	  }
	  break;

	case 2:
	  imod_cleanup_autosave();
	  /* DNM: It used to make one last autosave and quit, but if the user
	     says NO, then just clean up and quit! */
	  imod_exit(0);
	  break;

	case 3:
	  break;

	default:
	  break;
     }
     return;
}

/* Appends either image or model name to window name */
char *imodwEithername(char *intro, char *filein)
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
     return (imodwEithername(intro, filename));
}



/***********************************************************************
 * Core application plugin lookup functions.
 *
 */

Display      *imodDisplay(void){ return(App->display); }
XtAppContext  imodAppContext(void){ return(App->context); }

Widget        imodTopLevel(void){ return(App->toplevel); }
Visual       *imodVisual(void){ return(App->visual); }
XVisualInfo  *imodVisualInfo(void){ return(App->visualinfo); }
Colormap      imodColormap(void){ return(App->cmap); }
int           imodDepth(void){ return(App->depth); }

     

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
