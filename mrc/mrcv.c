/*  IMOD VERSION 2.30
 *
 *  mrcv.c -- Display MRC files on a SGI using Iris GL routines.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  This program soon is obsolete.
 */

/*****************************************************************************
 *   Copyright (C) 1994-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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
    Revision 3.3  2004/07/07 19:25:30  mast
    Changed exit(-1) to exit(3) for Cygwin

    Revision 3.2  2003/10/24 02:28:42  mast
    strip directory from program name and/or use routine to make backup file

    Revision 3.1  2002/11/05 23:55:30  mast
    Changed to call imodCopyright

*/

/* System include files */
#include <stdio.h> 
#ifndef __sgi
main(int argc, char **argv)
{
     fprintf(stderr, "%s only runs on sgi machines.\n", argv[0]);
}
#else

/* Include to old IrisGL libraries, these are NOT OpenGL
 * and cannot be ported to non SGI machines.
 * Use the program xmrcv to view mrc files on non SGI machines,
 * or better yet just use the IMOD program.
 */
#include <gl/gl.h>
#include <gl/device.h>
#include <limits.h>

/* MRC include files    */
#include "mrcfiles.h"
#include "mrcv.h"
#include "imodel.h"

struct Mod_Draw  Mrcv_md;
struct Mod_Model *Mrcv_model = NULL;
struct ViewInfo  *Mrcv_vi;
struct ViewInfo  *Mrcv1_vi;
struct ViewInfo  *Mrcv2_vi;

int Rampbase = RAMPBASE;
int Verbose  = TRUE;
int Mrcv_fullwin = FALSE;
int Mrcv_pointer = TRUE;
float Mrcv_vidscale = 1.0;
int   Mrcv_xoff = 0;
int   Mrcv_yoff = 0;
int   Mrcv_xmont = 1;
int   Mrcv_ymont = 1;
int   Mrcv_onside = 0;
int   Mrcv_stereo = FALSE;
int   Mrcv_plax   = 1;
int   Mrcv_border = 5;
int   Mrcv_background = 0;
long  Mrcv_menu;
int   Mrcv_szoom = 0;

int sphdraw(void){return 0;}

/* kill stereo hardware on exit. */
void killsthw(void);

main( int argc, char *argv[])
{

     Colorindex *data = NULL;
     Colorindex pixel;
     FILE *fin = NULL;
     FILE *mfin = NULL;
     unsigned char **idata;
     struct LoadInfo  li;
     struct ViewInfo vi, evi;
     struct RGBViewInfo cvi;
     struct MRCheader hdata;
     int winsize;
     int stdin_batch = 0;
     char command[128];

     short unsigned int sdata;
     float fdata;
     float fpixel;
     short int spixel;
     float conscale;
     int lowsection = -1, highsection = -1;
     int zmin, zmax, index;
     long fsize;
     int i, k = 0;
     int xysize;
     int filearg;
     
     /* option flags. */
     int resize = FALSE;
     int ntscwin = FALSE;
     int fullwin = FALSE;
     char *progname = imodProgName(argv[0]);

     if (argc == 3){
	  if ((argv[1][0] == '-') && (argv[1][1] == 'e')){
	       stdin_batch = TRUE;
	       sprintf(command, "%s %s %s", argv[0], argv[1], argv[2]);
	       system(command);
	       exit(0);
	  }
     }

     if (argc < 2){
	  fprintf(stderr, "mrcv version 1.11 %s %s\n", __DATE__,__TIME__);
	  imodCopyright();
	  fprintf(stderr, 
		  "%s: Usage, %s [-r] [-f] [-n] [-q] [-b] [-h]\n", 
		  progname, progname);
	  fprintf(stderr,"\t[-c #] [-x #,#] [-y #,#] [-z #,#]  filename\n");
	  fprintf(stderr, "%s -h for help\n", progname);
	  exit(1);
     }

      mrc_init_li(&li, NULL);
     Rampbase = 2048;

     /* Read options. */
     for (i = 1; i < argc ; i++){
	  if (argv[i][0] == '-'){
	       switch (argv[i][1]){
		    
		  case 'x':
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
		    
		  case 'c':
		    /* New colormap rampbase. */
		    if (argv[i][2] != 0x00)
			 sscanf(argv[i], "-b%d", &Rampbase);
		    else
			 sscanf(argv[++i], "%d", &Rampbase);
		    break;
		    
		  case 'v':
		    /* Be verbose */
		    Verbose = TRUE;
		    break;
		    
		  case 'r':
		    /* prompt user to resize data. */
		    resize = TRUE;
		    break;
		    
		  case 'm':
		    i++;
		    Mrcv_model = imodRead(argv[i]);
		    if (!Mrcv_model){
			 fprintf(stderr, "%s: error reading model file %s\n",
				 progname, argv[i]);
			 exit(3);
		    }
		    break;

		  case 'h':
		    printf("mrcv [-r] [-f] [-n] [-q] [-b] [-h] [-w]");
		    printf("[-c #] [-x #,#] [-y #,#] [-z #,#] filename.\n");
		    printf("Options: -r Request to resize data before viewing.\n");
		    printf("         [-c #] first entry in colormap\n");
		    printf("         [-f] Display full screen.\n");
		    printf("         [-n] Display ntsc size screen.\n");
		    printf("         [-q] Be quiet\n");
		    printf("         [-b] Display image on back/sideways.\n");
		    printf("         [-x #,#] Load in sub area x.\n");
		    printf("         [-y #,#] Load in sub area y.\n");
		    printf("         [-z #,#] Load in sub area z.\n");
		    printf("         [-m model file]  Load imod model file\n");
		    printf("         [-w] Set background to white.\n");
		    exit(1);
		    break;

		  case 'q':
		    Verbose = FALSE;
		    break;

		  case 'n':
		    /* set display to ntsc mode */
		    ntscwin = TRUE;
		    break;
		    
		  case 'f':
		    /* set display to full screen. */
		    fullwin = TRUE;
		    Mrcv_fullwin = TRUE;
		    break;
		    
		  case 's':
		    if (argv[i][2] != 0x00)
			 sscanf(argv[i], "-s%f", &Mrcv_vidscale);
		    else
			 sscanf(argv[++i], "%f", &Mrcv_vidscale);
		    break;

		  case 'o':
		    if (argv[i][2] != 0x00)
			 sscanf(argv[i], "-s%d", &Mrcv_xoff);
		    else
			 sscanf(argv[++i], "%d", &Mrcv_xoff);
		    sscanf(argv[++i], "%d", &Mrcv_yoff);
		    break;
		    
		  case 'b':
		    Mrcv_onside = TRUE;
		    break;

		  case 'w':
		    Mrcv_background = 255;
		    break;

		  case 'e':
		    stdin_batch = TRUE;
		    
		    break;

		  default:
		    break;
	       }
	  }else break;
     }

     if (Mrcv_model)
	  Mrcv_md.cmapbase = Rampbase + 256;

     /* open input file. */
     if ((argc - 1) < i)
	  exit(3);
     
     filearg = i;
     fin = fopen(argv[i], "rb");
     
     if (fin == NULL){
	  fprintf(stderr, "Couldn't read input file %s.\n", argv[i]);
	  exit(10);
     }

     if (Mrcv_fullwin)
	  ntscwin = FALSE;

     if (Verbose){
	  printf("MRC View, Version 1.0 %s \n", __DATE__);
	  imodCopyright();
     }

     Mrcv_vi = &vi;
     Mrcv1_vi = Mrcv_vi;
     Mrcv2_vi = &evi;

     /* Set colorramp */
     Mrcv1_vi->rampbase = Rampbase;
     Mrcv1_vi->rampsize = 256;
     Mrcv2_vi->rampbase = Mrcv1_vi->rampbase + Mrcv1_vi->rampsize + 1;
     Mrcv2_vi->rampsize = 256;


     if ( 1 == do_batch(fin, stdin_batch)){
	  if (Mrcv_vi->xsize){
	       if (Verbose)
		    printf("Interactive mode.\n");
	       show_image( Mrcv_vi );
	  }
	  gexit();
	  exit(1);
     }

     /* Read MRC header. */
     if (Verbose)
	  printf("Loading MRC file.\n");
     if (mrc_head_read(fin, &hdata))
	  {
	       fprintf(stderr, "Can't Read Input File Header.\n");
	       exit(3);
	  }

     if (hdata.mode == MRC_MODE_RGB)
	  {
	       fseek(fin, 1024, 0);
	       if (mrc_color_loadimage (fin, &cvi))
		    exit(1);
	       winsize = MRC_WINDOW_DATASIZE;
	       if (Mrcv_fullwin)
		    winsize = MRC_WINDOW_FULL;
	       if (ntscwin)
		    winsize = MRC_WINDOW_NTSC;
	       mrc_color_winopen(&cvi, winsize);
	       mrc_color_mainloop(&cvi);
	       
	       exit(1);
	  }

     vi.xsize = 0;
     if (resize){
	  printf("Default image size = ( %d,  %d, %d).\n", 
		 hdata.nx, hdata.ny, hdata.nz);
	  resize = get_loadinfo(&hdata, &li);
     }
     mrc_init_li(&li, &hdata);
     li.white = 255;
     li.black = 0;     
     li.contig = 0;
     if (Verbose)
	  idata = mrc_read_byte(fin, &hdata, &li, mrc_default_status);
     else
	  idata = mrc_read_byte(fin, &hdata, &li, NULL);


     vi.idata = idata;
     fsize = hdata.nx * hdata.ny;
     
     vi.viewdata = (Colorindex *)malloc(fsize * 2);
     if (!vi.viewdata){
	  fprintf(stderr, "Not Enough memory to load image data.\n");
	  exit(1);
     }
     
     for (i = 0; i < fsize; i++){
	  vi.viewdata[i] = vi.idata[0][i] + vi.rampbase;
     }
  
     vi.xsize = hdata.nx;
     vi.ysize = hdata.ny;
     vi.zsize = hdata.nz;
     vi.xysize = hdata.nx * hdata.ny;
     vi.zmouse = 0;
     vi.black = 0;
     vi.white = 255;

     if (ntscwin | fullwin){
	  /*       prefsize(NTSCX, NTSCY); */
	  if (ntscwin)
	       prefposition(0l, NTSCX, 0l, NTSCY);
	  if (fullwin)
	       prefposition(0l, getgdesc(GD_XPMAX), 0l, getgdesc(GD_YPMAX));
	  noborder();
	  winopen("");
	  minsize(vi.ysize /4 , vi.xsize / 4);
	  maxsize(getgdesc(GD_XPMAX), getgdesc(GD_YPMAX));
     }else{
	  if ( Mrcv_onside == TRUE)
	       minsize(vi.ysize, vi.xsize);
	  else
	       minsize(vi.xsize, vi.ysize);
	  maxsize(getgdesc(GD_XPMAX), getgdesc(GD_YPMAX));
	  winopen (argv[filearg]);
     }

     doublebuffer();
     gconfig();
     color(BLACK);
     clear();
     swapbuffers();
     gflush();
     qdevice(KEYBD);
     qdevice(LEFTMOUSE);
     qdevice(RIGHTMOUSE);
     qdevice(MIDDLEMOUSE);
     qdevice(LEFTARROWKEY);
     qdevice(RIGHTARROWKEY);
     qdevice(UPARROWKEY);
     qdevice(DOWNARROWKEY);
     qdevice(REDRAW);  
     qdevice(PAGEUPKEY);
     qdevice(PAGEDOWNKEY);
     qdevice(HOMEKEY);
     qdevice(ENDKEY);
     qdevice(ESCKEY);
     qdevice(F1KEY);
     qdevice(F2KEY);
     qdevice(F3KEY);
     qdevice(F4KEY);
     qdevice(F5KEY);
     qdevice(F6KEY);
     qdevice(F7KEY);
     qdevice(F8KEY);
     qdevice(F9KEY);
     qdevice(F10KEY);
     qdevice(F11KEY);
     qdevice(F12KEY);
     atexit(killsthw);
     rectzoom(vi.blowup, vi.blowup);
     cmap_init(vi.black, vi.white, vi.rampbase, 256);

     vi.llx = 0;
     vi.lly = 0;
     vi.urx = vi.xsize - 1;
     vi.ury = vi.ysize - 1;
     vi.zoom = 1.0;
     vi.blowup = 1.0;
     vi.xtrans = 0;
     vi.ytrans = 0;
     vi.frate  = 0;
     vi.movie = 0;

     mrcv_make_menu();
     show_image( &vi );

     gexit();
     return 0;
}


void show_image(struct ViewInfo *vi)
{
     int fun = 1;
     int i;
     
     /* Display first image of stack. */
     setview(vi);


     while(fun){
	  fun = getinput(vi);
	  if (!fun)
	       break;
	  if (vi->movie)
	       fun = xyz_movie_loop(vi, &(vi->zmouse), vi->zsize, 1);
     }
}
 

int getinput(struct ViewInfo *vi)
{
     int i;
     int xmouse, ymouse, lxm, lym;
     long ssize, stretch;
     short val = 0;
     Device dev;

     dev = qread(&val);
     switch (dev)
	  {
	       
	     case REDRAW:
	       reshapeviewport();
	       setview(vi);
	       break;
	       
	       
	     case LEFTMOUSE:
	       if(val){
		    
		    lxm = getvaluator(MOUSEX);
		    lym = getvaluator(MOUSEY);
		    
		    while (1){
			 while(qtest()){
			      dev = qread(&val);
			      if (dev == LEFTMOUSE)
				   return(1);
			 }
			 xmouse = getvaluator(MOUSEX);
			 ymouse = getvaluator(MOUSEY);
			 
			 if ( (xmouse == lxm) && (ymouse == lym)){
			      sginap(1);
			      continue;
			 }
			 
			 vi->xtrans += (xmouse - lxm);
			 vi->ytrans += (ymouse - lym);
			 if (vi->xtrans > vi->xsize)
			      vi->xtrans = vi->xsize;
			 if (vi->xtrans < -vi->xsize)
			      vi->xtrans = - vi->xsize;
			 if (vi->ytrans > vi->ysize)
			      vi->ytrans = vi->ysize;
			 if (vi->ytrans < -vi->ysize)
			      vi->ytrans = - vi->ysize;
			 lxm = xmouse;
			 lym = ymouse;
			 setview(vi);
		    }
	       }
	       break;
	       
	     case MIDDLEMOUSE:
	       if (val){
		    if (vi->zsize > 1)
			 xyz_movie_loop(vi, &(vi->zmouse), vi->zsize, 1);
	       }
	       break;
	       
	     case RIGHTMOUSE:
	       if (val){
		    if (val)
			 return(mrcv_do_menu(vi, dopup(Mrcv_menu)));
	       }
	       break;
	       
	       
	     case LEFTARROWKEY:
	       if (val){
		    vi->xtrans -= 5;
		    if (vi->xtrans > vi->xsize)
			 vi->xtrans = vi->xsize;
		    if (vi->xtrans < -vi->xsize)
			 vi->xtrans = - vi->xsize;
		    setview(vi);
	       }
	       break;
	     case RIGHTARROWKEY:
	       if (val){
		    vi->xtrans += 5;
		    if (vi->xtrans > vi->xsize)
			 vi->xtrans = vi->xsize;
		    if (vi->xtrans < -vi->xsize)
			 vi->xtrans = - vi->xsize;
		    setview(vi);
	       }
	       break;
	     case UPARROWKEY:
	       if (val){
		    vi->ytrans += 5;
		    if (vi->ytrans > vi->ysize)
			 vi->ytrans = vi->ysize;
		    if (vi->ytrans < -vi->ysize)
			 vi->ytrans = - vi->ysize;
		    setview(vi);
	       }
	       break;
	     case DOWNARROWKEY:
	       if (val){
		    vi->ytrans -= 5;
		    if (vi->ytrans > vi->ysize)
			 vi->ytrans = vi->ysize;
		    if (vi->ytrans < -vi->ysize)
			 vi->ytrans = - vi->ysize;
		    setview(vi);
	       }
	       break;
	       
	     case F1KEY:
	       if (val){
		    cmap_blacklevel(-1);
		    cmap_get_levels(&(vi->black), &(vi->white));
	       }
	       break;
	       
	     case F2KEY:
	       if (val){
		    cmap_blacklevel(1);
		    cmap_get_levels(&(vi->black), &(vi->white));
	       }
	       break;
	     case F3KEY:
	       if (val){
		    cmap_whitelevel(-1);
		    cmap_get_levels(&(vi->black), &(vi->white));
	       }
	       break;
	       
	     case F4KEY:
	       if (val){
		    cmap_whitelevel(1);
		    cmap_get_levels(&(vi->black), &(vi->white));
	       }
	       break;
	       
	     case F5KEY:
	       if (val){
		    cmap_contrast(-1);
		    cmap_get_levels(&(vi->black), &(vi->white));
	       }
	       break;
	       
	     case F6KEY:
	       if (val){
		    cmap_contrast(1);
		    cmap_get_levels(&(vi->black), &(vi->white));
	       }
	       break;
	       
	     case F7KEY:
	       if (val){
		    cmap_brightness(-1);
		    cmap_get_levels(&(vi->black), &(vi->white));
	       }
	       break;
	       
	     case F8KEY:
	       if (val){
		    cmap_brightness(1);
		    cmap_get_levels(&(vi->black), &(vi->white));
	       }
	       break;
	       
	     case F9KEY:
	       if (val){
		    cmap_index(0, &(vi->black), &(vi->white));
		    adjustcmap_pf( &(vi->black), &(vi->white), vi->rampbase);
	       }
	       break;
	       
	     case F10KEY:
	       if (val){
		    cmap_index(1, &(vi->black), &(vi->white));
		    adjustcmap_pf( &(vi->black), &(vi->white), vi->rampbase);
	       }
	       break;
	       
	     case F11KEY:
	       if (val){
		    cmap_toggle_reverse();
	       }
	       break;

	     case F12KEY:
	       if (val){
		    cmap_toggle_falsecolor();
	       }
	       break;
	       
	     case PAGEUPKEY:
	       if (val){
		    if (vi->zmouse < vi->zsize - 1)
			 vi->zmouse += 1;
		    setview(vi);
	       }
	       break;
	       
	     case PAGEDOWNKEY:
	       if (val){
		    if (vi->zmouse > 0)
			 vi->zmouse -= 1;
		    setview(vi);
	       }
	       break;
	       
	     case HOMEKEY:
	       if (val){
		    vi->zmouse = 0;
		    setview(vi);
	       }
	       break;
	       
	     case ENDKEY:
	       if (val){
		    vi->zmouse = vi->zsize - 1;
		    setview(vi);
	       }
	       break;
	       
	     case KEYBD:  
	       switch (val){
		    
		  case 'b':
		    if (Mrcv_onside)
			 Mrcv_onside = FALSE;
		    else
			 Mrcv_onside = TRUE;
		    setview(vi);
		    break;
		    
		  case '=':
		    /* Zoom In */
		    if (vi->zoom >= 1.0)
			 vi->zoom += 1.0;
		    else 
			 (vi->zoom = 1.0);
		    setview(vi);
		    break;
		    
		  case '+':
		    vi->zoom += 0.1;
		    setview(vi);
		    break;
		    
		  case '-':
		    /* Zoom Out */
		    vi->zoom -= 1.0;
		    if (vi->zoom < 1.0)
			 vi->zoom = 1.0;
		    setview(vi);
		    break;
		    
		  case '_':
		    vi->zoom -= 0.1;
		    if (vi->zoom < 1.0)
			 vi->zoom = 1.0;
		    setview(vi);
		    break;
		    
		  case '[':
		    /* Previous screen */
		    if (vi->zmouse > 0)
			 vi->zmouse -= 1;
		    setview(vi);
		    break;
		  case ']':
		    /* Next screen */
		    if (vi->zmouse < vi->zsize - 1)
			 vi->zmouse += 1;
		    setview(vi);
		    break;

		  case 'p':
		    cursoff();
		    
		  case 'P':
		    curson();
		    break;
		    
		  case 'd':
		    Mrcv_szoom = 5;
		    setview(vi);
		    Mrcv_szoom = 0;
		    break;
		    
		  case 's':
		    if (Mrcv_stereo)
			 Mrcv_stereo = FALSE;
		    else{
			 Mrcv_stereo = TRUE;
		    }
		    setview(vi);
		    break;

		  case '3':
		    sthw(vi);
		    setview(vi);
		    break;
		    
		  case 'r':
		    vi->frate = 1;
		    vi->xtrans = 0;
		    vi->ytrans = 0;
		    setview(vi);
		    rectzoom(vi->blowup, vi->blowup);
		    rectwrite(0, 0, vi->xsize-1, vi->ysize-1, vi->viewdata);
		    color(BLACK);
		    clear();
		    swapbuffers();
		    color(BLACK);
		    clear();
		    swapbuffers();
		    setview(vi);
		    break;

		  case 'o':
		    printf("\nx, y offset = %d, %d\n", vi->xtrans, vi->ytrans);
		    printf("Z-section = %d\n", vi->zmouse);
		    printf("Zoom = %g\n", vi->zoom);
		    printf("Black/White Level = %d/%d\n", 
			   vi->black, vi->white);
		    break;

		  case 'l':
		    if (vi->zsize > 1)
			 xyz_movie_rloop(vi, &(vi->zmouse), vi->zsize, 1);
		    break;
		  case 'L':
		    if (vi->zsize > 1)
			 xyz_movie_rloop(vi, &(vi->zmouse), vi->zsize, -1);
		    break;
		    
		  case 'm':
		    if (vi->zsize > 1)
			 xyz_movie_loop(vi, &(vi->zmouse), vi->zsize, 1);
		    break;
		  case ',':
		    vi->frate -= 1;
		    if (vi->frate < 1)
			 vi->frate = 1;
		    break;
		  case '.':
		    vi->frate += 1;
		    break;
		  case '?':
		    print_mrcv_inst();	   
		    break;
		    
		  case '9':
		    Mrcv_plax -= 1;
		    setview(vi);
		    break;
		  case '0':
		    Mrcv_plax += 1;
		    setview(vi);
		    break;
		    
		  case 'x':
		    Mrcv_xmont--;
		    if (Mrcv_xmont == 0)
			 Mrcv_xmont = 1;
		    setview(vi);
		    break;
		    
		  case 'X':
		    Mrcv_xmont++;
		    setview(vi);
		    break;
		    
		  case 'y':
		    Mrcv_ymont--;
		    if (Mrcv_ymont == 0)
			 Mrcv_ymont = 1;
		    setview(vi);
		    break;
		  case 'Y':
		    Mrcv_ymont++;
		    setview(vi);
		    break;

		  case 'M':
		    system("/usr/gfx/setmon -n 72HZ");
		    break;
		  case 'N':
		    system("/usr/gfx/setmon NTSC");
		    break;
		  case 'B':
		    system("/usr/gfx/setmon STR_RECT");
		    break;
		    
		  case 'q':
		  case 'Q':
		    vi->movie = 0;
		    return(0);

		  default:
		    break;
	       }
	       break;
	       
	     case ESCKEY:
	       return(0);

	     default:
	       break;
	  }
     
  return(1);
}

int mrcv_make_menu(void)
{
     long submenu;
     submenu = newpup();
     addtopup(submenu, "Less Columns (x)%x20", 0);
     addtopup(submenu, "More Columns (X)%x21", 0);
     addtopup(submenu, "Less Rows (y)%x22", 0);
     addtopup(submenu, "More Rows (Y)%x23", 0);

     Mrcv_menu = newpup();
     addtopup(Mrcv_menu, "Mrc View %t", 0);
     addtopup(Mrcv_menu, "Zoom up (=)%x10", 0);
     addtopup(Mrcv_menu, "Zoom down (-)%x11", 0);
     addtopup(Mrcv_menu, "Blend (d)%x12", 0);
     addtopup(Mrcv_menu, "Montage %m",  submenu);
     addtopup(Mrcv_menu, "Help %x98", 0);
     addtopup(Mrcv_menu, "Quit %x99", 0);
     return 0;
}


int mrcv_do_menu(struct ViewInfo *vi, long no)
{
      switch(no){
	   
	 case 99:
	   return(0);

	 case 98:
	   print_mrcv_inst();
	   break;

	 case 10:
	   if (vi->zoom >= 1.0)
		vi->zoom += 1.0;
	   else
		(vi->zoom = 1.0);
	   setview(vi);
	   break;
	   
	 case 11:
	   if (vi->zoom > 1)
		vi->zoom -= 1.0;
	   setview(vi);
	   break;
	   
	 case 12:
	   Mrcv_szoom = 5;
	   setview(vi);
	   Mrcv_szoom = 0;
	   break;

	 case 20:
	   Mrcv_xmont--;
	   if (Mrcv_xmont == 0)
		Mrcv_xmont = 1;
	   setview(vi);
	   break;

	 case 21:
	   Mrcv_xmont++;
	   setview(vi);
	   break;
	   
	 case 22:
	   Mrcv_ymont--;
	   if (Mrcv_ymont == 0)
		Mrcv_ymont = 1;
	   setview(vi);
	   break;

	 case 23:
	   Mrcv_ymont++;
	   setview(vi);
	   break;

	 default:
	   return(1);
      }

     return(1);
}

void print_mrcv_inst(void)
{

  printf("\n\n--------------------MRC View Help-----------------------\n\n");
  printf("\tArrows      Move area to be viewed.\n");
  printf("\tPage Down   Previous z image.\n");
  printf("\tPage Up     Next z image.\n");
  printf("\tHome        Go to first image.\n");
  printf("\tEnd         Go to last image.\n\n");

  printf("\tF1          Decrease black level.\n");
  printf("\tF2          Increase black level.\n");
  printf("\tF3          Decrease white level.\n");
  printf("\tF4          Increase white level.\n\n");


  printf("\t- / =       Decrease / Increase Zoom.\n");
  printf("\t. / ,       Faster / Slower frame rate.\n");
  printf("\tb / n       Decrease / Increase Brightness.\n");
  printf("\tc / v       Decrease / Increase Contrast.\n");
  printf("\tx / X       Decrease / Increase x montage.\n");
  printf("\ty / Y       Decrease / Increase y montage.\n\n");
  printf("\t9 / 0       Decrease / Increase 3D image separation.\n");
  printf("\tm           Toggle movie mode.\n");
  printf("\t3           Toggle 3-D view hardware.\n");

  printf("\tp           Toggle pointer on/off.\n");
  printf("\tr           Redraw Screen.\n");
  printf("\to           Print info to standard output.\n");
  printf("\t?           Print help to stardard output.\n");
  printf("\tq           Quit Program.\n");
  printf("\n\n\n");
}

#endif
