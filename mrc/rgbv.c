/*  IMOD VERSION 2.02
 *
 *  rgbv.c -- Color support for mrcv.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <gl/gl.h>
#include <gl/device.h>
#include <gl/image.h>

#include "mrcfiles.h"
#include "mrcv.h"


/*#define RGBV_TEST */

int mrc_color_dbuffer = FALSE;
int Mrc_color_stereo = FALSE;
int Mrc_color_plax = 1;
int Mrc_color_swing = TRUE;

#ifdef RGBV_TEST
main(int argc, char **argv)
{
     struct RGBViewInfo vi;
     FILE *fin;
     Device dev;
     short val;
     int fun = TRUE;


     if (argc < 2){
	  fprintf(stderr, "%s version 0.10 %s, %s\n",
		  argv[0], __DATE__, __TIME__);
	  exit(1);
     }

     fin = fopen(argv[1], "rb");
     if (!fin){
	  fprintf(stderr, "%s: error opening %s.\n", argv[0], argv[1]);
	  exit(1);
     }

     if (mrc_color_loadimage (fin, &vi))
	  exit(1);

     mrc_color_winopen(&vi, MRC_WINDOW_FULL);

     mrc_color_mainloop(&vi);

     free(vi.idata);
     fclose(fin);

     exit(0);
}
#endif

int mrc_color_winopen(struct RGBViewInfo *vi, int winsize)
{
     vi->llx = 0;
     vi->lly = 0;
     vi->zoom = 1.0;
     vi->frate = 1.0;
     vi->sdata = (unsigned long *)malloc(vi->xsize * vi->ysize * sizeof(long));
     vi->xtrans = 0;
     vi->ytrans = 0;
     vi->z = 0;
     switch (winsize){

	case MRC_WINDOW_FULL:
	  prefposition(0l, getgdesc(GD_XPMAX), 0l, getgdesc(GD_YPMAX));
	  noborder();
	  winopen("");
	  vi->urx = getgdesc(GD_XPMAX);
	  vi->ury = getgdesc(GD_YPMAX);
	  break;

	case MRC_WINDOW_NTSC:
	  prefposition(0l, 640l, 0l, 480l);
	  noborder();
	  winopen("");
	  vi->urx = 640;
	  vi->ury = 480;
	  break;
	case MRC_WINDOW_DATASIZE:
	default:
	  prefsize(vi->xsize, vi->ysize);
	  winopen("MRCV");
	  minsize(vi->xsize, vi->ysize);
	  maxsize(getgdesc(GD_XPMAX), getgdesc(GD_YPMAX));
	  winconstraints();
	  vi->urx = vi->xsize;
	  vi->ury = vi->ysize;
	  break;
     }

     RGBmode();
     gconfig();

     qdevice(ESCKEY);
     qdevice(KEYBD);
     qdevice(PAGEUPKEY);
     qdevice(PAGEDOWNKEY);
     qdevice(HOMEKEY);
     qdevice(ENDKEY);
     qdevice(LEFTARROWKEY);
     qdevice(RIGHTARROWKEY);
     qdevice(UPARROWKEY);
     qdevice(DOWNARROWKEY);
     qdevice(LEFTMOUSE);
     cpack(0x00000000);
     clear();
     mrc_color_draw(vi);
     return 0;
}


void mrc_color_trans(struct RGBViewInfo *vi, int x, int y)
{
     if (x){
	  vi->xtrans -= x;
	  if (vi->xtrans > vi->xsize)
	       vi->xtrans = vi->xsize;
	  if (vi->xtrans < -vi->xsize)
	       vi->xtrans = - vi->xsize;
     }
     if (y){
	  vi->ytrans += y;
	  if (vi->ytrans > vi->ysize)
	       vi->ytrans = vi->ysize;
	  if (vi->ytrans < -vi->ysize)
	       vi->ytrans = - vi->ysize;
     }
     
     mrc_color_draw(vi);
}

int mrc_color_mainloop(struct RGBViewInfo *vi)
{
     int xmouse, ymouse, lxm, lym;
     int fun = TRUE;
     Device dev;
     short val;
     int drag;

     while(fun) {
	  dev = qread(&val);

	  switch(dev){

	     case REDRAW:
	       reshapeviewport();
	       mrc_color_clear();
	       mrc_color_draw(vi);
	       break;
	       
	     case ESCKEY:
	       fun = FALSE;
	       break;

	     case LEFTARROWKEY:
	       if (val)
		    mrc_color_trans(vi, 5,  0);
	       break;
	       
	     case RIGHTARROWKEY:
	       if (val)
		    mrc_color_trans(vi, - 5,  0);
	       break;

	     case UPARROWKEY:
	       if (val)
		    mrc_color_trans(vi,  0,  5);
	       break;

	     case DOWNARROWKEY:
	       if (val)
		    mrc_color_trans(vi,  0, -5);
	       break;

	     case PAGEUPKEY:
	       if (!val)
		    break;
	       vi->z++;
	       if (vi->z >= vi->zsize)
		    vi->z--;
	       mrc_color_draw(vi);
	       break;

	     case PAGEDOWNKEY:
	       if (!val)
		    break;
	       vi->z--;

	       if (vi->z < 0)
		    vi->z = 0;
	       mrc_color_draw(vi);
	       break;

	     case HOMEKEY:
	       if (!val)
		    break;
	       vi->z = 0;
	       mrc_color_draw(vi);
	       break;

	     case ENDKEY:
	       if (!val)
		    break;
	       vi->z = vi->zsize - 1;
	       mrc_color_draw(vi);
	       break;

	     case KEYBD:
	       switch(val){
		    
		  case '-':
		    vi->zoom -= 1.0;
		    if (vi->zoom < 1.0)
			 vi->zoom = 1.0;
		    mrc_color_clear();
		    mrc_color_draw(vi);
		    break;

		  case '=':
		    vi->zoom += 1.0;
		    mrc_color_draw(vi);
		    break;

		  case 's':
		    if (Mrc_color_stereo)
			 Mrc_color_stereo = FALSE;
		    else
			 Mrc_color_stereo = TRUE;
		    mrc_color_clear();
		    mrc_color_draw(vi);
		    break;


		  case 'm':
		    mrc_color_movie(vi);
		    break;

		  case 'b':
		    if (mrc_color_dbuffer){
			 singlebuffer();
			 gconfig();
			 mrc_color_dbuffer = FALSE;
		    }
		    else{
			 doublebuffer();
			 gconfig();
			 mrc_color_dbuffer = TRUE;
		    }
		    mrc_color_clear();
		    mrc_color_draw(vi);
		    break;
		    
		  case ',':
		    Mrc_color_plax--;
		    mrc_color_draw(vi);
		    break;
		    
		  case '.':
		    Mrc_color_plax++;
		    mrc_color_draw(vi);
		    break;

		  case 'l':
		    if (Mrc_color_swing)
			 Mrc_color_swing = FALSE;
		    else 
			 Mrc_color_swing = TRUE;
		    break;

		    
		  case '[':
		    vi->frate--;
		    break;
		  case ']':
		    vi->frate++;
		    break;

		  case 'q':
		  case 'Q':
		    fun = FALSE;
		    break;
	       }
	       break;

	     case LEFTMOUSE:
	       if(val){
		    
		    lxm = getvaluator(MOUSEX);
		    lym = getvaluator(MOUSEY);
		    drag = 1;
		    while (drag){
			 while(qtest()){
			      dev = qread(&val);
			      if (dev == LEFTMOUSE){
				   drag = 0;
				   break;
			      }
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
			 mrc_color_draw(vi);
		    }
	       }
	       break;
	       
	  }
     }
     return(0);
}


int mrc_color_clear(void)
{
     cpack(0x00000000);
     clear();
     if (mrc_color_dbuffer){
	  swapbuffers();
	  cpack(0x00000000);
	  clear();
     }
     return 0;
}

int mrc_color_draw(struct RGBViewInfo *vi)
{

     long winx, winy;
     int z;

     getsize((long *)&winx, (long *)&winy);

     vi->llx = 0;
     vi->lly = 0;
     if (Mrc_color_stereo)
	  vi->urx = (winx / 2) - 5;
     else{
	  vi->urx = winx;
     }
     vi->ury = winy;

     color_draw_image(vi);

     if (Mrc_color_stereo){
	  vi->llx = (winx / 2) + 5;
	  vi->urx =  winx;
	  z = vi->z;
	  vi->z += Mrc_color_plax;
	  if (vi->z >= vi->zsize)
	       vi->z = vi->zsize - 1;
	  if (vi->z < 0)
	       vi->z = 0;
	  color_draw_image(vi);
	  vi->z = z;
     }

     if (mrc_color_dbuffer)
	  swapbuffers();
     return 0;
}

color_draw_image(struct RGBViewInfo *vi)
{

     int swinx, swiny;  /* sub-window size */
     int xdrawsize,   ydrawsize;
     int xborder = 0, yborder = 0;
     unsigned long *image;
     int xstart = 0;
     int ystart = 0;
     int i, j;

     swinx = vi->urx - vi->llx;
     swiny = vi->ury - vi->lly;

     image = vi->idata + (vi->z * (vi->xsize * vi->ysize));
     
     if ( ((vi->xsize - 1) * vi->zoom) < swinx ){
	  xdrawsize = (vi->xsize);
	  xborder = ( swinx - ((vi->xsize - 1) * vi->zoom)) / 2;
     }
     else{
	  xdrawsize = swinx / vi->zoom;
	  xstart = (vi->xsize / 2 ) - (swinx / vi->zoom / 2);
	  xstart -= vi->xtrans;
	  if (xstart < 0){
	       xstart += vi->xtrans;
	       vi->xtrans = xstart;
	       xstart -= vi->xtrans;
	  }
	  if ( (xstart + xdrawsize) > (vi->xsize - 1)){
	       xstart += vi->xtrans;
	       vi->xtrans = xstart - (vi->xsize - 1 - xdrawsize);
	       xstart -= vi->xtrans;
	  }
     }

     if ( ((vi->ysize - 1) * vi->zoom) < swiny ){
	  ydrawsize = (vi->ysize);
	  yborder = ( swiny - ((vi->ysize - 1) * vi->zoom)) / 2;
     }
     else{
	  ydrawsize = swiny / vi->zoom;
	  ystart = (vi->ysize / 2 ) - (swiny / vi->zoom / 2);
	  ystart -= vi->ytrans;
	  if (ystart < 0){
	       ystart += vi->ytrans;
	       vi->ytrans = ystart;
	       ystart -= vi->ytrans;
	  }
	  if ( (ystart + ydrawsize) > (vi->ysize - 1)){
	       ystart += vi->ytrans;
	       vi->ytrans = ystart - (vi->ysize - 1 - ydrawsize);
	       ystart -= vi->ytrans;
	  }
	  
     }
     
     if ((ydrawsize != vi->ysize) || (xdrawsize != vi->xsize)){

	  image += ystart * vi->xsize;
	  image += xstart;
	  
	  for (j = 0; j < ydrawsize; j++){
	       for(i = 0; i < xdrawsize; i++, image++)
		    vi->sdata[(j * xdrawsize) + i] = *image;
	       image += vi->xsize;
	       image -= xdrawsize;
	  }
	  image = vi->sdata;
     }

     rectzoom(vi->zoom, vi->zoom);
     lrectwrite(vi->llx + xborder, vi->lly + yborder,
	       vi->llx + xborder + xdrawsize - 1,
	       vi->lly + yborder + ydrawsize - 1,
	       image);

     return 0;
}


int mrc_color_loadimage(FILE *fin, struct RGBViewInfo *vi)
{
     struct MRCheader hdata;
     long ds;
     unsigned char r,g,b;
     int i;

     mrc_head_read(fin, &hdata);

     if (hdata.mode != MRC_MODE_RGB){
	  fprintf(stderr, "mrc_color_loadimage: mrcfile not rgb.\n");
	  return(-1);
     }

     vi->xsize = hdata.nx;
     vi->ysize = hdata.ny;
     vi->zsize = hdata.nz;

     printf("Color image size %d x %d, %d sections.\n\n", vi->xsize, vi->ysize,
	    vi->zsize);

     ds = vi->xsize * vi->ysize * vi->zsize;
     vi->idata = (unsigned long *)malloc(ds * sizeof(long));
     if(!vi->idata){
	  fprintf(stderr, "mrc_color_loadimage: memory allocation error.\n");
	  return(-1);
     }
     fseek(fin, 1024, 0);
     for (i = 0; i < ds; i++){
	  fread(&r, 1, 1, fin);
	  fread(&g, 1, 1, fin);
	  fread(&b, 1, 1, fin);
	  vi->idata[i] = 0x00 | r | g << 8 | b << 16;
     }
     return(0);
}



int mrc_color_movie(struct RGBViewInfo *vi)
{
     int fun = TRUE;


     while (fun){
	  while(vi->z < vi->zsize){
	       mrc_color_draw(vi);
	       if (vi->frate > 1)
		    sginap(vi->frate);
	       vi->z++;
	       if (mrc_color_check_quit(vi))
		    return(0);;
	  }
	  
	  if (Mrc_color_swing)
	       vi->z = vi->zsize - 1;
	  else
	       vi->z = -1;

	  while(vi->z >= 0){
	       mrc_color_draw(vi);
	       if (vi->frate > 1)
		    sginap(vi->frate);
	       vi->z--;
	       if (mrc_color_check_quit(vi))
		    return(0);
	  }
	  vi->z = 0;
     }
     return 0;
}


int mrc_color_check_quit(struct RGBViewInfo *vi)
{

     Device dev;
     short val;
     
     while(qtest()){
	  
	  switch (dev = qread(&val)){
	     case KEYBD:
	       switch(val){

		  case 'm':
		  case 'q':
		  case 'Q':
		  case ' ':
		    return(1);

		  case 'l':
		    if (Mrc_color_swing)
			 Mrc_color_swing = FALSE;
		    else 
			 Mrc_color_swing = TRUE;
		    break;

		  case '-':
		    vi->zoom -= 1.0;
		    if (vi->zoom < 1.0)
			 vi->zoom = 1.0;
		    mrc_color_clear();
		    mrc_color_draw(vi);
		    break;
		    
		  case '=':
		    vi->zoom += 1.0;
		    mrc_color_draw(vi);
		    break;
		    
		  case '[':
		    vi->frate--;
		    if (vi->frate < 0)
			 vi->frate = 0;
		    break;
		  case ']':
		    vi->frate++;
		    break;
		    
		  default:
		    return(0);

	       }
	       return(0);
	       
	     case REDRAW:
	       reshapeviewport();
	       mrc_color_clear();
	       mrc_color_draw(vi);
	       break;

	     case ESCKEY:
	       return(1);

	     case LEFTARROWKEY:
	       if (val)
		    mrc_color_trans(vi, -5,  0);
	       break;

	     case RIGHTARROWKEY:
	       if (val)
		    mrc_color_trans(vi,  5,  0);
	       break;

	     case UPARROWKEY:
	       if (val)
		    mrc_color_trans(vi,  0,  5);
	       break;

	     case DOWNARROWKEY:
	       if (val)
		    mrc_color_trans(vi,  0, -5);
	       break;
	       
	     default:
	       return(0);
		    
	  }
	  
	  
     }
     return(0);
}
