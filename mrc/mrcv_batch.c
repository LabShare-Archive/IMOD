/*  IMOD VERSION 2.50
 *
 *  mrcv_batch.c -- Handles mrcv batch files.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1994-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <stdio.h>
#include <stdlib.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <gl/get.h>
#include <fmclient.h>
#include <string.h>
#include "mrcc.h"
#include "colormap.h"
#include "mrcv.h"

fmfonthandle Mrcv_font;
Colorindex Mrcv_overcolor = 1;
float Fontscale = 1.0;
Colorindex Mrcv_titlebg = 1;

long Mrcv_batch_window = -1;
int MrcvWindrawLlx = 0, MrcvWindrawLly = 0;
int MrcvWindrawUrx = 0, MrcvWindrawUry = 0;
int Mrcv_label_corner = 0;
int Mrcv_labelx = 0;
int Mrcv_labely = 0;

/*
 *  Used to store what has been drawn to a buffer
 *  so that we can redraw the screen if needed.
 *  MrcvBatchWinDraw is true if MrcvBatchPixels contain
 *  the current image data.
 */
int         MrcvBatchWinDraw   = 0;    
int         MrcvBatchPixelSize = 0;
Colorindex *MrcvBatchPixels    = NULL;

char *Mrcv_command[10] = {NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL};

int mrcvb_loadsec(char *fname, int section, int black, int white, char axis);

int do_batch(FILE *fin, int force)
{
     int len;
     char line[128];
     int isbatch = 0;
     long xsize, ysize;

     if (force)
	  isbatch = 1;

     fgetline(fin, line, 128);

     if (substr(line, "mrcv"))
	  isbatch = 1;

     if (!isbatch)
	  return(0);
     
     if (Verbose)
	  printf("Reading script file.\n");

     init_vi(Mrcv1_vi);
     init_vi(Mrcv2_vi);
     open_ntsc(Mrcv_vi, Mrcv_fullwin);
     
     winset(Mrcv_batch_window);
     
     getsize(&xsize, &ysize);
     ortho2(-0.5, (float)xsize + 0.5f, -0.5, (float)ysize + 0.5f);
     viewport(0, xsize-1, 0, ysize-1);

     color(BLACK);
     clear();
     swapbuffers();
     color(BLACK);
     clear();
     swapbuffers();
     cmap_init(Mrcv1_vi->black, Mrcv1_vi->white, Rampbase, 256);
     adjustcmap_pf(&(Mrcv1_vi->black), &(Mrcv1_vi->white), Mrcv1_vi->rampbase);
     adjustcmap_pf(&(Mrcv2_vi->black), &(Mrcv2_vi->white), Mrcv2_vi->rampbase);
     gflush();


     if (Verbose)
	  puts("Begin");
     while ((len = fgetline(fin, line, 128)) >= 0){
       if (!len)
         continue;
	  if (parse_line(line, fin) != 0){
	       if (Verbose)
		    printf("All done.\n");
	       return(1);
	  }
     }
     if (Verbose)
	  puts("End of File");

     cursoff();

     return(1);
}


int open_ntsc(struct ViewInfo *vi, int flag)
{
     long xsize, ysize;

     foreground();

     xsize =  getgdesc(GD_XPMAX);
     ysize = getgdesc(GD_YPMAX);

     if (flag){
	  prefsize(xsize, ysize);
	  prefposition(0l, xsize, 0l, ysize);
     }else{
	  prefsize(NTSCX, NTSCY);
	  prefposition(0l, NTSCX, 0l, NTSCY);
     }
     noborder();

     Fontscale = 1.0;
     Fontscale = 1024.0 / (double)ysize;

     Mrcv_batch_window = winopen("");
     doublebuffer();

/*     glcompat(GLC_SLOWMAPCOLORS, TRUE); */
     gconfig();

     color(BLACK);
     clear();
     swapbuffers();
     color(BLACK);
     clear();
     swapbuffers();
     gflush();

     overlay(getgdesc(GD_BITS_OVER_SNG_CMODE));
     drawmode(OVERDRAW);
     mapcolor(1, 255, 100,   0);
     mapcolor(2, 0, 255, 0);
     mapcolor(3, 0, 0, 255);
     color(0);
     clear();
     gflush();
     drawmode(NORMALDRAW);
     Mrcv_titlebg = Rampbase - 2;
     mapcolor(Mrcv_titlebg, 255, 255, 255);

     qdevice(REDRAW);
     qdevice(KEYBD);
     qdevice(LEFTARROWKEY);
     qdevice(RIGHTARROWKEY);
     qdevice(UPARROWKEY);
     qdevice(DOWNARROWKEY);
     qdevice(REDRAW);
     qdevice(PAGEUPKEY);
     qdevice(PAGEDOWNKEY);
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
     qdevice(RIGHTMOUSE);
     qdevice(LEFTMOUSE);

     adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);     
     mapcolor(vi->rampbase - 1, 150, 150, 0);
     select_font("Helvetica", 12); 
     gflush();
     return(0);
}


void init_vi(struct ViewInfo *vi)
{
     vi->viewdata = NULL;
     vi->idata = NULL;
     vi->llx = 0;
     vi->lly = 0;
     vi->urx = 0;
     vi->ury = 0;
     vi->zoom = 1.0;
     vi->xsize = 0;
     vi->ysize = 0;
     vi->zsize = 0;
     vi->xmouse = 0;
     vi->ymouse = 0;
     vi->zmouse = 0; /* current section # */
     vi->xtrans = 0; /* yoffset */
     vi->ytrans = 0; /* xoffset */
     vi->ztrans = 0;
     vi->blowup = 1.0;
     vi->frate = 1;
     vi->black = 0;
     vi->white = 255;
     vi->imod = NULL;
     vi->blowup = 1;
}


int parse_line(char *iline, FILE *fin)
{
     struct ViewInfo *vi;
     int data;
     int i, section, black, white;
     int red, green, blue;
     int llx, lly, urx, ury;
     char *filename;
     char fname[256];
     char fontname[128];
     char charcode[2];
     char syscom[256];
     short val = 0;
     float angle;
     Device dev;
     char *line = iline;
     long xsize, ysize;
     int com;
     vi = Mrcv_vi;
  
     if (line[0] == '#')
	  return(0);

     if (line[0] == '\n')
	  return(0);

     if (line[0] == '%'){
	  line++;
	  if(line[0] == ' ')
	       line++;
     }
     
     if (substr(line, "system ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  for(i = 7; line[i]; i++)
	       syscom[i - 7] = line[i];
	  syscom[i - 7] = 0x00;
	  system((const char *)syscom);
	  return(0);
     }

     if (substr(line, "command ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  for(i = 10; line[i]; i++)
	       syscom[i - 10] = line[i];
	  syscom[i - 10] = 0x00;

	  com = atoi(&(line[8]));
	  Mrcv_command[com] = malloc(strlen(syscom)+1);
	  strcpy(Mrcv_command[com], syscom);
	  if (Verbose)
	       printf("command # %d = (%s)\n", com, Mrcv_command[com]);
	  return(0);
     }
	  
     if (substr(line, "load ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  filename = line;
	  filename += 5;
	  if (Verbose)
	       printf("Loading file %s.\n", filename);
	  load_mrc(filename);
	  return(0);
     }

     if (substr(line, "loadz ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  filename = line;
	  sscanf(line, "%*s %*s %d %d %d", 
		 &section, &black, &white);
	  for(i = 6; line[i] != ' '; i++)
	       fname[i - 6] = line[i];
	  fname[i - 6] = 0x00;
	  mrcvb_loadsec(fname, section, black, white, 'z');
	  return(0);
     }
     if (substr(line, "loady ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  filename = line;
	  sscanf(line, "%*s %*s %d %d %d", 
		 &section, &black, &white);
	  for(i = 6; line[i] != ' '; i++)
	       fname[i - 6] = line[i];
	  fname[i - 6] = 0x00;
	  mrcvb_loadsec(fname, section, black, white, 'y');
	  return(0);
     }
     if (substr(line, "loadx ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  filename = line;
	  sscanf(line, "%*s %*s %d %d %d", 
		 &section, &black, &white);
	  for(i = 6; line[i] != ' '; i++)
	       fname[i - 6] = line[i];
	  fname[i - 6] = 0x00;
	  mrcvb_loadsec(fname, section, black, white, 'x');
	  return(0);
     }

     /* load an imod model */
     if (substr(line, "loadm ")){
	  if (strlen(line) > 6){
	       filename = line;filename += 6;
	       Mrcv_vi->imod = imodRead(filename);
	       if (!Mrcv_vi->imod)
		    fprintf(stderr, "error loading: %s\n", filename);
	  }else{
	       Mrcv_vi->imod = NULL;
	  }
	  Mrcv_model = Mrcv_vi->imod;
	  if (Verbose){
	        printf("Executing: %s.\n", line);
		if (!Mrcv_model)
		     printf("NULL model loaded\n");
	  }
	  return(0);
     }

/*     if (substr(line, "setgamma")){
	  float gamma;
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  gamma = atod(&line[9]);

     }
*/
     if (substr(line, "setmon NTSC")){
	  if (!Mrcv_fullwin){
	       if (Verbose)
		    printf("Executing: %s.\n", line);
	       setmonitor(NTSC);
	  }
	  return(0);
     }

     if (substr(line, "setmon HZ60")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  setmonitor(HZ60);
	  return(0);
     }

     if (substr(line, "setmon HZ72")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  setmonitor(HZ72);
	  return(0);
     }


     if (substr(line, "szoom on")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  Mrcv_szoom = 5;
	  return(0);
     }

     if (substr(line, "szoom off")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  Mrcv_szoom = 0;
	  return(0);
     }
     

     if (substr(line, "swap")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  if (Mrcv_vi == Mrcv1_vi)
	       Mrcv_vi = Mrcv2_vi;
	  else
	       Mrcv_vi = Mrcv1_vi;
	  
	  adjustcmap_pf(&(Mrcv_vi->black), &(Mrcv_vi->white), 
			Mrcv_vi->rampbase);
	  Mrcv_model = Mrcv_vi->imod;
	  return(0);
     }
     
     if (substr(line, "zoom")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%f", &(vi->zoom));
	  if (Verbose)
	      printf("Zoom set to %f\n", vi->zoom);
	  return(0);
     }
     
     if (substr(line, "display ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &(Mrcv_vi->zmouse));
	  if (Mrcv_vi->zmouse >= Mrcv_vi->zsize){
	       fprintf(stderr, "MRCV INPUT ERROR: z value to big.\n");
	       Mrcv_vi->zmouse = Mrcv_vi->zsize -1;
	  }
	  if (Mrcv_vi->zmouse < 0)
	       Mrcv_vi->zmouse = 0;
	  setview(Mrcv_vi);
	  return(0);	  
     }
     
     if (substr(line, "offset ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d",&(Mrcv_vi->xtrans),&(Mrcv_vi->ytrans));
	  return(0);
     }

     if (substr(line, "section ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &(Mrcv_vi->zmouse));
	  if (Mrcv_vi->zmouse >= Mrcv_vi->zsize){
	       fprintf(stderr, "MRCV INPUT ERROR: z value to big.\n");
	       Mrcv_vi->zmouse = Mrcv_vi->zsize - 1;
	  }
	  if (Mrcv_vi->zmouse < 0)
	       Mrcv_vi->zmouse = 0;
	  return(0);
     }
     
     if (substr(line, "movie ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &(data));
	  MrcvBatchWinDraw = 0;
	  mrc_movie(vi, &(vi->zmouse), vi->zsize, 1, data);
	  return(0);
     }
     
     if (substr(line, "sleep ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &(data));
	  sleep(data);
	  return(0);
     }

     if (substr(line, "slide ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &(data));
	  show_slide(fin, data);
	  return(0);
     }
     
     if (substr(line, "title ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);   
	  sscanf(line, "%*s%d", &(data));
	  show_title(fin, data);
	  return(0);
     }
     
     if (substr(line, "clear")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  color(Rampbase + Mrcv_background);
	  clear();
	  swapbuffers();
	  color(Rampbase + Mrcv_background);
	  clear();
	  swapbuffers();
	  gflush();
	  MrcvBatchWinDraw = 0;
	  return(0);
     }

     if (substr(line, "font ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%s%d", fontname, &(data));
	  
	  if (select_font(fontname, data))
	       fprintf(stderr, "Couldn't open font %s %d\n", fontname, data);
	  return(0);
     }
     
     if (substr(line, "blacklevel ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &(Mrcv_vi->black));
	  adjustcmap_pf(&(Mrcv_vi->black), &(Mrcv_vi->white), 
			Mrcv_vi->rampbase);
	  return(0);
     }
     
     if (substr(line, "whitelevel ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &(Mrcv_vi->white));
	  adjustcmap_pf(&(Mrcv_vi->black), &(Mrcv_vi->white), 
			Mrcv_vi->rampbase);
	  return(0);
     }

     if (substr(line, "backlevel ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &Mrcv_background);
	  return(0);
     }

     if (substr(line, "reverse")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  cmap_toggle_reverse();
	  return(0);
     }

     if (substr(line, "falsecolor")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  cmap_toggle_falsecolor();
	  return(0);
     }
     
     
     if (substr(line, "moviedelay ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &(Mrcv_vi->frate));
	  return(0);
     }
     
     if (substr(line, "titlecolor ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d%*c%d", &red, &green, &blue);
	  mapcolor((Colorindex) Mrcv_vi->rampbase - (Colorindex)1, 
		   (short)red, (short)green, (short)blue);
	  gflush();
	  return(0);
     }

     if (substr(line, "titlebg ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d%*c%d", &red, &green, &blue);
	  mapcolor((Colorindex) Mrcv_titlebg,
		   (short)red, (short)green, (short)blue);
	  gflush();
	  return(0);
     }
     
     
     if (substr(line, "winclear")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  color(Rampbase + Mrcv_background);
	  clear();
	  return(0);
     }
     if (substr(line, "windraw ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d%*c%d%*c%d", &llx, &lly, &urx, &ury);
	  llx *= Mrcv_vidscale;
	  lly *= Mrcv_vidscale;
	  urx *= Mrcv_vidscale;
	  ury *= Mrcv_vidscale;
	  llx += Mrcv_xoff;
	  lly += Mrcv_yoff;
	  urx += Mrcv_xoff;
	  ury += Mrcv_yoff;
	  draw_image(llx, lly, urx, ury, Mrcv_vi);
	  MrcvWindrawLlx = llx; 
	  MrcvWindrawLly = lly;
	  MrcvWindrawUrx = urx;
	  MrcvWindrawUry = ury;	  
	  return(0);
     }
     if (substr(line, "winshow")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);

	  rectzoom(1.0f, 1.0f);
	  getsize(&xsize, &ysize);
	  swapbuffers();
	  readsource(SRC_FRONT);
	  rectcopy(0,0,xsize,ysize,0,0);
	  swapbuffers();
	  gflush();

	  MrcvBatchWinDraw = 1;
	  getsize(&xsize, &ysize);
	  if (MrcvBatchPixelSize < (xsize * ysize)){
	      if (MrcvBatchPixels) free (MrcvBatchPixels);
	      MrcvBatchPixelSize = xsize * ysize;
	      MrcvBatchPixels = (Colorindex *)
		  malloc(2*MrcvBatchPixelSize  * sizeof(Colorindex));
	  }
	  if (MrcvBatchPixels)
	      rectread(0,0,xsize,ysize,MrcvBatchPixels);


	  return(0);
     }

     if (substr(line, "text ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d", &llx, &lly);
	  llx *= Mrcv_vidscale;
	  lly *= Mrcv_vidscale;
	  llx += Mrcv_xoff;
	  lly += Mrcv_yoff;
	  draw_text(fin, llx, lly);
	  return(0);
     }
     

     if (substr(line, "label ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  switch(Mrcv_label_corner){
	     case 0:
	       llx = MrcvWindrawLlx;
	       lly = MrcvWindrawLly;
	       break;
	     case 1:
	       llx = MrcvWindrawLlx;
	       lly = MrcvWindrawUry;
	       break;
	     case 2:
	       llx = MrcvWindrawUrx;
	       lly = MrcvWindrawUry;
	       break;
	     case 3:
	       llx = MrcvWindrawUrx;
	       lly = MrcvWindrawLly;
	       break;
	     default:
	       break;
	  }
	  llx = MrcvWindrawLlx;
	  lly = MrcvWindrawLly;
	  llx += Mrcv_labelx;
	  lly += Mrcv_labely;
	  llx *= Mrcv_vidscale;
	  lly *= Mrcv_vidscale;
	  llx += Mrcv_xoff;
	  lly += Mrcv_yoff;
	  cmov2i(llx, lly);
	  drawmode(OVERDRAW);
	  color(Mrcv_overcolor);
	  fmprstr(&(line[6]));
	  drawmode(NORMALDRAW);
	  return(0);
     }

     if (substr(line, "labelxy ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d", &Mrcv_labelx, &Mrcv_labely);
	  return(0);
     }

     if (substr(line, "labelcorner ll")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  Mrcv_label_corner = 0;
	  return(0);
     }
     if (substr(line, "labelcorner ul")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  Mrcv_label_corner = 1;
	  return(0);
     }
     if (substr(line, "labelcorner ur")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  Mrcv_label_corner = 2;
	  return(0);
     }
     if (substr(line, "labelcorner lr")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  Mrcv_label_corner = 3;
	  return(0);
     }
     

     if (substr(line, "print ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  drawmode(OVERDRAW);
	  color(Mrcv_overcolor);
	  fmprstr(&(line[6]));
	  drawmode(NORMALDRAW);
	  
	  return(0);
     }

     if (substr(line, "textxy ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d", &llx, &lly);
	  llx *= Mrcv_vidscale;
	  lly *= Mrcv_vidscale;
	  llx += Mrcv_xoff;
	  lly += Mrcv_yoff;
	  cmov2i(llx, lly);
	  return(0);
	  
     }

     if (substr(line, "rotatefont ")){
	  sscanf(line, "%*s%f", &angle);
	  if (Verbose)
	       printf("Executing: %s, read in %g for angle\n", line, angle);
	  fmrotatepagematrix((double)angle);
	  return(0);
     }

     if (( substr(line, "overclear")) || (substr(line, "textclear"))){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  drawmode(OVERDRAW);
	  color(0);
	  clear();
	  drawmode(NORMALDRAW);
	  gflush();
	  return(0);
     }
     
     if (substr(line, "line ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d%*c%d%*c%d", &llx, &lly, &urx, &ury);
	  llx *= Mrcv_vidscale;
	  lly *= Mrcv_vidscale;
	  urx *= Mrcv_vidscale;
	  ury *= Mrcv_vidscale;
	  llx += Mrcv_xoff;
	  lly += Mrcv_yoff;
	  urx += Mrcv_xoff;
	  ury += Mrcv_yoff;
	  draw_line(llx, lly, urx, ury);
	  return(0);
     }

     if (substr(line, "linewidth ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &data);
	  linewidth(data);
	  return(0);
     }

     if (substr(line, "charcode ")){
	  sscanf(line, "%*s%d", &data);
	  if (Verbose)
	       printf("Executing: %s. %d\n", line, data);
	  charcode[0] = data;
	  charcode[1] = 0x00;
	  drawmode(OVERDRAW);
	  color((Colorindex)Mrcv_overcolor);
	  fmprstr(charcode);
	  printf("Charcode = %s\n", charcode);
	  drawmode(NORMALDRAW);
	  gflush();
	  return(0);
     }

     if (substr(line, "textcolor ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d%*c%d", &red, &green, &blue);
	  drawmode(OVERDRAW);
	  Mrcv_overcolor = 1;
	  mapcolor(1,(short)red, (short)green, (short)blue);
	  drawmode(NORMALDRAW);
	  gflush();
	  return(0);
     }

     if (substr(line, "oversetcolor ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d%*c%d%*c%d", &red, &green, &blue);
	  drawmode(OVERDRAW);
	  mapcolor(Mrcv_overcolor, (short)red, (short)green, (short)blue);
	  gflush();
	  drawmode(NORMALDRAW);

	  return(0);
     }
     
     if (substr(line, "overcolor ")){
	  if (Verbose)
	       printf("Executing: %s.\n", line);
	  sscanf(line, "%*s%d", &data);
	  Mrcv_overcolor = (Colorindex)data;
	  return(0);
     }

     if (substr(line, "keywait")){
	  mrcv_batch_interactive(Mrcv_vi);
	  return(0);
     }

     if (substr(line, "quit"))
	  exit(1);
     
     if (substr(line, "keyquit")){
	  if (Verbose)
	       printf("Executing: %s (Press q to quit)\n", line);
	  while (!mrcv_batch_interactive(Mrcv_vi));
	  exit(1);
     }
     
     if (Verbose)
	  if (strlen(iline) > 1)
	       printf("WARNING skipping line. (%s)\n", iline);

     return(0);
}

/*
unsigned long *b3dGetGLWindowImage()
{
     ong xsize, ysize;
     long xo, yo;
     Screencoord llx, lly, urx, ury;
     unsigned long *idat;

     getsize(&xsize, &ysize);
     getorigin(&xo, &yo);
     llx = xo;
     lly = yo;
     urx = xo + xsize - 1;
     ury = yo + ysize - 1;

     idat = (unsigned long *)malloc( xsize * ysize * sizeof(unsigned long));
     if (!idat)
	  return(NULL);

     pushattributes();
     rectzoom(1.0, 1.0);
     readdisplay(llx, lly, urx, ury, idat, 0);
     popattributes();
     return(idat);
}
*/


int mrcv_batch_interactive(void)
{
     Device dev;
     short val = 0;
     long xorg, yorg;
     long xmouse, ymouse;
     struct ViewInfo *vi;
     long xsize,ysize;

     vi = Mrcv_vi;

     getsize(&xsize, &ysize);
     readsource(SRC_FRONT);
     rectcopy(0,0,xsize,ysize,0,0);

     while (1){
	  dev = qread(&val);

	  switch(dev){

	     case REDRAW:
	      
	      if (MrcvBatchWinDraw)
		  if (MrcvBatchPixels){
		      rectzoom(1.0, 1.0);
		      getsize(&xsize, &ysize);
		      rectwrite(0,0,xsize,ysize,MrcvBatchPixels);
		      swapbuffers();
		  }
		      
	      /*
	       pushattributes();
	       readsource(SRC_BACK);
	       frontbuffer(TRUE);
	       rectcopy(0,0,xsize,ysize,0,0);
	       popattributes();
	       */
	       break;

	     case LEFTMOUSE:
	     case RIGHTMOUSE:
	       if (val){
		    getorigin(&xorg, &yorg);
		    xmouse = getvaluator(MOUSEX) - xorg;
		    ymouse = getvaluator(MOUSEY) - yorg;
		    printf("Mouse location (%d, %d)\n", xmouse, ymouse);
	       }
	       break;

	     case F1KEY:
	       if (!val)
		    break;
	       vi->black--;
	       if (vi->black < 0)
		    vi->black++;
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;
	     case F2KEY:
	       if (!val)
		    break;
	       vi->black++;
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;
	     case F3KEY:
	       if (!val)
		    break;
	       vi->white--;
	       if (vi->white <= vi->black)
		    vi->white++;
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;
	     case F4KEY:
	       if (!val)
		    break;
	       vi->white++;
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;
	       
	     case F5KEY:
	       if (!val)
		    break;
	       vi->white--;
	       vi->black++;
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;

	     case F6KEY:
	       if (!val)
		    break;
	       vi->white++;
	       vi->black--;
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;

	     case F7KEY:
	       if (!val)
		    break;
	       vi->white--;
	       vi->black--;
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;

	     case F8KEY:
	       if (!val)
		    break;
	       vi->white++;
	       vi->black++;
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;

/*
	     case F9KEY:
	       if (!val)
		    break;
	       setmonitor(HZ60);
	       break;
	     case F10KEY:
	       if (!val)
		    break;
	       setmonitor(NTSC);
	       break;
*/

	     case F11KEY:
	       if (!val)
		    break;
	       if (Cmap_Reverse){
		    Cmap_Reverse = 0;
		    vi->reverse = 0;
	       }
	       else{
		    Cmap_Reverse = 1;
		    vi->reverse = 1;
	       }
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;
	     case F12KEY:
	       if (!val)
		    break;
	       if (Cmap_Falsecolor){
		    Cmap_Falsecolor = 0;
		    vi->falsecolor = 0;
	       }
	       else{
		    Cmap_Falsecolor = 1;
		    vi->falsecolor = 1;
	       }
	       adjustcmap_pf(&(vi->black), &(vi->white), vi->rampbase);
	       break;
	       
	     case KEYBD:
	       switch(val){
		  case 'q':
		    return(1);

		  case ' ':
		  case 'c':
		    return(0);

		  case 'P':
		    curson();
		    break;
		  case 'p':
		    cursoff();
		    break;
		  case 'o':
/*		    printf("\nx, y offset = %d, %d\n", vi->xtrans, vi->ytrans); */
/*		    printf("Z-section = %d\n", vi->zmouse); */
/*		    printf("Zoom = %g\n", vi->zoom); */
		    printf("Black/White Level = %d/%d\n",
			   vi->black, vi->white);
		    break;
		  case 's':
		    if (Mrcv_vi == Mrcv1_vi)
			 Mrcv_vi = Mrcv2_vi;
		    else
			 Mrcv_vi = Mrcv1_vi;
		    vi = Mrcv_vi;
		    Cmap_Reverse = vi->reverse;
		    Cmap_Falsecolor = vi->falsecolor;
		    break;
		    
		  case 'S': /* snapshot */
		    break;
		    
		  case '0':
		  case '1':
		  case '2':
		  case '3':
		  case '4':
		  case '5':
		  case '6':
		  case '7':
		  case '8':
		  case '9':
		    if (Mrcv_command[val - 48])
			 system(Mrcv_command[val - 48]);
		    break;
	       }
	  }
     }
}

/*
int fgetline(FILE *fp, char s[],int limit)
{
  int c, i, length;
  
  for (i=0; ( ((c = getc(fp)) != EOF) && (i < (limit-1)) && (c != '\n') ); i++)
    s[i]=c;
  s[i]='\0';
  length = i;

  if (c == EOF)
    return ( -1);
  else
    return (length);
}
*/

load_mrc(char *filename)
{
     FILE *fin;
     struct MRCheader hdata;
     struct ViewInfo *vi;
     int i;
     int fsize;
     
     vi = Mrcv_vi;
     
     
     fin = fopen(filename, "rb");
     
     if (vi->viewdata)
	  free(vi->viewdata);
     
     if (vi->idata){
	  for(i = 0; i < vi->zsize; i++)
	       if (vi->idata[i])
		    free(vi->idata[i]);
	  free(vi->idata);
     }

     if (mrc_head_read(fin, &hdata)){
	  fprintf(stderr, "Can't Read Input File Header.\n");
	  exit(3);
     }
     
     if (Verbose)
	  vi->idata = mrc_read_byte(fin, &hdata, NULL, mrc_default_status);
     else
	  vi->idata = mrc_read_byte(fin, &hdata, NULL, NULL);
     

     fsize = hdata.nx * hdata.ny;
     vi->viewdata = (Colorindex *)malloc(fsize * 2);
     if (!vi->viewdata){
	  fprintf(stderr, "Not Enough memory to load image data.\n");
	  exit(1);
     }
     
     if (Verbose)
	  printf("\n");
     
     for (i = 0; i < fsize; i++){
	  vi->viewdata[i] = vi->idata[vi->zmouse][i] + vi->rampbase;
     }
     vi->xsize = hdata.nx;
     vi->ysize = hdata.ny;
     vi->zsize = hdata.nz;
     vi->xysize = hdata.nx * hdata.ny;
     vi->zmouse = 0;
     vi->urx = vi->xsize - 1;
     vi->ury = vi->ysize - 1;
     return 0;
}

int select_font(char *name, int size)
{
     fmfonthandle tfont;
     double fontsize;
     
     fminit();
     fminitpagematrix();

     if ( (tfont = fmfindfont(name)) == 0){
	  fprintf(stderr, "Couldn't open font %s.\n", name);
	  return(-1);
     }
     fontsize = size * Fontscale;
     
     Mrcv_font = fmscalefont(tfont, fontsize);
     if (!Mrcv_font){
	  fprintf(stderr, "Couldn't scale font %s to %d.\n", name, size);
	  return(-1);
     }
     fmsetfont(Mrcv_font);
     return(0);
}

int draw_line( int x1, int y1, int x2, int y2)
{
     int i;
     short vert1[2];
     short vert2[2];

     drawmode(OVERDRAW);
     color(Mrcv_overcolor);

     vert1[0] = x1;
     vert1[1] = y1;
     vert2[0] = x2;
     vert2[1] = y2;

     bgnline();
       v2s(vert1);
       v2s(vert2);
     endline();

     gflush();
     drawmode(NORMALDRAW);
     return 0;
}

int draw_text(FILE *fin, int x, int y)
{
     char line[128];
     char text[128];

     drawmode(OVERDRAW);
     color(Mrcv_overcolor);
     cmov2i(x, y);
     fgetline(fin, line, 128);
     sprintf(text, line);
     fmprstr(text);
     gflush();
     drawmode(NORMALDRAW);
     return 0;
}

int show_title(FILE *fin, int lines)
{
     fmfontinfo info;
     int i, len;
     int x, y;
     int charx;
     int width;
     int rows, row;
     long xsize, ysize, xoff, yoff, ytop, ycenter;
     char line[128];

     if (Mrcv_fullwin){
	  xsize =  getgdesc(GD_XPMAX);
	  ysize = getgdesc(GD_YPMAX);
	  xoff = 0;
	  yoff = 0;
	  ytop = 0;
	  ycenter = ysize / 2;
     }else{
	  xsize = NTSCX;
	  ysize = NTSCY;
	  xoff  = NTSCX_OVER;
	  yoff  = NTSCY_OVER;
	  ytop  = NTSCY_OVER_TOP;
	  ycenter = NTSCY_CENTER;
     }
     
     fmgetfontinfo(Mrcv_font, &info);
     y = ( ycenter ) + ((lines * info.ysize) / 2);
     y -= info.ysize;
     if (y > (ysize - ytop - info.ysize))
	  y = ysize - ytop - info.ysize;

     color(BLACK);
     clear();
     color(Mrcv_vi->rampbase - 1);

     for (i = 0; i < lines; i++){
	  fgetline(fin, line, 128);
	  len = strlen(line);

	  width = fmgetstrwidth(Mrcv_font, line);
	  if (width < (xsize - (xoff * 2)))
	       x = ((xsize - yoff) / 2) - (width / 2);
	  else
	       x = xoff;

	  cmov2i(x, y);
	  fmprstr(line);
	  y -= info.ysize;
     }
     swapbuffers();
     getsize(&xsize, &ysize);
     readsource(SRC_FRONT);
     rectcopy(0,0,xsize,ysize,0,0);
     swapbuffers();
     MrcvBatchWinDraw = 0;
     gflush();
     return 0;
}

int show_slide(FILE *fin, int lines)
{
     fmfontinfo info;
     int i, len;
     int x, y;
     int charx;
     int width;
     int rows, row;
     long xsize, ysize, xoff, yoff, ytop, ycenter;
     char line[128];

     if (Mrcv_fullwin){
	  xsize =  getgdesc(GD_XPMAX);
	  ysize = getgdesc(GD_YPMAX);
	  xoff = 0;
	  yoff = 0;
	  ytop = 0;
	  ycenter = ysize / 2;
     }else{
	  xsize = NTSCX;
	  ysize = NTSCY;
	  xoff  = NTSCX_OVER;
	  yoff  = NTSCY_OVER;
	  ytop  = NTSCY_OVER_TOP;
	  ycenter = NTSCY_CENTER;
     }

     fmgetfontinfo(Mrcv_font, &info);
     y = ( ycenter ) + ((lines * info.ysize) / 2);
     y -= info.ysize;
     if (y > (ysize - ytop - info.ysize))
	  y = ysize - ytop - info.ysize;

     color(Mrcv_titlebg);
     clear();
     color(Mrcv_vi->rampbase - 1);
     y = ysize - ytop - info.ysize;;
     x = 30;
     for (i = 0; i < lines; i++){
	  fgetline(fin, line, 128);
	  cmov2i(x, y);
	  fmprstr(line);
	  y -= info.ysize;
     }
     swapbuffers();
     getsize(&xsize, &ysize);
     readsource(SRC_FRONT);
     rectcopy(0,0,xsize,ysize,0,0);
     swapbuffers();
     gflush();
     MrcvBatchWinDraw = 0;
     return 0;
}
     

int mrcvb_loadsec(char *fname, int section, int black, int white, char axis)
{
     FILE *fin;
     struct MRCheader hdata;
     struct ViewInfo *vi;
     int i, j, ival;
     int fsize;
     struct MRCslice *sl;
     float slope, intercept;
     Ival val;
     float sblack, swhite;

     vi = Mrcv_vi;

     if (black > white){
	  ival = black; black = white; white = ival;
     }

     fin = fopen(fname, "rb");
     if (!fin){
	  fprintf(stderr, "mrcv: Couldn't open %s\n", fname);
	  return(-1);
     }

/*     if (vi->viewdata)
	  free(vi->viewdata);

     if (vi->idata){
	  for(i = 0; i < vi->zsize; i++)
	       if (vi->idata[i])
		    free(vi->idata[i]);
	  free(vi->idata);
     }
*/
     if (mrc_head_read(fin, &hdata)){
	  fprintf(stderr, "Can't Read Input File Header.\n");
	  return(-1);
     }

     
     if (axis == 'x'){
	  vi->xsize = hdata.nz;
	  vi->ysize = hdata.ny;
	  vi->zsize = 1;
     }
     if (axis == 'y'){
	  vi->xsize = hdata.nx;
	  vi->ysize = hdata.nz;
	  vi->zsize = 1;
     }

     if (axis == 'z'){
	  vi->xsize = hdata.nx;
	  vi->ysize = hdata.ny;
	  vi->zsize = 1;
     }

     vi->xysize = vi->xsize * vi->ysize;
     vi->zmouse = 0;
     vi->urx = vi->xsize - 1;
     vi->ury = vi->ysize - 1;

     fsize = vi->xysize;
     vi->viewdata = (Colorindex *)malloc(fsize * 2);
     if (!vi->viewdata){
	  fprintf(stderr, "Not Enough memory to load image data.\n");
	  return(-1);
     }

     sl = sliceCreate(vi->xsize, vi->ysize, hdata.mode);
     mrc_read_slice(sl->data.b, fin, &hdata, section, axis);
     vi->idata = (unsigned char **)malloc(sizeof(unsigned char*));
     vi->idata[0] = (unsigned char *)malloc
	  (sizeof(unsigned char) * fsize);

     if (hdata.mode == 0){
	  intercept = black;
	  if ( (white - black) != 0)
	       slope = 255.0f / (float)(white - black);
	  else
	       slope = 1;
     }else{
	  sblack = ((hdata.amax - hdata.amin) * (black / 255.0f));
	  swhite = ((hdata.amax - hdata.amin) * (white / 255.0f));
	  if (( swhite - sblack) == 0){
	       slope = 1.0f;
	       intercept = hdata.amin;
	  }else{
	       intercept = (float)hdata.amin + sblack;
	       slope = 255.0f / (float)(swhite - sblack);

	  }
     }

     sliceGetVal(sl, 0, 0, val);
/*
     if (hdata.mode)
	  printf("val = %g, data = %d\n", val[0], sl->data.s[0]);
     else
	  printf("val = %g, data = %d\n", val[0], sl->data.b[0]);
     printf("(%g - %g) * %g \n", val[0], intercept, slope);
*/
     for (j = 0; j < vi->ysize; j++)
	  for (i = 0; i < vi->xsize; i++){
	       sliceGetVal(sl, i, j, val);
/*	       printf("(%g - %g) * %g = ", val[0], intercept, slope); */
	       val[0] -= intercept;
	       val[0] *= slope;

	       ival = val[0] + 0.5;
	       if (ival < 0)
		    ival = 0;
	       if (ival > 255)
		    ival = 255;
/*	       printf("%d\n",ival); */
	       vi->idata[0][i + (j * vi->xsize)] = (unsigned char)ival;
	  }

     sliceFree(sl);
     if (fin)
	  fclose(fin);

     return(0);
}

