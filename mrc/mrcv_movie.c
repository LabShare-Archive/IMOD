/*  IMOD VERSION 2.02
 *
 *  xyz_movie.c -- Does movies for mrcv.c
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1994-1996 by Boulder Laboratory for 3-Dimensional Fine    *
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

/* System include files */
#include <stdio.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <limits.h>

/* MRC include files    */
#include "mrcfiles.h"
#include "mrcv.h"


int mrc_movie(struct ViewInfo *vi,
	      int *section,         /* Current section */
	      int frames,           /* Max section #   */
	      int direction,
	      int repeat)

{
     int i;

     if (!repeat)
	  return (xyz_movie_loop(vi, section, frames, direction));
	  
     
     for (i = 0; i < repeat; i++){

	  while( (*section < frames ) && (*section >= 0)){
	       setview(vi);
	       if (vi->frate > 1)
		    sginap(vi->frate);
	       *section += direction;
	       if (!movieinput(vi))
		    return(0);
	  }
	  *section -= direction;
	  
	  while( (*section < frames ) && (*section >= 0)){
	       setview(vi);
	       if (vi->frate > 1)
		    sginap(vi->frate);
	       *section -= direction;
	       if (!movieinput(vi))
		    return(0);
	  }
	  *section += direction;
     }
     return(1);
	  

}



int xyz_movie_loop(struct ViewInfo *vi, 
		   int *section,         /* Current section */
		   int frames,           /* Max section #   */
		   int direction)        
{
     int movie = 1;
    
     
     if ( (vi->zmouse + Mrcv_plax ) >= (vi->zsize - 2)){
	  vi->zmouse = vi->zsize - 2 - Mrcv_plax;
     }

     if ( (vi->zmouse + Mrcv_plax) <= 0){
	  vi->zmouse = -Mrcv_plax;
     }

     if (vi->zmouse >= (vi->zsize - 1))
	  vi->zmouse = vi->zsize - 1;
     if (vi->zmouse < 0)
	  vi->zmouse = 0;

     while(movie){

	  while( (*section < frames ) && (*section >= 0)){

	       setview(vi);

	       if (vi->frate)
		    sginap(vi->frate);

	       *section += direction;
	       if (!movieinput(vi))
		    return(0);
	       if (mrcv_stereo_end(vi))
		    break;

	  }
	  *section -= direction;
	  
	  while( (*section < frames ) && (*section >= 0)){
	       setview(vi);
	       *section -= direction;
	       if (vi->frate)
		    sginap(vi->frate);
	       if (!movieinput(vi))
		    return(0);
	       if (mrcv_stereo_end(vi))
	   break;
	  }
	  *section += direction;
	  
     }
     return(1);
}


int xyz_movie_rloop(struct ViewInfo *vi,
		    int *section,
		    int frames,
		    int direction)
{
     int movie = 1;

     while(movie){
	  while( (*section < frames ) && (*section >= 0)){
	       setview(vi);
	       if (vi->frate)
		    sginap(vi->frate);
	       if (!movieinput(vi))
		    return(0);
	       *section += direction;
	  }
	  
	  if (direction > 0)
	       *section = 0;
	  else
	       *section = frames;
     }
	  
     return 0;
}


int mrcv_stereo_end(struct ViewInfo *vi)
{
     if (!Mrcv_stereo)
	  return(0);
     if (!Mrcv_plax)
	  return(0);

     if (Mrcv_plax > 0){
	  if (!vi->zmouse)
	       return(1);
	  if ( vi->zmouse == (vi->zsize - 1 - Mrcv_plax))
	       return(1);
	  return(0);
     }

     if ( vi->zmouse == (-Mrcv_plax))
	  return(1);

     if ( vi->zmouse == (vi->zsize - 1))
	  return(1);

     return(0);
}


int movieinput(struct ViewInfo *vi)
{
  short val = 0;
  Device dev;


  while(qtest()){
       switch (dev = qread(&val)){
/*	    printf("movie glEvent %d, %d\n", dev, val); */

	  case REDRAW:
	    reshapeviewport();
	    setview(vi);
	    break;
	    
	  case MIDDLEMOUSE:
	    if (val)
		 return(0);
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

	    
	  case KEYBD:
	    switch (val){
	       case ',':
		 vi->frate++;
		 break;
	       case '.':
		 vi->frate--;
		 if (vi->frate < 0)
		      vi->frate = 0;
		 break;

	       case 'l':
	       case 'L':
	       case 'M':
	       case 'm':
		 return(0);
	       default:
		 break;
	    }
	    
	  default:
	    break;
       }
       
  }

  return(1);
}













