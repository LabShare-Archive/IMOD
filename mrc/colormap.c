/*  IMOD VERSION 2.10
 *
 *  colormap.c -- Provides grey level color map editing.
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include "colormap.h"


/* Globals */

struct Cmap_Levels Cmap_Levels_Array[CMAP_LEVEL_ARRAY_SIZE];

int Cmap_Falsecolor = 0;
int Cmap_CurrentMap = 0;
int Cmap_Rampbase = 0;
int Cmap_Reverse = 0;
int Cmap_rampsize = 0; /* JRK: rampsize can be smaller then 255. */

/* cmap_new: make a new ramp with black and white levels */
void cmap_new(int black, int white)
{
     adjustcmap_pf(&black, &white, Cmap_Rampbase);
}


/*****************************************************************************/
/* cmap_init: Initializes the colormap, starting at rampbase, with a         */
/* rampsize of 256.  Initial black and white levels (0-255), are also set.   */
/* void cmap_init(int black, int white, int rampbase) old function call.     */
void cmap_init(int black, int white, int rampbase, int rampmax)
{
     int i;

     /* Different scale levels can be used. */
     for (i = 0; i < CMAP_LEVEL_ARRAY_SIZE; i++){
	  Cmap_Levels_Array[i].black = 0;
	  Cmap_Levels_Array[i].white = 255;
     }

     /* Set the current scaleing to what was asked for. */
     Cmap_Levels_Array[Cmap_CurrentMap].black = black;
     Cmap_Levels_Array[Cmap_CurrentMap].white = white;
     
     /* Special default used for high contrast. */
     Cmap_Levels_Array[1].black = 128;
     Cmap_Levels_Array[1].white = 128;

     Cmap_Rampbase = rampbase;
     Cmap_rampsize = rampmax - rampbase + 1;


     /* kludge, but this need to be rewritten to */
     /* handle multipult ramps later.            */
     if (Cmap_rampsize > 256)
	  Cmap_rampsize = 256;
     if (Cmap_rampsize < 1)
	  Cmap_rampsize = 256;


     adjustcmap_pf(&black, &white, Cmap_Rampbase);
}


/****************************************************************************/
/* FUNCTION: cmap_index                                                     */
/* Changes the current color map to a new index.                            */
/* Returns the index if succsessful.                                        */
/* Returns a negative value for error.                                      */
/****************************************************************************/
int cmap_index(int index, int *black, int *white)
{
     if (index < 0)
	  return(-1);
     if (index > CMAP_LEVEL_ARRAY_SIZE - 1)
	  return(-1);

     Cmap_CurrentMap = index;

     cmap_get_levels(black, white);

     return(index);
}


/****************************************************************************/
/* FUNCTION: cmap_brightness                                                */
/* Adjusts the brightness by val.                                           */
/****************************************************************************/
void cmap_brightness(int val)
{
     int black;
     int white;

     black = Cmap_Levels_Array[Cmap_CurrentMap].black;
     white = Cmap_Levels_Array[Cmap_CurrentMap].white;

     black += val;
     white += val;

     /* here RAMPSIZE refers to max scaling value = 256 */
     if (white >= CMAP_RAMPSIZE){
	  black -= white -  CMAP_RAMPSIZE - 1; 
	  white = CMAP_RAMPSIZE - 1;
     }

     if (white < 1)
	  white = 1;

     Cmap_Levels_Array[Cmap_CurrentMap].black = black;
     Cmap_Levels_Array[Cmap_CurrentMap].white = white;

     adjustcmap_pf(&black, &white, Cmap_Rampbase);

}


/****************************************************************************/
/* FUNCTION: cmap_contrast                                                  */
/* Adjusts the contrast by val.                                             */
/****************************************************************************/
void cmap_contrast(int val)
{

     int black;
     int white;
     
     black = Cmap_Levels_Array[Cmap_CurrentMap].black;
     white = Cmap_Levels_Array[Cmap_CurrentMap].white;
     
     black += val;
     white -= val;

     if (black < 0)
	  black = 0;
     if (white > 255)
	  white = 255;

     if (black > white)
	  black = white;

     if (white < 1)
	  white = 1;


     Cmap_Levels_Array[Cmap_CurrentMap].black = black;
     Cmap_Levels_Array[Cmap_CurrentMap].white = white;
     
     adjustcmap_pf(&black, &white, Cmap_Rampbase);

}



/****************************************************************************/
/* FUNCTION: cmap_blacklevel                                                */
/* Adjusts the blacklevel by val.                                           */
/****************************************************************************/
void cmap_blacklevel(int val)
{
     int black;
     int white;

     black = Cmap_Levels_Array[Cmap_CurrentMap].black;
     white = Cmap_Levels_Array[Cmap_CurrentMap].white;

     black += val;

     if (black < 0)
	  black = 0;
     if (black > 254)
	  black = 254;

     if (black > white)
	  white = black + 1;

     Cmap_Levels_Array[Cmap_CurrentMap].black = black;
     Cmap_Levels_Array[Cmap_CurrentMap].white = white;

     adjustcmap_pf(&black, &white, Cmap_Rampbase);

}



/****************************************************************************/
/* FUNCTION: cmap_whitelevel                                                */
/* Adjusts the whitelevel by val.                                           */
/****************************************************************************/
void cmap_whitelevel(int val)
{
     int black;
     int white;
     
     black = Cmap_Levels_Array[Cmap_CurrentMap].black;
     white = Cmap_Levels_Array[Cmap_CurrentMap].white;
     
     white += val;
     
     if (white > 255)
	  white = 255;

     if (white < 1)
	  white = 1;

     if (white < black)
	  black = white - 1;

     Cmap_Levels_Array[Cmap_CurrentMap].black = black;
     Cmap_Levels_Array[Cmap_CurrentMap].white = white;
     
     adjustcmap_pf(&black, &white, Cmap_Rampbase);

}



/****************************************************************************/
/* FUNCTION: cmap_getlevels                                                 */
/* Writes the current black and white levels to the addressed integers.     */
/****************************************************************************/
void cmap_get_levels(int *black, int *white)
{

     *black = Cmap_Levels_Array[Cmap_CurrentMap].black;
     *white = Cmap_Levels_Array[Cmap_CurrentMap].white;

}
void cmap_new_levels(int black, int white)
{
     Cmap_Levels_Array[Cmap_CurrentMap].black = black;
     Cmap_Levels_Array[Cmap_CurrentMap].white = white;
     adjustcmap_pf(&black, &white, Cmap_Rampbase);
}

/****************************************************************************/
/* FUNCTION: cmap_setfalsecolor                                             */
/* Toggles false color on for val = non-zero or off for val = 0.            */
/****************************************************************************/
void cmap_toggle_falsecolor(void)
{
     int black;
     int white;
     
     cmap_get_levels( &black, &white);

     if (Cmap_Falsecolor)
	  Cmap_Falsecolor = 0;
     else
	  Cmap_Falsecolor = 1;

     adjustcmap_pf(&black, &white, Cmap_Rampbase);
}


void cmap_toggle_reverse(void)
{
  int black;
  int white;
  
  cmap_get_levels( &black, &white);

  if (Cmap_Reverse)
    Cmap_Reverse = 0;
  else
    Cmap_Reverse = 1;

  adjustcmap_pf(&black, &white, Cmap_Rampbase);
}


/*****************************************************************************
 *   Function adjustcmap_pf: Edits the color map by moving the black and white
 *                           levels and then ramping between them. 
 *
 *   Input:
 *     int *low  - Level that is black (0).
 *     int *high - Level that is white (255).
 *     int rampbase - Start of color map.
 *
 *   Output:
 *     Writes out colormap starting at rampbase.
 *
 *****************************************************************************/

void adjustcmap_pf(int *low, int *high, int rampbase)
{
     int i;
     int rampsize;  /* The size of the ramp needed */
     float slope,   /* The slope of the ramp. */
           point;   /* Temp variable to store colormap index as float. */
     int cmap[256]; /* Temp cmap, used for calculations. */
     int black, white;
     short red, green, blue;
     int revmap = 1;
     int rampmin, rampmax; /* added for smaller ramps. */
     float scale = (Cmap_rampsize / 255.0f);
	  
     /* Keep input variables in bounds. */
/*     if (*low > *high)
	  *low = *high;
     if (*high < *low)
	   *high = *low;
*/
     if (*low < 0) *low = 0;
     if (*low > 255) *low = 255;
     if (*high < 0) *high = 0;
     if (*high > 255) *high = 255;
     black = *low;
     white = *high;

     Cmap_Levels_Array[Cmap_CurrentMap].black = black;
     Cmap_Levels_Array[Cmap_CurrentMap].white = white;

     rampmin = black  * scale;
     rampmax = white * scale;

     if (Cmap_Reverse) revmap *= -1;
     if ( rampmin > rampmax){
	  revmap *= -1;
	  i = rampmin;
	  rampmin = rampmax;
	  rampmax = i;
	  i = black;
	  black = white;
	  white = i;
     }
     /* Write out black values to colormap. */
     for (i = 0; i < rampmin; i++)
	  cmap[i] = 0;
     /* Write out white values to colormap. */
     for (i = rampmax ; i < Cmap_rampsize; i++)
	  cmap[i] = 255;

     /* Keep rampsize to at least one. */
     rampsize = white - black;
     rampsize *= scale;
     if (rampsize < 1)
	  rampsize = 1;
     slope = 256.0 / (float)rampsize;

     /* Make the ramp. */
     for (i = rampmin; i < rampmax; i++){
	  point = (float)(i - rampmin) * slope;
	  cmap[i] = point;
     }
     cmap[0] = 0;
     cmap[255] = 255;

     if (revmap < 0)
	  for(i = 0; i < Cmap_rampsize; i++){
	       if (!cmap[i])
		    cmap[i] = 255;
	       else
	       if (cmap[i] > 127){
		    cmap[i] -= 128;
		    cmap[i] *= -1;
		    cmap[i] += 128;
	       }
	       else{
		    cmap[i] -= 128;
		    cmap[i] *= -1;
		    cmap[i] += 128;
	       }
	  }

     /* Write to color map */
     if (!Cmap_Falsecolor)
	       for (i = 0; i < Cmap_rampsize; i++)
		    mapcolor(i + rampbase, cmap[i], cmap[i], cmap[i]);
     else 
	  for (i = 0; i < Cmap_rampsize; i++){
	       cmap_falsecolor( (int)((float)cmap[i]/scale), 
			       &red, &green, &blue);
	       mapcolor(i + rampbase, red, green, blue);
	  }
}


/*****************************************************************************/
/* FUNCTION cmap_falsecolor()                                                */
/* Finds the false colorvalues for a given gray scale value.                 */
/*****************************************************************************/
void cmap_falsecolor(int gray, short *red, short *green, short *blue)
{
     /*  NEW false color 10-27-94, Jim Kremer                 */
     /*  blue = 255; for gray 0-64, blue = 0 for gray > 128)  */
     /*  green = 255 for gray > 64 & gray < 192               */
     /*  red = 0 for gray < 128,    red = 255 for gray > 192) */

     if (gray < 64){
	  *red   = 0;
	  *green = gray * 4;
	  *blue  = 255;
	  return;
     }

     if (gray < 128){
	  *red   = 0;
	  *green = 255;
	  *blue  =  ((gray * -1) + 127) * 4;
	  return;
     }

     if (gray < 192){
	  *red   = (gray - 128) * 4;
	  *green = 255;
	  *blue  = 0;
	  return;
     }


     *red   = 255;
     *green = ((gray * -1) + 255) * 4;
     *blue  = 0;
     return;

/* old version below */
/*
     *red = gray * 2;
     if (gray < 128 )
	  *red = 0;
     if (*red > 255)
	  *red = 255;

     
     if (gray > 128)
	  *green = 255 + 128 - gray;
     else 
	  *green = gray * 2;
     if (*green > 255)
	  *green = 255;
     if (*green < 0)
	  *green = 0;
     
     
     *blue = 255 - gray;
     *blue /= 2;
     
*/

}





/*****************************************************************************
 *   FUNCTION: adjustcmap
 *   Old function use adjustcmap_pf instead.
 *   Input:
 *     Contrast- Number of bins to fill same color with. (1,128)
 *     Brightness - Number to add to gray scale. (-256 , 255)
 *   Output:
 *     Writes out colormap starting at Rampbase out to RAMPSIZE.
 *     Returns 0 if ok, returns 1 if input is out of bounds.
 *****************************************************************************/


int adjustcmap(int contrast, int brightness, int rampbase)
{
     int i,j;
     int shade = 0;
     int bound = 0;
     int rampsize;
     float slope, point;
     int cmap[256];


     /* Keep values in bounds. */
     if (contrast < 0){
	  bound = 1;
	  contrast = 0;
     }
     if (contrast > 127){
	  bound = 1;
	  contrast = 127;
     }
     if (brightness < -255){
	  bound = 1;
	  brightness = -255;
     }
     if (brightness > 255){
	  bound = 1;
	  brightness = 255;
     }

     
     /* Adjust contrast */
     for (i = 0; i < 128; i++)
	  cmap[i] = 0;
     for (i = 128 ; i < 256; i++)
	  cmap[i] = 255;
     rampsize = 256 - ( 2 * contrast);
     slope = 256.0 / (256.0 - (2.0 * (float)contrast));
     
     for (i = contrast; i < (rampsize + contrast); i++){
	          point = (i - contrast) * slope;
		  cmap[i] = point;
	     }
     
     /* Adjust Brightness */
     if (brightness > 0)
	  for(i = 0; i < 256; i++){
	       if ((i + brightness) > 255)
		    cmap[i] = 255;
	       else
		    cmap[i] = cmap[i + brightness];
	  }
     if (brightness < 0)
	  for(i = 256; i > 0; i--){
	       if ((i + brightness) < 0)
				          cmap[i] = 0;
	       else
		    cmap[i] = cmap[i + brightness];
	  }
     
     /* Write to color map */
     for (i = 0; i < 256; i++)
	  mapcolor(i + rampbase, cmap[i], cmap[i], cmap[i]);
         return(bound);
     
}









