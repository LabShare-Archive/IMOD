/*
 * Example simple plugin program.
 *
 */

#include "imodplug.h"

/*
 *   Edit the name of the plugin here.
 */
static char *plugName = "Test this plug";


/*
 *  This functions doesn't need to be changed.
 */
char *imodPlugInfo(int *type)
{
     *type = IMOD_PLUG_MENU;
     return(plugName);
}


/*
 *  Execute any function or program you wish with this function.
 *  Here we just print a message to the imod information window.
 *  see imodplug.h for a list of support functions.
 */
void imodPlugExecute(ImodView *inImodView)
{
     int tsize, xsize, ysize, zsize;

     /* 
      * Here we get the size of the image. 
      */
     ivwGetImageSize(inImodView, &xsize, &ysize, &zsize);
     tsize = ivwGetMaxTime(inImodView);

     /*
      * Now that we got the data; print it out to the 
      * imod information window.
      */
     wprint("The image you are modeling has a size of\n%d x %d x %d\n",
	    xsize, ysize, zsize);

     if (tsize)
	  wprint("The image has %d time points.\n", tsize);
     else
	  wprint("The image is not time dependant\n");


     wprint("The test plugin has been executed.\n");

     return;
}

