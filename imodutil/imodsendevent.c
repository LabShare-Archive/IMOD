/*  IMOD VERSION 2.7.2
 *
 *  imodsendevent.c -- To send ClientMessage events to imod
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Fine    *
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
    Revision 3.1  2002/09/13 21:56:06  mast
    Initial addition to package

*/
#include <X11/Xlib.h>
#include <string.h>
#include <stdio.h>

static void send_error(int num)
{
     fprintf(stderr, "ERROR: imodsendevent - failure to send event # %d\n", 
	     num);
     exit(2);
}

int errorHandler(Display *display, XErrorEvent *event)
{
     fprintf(stderr, "ERROR: imodsendevent - X error sending event\n");
     exit(3);
     return 1;
}

int main(int argc, char **argv)
{
     Display *display;
     Window win;
     int action;
     int nchar, npackets;
     XClientMessageEvent event;
     int indexStart, i, ipack;
     unsigned int ui, uilim;
     char lastchar;

     if (argc < 3 || argc > 4 ) {
	  fprintf(stderr, 
		  "ERROR: imodsendevent - Wrong number of arguments\n"
		  "   Usage: imodsendevent Window_ID action [text_string]\n");
	  exit(-1);
     }

     display = XOpenDisplay(NULL);
     if (!display) {
	  fprintf(stderr, "ERROR: imodsendevent - Cannot connect to X "
		  "server %s\n", XDisplayName(NULL));
	  exit(1);
     }

     XSetErrorHandler(errorHandler);
     win = (Window)atoi(argv[1]);
     action = atoi(argv[2]);
     nchar = 0;
     if (argc == 4) {
	  /* include the null byte at the end */
	  nchar = strlen(argv[3]) + 1;
     }
     npackets = (nchar + 19) / 20;
#ifdef DEBUG
     fprintf(stderr, "characters %d  packets %d\n", nchar, npackets);
#endif

     /* Fill in crucial information - the win IS needed */
     event.format = 32;
     event.type = ClientMessage;
     event.data.l[0] = action;
     event.data.l[1] = npackets;
     event.window = win;

     /* send event with permissive mask */
     if (!XSendEvent(display, win, True, 0xffffff, (XEvent *)(&event)))
	    send_error(1);

     /* send packets, all in separate events */
     indexStart = 0;
     for (ipack = 1; ipack <= npackets; ipack++) {
#ifdef DEBUG
	  fprintf(stderr, "ipack %d  nchar %d  npackets %d  indexStart %d\n",
		  ipack, nchar, npackets, indexStart);
#endif
	  event.format = 8;
	  event.type = ClientMessage;
	  event.window = win;
	  for (i = indexStart; (i < indexStart + 20 && i < nchar); i++)
	       event.data.b[i - indexStart] = *(argv[3] + i);
	  if (!XSendEvent(display, win, False, 0, (XEvent *)(&event)))
	       send_error(ipack + 2);
#ifdef DEBUG
	  lastchar = event.data.b[19];
	  event.data.b[19] = 0x00;
	  fprintf(stderr, "sent %s%c\n", event.data.b, lastchar); 
#endif
	  indexStart += 20;
     }

     /* Have to flush or close to actually send them */
     XCloseDisplay(display);
     exit(0);
}

