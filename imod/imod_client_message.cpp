/*  IMOD VERSION 2.7.2
 *
 *  imod_client_meesage.c   Handles X client messages
 *
 *  Original author: David Mastronarde   email: mast@colorado.edu
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
Revision 1.1.2.3  2003/01/29 01:43:23  mast
Have it return true at end

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 23:05:57  mast
conversion to cpp

Revision 3.3.2.4  2003/01/23 19:54:09  mast
add include of imod_io

Revision 3.3.2.3  2003/01/13 01:15:42  mast
changes for Qt version of info window

Revision 3.3.2.2  2002/12/17 18:35:03  mast
Removing argument to call to open imodv

Revision 3.3.2.1  2002/12/05 16:23:52  mast
No changes - CVS detected as modified in branch

Revision 3.4  2002/12/03 15:53:57  mast
Make it set the forbid level before any action that can produce file
dialog box.  Have it distinguish cancel from error in error reports

Revision 3.3  2002/12/01 16:51:34  mast
Changes to eliminate warnings on SGI

Revision 3.2  2002/10/22 22:38:02  mast
Added include of imod_client_message.h, removed some obviously redundant
or unneeded includes carried over from imod_menu.c

Revision 3.1  2002/10/15 21:18:09  rickg
executeMessage() call moved so that it get called for all messages.

Revision 3.0  2002/09/27 20:30:06  rickg
Initital version of code moved from imod_menu_cb.c

*/

#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include "imod.h"
#include "imodv.h"
#include "imod_io.h"
#include "imod_info_cb.h"
#include "imod_client_message.h"

//  Module variables
static int message_action = MESSAGE_NO_ACTION;
static int packets_left = 0;
static int message_index;
static char *message_string = NULL;

static int debugMode = 0;

//  Module methods
static void executeMessage(void);


bool imodHandleClientMessage(XEvent *event)
{
  XClientMessageEvent *cmEvent = (XClientMessageEvent *)event;

  /* There are lots of other events around in Qt, only put out the
     first three error message in debug mode */

  if (cmEvent->format == 16) {
    if (debugMode)
      fprintf(stderr, "imodHandleClientMessage: received message in "
              "16-bit format\n");
    return FALSE;
  }

  /* If it is a string packet, and we are not expecting one, forget it */
  if (cmEvent->format == 8 && !packets_left) {
    if (debugMode)
      fprintf(stderr, "imodHandleClientMessage: received byte packet"
              " when not expecting one\n");
    return FALSE;
  }

  /* If it is an action packet without the signature, skip it */
  if (cmEvent->format == 32 && cmEvent->data.l[0] != ID_IMOD) {
    if (debugMode)
      fprintf(stderr, "imodHandleClientMessage: received client "
      "message without IMOD signature\n");
    return FALSE;
  }

  if (debugMode)
    fprintf(stderr, "imodHandleClientMessage: got IMOD client message\n");

  /* If it is an action packet and we are still expecting some string
     packets, clear out the last action */
  if (cmEvent->format == 32 && packets_left) {
    if (message_string)
      free(message_string);
    message_string = NULL;
    packets_left = 0;
    fprintf(stderr, "imodHandleClientMessage: received a new action"
            " message when still expecting more byte packets\n");
  }

  if (cmEvent->format == 32) {

    /* action message: get the action type and number of packets */
    message_action = cmEvent->data.l[1];
    packets_left = cmEvent->data.l[2];
    if (packets_left) {

      /* If there are packets coming, allocate memory, set up index,
         and return */
      message_string = (char *)malloc(20 * packets_left);
      message_index = 0;
      if (!message_string) {
        fprintf(stderr, "imodHandleClientMessage: failure to "
			    "obtain memory for %d packets\n", packets_left);
        packets_left = 0;
        message_action = MESSAGE_NO_ACTION;
      }
      return TRUE;
    }
  }
  else {

    /* string packet: add it to the string */
    memcpy(&message_string[message_index], cmEvent->data.b, 20);
    message_index += 20;
    if (--packets_left)
      return TRUE;
  }

  //  Execute the compiled message
  executeMessage();
  return TRUE;
}

static void executeMessage() {
  int returnValue;
  /* Execute the action */
  /* Each individual action is responsible for setting message_string NULL 
     if it has been freed already, and for setting OKtoFreeString if
     a string may still need freeing */
  switch (message_action) {

  case MESSAGE_OPEN_MODEL:
    if (!message_string) {
      fprintf(stderr, "imodHandleClientMessage: no filename sent"
              " with command to open model\n");
      break;
    }

    imod_info_forbid();
    imod_info_input();
    returnValue = openModel(message_string);
    if(returnValue == IMOD_IO_SUCCESS) {
      wprint("%s loaded.\n", Imod_filename);
    }
    else if(returnValue == IMOD_IO_SAVE_ERROR) {
      wprint("Error Saving Model. New model not loaded.\n");
    }
    else if(returnValue == IMOD_IO_SAVE_CANCEL) {
      wprint("Operation cancelled. New model not loaded.\n");
    }

    // The model does not exist yet.  Try creating a new model.
    else if(returnValue == IMOD_IO_DOES_NOT_EXIST) {
      returnValue = createNewModel(message_string);
      if(returnValue == IMOD_IO_SUCCESS) {
        
        wprint("New model %s created.\n", Imod_filename);
      }
      else {
        wprint("Could not create a new model %s.\n", Imod_filename);
      }
    }
    else if(returnValue == IMOD_IO_NO_ACCESS_ERROR) {
      wprint("Error opening model. Check file permissions\n.");
    }
    else {
      wprint("Unknown return code, new model not loaded!!\n");
    }
    imod_info_enable();
    
    break;

  case MESSAGE_SAVE_MODEL:
    imod_info_forbid();
    imod_info_input();
    App->cvi->imod->blacklevel = App->cvi->black;
    App->cvi->imod->whitelevel = App->cvi->white;
    SaveModel(App->cvi->imod);
    imod_info_enable();
    break;

  case MESSAGE_VIEW_MODEL:
    imod_autosave(App->cvi->imod);
    imodv_open();
    break;

  case MESSAGE_QUIT:
    imod_quit();
    break;

  default:
    fprintf(stderr, "imodHandleClientMessage: action %d not recognized\n"
            , message_action);
    break;
  }

  //  Free the message string if we allocated it above.
  if (message_string) {
    free(message_string);
    message_string = NULL;
  }
  message_action = MESSAGE_NO_ACTION;
}
