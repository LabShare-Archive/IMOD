/*  IMOD VERSION 2.7.2
 *
 *  $Id$
 *
 *  Original Author: David Mastronarde   email: mast@colorado.edu
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
/*  $Author$

$Date$

$Revision$

Log at end of file
*/

#ifndef IMOD_CLIENT_MESSAGE_H
#define IMOD_CLIENT_MESSAGE_H

#define MESSAGE_NO_ACTION   0
#define MESSAGE_OPEN_MODEL  1
#define MESSAGE_SAVE_MODEL  2
#define MESSAGE_VIEW_MODEL  3
#define MESSAGE_QUIT  4
#define MESSAGE_RAISE_WINDOWS 5
#define MESSAGE_MODEL_MODE  6
#define MESSAGE_OPEN_KEEP_BW 7
#define MESSAGE_OPEN_BEADFIXER 8
#define MESSAGE_ONE_ZAP_OPEN 9
#define MESSAGE_RUBBERBAND 10
#define MESSAGE_OBJ_PROPERTIES 11
#define MESSAGE_NEWOBJ_PROPERTIES 12
#define MESSAGE_SLICER_ANGLES 13
#define MESSAGE_PLUGIN_EXECUTE 14

/* Definitions for plugins/special modules */
#define MESSAGE_BEADFIX_OPENFILE 1
#define MESSAGE_BEADFIX_REREAD   2

#include <qobject.h>
#include <qstring.h>
class QTimer;

class ImodClipboard : public QObject
{
  Q_OBJECT

 public:
  ImodClipboard();
  ~ImodClipboard() {};
  bool handleMessage();
  bool executeMessage();
  void sendResponse(int succeeded);
  unsigned int ourWindowID();

  QTimer *mClipTimer;
  QTimer *mClipHackTimer;

 public slots:
  void clipTimeout();
  void clipHackTimeout();
  void clipboardChanged();

 private:  
  bool mHandling;
  bool mExiting;
  QString mSavedClipboard;
};


#endif /* IMOD_CLIENT_MESSAGE_H */
/*
$Log$
Revision 3.10  2004/08/12 17:15:04  mast
Added message to get slicer angles

Revision 3.9  2004/05/31 02:15:15  mast
Added messages for setting object properties

Revision 3.8  2004/05/05 17:32:46  mast
Added message to get rubberband coordinates

Revision 3.7  2004/04/28 23:51:26  mast
Added message to open zap

Revision 3.6  2003/11/12 18:48:55  mast
Added method to get relevant window ID

Revision 3.5  2003/10/02 01:30:22  mast
Added message to open bead fixer

Revision 3.4  2003/08/01 05:52:54  mast
*** empty log message ***

Revision 3.3  2003/06/04 23:42:54  mast
Move message defines here to avoid recompiling everything

Revision 3.2  2003/02/27 19:22:40  mast
Qt version that works on windows

Revision 3.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 3.0.2.1  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 3.0  2002/09/27 20:35:04  rickg
Initital version of code moved from imod_menu_cb.c

*/
