/*  IMOD VERSION 2.30
 *
 *  $Id$
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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
    Revision 3.9  2003/04/18 20:10:15  mast
    changed name

    Revision 3.8  2003/04/18 20:05:52  mast
    Add function for rejecting meta key on Mac

    Revision 3.7  2003/04/17 19:27:13  mast
    keypad workaround for Mac

    Revision 3.6  2003/03/29 00:22:43  mast
    add forward declaration of ImodView

    Revision 3.5  2003/03/13 07:15:16  mast
    Add raise window function

    Revision 3.4  2003/03/13 01:17:46  mast
    Add function to convert keypad keys

    Revision 3.3  2003/02/27 23:45:24  mast
    Add function to truncate contour

    Revision 3.2  2003/02/10 20:41:55  mast
    Merge Qt source

    Revision 3.1.2.4  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 3.1.2.3  2003/01/23 20:06:07  mast
    remove declarations for imod_cont_edit

    Revision 3.1.2.2  2003/01/13 01:15:42  mast
    changes for Qt version of info window

    Revision 3.1.2.1  2002/12/09 17:51:38  mast
    add declartion of defaultKeyInput

    Revision 3.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/

#ifndef IMOD_INPUT_H
#define IMOD_INPUT_H
typedef struct ViewInfo ImodView;

void inputQDefaultKeys(QKeyEvent *event, ImodView *vw);

void inputDeletePoint(ImodView *vw);
void inputInsertPoint(ImodView *vw);
void inputModifyPoint(ImodView *vw);
void inputNextz(ImodView *vw);
void inputPrevz(ImodView *vw);
void inputNexty(ImodView *vw);
void inputPrevy(ImodView *vw);
void inputNextx(ImodView *vw);
void inputPrevx(ImodView *vw);
void inputNewContour(ImodView *vw);
void inputNewSurface(ImodView *vw);
void inputNextObject(ImodView *vw);
void inputPrevObject(ImodView *vw);
void inputAdjacentSurface(ImodView *vw, int direction);
void inputAdjacentContInSurf(ImodView *vw, int direction);
void inputNextContour(ImodView *vw);
void inputPrevContour(ImodView *vw);
void inputNextPoint(ImodView *vw);
void inputPrevPoint(ImodView *vw);
void inputMoveObject(ImodView *vw);
void inputDeleteContour(ImodView *vw);
void inputTruncateContour(ImodView *vw);
void inputFindValue(ImodView *vw);
void inputPointMove(ImodView *vw, int x, int y, int z);
void inputTranslate(ImodView *vw, int x, int y);
void inputFindMaxValue(ImodView *vw);
void inputNewObject(ImodView *vw);
void inputSaveModel(ImodView *vw);
void inputGhostmode(ImodView *vw);
void inputFirstPoint(ImodView *vw);
void inputLastPoint(ImodView *vw);
void inputNextTime(ImodView *vw);
void inputPrevTime(ImodView *vw);
void inputRestorePointIndex(ImodView *vw);
void inputKeepContourAtSameTime(ImodView *vw);
void inputContourDup(ImodView *vw);
void inputConvertNumLock(int &keysym, int &keypad);
void inputRaiseWindows();
bool inputTestMetaKey(QKeyEvent *event);
void inputUndoRedo(ImodView *vw, bool redo);

void inputSetModelTime(ImodView *vw, int time);

int mouse_in_box(int llx, int lly, int urx, int  ury, int mousex, int mousey);

#endif /* imod_input.h */
