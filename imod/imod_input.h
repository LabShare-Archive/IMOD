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
*/

#ifndef IMOD_INPUT_H
#define IMOD_INPUT_H
#ifdef __cplusplus
extern "C" {
#endif

void inputDefaultKeys(XKeyEvent *event, ImodView *vw);
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

void inputContourBreak(ImodView *vw);
void inputContourJoin(ImodView *vw, int x, int y);
void inputContourSurf(ImodView *vw);
void inputContourDup(ImodView *vw);
void contSurfShow(void);
void inputContourMove(void);
void inputContourMoveDialog(ImodView *vw);
void inputContourMoveDialogUpdate(void);

void inputSetModelTime(ImodView *vw, int time);

int mouse_in_box(int llx, int lly, int urx, int  ury, int mousex, int mousey);
#ifdef __cplusplus
}
#endif

#endif /* imod_input.h */
