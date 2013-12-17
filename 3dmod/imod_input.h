//Added by qt3to4:
#include <QKeyEvent>
/* imod_input.h - declarations for imod_input.cpp  
 *
 *   Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*
  $Author$
    
  $Date$
  
  $Revision$
  
  Log at end
*/

#ifndef IMOD_INPUT_H
#define IMOD_INPUT_H
typedef struct ViewInfo ImodView;

#define INCOS_NEW_CONT -2
#define INCOS_NEW_SURF -1

void inputQDefaultKeys(QKeyEvent *event, ImodView *vw);

void inputDeletePoint(ImodView *vw);
void inputInsertPoint(ImodView *vw);
void inputModifyPoint(ImodView *vw);
void inputNextz(ImodView *vw, int step = 1);
void inputPrevz(ImodView *vw, int step = 1);
void inputPageUpOrDown(ImodView *vi, int shifted, int direction);
void inputNexty(ImodView *vw);
void inputPrevy(ImodView *vw);
void inputNextx(ImodView *vw);
void inputPrevx(ImodView *vw);
void inputNewContour(ImodView *vw);
void inputNewSurface(ImodView *vw);
void inputNewContourOrSurface(ImodView *vi, int surface, int timeLock);
void inputNextObject(ImodView *vw);
void inputPrevObject(ImodView *vw);
void inputAdjacentSurface(ImodView *vw, int direction);
void inputGotoSurface(ImodView *vw, int target);
void inputAdjacentContInSurf(ImodView *vw, int direction);
void inputNextContour(ImodView *vw);
void inputPrevContour(ImodView *vw);
void inputNextPoint(ImodView *vw);
void inputPrevPoint(ImodView *vw);
void inputMoveObject(ImodView *vw);
void inputDeleteContour(ImodView *vw);
void inputTruncateContour(ImodView *vw);
void inputToggleGap(ImodView *vw);
void inputFindValue(ImodView *vw);
void inputPointMove(ImodView *vw, int x, int y, int z);
void inputKeyPointMove(ImodView *vi, int keysym);
void inputTranslate(ImodView *vw, int x, int y);
void inputFindMaxValue(ImodView *vw);
void inputNewObject(ImodView *vw);
void inputSaveModel(ImodView *vw);
void inputGhostmode(ImodView *vw);
void inputFirstPoint(ImodView *vw);
void inputLastPoint(ImodView *vw);
void inputNextTime(ImodView *vw);
void inputPrevTime(ImodView *vw);
void inputMovieTime(ImodView *vw, int val);
void inputLimitingTime(ImodView *vw, int dir);
void inputRestorePointIndex(ImodView *vw, Iindex *oldIndex = NULL);
void inputKeepContourAtSameTime(ImodView *vw);
void inputContourDup(ImodView *vw);
void inputConvertNumLock(int &keysym, int &keypad);
void inputRaiseWindows();
bool inputTestMetaKey(QKeyEvent *event);
void inputUndoRedo(ImodView *vw, bool redo);

void inputSetModelTime(ImodView *vw, int time);

int mouse_in_box(int llx, int lly, int urx, int  ury, int mousex, int mousey);

#endif /* imod_input.h */
