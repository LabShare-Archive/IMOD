/*   scalebar.h  -  declarations for scalebar.cpp
 *
 *  $Id$
 *
 *  $Log$
 *  Revision 1.1  2008/01/25 20:22:25  mast
 *  Added to program
 *
 *
 */                                                                           

#ifndef SCALEBAR_H
#define SCALEBAR_H

typedef struct scale_bar {
  bool draw;
  bool white;
  int minLength;
  int thickness;
  int position;
  int indentX;
  int indentY;
  bool useCustom;
  int customVal;
} ScaleBar;

float scaleBarDraw(int winx, int winy, float zoom, int background);
void scaleBarUpdate();
void scaleBarOpen();
ScaleBar *scaleBarGetParams();
void scaleBarRedraw();
void scaleBarClosing();

#endif
