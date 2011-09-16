/*   scalebar.h  -  declarations for scalebar.cpp
 *
 *  $Id$
 *
 *  $Log$
 *  Revision 1.4  2008/05/27 01:51:40  mast
 *  Added color ramp options
 *
 *  Revision 1.3  2008/03/06 00:11:55  mast
 *  Added option to make scale bars vertical
 *
 *  Revision 1.2  2008/01/26 05:51:59  mast
 *  Forgot the define
 *
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
  bool vertical;
  int position;
  int indentX;
  int indentY;
  bool useCustom;
  int customVal;
  bool colorRamp;
  bool invertRamp;
} ScaleBar;

float scaleBarDraw(int winx, int winy, float zoom, int background);
float scaleBarAssess(int winx, int winy, float zoom, int &pixlen, int &xst,
                     int &yst, int &xsize, int &ysize);
void scaleBarTestAdjust(int winx, int winy, float zoom);
void scaleBarUpdate();
void scaleBarOpen();
ScaleBar *scaleBarGetParams();
void scaleBarRedraw();
void scaleBarClosing();

#endif
