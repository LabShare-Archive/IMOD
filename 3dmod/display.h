/*   display.h  -  declarations for display.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMOD_DISPLAY_H
#define IMOD_DISPLAY_H

typedef struct ViewInfo ImodView;
typedef struct imod_application ImodApp;
typedef struct Mod_Model Imod;

typedef struct imodglvisual {
  int doubleBuffer;
  int rgba;
  int colorBits;      // Color index depth, or r + b + g
  int depthBits;
  int stereo;
  int validDirect;    // 1 for direct, -1 for invalid
  int dbRequested;
  int rgbaRequested;
  int depthEnabled;
  int alpha;
} ImodGLVisual;

typedef struct imodglrequest {
  int doubleBuffer;
  int rgba;
  int colorBits;      // Color index depth, or r + b + g
  int depthBits;
  int stereo;
  int alpha;
} ImodGLRequest;

void imodAssessVisuals();
int imodFindQGLFormat(ImodApp *ap, char **argv);
ImodGLVisual *imodFindGLVisual(ImodGLRequest request);

int  imod_display_init(ImodApp *ap, char **argv);
int  imod_color_init(ImodApp *ap);
void imod_cmap(Imod *m);
void imodSetObjectColor(int ob);
int  imodDraw(ImodView *vw, int flag);
int mapcolor(int color, int red, int green, int blue);
void mapNamedColors(void);
void resetGhostColor(void);
void customGhostColor(int red, int green, int blue);

#endif
