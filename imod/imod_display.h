/*   imodv_display.h  -  declarations for imodv_display.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           
/*  $Author$

$Date$

$Log$
Revision 4.3  2004/11/02 20:15:04  mast
Added call to map all named colors

Revision 4.2  2004/06/06 21:27:21  mast
Eliminated stereo-command related items

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.4  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.3  2003/01/01 05:41:31  mast
add stereo testing to qt visual selection

Revision 1.1.2.2  2002/12/17 18:37:08  mast
Adding a declaration

Revision 1.1.2.1  2002/12/14 05:46:23  mast
Initial creation

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
} ImodGLVisual;

typedef struct imodglrequest {
  int doubleBuffer;
  int rgba;
  int colorBits;      // Color index depth, or r + b + g
  int depthBits;
  int stereo;
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
