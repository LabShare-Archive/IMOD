/*   finegrain.h  -  declarations for finegrain.cpp
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef FINEGRAIN_H
#define FINEGRAIN_H

typedef struct draw_properties DrawProps;
typedef struct ViewInfo ImodView;

#define HANDLE_LINE_COLOR  (1)
#define HANDLE_MESH_COLOR  (1l << 1)
#define HANDLE_MESH_FCOLOR (1l << 2)
#define HANDLE_TRANS       (1l << 3)
#define HANDLE_2DWIDTH     (1l << 4)
#define HANDLE_3DWIDTH     (1l << 5)
#define HANDLE_VALUE1      (1l << 6)

void ifgPtContSurfSelected(int which);
void ifgGotoNextChange();
void ifgLineColorChanged(int r, int g, int b);
void ifgFillColorChanged(int r, int g, int b);
void ifgTransChanged(int value);
void ifgWidth2DChanged(int value);
void ifgWidth3DChanged(int value);
void ifgSymsizeChanged(int value);
void ifgColorChanged(int type, int r, int g, int b);
void ifgIntChanged(int type, int value);
void ifgSymtypeChanged(int symtype, bool filled);
void ifgEndChange(int type);
void ifgClearChange(int type);
void ifgGapChanged(bool state);
void ifgConnectChanged(int value);
void ifgShowConnectChanged(bool state);
void ifgStippleGapsChanged(bool state);
void ifgChangeAllToggled(bool state);
int ifgGetChangeAll();
void ifgDump();
void ifgHelp();
void ifgClosing();
int ifgShowConnections();
int ifgStippleGaps();
int ifgToggleGap(ImodView *vw, Icont *cont, int ptIndex, bool state);

void fineGrainOpen(ImodView *vw);
void fineGrainUpdate();

int ifgSelectedLineWidth(int width, int selected);
int ifgHandleNextChange(Iobj *obj, Ilist *list, DrawProps *defProps, 
                        DrawProps *ptProps, int *stateFlags, int *changeFlags,
                        int handleFlags, int selected, int scaleThick = 1);
int ifgHandleContChange(Iobj *obj, int co, DrawProps *contProps, 
                        DrawProps *ptProps, int *stateFlags, int handleFlags,
                        int selected, int scaleThick = 1);
void ifgHandleSurfChange(Iobj *obj, int surf, DrawProps *contProps, 
                         DrawProps *ptProps, int *stateFlags, int handleFlags);
int ifgHandleMeshChange(Iobj *obj, Ilist *list, DrawProps *defProps, 
                        DrawProps *curProps, int *nextItemIndex, int curIndex, 
                        int *stateFlags, int *changeFlags, int handleFlags);
void ifgHandleColorTrans(Iobj *obj, float r, float g, float b, int trans);
int ifgMeshTransMatch(Imesh *mesh, int defTrans, int drawTrans, int *meshInd, 
                      int skipEnds);
int ifgContTransMatch(Iobj *obj, Icont *cont, int *matchPt, int drawTrans,
                      DrawProps *contProps, DrawProps *ptProps,
                      int *stateFlags, int *allChanges, int handleFlags);
void ifgMakeValueMap(Iobj *obj, unsigned char cmap[3][256]);
int ifgSetupValueDrawing(Iobj *obj, int type);
int ifgGetValueSetupState();
void ifgResetValueSetup();
#endif

/*

$Log$
Revision 1.7  2009/03/10 04:37:57  mast
Functions for new options

Revision 1.6  2008/12/08 17:26:29  mast
Changes for scaling line thicknesses

Revision 1.5  2006/08/31 23:27:44  mast
Changes for stored value display

Revision 1.4  2006/02/28 15:20:28  mast
Back out test changes that went in by mistake

Revision 1.3  2006/02/27 19:45:24  mast
Added next change button

Revision 1.2  2005/09/11 19:30:05  mast
Added some functions for trans matching

Revision 1.1  2005/06/26 19:36:13  mast
Addition to program

*/
