/*   finegrain.h  -  declarations for finegrain.cpp
 */
 
/*  $Author$

$Date$

$Revision$

$Log$
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


void ifgPtContSurfSelected(int which);
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
void ifgDump();
void ifgHelp();
void ifgClosing();

void fineGrainOpen(ImodView *vw);
void fineGrainUpdate();

int ifgSelectedLineWidth(int width, int selected);
int ifgHandleNextChange(Iobj *obj, Ilist *list, DrawProps *defProps, 
                        DrawProps *ptProps, int *stateFlags, int *changeFlags,
                        int handleFlags, int selected);
int ifgHandleContChange(Iobj *obj, int co, DrawProps *contProps, 
                        DrawProps *ptProps, int *stateFlags, int handleFlags,
                        int selected);

#endif
