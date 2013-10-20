/*  object_edit.h - declarations for object_edit.cpp
 *
 *  $Id$
 */

#ifndef IMOD_OBJECT_EDIT_H
#define IMOD_OBJECT_EDIT_H
#include <qobject.h>
//Added by qt3to4:
#include <QKeyEvent>

class ColorSelector;

class ImodObjColor : public QObject
{
  Q_OBJECT

 public:
  ImodObjColor(int imodObj);
  ~ImodObjColor() {};

  ColorSelector *mSelector;
  int mObjNum;

  public slots:
   void newColorSlot(int red, int green, int blue);
  void doneSlot();
  void closingSlot();

  void keyPressSlot ( QKeyEvent * e );
  void keyReleaseSlot ( QKeyEvent * e );

};

/* GLOBAL FUNCTIONS */
void ioew_help(void);
void ioew_quit(void);
void ioew_closing(void);
void ioew_draw(int state);
void ioew_fill(int state);
void ioew_ends(int state);
void ioew_arrow(int state);
void ioew_linewidth(int value);
void ioew_open(int value);
void ioew_surface(int value);
void ioew_pointsize(int value);
void ioew_pointLimit(int value);
void ioew_nametext(const char *name);
void ioew_symbol(int value);
void ioew_symsize(int value);
void ioew_sphere_on_sec(int state);
void ioew_time(int state);
void ioew_planar(int state);
void ioew_outline(int state);
void ioew_fillTrans(int value);
void ioewCopyObj(int value);
int imod_object_edit_draw(void);
int  imod_object_edit();
void imod_object_color(int objNum);

#endif /* IMOD_OBJECT_EDIT_H */
