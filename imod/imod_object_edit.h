/*  imod_object_edit.h - declarations for imod_object_edit.cpp
 *
 *  $Id$
 *  Log at end of file
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
int imod_object_edit_draw(void);
int  imod_object_edit();
void imod_object_color(int objNum);

#endif /* IMOD_OBJECT_EDIT_H */

/*
$Log$
Revision 4.4  2008/07/16 04:31:58  mast
Added call for point limit

Revision 4.3  2007/06/08 04:49:15  mast
Added call for planar flag

Revision 4.2  2006/06/09 20:25:39  mast
Added ability to display spheres on center section only

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.4  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.3  2003/01/06 15:45:34  mast
new object color stuf

Revision 1.1.2.2  2002/12/13 06:04:00  mast
moving imod_object_edit declaration to include file and removing argument

Revision 1.1.2.1  2002/12/05 16:30:22  mast
First addition to archive
*/
