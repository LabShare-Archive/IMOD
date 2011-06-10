#ifndef MIDASGL_H
#define MIDASGL_H
#include "midas.h"
//Added by qt3to4:
#include <QMouseEvent>
#include <QTime>
class MidasGL : public QGLWidget
{
  Q_OBJECT

 public:
  MidasGL(QGLFormat format, QWidget * parent = 0, 
	  const char * name = 0);
  ~MidasGL();

  void midas_clear();
  void draw();
  void fill_rgb(unsigned char *fbuf, b3dUInt32 *tobuf, 
		int size, int channel, struct Midas_transform *tr);
  void draw_image(struct Midas_view *vw, b3dUInt32 *image,
		  int llx, int lly, int urx, int ury, int *xdrawn, 
		  int *ydrawn);
  int fill_viewdata( struct Midas_view *vw);
  int update_slice_view(void);
  void manageMouseLabel(const char *string);
  void drawStar(float xcen, float ycen, float censize);
  int nearestControlPoint(int iz, float &mindist);
  void newCurrentControl(int newcur, bool updateSlice);
  void attachControlPoint();

protected:
  void initializeGL();
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void mouseMoveEvent ( QMouseEvent * e );

 private:
  bool mMousePressed;
  QTime mBut1downt;
};

#endif
