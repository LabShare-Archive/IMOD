#ifndef MIDASGL_H
#define MIDASGL_H
#include "midas.h"
class MidasGL : public QGLWidget
{
  Q_OBJECT

 public:
  MidasGL(QGLFormat format, QWidget * parent = 0, 
	  const char * name = 0);
  ~MidasGL();

  void midas_clear();
  void draw();
  void fill_rgb(unsigned char *fbuf, unsigned long *tobuf, 
		int size, int channel, struct Midas_transform *tr);
  void draw_image(struct Midas_view *vw, unsigned long *image,
		  int llx, int lly, int urx, int ury, int *xdrawn, 
		  int *ydrawn);
  int fill_viewdata( struct Midas_view *vw);
  int update_slice_view(void);

protected:
  void initializeGL();
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void mouseMoveEvent ( QMouseEvent * e );

 private:
  bool mSkipClearOnDraw;
  bool mMousePressed;
};

#endif
