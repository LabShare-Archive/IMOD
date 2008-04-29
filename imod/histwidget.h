#include <qwidget.h>
#include <qpixmap.h>
class HistWidget: public QWidget
{
  Q_OBJECT
  public:
    float hist[256];
    float maxHist;
    float minHist;
    HistWidget(QWidget*);
    void setMinMax();
    float *getHist(){return hist;};
    float computePercentile(float percentile);
  protected:
   void paintEvent(QPaintEvent *event);
};
