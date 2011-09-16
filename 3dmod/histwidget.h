#include <qwidget.h>
#include <qpixmap.h>
//Added by qt3to4:
#include <QPaintEvent>
class HistWidget: public QWidget
{
  Q_OBJECT
  public:
    float hist[256];
    float maxHist;
    float minHist;
    int min;
    int max;
    HistWidget(QWidget*);
    void setHistMinMax();
    void setMinMax(int minIn, int maxIn){ min=minIn; max=maxIn;}; 
    float *getHist(){return hist;};
    float computePercentile(float percentile);
  protected:
   void paintEvent(QPaintEvent *event);
};
