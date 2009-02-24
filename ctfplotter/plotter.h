#ifndef PLOTTER_H
#define PLOTTER_H

#include <QMap>
#include <QPixmap>
#include <QVector>
#include <QWidget>


class QLabel;
class QToolButton;
class PlotSettings;
class RangeDialog;
class AngleDialog;


class Plotter : public QWidget
{
    Q_OBJECT
public:
    Plotter(QWidget *parent = 0);
    ~Plotter();
    void setPlotSettings(const PlotSettings &settings);
    void setCurveData(int id, const QVector<QPointF> &data);
    void clearCurve(int id);
    QSize minimumSizeHint() const;
    QSize sizeHint() const;
    QLabel* zeroLabel;
    QLabel* defocusLabel;
    QToolButton *tileButton;
    RangeDialog *rDialog;
    AngleDialog *aDialog;
    QVector<PlotSettings> zoomStack;
    int curZoom;

public slots:
    void zoomIn();
    void zoomOut();
    void printIt();
    void rangeDiag();
    void angleDiag();
    void saveIt();
    void ctfHelp();

protected:
    void paintEvent(QPaintEvent *event);
    void resizeEvent(QResizeEvent *event);
    void mousePressEvent(QMouseEvent *event);
    void mouseDoubleClickEvent(QMouseEvent *event);
    void mouseMoveEvent(QMouseEvent *event);
    void mouseReleaseEvent(QMouseEvent *event);
    void keyPressEvent(QKeyEvent *event);
    void wheelEvent(QWheelEvent *event);

private:
    void updateRubberBandRegion();
    void refreshPixmap();
    void drawGrid(QPainter *painter, bool onScreen);
    void drawCurves(QPainter *painter);

    enum { Margin = 40 };

    QToolButton *zoomInButton;
    QToolButton *zoomOutButton;
    QToolButton *printButton;
    QToolButton *rangeButton;
    QToolButton *angleButton;
    QToolButton *saveButton;
    QToolButton *helpButton;
    QMap<int, QVector<QPointF> > curveMap;
    bool rubberBandIsShown;
    QRect rubberBandRect;
    QPixmap pixmap;
    QPrinter *printer;
};

class PlotSettings
{
public:
    PlotSettings();

    void scroll(int dx, int dy);
    void adjust();
    double spanX() const { return maxX - minX; }
    double spanY() const { return maxY - minY; }

    double minX;
    double maxX;
    int numXTicks;
    double minY;
    double maxY;
    int numYTicks;

private:
    void adjustAxis(double &min, double &max, int &numTicks);
};
#endif
