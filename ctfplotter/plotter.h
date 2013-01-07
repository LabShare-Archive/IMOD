/*
 * plotter.h - Header for Plotter class
 *
 *  $Id$
 *
 */
#ifndef PLOTTER_H
#define PLOTTER_H

#include <QMap>
#include <QPixmap>
#include <QVector>
#include <QWidget>


class QLabel;
class QToolButton;
class PlotSettings;
class FittingDialog;
class AngleDialog;
class MyApp;
class QPushButton;

class Plotter : public QWidget
{
    Q_OBJECT
public:
  Plotter(MyApp *app, QWidget *parent = 0);
    ~Plotter();
    void setPlotSettings(const PlotSettings &settings);
    void setCurveData(int id, const QVector<QPointF> &data);
    void clearCurve(int id);
    QSize minimumSizeHint() const;
    QSize sizeHint() const;
    void manageLabels(double zero, double defocus, double def2, double defAvg,
                      int type);
    QToolButton *mTileButton;
    FittingDialog *mFittingDia;
    AngleDialog *mAngleDia;
    QVector<PlotSettings> mZoomStack;
    int mCurZoom;

public slots:
    void zoomIn();
    void zoomOut();
    void printIt();
    void rangeDiag();
    void angleDiag();
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

    MyApp *mApp;
    QLabel* mZeroLabel;
    QLabel* mDefocusLabel;
    QLabel* mDefoc2Label;
    QLabel* mDefocAvgLabel;
    QToolButton *mZoomInButton;
    QToolButton *mZoomOutButton;
    QToolButton *mPrintButton;
    QPushButton *mRangeButton;
    QPushButton *mAngleButton;
    QToolButton *mSaveButton;
    QToolButton *mHelpButton;
    QMap<int, QVector<QPointF> > mCurveMap;
    bool mRubberBandIsShown;
    QRect mRubberBandRect;
    QPixmap mPixmap;
    QPrinter *mPrinter;
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
