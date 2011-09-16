#include <qthread.h>
#include "imodconfig.h"

class ImodvIsosurface;
struct Mod_Point;

class IsoThread : public QThread
{
  public:
    IsoThread(int subsliceIndex, ImodvIsosurface *iso);
    struct Mod_Point *getVertex_xyz(){return mVertex_xyz;};
    b3dInt32 *getTriangle(){return mTriangle;};
    int getNVertex(){return mNVertex;};
    int getNTriangle(){return mNTriangle;};
    ~IsoThread(){};

  protected:
    void run();
  private:
    int mWhichSubslice;
    ImodvIsosurface *mIso;
    int mNVertex;
    int mNTriangle;
    struct Mod_Point *mVertex_xyz;
    b3dInt32 *mTriangle;
};
