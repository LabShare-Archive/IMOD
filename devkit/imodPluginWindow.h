#include <X11/Xlib.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>

#include "imodplug.h"

extern char *plugName;

class imodPlugWindow
{
    int        control;
    int        setup;

  protected:
    ImodView  *view;
    Widget     window;

  public:
    imodPlugWindow(ImodView *inImodView);
    virtual ~imodPlugWindow();
    
    virtual void draw(int drawflag);
    virtual void openWindow(void);
    virtual void windowSetup(void);

    void deleteControl(void);

    static void plugDraw_cb(ImodView *inImodView, void *client, int drawflag);
    static void plugClose_cb(ImodView *vi, void *client, int reason);
    static void quit_cb(Widget w, XtPointer client, XtPointer call);
};





// These next three lines set the emacs edit mode to c++ instead of c.
// Local Variables:
// mode:c++
// End:
