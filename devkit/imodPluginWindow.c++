//-------------------------------------------------------
// Note calling C++ functions from C is not well defined
// and may not be possible on your system.
//-------------------------------------------------------


#include "imodPluginWindow.h"

//
// imod calls this function when it updates itself.
//
void imodPlugWindow::plugDraw_cb
(ImodView *, void *client, int drawflag)
{
    imodPlugWindow *win = (imodPlugWindow *)client;
    win->draw(drawflag);
}

//
// imod calls this function to close the window.
//
void imodPlugWindow::plugClose_cb
(ImodView *, void *client, int)
{
    imodPlugWindow *win = (imodPlugWindow *)client;
    delete win;
}

//
// The window manager calls this function to close the window.
//
void imodPlugWindow::quit_cb(Widget, XtPointer client, XtPointer)
{
    imodPlugWindow *win = (imodPlugWindow *)client;
    win->deleteControl();
}


//
// OK, so delete the window...
//
imodPlugWindow::~imodPlugWindow()
{
    if (window)
	XtDestroyWidget(window);
}

//
// Remove this from imod control.
//
void imodPlugWindow::deleteControl(void)
{
    ivwDeleteControl(view, control);
}

//
// Create the window.
//
imodPlugWindow::imodPlugWindow(ImodView *inImodView)
{
    Atom     wmclose;

    window = 0;
    setup  = 0;
    view = inImodView;
    if (!view) delete this;

    window = XtVaCreatePopupShell
	(plugName, topLevelShellWidgetClass, imodTopLevel(),
	 XmNvisual, imodVisual(),
	 XtNtitle, plugName,
	 NULL);

    if (!window) delete this;

    control = ivwNewControl
	(inImodView, imodPlugWindow::plugDraw_cb, 
	 imodPlugWindow::plugClose_cb, (XtPointer)this);

    if (!control) delete this;

    wmclose = XmInternAtom(imodDisplay(),
			   "WM_DELETE_WINDOW", False);
    XmAddWMProtocolCallback(window, wmclose, quit_cb,
			    (caddr_t)this);
}

//
// Override this function to have the window actually do something.
//
void imodPlugWindow::windowSetup(void)
{
    return;
}


//
// Look what we got.
//
void imodPlugWindow::openWindow()
{
    /* Open up the window. */
    if (window){
	if (!setup)
	    windowSetup();
	XtPopup(window, XtGrabNone);
    }
    else
	wprint("Warning %s : openWindow failed\n", plugName);
}


