#include "imodPluginWindow.h"
#include "imodplug.h"

//-------------------------------------------------------
// Note calling C++ functions from C is not well defined
// and may not be possible on your system.
//-------------------------------------------------------

//
// This is the name of the menu item.
//
char *plugName = "Test C++ Plugin";

//
// This is the class to create when the menu is invoked.
//
void execute(ImodView *inImodView)
{
    imodPlugWindow *window = new imodPlugWindow(inImodView);
    window->openWindow();
    wprint("The C++ Test Plugin has executed");
}

/////////////////////////////////////////////////////////////////////////////
//
// This code interfaces the Plugin Class to imod.
//
extern "C" {

    char *imodPlugInfo(int *type)
	{
	    *type = IMOD_PLUG_MENU;
	    return(plugName);
	} 
    
    void imodPlugExecute(ImodView *inImodView)
	{
	    execute(inImodView);
	}
    
}
