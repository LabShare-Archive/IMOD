/*  IMOD VERSION 2.50
 *
 *  imodv_menu.c -- menu setup for imodv main window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.1  2002/11/05 23:27:46  mast
    Changed copyright notice to use defined lab name and years

*/

#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/CascadeB.h>
#include <Xm/Separator.h>
#include <dia.h>
#include "imodv.h"
#include "b3dgfx.h"

/* Callback function for setting the background color in
 * the model view window.
 */
static int bgColorOpen = False;
void imodv_setbgcolor(Widget w, XtPointer client, XtPointer call)
{
     DiaColorCallbackStruct *cbs = (DiaColorCallbackStruct *)call;
     ImodvApp *a = (ImodvApp *)client;

     /* Keep track if it is closed */
     if (cbs->reason == DIA_OK || cbs->reason == DIA_CANCEL)
       bgColorOpen = False;

     if (ImodvClosed) return;

     if (cbs->reason == DIA_SLIDER_DRAG)
	  return;
     if (cbs->reason == DIA_SLIDER_CHANGED)
	  return;
     if (cbs->reason == DIA_CANCEL)
	  return;
     if (cbs->reason == DIA_HELP)
	  return;

     a->rbgcolor.red   = cbs->red *   255;
     a->rbgcolor.green = cbs->green * 255;
     a->rbgcolor.blue  = cbs->blue *  255;
     if (a->cindex)
	  imodvMapColor(a->gfx, a->bindex, cbs->red, cbs->green, cbs->blue);

     imodvDraw(a);
     return;
}
				 

static int load_model(ImodvApp *a)
{
     Imod **tmoda;
     Imod *tmod;
     int i, ob, co;
     char *fname;

     fname = dia_filename("Enter model file to load.");
     if (!fname)
	  return(-1);
     if (ImodvClosed) return -1;

     tmod = imodRead(fname);
     if (!tmod)
	  return(-1);

     /* DNM 6/20/01: find out max time and set current time */
     tmod->tmax = 0;
     for (ob = 0; ob < tmod->objsize; ob++)
	  for (co = 0; co < tmod->obj[ob].contsize; co++)
	       if (tmod->tmax < tmod->obj[ob].cont[co].type)
		    tmod->tmax = tmod->obj[ob].cont[co].type;
     tmod->ctime = tmod->tmax ? 1 : 0;

     tmoda = (Imod **)malloc(sizeof(Imod *) * (a->nm + 1));
     for (i = 0; i < a->nm; i++)
	  tmoda[i] = a->mod[i];
     tmoda[i] = tmod;
     if (a->nm)
	  free(a->mod);
     a->mod = tmoda;

     /*     a->cm = a->nm; */
     a->nm++;
     /*     a->imod = tmod; */

     /* DNM: changes for storage of object properties in view and 
	relying on default scaling */

     imodViewStore(tmod, 0);
     
     if (!tmod->cview){
	 imodViewModelDefault(tmod, tmod->view);
     }else
         imodViewUse(tmod);

     imodvSelectModel(a, a->nm - 1);
     return(0);
 }


static void save_model_cb(Widget w,  XtPointer client, XtPointer call)
{
     /* DNM: added rename of existing file and improved	error checks */

     ImodvApp *a = (ImodvApp *)client;
     char *filename = (char *)call;
     FILE *fout;
     int len, error;
     char *nfname1;

     /* DNM 8/4/01: store the current view when saving, if appropriate */
     imodvAutoStoreView(a);

     len = strlen(filename)+1;

     nfname1 = (char *)malloc(len + 1);
     sprintf(nfname1, "%s~", filename);
     rename(filename, nfname1);

     fout = fopen(filename, "w");
     if (fout){
	  a->imod->file = fout;
	  error = imodWrite(Imodv->imod, fout);
	  fflush(fout);
	  fclose(fout);
	  Imodv->imod->file = NULL;

	  if (!error) {
	       if (a->imod->fileName)
		    free(a->imod->fileName);
	       a->imod->fileName = (char *)malloc(len);
	       if (a->imod->fileName)
		    memcpy(a->imod->fileName, filename, len);
	  
	       if ((strlen(filename)+1) < IMOD_STRSIZE)
		    memcpy(a->imod->name, filename, strlen(filename)+1);
	       else
		    a->imod->name[0] = 0x00;
	  
	       dia_puts("Model file saved.");
	  }
     } else {
          error = 1;
     }
     if (error) {
          dia_err("Error writing model; attempting to restore backup file");
          rename(nfname1, filename);
     }
     free(nfname1);
}

/*********************************/
/* Edit menu callback functions. */
static void menu_objed_cb(Widget w, XtPointer client, XtPointer call)
{
     objed(Imodv);
}
static void menu_controls_cb(Widget w, XtPointer client, XtPointer call)
{
     imodv_control(Imodv, 1);
}
static void menu_objlist_cb(Widget w, XtPointer client, XtPointer call)
{
     imodvObjectListDialog(Imodv, 1);
}

/* DNM 12/1/02: make this the single path to opening the window, and have
   it keep track of whether window is open or not, and return if it is */
void menu_bgcolor_cb(Widget w, XtPointer client, XtPointer call)
{
     short red, green, blue;
     if (bgColorOpen)
       return;

     red   = Imodv->rbgcolor.red / 256;
     green = Imodv->rbgcolor.green / 256;
     blue  = Imodv->rbgcolor.blue / 256;
     dia_setcolor(red, green, blue, "Imodv background color.",
		  imodv_setbgcolor, Imodv);
     bgColorOpen = True;
}
static void menu_model_cb(Widget w, XtPointer client, XtPointer call)
{
    imodvModelEditDialog(Imodv, 1);
}
static void menu_views_cb(Widget w, XtPointer client, XtPointer call)
{
     imodvViewEditDialog(Imodv, 1);
}
static void menu_image_cb(Widget w, XtPointer client, XtPointer call)
{
     imodvImageEditDialog(Imodv, 1);
}
void imodv_edit_cb(Widget w, XtPointer client, XtPointer call)
{
     int item = (int)client;
     int mode;
     
     switch(item){
	case 0: /* object edit */
	  objed(Imodv);
	  break;
	case 1: /* controls */
	  imodv_control(Imodv, 1);
	  break;
	case 2: /* object list */
	  imodvObjectListDialog(Imodv, 1);
	  break;
	case 3: /* background color */
	  menu_bgcolor_cb(w, client, call);
	  break;
	case 4: /* models */
	  imodvModelEditDialog(Imodv, 1);
	  break;
	case 5: /* views */
	  imodvViewEditDialog(Imodv, 1);
	  break;

	case 6: /* image */
	  imodvImageEditDialog(Imodv, 1);
	  break;

     }
     return;
}

/*********************************/
/* help menu callback functions. */
void imodv_help_menu_cb(Widget w, XtPointer client, XtPointer call)
{
	 dia_vasmsg
	       ("Imodv Help for menus\n",
		"---------------------------------\n",
		"File\n",
		"    Open Model     - Load a new model to view.\n",
		"    Save Model     - Save the current model.\n",
		"    Save Model As. - Save model under a different name.\n",
		"    Snap RGB As... - Save a snapshot to a specified RGB file.\n",
		"    Snap TIFF As.. - Save a snapshot a specified TIFF file.\n",
		"    Zero Snap File # - Reset the counter for snapshot files to 0.\n",
		"    Movie...       - Program a sequence of displays and save them.\n",
		"    About          - Info about this program.\n",
		"    Quit           - Quit this program.\n",
		
		"\nEdit\n",
		"    Objects...     - Open the object edit dialog.\n",
		"    Controls...    - Open the display control dialog.\n",
		"    Object List... - Show objects by name with On/Off buttons.\n",
		"    Background...  - Change the background color.\n",
		"    Models...      - Control the display of multiple models.\n",
		"    Views...       - Open a dialog to save and restore views.\n",
		"    Image...       - Display an image slice on the model.\n",
		
		"\nView - Rendering Options\n",
		"    Double Buffer  - Change between double and single\n",
		"                     buffer visual.\n",
		"    Lighting       - Turn rendering with a light on or off.\n"
		"    Wireframe      - Render data in wireframe only.\n",
		"    Low Res        - Display low resolution mesh.\n",
		"    Stereo...      - Open stereo control dialog.\n",
		"    Depth Cue...   - Control dimming of display with distance.\n",
		NULL);

}

void imodv_help_key_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg
	  ("Imodv Help for Keyboard Commands\n",
	   "----------------------------------------------------------\n",
	   "\nKeys   | Command \n",
	   "----------------------------------------------------------\n",
	   "Arrows | Translate model in x and y\n",
	   "Page   | Up and Down keys translate model in z\n",
	   "Keypad | Rotates model in x, y and z. the '5' key toggles\n",
	   "       | movie mode on/off\n"
	   " Esc/q | Quit this program\n",
	   "  s    | Toggle stereo mode\n",
	   "  S    | Snapshot image as an RGB file to imodvnnnn.rgb\n",
	   "Ctrl-S | Snapshot image as a TIFF file to imodvnnnn.tif\n",
	   "  o    | Output transformation information\n",
	   "  c    | Output clipping plane information\n",
	   " -/=   | Decrease/Increase zoom\n",
	   " _/+   | Decrease/Increase zoom by big steps\n"
	   "  m    | Open movie control window\n",
	   "  O    | Open Object Edit window\n",
	   "  C    | Open controls window\n",
	   "  B    | Open background color window\n",
	   "  L    | Open Object List window\n",
	   "  M    | Open model selection window\n",
	   "  v    | Open view editing window\n",
	   "  i    | Open image overlay control window\n",
       	   "  b    | Toggle double buffering\n",
       	   "  r    | Toggle low resolution drawing of mesh and spheres\n",
       	   " g/G   | Increase/Decrease the quality of sphere drawing\n",
	   " [/]   | Adjust parallax for stereo viewing\n",
	   "  l    | Invert the parallax angle\n",
	   " ,/.   | Decrease/Increase rotation increment and thus speed\n",
	   " 1/2   | Decrease/Increase time for 4D models\n",
       	   "  8    | Toggle displaying all models or one model\n",
	   " 9/0   | Previous/Next model\n",
	   "----------------------------------------------------------\n",
	   NULL);
}

void imodv_help_control_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg
	  ("Imodv Help for Mouse Controls\n",
	   "----------------------------------------------------------\n",
	   "Left Mouse Button Drag\n",
	   "\tThe left mouse button moves the model when held down.\n\n",
	   "\tWhen the Ctrl key is held down the left mouse button "
	   "moves the current object clipping plane.\n\n",
	   "Middle Mouse Button Drag\n",
	   "\tThe Middle mouse button rotates the model around an axis "
	   "perpendicular to the direction of motion of the mouse.\n\n"
	   "\tWhen the shift key is held down the middle ",
	   "mouse button rotates the light source instead.\n\n",
	   "\tWhen the Ctrl key is held down the middle mouse button "
	   "rotates the current object clipping plane instead.\n\n",
	   "Right Mouse Button\n",
	   "\tThe right mouse button controls the pop up menus when",
	   "imodv is started with the -noborder option.\n\n",
	   "\tWhen running Model View from Imod, clicking on a point in the "
	   "model with the right mouse button will select the nearest, "
	   "frontmost point in the model as the current model point within "
	   "Imod.\n",
	   NULL);

}

void imodv_help_cb(Widget w, XtPointer client, XtPointer call)
{
     int item = (int)client;
     int mode;

     switch(item){
	case 0:
	 imodv_help_menu_cb(w, client, call);
	  break;
	case 1:
	 imodv_help_key_cb(w, client, call);
	 break;
	case 2:
	 imodv_help_control_cb(w, client, call);
	  break;
     }
     return;
}

void imodv_popup_cb(Widget w, XtPointer client, XtPointer call)
{
     return;
}

/****************************************************************************/
/* The FILE MENU */

static void file_open_cb(Widget w, XtPointer client, XtPointer call)
{
     if (load_model(Imodv))
	  dia_err("No model loaded.");
}


void imodv_file_save_cb(Widget w, XtPointer client, XtPointer call)
{
     /* DNM: added rename of existing file to backup.  Also eliminated use of
	Imodv pointer in favor of a->, and the double save (!?!), and added
	error checks */

     int len, error;
     char *nfname1;
     FILE *fout = NULL;
     ImodvApp *a = (ImodvApp *)client;
     char *filename = a->imod->fileName;

     /* DNM 8/4/01: store the current view when saving, if appropriate */
     imodvAutoStoreView(a);

     len = strlen(filename)+1;

     nfname1 = (char *)malloc(len + 1);
     sprintf(nfname1, "%s~", filename);
     rename(filename, nfname1);


     if (a->imod->fileName)
	  fout = fopen(a->imod->fileName, "w");

     if (fout){
          a->imod->file = fout;
	  error = imodWrite(a->imod, a->imod->file);
/*	  error = imodWrite(Imodv->imod, Imodv->imod->file); */
	  fflush(fout);
	  fclose(fout);
	  a->imod->file = NULL;
	  if (!error)
	       dia_puts("Model file saved.");
     } else
	  error = 1;

     if (error) {

	  dia_err("File not saved, bad filename or error;"
		  " attempting to restore backup file.");
	  rename (nfname1, filename);
     }
     free(nfname1);
}

static void file_saveas_cb(Widget w, XtPointer client, XtPointer call)
{
     diaEasyFileAct("Model Save FILE",
		    save_model_cb, (XtPointer)Imodv);
}

static void named_snapshotRGB_cb(Widget w,  XtPointer client, XtPointer call)
{
     char *filename = (char *)call;
     imodv_auto_snapshot(filename, SnapShot_RGB);
}
static void file_snapshotRGB_cb(Widget w, XtPointer client, XtPointer call)
{
     diaEasyFileAct("RGB Snapshot Save FILE",
		    named_snapshotRGB_cb, (XtPointer)Imodv);
}

static void named_snapshotTIF_cb(Widget w,  XtPointer client, XtPointer call)
{
     char *filename = (char *)call;
     imodv_auto_snapshot(filename, SnapShot_TIF);
}
static void file_snapshotTIF_cb(Widget w, XtPointer client, XtPointer call)
{
     diaEasyFileAct("TIFF Snapshot Save FILE",
		    named_snapshotTIF_cb, (XtPointer)Imodv);
}

static void file_movie_cb(Widget w, XtPointer client, XtPointer call)
{
     imodvMovieDialog(Imodv, 1);
}

static void file_about_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg
	  ("Imodv OpenGL Version ",
	   VERSION_NAME,
	   ", originally written by James Kremer and revised",
	   "by David Mastronarde\n",
	   "Copyright (C)",COPYRIGHT_YEARS,"by",LAB_NAME1,"\n",LAB_NAME2,
	   "& Regents of the University of Colorado\n\n",
	   NULL);
}

static void file_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     stereoHWOff();
     imodv_exit(a);
}


static Widget getPulldown(Widget parent, char *name, char mnemonic)
{
    Arg args[4];
    int n = 0;
    Widget pullDown, cascade;
    XmString str;
    
    XtSetArg(args[n], XmNdepth, Imodv->depth); n++;
    XtSetArg(args[n], XmNvisual,   Imodv->visual); n++;
    XtSetArg(args[n], XmNcolormap, Imodv->cmap);n++;

    pullDown = XmCreatePulldownMenu
	(parent, "_pulldown", args, n);
    str = XmStringCreateSimple(name);

    cascade = XtVaCreateManagedWidget
	("File", xmCascadeButtonWidgetClass, parent,
	 XmNsubMenuId, pullDown,
	 XmNlabelString, str,
	 XmNmnemonic, mnemonic,
	 XmNdepth,    Imodv->depth,
	 XmNvisual,   Imodv->visual,
	 XmNcolormap, Imodv->cmap,
	 NULL);

    XmStringFree(str);
    return(pullDown);
}

static Widget getItem(Widget parent, char *name, char mnemonic,
		      XtCallbackProc cb)
{
    Widget item = XtVaCreateManagedWidget
	(name, xmPushButtonWidgetClass, parent, 
	 XmNdepth,    Imodv->depth,
         XmNvisual,   Imodv->visual,
         XmNcolormap, Imodv->cmap,
	 NULL);
    if (mnemonic)
	XtVaSetValues (item, XmNmnemonic, mnemonic, NULL);
    XtAddCallback(item, XmNactivateCallback, cb, Imodv);

    return(item);
}
static Widget makeItem(Widget parent, char *name, char mnemonic,
		       char *accel, char *acceltext,
		       XtCallbackProc cb,  XtPointer data)
{
    XmString accelText = XmStringCreateSimple(acceltext);
    Widget item = XtVaCreateManagedWidget
	(name, xmPushButtonWidgetClass, parent,
	 XmNaccelerator, accel,
	 XmNacceleratorText, accelText,
	 XmNdepth,    Imodv->depth,
         XmNvisual,   Imodv->visual,
         XmNcolormap, Imodv->cmap,
	 NULL);
    if (mnemonic)
	XtVaSetValues (item, XmNmnemonic, mnemonic, NULL);
    XtAddCallback(item, XmNactivateCallback, cb, data);
    XmStringFree(accelText);
    return(item);
}

static Widget imodv_create_file_menu(Widget parent)
{
    Widget item;
    Widget fileMenu;

    fileMenu = getPulldown(parent, "File", 'F');

    item = makeItem(fileMenu, "Open Model", 'O',
		    "Ctrl<Key>O", "Ctrl-O",
		    file_open_cb, Imodv);
    if (!Imodv->standalone) XtSetSensitive(item, False);

    /* DNM: eliminate accelerator key now that it works */
    item = getItem(fileMenu, "Save Model", 'S', 
		    imodv_file_save_cb);
    if (!Imodv->standalone) XtSetSensitive(item, False);

    item = makeItem(fileMenu, "Save Model As...", 'A', 
		   "Ctrl<Key>A", "Ctrl-A",
		   file_saveas_cb, Imodv);
    if (!Imodv->standalone) XtSetSensitive(item, False);

    getItem(fileMenu, "Snap RGB As...", 'R', file_snapshotRGB_cb);
    getItem(fileMenu, "Snap TIFF As...", 'T', file_snapshotTIF_cb);
    getItem(fileMenu, "Zero Snap File #", 'Z', imodv_reset_snap_cb);
    getItem(fileMenu, "Movie...", 'M', file_movie_cb);
    getItem(fileMenu, "About", 'b', file_about_cb);
    
    item = XtVaCreateManagedWidget
	("separator", xmSeparatorWidgetClass,  fileMenu, NULL);
    
    if (Imodv->standalone)
	makeItem(fileMenu, "Quit", 'Q', "Ctrl<Key>Q", "Ctrl-Q", 
		 file_quit_cb, Imodv);
    else
	makeItem(fileMenu, "Close Window", 'W', 
		 "Ctrl<Key>W", "Ctrl-W",
		 file_quit_cb, Imodv);

    return 0;
}

static Widget imodv_create_edit_menu(Widget parent)
{
     Widget menu, item;
     
     menu = getPulldown(parent, "Edit", 'E');
     
     getItem(menu, "Objects...",    'O', menu_objed_cb);
     getItem(menu, "Controls...",   'C', menu_controls_cb);
     getItem(menu, "Object List...", 'L', menu_objlist_cb);
     getItem(menu, "Background...", 'B', menu_bgcolor_cb);
     getItem(menu, "Models...",     'M', menu_model_cb);
     getItem(menu, "Views...",         'V', menu_views_cb);

     if (!Imodv->standalone)
	 getItem(menu, "Image...", 'I', menu_image_cb);

     return(menu);
}

/****************************************************************************/
static void stereo_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     imodvStereoEditDialog(a, 1);
}
static void doubleBuffer_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     imodv_setbuffer(a);
}
static void lighting_cb(Widget w, XtPointer client, XtPointer call)
{
    ImodvApp *a = (ImodvApp *)client;
     Boolean lightSet = XmToggleButtonGetState(w);

     if (!lightSet){
	 Imodv->imod->view->world &= ~VIEW_WORLD_LIGHT;
	 a->lighting = 0;
     }else{
	 Imodv->imod->view->world |= VIEW_WORLD_LIGHT;
	 a->lighting = 1;
     }

     imodvDraw(a);
}
static void wireframe_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     Boolean wireSet = XmToggleButtonGetState(w);

     if (!wireSet){
	  Imodv->imod->view->world &= ~VIEW_WORLD_WIREFRAME;
	  a->wireframe = 0;
     }else {
	  a->wireframe = 1;
	  Imodv->imod->view->world |= VIEW_WORLD_WIREFRAME;
     }

     imodvDraw(a);
}
static void lowres_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     Boolean resSet = XmToggleButtonGetState(w);

     if (!resSet){
	  Imodv->imod->view->world &= ~VIEW_WORLD_LOWRES;
	  a->lowres = 0;
     }else {
	  a->lowres = 1;
	  Imodv->imod->view->world |= VIEW_WORLD_LOWRES;
     }

     imodvDraw(a);
}

#define DEPTHCUE_DIALOG
#ifdef DEPTHCUE_DIALOG
void imodvDepthCueEditDialog(ImodvApp *a, int state);
#endif
static void depthcue_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
#ifdef DEPTHCUE_DIALOG
     imodvDepthCueEditDialog(a, 1);
#else
     if (a->depthcue) a->depthcue = 0;
     else a->depthcue = 1;
     imodvDraw(a);
#endif
}

/*
 * imodv plugin menu additions.
 */
#ifdef __linux
#define NOPLUGS
#endif
#ifdef __vms
#define NOPLUGS
#endif
#ifdef SVR3
#define NOPLUGS
#endif


#ifndef NOPLUGS
#include <dlfcn.h>
#include <dirent.h>
#include <string.h>
#endif

static void addImodvViewPlugins(Widget w, ImodvApp *a)
{
#ifndef NOPLUGS
     void *handle;
     void (*fptr)(Widget,ImodvApp *);
     char soname[256];
     char *plugdir = getenv("IMOD_PLUGIN_DIR");
     if (!plugdir) return;

     XtVaCreateManagedWidget("", xmSeparatorWidgetClass, w, NULL);

#ifdef IMODV_PLUGIN_GENERAL
     {
	  DIR *dirp;
	  struct direct *dp;

	  dirp = opendir(plugdir);
	  if (!dirp) return;
	  while ((dp = readdir(dirp)) != NULL) {
	       char *ext = dp->d_name + dp->d_namlen - 3;
	       if (strcmp(ext, ".so") == 0){
		    /* try and load plug */
		    sprintf(soname, "%s/%s", plugdir, dp->d_name);
		    
		    handle = dlopen(soname, RTLD_LAZY);
		    if (!handle)
			 continue;
		    
		    /* find address of function and data objects */
		    fptr = (void (*)(Widget, ImodvApp *))dlsym
			 (handle, "imodvPlugInAttach");
		    if (!fptr){
			 dlclose(handle);
			 contunue;
		    }
		    /* invoke function */
		    (*fptr)(w, a);
		    printf("loaded plugin : %s", dp->d_name);
	       }
	  }
	  closedir(dirp);
     }
#else
     sprintf(soname, "%s/lineage.so", plugdir);
     /* open the needed object */
     handle = dlopen(soname, RTLD_LAZY);
     if (handle){
	  /* find address of function and data objects */
	  fptr = (void (*)(Widget, ImodvApp *))dlsym
	       (handle, "lineageAttach");

	  /* invoke function, passing value of integer as a parameter */
	  if (fptr){
	       (*fptr)(w, a);
	       printf("loaded lineage plugin.\n");
	  }
     }

#endif

#endif
     return;
}

Widget imodvMenuLightWidget;
Widget imodvMenuWireframeWidget;
Widget imodvMenuLowresWidget;
void imodvMenuLight(int value)
{
    if (value){
	XmToggleButtonSetState(imodvMenuLightWidget, True, False);
    }else{
	XmToggleButtonSetState(imodvMenuLightWidget, False, False);
    }
}
void imodvMenuWireframe(int value)
{
    if (value)
	XmToggleButtonSetState(imodvMenuWireframeWidget, True, False);
    else
	XmToggleButtonSetState(imodvMenuWireframeWidget, False, False);
}
void imodvMenuLowres(int value)
{
    if (value)
	XmToggleButtonSetState(imodvMenuLowresWidget, True, False);
    else
	XmToggleButtonSetState(imodvMenuLowresWidget, False, False);
}

static Widget imodv_create_option_menu(Widget parent)
{
     Widget pullDownMenu, item;

     pullDownMenu = getPulldown(parent, "View", 'V');

     item = XtVaCreateManagedWidget
	  ("Double Buffer", xmToggleButtonWidgetClass, pullDownMenu,
	   XmNmnemonic, 'B',
	   XmNdepth,    Imodv->depth,
	   XmNvisual,   Imodv->visual,
	   XmNcolormap, Imodv->cmap,
	   NULL);
     XtAddCallback(item, XmNvalueChangedCallback, doubleBuffer_cb, Imodv);
     XmToggleButtonSetState(item, True, False);
     if (Imodv->cindex)
	 XtSetSensitive(item, False);

     imodvMenuLightWidget = item = XtVaCreateManagedWidget
	  ("Lighting", xmToggleButtonWidgetClass, pullDownMenu,
	   XmNmnemonic, 'L',
	   XmNdepth,    Imodv->depth,
	   XmNvisual,   Imodv->visual,
	   XmNcolormap, Imodv->cmap,
	   NULL);
     XtAddCallback(item, XmNvalueChangedCallback, lighting_cb, Imodv);
     /* DNM: this didn't make sense; it set button off in both cases and set 
	lighting to 0 if cindex was false.  Changed it to set button correctly
	and set lighting flag correctly */
     if (Imodv->cindex){
	 XtSetSensitive(item, False);
	 XmToggleButtonSetState(item, False, False);
	 Imodv->lighting = 0;
     }else{
	 Imodv->lighting = Imodv->imod->view->world & VIEW_WORLD_LIGHT;
	 if (Imodv->lighting)
	      XmToggleButtonSetState(item, True, False);
	 else
	      XmToggleButtonSetState(item, False, False);
     }

     imodvMenuWireframeWidget = item = XtVaCreateManagedWidget
	  ("Wireframe", xmToggleButtonWidgetClass, pullDownMenu,
	   XmNmnemonic, 'W',
	   XmNdepth,    Imodv->depth,
	   XmNvisual,   Imodv->visual,
	   XmNcolormap, Imodv->cmap,
	   NULL);
     XtAddCallback(item, XmNvalueChangedCallback, wireframe_cb, Imodv);
     XmToggleButtonSetState(item, False, False);

     imodvMenuLowresWidget = item = XtVaCreateManagedWidget
	  ("Low Res", xmToggleButtonWidgetClass, pullDownMenu,
	   XmNmnemonic, 'L',
	   XmNdepth,    Imodv->depth,
	   XmNvisual,   Imodv->visual,
	   XmNcolormap, Imodv->cmap,
	   NULL);
     XtAddCallback(item, XmNvalueChangedCallback, lowres_cb, Imodv);
     Imodv->lowres = Imodv->imod->view->world & VIEW_WORLD_LOWRES;
     if (Imodv->lowres)
	  XmToggleButtonSetState(item, True, False);
     else
	  XmToggleButtonSetState(item, False, False);

     XtVaCreateManagedWidget("", xmSeparatorWidgetClass, pullDownMenu, NULL);

     item = XtVaCreateManagedWidget
	 ("Stereo...", xmPushButtonWidgetClass, pullDownMenu, 
	  XmNmnemonic, 'S',
	  XmNdepth,    Imodv->depth,
         XmNvisual,   Imodv->visual,
         XmNcolormap, Imodv->cmap,
	  NULL);
     XtAddCallback(item, XmNactivateCallback, stereo_cb, Imodv);



     item = XtVaCreateManagedWidget
#ifdef DEPTHCUE_DIALOG
	   ("Depth Cue...",
	   xmPushButtonWidgetClass,
#else
	  ("Depth Cue", 
	   xmToggleButtonWidgetClass,
#endif
	   pullDownMenu,
	   XmNmnemonic, 'D',
	   XmNdepth,    Imodv->depth,
	   XmNvisual,   Imodv->visual,
	   XmNcolormap, Imodv->cmap,
	   NULL);
     XtAddCallback(item, 
#ifdef DEPTHCUE_DIALOG
		   XmNactivateCallback,
#else
		   XmNvalueChangedCallback,
#endif
		   depthcue_cb, Imodv);
     XmToggleButtonSetState(item, False, False);
     if (Imodv->cindex)
	 XtSetSensitive(item, False);



     addImodvViewPlugins(pullDownMenu, Imodv);
     return(pullDownMenu);
}

static Widget imodv_create_help_menu(Widget parent)
{
    Arg args[4];
    int n = 0;
    Widget menu, item, cascade;
    XmString str;

    XtSetArg(args[n], XmNdepth, Imodv->depth); n++;
    XtSetArg(args[n], XmNvisual,   Imodv->visual); n++;
    XtSetArg(args[n], XmNcolormap, Imodv->cmap);n++;

    menu = XmCreatePulldownMenu
        (parent, "_pulldown", args, n);
    str = XmStringCreateSimple("Help");

    cascade = XtVaCreateManagedWidget
        ("Help", xmCascadeButtonWidgetClass, parent,
         XmNsubMenuId, menu,
         XmNlabelString, str,
         XmNmnemonic, 'H',
         XmNdepth,    Imodv->depth,
         XmNvisual,   Imodv->visual,
         XmNcolormap, Imodv->cmap,
         NULL);
    XmStringFree(str);
    
    item = XtVaCreateManagedWidget
         ("Menus", xmPushButtonWidgetClass, menu, NULL);
    XtAddCallback(item, XmNactivateCallback, imodv_help_menu_cb, Imodv);
    item = XtVaCreateManagedWidget
	("Keyboard", xmPushButtonWidgetClass, menu, NULL);
    XtAddCallback(item, XmNactivateCallback, imodv_help_key_cb, Imodv);
    item = XtVaCreateManagedWidget
	("Mouse", xmPushButtonWidgetClass, menu, NULL);
    XtAddCallback(item, XmNactivateCallback, imodv_help_control_cb, Imodv);

    return(cascade);
}

/* Create the menu bar in the model view window. */
Widget imodv_create_menu(ImodvApp *a)
{
     Widget menubar;
     Widget help;

     Arg args[4];
     int n = 0;

     XtSetArg(args[n], XmNdepth, Imodv->depth); n++;
     XtSetArg(args[n], XmNvisual,   Imodv->visual); n++;
     XtSetArg(args[n], XmNcolormap, Imodv->cmap);n++;

     menubar = XmCreateMenuBar(a->mainWin, "menubar", args, n);
 
     imodv_create_file_menu(menubar);
     imodv_create_edit_menu(menubar);
     imodv_create_option_menu(menubar);
     help = imodv_create_help_menu(menubar);
     XtVaSetValues(menubar, XmNmenuHelpWidget, help, NULL);
     
     return(menubar);
}
	   

void PostIt(Widget pb, XtPointer client, XEvent *event)
{
     ImodvApp *a = (ImodvApp *)client;
     XButtonPressedEvent *bevent = (XButtonPressedEvent *)event;

     if (bevent->button != 3)
	  return;
     XmMenuPosition(a->popup, bevent);
     XtManageChild(a->popup);
     return;
}

Widget imodv_create_popup(ImodvApp *a)
{
    Widget popup;
    Arg args[4];
    int n = 0;
    
    XtSetArg(args[n], XmNdepth, Imodv->depth); n++;
    XtSetArg(args[n], XmNvisual,   Imodv->visual); n++;
    XtSetArg(args[n], XmNcolormap, Imodv->cmap);n++;
    popup = XmCreatePopupMenu(a->form, "menubar", args, n);

    imodv_create_file_menu(popup);
    imodv_create_edit_menu(popup);
    imodv_create_option_menu(popup);
    imodv_create_help_menu(popup);

     XtAddEventHandler(a->form, ButtonPressMask, False, 
		       (XtEventHandler)PostIt, (XtPointer)a);
     return(popup);
}
