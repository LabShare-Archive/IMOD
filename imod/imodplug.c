/*  $Author$

    $Date$

    $Revision$

    $Log$
*/
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Separator.h>

/*#ifdef __linux
#define NOPLUGS
#endif */
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


#include <imodel.h>
#include "imod.h"


extern int Imod_debug;
Ilist *plugList = NULL;

static int maxPlug = 0;
typedef struct
{
     char *name;
     void *handle;
     int   type;
}PlugData;

/*char **plugNames;*/

static void file_open_cb(Widget w, XtPointer client, XtPointer call)
{
     wprint("\nOpen a plugin not working yet.\n");
}

static void open_cb(Widget w, XtPointer client, XtPointer call)
{
#ifndef NOPLUGS
     void (*fptr)(ImodView *);
     void (*nfptr)(ImodView *, int, int);
     int item = (int)client;
     PlugData *pd = (PlugData *)ilistItem(plugList, item);
     if (!pd) return;


     /* find address of function and data objects */
     fptr = (void (*)(ImodView *))dlsym
	  (pd->handle, "imodPlugExecute");
     if (!fptr){
	  nfptr = (void (*)(ImodView *, int, int))dlsym
	       (pd->handle, "imodPlugExecuteType");
	  if (!nfptr){
	       wprint("\nError invoking %s\n", pd->name);
	       return;
	  }
	  (*nfptr)(App->cvi, 1 /*IMOD_PLUG_MENU*/, 0);
	  return;
     }
     /* invoke function */
     (*fptr)(App->cvi);
#endif
     return;
}


void imodPlugClean(void)
{
     imodPlugCall(App->cvi, 0, IMOD_REASON_CLEANUP);
     return;
}


#ifndef NOPLUGS
/* 
 * Loads the plugin given by the plugpath in to the
 * global pluglist.
 */
int imodPlugLoad(char *plugpath)
{
    PlugData thePlug, *lplug;
    char *(*fptr)(int *);
    void *handle;
    int i, mi, type;

    handle = dlopen(plugpath, RTLD_LAZY);
    if (!handle){
	fprintf(stderr, "warning: %s failed to open. %s\n",
		plugpath, dlerror());
	return (1);
    }
    
    /* find address of function and data objects */
    fptr = (char *(*)(int *))dlsym
	(handle, "imodPlugInfo");

    if (!fptr){
	thePlug.name = dlerror();
	if (Imod_debug)
	    fprintf(stderr, "warning: %s not an imod plugin (%s)\n",
		    plugpath, thePlug.name);

	dlclose(handle);
	return(2);
    }

    /* invoke imodPlugInfo function */
    thePlug.handle = handle;
    fptr = (char *(*)(int *))dlsym(handle, "imodPlugInfo");
    thePlug.name = (*fptr)(&type);
    thePlug.type = type;

    /*
     * Search the pluglist for an identical plug.
     * then add this plug to plug list.
     */
    mi = ilistSize(plugList);
    for(i = 0; i < mi; i++){
	lplug = (PlugData *)ilistItem(plugList, i);
	if (thePlug.type != lplug->type)
	    continue;
	if (strcmp(thePlug.name, lplug->name) == 0){
	    dlclose(handle);
	    return(3);
	}
    }
    ilistAppend(plugList, &thePlug);
    
    if (Imod_debug)
	fprintf(stderr, "loaded plugin : %s %s\n",
		plugpath, thePlug.name);
    
    return 0;
}

int imodPlugLoadDir(char *plugdir)
{
     char soname[1024];

     DIR *dirp;
     struct dirent *dp;
     char **names;
     int len, type;
     char *ext;

     int pload = 0; /* Return the number of plugs loaded. */

     if (!plugdir) 
	 return pload;

     dirp = opendir(plugdir);
     if (!dirp)
	  return pload;
     
     while ((dp = readdir(dirp)) != NULL) {
	  len = strlen(dp->d_name);
	  ext = dp->d_name + len - 3;

	  if (!(strcmp(ext, ".so") == 0))
	       continue;

	   /* try and load plug */
	  sprintf(soname, "%s/%s", plugdir, dp->d_name);
	  
	  if (imodPlugLoad(soname))
	      continue;
     }
     closedir(dirp);

     return(pload);
}
#endif

/* Load all plugins that we can use. */
/* Return the number we have loaded. */
int imodPlugInit(void)
{
#ifdef NOPLUGS
    return(0);
#else
    PlugData thePlug;

    char *defdir3 = "/usr/freeware/lib/imodplugs/";
    char *defdir2 = "/usr/local/IMOD/plugins";
    char *defdir1 = "/usr/IMOD/plugins";
    char *envdir = getenv("IMOD_PLUGIN_DIR");


    DIR *dirp;
    struct dirent *dp;
    char **names;
    int len, type;
    char *ext;

    maxPlug = 0;
    plugList = ilistNew(sizeof(PlugData), 8);

    /* load system plugins. */
    imodPlugLoadDir(envdir);

    /* load plugs in environment varialble. */
    imodPlugLoadDir(defdir1);
    imodPlugLoadDir(defdir2);
    imodPlugLoadDir(defdir3);

    maxPlug = ilistSize(plugList);
    return(maxPlug);
#endif
}

/* returns the number of plugins of a given type that loaded. */
int imodPlugLoaded(int type)
{
     PlugData *pd;
     int plugs = 0;
     int i, mi = ilistSize(plugList);

     for(i = 0; i < mi; i++){
	  pd = (PlugData *)ilistItem(plugList, i);
	  if (!pd) continue;
	  if (pd->type & type)
	       plugs++;
     }
     return(plugs);
}

int imodPlugCall(ImodView *vw, int type, int reason)
{
#ifndef NOPLUGS
    PlugData *pd;
    int keyhandled,i,mi = ilistSize(plugList);
    void (*fptr)(ImodView *, int, int);
    
    if (!mi) return 0;
    pd = (PlugData *)ilistFirst(plugList);
    for(i = 0; i < mi; i++){
	
	pd = (PlugData *)ilistItem(plugList, i);
	if (!pd) continue;
	
	fptr = (void (*)(ImodView *, int, int))dlsym
	    (pd->handle, "imodPlugExecuteType");
	if (fptr){
	    (*fptr)(vw,type,reason);
	}
     }
    return 1;
#else
     return 0;
#endif

}

int imodPlugHandleKey(ImodView *vw, XKeyEvent *event)
{
#ifndef NOPLUGS
     PlugData *pd;
     int keyhandled,i,mi = ilistSize(plugList);
     int (*fptr)(ImodView *, XKeyEvent *);

     if (!mi) return 0;
     pd = (PlugData *)ilistFirst(plugList);
     for(i = 0; i < mi; i++){
	  
	  pd = (PlugData *)ilistItem(plugList, i);
	  if (!pd) continue;
	  
	  if (pd->type & IMOD_PLUG_KEYS){
	       fptr = (int (*)(ImodView *, XKeyEvent *))dlsym
		    (pd->handle, "imodPlugKeys");
	       if (fptr){
		    keyhandled = (*fptr)(vw, event);
		    if (keyhandled) return 1;
	       }

	  }
     }
#endif
     return 0;
}


void imodPlugMenu(Widget parent, int pos)
{
#ifndef NOPLUGS
     Widget item, plugMenu, cascade;
     Colormap cmap;
     Visual *visual;
     int    depth;
     
     PlugData *pd;
     int    i, mi;

     mi = ilistSize(plugList);
     if (!mi) return;

     XtVaGetValues(App->toplevel,
		   XmNdepth, &depth,
		   XmNvisual, &visual,
		   XmNcolormap, &cmap,
		   NULL);

     plugMenu = XmVaCreateSimplePulldownMenu
	  (parent, "plug_menu", pos, NULL,
	   XmNdepth,    depth,
	   XmNvisual,   visual,
	   XmNcolormap, cmap,
	   NULL);


     if (mi){

	  pd = (PlugData *)ilistFirst(plugList);
	  for(i = 0; i < mi; i++){
	       
	       pd = (PlugData *)ilistItem(plugList, i);
	       if (!pd) continue;
	       
	       if (pd->type & IMOD_PLUG_MENU){
		   item = XtVaCreateManagedWidget
		       (pd->name, xmPushButtonWidgetClass, plugMenu, NULL);
		   XtAddCallback(item, XmNactivateCallback, open_cb, 
				 (XtPointer)i);
	       }
	  }


	  item = XtVaCreateManagedWidget
	       ("separator", xmSeparatorWidgetClass,  plugMenu, NULL);
     }

#ifdef OPEN_PLUG_WORKING
     item = XtVaCreateManagedWidget
	  ("Open Plugin...", xmPushButtonWidgetClass, plugMenu,
	   XmNmnemonic, 'O',
	   NULL);
     XtAddCallback(item, XmNactivateCallback, file_open_cb, App);
#endif
#endif
     return;
}


#ifdef USE_PLUG_FILE
typedef struct
{
    /* Must be set by plugin. */
    int   xsize, ysize, zsize;
    int   type;      /* IMOD_UNSIGNED_CHAR, IMOD_SHORT, IMOD_FLOAT */
    int   format;    /* IMOD_LUMINANCE, IMOD_RGB */

    /* Should be set by plugin. */
    int   axis;
    float scale[3];           /* All default to 1.0f */
    float translate[3];       /* All default to 0.0f */
    float rotate[3];          /* All default to 0.0f */
    float amin, amax, amean;  /* All default to 0.0f */

    /* Read only for plugin */
    char *filename;
    char *timelabel;
    int   time;

    /* internal use only */
    /* functions pointers to load this image. */

    /* plugImageSectionRead(PlugLoad, unsigned char *buf, int z); */
    

}PlugLoad;

static PlugData *filePlug = NULL;
static PlugLoad plugLoad;

int imodPlugImageHandle(char *filename)
{
    PlugData *pd;
    int i, mi = ilistSize(plugList);
    
    plugLoad.filename = filename;

    for(i = 0; i < mi; i++){
	pd = ilistItem(plugList, i);
	if (!pd) continue;
	if (pd->type & IMOD_PLUG_FILE){
	    
	}
    }
    return(plugs);
    
}


#endif
