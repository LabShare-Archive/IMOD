/*   imodplug.cpp  -  load and communicate with plugins or special modules
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

Log at end of file
*/

#ifdef _WIN32
#define NOPLUGS
#endif
#ifdef SVR3
#define NOPLUGS
#endif

#ifndef NOPLUGS
#include <sys/types.h>
#include <dlfcn.h>
#include <dirent.h>
#endif

#include <string.h>
#include <qpopupmenu.h>
#include "imod.h"
#include "imodplug.h"
#include "special_module.h"
#include "beadfix.h"
#ifndef TRACK_PLUGIN
#include "linegui.h"
#endif

enum {IP_INFO, IP_EXECUTE, IP_EXECUTETYPE, IP_KEYS};

typedef struct
{
  char *name;
  void *handle;
  SpecialModule *module;
  int   type;
}PlugData;

static int imodPlugLoadDir(char *plugdir);
static int imodPlugLoad(char *plugpath);
static int ipAddInternalModules();
static void *ipGetFunction(PlugData *pd, int which);

static Ilist *plugList = NULL;


/* Load all plugins that we can use. */
/* Return the number we have loaded. */
int imodPlugInit(void)
{
  PlugData thePlug;

  char *defdir3 = "/usr/freeware/lib/imodplugs/";
  char *defdir2 = "/usr/local/IMOD/plugins";
  char *defdir1 = "/usr/IMOD/plugins";
#ifndef NOPLUGS  
  char *envdir = getenv("IMOD_PLUGIN_DIR");

  DIR *dirp;
  struct dirent *dp;
#endif

  int len, type;
  char *ext;
  int maxPlug = 0;

  plugList = ilistNew(sizeof(PlugData), 8);

  /* Add internal modules to list first */
  ipAddInternalModules();

#ifndef NOPLUGS  
  /* load system plugins. */
  imodPlugLoadDir(envdir);

  /* load plugs in environment varialble. */
  imodPlugLoadDir(defdir1);
  imodPlugLoadDir(defdir2);
  imodPlugLoadDir(defdir3);
#endif

  maxPlug = ilistSize(plugList);
  return(maxPlug);
}

/*
 * Add special modules to the list
 */
static int ipAddInternalModules()
{
  PlugData thePlug, *lplug;
  int mi, i;

  thePlug.module = new BeadFixerModule;
  ilistAppend(plugList, &thePlug);
#ifndef TRACK_PLUGIN
  thePlug.module = new LineTrackModule;
  ilistAppend(plugList, &thePlug);
#endif

  // After all are added, go get the information from them
  mi = ilistSize(plugList);
  for (i = 0; i < mi; i++){
    lplug = (PlugData *)ilistItem(plugList, i);
    lplug->name = (*lplug->module->mInfo)(&(lplug->type));
    lplug->handle = NULL;
    if (Imod_debug)
      fprintf(stderr, "Added %s module to Special menu\n", lplug->name);
  }
  return mi;
}

#ifndef NOPLUGS
/*
 * Load all the plugins in a given directory 
 */
static int imodPlugLoadDir(char *plugdir)
{
  char soname[1024];

  DIR *dirp;
  struct dirent *dp;
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

/* 
 * Loads the plugin given by the plugpath in to the
 * global pluglist.
 */
static int imodPlugLoad(char *plugpath)
{
  PlugData thePlug, *lplug;
  SpecialInfo fptr;
  void *handle;
  int i, mi, type;

  handle = dlopen(plugpath, RTLD_LAZY);
  if (!handle){
    fprintf(stderr, "warning: %s failed to open. %s\n",
	    plugpath, dlerror());
    return (1);
  }
    
  /* find address of function and data objects */
  fptr = (SpecialInfo)dlsym (handle, "imodPlugInfo");

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
#endif

/*
 * Open a plugin when selected from Special menu 
 */
void imodPlugOpen(int item)
{
  SpecialExecute fptr = NULL;
  SpecialExecuteType nfptr = NULL;
  PlugData *pd = (PlugData *)ilistItem(plugList, item);
  if (!pd) 
    return;

  /* Get function from internal module or plugin*/
  
  fptr = (SpecialExecute)ipGetFunction(pd, IP_EXECUTE);
  if (!fptr)
    nfptr = (SpecialExecuteType)ipGetFunction(pd, IP_EXECUTETYPE);
    
  /* invoke function */
  if (fptr) {
    (*fptr)(App->cvi);
    return;
  }
  if (!nfptr){
    wprint("\nError invoking %s\n", pd->name);
    return;
  }
  (*nfptr)(App->cvi, 1 /*IMOD_PLUG_MENU*/, 0);
}

/*
 * Open a plugin by name
 */
void imodPlugOpenByName(char *name)
{
  PlugData *pd;
  int i, mi = ilistSize(plugList);

  for(i = 0; i < mi; i++){
    pd = (PlugData *)ilistItem(plugList, i);
    if (!pd)
      continue;
    if (!strcmp(pd->name, name))
      imodPlugOpen(i);
  }
}

/*
 * returns the number of plugins of a given type that loaded. 
 */
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

/*
 * Populates the menu with the plugin names
 */
void imodPlugMenu(QPopupMenu *parent)
{
  PlugData *pd;
  int    i, mi;
  QString str;

  mi = ilistSize(plugList);
  if (!mi) return;

  if (mi){

    pd = (PlugData *)ilistFirst(plugList);
    for(i = 0; i < mi; i++){
               
      pd = (PlugData *)ilistItem(plugList, i);
      if (!pd) continue;
               
      if (pd->type & IMOD_PLUG_MENU){
	str = pd->name;
	parent->insertItem(str, i);
      }
    }
  }
}


/*
 * Calls all the plugins of a given type that have an imodPlugExecute
 * function with the given reason
 */
int imodPlugCall(ImodView *vw, int type, int reason)
{
  PlugData *pd;
  int i, mi = ilistSize(plugList);
  SpecialExecuteType fptr;
  int retval = 0;
    
  if (!mi) 
    return 0;
  pd = (PlugData *)ilistFirst(plugList);
  for (i = 0; i < mi; i++){
        
    pd = (PlugData *)ilistItem(plugList, i);
    if (!pd)
      continue;
         
    fptr = (SpecialExecuteType)ipGetFunction(pd, IP_EXECUTETYPE);
    if (fptr){
      (*fptr)(vw, type, reason);
      retval++;
    }
  }
  return retval;
}

/*
 * Asks plugins to handle a key 
 */
int imodPlugHandleKey(ImodView *vw, QKeyEvent *event)
{
  PlugData *pd;
  int keyhandled, i, mi = ilistSize(plugList);
  SpecialKeys fptr;

  if (!mi) return 0;
  pd = (PlugData *)ilistFirst(plugList);
  for(i = 0; i < mi; i++){
          
    pd = (PlugData *)ilistItem(plugList, i);
    if (!pd) continue;
          
    if (pd->type & IMOD_PLUG_KEYS){
      fptr = (SpecialKeys)ipGetFunction(pd, IP_KEYS);
      if (fptr){
	keyhandled = (*fptr)(vw, event);
	if (keyhandled) return 1;
      }

    }
  }
  return 0;
}

void imodPlugClean(void)
{
  imodPlugCall(App->cvi, 0, IMOD_REASON_CLEANUP);
  return;
}

/*
 * Get a function - handle internal versus external and possibly 
 * system-dependent stuff here
 */
static void *ipGetFunction(PlugData *pd, int which)
{
  if (!pd->handle) {
    switch (which) {
    case IP_INFO:
      return (void *)pd->module->mInfo;
    case IP_EXECUTE:
      return (void *)pd->module->mExecute;
    case IP_EXECUTETYPE:
      return (void *)pd->module->mExecuteType;
    case IP_KEYS:
      return (void *)pd->module->mKeys;
    default:
      return NULL;
    }
#ifndef NOPLUGS
  } else {
    switch (which) {
    case IP_INFO:
      return dlsym(pd->handle, "imodPlugInfo");
    case IP_EXECUTE:
      return dlsym(pd->handle, "imodPlugExecute");
    case IP_EXECUTETYPE:
      return dlsym(pd->handle, "imodPlugExecuteType");
    case IP_KEYS:
      return dlsym(pd->handle, "imodPlugKeys");
    default:
      return NULL;
    }
#endif
  }
  return NULL;
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

/*
$Log$
Revision 4.7  2003/10/02 01:31:11  mast
Added ability to open a plugin by name

Revision 4.6  2003/10/01 19:38:59  mast
Simply code, add function for getting functions

Revision 4.5  2003/10/01 05:13:28  mast
Changes to use special internal modules

Revision 4.4  2003/04/17 19:09:33  mast
plugs OK on Mac now

Revision 4.3  2003/03/28 05:49:16  mast
No plugs for Mac either

Revision 4.2  2003/02/27 19:41:15  mast
No plugs in windows yet

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/13 01:19:04  mast
Qt versions

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/
