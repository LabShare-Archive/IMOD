/*   imodplug.cpp  -  load and communicate with plugins or special modules
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
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

static int imodPlugLoadDir(char *plugdir);
static int imodPlugLoad(char *plugpath);
static int addInternalModules();

static Ilist *plugList = NULL;


typedef struct
{
  char *name;
  void *handle;
  SpecialModule *module;
  int   type;
}PlugData;

/* Load all plugins that we can use. */
/* Return the number we have loaded. */
int imodPlugInit(void)
{
  PlugData thePlug;

  char *defdir3 = "/usr/freeware/lib/imodplugs/";
  char *defdir2 = "/usr/local/IMOD/plugins";
  char *defdir1 = "/usr/IMOD/plugins";
#ifndef NO_PLUGS  
  char *envdir = getenv("IMOD_PLUGIN_DIR");

  DIR *dirp;
  struct dirent *dp;
#endif

  int len, type;
  char *ext;
  int maxPlug = 0;

  plugList = ilistNew(sizeof(PlugData), 8);

  /* Add internal modules to list first */
  addInternalModules();

#ifndef NO_PLUGS  
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
static int addInternalModules()
{
  PlugData thePlug, *lplug;
  int mi, i;

  thePlug.module = new BeadFixerModule;
  ilistAppend(plugList, &thePlug);

  // After all are added, go get the information from them
  mi = ilistSize(plugList);
  for (i = 0; i < mi; i++){
    lplug = (PlugData *)ilistItem(plugList, i);
    lplug->name = (*lplug->module->mInfo)(&(lplug->type));
    lplug->handle = NULL;
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
#endif

/*
 * Open a plugin when selected from Special menu 
 */
void imodPlugOpen(int item)
{
  void (*fptr)(ImodView *) = NULL;
  void (*nfptr)(ImodView *, int, int) = NULL;
  PlugData *pd = (PlugData *)ilistItem(plugList, item);
  if (!pd) 
    return;

  /* Get function from internal module */
  if (!pd->handle) {
    fptr = pd->module->mExecute;
    if (!fptr)
      nfptr = pd->module->mExecuteType;

#ifndef NO_PLUGS
  } else {

    /* or find address of function and data objects */
    fptr = (void (*)(ImodView *))dlsym(pd->handle, "imodPlugExecute");
    if (!fptr)
      nfptr = (void (*)(ImodView *, int, int))dlsym
        (pd->handle, "imodPlugExecuteType");
#endif
  }
    
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
  void (*fptr)(ImodView *, int, int);
  int retval = 0;
    
  if (!mi) 
    return 0;
  pd = (PlugData *)ilistFirst(plugList);
  for (i = 0; i < mi; i++){
        
    pd = (PlugData *)ilistItem(plugList, i);
    if (!pd)
      continue;
         
    if (!pd->handle)
      fptr = pd->module->mExecuteType;
#ifndef NOPLUGS
    else
      fptr = (void (*)(ImodView *, int, int))dlsym
        (pd->handle, "imodPlugExecuteType");
#endif
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
      if (!pd->handle)
        fptr = pd->module->mKeys;
#ifndef NOPLUGS
      else
        fptr = (SpecialKeys)dlsym(pd->handle, "imodPlugKeys");
#endif
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
