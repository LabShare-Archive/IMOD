/*   imodplug.cpp  -  load and communicate with plugins or special modules
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#include <string.h>
#include <qmenu.h>
#include <qsignalmapper.h>
#include <qfiledialog.h>
#include <qaction.h>
#include <qdir.h>
#include <qlibrary.h>
//Added by qt3to4:
#include <QMouseEvent>
#include <QKeyEvent>
#include <QEvent>
#include "b3dutil.h"
#include "imod.h"
#include "imodplug.h"
#include "special_module.h"
#include "beadfix.h"
#ifndef TRACK_PLUGIN
#include "linegui.h"
#endif

enum {IP_INFO, IP_EXECUTE, IP_EXECUTETYPE, IP_KEYS, IP_MOUSE, IP_EVENT, 
      IP_EXECUTE_MESSAGE, IP_OPEN_FILE_NAME, IP_OPEN_FILE_NAMES, IP_SAVE_FILE_NAME};

typedef struct
{
  const char *name;
  QLibrary *library;
  SpecialModule *module;
  int   type;
}PlugData;

static int imodPlugLoadDir(const char *plugdir);
static int imodPlugLoad(QString plugpath);
static int ipAddInternalModules();
static void *ipGetFunction(PlugData *pd, int which);

static Ilist *plugList = NULL;
static int numInternal;

/* Load all plugins that we can use. */
/* Return the number we have loaded. */
int imodPlugInit(void)
{
#ifdef _WIN32
  const char *defdir3 = "C:/Program Files/IMOD/lib/imodplug";
  const char *defdir4 = "C:/Program Files/3dmod/lib/imodplug";
#else
  const char *defdir3 = "usr/freeware/lib/imodplugs";
#endif
  const char *defdir2 = "/usr/local/IMOD/plugins";
  const char *envdir2 = getenv("IMOD_CALIB_DIR");
  const char *envdir = getenv("IMOD_PLUGIN_DIR");
  QString str;
  int maxPlug = 0;

  plugList = ilistNew(sizeof(PlugData), 8);

  /* Add internal modules to list first */
  ipAddInternalModules();
  numInternal = ilistSize(plugList);

  /* load plugs in environment variables: IMOD_PLUGIN_DIR if it exists,
    otherwise IMOD_DIR/lib/imodplug using a default IMOD_DIR if necessary;
    then IMOD_CALIB_DIR/plugins. */
  if (envdir)
    imodPlugLoadDir(envdir);
  else {
    
    str = QString(IMOD_DIR_or_default(NULL));
    str += QString("/lib/imodplug");
    imodPlugLoadDir(LATIN1(str));
  }

  if (envdir2) {
    str = QString(envdir2) + QString("/plugins");
    imodPlugLoadDir(LATIN1(str));
  }

  /* load system plugins. */
  imodPlugLoadDir(defdir2);
  imodPlugLoadDir(defdir3);
#ifdef _WIN32
  imodPlugLoadDir(defdir4);
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
    lplug->library = NULL;
    if (Imod_debug)
      imodPrintStderr("Added %s module to Special menu\n", lplug->name);
  }
  return mi;
}

/*
 * Load all the plugins in a given directory 
 */
static int imodPlugLoadDir(const char *plugdir)
{
#ifdef _WIN32
  const char *filter = "*.dll";
#else 
#ifdef Q_OS_MACX
    const char *filter = "*.dylib";
#else
    const char *filter = "*.so";
#endif    
#endif    
  int pload = 0; /* Return the number of plugs loaded. */
  int i;
  QDir *dir = new QDir(QString(plugdir), QString(filter));
  for (i = 0; i < dir->count(); i++)
    if (!imodPlugLoad(dir->absoluteFilePath(dir->entryList()[i])))
      pload++;

  delete dir;
  return(pload);
}

/* 
 * Loads the plugin given by the plugpath in to the
 * global pluglist.
 */
static int imodPlugLoad(QString plugpath)
{
  PlugData thePlug, *lplug;
  SpecialInfo fptr;
  QLibrary *library;
  QString str;
  int i, mi, type;

  library = new QLibrary(plugpath);

  if (!library->load()) {
    if (Imod_debug)
      imodPrintStderr("Warning: %s cannot be loaded as a 3dmod plugin.\n",
                      LATIN1(plugpath));
    delete library;
    return(2);
  }


  /* find address of function and data objects */
  fptr = (SpecialInfo)library->resolve("imodPlugInfo");

  if (!fptr){
    if (Imod_debug)
      imodPrintStderr("Warning: imodPlugInfo cannot be resolved in %s.\n",
                      LATIN1(plugpath));
    delete library;
    return(2);
  }

  /* invoke imodPlugInfo function */
  thePlug.library = library;
  thePlug.name = (*fptr)(&type);
  thePlug.type = type;

  /*
   * Search the pluglist for an identical plug.
   * then add this plug to plug list.
   */
  mi = ilistSize(plugList);
  for (i = 0; i < mi; i++) {
    lplug = (PlugData *)ilistItem(plugList, i);
    if (thePlug.type != lplug->type)
      continue;
    if (strcmp(thePlug.name, lplug->name) == 0){
      delete library;
      return(3);
    }
  }
  ilistAppend(plugList, &thePlug);
  if (thePlug.type & IMOD_PLUG_CHOOSER)
    App->chooserPlugin = 1;
    
  if (Imod_debug)
    imodPrintStderr("loaded plugin : %s %s\n", LATIN1(plugpath), thePlug.name);
  return 0;
}

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
  if (fptr) {
    (*fptr)(App->cvi);
    return;
  }
  nfptr = (SpecialExecuteType)ipGetFunction(pd, IP_EXECUTETYPE);
    
  /* invoke function */
  if (!nfptr){
    wprint("\nError invoking %s\n", pd->name);
    return;
  }
  (*nfptr)(App->cvi, IMOD_PLUG_MENU, IMOD_REASON_EXECUTE);
}

/*
 * Open a plugin by name
 */
void imodPlugOpenByName(const char *name)
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
 * Open all true plugins, not special modules (for easy testing)
 */
void imodPlugOpenAllExternal(void)
{
  int i;
  for (i = numInternal; i < ilistSize(plugList); i++)
    imodPlugOpen(i);
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
void imodPlugMenu(QMenu *parent, QSignalMapper *mapper)
{
  PlugData *pd;
  int    i, mi;
  QString str;
  QAction *action;

  mi = ilistSize(plugList);
  if (!mi) return;

  if (mi){

    pd = (PlugData *)ilistFirst(plugList);
    for(i = 0; i < mi; i++){
               
      pd = (PlugData *)ilistItem(plugList, i);
      if (!pd) continue;
               
      if (pd->type & IMOD_PLUG_MENU){
        str = pd->name;
        action = parent->addAction(str);
        QObject::connect(action, SIGNAL(triggered()), mapper, SLOT(map()));
        mapper->setMapping(action, i);
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

  if (!mi) 
    return 0;
  pd = (PlugData *)ilistFirst(plugList);
  for(i = 0; i < mi; i++){
          
    pd = (PlugData *)ilistItem(plugList, i);
    if (!pd) 
      continue;
          
    if (pd->type & IMOD_PLUG_KEYS){
      fptr = (SpecialKeys)ipGetFunction(pd, IP_KEYS);
      if (fptr){
        keyhandled = (*fptr)(vw, event);
        if (keyhandled)
          return 1;
      }

    }
  }
  return 0;
}

int imodPlugHandleMouse(ImodView *vw, QMouseEvent *event, float imx, float imy,
                        int but1, int but2, int but3)
{
  PlugData *pd;
  int handled, i, needDraw = 0, mi = ilistSize(plugList);
  SpecialMouse fptr;

  if (!mi) 
    return 0;
  pd = (PlugData *)ilistFirst(plugList);
  for(i = 0; i < mi; i++){
          
    pd = (PlugData *)ilistItem(plugList, i);
    if (!pd) 
      continue;
          
    if (pd->type & IMOD_PLUG_MOUSE){
      fptr = (SpecialMouse)ipGetFunction(pd, IP_MOUSE);
      if (fptr){
        handled = (*fptr)(vw, event, imx, imy, but1, but2, but3);
        if (handled & 1)
          return handled;
        needDraw |= handled;
      }

    }
  }
  return needDraw;
}

int imodPlugHandleEvent(ImodView *vw, QEvent *event, float imx, float imy)
{
  PlugData *pd;
  int handled, i, needDraw = 0, mi = ilistSize(plugList);
  SpecialEvent fptr;

  if (!mi) 
    return 0;
  pd = (PlugData *)ilistFirst(plugList);
  for(i = 0; i < mi; i++){
          
    pd = (PlugData *)ilistItem(plugList, i);
    if (!pd) 
      continue;
          
    if (pd->type & IMOD_PLUG_EVENT){
      fptr = (SpecialEvent)ipGetFunction(pd, IP_EVENT);
      if (fptr){
        handled = (*fptr)(vw, event, imx, imy);
        if (handled & 1)
          return handled;
        needDraw |= handled;
      }

    }
  }
  return needDraw;
}

/*
 * Find a plugin that the message is designated for and pass it on
 */
int imodPlugMessage(ImodView *vw, QStringList *strings, int *arg)
{
  PlugData *pd;
  int  i, j, numWords, mi = ilistSize(plugList);
  SpecialExecuteMessage fptr;
  QString str;
  QStringList nameList;
  QStringList message = *strings;

  if (!mi) 
    return 1;
  pd = (PlugData *)ilistFirst(plugList);
  for(i = 0; i < mi; i++){
          
    pd = (PlugData *)ilistItem(plugList, i);
    if (!pd)
      continue;
          
    if (pd->type & IMOD_PLUG_MESSAGE) {

      // Check name for match
      str = pd->name;
      nameList = str.split(" ", QString::SkipEmptyParts);
      numWords = nameList.count();
      for (j = 0; j < numWords; j++)
        if (nameList[j] != message[*arg + j])
          break;
      if (j < numWords)
        continue;

      fptr = (SpecialExecuteMessage)ipGetFunction(pd, IP_EXECUTE_MESSAGE);
      if (!fptr)
        return 1;
      *arg += numWords;
      return (*fptr)(vw, strings, arg);
    }
  }
  return 1;
}

/*
 * File chooser calls
 * For each of these, look for the first file chooser with the call and call it,
 * or fall back to the Qt dialog
 */
QString imodPlugGetOpenName(QWidget *parent, const QString &caption,
                            const QString &dir, const QString &filter)
{
  PlugData *pd;
  int i, mi = ilistSize(plugList);
  SpecialOpenFileName fptr;
  if (App->chooserPlugin) {
    for(i = 0; i < mi; i++) {
      pd = (PlugData *)ilistItem(plugList, i);
      if (pd->type & IMOD_PLUG_CHOOSER) {
        fptr = (SpecialOpenFileName)ipGetFunction(pd, IP_OPEN_FILE_NAME);
        if (fptr)
          return (*fptr)(parent, caption, dir, filter);
      }
    }
  }
  return QFileDialog::getOpenFileName(parent, caption, dir, filter);
}

QStringList imodPlugGetOpenNames(QWidget *parent, const QString &caption,
                                 const QString &dir, const QString &filter)
{
  PlugData *pd;
  int i, mi = ilistSize(plugList);
  SpecialOpenFileNames fptr;
  if (App->chooserPlugin) {
    for(i = 0; i < mi; i++) {
      pd = (PlugData *)ilistItem(plugList, i);
      if (pd->type & IMOD_PLUG_CHOOSER) {
        fptr = (SpecialOpenFileNames)ipGetFunction(pd, IP_OPEN_FILE_NAMES);
        if (fptr)
          return (*fptr)(parent, caption, dir, filter);
      }
    }
  }
  return QFileDialog::getOpenFileNames(parent, caption, dir, filter);
}

QString imodPlugGetSaveName(QWidget *parent, const QString &caption)
{
  PlugData *pd;
  int i, mi = ilistSize(plugList);
  SpecialSaveFileName fptr;
  if (App->chooserPlugin) {
    for(i = 0; i < mi; i++) {
      pd = (PlugData *)ilistItem(plugList, i);
      if (pd->type & IMOD_PLUG_CHOOSER) {
        fptr = (SpecialSaveFileName)ipGetFunction(pd, IP_SAVE_FILE_NAME);
        if (fptr)
          return (*fptr)(parent, caption);
      }
    }
  }
  return QFileDialog::getSaveFileName(parent, caption);
}


/*
 * Get a function - handle internal versus external and possibly 
 * system-dependent stuff here
 */
static void *ipGetFunction(PlugData *pd, int which)
{
  if (!pd->library) {
    switch (which) {
    case IP_INFO:
      return (void *)pd->module->mInfo;
    case IP_EXECUTE:
      return (void *)pd->module->mExecute;
    case IP_EXECUTETYPE:
      return (void *)pd->module->mExecuteType;
    case IP_EXECUTE_MESSAGE:
      return (void *)pd->module->mExecuteMessage;
    case IP_KEYS:
      return (void *)pd->module->mKeys;
    case IP_MOUSE:
      return (void *)pd->module->mMouse;
    case IP_OPEN_FILE_NAME:
      return (void *)pd->module->mOpenFileName;
    case IP_OPEN_FILE_NAMES:
      return (void *)pd->module->mOpenFileNames;
    case IP_SAVE_FILE_NAME:
      return (void *)pd->module->mSaveFileName;
    default:
      break;
    }
  } else {
    switch (which) {
    case IP_INFO:
      return pd->library->resolve("imodPlugInfo");
    case IP_EXECUTE:
      return pd->library->resolve("imodPlugExecute");
    case IP_EXECUTETYPE:
      return pd->library->resolve("imodPlugExecuteType");
    case IP_EXECUTE_MESSAGE:
      return pd->library->resolve("imodPlugExecuteMessage");
    case IP_KEYS:
      return pd->library->resolve("imodPlugKeys");
    case IP_MOUSE:
      return pd->library->resolve("imodPlugMouse");
    case IP_EVENT:
      return pd->library->resolve("imodPlugEvent");
    case IP_OPEN_FILE_NAME:
      return pd->library->resolve("imodPlugOpenFileName");
    case IP_OPEN_FILE_NAMES:
      return pd->library->resolve("imodPlugOpenFileNames");
    case IP_SAVE_FILE_NAME:
      return pd->library->resolve("imodPlugSaveFileName");
    default:
      break;
    }
  }
  return NULL;
}
