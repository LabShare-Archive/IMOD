/*   imodview.h  -  header file for public imodview functions
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  No more Log
 */                                                                           

#ifndef IMODVIEW_H
#define IMODVIEW_H

#ifndef IMODVIEWP_H
#include "imodviewP.h"
#endif

/* Define macro for export of functions under Windows */
#ifndef DLL_EX_IM
#ifdef _WIN32
#define DLL_EX_IM _declspec(dllexport)
#else
#define DLL_EX_IM
#endif
#endif

extern "C" {

/*!
 * Sends a message to all 3dmod image windows that a redraw is needed.
 * [inFlags] should contain an OR of the various IMOD_DRAW_ flags.
 */
int DLL_EX_IM ivwDraw(ImodView *inImodView, int inFlags);

/*!
 * The general draw update function.  If a current model point is defined,
 * calls for a draw with IMOD_DRAW_ALL; otherwise calls with IMOD_DRAW_MOD.
 */
int DLL_EX_IM ivwRedraw(ImodView *vw);

/* Gets pixel values of the greyscale color ramp. For index mode. */
void DLL_EX_IM ivwGetRamp(ImodView *inImodView, int *outRampBase, 
                          int *outRampSize);
/* Gets pixel value corresponding to an object color in index mode. */
int DLL_EX_IM  ivwGetObjectColor(ImodView *inImodView, int inObject);

/*! Gets the dimensions of the loaded image into [outX], [outY], [outZ] */
void DLL_EX_IM ivwGetImageSize(ImodView *inImodView, int *outX, int *outY, int *outZ);

/*! Gets the current marker point location into [outX], [outY], [outZ] */
void DLL_EX_IM ivwGetLocation(ImodView *inImodView, int *outX, int *outY, int *outZ);

/*! Gets the current marker point location into [outPoint] */
void DLL_EX_IM ivwGetLocationPoint(ImodView *inImodView, Ipoint *outPoint);

/*! Sets the current marker point location as [inX], [inY], [inZ] */
void DLL_EX_IM ivwSetLocation(ImodView *inImodView, int inX, int inY, int inZ);

/*!
 * Sets the current marker point location from [inPoint].  Z and Y are
 * truncated and Z is rounded to integers to be consistent with 
 * @ivwSetLocation.
 */
void DLL_EX_IM ivwSetLocationPoint(ImodView *inImodView, Ipoint *inPoint);


/*
 * 4D data functions     
 */
/*!
 * Returns the maximum time index, the current time is returned in [outTime].
 * When there is only a single image file, the maximum index and time index 
 * are both 0; when there are multiple times the index is numbered from 1.
 */
int DLL_EX_IM   ivwGetTime(ImodView *inImodView, int *outTime);

/*! Sets the current time to [inTime] */
void DLL_EX_IM  ivwSetTime(ImodView *inImodView, int inTime);

/* !Get the maximum time index */
int DLL_EX_IM   ivwGetMaxTime(ImodView *inImodView);

/*! Gets the label for the current time. */
const char DLL_EX_IM *ivwGetTimeLabel(ImodView *inImodView);

/*! Gets the label for time [inIndex]. */
const char DLL_EX_IM *ivwGetTimeIndexLabel(ImodView *inImodView, int inIndex);

/*!
 * Sets the time for a new contour [cont] in object [obj] to match the 
 * current image time. 
 */
void DLL_EX_IM ivwSetNewContourTime(ImodView *vi, Iobj *obj, Icont *cont);

/*! Gets the mode of the image data stored in the program.  It will be 0 for bytes,
  MRC_MODE_USHORT (6) for unsigned shorts, or MRC_MODE_RGB (16) for RGB triplets. */
int DLL_EX_IM ivwGetImageStoreMode(ImodView *inImodView);

/*!
 * Returns true if image data are loaded into a tile or strip cache.  If this is the
 * case, then the regular image access functions @ivwGetZSection
 * and @ivwGetZSectionTime will return NULL, and either ivwGetCurrentZSection or
 * @ivwGetTileCachedSection must be used instead.
 */
bool DLL_EX_IM ivwDataInTileOrStripCache(ImodView *inImodView);

/*!
 * Returns line pointers to loaded image data for the given Z section, 
 * [inSection].  The line pointers are an array of pointers to the data on
 * each line in Y.  The return value must be cast to (b3dUInt16 **) to use to access 
 * array values when unsigned shorts are loaded.  Returns NULL if data are loaded as 
 * strips or tiles.
 */
unsigned char DLL_EX_IM **ivwGetZSection(ImodView *inImodView, int inSection);

/*!
 * Returns line pointers to loaded image data for the current Z section.  If data are 
 * loaded as strips or tiles, this will load all tiles in the section if necessary/
 */
unsigned char DLL_EX_IM **ivwGetCurrentZSection(ImodView *inImodView);

/*!
 * Returns line pointers to loaded image data for the Z slice [section] and
 * time index [time].  Returns NULL if data are loaded as strips or tiles.
 */
unsigned char DLL_EX_IM **ivwGetZSectionTime(ImodView *vi, int section, int time);

/*!
 * Returns line pointers to a buffer with the entire Z slice [section] loaded from
 * the highest resolution image file, when data are cached as tiles or strips.  This
 * call will force all of the data to be loaded synchronously, which can be 
 * time-consuming if images are large.  The call will allocate an array for the image
 * which should be freed when operations are done (or when a window closes) by calling
 * @@ivwFreeTileCachedSection@.
 */
unsigned char DLL_EX_IM **ivwGetTileCachedSection(ImodView *inImodView, int section);

/*!
 * Frees the arrays allocated when @ivwGetTileCachedSection is called, if they have
 * been allocated; otherwise does nothing.
 */ 
void DLL_EX_IM ivwFreeTileCachedSection(ImodView *inImodView);

/*!
 * Returns a lookup table for mapping from unsigned short values to bytes based on the 
 * current setting of the Low and High range sliders.  It calls 
 * @@mrcfiles.html#get_short_map@, which returns an allocated map that must be freed.
 */
unsigned char DLL_EX_IM *ivwUShortInRangeToByteMap(ImodView *inImodView);

/*!
 * Copies one Z plane of a loaded image with line pointers in [image] as bytes to the
 * buffer [buf].  If the loaded image is unsigned shorts, it is scaled based on the
 * current settings of the Low and High range sliders.  Returns 1 for failure to get
 * a lookup table.
 */
int DLL_EX_IM ivwCopyImageToByteBuffer(ImodView *inImodView, unsigned char **image, 
                                       unsigned char *buf);

/*!
 * Returns grey scale value in memory for given image coordinate [inX],
 * [inY], [inZ].  Works when loaded data are bytes or unsigned shorts, but not RGB.
 */
int DLL_EX_IM ivwGetValue(ImodView *inImodView, int inX, int inY, int inZ);

/*! Return value from image file. */
float DLL_EX_IM ivwGetFileValue(ImodView *inImodView, int inX, int inY, int inZ);

/*!
 * Reads tilt angles, or any set of floating point values from the text file 
 * [fname].  The file should have one value per line.  Returns 1 for error 
 * opening the file, or 2 for memory error.
 */
int DLL_EX_IM ivwReadAngleFile(ImodView *vi, const char *fname);

/*!
 * Returns a pointer to angles read from a file, or NULL if there are none,
 * and returns the number of angles in [numAngles].  If a subset in Z was
 * read in, this function assumes the tilt angle list was for the complete 
 * stack and returns a pointer that starts at the starting Z.
 */ 
  float DLL_EX_IM *ivwGetTiltAngles(ImodView *vi, int &numAngles);

/**************************** Model data functions. ***************************
 *
 * See the libimod library functions for using the model structure.
 *
 */

/*!
 * Gets the model associated with the view.
 */
Imod DLL_EX_IM *ivwGetModel(ImodView *inImodView);


/*! 
 * Gets the number of a free extra object; returns a number greater than 1 or 
 * -1 for an error.  A plugin could claim an object it opens and free it when
 * it closes.
 */
int DLL_EX_IM ivwGetFreeExtraObjectNumber(ImodView *vi);

/*!
 * Clears out and releases the extra object specified by [objNum].  Returns 1 
 * for an object number out of bounds.
 */
int DLL_EX_IM ivwFreeExtraObject(ImodView *vi, int objNum);

/*!
 * Gets pointer to the extra object specified by [objNum].  Do not save this 
 * in a static structure; always get a new pointer before working with it.
 * Returns NULL for an object number out of bounds.
 */
Iobj DLL_EX_IM *ivwGetAnExtraObject(ImodView *inImodView, int objNum);

/*!
 * Deletes all contours, meshes, and general storage data in the extra object 
 * specified by [objNum].
 */
void DLL_EX_IM ivwClearAnExtraObject(ImodView *inImodView, int objNum);

/*!
 *  Gets the original extra object.  
 *  Do not use this.  Get a free object number and use that instead.
 */
Iobj DLL_EX_IM *ivwGetExtraObject(ImodView *inImodView);

/*!
 * Delete all contours in the original extra object
 */
void DLL_EX_IM ivwClearExtraObject(ImodView *inImodView);

/*!
 * Enable or disable drawing of stippled contours
 */
void DLL_EX_IM ivwEnableStipple(ImodView *inImodView, int enable);

/*!
 * Enable or disable sending mouse moves with button up to plugins
 */
void DLL_EX_IM ivwTrackMouseForPlugs(ImodView *inImodView, int enable);

/*!
 * Gets the current contour or makes a new one if there is none in object 
 * [obj], at the current time or the time indicated by [timeLock].
 */
Icont DLL_EX_IM *ivwGetOrMakeContour(ImodView *vw, Iobj *obj, int timeLock);

/*!
 * Returns 1 if in model mode or 0 if in movie mode
 */
int DLL_EX_IM ivwGetMovieModelMode(ImodView *vw);

/*!
 * Sets the program into movie or model mode if [mode] is IMOD_MMOVIE or
 * IMOD_MMODEL, or toggles the mode if [mode] is [IMOD_MM_TOGGLE].  This will
 * cause a redraw.
 */
void DLL_EX_IM ivwSetMovieModelMode(ImodView *inImodView, int mode);

/*! Returns non-zero if contrast is reversed, otherwise returns 0 */
int DLL_EX_IM ivwGetContrastReversed(ImodView *inImodView);

/*!
 * Return true if it is possible to display images in overlay mode in the Zap
 * window.
 */
int DLL_EX_IM ivwOverlayOK(ImodView *inImodView);

/*!
 * Sets the overlay mode section difference to [sec], reverse contrast if 
 * [reverse] is nonzero, and displays current or other section in green if 
 * [whichGreen] is 0 or 1.
 */
void DLL_EX_IM ivwSetOverlayMode(ImodView *inImodView, int sec, int reverse, 
                       int whichGreen);

/*!
 * Returns the Z slice of the top Zap window in [outZ].  The return value is 1
 * if there is no Zap window.
 */
int DLL_EX_IM ivwGetTopZapZslice(ImodView *inImodView, int *outZ);

/*!
 * Sets the Z slice of the top Zap window to [inZ].  This will set the Zap
 * window's section number if it is locked, or the current point location if
 * not, but in either case it will not cause a redraw.  The return value is 1
 * if there is no Zap window or [inZ] is out of range.
 */
int DLL_EX_IM ivwSetTopZapZslice(ImodView *inImodView, int inZ);

/*!
 * Returns the zoom of the top Zap window in [outZoom].  The return value is 1
 * if there is no Zap window.
 */
int DLL_EX_IM ivwGetTopZapZoom(ImodView *inImodView, float *outZoom);

/*!
 * Sets the zoom of the top Zap window to [inZoom] and redraws that Zap window
 * if [draw] is {true}.  Returns 1 if there is no Zap window or the zoom value
 * is out of the range 0.005 to 200.
 */
int DLL_EX_IM ivwSetTopZapZoom(ImodView *inImodView, float inZoom, bool draw);

/*!
 * Returns the image coordinates of the mouse in the top Zap window in 
 * [imagePt].  The X and Y coordinates may be out of range if the mouse is 
 * outside the graphics area.  The return value is 1 if there is no Zap window.
 */
int DLL_EX_IM ivwGetTopZapMouse(ImodView *inImodView, Ipoint *imagePt);

/*!
 * Returns the image coordinates at the center of the top Zap window in [imX]
 * and [imY], and its current Z coordinate in [imZ].  The return value is 1 if
 * there is no top Zap window.
 */
int DLL_EX_IM ivwGetTopZapCenter(ImodView *inImodView, float &imX, float &imY, int &imZ);

/*!
 * Sets the image coordinates at the center of the top Zap window to [imX]
 * and [imY], and its current Z coordinate to [imZ].  If [draw] is true, 
 * either draws the Zap window (if it is locked) or issues a general draw 
 * command; otherwise the caller is responsible for drawing.  The return value 
 * is 1 if there is no top Zap window or if coordinates are out of range.
 */
int DLL_EX_IM ivwSetTopZapCenter(ImodView *inImodView, float imX, float imY,
                                 int imZ, bool draw);

/*!
 * Returns the name of the current image file being displayed.  If multiple single-image 
 * files are loaded in Z, returns the name of the file at the current Z slice.  If 
 * [asEntered] is false, it returns a correct absolute path; otherwise, it returns the 
 * path as the user entered it, which may not be correct if the file is listed in an image
 * descriptor file located in another directory.
 */
QString DLL_EX_IM ivwCurrentImageFile(ImodView *inImodView, bool asEntered);

/*!
 * Set the captions for the next single-frame snapshot of an image or model view window.
 * [captionLines] can contain a series of captions. All but the last will be drawn on a
 * single line.  If [wrapLastLine] is true, the last string will be drawn on multiple 
 * lines with text wrapping at word boundaries.
 */
void DLL_EX_IM b3dSetSnapshotCaption(QStringList &captionLines, bool wrapLastLine);

/*!
 * Takes a single-frame snapshot of the top Zap window.  If [name] is not empty, the 
 * snapshot will be given that filename; otherwise, the name is automatically generated 
 * as usual and returned in [name].  The snapshot format is specified by [format], which
 * should be SnapShot_TIF, SnapShot_PNG, or SnapShot_JPG, not the ambiguous SnapShot_RGB.
 * For a TIFF snapshot, the user's choice of whether to convert to gray-scale is honored
 * if [checkGrayConvert] is true and ignored otherwise.  If [fullArea] is true, the
 * snapshot will be of the whole window even if there is a rubber band.  Returns -1 if 
 * there is no top zap window, -2 if the format is not one of the three listed, -3 if the
 * format is not available as the first or second nonTiff format selected by the user, 
 * or 1 for any kind of error in the snapshotting process.
 */
int DLL_EX_IM ivwSnapshotTopZap(QString &name, int format, bool checkGrayConvert, 
                                bool fullArea);

/*!
 * Takes a single-frame snapshot of the top Slicer window; arguments and return values 
 * are the same as in @@ivwSnapshotTopZap.@
 */
int DLL_EX_IM ivwSnapshotTopSlicer(QString &name, int format, bool checkGrayConvert, 
                                   bool fullArea);

/*!
 * Starts a new arrow, retaining any existing arrows, in the top window of the type given
 * by [windowType], which can be either ZAP_WINDOW_TYPE or SLICER_WINDOW_TYPE.  Returns
 * -4 for an invalid type and -1 if there is no top window.
 */
int DLL_EX_IM startAddedArrow(int windowType);

/*!
 * Clears all arrows in the top window of the type given by [windowType] if [allWindows]
 * is false, or all windows of that type if [allWindows] is true.  [windowType] can be 
 * either ZAP_WINDOW_TYPE or SLICER_WINDOW_TYPE.  Returns -4 for an invalid type and -1 
 * if there are now windows of the given type.
 */
int DLL_EX_IM clearAllArrows(int windowType, bool allWindows);
  
/*!
 * Returns the X, Y, and Z angles of the top slicer in [angles], the center position in
 * [center], and the time of that slicer in [time].  The return value is 1 if there is
 * no slicer open.
 */
int DLL_EX_IM getTopSlicerAngles(float angles[3], Ipoint *center, int &time);

/*!
 * Returns the zoom of the top slicer window in [outZoom].  The return value is 1 if 
 * there is no slicer open.
 */
int DLL_EX_IM ivwGetTopSlicerZoom(ImodView *inImodView, float *outZoom);

/*!
 * Returns the lengths of the scale bars in the top windows of each of the different 
 * types, Zap, Slicer, XYZ, multiZ, or model view.  A -1 is returned
 * if there are no scale bars being displayed or if the particular window is not open.
 * Call imodUnits with the current model to obtain a units string.
 */
void DLL_EX_IM scaleBarAllLengths(float &zapLen, float &slicerLen, float &xyzLen,
                                  float &multiZlen, float &modvLen);

/*!
 * Enables or disables the drawing of scale bars without the Scale Bar dialog open.
 */
void DLL_EX_IM setScaleBarWithoutDialog(bool enable);

/*!
 * Opens or raises the 3dmod dialog windows specified by the key letters provided in 
 * [keys], where the letters are the same as can be given with the -E option.
 */
void DLL_EX_IM ivwOpen3dmodDialogs(const char *keys);

/*!
 * Returns the current snapshot directory or an empty string if one is not set, in
 * which case snapshots will be to QDir::currentPath().
 */
QString DLL_EX_IM b3dGetSnapDirectory(void);

/*
 * Selection list functions in imod_edit.cpp
 */

/*!
 * Adds an item to the selection list defined by the object, contour, and point
 * indices in [newIndex].
 */
void DLL_EX_IM imodSelectionListAdd(ImodView *vi, Iindex newIndex);

/*!
 * Clears the selection list and returns the number previously on the list.
 */
int DLL_EX_IM imodSelectionListClear(ImodView *vi);

/*!
 * If contour [co] in object [ob] is on the selection list, returns the point
 * number; otherwise returns -2.  If [co] < 0, it simply tests whether the 
 * object is on the selection list and returns nonzero if so.
 */
int DLL_EX_IM imodSelectionListQuery(ImodView *vi, int ob, int co);

/*!
 * Removes contour [co] in object [ob] from the selection list if it is on it.
 */
void DLL_EX_IM imodSelectionListRemove(ImodView *vi, int ob, int co);

/*!
 * Manages the selection list when there is a new current point selected, for
 * the model [imod].  The current point index is defined in [indSave], and 
 * [controlDown] should be nonzero if the control key is down.
 */
void DLL_EX_IM imodSelectionNewCurPoint(ImodView *vi, Imod *imod, 
                                        Iindex indSave, int controlDown);
/*!
 * Returns the number of selected objects, and returns the minimum and maximum 
 * selected object number in [minOb] and [maxOb].
 */
int DLL_EX_IM imodNumSelectedObjects(ImodView *vi, int &minOb, int &maxOb);

/*!
 * Updates the Object Type dialog, the Model View Edit Object and Object List
 * dialogs, and the color in the Info window.
 */
void DLL_EX_IM imodUpdateObjectDialogs();
}

/*
 * Preference wrapper calls
 */
/*! 
 * Saves settings in the user preferences under a key given by [key], which 
 * should be the name of a  module.  It saves [numVals] values from the 
 * array [values].  Returns 1 for memory allocation errors.
 */
int DLL_EX_IM prefSaveGenericSettings(char *key, int numVals, double *values);

/*! 
 * Returns settings from the user preferences under the key given by [key].
 * Up to [maxVals] values are returned into the array [values].  The return
 * value is the number of values returned.
 */
int DLL_EX_IM prefGetGenericSettings(char *key, double *values, int maxVals);

/*
 * File chooser call
 */
/*!
 * Gets the name of a single existing file with a file chooser that will show
 * [caption] in its title bar.  A set of [numFilters] filters can be given in
 * [filters]; the first will be the default filter.  Returns an empty string
 * if the user cancels.  Uses the file chooser plugin if one is defined.
 */
QString DLL_EX_IM utilOpenFileName(QWidget *parent, const char *caption, 
                                   int numFilters, const char *filters[]);

/*!
 * Gets the name of a file in which to save with a file chooser that will show
 * [caption] in its title bar.  Uses the file chooser plugin if one is defined.
 */
QString DLL_EX_IM imodPlugGetSaveName(QWidget *parent, const QString &caption);

#endif
