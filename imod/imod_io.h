/*
 *  $Id$
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 4.8  2006/01/14 18:15:03  mast
    Added function for new model initialization

    Revision 4.7  2005/10/14 22:00:13  mast
    Changes for reload and some cleanup

    Revision 4.6  2004/06/04 03:15:43  mast
    Added argument to loadModel to keep black/white level

    Revision 4.5  2004/01/05 17:57:30  mast
    Changed imod_io_image_load to just take vi as argument

    Revision 4.4  2003/02/10 20:41:14  mast
    Resolve merge conflict


    Revision 4.2.2.2  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 4.2.2.1  2002/12/05 16:23:52  mast
    No changes - CVS detected as modified in branch

    Revision 4.3  2002/12/03 15:54:35  mast
    Added define for read cancel

    Revision 4.2  2002/12/01 16:51:34  mast
    Changes to eliminate warnings on SGI

    Revision 4.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

    Revision 4.0  2002/09/27 20:20:44  rickg
    Added error reporting functionality and codes
    Moved the io functionality in imod_menu_cb to this module
    Added C++ conditional declarations

    Revision 3.1  2002/09/13 21:07:39  mast
    Removed redundant declarations, changed LoadModel

*/
/* imod_io.h */

#ifndef IMOD_IO_H
#define IMOD_IO_H

#define IMOD_IO_SUCCESS 0
#define IMOD_IO_SAVE_ERROR 1
#define IMOD_IO_SAVE_CANCEL 2
#define IMOD_IO_DOES_NOT_EXIST 3
#define IMOD_IO_NO_ACCESS_ERROR 4
#define IMOD_IO_READ_ERROR 5
#define IMOD_IO_NO_FILE_SELECTED 6
#define IMOD_IO_NOMEM 7
#define IMOD_IO_READ_CANCEL 8
#define IMOD_IO_UNIMPLEMENTED_ERROR 99

/* Functions */
int imodIOGetError(void);
char *imodIOGetErrorString(void);
void setImod_filename(const char *name);
int createNewModel(const char *modelFilename);
int openModel(const char *modelFilename, bool keepBW, bool saveAs);
Imod *LoadModel(FILE *mfin);
int SaveModel(struct Mod_Model *mod);
int SaveasModel(struct Mod_Model *mod);
int imod_model_changed(Imod *imodel);
void imod_cleanup_autosave(void);
int imod_autosave(struct Mod_Model *mod);
void initNewModel(Imod *imod);
unsigned char **imod_io_image_load(struct ViewInfo *vi);

#endif /* IMOD_IO_H */
