/*
 *  $Id$
 *
 *  Original Author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 */

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
const char *imodIOGetErrorString(void);
char *currentSavedModelFile(void);
void setImod_filename(const char *name);
int createNewModel(const char *modelFilename);
void initReadInModelData(Imod *newModel, bool keepBW);
int openModel(const char *modelFilename, bool keepBW, bool saveAs);
Imod *LoadModel(FILE *mfin);
int SaveModel(struct Mod_Model *mod);
int SaveasModel(struct Mod_Model *mod);
int imod_model_changed(Imod *imodel);
void imod_cleanup_autosave(void);
int imod_autosave(struct Mod_Model *mod);
void initNewModel(Imod *imod);
void setModelScalesFromImage(Imod *imod, bool doZscale);
unsigned char **imod_io_image_load(struct ViewInfo *vi);

#endif /* IMOD_IO_H */
