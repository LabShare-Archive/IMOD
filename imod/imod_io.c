/*  IMOD VERSION 2.50
 *
 *  imod_io.c -- File I/O for imod.
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
*/

#include <stdio.h>
#include <dia.h>
#include <sys/types.h>
#include <time.h>
#include "mrcfiles.h"
#include "imod.h"
#include "xzap.h"
#include "imod_info.h"
#include "imodv.h"

static char *datetime(void);
static void imod_undo_backup(void);
static void imod_finish_backup(void);


char Statstring[128];  /* used in function show_staus(); */
char Inputstring[128];
char Imod_filename[256] = {0x00};

static char autosave_filename[266] = {0x00};
static char saved_filename[256] = {0x00};
static int last_checksum = -1;

/* The VMS operating system doesn't allow filenames with the
 * charactor '#'
 */
#ifdef __vms
char *Imod_autosave_string = "_autosave";
#else
char *Imod_autosave_string = "#autosave#";
#endif

static char dummystring[] = "         ";
static char *datetime()
{
     time_t clock;
     char *string;
     clock = time(NULL);
     string = ctime(&clock);
     strncpy(dummystring, string + 11, 8);
     return dummystring;
}

int imod_model_changed(Imod *imodel)
{
     int checksum;

     checksum = imodChecksum(imodel);
     if (checksum == imodel->csum)
	  return(0);
     return(1);
}

/* DNM: improved functions for making a backup (~) file: first one makes the
   backup and renames an older backup to ~~, then if the save is successful
   one finishes by deleting the ~~, or if unsuccessful, one undoes the moves
   so that both the current file and the backup are retained. */

/* Rename existing copy of saved file if it hasn't been done before for this
   file name */
static void imod_make_backup(char *filename)
{
     char *nfname1, *nfname2;

     if ((filename[0] != 0x00) && strcmp(filename, saved_filename)) {
          nfname1 = (char *)malloc(strlen(filename) + 2);
          nfname2 = (char *)malloc(strlen(filename) + 3);
	  sprintf(nfname1, "%s~", filename);
	  sprintf(nfname2, "%s~~", filename);
	  rename(nfname1, nfname2);
	  rename(filename, nfname1);
	  free(nfname1);
	  free(nfname2);
          strcpy(saved_filename, filename);
     }
}

/* undo the renaming of files to backup files if the save fails */
static void imod_undo_backup()
{
     char *nfname1, *nfname2;

     if (saved_filename[0]) {
          nfname1 = (char *)malloc(strlen(saved_filename) + 2);
          nfname2 = (char *)malloc(strlen(saved_filename) + 3);
	  sprintf(nfname1, "%s~", saved_filename);
	  sprintf(nfname2, "%s~~", saved_filename);
	  rename(nfname1, saved_filename);
	  rename(nfname2, nfname1);
	  free(nfname1);
	  free(nfname2);
	  saved_filename[0] = 0x00;
     }
}

/* Or finish the making of a backup by cleaning up the ~~ file */
static void imod_finish_backup()
{
     char *nfname2;

     nfname2 = (char *)malloc(strlen(saved_filename) + 3);
     sprintf(nfname2, "%s~~", saved_filename);
     remove(nfname2);
     free(nfname2);
}

/* DNM: changed autosave functions to implement better cleanup of last autosave
   file when the nae changes, and to autosave only if the model is changed
   since the last autosave */

/* Clean up the autosave file if one was created */
void imod_cleanup_autosave(void)
{
     if (autosave_filename[0])
          remove(autosave_filename);
     last_checksum = -1;
}

/* save autosave file */
/* DNM 6/15/01: add directory environment variable to allow save to local disk,
   and save even if in movie mode */
int imod_autosave(struct Mod_Model *mod)
{
     FILE *tfilep;
     int new_checksum;
     char *timestr;
     char *savedir = getenv("IMOD_AUTOSAVE_DIR");
     
     if (!mod)
	  return(-1);

      /*if (mod->mousemode == IMOD_MMOVIE)
	  return(0); */

     new_checksum = imodChecksum(mod);
     if ((new_checksum == mod->csum) || (last_checksum == new_checksum))
          return(0);

     imod_cleanup_autosave();

     if (savedir)
	  sprintf(autosave_filename, "%s/%s%s", savedir, Imod_filename,
		  Imod_autosave_string);
     else
	  sprintf(autosave_filename, "%s%s", Imod_filename,
		  Imod_autosave_string);
     
     remove(autosave_filename);
     tfilep = fopen(autosave_filename, "w");
     if (tfilep == NULL){
	  wprint("Error: Can't open autosave file %s\n", autosave_filename);
	  autosave_filename[0] = 0x00;
	  return(-1);
     }


     if (imodWrite(Model, tfilep))
	  wprint("Error: Autosave model file not saved. %s\n", 
		 autosave_filename);
     else {
	  timestr = datetime();
	  wprint("Saved autosave file %s\n", timestr);
     }

     last_checksum = new_checksum;
     imod_draw_window();
     fclose(tfilep);
     return(0);
}


int SaveModelQuit(Imod *mod)
{
     char *nfname;
     char *ff;
     FILE *fout = NULL;
     int retval = 0;
     
     /* DNM 8./4/01: Here and in next two functions, if imodv window is open,
	save the view if appropriate */
     if (!ImodvClosed)
	  imodvAutoStoreView(Imodv);

     imod_make_backup(Imod_filename);

     fout = fopen(Imod_filename, "w");
     
     if (fout == NULL){
          imod_undo_backup();
	  return(1);
     }

     retval = imodWrite(mod, fout);
     fclose(fout);

     imod_finish_backup();
     return(retval);
}


int SaveModel(struct Mod_Model *mod)
{
     FILE *fout = NULL;
     int retval;
     char *timestr;
     
     if (!ImodvClosed)
	  imodvAutoStoreView(Imodv);

     imod_make_backup(Imod_filename);

     fout = fopen(Imod_filename, "w");

     if (fout == NULL){
          imod_undo_backup();
	  retval = SaveasModel(mod);
	  return(retval);
     }

     /* DNM: save the checksum and cleanup autosaves only if the save is
	successful, otherwise leave double backups and autosaves all over the
	the place */

     if (!imodWrite(mod, fout)){
	  timestr = datetime();
	  wprint("Done saving Model %s\n%s\n", timestr, Imod_filename);
	  imod_finish_backup();
	  mod->csum = imodChecksum(mod);
	  imod_cleanup_autosave();
     }
     else
	  wprint("Imod: Error Saving Model.");

     fclose(fout);
     

     return(0);
}


static void save_model(Widget w, XtPointer client, XtPointer call)
{
     Imod *mod = (Imod *)client;
     char *filename = (char *)call;
     FILE *fout = NULL;
     int retval = -1;
     
     if ((filename == NULL) || (filename[0] == 0x00)) {
	  wprint("No file selected. Model not saved.");
	  XBell(imodDisplay(), 100);
	  return;
     }

     if (!ImodvClosed)
	  imodvAutoStoreView(Imodv);

     imod_make_backup(filename);
     fout = fopen(filename, "w");
     if (fout == NULL){
	  wprint("Error: Couldn't open %s .  Model not saved.\n", filename);
	  XBell(imodDisplay(), 100);
	  imod_undo_backup();
	  return;
     }

     mod->xmax = App->cvi->xsize;
     mod->ymax = App->cvi->ysize;
     mod->zmax = App->cvi->zsize;
     
     retval = imodWrite(mod, fout);
     fclose(fout);

     strcpy(Imod_filename, filename);
     free(filename); 
     
     if (!retval) {
	  wprint("Done saving Model\n%s\n", Imod_filename);
	  imod_finish_backup();
	  mod->csum = imodChecksum(mod);
	  imod_cleanup_autosave();
     }
     else {
	  wprint("Imod: Error Saving Model.");
	  XBell(imodDisplay(), 100);
     }
     MaintainModelName(mod);

}



int SaveasModel(struct Mod_Model *mod)
{
     diaEasyFileAct("Model Save File", (void (*)())save_model, 
		    (XtPointer)mod);
     return(0);
}

/*  THIS IS NOT USED
static void load_model(Widget w, XtPointer client, XtPointer call)
{
     Imod **rmod = (Imod **)client;
     Imod *imod = NULL;
     char *filename = (char *)call;
     FILE *fout = NULL;

     if ((filename == NULL) || (filename[0] == '/0')) {
	  wprint("No file selected for model load.\n");
	  if (filename) free(filename);
	  return;
     }

     imod = imodRead(filename);
     if (!imod){
	  wprint("Error loading %s.\n", filename);
	  return;
     }
     fclose (imod->file);
     free(filename);
     strcpy(Imod_filename, filename);
     *rmod = imod;
     
     imod->csum = imodChecksum(imod);
     if (!imod_autosave(imod)){
	  filename = (char *)malloc(strlen(Imod_filename) +
			      strlen(Imod_autosave_string) + 2);
	  sprintf(filename, "%s%s", Imod_filename, Imod_autosave_string);
	  remove(filename);
	  if (filename)
	       free(filename);
     }
     return;
}
*/

Imod *LoadModel(FILE *mfin, char *filename)
{
     FILE *fin = NULL;
     char *ff = filename;
     Imod *imod;
     
     /* Added 2.00b7:
      * ifd files confuse imod library into thinking that
      * we have a model file.  So check for ifd file.
      * Also check for bad input.
      */
     if (mfin)
	 if (imodImageFileDesc(mfin)) return(NULL);
     
     imod = (Imod *)malloc(sizeof(Imod));
     
     if (imod == NULL){
	 wprint("IMOD Model Load: Not enough memory for model.");
	  return(NULL);
     }
     
     if (mfin == NULL){
	  /*  ff = Imod_filename;  HUH? */

	  /* DNM 9/12/02: added filename argument, only go to dialog if there
	     is no filename passed in */
	  if (ff == NULL)
	       ff = dia_filename("Enter model file name to LOAD.");
	  if (ff == NULL || ff[0] == '\0') {
	       show_status("File not selected. Model not loaded.");
	       return((Imod *)NULL);
	  }
	  strcpy(Imod_filename, ff);
	  if (ff)
	       free(ff);

	  fin = fopen(Imod_filename, "r");
	  show_status("Loading...");
	  if (fin == NULL) {
	       Imod_filename[0] = 0x00;
	       return((Imod *)NULL);
	  }
	  imod->file = fin;
	  if (imodReadFile(imod)){
	     fclose(fin);
	     if (imod)
		  free(imod);
	     Imod_filename[0] = 0x00;
	     return(NULL);
	  }
	  imod->file = NULL;   /* WHY IS THIS ONE NULL, THE OTHER ONE NOT? */
	  fclose(fin);
	  imod->csum = imodChecksum(imod);
	  return(imod);
     }

     imod->file = mfin;

     if (imodReadFile(imod)){
	  if (imod)
	       free(imod);
	  return(NULL);
     }

     imod->csum = imodChecksum(imod);
     imod_cleanup_autosave();

     return(imod);
}



/*************************************************************************/
/* Testing : future code */

#ifdef WHEN_IMAGE_IO_IS_ADDED

int SaveImage(struct ViewInfo *vi)
{
     char filename[256];
     FILE *fout = NULL;
     int retval = -1;
     
     /* get file name */
     filename[0] = 0x00;
     imod_getline("Enter filename for saved image."  , filename);
     
     /* open file */
     if (fout == NULL){
	  show_status("Couldn't open File.");
	  fprintf(stderr, "Imod: couldn't open %s\n",filename);
	  return(-1);
     }
     
     
     retval = WriteImage(fout, vi, NULL);
     if (retval)
	  show_status("Error saving image file.");
     else
	  show_status("Done saving Image.");
     
     fclose( fout);
     return(retval);
}

int WriteImage(FILE *fout, struct ViewInfo *vi, struct LoadInfo *li)
{

     struct MRCheader hdata;
     double min, max, mean;
     int i, j, k;
     long xysize;
     int error= -1;
     int xmin, xmax, ymin, ymax, zmin, zmax;
     int xsize, xoff;

     xsize = li->xmax - li->xmin;

     mrc_head_new(&hdata, vi->xsize, vi->ysize, vi->zsize, 0);
     mrc_byte_mmm(&hdata, vi->idata);

     if (li){
	  mrc_head_new(&hdata, xsize,
		       li->ymax - li->ymin, li->zmax - li->zmin, 0);
	  xoff = li->xmin;
	  xsize = li->xmax - li->xmin - 1;
	  xmin = li->xmin;
	  xmax = li->xmax;
	  ymin = li->ymin;
	  ymax = li->ymax;
	  zmin = li->zmin;
	  zmax = li->zmax;
     }else{
	  xsize = vi->xsize;
	  xmin = 0;
	  xmax = vi->xsize - 1;
	  ymin = 0;
	  ymax = vi->ysize - 1;
	  zmin = 0;
	  zmax = vi->zsize - 1;
     }
	  
     mrc_head_label(&hdata, "Imod: Wrote sub image.   ");
     mrc_head_write(fout, &hdata);
     
     for (k = zmin; k <= zmax; k++){
	  
	  for(j = ymin; j <= ymax; j++){
	       
	       error = fwrite(&(vi->idata[k][(j * vi->xsize) + xmin]), 
			      1, xsize, fout);
	  }
     }
     return(error);
}
#endif

#ifdef USEIMODI
unsigned char **imod_io_image_load(ImodImageFile *im,
				   struct LoadInfo *li,
				   void (*func)(char *))
{
    unsigned char **idata;
    struct MRCheader *mrchead;
    struct MRCheader savehdr;

    if (im->file == IIFILE_MRC){
	if (!im->fp)
	    iiReopen(im);
	if (!im->fp) return NULL;
	mrchead = (struct MRCheader *)im->header;

	/* DNM: save and restore header after call to mrc_read_byte */
	savehdr = *mrchead;
	idata = (unsigned char **)mrc_read_byte(im->fp, mrchead, li, func);
	*mrchead = savehdr;

	return(idata);
    }

    {
	int i;
	int xsize, ysize, zsize, xysize;
	unsigned char **idata, *bdata;
	
	/* ignore load info for now. */
	xsize = im->nx;
	ysize = im->ny;
	zsize = im->nz;
	xysize = xsize * ysize;
	
	im->llx = 0;
	im->lly = 0;
	im->urx = xsize;
	im->ury = ysize;

	im->slope  = li->slope;
	im->offset = li->offset;
	im->imin   = li->imin;
	im->imax   = li->imax;
	im->axis   = li->axis;

	idata = (unsigned char **)malloc(zsize * sizeof(unsigned char *));
	if (!idata) return NULL;
	bdata = malloc(xysize * zsize * sizeof(unsigned char));
	if (!bdata) return NULL;
	for(i = 0; i < zsize; i++){
	    idata[i] = bdata + (xysize * i);
	    iiReadSectionByte(im, (char *)idata[i], i);
	}
	return(idata);
    }
}
#endif

/* DNM 9/12/02: deleted old version of imod_io_image_reload */
