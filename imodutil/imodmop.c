/*  IMOD VERSION 2.30
 *
 *  imodmop.c --  Create an mrc image file from a model
 *                and an mrc image file.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

/*
 * ToDo:
 * 1) Add red, green or blue filters to model colors.
 * 2) Finish short and float type data. ONLY WORKS ON BYTE DATA NOW.
 */

#include <stdio.h>
#include <math.h>
#include "imodel.h"
#include "mrcfiles.h"

int Mop_red = FALSE;
int Mop_green = FALSE;
int Mop_blue = FALSE;
int Mop_help = FALSE;

int modpaint(FILE *mfin, FILE *gfin, FILE *gfout, 
             struct Mod_Model *model,
             struct MRCheader *hdata, int invert);


int main( int argc, char *argv[])
{
     
  FILE *gfin  = NULL;
  FILE *gfout = NULL;
  FILE *mfin = NULL;

  struct Mod_Model model;
  struct MRCheader hdata;
  int i;
  int invert = 0;
  char *progname = imodProgName(argv[0]);


  if (argc == 1){
    imodVersion(progname);
    imodCopyright();
    fprintf(stderr,
            "Usage: mop [-hrgbi] [model file] [input image] [output image]\n");
    fprintf(stderr, "use mop -h for help\n");
    exit(3);
  }

  for (i = 1; i < argc; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){

      case 'h': /* help */
        printf("\n\tMop stands for MOdel Painter\n");
        printf("\tOptions -h,help  -r,red -g,green -b,blue -i,invert\n");
        printf("\tModel file is an imod model file");
        printf("\tInput image is an mrc image file");
        printf("\tOutput image is the image data inside (or outside) of the model\n");
        exit(1);
        break;

      case 'r':
        Mop_red = TRUE;
        break;

      case 'g':
        Mop_green = TRUE;
        break;

      case 'b':
        Mop_blue = TRUE;
        break;

      case 'i':
        invert = 1;
        break;

      }
    }
    else
      break;
  }

  if (((argc - i) < 3) & (!Mop_help)){
    fprintf(stderr,
            "Usage: mop [-hrgbi] [model file] [input image] [output image]\n");
    exit(3);
  }


  mfin = fopen(argv[i++], "rb");
  if (mfin == NULL){
    fprintf(stderr, "MOP: Couldn't open %s\n", argv[1]);
    exit(3);
  }
     
  gfin = fopen(argv[i++], "rb");
  if (gfin == NULL){
    fprintf(stderr, "MOP: Couldn't open %s\n", argv[2]);
    exit(3);
  }
     

  gfout = fopen(argv[i], "wb");
  if (gfout == NULL){
    fprintf(stderr, "MOP: Couldn't open %s\n", argv[3]);
    exit(3);
  }
     

  model.file = mfin;
  if (imodReadFile(&model)){
    fprintf(stderr, "MOP: Error reading modelfile %s.\n", argv[1]);
    exit(3);
  }
     
     
  if (mrc_head_read(gfin, &hdata)){
    fprintf(stderr, "MOP: Can't Read Header for %s.\n", argv[2]);
    exit(3);
  }
     
  modpaint(mfin, gfin, gfout, &model, &hdata, invert);


  fclose(gfout);
  fclose(gfin);
  fclose(mfin);
  printf("\n");
  return(0);
}     
