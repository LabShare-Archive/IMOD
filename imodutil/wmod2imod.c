/*
 *  $Id$
 *
 *  Author: James Kremer email: kremer@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */


#include <stdio.h>
#include <string.h>

#include "imodel.h"

float Xscale = 1.0;
float Yscale = 1.0;
float Zscale = 1.0;

int main( int argc, char *argv[])
{


  int i;
  FILE *fin, *fout;
  struct Mod_Model *Model;

  if (argc < 2){
      
    printf("wmod2imod version 1.0 usage:\n");
    printf("wmod2imod [-z scale] [wmod filename] [imod filename]\n");
    exit(1);

  }


  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
            
      case 'x':
        sscanf(argv[++i], "%f", &Xscale);
        break;
            
      case 'y':
        sscanf(argv[++i], "%f", &Yscale);
        break;
            
      case 'z':
        sscanf(argv[++i], "%f", &Zscale);
        break;
            
      default:
        break;
      }
    }else
      break;
  }
  if (i > (argc - 2)){
    printf("i = %d\n",i);
    printf("wmod2imod version 0.9 usage:\n");
    printf("wmod2imod [-z scale] [wmod filename] [imod filename]\n");
    exit(1);
  }


  fin = fopen(argv[i++], "r");
  if (!fin){
    fprintf(stderr, "Couldn't open %s\n", argv[i]);
    exit(3);
  }

  fout = fopen(argv[i], "wb");
  if (!fout){
    fprintf(stderr, "Couldn't open %s\n", argv[i]);
    exit(3);
  }

  Model = (struct Mod_Model *)imod_from_wmod(fin);
     
  if (Model == NULL){
    fprintf(stderr, "Error reading imod file.\n");
    exit(3);
  }
     

  Model->zscale = Xscale;
  Model->yscale = Yscale;
  Model->zscale = Zscale;
     
  imodWrite(Model, fout);

  imodDelete(Model);
  exit(0);
}




#include <stdio.h>
#include <string.h>
#include "imodel.h"

#define MAXLINE 128
#define MAXOBJ  256

/* Display 247 - 255 */
float Wmod_Colors[9][3]  =   { {0.90, 0.82, 0.37},  /* Dim Yellow  */
                               {0.54, 0.51, 0.01},  /* Olive Brown */
                               {0.94, 0.49, 0.0},   /* Orange      */
                               {1.00, 0.0,  0.0},   /* Red         */
                               {0.0,  1.0,  0.0},   /* Green       */
                               {0.0,  0.0,  1.0},   /* Blue        */
                               {1.0,  1.0,  0.0},   /* Yellow      */
                               {1.0,  0.0,  1.0},   /* Magenta     */
                               {0.0,  1.0,  1.0}    /* Cyan        */
};



struct Mod_Model *imod_from_wmod(FILE *fin)
{
  int display_switch;
  int points;
  int len;
  int i;
     
  char cont_string[] = "Object #:";
  char line[MAXLINE];
  char *tline;

  int objlookup[MAXOBJ];
  int nobj = 0;

  struct Mod_Model *mod;
  struct Mod_Object *obj;
  struct Mod_Point point;
     
  for(i = 0; i < MAXOBJ; i++)
    objlookup[i] = 0;

  while ( ((len = fgetline(fin,line, MAXLINE)) >= 0)){
    if (!len)
      continue;
    tline = NULL;
    for(i = 0; line[i]; i++){
      if (line[i] == 'O'){
        tline = &(line[i]);
        break;
      }
    }
    if (!tline)
      continue;
    if (!substr(tline, cont_string))
      continue;
      
    fgetline(fin,line,MAXLINE);
    sscanf(line, "%*s %*s %*s %d", &points);
    fgetline(fin,line,MAXLINE);
    sscanf(line, "%*s %*s %d", &display_switch);
    if (display_switch >= 0){
      objlookup[display_switch] = 1;
    }
  }

  mod = imodNew();     
  for(i = 0; i < MAXOBJ; i++){
    if (objlookup[i]){
      objlookup[i] = nobj;

      imodNewObject(mod);

      if (i >= 247){
        mod->obj[nobj].red   = Wmod_Colors[i-247][0];
        mod->obj[nobj].green = Wmod_Colors[i-247][1];
        mod->obj[nobj].blue  = Wmod_Colors[i-247][2];

      }else{
        mod->obj[nobj].red   = (float)i / 255.0f;
        mod->obj[nobj].green = (float)i / 255.0f;
        mod->obj[nobj].blue  = (float)i / 255.0f;
      }
      sprintf(mod->obj[nobj].name, "Wimp no. %d", i);
      nobj++;

    }else{
      objlookup[i] = -1;
    }
  }


     
     
  if (!mod){
    fprintf(stderr, "Couldn't get new model\n");
    return(NULL);
  }
     
  /*
    for (i = 0; i < 9; i++){
    imodNewObject(mod);
    mod->obj[i].red   = Wmod_Colors[i][0];
    mod->obj[i].green = Wmod_Colors[i][1];
    mod->obj[i].blue  = Wmod_Colors[i][2];
    }
  */   
  rewind(fin);

  while ( ((len = fgetline(fin,line, MAXLINE)) >= 0)){
    if (!len)
      continue;
    /* Search for the given type of contour. */
      
    tline = NULL;
    for(i = 0; line[i]; i++){
      if (line[i] == 'O'){
        tline = &(line[i]);
        break;
      }
    }
    if (!tline)
      continue;
    if (!substr(tline, cont_string))
      continue;

    fgetline(fin,line,MAXLINE);
    sscanf(line, "%*s %*s %*s %d", &points);
    fgetline(fin,line,MAXLINE);
    sscanf(line, "%*s %*s %d", &display_switch);
      
    fgetline(fin,line,MAXLINE);

    mod->cindex.object = objlookup[display_switch];
    if (mod->cindex.object < 0){
      mod->cindex.object = 0;
    }

    obj = &(mod->obj[mod->cindex.object]);
    mod->cindex.contour = obj->contsize - 1;
    imodNewContour(mod);

    /* Go through and collect points. */
    for (i = 0; i < points; i++){
      fgetline(fin,line,MAXLINE);
      sscanf(line,"%*d %f %f %f", &(point.x), &(point.y), &(point.z));
      imodPointAdd( &(obj->cont[mod->cindex.contour]), 
                        &point, i);
           
    }
    obj->cont[mod->cindex.contour].psize = points;
      
    for (i = 0; i < mod->objsize; i++){
      obj = &(mod->obj[i]);
      if (obj->contsize){
        if (obj->cont[0].psize < 2)
          obj->flags |= IMOD_OBJFLAG_OPEN;
        else
          if (obj->cont[0].pts[0].z != obj->cont[0].pts[1].z)
            obj->flags |= IMOD_OBJFLAG_OPEN;
      }
    }
      
          
  }

  for(i = 0; i < mod->objsize; i++){
    if (!mod->obj[i].contsize)
      imodDeleteObject(mod, i--);
  }


  return(mod);
     
}
