/*  IMOD VERSION 2.02
 *
 *  imodel_from.c -- Make an imod model from other file formats.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <stdio.h>
#include <string.h>
#include "imodel.h"
#include "b3dutil.h"

#define MAXLINE 128
#define MAXOBJ  256

#ifdef NEEDWMOD
/* Display 247 - 255 */
float Wmod_Colors[9][3]  =   { 0.90, 0.82, 0.37,  /* Dim Yellow  */
			       0.54, 0.51, 0.01,  /* Olive Brown */
			       0.94, 0.49, 0.0,   /* Orange      */
			       1.00, 0.0,  0.0,   /* Red         */
			       0.0,  1.0,  0.0,   /* Green       */
			       0.0,  0.0,  1.0,   /* Blue        */
			       1.0,  1.0,  0.0,   /* Yellow      */
			       1.0,  0.0,  1.0,   /* Magenta     */
			       0.0,  1.0,  1.0    /* Cyan        */
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

     while ( ((len = imodel_from_fgetline(fin,line, MAXLINE)) > 0)){
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
	  
	  imodel_from_fgetline(fin,line,MAXLINE);
	  sscanf(line, "%*s %*s %*s %d", &points);
	  imodel_from_fgetline(fin,line,MAXLINE);
	  sscanf(line, "%*s %*s %d", &display_switch);
	  if (display_switch > 0){
	       objlookup[display_switch] = 1;
	  }
     }
     
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

     mod = imodNew();
     
     
     if (!mod){
	  b3dError(stderr, "Couldn't get new model\n");
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

     while ( ((len = imodel_from_fgetline(fin,line, MAXLINE)) > 0)){
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

	  imodel_from_fgetline(fin,line,MAXLINE);
	  sscanf(line, "%*s %*s %*s %d", &points);
	  imodel_from_fgetline(fin,line,MAXLINE);
	  sscanf(line, "%*s %*s %d", &display_switch);
	  
	  imodel_from_fgetline(fin,line,MAXLINE);
	  


	  mod->cindex.object = objlookup[display_switch];
	  if (mod->cindex.object < 0){
	       mod->cindex.object;
	       
	  }
	  if (mod->cindex.object > 8)
	       mod->cindex.object = 8;


	  obj = &(mod->obj[mod->cindex.object]);
	  mod->cindex.contour = obj->contsize - 1;
	  NewContour(mod);

	  /* Go through and collect points. */
	  for (i = 0; i < points; i++){
	       imodel_from_fgetline(fin,line,MAXLINE);
	       sscanf(line,"%*d %f %f %f", &(point.x), &(point.y), &(point.z));
	       imodel_point_add( &(obj->cont[mod->cindex.contour]), 
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
	       imodFreeObject(mod, i--);
     }


     return(mod);
     
}


#endif


/*
struct Mod_Model *imod_from_synu(FILE *fin)
{
     return(NULL);
}
*/

int substr(char bs[], char ls[])
{
     int i,len;
     
     len = strlen(ls);
     
     for (i = 0; i < len; i++){
	  if (bs[i] == ls[i])
	       continue;
	  else
	       return(0);
     }
     return(1);
     
}


int imodel_from_fgetline(FILE *fp, char s[],int limit)
{
     int c, i, length;

     if (fp == NULL){
	  b3dError(stderr, "fgetline: file pointer not valid\n");
	  return(0);
     }

     if (limit < 3){
	  b3dError(stderr, "fgetline: limit (%d) > 2,\n", limit);
	  return(0);
     }
     
     for (i=0; ( ((c = getc(fp)) != EOF) && (i < (limit-1)) && (c != '\n') ); i++)
	  s[i]=c;
     
     if (i == 1){
	  if (c == EOF){
	       return(0);
	  }
	  if (c == '\n'){
	       s[++i] = '\0';
	       return(1);
	  }
     }
	       

     s[i]='\0';
     length = i;

     if (c == EOF)
	  return (-1 * length);
     else
	  return (length);
}
