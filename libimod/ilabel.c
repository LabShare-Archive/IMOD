/*  IMOD VERSION 2.02
 *
 *  ilabel.c -- labels for points and contours.
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
/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.4  2004/11/05 18:53:00  mast
Include local files with quotes, not brackets

Revision 3.3  2004/09/21 20:10:42  mast
Added tag argument for label writing

Revision 3.2  2003/02/27 00:34:40  mast
add projects' *.dsw *.dsp

Revision 3.1  2003/02/21 22:20:37  mast
Use new b3d types

*/

#include <string.h>
#include "imodel.h"

/*#include <unistd.h>*/

/* create a new empty label. */
Ilabel *imodLabelNew()
{
  Ilabel *label = (Ilabel *)malloc(sizeof(Ilabel));
  label->nl     = 0;
  label->name   = NULL;
  label->label  = NULL;
  label->len    = 0;
  return(label);
}

/* delete a label, free all data. */
void imodLabelDelete(Ilabel *label)
{
  int l;
  if (!label)
    return;

  for(l = 0; l < label->nl; l++){
    if (label->label[l].name)
      free(label->label[l].name);
  }
  if (label->name)
    free(label->name);
  if (label->label)
    free(label->label);
  free(label);
}

/* Duplicate a label; return NULL if error or no existing label */
Ilabel *imodLabelDup(Ilabel *label)
{
  Ilabel *newLabel;
  int i, len = 0;

  if (!label)
    return(NULL);

  newLabel = imodLabelNew();
  if (!newLabel)
    return(NULL);

  if (label->name){
    len = strlen(label->name);
    
    if (label->len){
      newLabel->name = malloc(len+1);
      memcpy(newLabel->name, label->name, len+1);
      if (!newLabel->name)
        return(NULL);
      newLabel->len = len;
    }
  }

  for (i = 0; i < label->nl; i++)
    imodLabelItemAdd(newLabel, label->label[i].name, label->label[i].index);
  return newLabel;
}

/* Give the label a title given by val. */
void imodLabelName(Ilabel *label, char *val)
{
  int len;

  if ((!label) || (!val)) return;
  len = strlen(val);
     
  if (!(label->name)){
    label->name = malloc(len+1);
    label->len  = len+1;
  }else{
    if (label->len < (len + 1)){
      if (label->name) free(label->name);
      label->name = malloc(len+1);
      label->len  = len+1;
    }
  }
  memcpy(label->name, val, len+1);
  return;
}

/* Add a label item with the title val and the value given by index. */
void imodLabelItemAdd(Ilabel *label, char *val, int index)
{
  IlabelItem *li;
  int i=0, len;
  char *name;

  if ((!label) || (!val)) return;

  len = strlen(val);
  if (len <= 0) return;

  for (i = 0; i < label->nl; i++)
    if (index == label->label[i].index){
      if (label->label[i].len < (len + 1)){
        name = malloc(len + 1);
        if (label->label[i].name) free (label->label[i].name);
        label->label[i].name = name;
        label->label[i].len  = len+1;
      }
      memcpy(label->label[i].name, val, len+1);
      return;
    }
     
  li = (IlabelItem *)malloc(sizeof(IlabelItem) * (label->nl + 1));
  if (label->nl > 0){
    for (i = 0; i < label->nl; i++)
      li[i] = label->label[i];
    free(label->label);
  }

  li[i].name  = malloc(len + 1);
  li[i].index = index;
  li[i].len   = len+1;
  memcpy(li[i].name, val, len + 1);

  label->label = li;
  label->nl++;
  return;
}

void imodLabelItemMove(Ilabel *label, int to_index, int from_index)
{
  int i;
  if (!label) return;

  for(i = 0; i < label->nl; i++)
    if (from_index == (label->label[i].index)){
      label->label[i].index = to_index;
      break;
    }
  return;
}

void imodLabelItemDelete(Ilabel *label, int index)
{
  int i;
  int deli = -1;

  if (!label) return;

  for(i = 0; i < label->nl; i++)
    if (index == label->label[i].index){
      deli = i;
      break;
    }
  if (deli < 0) return;
  label->nl--;
  for(i = deli; i < label->nl; i++)
    label->label[i] = label->label[i+1];
  return;
}

char *imodLabelItemGet(Ilabel *label, int index)
{
  int i;

  if ((!label) || (!label->nl)) return(NULL);
  for(i = 0; i < label->nl; i++)
    if (index == label->label[i].index)
      return(label->label[i].name);
  return(NULL);
}

void imodLabelPrint(Ilabel *lab, FILE *fout)
{
  if (!lab) return;
  if (lab->name)
    fprintf(fout, "contour label : \"%s\"\n", lab->name);
  if (lab->nl){
    int i;
    for(i = 0; i < lab->nl; i++)
      fprintf(fout, "\t%3d : \"%s\"\n", lab->label[i].index,
              lab->label[i].name);
  }
  return;
}


/*
 * return true if label name matches the test strin tstr.
 */
int imodLabelMatch(Ilabel *label, char *tstr)
{
  if ((!label) || (!tstr))
    return(0);
     
  return(ilabelMatchReg(tstr, label->name));
}


int imodLabelItemMatch(Ilabel *label, char *tstr, int index)
{
  char *lstr;
  if ((!label) || (!tstr))
    return(0);

  lstr = imodLabelItemGet(label, index);
  if (!lstr) return(0);
     
  return(ilabelMatchReg(tstr, lstr));
}


int ilabelMatchReg(char *exp, char *str)
{
  int i, len;
  int n = -1;
     
  if ((!exp) || (!str)) return(0);
     
  len = strlen(str);
     
  for(i = 0; i < len; i++){
    if (!exp[0]) return(0);
      
    if (exp[0] == '\\'){
      exp++;
    }else{
      if (exp[0] == '?'){
        exp++;
        continue;
      }
      if (exp[0] == '*'){
        n = exp[1];
        if (!n) return(1);
        if (n == str[i]){
          if (ilabelMatchReg(exp+1, &str[i]))
            return(1);
        }
        continue;
      }
    }
      
    if (exp[0] != str[i])
      return(0);
    exp++;
  }
  if (exp[0]) return(0);
  return(1);
}



/*****************************************************************************/
/* file io */
/* format

bytes data
----------
4     'LABL'
4     length of entire label data structures

4     # of label items
4     size of contour label
size  contour label string padded to 4 byte chunks.

for each label item:

4     size
size  data.

*/

static int getpadlen(char *string)
{
  int len,pad;
  if (!string) return 0;
  len = strlen(string) + 1;
  if (!len) return 0;
  pad = len%4;
  len /= 4;
  len *= 4;
  if (pad) len += 4;
  return len;
}


int imodLabelWrite(Ilabel *lab, b3dUInt32 tag, FILE *fout)
{
  b3dUInt32 id;
  b3dInt32 l, len, pad, lpad;
  b3dInt32 bgnpos, endpos, datasize;

  if (!lab) return -1;

  bgnpos = ftell(fout);
  imodPutInt(fout, &tag);

  /* Calculate lenth of data to be written. */
  id  = 16;
  len = getpadlen(lab->name);
  id += len;
  for(l = 0; l < lab->nl; l++){
    id += 4;
    id += getpadlen(lab->label[l].name);
  }
  imodPutInt(fout, &id);
     
  /* write the number of labels. */
  imodPutInt(fout, &lab->nl);
  lpad = getpadlen(lab->name);
  imodPutInt(fout, &lpad);
     
  if (lab->name){
    len = strlen(lab->name);
    imodPutBytes(fout, (unsigned char *)lab->name, len);
    pad = lpad - len;
    if (pad>0){
      id = 0;
      imodPutBytes(fout, (unsigned char *)&id, pad);
    }
  }

  for(l = 0; l < lab->nl; l++){
    imodPutInt(fout, &lab->label[l].index);
    lpad = getpadlen(lab->label[l].name);
    len = strlen(lab->label[l].name);
    pad = lpad - len;

    imodPutInt(fout, &lpad);
    imodPutBytes(fout, (unsigned char *)lab->label[l].name, len);
    if (pad>0){
      id = 0;
      imodPutBytes(fout, (unsigned char *)&id, pad);
    }
    if (ferror(fout))
      return(IMOD_ERROR_WRITE);
  }
     
  if (ferror(fout))
    return(IMOD_ERROR_WRITE);
  else
    return(0);
}

Ilabel *imodLabelRead(FILE *fin, int *err)
{
  int retcode = 0;
  Ilabel *lab = imodLabelNew();
  b3dInt32 ml, l;

  if (!lab){
    *err = IMOD_ERROR_MEMORY;
    return(NULL);
  }

  /* size of data chunk. */
  ml = imodGetInt(fin);

  /* number of labels. */
  ml = lab->nl = imodGetInt(fin);

  /* The name of the label list. */
  lab->len   = imodGetInt(fin);
  lab->name  = (char *)malloc(lab->len);
  imodGetBytes(fin, (unsigned char *)lab->name, lab->len);

  /* The label list. */
  lab->label = (IlabelItem *)malloc(sizeof(IlabelItem) * ml);
  for(l = 0; l < lab->nl; l++){
    lab->label[l].index = imodGetInt(fin);
    lab->label[l].len   = imodGetInt(fin);
    lab->label[l].name  = (char *)malloc(lab->label[l].len);
    imodGetBytes(fin, (unsigned char *)lab->label[l].name, 
                 lab->label[l].len);
  }

  *err = retcode;
  return(lab);
}

