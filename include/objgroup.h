/*
 *  object.h -- Header file for object group routines
 *
 * $Id$
 * $Log$
 */

#ifndef OBJGROUP_H
#define OBJGROUP_H

#include <stdio.h>
#include "ilist.h"

#ifdef __cplusplus
extern "C" {
#endif

/* DOC_CODE IobjGroup structure */
#define OBJGRP_STRSIZE  32

typedef struct object_group {
  Ilist *objList;
  char name[OBJGRP_STRSIZE];
} IobjGroup;
/* END_CODE */

Ilist *objGroupListDup(Ilist *grplist);
void objGroupListDelete(Ilist *grplist);
IobjGroup *objGroupListExpand(Ilist **grplistp);
int objGroupListRemove(Ilist *grplist, int item);
IobjGroup *objGroupNew();
void objGroupDelete(IobjGroup *group);
void objGroupClear(IobjGroup *group);
IobjGroup *objGroupDup(IobjGroup *oldGroup);
int objGroupAppend(IobjGroup *group, int ob);
int objGroupLookup(IobjGroup *group, int ob);
int objGroupListWrite(Ilist *grplist, FILE *fout);
int objGroupRead(Ilist **grplistp, FILE *fin);
int objGroupListBytes(Ilist *grplist);
double objGroupListChecksum(Ilist *grplist);

#ifdef __cplusplus
}
#endif

#endif
