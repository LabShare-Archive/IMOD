/* 
 *  objgroup.c -- Routines for managing groups of object numbers
 *
 *  Author: David Mastronarde  email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 * Log at end
 */

#include "string.h"
#include "objgroup.h"
#include "imodel.h"

/*! Returns a new object group with the object list set to NULL and an empty
  string for the name.  Returns NULL for memory error */
IobjGroup *objGroupNew()
{
  IobjGroup *group = (IobjGroup *)malloc(sizeof(IobjGroup));
  if (!group)
    return NULL;
  group->objList = NULL;
  group->name[0] = 0x00;
  return group;
}

/*! Frees [group] and all its data */
void objGroupDelete(IobjGroup *group)
{
  objGroupClear(group);
  if (group)
    free(group);
}

/*! Clears out the data in [group]: frees the object list and sets the name
  to an empty string */
void objGroupClear(IobjGroup *group)
{
  if (!group)
    return;
  ilistDelete(group->objList);
  group->name[0] = 0x00;
  group->objList = NULL;
}

/*! Returns a duplicate of the object group [oldGroup] */
IobjGroup *objGroupDup(IobjGroup *oldGroup)
{
  IobjGroup *newGroup = objGroupNew();
  if (!newGroup)
    return NULL;
  memcpy(newGroup->name, oldGroup->name, OBJGRP_STRSIZE);
  newGroup->objList = ilistDup(oldGroup->objList);
  if (newGroup->objList)
    return newGroup;
  objGroupDelete(newGroup);
  return NULL;
}

/*! Appends one object number [o] to [group]. Returns 1 for memory error */
int objGroupAppend(IobjGroup *group, int ob)
{
  if (!group->objList) {
    group->objList = ilistNew(sizeof(b3dInt32), 16);
    if (!group->objList)
      return 1;
    ilistQuantum(group->objList, 16);
  }
  return ilistAppend(group->objList, &ob);
}

/*! Returns the index of object [ob] if it is in [group], otherwise -1 */
int objGroupLookup(IobjGroup *group, int ob)
{
  int i;
  int *j;
  for (i = 0; i < ilistSize(group->objList); i++) {
    j = (int *)ilistItem(group->objList, i);
    if (ob == *j)
      return i;
  }
  return -1;
}

/*! Adds a new object group to the list pointed to by [grplistp], allocating
 a new group list if necessary and returning the address in [grplistp].  
 Returns a pointer to the new group or NULL for memory error. */
IobjGroup *objGroupListExpand(Ilist **grplistp)
{
  IobjGroup *group;
  int err;
  if (!*grplistp) {
    *grplistp = ilistNew(sizeof(IobjGroup), 4);
    if (!*grplistp)
      return NULL;
  }
  group = objGroupNew();
  if (!group)
    return NULL;
  err = ilistAppend(*grplistp, group);
  objGroupDelete(group);
  if (err)
    return NULL;
  return (IobjGroup *)ilistItem(*grplistp, ilistSize(*grplistp) - 1);
}

/*! Returns a duplicate of the object group list [grplist] including all its
   data; returns NULL for error. */
Ilist *objGroupListDup(Ilist *grplist)
{
  int i, j;
  IobjGroup *ogroup, *ngroup;
  Ilist *newlist = ilistDup(grplist);
  if (!newlist)
    return NULL;
  for (i = 0; i < ilistSize(grplist); i++) {
    ogroup = (IobjGroup *)ilistItem(grplist, i);
    ngroup = (IobjGroup *)ilistItem(newlist, i);
    ngroup->objList = ilistDup(ogroup->objList);
    
    /* If there is an error duping a list, need to free all duped lists */
    if (!ngroup->objList) {
      for (j = 0; j < i; j++) {
        ngroup = (IobjGroup *)ilistItem(newlist, j);
        ilistDelete(ngroup->objList);
      }
      ilistDelete(newlist);
      return NULL;
    }
  }
  return newlist;
}

/*! Frees the object group list [grplist] and all its data */
void objGroupListDelete(Ilist *grplist)
{
  int i;
  for (i = 0; i < ilistSize(grplist); i++)
    objGroupClear((IobjGroup *)ilistItem(grplist, i));
  if (grplist)
    free(grplist);
}

/*! Removes one object group at index [item] from the group list [grplist],
  freeing its data first.  Returns 1 for inappropriate index. */
int objGroupListRemove(Ilist *grplist, int item)
{
  IobjGroup *group = (IobjGroup *)ilistItem(grplist, item);
  if (!group)
    return 1;
  objGroupClear(group);
  ilistRemove(grplist, item);
  return 0;
}

/*! Returns a byte count for the object group list in [grplist] */
int objGroupListBytes(Ilist *grplist)
{
  IobjGroup *group;
  int i;
  int count = ilistSize(grplist) * sizeof(IobjGroup);
  for (i = 0; i < ilistSize(grplist); i++) {
    group = (IobjGroup *)ilistItem(grplist, i);
    if (group)
      count += ilistSize(group->objList) * 4 + sizeof(Ilist); 
  }
  return count;
}

/*! Returns a checksum value for the object group list in [grplist] */
double objGroupListChecksum(Ilist *grplist)
{
  IobjGroup *group;
  int i, j;
  b3dInt32 *objs;
  double count = ilistSize(grplist);
  for (i = 0; i < ilistSize(grplist); i++) {
    group = (IobjGroup *)ilistItem(grplist, i);
    if (!group)
      continue;
    for (j = 0; j < strlen(group->name); j++)
      count += (int)group->name[j];
    count += ilistSize(group->objList);
    objs = (b3dInt32 *)ilistFirst(group->objList);
    if (objs)
      for (j = 0; j < ilistSize(group->objList); j++)
        count += objs[j];
  }
  return count;
}

/*! Writes the group list [grplist] to the model file with pointer [fout].  
  Returns 1 for error */
int objGroupListWrite(Ilist *grplist, FILE *fout)
{
  int i, j, id, size, num;
  b3dInt32 *objs;
  IobjGroup *group;

  for (i = 0; i < ilistSize(grplist); i++) {
    group = (IobjGroup *)ilistItem(grplist, i);
    if (!group)
      return IMOD_ERROR_FORMAT;
    id = ID_OGRP;
    imodPutInt(fout, &id);
    num = ilistSize(group->objList);
    size = OBJGRP_STRSIZE + 4 * num;
    imodPutInt(fout, &size);
    imodPutBytes(fout, (unsigned char *)&(group->name[0]), OBJGRP_STRSIZE);
    if (num) {
      objs = (b3dInt32 *)ilistFirst(group->objList);
      if (!objs)
        return IMOD_ERROR_FORMAT;
      imodPutInts(fout, objs, num);
    }
  }
  return 0;
}

/*! Reads one object group from the model file pointed to by [fin] into the
  model [imod].  Returns 1 for error. */
int objGroupRead(Ilist **grplistp, FILE *fin)
{
  int i, size, num;
  IobjGroup *group;
  b3dInt32 *objs;
  size = imodGetInt(fin);
  num = (size - OBJGRP_STRSIZE) / 4;
  group = objGroupListExpand(grplistp);
  if (!group)
    return IMOD_ERROR_MEMORY;
  imodGetBytes(fin,  (unsigned char *)(&group->name[0]), OBJGRP_STRSIZE);
  group->objList = ilistNew(sizeof(b3dInt32), B3DMAX(num, 16));
  if (!group->objList)
    return IMOD_ERROR_MEMORY;

  /* Read the data directly into the list structure and set size directly */
  objs = (b3dInt32 *)group->objList->data;
  if (!objs)
    return IMOD_ERROR_MEMORY;
  ilistQuantum(group->objList, 16);
  if (num) {
    imodGetInts(fin, objs, num);
    group->objList->size = num;
  }
  if ((i = ferror(fin)))
    return(i);
  return 0;
}

/*

$Log$
Revision 1.1  2008/01/27 06:19:26  mast
Added to library


*/
