/* 
 *  ilist.c -- Model list structure library routines.
 *
 *  Original author: James Kremer
 *  Modified by David Mastronarde  email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.2  2004/11/21 06:15:27  mast
Fixed a declaration for Windows

*/

#include <stdlib.h>
#include <string.h>
#include "ilist.h"

static void ilistShift(Ilist *list, int from, int to);

/* Get a new list with data size given by dsize, initial allocation by asize */
Ilist *ilistNew(int dsize, int asize)
{
  Ilist *l = (Ilist *)malloc(sizeof(Ilist));

  if (!l)
    return(NULL);
  l->dsize = dsize;
  l->current = -1;
  l->size  = 0;
  l->store = asize;
  l->data = NULL;
  l->quantum = LIST_QUANTUM;
  if (asize) {
    l->data = malloc(asize * dsize);
    if (!l->data) {
      ilistDelete(l);
      return(NULL);
    }
  }
  return(l);
}

/* Limit the size of the list */
void   ilistTruncate(Ilist *list, int size)
{
  if (size >= 0 && size < list->size)
    list->size = size;
}

/* Set the quantum size for increasing the list when needed */
void   ilistQuantum(Ilist *list, int size)
{
  if (size > 0)
    list->quantum = size;
}

/* Duplicate a list */
Ilist *ilistDup(Ilist *list)
{
  void *dsave;
  Ilist *newList;
  if (!list)
    return(NULL);

  /* First get a new list, then copy the structure, saving the data pointer */
  newList = ilistNew(list->dsize, list->store);
  if (!newList)
    return(NULL);
  dsave = newList->data;
  memcpy(newList, list, sizeof(Ilist));
  newList->data = dsave;
  if (list->size)
    memcpy(newList->data, list->data, list->dsize * list->size);
  return(newList);
}


/* Delete the list, free all memory */
void   ilistDelete (Ilist *list)
{
  if (!list) 
    return;
  if (list->data) 
    free(list->data);
  free(list);
}

/* Get the first list item */
void *ilistFirst(Ilist *list)
{
  if (!list || !list->size)
    return(NULL);
  list->current = 0;
  return(list->data);
}

/* Get the next list item */
void *ilistNext(Ilist *list)
{
  char *rptr;
  list->current++;
  if (list->current >= list->size)
    return(NULL);
  rptr = (char *)list->data + (list->current * list->dsize);
  return((void *)rptr);
}

/* Get the last item from the list */
void  *ilistLast(Ilist *list)
{
  char *rptr;
  if (!list || !list->size)
    return(NULL);
  list->current = list->size - 1;
  rptr = (char *)list->data + (list->current * list->dsize);
  return((void *)rptr);
}

/* Get the list item at the index given by element */
void  *ilistItem(Ilist *list, int element)
{
  char *rptr;

  if (!list || !list->size || element < 0 || element >= list->size)
    return(NULL);
  list->current = element;
  rptr = (char *)list->data + (list->current * list->dsize);
  return((void *)rptr);
}

/* Return size of list */
int    ilistSize(Ilist *list)
{
  if (!list) return(0);
  return(list->size);
}

/* Append data to the end of the list; return 1 if memory error */
int   ilistAppend(Ilist *list, void *data)
{
  char *to;
  if (list->store <= list->size){
    if (list->store)
      list->data = realloc(list->data, 
                           list->dsize * (list->size + list->quantum));
    else
      list->data = malloc(list->dsize * list->quantum);
    if (!list->data)
      return 1;
    list->store += list->quantum;
  }

  to = (char *)list->data + (list->size * list->dsize);
  list->size++;
  memcpy(to, data, list->dsize);
  return 0;
}

/* Remove the given element from the list */
void   ilistRemove(Ilist *list, int element)
{
  int i;
  char *to, *from;

  if (list->size <= element)
    return;

  ilistShift(list, element + 1, -1);
  list->size--;
}

/* Swap the two elements on the list; return 1 if error */
int   ilistSwap  (Ilist *list, int e1, int e2)
{
  char *p1, *p2;
  void *tptr;

  if (e1 >= list->size || e2 >= list->size)
    return 1;

  p1 = p2 = (char *)list->data;
  p1 += (e1 * list->dsize);
  p2 += (e2 * list->dsize);
  tptr = malloc(list->dsize);
  if (!tptr)
    return 1;

  memcpy(tptr, p1, list->dsize);
  memcpy(p1,   p2, list->dsize);
  memcpy(p2, tptr, list->dsize);
  free(tptr);
  return 0;
}

/* Insert an item on the front of the list; return 1 if error */
int ilistPush(Ilist *list, void *data)
{
  return ilistInsert(list, data, 0);
}

/* Pop an item off the front of the list; return NULL if error
 * the item has been malloc'ed and must be freed */
void *ilistPop(Ilist *list)
{
  void *data;
     
  if (!list->size) 
    return (NULL);
  data = (void *)malloc(list->dsize);
  if (!data)
    return(NULL);
  memcpy(data, list->data, list->dsize);
  ilistRemove(list, 0);
  return(data);
}

/* Move the given element to the front of the list; return 1 if error */
int ilistFloat(Ilist *list, int element)
{
  void *data;
  char *p1;
  int err;

  if (element < 1 || element >= list->size)
    return 1;

  p1 = (char *)list->data + (element * list->dsize);
  data = (void *)malloc(list->dsize);
  if (!data)
    return 1;

  memcpy(data, p1, list->dsize);
  ilistRemove(list, element);
  err = ilistPush(list, data);
  free(data);
  return err;
}

/* Insert an item at the position given by element; return 1 if error */
int   ilistInsert(Ilist *list, void *data, int element)
{
  char *to;
  if (element < 0 || element > list->size)
    return 1;

  if (ilistAppend(list, data))
    return 1;
  if (element == list->size - 1)
    return 0;
      
  ilistShift(list, element, 1);
  to = (char *)list->data + element * list->dsize;
  memcpy(to, data, list->dsize);
  return 0;
}

/* Shift all items from the given start point to the end by the given amount */
static void ilistShift(Ilist *list, int start, int amount)
{
  int l, lst, lnd, ldir;
  char *to, *from;

  if (amount > 0) {
    lst = list->size - 1 - amount;
    lnd = start;
    ldir = -1;
  } else {
    lst = start;
    lnd = list->size - 1;
    ldir = 1;
  } 

  for (l = lst; ldir * (l - lnd) <= 0; l += ldir) {
    from = (char *)list->data + l * list->dsize;
    to = from + amount * list->dsize;
    memcpy(to, from, list->dsize);
  }
}
