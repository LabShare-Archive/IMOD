/* 
 *  istore.c -- General storage library routines.
 *
 *  Author: David Mastronarde  email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include "imodel.h"
#include "istore.h"

static int storeCompare(const void *s1, const void *s2);

/*!
 * Writes general storage list to the model file with pointer [fout], including
 * the value in [id], which can indicate a model, objext, contour, or mesh
 * list.  Returns non-zero for error.
 */
int imodWriteStore(Ilist *list, int id, FILE *fout)
{
  int i, j, dtype, error;
  Istore *store;
  StoreUnion *item;
  if (!ilistSize(list))
    return 0;
  imodPutInt(fout, &id);
  i = ilistSize(list) * SIZE_STOR;
  imodPutInt(fout, &i);

  for (i = 0; i < ilistSize(list); i++) {
    store = istoreItem(list, i);
    imodPutShort(fout, &store->type);
    imodPutShort(fout, &store->flags);

    /* Set up to write index */
    dtype = store->flags & 3;
    item = &store->index;

    for (j = 0; j < 2; j++) {
      switch (dtype ) {
      case GEN_STORE_INT:
        imodPutInt(fout, &item->i);
        break;
      case GEN_STORE_FLOAT:
        imodPutFloat(fout, &item->f);
        break;
      case GEN_STORE_SHORT:
        imodPutShort(fout, &item->s[0]);
        imodPutShort(fout, &item->s[1]);
        break;
      case GEN_STORE_BYTE:
        if ((error = imodPutBytes(fout, item->b, 4)))
          return(error);
        break;
      }

      /* For second time through, set up to read value */
      dtype = (store->flags >> 2) & 3;
      item = &store->value;
    }
  }
  return 0;
}

/*!
 * Reads a general storage chunk from the model file with pointer [fin].  
 * Allocates a new {Ilist} and returns its address, or NULL if an error
 * occurs.  Set [error] non-zero for error also.
 */
Ilist *imodReadStore(FILE *fin, int *error)
{
  int i, j, dtype, lastIndex, needSort, ind;
  Ilist *list;
  Istore store;
  StoreUnion *item;
  int nread = imodGetInt(fin) / SIZE_STOR;
  needSort = 0;
  *error = 0;
  if (nread <= 0) {
    *error = IMOD_ERROR_READ;
    return(NULL);
  }
  list = ilistNew(sizeof(Istore), nread);
  if (!list) {
    *error = IMOD_ERROR_MEMORY;
    return(NULL);
  }

  for (i = 0; i < nread; i++) {
    store.type = imodGetShort(fin);
    store.flags = imodGetShort(fin);

    /* Set up to read index */
    dtype = store.flags & 3;
    item = &store.index;

    for (j = 0; j < 2; j++) {
      switch (dtype ) {
      case GEN_STORE_INT:
        item->i = imodGetInt(fin);
        break;
      case GEN_STORE_FLOAT:
        item->f = imodGetFloat(fin);
        break;
      case GEN_STORE_SHORT:
        item->s[0] = imodGetShort(fin);
        item->s[1] = imodGetShort(fin);
        break;
      case GEN_STORE_BYTE:
        if (imodGetBytes(fin, item->b, 4)) {
          *error = IMOD_ERROR_READ;
          return(NULL);
        }
        break;
      }

      /* For second time through, set up to read value */
      dtype = (store.flags >> 2) & 3;
      item = &store.value;
    }

    /* Add current store item to list */
    if (ilistAppend(list, &store)) {
      *error = IMOD_ERROR_MEMORY;
      return(NULL);
    }

    /* Keep track of whether it is out of order */
    ind = INT_MAX;
    if (!(store.flags & (GEN_STORE_NOINDEX | 3)))
      ind = store.index.i;
    if (i && ind < lastIndex)
      needSort = 1;
    lastIndex = ind;
  }

  /* Sort if necessary */
  if (needSort)
    istoreSort(list);

  return(list);
}

/*!
 * Sorts the given [list] of {Istore} elements based on their {index} values
 * placing any elements with no index value at the end.
 */
void istoreSort(Ilist *list)
{
  if (ilistSize(list) >= 2)
    qsort(list->data, ilistSize(list), list->dsize, storeCompare);
}

/* Comparison function for sorting the list */
static int storeCompare(const void *s1, const void *s2)
{
  Istore *st1 = (Istore *)s1;
  Istore *st2 = (Istore *)s2;
  int ind1 = INT_MAX;
  int ind2 = INT_MAX;
  if (!(st1->flags & (GEN_STORE_NOINDEX | 3)))
    ind1 = st1->index.i;
  if (!(st2->flags & (GEN_STORE_NOINDEX | 3)))
    ind2 = st2->index.i;
  if (ind1 < ind2)
    return -1;
  if (ind1 > ind2)
    return 1;
  return 0;
}

/*!
 * Inserts the element [store] into the list of sorted {Istore} elements 
 * pointed to by [list].  The element is inserted after any existing elements 
 * with the same {index}, unless GEN_STORE_REVERT is set in {flags}, in which
 * case it is inserted before existing elements.  If GEN_STORE_NOINDEX is set in {flags},
 * it is inserted at the end of the list.  Works if [list] points to a NULL.
 * Returns 1 for error.
 */
int istoreInsert(Ilist **list, Istore *store)
{
  int after, lookup = -1;
  if (store->flags & GEN_STORE_NOINDEX)
    after = ilistSize(*list);
  else
    lookup = istoreLookup(*list, store->index.i, &after);
  if (!*list)
    *list = ilistNew(sizeof(Istore), 4);
  if (!*list)
    return 1;
  if ((store->flags & GEN_STORE_REVERT) && lookup >= 0)
    return (ilistInsert(*list, store, lookup));
  return (ilistInsert(*list, store, after));
}

/*!
 * Looks for elements with [index] in the given [list] of sorted {Istore} 
 * elements, returns [after] with the list index of the first element with an
 * index after the given value or the list size if there is none, and returns 
 * the list index of the first matching element, or -1 if there is no match or
 * if [list] is empty or NULL.
 */
int istoreLookup(Ilist *list, int index, int *after)
{
  int below, above, mid, match = -1;
  int noindex = GEN_STORE_NOINDEX | 3;
  Istore *store;
  *after = 0;
  if (!ilistSize(list))
    return -1;
  below = 0;
  above = ilistSize(list) - 1;

  /* test that first element is below item  - if above, done*/
  store = istoreItem(list, 0);
  if ((store->flags & noindex) || store->index.i > index)
    return -1;
  else if (store->index.i == index)
    match = 0;

  /* test that last element is above item - if below, set after to list
     end and return */
  store = istoreItem(list, above);
  if (match < 0 && !(store->flags & noindex)) {
    if (store->index.i < index) {
      *after = ilistSize(list);
      return -1;

    } else if (store->index.i == index)
      match = above;
  }

  /* Look at element midway between below and above and replace either the
     below or the above element.  Loop until a match is found or there is no 
     middle left */
  while (match < 0 && above - below > 1) {
    mid = (above + below) / 2;
    store = istoreItem(list, mid);
    if ((store->flags & noindex) || store->index.i > index)
      above = mid;
    else if (store->index.i == index)
      match = mid;
    else
      below = mid;
    /* printf("mid %d below %d above %d match %d\n", mid, below, above, 
       match);*/
  }

  /* If there is still no match, then set after to the one above */
  if (match < 0) {
    *after = above;
    return -1;
  }

  /* If there is a match, find first one after the matching index */
  for (mid = match + 1; mid < ilistSize(list); mid++) {
    store = istoreItem(list, mid);
    if ((store->flags & noindex) || store->index.i > index)
      break;
  }
  *after = mid;

  /* Then find first one before the match, and return first match */
  for (mid = match - 1; mid >= 0; mid--) {
    store = istoreItem(list, mid);
    if (store->index.i < index)
      break;
  }
  return (mid + 1);
}

/*!
 * Prints the values of general storage elements in [list].  {index} and
 * {value} items will be printed as int, byte, short, or float based upon
 * the flags in {flags}.
 */
void istoreDump(Ilist *list)
{
  int i, j, dtype;
  Istore *store;
  StoreUnion *item;
  char *types[] = {"COLOR", "FCOLOR", "TRANS", "GAP", "CONNECT", "3DWIDTH",
                   "2DWIDTH", "SYMTYPE", "SYMSIZE", "VALUE1", "MINMAX1", 
                   "VALUE2"};
  printf(" %d items in list:\n", ilistSize(list));
  for (i = 0; i < ilistSize(list); i++) {
    store = istoreItem(list, i);
    printf("%6d-", store->type);
    if (store->type > 0 || store->type <= 12)
      printf("%s", types[store->type - 1]);
    printf("  %6o-", store->flags);
    dtype = 0;
    if (store->flags & GEN_STORE_NOINDEX) {
      printf("NOIND");
      dtype = 1;
    }
    if (store->flags & GEN_STORE_REVERT) {
      printf("%sREVERT", dtype ? "|" : "");
      dtype = 1;
    }
    if (store->flags & GEN_STORE_SURFACE) {
      printf("%sSURF", dtype ? "|" : "");
      dtype = 1;
    }
    if (store->flags & GEN_STORE_ONEPOINT)
      printf("%sONEPT", dtype ? "|" : "");

    dtype = store->flags & 3;
    item = &store->index;

    for (j = 0; j < 2; j++) {
      switch (dtype ) {
      case GEN_STORE_INT:
        printf(" %11d", item->i);
        break;
      case GEN_STORE_FLOAT:
        printf(" %12.6g", item->f);
        break;
      case GEN_STORE_SHORT:
        printf(" %6d %6d", item->s[0], item->s[1]);
        break;
      case GEN_STORE_BYTE:
        printf(" %3d %3d %3d %3d", item->b[0], item->b[1], item->b[2],
               item->b[3]);
        break;
      }

      dtype = (store->flags >> 2) & 3;
      item = &store->value;
    }
    printf("\n");
  }    
}

/*!
 * Computes a checksum from the elements in the list, adding all types, flags,
 * indexes and values.
 */
double istoreChecksum(Ilist *list)
{
  int i, j, dtype;
  Istore *store;
  StoreUnion *item;
  double sum = 0.;

  for (i = 0; i < ilistSize(list); i++) {
    store = istoreItem(list, i);
    dtype = store->flags & 3;
    sum += store->flags + store->type;
    item = &store->index;

    for (j = 0; j < 2; j++) {
      switch (dtype ) {
      case GEN_STORE_INT:
        sum += item->i;
        break;
      case GEN_STORE_FLOAT:
        sum += item->f;
        break;
      case GEN_STORE_SHORT:
        sum += item->s[0] + item->s[1];
        break;
      case GEN_STORE_BYTE:
        sum += item->b[0] + item->b[1] + item->b[2] + item->b[3];
        break;
      }

      dtype = (store->flags >> 2) & 3;
      item = &store->value;
    }
  }    
  return sum;
}

/*!
 * Returns the number of items whose type is [type] in the [list], or just 
 * stops and returns 1 upon finding the first such change if [stop] is nonzero.
 */
int istoreCountItems(Ilist *list, int type, int stop)
{
  int i, count = 0;
  Istore *stp;

  if (!ilistSize(list))
    return 0;
  for (i = 0; i < list->size; i++) {
    stp = istoreItem(list, i);
    if (stp->type == type) {
      count++;
      if (stop)
        return 1;
    }
  }
  return count;
}
 
/*!
 * Returns the number of items whose type is [type] in the storage lists of
 * object [obj].  It always searches {obj->store}, then searches the {store}
 * lists of all contours if [doCont] is nonzero, then the {store} lists of all
 * meshes if [doMesh] is nonzero.  If [stop] is nonzero, it just
 * stops and returns 1 upon finding the first such change.
 */
int istoreCountObjectItems(Iobj *obj, int type, int doCont, int doMesh,
                             int stop)
{
  int co, me, count;
  count = istoreCountItems(obj->store, type, stop);
  if (count && stop)
    return count;
  if (doCont) {
    for (co = 0; co < obj->contsize; co++) {
      count += istoreCountItems(obj->cont[co].store, type, stop);
      if (count && stop)
        return count;
    }
  }
  if (doMesh) {
    for (me = 0; me < obj->meshsize; me++) {
      count += istoreCountItems(obj->mesh[me].store, type, stop);
      if (count && stop)
        return count;
    }
  }
  return count;
}

/*!
 * Counts the number of items in [list] with index value of [index], for
 * either contours, if [surfFlag] is 0, or surfaces, if [surfFlag] is nonzero.
 */    
int istoreCountContSurfItems(Ilist *list, int index, int surfFlag)
{
  Istore *stp;
  int i, count = 0;
  if (!ilistSize(list))
    return 0;
  surfFlag = surfFlag ? GEN_STORE_SURFACE : 0;
  for (i = 0; i < list->size; i++) {
    stp = istoreItem(list, i);
    if (stp->flags & (GEN_STORE_NOINDEX | 3))
      break;
    index = stp->index.i;
    if ((stp->flags & GEN_STORE_SURFACE) == surfFlag && index == stp->index.i)
      count++;
  }
  return count;
}

/*!
 * Returns 1 if an item in [list] with point index equal to [index] is a gap,
 * or 0 if not.
 */
int istorePointIsGap(Ilist *list, int index)
{
  int lookup, after, i;
  Istore *stp;
  lookup = istoreLookup(list, index, &after);
  if (lookup < 0)
    return 0;
  for (i = lookup; i < after; i++) {
    stp = istoreItem(list, i);
    if (stp->type == GEN_STORE_GAP)
      return 1;
  }
  return 0;
}

/*!
 * Returns connection number for an item in [list] with point index equal to
 * [index], or -1 if there is none.
 */
int istoreConnectNumber(Ilist *list, int index)
{
  int lookup, after, i;
  Istore *stp;
  lookup = istoreLookup(list, index, &after);
  if (lookup < 0)
    return -1;
  for (i = lookup; i < after; i++) {
    stp = istoreItem(list, i);
    if (stp->type == GEN_STORE_CONNECT)
      return stp->value.i;
  }
  return -1;
}

/*!
 * Adds an item containing the given [min] and [max], of the given [type]
 * (e.g., GEN_STORE_MINMAX1) to the storage list pointed to by [list], or updates the
 * values in such an item if it already exists. Returns 1 for error.
 */
int istoreAddMinMax(Ilist **list, int type, float min, float max)
{
  Istore *stp;
  Istore store;
  int i;
  store.value.f = max;
  store.index.f = min;
  store.type = GEN_STORE_MINMAX1;
  store.flags = (GEN_STORE_FLOAT << 2) | GEN_STORE_FLOAT | GEN_STORE_NOINDEX;

  /* Look for an existing entry of the same kind and replace its values */
  for (i = ilistSize(*list) - 1 ; i >= 0; i--) {
    stp = istoreItem(*list, i);
    if (!(stp->flags & (GEN_STORE_NOINDEX | 3)))
      break;
    if (stp->type == GEN_STORE_MINMAX1 && stp->flags == store.flags) {
      stp->index.f = min;
      stp->value.f = max;
      return 0;
    }
  }

  /* Or insert the change; it will be added to end */
  return istoreInsert(list, &store);
}

/*!
 * Computes the min and max values for all GEN_STORE_VALUE1 items in object
 * [obj] and its contours, and adds or updates a GEN_STORE_MINMAX1 item with these 
 * values for the object.  Returns -1 if there are no values in the object, or 1 if
 * there is an error inserting the min/max in the object store.
 */
int istoreFindAddMinMax1(Iobj *obj)
{
  Istore *store;
  Ilist *list;
  int i, co;
  float min = 1.e37;
  float max = -1.e37;
  for (co = -1; co < obj->contsize; co++) {
    if (co >= 0)
      list = obj->cont[co].store;
    else
      list = obj->store;
    for (i = 0; i < ilistSize(list); i++) {
      store = istoreItem(list, i);
      if (store->type == GEN_STORE_VALUE1 && !(store->flags & GEN_STORE_REVERT)) {
        min = B3DMIN(min, store->value.f);
        max = B3DMAX(max, store->value.f);
      }
    }
  }
  if (min > max)
    return -1;
  return istoreAddMinMax(&obj->store, GEN_STORE_MINMAX1, min, max);
}

/*!
 * Looks for a min/max value in the given [list] for values of a given [type]
 * (e.g., GEN_STORE_MINMAX1).  The size of the entity containing this storage 
 * list should be provided in [size], although this entry is currently unused (e.g., 
 * {contsize} for an object store).  If a min/max entry is found, values are returned 
 * in [min] and [max] and the return value is 1; otherwise the function returns 0.
 */
int istoreGetMinMax(Ilist *list, int size, int type, float *min, float *max)
{
  int i;
  Istore *stp;
  for (i = ilistSize(list) - 1 ; i >= 0; i--) {
    stp = istoreItem(list, i);
    if (!(stp->flags & (GEN_STORE_NOINDEX | 3)))
      return 0;
    if (stp->type == type && (stp->flags & 3) == 1) {
      *min = stp->index.f;
      *max = stp->value.f;
      return 1;
    }
  }
  return 0;
}

/*!
 * Returns 1 if a point should be retained in a shave or reduce operation
 * because either it has an item, it is after a gap, or it precedes an end.
 */
int istoreRetainPoint(Ilist *list, int index)
{
  int lookup, after, i;
  Istore *stp;

  if (!ilistSize(list))
    return 0;

  /* If there is anything for this index, retain point */
  lookup = istoreLookup(list, index, &after);
  if (lookup >= 0)
    return 1;
  
  /* Look forward for an end on next index */
  for (i = after; i < list->size; i++) {
    stp = istoreItem(list, i);
    if ((stp->flags & (GEN_STORE_NOINDEX | 3)) || stp->index.i > index + 1)
      break;
    if (stp->flags & GEN_STORE_REVERT)
      return 1;
  }

  /* Look backward for a gap on previous index */
  for (i = after - 1; i >= 0; i--) {
    stp = istoreItem(list, i);
    if (stp->index.i < index - 1)
      break;
    if (stp->type == GEN_STORE_GAP)
      return 1;
  }
  return 0;
}

/* 
 * HIGHER LEVEL INSERTION AND DELETION OF CHANGES
 * DOC_SECTION HIGHLEVEL
 */

/*!
 * Inserts a change described in [store] into the list pointed to by [listp].
 * A matching change or end at the same index is removed.  Redundant entries
 * of the change are avoided or eliminated.  Returns 1 for error.
 */
int istoreInsertChange(Ilist **listp, Istore *store)
{
  Istore *stp;
  Ilist *list = *listp;
  int i, lookup, after, needItem;
  lookup = istoreLookup(list, store->index.i, &after);
  
  /* If there is a match at the current index, eliminate it */
  if (lookup >= 0) {
    for (i = lookup; i < after; i++) {
      stp = istoreItem(list, i);
      if (stp->type == store->type) {
        ilistRemove(list, i);
        i--;
        after--;
      }
    }
  }

  /* Look backwards for multiple-point items and see if there is fully matching
     start; if not insert the new item */
  if (lookup < 0)
    lookup = after;
  needItem = 1;
  if (store->flags & GEN_STORE_ONEPOINT)
    lookup = 0;
  for (i = lookup - 1; i >= 0; i--) {
    stp = istoreItem(list, i);
    if (stp->type == store->type) {
       if (!(stp->flags & GEN_STORE_REVERT) && 
           stp->value.i == store->value.i)
         needItem = 0;
       break;
    }
  }

  /* Insert if still needed, adjust after index */
  if (needItem) {
    if (istoreInsert(listp, store))
      return 1;
    after++;
    list = *listp;
  }

  /* Look forward and eliminate any fully matching starts */
  /* 6/17/08: This is a bad idea, it propagates changes through points that
     the user set explicitly to the matching value */
  /* for (i = after; i < list->size; i++) {
    stp = istoreItem(list, i);
    if (stp->flags & (GEN_STORE_NOINDEX | 3))
      break;
    if (stp->type == store->type) {
      if ((stp->flags & GEN_STORE_REVERT) || stp->value.i != store->value.i)
        break;
      ilistRemove(list, i);
      i--;
    }
    } */
  return 0;
}

/*!
 * Inserts an end for a change into [list], where [type] specifies the type and
 * [index] indicates the point index.  A later end will be deleted if there
 * is not an intervening start of the same type, and a start at the given index
 * will also be deleted.  Returns 1 for error.
 */
int istoreEndChange(Ilist *list, int type, int index)
{
  Istore *stp;
  Istore store;
  int i, lookup, after, j, needEnd;
  if (!ilistSize(list))
    return 1;
  lookup = istoreLookup(list, index, &after);

  /* Search forward and eliminate matching end, unless there is another set */
  for (i = after; i < list->size; i++) {
    stp = istoreItem(list, i);
    if (stp->flags & (GEN_STORE_NOINDEX | 3))
      break;
    if (stp->type == type) {
      if (!(stp->flags & GEN_STORE_REVERT))
        break;
      ilistRemove(list, i);
      i--;
    }
  }
  
  /* If there is a change at this index it needs to be removed */
  needEnd = 1;
  if (lookup >= 0) {
    for (i = lookup; i < after; i++) {
      stp = istoreItem(list, i);
      if (stp->type == type) {

        /* If end already exists, don't need end */
        needEnd = 0;
        if (!(stp->flags & GEN_STORE_REVERT)) {

          /* Search back for a previous start; if find one, still need end */
          needEnd = 0;
          for (j = lookup - 1; j >= 0; j--) {
            stp = istoreItem(list, j);
            if (stp->type == type) {
              if (!(stp->flags & GEN_STORE_REVERT))
                needEnd = 1;
              break;
            }
          }

          /* Remove start at this index */
          ilistRemove(list, i);
          i--;
          after--;
        }
      }      
    }
  }

  /* Insert the end if still needed */
  if (needEnd) {
    store.type = type;
    store.index.i = index;
    store.flags = GEN_STORE_REVERT;
    store.value.i = 0;
    if (istoreInsert(&list, &store))
      return 1;
  }
  return 0;
}

/*!
 * Clears a whole change sequence in [list] with the type given by [type] and 
 * containing the point at [index].  All changes of the given type are removed
 * from the starting change to an end, if any.  Returns 1 for an empty list.
 * This must be called only if the given point is contained in a change.
 */
int istoreClearChange(Ilist *list, int type, int index)
{
  Istore *stp;
  int i, lookup, after, flags;
  if (!ilistSize(list))
    return 1;
  lookup = istoreLookup(list, index - 1, &after);
  
  /* Search forward to end, removing all matching changes and end */
  for (i = after; i < list->size; i++) {
    stp = istoreItem(list, i);
    if (stp->flags & (GEN_STORE_NOINDEX | 3))
      break;
    if (stp->type == type) {
      flags = stp->flags;
      ilistRemove(list, i);
      i--;
      if (flags & GEN_STORE_REVERT)
        break;
    }
  }

  /* Search backward, deleting all matching changes unless an end is found */
  for (i = after - 1; i >= 0; i--) {
    stp = istoreItem(list, i);
    if (stp->type == type) {
      if (stp->flags & GEN_STORE_REVERT)
        break;
      ilistRemove(list, i);
    }
  }
  return 0;
}

/*!
 * Clears a change sequence of the given [type] from the [list] for all points
 * from [start] to [end], removing starts before the range and an end after the
 * range if necessary.  This may be called even if there is no change.
 */
void istoreClearRange(Ilist *list, int type, int start, int end)
{
  Istore *stp;
  int i, lookup, after, flags, index, ended, hasChange = 0;
  if (!ilistSize(list))
    return;
  lookup = istoreLookup(list, start, &after);

  /* Search forward from after the start for an end or a start */
  for (i = after; i < list->size; i++) {
    stp = istoreItem(list, i);
    if ((stp->flags & (GEN_STORE_NOINDEX | 3)) || stp->index.i > end)
      break;
    if (stp->type == type) {
      hasChange = 1;
      break;
    }
  }

  /* If none found within range, search back for a start before an end */
  if (!hasChange) {
    for (i = after - 1; i >= 0; i--) {
      stp = istoreItem(list, i);
      if (stp->type == type) {
        if (!(stp->flags & GEN_STORE_REVERT))
          hasChange = 1;
        break;
      }
    }
  }
  
  if (!hasChange)
    return;

  /* Search forward to end, removing all matching changes until an end past
     the end index is found*/
  ended = 0;
  for (i = after; i < list->size; i++) {
    stp = istoreItem(list, i);
    if (stp->flags & (GEN_STORE_NOINDEX | 3))
      break;
    if (stp->type == type) {
      flags = stp->flags;
      index = stp->index.i;
      if (ended && index > end)
        break;
      ilistRemove(list, i);
      i--;
      ended = flags & GEN_STORE_REVERT;
      if (ended && index >= end)
        break;
    }
  }

  /* Search backward, deleting all matching changes unless an end is found */
  for (i = after - 1; i >= 0; i--) {
    stp = istoreItem(list, i);
    if (stp->type == type) {
      if (stp->flags & GEN_STORE_REVERT)
        break;
      ilistRemove(list, i);
    }
  }
}

/*!
 * Inserts the item in [store] into the list pointed to by [listp], where the
 * item specifies a property that applies to only one point or contour.
 * A matching item at the same index is replaced.  Returns 1 for error.
 */
int istoreAddOneIndexItem(Ilist **listp, Istore *store)
{
  int i, lookup, after;
  Istore *stp;

  if (*listp) {
    lookup = istoreLookup(*listp, store->index.i, &after);

    /* Look for a matching item and replace it */
    if (lookup >= 0) {
      for (i = lookup; i < after; i++) {
        stp = istoreItem(*listp, i);
        if (stp->type == store->type && (stp->flags & GEN_STORE_SURFACE) == 
            (store->flags & GEN_STORE_SURFACE)) {
          *stp = *store;
          return 0;
        }
      }
    }
  }

  /* If no match, insert the item */
  return (istoreInsert(listp, store));
}

/*!
 * Removes the item in [list] with the type given by [type] and containing the
 * point at [index], where the item specifies a property that applies to only
 * one point or contour.  [surfFlag] should be 0 for a point or contour, or 
 * non-zero for a surface.  Returns 1 for an empty list, -1 if no
 * matching item is found.
 */
int istoreClearOneIndexItem(Ilist *list, int type, int index, int surfFlag)
{
  int i, lookup, after;
  Istore *stp;
  if (!ilistSize(list))
    return 1;
  surfFlag = surfFlag ? GEN_STORE_SURFACE : 0;
  lookup = istoreLookup(list, index, &after);
  if (lookup < 0)
    return -1;
  for (i = lookup; i < after; i++) {
    stp = istoreItem(list, i);
    if (stp->type == type && (stp->flags & GEN_STORE_SURFACE) == surfFlag) {
      ilistRemove(list, i);
      return 0;
    }
  }
  return -1;
}

/*!
 * Adds {Istore} items to the list pointed to by [listp] for properties in 
 * [props] specified by [genFlags] that are not in the default state, as 
 * indicated by [stateFlags].
 * Each item's index will be [index].  Only handles 3D items: color, fill 
 * color, transparency, 3D width, and general value.  Returns 1 for error.
 */
int istoreGenerateItems(Ilist **listp, DrawProps *props, int stateFlags, 
                        int index, int genFlags)
{
  Istore store;
  store.index.i = index;
  if (genFlags & stateFlags & CHANGED_COLOR) {
    store.type = GEN_STORE_COLOR;
    store.flags = (GEN_STORE_BYTE << 2);
    store.value.b[0] = (int)(255. * props->red);
    store.value.b[1] = (int)(255. * props->green);
    store.value.b[2] = (int)(255. * props->blue);
    if (istoreInsert(listp, &store))
      return 1;
  }
    
  if (genFlags & stateFlags & CHANGED_FCOLOR) {
    store.type = GEN_STORE_FCOLOR;
    store.flags = (GEN_STORE_BYTE << 2);
    store.value.b[0] = (int)(255. * props->fillRed);
    store.value.b[1] = (int)(255. * props->fillGreen);
    store.value.b[2] = (int)(255. * props->fillBlue);
    if (istoreInsert(listp, &store))
      return 1;
  }
    
  if (genFlags & stateFlags & CHANGED_TRANS) {
    store.type = GEN_STORE_TRANS;
    store.flags = 0;
    store.value.i = props->trans;
    if (istoreInsert(listp, &store))
      return 1;
  }
    
  if (genFlags & stateFlags & CHANGED_3DWIDTH) {
    store.type = GEN_STORE_3DWIDTH;
    store.flags = 0;
    store.value.i = props->linewidth;
    if (istoreInsert(listp, &store))
      return 1;
  }

  if (genFlags & stateFlags & CHANGED_VALUE1) {
    store.type = GEN_STORE_VALUE1;
    store.flags = (GEN_STORE_FLOAT << 2);
    store.value.f = props->value1;
    if (istoreInsert(listp, &store))
      return 1;
  }
  return 0;
}

/*!
 * Adds {Istore} items to the mesh storage list pointed to by [mlistp] for a
 * single point with index [ptInd].  Its properties are determined from the 
 * contour storage list in [clist], the contour properties in [contProps], and
 * the state flags for the contour in [contState].  Items are added for
 * non-default properties specified by [genFlags].
 * Each item's index will be [meshInd].  Only handles 3D items: color, fill 
 * color, transparency, 3D width. and general value.  Returns 1 for error.
 */
int istoreGenPointItems(Ilist *clist, DrawProps *contProps, int contState, 
                        int ptInd, Ilist **mlistp, int meshInd, int genFlags)
{
  DrawProps ptProps;
  int stateFlags = istoreListPointProps(clist, contProps, &ptProps, ptInd);
  return (istoreGenerateItems(mlistp, &ptProps, stateFlags | contState,
                              meshInd, genFlags));
}

/* 
 * CONTOUR EDITING FUNCTIONS
 * DOC_SECTION EDITING
 */

/*!
 * Breaks all changes in [list] at the point index given by [index]; namely 
 * terminates all changes at index - 1 and restarts them at index.  [psize]
 * specifies the number of points in the contour.  If [index] equals [psize]
 * then terminations will be inserted without any restarts.  Returns 1 for 
 * error.
 */
int istoreBreakChanges(Ilist *list, int index, int psize)
{
  Istore store, storeEnd;
  Istore *stp;
  int curStart, curRevert, ptLastSet, ptFirstSet, ptRevert, curSet, i;
  int needRestart;
  if (!ilistSize(list))
    return 0;
  
  for (curStart = 0; curStart < list->size; curStart++) {
    stp = istoreItem(list, curStart);

    /* Done when reach the given index, or a non-index item */
    if ((stp->flags & (GEN_STORE_NOINDEX | 3)) || stp->index.i >= index)
      break;

    /* Skip if it is a one-point type or an end */
    if (stp->flags & (GEN_STORE_ONEPOINT | GEN_STORE_REVERT))
      continue;

    /* Save the item, set up index for last time it was set, and set the
       endpoint past the break index.  Keep track of point and list indexes */
    store = *stp;
    ptFirstSet = stp->index.i;
    ptLastSet = ptFirstSet;
    ptRevert = index + 1;
    curSet = curStart;
    needRestart = index < psize ? 1 : 0;

    /* Loop forward from this point looking for an end */
    for (i = curStart + 1; i < list->size; i++) {
      stp = istoreItem(list, i);

      /* This search is done when get past the given index */
      if ((stp->flags & (GEN_STORE_NOINDEX | 3)) || stp->index.i > index)
        break;

      /* If the type matches, stop on an end */
      if (store.type == stp->type) {
        if (stp->flags & GEN_STORE_REVERT) {
          ptRevert = stp->index.i;
          curRevert = i;
          break;
        }

        /* or replace the saved item, unless it is restart after break */
        if (stp->index.i == index) {
          needRestart = 0;
        } else {
          store = *stp;
          ptLastSet = stp->index.i;
          curSet = i;
        }
      }
    }
    /* printf("%d %d %d %d %d %d %d\n", index, curStart, curSet, curRevert, 
           ptFirstSet, ptLastSet, ptRevert); */

    /* Nothing to do if it ends before or at the break */
    if (ptRevert <= index)
      continue;

    /* Otherwise insert an end */
    storeEnd = store;
    storeEnd.flags |= GEN_STORE_REVERT;
    storeEnd.index.i = index;
    if (istoreInsert(&list, &storeEnd))
      return 1;
    
    /* Now restart if needed */
    if (needRestart) {
      store.index.i = index;
      if (istoreInsert(&list, &store))
        return 1;
    }
  }
  return 0;
}

/*!
 * Returns the index in [list] of the first new change starting at a point 
 * index greater than or equal to [index].  Ends at that index are assumed
 * to occur before changes.
 */
int istoreFindBreak(Ilist *list, int index)
{
  int lookup, after, i;
  Istore *stp;
  after = 0;
  lookup = istoreLookup(list, index, &after);
  if (lookup < 0)
    return after;
  for (i = lookup; i < after; i++) {
    stp = istoreItem(list, i);
    if (!(stp->flags & GEN_STORE_REVERT))
      return i;
  }
  return after;
}

/*!
 * Shifts indexes in [list] by [amount], for all storage items with indexes
 * >= [ptIndex].  Set [startScan] to a list index at which to start scanning
 * the list, or to -1 to have the routine search for the starting index.
 * Items with the GEN_STORE_SURFACE flag set will not be shifted, so this 
 * routine will work for contour or point indexes.
 */
void istoreShiftIndex(Ilist *list, int ptIndex, int startScan, int amount)
{
  int after, i;
  Istore* stp;
  if (!ilistSize(list))
    return;

  /* Getting starting position if it is not provided */
  if (startScan < 0) {
    startScan = istoreLookup(list, ptIndex, &after);
    if (startScan < 0)
      startScan = after;
  }

  /* From start to end or non-index item, shift all indexes */
  for (i = startScan; i < list->size; i++) {
    stp = istoreItem(list, i);
    if (stp->flags & (GEN_STORE_NOINDEX | 3))
      break;
    if (stp->flags & GEN_STORE_SURFACE)
      continue;
    if (stp->index.i >= ptIndex)
      stp->index.i += amount;
  }
  istoreSort(list);
}

/*!
 * Manages any elements in [list] associated with the point given by [index]
 * when that point is deleted.  They will be moved to the next point or
 * removed, and all following indexes will be reduced by 1.  [psize] must 
 * indicate the size of the contour before the point is deleted.  Returns 1
 * for error.
 */
int istoreDeletePoint(Ilist *list, int index, int psize)
{
  int after, lind, i, j, needMove, delEnd, lookup;
  Istore store;
  Istore *stp;
  if (!ilistSize(list))
    return 0;
    
  lookup = istoreLookup(list, index, &after);
  if (lookup < 0) {
    istoreShiftIndex(list, index + 1, after, -1);
    return 0;
  }

  /* If there are following points, loop on the items, see if each
     item needs to be moved to a following point */
  if (index < psize - 1) {
    for (lind = lookup; lind < after; lind++) {
      stp = istoreItem(list, lind);

      /* Skip single-point items, they just delete */
      if (stp->flags & GEN_STORE_ONEPOINT)
        continue;
      store = *stp;
      needMove = 1;

      /* Look forward for items of same type */
      for (i = after; i < list->size; i++) {
        stp = istoreItem(list, i);

        /* If no items found, done, and still need to move */
        if ((stp->flags & (GEN_STORE_NOINDEX | 3)) || 
            stp->index.i > index + 1)
          break;

        /* If matching item is found, no move needed */
        if (stp->type == store.type) {
          needMove = 0;
          
          /* If it is another start, that is fine */
          if (!(stp->flags & GEN_STORE_REVERT))
            break;

          /* If it is an end, need to look back and see if the point in
          question is a successor; if not this end needs to be removed */
          delEnd = 1;
          for (j = lookup - 1; j >= 0; j--) {
            stp = istoreItem(list, j);
            if (stp->type == store.type) {
              if (!(stp->flags & GEN_STORE_REVERT))
                delEnd = 0;
              break;
            }
          }

          if (delEnd)
            ilistRemove(list, i);
          break;
        }
      }

      /* Insert item at next index if still appropriate */
      if (needMove) {
        store.index.i++;
        if (istoreInsert(&list, &store))
          return 1;
      }
    }
  }

  /* Delete the current point items and then shift indexes */
  ilistShift(list, after, lookup - after);
  list->size -= after - lookup;
  istoreShiftIndex(list, index + 1, lookup, -1);
  return 0;
}

/*!
 * Removes any elements in [list] associated with the contour or surface whose 
 * index is [index], depending on whether [surfFlag] is 0 or not.  For 
 * contours, it reduces all following contour indexes by 1.
 */
void istoreDeleteContSurf(Ilist *list, int index, int surfFlag)
{
  int after, i, lookup;
  Istore *stp;
  if (!ilistSize(list))
    return;
    
  surfFlag = surfFlag ? GEN_STORE_SURFACE : 0;
  lookup = istoreLookup(list, index, &after);
  if (lookup >= 0) {
    for (i = lookup; i < after; i++) {
      stp = istoreItem(list, i);
      if ((stp->flags & GEN_STORE_SURFACE) == surfFlag) {
        ilistRemove(list, i);
        i--;
        after--;
      }
    }
  }
  if (!surfFlag)
    istoreShiftIndex(list, index + 1, after, -1);
}

/*!
 * Breaks general storage list in contour [cont] into two pieces and assigns
 * changes occurring from point index [p1] through [p2] to the new contour 
 * [ncont].  Returns 1 for error.
 */
int istoreBreakContour(Icont *cont, Icont *ncont, int p1, int p2) 
{
  Ilist *ostore = cont->store;
  Ilist *nstore = NULL;
  Ilist *tmpList1 = NULL;

  if (!ilistSize(ostore))
    return 0;

  if (p2 < 0)
    p2 = cont->psize - 1;
  if (!cont || !ncont || !cont->psize || p1 < 0 || p1 >= cont->psize ||
      p2 >= cont->psize || p2 < p1)
    return 1;

  if (istoreExtractChanges(ostore, &tmpList1, 0, p1 - 1, 0, cont->psize))
    return 1;

  if (istoreExtractChanges(ostore, &nstore, p1, p2, 0, cont->psize))
    return 1;

  ilistDelete(ncont->store);
  ncont->store = nstore;

  if (istoreExtractChanges(ostore, &tmpList1, p2 + 1, cont->psize - 1, p1,
                           cont->psize))
    return 1;
  ilistDelete(cont->store);
  cont->store = tmpList1;
  return 0;
}

/*!
 * Inverts the changes in the list of {Istore} elements pointed to by [listp]
 * so that the same changes will occur with an inverted contour.  [psize]
 * specifies the size of the corresponding contour.  Returns 1 for error.
 */
int istoreInvert(Ilist **listp, int psize)
{
  Ilist *nlist = NULL;
  Ilist *list = *listp;
  int curStart, i;
  Istore *stp;
  Istore store;

  if (psize < 2 || !ilistSize(list))
    return 0;
  if (istoreBreakChanges(list, psize, psize))
    return 1;

  for (curStart = 0; curStart < list->size; curStart++) {
    stp = istoreItem(list, curStart);

    /* Copy non-index item */
    if ((stp->flags & (GEN_STORE_NOINDEX | 3))) {
      if (istoreInsert(&nlist, stp)) {
        ilistDelete(nlist);
        return 1;
      }
      continue;
    }

    /* Copy one-point type but move gap back by one */
    if (stp->flags & GEN_STORE_ONEPOINT) {
      stp->index.i = psize - 1 - stp->index.i;
      if (stp->type == GEN_STORE_GAP) {
        stp->index.i--;
        if (stp->index.i < 0)
          stp->index.i = psize - 1;
      }
      if (istoreInsert(&nlist, stp)) {
        ilistDelete(nlist);
        return 1;
      }
      continue;
    }

    /* Convert the start to an end and add it with inverted index, then
       save the starting change.  Advance index by one for the new end */
    store = *stp;
    store.index.i = psize - store.index.i;
    store.flags |= GEN_STORE_REVERT;
    if (istoreInsert(&nlist, &store)) {
      ilistDelete(nlist);
      return 1;
    }
    store = *stp;

    /* Loop forward from this point looking for the end */
    for (i = curStart + 1; i < list->size; i++) {
      stp = istoreItem(list, i);

      /* If the type matches, output saved item with current index inverted
         advanced by one for the new start */
      if (store.type == stp->type) {
        store.index.i = psize - stp->index.i;
        if (istoreInsert(&nlist, &store)) {
          ilistDelete(nlist);
          return 1;
        }

        /* Remove the successor or end, break on an end */
        store = *stp;
        ilistRemove(list, i);
        i--;
        if (store.flags & GEN_STORE_REVERT)
          break;
      }
    }
  }

  /* Free the remnants of the list and assign new list */
  ilistDelete(list);
  *listp = nlist;
  return 0;
}

/*!
 * Removes extraneous ends from [list] at indexes that have a matching start.
 */
void istoreCleanEnds(Ilist *list)
{
  Istore *stp, *stp2;
  int i, j;
  for (i = 0; i < ilistSize(list); i++) {
    stp = istoreItem(list, i);
    if (stp->flags & (GEN_STORE_NOINDEX | 3))
      return;
    if (stp->flags & GEN_STORE_REVERT) {
      for (j = i + 1; j < ilistSize(list); j++) {
        stp2 = istoreItem(list, j);
        if ((stp->flags & (GEN_STORE_NOINDEX | 3)) || 
            stp2->index.i != stp->index.i)
          break;
        if (stp2->type == stp->type) {
          ilistRemove(list, i);
          i--;
          break;
        }
      } 
    }
  }
}

/*!
 * Extracts all of the changes in [olist] between indexes [indStart] and
 * [indEnd], inclusive, into the list pointed to by [nlistp].  The indexes
 * of the changes are shifted so that [indStart] is shifted to [newStart].
 * [psize] specifies the size of the corresponding contour.  [nlistp] may point
 * to a NULL.  Returns 1 for error.
 */
int istoreExtractChanges(Ilist *olist, Ilist **nlistp, int indStart, 
                         int indEnd, int newStart, int psize)
{
  Ilist *tmpList;
  int i, after1, after2;
  Istore *stp;

  if (!ilistSize(olist) || indStart > indEnd)
    return 0;

  /* Copy the list */
  tmpList = ilistDup(olist);
  if (!tmpList)
    return 1;
 
  /* If indices are inverted, then invert the list and invert indices */
  if (indStart > indEnd) {
    if (istoreInvert(&tmpList, psize)) {
      ilistDelete(tmpList);
      return 1;
    }
    indStart = psize - 1 - indStart;
    indEnd = psize - 1 - indEnd;
  }

  /* Break the list before and after the indexes */
  if (indStart && istoreBreakChanges(tmpList, indStart, psize)) {
    ilistDelete(tmpList);
    return 1;
  }
  if (istoreBreakChanges(tmpList, indEnd + 1, psize)) {
    ilistDelete(tmpList);
    return 1;
  }

/* Copy data to new list, shifting indexes */
  after1 = istoreFindBreak(tmpList, indStart);
  after2 = istoreFindBreak(tmpList, indEnd + 1);
  for (i = after1; i < after2; i++) {
    stp = istoreItem(tmpList, i);
    stp->index.i += newStart - indStart;
    if (istoreInsert(nlistp, stp)) {
      ilistDelete(tmpList);
      return 1;
    }
  }
  ilistDelete(tmpList);
  return 0;
}

/*!
 * Copies all non-index items in [olist] to the list pointed to by [nlistp].
 * Returns 1 for error.
 */
int istoreCopyNonIndex(Ilist *olist, Ilist **nlistp)
{
  int i, lookup, after;
  if (!ilistSize(olist))
    return 0;
  lookup = istoreLookup(olist, INT_MAX, &after);
  for (i = after; i < olist->size; i++)
    if (istoreInsert(nlistp, istoreItem(olist, i)))
      return 1;
  return 0;
}

/*!
 * Copies items having index [indFrom] from general store list [olist] to the
 * new list pointed to by [nlist], changing the index to [indTo] in the new
 * list.  Contour items are copied if [surfFlag] is 0, and surface items are
 * copied if [surfFlag] is nonzero.  Returns 1 for error.
 */
int istoreCopyContSurfItems(Ilist *olist, Ilist **nlistp, int indFrom,
                            int indTo, int surfFlag)
{
  int i, lookup, after;
  Istore *stp;
  Istore store;
  surfFlag = surfFlag ? GEN_STORE_SURFACE : 0;
  lookup = istoreLookup(olist, indFrom, &after);
  if (lookup >= 0) {
    for (i = lookup; i < after; i++) {
      stp = istoreItem(olist, i);
      if ((stp->flags & GEN_STORE_SURFACE) == surfFlag) {
        store = *stp;
        store.index.i = indTo;
        if (istoreInsert(nlistp, &store))
          return 1;
      }
    }
  }
  return 0;
}

/*
 * This didn't get used... 
 */    
Istore *istoreNextObjItem(Ilist *list, int co, int surf, int first)
{
  Istore *stp;
  int index;
  if (!ilistSize(list))
    return NULL;
  if (first)
    stp = (Istore *)ilistFirst(list);
  else
    stp = (Istore *)ilistNext(list);
  while (stp) {
    if (!stp || (stp->flags & (GEN_STORE_NOINDEX | 3)))
      return NULL;
    index = stp->index.i;
    if ((!(stp->flags & GEN_STORE_SURFACE) && index == co) ||
        ((stp->flags & GEN_STORE_SURFACE) && index == surf))
      return stp;
    stp = (Istore *)ilistNext(list);
  }
  return NULL;
}

/* 
 * FUNCTIONS RELATED TO DRAWING
 * DOC_SECTION DRAWING
 */

/*!
* Fills a draw property structure [props] with default values for the object
* [obj].
*/
void istoreDefaultDrawProps(Iobj *obj, DrawProps *props)
{
  props->red = obj->red;
  props->green = obj->green;
  props->blue = obj->blue;
  props->fillRed = obj->fillred / 255.f;
  props->fillGreen = obj->fillgreen / 255.f;
  props->fillBlue = obj->fillblue / 255.f;
  props->trans = obj->trans;
  props->connect = 0;
  props->gap = 0;
  props->linewidth = obj->linewidth;
  props->linewidth2 = obj->linewidth2;
  props->symtype = obj->symbol;
  props->symflags = obj->symflags;
  props->symsize = obj->symsize;
  props->value1 = 0.;
}

/*!
* Gets a draw property structure [contProps] for a contour or surface based on
* the default object properties in [defProps] and entries in [list].  For a
* contour, [co] specifies the contour number and [surf] specifies its surface
* number; for a surface, [co] should be negative.  Returns a set of flags for 
* which items are changed from the default; separate flags for nondefault items
* are returned in [contState] and [surfState] for contour and surface,
* respectively.
*/
int istoreContSurfDrawProps(Ilist *list, DrawProps *defProps, 
                            DrawProps *contProps, int co, int surf, 
                            int *contState, int *surfState)
{
  int i, j, lookup, after, state, which, surfFlag;
  Istore *stp;
  *contProps = *defProps;
  *contState = 0;
  *surfState = 0;
  if (!ilistSize(list))
    return 0;

  /* Set up to loop on surface entries first */
  which = surf;
  surfFlag = GEN_STORE_SURFACE;

  for (j = 0; j < 2; j++) {
    state = 0;
    if (which < 0)
      continue;
    lookup = istoreLookup(list, which, &after);
    if (lookup >= 0) {
      for (i = lookup; i < after; i++) {
        stp = istoreItem(list, i);
        if ((stp->flags & GEN_STORE_SURFACE) != surfFlag)
          continue;
        switch (stp->type) {
        case GEN_STORE_COLOR:
          state |= CHANGED_COLOR;
          contProps->red = stp->value.b[0] / 255.f;
          contProps->green = stp->value.b[1] / 255.f;
          contProps->blue = stp->value.b[2] / 255.f;
          break;

        case GEN_STORE_FCOLOR:
          state |= CHANGED_FCOLOR;
          contProps->fillRed = stp->value.b[0] / 255.f;
          contProps->fillGreen = stp->value.b[1] / 255.f;
          contProps->fillBlue = stp->value.b[2] / 255.f;
          break;

        case GEN_STORE_TRANS:
          state |= CHANGED_TRANS;
          contProps->trans = stp->value.i;
          break;

        case GEN_STORE_GAP:
          state |= CHANGED_GAP;
          contProps->gap = 1;
          break;

        case GEN_STORE_CONNECT:
          state |= CHANGED_CONNECT;
          contProps->connect = stp->value.i;
          break;

        case GEN_STORE_3DWIDTH:
          state |= CHANGED_3DWIDTH;
          contProps->linewidth = stp->value.i;
          break;

        case GEN_STORE_2DWIDTH:
          state |= CHANGED_2DWIDTH;
          contProps->linewidth2 = stp->value.i;
          break;

        case GEN_STORE_SYMSIZE:
          state |= CHANGED_SYMSIZE;
          contProps->symsize = stp->value.i;
          break;

        case GEN_STORE_SYMTYPE:
          state |= CHANGED_SYMTYPE;
          contProps->symflags &= ~IOBJ_SYMF_FILL;
          contProps->symtype = stp->value.i;
          if (contProps->symtype < 0) {
            contProps->symtype = -1 - contProps->symtype;
            contProps->symflags |= IOBJ_SYMF_FILL;
          }
          break;

        case GEN_STORE_VALUE1:
          state |= CHANGED_VALUE1;
          contProps->value1 = stp->value.f;
          break;
        }
      }
    }
    if (j)
      *contState = state;
    else
      *surfState = state;

    /* Next pass through, loop on contour entries */
    which = co;
    surfFlag = 0;
  }
  return (*contState | *surfState);
}

/*!
* Returns the point index of the first item with a change in the [list], or
* -1 if there are no changes.  Leaves the current index of [list] at this
* item.
*/
int istoreFirstChangeIndex(Ilist *list)
{
  Istore *stp;
  if (!ilistSize(list))
    return -1;
  stp = istoreItem(list, 0);
  if (stp->flags & (GEN_STORE_NOINDEX | 3))
    return -1;
  return stp->index.i;
}

/*!
* Gets the next change in point drawing properties described by [list].   The
* default drawing properties for the contour are supplied in [defProps], and
* the point's drawing properties are returned in [ptProps].  The current
* state of each type of change is returned in [stateFlags], which should be
* zeroed by the caller at the start of a contour and maintained between calls.
* Flags for which properties changed at this point are returned in 
* [changeFlags], while the return value is the point index of the next change 
* in the list, or -1 if there are no more changes.
*/
int istoreNextChange(Ilist *list, DrawProps *defProps,
                     DrawProps *ptProps, int *stateFlags, int *changeFlags)
{
  Istore *stp;
  int ending;
  int index = -1;
  *changeFlags = 0;
  
  /* Overkill? These are being marked 3 ways when they change */
  *stateFlags &= ~(CHANGED_GAP | CHANGED_CONNECT);
  ptProps->gap = 0;
  ptProps->connect = 0;
  if (!ilistSize(list))
    return -1;

  while(1) {

    /* If at end of list or item is past index items, return -1 */
    if (list->current >= list->size)
      return -1;
    stp = istoreItem(list, list->current);
    if (stp->flags & (GEN_STORE_NOINDEX | 3))
      return -1;

    /* Record index if not set yet, return index if it has changed */
    if (index < 0)
      index = stp->index.i;
    else if (stp->index.i != index)
      return stp->index.i;

    /* Increment list index for next round */
    list->current++;
    ending = stp->flags & GEN_STORE_REVERT;
    switch (stp->type) {
    case GEN_STORE_COLOR:
      *changeFlags |= CHANGED_COLOR;
      if (ending) {
        ptProps->red = defProps->red;
        ptProps->green = defProps->green;
        ptProps->blue = defProps->blue;
        *stateFlags &= ~CHANGED_COLOR;
      } else {
        ptProps->red = stp->value.b[0] / 255.f;
        ptProps->green = stp->value.b[1] / 255.f;
        ptProps->blue = stp->value.b[2] / 255.f;
        *stateFlags |= CHANGED_COLOR;
      }
      break;

    case GEN_STORE_FCOLOR:
      *changeFlags |= CHANGED_FCOLOR;
      if (ending) {
        ptProps->fillRed = defProps->fillRed;
        ptProps->fillGreen = defProps->fillGreen;
        ptProps->fillBlue = defProps->fillBlue;
        *stateFlags &= ~CHANGED_FCOLOR;
      } else {
        ptProps->fillRed = stp->value.b[0] / 255.f;
        ptProps->fillGreen = stp->value.b[1] / 255.f;
        ptProps->fillBlue = stp->value.b[2] / 255.f;
        *stateFlags |= CHANGED_FCOLOR;
      }
      break;

    case GEN_STORE_TRANS:
      *changeFlags |= CHANGED_TRANS;
      if (ending) {
        ptProps->trans = defProps->trans;
        *stateFlags &= ~CHANGED_TRANS;
      } else {
        ptProps->trans = stp->value.i;
        *stateFlags |= CHANGED_TRANS;
      }
      break;

    case GEN_STORE_GAP:
      *changeFlags |= CHANGED_GAP;
      *stateFlags |= CHANGED_GAP;
      ptProps->gap = 1;
      break;

    case GEN_STORE_CONNECT:
      *changeFlags |= CHANGED_CONNECT;
      *stateFlags |= CHANGED_CONNECT;
      ptProps->connect = stp->value.i;
      break;

    case GEN_STORE_3DWIDTH:
      *changeFlags |= CHANGED_3DWIDTH;
      if (ending) {
        *stateFlags &= ~CHANGED_3DWIDTH;
        ptProps->linewidth = defProps->linewidth;
      } else {
        *stateFlags |= CHANGED_3DWIDTH;
        ptProps->linewidth = stp->value.i;
      }
      break;

    case GEN_STORE_2DWIDTH:
      *changeFlags |= CHANGED_2DWIDTH;
      if (ending) {
        *stateFlags &= ~CHANGED_2DWIDTH;
        ptProps->linewidth2 = defProps->linewidth2;
      } else {
        *stateFlags |= CHANGED_2DWIDTH;
        ptProps->linewidth2 = stp->value.i;
      }
      break;

    case GEN_STORE_SYMSIZE:
      *changeFlags |= CHANGED_SYMSIZE;
      if (ending) {
        *stateFlags &= ~CHANGED_SYMSIZE;
        ptProps->symsize = defProps->symsize;
      } else {
        *stateFlags |= CHANGED_SYMSIZE;
        ptProps->symsize = stp->value.i;
      }
      break;

    case GEN_STORE_SYMTYPE:
      *changeFlags |= CHANGED_SYMTYPE;
      if (ending) {
        *stateFlags &= ~CHANGED_SYMTYPE;
        ptProps->symflags = defProps->symflags;
        ptProps->symtype = defProps->symtype;
      } else {
        *stateFlags |= CHANGED_SYMTYPE;
        ptProps->symflags &= ~IOBJ_SYMF_FILL;
        ptProps->symtype = stp->value.i;
        if (ptProps->symtype < 0) {
          ptProps->symtype = -1 - ptProps->symtype;
          ptProps->symflags |= IOBJ_SYMF_FILL;
        }
      }
      break;

    case GEN_STORE_VALUE1:
      *changeFlags |= CHANGED_VALUE1;
      if (ending) {
        *stateFlags &= ~CHANGED_VALUE1;
        ptProps->value1 = defProps->value1;
      } else {
        *stateFlags |= CHANGED_VALUE1;
        ptProps->value1 = stp->value.f;
      }
      break;

    }
  }
  return -1;
}

/*!
* Determines the drawing properties for point [pt] in contour [co] of object 
* [obj].  Returns the default contour drawing properties in [contProps], the
* point drawing properties in [ptProps], and the return value is the flags
* for the state of various properties (changed versus default for contour).
*/
int istorePointDrawProps(Iobj *obj, DrawProps *contProps, DrawProps *ptProps,
                         int co, int pt)
{
  int contState, surfState;
  DrawProps defProps;

  /* Get the properties for the contour */
  istoreDefaultDrawProps(obj, &defProps);
  istoreContSurfDrawProps(obj->store, &defProps, contProps, co, 
                         obj->cont[co].surf, &contState, &surfState);
  return (istoreListPointProps(obj->cont[co].store, contProps, ptProps, pt));
}

/*!
* Determines the drawing properties for point [pt] from the changes in [list],
* a list of changes for a contour.  The default contour drawing properties are 
* supplied in [contProps], the point drawing properties are returned in 
* [ptProps], and the return value is the flags
* for the state of various properties (changed versus default).
*/
int istoreListPointProps(Ilist *list, DrawProps *contProps, DrawProps *ptProps,
                         int pt)
{
  int stateFlags = 0;
  int changeFlags, nextChange, lastChange = -1;
  *ptProps = *contProps;

  /* Go through the changes until the point index is passed */
  nextChange = istoreFirstChangeIndex(list);
  /* printf("pt %d firstChange %d  list size %d\n",pt, nextChange, 
     ilistSize(list)); */
  while (nextChange >= 0 && nextChange <= pt) {
    lastChange = nextChange;
    nextChange = istoreNextChange(list, contProps, ptProps, &stateFlags,
                                  &changeFlags);
    /* printf("nextChange %d  state %d\n", nextChange, stateFlags); */
  }

  /* If not ending exactly on the point, cancel gap and connect */
  if (lastChange != pt) {
    ptProps->gap = ptProps->connect = 0;
    stateFlags &= ~(CHANGED_GAP | CHANGED_CONNECT);
  }
  /* printf("State flags at end of PointProps = %o\n", stateFlags);*/
  /* fflush(stdout); */
  return stateFlags;
}

/*!
 * Skips to location in [list] where the item index is [index] or greater.
 * Returns the index of the next item in the list, or -1 if there is none.
 */
int istoreSkipToIndex(Ilist *list, int index)
{
  int lookup, after;
  Istore *stp;
  lookup = istoreLookup(list, index, &after);
  if (lookup < 0)
    lookup = after;
  if (lookup >= ilistSize(list))
    return -1;
  stp = istoreItem(list, lookup);
  if (stp->flags & (GEN_STORE_NOINDEX | 3))
    return -1;
  return stp->index.i;
}


/*!
 * Returns 1 if the storage list in [list] has a transparency change that
 * matches [state], which should be 0 for trans of 0 and 1 for trans > 0,
 * or returns 0 if there is no list or no such trans change.  A reversion to
 * the default state is not counted.
 */
int istoreTransStateMatches(Ilist *list, int state)
{
  int i;
  Istore *stp;

  if (!ilistSize(list))
    return 0;
  for (i = 0; i < list->size; i++) {
    stp = istoreItem(list, i);
    if (stp->type == GEN_STORE_TRANS && !(stp->flags & GEN_STORE_REVERT) &&
        (stp->value.i ? 1 : 0) == state)
      return 1;
  }
  return 0;
}

/* END_SECTION */
