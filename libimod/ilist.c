/*  IMOD VERSION 2.02
 *
 *  ilist.c -- Model list structure library routines.
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

#include "ilist.h"

Ilist *ilistNew(int dsize, int asize)
{
     Ilist *l = (Ilist *)malloc(sizeof(Ilist));

     if (!l) return(NULL);
     l->dsize = dsize;
     l->current = -1;
     l->size  = 0;
     l->store = asize;
     l->data = NULL;
     if (asize)
	  l->data = malloc(asize * dsize);
     return(l);
}

void   ilistDelete (Ilist *list)
{
     if (!list) return;
     if (list->data) free(list->data);
     free(list);
}

void *ilistFirst(Ilist *list)
{
     if (!list) return(NULL);
     if (!list->size) return(NULL);
     list->current = 0;
     return(list->data);
}

void *ilistNext(Ilist *list)
{
     char *rptr;
     list->current++;
     if (list->current >= list->size)
	  return(NULL);
     rptr = (char *)list->data;
     rptr += (list->current * list->dsize);
     return((void *)rptr);
}

void  *ilistItem(Ilist *list, int element)
{
     char *rptr;

     if (!list) return(NULL);
     if (!list->size) return(NULL);
     if (element < 0) return(NULL);
     if (element >= list->size) return(NULL);

     rptr = (char *)list->data;
     list->current = element;
     rptr += (list->current * list->dsize);
     return((void *)rptr);
}

int    ilistSize(Ilist *list)
{
     if (!list) return(0);
     return(list->size);
}

void   ilistAppend(Ilist *list, void *data)
{
     char *to;

     if (list->store <= list->size){
	  void *tptr = realloc(list->data, 
			       (list->dsize * (list->size+LIST_QUANTUM)));
	  list->store += LIST_QUANTUM;
	  list->data = tptr;
     }

     to = (char *)list->data;
     to += (list->size * list->dsize);
     list->size++;
     memcpy(to, data, list->dsize);
}

void   ilistRemove(Ilist *list, int element)
{
     int i;
     char *to, *from;

     if (list->size <= element)
	  return;

     to = list->data;
     to += element * list->dsize;
     from = to;
     from += list->dsize;
     for(i = element; i < (list->size - 1); i++){
	  memcpy(to, from, list->dsize);
	  from += list->dsize;
	  to   += list->dsize;
     }
     list->size--;
}

void   ilistSwap  (Ilist *list, int e1, int e2)
{
     char *p1, *p2;
     void *tptr;

     if (e1 >= list->size) return;
     if (e2 >= list->size) return;

     p1 = p2 = (char *)list->data;
     p1 += (e1 * list->dsize);
     p2 += (e2 * list->dsize);
     tptr = malloc(list->dsize);
     if (!tptr) return;

     memcpy(tptr, p1, list->dsize);
     memcpy(p1,   p2, list->dsize);
     memcpy(p2, tptr, list->dsize);
     free(tptr);
}


void ilistPush(Ilist *list, void *data)
{
     int i;
     ilistAppend(list, data);

     for(i = list->size - 1; i > 0; i--){
	  ilistSwap(list, i, i - 1);
     }
     memcpy(list->data, data, list->dsize);
     return;
}

void *ilistPop(Ilist *list)
{
     void *data;
     
     if (!list->size) return (NULL);
     data = (void *)malloc(list->dsize);
     if (!data) return(NULL);
     memcpy(data, list->data, list->dsize);
     ilistRemove(list, 0);
     return(list);
}

void ilistFloat(Ilist *list, int element)
{
     void *data;
     char *p1;

     if (element < 1) return;
     if (element >= list->size) return;

     p1 = (char *)list->data;
     p1 += (element * list->dsize);
     data = (void *)malloc(list->dsize);
     if (!data) return;

     memcpy(data, p1, list->dsize);
     ilistRemove(list, element);
     ilistPush(list, data);
     free(data);
}

void   ilistInsert(Ilist *list, void *data, int element)
{
     ilistAppend(list, data);
     ilistSwap(list, element, list->size - 1);
}



