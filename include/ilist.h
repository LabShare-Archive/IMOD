/*  IMOD VERSION 2.02
 *
 *  ilist.h -- Image model list header.
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
Revision 3.2  2003/10/01 05:15:28  mast
Give the structure a name to avoid including in plugins

Revision 3.1  2002/12/01 15:39:50  mast
Declare extern C if c++

*/

#ifndef ILIST_H
#define ILIST_H

#ifdef __cplusplus
extern "C" {
#endif

#define LIST_QUANTUM 1

  typedef struct ilist_struct
  {
    void *data;        /* Pointer to data */
    int   dsize;       /* Size of data element in bytes */
    int   current;     /* Current item */
    int   size;        /* Number of items on list */
    int   store;       /* Number of items space allocated for */
    int   quantum;     /* Increment when allocating more space */
  }Ilist;


  Ilist *ilistNew(int dsize, int asize);
  void   ilistDelete (Ilist *list);
  void   ilistQuantum(Ilist *list, int size);
  Ilist *ilistDup(Ilist *list);

  void  *ilistFirst(Ilist *list);
  void  *ilistNext(Ilist *list);
  void  *ilistLast(Ilist *list);
  void  *ilistItem(Ilist *list, int element);
  int    ilistSize(Ilist *list);
  void   ilistTruncate(Ilist *list, int size);

  int    ilistAppend(Ilist *list, void *data);
  void   ilistRemove(Ilist *list, int element);
  int    ilistSwap  (Ilist *list, int e1, int e2);
  int   ilistInsert(Ilist *list, void *data, int element);

  int  ilistPush(Ilist *list, void *data);
  void *ilistPop(Ilist *list);
  int  ilistFloat(Ilist *list, int element);

#ifdef __cplusplus
}
#endif

#endif

