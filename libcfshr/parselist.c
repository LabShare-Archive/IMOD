/* Parselist - parses a list of numbers
 * Translated from a Fortran routine, a sobering experience.
 * $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "b3dutil.h"
#include "imodconfig.h"
#define True  1
#define False 0

#ifdef F77FUNCAP
#define parselistfw PARSELISTFW
#else
#define parselistfw parselistfw_
#endif

/*! 
 * Converts a list entry in the string [line] into a set of
 * integers, returns a pointer to the list of numbers, and returns the number 
 * of values in [nlist].  An example of a list is 1-5,7,9,11,15-20.
 * Numbers separated by dashes are replaced by
 * all of the numbers in the range.  Numbers need not be in any order,
 * and backward ranges (10-5) are handled.  Any characters besides
 * digits are valid separators.  A / at the beginning of the string will
 * return a NULL and a value of -1 in [nlist]; memory allocation errors return a NULL
 * and a nonnegative value in [nlist].  Negative numbers can be entered provided
 * that the minus sign immediately precedes the number.  E.g.: -3 - -1
 * or -3--1 will give -3,-2,-1; -3, -1,1 or -3,-1,1 will give -3,-1,1.
 */    
int *parselist (const char *line, int *nlist)
{
  char intern[10];
  char next;
  int dashlast, negnum, gotcomma, gotnum;
  int nchars;
  int number, lastnum, ind, numst, loopst, idir, i;
  int *list = NULL;

  dashlast = False;
  negnum = False;
  gotcomma = False;
  gotnum = False;
  nchars = strlen(line);
  /*  printf("Entry: %s\n", line); */
  *nlist = 0;
  if (line[0] == '/')  {
    *nlist = -1;
    return(NULL);
  }
  ind = 0;
  lastnum = 0;

  /*   find next digit and look for '-', but terminate on non -,space */
  do {
    next = line[ind];
    if (next >= '0' && next <= '9') {

      /*   got a digit: save ind, find next non-digit */
      gotnum = True;
      numst = ind;
      do {
        ind++;
        next = line[ind];
      } while (next >= '0' && next <= '9');

      /*  move number right-justified into intern and read it */
      
      if (negnum) 
        numst--;
      for (i = numst; i < ind && i < numst + 9; i++)
        intern[i - numst] = line[i];
      intern[i - numst] = 0x00;
      sscanf(intern, "%d", &number);

      /* set up loop to add to list */
      loopst = number;
      idir = 1;
      if (dashlast) {
        if (lastnum > number) idir = -1;
        loopst = lastnum + idir;
      }
      for (i = loopst; idir * i <= idir * number; i += idir) {
        if (*nlist)
          list = (int *)realloc(list, (*nlist + 1) *
                                sizeof(int));
        else
          list = (int *)malloc(sizeof(int));
        if (!list)
          return(NULL);
        list[(*nlist)++] = i;
      }
      lastnum = number;
      negnum = False;
      dashlast = False;
      gotcomma = False;
      continue;
    }
    
    if (next != ',' && next != ' ' && next != '-') 
      return(0);
    if (next == ',') 
      gotcomma = True;
    if (next == '-') {
      if (dashlast || !gotnum || gotcomma) 
        negnum = True;
      else
        dashlast = True;
    }
    ind++;
  } while (ind < nchars);
  return list;
}

/*!
 * Fortran wrapper to @parselist, called by the Fortran parselist2 subroutine.  
 * The list is returned into array [list] and [limlist] is the size of that array, or
 * 0 if unknown.  The return value is 1 for a memory allocation error and -1 for the
 * list too big for the array; in the latter case the portion of the list that fits
 * is returned but parselist2 will exit with an error.  If a slash is entered, the input 
 * list is returned unchanged.
 */
int parselistfw(const char *line, int *list, int *nlist, int *limlist, int linelen)
{
  int ncopy;
  int *retlist;
  char *tempstr =   f2cString(line, linelen);
  if (!tempstr)
    return 1;
  if (tempstr[0] == 0x00) {
    *nlist = 0;
    return 0;
  }
  retlist = parselist(tempstr, &ncopy);
  free(tempstr);
  if (!retlist && ncopy < 0)
    return 0;
  if (!retlist)
    return 1;
  *nlist = ncopy;
  if (*limlist > 0 && ncopy > *limlist)
    *nlist = *limlist;
  memcpy(list, retlist, *nlist * sizeof(int));
  free(retlist);
  return (*limlist > 0 && ncopy > *limlist) ? -1 : 0;
}

/*
  int main(int argc, char **argv)
  {
  int nlist, i;
  int *list;
  nlist = 0;
  if (argc < 2) exit(1);
  list = parselist(argv[1], &nlist);
  if (!list)
  printf ("Error return\n");
  printf ("%d values:\n", nlist);
  for (i = 0; i< nlist; i++)
  printf("%d ", list[i]);
  exit(0);
  } */
