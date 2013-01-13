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

#define NULL_RETURN(a) {*nlist = (a); B3DFREE(list); return NULL;}

/*! 
 * Converts a list entry in the string [line] into a set of
 * integers, returns a pointer to the list of numbers, and returns the number 
 * of values in [nlist].  An example of a list is 1-5,7,9,11,15-20.
 * Numbers separated by dashes are replaced by
 * all of the numbers in the range.  Numbers need not be in any order,
 * and backward ranges (10-5) are handled.  Any characters besides
 * digits are valid separators.  A / at the beginning of the string will
 * return a NULL and a value of -1 in [nlist]; memory allocation errors return a NULL
 * and -2 in [nlist].  Parsing stops and the list is returned when a  
 * anything besides space, tab, comma, dash, or a digit occurs after a space or tab;
 * otherwise other characters (as well as comma or dash right after the last number)
 * make it return NULL and -3 in [nlist].  Negative numbers can be entered provided
 * that the minus sign immediately precedes the number.  E.g.: -3 - -1
 * or -3--1 will give -3,-2,-1; -3, -1,1 or -3,-1,1 will give -3,-1,1.
 */    
int *parselist (const char *line, int *nlist)
{
  char intern[10];
  char next;
  int dashlast, negnum, gotcomma, gotnum, gotSpace;
  int nchars;
  int number, lastnum, ind, numst, loopst, idir, i;
  int *list = NULL;

  dashlast = False;
  negnum = False;
  gotcomma = False;
  gotnum = False;
  gotSpace = False;
  nchars = strlen(line);
  /*printf("Entry: %s\n", line); */
  *nlist = 0;
  if (line[0] == 0x00)
    return(NULL);
  if (line[0] == '/')
    NULL_RETURN(-1);
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
          list = (int *)realloc(list, (*nlist + 1) *sizeof(int));
        else
          list = (int *)malloc(sizeof(int));
        if (!list)
          NULL_RETURN(-2);
        list[(*nlist)++] = i;
      }
      lastnum = number;
      negnum = False;
      dashlast = False;
      gotcomma = False;
      gotSpace = False;
      continue;
    }

    if (next == ' ' || next == '\t')
      gotSpace = True;

    /* With a non-allowed character, finish up if it is after a space, otherwise error */
    if (next != ',' && next != ' ' && next != '-' && next != '\t') {
      if (gotSpace)
        break;
      NULL_RETURN(-3);
    }
    if (next == ',') {
      gotcomma = True;
      gotSpace = False;
    }
      
    if (next == '-') {
      gotSpace = False;
      if (dashlast || !gotnum || gotcomma) 
        negnum = True;
      else
        dashlast = True;
    }
    ind++;
  } while (ind < nchars);

  /* If there are dangling commas or dashes it is an error too */
  if (gotcomma || negnum || dashlast)
    NULL_RETURN(-3);
  return list;
}

/*!
 * Fortran wrapper to @parselist, called by the Fortran parselist2 subroutine.  
 * The list is returned into array [list] and [limlist] is the size of that array, or
 * 0 if unknown.  The return value is 1 for a memory allocation error, 2 for a bad
 * character in the entry, and -1 for the
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
  retlist = parselist(tempstr, &ncopy);
  free(tempstr);
  if (!retlist && !ncopy) {
    *nlist = 0;
    return 0;
  }
  /* Return 0 for -1 (/), 1 for -2, and 2 for -3 */
  if (!retlist && ncopy < 0)
    return (-ncopy - 1);
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
