/*
 * percentile.c - Selecting an item at a percentile in a list
 *
 * Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 *
 */
#include "b3dutil.h"

#ifdef F77FUNCAP
#define percentilefloat PERCENTILEFLOAT
#else
#define percentilefloat percentilefloat_
#endif

/* 
 * Routines for selecting item number s (numbered from 1) out of num items
 * Based on a pascal program apparently from Handbook of Data Structures and 
 * Algorithms, by Gonnet and Baeza-Yates 
 */

/*!
 * Selects item number [s] (numbered from 1) out of [num] items in the array
 * [r], where items are considered in order from low to high.  [r] is partially
 * rearranged while finding the item.
 */
float percentileFloat(int s, float *r, int num)
{
  int lo = 0;
  int up = num - 1;
  int i, j;
  float temp;
  s--;
  while (up >= s && s >= lo) {
    i = lo;
    j = up;
    temp = r[s];
    r[s] = r[lo];
    r[lo] = temp;
    while (i < j) {
      while (r[j] > temp)
        j--;
      r[i] = r[j];
      while (i < j && r[i] <= temp)
        i++;
      r[j] = r[i];
    }
    r[i] = temp;
    if (s < i)
      up = i - 1;
    else
      lo = i + 1;
  }

  return r[s];
}

/*!
 * Fortran wrapper to @percentileFloat 
 */
double percentilefloat(int *s, float *r, int *num)
{
  return (double)percentileFloat(*s, r, *num);
}

/*!
 * Same as @percentileFloat but with an integer array [r]
 */
int percentileInt(int s, int *r, int num)
{
  int lo = 0;
  int up = num - 1;
  int i, j;
  int temp;
  s--;
  while (up >= s && s >= lo) {
    i = lo;
    j = up;
    temp = r[s];
    r[s] = r[lo];
    r[lo] = temp;
    while (i < j) {
      while (r[j] > temp)
        j--;
      r[i] = r[j];
      while (i < j && r[i] <= temp)
        i++;
      r[j] = r[i];
    }
    r[i] = temp;
    if (s < i)
      up = i - 1;
    else
      lo = i + 1;
  }

  return r[s];
}
