/*
 * stat.cpp - different basic statistical measures
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */

#include "stat.h"

/*
double variance(float* f, int size) {
  double ans = 0;
  for (int i = 0; i < size; i++) {
    ans += (double)f[i] * (double)f[i];
  }
  double avg = average(f, size);
  return (ans / size - avg * avg);
}
*/
double average(float* f, int size) {
  double ans = 0;
  for (int i = 0; i < size; i++)
    ans += (double)f[i];
  return (ans / size);
}

void meanAndVariance(float* f, int size,float *average,float *variance) {
  float q2=0.0;
  float q=0.0;
  for (int i = 0; i < size; i++)
  {
    q += f[i];
    q2+=(f[i]*f[i]);
  }
  *average=(q/(double)size);
  *variance=(q2-size*(*average)*(*average))/(double)(size-1);
}

float max(float* f, int size) {
  float ans = - 1E+37;
  for (int i = 0; i < size; i++)
    if (f[i] > ans)
      ans = f[i];
  return ans;
}
