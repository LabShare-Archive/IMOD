/* A simple file to provide string output from the fortran library */

#include <stdio.h>
#include "imodconfig.h"
#ifdef F77FUNCAP
#define b3dputs B3DPUTS
#else
#define b3dputs b3dputs_
#endif

void b3dputs(char *string, int strlen)
{
  int i;
  for (i = 0; i < strlen; i++)
    printf("%c", string[i]);
  printf("\n");
  fflush(stdout);
}
