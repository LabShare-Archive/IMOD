/* licenseusfft checks whether the USFFT license file exists.  It returns 0
   if it does, 1 of there is no environment varaible set, or 2 if the file
   cannot be opened */

#include <stdio.h>
#include <stdlib.h>
#include "imodconfig.h"

#ifdef F77FUNCAP
#define licenseusfft LICENSEUSFFT
#else
#define licenseusfft licenseusfft_
#endif

int licenseusfft(void)
{
     FILE *fp;
     char *filename = getenv("USFFT2_LICENSE_FILE");
     if (filename == NULL)
	  return 1;
     fp = fopen(filename, "r");
     if (fp == NULL)
	  return 2;
     fclose(fp);
     return 0;
}
