/* 7/21/00 CER                                          */
/*	provide access to rand and srand for FORTRAN    */
/*							*/

#include <stdlib.h>
/*  #include <stdio.h>                                  */

#define myrand myrand_
#define mysrand mysrand_


void mysrand(int *seed, float *val)
{
	srand(*seed);
        *val = (float) rand()/RAND_MAX;
/*        printf("mysrand:  *seed:  %d  *val: %20.8f\n",*seed,*val); */
/*	printf("MAX RND: %d\n",RAND_MAX);			     */
	return;
}

void myrand(int *seed, float *val)
{
	*val = (float) rand()/RAND_MAX;
/*        printf("myrand:              *val: %20.8f\n",*val);        */
	return;
}


