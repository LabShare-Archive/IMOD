/*
 * Convert data between VMS and Unix, or between little- and big-endian Unix
 * 
 * Functions callable from fortran.
 *
 * define nothing to run on big-endian Unix and convert VMS data 
 * define OLD_F77FUNCAP to run on VMS and convert big-endian Unix data
 * define SWAP_IEEE_FLOATS to run on Unix and convert between little- and
 *        big-endian Unix data (i.e., with IEEE floats)
 * define F77FUNCAP for capitalized function names
 * define G77__HACK if the g77 compiler generates names with two __
 * 
 * July 2000: CER worked out changes needed for running under Linux with g77
 * August 2000: DNM integrated these changes into master code
 * Note: not tested under VMS
 * October 2003: DNM switched meaning of F77FUNCAP to work on Windows/Intel
 */

#include "imodconfig.h"

#ifdef F77FUNCAP

#define convert_shorts CONVERT_SHORTS
#define convert_longs  CONVERT_LONGS
#define convert_floats CONVERT_FLOATS

#else

#ifdef G77__HACK

#define convert_shorts convert_shorts__
#define convert_longs  convert_longs__
#define convert_floats convert_floats__

#else

#define convert_shorts convert_shorts_
#define convert_longs  convert_longs_
#define convert_floats convert_floats_

#endif
#endif

void convert_shorts(unsigned char *data, int *amt)
{
     register unsigned char *ldata = data + (*amt * 2);
     register unsigned char *ptr = data;
     register unsigned char tmp;

     while(ptr < ldata){
	  tmp = *ptr;
	  *ptr = ptr[1];
	  ptr[1] = tmp;
	  ptr+=2;
     }

}

void convert_longs(unsigned char *data, int *amt)
{
     register unsigned char *ldata = data + (*amt * 4);
     register unsigned char *ptr = data;
     register unsigned char tmp;
     while(ptr < ldata){
	  tmp = ptr[0];
	  ptr[0] = ptr[3];
	  ptr[3] = tmp;
	  ptr++;
	  tmp = *ptr;
	  *ptr = ptr[1];
	  ptr[1] = tmp;
	  ptr+=3;
     }
}

#ifdef SWAP_IEEE_FLOATS

/* IEEE: use a copy of convert_longs to swap the bytes for convert_floats */

void convert_floats(unsigned char *data, int *amt)
{
     register unsigned char *ldata = data + (*amt * 4);
     register unsigned char *ptr = data;
     register unsigned char tmp;
     while(ptr < ldata){
          tmp = ptr[0];
          ptr[0] = ptr[3];
          ptr[3] = tmp;
          ptr++;
          tmp = *ptr;
          *ptr = ptr[1];
          ptr[1] = tmp;
          ptr+=3;
     }
}

#else

/* To convert floats from little-endian VMS to big-endian IEEE */

void convert_floats(unsigned char *data, int *amt)
{
     unsigned char exp, temp;
     int i;
     register unsigned char *ptr = data;
     register unsigned char *maxptr = data + (*amt * 4);
     
     while (ptr < maxptr){

	  if ((exp = (ptr[1] << 1) | (ptr[0] >> 7 & 0x01)) > 3 &&
	      exp != 0)
	       ptr[1] -= 1;
	  else if (exp <= 3 && exp != 0)  /*must zero out the mantissa*/
	       {
		    /*we want manitssa 0 & exponent 1*/
		    ptr[0] = 0x80;
		    ptr[1] &= 0x80;
		    ptr[2] = ptr[3] = 0;
	       }
	  
	  temp = ptr[0];
	  ptr[0] = ptr[1];
	  ptr[1] = temp;
	  temp = ptr[2];
	  ptr[2] = ptr[3];
	  ptr[3] = temp;
	  ptr+=4;
     }
}

#endif

#ifdef OLD_F77FUNCAP

/* If VMS: To convert floats from big-endian IEEE to little-endian VMS */

void convert_ufloats(unsigned char *data, int *amt)
{
     unsigned char exp, temp;
     int i;
     register unsigned char *ptr = data;
     register unsigned char *maxptr = data + (*amt * 4);
     
     while (ptr < maxptr){
	  if ((exp = (ptr[0] << 1) | (ptr[1] >> 7 & 0x01)) < 253 && exp != 0)
	       ptr[0] += 1;
	  else if (exp >= 253) /*must also max out the exp & mantissa*/
	       {
		    /*we want manitssa all 1 & exponent 255*/
		    ptr[0] |= 0x7F;
		    ptr[1] = 0xFF;
		    ptr[2] = ptr[3] = 0xFF;
	       }
	  
	  temp = ptr[0];
	  ptr[0] = ptr[1];
	  ptr[1] = temp;
	  temp = ptr[2];
	  ptr[2] = ptr[3];
	  ptr[3] = temp;
	  ptr+=4;
     }
}

#endif

#ifdef TEST_DRIVER

#include <stdio.h>

int main(int argc, char **argv)
{
     char *tst_short = "BLBLBLBL";
     char *tst_long  = "AbCdAbCd";
     int ssize = 4;
     int lsize = 2;

     float ftst[2]; 

     printf("short BL = %s\n", tst_short);
     convert_shorts_(tst_short, &ssize);
     printf("swap short LB = %s\n", tst_short);

     printf("long ABCD = %s\n", tst_long);
     convert_longs_(tst_long, &lsize);
     printf("swap long DCBA = %s\n", tst_long);
     convert_longs_(tst_long, &lsize);

     printf("float ABCD = %s\n", tst_long);
     convert_floats_(tst_long, &lsize);
     printf("swap float DCBA = %s\n", tst_long);
	  
     ftst[0] = 1559.8f;
     ftst[1] = 545.0f;

     printf("floats %g %g converted to",  ftst[0], ftst[1]);
     convert_floats_((unsigned char *)ftst, &lsize);
     convert_ufloats((unsigned char *)ftst, &lsize);
     printf("%g %g \n", ftst[0], ftst[1]);

     return(0);
}

#endif

