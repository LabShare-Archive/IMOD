/*  IMOD VERSION 2.63
 *
 *  dmprops.c - Determine size, data type, and offset to data in 
 *              DigitalMicrograph 3 files
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 2001 by Boulder Laboratory for 3-Dimensional Fine         *
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
Revision 3.2  2003/10/24 02:28:42  mast
strip directory from program name and/or use routine to make backup file

Revision 3.1  2001/12/31 19:19:52  mast
Initial version

*/
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#define BUFSIZE 160000
int main(int argc, char **argv)
{
  int i,c, offset, toffset, typeOffset;
  FILE *fp;
  char buf[BUFSIZE];
  char *found;
  struct stat statbuf;
  int xsize, ysize, maxread, type;
  int lowbyte, hibyte;

  /* The type-dependent values that were found after 
     D a t a % % % % 0 0 0 3 0 0 0 24 0 0 0 */
  int datacode[12] = {0, 2, 6, 0, 0, 0, 10, 3, 0, 9, 4, 5};
     
  int dataSize[12] = {1, 2, 4, 1, 1, 1, 1, 4, 1, 1, 2, 4};

  for (i = 1; i < argc; i++){
      
    offset = 0;
    xsize = 0;
    ysize = 0;
    type = -1;
    typeOffset = 0;

    if (stat(argv[i], &statbuf)) {
      fprintf(stderr, "Error doing stat of %s\n", argv[i]);
      exit(1);
    }
    fp = fopen(argv[i], "rb");
    if (!fp) {
      fprintf(stderr, "Error opening %s\n", argv[i]);
      exit(1);
    }
      
    maxread = statbuf.st_size - 2;
    if (maxread >= BUFSIZE)
      maxread = BUFSIZE - 1;

    /* Read the end of the file first because we need the data type
       before we can be sure we have the right Data%%%% entry */

    if (fseek(fp, -(maxread+1), SEEK_END)) {
      fprintf(stderr, "Error seeking to end of %s\n", argv[i]);
      exit(1);
    }
      
    if (!fread(buf, 1, maxread, fp)) {
      fprintf(stderr, "Error reading tail end of %s\n", argv[i]);
      exit(1);
    }

    buf[maxread - 1] = 0x00;
    for (c = 0; c < maxread && (xsize == 0 || type < 0); c++) {

      /* Look for D, then check if it is Dimensions or DataType */
      if (buf[c] == 68) {
        found = strstr(&buf[c], "Dimensions");
        if (found) {
          lowbyte =  (unsigned char)buf[c + 31];
          hibyte =  (unsigned char)buf[c + 32];
          xsize = lowbyte + 256 * hibyte;
          lowbyte =  (unsigned char)buf[c + 50];
          hibyte =  (unsigned char)buf[c + 51];
          ysize = lowbyte + 256 * hibyte;
        } else {
          found = strstr(&buf[c], "DataType");
          if (found) {
            type = buf[c + 20];
            if (!typeOffset)
              typeOffset = c + statbuf.st_size - (maxread + 1);
          } 
        }
      }
    }
    if (!xsize || !ysize || type < 0) {
      fprintf(stderr, "Dimensions or type not found in %s\n", 
              argv[i]);
      exit(1);
    }
      
    /* Now look for the Data string in the front of the file */
    rewind(fp);
    if (!fread(buf, 1, maxread, fp)) {
      fprintf(stderr, "Error reading beginning of %s\n", argv[i]);
      exit(1);
    }

    buf[maxread - 1] = 0x00;
    for (c = 0; c < maxread; c++) {
      if (buf[c] == 68) {
        found = strstr(&buf[c], "Data%%%%");
        if (found) {
          toffset = found + 24 - buf;

          /* If this is the first data string, or any data
             string that could still be far enough in front of the datatype
             string, save the offset */
          if (!offset || 
              toffset <= typeOffset - xsize * ysize * dataSize[type])
            offset = toffset;
             
          /* It used to be done with code types but that turned out to be
             unreliable */
          /* And if the code type is appropriate, save the 
             offset and break out */
          /*if (type <= 11 && buf[c + 19] == datacode[type]) {
            offset = toffset;
            break; 
            } */        
        }
      }
    }
    if (!offset) {
      fprintf(stderr, "Data string not found in %s\n", argv[i]);
      exit(1);
    }
    /* if (c == maxread)
      fprintf(stderr, "Code after data string not usual for file "
      "type for %s\n", argv[i]); */

    printf("%d %d %d %d\n", xsize, ysize, type, offset);
    fclose(fp);
  }
  return(0);
}
