/*  UNSUPPORTED
 *
 *  tifinfo.c
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

#include <stdio.h>

int Verbose = 0;

char *typeStrings[] = 
{"NULL ", "BYTE ", "ASCII", "SHORT", "LONG ", "RATIONAL"};


static void swap(char *ptr, unsigned size)

{
     unsigned char *begin;
     unsigned char *end;
     unsigned char tmp;
     int           i;
     int           len;
     
     if ((size % 2) != 0)
	  size--;
     
     begin = ( unsigned char *)ptr;
     end   = ( unsigned char *)ptr + (size - 1);
     
     for (i = 0; i < (size/2); i++) {
	  tmp = *begin;
	  *begin = *end;
	  *end   = tmp;
	  
	  begin++;
	  end--;
     }
}

static void tiff_print_info(FILE *fp)
{

     unsigned short byteOrder, versionNumber, IFDentries;
     int ifdi = 1;
     unsigned int i, IFDoffset;
     int dataIndex, dsize;
     short swapData = 0;
     rewind(fp);

     fread(&byteOrder, sizeof(unsigned short), 1, fp);

     if ( (byteOrder != 0x4949) && (byteOrder != 0x4d4d)){
	  printf("Not a tif file: first word must be 0x4949 or 0x4d4d,  "
		 "not %x\n", byteOrder);
	  return;
     }
     if ( byteOrder != 0x4d4d){
	  swapData = 1;
     }
     printf("TIFF: %x" , byteOrder);

     fread(&versionNumber, sizeof(unsigned short), 1, fp);

     if (swapData)
	  swap((char *)&(versionNumber), 2);

     if ((versionNumber != 0x002a) && (versionNumber != 0x2a00)){
	  printf("\nBad TIFF Version number. Must be 42, not %d\n",
		 versionNumber);
	  return;
     }
     printf(" %x\n", versionNumber);

     fread(&IFDoffset, 4, 1, fp);
     if (swapData)
	  swap((char *)&(IFDoffset), 4);
     
     while (IFDoffset){
	 fseek(fp, IFDoffset, SEEK_SET);
	 fread(&IFDentries, 2, 1, fp);
	 if (swapData)
	     swap((char *)&IFDentries, 2);
	  printf("Reading %d entries in record %d at %d:\n", 
		 IFDentries, ifdi++, IFDoffset);
	  for (i = 0; i < IFDentries; i++){
	       unsigned short tag, type;
	       unsigned int   length, value;

	       fread(&tag, 2, 1, fp);
	       fread(&type, 2, 1, fp);
	       fread(&length, 4, 1, fp);
	       fread(&value,  4, 1, fp);

	       if (ferror(fp)){
		    perror("tifinfo");
		    return;
	       }

	       if (swapData){
		    swap((char *)&tag, 2);
		    swap((char *)&type, 2);
		    swap((char *)&length, 4);
		    swap((char *)&value, 4);
	       }
	       else if ((type == 3) && (length < 3))
		    value >>= 16;

	       printf("\t%d %s %d %d ",
		      tag, typeStrings[type], length, value);

	       switch(type){
		  case 1:
		  case 2:
		    dsize = length;
		    break;
		  case 3:
		     dsize = length * 2;
		    break;
		  case 4:
		    dsize = length * 4;
		    break;
		  default:
		    dsize = 0;
		    break;
	       }

if (Verbose)
	       if (dsize > 4){
		    int pos = ftell(fp);
		    char bbuf;
		    short sbuf;
		    int   lbuf;
		    fseek(fp, value, SEEK_SET);
		    printf("\t : ");

		    for(dataIndex = 0; dataIndex < length; dataIndex++){

			 switch(type){
			      
			    case 1: /* byte */
			      fread(&bbuf, 1, 1, fp);
			      printf("%d ",bbuf);
			      break;
			    case 2: /* ascii */
			      fread(&bbuf, 1, 1, fp);
			      printf("%c",bbuf);
			      break;
			    case 3: /* short */
			      fread(&sbuf, 2, 1, fp);
			      printf("%d ",sbuf);
			      break;
			    case 4: /* long */
			      fread(&lbuf, 4, 1, fp);
			      printf("%d ",lbuf);
			      break;
			    case 5: /* rational */
			    default:
			      break;
			 }
		    }
		    fseek(fp, pos, SEEK_SET);
	       }

	       printf("\n");

	  }
	 fseek(fp, (IFDoffset + 2) + (IFDentries * 12), SEEK_SET);
	 fread(&IFDoffset, 4, 1, fp);
	 if (swapData)
	       swap((char *)&IFDoffset, 4);
     }
     return;
}


int main(int argc, char **argv)
{
     FILE *fin;
     int first = 1;

     int i;

     if (argc > 2)
	 if (argv[1][0] == '-')
	     if (argv[1][1] == 'v'){
		 Verbose = 1;
		 first++;
	     }

     for(i = first; i < argc; i++){
	 fin = fopen(argv[i], "rb");
	 if (!fin) exit(1);
	 tiff_print_info(fin);
	 fclose(fin);
     }

     exit(0);
}
