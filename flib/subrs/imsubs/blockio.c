/******************************************************************************
 * SYSTEM.....: Integrated Crystollagraphy Environment (I.C.E)
 * APPLICATION:
 *
 * MODULE.....: blockio.c
 *
 * PURPOSE....:
 *   to provide low level file access methods for accessing mrc files
 *
 * TAG: BIO
 *
 * REVISIONS..:
 *   Ross Dargahi                December 1990
 *
 ******************************************************************************/
/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.4  2002/10/23 21:01:06  mast
Directed error messages to stdout instead of stderr because that is
where Fortran program messages go

Revision 3.3  2002/07/21 19:22:34  mast
Standardized error output to ERROR: ROUTINE

Revision 3.2  2002/06/26 00:22:53  mast
Changed abort calls to exit(-1) so that they would set error status

Revision 3.1  2002/01/07 18:23:34  mast
Add an error message if big_seek fails

*/
 
#define BLOCKIO_C    /*define source name for includes*/
 
/******************************************************************************
Include Files
******************************************************************************/
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>   /* JRK: added for strcmp */
#include <math.h>
 
#include "environ.h"
 
/******************************************************************************
Private Defines
******************************************************************************/

/* DNM: change from ST3000 to F77FUNCAP to be consistent with other code */
#ifdef F77FUNCAP

#define fill       FILL
#define move       MOVE
#define qback      QBACK
#define qclose     QCLOSE
#define qinquire   QINQUIRE
#define qlocate    QLOCATE
#define qmode      QMODE
#define qopen      QOPEN
#define qread      QREAD
#define qseek      QSEEK
#define qskip      QSKIP
#define qwrite     QWRITE
#define zero       ZERO

#else

#define fill       fill_
#define move       move_
#define qback      qback_
#define qclose     qclose_
#define qinquire   qinquire_
#define qlocate    qlocate_
#define qmode      qmode_
#define qopen      qopen_
#define qread      qread_
#define qseek      qseek_
#define qskip      qskip_
#define qwrite     qwrite_
#define zero       zero_

#endif
 
#define MAX_MODE 17    /* JRK: max mode from 5 to 17 */
#define MAX_UNIT 20    /* JRK: max unit from 5 to 10; DNM to 20 */

/* JRK: added these for the new attribute in Unit struct. */
#define UNIT_ATBUT_RO      1
#define UNIT_ATBUT_NEW     2
#define UNIT_ATBUT_OLD     3
#define UNIT_ATBUT_SCRATCH 4

/******************************************************************************
Private Data Structures
******************************************************************************/

/* DNM 10/23/00: switched from using system dependent "seek_type" and 
   "seek_name" to calling a "big_seek" routine as in mrcfiles.  But had to use
   lseek instead of fseek because files here are opened with open, not fopen. 
   Make the "pos" element be an unsigned int, to preserve its function for
   files up to 4 GB.  It is used only in qlocate, which is used only in an
   internal test in imsubs */

typedef struct
{
  /* DNM, 8/13/00: changed this to be consistent with all of the other
     VMS calling conventions in our code.  We no longer have the blockio.c
     that was running under VMS, so it is hard to tell if this is right,
     but it has a bigger chance o being right than the other way */
  /*  char* str;
      int   len; */
  unsigned short len;
  char dum1;
  char dum2;
  char *str;
} strDesc_s;
 
typedef struct
{
  int  being_used;
  char fname[256];
  FILE *fp;
  int  num_char_per_item;
  int  read_only;
  int  write_only;
  unsigned int  pos;
  int  attribute;  /* JRK: keep track of attibutes for files. */
} Unit;
 
/******************************************************************************
Private Function Prototypes
******************************************************************************/

/* CER July 2000: renamed these functions to avoid overlap with standard
   functions */
/* PRIVATE void bcopy(); */
PRIVATE void mybcopy(register char *a,register char *b,int n);
/* PRIVATE void bzero(); */
PRIVATE void mybzero(register char *a, int n); 
PRIVATE fcmp();
PRIVATE find_unit();
PRIVATE void get_fstr();
PRIVATE void set_fstr();
PRIVATE int big_seek(FILE *fp, int base, int size1, int size2, int flag);
 
/******************************************************************************
Private Global Declarations
******************************************************************************/
 
PRIVATE Unit units[MAX_UNIT];
PRIVATE int modes[MAX_MODE] = {1, 2, 4, 4, 8, 1, 1, 1,
                               1,1,1,1,1,1,1,1,3 };
/* JRK: extented modes to include bit modes 9-15, and rgb mode 16. */

/******************************************************************************
 ***************************    PUBLIC FUNCTIONS    ****************************
 ******************************************************************************/

/* DNM 6/14/01: rewrite to use fopen instead of open, etc.  Also so that 
   F77STRING version just sets up the variables and uses the rest of the code
   the same.  Not tested, of course */
/* DNM: make this F77STRING instead of ST3000 */
#ifdef F77STRING

void qopen(iunit, f77name, f77attribute)
     int *iunit;
     strDesc_s *f77name;
     strDesc_s *f77attribute;
{
  char *name = f77name->str;
  char *attribute = f77attribute->str;
  int name_l = f77name->len;
  int attr_l = f77attribute->len;

#else

void qopen(iunit, name, attribute, name_l, attr_l) 
  int *iunit;
  char *name;
  char *attribute;
  int name_l;
  int attr_l;
{
#endif     

  int unit = find_unit();
  Unit *u = units + unit;  /* JRK: Style, declarations should be up here. */
  int  noChars;
  char oldfilename[257];
  char matstr[16];
  int mode;
  char *modes[4] = {"rb", "rb+", "wb", "wb+"};
  struct stat buf;

  if (unit >= 0)
    {
      u->being_used = 1;
      get_fstr(name, name_l, u->fname, sizeof u->fname);
      u->write_only = 0;
      u->read_only = 0;
      u->pos = 0;
      u->num_char_per_item = 1;
      get_fstr(attribute, attr_l, matstr, sizeof matstr);

      if (strncmp(matstr, "RO", 1) == 0){
        mode = 0;
        u->attribute = UNIT_ATBUT_RO;
        u->read_only = 1;
      }
      if (strncmp(matstr, "NEW", 1) == 0){
        get_fstr(name, name_l, oldfilename, sizeof u->fname);

        /* DNM 10/20/03: check for existence of file before making backup,
           and delete old backup first */
        if (!stat(oldfilename, &buf)) {
          oldfilename[strlen(oldfilename) + 1] = 0x00;
          oldfilename[strlen(oldfilename)] = '~';
          remove(oldfilename);
          if (rename(u->fname, oldfilename)) {
            fprintf(stdout, "WARNING: qopen - Could not rename '%s' to '%s'\n"
                    , u->fname, oldfilename);
            perror("");
          }
        }
        mode = 3;
        u->attribute = UNIT_ATBUT_NEW;
      }
      if (strncmp(matstr, "OLD", 1) == 0){
        mode = 1;
        u->attribute = UNIT_ATBUT_OLD;
      }
      if (strncmp(matstr, "SCRATCH", 1) == 0){
        mode = 3;
        u->attribute = UNIT_ATBUT_SCRATCH;
      }

      u->fp = fopen(u->fname, modes[mode]);
      if (u->fp == NULL)
        {
          fprintf(stdout, "ERROR: qopen - Could not open '%s'\n"
                  , u->fname);
          perror(""); /* JRK: have system tell why. */
          exit(-1);
        }
      *iunit = unit + 1;
    }
  else
    {
      *iunit = -1;
    }
}


void qclose(iunit)
  int *iunit;
 {
   int unit = *iunit - 1;
   if (unit >= 0 && unit < MAX_UNIT)
     {
       Unit *u = units + unit;
       if (u->being_used)
         {
           u->being_used = 0;
           fclose(u->fp);
           /* JRK: Delete scratch files */
           if (u->attribute == UNIT_ATBUT_SCRATCH)
             unlink(u->fname);
         }
     }
}
 
 void qmode(iunit, mode, nchitm)
   int *iunit;
 int *mode;
 int *nchitm;
 {
   int unit = *iunit - 1;
   if (unit >= 0 && unit < MAX_UNIT)
     {
       Unit *u = units + unit;
       int mchitm =
         (*mode >= 0 && *mode < MAX_MODE) ?
         modes[*mode] :
         1;
       *nchitm = u->num_char_per_item = mchitm;
     }
 }
 
 void qread(iunit, array, nitems, ier)
   int *iunit;
 char *array;
 int *nitems;
 int *ier;
 {
   int unit = *iunit - 1;
   if (unit >= 0 && unit < MAX_UNIT)
     {
       Unit *u = units + unit;
       int bc = u->num_char_per_item * *nitems;
       if (u->write_only)
         {
           fprintf(stdout, "ERROR: qread - file is write only.\n");
           exit(-1);
         }
       if (fread(array, 1, bc, u->fp) != bc)
         {
           fprintf(stdout, "ERROR: qread - read error\n");
           perror("");
           exit(-1);
         }
       u->pos += bc;
       *ier = 0;
     }
   else
     {
       *ier = -1;
     }
 }
 
void qwrite(iunit, array, nitems)
   int *iunit;
 char *array;
 int *nitems;
 {
   int unit = *iunit - 1;

   if (unit >= 0 && unit < MAX_UNIT)
     {
       Unit *u = units + unit;
       int bc = u->num_char_per_item * *nitems;
       if (u->read_only)
         {
           fprintf(stdout, "ERROR: qwrite - file is read only.\n");
           exit(-1);
         }
       if (fwrite(array, 1, bc, u->fp) != bc)
         {
           fprintf(stdout, "ERROR: qwrite - error writing file.\n");
           perror("");
           exit(-1);
         }
       u->pos += bc;
     }
 }
 
 /* DNM 10/23/00: switch from using system-dependent "seek_name" to call a
    big_seek function that seeks in chunks less than 2 GB; also change test
    for error to test for -1 returned rather than a negative number.
    Change to test for nonzero when switch to fseek */
 void qseek(iunit, irecord, ielement, ireclength)
   int *iunit;
 int *irecord;
 int *ielement;
 int *ireclength;
 {
   int unit = *iunit - 1;
   if (unit >= 0 && unit < MAX_UNIT)
     {
       Unit *u = units + unit;
       u->pos = ((unsigned int)(*irecord - 1) * 
                 (unsigned int)*ireclength + 
                 (unsigned int)(*ielement - 1)) * 
         (unsigned int)u->num_char_per_item;
       /*  if (lseek(u->fp, u->pos = pos, 0) < 0) */
       if (big_seek(u->fp, (*ielement - 1) * u->num_char_per_item,
                    *irecord - 1, 
                    *ireclength * u->num_char_per_item, 
                    SEEK_SET))
         {
           fprintf(stdout, "ERROR: qseek - Error on big_seek\n");
           exit(-1);
         }
     }
 }
 
 /* qback and qskip are used only for small movements, within a section, so
    they don't need to call big_seek.  However, change the test for error to
    test for = -1 instead of < 0; then to test for !=0 when switch to fseek */  
 void qback(iunit, ireclength)
   int *iunit;
 int *ireclength;
 {
   int unit = *iunit - 1;
   if (unit >= 0 && unit < MAX_UNIT)
     {
       Unit *u = units + unit;
       int amt = -(*ireclength * u->num_char_per_item);
       u->pos += amt;
       if (fseek(u->fp, amt, SEEK_CUR))
         {
           fprintf(stdout, "ERROR: qskip - Error on seek\n");
           exit(-1);
         }
     }
 }
 
 void qskip(iunit, ireclength)
   int *iunit;
 int *ireclength;
 {
   int unit = *iunit - 1;
   if (unit >= 0 && unit < MAX_UNIT)
     {
       Unit *u = units + unit;
       int amt = *ireclength * u->num_char_per_item;
       u->pos += amt;
       if (fseek(u->fp, amt, SEEK_CUR))
         {
           fprintf(stdout, "ERROR: qskip - Error on seek\n");
           exit(-1);
         }
     }
 }

 /* DNM 8/13/00: added the version for VMS for completeness (not tested), and
    added a declaration for filename_l */
#ifdef F77STRING

 void qinquire(iunit, f77str, flen)
   int *iunit;
 strDesc_s *f77str;
 int *flen;
 {
   char *filename = f77str->str;
   int filename_l = f77str->len;

#else 

void qinquire(iunit, filename, flen, filename_l)
  int *iunit;
 char *filename;
 int *flen;
 int filename_l;
 {

#endif

   int unit = *iunit - 1;
   if (unit >= 0 && unit < MAX_UNIT)
     {
       Unit *u = units + unit;
       struct stat buf;
       stat(u->fname, &buf);
       set_fstr(filename, filename_l, u->fname);
       *flen = buf.st_size / 512;
     }
   else
     {
       /* DNM: HUH? */
       /* set_fstr(filename, filename_l, ""); */
       *flen = -1;
     }
 }
 
 /* This will fail for large files, but try to keep pos good up to 4 GB by 
    making it an unsigned int */
 void qlocate(iunit, location)
   int *iunit;
 int *location;
 {
   int unit = *iunit - 1;
   if (unit >= 0 && unit < MAX_UNIT)
     {
       Unit *u = units + unit;
       *location = u->pos / u->num_char_per_item + 1;
     }
 }
 
 void move(a, b, n)
   char *a;
 char *b;
 int *n;
 {
   mybcopy(a, b, *n);
 }
 
 void zero(a, n)
   char *a;
 int *n;
 {
   mybzero(a, *n);
 }
 
 void fill(a, b, np, lb)
   register char *a;
 char *b;
 int *np;
 int lb;             /* MWT : added on 19.AUG.91 */
 {
   register int n = *np;

   if (strlen(b) == 1)   /* MWT :: temporary fix 20.AUG.91 */
     {
       lb = *np;
       n  = 1;
     }

   while(n--)
     {
       register int l = lb;
       register char *c = b;
       while(l--)
         {
           *a++ = *c++;
         }
     }
 }
 
 /************************************************************************
  ***************************    PRIVATE FUNCTIONS    ********************
  ************************************************************************/
 
 PRIVATE fcmp(s, l, str)
 
   char *s;
 int  l;
 char *str;
 
 /* Compares a C and Fortran string */
 
 {
   while (*str && l--)
     {
       int diff = *str++ - *s++;
 
       if (diff)
         return diff;
     }
 
   if (*str)
     return(-1);
 
   while (l--)
     if (*s++ != ' ')
       return(1);
 
   return(0);
 
 } /*fcmp*/
 
   /************************************************************************/
 PRIVATE void get_fstr(fstr, lfstr, str, l)
 
   char *fstr;
 char *str;
 
 /* Creates a c string from a fortran one */
 
 {
   int i = 0;
 
   while (lfstr > 0 && l > 1)
     {
       str[i++] = *fstr++;
       lfstr--;
       l--;
     }
 
   while (i > 0 && str[i - 1] == ' ')
     {
       i--;
       l++;
     }
 
   if (l > 0)
     str[i] = 0;
 
 } /*get_fstr*/
 
   /************************************************************************/
   /* Creates a fortran string from a c one */
 PRIVATE void set_fstr(fstr, lfstr, str)
   char *fstr;
 char *str;
 {
   while(lfstr > 0 && *str)
     {
       *fstr++ = *str++;
       lfstr--;
     }
   while(lfstr > 0)
     {
       *fstr++ = ' ';
       lfstr--;
     }
 }
 
 PRIVATE int find_unit()
   {
     int i;
     for(i = 0; i < MAX_UNIT; i++)
       {
         if (!units[i].being_used)
           {
             return i;
           }
       }
     return -1;
   }
 
 PRIVATE  void mybcopy(a, b, n)
   register char *a;
 register char *b;
 register int n;
 {
   while(n--)
     {
       *a++ = *b++;
     }
 }
 
 PRIVATE  void mybzero(a, n)
   register char *a;
 register int n;
 {
   while(n--)
     {
       *a++ = 0;
     }
 } 
 
#define SEEK_LIMIT 2000000000
 
PRIVATE int big_seek(FILE *fp, int base, int size1, int size2, int flag)
{
  int smaller, bigger, ntodo, ndo, abs1, abs2;
  int steplimit, err;

  /* Do the base seek if it is non-zero, or if the rest of the seek is
     zero and we are doing a SEEK_SET */
  if (base || ((!size1 || !size2) && (flag == SEEK_SET))) {
    if (err = fseek(fp, base, flag))
      return err;
    flag = SEEK_CUR;
  }

  if (!size1 || !size2)
    return 0;

  /* Find smaller and larger size */
  abs1 = size1 >= 0 ? size1 : -size1;
  abs2 = size2 >= 0 ? size2 : -size2;
  smaller = abs1 < abs2 ? abs1 : abs2;
  bigger = abs1 < abs2 ? abs2 : abs1;

  /* Step by multiples of the larger size, but not by more than the limit */
  steplimit = SEEK_LIMIT / bigger;
  ntodo = smaller;

  /* If one of the size entries is negative, negate the steps */
  if ((size1 < 0 && size2 >= 0) || (size1 >= 0 && size2 < 0))
    bigger = -bigger;

  while (ntodo > 0) {
    ndo = ntodo <= steplimit ? ntodo : steplimit;
    if (err = fseek(fp, ndo * bigger, flag))
      return err;
    ntodo -= ndo;
    flag = SEEK_CUR;
  }
  return 0;
}
 
/************************************************************************
Private undefines
*************************************************************************/
#undef BLOCKIO_C
 
    
 
