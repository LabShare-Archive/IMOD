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
 * $Id$
 * Log at end
 *****************************************************************************/
 
#define BLOCKIO_C    /*define source name for includes*/
 
/******************************************************************************
Include Files
******************************************************************************/
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>   /* JRK: added for strcmp */
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "environ.h"
#include "b3dutil.h"

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

/* DNM 9/27/06: eliminated num_char_per_item and the modes.  These functions
   work only in terms of bytes and callers assumed that */
 
typedef struct
{
  int  being_used;
  char fname[326];
  FILE *fp;
  int  read_only;
  int  write_only;
  unsigned int  pos;
  int  attribute;  /* JRK: keep track of attibutes for files. */
  char *tailName;  /* Pointer to filename only in fname */
} Unit;
 
/******************************************************************************
Private Function Prototypes
******************************************************************************/

static void mybcopy(register char *a,register char *b,int n);
static int fcmp();
static int find_unit();
static void get_fstr();
static void set_fstr();
static Unit *check_unit(int iunit, char *function, int doExit);
 
/******************************************************************************
Private Global Declarations
******************************************************************************/
 
static Unit units[MAX_UNIT];

/******************************************************************************
 ***************************    PUBLIC FUNCTIONS    ****************************
 ******************************************************************************/

/* DNM 6/14/01: rewrite to use fopen instead of open, etc.  Also so that 
   F77STRING version just sets up the variables and uses the rest of the code
   the same.  Not tested, of course */
/* DNM: make this F77STRING instead of ST3000 */
#ifdef F77STRING

void qopen(int *iunit, strDesc_s *f77name, strDesc_s *f77attribute)
#else

void qopen(int *iunit, char *name, char *attribute, int name_l, int attr_l)
#endif     
{
#ifdef F77STRING
  char *name = f77name->str;
  char *attribute = f77attribute->str;
  int name_l = f77name->len;
  int attr_l = f77attribute->len;
#endif     

  int unit = find_unit();
  Unit *u = units + unit;  /* JRK: Style, declarations should be up here. */
  int  noChars;
  char oldfilename[327];
  char matstr[16];
  int mode, errSave;
  char *modes[4] = {"rb", "rb+", "wb", "wb+"};
  char *tailback;
  struct stat buf;

  if (unit >= 0) {
    u->being_used = 1;
    get_fstr(name, name_l, u->fname, sizeof u->fname);
    u->write_only = 0;
    u->read_only = 0;
    u->pos = 0;
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
      if (!getenv("IMOD_NO_IMAGE_BACKUP") && !stat(oldfilename, &buf)) {
        oldfilename[strlen(oldfilename) + 1] = 0x00;
        oldfilename[strlen(oldfilename)] = '~';
        remove(oldfilename);
        errno = 0;
        if (rename(u->fname, oldfilename)) {
          errSave = errno;
          fprintf(stdout, "\nWARNING: qopen - Could not rename '%s' to '%s'"
                  "\n", u->fname, oldfilename);
          if (errSave)
            fprintf(stdout, "WARNING: from system - %s\n", strerror(errSave));
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

    errno = 0;
    u->fp = fopen(u->fname, modes[mode]);
    if (u->fp == NULL) {
      fprintf(stdout, "\nERROR: qopen - Could not open '%s'\n" , u->fname);
      errSave = errno;
      if (errSave)
        fprintf(stdout, "ERROR: from system - %s\n", strerror(errSave));
      exit(3);
    }
    *iunit = unit + 1;

    /* Get the tail of the filename for other error messages */
    u->tailName = strrchr(u->fname, '/');
    tailback = strrchr(u->fname, '\\');
    if (tailback > u->tailName)
      u->tailName = tailback;
    if (!u->tailName)
      u->tailName = &u->fname[0];
    else
      u->tailName++;

  } else {
    *iunit = -1;
  }
}


void qclose(int *iunit)
{
  int unit = *iunit - 1;
  if (unit >= 0 && unit < MAX_UNIT) {
    Unit *u = units + unit;

    if (u->being_used) {
      u->being_used = 0;
      fclose(u->fp);

      /* JRK: Delete scratch files */
      /* DNM 2/10/05: switch from unlink to remove to avoid unistd.h */
      if (u->attribute == UNIT_ATBUT_SCRATCH)
        remove(u->fname);
    }
  }
}

void qread(int *iunit, char *array, int *nitems, int *ier)
{
  int unit = *iunit - 1;
  Unit *u = check_unit(unit, "qread", 0);
  if (u) {
    int bc = *nitems;
    if (u->write_only) {
      fprintf(stdout, "\nERROR: qread - '%s' is write only.\n", 
              u->tailName);
      exit(3);
    }
    errno = 0;
    if (b3dFread(array, 1, bc, u->fp) != bc) {
      unit = errno;
      fprintf(stdout, "\nERROR: qread - reading '%s'\n", u->tailName);
      if (unit)
        fprintf(stdout, "ERROR: from system - %s\n", strerror(unit));
      exit(3);
    }
    u->pos += bc;
    *ier = 0;
    
  } else {
    *ier = -1;
  }
}
 
void qwrite(int *iunit, char *array, int *nitems)
{
  int unit = *iunit - 1;
  
  Unit *u = check_unit(unit, "qwrite", 1);
  int bc = *nitems;
  if (u->read_only) {
    fprintf(stdout, "\nERROR: qwrite - '%s' is read only.\n", u->tailName);
    exit(3);
  }

  errno = 0;
  if (b3dFwrite(array, 1, bc, u->fp) != bc) {
    unit = errno;
    fprintf(stdout, "\nERROR: qwrite - writing '%s'\n", u->tailName);
    if (unit)
      fprintf(stdout, "ERROR: from system - %s\n", strerror(unit));
    exit(3);
  }
  u->pos += bc;
}

  
 /* DNM 10/23/00: switch from using system-dependent "seek_name" to call a
    big_seek function that seeks in chunks less than 2 GB; also change test
    for error to test for -1 returned rather than a negative number.
    Change to test for nonzero when switch to fseek */
void qseek(int *iunit, int *base, int *line, int *section, int *nxbytes, 
           int *nylines)
{
  int unit = *iunit - 1;
  Unit *u = check_unit(unit, "qseek", 1);
  u->pos = ((unsigned int)(*nxbytes) * 
            (unsigned int)((*line - 1) + (unsigned int)*nylines * 
                           (unsigned int)(*section - 1)) +
            (unsigned int)(*base - 1));
 
  /*  if (lseek(u->fp, u->pos = pos, 0) < 0) */
  errno = 0;
  if (mrcHugeSeek(u->fp, *base - 1, 0, *line - 1, *section - 1, *nxbytes,
                  *nylines, 1, SEEK_SET)) {
    unit = errno;
    fprintf(stdout, "\nERROR: qseek - Doing mrcHugeSeek in '%s'\n", 
            u->tailName);
    if (unit)
      fprintf(stdout, "ERROR: from system - %s\n", strerror(unit));
    exit(3);
  }
}

/* qback is used only for small movements, within a section, so
   it doesn't need to call big_seek.  However, change the test for error to
   test for = -1 instead of < 0; then to test for !=0 when switch to fseek */  
void qback(int *iunit, int *ireclength)
{
  int unit = *iunit - 1;
  Unit *u = check_unit(unit, "qback", 1);
  int amt = -(*ireclength);
  u->pos += amt;
  errno = 0;
  if (b3dFseek(u->fp, amt, SEEK_CUR)) {
    unit = errno;
    fprintf(stdout, "\nERROR: qback - Doing seek in '%s'\n", u->tailName);
    if (unit)
      fprintf(stdout, "ERROR: from system - %s\n", strerror(unit));
    exit(3);
  }
}
 
/* qskip needs to move by large amounts within sections so it takes two numbers
   and moves by the product */ 
void qskip(int *iunit, int *ireclength, int *nrecords)
{
  int unit = *iunit - 1;
  Unit *u = check_unit(unit, "qskip", 1);
  u->pos += *ireclength * *nrecords;
  errno = 0;
  if (mrc_big_seek(u->fp, 0, *ireclength, *nrecords, SEEK_CUR)) {
    unit = errno;
    fprintf(stdout, "\nERROR: qskip - Doing seek in '%s'\n", u->tailName);
    if (unit)
      fprintf(stdout, "ERROR: from system - %s\n", strerror(unit));
    exit(3);
  }
}

 /* DNM 8/13/00: added the version for VMS for completeness (not tested), and
    added a declaration for filename_l */
#ifdef F77STRING

void qinquire(int *iunit, strDesc_s *f77str, int *flen)
#else 

void qinquire(int *iunit, char *filename, int *flen, int filename_l)
#endif
{
#ifdef F77STRING
  char *filename = f77str->str;
  int filename_l = f77str->len;
#endif
  
  int unit = *iunit - 1;
  Unit *u = check_unit(unit, "qinquire", 0);
  if (u) {

    /* generic stat is not good enough on Windows */
#ifdef _WIN32
    struct _stat64 buf;
    _stat64(u->fname, &buf);
#else
    struct stat buf;
    stat(u->fname, &buf);
#endif
    set_fstr(filename, filename_l, u->fname);
    *flen = (int)((double)buf.st_size / 1024.);
  } else {

      /* DNM: HUH? */
      /* set_fstr(filename, filename_l, ""); */
    *flen = -1;
  }
}
 
 /* This will fail for large files, but try to keep pos good up to 4 GB by 
    making it an unsigned int */
void qlocate(int *iunit, int *location)
{
  int unit = *iunit - 1;
  Unit *u = check_unit(unit, "qlocate", 1);
  *location = u->pos + 1;
}
 
void move(char *a, char *b, int *n)
{
  mybcopy(a, b, *n);
}
 
void zero(char *a, int *n)
{
  memset(a, 0, *n);
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
 ***************************    static FUNCTIONS    ********************
 ************************************************************************/

/* Compares a C and Fortran string */
static int fcmp(char *s, int  l, char *str)
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
/* Creates a c string from a fortran one */

static void get_fstr(char *fstr, int lfstr, char *str, int l)
{
  int i = 0;
  int lnblnk = -1;
  
  /* Keep track of last non blank, non null character and put null after it */
  while (lfstr > 0 && l > 1)
    {
      if (*fstr != ' ' && *fstr)
        lnblnk = i;
      str[i++] = *fstr++;
      lfstr--;
      l--;
    }
  str[lnblnk + 1] = 0;
  
} /*get_fstr*/
 
/************************************************************************/
/* Creates a fortran string from a c one */
static void set_fstr(char *fstr, int lfstr, char *str)
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
 
static int find_unit()
{
  static int firstTime = 1;
  int i;
  
  if (firstTime) 
    for (i = 0; i < MAX_UNIT; i++)
      units[i].being_used = 0;
  firstTime = 0;

  for (i = 0; i < MAX_UNIT; i++) {
    if (!units[i].being_used)
      return i;
  }
  return -1;
}

static  void mybcopy(a, b, n)
     register char *a;
     register char *b;
     register int n;
{
  while(n--)
    {
      *a++ = *b++;
    }
}

/* Checks for legal unit number and whether unit is open, gives error
   message with function name and unit number, exits if doExit set */

static Unit *check_unit(int unit, char *function, int doExit)
{
  Unit *u = units + unit;
  if (unit < 0 || unit >= MAX_UNIT) {
    fprintf(stdout, "\nERROR: %s - %d is not a legal unit number.\n", function,
            unit + 1);
    if (doExit)
      exit(3);
    return NULL;
  }
  if (!u->being_used) {
    fprintf(stdout, "\nERROR: %s - unit %d is not open.\n", function, 
            unit + 1);
    if (doExit)
      exit(3);
    return NULL;
  }
  return u;
}

 
/************************************************************************
Private undefines
*************************************************************************/
#undef BLOCKIO_C
 
/*
$Log$
Revision 3.20  2008/11/21 02:16:27  mast
Increased filename lengths so that 320-character filenames will work

Revision 3.19  2007/10/14 17:10:06  mast
Return file size in kilobytes, not 512-byte blocks

Revision 3.18  2007/06/08 23:24:54  mast
Add tail of filename to various error messages, direct system errors to
standard out with correct prefix, and only if set

Revision 3.17  2006/09/28 21:21:58  mast
Changed qseek and qskip to work with huge images, eliminated the mode array
and number of characters per item variable

Revision 3.16  2005/02/11 01:42:33  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.15  2004/11/08 05:55:30  mast
Backed out atexit, will not compile in AMD64 or work in Windows

Revision 3.13  2004/07/07 19:25:31  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.12  2004/06/15 15:15:50  mast
Had to move declaration out of body of check_unit for SGI

Revision 3.11  2004/06/15 04:42:36  mast
Fixed bugs created by unneeded cosmetic changes!

Revision 3.10  2004/06/14 20:05:58  mast
Added check and error reporting for illegal unit number and unit not open

Revision 3.9  2004/04/24 04:42:40  mast
Rewrote get_fstr to be clearer, but valgrind still complains

Revision 3.8  2004/04/19 18:06:51  mast
Added newlines in front of all ERROR outputs

Revision 3.7  2004/01/17 20:33:26  mast
Changes for 2GB problem on Mac OS 10.3 - switch to calling routines in
b3dutil

Revision 3.6  2003/11/18 19:19:09  mast
changes for 2GB problem on Windows

Revision 3.5  2003/10/24 03:40:33  mast
open file as binary; delete ~ before renaming for Windows/Intel

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
