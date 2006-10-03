/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.4  2006/06/08 03:12:51  mast
    Added stubs and new type of output

    Revision 3.3  2003/10/25 02:53:30  mast
    add a ;

    Revision 3.2  2003/10/24 19:50:06  mast
    Remove unused argument in usage statement

    Revision 3.1  2003/10/08 17:23:25  mast
    Addition to repository

*/
#include "parse_params.h"
#include <stdlib.h>
#include <stdio.h>

/* Simple program to convert an option file to a man page segment */
/* First argument = -1 for cat output (.1), 1 for .man output */
/* Second argument is # of directories to go up to look for option files */
/* Third argument is program name */

int main(int argc , char **argv)
{
  int mantype, local, ierr;
  
  if (argc == 1) {
    fprintf(stderr, "Usage: autodoc2man output_type directories_up "
            "program_name\n"
            "   type = -2 for fallback Fortran option code\n"
            "          -1 for cat output (.1)\n"
            "           1 for man output (.man)\n"
            "           2 for fallback C option code\n"
            "           3 for fallback Python option code\n"
            "   directories_up = number of ../ to find autodoc directory\n");
    exit(1);
  }

  if (argc < 4) {
    fprintf(stderr, "autodoc2man: not enough arguments\n");
    exit(1);
  }

  mantype = atoi(argv[1]);
  local = atoi(argv[2]);
  PipExitOnError(1, "ERROR: AUTODOC2MAN - ");
  PipReadOptionFile(argv[3], 3, local);
  PipSetManpageOutput(mantype);
  PipPrintHelp(argv[3], 0, 0, 0);
  return 0;
}

/* Stubs to avoid having to link with libimod */
int imodVersion(char *pname)
{
  return 0;
}
void imodCopyright(void)
{
}
