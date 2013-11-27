/* 
 *  $Id$
 */
#include "parse_params.h"
#include <stdlib.h>
#include <stdio.h>

/* Simple program to convert an option file to a man page segment */
/* First argument = type of output                                */
/* Second argument is # of directories to go up to look for option files */
/* Third argument is program name */

int main(int argc , char **argv)
{
  int mantype, local, ierr;
  
  if (argc == 1) {
    fprintf(stderr, "Usage: autodoc2man output_type directories_up program_name\n"
            "   type = -3 for fallback Fortran option code (f90 free format)\n"
            "          -2 for fallback Fortran option code (f77 fixed format)\n"
            "          -1 for cat output (.1)\n"
            "           0 for usage output\n"
            "           1 for man output (.man)\n"
            "           2 for fallback C option code\n"
            "           3 for fallback Python option code\n"
            "   directories_up = number of ../ to find autodoc directory,\n"
            "                    or 0 to use $IMOD_DIR/autodoc\n");
    exit(1);
  }

  if (argc < 4) {
    fprintf(stderr, "autodoc2man: not enough arguments\n");
    exit(1);
  }

  mantype = atoi(argv[1]);
  local = atoi(argv[2]);
  PipExitOnError(1, "ERROR: AUTODOC2MAN - ");
  PipSetManpageOutput(mantype);
  PipReadOptionFile(argv[3], mantype ? 3 : 1, local);
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
