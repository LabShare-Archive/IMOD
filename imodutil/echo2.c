/* Simple program to echo to standard error.
   From Anderson and Anderson, The UNNIX C shell Filed Guide */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
int main(int argc,char *argv[])
{
  short nl = 1;
  if (argc <= 1)
    exit(0);
  if (strcmp(argv[1],"-n") == 0) {
    nl = 0;
    argc--;
    argv++;
  }
  while (--argc) {
    fputs(*++argv,stderr);
      if (argc > 1)
	putc(' ',stderr);
  }
  if(nl)
    putc('\n',stderr);
  exit(0);
}
