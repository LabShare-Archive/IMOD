/*****************************************************************************
 *                                                                           *
 *   FILE: test1.c                                                           *
 *                                                                           *
 *   PURPOSE: Test dia library.                                              *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0  James Kremer  kremer@beagle.colorado.edu               *
 *                                                                           *
 *****************************************************************************
 *   Copyright (C) 1994,1995 by Boulder Laboratory for 3-Dimensional Fine    *
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
#include "dia.h"


char *test_help[] = {
     "here is a test help message",
     "It is multi lines of text in",
     "a scrolled window",
     NULL
};


main (int argc, char **argv)
{
     int c;

     char *s = NULL;

     dia_init(&argc, argv);

     dia_vasmsg("here is a test help message.",
		"It is multi lines of text in",
		"a scrolled window",
		"This is the var args version.",
		NULL);

     dia_smsg(test_help);

     
     s = dia_gets("Edit me", "String Editor");
     if (s)
	  printf("String is %s\n", s);
     else
	  printf("String is NULL\n");

     c = dia_ask("Here is a question");

     if (c)
	  dia_puts("You said YES");
     else
	  dia_puts("You said NO");

     c = dia_ask("This is a holder");


}
