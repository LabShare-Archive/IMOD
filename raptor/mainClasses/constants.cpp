/*
 * constants.cpp - global constants for the program
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */

#include "constants.h"


string getDate(void) //simple rutine to get date and time
{
   time_t rawtime;
    struct tm * timeinfo;
    char buffer [80];

  time ( &rawtime );
  timeinfo = localtime ( &rawtime );



    strftime(buffer, 80, "%c", timeinfo);


   return std::string(buffer);
}
