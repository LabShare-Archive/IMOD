/*   mv_movie.h  -  declarations for mv_movie.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMODV_MOVIE_H
#define IMODV_MOVIE_H

#define IMODV_MOVIE_FULLAXIS_X -1
#define IMODV_MOVIE_FULLAXIS_Y 1

typedef struct __imodv_struct ImodvApp;

void imodvMovieFullAxis(int ixy);
void imodvMovieSetStart();
void imodvMovieSetEnd();
void imodvMovieDialog(ImodvApp *a, int state);
void imodvMovieFullAxis();
void imodvMovieUpdate();
void imodvMovieHelp();
void imodvMovieQuit();
void imodvMovieMake();
void imodvMovieStop();
void imodvMovieHelp();
void imodvMovieClosing();

#endif
