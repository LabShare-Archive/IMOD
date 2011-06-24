/*
 * frame.cpp - class to perform operations ine each projection 
 *
 * Copyright (C) 2007-2011 by  Fernando Amat, Farshid Moussavi, Mark Horowitz.
 * See RAPTORlicense.txt for full license and copyright notice.
 *
 * Authors: Fernando Amat, Farshid Moussavi
 */
#include "frame.h"

frame::frame(ioMRC* new_vol, int new_frameID, int new_width, int new_height)
{
  vol = new_vol;
  frameID = new_frameID;
  width = new_width;
  height = new_height;
  discard = false;
}

frame::frame()
{
    discard = false;
}
