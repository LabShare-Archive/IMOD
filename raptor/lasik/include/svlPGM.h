/*****************************************************************************
** STAIR VISION LIBRARY
** Copyright (c) 2007-2009, Stephen Gould
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are met:
**     * Redistributions of source code must retain the above copyright
**       notice, this list of conditions and the following disclaimer.
**     * Redistributions in binary form must reproduce the above copyright
**       notice, this list of conditions and the following disclaimer in the
**       documentation and/or other materials provided with the distribution.
**     * Neither the name of the Stanford University nor the
**       names of its contributors may be used to endorse or promote products
**       derived from this software without specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
** EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
** WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
** DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
** DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
** (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
** LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
** ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
******************************************************************************
** FILENAME:    svlPGM.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**  Include file for probabilistic graphical models library.
*****************************************************************************/

#pragma once

#include "../svl/lib/pgm/svlAlphabetSOUP.h"
#include "../svl/lib/pgm/svlClusterGraph.h"
#include "../svl/lib/pgm/svlDualDecomposition.h"
#include "../svl/lib/pgm/svlFactor.h"
#include "../svl/lib/pgm/svlFactorOperations.h"
#include "../svl/lib/pgm/svlFactorTemplate.h"
#include "../svl/lib/pgm/svlGeneralCRF.h"
#include "../svl/lib/pgm/svlGraphCuts.h"
#include "../svl/lib/pgm/svlGraphUtils.h"
#include "../svl/lib/pgm/svlMessagePassing.h"
#include "../svl/lib/pgm/svlPairwiseCRF.h"

