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
** FILENAME:    svlConstants.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**              Ian Goodfellow <ia3n@cs.stanford.edu>
**
*****************************************************************************/

#pragma once

#define SVL_VERSION   "2.3"
#define SVL_TITLE     "STAIR Vision Library"
#define SVL_COPYRIGHT "Copyright (c) 2007-2009, Stephen Gould"

#define SVL_USAGE_HEADER (SVL_TITLE " (Version: " SVL_VERSION ")\n" SVL_COPYRIGHT "\n")

#define X_COORDINATE 0
#define Y_COORDINATE 1
#define Z_COORDINATE 2

#define SVL_EPSILON (1.0e-6)
#define SVL_DBL_MIN numeric_limits<double>::min()
#define SVL_DBL_MAX numeric_limits<double>::max()
#define SVL_FLT_MIN numeric_limits<float>::min()
#define SVL_FLT_MAX numeric_limits<float>::max()
#define SVL_INT_MIN numeric_limits<int>::min()
#define SVL_INT_MAX numeric_limits<int>::max()

#define SVL_NOT_IMPLEMENTED { \
    std::cerr << "ERROR: function not implemented (yet)" << std::endl; \
    assert(false); \
}

