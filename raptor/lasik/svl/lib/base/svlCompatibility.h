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
** FILENAME:    svlCompatibility.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**  Windows/linux compatibility layer.
*****************************************************************************/

#pragma once

// Microsoft Visual Studio (Win32)
#if defined(_WIN32)||defined(WIN32)||defined(__WIN32__)
#pragma warning(disable: 4018) // signed/unsigned mismatch
#pragma warning(disable: 4267) // conversion from size_t to int
#pragma warning(disable: 4244) // conversion from double to float
#define __PRETTY_FUNCTION__ __FUNCTION__
#define _CRT_SECURE_NO_DEPRECATE
#define _USE_MATH_DEFINES
#ifndef NOMINMAX
#define NOMINMAX
#endif
#undef min
#undef max
#define strcasecmp _stricmp
#define uint32_t __int32
#ifndef isnan
#define isnan(x) (_isnan(x))
#endif
#ifndef isinf
#define isinf(x) (!_finite(x))
#endif
#ifndef isfinite
#define isfinite(x) (_finite(x))
#endif
#define drand48() ((double) rand() / (double)(RAND_MAX - 1))
#define round(x) (((x) < 0) ? ceil((x)-0.5) : floor((x)+0.5))
#endif


