/******************************************************************************
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
** FILENAME:    svlLogger.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**              Ian Goodfellow <ia3n@stanford.edu>
** DESCRIPTION:
**  Message and error logging. This class is not thread-safe in the interest
**  of not having to flush the log on every message.
** 
*****************************************************************************/

#pragma once

#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

#define SVL_LOG(L, M) \
  if ((L) > svlLogger::getLogLevel()) { } \
  else { std::stringstream __s; if (L == SVL_LOG_FATAL) { __s << "(" << __FILE__ << ", " << __LINE__ << ") "; } \
      __s << M; svlLogger::logMessage(L, __s.str()); }

#define SVL_ASSERT(C) \
  if (!(C)) { SVL_LOG(SVL_LOG_FATAL, #C); }

#define SVL_ASSERT_MSG(C, M) \
  if (!(C)) { SVL_LOG(SVL_LOG_FATAL, #C << ": " << M); }

typedef enum _svlLogLevel {
    SVL_LOG_FATAL = 0,
    SVL_LOG_ERROR,
    SVL_LOG_WARNING,
    SVL_LOG_MESSAGE,
    SVL_LOG_VERBOSE,
    SVL_LOG_DEBUG
} svlLogLevel;

class svlLogger
{
 public:
    static void (*showFatalCallback)(const char *message);
    static void (*showErrorCallback)(const char *message);
    static void (*showWarningCallback)(const char *message);
    static void (*showMessageCallback)(const char *message);

 private:
    static ofstream _log;
    static svlLogLevel _logLevel;

 public:
    svlLogger();
    ~svlLogger();

    static void initialize(const char *filename,
        bool bOverwrite = false);
    static void initialize(const char *filename,
        bool bOverwrite, svlLogLevel level);
    static inline void setLogLevel(svlLogLevel level) {
        _logLevel = level;
    }
    static inline svlLogLevel getLogLevel() {
        return _logLevel;
    }

    static void logMessage(svlLogLevel level, const string& msg);
};
