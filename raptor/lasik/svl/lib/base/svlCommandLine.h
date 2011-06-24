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
** FILENAME:    svlCommandLine.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION: Command line processing macros. Applications should use these
**  macros to present a consistent interface. The following is an example usage
**
**  int main(int argc, char* argv[])
**  {
**     int integerVariable = 0;
**     const char *stringVariable = NULL;
**
**     SVL_BEGIN_CMDLINE_PROCESSING(argc, argv)
**         SVL_CMDLINE_INT_OPTION("-intgerOption", integerVariable)
**         SVL_CMDLINE_STR_OPTION("-stringOption", stringVariable)
**         SVL_CMDLINE_OPTION_BEGIN("-longOption", p)
**             cerr << p[0] << "\n";
**             cerr << p[1] << "\n";
**         SVL_CMDLINE_OPTION_END(2)
**     SVL_END_CMDLINE_PROCESSING();
**
**     return 0;
**  }
**
*****************************************************************************/

#pragma once

#define SVL_STANDARD_OPTIONS_USAGE                            \
    "  -help             :: display application usage\n"      \
    "  -config <xml>     :: configure SVL from XML file\n"    \
    "  -set <m> <n> <v>  :: set (configuration) <m>::<n> to value <v>\n" \
    "  -profile          :: profile code\n"                   \
    "  -quiet            :: only show warnings and errors\n"  \
    "  -verbose          :: show verbose messages\n"          \
    "  -debug            :: show debug messages\n"            \
    "  -log <filename>   :: log filename\n"                   \
    "  -threads <max>    :: set maximum number of threads\n"

#define SVL_PROCESS_STANDARD_OPTIONS(ARGS, ARGC)              \
    if (!strcmp(*ARGS, "-config")) {                          \
        if (ARGC == 1) {                                      \
            svlConfigurationManager::get().showRegistry();    \
            return 0;                                         \
        }                                                     \
        svlConfigurationManager::get().configure(*(++ARGS));  \
        ARGC -= 1;                                            \
    } else if (!strcmp(*ARGS, "-set")) {                      \
        SVL_ASSERT_MSG(ARGC > 3, "not enough arguments for -set"); \
        svlConfigurationManager::get().configure(ARGS[1], ARGS[2], ARGS[3]); \
        ARGC -= 3; ARGS += 3;                                 \
    } else if (!strcmp(*ARGS, "-profile")) {                  \
        svlCodeProfiler::enabled = true;                      \
    } else if (!strcmp(*ARGS, "-quiet")) {                    \
        svlLogger::setLogLevel(SVL_LOG_WARNING);              \
    } else if (!strcmp(*ARGS, "-verbose") || !strcmp(*ARGS, "-v")) { \
        if (svlLogger::getLogLevel() < SVL_LOG_VERBOSE)       \
            svlLogger::setLogLevel(SVL_LOG_VERBOSE);          \
    } else if (!strcmp(*ARGS, "-debug")) {                    \
        svlLogger::setLogLevel(SVL_LOG_DEBUG);                \
    } else if (!strcmp(*ARGS, "-log")) {                      \
        svlLogger::initialize(*(++ARGS), true);               \
        ARGC -= 1;                                            \
    } else if (!strcmp(*ARGS, "-threads")) {                  \
        svlThreadPool::MAX_THREADS = atoi(*(++ARGS));         \
        ARGC -= 1;                                            \
    }

#define SVL_BEGIN_CMDLINE_PROCESSING(ARGC, ARGV)              \
    char **_svl_args = ARGV + 1;                              \
    int _svl_argc = ARGC;                                     \
    while (--_svl_argc > 0) {                                 \
        SVL_LOG(SVL_LOG_DEBUG, "processing command line argument " << *_svl_args); \
        SVL_PROCESS_STANDARD_OPTIONS(_svl_args, _svl_argc)    \

#define SVL_CMDLINE_STR_OPTION(OPTSTR, VAR)                   \
    else if (!strcmp(*_svl_args, OPTSTR)) {                   \
        VAR = *(++_svl_args); _svl_argc -= 1; }

#define SVL_CMDLINE_INT_OPTION(OPTSTR, VAR)                   \
    else if (!strcmp(*_svl_args, OPTSTR)) {                   \
        VAR = atoi(*(++_svl_args)); _svl_argc -= 1; }

#define SVL_CMDLINE_REAL_OPTION(OPTSTR, VAR)                  \
    else if (!strcmp(*_svl_args, OPTSTR)) {                   \
        VAR = atof(*(++_svl_args)); _svl_argc -= 1; }

#define SVL_CMDLINE_BOOL_OPTION(OPTSTR, VAR)                  \
    else if (!strcmp(*_svl_args, OPTSTR)) { VAR = true; }

#define SVL_CMDLINE_BOOL_TOGGLE_OPTION(OPTSTR, VAR)           \
    else if (!strcmp(*_svl_args, OPTSTR)) { VAR = !VAR; }

#define SVL_CMDLINE_VEC_OPTION(OPTSTR, VAR)                   \
  else if (!strcmp(*_svl_args, OPTSTR)) {                     \
    VAR.push_back(*(++_svl_args)); _svl_argc -= 1; }

#define SVL_CMDLINE_OPTION_BEGIN(OPTSTR, PTR)                 \
    else if (!strcmp(*_svl_args, OPTSTR)) {                   \
        const char **PTR = (const char **)(_svl_args + 1);

#define SVL_CMDLINE_OPTION_END(N)                             \
        _svl_args += (N); _svl_argc -= (N); }

#define SVL_CMDLINE_FLAG_BEGIN(OPTSTR)                        \
    else if (!strcmp(*_svl_args, OPTSTR)) {

#define SVL_CMDLINE_FLAG_END }

#define SVL_END_CMDLINE_PROCESSING(USAGE)                     \
        else if (!strcmp(*_svl_args, "-help")) {              \
            USAGE;                                            \
            svlConfigurationManager::get().showRegistry();    \
            return 0;                                         \
        } else if ((*_svl_args)[0] == '-') {                  \
            USAGE;                                            \
	    SVL_LOG(SVL_LOG_ERROR, "unrecognized option " << *_svl_args); \
	    return -1;                                        \
        } else {                                              \
            break;                                            \
        }                                                     \
        _svl_args++;                                          \
    }


#define SVL_OPTION_DEPRECATED(ORIG, INSTEAD)		 \
  SVL_LOG(SVL_LOG_WARNING, "Deprecated option " << string(ORIG)	\
	  << "; use \"" << string(INSTEAD) << "\" instead")

#define SVL_CMDLINE_ARGV _svl_args
#define SVL_CMDLINE_ARGC _svl_argc
