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
** FILENAME:    svlLogger.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**              Ian Goodfellow <ia3n@stanford.edu>
** 
*****************************************************************************/

#include <ctime>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <cassert>

#include "svlCompatibility.h"
#include "svlConfigManager.h"
#include "svlLogger.h"

using namespace std;

// logging callbacks
void (*svlLogger::showFatalCallback)(const char *message) = NULL;
void (*svlLogger::showErrorCallback)(const char *message) = NULL;
void (*svlLogger::showWarningCallback)(const char *message) = NULL;
void (*svlLogger::showMessageCallback)(const char *message) = NULL;

ofstream svlLogger::_log;
svlLogLevel svlLogger::_logLevel = SVL_LOG_MESSAGE;

svlLogger::svlLogger() 
{
    // do nothing
}

svlLogger::~svlLogger()
{
    // do nothing
}

void svlLogger::initialize(const char *filename, bool bOverwrite)
{
    if (_log.is_open()) {
        _log.close();
    }

    if ((filename != NULL) && (strlen(filename) > 0)) {
        _log.open(filename, bOverwrite ? ios_base::out : ios_base::out | ios_base::app);
        assert(!_log.fail());

        time_t t = time(NULL);
        struct tm *lt = localtime(&t);

        _log << "-" << setw(2) << setfill('0') << lt->tm_hour
             << ":" << setw(2) << setfill('0') << lt->tm_min 
             << ":" << setw(2) << setfill('0') << lt->tm_sec
             << "--- log opened --- \n";
    }    
}

void svlLogger::initialize(const char *filename,
    bool bOverwrite, svlLogLevel level)
{
    _logLevel = level;
    initialize(filename, bOverwrite);
}

void svlLogger::logMessage(svlLogLevel level, const string& msg)
{
    if (level > _logLevel) return;

    char prefix[4] = "---";
    switch (level) {
      case SVL_LOG_FATAL:   prefix[1] = '*'; break;
      case SVL_LOG_ERROR:   prefix[1] = 'E'; break;
      case SVL_LOG_WARNING: prefix[1] = 'W'; break;
      case SVL_LOG_MESSAGE: prefix[1] = '-'; break;
      case SVL_LOG_VERBOSE: prefix[1] = '-'; break;
      case SVL_LOG_DEBUG:   prefix[1] = 'D'; break;
    }

    if (_log.is_open()) {
        time_t t = time(NULL);
        struct tm *lt = localtime(&t);

        _log << "-" << setw(2) << setfill('0') << lt->tm_hour
             << ":" << setw(2) << setfill('0') << lt->tm_min 
             << ":" << setw(2) << setfill('0') << lt->tm_sec
             << prefix << ' ' << msg << "\n";
    }

    switch (level) {
    case SVL_LOG_FATAL:
        if (showFatalCallback == NULL) {
            cerr << prefix << ' ' << msg << endl;
        } else {
            showFatalCallback(msg.c_str());
        }
        break;
    case SVL_LOG_ERROR:
        if (showErrorCallback == NULL) {
            cerr << prefix << ' ' << msg << endl;
        } else {
            showErrorCallback(msg.c_str());
        }
        break;
    case SVL_LOG_WARNING:
        if (showWarningCallback == NULL) {
            cerr << prefix << ' ' << msg << endl;
        } else {
            showWarningCallback(msg.c_str());
        }
        break;
    default:
        if (showMessageCallback == NULL) {
            cout << prefix << ' ' << msg << endl;
        } else {
            showMessageCallback(msg.c_str());
        }
    }

    if (level == SVL_LOG_FATAL) {
        abort();
    }
}

// configuration --------------------------------------------------------

class svlLoggerConfig : public svlConfigurableModule {
public:
    svlLoggerConfig() : svlConfigurableModule("svlBase.svlLogger") { }
    ~svlLoggerConfig() { }

    void usage(ostream &os) const {
        os << "      logLevel     :: verbosity level. Can be one of (ERROR, WARNING,\n"
           << "                      MESSAGE (default), VERBOSE, or DEBUG)\n"
           << "      logFile      :: name of file for logging message\n";
    }

    void setConfiguration(const char *name, const char *value) {
        // log level
        if (!strcmp(name, "logLevel")) {
            if (!strcasecmp(value, "ERROR")) {
                svlLogger::setLogLevel(SVL_LOG_ERROR);
            } else if (!strcasecmp(value, "WARNING")) {
                svlLogger::setLogLevel(SVL_LOG_WARNING);
            } else if (!strcasecmp(value, "MESSAGE")) {
                svlLogger::setLogLevel(SVL_LOG_MESSAGE);
            } else if (!strcasecmp(value, "VERBOSE")) {
                svlLogger::setLogLevel(SVL_LOG_VERBOSE);
            } else if (!strcasecmp(value, "DEBUG")) {
                svlLogger::setLogLevel(SVL_LOG_DEBUG);
            } else {
                SVL_LOG(SVL_LOG_FATAL, "invalid configuration value for logLevel");
            }

        // log file
        } else if (!strcmp(name, "logFile")) {
            svlLogger::initialize(value);

        } else {
            SVL_LOG(SVL_LOG_FATAL, "unknown configuration option " << name);
        }
    }
};

static svlLoggerConfig gLoggerConfig;
