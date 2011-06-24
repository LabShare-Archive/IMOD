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
** FILENAME:    svlCodeProfiler.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**
*****************************************************************************/

#include <vector>
#include <map>
#include <iostream>
#include <iomanip>
#include <cmath>

#include "svlCompatibility.h"
#include "svlLogger.h"
#include "svlConfigManager.h"
#include "svlCodeProfiler.h"

using namespace std;

// svlCodeProfiler -----------------------------------------------------------

bool svlCodeProfiler::enabled = false;

vector<svlCodeProfiler::svlCodeProfilerEntry> svlCodeProfiler::_entries;
map<string, int> svlCodeProfiler::_names;

int svlCodeProfiler::getHandle(const char *name)
{
    if (!enabled) return -1;

    // if entry exists return it
    map<string, int>::const_iterator i;
    if ((i = _names.find(string(name))) != _names.end()) {
        return i->second;
    }

    // otherwise create a new entry for profiling
    int handle = (int)_entries.size();
    _names[string(name)] = handle;
    _entries.push_back(svlCodeProfiler::svlCodeProfilerEntry());

    return handle;
}

void svlCodeProfiler::print(ostream &os)
{
    if (!enabled || _entries.empty())
        return;

    os << "  CALLS        CPU TIME   WALL TIME      TIME PER   FUNCTION\n";
    for (map<string, int>::const_iterator it = _names.begin(); it != _names.end(); it++) {
	int handle = it->second;
	os << setw(7) << setfill(' ') << _entries[handle].totalCalls << "   ";

	// CPU time
	double mseconds = 1000.0 * (double)_entries[handle].totalClock / (double)CLOCKS_PER_SEC;
	int seconds = (int)floor(mseconds / 1000.0);
	mseconds -= 1000.0 * seconds;
	int minutes = (int)floor(seconds / 60.0);
	seconds -= 60 * minutes;
	int hours = (int)floor(minutes / 60.0);
	minutes -= 60 * hours;
	os << setw(3) << setfill(' ') << hours << ":"
	   << setw(2) << setfill('0') << minutes << ":"
	   << setw(2) << setfill('0') << seconds << "."
	   << setw(3) << setfill('0') << (int)mseconds << "   ";

	// Calendar time
	seconds =  (int) _entries[handle].totalTime;
	minutes = (int)floor(seconds / 60.0);
	seconds -= 60 * minutes;
	hours = (int)floor(minutes / 60.0);
	minutes -= 60 * hours;
	os << setw(3) << setfill(' ') << hours << ":"
	   << setw(2) << setfill('0') << minutes << ":"
	   << setw(2) << setfill('0') << seconds << "   ";

	// Time per method
	os << " ";
	if ( _entries[handle].totalCalls == 0 ) {
		os << "      0  s   ";
	} else {
		float time_per_s = (float)_entries[handle].totalClock / (float)CLOCKS_PER_SEC / float(_entries[handle].totalCalls);
		if ( time_per_s < 1e-6 ) {
			// Print in nanoseconds.
			os << setw(3) << setfill(' ') << int(1e9*time_per_s) << "."
			   << setw(3) << setfill('0') << int(1e12*time_per_s)%1000 << " ns   ";
		} else if ( time_per_s < 1e-3 ) {
			// Print in microseconds.
			os << setw(3) << setfill(' ') << int(1e6*time_per_s) << "."
			   << setw(3) << setfill('0') << int(1e9*time_per_s)%1000 << " us   ";
		} else if ( time_per_s < 1.0 ) {
			// Print in milliseconds.
			os << setw(3) << setfill(' ') << int(1e3*time_per_s) << "."
			   << setw(3) << setfill('0') << int(1e6*time_per_s)%1000 << " ms   ";
		} else if ( time_per_s < 1e3 ) {
			// Print in seconds.
			os << setw(3) << setfill(' ') << int(time_per_s) << "."
			   << setw(3) << setfill('0') << int(1e3*time_per_s)%1000 << "  s   ";
		} else {
			// Print saturated.
			os << "   >999  s   ";
		}
	}

	os << it->first.c_str() << endl;
    }
}

// configuration --------------------------------------------------------

class svlCodeProfilerConfig : public svlConfigurableModule {
public:
    svlCodeProfilerConfig() : svlConfigurableModule("svlBase.svlCodeProfiler") { }
    ~svlCodeProfilerConfig() { }

    void usage(ostream &os) const {
        os << "      enabled      :: enable profiling (default: false)\n";
    }

    void setConfiguration(const char *name, const char *value) {
        if (!strcmp(name, "enabled")) {
            svlCodeProfiler::enabled = 
                (!strcasecmp(value, "TRUE") || !strcmp(value, "1"));                
        } else {
            SVL_LOG(SVL_LOG_FATAL, "unrecognized configuration option for " << this->name());
        }
    }
};

static svlCodeProfilerConfig gCodeProfilerConfig;

