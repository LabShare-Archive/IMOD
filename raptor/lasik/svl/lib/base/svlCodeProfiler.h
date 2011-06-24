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
** FILENAME:    svlCodeProfiler.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
** Class for providing profile information on functions. Typically used to
** accumulate information on functions (or within routines). Wrap function
** or code block in
**      svlCodeProfiler::tic(handle = getHandle("functionName"));
**      svlCodeProfiler::toc(handle);
** to accumulate timing and number of calls for a given function. The class
** must be enabled in main() with
**      svlCodeProfiler::enabled = true;
** This class is not accurate for small functions. In those cases you are
** better off using the gcc -pg option.
**
** The code can also be used to setting time limits or recursive call limits
** within instrumented functions. Use time(handle) and calls(handle) to get
** the total running time or total number of calls for a given handle.
**
*****************************************************************************/

#pragma once

#include <ctime>
#include <vector>
#include <map>
#include <string>
#include <iostream>

// macro for function tic/toc
#define SVL_FCN_TIC {svlCodeProfiler::tic(svlCodeProfiler::getHandle(__PRETTY_FUNCTION__));}
#define SVL_FCN_TOC {svlCodeProfiler::toc(svlCodeProfiler::getHandle(__PRETTY_FUNCTION__));}

// profiling class
class svlCodeProfiler {
public:
    static bool enabled;

private:
    class svlCodeProfilerEntry {
    public:
        clock_t startClock;
	time_t startTime;
	unsigned long totalClock;
        double totalTime;
        int totalCalls;

        svlCodeProfilerEntry() { clear(); };
        ~svlCodeProfilerEntry() { /* do nothing */ };

        inline void clear() {
            startClock = clock();
            totalClock = 0L;

	    startTime = std::time(NULL);
	    totalTime = 0L;

            totalCalls = 0;
        }
        inline void tic() {
            startClock = clock();
	    startTime = std::time(NULL);
        }
        inline void toc() {
            clock_t endClock = clock();
	    time_t endTime;
	    endTime = std::time(NULL);

	    if (endClock >= startClock) {
		totalClock += (endClock - startClock);		
	    } else {
		totalClock += ((clock_t)(-1) - startClock) + endClock;
	    }
            startClock = endClock;
 
	    totalTime += difftime(endTime, startTime);
	    
            totalCalls += 1;
        }
    };
    
    static std::vector<svlCodeProfilerEntry> _entries;
    static std::map<std::string, int> _names;

public:
    svlCodeProfiler() { /* do nothing */ }
    ~svlCodeProfiler() { /* do nothing */ }

    static int getHandle(const char *name);

    static inline void clear(int handle) {
        if (enabled) _entries[handle].clear();
    }
    static inline void tic(int handle) {
        if (enabled) _entries[handle].tic();
    }
    static inline void toc(int handle) {
        if (enabled) _entries[handle].toc();
    }
    static inline double calendarSeconds(int handle) {
        if (!enabled) return -1.0;
	return _entries[handle].totalTime;
    }
    static inline double time(int handle) {
	if (!enabled) return -1.0;
	return (double)_entries[handle].totalClock / (double)CLOCKS_PER_SEC;
    }
    static inline int calls(int handle) {
	if (!enabled) return -1;
	return _entries[handle].totalCalls;
    }

    static void print(std::ostream& os);
};

