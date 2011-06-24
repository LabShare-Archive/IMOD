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
** FILENAME:    svlLoopTimer.h
** AUTHOR(S):   Paul Baumstarck <pbaumstarck@stanford.edu>
** DESCRIPTION:
**  Timing routines for loops and nested loops providing estimated time to
**  completion.
**
*****************************************************************************/

#pragma once

#include <cmath>
#include <ctime>
#include <vector>
#include <iostream>
using namespace std;

enum svlLoopTimerRatio { GEOMETRIC, ADDITIVE };

struct svlLoopTimerUnit {
	long tic, last_inc_toc;
	svlLoopTimerRatio type;
	int i, n, width;
	float ratio;
	svlLoopTimerUnit(long tic, svlLoopTimerRatio type, int n, float ratio)
		: tic(tic), last_inc_toc(0), type(type), i(0), n(n), ratio(ratio) {
		// Find display width of max number.
        width = (int)log10(abs((double)n) + 1.0) + 1;
	}
	inline void inc() {
		++i;
		last_inc_toc = clock() - tic;
	}
	inline long toc() const {
		return clock() - tic;
	}
};

class svlLoopTimer {
public:
	vector<svlLoopTimerUnit> units;

public:
	svlLoopTimer() {}
	virtual ~svlLoopTimer() {}

	// Push a new timer onto the stack.
	void push(int n = 10, float ratio = 1.0f, svlLoopTimerRatio type = GEOMETRIC) {
		units.push_back( svlLoopTimerUnit(clock(), type, n, ratio) );
	}
	// Remove the top timer.
	void pop() {
		units.pop_back();
	}
	// Remove all timers.
	void clear() {
		units.clear();
	}
	// Increment loop counter of top timer.
	void inc() {
		units.back().inc();
	}
	// Get time elapsed by top timer.
	long toc() {
		return units.back().toc();
	}

	// Pretty print elapsed time.
	void print(ostream &os = cout);
	// Print a time provided in seconds.
	static void printTime(long s, ostream &os = cout);
	// Print a time with fractional component provided in milliseconds.
	static void printMsTime(long ms, ostream &os = cout);
	// Get expected time to completion for one time.
	static long getETC(int i, int n, long toc, float ratio, svlLoopTimerRatio type);
	// Ibid.
	static long getETC(svlLoopTimerUnit &t);
	// Print elapsed time and estimated time to completion based on progress i/n.
	void printETC(ostream &os = cout);
};


