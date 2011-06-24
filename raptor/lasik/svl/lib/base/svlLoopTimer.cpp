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
** FILENAME:    svlLoopTimer.cpp
** AUTHOR(S):   Paul Baumstarck <pbaumstarck@stanford.edu>
**
*****************************************************************************/

#include <iomanip>
#include <iostream>
#include "svlLoopTimer.h"

#if defined(_WIN32)||defined(WIN32)||defined(__WIN32__)
#undef min
#undef max
#endif

// *** svlLoopTimer functions ***

void svlLoopTimer::printTime(long s, ostream &os)
{
	long gi = s/3600;
	//os << setw(2) << setfill(' ') << gi << ":";
	os << gi << ":";
	s -= gi*3600;
	gi = s/60;
	os << setw(2) << setfill('0') << gi << ":";
	s -= gi*60;
	os << setw(2) << setfill('0') << s;
}

void svlLoopTimer::printMsTime(long ms, ostream &os)
{
	long gi = ms/3600000;
	//os << setw(2) << setfill(' ') << gi << ":";
	os << gi << ":";
	ms -= gi*3600000;
	gi = ms/60000;
	os << setw(2) << setfill('0') << gi << ":";
	ms -= gi*60000;
	gi = ms/1000;
	os << setw(2) << setfill('0') << gi << ".";
	ms -= gi*1000;
	os << setw(2) << setfill('0') << ms;
}

//void svlLoopTimer::printETC(int i, int n, ostream &os, float ratio, svlLoopTimerRatio type)
//{
//	os << "Elapsed " << setw(4) << setfill(' ') << i << "/" << n << ": ";
//	printTime( long(toc()/CLOCKS_PER_SEC), os);
//	os << "  ETC: ";
//	if ( i == 0 ) {
//		os << "--:--:--";
//	} else if ( type == GEOMETRIC ) {
//		if ( ratio == 1.0f ) {
//			// Default: all loops take same amount of time.
//			printTime( ( long(toc())*long(n-i) ) / long(i) / CLOCKS_PER_SEC, os);
//			// Output time per:
//			os << "  (";
//			printMsTime( ( long(toc())*1000 ) / long(i) / CLOCKS_PER_SEC, os);
//			os << " per)";
//		} else {
//			// Compute baseline duration.
//			float w_sofar = 0.0f, w_tocome = 0.0f, gf = 1.0f;
//			int j=0;
//			for ( ; j<i; ++j ) {
//				w_sofar += gf;
//				gf *= ratio;
//			}
//			for ( ; j<n; ++j ) {
//				w_tocome += gf;
//				gf *= ratio;
//			}
//			//printf(" %0.2f/%0.2f\n", w_sofar, w_tocome);
//			printTime( long( ( float(toc())*w_tocome / w_sofar ) / CLOCKS_PER_SEC ), os);
//		}
//	} else if ( type == ADDITIVE ) {
//		// Compute baseline duration.
//		float w_sofar = 0.0f, w_tocome = 0.0f, gf = 1.0f;
//		int j=0;
//		for ( ; j<i; ++j ) {
//			w_sofar += gf;
//			gf += ratio;
//		}
//		for ( ; j<n; ++j ) {
//			w_tocome += gf;
//			gf += ratio;
//		}
//		printTime( long( ( float(toc())*w_tocome / w_sofar ) / CLOCKS_PER_SEC ), os);
//	}
//	os << endl;
//}

long svlLoopTimer::getETC(int i, int n, long toc, float ratio, svlLoopTimerRatio type)
{
	if ( type == GEOMETRIC ) {
		if ( ratio == 1.0f ) {
			// Default: all loops take same amount of time.
			return ( toc*long(n-i) ) / long(i);
		} else {
			// Compute baseline duration.
			float w_sofar = 0.0f, w_tocome = 0.0f, gf = 1.0f;
			int j=0;
			for ( ; j<i; ++j ) {
				w_sofar += gf;
				gf *= ratio;
			}
			for ( ; j<n; ++j ) {
				w_tocome += gf;
				gf *= ratio;
			}
			//printf(" %0.2f/%0.2f\n", w_sofar, w_tocome);
			return long( float(toc)*w_tocome / w_sofar );
		}
	} else if ( type == ADDITIVE ) {
		// Compute baseline duration.
		float w_sofar = 0.0f, w_tocome = 0.0f, gf = 1.0f;
		int j=0;
		for ( ; j<i; ++j ) {
			w_sofar += gf;
			gf += ratio;
		}
		for ( ; j<n; ++j ) {
			w_tocome += gf;
			gf += ratio;
		}
		return long( float(toc)*w_tocome / w_sofar );
	}
	return 0;
}

long svlLoopTimer::getETC(svlLoopTimerUnit &t)
{
	return getETC(t.i, t.n, t.last_inc_toc, t.ratio, t.type);
}


void svlLoopTimer::print(ostream &os)
{
	printTime( long(toc()/CLOCKS_PER_SEC), os );
}

// Print all estimated time to completion.
void svlLoopTimer::printETC(ostream &os)
{
	if ( units.size() == 0 )
		return;
	// j points to last (most current) timer.
	int j = int(units.size()-1);
	long toc = units[j].toc();
	os << setw(units[j].width) << setfill(' ') << units[j].i << "/" << units[j].n << ": "; svlLoopTimer::printTime( toc/long(CLOCKS_PER_SEC), os );
	/*if ( toc == 0 ) {
		os << endl;
		return;
	}*/
	long etc = toc > 0 ? getETC(units[j]) : 0;
	os << " > ";
	if ( toc == 0 ) {
		os << "-:--:--";
	} else {
		printTime( etc/long(CLOCKS_PER_SEC), os );
	}
	long last_total = toc + etc;
	for ( --j; j>=0; --j ) {
		// Simluate passage of loop with total of inner loops.
		bool zero = units[j].last_inc_toc == 0 && last_total == 0;
		if ( zero ) {
			etc = 0;
		} else if ( last_total == 0 ) {
			etc = getETC(units[j]);
		} else {
			etc = getETC( units[j].i+1, units[j].n, units[j].last_inc_toc + last_total, units[j].ratio, units[j].type );
		}
		os << "  -- " << setw(units[j].width) << setfill(' ') << units[j].i << "/" << units[j].n << ": ";
		printTime( units[j].toc()/long(CLOCKS_PER_SEC), os );

		os << " > ";
		if ( zero ) {
			os << "-:--:--";
		} else {
			printTime( max(long(0), etc - units[j+1].last_inc_toc + last_total ) /long(CLOCKS_PER_SEC), os );
		}
		last_total = units[j].last_inc_toc + etc;
	}
	os << endl;
}


