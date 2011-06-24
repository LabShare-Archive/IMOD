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
** FILENAME:    svlBitArray.h
** AUTHOR(S):   Paul Baumstarck <pbaumstarck@stanford.edu>
**              Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**  Implements a packed array of bits.
**
*****************************************************************************/

#pragma once

#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <math.h>
#include <cstring>

using namespace std;

#define INT_BIT_SIZE (8*sizeof(int))
#define SVL_BIT_GET(x,b) ( ( (x)[(b)/INT_BIT_SIZE] & (1<<((b)%INT_BIT_SIZE)) ) != 0 )
#define SVL_BIT_SET(x,b) ( (x)[(b)/INT_BIT_SIZE] |= (1<<((b)%INT_BIT_SIZE)) )
#define SVL_BIT_CLEAR(x,b) ( (x)[(b)/INT_BIT_SIZE] &= (unsigned int)(-1) - (1<<((b)%INT_BIT_SIZE)) )
#define SVL_BIT_FLIP(x,b) ( (x)[(b)/INT_BIT_SIZE] ^= (1<<((b)%INT_BIT_SIZE)) )
#define SVL_BITMAP_SIZE(x) int( ((x+INT_BIT_SIZE-1)/INT_BIT_SIZE) )

class svlBitArray {
protected:
    static const int NUNSETLOOKUP[256]; // Lookup table containing number of bits set in an unsigned char.
    int _sz, _map_sz;
    int *_map;

public:
    svlBitArray() : _sz(0), _map_sz(0) {
        _map = NULL;
    }
    svlBitArray(int sz) : _sz(sz), _map_sz(SVL_BITMAP_SIZE(sz)) {
        _map = new int[_map_sz];
        memset(_map, 0, _map_sz*sizeof(int));
    }
    // Copy an existing bitmap.
    svlBitArray(const svlBitArray &m) : _sz(m._sz), _map_sz(m._map_sz) {
        _map = new int[_map_sz];
        memcpy(_map, m._map, _map_sz*sizeof(int));
    }
    // Copy a range out of a bitmap.
    //svlBitArray(const svlBitArray &m, int start, int len = 0);
    virtual ~svlBitArray() {
        if ( _map ) delete[] _map;
    }

    // I/O
    void save(const char *fname);

    // Accessors.
    inline int size() { return _sz; } // Return number of elements that can be stored here.
    inline bool operator[](int i) const {
        return SVL_BIT_GET(_map,i);
    }
    inline bool get(int i) const { // Function alias of above.
        return SVL_BIT_GET(_map,i);
    }
    inline void set(int i) {
        SVL_BIT_SET(_map,i);
    }
    inline void clear(int i) {
        SVL_BIT_CLEAR(_map,i);
    }
    inline void flip(int i) {
        SVL_BIT_FLIP(_map,i);
    }
    inline void setAll() {
        memset(_map, 0xff, _map_sz*sizeof(int));
    }
    inline void clearAll() {
        memset(_map, 0, _map_sz*sizeof(int));
    }
    void flipAll();
    
    // Copy one bitmap to another.
    void copy(const svlBitArray &c);
    inline void operator=(const svlBitArray &c) {
        copy(c);
    }
    // Copy range out of a bitmap.
    //void copy(const svlBitArray &c, int c_start, int len, int this_start = 0); // Under development; possibly to be deprecated.
    
    // Calculate number of bits set via lookup table.
    int count() const;
    
    // Printing functions.
    void print(ostream &os = cout, int stride = -1) const;
};
