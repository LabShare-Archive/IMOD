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
** FILENAME:    svlBitArray.cpp
** AUTHOR(S):   Paul Baumstarck <pbaumstarck@stanford.edu>
**
*****************************************************************************/

#include "svlBitArray.h"

// A lookup table for the number of bits set in a char.
const int svlBitArray::NUNSETLOOKUP[256] =
   {0, 1, 1, 2, 1, 2, 2, 3,
	1, 2, 2, 3, 2, 3, 3, 4,
	1, 2, 2, 3, 2, 3, 3, 4,
	2, 3, 3, 4, 3, 4, 4, 5,
	1, 2, 2, 3, 2, 3, 3, 4,
	2, 3, 3, 4, 3, 4, 4, 5,
	2, 3, 3, 4, 3, 4, 4, 5,
	3, 4, 4, 5, 4, 5, 5, 6,
	1, 2, 2, 3, 2, 3, 3, 4,
	2, 3, 3, 4, 3, 4, 4, 5,
	2, 3, 3, 4, 3, 4, 4, 5,
	3, 4, 4, 5, 4, 5, 5, 6,
	2, 3, 3, 4, 3, 4, 4, 5,
	3, 4, 4, 5, 4, 5, 5, 6,
	3, 4, 4, 5, 4, 5, 5, 6,
	4, 5, 5, 6, 5, 6, 6, 7,
	1, 2, 2, 3, 2, 3, 3, 4,
	2, 3, 3, 4, 3, 4, 4, 5,
	2, 3, 3, 4, 3, 4, 4, 5,
	3, 4, 4, 5, 4, 5, 5, 6,
	2, 3, 3, 4, 3, 4, 4, 5,
	3, 4, 4, 5, 4, 5, 5, 6,
	3, 4, 4, 5, 4, 5, 5, 6,
	4, 5, 5, 6, 5, 6, 6, 7,
	2, 3, 3, 4, 3, 4, 4, 5,
	3, 4, 4, 5, 4, 5, 5, 6,
	3, 4, 4, 5, 4, 5, 5, 6,
	4, 5, 5, 6, 5, 6, 6, 7,
	3, 4, 4, 5, 4, 5, 5, 6,
	4, 5, 5, 6, 5, 6, 6, 7,
	4, 5, 5, 6, 5, 6, 6, 7,
	5, 6, 6, 7, 6, 7, 7, 8};

// Constructor that copies a range out of a bitmap.
//svlBitArray::svlBitArray(const svlBitArray &m, int start, int len)
//{
//	if ( len == 0 )
//		len = m._sz - start; // Set length to rest of array.
//	_sz = len;
//	_map_sz = SVL_BITMAP_SIZE(_sz);
//	_map = new int[_map_sz];
//	memset(_map, 0, _map_sz*sizeof(int));
//	copy(m, start, len, 0);
//}

// Save to file.
void svlBitArray::save(const char *fname)
{
    FILE *fo = fopen(fname,"wb");
    bool gb;
    for ( int i=0; i<_sz; ++i )
        fwrite(&(gb=get(i)),sizeof(bool),1,fo);
    fclose(fo);
}

// Flip all bits in the map.
void svlBitArray::flipAll()
{
    for ( int i=0; i<_map_sz; ++i )
        _map[i] ^= -1;
}

// Copy one bitmap to another.
void svlBitArray::copy(const svlBitArray &c)
{
    if ( _map_sz < c._map_sz ) {
        // Must allocate.
        if ( _map ) delete[] _map;
        _map = new int[c._map_sz];
        _sz = c._sz;
        _map_sz = c._map_sz;
    }
    memcpy(_map, c._map, c._map_sz*sizeof(int));
}

// Copy a range out of one bitmap.
//void svlBitArray::copy(const svlBitArray &c, int c_start, int len, int this_start)
//{
//	if ( c_start % 8 == this_start % 8 ) {
//		// TODO paulb: This doesn't work for this_start%8 != 0, since it doesn't mask the first copy.
//
//		// Copy whole bytes from c to this.
//		int c_ix = c_start/8, c_ix_end = (c_start + len)/8;
//		int c_ix_full = c_ix_end + ( c_start + len % 8 == 0 ? 1 : 0 );
//		//printf("cix, end, full: %d, %d, %d\n", c_ix, c_ix_end, c_ix_full);
//		int ix = this_start/8;
//		if ( c_ix_full > c_ix )
//			memcpy( ((unsigned char*)_map) + ix, ((unsigned char*)c._map) + c_ix, c_ix_full - c_ix );
//		if ( c_ix_full == c_ix_end ) {
//			// Get last byte and mask out valid bits.
//			unsigned char cha = *( ((unsigned char*)c._map) + c_ix_end );
//			//printf("Last: %d\n", cha);
//			int val_bits = (c_start + len) % 8;
//			//printf("V bits: %d\n", val_bits);
//			unsigned char mask = (1<<val_bits)-1;
//			//printf("Mask: %d\n", mask);
//			cha &= mask;
//			//printf("len: %d\n", len);
//			*( ((unsigned char*)_map) + (len/8) ) = cha;
//		}
//	} else {
//		SVL_LOG(SVL_LOG_FATAL, "Not implemented");
//	}
//}

// Calculate number of bits set via lookup table.
int svlBitArray::count() const
{
    unsigned char *ptr = (unsigned char*)_map;
    int ret = 0;
    for (int i=0; i<int(_map_sz*sizeof(int)); ++i)
        ret += NUNSETLOOKUP[*ptr++];
    return ret;
}

// Printing functions.
void svlBitArray::print(ostream &os, int stride) const
{
    for ( int i=0; i<_sz; ++i ) {
        os << ( SVL_BIT_GET(_map,i) ? 1 : 0 );
        if ( stride > 0 && (i+1)%stride == 0 )
            os << endl;
    }
    if ( !( stride > 0 && _sz%stride == 0 ) )
        os << endl;
}


