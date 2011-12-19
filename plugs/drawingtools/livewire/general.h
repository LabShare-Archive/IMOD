/**

Livewire - Core code for running the Livewire algorithm
Copyright (C) 2011  Jeffrey Bush  jeff@coderforlife.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

**/

// General definitions, functions, and classes used by many of the other files

#ifndef GENERAL_H
#define GENERAL_H

typedef unsigned char byte;
typedef unsigned int uint;

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#define CASSERT(b)	typedef char __C_ASSERT__[b]	// compile-time assert

namespace Livewire
{
	template<uint scale> inline static uint ScaleBack(uint x) { return (x + scale - 1) / scale; }
	inline static uint ScaleBack(uint x, uint scale) { return (x + scale - 1) / scale; }

#if SIZE_MAX == 0xFFFFFFFFFFFFFFFF
	inline static size_t NextPowerOfTwo(size_t x) { --x; x |= x >> 1; x |= x >> 2; x |= x >> 4; x |= x >> 8; x |= x >> 16; x |= x >> 32; return x+1; }
#else
	inline static size_t NextPowerOfTwo(size_t x) { --x; x |= x >> 1; x |= x >> 2; x |= x >> 4; x |= x >> 8; x |= x >> 16; return x+1; }
#endif
	#define IS_POWER_OF_TWO(x)	(!(((x)-1)&(x)))
	#define LOG2(n)				_LOG2_<n>::x
	template <uint n> struct _LOG2_ { static const uint x; };
	template<> struct _LOG2_<          1 > { static const uint x =  0; };
	template<> struct _LOG2_<          2 > { static const uint x =  1; };
	template<> struct _LOG2_<          4 > { static const uint x =  2; };
	template<> struct _LOG2_<          8 > { static const uint x =  3; };
	template<> struct _LOG2_<         16 > { static const uint x =  4; };
	template<> struct _LOG2_<         32 > { static const uint x =  5; };
	template<> struct _LOG2_<         64 > { static const uint x =  6; };
	template<> struct _LOG2_<        128 > { static const uint x =  7; };
	template<> struct _LOG2_<        256 > { static const uint x =  8; };
	template<> struct _LOG2_<        512 > { static const uint x =  9; };
	template<> struct _LOG2_<       1024 > { static const uint x = 10; };
	template<> struct _LOG2_<       2048 > { static const uint x = 11; };
	template<> struct _LOG2_<       4096 > { static const uint x = 12; };
	template<> struct _LOG2_<       8192 > { static const uint x = 13; };
	template<> struct _LOG2_<      16384 > { static const uint x = 14; };
	template<> struct _LOG2_<      32768 > { static const uint x = 15; };
	template<> struct _LOG2_<      65536 > { static const uint x = 16; };
	template<> struct _LOG2_<     131072 > { static const uint x = 17; };
	template<> struct _LOG2_<     262144 > { static const uint x = 18; };
	template<> struct _LOG2_<     524288 > { static const uint x = 19; };
	template<> struct _LOG2_<    1048576 > { static const uint x = 20; };
	template<> struct _LOG2_<    2097152 > { static const uint x = 21; };
	template<> struct _LOG2_<    4194304 > { static const uint x = 22; };
	template<> struct _LOG2_<    8388608 > { static const uint x = 23; };
	template<> struct _LOG2_<   16777216 > { static const uint x = 24; };
	template<> struct _LOG2_<   33554432 > { static const uint x = 25; };
	template<> struct _LOG2_<   67108864 > { static const uint x = 26; };
	template<> struct _LOG2_<  134217728 > { static const uint x = 27; };
	template<> struct _LOG2_<  268435456 > { static const uint x = 28; };
	template<> struct _LOG2_<  536870912 > { static const uint x = 29; };
	template<> struct _LOG2_< 1073741824 > { static const uint x = 30; };
	template<> struct _LOG2_< 2147483648 > { static const uint x = 31; };

	// A very simple and fast vector class that grows as needed
	// Due to the lack of (de)allocation protocols for the type this is best for pointer types that you manage the memory for
	// Provides a subset of the STL vector functions
	template<class T> class vector
	{
	private:
		T *_x;
		size_t _len, _cap;
		static const size_t min_cap = 0x08;
	public:
		inline explicit	vector() : _len(0), _cap(0), _x(NULL)	{ }
		inline			~vector()					{ this->reset(); }
		inline T&		operator[](size_t i)		{ return this->_x[i]; }
		inline const T&	operator[](size_t i) const	{ return this->_x[i]; }
		inline T&		front()						{ return this->_x[0]; }
		inline const T&	front() const				{ return this->_x[0]; }
		inline T&		back()						{ return this->_x[this->_len-1]; }
		inline const T&	back() const				{ return this->_x[this->_len-1]; }
		inline bool		empty() const				{ return this->_len == 0; }
		inline size_t	size() const				{ return this->_len; }
		inline size_t	capacity() const			{ return this->_cap; }
		inline void		clear()						{ this->_len = 0; }
		inline void		reset()						{ free(this->_x); this->_x = NULL; this->_len = 0; this->_cap = 0; } // Not part of STL
		inline void		reserve(size_t n)
		{
			if (n > this->_cap)
			{
				if (!this->_cap)		{ this->_cap = min_cap; }
				while (n > this->_cap)	{ this->_cap <<= 1; }
				this->_x = (T*)realloc(this->_x, this->_cap*sizeof(T));
			}
		}
		inline void		push_back(const T& v)
		{
			if (this->_len == this->_cap)
			{
				this->_cap = this->_cap ? this->_cap << 1 : min_cap;
				this->_x = (T*)realloc(this->_x, this->_cap*sizeof(T));
			}
			this->_x[this->_len++] = v;
		}
		//inline void		pop_back()					{ --this->_len; } // STL
		inline T&		pop_back()					{ return this->_x[--this->_len]; } // Easier definition
	};

	// A simple pool for blocks of memory that may be reused
	// No checks are done to ensure that pointers returned are actually pointers that CAN be returned (due to size)
	// Must call Clear to free all memory used (will not free non-returned memory)
	class BlockPool
	{
	public:
		static void *Get(size_t size);
		inline static void *Get(size_t size, int init_val) { return memset(Get(size), init_val, size); }
		static void *Resize(void *block, size_t size);
		static void Return(void *block);
		static void Clear(); // TODO: call this function somewhere

		// Free a block you want to never return to the pool
		//inline static void Free(void *block) { free(((byte*)block) - sizeof(size_t)); }
	};

	// A sparse matrix which only uses memory for blocks around non-zero values
	template <typename T, uint block_size = 256> class SparseMatrix
	{
	private:
		CASSERT(IS_POWER_OF_TWO(block_size)); // must be a power of 2, there are numerous mathematical shortcuts taken with this assumption in mind
		static const uint block_size_ = LOG2(block_size), block_size_2 = block_size*block_size, block_mem_size = block_size_2 * sizeof(T);

		struct Block { T *data; uint zeroes; }; // if data == NULL then all values in the block are 0 (even if zeroes != block_size_2)

		Block *_x;
		const uint _w, _h, _wh; // scaled by sz
	public:
		inline SparseMatrix(uint w, uint h) : _w(ScaleBack<block_size>(w)), _h(ScaleBack<block_size>(h)), _wh(_h*_w)
		{
			const uint bytes = this->_wh * sizeof(Block);
			this->_x = (Block*)memset(malloc(bytes), 0, bytes);
		}
		inline ~SparseMatrix()
		{
			const uint wh = this->_wh;
			for (size_t i = 0; i < wh; ++i)
				if (this->_x[i].data)
					BlockPool::Return(this->_x[i].data);
			free(this->_x);
		}
		inline T Get(const uint X, const uint Y) const
		{
			const Block *b = this->_x + (Y >> block_size_)*this->_w + (X >> block_size_);
			return b->data ? b->data[((Y & (block_size-1)) << block_size_) + (X & (block_size-1))] : 0;
		}
		void Set(const uint X, const uint Y, const T& val)
		{
			Block *b = this->_x + (Y >> block_size_)*this->_w + (X >> block_size_);

			if (b->data)
			{
				T *d = b->data + (((Y & (block_size-1)) << block_size_) + (X & (block_size-1)));
				if (val)
				{
					if (!*d) --b->zeroes; // changing from 0 to non-0
					*d = val;
				}
				else if (*d) // changing from non-0 to 0
				{
					if (++b->zeroes == block_size_2) // now all 0s
					{
						BlockPool::Return(b->data);
						b->data = NULL;
					}
					else
					{
						*d = 0;
					}
				}
			}
			else if (val)
			{
				b->zeroes = block_size_2 - 1;
				b->data = (T*)BlockPool::Get(block_mem_size, 0);
				b->data[((Y & (block_size-1)) << block_size_) + (X & (block_size-1))] = val;
			}
		}
		inline void Clear()
		{
			const uint wh = this->_wh;
			for (size_t i = 0; i < wh; ++i)
				if (this->_x[i].data)
					BlockPool::Return(this->_x[i].data);
			memset(this->_x, 0, wh*sizeof(Block));
		}
	};
	// TODO: make a specialized bool version (blocks would be 1/8 the size, allow for blocks of solid 1 or solid 0, and take a little longer to access...)
}

#endif