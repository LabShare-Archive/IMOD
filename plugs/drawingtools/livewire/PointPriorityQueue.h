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

/*
 * Inspired by a min-heap priority queue by Alexey Kurakin (http://www.codeproject.com/KB/threads/PriorityQueueGeneric.aspx)
 */

#ifndef POINT_PRIORITY_QUEUE_H
#define POINT_PRIORITY_QUEUE_H

typedef unsigned char byte;
typedef unsigned int uint;

#include <stddef.h>

namespace Livewire {
	class PointPriorityQueue {
		/* Necessary public functions:
		 *  public void Enqueue(uint x, uint y, uint I, uint score)
		 *  public bool Dequeue(uint& x, uint& y, uint& I, uint& score)
		 *  public bool DescreaseScore(uint I, uint score)
		 *  public void Clear()
		 * All other public functions can be removed
		 */

	private:
		struct Entry
		{
			uint score;
			size_t *index;
			const uint x, y, I;
			Entry(uint s, size_t i, uint x, uint y, uint I);
		};

	private:
		const uint _width, _height;
		size_t _count, _heap_capacity;
		size_t** _map; // access like x+y*_width
		Entry **_heap;

	public:
		PointPriorityQueue(uint w, uint h);
		~PointPriorityQueue();

		void Enqueue(uint x, uint y, uint I, uint score);
//		void Enqueue(uint x, uint y, uint score);
//		void Enqueue(uint I, uint score);

		bool Dequeue(uint& x, uint& y, uint& I, uint& score);
//		bool Peek(uint& x, uint& y, uint& I, uint& score) const;

//		bool IsEmpty() const;
//		size_t Size() const;

		void Clear();

//		bool Contains(uint x, uint y) const;
//		bool UpdateScore(uint x, uint y, uint score);
//		bool DescreaseScore(uint x, uint y, uint score);
//		bool IncreaseScore(uint x, uint y, uint score);

//		bool Contains(uint I) const;
//		bool UpdateScore(uint I, uint score);
		bool DescreaseScore(uint I, uint score);
//		bool IncreaseScore(uint I, uint score);

	private:
		void ExchangeElements(size_t a, size_t b);

		void Insert(uint x, uint y, uint I, uint score);
		void HeapifyFromEndToBeginning(size_t pos);

		void DeleteRoot();
		void HeapifyFromBeginningToEnd(size_t pos);
	};
}

#endif
