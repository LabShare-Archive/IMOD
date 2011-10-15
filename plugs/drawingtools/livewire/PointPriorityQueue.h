/*
 * Adapted form a min-heap priority queue by Alexey Kurakin (http://www.codeproject.com/KB/threads/PriorityQueueGeneric.aspx)
 */

#ifndef POINT_PRIORITY_QUEUE_H
#define POINT_PRIORITY_QUEUE_H
#include <stddef.h>

typedef unsigned char byte;
typedef unsigned int uint;

namespace Livewire {
	class PointPriorityQueue {
		/* Necessary public functions:
		 *  public bool IsEmpty()
		 *  public void Enqueue(uint x, uint y, uint I, uint score)
		 *  public bool Dequeue(uint& x, uint& y, uint& I, uint& score)
		 *  public bool DescreaseScore(uint I, uint score)
		 *  public void Clear()
		 * All other public functions can be removed
		 */

	public:
		struct Entry {
			uint score;
			size_t *index;
			const uint x, y, I;
			Entry(uint s, size_t i, uint x, uint y, uint I);
		};

	private:
		const uint _width, _height;
		size_t _count, _heap_capacity;
		const size_t _map_size;
		size_t** _map; // access like x+y*_width
		Entry **_heap;

	public:
		PointPriorityQueue(uint w, uint h);
		~PointPriorityQueue();

		void Enqueue(uint x, uint y, uint I, uint score);
		void Enqueue(uint x, uint y, uint score);
		void Enqueue(uint I, uint score);

		bool Dequeue(uint& x, uint& y, uint& I, uint& score);
		bool Peek(uint& x, uint& y, uint& I, uint& score) const;

		bool IsEmpty() const;
		size_t Size() const;

		void Clear();

		bool Contains(uint x, uint y) const;
		bool UpdateScore(uint x, uint y, uint score);
		bool DescreaseScore(uint x, uint y, uint score);
		bool IncreaseScore(uint x, uint y, uint score);

		bool Contains(uint I) const;
		bool UpdateScore(uint I, uint score);
		bool DescreaseScore(uint I, uint score);
		bool IncreaseScore(uint I, uint score);

	private:
		void ExchangeElements(size_t a, size_t b);

		void Insert(uint x, uint y, uint I, uint score);
		void HeapifyFromEndToBeginning(size_t pos);

		void DeleteRoot();
		void HeapifyFromBeginningToEnd(size_t pos);
	};
}

#endif
