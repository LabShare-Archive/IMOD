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

// Inspired by a min-heap priority queue by Alexey Kurakin (http://www.codeproject.com/KB/threads/PriorityQueueGeneric.aspx)

#include "PointPriorityQueue.h"

//#define ENABLE_CHECKING

using namespace Livewire;

struct PointPriorityQueue::Entry
{
	uint score;
	size_t index;
	/*const*/ uint x, y, I;

private:
	static vector<Entry*> avail;
	static vector<void*> blocks;

public:
	inline static Entry *Get(uint s, size_t idx, uint x, uint y, uint I)
	{
		if (avail.size() == 0)
		{
			// Allocate a bunch at a time
			Entry *mem = (Entry*)malloc(0x1000*sizeof(Entry));
			blocks.push_back(mem);
			avail.reserve(0x1000);
			for (size_t i = 0; i < 0x1000; ++i)
				avail.push_back(mem+i);
		}
		Entry *e = avail.pop_back();
		e->score = s; e->index = idx; e->x = x; e->y = y; e->I = I;
		return e;
	}
	inline static void Return(Entry *e) { avail.push_back(e); }
	//inline void Return() { Return(this); }
	inline static void Clear() // TODO: call this function somewhere
	{
		for (size_t i = 0; i < blocks.size(); ++i)
			free(blocks[i]);
		blocks.reset();
		avail.reset();
	}
};
vector<PointPriorityQueue::Entry*> PointPriorityQueue::Entry::avail;
vector<void*> PointPriorityQueue::Entry::blocks;

#ifdef ENABLE_CHECKING
#ifdef _MSC_VER
#include <crtdbg.h>
#define assert _ASSERT
//#include <stdio.h>
//#define assert(expr) (!!(expr) ||  fprintf(stderr, "Assertion failed: %s\n", #expr))
#else
#include <assert.h>
#endif
#define ASSERT(B)	assert(B)
#define CHECK(O)	Check(O, this->_heap, this->_map, this->_width, this->_height)
static void Check(const bool cHO, const vector<PointPriorityQueue::Entry*>& heap, const SparseMatrix<size_t>& map, const uint w, const uint h)
{
	for (size_t i = 0; i < heap.size(); ++i)
	{
		const PointPriorityQueue::Entry *e = heap[i];
		assert(i == *e->index);
		if (i && cHO)
			assert(e->score >= heap[(i-1)/2]->score);
		assert(e->I == IW(e->x, e->y, w));
		assert(&e->index == map.Get(e->x, e->y));
	}
	/*const size_t wh = w * h;
	for (uint I = 0; I < wh; ++I)
	{
		// TODO: update for new map format
		const size_t *index = map[I];
		size_t i;
		assert(index == NULL || ((i = *index) < heap.size() && heap[i]->I == I));
	}*/
}
#else
#define ASSERT(B)	
#define CHECK(O)	
#endif

PointPriorityQueue::PointPriorityQueue(uint w, uint h) : _width(w), _height(h), _heap(), _map(w, h) { const uint perim = w+w+h+h; this->_heap.reserve(perim > 0x4000 ? 0x4000 : perim); }
PointPriorityQueue::~PointPriorityQueue() { this->Clear(); }

#pragma region Priority queue operations

//void PointPriorityQueue::Enqueue(uint I, uint score) { this->Insert(I % this->_width, I / this->_width, I, score); }
//void PointPriorityQueue::Enqueue(uint x, uint y, uint score) { this->Insert(x, y, x + y * this->_width, score); }
void PointPriorityQueue::Enqueue(uint x, uint y, uint I, uint score) { this->Insert(x, y, I, score); }

bool PointPriorityQueue::Dequeue(uint& x, uint& y, uint& I, uint& score)
{
	if (this->_heap.empty()) return false;
	//this->Peek(x, y, I, score);
	Entry *e = this->_heap[0];
	I = e->I; x = e->x; y = e->y; score = e->score;
	this->DeleteRoot();
	return true;
}

//bool PointPriorityQueue::Peek(uint& x, uint& y, uint& I, uint& score) const
//{
//	if (this->_heap.empty()) return false;
//	Entry *e = this->_heap[0];
//	I = e->I; x = e->x; y = e->y; score = e->score;
//	return true;
//}

//bool PointPriorityQueue::IsEmpty() const { return this->_heap.empty(); }
//size_t PointPriorityQueue::Size() const { return this->_heap.size(); }

void PointPriorityQueue::Clear()
{
	const size_t count = this->_heap.size();
	for (size_t i = 0; i < count; ++i)
		Entry::Return(this->_heap[i]);
	this->_heap.clear();
	this->_map.Clear();
	CHECK(true);
}

#pragma endregion

#pragma region Advanced priority queue operations (added by Jeff) - These require the _map variable and is the only place that variable is used

bool PointPriorityQueue::Contains(uint x, uint y) const { return this->_map.Get(x, y) != NULL; }
//bool PointPriorityQueue::UpdateScore(uint x, uint y, uint score)
//{
//	ASSERT(this->_map.Get(x, y));
//	size_t i = *this->_map.Get(x, y);
//	uint s = this->_heap[i]->score;
//	if (score == s) return false;
//	this->_heap[i]->score = score;
//	if (score > s)	this->HeapifyFromBeginningToEnd(i); // priority is larger, heapify
//	else			this->HeapifyFromEndToBeginning(i); // priority is smaller, heapify
//	return true;
//}
bool PointPriorityQueue::DescreaseScore(uint x, uint y, uint score)
{
	ASSERT(this->_map.Get(x, y));
	size_t i = *this->_map.Get(x, y);
	if (score >= this->_heap[i]->score) return false;
	this->_heap[i]->score = score;
	this->HeapifyFromEndToBeginning(i);
	return true;
}
//bool PointPriorityQueue::IncreaseScore(uint x, uint y, uint score)
//{
//	ASSERT(this->_map.Get(x, y));
//	size_t i = *this->_map.Get(x, y);
//	if (score <= this->_heap[i]->score) return false;
//	this->_heap[i]->score = score;
//	this->HeapifyFromBeginningToEnd(i);
//	return true;
//}

//bool PointPriorityQueue::Contains(uint I) const { ... }
//bool PointPriorityQueue::UpdateScore(uint I, uint score) { ... }
//bool PointPriorityQueue::DescreaseScore(uint I, uint score) { ... }
//bool PointPriorityQueue::IncreaseScore(uint I, uint score) { ...; }

void PointPriorityQueue::UpdateAllScores(CalcScore f, const void *param)
{
	const size_t count = this->_heap.size();

	for (size_t i = 0; i < count; ++i)
	{
		Entry *e = this->_heap[i];
		e->score = f(e->x, e->y, e->I, param);
	}

	// Runs in linear time
	for (ptrdiff_t i = count / 2 - 1; i >= 0; --i)
		this->HeapifyFromBeginningToEnd(i);
}

#pragma endregion

#pragma region Heap operations
void PointPriorityQueue::ExchangeElements(size_t a, size_t b)
{
	Entry *e = this->_heap[a];
	(this->_heap[a] = this->_heap[b])->index = a;
	(this->_heap[b] = e)->index = b;
	CHECK(false);
}

void PointPriorityQueue::Insert(uint x, uint y, uint I, uint score)
{
	ASSERT(x < this->_width && y < this->_height);
	ASSERT(!this->_map.Get(x,y));
	
	const size_t count = this->_heap.size();
	Entry *e = Entry::Get(score, count, x, y, I);
	this->_heap.push_back(e);
	this->_map.Set(x, y, &e->index);
	CHECK(false);

	// heapify after insert, from end to beginning
	this->HeapifyFromEndToBeginning(count);
}
void PointPriorityQueue::HeapifyFromEndToBeginning(size_t pos)
{
	ASSERT(pos < this->_heap.size()); 

	while (pos > 0) {
		// heap[i] have children heap[2*i + 1] and heap[2*i + 2] and parent heap[(i-1)/ 2];
		size_t parentPos = (pos - 1) / 2;
		if (this->_heap[parentPos]->score > this->_heap[pos]->score)
		{
			this->ExchangeElements(parentPos, pos);
			pos = parentPos;
		}
		else break;
	}
	CHECK(true);
}

void PointPriorityQueue::DeleteRoot()
{
	ASSERT(this->_heap.size() > 0); 

	Entry *e = this->_heap[0], *E = this->_heap.pop_back();
	this->_map.Set(e->x, e->y, NULL);

	if (e != E)
	{
		(this->_heap[0] = E)->index = 0;
	
		CHECK(false);
	
		// heapify
		this->HeapifyFromBeginningToEnd(0);
	}

	Entry::Return(e);
}
void PointPriorityQueue::HeapifyFromBeginningToEnd(size_t pos)
{
	const size_t count = this->_heap.size();

	ASSERT(pos < count); 

	const uint S = this->_heap[pos]->score;
	size_t smallest = pos;
	for(;;)
	{
		// on each iteration exchange element with its smallest child
		// heap[i] have children heap[2*i + 1] and heap[2*i + 2] and parent heap[(i-1)/ 2];
		uint score;
		size_t l = 2 * pos + 1, r = 2 * pos + 2;
		if (l < count && S > this->_heap[l]->score) { smallest = l; score = this->_heap[l]->score; } else { score = S; }
		if (r < count && score > this->_heap[r]->score) { smallest = r; }
		else if (smallest == pos) break;

		this->ExchangeElements(smallest, pos);
		pos = smallest;
	}

	CHECK(true);
}
#pragma endregion
