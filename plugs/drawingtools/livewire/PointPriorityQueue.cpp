/*
 * Adapted form a min-heap priority queue by Alexey Kurakin (http://www.codeproject.com/KB/threads/PriorityQueueGeneric.aspx)
 */

#include <stdlib.h>
#include <string.h>

#include "PointPriorityQueue.h"

//#define ENABLE_CHECKING

using namespace Livewire;

PointPriorityQueue::Entry::Entry(uint s, size_t i, uint x, uint y, uint I) : score(s), index(new size_t(i)), x(x), y(y), I(I) {}

#define IW(x, y, w)	((x) + (y) * (w))
#define I(x, y)		IW(x, y, this->_width)

#ifdef ENABLE_CHECKING
#define ASSERT(B)	assert(B)
#define CHECK(O)	Check(O, this->_heap, this->_count, this->_map, this->_width, this->_height)
#include <assert.h>
static void Check(const bool cHO, const PointPriorityQueue::Entry **heap, const uint count, const size_t** map, const uint w, const uint h)
{
	for (size_t i = 1; i < count; ++i)
	{
		PointPriorityQueue::Entry *e = heap[i];
		assert(i == *e->index);
		if (cHO)
			assert(e->score >= heap[(i-1)/2]->score);
		assert(e->I == IW(e->x, e->y, w));
		assert(e->index == map[e->I]);
	}
	const size_t wh = w * h;
	for (uint I = 0; I < wh; ++I)
	{
		size_t *index = map[I], i;
		assert(index == NULL || ((i = *index) < count && heap[i]->I == I));
	}
}
#else
#define ASSERT(B)	
#define CHECK(O)	
#endif

PointPriorityQueue::PointPriorityQueue(uint w, uint h) :
	_width(w), _height(h), _count(0), _heap_capacity(w*h/100), _map_size(w*h*sizeof(size_t*)),
	_map((size_t**)memset(malloc(_map_size), NULL, _map_size)), _heap((Entry**)malloc(_heap_capacity*sizeof(Entry*))) { }

PointPriorityQueue::~PointPriorityQueue() { this->Clear(); free(this->_map); }

#pragma region Priority queue operations

void PointPriorityQueue::Enqueue(uint I, uint score) { this->Insert(I % this->_width, I / this->_width, I, score); }
void PointPriorityQueue::Enqueue(uint x, uint y, uint score) { this->Insert(x, y, I(x,y), score); }
void PointPriorityQueue::Enqueue(uint x, uint y, uint I, uint score) { this->Insert(x, y, I, score); }

bool PointPriorityQueue::Dequeue(uint& x, uint& y, uint& I, uint& score)
{
	if (this->_count == 0) return false;
	//this->Peek(x, y, I, score);
	Entry *e = this->_heap[0];
	I = e->I; x = e->x; y = e->y; score = e->score;
	this->DeleteRoot();
	return true;
}

bool PointPriorityQueue::Peek(uint& x, uint& y, uint& I, uint& score) const
{
	if (this->_count == 0) return false;
	Entry *e = this->_heap[0];
	I = e->I; x = e->x; y = e->y; score = e->score;
	return true;
}

bool PointPriorityQueue::IsEmpty() const { return this->_count == 0; }
size_t PointPriorityQueue::Size() const { return this->_count; }

void PointPriorityQueue::Clear()
{
	for (size_t i = 0; i < this->_count; ++i)
	{
		Entry *e = this->_heap[i];
		this->_map[e->I] = NULL;
		delete e->index;
		delete e;
	}
	this->_count = 0;
	//memset(this->_map, NULL, this->_map_size);
	CHECK(true);
}

#pragma endregion

#pragma region Advanced priority queue operations (added by Jeff)

bool PointPriorityQueue::Contains(uint x, uint y) const { return this->_map[I(x, y)] != NULL; }
bool PointPriorityQueue::UpdateScore(uint x, uint y, uint score) { return this->UpdateScore(I(x,y), score); }
bool PointPriorityQueue::DescreaseScore(uint x, uint y, uint score) { return this->DescreaseScore(I(x,y), score); }
bool PointPriorityQueue::IncreaseScore(uint x, uint y, uint score) { return this->IncreaseScore(I(x,y), score); }

bool PointPriorityQueue::Contains(uint I) const { return this->_map[I] != NULL; }
bool PointPriorityQueue::UpdateScore(uint I, uint score)
{
	ASSERT(this->_map[I] != NULL);
	size_t i = *this->_map[I];
	uint s = this->_heap[i]->score;
	if (score == s) return false;
	this->_heap[i]->score = score;
	if (score > s)	this->HeapifyFromBeginningToEnd(i); // priority is larger, heapify
	else			this->HeapifyFromEndToBeginning(i); // priority is smaller, heapify
	return true;
}
bool PointPriorityQueue::DescreaseScore(uint I, uint score)
{
	ASSERT(this->_map[I] != NULL);
	size_t i = *this->_map[I];
	if (score >= this->_heap[i]->score) return false;
	this->_heap[i]->score = score;
	this->HeapifyFromEndToBeginning(i);
	return true;
}
bool PointPriorityQueue::IncreaseScore(uint I, uint score)
{
	ASSERT(this->_map[I] != NULL);
	size_t i = *this->_map[I];
	if (score <= this->_heap[i]->score) return false;
	this->_heap[i]->score = score;
	this->HeapifyFromBeginningToEnd(i);
	return true;
}

#pragma endregion

#pragma region Heap operations
void PointPriorityQueue::ExchangeElements(size_t a, size_t b)
{
	Entry *e = this->_heap[a];
	*(this->_heap[a] = this->_heap[b])->index = a;
	*(this->_heap[b] = e)->index = b;
	CHECK(false);
}

void PointPriorityQueue::Insert(uint x, uint y, uint I, uint score)
{
	ASSERT(x < this->_width && y < this->_height);
	ASSERT(this->_map[I] == NULL);

	const size_t count = this->_count;
	if (count >= this->_heap_capacity)
		this->_heap = (Entry**)realloc(this->_heap, this->_heap_capacity *= 2);

	Entry *e = new Entry(score, count, x, y, I);
	this->_heap[count] = e;
	++this->_count;
	this->_map[I] = e->index;
	CHECK(false);

	// heapify after insert, from end to beginning
	this->HeapifyFromEndToBeginning(count);
}
void PointPriorityQueue::HeapifyFromEndToBeginning(size_t pos)
{
	ASSERT(pos < this->_count); 

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
	ASSERT(this->_count > 0); 

	if (this->_count == 1) { this->Clear(); return; }

	Entry *e = this->_heap[0];
	this->_map[e->I] = NULL;
	delete e->index;
	delete e;

	*(this->_heap[0] = this->_heap[--this->_count])->index = 0;
	CHECK(false);

	// heapify
	this->HeapifyFromBeginningToEnd(0);
}
void PointPriorityQueue::HeapifyFromBeginningToEnd(size_t pos)
{
	const size_t count = this->_count;

	ASSERT(pos < count); 

	for(;;)
	{
		// on each iteration exchange element with its smallest child
		// heap[i] have children heap[2*i + 1] and heap[2*i + 2] and parent heap[(i-1)/ 2];
		size_t smallest = pos, l = 2 * pos + 1, r = 2 * pos + 2;
		uint score = this->_heap[pos]->score;
		if (l < count && score > this->_heap[l]->score) { smallest = l; score = this->_heap[l]->score; } 
		if (r < count && score > this->_heap[r]->score) { smallest = r; }

		if (smallest == pos) break;

		this->ExchangeElements(smallest, pos);
		pos = smallest;
	}

	CHECK(true);
}

#pragma endregion
