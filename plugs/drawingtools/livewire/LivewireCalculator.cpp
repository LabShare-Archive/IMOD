#include "LivewireCalculator.h"

//#define SAVE_SCORE_IMAGE

#ifdef SAVE_SCORE_IMAGE
#include "BitmapWriter.h"
#endif

#define SQRT2		1.41421356237309504880168872420969807856967187537694807317667973799
#define IW(x, y, w)	((x) + (y) * (w))
#define I(x, y)		IW(x, y, this->_w)

using namespace Livewire;

LivewireCalculator::LivewireCalculator(uint w, uint h, byte *weights) : Threaded("Livewire Calculator"),
	_weights(weights),  _w(w), _h(h), _wh(w*h), _data(new Pixel[_wh]), _edge(w, h)
	{ this->SetTotalProgress(_wh); }
LivewireCalculator::~LivewireCalculator() { delete[] this->_data; }

void LivewireCalculator::Start(QPoint start)
{
	// TODO: Have a maximum window for the livewire data

	this->_start = start;
	this->_x = start.x();
	this->_y = start.y();
	this->_I = I(this->_x, this->_y);
	Threaded::Start();
}

void LivewireCalculator::Run()
{
	// TODO: Optimize more

	const uint w = this->_w, w1 = w - 1, h = this->_h, h1 = h - 1, wh = this->_wh;

	// Reset data structures
	memset(this->_data, 0, wh * sizeof(Pixel));
	this->_edge.Clear();

#ifdef SAVE_SCORE_IMAGE
	uint *scores = (uint*)memset(malloc(wh*sizeof(uint)), 0, wh*sizeof(uint));
#endif

	// Start with just the start point
	uint X, Y, I = this->_I, S;
	this->_edge.Enqueue(this->_x, this->_y, I, 0);
	//this->_data[I].status = inEdge; // not necessary, immediately set to visited
	this->_data[I].traceI = UINT_MAX; // it points to off-screen

	// Loop until there are no more points
	while (this->IsExecuting() && this->_edge.Dequeue(X, Y, I, S))
	{
		// Get the best-scoring point and mark it as visited (done being calculated)
		this->_data[I].status = visited;
#ifdef SAVE_SCORE_IMAGE
		scores[I] = S;
#endif

#define CALC_POINT(dx, dy, diag)	this->CalcPoint(X + dx, Y + dy, I + dy*w + dx, diag, I, S)

		if (X && X < w1 && Y && Y < h1)
		{
			// entire 3x3 neighborhood
			CALC_POINT(-1, -1, true );
			CALC_POINT( 0, -1, false);
			CALC_POINT(+1, -1, true );
			CALC_POINT(-1,  0, false);
			CALC_POINT(+1,  0, false);
			CALC_POINT(-1, +1, true );
			CALC_POINT( 0, +1, false);
			CALC_POINT(+1, +1, true );
		}
		else
		{
			// Get the range for the neighboring points
			uint min_x = X ? X - 1 : 0, max_x = X == w1 ? w1 : X + 1;
			uint min_y = Y ? Y - 1 : 0, max_y = Y == h1 ? h1 : Y + 1;

			// Cycle through all neighboring points
			for (uint y = min_y, off = y * w; y <= max_y; ++y, off += w)
				for (uint x = min_x, i = off + x; x <= max_x; ++x, ++i)
					this->CalcPoint(x, y, i, ((X-x + Y-y) & 1) == 0, I, S);
		}

		this->IncProgress();
	}

#ifdef SAVE_SCORE_IMAGE
	this->Checkpoint("saving scores image");
	WriteBitmap(scores, w, h, "scores");

	uint *dscores = (uint*)memset(malloc(wh*sizeof(uint)), 0, wh*sizeof(uint));
	for (uint i = 0; i < wh; ++i)
	{
		uint I = i, Y = i / w, X = i - Y * w;
		double dist = 0;
		while (I != this->_I && I != 0 && dist < 10000)
		{
			uint i = this->_data[I].traceI, y = i / w, x = i - y * w;
			bool diagonal = ((((X-x) + (Y-y)) & 1) == 0);
			dist += diagonal ? SQRT2 : 1;
			I = IW(X = x, Y = y, w);
		}
		dscores[i] = (uint)(scores[i] / dist);
	}
	WriteBitmap(dscores, w, h, "dscores");

	free(dscores);
	free(scores);
#endif
}

inline void LivewireCalculator::CalcPoint(const uint x, const uint y, const uint i, const bool diagonal, const uint I, const uint S)
{
	// Make sure the point isn't already done
	const Status s = this->_data[i].status;
	if (s != visited)
	{
		// Calculate the score for this point
		const uint score = S + (diagonal ? (uint)(SQRT2 * this->_weights[i]) : this->_weights[i]);

		// Add the point to the edge or update its score
		if (s == unvisited)
		{
			this->_edge.Enqueue(x, y, i, score);
			this->_data[i].status = inEdge;
			this->_data[i].traceI = I;
		}
		else if (this->_edge.DescreaseScore(i, score))
		{
			this->_data[i].traceI = I;
		}
	}
}

QVector<QPoint> LivewireCalculator::GetTrace(QPoint end)
{
	uint I = I(end.x(), end.y());
	if (this->_data[I].status == visited) // The endpoint has been solved, so we can actually draw the livewire
	{
		// Get every point from end to start by looping through the trace data
		QVector<QPoint> pts;
		do
		{
			pts.push_back(QPoint(I % this->_w, I / this->_w));
		}
		while ((I = this->_data[I].traceI) != UINT_MAX); // (I = this->_data[I].traceI) != this->_I
		return pts;
	}
	return QVector<QPoint>();
}


void LivewireCalculator::DrawTrace(QPoint end, QPainter &g)
{
	uint I = I(end.x(), end.y());
	if (this->_data[I].status == visited) // The endpoint has been solved, so we can actually draw the livewire
	{
		// Get every point from end to start by looping through the trace data
		QVector<QPoint> pts;
		do
		{
			pts.push_back(QPoint(I % this->_w, I / this->_w));
		}
		while ((I = this->_data[I].traceI) != UINT_MAX); // (I = this->_data[I].traceI) != this->_I

		// Draw the points
		if (pts.size() >= 2)
			g.drawLines(pts);
	}
}
