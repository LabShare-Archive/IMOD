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

#include "LivewireCalculator.h"

#ifndef IMOD_PLUGIN
//#define SAVE_SCORE_IMAGE
#endif

#ifdef SAVE_SCORE_IMAGE
#include "BitmapWriter.h"
#endif

#define SQRT2		1.41421356237309504880168872420969807856967187537694807317667973799

#define IW(x, y, w)	((x) + (y) * (w))
#define I(x, y)		IW(x, y, this->_w)

using namespace Livewire;

LivewireCalculator::LivewireCalculator(WeightCalculator *weights) : Threaded("Livewire Calculator"),
	_weights(weights), _w(weights->GetReducedWidth()), _h(weights->GetReducedHeight()), _scale(weights->GetScale()), _visited(_w, _h), _edge(_w, _h), _trace(_w, _h)
{
	this->SetTotalProgress(this->_w * this->_h);
}
LivewireCalculator::~LivewireCalculator()
{
	this->Stop(true);
}

void LivewireCalculator::Start(uint x, uint y, uint min_room)
{
	this->_x = x / this->_scale;
	this->_y = y / this->_scale;
	this->_min_room = ScaleBack(min_room, this->_scale);
	Threaded::Start();
}

void LivewireCalculator::Run()
{
	const uint w = this->_w, w1 = w - 1, h = this->_h, h1 = h - 1;

	// Reset data structures
	this->_visited.Clear();
	this->_edge.Clear();
	this->_trace.Clear();

#ifdef SAVE_SCORE_IMAGE
	SparseMatrix<uint> scores(w, h);
#endif

	// Start calculating weights
	this->_weights->CalculateRegion(this->_x, this->_y, this->_min_room);

	// Start with just the start point
	uint X = this->_x, Y = this->_y, I, S;
	this->_edge.Enqueue(X, Y, IW(this->_x, this->_y, w), 0);
	this->_trace.Set(X, Y, UINT_MAX);

	// Loop until stopped or there are no more points
	while (this->IsExecuting() && this->_edge.Dequeue(X, Y, I, S))
	{
		// Mark the best-scoring point as visited (done being calculated)
		this->_visited.Set(X, Y, true);
#ifdef SAVE_SCORE_IMAGE
		scores.Set(X, Y, S);
#endif

		if (X && X < w1 && Y && Y < h1)
		{
#define CALC_POINT(dx, dy, diag)	this->CalcPoint(X + dx, Y + dy, I + dy*w + dx, diag, I, S)
			// entire 3x3 neighborhood
			CALC_POINT(-1, -1, true );
			CALC_POINT( 0, -1, false);
			CALC_POINT(+1, -1, true );
			CALC_POINT(-1,  0, false);
			CALC_POINT(+1,  0, false);
			CALC_POINT(-1, +1, true );
			CALC_POINT( 0, +1, false);
			CALC_POINT(+1, +1, true );
#undef CALC_POINT
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

	SparseMatrix<uint> dscores(w, h);
	for (uint i = 0; i < wh; ++i)
	{
		// TODO: update for use with sparse matrix
		uint I = i, Y = i / w, X = i - Y * w;
		double dist = 0;
		while (I != this->_I && I != 0 && dist < 10000)
		{
			uint i = this->_trace[I], y = i / w, x = i - y * w;
			bool diagonal = ((((X-x) + (Y-y)) & 1) == 0);
			dist += diagonal ? SQRT2 : 1;
			I = IW(X = x, Y = y, w);
		}
		dscores[i] = (uint)(scores[i] / dist);
	}
	WriteBitmap(dscores, w, h, "dscores");
#endif
}

inline void LivewireCalculator::CalcPoint(const uint x, const uint y, const uint i, const bool diagonal, const uint I, const uint S)
{
	// Make sure the point isn't already done or one that won't have the weights calculated
	byte weight;
	if (!this->_visited.Get(x, y) && this->_weights->Get(x, y, &weight))
	{
		// Calculate the score for this point
		const uint s = S + (diagonal ? (uint)(SQRT2 * weight) : weight);

		// Add the point to the edge or update its score
		if (!this->_edge.Contains(x, y))
		{
			this->_edge.Enqueue(x, y, i, s);
			this->_trace.Set(x, y, I);
		}
		else if (this->_edge.DescreaseScore(x, y, s))
		{
			this->_trace.Set(x, y, I);
		}
	}
}

QVector<QPoint> LivewireCalculator::GetTrace(uint x, uint y)
{
	const uint scale = this->_scale, s2 = scale / 2;
	QVector<QPoint> pts;
	x /= scale;
	y /= scale;
	if (this->_visited.Get(x, y)) // The endpoint has been solved, so we can actually draw the livewire
	{
		// Get every point from end to start by looping through the trace data
		uint I = I(x, y);
		do
		{
			x = I % this->_w;
			y = I / this->_w;
			pts.push_back(QPoint(x * scale + s2, y * scale + s2));
		}
		while ((I = this->_trace.Get(x, y)) != UINT_MAX);
	}
	return pts;
}

#ifndef IMOD_PLUGIN
void LivewireCalculator::DrawTrace(uint x, uint y, QPainter &g)
{
	const uint scale = this->_scale, s2 = scale / 2;
	x /= scale;
	y /= scale;
	if (this->_visited.Get(x, y)) // The endpoint has been solved, so we can actually draw the livewire
	{
		// Get every point from end to start by looping through the trace data
		QVector<QPoint> pts;
		uint I = I(x, y);
		do
		{
			x = I % this->_w;
			y = I / this->_w;
			QPoint p = QPoint(x * scale + s2, y * scale + s2);
			pts.push_back(p);
			pts.push_back(p);
		}
		while ((I = this->_trace.Get(x, y)) != UINT_MAX);

		// Draw the points
		if (pts.size() >= 4)
		{
			pts.pop_back();
			pts.pop_front();
			g.drawLines(pts);
		}
	}
}
#endif
