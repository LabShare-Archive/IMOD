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

#ifndef LIVEWIRE_H
#define LIVEWIRE_H

typedef unsigned char byte;
typedef unsigned int uint;

#include <stddef.h>

#include "PointPriorityQueue.h"
#include "Threaded.h"
#include "WeightCalculator.h"

#include <QPoint>
#include <QVector>

#ifndef IMOD_PLUGIN
#include <QPainter>
#endif

namespace Livewire
{
	class LivewireCalculator : public Threaded
	{
	private:
/// The livewire algorithm has not reached the pixel</summary>
#define STATUS_UNVISITED	0
/// The livewire algorithm has reached the pixel but not finished calculating it</summary>
#define STATUS_IN_EDGE		1
/// The livewire algorithm has finished calculating the pixel</summary>
#define STATUS_VISITED		2

		/// <summary>The width of the data</summary>
		const uint _w;

		/// <summary>The height of the data</summary>
		const uint _h;

		/// <summary>The scale of the data (amount of binning)</summary>
		const uint _scale;

		/// <summary>The weight calculator backing this livewire calculator</summary>
		const WeightCalculator *_weights_calc;

		/// <summary>The weights used in the livewire algorithm</summary>
		const byte *_weights;
		
		/// <summary>The X coordinate of the point at which the livewire data is calculated for</summary>
		uint _x;
		
		/// <summary>The Y coordinate of the point at which the livewire data is calculated for</summary>
		uint _y;

		/// <summary>The point at which the livewire data is calculated for (in the form x+y*w)</summary>
		uint _I;

		/// <summary>The livewire algorithm status for every pixel</summary>
		byte *_status;

		/// <summary>The livewire algorithm trace: the point which led to the pixel's best score (in the form x+y*w)</summary>
		uint *_trace;

		/// <summary>The priority queue representing the "InEdge" points used for calculating the data, which we only want to allocated once</summary>
		PointPriorityQueue _edge;

	public:
		/// <summary>Create a livewire calculator object from the given weights.</summary>
		/// <param name="weights">The weights object, which includes the width and height of the output data.</param>
		/// <remarks>The weight matrix doesn't have to be done calculating when given to the livewire calculator.</remarks>
		LivewireCalculator(WeightCalculator *weights);
		~LivewireCalculator();

#ifndef IMOD_PLUGIN
		/// <summary>
		/// Draws the livewire trace using all of the precomputed data.
		/// If the livewire data is not completely computed yet the available livewire data is used if the end point is computed.
		/// This takes only a few milliseconds (&lt;10ms) for almost any image or path size.
		/// </summary>
		/// <param name="x">The X coordinate of the end of the trace</param>
		/// <param name="y">The Y coordinate of the end of the trace</param>
		/// <param name="painter">The painter to use</param>
		void DrawTrace(uint x, uint y, QPainter &painter);
#endif

		QVector<QPoint> GetTrace(uint x, uint y);

		/// <summary>Starts the livewire-calculating thread for the given point, see prepareLivewire for the function that actually does the work</summary>
		/// <param name="x">The X coordinate at which to calculate the livewire data for, it is given a score of 0</param>
		/// <param name="y">The Y coordinate at which to calculate the livewire data for, it is given a score of 0</param>
		void Start(uint x, uint y);

	protected:
		/// <summary>Run the livewire LivewireCalculator</summary>
		void Run();

	private:
		void CalcPoint(const uint x, const uint y, const uint i, const bool diagonal, const uint I, const uint S);
	};
}

#endif
