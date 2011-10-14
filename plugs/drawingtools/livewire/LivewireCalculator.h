#ifndef LIVEWIRE_H
#define LIVEWIRE_H
#include <stddef.h>

typedef unsigned char byte;
typedef unsigned int uint;

#include "PointPriorityQueue.h"
#include "Threaded.h"

#include <QPoint>
#include <QPainter>
#include <QVector>

namespace Livewire
{
	class LivewireCalculator : public Threaded
	{
	private:
		/// <summary>The status of the livewire algorithm for a pixel</summary>
		enum Status
		{
			/// <summary>The livewire algorithm has not reached the pixel</summary>
			unvisited = 0,

			/// <summary>The livewire algorithm has reached the pixel but not finished calculating it</summary>
			inEdge,

			/// <summary>The livewire algorithm has finished calculating the pixel</summary>
			visited
		};

		/// <summary>A single pixel in the livewire algorithm</summary>
		struct Pixel
		{
			/// <summary>The livewire algorithm status for the pixel</summary>
			Status status;

			/// <summary>The point which led to the pixel's best score (in the form x+y*w)</summary>
			uint traceI;
		};

		/// <summary>The width of the data</summary>
		const uint _w;

		/// <summary>The height of the data</summary>
		const uint _h;

		/// <summary>The total number of pixels in the data (width * height)</summary>
		const uint _wh;

		/// <summary>The weights used in the livewire algorithm</summary>
		const byte *_weights;

		/// <summary>The point at which the livewire data is calculated for</summary>
		QPoint _start;
		
		/// <summary>The X coordinate of the point at which the livewire data is calculated for</summary>
		uint _x;
		
		/// <summary>The Y coordinate of the point at which the livewire data is calculated for</summary>
		uint _y;

		/// <summary>The point at which the livewire data is calculated for (in the form x+y*w)</summary>
		uint _I;

		/// <summary>The livewire algorithm data</summary>
		Pixel *_data;

		/// <summary>The priority queue representing the "InEdge" points used for calculating the data, which we only want to allocated once</summary>
		PointPriorityQueue _edge;

	public:
		/// <summary>Create a livewire calculator object from the given weights.</summary>
		/// <param name="w">The width of the weight data</param>
		/// <param name="h">The height of the weight data</param>
		/// <param name="weights">The weights matrix</param>
		/// <remarks>The weight matrix doesn't have to be filled in completely since it is used as a reference.</remarks>
		LivewireCalculator(uint w, uint h, byte *weights);
		~LivewireCalculator();

		/// <summary>
		/// Draws the livewire trace using all of the precomputed data.
		/// If the livewire data is not completely computed yet the available livewire data is used if the end point is computed.
		/// This takes only a few milliseconds (&lt;10ms) for almost any image or path size.
		/// </summary>
		/// <param name="end">The end of the trace</param>
		/// <param name="painter">The painter to use</param>
		void DrawTrace(QPoint end, QPainter &painter);
		
		QVector<QPoint> GetTrace(QPoint end);

		/// <summary>Starts the livewire-calculating thread for the given point, see prepareLivewire for the function that actually does the work</summary>
		/// <param name="start">The point at which to calculate the livewire data for, it is given a score of 0</param>
		void Start(QPoint start);

	protected:
		/// <summary>Run the livewire LivewireCalculator</summary>
		void Run();

	private:
		void CalcPoint(const uint x, const uint y, const uint i, const bool diagonal, const uint I, const uint S);
	};
}

#endif
