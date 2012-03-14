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

#ifndef WEIGHT_CALCULATOR_H
#define WEIGHT_CALCULATOR_H

#include "general.h"

#include <QMutex>
#include <QWaitCondition>

#include <QPoint>
#include <QVector>

#include "Threaded.h"
#include "PointPriorityQueue.h"

namespace Livewire
{
	class WeightCalculator : public Threaded
	{
	public:
		enum DataFormat
		{
			GrayscaleByte,
			GrayscaleUShort,
			RGB
		};

		enum CoalescingMethod
		{
			RedChannel, GreenChannel, BlueChannel,
			AvgRGB,
			Luma, Luma601, LumaSMPTE, // Rec. 709, Rec. 601, or SMPTE 240M
			WeightedHSV, WeightedHSL, WeightedHSI // 0.6 * [V or L or I] + 0.3 * H + 0.1 * S
		};
		enum PixelReductionMethod
		{
			NoPixelReduction  = 0x01,
			Median2pxWindow   = 0x12, Median3pxWindow   = 0x13, Median4pxWindow   = 0x14, Median5pxWindow   = 0x15,
			Mean2pxWindow     = 0x22, Mean3pxWindow     = 0x23, Mean4pxWindow     = 0x24, Mean5pxWindow     = 0x25,
			/* same as mean 2x2 */    Gaussian3pxWindow = 0x33, Gaussian4pxWindow = 0x34, Gaussian5pxWindow = 0x35
			// TODO: any noise reduction methods added
		};
		enum NoiseReductionMethod
		{
			NoNoiseReduction        = 0x01,
			MedianFilter3pxWindow   = 0x13, MedianFilter5pxWindow   = 0x15,
			MeanFilter3pxWindow     = 0x23, MeanFilter5pxWindow     = 0x25,
			GaussianFilter3pxWindow = 0x33, GaussianFilter5pxWindow = 0x35
			// TODO: Other RCRS filter, // http://en.wikipedia.org/wiki/Noise_reduction#Nonlinear_filters
			// TODO: k-Nearest Neighbor Filtering, // http://www.anirudh.net/courses/cse585/project1/
			// TODO: AnisotropicDiffusion, // http://en.wikipedia.org/wiki/Anisotropic_diffusion
		};
		enum EdgeDetectionMethod
		{
			NoEdgeDetection = 0x01,
			Sobel = 0x13
			// TODO: Canny, // http://en.wikipedia.org/wiki/Canny_edge_detector http://homepages.inf.ed.ac.uk/rbf/HIPR2/canny.htm
		};
		enum AccentuationMethod
		{
			NoAccentuation,
			Sigmoid
			// TODO: Linear
		};

		struct Settings
		{
			CoalescingMethod Method;
			PixelReductionMethod PixelReduction;
			NoiseReductionMethod NoiseReduction;
			EdgeDetectionMethod EdgeDetection;
			AccentuationMethod Accentuation;
			bool Invert;

			Settings(
				CoalescingMethod Method = BlueChannel,
				PixelReductionMethod PixelReduction = Mean2pxWindow,
				NoiseReductionMethod NoiseReduction = NoNoiseReduction,
				EdgeDetectionMethod EdgeDetection = NoEdgeDetection,
				AccentuationMethod Accentuation = Sigmoid,
				bool Invert = false
				);
		};

		static const Settings GrayScaleSettings;
		static const Settings ColorSettings;

	private:
		Settings _settings;
		uint _filter_overflow;

		const uint _width_raw, _height_raw;
		uint _stride;
		DataFormat _format;
		const byte *_data_raw;

		const uint _width, _height, _scale;
		byte **_data;

		const uint _width_status, _height_status, _last_col_width;
		byte *_status;
		PointPriorityQueue _block_queue;

		QMutex _queue_lock, _status_lock;
		QWaitCondition _blocks_queued, _block_finished;

		// prevent copying
		WeightCalculator(const WeightCalculator&);
		WeightCalculator& operator=(const WeightCalculator&);

	public:
		WeightCalculator(uint w, uint h, const Settings& settings);
		~WeightCalculator();

		void ChangeSettings(const Settings& settings);

		// points in the original size, not reduced
		// points are of the upper-left corner of each calculated block
		// the return value is the size of the blocks
		uint GetBlocksCalculated(QVector<QPoint>& done, QVector<QPoint>& doing, QVector<QPoint>& not_done) const;

		// Gets a weight
		// All indices are of the reduced size
		// x = x coord
		// y = y coord
		// w = (out) the weight
		// returns true if the point is obtainable, false if the point is out of bounds of the calculation region
		// if the requested point is not yet calculated but has been requested this function will block until it is ready
		bool Get(uint x, uint y, byte* w) /*const*/;

		uint GetScale() const;

		uint GetOriginalWidth() const;
		uint GetOriginalHeight() const;

		uint GetReducedWidth() const;
		uint GetReducedHeight() const;

		void SetImage(const byte* imageData, DataFormat format, uint stride);
		void CalculateRegion(uint x, uint y, uint min_room); // indices of the reduced size
		virtual void Stop();
	protected:
		void Run();

	private:
		void CalcBlock(uint x, uint y, uint I);

	};
}

#endif
