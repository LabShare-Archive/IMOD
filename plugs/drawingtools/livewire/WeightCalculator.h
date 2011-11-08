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

typedef unsigned char byte;
typedef unsigned int uint;

#include <stddef.h>

#include "Threaded.h"

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
			WeightedHSV, WeightedHSL, WeightedHSI, // 0.6 * [V or L or I] + 0.3 * H + 0.1 * S
		};
		enum PixelReductionMethod
		{
			NoPixelReduction  = 0x01,
			Median2pxWindow   = 0x12, Median3pxWindow   = 0x13, Median4pxWindow   = 0x14, Median5pxWindow   = 0x15,
			Mean2pxWindow     = 0x22, Mean3pxWindow     = 0x23, Mean4pxWindow     = 0x24, Mean5pxWindow     = 0x25,
			/* same as mean 2x2 */    Gaussian3pxWindow = 0x33, Gaussian4pxWindow = 0x34, Gaussian5pxWindow = 0x35,
			// TODO: any noise reduction methods added
		};
		enum NoiseReductionMethod
		{
			NoNoiseReduction        = 0x01,
			MedianFilter3pxWindow   = 0x13, MedianFilter5pxWindow   = 0x15,
			MeanFilter3pxWindow     = 0x23, MeanFilter5pxWindow     = 0x25,
			GaussianFilter3pxWindow = 0x33, GaussianFilter5pxWindow = 0x35,
			// TODO: Other RCRS filter, // http://en.wikipedia.org/wiki/Noise_reduction#Nonlinear_filters
			// TODO: k-Nearest Neighbor Filtering, // http://www.anirudh.net/courses/cse585/project1/
			// TODO: AnisotropicDiffusion, // http://en.wikipedia.org/wiki/Anisotropic_diffusion
		};
		enum EdgeDetectionMethod
		{
			NoEdgeDetection,
			Sobel,
			// TODO: Canny, // http://en.wikipedia.org/wiki/Canny_edge_detector http://homepages.inf.ed.ac.uk/rbf/HIPR2/canny.htm
		};
		enum AccentuationMethod
		{
			NoAccentuation,
			Sigmoid,
			// TODO: Linear
		};

		struct Settings
		{
			const CoalescingMethod Method;
			const PixelReductionMethod PixelReduction;
			const NoiseReductionMethod NoiseReduction;
			const EdgeDetectionMethod EdgeDetection;
			const AccentuationMethod Accentuation;
			const bool Invert;

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
		const Settings _settings;

		const uint _width_raw, _height_raw;
		uint _stride;
		DataFormat _format;
		const byte *_data_raw;

		const uint _width, _height, _scale;
		byte *_data;

	public:
		WeightCalculator(uint w, uint h, Settings settings);
		~WeightCalculator();

		const byte *GetWeights() const;

		uint GetScale() const;

		uint GetOriginalWidth() const;
		uint GetOriginalHeight() const;

		uint GetReducedWidth() const;
		uint GetReducedHeight() const;

		void Start(const byte* imageData, DataFormat format, uint stride);

	protected:
		void Run();
	};
}

#endif
