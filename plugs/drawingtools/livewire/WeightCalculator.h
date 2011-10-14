#ifndef WEIGHT_CALCULATOR_H
#define WEIGHT_CALCULATOR_H
#include <stddef.h>

typedef unsigned char byte;
typedef unsigned int uint;

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
		enum NoiseReductionMethod
		{
			NoNoiseReductionMethod,
			MedianFilter3pxWindow,   MedianFilter5pxWindow,
			MeanFilter3pxWindow,     MeanFilter5pxWindow,
			GaussianFilter3pxWindow, GaussianFilter5pxWindow,
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
			const NoiseReductionMethod NoiseReduction;
			const EdgeDetectionMethod EdgeDetection;
			const AccentuationMethod Accentuation;
			const bool Invert;

			Settings(
				CoalescingMethod Method = BlueChannel,
				NoiseReductionMethod NoiseReduction = MedianFilter3pxWindow,
				EdgeDetectionMethod EdgeDetection = NoEdgeDetection,
				AccentuationMethod Accentuation = Sigmoid,
				bool Invert = false
				);
		};

		static const Settings GrayScaleSettings;
		static const Settings ColorSettings;

	private:
		Settings _settings;

		uint _width;

		uint _height;

		uint _stride;

		DataFormat _format;

		const byte *_data_raw;

		byte *_data;

		byte *_data_tmp;

	public:
		WeightCalculator(uint w, uint h, Settings settings);
		~WeightCalculator();

		byte *GetWeights();

		void Start(const byte* imageData, DataFormat format, uint stride);

	protected:
		void Run();
	};
}

#endif
