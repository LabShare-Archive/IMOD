#include "WeightCalculator.h"

#include "Colors.h"

#include <QRgb>

#include <stdlib.h>
#include <math.h>

//#define SAVE_WEIGHT_IMAGE

#ifdef SAVE_WEIGHT_IMAGE
#include "BitmapWriter.h"
#endif

#define CASSERT(b)	typedef char __C_ASSERT__[b]

#define MIN(a, b)	(((a) < (b)) ? (a) : (b))
#define MAX(a, b)	(((a) > (b)) ? (a) : (b))

using namespace Livewire;

#pragma region Filter Helpers

typedef byte PixelFilterCB(byte p, void *param);
inline static void PixelFilter(const uint w, const uint h, const byte *in, byte *out, PixelFilterCB *cb, void *param)
{
	const uint wh = w*h;
	for (uint I = 0; I < wh; ++I)
		out[I] = cb(in[I], param);
}

typedef byte WindowFilterCBFull(const byte **window, void *param);
typedef byte WindowFilterCB(const byte **window, uint w, uint h, uint cx, uint cy, void *param);
template <uint windowSize> // windowSize must be odd and greater than 1
static void WindowFilter(const uint w, const uint h, const byte *in, byte *out, WindowFilterCBFull *cb_full, WindowFilterCB *cb, void *param)
{
	// Make sure that the window size is >1 and odd
	CASSERT(windowSize != 1 && (windowSize & 1) == 1);

	static const uint ws2 = windowSize * windowSize, WS1 = windowSize - 1, ws = (windowSize - 1) / 2, ws1 = ws + 1;
	const uint w1 = w - 1, w_ws = w - ws, ws_w = w + ws, h1 = h - 1, h_ws = h - ws, ws_h = h + ws;
	const byte *window[windowSize];

	uint X, Y, Yw, I;

	// TODO: Optimize

#define ADVANCE_X(N) for (uint y = 0; y < N; ++y) ++window[y];

	for (uint y = 0, i = 0; y < WS1; ++y, i += w) window[y] = in + i;

	// missing left columns and top rows
	for (X = 0; X < ws; ++X)		{ for (Y = 0, I = X; Y < ws; ++Y, I += w) out[I] = cb(window, ws1+X, ws1+Y, X, Y, param); }

	// missing no columns and top rows
	for (/*X = ws*/; X < w_ws; ++X)	{ for (Y = 0, I = X; Y < ws; ++Y, I += w) out[I] = cb(window, windowSize, ws1+Y, ws, Y, param);	ADVANCE_X(WS1); }
	
	// missing right columns and top rows
	for (/*X = w_ws*/; X < w1; ++X)	{ for (Y = 0, I = X; Y < ws; ++Y, I += w) out[I] = cb(window, ws_w-X, ws1+Y, ws, Y, param);		ADVANCE_X(WS1); }
	/*X = w1*/ for (Y = 0, I = w1; Y < ws; ++Y, I += w) out[I] = cb(window, ws1, ws1+Y, ws, Y, param);

	for (/*Y = ws,*/ I = ws*w, Yw = 0; Y < h_ws; ++Y, Yw += w)
	{
		for (uint y = 0, i = Yw; y < windowSize; ++y, i += w) window[y] = in + i;

		// missing left columns and no rows
		for (X = 0; X < ws; ++X)		{ out[I++] = cb(window, ws1+X, windowSize, X, ws, param); }

		// full neighborhood
		for (/*X = ws*/; X < w_ws; ++X)	{ out[I++] = cb_full(window, param);						ADVANCE_X(windowSize); }

		// missing right columns and no rows
		for (/*X = w_ws*/; X < w1; ++X)	{ out[I++] = cb(window, ws_w-X, windowSize, ws, ws, param);	ADVANCE_X(windowSize); }
		out[I++] = cb(window, ws1, windowSize, ws, ws, param);
	}

	for (uint y = 0, i = Yw; y < WS1; ++y, i += w) window[y] = in + i;
	Yw = I;

	// missing left columns and bottom rows
	for (X = 0; X < ws; ++X)		{ for (Y = h_ws, I = X+Yw; Y < h; ++Y, I += w) out[I] = cb(window, ws1+X, ws_h-Y, X, ws, param); }

	// missing no columns and bottom rows
	for (/*X = ws*/; X < w_ws; ++X)	{ for (Y = h_ws, I = X+Yw; Y < h; ++Y, I += w) out[I] = cb(window, windowSize, ws_h-Y, ws, ws, param);	ADVANCE_X(WS1); }
	
	// missing right columns and bottom rows
	for (/*X = w_ws*/; X < w1; ++X)	{ for (Y = h_ws, I = X+Yw; Y < h; ++Y, I += w) out[I] = cb(window, ws_w-X, ws_h-Y, ws, ws, param);		ADVANCE_X(WS1); }
	/*X = w1*/ for (Y = h_ws, I = w1+Yw; Y < h; ++Y, I += w) out[I] = cb(window, ws1, ws_h-Y, ws, ws, param);
}
//template <>
//static void WindowFilter<1>(uint w, uint h, const byte *in, byte *out, WindowFilterCBFull *cb_full, WindowFilterCB *cb, void *param)
//{
//	PixelFilter(w, h, in, out, cb_full, param);
//}
template <>
static void WindowFilter<3>(const uint w, const uint h, const byte *in, byte *out, WindowFilterCBFull *cb_full, WindowFilterCB *cb, void *param)
{
	// Specialize the most common and smallest window filter

	static const uint ws2 = 9, ws = 1;
	const uint w1 = w - 1, h1 = h - 1;
	const byte *window[3];

	uint X, Y, Yw, I = 0;

	window[0] = in;
	window[1] = in + w;
	out[I++] = cb(window, 2, 2, 0, 0, param);																	// missing left columns and top rows
	for (X = 1; X < w1; ++X) { out[I++] = cb(window, 3, 2, 1, 0, param); ++window[0]; ++window[1]; }			// missing no columns and top rows
	out[I++] = cb(window, 2, 2, 1, 0, param);																	// missing right columns and top rows

	for (Y = 1, Yw = w; Y < h1; ++Y, Yw += w)
	{
		window[0] = in + Yw - w;
		window[1] = in + Yw;
		window[2] = in + Yw + w;
		out[I++] = cb(window, 2, 3, 0, 1, param);																// missing left columns and no rows
		for (X = 1; X < w1; ++X) { out[I++] = cb_full(window, param); ++window[0]; ++window[1]; ++window[2]; }	// full neighborhood
		out[I++] = cb(window, 2, 3, 1, 1, param);																// missing right columns and no rows
	}
	
	window[0] = in + Yw - w;
	window[1] = in + Yw;
	out[I++] = cb(window, 2, 2, 0, 1, param);																	// missing left columns and bottom rows
	for (X = 1; X < w1; ++X) { out[I++] = cb(window, 3, 2, 1, 1, param); ++window[0]; ++window[1]; }			// missing no columns and bottom rows
	out[I++] = cb(window, 2, 2, 1, 1, param);																	// missing right columns and bottom rows
}

template<uint windowSize> struct Filter { /*static const uint Matrix[windowSize][windowSize], Total;*/ };

#pragma endregion

#pragma region Coalescing Functions
template <uint channel>
static void CoalesceChannel(const uint w, const uint h, const uint stride, const QRgb *in, byte *out)
{
	static const uint shift = channel * 8;
	const uint _stride = stride / sizeof(QRgb);
	for (const byte *out_end = out + w*h; out < out_end; out += w, in += _stride)
		for (uint x = 0; x < w; ++x)
			out[x] = (in[x] >> shift) & 0xFF;

}
#define CoalesceRed(w, h, stride, in, out)		CoalesceChannel<2>(w, h, stride, in, out)
#define CoalesceGreen(w, h, stride, in, out)	CoalesceChannel<1>(w, h, stride, in, out)
#define CoalesceBlue(w, h, stride, in, out)		CoalesceChannel<0>(w, h, stride, in, out)

template <uint r, uint g, uint b>
static void CoalesceWeightedAvgRGB(const uint w, const uint h, const uint stride, const QRgb *in, byte *out)
{
	for (const byte *out_end = out + w*h; out < out_end; out += w, in += stride)
		for (uint x = 0; x < w; ++x)
		{
			QRgb rgb = in[x];
			out[x] = (r * qRed(rgb) + g * qGreen(rgb) + b * qBlue(rgb)) / (r + g + b);
		}
}

#define CoalesceAvgRGB(w, h, stride, in, out)		CoalesceWeightedAvgRGB<   1,   1,   1>(w, h, stride, in, out)
#define CoalesceLuma(w, h, stride, in, out)			CoalesceWeightedAvgRGB<2126,7152, 722>(w, h, stride, in, out)
#define CoalesceLuma601(w, h, stride, in, out)		CoalesceWeightedAvgRGB< 299, 587, 114>(w, h, stride, in, out)
#define CoalesceLumaSMPTE(w, h, stride, in, out)	CoalesceWeightedAvgRGB< 212, 701,  87>(w, h, stride, in, out)

/*
template <void conv(byte R, byte G, byte B, byte& H, byte& S, byte& X)>
static void CoalesceWeightedHSX(const uint w, const uint h, const uint stride, const QRgb *in, byte *out)
{
	for (const byte *out_end = out + w*h; out < out_end; out += w, in += stride)
		for (uint x = 0; x < w; ++x)
		{
			QRgb rgb = in[x];
			byte H, S, X;
			conv(qRed(rgb), qGreen(rgb), qBlue(rgb), H, S, X);
			out[x] = 0.6 * X + 0.3 * H + 0.1 * S;
		}
}

#define CoalesceWeightedHSV(w, h, stride, in, out)	CoalesceWeightedHSX<RGBtoHSV>(w, h, stride, in, out)
#define CoalesceWeightedHSL(w, h, stride, in, out)	CoalesceWeightedHSX<RGBtoHSL>(w, h, stride, in, out)
#define CoalesceWeightedHSI(w, h, stride, in, out)	CoalesceWeightedHSX<RGBtoHSI>(w, h, stride, in, out)
*/

#pragma endregion

#pragma region Noise Reduction Functions
int byte_comp(const void *a, const void *b) { return *(byte*)a-*(byte*)b; }
template <uint windowSize>
static byte MedianFilterCBFull(const byte **window, void *param)
{
	static const uint ws2 = windowSize*windowSize, ws2_2 = ws2 / 2;
	byte *list = (byte*)param;
	for (uint y = 0, i = 0; y < windowSize; ++y, i += windowSize)
		memcpy(list+i, window[y], windowSize);
	qsort(list, ws2, 1, &byte_comp);
	return list[ws2_2];
}
static byte MedianFilterCB(const byte **window, uint w, uint h, uint cx, uint cy, void *param)
{
	const uint count = w*h, half = count / 2;
	byte *list = (byte*)param;
	for (uint y = 0, i = 0; y < h; ++y, i += w)
		memcpy(list+i, window[y], w);
	qsort(list, count, 1, &byte_comp);
	return (((w & 1) == 0) || ((h & 1) == 0)) ? (byte)(((uint)list[half - 1] + list[half]) / 2) : list[half];
}

template <uint windowSize> // windowSize must be odd and greater than 1
inline static void RunMedianFilter(const uint w, const uint h, const byte *in, byte *out)
{
	static const uint ws2 = windowSize * windowSize;
	byte list[ws2];
	WindowFilter<windowSize>(w, h, in, out, &MedianFilterCBFull<windowSize>, &MedianFilterCB, list);
}

template<uint windowSize>
static byte MeanFilterCBFull(const byte **window, void*)
{
	static const uint ws2 = windowSize * windowSize;
	uint v = 0;
	for (uint y = 0; y < windowSize; ++y)
		for (uint x = 0; x < windowSize; ++x)
			v += window[y][x];
	return (byte)(v / ws2);
}
static byte MeanFilterCB(const byte **window, uint w, uint h, uint cx, uint cy, void*)
{
	uint v = 0;
	for (uint y = 0; y < h; ++y)
		for (uint x = 0; x < w; ++x)
			v += window[y][x];
	return (byte)(v / (w * h));
}
template<uint windowSize>
inline static void RunMeanFilter(const uint w, const uint h, const byte *in, byte *out)
{
	WindowFilter<windowSize>(w, h, in, out, &MeanFilterCBFull<windowSize>, &MeanFilterCB, NULL);
}


// Gaussian Filter Generator in Python:
//def G(x,y,sigma): return exp(-(multiply(x,x)+multiply(y,y))/(2.0*sigma*sigma))/(2.0*pi*sigma*sigma)
//def X(l): return ones((l, l))*[x for x in range(-floor(l/2.0),ceil(l/2.0))]
//def Y(l): return ones((l, l))*[[x] for x in range(floor(l/2.0),-ceil(l/2.0),-1)]
//def GF(l,sigma): M = G(X(l),Y(l),sigma); M = around(M/M[0,0]).astype(int); return M,sum(M);
// Use it like GF(3,1) to make a 3x3 filter with stddev=1
// Recommendations:
//  need at most 3*sigma boxes from the origin (or ceil(6*sigma) window size)

/*
template<uint windowSize> struct GaussianFilter : Filter<windowSize> { static const uint Matrix[windowSize][windowSize], Total; };

#define GAUSSIAN_FILTER(N, T) const uint GaussianFilter<N>::Total = T, GaussianFilter<N>::Matrix[N][N]

// Gaussian 3x3 with stddev = 1
GAUSSIAN_FILTER(3, 15) =	  {	{ 1,  2,  1},
								{ 2,  3,  2},
								{ 1,  2,  1}, };

// Gaussian 5x5 with stddev = 1
GAUSSIAN_FILTER(5, 331) =	  {	{ 1,  4,  7,  4,  1},
								{ 4, 20, 33, 20,  4},
								{ 7, 33, 55, 33,  7},
								{ 4, 20, 33, 20,  4},
								{ 1,  4,  7,  4,  1}, };
*/
// Gaussian 7x7 with stddev = 1
//GAUSSIAN_FILTER(7, 50887) = {	{  1,   12,   55,   90,   55,   12,  1},
//								{ 12,  148,  665, 1097,  665,  148, 12},
//								{ 55,  665, 2981, 4915, 2981,  665, 55},
//								{ 90, 1097, 4915, 8103, 4915, 1097, 90},
//								{ 55,  665, 2981, 4915, 2981,  665, 55},
//								{ 12,  148,  665, 1097,  665,  148, 12},
//								{  1,   12,   55,   90,   55,   12,  1}, };
/*
//Gaussian 9x9 with stddev = 1.5
GAUSSIAN_FILTER(9, 17242) = 
{{   1,    5,   14,   28,   35,   28,   14,    5,    1},
 {   5,   22,   68,  133,  166,  133,   68,   22,    5},
 {  14,   68,  207,  403,  504,  403,  207,   68,   14},
 {  28,  133,  403,  786,  981,  786,  403,  133,   28},
 {  35,  166,  504,  981, 1226,  981,  504,  166,   35},
 {  28,  133,  403,  786,  981,  786,  403,  133,   28},
 {  14,   68,  207,  403,  504,  403,  207,   68,   14},
 {   5,   22,   68,  133,  166,  133,   68,   22,    5},
 {   1,    5,   14,   28,   35,   28,   14,    5,    1}};
*/
/*
//Gaussian 11x11 with stddev = 1.8
GAUSSIAN_FILTER(11, 45508) = 
{{   1,    4,   12,   26,   41,   47,   41,   26,   12,    4,    1},
 {   4,   16,   47,  102,  163,  190,  163,  102,   47,   16,    4},
 {  12,   47,  140,  302,  480,  560,  480,  302,  140,   47,   12},
 {  26,  102,  302,  653, 1037, 1210, 1037,  653,  302,  102,   26},
 {  41,  163,  480, 1037, 1648, 1923, 1648, 1037,  480,  163,   41},
 {  47,  190,  560, 1210, 1923, 2244, 1923, 1210,  560,  190,   47},
 {  41,  163,  480, 1037, 1648, 1923, 1648, 1037,  480,  163,   41},
 {  26,  102,  302,  653, 1037, 1210, 1037,  653,  302,  102,   26},
 {  12,   47,  140,  302,  480,  560,  480,  302,  140,   47,   12},
 {   4,   16,   47,  102,  163,  190,  163,  102,   47,   16,    4},
 {   1,    4,   12,   26,   41,   47,   41,   26,   12,    4,    1}};
*/
/*
//Gaussian 13x13 with stddev = 2.1
GAUSSIAN_FILTER(13, 96894) = 
{{   1,    3,   10,   21,   38,   53,   59,   53,   38,   21,   10,    3,    1},
 {   3,   12,   34,   74,  131,  184,  206,  184,  131,   74,   34,   12,    3},
 {  10,   34,   93,  206,  363,  511,  572,  511,  363,  206,   93,   34,   10},
 {  21,   74,  206,  456,  804, 1129, 1265, 1129,  804,  456,  206,   74,   21},
 {  38,  131,  363,  804, 1417, 1991, 2230, 1991, 1417,  804,  363,  131,   38},
 {  53,  184,  511, 1129, 1991, 2798, 3133, 2798, 1991, 1129,  511,  184,   53},
 {  59,  206,  572, 1265, 2230, 3133, 3510, 3133, 2230, 1265,  572,  206,   59},
 {  53,  184,  511, 1129, 1991, 2798, 3133, 2798, 1991, 1129,  511,  184,   53},
 {  38,  131,  363,  804, 1417, 1991, 2230, 1991, 1417,  804,  363,  131,   38},
 {  21,   74,  206,  456,  804, 1129, 1265, 1129,  804,  456,  206,   74,   21},
 {  10,   34,   93,  206,  363,  511,  572,  511,  363,  206,   93,   34,   10},
 {   3,   12,   34,   74,  131,  184,  206,  184,  131,   74,   34,   12,    3},
 {   1,    3,   10,   21,   38,   53,   59,   53,   38,   21,   10,    3,    1}};
*/
/*
// Gaussian 15x15 with stddev = 2.5
GAUSSIAN_FILTER(15, 10) = //99272
{{   1,    3,    7,   14,   25,   37,   47,   50,   47,   37,   25,   14,    7,    3,    1},
 {   3,    8,   19,   40,   69,  104,  132,  143,  132,  104,   69,   40,   19,    8,    3},
 {   7,   19,   47,   96,  167,  250,  317,  344,  317,  250,  167,   96,   47,   19,    7},
 {  14,   40,   96,  196,  344,  513,  652,  706,  652,  513,  344,  196,   96,   40,   14},
 {  25,   69,  167,  344,  602,  898, 1141, 1236, 1141,  898,  602,  344,  167,   69,   25},
 {  37,  104,  250,  513,  898, 1339, 1703, 1845, 1703, 1339,  898,  513,  250,  104,   37},
 {  47,  132,  317,  652, 1141, 1703, 2165, 2345, 2165, 1703, 1141,  652,  317,  132,   47},
 {  50,  143,  344,  706, 1236, 1845, 2345, 2540, 2345, 1845, 1236,  706,  344,  143,   50},
 {  47,  132,  317,  652, 1141, 1703, 2165, 2345, 2165, 1703, 1141,  652,  317,  132,   47},
 {  37,  104,  250,  513,  898, 1339, 1703, 1845, 1703, 1339,  898,  513,  250,  104,   37},
 {  25,   69,  167,  344,  602,  898, 1141, 1236, 1141,  898,  602,  344,  167,   69,   25},
 {  14,   40,   96,  196,  344,  513,  652,  706,  652,  513,  344,  196,   96,   40,   14},
 {   7,   19,   47,   96,  167,  250,  317,  344,  317,  250,  167,   96,   47,   19,    7},
 {   3,    8,   19,   40,   69,  104,  132,  143,  132,  104,   69,   40,   19,    8,    3},
 {   1,    3,    7,   14,   25,   37,   47,   50,   47,   37,   25,   14,    7,    3,    1}};
*/
/*template<uint windowSize>
static byte GaussianFilterCBFull(const byte **window, void*)
{
	// TODO: There are faster ways? http://homepages.inf.ed.ac.uk/rbf/HIPR2/gsmooth.htm http://en.wikipedia.org/wiki/Gaussian_blur#Mechanics
	uint v = 0;
	for (uint y = 0; y < windowSize; ++y)
		for (uint x = 0; x < windowSize; ++x)
			v += GaussianFilter<windowSize>::Matrix[x][y] * window[y][x];
	return (byte)(v / GaussianFilter<windowSize>::Total);
}
template<uint windowSize>
static byte GaussianFilterCB(const byte **window, uint w, uint h, uint cx, uint cy, void*)
{
	static const uint ws = (windowSize - 1) / 2;
	const uint ws_cx = ws-cx, ws_cy = ws-cy;

	uint v = 0, t = 0;
	for (uint y = 0; y < h; ++y)
		for (uint x = 0; x < w; ++x)
		{
			uint m = GaussianFilter<windowSize>::Matrix[x+ws_cx][y+ws_cy];
			v += m * window[y][x];
			t += m;
		}
	return (byte)(v / t);
}
template<uint windowSize>
inline static void RunGaussianFilter(const uint w, const uint h, const byte *in, byte *out)
{
	WindowFilter<windowSize>(w, h, in, out, &GaussianFilterCBFull<windowSize>, &GaussianFilterCB<windowSize>, NULL);
}*/

#pragma endregion

#pragma region Edge Detection Functions
static const int sobel_x[3][3] = {	{-1,  0,  1},
									{-2,  0,  2},
									{-1,  0,  1}, };
static const int sobel_y[3][3] = {	{-1, -2, -1},
									{ 0,  0,  0},
									{ 1,  2,  1}, };
static const int sobel_factor = 20;
static byte SobelFilterCBFull(const byte **window, void*)
{
	int Gx = 0, Gy = 0;
	for (uint y = 0; y < 3; ++y)
		for (uint x = 0; x < 3; ++x)
		{
			byte val = window[y][x];
			Gx += sobel_x[x][y] * val;
			Gy += sobel_y[x][y] * val;
		}
	return ~(byte)sqrt((double)((Gx*Gx + Gy*Gy) / sobel_factor));
}
static byte SobelFilterCB(const byte **window, uint w, uint h, uint cx, uint cy, void*)
{
	// TODO: This does quite poorly along image border
	const uint cx_1 = 1-cx, cy_1 = 1-cy;
	int Gx = 0, Gy = 0;
	for (uint y = 0; y < h; ++y)
		for (uint x = 0; x < w; ++x)
		{
			byte val = window[y][x];
			Gx += sobel_x[x+cx_1][y+cy_1] * val;
			Gy += sobel_y[x+cx_1][y+cy_1] * val;
		}
	return ~(byte)sqrt((double)((Gx*Gx + Gy*Gy) / sobel_factor));
}
inline static void RunSobelEdgeDetection(const uint w, const uint h, const byte *in, byte *out)
{
	WindowFilter<3>(w, h, in, out, &SobelFilterCBFull, &SobelFilterCB, NULL);
}
#pragma endregion

#pragma region Accentuation Functions
/// <summary>
/// Calculates a sigmoid function for a single value with the given parameters.
/// Equation:
/// f(x) = floor(max_y / (1 + exp(slope * (halfmax_x - x)));
/// </summary>
/// <param name="x">The value of x to calculate the sigmoid for</param>
/// <param name="halfmax_x">The value of x at which the result is half of the max, below this values will become smaller and above they will become larger</param>
/// <param name="max_y">The maximum value of the result</param>
/// <param name="slope">The slope of the function, smaller values are create a more linear relationship while larger values tend toward a step function</param>
/// <returns>The evaluated sigmoid function value</returns>
inline static uint sigmoid(const uint x, const int halfmax_x, const int max_y, const double slope) { return (uint)(max_y / (1 + exp(slope * (halfmax_x - (int)x)))); }
/// <summary>Calculates a sigmoid function for the given value with a max of 255, half-max at 128 and a slope of 0.05.</summary>
/// <param name="x">The value of x to calculate the sigmoid for</param>
/// <returns>The evaluated sigmoid function value</returns>
inline static byte sigmoid(byte x, void*) { return sigmoid(x, 128, 255, 0.05); }

//inline static double sigmoid(float x, double halfmax_x, double max_y, double slope) { return max_y / (1 + exp(slope * (halfmax_x - x))); }
//inline static double sigmoid(float x) { return sigmoid(x, 0.5, 1.0, 10.0); }

static void RunSigmoidAccentuation(const uint w, const uint h, const byte *in, byte *out)
{
	PixelFilter(w, h, in, out, &sigmoid, NULL);
}
#pragma endregion

inline static void Swap(byte *&a, byte *&b) { byte *x = a; a = b; b = x; }

WeightCalculator::Settings::Settings(CoalescingMethod Method, NoiseReductionMethod NoiseReduction, EdgeDetectionMethod EdgeDetection, AccentuationMethod Accentuation, bool Invert) :
	Method(Method), NoiseReduction(NoiseReduction), EdgeDetection(EdgeDetection), Accentuation(Accentuation), Invert(Invert) {}

//const WeightCalculator::Settings WeightCalculator::GrayScaleSettings;

const WeightCalculator::Settings WeightCalculator::GrayScaleSettings(WeightCalculator::BlueChannel,
	WeightCalculator::NoNoiseReductionMethod, WeightCalculator::NoEdgeDetection, WeightCalculator::NoAccentuation, false);

const WeightCalculator::Settings WeightCalculator::ColorSettings(WeightCalculator::WeightedHSV, WeightCalculator::MedianFilter3pxWindow, WeightCalculator::Sobel, WeightCalculator::NoAccentuation, false);

WeightCalculator::WeightCalculator(uint w, uint h, Settings settings) : Threaded("Weight Calculator"),
	_settings(settings), _width(w), _height(h), _data_raw(NULL), _data(new byte[w*h]), _data_tmp(new byte[w*h])
	{ this->SetTotalProgress(w); }
WeightCalculator::~WeightCalculator() { delete[] this->_data; delete[] this->_data_tmp; }

byte *WeightCalculator::GetWeights() { return this->_data; }

void WeightCalculator::Start(const byte* imageData, DataFormat format, uint stride)
{
	if (format == GrayscaleUShort) // TODO: NOT SUPPORTED YET!
		return;

	this->_stride = stride;
	this->_format = format;
	this->_data_raw = imageData;

	Threaded::Start();
}

void WeightCalculator::Run()
{
	// TODO: make multi-threaded
	// TODO: incorporate this->IsExecuting() and this->IncProgress()
	
	const uint w = this->_width, h = this->_height;
	
	if (this->_format == GrayscaleByte)
	{
		const byte *in = this->_data_raw;
		byte *out = this->_data;
		for (const byte *out_end = out + w*h; out < out_end; out += w, in += this->_stride)
			memcpy(out, in, w);
	}
	else if (this->_format == RGB)
	{
		const QRgb *in = (QRgb*)this->_data_raw;
		switch (this->_settings.Method)
		{
			case RedChannel:	CoalesceRed			(w, h, this->_stride, in, this->_data); break;
			case GreenChannel:	CoalesceGreen		(w, h, this->_stride, in, this->_data); break;
			case BlueChannel:	CoalesceBlue		(w, h, this->_stride, in, this->_data); break;
			case AvgRGB:		CoalesceAvgRGB		(w, h, this->_stride, in, this->_data); break;
			case Luma:			CoalesceLuma		(w, h, this->_stride, in, this->_data); break;
			case Luma601:		CoalesceLuma601		(w, h, this->_stride, in, this->_data); break;
			case LumaSMPTE:		CoalesceLumaSMPTE	(w, h, this->_stride, in, this->_data); break;
			//case WeightedHSV:	CoalesceWeightedHSV	(w, h, this->_stride, in, this->_data); break; // TODO: test
			//case WeightedHSL:	CoalesceWeightedHSL	(w, h, this->_stride, in, this->_data); break; // TODO: test
			//case WeightedHSI:	CoalesceWeightedHSI	(w, h, this->_stride, in, this->_data); break; // TODO: test
		}
	}
	
	switch (this->_settings.NoiseReduction)
	{
		case NoNoiseReductionMethod: break; // do nothing
		case MedianFilter3pxWindow:		RunMedianFilter<3>		(w, h, this->_data, this->_data_tmp); Swap(this->_data, this->_data_tmp); break;
		case MedianFilter5pxWindow:		RunMedianFilter<5>		(w, h, this->_data, this->_data_tmp); Swap(this->_data, this->_data_tmp); break;
		case MeanFilter3pxWindow:		RunMeanFilter<3>		(w, h, this->_data, this->_data_tmp); Swap(this->_data, this->_data_tmp); break;
		case MeanFilter5pxWindow:		RunMeanFilter<5>		(w, h, this->_data, this->_data_tmp); Swap(this->_data, this->_data_tmp); break;
		//case GaussianFilter3pxWindow:	RunGaussianFilter<3>	(w, h, this->_data, this->_data_tmp); Swap(this->_data, this->_data_tmp); break;
		//case GaussianFilter5pxWindow:	RunGaussianFilter<5>	(w, h, this->_data, this->_data_tmp); Swap(this->_data, this->_data_tmp); break;
	}

	switch (this->_settings.EdgeDetection)
	{
		case NoEdgeDetection: break; // do nothing
		case Sobel: RunSobelEdgeDetection(w, h, this->_data, this->_data_tmp); Swap(this->_data, this->_data_tmp); break;
	}
	
	switch (this->_settings.Accentuation)
	{
		case NoAccentuation: break; // do nothing
		case Sigmoid: RunSigmoidAccentuation(w, h, this->_data, this->_data_tmp); Swap(this->_data, this->_data_tmp); break;
	}

	// Loop through all cells calculating the weights for each
	/*for (uint x = 0; x < w && this->IsExecuting(); ++x)
	{
		for (uint y = 0; y < h; ++y) {}
		this->IncProgress();
	}*/

	if (this->_settings.Invert)
	{
		const uint wh = w*h;
		for (uint I = 0; I < wh; ++I)
			this->_data[I] = ~this->_data[I];
	}

#ifdef SAVE_WEIGHT_IMAGE
	this->Checkpoint("saving weights image");
	WriteBitmap(this->_data, w, h, "weights");
#endif
}
