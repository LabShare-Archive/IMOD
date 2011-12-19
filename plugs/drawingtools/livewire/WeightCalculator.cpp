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

#include "WeightCalculator.h"

#include "Colors.h"

#include <QRgb>

#include <stdlib.h>
#include <math.h>

#ifndef IMOD_PLUGIN
//#define SAVE_WEIGHT_IMAGE
#endif

#ifdef SAVE_WEIGHT_IMAGE
#include "BitmapWriter.h"
#endif

#define MIN(a, b)	(((a) < (b)) ? (a) : (b))
#define MAX(a, b)	(((a) > (b)) ? (a) : (b))

using namespace Livewire;

#pragma region Filter Helpers

typedef byte PixelFilterCB(byte p, void *param);
inline static void PixelFilter(const uint w, const uint h, byte *data, PixelFilterCB *cb, void *param)
{
	const uint wh = w*h;
	for (uint I = 0; I < wh; ++I)
		data[I] = cb(data[I], param);
}

template<uint windowSize> struct Filter { /*static const uint Matrix[windowSize][windowSize], Total;*/ };

typedef byte WindowFilterCBFull(const byte **window, void *param);
typedef byte WindowFilterCB(const byte **window, uint w, uint h, uint cx, uint cy, void *param);
template <uint windowSize> // windowSize must be odd and greater than 1
static void WindowFilter(const uint w, const uint h, const byte *in, byte *out, WindowFilterCBFull *cb_full, WindowFilterCB *cb, void *param)
{
	// When cb == NULL only wholes are processed

	// Make sure that the window size is >1 and odd
	CASSERT(windowSize != 1 && (windowSize & 1) == 1);

	static const uint WS1 = windowSize - 1, ws = windowSize / 2, ws1 = ws + 1;
	const uint w1 = w - 1, w_ws = w - ws, ws_w = w + ws, /*h1 = h - 1,*/ h_ws = h - ws, ws_h = h + ws;
	const byte *window[windowSize];

	uint X, Y, Yw, I;

	// TODO: Optimize more?

#define ADVANCE_X(N) for (uint y = 0; y < N; ++y) ++window[y];
	if (cb)
	{
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
	else
	{
		// full neighborhood only
		for (Y = ws, I = ws*(w+1), Yw = 0; Y < h_ws; ++Y, Yw += w, I += 2*ws)
		{
			for (uint y = 0, i = Yw; y < windowSize; ++y, i += w) window[y] = in + i;
			for (X = ws; X < w_ws; ++X)	{ out[I++] = cb_full(window, param); ADVANCE_X(windowSize); }
		}
	}

#undef ADVANCE_X
}

template <>
void WindowFilter<3>(const uint w, const uint h, const byte *in, byte *out, WindowFilterCBFull *cb_full, WindowFilterCB *cb, void *param)
{
	// Specialize the most common and smallest window filter

	const uint w1 = w - 1, h1 = h - 1;
	const byte *window[3];

	uint X, Y, Yw, I = 0;

	if (cb)
	{
		window[0] = in;
		window[1] = in + w;
		out[I++] = cb(window, 2, 2, 0, 0, param);																	// missing left columns and top rows
		for (X = 1; X < w1; ++X) { out[I++] = cb(window, 3, 2, 1, 0, param); ++window[0]; ++window[1]; }			// missing no columns and top rows
		out[I++] = cb(window, 2, 2, 1, 0, param);																	// missing right columns and top rows

		for (/*I = w,*/ Y = 1, Yw = w; Y < h1; ++Y, Yw += w)
		{
			window[0] = in + Yw - w;
			window[1] = in + Yw;
			window[2] = in + Yw + w;
			out[I++] = cb(window, 2, 3, 0, 1, param);																// missing left columns and no rows
			for (X = 1; X < w1; ++X, ++window[0], ++window[1], ++window[2])	{ out[I++] = cb_full(window, param); }	// full neighborhood
			out[I++] = cb(window, 2, 3, 1, 1, param);																// missing right columns and no rows
		}
	
		window[0] = in + Yw - w;
		window[1] = in + Yw;
		out[I++] = cb(window, 2, 2, 0, 1, param);																	// missing left columns and bottom rows
		for (X = 1; X < w1; ++X) { out[I++] = cb(window, 3, 2, 1, 1, param); ++window[0]; ++window[1]; }			// missing no columns and bottom rows
		out[I  ] = cb(window, 2, 2, 1, 1, param);																	// missing right columns and bottom rows
	}
	else
	{
		// full neighborhood only
		for (I = w + 1, Y = 1, Yw = w; Y < h1; ++Y, Yw += w, I += 2)
		{
			window[0] = in + Yw - w;
			window[1] = in + Yw;
			window[2] = in + Yw + w;
			for (X = 1; X < w1; ++X, ++window[0], ++window[1], ++window[2])	{ out[I++] = cb_full(window, param); }
		}
	}
}

template <uint windowSize> // windowSize must be greater than 0
static void WindowBinning(const uint W, const uint H, const byte *in, byte *out, WindowFilterCBFull *cb_full, WindowFilterCB *cb, void *param)
{
	// Make sure that the window size is >0
	CASSERT(windowSize > 0);

	// W and H are the raw width and height (in)
	// w and h are the binned width and height (out)

	static const uint ws = windowSize / 2; // WS1 = windowSize - 1

	const uint w_f =  W        / windowSize, h_f =  H        / windowSize; // the number of full bins
	//const uint w   = (W + WS1) / windowSize, h   = (H + WS1) / windowSize; // binned size
	const uint w_r =  W - w_f  * windowSize, h_r =  H - h_f  * windowSize; // the remainder that doesn't fit in a full bin

	const byte *window[windowSize];

#define ADVANCE_X(N) for (uint y = 0; y < N; ++y) window[y] += windowSize;

	uint I = 0, Yw = 0;
	for (uint Y = 0; Y < h_f; ++Y)
	{
		for (uint y = 0; y < windowSize; ++y, Yw += W) window[y] = in + Yw;

		// full neighborhood
		for (uint X = 0; X < w_f; ++X)	{ out[I++] = cb_full(window, param); ADVANCE_X(windowSize); }

		// missing right columns and no rows
		if (w_r)						{ out[I++] = cb(window, w_r, windowSize, ws, ws, param); }
	}

	if (h_r)
	{
		for (uint y = 0; y < h_r; ++y, Yw += W) window[y] = in + Yw;

		// missing no columns and bottom rows
		for (uint X = 0; X < w_f; ++X)	{ out[I++] = cb(window, windowSize, h_r, ws, ws, param); ADVANCE_X(h_r); }
		
		// missing right columns and bottom rows
		if (w_r)						{ out[I] = cb(window, w_r, h_r, ws, ws, param); }
	}

#undef ADVANCE_X
}

#pragma endregion

#pragma region Coalescing Functions
static void CoalesceGrayByte(const uint w, const uint h, const uint stride, const byte *in, byte *out)
{
	for (const byte *out_end = out + w*h; out < out_end; out += w, in += stride)
		memcpy(out, in, w);
}

static void CoalesceGrayUShort(const uint w, const uint h, const uint stride, const unsigned short *in, byte *out)
{
	const uint _stride = stride / sizeof(unsigned short);
	for (const byte *out_end = out + w*h; out < out_end; out += w, in += _stride)
		for (uint x = 0; x < w; ++x)
			out[x] = (in[x] >> 8) & 0xFF;
}

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

#pragma endregion

#pragma region Binning and Noise Reduction Functions
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
static byte MedianFilterCB(const byte **window, uint w, uint h, uint, uint, void *param)
{
	const uint count = w*h, half = count / 2;
	byte *list = (byte*)param;
	for (uint y = 0, i = 0; y < h; ++y, i += w)
		memcpy(list+i, window[y], w);
	qsort(list, count, 1, &byte_comp);
	return (((w & 1) == 0) || ((h & 1) == 0)) ? (byte)(((uint)list[half - 1] + list[half]) / 2) : list[half];
}

template <uint windowSize> // windowSize must be odd and greater than 1
inline static void RunMedianFilter(const uint w, const uint h, const byte *in, byte *out, bool wholesOnly)
{
	static const uint ws2 = windowSize * windowSize;
	byte list[ws2];
	WindowFilter<windowSize>(w, h, in, out, &MedianFilterCBFull<windowSize>, wholesOnly ? NULL : &MedianFilterCB, list);
}
template <uint windowSize>
inline static void RunMedianBinning(const uint W, const uint H, const byte *in, byte *out)
{
	static const uint ws2 = windowSize * windowSize;
	byte list[ws2];
	WindowBinning<windowSize>(W, H, in, out, &MedianFilterCBFull<windowSize>, &MedianFilterCB, list);
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
static byte MeanFilterCB(const byte **window, uint w, uint h, uint, uint, void*)
{
	uint v = 0;
	for (uint y = 0; y < h; ++y)
		for (uint x = 0; x < w; ++x)
			v += window[y][x];
	return (byte)(v / (w * h));
}
template<uint windowSize>
inline static void RunMeanFilter(const uint w, const uint h, const byte *in, byte *out, bool wholesOnly)
{
	WindowFilter<windowSize>(w, h, in, out, &MeanFilterCBFull<windowSize>, wholesOnly ? NULL : &MeanFilterCB, NULL);
}
template<uint windowSize>
inline static void RunMeanBinning(const uint W, const uint H, const byte *in, byte *out)
{
	WindowBinning<windowSize>(W, H, in, out, &MeanFilterCBFull<windowSize>, &MeanFilterCB, NULL);
}

// Gaussian Filter Generator in Python:
//from numpy import *
//def G(x,y,sigma): return exp(-(multiply(x,x)+multiply(y,y))/(2.0*sigma*sigma))/(2.0*pi*sigma*sigma)
//def X(l): return ones((l, l))*[x for x in range(-int(floor(l/2.0)),int(ceil(l/2.0)))]
//def Y(l): return ones((l, l))*[[x] for x in range(int(floor(l/2.0)),-int(ceil(l/2.0)),-1)]
//def GF(l,sigma): M = G(X(l),Y(l),sigma); M = around(M/M[0,0]).astype(int); return M,sum(M);
// Use it like GF(3,1) to make a 3x3 filter with stddev=1. Only works for odd-sized windows
// Recommendations:
//  need at most 3*sigma boxes from the origin (or ceil(6*sigma) window size)


template<uint windowSize> struct GaussianFilter : Filter<windowSize> { static const uint Matrix[windowSize][windowSize], Total; };

#define GAUSSIAN_FILTER(N, T) template<> const uint GaussianFilter<N>::Total = T; template<> const uint GaussianFilter<N>::Matrix[N][N]

// Gaussian 2x2 with any stddev is equivalent to mean filter

// Gaussian 3x3 with stddev = 1
GAUSSIAN_FILTER(3, 15) =	  {	{ 1,  2,  1},
								{ 2,  3,  2},
								{ 1,  2,  1}, };

// Gaussian 4x4 with stddev = 1
GAUSSIAN_FILTER(4, 56) =	  {	{ 1,  3,  3,  1},
								{ 3,  7,  7,  3},
								{ 3,  7,  7,  3},
								{ 1,  3,  3,  1}, };

// Gaussian 5x5 with stddev = 1
GAUSSIAN_FILTER(5, 331) =	  {	{ 1,  4,  7,  4,  1},
								{ 4, 20, 33, 20,  4},
								{ 7, 33, 55, 33,  7},
								{ 4, 20, 33, 20,  4},
								{ 1,  4,  7,  4,  1}, };

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
template<uint windowSize>
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
inline static void RunGaussianFilter(const uint w, const uint h, const byte *in, byte *out, bool wholesOnly)
{
	WindowFilter<windowSize>(w, h, in, out, &GaussianFilterCBFull<windowSize>, wholesOnly ? NULL : ((WindowFilterCB*)&GaussianFilterCB<windowSize>), NULL);
}
template<uint windowSize>
inline static void RunGaussianBinning(const uint W, const uint H, const byte *in, byte *out)
{
	WindowBinning<windowSize>(W, H, in, out, &GaussianFilterCBFull<windowSize>, &GaussianFilterCB<windowSize>, NULL);
}
template<>
inline void RunGaussianBinning<2>(const uint W, const uint H, const byte *in, byte *out)
{
	RunMeanBinning<2>(W, H, in, out);
}

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
inline static void RunSobelEdgeDetection(const uint w, const uint h, const byte *in, byte *out, bool wholesOnly)
{
	WindowFilter<3>(w, h, in, out, &SobelFilterCBFull, wholesOnly ? NULL : &SobelFilterCB, NULL);
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

static void RunSigmoidAccentuation(const uint w, const uint h, byte *data)
{
	PixelFilter(w, h, data, &sigmoid, NULL);
}
#pragma endregion

inline static byte invert(byte x, void*) { return ~x; }
static void RunInvert(const uint w, const uint h, byte *data) { PixelFilter(w, h, data, &invert, NULL); }

inline static void Swap(byte *&a, byte *&b) { byte *x = a; a = b; b = x; }

#define WINDOW_SIZE(m) ((m) & 0xF) // works for PixelReductionMethod, NoiseReductionMethod, and EdgeDetectionMethod

#define STATUS_NOT_DONE	0 // have not done the block and it isn't in the queue
#define STATUS_WILL_DO	1 // have not done the block but it is in the queue
#define STATUS_DOING	2 // actively computing the block and no longer in the queue
#define STATUS_DONE		3 // done computing the block / it is usable
// TODO: status really only takes up 2 bits, can fit 4 in a byte

#define BLOCK_SIZE	0x100
#define BLOCK_SIZE_	LOG2(BLOCK_SIZE)
CASSERT(IS_POWER_OF_TWO(BLOCK_SIZE)); // must be a power of 2, there are numerous mathematical shortcuts taken with this assumption in mind

WeightCalculator::Settings::Settings(CoalescingMethod Method, PixelReductionMethod PixelReduction, NoiseReductionMethod NoiseReduction, EdgeDetectionMethod EdgeDetection, AccentuationMethod Accentuation, bool Invert) :
	Method(Method), PixelReduction(PixelReduction), NoiseReduction(NoiseReduction), EdgeDetection(EdgeDetection), Accentuation(Accentuation), Invert(Invert) {}

const WeightCalculator::Settings WeightCalculator::GrayScaleSettings;

//const WeightCalculator::Settings WeightCalculator::GrayScaleSettings(WeightCalculator::BlueChannel,
//	WeightCalculator::Mean2pxWindow, WeightCalculator::NoNoiseReduction, WeightCalculator::NoEdgeDetection, WeightCalculator::Sigmoid, false);

//const WeightCalculator::Settings WeightCalculator::GrayScaleSettings(WeightCalculator::BlueChannel,
//	WeightCalculator::NoPixelReduction, WeightCalculator::NoNoiseReduction, WeightCalculator::NoEdgeDetection, WeightCalculator::NoAccentuation, false);

const WeightCalculator::Settings WeightCalculator::ColorSettings(WeightCalculator::WeightedHSV, WeightCalculator::Mean3pxWindow, WeightCalculator::NoNoiseReduction, WeightCalculator::Sobel, WeightCalculator::NoAccentuation, false);

WeightCalculator::WeightCalculator(uint w, uint h, const Settings& settings) : Threaded("Weight Calculator"),
	_data_raw(NULL), _stride(0), _format(GrayscaleByte),
	_settings(settings), _scale(WINDOW_SIZE(settings.PixelReduction)),
	_width_raw(w), _height_raw(h),
	_width(ScaleBack(w, WINDOW_SIZE(settings.PixelReduction))), _height(ScaleBack(h, WINDOW_SIZE(settings.PixelReduction))),
	_width_status(ScaleBack<BLOCK_SIZE>(_width)), _height_status(ScaleBack<BLOCK_SIZE>(_height)), _last_col_width(this->_width - ((this->_width_status-1) << BLOCK_SIZE_)),
	_filter_overflow(MAX(WINDOW_SIZE(settings.NoiseReduction), WINDOW_SIZE(settings.EdgeDetection)) / 2),
	_block_queue(_width_status, _height_status)
{
	const uint wh_s = this->_width_status*this->_height_status;

	this->SetTotalProgress(wh_s);
	this->_data   = (byte**)memset(malloc(wh_s*sizeof(byte*)), NULL, wh_s*sizeof(byte*));
	this->_status = (byte*)memset(malloc(wh_s), STATUS_NOT_DONE, wh_s);
}
WeightCalculator::~WeightCalculator()
{
	this->Stop();
	free(this->_data);
	free(this->_status);
}

uint WeightCalculator::GetBlocksCalculated(QVector<QPoint>& done, QVector<QPoint>& doing, QVector<QPoint>& not_done) const
{
	const uint BS = this->_scale << BLOCK_SIZE_;
	const uint w = this->_width_status * BS, h = this->_height_status * BS;
	for (uint y = 0, I = 0; y < h; y += BS)
		for (uint x = 0; x < w; x += BS, ++I)
		{
			if (this->_status[I] == STATUS_DONE)												done.push_back(QPoint(x, y));
			else if (this->_status[I] == STATUS_DOING || this->_status[I] == STATUS_WILL_DO)	doing.push_back(QPoint(x, y));
			else if (this->_status[I] == STATUS_NOT_DONE)										not_done.push_back(QPoint(x, y));
		}
	return BS;
}

bool WeightCalculator::Get(uint X, uint Y, byte* w) /*const*/
{
	const uint x = X >> BLOCK_SIZE_, i = (Y >> BLOCK_SIZE_) * this->_width_status + x;
	if (this->_status[i] == STATUS_WILL_DO || this->_status[i] == STATUS_DOING)
	{
		this->_status_lock.lock();
		while (this->_status[i] == STATUS_WILL_DO || this->_status[i] == STATUS_DOING)
			this->_block_finished.wait(&this->_status_lock);
		this->_status_lock.unlock();
	}
	if (this->_status[i] == STATUS_NOT_DONE)
		return false;
	const uint x_ = X & (BLOCK_SIZE - 1), y_ = Y & (BLOCK_SIZE - 1); // X % d (where d is a power of 2) = X & (d - 1)
	*w = this->_data[i][((x == this->_width_status-1) ? (y_ * this->_last_col_width) : (y_ << BLOCK_SIZE_)) + x_];
	return true;
}

uint WeightCalculator::GetScale()          const { return this->_scale;      }
uint WeightCalculator::GetOriginalWidth()  const { return this->_width_raw;  }
uint WeightCalculator::GetOriginalHeight() const { return this->_height_raw; }
uint WeightCalculator::GetReducedWidth()   const { return this->_width;      }
uint WeightCalculator::GetReducedHeight()  const { return this->_height;     }

void WeightCalculator::Stop()
{
	if (this->IsExecuting())
	{
		this->_queue_lock.lock();
		this->_block_queue.Clear();
		Threaded::Stop(false);
		this->_blocks_queued.wakeAll();
		this->_queue_lock.unlock();
	}

	const uint wh = this->_width_status*this->_height_status;

	this->wait();

	this->_data_raw = NULL;
	for (uint I = 0; I < wh; ++I)
		if (this->_data[I])
		{
			BlockPool::Return(this->_data[I]);
			this->_data[I] = NULL;
		}

	this->_status_lock.lock();
	memset(this->_status, STATUS_NOT_DONE, wh);
	this->_status_lock.unlock();
}

void WeightCalculator::SetImage(const byte* imageData, DataFormat format, uint stride)
{
	this->Stop();

	//assert(format != GrayscaleUShort || stride % sizeof(unsigned short) == 0);
	//assert(format != RGB             || stride % sizeof(QRgb) == 0);

	this->_stride = stride;
	this->_format = format;
	this->_data_raw = imageData;

	this->Start();
}

void WeightCalculator::ChangeSettings(const Settings& settings)
{
	const byte *data_raw = this->_data_raw;

	this->Stop();

	this->_settings = settings;
	this->_filter_overflow = MAX(WINDOW_SIZE(settings.NoiseReduction), WINDOW_SIZE(settings.EdgeDetection)) / 2;

	if (data_raw)
	{
		this->_data_raw = data_raw;
		this->Start();
	}
}

inline static uint CalcScore(uint x, uint y, double X, double Y)
{
	uint _X = (uint)X, _Y = (uint)Y;
	if (x == _X) return (y == _Y) ? 0 : (((y < _Y) ? (Y - y - 1) : (y - Y)) * BLOCK_SIZE);
	if (y == _Y) return                  ((x < _X) ? (X - x - 1) : (x - X)) * BLOCK_SIZE;

	double dx = (x < _X) ? (X - x - 1) : (x - X);
	double dy = (y < _Y) ? (Y - y - 1) : (y - Y);
	return (uint)(sqrt(dx*dx+dy*dy) * BLOCK_SIZE);
}
typedef struct _dblpoint { double x, y; } dblpoint;
static uint CalcScoreCB(uint x, uint y, uint I, dblpoint *pt) { (I); /* unreferenced parameter */ return CalcScore(x, y, pt->x, pt->y); }

void WeightCalculator::CalculateRegion(uint x, uint y, uint min_room)
{
	//assert(this->_data_raw != NULL);

	const uint l = x > min_room ? (x - min_room) >> BLOCK_SIZE_ : 0;
	const uint t = y > min_room ? (y - min_room) >> BLOCK_SIZE_ : 0;
	const uint r = x + min_room < this->_width  ? (x + min_room) >> BLOCK_SIZE_ : this->_width_status  - 1;
	const uint b = y + min_room < this->_height ? (y + min_room) >> BLOCK_SIZE_ : this->_height_status - 1;
	const double X = x / (double)BLOCK_SIZE, Y = y / (double)BLOCK_SIZE;
	const dblpoint pt = {X, Y};

	uint I = t * this->_width_status + l, i;

	bool added = false;

	QMutexLocker locker1(&this->_status_lock);
	QMutexLocker locker2(&this->_queue_lock);

	this->_block_queue.UpdateAllScores((PointPriorityQueue::CalcScore)CalcScoreCB, &pt);

	for (y = t; y <= b; I += this->_width_status, ++y)
	{
		for (x = l, i = I; x <= r; ++x, ++i)
		{
			if (this->_status[i] == STATUS_NOT_DONE)
			{
				// add to the queue
				this->_block_queue.Enqueue(x, y, i, CalcScore(x, y, X, Y));
				this->_status[i] = STATUS_WILL_DO;
				added = true;
			}
		}
	}
		
	if (added)
		this->_blocks_queued.wakeAll();
}

void WeightCalculator::Run()
{
	// TODO: make multi-threaded
	// int threadCount = QThread::idealThreadCount(); // equals the number of processor cores

	uint x, y, I, score;
	bool not_empty;

	do
	{
		for (;;)
		{
			this->_queue_lock.lock();
			not_empty = this->_block_queue.Dequeue(x, y, I, score);
			this->_queue_lock.unlock();
			
			if (!this->IsExecuting()) return;
			if (!not_empty) break;

			this->CalcBlock(x, y, I);
			this->IncProgress();
		}

		this->_queue_lock.lock();
		if (this->IsExecuting())
			this->_blocks_queued.wait(&this->_queue_lock);
		this->_queue_lock.unlock();
	}
	while (this->IsExecuting());
}

void WeightCalculator::CalcBlock(uint x_s, uint y_s, uint I)
{
	this->_status_lock.lock();
	this->_status[I] = STATUS_DOING;
	this->_status_lock.unlock();

	const uint w = this->_width,     h = this->_height;
	const uint W = this->_width_raw, H = this->_height_raw;
	const uint scale = this->_scale, extra = this->_filter_overflow;

	const bool no_left = !x_s, no_top = !y_s, short_right = x_s == this->_width_status-1, short_bottom = y_s == this->_height_status-1;
	const bool wholesOnly = !no_left && !no_top && !short_right && !short_bottom;
	const uint x = x_s << BLOCK_SIZE_, y = y_s << BLOCK_SIZE_;

	const uint len = BLOCK_SIZE + extra*2;
	const uint no_left_extra = extra * no_left, no_top_extra = extra * no_top;
	const uint usable_w = short_right  ? this->_last_col_width : BLOCK_SIZE;
	const uint usable_h = short_bottom ? h & (BLOCK_SIZE-1)    : BLOCK_SIZE;
	const uint bw = (short_right  ? usable_w : len) - no_left_extra;
	const uint bh = (short_bottom ? usable_h : len) - no_top_extra;

	const uint raw_len = len*scale;
	const uint BW = (short_right  ? W % (scale << BLOCK_SIZE_) : raw_len) - no_left_extra * scale;
	const uint BH = (short_bottom ? H % (scale << BLOCK_SIZE_) : raw_len) - no_top_extra  * scale;

	const uint in_offset = ((y - (no_top ? 0 : extra)) * W + (x - (no_left ? 0 : extra))) * scale;

	byte *out = (byte*)BlockPool::Get(raw_len*raw_len);
	byte *temp = (byte*)BlockPool::Get(len*len);
	const byte *big = out;

	if (this->_format == GrayscaleByte)
	{
		CoalesceGrayByte(BW, BH, this->_stride, this->_data_raw + in_offset, out);
	}
	else if (this->_format == GrayscaleUShort)
	{
		CoalesceGrayUShort(BW, BH, this->_stride, ((unsigned short*)this->_data_raw) + in_offset, out);
	}
	else if (this->_format == RGB)
	{
		switch (this->_settings.Method)
		{
		case RedChannel:	CoalesceRed			(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break;
		case GreenChannel:	CoalesceGreen		(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break;
		case BlueChannel:	CoalesceBlue		(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break;
		case AvgRGB:		CoalesceAvgRGB		(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break;
		case Luma:			CoalesceLuma		(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break;
		case Luma601:		CoalesceLuma601		(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break;
		case LumaSMPTE:		CoalesceLumaSMPTE	(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break;
		case WeightedHSV:	CoalesceWeightedHSV	(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break; // TODO: test
		case WeightedHSL:	CoalesceWeightedHSL	(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break; // TODO: test
		case WeightedHSI:	CoalesceWeightedHSI	(BW, BH, this->_stride, ((QRgb*)this->_data_raw) + in_offset, out); break; // TODO: test
		}
	}

	switch (this->_settings.PixelReduction)
	{
	case NoPixelReduction: break; // do nothing
	case Median2pxWindow:	RunMedianBinning<2>		(BW, BH, out, temp); Swap(out, temp); break; // TODO: test (with new setup)
	case Median3pxWindow:	RunMedianBinning<3>		(BW, BH, out, temp); Swap(out, temp); break; // TODO: test
	case Median4pxWindow:	RunMedianBinning<4>		(BW, BH, out, temp); Swap(out, temp); break; // TODO: test (with new setup)
	case Median5pxWindow:	RunMedianBinning<5>		(BW, BH, out, temp); Swap(out, temp); break; // TODO: test (with new setup)
	case Mean2pxWindow:		RunMeanBinning<2>		(BW, BH, out, temp); Swap(out, temp); break;
	case Mean3pxWindow:		RunMeanBinning<3>		(BW, BH, out, temp); Swap(out, temp); break;
	case Mean4pxWindow:		RunMeanBinning<4>		(BW, BH, out, temp); Swap(out, temp); break; // TODO: test (with new setup)
	case Mean5pxWindow:		RunMeanBinning<5>		(BW, BH, out, temp); Swap(out, temp); break; // TODO: test
	case Gaussian3pxWindow:	RunGaussianBinning<3>	(BW, BH, out, temp); Swap(out, temp); break; // TODO: test (with new setup)
	case Gaussian4pxWindow:	RunGaussianBinning<4>	(BW, BH, out, temp); Swap(out, temp); break; // TODO: test (with new setup)
	case Gaussian5pxWindow:	RunGaussianBinning<5>	(BW, BH, out, temp); Swap(out, temp); break; // TODO: test (with new setup)
	}

	switch (this->_settings.NoiseReduction)
	{
	case NoNoiseReduction: break; // do nothing
	case MedianFilter3pxWindow:		RunMedianFilter<3>		(bw, bh, out, temp, wholesOnly); Swap(out, temp); break;
	case MedianFilter5pxWindow:		RunMedianFilter<5>		(bw, bh, out, temp, wholesOnly); Swap(out, temp); break;
	case MeanFilter3pxWindow:		RunMeanFilter<3>		(bw, bh, out, temp, wholesOnly); Swap(out, temp); break;
	case MeanFilter5pxWindow:		RunMeanFilter<5>		(bw, bh, out, temp, wholesOnly); Swap(out, temp); break;
	case GaussianFilter3pxWindow:	RunGaussianFilter<3>	(bw, bh, out, temp, wholesOnly); Swap(out, temp); break;
	case GaussianFilter5pxWindow:	RunGaussianFilter<5>	(bw, bh, out, temp, wholesOnly); Swap(out, temp); break;
	}

	switch (this->_settings.EdgeDetection)
	{
	case NoEdgeDetection: break; // do nothing
	case Sobel:		RunSobelEdgeDetection(bw, bh, out, temp, wholesOnly); Swap(out, temp); break;
	}

	switch (this->_settings.Accentuation)
	{
	case NoAccentuation: break; // do nothing
	case Sigmoid:	RunSigmoidAccentuation(bw, bh, out); break;
	}

	if (this->_settings.Invert)
		RunInvert(bw, bh, out);

	if (scale > 1 && out == big)
	{
		// we want the memory that is going to be staying around for awhile to be the smaller one
		// only when scale > 1 are the memories different sizes
		memcpy(temp, out, usable_w*usable_h);
		Swap(out, temp);
	}
	this->_data[I] = out; //(byte*)BlockPool::Resize(out, usable_w*usable_h); // shrink the memory size to the minimum

	this->_status_lock.lock();
	this->_status[I] = STATUS_DONE;
#ifdef _DEBUG
	qDebug("[%p] %u done", QThread::currentThreadId(), I);
#endif
	this->_block_finished.wakeAll();
	this->_status_lock.unlock();

#ifdef SAVE_WEIGHT_IMAGE
	this->Checkpoint("saving weights image");
	WriteBitmap(this->_data, w, h, "weights");
#endif

	BlockPool::Return(temp);
}
