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

#ifndef COLORS_H
#define COLORS_H

#include "general.h"

// For color-space conversion, the following variables and equations are used:

// R : 0-255
// G : 0-255
// B : 0-255
// M : 0-255	M = max(R, G, B)
// m : 0-255	m = min(R, G, B)
// C : 0-255	C = M - m
//
// H : 0-255	if (C == 0) { H = 0 } else ->
//				M==R: H = (G-B)*85/(2*C) % 255
//				M==G: H = (B-R)*85/(2*C) + 85
//				M==B: H = (R-G)*85/(2*C) + 170
//
// Mm  : 0-510	Mm = M + m			(only in HSL)
// RGB : 0-765	RGB = R + G + B		(only in HSI)
//
// V : 0-255	V = M
// L : 0-255	L = Mm / 2
// I : 0-255	I = RGB / 3
//
// S : 0-255	if (C == 0) { S = 0 } else ->
//				HSV: S = C*255 / V
//				HSL: S = C*255 / ((Mm <= 255) ? Mm : (510 - Mm));
//				HSI: S = 255*(1-m/I) = 255 - m*765 / RGB = ~(m*765/RGB)

void RGBtoHSV(byte R, byte G, byte B, byte& H, byte& S, byte& V)
{
	byte M, m;
	uint C;

	if (R > G)
	{
		if (R > B)	{ M = R; m = B > G ? G : B;	C = M - m; H = ((int)G-B)*85/(2*C)%255;   S = C*255/M;   }	// R > G, R > B		M != m, C != 0
		else		{ M = B; m = G;				C = M - m; H = ((int)R-G)*85/(2*C)+170;   S = C*255/M;   }	// B >= R > G		M != m, C != 0
	}
	else if (G > B)	{ M = G; m = B > R ? R : B;	C = M - m; H = ((int)B-R)*85/(2*C)+85;    S = C*255/M;   }	// G >= R, G > B	M != m, C != 0
	else			{ M = B; m = R;				C = M - m; H=C?((int)R-G)*85/(2*C)+170:0; S=C?C*255/M:0; }	// B >= G >= R		[M could == m, C could == 0]

	V = M;
}
void RGBtoHSL(byte R, byte G, byte B, byte& H, byte& S, byte& L)
{
	byte M, m;
	uint C, Mm;

	if (R > G)
	{
		if (R > B)	{ M = R; m = B > G ? G : B;	C = M - m; Mm = (uint)M + m; H = ((int)G-B)*85/(2*C)%255;   S = C*255/((Mm<=255)?Mm:(510-Mm));   }	// R > G, R > B		M != m, C != 0
		else		{ M = B; m = G;				C = M - m; Mm = (uint)M + m; H = ((int)R-G)*85/(2*C)+170;   S = C*255/((Mm<=255)?Mm:(510-Mm));   }	// B >= R > G		M != m, C != 0
	}
	else if (G > B)	{ M = G; m = B > R ? R : B;	C = M - m; Mm = (uint)M + m; H = ((int)B-R)*85/(2*C)+85;    S = C*255/((Mm<=255)?Mm:(510-Mm));   }	// G >= R, G > B	M != m, C != 0
	else			{ M = B; m = R;				C = M - m; Mm = (uint)M + m; H=C?((int)R-G)*85/(2*C)+170:0; S=C?C*255/((Mm<=255)?Mm:(510-Mm)):0; }	// B >= G >= R		[M could == m, C could == 0]

	L = Mm / 2;
}
void RGBtoHSI(byte R, byte G, byte B, byte& H, byte& S, byte& I)
{
	byte M, m;
	uint C, RGB = (uint)R+G+B;

	if (R > G)
	{
		if (R > B)	{ M = R; m = B > G ? G : B;	C = M - m; H = ((int)G-B)*85/(2*C)%255;   S = 255-((uint)m)*765/RGB;   }	// R > G, R > B		M != m, C != 0
		else		{ M = B; m = G;				C = M - m; H = ((int)R-G)*85/(2*C)+170;   S = 255-((uint)m)*765/RGB;   }	// B >= R > G		M != m, C != 0
	}
	else if (G > B)	{ M = G; m = B > R ? R : B;	C = M - m; H = ((int)B-R)*85/(2*C)+85;    S = 255-((uint)m)*765/RGB;   }	// G >= R, G > B	M != m, C != 0
	else			{ M = B; m = R;				C = M - m; H=C?((int)R-G)*85/(2*C)+170:0; S=C?255-((uint)m)*765/RGB:0; }	// B >= G >= R		[M could == m, C could == 0]

	I = RGB / 3;
}

#endif
