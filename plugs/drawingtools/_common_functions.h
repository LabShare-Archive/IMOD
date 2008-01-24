/*
 *  _common_functions.h -- Contains several inline functions and templated
 *                         functions which I commonly use.
 */

/*****************************************************************************
*   Copyright (C) 2007 by Andrew Noske from the Institute for Molecular     *
*   Bioscience at the University of Queensland (Australia)                  *
*****************************************************************************/

//----------------------------------------------------------------------------
//
//    Note that inline template functions have been used instead of
//    macros. Although macros produce simple code
//    (e.g.: "#define SQ(x) ((x)*(x))" ), they are not recommended
//    in c++ programming.
//    
//----------------------------------------------------------------------------


#ifndef INC_COMMON_FUNCTIONS_H
#define INC_COMMON_FUNCTIONS_H

//############################################################
//## INCLUDES:

#include <math.h>       // for some simple maths algorithms
#include <vector>       // for use of vectors
#include <sstream>			// for formatting string output
#include <iostream>			// for redirecting cout
#include <string>       // for string
#include <algorithm>		// for special algorithms ( such as sort() and tranform() )

using namespace std;

//############################################################



//----------------------------------------------------------------------------
//  
//					CONSTANTS:
//  
//----------------------------------------------------------------------------


//## GEOMETRY RELATED:

enum angletype { X, Y, Z, ALL, NONE };

const double PI = 3.141592654;
const double RADS_TO_DEGS = 180.0/PI;
const double DEGS_TO_RADS = PI/180.0;



//## LIMITS:


const long MAX_LONG			= (long)2147483647; 			// largest negative value of a long
const long MIN_LONG			= (long)2147483648;       // largest positive value of a long

const int MAX_INT			= (int)32767; 			// largest negative value of a (singed) int
const int MIN_INT			= (int)32768;				// largest positive value of a (signed) int

const double DBL_MAX		= (double) 1.7976931348623158e+308;     // largest pos double
const double DBL_MIN		= (double)-1.7976931348623158e+308;     // largest neg double

const float FLOAT_MAX		= (float) 3.40282e38;					// largest positive value float
const float FLOAT_MIN_POS	= (float) 1.17549e-38;			// smallest positive value float
const float FLOAT_MIN		= (float)-3.4027e35;					// largest negative value float


//----------------------------------------------------------------------------
//  
//					FUNCTION DECLARATION:
//  
//----------------------------------------------------------------------------


//## BASIC FUNCTIONS:

template <typename type>	type SQ( type x );
template <typename type>	type CUBE( type x );
template <typename type>	type MIN( type val1, type val2 );
template <typename type>	type MAX( type val1, type val2 );
template <typename type>	void updateMax( type &max, type newVal );
template <typename type>	void updateMin( type &min, type newVal );
template <typename type>	type ABS( type val );
template <typename type>	void swapVals( type &val1, type &val2 );
template <typename type>	void swapValsAsc( type &val1, type &val2 );
template <typename type>	void keepWithinRange( type &val, type min, type max );
template <typename type>	bool isBetween( type limit1, type val, type limit2 );
template <typename type>	bool isBetweenAsc( type min, type middle, type max );
template <typename type>	bool isBetweenNI( type limit1, type val, type limit2 );

inline float fMod( float val, float modVal );
inline int intMod( int val, int modVal );
inline float fModWithinRange( float val, float min, float max );
inline void changeIntWithinRange( int &val, int min, int max, int increment );
inline void cycleIntWithinRange( int &val, int min, int max, int increment );
template <typename type>  void changeNumWithinRange( type &val, type min, type max, type changeAmount );
inline float fDivide( float numerator, float denominator );
inline float fDivideCustom( float numerator, float denominator, float infinityValue=FLOAT_MAX );
inline bool isFactor( float value, float divisor );
inline int roundToInt(float x);
inline float roundPrec( float value, float precision );
inline float avg( float val1, float val2 );
inline float getFractBetween( float val1, float val2, float fractTowards2 );


//## STRING FUNCTIONS:

template <typename type>	string toString( type value );
template <typename data>	string toStringPadNumber( data value, int padLength, char padChar='0' );
template <typename data>	string toStringWithCommas( data value );

inline string toString( float value, int decimal );
inline string toStringAsPercent( float numerator, float denominator, int decimal=0 );
inline float string_getFloatFromString( string str );

inline string string_substr (string str, int chars, int offset );
inline string string_substrFromEnd (string str, int charsAtEnd);



//## VECTOR FUNCTIONS:

template <typename type>	bool vector2D_transpose( vector< vector<type> > &v );
template <typename type>	void vector_eliminateDuplicates( vector<type> &v );
template <typename type>	bool vector_doesElementExistInVector( vector<type> v, type element );
template <typename type>	vector<type> vector_concat( vector<type> &v1, vector<type> &v2 );
template <typename type>	vector<type> vector_sort( vector<type> v, int startIdx );
template <typename type>	vector<type> vector_sort( vector<type> v );






//----------------------------------------------------------------------------
//  
//					FUNCTION DEFINITIONS:
//  
//----------------------------------------------------------------------------





//----------------------------------------------------------------------------
//## BASIC FUNCTIONS
//----------------------------------------------------------------------------


//----------------
//-- Returns the given number squared
template <typename type>
inline type SQ(type x)		{ return (x*x);	}
//----------------
//-- Returns the given number cubed
template <typename type>
inline type CUBE(type x)		{ return (x*x*x); }

//----------------
//-- Returns the smaller of the two values fed in (NOTE: this
//-- function already exists, but use capitals to avoid ambiguity)
template <typename type>
inline type MIN( type val1, type val2 )	
{ return ( val1 < val2 ) ? val1 : val2; }
//----------------
//-- Returns the greater of the two values fed in (NOTE: this
//-- function already exists, but use capitals to avoid ambiguity)
template <typename type>
inline type MAX( type val1, type val2 )	
{ return ( val1 > val2 ) ? val1 : val2; }

//----------------
//-- Updates max is newVal is greater than max
template <typename type>
inline void updateMax( type &max, type newVal )		
{ if( max < newVal )	max = newVal; }
//----------------
//-- Updates min is newVal is less than than min
template <typename type>
inline void updateMin( type &min, type newVal )		
{ if( min > newVal )	min = newVal; }

//----------------
//-- Used to avoid shitty error message g++ was giving me "error:
//-- call of overloaded 'abs(float)' is ambiguous"
template <typename type>
inline type ABS( type val )						
{ return ( val >= 0 ) ? val : -val; }


//----------------
//-- Will swap the two values around
//-- (i.e. the value in "val2" will end up in val1 and vice-versa)
template <typename type>
inline void swapVals( type &val1, type &val2 ) {				
	type tempVal = val1;
	val1 = val2;
	val2 = tempVal;
}
//----------------
//-- Will (if necessary) swap the values around such
//-- that val1 <= val2 will be true
template <typename type>
inline void swapValsAsc( type &val1, type &val2 ) {				
	if( val1 > val2 ) {
		type tempVal = val1;
		val1 = val2;
		val2 = tempVal;
	}
}


//----------------
//-- Will change val to min or max (whichever is closer)
//-- if it's outside of these two value.
template <typename type>
inline void keepWithinRange( type &val, type min, type max ) {				
	if( val < min )		val = min;
	if( val > max )		val = max;
}


//----------------
//-- Determines if val is between the two limit values, whereby
//-- the limit values are not necessarily in the order lowest, highest.
template <typename type>
inline bool isBetween( type limit1, type val, type limit2 ) {		
	return ( (limit1 <= val && val <= limit2) || (limit2 <= val && val <= limit1) );
}
//----------------
//-- Determines if val is between the two limit values (min & max) inclusive.
template <typename type>
inline bool isBetweenAsc( type min, type middle, type max ) {
	return (min <= middle && middle <= max);
}
//----------------
//-- Determines if val is between the two limit values NOT INCLUSIVE, whereby
//-- the limit values are not necessarily in the order lowest, highest.
template <typename type>
inline bool isBetweenNI( type limit1, type val, type limit2 ) {		
	return ( (limit1 < val && val < limit2) || (limit2 < val && val < limit1) );
}


//----------------
//-- Returns (positive) remainder between 0 and modVal (modVal not
//-- inclusive) after dividing value by modVal. Is equivalent to
//-- fmod(d1,d2), but seems to be faster.
inline float fMod( float val, float modVal ) {				
	if( val >= modVal ) {
		int divisor = int(val / modVal);
		return (val - divisor*modVal);
	}
	else if( val < 0 ) {
		int divisor = (int)ceil(-val / modVal);		//rounds up
		return (val + divisor*modVal);
	}
	return (val);
}
//----------------
//-- Returns (positive) remainder between 0 and modVal-1. Unlike normal
//-- mod (%) this can handle negative numbers.
inline int intMod( int val, int modVal ) {				
	if( modVal == 0 ) {
		return (0);
	}
	else if( val >= modVal ) {
		return (val % modVal);
	}
	else if( val < 0 ) {
		int divisor = (int)ceil(float(-val) / float(modVal));		//rounds up
		return (val + divisor*modVal);
	}
	return (val);
}


//----------------
//-- If values is outside min or max, it wraps it around so
//-- it's between the values using fMod
//-- EG: fModWithinRange (270, -180, 180) -> -90
//-- EG: fModWithinRange (4,   5,    10 ) -> 9
inline float fModWithinRange( float val, float min, float max ) {				
	if( val >= min && val < max ) {
		return (val);
	}
	else {
		return ( fMod( val-min, max-min ) + min);
	}
}

//----------------
//-- Changes "val" by "increment", but prevents it from becoming any
//-- less than "min" or greater than "max".
inline void changeIntWithinRange( int &val, int min, int max,
                                  int increment )
{
	val = val + increment;
	if( val < min )		val = min;
	if( val > max )		val = max;
}

//----------------
//-- Changes/cycles "val" by "increment", while wrapping it around
//-- the edges so that it stays always remains between min and max.
inline void cycleIntWithinRange( int &val, int min, int max,
                                 int increment )
{
	val = val + increment;
	val = intMod( val-min, max-min ) + min;
}

//----------------
//-- Changes "val" by "increment", but prevents it from becoming
//-- any less than "min" or greater than "max".

template <typename type>
inline void changeNumWithinRange( type &val, type min, type max,
                                  type changeAmount )
{
	val = val + changeAmount;
	if( val < min )		val = min;
	if( val > max )		val = max;
}


//----------------
//-- Used to avoid divide by 0 error
inline float fDivide( float numerator, float denominator )
{
	if (denominator == 0)
		return DBL_MAX;
	return numerator/denominator;
}

//----------------
//-- Used to avoid divide by 0 error
inline float fDivideCustom( float numerator, float denominator,
                             float infinityValue )
{
	if (denominator == 0)
		return infinityValue;
	return numerator/denominator;
}

//----------------
//-- Returns true if the given value is evenly divisible by the given divosor

inline bool isFactor( float value, float divisor )
{
	float result = value / divisor;
	return ( result == (float)(floor(result)) );
}

//----------------
//-- Rounds float to the nearest integer
inline int roundToInt(float x)												
{
	return int( (x > 0.0) ? (x + 0.5) : (x - 0.5) );
}
//----------------
//-- Founds float to nearest multiple of "precision"
inline float roundPrec( float value, float precision ) 				
{
	return ( roundToInt(value / precision) * precision );
}

//----------------
//-- Averages two numbers.
inline float avg( float val1, float val2 )
{
	return (val1 + val2) / 2.0;
}

//----------------
//-- Finds the value "fractTowards2" along the way from val1 towards val2.

inline float getFractBetween( float val1, float val2,
                               float fractTowards2 )
{
	return (((val2 - val1) * fractTowards2) + val1);
}



//----------------------------------------------------------------------------
//##									STRING FUNCTIONS:
//----------------------------------------------------------------------------


//-------------
//-- Takes a single value of *almost* any type and returns it as a string
//-- by using ostringstream (string output stream).
//-- This function will also work for your own classes/structures, but only
//-- if you specify a "<<" operator.
//--
//-- FOR EXAMPLE: "friend ostream& operator<< (ostream &os, myPoint p)
//-- {os << pd.x << ',' << pd.y; return os;}"

template <typename type>
inline string toString( type value )
{
	ostringstream out;
	out << value;
	return out.str();
}

//-------------
//-- Takes a number and converts to a string, but if it
//-- is < padLength digits long, then it will add extra characters
//-- ("0"'s by default) on the left of it if.
//-- 
//-- EXAMPLE: (value=5, padLength=4, padChar='0') ->  return "0005"

template <typename data>
inline string toStringPadNumber( data value, int padLength, char padChar )
{	
	string returnStr = toString(value);
	while( (int)returnStr.length() < padLength )
		returnStr = padChar + returnStr;
	return returnStr;
}
 
//-------------
//-- Takes a number and adds commas (eg: 10000.05 -> 10,000.05)

template <typename data>
inline string toStringWithCommas( data value )
{
	string str = toString(value);
	int digitsBeforeDec = toString((int)value).length();
	
	if( digitsBeforeDec <= 3)
		return str;
	
	int numCommasNeeded = ((digitsBeforeDec-1) / 3);
	int leadingDigits   = digitsBeforeDec - numCommasNeeded*3;
	
	string returnStr = string_substr( str, leadingDigits, 0 );
	for (int i=0; i<numCommasNeeded; i++)
		returnStr = returnStr + "," + string_substr( str, 3, leadingDigits+i*3 );

	return returnStr;
}


//-------------
//-- Takes a float and converts it to a string but limits the decimal places

inline string toString( float value, int decimal )
{	
	float	exponent = (float)pow(10,(int)decimal);
	float	newValue = ((int)(value * exponent))/exponent ;
	return  toString(newValue);
}


//-------------
//-- Takes a numerator and demoninator and returns the result
//-- of division as a percent, including a percentage sign.
//-- EXAMPLE: (numerator=1,denominator=3,decimal=0) -> return "33%"

inline string toStringAsPercent( float numerator, float denominator, int decimal )
{
	if(denominator == 0)
		return "-%";
	float percentage = ( numerator / denominator ) * 100;
	return (toString(percentage, decimal) + "%");
}

//-------------
//-- Takes a string an returns a float by calling "atof" - which can
//-- handle the chars: '-', '.' and 'e' - eg: "-5.3e4" -> 53000 ).
//-- If there are bad characters 0 will be returned (maybe an error
//-- message should appear instead).

inline float string_getFloatFromString( string str ) {
	return ( (float)atof( str.c_str() ) );
}

//-------------
//-- Gets a substring (safely), from the start of a string.

inline string string_substr (string str, int chars, int offset )
{
	if( offset >= (int)str.length() || offset < 0 )
		offset = (int)str.length();
	if( chars + offset >= (int)str.length() )
		chars = (int)str.length() - offset;
	return str.substr( offset, chars );
}


//-------------
//-- Gets a substring (safely), from the end of the string.
//-- EXAMPLE: ("myfile.jpg",3) -> "jpg"

inline string string_substrFromEnd (string str, int charsAtEnd)
{
	if ( charsAtEnd >= (int)str.length() )
		return str;
	return str.substr( (int)str.length()-charsAtEnd, charsAtEnd );
}





//----------------------------------------------------------------------------
//##					VECTOR RELATED FUNCTIONS:
//----------------------------------------------------------------------------


//-------------
//-- Takes a two dimensional vector and reverses the order of the dimensions.
//-- for example:
//---               { {0,1,2},
//--                  {3,4,5} }
//-- would become:  
//--                { {0,3},
//--                  {1,4},
//--                  {2,5} }
//--
//-- NOTE: If any of the second dimension are DIFFERENT lengths,
//-- then returns false.

template <typename type>
bool vector2D_transpose( vector< vector<type> > &v )			
{
	if( v.empty() || v[0].empty() )
		return false;
		
	int DIM_X = (int)v.size();
	int DIM_Y = (int)v[0].size();
	
	//## CHECK ALL VECTORS (IN THE MAIN VECTOR) ARE SAME SIZE:
	for(int x=1; x<DIM_X; x++)
		if( v[x-1].size() != v[x].size() )
			return false;
	
	//## CREATE TRANSPOSED VERSION:
	vector< vector<type> > vT;
	vT.resize( DIM_Y );
	for(int x=0; x<DIM_Y; x++)
		vT[x].resize( DIM_X );
		
	for(int x=0; x<DIM_X; x++)
		for(int y=0; y<DIM_Y; y++)
			vT[y][x] = v[x][y];
			
	v = vT;
	
	return true;
}


//-------------
//-- Eliminates duplicates from a vector, but ONLY if they
//-- occur sequentially - hence user should sort the vector first.
//--
//-- EXAMPLE 1: {1,3,3,3,5} -> the second two 3's will be removed
//-- EXAMPLE 1: {3,1,3,5,3} -> no elements will be removed  :(

template <typename type>
void vector_eliminateDuplicates( vector<type> &v )
{
    for(int i=1; i<(int)v.size(); i++)
		if( v[i-1]==v[i] )
		{
			v.erase( v.begin()+i );
			--i;
		}
}

//-------------
//-- Checks if element already exists

template <typename type>
inline bool vector_doesElementExistInVector( vector<type> v, type element )
{
    for(int i=0; i<(int)v.size(); i++)
		if( element == v.at(i) ) {
			return (true);
		}
		
	return (false);
}



//-------------
//-- Appends the contents of the second vector to the end of
//-- the first vector and returns the result

template <typename type>
vector<type> vector_concat( vector<type> &v1, vector<type> &v2 )
{
	vector<type> returnVec = v1;
	
	for(int i=0; i<(int)v2.size(); i++)
		returnVec.push_back( v2[i] );
		
	return returnVec;
}


//-------------
//-- Sorts given vector in ascending order, starting with
//-- the elment at the index startIdx

template <typename type>
inline vector<type> vector_sort( vector<type> v, int startIdx )
{
#if defined (__APPLE__)			
        // NOTE: For some reason the "sort( )" command doesn't work on OSX,
        //       so I've had to write my own (which is not as efficient)
  
	vector<type> returnVec = v;
	for(int i=startIdx; i<(int)v.size(); i++)
	{
		int minIdx = i;
		
		for(int j=i+1; j<(int)v.size(); j++)
			if( v[j] < v[minIdx] )
				minIdx = j;
				
		if( minIdx!=i )
			swapVals( v[i], v[minIdx] );
	}
	return returnVec;
	
#else
	
	vector<type> returnVec = v;
	sort( returnVec.begin()+startIdx, returnVec.end() );
	return returnVec;
	
#endif

}


//-------------
//-- Sorts given vector in ascending order (from first to last element)

template <typename type>
vector<type> vector_sort( vector<type> v )
{
#if defined (__APPLE__)
	
	return vector_sort( v, 0 );
	
#else
	
	vector<type> returnVec = v;
	sort( returnVec.begin(), returnVec.end() );
	return returnVec;
	
#endif

}

//----------------------------------------------------------------------------


#endif
