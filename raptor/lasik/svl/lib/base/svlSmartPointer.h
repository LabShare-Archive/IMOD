/*****************************************************************************
** STAIR VISION LIBRARY
** Copyright (c) 2007-2009, Stephen Gould
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are met:
**     * Redistributions of source code must retain the above copyright
**       notice, this list of conditions and the following disclaimer.
**     * Redistributions in binary form must reproduce the above copyright
**       notice, this list of conditions and the following disclaimer in the
**       documentation and/or other materials provided with the distribution.
**     * Neither the name of the Stanford University nor the
**       names of its contributors may be used to endorse or promote products
**       derived from this software without specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY
** EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
** WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
** DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
** DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
** (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
** LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
** ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
** (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
******************************************************************************
** FILENAME:    svlSmartPointer.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**   Provides a smart pointer interface to avoid the need to deep copy large
**   constant (shared) objects. Example use:
**      svlSmartPointer<TImage> A(new TImage());
**      svlSmartPointer<TImage> B(A);
**      svlSmartPointer<TImage> C = A;
**      C->showImage();
**
*****************************************************************************/

#pragma once

// svlSmartPointer class ----------------------------------------------------

template <typename T>
class svlSmartPointer {
 protected:
    T* _objPtr;
    unsigned *_refCount;

 public:
    inline svlSmartPointer() : _objPtr(NULL), _refCount(NULL) {
	// do nothing
    }
    
    inline svlSmartPointer(T* obj) : _objPtr(obj) {
	_refCount = (_objPtr == NULL) ? NULL : new unsigned(1);
    }

    inline svlSmartPointer(const svlSmartPointer<T>& p) : _objPtr(p._objPtr), _refCount(p._refCount) {
	if (_refCount != NULL)
	  {
	    ++*_refCount;
	  }
    }

    inline ~svlSmartPointer() {
	if ((_refCount != NULL) && (--*_refCount == 0)) {
	    delete _objPtr;
	    delete _refCount;
	    _objPtr = NULL;
	    _refCount = NULL;
	}
    }

    inline svlSmartPointer<T>& operator=(const svlSmartPointer<T>& p) {
	if (p._refCount)
	  {
	    ++*p._refCount;
	  }

	if ((_refCount != NULL) && (--*_refCount == 0)) {
	    delete _objPtr;
	    delete _refCount;
	}

	_objPtr = p._objPtr;
	_refCount = p._refCount;
	

	return *this;
    }

    inline bool operator==(const svlSmartPointer<T>& p) {
	return (_refCount == p._refCount);
    }
    inline bool operator!=(const svlSmartPointer<T>& p) {
	return (_refCount != p._refCount);
    }
    inline bool operator==(const T* o) {
        return (_objPtr == o);
    }
    inline bool operator!=(const T* o) {
        return (_objPtr != o);
    }

    inline T* operator->() { 
return _objPtr; }
    inline const T* operator->() const { return _objPtr; }

    inline operator T*() { return _objPtr; }
    inline operator const T*() const { return _objPtr; }
};

// svlSmartPointer comparison classes -----------------------------------------

template <typename T>
struct svlSmartPointerCmpLessThan {
    bool operator()(svlSmartPointer<T>& a, svlSmartPointer<T>& b) const {
        return (*a < *b);
    }
};
