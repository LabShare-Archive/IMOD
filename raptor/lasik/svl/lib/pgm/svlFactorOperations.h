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
** (INCLUDING NEgGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
** SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
**
******************************************************************************
** FILENAME:    svlFactorOperations.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**  Implements various operations on factors. The classes store mappings
**  between factor elements making them very fast for iterative algorithms.
**
*****************************************************************************/

#pragma once

#include <vector>
#include <set>
#include <map>

#include "svlFactor.h"
#include "../base/svlSmartPointer.h"

// svlFactorIndexCache class -----------------------------------------------

typedef svlSmartPointer<const std::vector<int> > svlFactorIndexRef;


class svlFactorIndexCache {
 protected:
    std::multimap<unsigned, svlFactorIndexRef> _cache;

 public:
    svlFactorIndexCache();
    ~svlFactorIndexCache();

    void clear();
    svlFactorIndexRef find(const vector<int>& index);
};

// svlFactorOperation class ------------------------------------------------

class svlFactorOperation {
 public:
    static bool CACHE_INDEX_MAPPING;
    static bool USE_SHARED_INDEX_CACHE;

 protected:
    static svlFactorIndexCache GLOBAL_FACTOR_INDEX_CACHE;

 protected:
    svlFactor *_target;

 public:
    svlFactorOperation(svlFactor *target);
    svlFactorOperation(const svlFactorOperation& op);
    virtual ~svlFactorOperation();

    // Clears the shared index cache. This is safe to call (I think) when
    // operations are still in scope, but may then cause multiple copies
    // of the same index.
    static void clearIndexCache();

    // Execute the operation storing the results in target. This function
    // must be overriden by each derived class. It is defined as pure virtual
    // so svlFactorOperation objects cannot be instantiated themselves---use
    // pointers where necessary.
    virtual void execute() = 0;

    // Access to the target factor.
    inline svlFactor *target() const { return _target; }

 protected:
    // Initialize the mappings from sources to targets and (if the target
    // is empty) initialize the target factor with the right set of
    // variables.
    virtual void initialize() { /* default: do nothing */ };
    // Check that the target is not empty and has the right set of
    // variables for this operation.
    virtual bool checkTarget() { /* default: true */ return true; }
    // Returns a reference to a factor index.
    inline svlFactorIndexRef indexRef(const vector<int>& mapping) const;
};

// svlFactorBinaryOp class -------------------------------------------------

class svlFactorBinaryOp : public svlFactorOperation {
 protected:
    const svlFactor * const _A;
    const svlFactor * const _B;

    svlFactorIndexRef _mappingA;
    svlFactorIndexRef _mappingB;

 public:
    svlFactorBinaryOp(svlFactor *target,
	const svlFactor *A, const svlFactor *B);
    svlFactorBinaryOp(const svlFactorBinaryOp &phi);
    ~svlFactorBinaryOp();

 protected:
    void initialize();
    bool checkTarget();
};

// svlFactorNAryOp class ---------------------------------------------------

class svlFactorNAryOp : public svlFactorOperation {
 protected:
    std::vector<const svlFactor *> _factors;
    std::vector<svlFactorIndexRef> _mappings;

 public:
    svlFactorNAryOp(svlFactor *target,
	const svlFactor *A, const svlFactor *B);
    svlFactorNAryOp(svlFactor *target,
	const std::vector<const svlFactor *>& A);
    svlFactorNAryOp(const svlFactorNAryOp &phi);
    ~svlFactorNAryOp();

 protected:
    void initialize();
    bool checkTarget();
};

// svlFactorAtomicOp class -------------------------------------------------

class svlFactorAtomicOp : public svlFactorOperation {
 protected:
    // Factor operations are owned by object (i.e. they will be deleted when
    // the object is destroyed).
    std::vector<svlFactorOperation *> _computations;

 public:
    svlFactorAtomicOp(svlFactorOperation *op);
    svlFactorAtomicOp(const std::vector<svlFactorOperation *>& ops);
    ~svlFactorAtomicOp();

    void execute();

    void addOperation(svlFactorOperation *op);
};

// svlFactorCopyOp class ---------------------------------------------------

class svlFactorCopyOp : public svlFactorOperation {
 protected:
    const svlFactor * const _A;

 public:
    svlFactorCopyOp(svlFactor *target, const svlFactor *A);
    svlFactorCopyOp(const svlFactorCopyOp& phi);
    ~svlFactorCopyOp();

    void execute();

 protected:
    void initialize();
    bool checkTarget();
};

// svlFactorProductOp class ------------------------------------------------

class svlFactorProductOp : public svlFactorNAryOp {
 public:
    svlFactorProductOp(svlFactor *target,
	const svlFactor *A, const svlFactor *B);
    svlFactorProductOp(svlFactor *target,
	const std::vector<const svlFactor *>& A);
    svlFactorProductOp(const svlFactorProductOp &phi);
    ~svlFactorProductOp();

    void execute();
};

// svlFactorDivideOp class -------------------------------------------------

class svlFactorDivideOp : public svlFactorBinaryOp {
 public:
    svlFactorDivideOp(svlFactor *target,
	const svlFactor *A, const svlFactor *B);
    svlFactorDivideOp(const svlFactorDivideOp &phi);
    ~svlFactorDivideOp();

    void execute();
};

// svlFactorAdditionOp class -----------------------------------------------

class svlFactorAdditionOp : public svlFactorNAryOp {
 public:
    svlFactorAdditionOp(svlFactor *target,
	const svlFactor *A, const svlFactor *B);
    svlFactorAdditionOp(svlFactor *target,
	const std::vector<const svlFactor *>& A);
    svlFactorAdditionOp(const svlFactorAdditionOp &phi);
    ~svlFactorAdditionOp();

    void execute();
};

// svlFactorSubtractOp class ------------------------------------------------

class svlFactorSubtractOp : public svlFactorBinaryOp {
 public:
    svlFactorSubtractOp(svlFactor *target,
	const svlFactor *A, const svlFactor *B);
    svlFactorSubtractOp(const svlFactorSubtractOp &phi);
    ~svlFactorSubtractOp();

    void execute();
};

// svlFactorWeightedSumOp class ---------------------------------------------

class svlFactorWeightedSumOp : public svlFactorBinaryOp {
 protected:
    double _weightA;
    double _weightB;

 public:
    svlFactorWeightedSumOp(svlFactor *target, const svlFactor *A,
        const svlFactor *B, double wA = 1.0, double wB = 1.0);
    svlFactorWeightedSumOp(const svlFactorWeightedSumOp &phi);
    ~svlFactorWeightedSumOp();

    void execute();
};

// svlFactorMarginalizeOp class --------------------------------------------

class svlFactorMarginalizeOp : public svlFactorOperation {
 protected:
    const svlFactor * const _A;
    svlFactorIndexRef _mappingA;

 public:
    svlFactorMarginalizeOp(svlFactor *target,
	const svlFactor *A);
    svlFactorMarginalizeOp(svlFactor *target,
	const svlFactor *A, int v);
    svlFactorMarginalizeOp(svlFactor *target,
	const svlFactor *A, const std::set<int>& v);
    svlFactorMarginalizeOp(const svlFactorMarginalizeOp &phi);
    ~svlFactorMarginalizeOp();

    void execute();

 protected:
    bool checkTarget();
};

// svlFactorMaximizeOp class -----------------------------------------------

class svlFactorMaximizeOp : public svlFactorOperation {
 protected:
    const svlFactor * const _A;
    svlFactorIndexRef _mappingA;

 public:
    svlFactorMaximizeOp(svlFactor *target,
	const svlFactor *A);
    svlFactorMaximizeOp(svlFactor *target,
	const svlFactor *A, int v);
    svlFactorMaximizeOp(svlFactor *target,
	const svlFactor *A, const std::set<int>& v);
    svlFactorMaximizeOp(const svlFactorMaximizeOp &phi);
    ~svlFactorMaximizeOp();

    void execute();

 protected:
    bool checkTarget();
};

// svlFactorNormalizeOp class ----------------------------------------------

class svlFactorNormalizeOp : public svlFactorOperation {
 public:
    svlFactorNormalizeOp(svlFactor *target);
    ~svlFactorNormalizeOp();

    void execute();

 protected:
    bool checkTarget();
};

// svlFactorLogNormalizeOp class ----------------------------------------------

class svlFactorLogNormalizeOp : public svlFactorOperation {
 public:
    svlFactorLogNormalizeOp(svlFactor *target);
    ~svlFactorLogNormalizeOp();

    void execute();

 protected:
    bool checkTarget();
};
