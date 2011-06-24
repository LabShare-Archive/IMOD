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
** FILENAME:    svlFactorOperations.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**
*****************************************************************************/

#include <stdlib.h>
#include <cassert>
#include <iostream>
#include <vector>
#include <cmath>
#include <limits>

#include "svlBase.h"
#include "svlFactor.h"
#include "svlFactorOperations.h"

using namespace std;

// svlFactorIndexCache class -----------------------------------------------

svlFactorIndexCache::svlFactorIndexCache()
{
    // do nothing
}

svlFactorIndexCache::~svlFactorIndexCache()
{
    // do nothing
}

void svlFactorIndexCache::clear()
{
    _cache.clear();
}

svlFactorIndexRef svlFactorIndexCache::find(const vector<int>& index)
{
    if (!svlFactorOperation::USE_SHARED_INDEX_CACHE) {
        return svlFactorIndexRef(new vector<int>(index));
    }

    // find index vector
    pair<multimap<unsigned, svlFactorIndexRef>::const_iterator,
        multimap<unsigned, svlFactorIndexRef>::const_iterator> range;
    range = _cache.equal_range((unsigned)index.size());
    for (multimap<unsigned, svlFactorIndexRef>::const_iterator it = range.first;
         it != range.second; ++it) {
        if (*(it->second) == index) {
            return it->second;
        }
    }

    // not found, so insert new vector
    svlFactorIndexRef p(new vector<int>(index));
    _cache.insert(make_pair(index.size(), p));

    return p;
}

// svlFactorOperation class ------------------------------------------------

bool svlFactorOperation::CACHE_INDEX_MAPPING = true;
bool svlFactorOperation::USE_SHARED_INDEX_CACHE = false;
svlFactorIndexCache svlFactorOperation::GLOBAL_FACTOR_INDEX_CACHE;

svlFactorOperation::svlFactorOperation(svlFactor *target) :
    _target(target)
{
    SVL_ASSERT(target != NULL);
    // do nothing
}

svlFactorOperation::svlFactorOperation(const svlFactorOperation& op) :
    _target(op._target)
{
    // do nothing
}

svlFactorOperation::~svlFactorOperation()
{
    // do nothing
}

void svlFactorOperation::clearIndexCache()
{
    GLOBAL_FACTOR_INDEX_CACHE.clear();
}


inline svlFactorIndexRef svlFactorOperation::indexRef(const vector<int>& mapping) const
{
    return GLOBAL_FACTOR_INDEX_CACHE.find(mapping);
}

// svlFactorBinaryOp -------------------------------------------------------

svlFactorBinaryOp::svlFactorBinaryOp(svlFactor *target,
    const svlFactor *A, const svlFactor *B) :
    svlFactorOperation(target), _A(A), _B(B)
{
    SVL_ASSERT((A != NULL) && (B != NULL));
    initialize();
}

svlFactorBinaryOp::svlFactorBinaryOp(const svlFactorBinaryOp &phi) :
    svlFactorOperation(phi), _A(phi._A), _B(phi._B)
{
    _mappingA = phi._mappingA;
    _mappingB = phi._mappingB;
}

svlFactorBinaryOp::~svlFactorBinaryOp()
{
    // do nothing
}

void svlFactorBinaryOp::initialize()
{
    // add variables and check domains match
    if (_target->empty()) {
	_target->addVariables(*_A);
	_target->addVariables(*_B);
    } else {
	SVL_ASSERT(checkTarget());
    }

    // create mappings
    if (CACHE_INDEX_MAPPING) {
        _mappingA = indexRef(_target->mapFrom(*_A));
        _mappingB = indexRef(_target->mapFrom(*_B));
    } else {
        _mappingA = indexRef(_A->strideMapping(_target->vars()));
        _mappingB = indexRef(_B->strideMapping(_target->vars()));
    }
}

bool svlFactorBinaryOp::checkTarget()
{
    //if ((_A->empty()) || (_B->empty()))
    //    return false;

    for (int i = 0; i < _A->numVars(); i++) {
	if (!_target->hasVariable(_A->variableId(i))) {
            SVL_LOG(SVL_LOG_ERROR, "target vars:" <<
                toString(_target->vars()) << "; A vars:" <<
                toString(_A->vars()));
	    return false;
	}
    }

    for (int i = 0; i < _B->numVars(); i++) {
	if (!_target->hasVariable(_B->variableId(i))) {
            SVL_LOG(SVL_LOG_ERROR, "target vars:" <<
                toString(_target->vars()) << "; B vars:" <<
                toString(_B->vars()));
	    return false;
	}
    }

    // TO DO: check reverse direction and domain sizes

    return true;
}

// svlFactorNAryOp --------------------------------------------------------

svlFactorNAryOp::svlFactorNAryOp(svlFactor *target,
    const svlFactor *A, const svlFactor *B) :
    svlFactorOperation(target)
{
    SVL_ASSERT((A != NULL) && (B != NULL));

    // add factors to list
    _factors.push_back(A);
    _factors.push_back(B);

    initialize();
}

svlFactorNAryOp::svlFactorNAryOp(svlFactor *target,
    const std::vector<const svlFactor *>& A) :
    svlFactorOperation(target)
{
    SVL_ASSERT(!A.empty());

    // add factors to list
    for (unsigned i = 0; i < _factors.size(); i++) {
	SVL_ASSERT(_factors[i] != NULL);
    }
    _factors.insert(_factors.end(), A.begin(), A.end());

    initialize();
}

svlFactorNAryOp::svlFactorNAryOp(const svlFactorNAryOp &phi) :
    svlFactorOperation(phi)
{
    _factors = phi._factors;
    _mappings = phi._mappings;
}

svlFactorNAryOp::~svlFactorNAryOp()
{
    // do nothing
}

void svlFactorNAryOp::initialize()
{
    // add variables and check domains match
    if (_target->empty()) {
	for (unsigned i = 0; i < _factors.size(); i++) {
	    _target->addVariables(*_factors[i]);
	}
    } else {
	SVL_ASSERT(checkTarget());
    }

    // create mappings
    if (CACHE_INDEX_MAPPING) {
        _mappings.resize(_factors.size());
        for (unsigned i = 0; i < _factors.size(); i++) {
            _mappings[i] = indexRef(_target->mapFrom(*_factors[i]));
        }
    } else {
        _mappings.resize(_factors.size());
        for (unsigned i = 0; i < _factors.size(); i++) {
            _mappings[i] = indexRef(_factors[i]->strideMapping(_target->vars()));
        }
    }
}

bool svlFactorNAryOp::checkTarget()
{
    for (unsigned i = 0; i < _factors.size(); i++) {
	for (int j = 0; j < _factors[i]->numVars(); j++) {
	    if (!_target->hasVariable(_factors[i]->variableId(j))) {
		return false;
	    }
	}
    }

    // TO DO: check reverse direction and domain sizes

    return true;
}

// svlFactorAtomicOp class -------------------------------------------------

svlFactorAtomicOp::svlFactorAtomicOp(svlFactorOperation *op) :
    svlFactorOperation(op->target())
{
    _computations.push_back(op);
}

svlFactorAtomicOp::svlFactorAtomicOp(const vector<svlFactorOperation *>& ops) :
    svlFactorOperation(ops.back()->target()), _computations(ops)
{
    SVL_ASSERT(!_computations.empty());
    for (unsigned i = 0; i < _computations.size(); i++) {
	SVL_ASSERT(_computations[i] != NULL);
    }
    _target = _computations.back()->target();
}

svlFactorAtomicOp::~svlFactorAtomicOp()
{
    // delete computation tree
    for (unsigned i = 0; i < _computations.size(); i++) {
	delete _computations[i];
    }
    _computations.clear();
}

void svlFactorAtomicOp::execute()
{
    for (unsigned i = 0; i < _computations.size(); i++) {
	_computations[i]->execute();
    }
}

void svlFactorAtomicOp::addOperation(svlFactorOperation *op)
{
    SVL_ASSERT(op != NULL);
    _computations.push_back(op);
    _target = _computations.back()->target();
}

// svlFactorCopyOp class ---------------------------------------------------

svlFactorCopyOp::svlFactorCopyOp(svlFactor *target, const svlFactor *A) :
    svlFactorOperation(target), _A(A)
{
    SVL_ASSERT(A != NULL);
    initialize();
}

svlFactorCopyOp::svlFactorCopyOp(const svlFactorCopyOp& phi) :
    svlFactorOperation(phi), _A(phi._A)
{
    // do nothing
}

svlFactorCopyOp::~svlFactorCopyOp()
{
    // do nothing
}

void svlFactorCopyOp::execute()
{
    memcpy(&_target[0], &_A[0], _target->size() * sizeof(double));
}

void svlFactorCopyOp::initialize()
{
    // add variables and check domains match
    if (_target->empty()) {
	_target->addVariables(*_A);
    } else {
	SVL_ASSERT(checkTarget());
    }
}

bool svlFactorCopyOp::checkTarget()
{
    if ((_A->vars() != _target->vars()) ||
        (_A->cards() != _target->cards())) {
        return false;
    }

    return true;
}

// svlFactorProductOp ------------------------------------------------------

svlFactorProductOp::svlFactorProductOp(svlFactor *target,
    const svlFactor *A, const svlFactor *B) :
    svlFactorNAryOp(target, A, B)
{
    // do nothing
}

svlFactorProductOp::svlFactorProductOp(svlFactor *target,
    const std::vector<const svlFactor *>& A) :
    svlFactorNAryOp(target, A)
{
    // do nothing
}

svlFactorProductOp::svlFactorProductOp(const svlFactorProductOp &phi) :
    svlFactorNAryOp(phi)
{
    // do nothing
}

svlFactorProductOp::~svlFactorProductOp()
{
    // do nothing
}

void svlFactorProductOp::execute()
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    svlFactor::_dbStatsProductCount += (int)_factors.size() - 1;
#endif

    if (CACHE_INDEX_MAPPING) {
        if (_factors[0]->empty()) {
            _target->fill(1.0);
        } else {
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] = (*_factors[0])[(*_mappings[0])[i]];
            }
        }

        for (int k = 1; k < (int)_factors.size(); k++) {
            if (_factors[k]->empty())
                continue;
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] *= (*_factors[k])[(*_mappings[k])[i]];
            }
        }
    } else {
        vector<int> assignment(_target->numVars(), 0);

        // speed up two-factor common case
        if ((_factors.size() == 2) && (!_factors[0]->empty()) && (!_factors[1]->empty())) {
            int kPhiA = 0;
            int kPhiB = 0;
            for (int k = 0; k < _target->size(); k++) {
                //SVL_ASSERT((kPhiA >= 0) && (kPhiA < _factors[0]->size()));
                //SVL_ASSERT((kPhiB >= 0) && (kPhiB < _factors[1]->size()));
                (*_target)[k] = (*_factors[0])[kPhiA] * (*_factors[1])[kPhiB];
                // update assignment
                assignment[0] += 1;
                kPhiA += (*_mappings[0])[0];
                kPhiB += (*_mappings[1])[0];
                for (int i = 1; (i < _target->numVars()); i++) {
                    if (assignment[i - 1] < _target->cards()[i - 1])
                        break;
                    assignment[i - 1] = 0;
                    assignment[i] += 1;
                    kPhiA += (*_mappings[0])[i];
                    kPhiB += (*_mappings[1])[i];
                }
            }

            return;
        }

        if (_factors[0]->empty()) {
            _target->fill(1.0);
        } else {
            int kPhi = 0;
            for (int k = 0; k < _target->size(); k++) {
                //SVL_ASSERT((kPhi >= 0) && (kPhi < _factors[0]->size()));
                (*_target)[k] = (*_factors[0])[kPhi];
                for (int i = 0; i < _target->numVars(); i++) {
                    assignment[i] += 1;
                    kPhi += (*_mappings[0])[i];
                    if (assignment[i] == _target->cards()[i]) {
                        assignment[i] = 0;
                    } else {
                        break;
                    }
                }
            }
        }

        for (int f = 1; f < (int)_factors.size(); f++) {
            if (_factors[f]->empty())
                continue;

            int kPhi = 0;
            for (int k = 0; k < _target->size(); k++) {
                //SVL_ASSERT((kPhi >= 0) && (kPhi < _factors[f]->size()));
                (*_target)[k] *= (*_factors[f])[kPhi];
                for (int i = 0; i < _target->numVars(); i++) {
                    assignment[i] += 1;
                    kPhi += (*_mappings[f])[i];
                    if (assignment[i] == _target->cards()[i]) {
                        assignment[i] = 0;
                    } else {
                        break;
                    }
                }
            }
        }
    }
}

// svlFactorDivideOp -------------------------------------------------------

svlFactorDivideOp::svlFactorDivideOp(svlFactor *target,
    const svlFactor *A, const svlFactor *B) :
    svlFactorBinaryOp(target, A, B)
{
    // do nothing
}

svlFactorDivideOp::svlFactorDivideOp(const svlFactorDivideOp &phi) :
    svlFactorBinaryOp(phi)
{
    // do nothing
}

svlFactorDivideOp::~svlFactorDivideOp()
{
    // do nothing
}

void svlFactorDivideOp::execute()
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    svlFactor::_dbStatsDivideCount += 1;
#endif

    if (CACHE_INDEX_MAPPING) {
        for (int i = 0; i < _target->size(); i++) {
            // check for zero (and prevent zero/zero)
            if ((*_A)[(*_mappingA)[i]] == 0.0) {
                (*_target)[i] = 0.0;
                continue;
            }
            (*_target)[i] = (*_A)[(*_mappingA)[i]] / (*_B)[(*_mappingB)[i]];
        }
    } else {
        vector<int> assignment(_target->numVars(), 0);

        int kA = 0;
        int kB = 0;
        for (int k = 0; k < _target->size(); k++) {
            //SVL_ASSERT((kA >= 0) && (kA < _A->size()));
            //SVL_ASSERT((kB >= 0) && (kB < _B->size()));

            // check for zero (and prevent zero/zero)
            if ((*_A)[kA] == 0.0) {
                (*_target)[k] = 0.0;
            } else {
                (*_target)[k] = (*_A)[kA] / (*_B)[kB];
            }

            // update assignment
            assignment[0] += 1;
            kA += (*_mappingA)[0];
            kB += (*_mappingB)[0];
            for (int i = 1; i < _target->numVars(); i++) {
                if (assignment[i - 1] < _target->cards()[i - 1])
                    break;
                assignment[i - 1] = 0;
                assignment[i] += 1;
                kA += (*_mappingA)[i];
                kB += (*_mappingB)[i];
            }
        }
    }
}

// svlFactorAdditionOp -----------------------------------------------------

svlFactorAdditionOp::svlFactorAdditionOp(svlFactor *target,
    const svlFactor *A, const svlFactor *B) :
    svlFactorNAryOp(target, A, B)
{
    // do nothing
}

svlFactorAdditionOp::svlFactorAdditionOp(svlFactor *target,
    const std::vector<const svlFactor *>& A) :
    svlFactorNAryOp(target, A)
{
    // do nothing
}

svlFactorAdditionOp::svlFactorAdditionOp(const svlFactorAdditionOp &phi) :
    svlFactorNAryOp(phi)
{
    // do nothing
}

svlFactorAdditionOp::~svlFactorAdditionOp()
{
    // do nothing
}

void svlFactorAdditionOp::execute()
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    svlFactor::_dbStatsAdditionCount += (int)_factors.size() - 1;
#endif

    if (CACHE_INDEX_MAPPING) {

#if 0
        if (_factors[0]->empty()) {
            _target->fill(0.0);
        } else if (_target != _factors[0]) {
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] = (*_factors[0])[(*_mappings[0])[i]];
            }
        }

        for (int k = 1; k < (int)_factors.size(); k++) {
            if (_factors[k]->empty())
                continue;
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] += (*_factors[k])[(*_mappings[k])[i]];
            }
        }
#else
        // accelerate common case of unary message
        if (_target->numVars() == 1) {
            if (_factors[0]->empty()) {
                _target->fill(0.0);
            } else if (_target != _factors[0]) {
                for (int i = 0; i < _target->size(); i++) {
                    (*_target)[i] = (*_factors[0])[i];
                }
            }

            for (int k = 1; k < (int)_factors.size(); k++) {
                if (_factors[k]->empty())
                    continue;
                for (int i = 0; i < _target->size(); i++) {
                    (*_target)[i] += (*_factors[k])[i];
                }
            }
        } else {
            if (_factors[0]->empty()) {
                _target->fill(0.0);
            } else if (_target != _factors[0]) {
                for (int i = 0; i < _target->size(); i++) {
                    (*_target)[i] = (*_factors[0])[(*_mappings[0])[i]];
                }
            }

            for (int k = 1; k < (int)_factors.size(); k++) {
                if (_factors[k]->empty())
                    continue;
                for (int i = 0; i < _target->size(); i++) {
                    (*_target)[i] += (*_factors[k])[(*_mappings[k])[i]];
                }
            }
        }
#endif

    } else {
        vector<int> assignment(_target->numVars(), 0);

        // speed up two-factor common case
        if ((_factors.size() == 2) && (!_factors[0]->empty()) && (!_factors[1]->empty())) {
            int kPhiA = 0;
            int kPhiB = 0;
            for (int k = 0; k < _target->size(); k++) {
                //SVL_ASSERT((kPhiA >= 0) && (kPhiA < _factors[0]->size()));
                //SVL_ASSERT((kPhiB >= 0) && (kPhiB < _factors[1]->size()));
                (*_target)[k] = (*_factors[0])[kPhiA] + (*_factors[1])[kPhiB];
                // udpate assignment
                assignment[0] += 1;
                kPhiA += (*_mappings[0])[0];
                kPhiB += (*_mappings[1])[0];
                for (int i = 1; i < _target->numVars(); i++) {
                    if (assignment[i - 1] < _target->cards()[i - 1])
                        break;
                    assignment[i - 1] = 0;
                    assignment[i] += 1;
                    kPhiA += (*_mappings[0])[i];
                    kPhiB += (*_mappings[1])[i];
                }
            }

            return;
        }

        if (_factors[0]->empty()) {
            _target->fill(0.0);
        } else if (_target != _factors[0]) {
            int kPhi = 0;
            for (int k = 0; k < _target->size(); k++) {
                //SVL_ASSERT((kPhi >= 0) && (kPhi < _factors[0]->size()));
                // operation
                (*_target)[k] = (*_factors[0])[kPhi];
                // update assignment
                for (int i = 0; i < _target->numVars(); i++) {
                    assignment[i] += 1;
                    kPhi += (*_mappings[0])[i];
                    if (assignment[i] == _target->cards()[i]) {
                        assignment[i] = 0;
                    } else {
                        break;
                    }
                }
            }
        }

        for (int f = 1; f < (int)_factors.size(); f++) {
            if (_factors[f]->empty())
                continue;

            int kPhi = 0;
            for (int k = 0; k < _target->size(); k++) {
                //SVL_ASSERT((kPhi >= 0) && (kPhi < _factors[f]->size()));
                // operation
                (*_target)[k] += (*_factors[f])[kPhi];
                // update assignment
                for (int i = 0; i < _target->numVars(); i++) {
                    assignment[i] += 1;
                    kPhi += (*_mappings[f])[i];
                    if (assignment[i] == _target->cards()[i]) {
                        assignment[i] = 0;
                    } else {
                        break;
                    }
                }
            }
        }
    }
}

// svlFactorSubtractOp -------------------------------------------------------

svlFactorSubtractOp::svlFactorSubtractOp(svlFactor *target,
    const svlFactor *A, const svlFactor *B) :
    svlFactorBinaryOp(target, A, B)
{
    // do nothing
}

svlFactorSubtractOp::svlFactorSubtractOp(const svlFactorSubtractOp &phi) :
    svlFactorBinaryOp(phi)
{
    // do nothing
}

svlFactorSubtractOp::~svlFactorSubtractOp()
{
    // do nothing
}

void svlFactorSubtractOp::execute()
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    svlFactor::_dbStatsSubtractionCount += 1;
#endif

    if (CACHE_INDEX_MAPPING) {
        if (_A->empty()) {
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] = - (*_B)[(*_mappingB)[i]];
            }
        } else if (_B->empty()) {
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] = (*_A)[(*_mappingA)[i]];
            }
        } else {
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] = (*_A)[(*_mappingA)[i]] - (*_B)[(*_mappingB)[i]];
            }
        }
    } else {
        vector<int> assignment(_target->numVars(), 0);

        int kA = 0;
        int kB = 0;
        if (_A->empty()) {
            for (int k = 0; k < _target->size(); k++) {
                // operation
                (*_target)[k] = -(*_B)[kB];
                // update assignment
                assignment[0] += 1;
                kB += (*_mappingB)[0];
                for (int i = 1; i < _target->numVars(); i++) {
                    if (assignment[i - 1] < _target->cards()[i - 1])
                        break;
                    assignment[i - 1] = 0;
                    assignment[i] += 1;
                    kB += (*_mappingB)[i];
                }
            }
        } else if (_B->empty()) {
            for (int k = 0; k < _target->size(); k++) {
                // operation
                (*_target)[k] = (*_A)[kA];
                // update assignment
                assignment[0] += 1;
                kA += (*_mappingA)[0];
                for (int i = 1; i < _target->numVars(); i++) {
                    if (assignment[i - 1] < _target->cards()[i - 1])
                        break;
                    assignment[i - 1] = 0;
                    assignment[i] += 1;
                    kA += (*_mappingA)[i];
                }
            }
        } else {
            for (int k = 0; k < _target->size(); k++) {
                // operation
                (*_target)[k] = (*_A)[kA] - (*_B)[kB];
                // update assignment
                assignment[0] += 1;
                kA += (*_mappingA)[0];
                kB += (*_mappingB)[0];
                for (int i = 1; i < _target->numVars(); i++) {
                    if (assignment[i - 1] < _target->cards()[i - 1])
                        break;
                    assignment[i - 1] = 0;
                    assignment[i] += 1;
                    kA += (*_mappingA)[i];
                    kB += (*_mappingB)[i];
                }
            }
        }
    }
}

// svlFactorWeightedSumOp ----------------------------------------------------

svlFactorWeightedSumOp::svlFactorWeightedSumOp(svlFactor *target,
    const svlFactor *A, const svlFactor *B, double wA, double wB) :
    svlFactorBinaryOp(target, A, B), _weightA(wA), _weightB(wB)
{
    // do nothing
}

svlFactorWeightedSumOp::svlFactorWeightedSumOp(const svlFactorWeightedSumOp &phi) :
    svlFactorBinaryOp(phi), _weightA(phi._weightA), _weightB(phi._weightB)
{
    // do nothing
}

svlFactorWeightedSumOp::~svlFactorWeightedSumOp()
{
    // do nothing
}

void svlFactorWeightedSumOp::execute()
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    svlFactor::_dbStatsAdditionCount += 1;
#endif

    if (CACHE_INDEX_MAPPING) {
        if (_A->empty()) {
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] = _weightB * (*_B)[(*_mappingB)[i]];
            }
        } else if (_B->empty()) {
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] = _weightA * (*_A)[(*_mappingA)[i]];
            }
        } else {
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] = _weightA * (*_A)[(*_mappingA)[i]] +
                    _weightB * (*_B)[(*_mappingB)[i]];
            }
        }
    } else {
        vector<int> assignment(_target->numVars(), 0);

        int kA = 0;
        int kB = 0;

        if (_A->empty()) {
            for (int k = 0; k < _target->size(); k++) {
                //SVL_ASSERT((kB >= 0) && (kB < _B->size()));
                // operation
                (*_target)[k] = _weightB * (*_B)[kB];
                // update assignment
                for (int i = 0; i < _target->numVars(); i++) {
                    assignment[i] += 1;
                    kB += (*_mappingB)[i];
                    if (assignment[i] == _target->cards()[i]) {
                        assignment[i] = 0;
                    } else {
                        break;
                    }
                }
            }
        } else if (_B->empty()) {
            for (int k = 0; k < _target->size(); k++) {
                //SVL_ASSERT((kA >= 0) && (kA < _A->size()));
                // operation
                (*_target)[k] = _weightA * (*_A)[kA];
                // update assignment
                for (int i = 0; i < _target->numVars(); i++) {
                    assignment[i] += 1;
                    kA += (*_mappingA)[i];
                    if (assignment[i] == _target->cards()[i]) {
                        assignment[i] = 0;
                    } else {
                        break;
                    }
                }
            }
        } else {
            for (int k = 0; k < _target->size(); k++) {
                //SVL_ASSERT((kA >= 0) && (kA < _A->size()));
                //SVL_ASSERT((kB >= 0) && (kB < _B->size()));
                // operation
                (*_target)[k] = _weightA * (*_A)[kA] + _weightB * (*_B)[kB];
                // update assignment
                for (int i = 0; i < _target->numVars(); i++) {
                    assignment[i] += 1;
                    kA += (*_mappingA)[i];
                    kB += (*_mappingB)[i];
                    if (assignment[i] == _target->cards()[i]) {
                        assignment[i] = 0;
                    } else {
                        break;
                    }
                }
            }
        }
    }
}

// svlFactorMarginalizeOp class --------------------------------------------

svlFactorMarginalizeOp::svlFactorMarginalizeOp(svlFactor *target,
    const svlFactor *A) :
    svlFactorOperation(target), _A(A)
{
    SVL_ASSERT(A != NULL);
    SVL_ASSERT(checkTarget());

    // create mapping
    if (CACHE_INDEX_MAPPING) {
        _mappingA = indexRef(_target->mapOnto(*_A));
    } else {
        _mappingA = indexRef(_target->strideMapping(_A->vars()));
    }
}


svlFactorMarginalizeOp::svlFactorMarginalizeOp(svlFactor *target,
    const svlFactor *A, int v) :
    svlFactorOperation(target), _A(A)
{
    SVL_ASSERT(A != NULL);
    SVL_ASSERT(_A->hasVariable(v));

    // initialize target
    if (_target->empty()) {
	for (int i = 0; i < _A->numVars(); i++) {
	    if (_A->variableId(i) == v)
		continue;
	    _target->addVariable(_A->variableId(i),
		_A->varCardinality(_A->variableId(i)));
	}
    } else {
	SVL_ASSERT(checkTarget());
    }

    // create mapping
    if (CACHE_INDEX_MAPPING) {
        _mappingA = indexRef(_target->mapOnto(*_A));
    } else {
        _mappingA = indexRef(_target->strideMapping(_A->vars()));
    }
}

svlFactorMarginalizeOp::svlFactorMarginalizeOp(svlFactor *target,
    const svlFactor *A, const std::set<int>& v) :
    svlFactorOperation(target), _A(A)
{
    SVL_ASSERT(A != NULL);

    // add variables
    if (_target->empty()) {
	for (int i = 0; i < _A->numVars(); i++) {
	    if (v.find(_A->variableId(i)) != v.end())
		continue;
	    _target->addVariable(_A->variableId(i),
		_A->varCardinality(_A->variableId(i)));
	}
    } else {
	SVL_ASSERT(checkTarget());
    }

    // create mapping
    if (CACHE_INDEX_MAPPING) {
        _mappingA = indexRef(_target->mapOnto(*_A));
    } else {
        _mappingA = indexRef(_target->strideMapping(_A->vars()));
    }
}

svlFactorMarginalizeOp::svlFactorMarginalizeOp(const svlFactorMarginalizeOp &phi) :
    svlFactorOperation(phi), _A(phi._A)
{
    _mappingA = phi._mappingA;
}

svlFactorMarginalizeOp::~svlFactorMarginalizeOp()
{
    // do nothing
}

void svlFactorMarginalizeOp::execute()
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    svlFactor::_dbStatsMarginalCount += 1;
#endif
    _target->fill(0.0);

    if (CACHE_INDEX_MAPPING) {
        for (int i = 0; i < (int)_mappingA->size(); i++) {
            (*_target)[(*_mappingA)[i]] += (*_A)[i];
        }
    } else {
        vector<int> assignment(_A->numVars(), 0);

        int kTarget = 0;
        for (int k = 0; k < _A->size(); k++) {
            //SVL_ASSERT((kTarget >= 0) && (kTarget < _target->size()));
            // perform operation
            (*_target)[kTarget] += (*_A)[k];
            // increment assignment
            assignment[0] += 1;
            kTarget += (*_mappingA)[0];
            for (int i = 1; i < _A->numVars(); i++) {
                if (assignment[i - 1] < _A->cards()[i - 1])
                    break;
                assignment[i - 1] = 0;
                assignment[i] += 1;
                kTarget += (*_mappingA)[i];
            }
        }
    }
}

bool svlFactorMarginalizeOp::checkTarget()
{
    // TO DO
    return true;
}

// svlFactorMaximizeOp class ------------------------------------------------

svlFactorMaximizeOp::svlFactorMaximizeOp(svlFactor *target,
    const svlFactor *A) :
    svlFactorOperation(target), _A(A)
{
    SVL_ASSERT(A != NULL);
    SVL_ASSERT(checkTarget());

    // create mapping
    if (CACHE_INDEX_MAPPING) {
        _mappingA = indexRef(_target->mapOnto(*_A));
    } else {
        _mappingA = indexRef(_target->strideMapping(_A->vars()));
    }
}

svlFactorMaximizeOp::svlFactorMaximizeOp(svlFactor *target,
    const svlFactor *A, int v) :
    svlFactorOperation(target), _A(A)
{
    SVL_ASSERT(A != NULL);
    SVL_ASSERT(_A->hasVariable(v));

    // initialize target
    if (_target->empty()) {
	for (int i = 0; i < _A->numVars(); i++) {
	    if (_A->variableId(i) == v)
		continue;
	    _target->addVariable(_A->variableId(i),
		_A->varCardinality(_A->variableId(i)));
	}
    } else {
	SVL_ASSERT(checkTarget());
    }

    // create mapping
    if (CACHE_INDEX_MAPPING) {
        _mappingA = indexRef(_target->mapOnto(*_A));
    } else {
        _mappingA = indexRef(_target->strideMapping(_A->vars()));
    }
}

svlFactorMaximizeOp::svlFactorMaximizeOp(svlFactor *target,
    const svlFactor *A, const std::set<int>& v) :
    svlFactorOperation(target), _A(A)
{
    SVL_ASSERT(A != NULL);

    // add variables
    if (_target->empty()) {
	for (int i = 0; i < _A->numVars(); i++) {
	    if (v.find(_A->variableId(i)) != v.end())
		continue;
	    _target->addVariable(_A->variableId(i),
		_A->varCardinality(_A->variableId(i)));
	}
    } else {
	SVL_ASSERT(checkTarget());
    }

    // create mapping
    if (CACHE_INDEX_MAPPING) {
        _mappingA = indexRef(_target->mapOnto(*_A));
    } else {
        _mappingA = indexRef(_target->strideMapping(_A->vars()));
    }
}

svlFactorMaximizeOp::svlFactorMaximizeOp(const svlFactorMaximizeOp &phi) :
    svlFactorOperation(phi), _A(phi._A)
{
    _mappingA = phi._mappingA;
}

svlFactorMaximizeOp::~svlFactorMaximizeOp()
{
    // do nothing
}

void svlFactorMaximizeOp::execute()
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    svlFactor::_dbStatsMaximizeCount += 1;
#endif
    if (_target->empty())
        return;

    _target->fill(-numeric_limits<double>::max());
    if (CACHE_INDEX_MAPPING) {
        for (int i = 0; i < (int)_mappingA->size(); i++) {
            if ((*_target)[(*_mappingA)[i]] < (*_A)[i]) {
                (*_target)[(*_mappingA)[i]] = (*_A)[i];
            }
        }
    } else {
        vector<int> assignment(_A->numVars(), 0);

        int kTarget = 0;
        for (int k = 0; k < _A->size(); k++) {
            //SVL_ASSERT((kTarget >= 0) && (kTarget < _target->size()));
            // perform operation
            if ((*_target)[kTarget] < (*_A)[k])
                (*_target)[kTarget] = (*_A)[k];
            // increment assignment
            assignment[0] += 1;
            kTarget += (*_mappingA)[0];
            for (int i = 1; i < _A->numVars(); i++) {
                if (assignment[i - 1] < _A->cards()[i - 1])
                    break;
                assignment[i - 1] = 0;
                assignment[i] += 1;
                kTarget += (*_mappingA)[i];
            }
        }
    }
}

bool svlFactorMaximizeOp::checkTarget()
{
    // TO DO
    SVL_ASSERT(!_A->empty());

    return true;
}

// svlFactorNormalizeOp class ----------------------------------------------

svlFactorNormalizeOp::svlFactorNormalizeOp(svlFactor *target) :
    svlFactorOperation(target)
{
    // do nothing
}

svlFactorNormalizeOp::~svlFactorNormalizeOp()
{
    // do nothing
}

void svlFactorNormalizeOp::execute()
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    svlFactor::_dbStatsNormalizeCount += 1;
#endif

    if (_target->empty())
	return;

    double total = 0.0;
    for (int i = 0; i < _target->size(); i++) {
        total += (*_target)[i];
    }
    if (total > 0.0) {
        if (total != 1.0) {
            double invTotal = 1.0 / total;
            for (int i = 0; i < _target->size(); i++) {
                (*_target)[i] *= invTotal;
            }
        }
    } else {
#ifdef SVL_FACTOR_DEBUG_STATISTICS
	svlFactor::_dbStatsNormalizeErrors += 1;
#endif
        _target->fill(1.0 / (double)_target->size());
    }
}

bool svlFactorNormalizeOp::checkTarget()
{
    // normalizing into yourself, so always true
    return true;
}

// svlLogFactorNormalizeOp class -------------------------------------------

svlFactorLogNormalizeOp::svlFactorLogNormalizeOp(svlFactor *target) :
    svlFactorOperation(target)
{
    // do nothing
}

svlFactorLogNormalizeOp::~svlFactorLogNormalizeOp()
{
    // do nothing
}

void svlFactorLogNormalizeOp::execute()
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    svlFactor::_dbStatsNormalizeCount += 1;
#endif

    if (_target->empty())
	return;

    double maxVal = -numeric_limits<double>::max();
    for (int i = 0; i < _target->size(); i++) {
        if ((*_target)[i] > maxVal)
            maxVal = (*_target)[i];
    }
    for (int i = 0; i < _target->size(); i++) {
        (*_target)[i] -= maxVal;
    }
}

bool svlFactorLogNormalizeOp::checkTarget()
{
    // normalizing into yourself, so always true
    return true;
}

// configuration --------------------------------------------------------

class svlFactorOperationsConfig : public svlConfigurableModule {
public:
    svlFactorOperationsConfig() : svlConfigurableModule("svlPGM.svlFactorOperations") { }
    ~svlFactorOperationsConfig() { }

    void usage(ostream &os) const {
        os << "      cacheIndexMapping :: default: " <<
            (svlFactorOperation::CACHE_INDEX_MAPPING ? "true" : "false") << "\n"
           << "      useSharedIndexCache :: default: " <<
            (svlFactorOperation::USE_SHARED_INDEX_CACHE ? "true" : "false") << "\n";
    }

    void setConfiguration(const char *name, const char *value) {
        // factor operation cache
        if (!strcmp(name, "cacheIndexMapping")) {
            svlFactorOperation::CACHE_INDEX_MAPPING =
                (!strcasecmp(value, "TRUE") || !strcmp(value, "1"));
        } else if (!strcmp(name, "useSharedIndexCache")) {
            svlFactorOperation::USE_SHARED_INDEX_CACHE =
                (!strcasecmp(value, "TRUE") || !strcmp(value, "1"));
        } else {
            SVL_LOG(SVL_LOG_FATAL, "unrecognized configuration option for " << this->name());
        }
    }
};

static svlFactorOperationsConfig gFactorOperationsConfig;
