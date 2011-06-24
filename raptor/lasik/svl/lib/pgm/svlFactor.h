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
** FILENAME:    svlFactor.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**  Implements a discrete factor class for use in Bayesian Networks, Markov
**  random fields (MRFs) and conditional random fields (CRFs). The factor
**  assumes that variables are index by integer. Some optimizations have been
**  incorporated for fast factor multiply and marginalization. Normal and
**  log-space table factors have been implemented. Variable values are indexed
**  from zero to cardinality(v) - 1. Operations are done by creating svlFactor<>Op
**  objects which allow indices to be precomputed for faster message passing
**  algorithms. Empty factors are considered as all ones for operations. This
**  can save a significant amount of memory if large uniform cliques are needed.
**
** TO DO:
**  1. Add code to handle singleton factor: treat as all ones for product.
**  3. svlBaseFactor virtual class and svlLogFactor class
**  4. Handle structured factors (e.g. potts and general pattern potential)
**
*****************************************************************************/

#pragma once

#include <vector>
#include <set>
#include <map>

#include "xmlParser/xmlParser.h"
#include "../base/svlLogger.h"

#define SVL_FACTOR_DEBUG_STATISTICS

// svlFactor class ----------------------------------------------------------

class svlFactorStorage;

class svlFactor {
 protected:
    static const double _tol;

    std::vector<int> _variables;    // list of variables in factor (by index)
    std::map<int, int> _varIndex;   // index of variable in factor (by var)
    std::vector<int> _cards;        // cardinality of each variable (by index)
    std::vector<int> _stride;       // stride of variable in table (by index)

    int _nSize;       // total size of factor (or 1 for singular factors)
    //double *_data;    // data is stored as (a0, b0, ...), (a1, b0, ...), ...
    svlFactorStorage *_data;

 public:
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    static unsigned _dbStatsRefCount;
    static unsigned _dbStatsProductCount;
    static unsigned _dbStatsDivideCount;
    static unsigned _dbStatsAdditionCount;
    static unsigned _dbStatsSubtractionCount;
    static unsigned _dbStatsMarginalCount;
    static unsigned _dbStatsMaximizeCount;
    static unsigned _dbStatsReductionCount;
    static unsigned _dbStatsNormalizeCount;
    static unsigned _dbStatsNormalizeErrors;
    static vector<int> _dbStatsLargestFactor;
    static unsigned _dbStatsLargestFactorSize;
#endif

 public:
    svlFactor();
    svlFactor(int v, int dim);
    svlFactor(const std::vector<int>& v, const std::vector<int>& d);
    svlFactor(const svlFactor& phi);
    svlFactor(XMLNode &xml);
    svlFactor(svlFactorStorage* sharedStorage);
    virtual ~svlFactor();

    inline bool empty() const { return _variables.empty(); }
    inline int size() const { return _nSize; }
    inline int numVars() const { return (int)_variables.size(); }

    inline const std::vector<int>& vars() const;
    inline const std::vector<int>& cards() const;

    inline int variableId(int indx) const;
    inline bool hasVariable(int v) const;
    inline int varCardinality(int v) const;
    inline bool isShared() const;

    int addVariable(int v, int dim);
    int addVariables(const std::vector<int>& v, const std::vector<int>& d);
    int addVariables(const svlFactor& phi);

    // indexing into and out of the data table
    inline int indexOf(int var, int val, int indx = 0) const;
    inline int indexOf(const std::map<int, int>& assignment) const;
    inline int indexOf(const std::vector<int>& assignment) const;
    inline int valueOf(int var, int indx) const;
    inline void assignmentOf(int indx, std::map<int, int>& assignment) const;
    inline void assignmentOf(int indx, std::vector<int>& assignment) const;

    int indexOfMax() const;
    int indexOfMin() const;

    // inline modifications
    virtual svlFactor& initialize();
    svlFactor& fill(double alpha);
    svlFactor& scale(double alpha);
    svlFactor& offset(double alpha);
    virtual svlFactor& normalize();

    // The following functions perform inline operations and
    // are provided as a convenience. Implementations of iterative
    // algorithms should use the various svlFactor<>Op classes.
    virtual svlFactor& marginalize(int v);
    virtual svlFactor& maximize(int v);
    virtual svlFactor& reduce(int var, int val);
    virtual svlFactor& product(const svlFactor& phi);
    virtual svlFactor& divide(const svlFactor& phi);
    virtual svlFactor& add(const svlFactor& phi);
    virtual svlFactor& subtract(const svlFactor& phi);

    // xml input/output functions
    std::ostream& write(std::ostream& os, int indent = 0) const;

    bool dataCompare(const svlFactor& phi) const;
    bool dataCompareAndCopy(const svlFactor& phi);

    inline double& operator[](unsigned index);
    inline double operator[](unsigned index) const;
    bool operator==(const svlFactor& phi);
    svlFactor& operator=(const svlFactor& phi);

    // create mappings for operators
    // TO DO: move into svlFactorOperation class and protect
    std::vector<int> mapFrom(const svlFactor& phi) const;
    std::vector<int> mapOnto(const svlFactor& phi) const {
	return phi.mapFrom(*this);
    }
    std::vector<int> strideMapping(const std::vector<int>& vars) const;
};

// svlFactorStorage class ---------------------------------------------------

class svlFactorStorage {
 protected:
    bool _bShared;    // shared factor
    int _dataSize;    // amount of memory allocated
    double *_data;    // memory allocation

 public:
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    static unsigned _dbStatsCurrentMem;
    static unsigned _dbStatsMaxMem;
    static unsigned _dbStatsMaxTable;
    static unsigned _dbStatsTotalMem;
#endif

 public:
    svlFactorStorage(int nSize = 0, bool bShared = false);
    ~svlFactorStorage();

    // shared status
    inline bool isShared() const { return _bShared; }

    // capacity
    inline int capacity() const { return _dataSize; }
    void reserve(int nSize);

    // copying/setting memory
    void zero(int nSize = -1);
    void fill(double v, int nSize = -1);
    void copy(const double *p, int nSize = -1);
    void copy(const svlFactorStorage *p, int nSize = -1);

    // data access
    inline double& operator[](unsigned index) { return _data[index]; }
    inline double operator[](unsigned index) const { return _data[index]; }
};

// Inline function implementations ------------------------------------------

inline const std::vector<int>& svlFactor::vars() const
{
    return _variables;
}

inline const std::vector<int>& svlFactor::cards() const
{
    return _cards;
}

inline int svlFactor::variableId(int indx) const {
    return _variables[indx];
}

inline bool svlFactor::hasVariable(int v) const {
    return (_varIndex.find(v) != _varIndex.end());
}

inline int svlFactor::varCardinality(int v) const {
    //SVL_ASSERT(hasVariable(v));
    return _cards[_varIndex.find(v)->second];
}

inline bool svlFactor::isShared() const {
    return (_data == NULL ? false : _data->isShared());
}

inline int svlFactor::indexOf(int var, int val, int indx) const {
    //SVL_ASSERT(hasVariable(var));
    int vi = _varIndex.find(var)->second;
    //SVL_ASSERT((val >= 0) && (val < _cards[vi]));

    val -= ((int)(indx / _stride[vi])) % _cards[vi];
    indx += val * _stride[vi];

    return indx;
}

inline int svlFactor::indexOf(const std::map<int, int>& assignment) const
{
    int indx = 0;
    for (int i = 0; i < (int)_variables.size(); i++) {
	std::map<int, int>::const_iterator it = assignment.find(_variables[i]);
	//SVL_ASSERT(it != assignment.end());
	//SVL_ASSERT((it->second >= 0) && (it->second < _cards[i]));
	indx += it->second * _stride[i];
    }

    return indx;
}

inline int svlFactor::indexOf(const std::vector<int>& assignment) const
{
    //SVL_ASSERT(assignment.size() == _variables.size());
    int indx = 0;
    for (int i = 0; i < (int)_variables.size(); i++) {
	//SVL_ASSERT((assignment[i] >= 0) && (assignment[i] < _cards[i]));
	indx += assignment[i] * _stride[i];
    }
    return indx;
}

inline int svlFactor::valueOf(int var, int indx) const {
    //SVL_ASSERT(hasVariable(var));
    int vi = _varIndex.find(var)->second;
    return ((int)(indx / _stride[vi])) % _cards[vi];
}

inline void svlFactor::assignmentOf(int indx, std::map<int, int>& assignment) const
{
    for (int i = 0; i < (int)_variables.size(); i++) {
	assignment[_variables[i]] = ((int)(indx / _stride[i])) % _cards[i];
    }
}

inline void svlFactor::assignmentOf(int indx, std::vector<int>& assignment) const
{
    assignment.resize(_variables.size());
    for (int i = 0; i < (int)_variables.size(); i++) {
	assignment[i] = ((int)(indx / _stride[i])) % _cards[i];
    }
}

inline double& svlFactor::operator[](unsigned index)
{
    //SVL_ASSERT(index < (unsigned)_nSize);
    return (*_data)[index];
}

inline double svlFactor::operator[](unsigned index) const
{
    //SVL_ASSERT(index < (unsigned)_nSize);
    return (*_data)[index];
}

