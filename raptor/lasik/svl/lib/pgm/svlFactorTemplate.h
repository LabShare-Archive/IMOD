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
** FILENAME:    svlFactorTemplate.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**  Template for creating factor as exp{w^T x} where w is a weight vector
**  and x is an observed feature vector. The class supports parameter
**  sharing. See svlGeneralCRF.h for more details. A special case of < 0
**  for a feature index implies a constant term of 1. This saves having to
**  explicitly provide features for non-conditional factors.
**
*****************************************************************************/

#pragma once

#include <vector>
#include <set>
#include <map>

#include "xmlParser/xmlParser.h"

#include "../base/svlLogger.h"
#include "svlFactor.h"

// svlFactorTemplate class ---------------------------------------------------

class svlFactorTemplate {
 protected:
    std::vector<int> _cards;        // cardinality of each variable
    std::vector<int> _stride;       // stride of variable in table
    
    int _nSize;                     // total number of entries in factor

    std::vector<std::vector<std::pair<int, int> > > _entryMapping;
                                    // entry_i = sum(w_j * x_j)

 public:
    svlFactorTemplate();
    svlFactorTemplate(int dim, int repeats = 1);
    svlFactorTemplate(const std::vector<int>& d);
    //svlFactorTemplate(const svlTemplateFactor& t);
    svlFactorTemplate(XMLNode &xml);
    virtual ~svlFactorTemplate();

    inline bool empty() const { return _cards.empty(); }
    inline int size() const { return _nSize; }
    inline int numVars() const { return (int)_cards.size(); }

    inline int varCardinality(int v) const;

    int addVariable(int dim);
    int addVariables(const std::vector<int>& d);

    int maxWeightIndex() const;
    int minWeightIndex() const;
    void offsetWeights(int offset);
    std::set<int> getWeightIndices() const;
    int maxFeatureIndex() const;
    int minFeatureIndex() const;

    // methods for creating factors
    svlFactor createFactor(const std::vector<int>& vars,
	const std::vector<double> &weights,
	const std::vector<double> &features) const;
    void updateFactor(svlFactor &phi, 
	const std::vector<double> &weights,
	const std::vector<double> &features) const;

    svlFactor createLogFactor(const std::vector<int>& vars,
	const std::vector<double> &weights,
	const std::vector<double> &features) const;
    void updateLogFactor(svlFactor &phi, 
	const std::vector<double> &weights,
	const std::vector<double> &features) const;

    svlFactor createReducedFactor(const std::vector<int>& vars,
	const std::vector<double> &weights,
	const std::vector<double> &features,
        const std::vector<int> &assignments) const;        
    void updateReducedFactor(svlFactor &phi,
	const std::vector<double> &weights,
	const std::vector<double> &features,
        const std::vector<int> &assignments) const;
        
    svlFactor createReducedLogFactor(const std::vector<int>& vars,
	const std::vector<double> &weights,
	const std::vector<double> &features,
        const std::vector<int> &assignments) const;        
    void updateReducedLogFactor(svlFactor &phi,
	const std::vector<double> &weights,
	const std::vector<double> &features,
        const std::vector<int> &assignments) const;

    // methods for accumulating sufficient statistics
    void accumulateStatistics(std::vector<double> &statistics,
	const std::vector<int> &assignment,
	const std::vector<double> &features,
	double weight = 1.0) const;
    void accumulateStatistics(std::vector<double> &statistics,
	const std::vector<int> &assignment,
	const std::vector<double> &features,
	const svlFactor& weights) const;

    // indexing into and out of the (weight, features) to entry mapping
    inline int indexOf(int vi, int val, int indx = 0) const;
    inline int indexOf(const std::vector<int>& assignment) const;
    inline int valueOf(int vi, int indx) const;
    inline void assignmentOf(int indx, std::vector<int>& assignment) const;
    inline double entryLogValue(int indx, const std::vector<double>& weights,
        const std::vector<double>& features) const;

    // xml output
    std::ostream& write(std::ostream& os, int indent = 0) const;

    // access to mappings
    inline std::vector<std::pair<int, int> >& operator[](unsigned index);
    inline std::vector<std::pair<int, int> > operator[](unsigned index) const;
};


// Inline function implementations ------------------------------------------

inline int svlFactorTemplate::varCardinality(int v) const {
    SVL_ASSERT((v >= 0) && (v < (int)_cards.size()));
    return _cards[v];
}
 
inline int svlFactorTemplate::indexOf(int vi, int val, int indx) const {
    SVL_ASSERT((vi >= 0) && (vi < (int)_cards.size()));
    SVL_ASSERT((val >= 0) && (val < _cards[vi]));
    
    indx -= ((int)(indx / _stride[vi])) % _cards[vi];
    indx += val * _stride[vi];
    
    return indx;
}

inline int svlFactorTemplate::indexOf(const std::vector<int>& assignment) const
{
    SVL_ASSERT(assignment.size() == _cards.size());
    int indx = 0;
    for (int i = 0; i < (int)_cards.size(); i++) {
	SVL_ASSERT((assignment[i] >= 0) && (assignment[i] < _cards[i]));
	indx += assignment[i] * _stride[i];
    }
    return indx;
}

inline int svlFactorTemplate::valueOf(int vi, int indx) const {
    SVL_ASSERT((vi >= 0) && (vi < (int)_cards.size()));
    return ((int)(indx / _stride[vi])) % _cards[vi];
}

inline void svlFactorTemplate::assignmentOf(int indx, std::vector<int>& assignment) const
{
    assignment.resize(_cards.size());
    for (int i = 0; i < (int)_cards.size(); i++) {
	assignment[i] = ((int)(indx / _stride[i])) % _cards[i];
    }
}

inline double svlFactorTemplate::entryLogValue(int indx, const std::vector<double>& weights,
    const std::vector<double>& features) const
{    
    double phiVal = 0.0;
    
    for (vector<pair<int, int> >::const_iterator it = _entryMapping[indx].begin();
         it != _entryMapping[indx].end(); ++it) {
        double w = (it->first >= 0) ? weights[it->first] : 1.0;
        double v = (it->second >= 0) ? features[it->second] : 1.0;
        phiVal += w * v;
    }

    return phiVal;
}

inline std::vector<std::pair<int, int> >& svlFactorTemplate::operator[](unsigned index)
{
    SVL_ASSERT(index < (unsigned)_nSize);
    return _entryMapping[index]; 
}

inline std::vector<std::pair<int, int> > svlFactorTemplate::operator[](unsigned index) const
{
    SVL_ASSERT(index < (unsigned)_nSize);
    return _entryMapping[index];
}

