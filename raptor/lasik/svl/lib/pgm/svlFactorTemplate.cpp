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
** FILENAME:    svlFactorTemplate.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**
*****************************************************************************/

#include <cstdlib>
#include <cassert>
#include <iostream>
#include <vector>
#include <set>
#include <map>
#include <cmath>
#include <limits>

#include "xmlParser/xmlParser.h"

#include "svlBase.h"
#include "svlFactor.h"
#include "svlFactorTemplate.h"

using namespace std;

// svlFactorTemplate class --------------------------------------------------

svlFactorTemplate::svlFactorTemplate() : _nSize(1)
{
    // do nothing
}

svlFactorTemplate::svlFactorTemplate(int dim, int repeats) : _nSize(1)
{
    while (repeats-- > 0)
	addVariable(dim);
}

svlFactorTemplate::svlFactorTemplate(const vector<int>& d) : _nSize(1)
{
    addVariables(d);
}

svlFactorTemplate::svlFactorTemplate(XMLNode &xml) : _nSize(1)
{
    XMLNode node = xml.getChildNode("Cards");
    SVL_ASSERT(node.getText() != NULL);
    parseString<int>(string(node.getText()), _cards);
    
    for (int i = 0; i < (int)_cards.size(); i++) {
	SVL_ASSERT(_cards[i] > 1);
	_stride.push_back(_nSize);
	_nSize *= _cards[i];
    }

    _entryMapping.resize(_nSize);

    SVL_ASSERT(xml.nChildNode("Entry") == _nSize);
    vector<int> v;
    for (int i = 0; i < xml.nChildNode("Entry"); i++) {
	node = xml.getChildNode("Entry", i);
	if (node.getText() == NULL) {
            // empty entry has value 0
            continue;
        }
	v.clear();
	parseString<int>(string(node.getText()), v);
	SVL_ASSERT(v.size() % 2 == 0);
	_entryMapping[i].reserve(v.size() / 2);
	for (int j = 0; j < (int)v.size(); j += 2) {
	    SVL_ASSERT(v[j] >= 0);
	    _entryMapping[i].push_back(make_pair(v[j], v[j+1]));
	}
    }
}

svlFactorTemplate::~svlFactorTemplate()
{
    // do nothing
}

int svlFactorTemplate::addVariable(int dim)
{
    SVL_ASSERT(dim > 1);

    _cards.push_back(dim);
    _stride.push_back(_nSize);
    _nSize *= dim;

    _entryMapping.resize(_nSize);

    return _nSize;
}

int svlFactorTemplate::addVariables(const vector<int>& d)
{
    for (vector<int>::const_iterator it = d.begin(); it != d.end(); ++it) {
	SVL_ASSERT(*it > 1);
	_cards.push_back(*it);
	_stride.push_back(_nSize);
	_nSize *= (*it);	
    }

    _entryMapping.resize(_nSize);

    return _nSize;    
}

int svlFactorTemplate::maxWeightIndex() const
{
    int maxIndex = 0;

    for (int indx = 0; indx < _nSize; indx++) {
	for (vector<pair<int, int> >::const_iterator it = _entryMapping[indx].begin();
	     it != _entryMapping[indx].end(); ++it) {
	    if (it->first > maxIndex)
		maxIndex = it->first;
	}
    }
    
    return maxIndex;
}

int svlFactorTemplate::minWeightIndex() const
{
    int minIndex = numeric_limits<int>::max();

    for (int indx = 0; indx < _nSize; indx++) {
	for (vector<pair<int, int> >::const_iterator it = _entryMapping[indx].begin();
	     it != _entryMapping[indx].end(); ++it) {
	    if (it->first < minIndex)
		minIndex = it->first;
	}
    }
    
    return minIndex;
}

void svlFactorTemplate::offsetWeights(int offset)
{
    for (int indx = 0; indx < _nSize; indx++) {
	for (vector<pair<int, int> >::iterator it = _entryMapping[indx].begin();
	     it != _entryMapping[indx].end(); ++it) {
	    it->first += offset;
	    SVL_ASSERT(it->first >= 0);
	}
    }
}

set<int> svlFactorTemplate::getWeightIndices() const
{
    set<int> wi;

    for (int indx = 0; indx < _nSize; indx++) {
	for (vector<pair<int, int> >::const_iterator it = _entryMapping[indx].begin();
	     it != _entryMapping[indx].end(); ++it) {
            wi.insert(it->first);
	}
    }

    return wi;
}

int svlFactorTemplate::maxFeatureIndex() const
{
    int maxIndex = -1;

    for (int indx = 0; indx < _nSize; indx++) {
	for (vector<pair<int, int> >::const_iterator it = _entryMapping[indx].begin();
	     it != _entryMapping[indx].end(); ++it) {
	    if (it->second > maxIndex)
		maxIndex = it->second;
	}
    }
    
    return maxIndex;
}

int svlFactorTemplate::minFeatureIndex() const
{
    int minIndex = numeric_limits<int>::max();

    for (int indx = 0; indx < _nSize; indx++) {
	for (vector<pair<int, int> >::const_iterator it = _entryMapping[indx].begin();
	     it != _entryMapping[indx].end(); ++it) {
	    if (it->second < minIndex)
		minIndex = it->second;
	}
    }
    
    return minIndex;
}

// methods for creating factors and accumulating sufficient statistics
svlFactor svlFactorTemplate::createFactor(const vector<int>& vars,
    const vector<double> &weights, const vector<double> &features) const
{
    SVL_ASSERT_MSG(vars.size() == _cards.size(), 
        vars.size() << " == " << _cards.size() << " failed");
    
    svlFactor phi(vars, _cards);
    updateFactor(phi, weights, features);
    
    return phi;
}

void svlFactorTemplate::updateFactor(svlFactor &phi,
    const vector<double> &weights, const vector<double> &features) const
{
    updateLogFactor(phi, weights, features);

    // exponentiate
    for (int indx = 0; indx < _nSize; indx++) {
	phi[indx] = exp(phi[indx]);
    }

#if 0
    // normalize (not really necessary, but helps with testing)
    phi.normalize();
#endif
}

svlFactor svlFactorTemplate::createLogFactor(const vector<int>& vars,
    const vector<double> &weights, const vector<double> &features) const
{
    SVL_ASSERT_MSG(vars.size() == _cards.size(), 
        vars.size() << " == " << _cards.size() << " failed");

    svlFactor phi(vars, _cards);
    updateLogFactor(phi, weights, features);
    
    return phi;
}

void svlFactorTemplate::updateLogFactor(svlFactor &phi,
    const vector<double> &weights, const vector<double> &features) const
{
    SVL_ASSERT((phi.size() == _nSize) &&  (phi.numVars() == (int)_cards.size()));
    SVL_ASSERT((int)weights.size() > maxWeightIndex());
    SVL_ASSERT_MSG((int)features.size() > maxFeatureIndex(), 
        features.size() << " < " << maxFeatureIndex() << " failed");

    // compute w^t x
    double phiMax = -numeric_limits<double>::max();
    for (int indx = 0; indx < _nSize; indx++) {
        phi[indx] = entryLogValue(indx, weights, features);
	if (phi[indx] > phiMax)
            phiMax = phi[indx];
    }

    // normalize
    for (int indx = 0; indx < _nSize; indx++) {
	phi[indx] -= phiMax;
    }
}

svlFactor svlFactorTemplate::createReducedFactor(const vector<int>& vars,
    const vector<double> &weights, const vector<double> &features,
    const vector<int> &assignments) const
{
    SVL_ASSERT(vars.size() == _cards.size());
    SVL_ASSERT(assignments.size() == _cards.size());

    vector<int> unobservedVars;
    vector<int> unobservedCards;
    unobservedVars.reserve(vars.size());
    unobservedCards.reserve(vars.size());
    for (int i = 0; i < (int)vars.size(); i++) {
        if (assignments[i] < 0) {
            unobservedVars.push_back(vars[i]);
            unobservedCards.push_back(_cards[i]);
        }
    }
    
    svlFactor phi(unobservedVars, unobservedCards);
    updateReducedFactor(phi, weights, features, assignments);
    
    return phi;
}

void svlFactorTemplate::updateReducedFactor(svlFactor &phi,
    const vector<double> &weights, const vector<double> &features,
    const vector<int> &assignments) const
{
    updateReducedLogFactor(phi, weights, features, assignments);

    // exponentiate
    for (int phiIndx = 0; phiIndx < (int)phi.size(); phiIndx++) {
	phi[phiIndx] = exp(phi[phiIndx]);
    }
}

svlFactor svlFactorTemplate::createReducedLogFactor(const vector<int>& vars,
    const vector<double> &weights, const vector<double> &features,
    const vector<int> &assignments) const
{
    SVL_ASSERT(vars.size() == _cards.size());
    SVL_ASSERT(assignments.size() == _cards.size());

    vector<int> unobservedVars;
    vector<int> unobservedCards;
    unobservedVars.reserve(vars.size());
    unobservedCards.reserve(vars.size());
    for (int i = 0; i < (int)vars.size(); i++) {
        if (assignments[i] < 0) {
            unobservedVars.push_back(vars[i]);
            unobservedCards.push_back(_cards[i]);
        }
    }
    
    svlFactor phi(unobservedVars, unobservedCards);
    updateReducedLogFactor(phi, weights, features, assignments);
    
    return phi;
}

void svlFactorTemplate::updateReducedLogFactor(svlFactor &phi,
    const vector<double> &weights, const vector<double> &features,
    const vector<int> &assignments) const
{
    SVL_ASSERT(assignments.size() == _cards.size());

    vector<int> unobservedIndex;
    vector<int> unobservedCards;
    unobservedIndex.reserve(assignments.size());
    unobservedCards.reserve(assignments.size());
    for (int i = 0; i < (int)assignments.size(); i++) {
        if (assignments[i] < 0) {
            unobservedIndex.push_back(i);
            unobservedCards.push_back(_cards[i]);
        }
    }

    SVL_ASSERT(unobservedCards.size() == (unsigned)phi.numVars());
    
    // compute w^t x
    vector<int> subAssignments(phi.numVars(), 0);
    vector<int> completeAssignments(assignments);
    double phiMax = -numeric_limits<double>::max();
    for (int phiIndx = 0; phiIndx < (int)phi.size(); phiIndx++) {
        for (int j = 0; j < (int)unobservedIndex.size(); j++) {
            completeAssignments[unobservedIndex[j]] = subAssignments[j];            
        }
        // TO DO: speed this up by using incremental version of indexOf
        int indx = this->indexOf(completeAssignments);

        phi[phiIndx] = entryLogValue(indx, weights, features);
	if (phi[phiIndx] > phiMax)
            phiMax = phi[phiIndx];

        // get next assignment to unobserved variables
        successor(subAssignments, unobservedCards);
    }

    // normalize
    for (int phiIndx = 0; phiIndx < (int)phi.size(); phiIndx++) {
	phi[phiIndx] -= phiMax;
    }
}

void svlFactorTemplate::accumulateStatistics(vector<double> &statistics,
    const vector<int> &assignment, const vector<double> &features,
    double weight) const
{
    // TO DO: need to deal with missing assignments (-1)
    int indx = indexOf(assignment);
    for (vector<pair<int, int> >::const_iterator it = _entryMapping[indx].begin();
	 it != _entryMapping[indx].end(); ++it) {
        if (it->first >= 0) {
            if (it->second < 0) {
                statistics[it->first] += weight;
            } else {
                statistics[it->first] += weight * features[it->second];
            }
        }
    }
}

void svlFactorTemplate::accumulateStatistics(vector<double> &statistics,
    const vector<int> &assignment, const vector<double> &features,
    const svlFactor& weights) const
{
    // TO DO: need to think about this to make it general
    SVL_ASSERT(false);
#if 0
    SVL_ASSERT((weights.size() == _nSize) && (weights.numVars() == (int)_cards.size()));

    for (int indx = 0; indx < _nSize; indx++) {
	for (vector<pair<int, int> >::const_iterator it = _entryMapping[indx].begin();
	     it != _entryMapping[indx].end(); ++it) {
            if (it->first >= 0) {
                if (it->second < 0) {
                    statistics[it->first] += weights[indx];
                } else {
                    statistics[it->first] += weights[indx] * features[it->second];
                }
            }
	}
    }
#endif    
}

// xml output
ostream& svlFactorTemplate::write(ostream& os, int indent) const
{
    string indentStr(indent, ' ');

    os << indentStr << "<FactorTemplate>\n";
    if (this->empty()) {
	os << indentStr << "</FactorTemplate>" << endl;
	return os;
    }

    os << indentStr << "  <Cards>\n   " << indentStr;
    for (int i = 0; i < (int)_cards.size(); i++) {
	os << " " << _cards[i];
    }
    os << "\n" << indentStr << "  </Cards>\n";

    for (int i = 0; i < _nSize; i++) {
	vector<int> assignment;
	this->assignmentOf(i, assignment);
	os << indentStr << "  <!-- variable assignment: ("
	   << toString(assignment)
	   << " ) -->\n";
	os << indentStr << "  <Entry>\n";
	for (int j = 0; j < (int)_entryMapping[i].size(); j++) {
	    os << indentStr << "    " << _entryMapping[i][j].first
	       << " " << _entryMapping[i][j].second << "\n";
	}
	os << indentStr << "  </Entry>\n";
    }

    os << indentStr << "</FactorTemplate>" << endl;
    
    return os;
}


