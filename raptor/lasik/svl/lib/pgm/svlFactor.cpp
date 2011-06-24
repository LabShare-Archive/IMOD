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
** FILENAME:    svlFactor.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**
*****************************************************************************/

#include <stdlib.h>
#include <cassert>
#include <iostream>
#include <vector>
#include <cmath>
#include <limits>

#include "xmlParser/xmlParser.h"
#include "svlBase.h"
#include "svlFactor.h"

using namespace std;

// Globals and constants ----------------------------------------------------

const double svlFactor::_tol = 1.0e-9;

#ifdef SVL_FACTOR_DEBUG_STATISTICS
unsigned svlFactor::_dbStatsRefCount = 0;
unsigned svlFactor::_dbStatsProductCount = 0;
unsigned svlFactor::_dbStatsAdditionCount = 0;
unsigned svlFactor::_dbStatsSubtractionCount = 0;
unsigned svlFactor::_dbStatsDivideCount = 0;
unsigned svlFactor::_dbStatsMarginalCount = 0;
unsigned svlFactor::_dbStatsMaximizeCount = 0;
unsigned svlFactor::_dbStatsReductionCount = 0;
unsigned svlFactor::_dbStatsNormalizeCount = 0;
unsigned svlFactor::_dbStatsNormalizeErrors = 0;
vector<int> svlFactor::_dbStatsLargestFactor;
unsigned svlFactor::_dbStatsLargestFactorSize = 0;
static svlFactor dbDummyFactor;
#endif

#ifdef SVL_FACTOR_DEBUG_STATISTICS
unsigned svlFactorStorage::_dbStatsCurrentMem = 0;
unsigned svlFactorStorage::_dbStatsMaxMem = 0;
unsigned svlFactorStorage::_dbStatsMaxTable = 0;
unsigned svlFactorStorage::_dbStatsTotalMem = 0;
#endif

// svlFactor class ------------------------------------------------------

svlFactor::svlFactor() : _nSize(0), _data(NULL)
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsRefCount += 1;
#endif
}

svlFactor::svlFactor(int v, int d) : _nSize(0), _data(NULL)
{
    addVariable(v, d);
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsRefCount += 1;
#endif
}

svlFactor::svlFactor(const std::vector<int>& v, const std::vector<int>& d) :
    _nSize(0), _data(NULL)
{
    addVariables(v, d);
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsRefCount += 1;
#endif
}

svlFactor::svlFactor(const svlFactor& phi) :
    _variables(phi._variables), _varIndex(phi._varIndex), _cards(phi._cards),
    _stride(phi._stride), _nSize(phi._nSize), _data(NULL)
{
    if (phi._data != NULL) {
        if (phi._data->isShared()) {
            _data = phi._data;
        } else {
            _data = new svlFactorStorage(_nSize);
            _data->copy(phi._data);
        }
    }

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsRefCount += 1;
#endif
}

svlFactor::svlFactor(XMLNode &xml) : _nSize(0), _data(NULL)
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsRefCount += 1;
#endif
    XMLNode node = xml.getChildNode("Vars");
    SVL_ASSERT(node.getText() != NULL);
    parseString<int>(string(node.getText()), _variables);

    node = xml.getChildNode("Cards");
    SVL_ASSERT(node.getText() != NULL);
    parseString<int>(string(node.getText()), _cards);

    SVL_ASSERT(_variables.size() == _cards.size());
    for (int i = 0; i < (int)_variables.size(); i++) {
	_varIndex[_variables[i]] = i;
	_stride.push_back(_nSize == 0 ? 1 : _nSize);
	_nSize = _stride.back() * _cards[i];
    }

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    if (_nSize > (int)_dbStatsLargestFactorSize) {
        _dbStatsLargestFactorSize = _nSize;
        _dbStatsLargestFactor = _cards;
    }
#endif

    if (!_variables.empty()) {
        _data = new svlFactorStorage(_nSize);
	node = xml.getChildNode("Data");
        if (node.getText() == NULL) {
            initialize();
        } else {
            vector<double> v;
            parseString<double>(string(node.getText()), v);
            if (v.empty()) {
                initialize();
            } else {
                SVL_ASSERT_MSG(v.size() == (unsigned)_nSize,
                    v.size() << " == " << _nSize
                    << " failed for clique {" << toString(_variables) << "}");
                for (int i = 0; i < _nSize; i++) {
                    (*_data)[i] = v[i];
                }
            }
        }
    }
}

svlFactor::svlFactor(svlFactorStorage* sharedStorage) : _nSize(0), _data(NULL)
{
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsRefCount += 1;
#endif
    SVL_ASSERT((sharedStorage != NULL) && (sharedStorage->isShared()));
    _data = sharedStorage;
}

svlFactor::~svlFactor()
{
    if ((_data != NULL) && (!_data->isShared())) {
	delete _data;
    }

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsRefCount -= 1;
    if ((_dbStatsRefCount == 0) && ((_dbStatsProductCount > 0) ||
	    (_dbStatsDivideCount > 0) || (_dbStatsMarginalCount > 0) ||
	    (_dbStatsAdditionCount > 0) || (_dbStatsSubtractionCount > 0) ||
	    (_dbStatsMaximizeCount > 0) || (_dbStatsNormalizeCount > 0))) {
	cerr << "svlFactor class computed " << _dbStatsProductCount << " products" << endl;
	cerr << "svlFactor class computed " << _dbStatsDivideCount << " divisions" << endl;
	cerr << "svlFactor class computed " << _dbStatsAdditionCount << " additions" << endl;
	cerr << "svlFactor class computed " << _dbStatsSubtractionCount << " subtractions" << endl;
	cerr << "svlFactor class computed " << _dbStatsMarginalCount << " marginals" << endl;
	cerr << "svlFactor class computed " << _dbStatsMaximizeCount << " maximizations" << endl;
	cerr << "svlFactor class computed " << _dbStatsReductionCount << " reductions" << endl;
	cerr << "svlFactor class computed " << _dbStatsNormalizeCount << " normalizations" << endl;
	cerr << "svlFactor class computed " << _dbStatsNormalizeErrors << " normalization errors" << endl;
        cerr << "svlFactor class largest factor [" << toString(_dbStatsLargestFactor) << " ]" << endl;

        cerr << "svlFactorStorage class allocated " << svlFactorStorage::_dbStatsTotalMem << " total entries" << endl;
        cerr << "svlFactorStorage class allocated " << svlFactorStorage::_dbStatsMaxMem << " max. concurrent entries" << endl;
        cerr << "svlFactorStorage class allocated " << svlFactorStorage::_dbStatsCurrentMem << " current entries" << endl;
        cerr << "svlFactorStorage class allocated " << svlFactorStorage::_dbStatsMaxTable << " entries in largest factor" << endl;
    }
#endif
}

int svlFactor::addVariable(int v, int d)
{
    vector<int> vvec(1, v);
    vector<int> dvec(1, d);
    return addVariables(vvec, dvec);
}

int svlFactor::addVariables(const vector<int>& v, const vector<int>& d)
{
    SVL_ASSERT(v.size() == d.size());

    int oldSize = _nSize;
    for (int i = 0; i < (int)v.size(); i++) {
	SVL_ASSERT(d[i] >= 1);
	SVL_ASSERT(!hasVariable(v[i]));

	_variables.push_back(v[i]);
	_varIndex[v[i]] = (int)_variables.size() - 1;
	_cards.push_back(d[i]);
	_stride.push_back(_nSize == 0 ? 1 : _nSize);
	_nSize = _stride.back() * d[i];
    }

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    if (_nSize > (int)_dbStatsLargestFactorSize) {
        _dbStatsLargestFactorSize = _nSize;
        _dbStatsLargestFactor = _cards;
    }
#endif

    if (oldSize == _nSize)
        return _nSize;

    if (_data == NULL) {
        _data = new svlFactorStorage(_nSize);
	initialize();
    } else if (_data->isShared()) {
        _data->reserve(_nSize);
	// replicate table
        if (oldSize != 0) {
            for (int i = oldSize; i < _nSize; i += oldSize) {
                memcpy(&(*_data)[i], &(*_data)[0], oldSize * sizeof(double));
            }
        } else {
            initialize();
        }
    } else {
        svlFactorStorage *newData = new svlFactorStorage(_nSize);
	// replicate table
        SVL_ASSERT(oldSize != 0);
        for (int i = 0; i < _nSize; i += oldSize) {
            memcpy(&(*newData)[i], &(*_data)[0], oldSize * sizeof(double));
        }
        delete _data;
        _data = newData;
    }

    return _nSize;
}

int svlFactor::addVariables(const svlFactor& phi)
{
    vector<int> vvec;
    vector<int> dvec;

    for (int i = 0; i < (int)phi._variables.size(); i++) {
	int v = phi._variables[i];
	if (hasVariable(v)) {
	    SVL_ASSERT(_cards[_varIndex.find(v)->second] == phi._cards[i]);
	    continue;
	}

        vvec.push_back(v);
        dvec.push_back(phi._cards[i]);
    }

    return addVariables(vvec, dvec);
}

int svlFactor::indexOfMax() const
{
    int indx = 0;
    for (int i = 1; i < _nSize; i++) {
        if ((*_data)[indx] < (*_data)[i]) {
            indx = i;
        }
    }

    return indx;
}

int svlFactor::indexOfMin() const
{
    int indx = 0;
    for (int i = 1; i < _nSize; i++) {
        if ((*_data)[indx] > (*_data)[i]) {
            indx = i;
        }
    }

    return indx;
}

svlFactor& svlFactor::initialize()
{
    return fill(1.0);
}

svlFactor& svlFactor::fill(double alpha)
{
    if (_data == NULL) return *this;
    _data->fill(alpha, _nSize);
    return *this;
}

svlFactor& svlFactor::scale(double alpha)
{
    if (_data == NULL) return *this;

    for (int i = 0; i < _nSize; i++) {
        (*_data)[i] *= alpha;
    }

    return *this;
}

svlFactor& svlFactor::offset(double alpha)
{
    if (_data == NULL) return *this;

    for (int i = 0; i < _nSize; i++) {
        (*_data)[i] += alpha;
    }

    return *this;
}

svlFactor& svlFactor::normalize()
{
    if (_data == NULL) return *this;

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsNormalizeCount += 1;
#endif
    double total = 0.0;
    for (int i = 0; i < _nSize; i++) {
        total += (*_data)[i];
    }
    if (total > 0.0) {
        if (total != 1.0) {
            double invTotal = 1.0 / total;
            for (int i = 0; i < _nSize; i++) {
                (*_data)[i] *= invTotal;
            }
        }
    } else {
#ifdef SVL_FACTOR_DEBUG_STATISTICS
	_dbStatsNormalizeErrors += 1;
#endif
        fill(1.0 / (double)_nSize);
    }

    return *this;
}

svlFactor& svlFactor::marginalize(int v)
{
    SVL_ASSERT(hasVariable(v));

    int i, j, k;
    int newSize;

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsMarginalCount += 1;
#endif

    if (_variables.size() == 1) {
        // return the singleton factor
        *this = svlFactor();
        return *this;
    }

    int index = _varIndex[v];

    newSize = _nSize / _cards[index];
    svlFactorStorage *newData = new svlFactorStorage(newSize);
    newData->zero();

    for (i = 0, k = 0; k < newSize; k++) {
        for (j = 0; j < _cards[index]; j++) {
            (*newData)[k] += (*_data)[i + j * _stride[index]];
        }

        if (k % _stride[index] == (_stride[index] - 1)) {
            i += _stride[index] * (_cards[index] - 1) + 1;
        } else {
            i += 1;
        }
    }

    if (!_data->isShared()) {
        delete _data;
        _data = newData;
    } else {
        _data->copy(newData, newSize);
        delete newData;
    }
    _nSize = newSize;

    _variables.erase(_variables.begin() + index);
    _cards.erase(_cards.begin() + index);
    _varIndex.erase(v);
    for (map<int, int>::iterator it = _varIndex.begin();
	 it != _varIndex.end(); ++it) {
	if (it->second > index) {
	    it->second -= 1;
	}
    }
    _stride.resize(_variables.size());
    for (i = 0; i < (int)_variables.size(); i++) {
        _stride[i] = (i == 0 ? 1 : _stride[i - 1] * _cards[i - 1]);
    }

    return *this;
}

svlFactor& svlFactor::maximize(int v)
{
    SVL_ASSERT(hasVariable(v));

    int i, j, k;
    int newSize;

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsMaximizeCount += 1;
#endif

    if (_variables.size() == 1) {
        // return the singleton factor
        *this = svlFactor();
        return *this;
    }

    int index = _varIndex[v];

    newSize = _nSize / _cards[index];
    svlFactorStorage *newData = new svlFactorStorage(newSize);
    newData->fill(-numeric_limits<double>::max());

    for (i = 0, k = 0; k < newSize; k++) {
        for (j = 0; j < _cards[index]; j++) {
            if ((*newData)[k] < (*_data)[i + j * _stride[index]]) {
                (*newData)[k] = (*_data)[i + j * _stride[index]];
            }
        }

        if (k % _stride[index] == (_stride[index] - 1)) {
            i += _stride[index] * (_cards[index] - 1) + 1;
        } else {
            i += 1;
        }
    }

    if (!_data->isShared()) {
        delete _data;
        _data = newData;
    } else {
        _data->copy(newData, newSize);
        delete newData;
    }
    _nSize = newSize;

    _variables.erase(_variables.begin() + index);
    _cards.erase(_cards.begin() + index);
    _varIndex.erase(v);
    for (map<int, int>::iterator it = _varIndex.begin();
	 it != _varIndex.end(); ++it) {
	if (it->second > index) {
	    it->second -= 1;
	}
    }
    _stride.resize(_variables.size());
    for (i = 0; i < (int)_variables.size(); i++) {
        _stride[i] = (i == 0 ? 1 : _stride[i - 1] * _cards[i - 1]);
    }

    return *this;
}

svlFactor& svlFactor::reduce(int var, int val)
{
    SVL_ASSERT(hasVariable(var));

    int i, k;
    int newSize;

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsReductionCount += 1;
#endif

    if (_variables.size() == 1) {
        // return the singleton factor
        *this = svlFactor();
        return *this;
    }

    int index = _varIndex[var];
    SVL_ASSERT_MSG((val >= 0) && (val < _cards[index]),
        val << " >= " << 0 << " && " << val << " < " << _cards[index] << " failed");

    newSize = _nSize / _cards[index];
    svlFactorStorage *newData = new svlFactorStorage(newSize);

    for (i = 0, k = 0; k < newSize; k++) {
	(*newData)[k] = (*_data)[i + val * _stride[index]];

	if (k % _stride[index] == (_stride[index] - 1)) {
	    i += _stride[index] * (_cards[index] - 1) + 1;
	} else {
	    i += 1;
	}
    }

    if (!_data->isShared()) {
        delete _data;
        _data = newData;
    } else {
        _data->copy(newData, newSize);
        delete newData;
    }
    _nSize = newSize;

    _variables.erase(_variables.begin() + index);
    _cards.erase(_cards.begin() + index);
    _varIndex.erase(var);
    for (map<int, int>::iterator it = _varIndex.begin();
	 it != _varIndex.end(); ++it) {
	if (it->second > index) {
	    it->second -= 1;
	}
    }
    _stride.resize(_variables.size());
    for (i = 0; i < (int)_variables.size(); i++) {
        _stride[i] = (i == 0 ? 1 : _stride[i - 1] * _cards[i - 1]);
    }

    return *this;
}

svlFactor& svlFactor::product(const svlFactor& phi)
{
    int index, value;
    int i, k;
    int kPhi;

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsProductCount += 1;
#endif

    // singleton factor
    if (phi._nSize == 0) {
        return *this;
    } else if (_nSize == 0) {
        *this = phi;
        return *this;
    }

    // check variables are the correct size and add missing to this
    for (int i = 0; i < (int)phi._variables.size(); i++) {
        if (hasVariable(phi._variables[i])) {
            index = _varIndex[phi._variables[i]];
            SVL_ASSERT(_cards[index] == phi._cards[i]);
        } else {
            // replicates factor entries to all values of new variable
            addVariable(phi._variables[i], phi._cards[i]);
        }
    }

    // special case for multiplying by a single variable factor
    if (phi._variables.size() == 1) {
        index = _varIndex[phi._variables[0]];
        for (i = 0, k = 0; k < _nSize; k++) {
            (*_data)[k] *= (*phi._data)[i];
            if (k % _stride[index] == _stride[index] - 1) {
                i = (i + 1) % phi._nSize;
            }
        }

        return *this;
    }

    // full factor multiplication
    for (k = 0; k < _nSize; k++) {
        kPhi = 0;
        for (vector<int>::const_iterator it = phi._variables.begin();
	     it != phi._variables.end(); ++it) {
            value = valueOf(*it, k);
            kPhi = phi.indexOf(*it, value, kPhi);
        }
        (*_data)[k] *= (*phi._data)[kPhi];
    }

    return *this;
}

svlFactor& svlFactor::divide(const svlFactor& phi)
{
    int index, value;
    int i, k;
    int kPhi;

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsDivideCount += 1;
#endif

    // singleton factor
    if (phi._nSize == 0) {
        return *this;
    } else if (_nSize == 0) {
        *this = phi;
        for (int i = 0; i < phi._nSize; i++) {
            (*_data)[i] = 1.0 / (*phi._data)[i];
        }
    }

    // check variables are the correct size and add missing to this
    for (int i = 0; i < (int)phi._variables.size(); i++) {
        if (hasVariable(phi._variables[i])) {
            index = _varIndex[phi._variables[i]];
            SVL_ASSERT(_cards[index] == phi._cards[i]);
        } else {
            // replicates factor entries to all values of new variable
            addVariable(phi._variables[i], phi._cards[i]);
        }
    }

    // special case for dividing by a single variable factor
    if (phi._variables.size() == 1) {
        index = _varIndex[phi._variables[0]];
        for (i = 0, k = 0; k < _nSize; k++) {
            (*_data)[k] /= (*phi._data)[i];
            if (k % _stride[index] == _stride[index] - 1) {
                i = (i + 1) % phi._nSize;
            }
        }

        return *this;
    }

    // full factor division
    for (k = 0; k < _nSize; k++) {
        kPhi = 0;
        for (vector<int>::const_iterator it = phi._variables.begin();
	     it != phi._variables.end(); ++it) {
            value = valueOf(*it, k);
            kPhi = phi.indexOf(*it, value, kPhi);
        }
        (*_data)[k] /= (*phi._data)[kPhi];
    }

    return *this;
}

svlFactor& svlFactor::add(const svlFactor& phi)
{
    int index, value;
    int i, k;
    int kPhi;

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsAdditionCount += 1;
#endif

    // singleton factor
    if (phi._nSize == 0) {
        return *this;
    } else if (_nSize == 0) {
        *this = phi;
        return *this;
    }

    // check variables are the correct size and add missing to this
    for (int i = 0; i < (int)phi._variables.size(); i++) {
        if (hasVariable(phi._variables[i])) {
            index = _varIndex[phi._variables[i]];
            SVL_ASSERT(_cards[index] == phi._cards[i]);
        } else {
            // replicates factor entries to all values of new variable
            addVariable(phi._variables[i], phi._cards[i]);
        }
    }

    // special case for single variable factor
    if (phi._variables.size() == 1) {
        index = _varIndex[phi._variables[0]];
        for (i = 0, k = 0; k < _nSize; k++) {
            (*_data)[k] += (*phi._data)[i];
            if (k % _stride[index] == _stride[index] - 1) {
                i = (i + 1) % phi._nSize;
            }
        }

        return *this;
    }

    // full factor addition
    for (k = 0; k < _nSize; k++) {
        kPhi = 0;
        for (vector<int>::const_iterator it = phi._variables.begin();
	     it != phi._variables.end(); ++it) {
            value = valueOf(*it, k);
            kPhi = phi.indexOf(*it, value, kPhi);
        }
        (*_data)[k] += (*phi._data)[kPhi];
    }

    return *this;
}

svlFactor& svlFactor::subtract(const svlFactor& phi)
{
    int index, value;
    int i, k;
    int kPhi;

#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsSubtractionCount += 1;
#endif

    // singleton factor
    if (phi._nSize == 0) {
        return *this;
    } else if (_nSize == 0) {
        *this = phi;
        return *this;
    }

    // check variables are the correct size and add missing to this
    for (int i = 0; i < (int)phi._variables.size(); i++) {
        if (hasVariable(phi._variables[i])) {
            index = _varIndex[phi._variables[i]];
            SVL_ASSERT(_cards[index] == phi._cards[i]);
        } else {
            // replicates factor entries to all values of new variable
            addVariable(phi._variables[i], phi._cards[i]);
        }
    }

    // special case for single variable factor
    if (phi._variables.size() == 1) {
        index = _varIndex[phi._variables[0]];
        for (i = 0, k = 0; k < _nSize; k++) {
            (*_data)[k] -= (*phi._data)[i];
            if (k % _stride[index] == _stride[index] - 1) {
                i = (i + 1) % phi._nSize;
            }
        }

        return *this;
    }

    // full factor subtraction
    for (k = 0; k < _nSize; k++) {
        kPhi = 0;
        for (vector<int>::const_iterator it = phi._variables.begin();
	     it != phi._variables.end(); ++it) {
            value = valueOf(*it, k);
            kPhi = phi.indexOf(*it, value, kPhi);
        }
        (*_data)[k] -= (*phi._data)[kPhi];
    }

    return *this;
}

ostream& svlFactor::write(ostream& os, int indent) const
{
    string indentStr(indent, ' ');

    os << indentStr << "<Factor>\n";
    if (this->empty()) {
	os << indentStr << "</Factor>" << endl;
	return os;
    }

    os << indentStr << "  <Vars>\n   " << indentStr;
    for (int i = 0; i < (int)_variables.size(); i++) {
	os << " " << _variables[i];
    }
    os << "\n" << indentStr << "  </Vars>\n";
    os << indentStr << "  <Cards>\n   " << indentStr;
    for (int i = 0; i < (int)_cards.size(); i++) {
	os << " " << _cards[i];
    }
    os << "\n" << indentStr << "  </Cards>\n";

    os << indentStr << "  <Data>\n";
    for (int i = 0; i < _nSize; i++) {
	os << indentStr << "    ";
        os << (*_data)[i] << endl;
    }
    os << indentStr << "  </Data>\n";

    os << indentStr << "</Factor>" << endl;

    return os;
}

bool svlFactor::dataCompare(const svlFactor& phi) const
{
    if (phi._nSize != _nSize) return false;

#if 0
    for (int i = 0; i < _nSize; i++) {
        if (fabs((*_data)[i] - (*phi._data)[i]) > _tol) {
            return false;
        }
    }

    return true;
#else
    // unroll loop
    const double *p = &(*_data)[0];
    const double *q = &(*phi._data)[0];
    for (int i = _nSize / 2; i != 0; i--, p += 2, q += 2) {
        if ((fabs(p[0] - q[0]) > _tol) || (fabs(p[1] - q[1]) > _tol)) {
            return false;
        }
    }

    return ((_nSize % 2 == 0) || (fabs(p[0] - q[0]) <= _tol));
#endif
}

bool svlFactor::dataCompareAndCopy(const svlFactor& phi)
{
    if (phi._nSize != _nSize) {
        *this = phi;
        return false;
    }

    for (int i = 0; i < _nSize; i++) {
        if (fabs((*_data)[i] - (*phi._data)[i]) > _tol) {
            memcpy(&(*_data)[i], &(*phi._data)[i], (_nSize - i) * sizeof(double));
            return false;
        }
        (*_data)[i] = (*phi._data)[i];
    }

    return true;
}

bool svlFactor::operator==(const svlFactor& phi)
{
    int value;

    if ((phi._nSize != _nSize) || (_variables.size() != phi._variables.size())) {
        return false;
    } else if (_nSize == 0) {
        return true;
    }

    // check all variables are the same (and have the same dimension)
    for (int i = 0; i < (int)_variables.size(); i++) {
	map<int, int>::const_iterator vi = phi._varIndex.find(_variables[i]);
        if (vi == phi._varIndex.end()) {
            return false;
        }
        if (phi._cards[vi->second] != _cards[i]) {
            return false;
        }
    }

    for (int k = 0; k < _nSize; k++) {
        int k_dash = 0;
        for (int i = 0; i < (int)_variables.size(); i++) {
            value = valueOf(_variables[i], k);
            k_dash = phi.indexOf(_variables[i], value, k_dash);
        }

        if (fabs((*_data)[k] - (*phi._data)[k_dash]) > _tol) {
            return false;
        }
    }

    return true;
}

svlFactor& svlFactor::operator=(const svlFactor& phi)
{
    if (this->_data == phi._data) {
        // Really want to check (*this == phi), but check on
        // data is much quicker. Also works for _data == NULL
        return *this;
    }

    if ((_data != NULL) && (_nSize != phi._nSize)) {
        if (_data->isShared()) {
            _data->reserve(phi._nSize);
        } else {
            delete _data;
            _data = NULL;
        }
    }

    _nSize = phi._nSize;
    _variables = phi._variables;
    _varIndex = phi._varIndex;
    _cards = phi._cards;
    _stride = phi._stride;

    if (phi._data != NULL) {
        if (_data == NULL) {
	    _data = new svlFactorStorage(_nSize);
	}
        _data->copy(phi._data, _nSize);
    }

    return *this;
}

vector<int> svlFactor::mapFrom(const svlFactor& phi) const
{
    if (phi.empty() || this->empty()) {
        return vector<int>(size(), 0);
    }

    vector<int> mapping(size());

    // special case for speed
    if (phi.numVars() == 1) {
	SVL_ASSERT(hasVariable(phi._variables[0]));
	int vi = _varIndex.find(phi._variables[0])->second;
 	for (vector<int>::iterator it = mapping.begin(); it != mapping.end(); ) {
            for (int kPhi = 0; kPhi < _cards[vi]; kPhi++) {
                std::fill(it, it + _stride[vi], kPhi);
                it += _stride[vi];
            }
        }
	return mapping;
    }

    if (numVars() == 1) {
        map<int, int>::const_iterator v = phi._varIndex.find(_variables[0]);
        SVL_ASSERT(v != phi._varIndex.end());
        const int phiStride = phi._stride[v->second];
        int kPhi = 0;
        for (vector<int>::iterator it = mapping.begin(); it != mapping.end(); it++) {
            *it = kPhi;
            kPhi += phiStride;
        }
        return mapping;
    }

    // slower case
    vector<int> assignment(numVars(), 0);
    const vector<int> phiStride(phi.strideMapping(_variables));

    int kPhi = 0;
    for (int k = 0; k < size(); k++) {
	SVL_ASSERT((kPhi >= 0) && (kPhi < phi.size()));
        mapping[k] = kPhi;
        assignment[0] += 1;
        kPhi += phiStride[0];
        for (int i = 1; i < numVars(); i++) {
            if (assignment[i - 1] < _cards[i - 1])
                break;
            assignment[i - 1] = 0;
            assignment[i] += 1;
            kPhi += phiStride[i];
        }
    }

    return mapping;
}

vector<int> svlFactor::strideMapping(const vector<int>& vars) const
{
    vector<int> strideMap(vars.size(), 0);
    map<int, int>::const_iterator lastV = _varIndex.end();
    for (unsigned i = 0; i < vars.size(); i++) {
        map<int, int>::const_iterator v = _varIndex.find(vars[i]);
        if (v != _varIndex.end()) {
            strideMap[i] = _stride[v->second];
        }
        if (lastV != _varIndex.end()) {
            strideMap[i] -= _cards[lastV->second] * _stride[lastV->second];
        }
        lastV = v;
    }

    return strideMap;
}

// svlFactorStorage class ---------------------------------------------------

svlFactorStorage::svlFactorStorage(int nSize, bool bShared) :
    _bShared(bShared), _dataSize(0), _data(NULL)
{
    reserve(nSize);
}

svlFactorStorage::~svlFactorStorage()
{
    if (_data != NULL)
        delete[] _data;
#ifdef SVL_FACTOR_DEBUG_STATISTICS
    _dbStatsCurrentMem -= _dataSize;
#endif
}

// reserve capacity
void svlFactorStorage::reserve(int nSize)
{
    if (_dataSize < nSize) {
#ifdef SVL_FACTOR_DEBUG_STATISTICS
        _dbStatsCurrentMem += (nSize - _dataSize);
        if (_dbStatsCurrentMem > _dbStatsMaxMem)
            _dbStatsMaxMem = _dbStatsCurrentMem;
        if (nSize > (int)_dbStatsMaxTable)
            _dbStatsMaxTable = nSize;
        _dbStatsTotalMem += (nSize - _dataSize);
        // check for overflow
        if ((int)_dbStatsTotalMem - (nSize - _dataSize) < 0) {
            _dbStatsTotalMem = (unsigned)-1;
        }
#endif
        double *newData = new double[nSize];
        SVL_ASSERT(newData != NULL);
        if (_data != NULL) {
            memcpy(newData, _data, _dataSize * sizeof(double));
            delete _data;
        }
        _data = newData;
        _dataSize = nSize;
    }
}

// copying/setting memory
void svlFactorStorage::zero(int nSize)
{
    if (nSize < 0) nSize = _dataSize;
    reserve(nSize);
    memset((void *)_data, (int)0.0, nSize * sizeof(double));
}

void svlFactorStorage::fill(double v, int nSize)
{
    if (nSize < 0) nSize = _dataSize;
    reserve(nSize);
#if 0
    for (int i = 0; i < nSize; i++) {
        _data[i] = v;
    }
#else
    double *p = &_data[0];
    for (int i = nSize / 4; i != 0; i--) {
        p[0] = p[1] = p[2] = p[3] = v;
        p += 4;
    }
    for (int i = 0; i < nSize % 4; i++) {
        (*p++) = v;
    }
#endif
}

void svlFactorStorage::copy(const double *p, int nSize)
{
    SVL_ASSERT(p != NULL);
    if (nSize < 0) nSize = _dataSize;
    reserve(nSize);
    memcpy(_data, p, nSize * sizeof(double));
}

void svlFactorStorage::copy(const svlFactorStorage *p, int nSize)
{
    SVL_ASSERT((p != NULL) && (p->_dataSize >= nSize));
    if (nSize < 0) nSize = _dataSize;
    reserve(nSize);
    memcpy(_data, p->_data, nSize * sizeof(double));
}
