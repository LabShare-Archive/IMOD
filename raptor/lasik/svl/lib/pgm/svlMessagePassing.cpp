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
** FILENAME:    svlMessagePassing.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**
** DESCRIPTION:
**  Algorithms that operate in log-space expect their factor to be provided
**  in log-space. These are: LOGMAXPROD, ASYNCLOGMAXPROD, ASYNCLOGMAXPRODDIV,
**  GEMPLP and SONTAG08. All other algorithms expect input in normal-space
**  (i.e., already exponentiated).
*****************************************************************************/

#include <stdlib.h>
#include <cassert>
#include <iostream>
#include <fstream>
#include <vector>
#include <cmath>
#include <deque>
#include <queue>
#include <algorithm>
#include <limits>
#include <iterator>

#include "svlBase.h"
#include "svlFactor.h"
#include "svlClusterGraph.h"
#include "svlMessagePassing.h"

using namespace std;

// svlMessagePassingAlgorithm utility functions -----------------------------

string toString(svlMessagePassingAlgorithms& mpa)
{
    string infAlgoStr = string("NONE");
    switch (mpa) {
    case SVL_MP_SUMPROD:
        infAlgoStr = string("SUMPROD"); break;
    case SVL_MP_MAXPROD:
        infAlgoStr = string("MAXPROD"); break;
    case SVL_MP_LOGMAXPROD:
        infAlgoStr = string("LOGMAXPROD"); break;
    case SVL_MP_SUMPRODDIV:
        infAlgoStr = string("SUMPRODDIV"); break;
    case SVL_MP_MAXPRODDIV:
        infAlgoStr = string("MAXSUMPRODDIV"); break;
    case SVL_MP_LOGMAXPRODDIV:
        infAlgoStr = string("LOGMAXPRODDIV"); break;
    case SVL_MP_ASYNCSUMPROD:
        infAlgoStr = string("ASYNCSUMPROD"); break;
    case SVL_MP_ASYNCMAXPROD:
        infAlgoStr = string("ASYNCMAXPROD"); break;
    case SVL_MP_ASYNCLOGMAXPROD:
        infAlgoStr = string("ASYNCLOGMAXPROD"); break;
    case SVL_MP_ASYNCLOGMAXPROD_LAZY:
        infAlgoStr = string("ASYNCLOGMAXPRODLAZY"); break;
    case SVL_MP_ASYNCSUMPRODDIV:
        infAlgoStr = string("ASYNCSUMPRODDIV"); break;
    case SVL_MP_ASYNCMAXPRODDIV:
        infAlgoStr = string("ASYNCMAXPRODDIV"); break;
    case SVL_MP_ASYNCLOGMAXPRODDIV:
        infAlgoStr = string("ASYNCLOGMAXPRODDIV"); break;
    case SVL_MP_RBP_SUMPROD:
        infAlgoStr = string("RBPSUMPROD"); break;
    case SVL_MP_RBP_MAXPROD:
        infAlgoStr = string("RBPMAXPROD"); break;
    case SVL_MP_RBP_LOGMAXPROD:
        infAlgoStr = string("RBPLOGMAXPROD"); break;
    case SVL_MP_GEMPLP:
        infAlgoStr = string("GEMPLP"); break;
    case SVL_MP_SONTAG08:
        infAlgoStr = string("SONTAG08"); break;
    default:
        SVL_ASSERT(false);
    }

    return infAlgoStr;
}

svlMessagePassingAlgorithms decodeMessagePassingAlgorithm(const char *s)
{
    svlMessagePassingAlgorithms infAlgorithm = SVL_MP_NONE;

    if (!strcasecmp(s, "SUMPROD")) {
        infAlgorithm = SVL_MP_SUMPROD;
    } else if (!strcasecmp(s, "MAXPROD")) {
        infAlgorithm = SVL_MP_MAXPROD;
    } else if (!strcasecmp(s, "LOGMAXPROD")) {
        infAlgorithm = SVL_MP_LOGMAXPROD;
    } else if (!strcasecmp(s, "SUMPRODDIV")) {
        infAlgorithm = SVL_MP_SUMPRODDIV;
    } else if (!strcasecmp(s, "MAXPRODDIV")) {
        infAlgorithm = SVL_MP_MAXPRODDIV;
    } else if (!strcasecmp(s, "LOGMAXPRODDIV")) {
        infAlgorithm = SVL_MP_LOGMAXPRODDIV;
    } else if (!strcasecmp(s, "ASYNCSUMPROD")) {
        infAlgorithm = SVL_MP_ASYNCSUMPROD;
    } else if (!strcasecmp(s, "ASYNCMAXPROD")) {
        infAlgorithm = SVL_MP_ASYNCMAXPROD;
    } else if (!strcasecmp(s, "ASYNCLOGMAXPROD")) {
        infAlgorithm = SVL_MP_ASYNCLOGMAXPROD;
    } else if (!strcasecmp(s, "ASYNCLOGMAXPRODLAZY")) {
        infAlgorithm = SVL_MP_ASYNCLOGMAXPROD_LAZY;
    } else if (!strcasecmp(s, "ASYNCSUMPRODDIV")) {
        infAlgorithm = SVL_MP_ASYNCSUMPRODDIV;
    } else if (!strcasecmp(s, "ASYNCMAXPRODDIV")) {
        infAlgorithm = SVL_MP_ASYNCMAXPRODDIV;
    } else if (!strcasecmp(s, "ASYNCLOGMAXPRODDIV")) {
        infAlgorithm = SVL_MP_ASYNCLOGMAXPRODDIV;
    } else if (!strcasecmp(s, "RBPSUMPROD")) {
        infAlgorithm = SVL_MP_RBP_SUMPROD;
    } else if (!strcasecmp(s, "RBPMAXPROD")) {
        infAlgorithm = SVL_MP_RBP_MAXPROD;
    } else if (!strcasecmp(s, "RBPLOGMAXPROD")) {
        infAlgorithm = SVL_MP_RBP_LOGMAXPROD;
    } else if (!strcasecmp(s, "GEMPLP")) {
        infAlgorithm = SVL_MP_GEMPLP;
    } else if (!strcasecmp(s, "SONTAG08")) {
        infAlgorithm = SVL_MP_SONTAG08;
    }

    return infAlgorithm;
}

// svlMessagePassingInference Class -----------------------------------------

bool svlMessagePassingInference::SINGLETON_MARGINALS_ONLY = true;

svlMessagePassingInference::svlMessagePassingInference(svlClusterGraph& graph) :
    _graph(graph), _algorithm(SVL_MP_NONE)
{
#if 0
    // check running intersection property (slow)
    SVL_ASSERT(_graph.checkRunIntProp());
#endif
}

svlMessagePassingInference::~svlMessagePassingInference()
{
    // free memory used by intermediate factors and computations
    reset();
}

void svlMessagePassingInference::reset()
{
    for (unsigned i = 0; i < _computations.size(); i++) {
	delete _computations[i];
    }
    _computations.clear();

    for (unsigned i = 0; i < _intermediateFactors.size(); i++) {
	delete _intermediateFactors[i];
    }
    _intermediateFactors.clear();

    for (unsigned i = 0; i < _forwardMessages.size(); i++) {
        delete _forwardMessages[i];
        delete _backwardMessages[i];
    }
    for (unsigned i = 0; i < _oldForwardMessages.size(); i++) {
        delete _oldForwardMessages[i];
        delete _oldBackwardMessages[i];
    }
    _forwardMessages.clear();
    _backwardMessages.clear();
    _oldForwardMessages.clear();
    _oldBackwardMessages.clear();

    for (unsigned i = 0; i < _cliquePotentials.size(); i++) {
        delete _cliquePotentials[i];
    }
    _cliquePotentials.clear();

    _algorithm = SVL_MP_NONE;

    for (unsigned i = 0; i < _sharedStorage.size(); i++) {
        delete _sharedStorage[i];
    }
    _sharedStorage.clear();

    // convergent message passing data
    _lpCliqueEdges.clear();
    _lpSeparatorEdges.clear();
    _lpSeparators.clear();
    _lpEdges.clear();    

    // lazy sets
    _forwardLazySet.clear();
    _backwardLazySet.clear();
    _fwdMessagesModified.clear();
    _bckMessagesModified.clear();
}

bool svlMessagePassingInference::isLogSpace() const
{
    switch (_algorithm) {
    case SVL_MP_SUMPROD:
    case SVL_MP_MAXPROD:
    case SVL_MP_SUMPRODDIV:
    case SVL_MP_MAXPRODDIV:
    case SVL_MP_ASYNCSUMPROD:
    case SVL_MP_ASYNCMAXPROD:
    case SVL_MP_ASYNCSUMPRODDIV:
    case SVL_MP_ASYNCMAXPRODDIV:
    case SVL_MP_RBP_SUMPROD:
    case SVL_MP_RBP_MAXPROD:
        return false;
    case SVL_MP_LOGMAXPROD:
    case SVL_MP_LOGMAXPRODDIV:
    case SVL_MP_ASYNCLOGMAXPROD:
    case SVL_MP_ASYNCLOGMAXPROD_LAZY:
    case SVL_MP_ASYNCLOGMAXPRODDIV:
    case SVL_MP_RBP_LOGMAXPROD:
    case SVL_MP_GEMPLP:
    case SVL_MP_SONTAG08:
        return true;
    default:
        return false;
    }    
}

bool svlMessagePassingInference::inference(svlMessagePassingAlgorithms mpAlgorithm,
    int maxIterations)
{
    // make sure we're running the same algorithm as the computation graph
    if (mpAlgorithm != _algorithm) {
	reset();
    }
    _algorithm = mpAlgorithm;

    // assign initial clique potentials and messages
    switch (_algorithm) {
    case SVL_MP_SUMPROD:
    case SVL_MP_MAXPROD:
    case SVL_MP_SUMPRODDIV:
    case SVL_MP_MAXPRODDIV:
    case SVL_MP_ASYNCSUMPROD:
    case SVL_MP_ASYNCMAXPROD:
    case SVL_MP_ASYNCSUMPRODDIV:
    case SVL_MP_ASYNCMAXPRODDIV:
    case SVL_MP_RBP_SUMPROD:
    case SVL_MP_RBP_MAXPROD:
        initializeMessagePassing();
        break;
    case SVL_MP_LOGMAXPROD:
    case SVL_MP_LOGMAXPRODDIV:
    case SVL_MP_ASYNCLOGMAXPROD:
    case SVL_MP_ASYNCLOGMAXPRODDIV:
    case SVL_MP_ASYNCLOGMAXPROD_LAZY:        
    case SVL_MP_RBP_LOGMAXPROD:
        initializeLogMessagePassing();
        break;
    case SVL_MP_GEMPLP:
    case SVL_MP_SONTAG08:
        initializeGeneralizedMPLP();
        break;
    default:
        SVL_ASSERT(_algorithm == SVL_MP_NONE);
        break;
    }

    // update old message cache
    if ((_algorithm != SVL_MP_GEMPLP) && (_algorithm != SVL_MP_SONTAG08)) {
        if (_oldForwardMessages.empty()) {
            _oldForwardMessages.reserve(_forwardMessages.size());
            _oldBackwardMessages.reserve(_backwardMessages.size());
            for (unsigned i = 0; i < _forwardMessages.size(); i++) {
                _oldForwardMessages.push_back(new svlFactor());
                _oldBackwardMessages.push_back(new svlFactor());
            }
        }

        for (unsigned i = 0; i < _forwardMessages.size(); i++) {
            *_oldForwardMessages[i] = *_forwardMessages[i];
            *_oldBackwardMessages[i] = *_backwardMessages[i];
        }
    }

    // set up computation graph
    if (_computations.empty()) {
        int handle = svlCodeProfiler::getHandle("svlMessagePassingInference::buildComputationGraph");
        svlCodeProfiler::tic(handle);
	switch (_algorithm) {
	case SVL_MP_SUMPROD:
	    buildSumProdComputationGraph();
	    break;
	case SVL_MP_MAXPROD:
	    buildMaxProdComputationGraph();
	    break;
	case SVL_MP_LOGMAXPROD:
	    buildLogMaxProdComputationGraph();
	    break;
	case SVL_MP_SUMPRODDIV:
	    buildSumProdDivComputationGraph();
	    break;
	case SVL_MP_MAXPRODDIV:
	    buildMaxProdDivComputationGraph();
	    break;
	case SVL_MP_LOGMAXPRODDIV:
	    buildLogMaxProdDivComputationGraph();
	    break;
	case SVL_MP_ASYNCSUMPROD:
	    buildAsyncSumProdComputationGraph();
	    break;
	case SVL_MP_ASYNCMAXPROD:
	    buildAsyncMaxProdComputationGraph();
	    break;
	case SVL_MP_ASYNCLOGMAXPROD:
	    buildAsyncLogMaxProdComputationGraph();
	    break;
        case SVL_MP_ASYNCLOGMAXPROD_LAZY:
            buildAsyncLogMaxProdLazyComputationGraph();
            break;
	case SVL_MP_ASYNCSUMPRODDIV:
	    buildAsyncSumProdDivComputationGraph();
	    break;
	case SVL_MP_ASYNCMAXPRODDIV:
	    buildAsyncMaxProdDivComputationGraph();
	    break;
	case SVL_MP_ASYNCLOGMAXPRODDIV:
	    buildAsyncLogMaxProdDivComputationGraph();
	    break;
	case SVL_MP_RBP_SUMPROD:
	    buildResidualBPComputationGraph();
	    break;
	case SVL_MP_RBP_MAXPROD:
	    buildResidualBPMaxProdComputationGraph();
	    break;
	case SVL_MP_RBP_LOGMAXPROD:
	    buildResidualBPLogMaxProdComputationGraph();
	    break;
        case SVL_MP_GEMPLP:
        case SVL_MP_SONTAG08:
            buildGeneralizedMPLPGraph();
            break;
	default:
	    SVL_ASSERT(_algorithm == SVL_MP_NONE);
	    break;
	}
        svlCodeProfiler::toc(handle);
    }

    // assert that all messages and intermediate factors are non-empty
    for (int i = 0; i < (int)_forwardMessages.size(); i++) {
        SVL_ASSERT(!_forwardMessages[i]->empty());
        SVL_ASSERT(!_backwardMessages[i]->empty());
    }
#if 0
    for (int i = 0; i < (int)_intermediateFactors.size(); i++) {
        //SVL_ASSERT(!_intermediateFactors[i]->empty());
        if (!_intermediateFactors[i]->empty()) {
            SVL_LOG(SVL_LOG_WARNING, "empty intermediate factor");
        }
    }
#endif

    // run inference
    bool bConverged = false;
    switch (_algorithm) {
    case SVL_MP_ASYNCLOGMAXPROD_LAZY:
        bConverged = lazyMessagePassingLoop(maxIterations);
        break;
    case SVL_MP_RBP_SUMPROD:
    case SVL_MP_RBP_MAXPROD:
    case SVL_MP_RBP_LOGMAXPROD:
	bConverged = residualBPMessagePassingLoop(maxIterations);
	break;
    case SVL_MP_GEMPLP:
	bConverged = gemplpMessagePassingLoop(maxIterations);
	break;        
    case SVL_MP_SONTAG08:
	bConverged = sontag08MessagePassingLoop(maxIterations);
	break;        
    default:
	bConverged = messagePassingLoop(maxIterations);
	break;
    }
    
    // compute final clique potentials
    switch (_algorithm) {
    case SVL_MP_SUMPROD:
    case SVL_MP_MAXPROD:
    case SVL_MP_SUMPRODDIV:
    case SVL_MP_MAXPRODDIV:
    case SVL_MP_ASYNCSUMPROD:
    case SVL_MP_ASYNCMAXPROD:
    case SVL_MP_ASYNCSUMPRODDIV:
    case SVL_MP_ASYNCMAXPRODDIV:
    case SVL_MP_RBP_SUMPROD:
    case SVL_MP_RBP_MAXPROD:
        finalizeMessagePassing();
        break;
    case SVL_MP_LOGMAXPROD:
    case SVL_MP_LOGMAXPRODDIV:
    case SVL_MP_ASYNCLOGMAXPROD:
    case SVL_MP_ASYNCLOGMAXPROD_LAZY:
    case SVL_MP_ASYNCLOGMAXPRODDIV:
    case SVL_MP_RBP_LOGMAXPROD:
        finalizeLogMessagePassing();
        break;
    case SVL_MP_GEMPLP:
        finalizeGeneralizedMPLP();
        break;
    case SVL_MP_SONTAG08:
        break;
    default:
        SVL_ASSERT(_algorithm == SVL_MP_NONE);
        break;
    }

    return bConverged;
}

// message passing initialization and finalization ---------------------------

void svlMessagePassingInference::initializeMessagePassing()
{
    // initialize clique potentials
    if (_cliquePotentials.empty()) {
        _cliquePotentials.resize(_graph.numCliques());
        for (int n = 0; n < _graph.numCliques(); n++) {
            _cliquePotentials[n] = new svlFactor(_graph.getCliquePotential(n));
        }
    } else {
        SVL_ASSERT(_cliquePotentials.size() == (unsigned)_graph.numCliques());
        for (int n = 0; n < _graph.numCliques(); n++) {
            *_cliquePotentials[n] = _graph.getCliquePotential(n);
        }
    }

    // set up forward and backward messages
    if (_forwardMessages.empty()) {
	_forwardMessages.reserve(_graph.numEdges());
	_backwardMessages.reserve(_graph.numEdges());
	for (int i = 0; i < _graph.numEdges(); i++) {
	    _forwardMessages.push_back(new svlFactor());
	    const svlClique s = _graph.getSepSet(i);
	    for (svlClique::const_iterator j = s.begin(); j != s.end(); ++j) {
		_forwardMessages.back()->addVariable(*j, _graph.getCardinality(*j));
	    }
	    _forwardMessages.back()->fill(1.0);
            _backwardMessages.push_back(new svlFactor(*_forwardMessages.back()));
	}
    } else {
        // reset messages to all ones
	for (unsigned i = 0; i < _forwardMessages.size(); i++) {
            _forwardMessages[i]->fill(1.0);
            _backwardMessages[i]->fill(1.0);
        }
    }
}

void svlMessagePassingInference::initializeLogMessagePassing()
{
    initializeMessagePassing();

    // reset messages to all ones
    for (unsigned i = 0; i < _forwardMessages.size(); i++) {
        _forwardMessages[i]->fill(0.0);
        _backwardMessages[i]->fill(0.0);
    }
}

void svlMessagePassingInference::initializeGeneralizedMPLP()
{
    // Get mapping from cliques and separators to edges where an edge is
    // between a clique and a spearator set.
    _lpCliqueEdges.resize(_graph.numCliques());
    _lpSeparatorEdges.clear();
    _lpSeparators.clear();
    _lpEdges.clear();
    _lastDualObjective = numeric_limits<double>::max();

#if 0
    // only cluster graph separators
    for (int i = 0; i < _graph.numEdges(); i++) {
        pair<int, int> e = _graph.getEdge(i);
        svlClique s = _graph.getSepSet(i);
        int k = 0;
        for ( ; k < (int)_lpSeparators.size(); k++) {
            if (_lpSeparators[k] == s)
                break;
        }
        if (k == (int)_lpSeparators.size()) {
            _lpSeparators.push_back(s);
            _lpSeparatorEdges.push_back(vector<int>());
        }

        if (find(_lpEdges.begin(), _lpEdges.end(), make_pair(e.first, k)) == _lpEdges.end()) {
            _lpCliqueEdges[e.first].push_back((int)_lpEdges.size());
            _lpSeparatorEdges[k].push_back((int)_lpEdges.size());
            _lpEdges.push_back(make_pair(e.first, k));
        }

        if (find(_lpEdges.begin(), _lpEdges.end(), make_pair(e.second, k)) == _lpEdges.end()) {
            _lpCliqueEdges[e.second].push_back((int)_lpEdges.size());
            _lpSeparatorEdges[k].push_back((int)_lpEdges.size());
            _lpEdges.push_back(make_pair(e.second, k));
        }
    }
#else
    // standard GEMPLP
    for (int i = 0; i < _graph.numCliques(); i++) {
        svlClique c_i = _graph.getClique(i);
        for (int j = i + 1; j < _graph.numCliques(); j++) {
            svlClique c_j = _graph.getClique(j);
            svlClique s_ij;
            set_intersection(c_i.begin(), c_i.end(),
                c_j.begin(), c_j.end(), 
                insert_iterator<svlClique>(s_ij, s_ij.begin()));

            if (s_ij.empty())
                continue;
            
            int k = 0;
            for ( ; k < (int)_lpSeparators.size(); k++) {
                if (_lpSeparators[k] == s_ij)
                    break;
            }

            if (k == (int)_lpSeparators.size()) {
                _lpSeparators.push_back(s_ij);
                _lpSeparatorEdges.push_back(vector<int>());
            }

            if (find(_lpEdges.begin(), _lpEdges.end(), make_pair(i, k)) == _lpEdges.end()) {
                _lpCliqueEdges[i].push_back(_lpEdges.size());
                _lpSeparatorEdges[k].push_back(_lpEdges.size());
                _lpEdges.push_back(make_pair(i, k));
            }

            if (find(_lpEdges.begin(), _lpEdges.end(), make_pair(j, k)) == _lpEdges.end()) {
                _lpCliqueEdges[j].push_back(_lpEdges.size());
                _lpSeparatorEdges[k].push_back(_lpEdges.size());
                _lpEdges.push_back(make_pair(j, k));
            }
        }
    }
#endif

    // assign initial clique potentials
    _cliquePotentials.resize(_graph.numCliques());
    for (int n = 0; n < _graph.numCliques(); n++) {
        _cliquePotentials[n] = new svlFactor(_graph.getCliquePotential(n));
        for (int i = 0; i < _cliquePotentials[n]->size(); i++) {
            (*_cliquePotentials[n])[i] = (*_cliquePotentials[n])[i];
        }
    }

    // set up forward and backward messages
    _forwardMessages.reserve(_lpEdges.size());
    _backwardMessages.reserve(_forwardMessages.size());
    for (unsigned i = 0; i < _lpEdges.size(); i++) {
        _forwardMessages.push_back(new svlFactor());
        const svlClique s = _lpSeparators[_lpEdges[i].second];
        for (svlClique::const_iterator j = s.begin(); j != s.end(); ++j) {
            _forwardMessages.back()->addVariable(*j, _graph.getCardinality(*j));
        }
        if (_cliquePotentials[_lpEdges[i].first]->empty()) {
            _forwardMessages.back()->fill(0.0);
        } else {
            svlFactorMaximizeOp msgInitOp(_forwardMessages.back(), 
                _cliquePotentials[_lpEdges[i].first]);
            msgInitOp.execute();
            _forwardMessages.back()->scale(1.0 / 
                (double)_lpCliqueEdges[_lpEdges[i].first].size());
        }
    }

    for (unsigned i = 0; i < _lpEdges.size(); i++) {
        int cliqueId = _lpEdges[i].first;
        int separatorId = _lpEdges[i].second;
        _backwardMessages.push_back(new svlFactor());
        _backwardMessages.back()->fill(0.0);
        for (unsigned c = 0; c < _lpSeparatorEdges[separatorId].size(); c++) {
            if (_lpEdges[_lpSeparatorEdges[separatorId][c]].first == cliqueId)
                continue;
            //_backwardMessages.back()->add(*_forwardMessages[_lpSeparatorEdges[separatorId][c]]);
            svlFactorAdditionOp addOp(_backwardMessages.back(), _backwardMessages.back(),
                _forwardMessages[_lpSeparatorEdges[separatorId][c]]);
            addOp.execute();
        }
    }
}

void svlMessagePassingInference::finalizeMessagePassing()
{
    // compute final beliefs
    for (int m = 0; m < _graph.numEdges(); m++) {
	pair<int, int> e = _graph.getEdge(m);
        if (SINGLETON_MARGINALS_ONLY) {
            // don't compute beliefs on non-singleton cliques
            if (_graph.getClique(e.first).size() == 1)
                _cliquePotentials[e.first]->product(*_backwardMessages[m]);
            if (_graph.getClique(e.second).size() == 1)
                _cliquePotentials[e.second]->product(*_forwardMessages[m]);
        } else {
            // don't compute belief's on non-singleton empty or shared factors       
            if ((!_cliquePotentials[e.first]->empty() && 
                !_cliquePotentials[e.first]->isShared()) || 
                (_graph.getClique(e.first).size() == 1))
            _cliquePotentials[e.first]->product(*_backwardMessages[m]);
            if ((!_cliquePotentials[e.second]->empty() &&
                    !_cliquePotentials[e.second]->isShared()) ||
                (_graph.getClique(e.second).size() == 1))
                _cliquePotentials[e.second]->product(*_forwardMessages[m]);
        }
    }

    // normalize
    for (int n = 0; n < _graph.numCliques(); n++) {
        if (_cliquePotentials[n]->empty() || _cliquePotentials[n]->isShared())
            continue;
        if (SINGLETON_MARGINALS_ONLY && (_cliquePotentials[n]->numVars() != 1))
            continue;
        _cliquePotentials[n]->normalize();
    }
}

void svlMessagePassingInference::finalizeLogMessagePassing()
{
    // compute final beliefs
    for (int m = 0; m < _graph.numEdges(); m++) {
	pair<int, int> e = _graph.getEdge(m);
        if (SINGLETON_MARGINALS_ONLY) {
            // don't compute beliefs on non-singleton cliques
            if (_graph.getClique(e.first).size() == 1)
                _cliquePotentials[e.first]->add(*_backwardMessages[m]);
            if (_graph.getClique(e.second).size() == 1)
                _cliquePotentials[e.second]->add(*_forwardMessages[m]);
        } else {
            // don't compute belief's on non-singleton empty or shared factors
            if ((!_cliquePotentials[e.first]->empty() && 
                    !_cliquePotentials[e.first]->isShared()) || 
                (_graph.getClique(e.first).size() == 1))
                _cliquePotentials[e.first]->add(*_backwardMessages[m]);
            if ((!_cliquePotentials[e.second]->empty() &&
                    !_cliquePotentials[e.second]->isShared()) ||
                (_graph.getClique(e.second).size() == 1))
                _cliquePotentials[e.second]->add(*_forwardMessages[m]);
        }
    }
}

void svlMessagePassingInference::finalizeGeneralizedMPLP()
{
    // compute final beliefs
    for (unsigned m = 0; m < _lpEdges.size(); m++) {
        int c = _lpEdges[m].first;
        // don't compute belief's on empty factors
        if (_cliquePotentials[c]->empty() || _cliquePotentials[c]->isShared())
            continue;
        //_cliquePotentials[c]->add(*_backwardMessages[m]);
        svlFactorAdditionOp addOp(_cliquePotentials[c],
            _cliquePotentials[c], _backwardMessages[m]);
        addOp.execute();
    }
}


// computation graphs --------------------------------------------------------

void svlMessagePassingInference::buildSumProdComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());  
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_oldBackwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_oldForwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_oldBackwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_oldForwardMessages[*k]);
        }
	
	// forwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingFwdMsgs));
	_computations.push_back(new svlFactorMarginalizeOp(_forwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_forwardMessages[m]));

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingBckMsgs));
	_computations.push_back(new svlFactorMarginalizeOp(_backwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_backwardMessages[m]));
    }
}

void svlMessagePassingInference::buildMaxProdComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_oldBackwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_oldForwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_oldBackwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_oldForwardMessages[*k]);
        }
	
	// forwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingFwdMsgs));
	_computations.push_back(new svlFactorMaximizeOp(_forwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_forwardMessages[m]));

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingBckMsgs));
	_computations.push_back(new svlFactorMaximizeOp(_backwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_backwardMessages[m]));
    }
}

void svlMessagePassingInference::buildLogMaxProdComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_oldBackwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_oldForwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_oldBackwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_oldForwardMessages[*k]);
        }
	
	// forwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorAdditionOp(_intermediateFactors.back(),
				    incomingFwdMsgs));
	_computations.push_back(new svlFactorMaximizeOp(_forwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorLogNormalizeOp(_forwardMessages[m]));

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorAdditionOp(_intermediateFactors.back(),
				    incomingBckMsgs));
	_computations.push_back(new svlFactorMaximizeOp(_backwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorLogNormalizeOp(_backwardMessages[m]));
    }
}

void svlMessagePassingInference::buildSumProdDivComputationGraph()
{
    _intermediateFactors.reserve(_graph.numCliques() + 2 * _graph.numEdges());

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // incoming message products
    for (int n = 0; n < _graph.numCliques(); n++) {
	vector<const svlFactor *> incomingMessages;
	incomingMessages.push_back(_cliquePotentials[n]);
        for (vector<int>::const_iterator k = fwdIncidentEdges[n].begin();
             k != fwdIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[n].begin();
             k != bckIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_forwardMessages[*k]);
        }
	_intermediateFactors.push_back(new svlFactor());
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingMessages));
    }

    // all following intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // outgoing marginalized messages
    for (int m = 0; m < _graph.numEdges(); m++) {
	// forwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorDivideOp(_intermediateFactors.back(),
				    _intermediateFactors[_graph.getEdge(m).first], 
				    _oldBackwardMessages[m]));
	_computations.push_back(new svlFactorMarginalizeOp(_forwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_forwardMessages[m]));

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorDivideOp(_intermediateFactors.back(),
				    _intermediateFactors[_graph.getEdge(m).second], 
				    _oldForwardMessages[m]));
	_computations.push_back(new svlFactorMarginalizeOp(_backwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_backwardMessages[m]));
    }
}

void svlMessagePassingInference::buildMaxProdDivComputationGraph()
{
    _intermediateFactors.reserve(_graph.numCliques() + 2 * _graph.numEdges());

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // incoming message products
    for (int n = 0; n < _graph.numCliques(); n++) {
	vector<const svlFactor *> incomingMessages;
	incomingMessages.push_back(_cliquePotentials[n]);
        for (vector<int>::const_iterator k = fwdIncidentEdges[n].begin();
             k != fwdIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[n].begin();
             k != bckIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_forwardMessages[*k]);
        }
	_intermediateFactors.push_back(new svlFactor());
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingMessages));
    }

    // all following intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // outgoing marginalized messages
    for (int m = 0; m < _graph.numEdges(); m++) {
	// forwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorDivideOp(_intermediateFactors.back(),
				    _intermediateFactors[_graph.getEdge(m).first], 
				    _oldBackwardMessages[m]));
	_computations.push_back(new svlFactorMaximizeOp(_forwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_forwardMessages[m]));

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorDivideOp(_intermediateFactors.back(),
				    _intermediateFactors[_graph.getEdge(m).second], 
				    _oldForwardMessages[m]));
	_computations.push_back(new svlFactorMaximizeOp(_backwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_backwardMessages[m]));
    }
}

void svlMessagePassingInference::buildLogMaxProdDivComputationGraph()
{
    _intermediateFactors.reserve(_graph.numCliques() + 2 * _graph.numEdges());

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // incoming message products
    for (int n = 0; n < _graph.numCliques(); n++) {
	vector<const svlFactor *> incomingMessages;
	incomingMessages.push_back(_cliquePotentials[n]);
        for (vector<int>::const_iterator k = fwdIncidentEdges[n].begin();
             k != fwdIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[n].begin();
             k != bckIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_forwardMessages[*k]);
        }
	_intermediateFactors.push_back(new svlFactor());
	_computations.push_back(new svlFactorAdditionOp(_intermediateFactors.back(),
				    incomingMessages));
    }

    // all following intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // outgoing marginalized messages
    for (int m = 0; m < _graph.numEdges(); m++) {
	// forwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorSubtractOp(_intermediateFactors.back(),
				    _intermediateFactors[_graph.getEdge(m).first], 
				    _oldBackwardMessages[m]));
	_computations.push_back(new svlFactorMaximizeOp(_forwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorLogNormalizeOp(_forwardMessages[m]));

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorSubtractOp(_intermediateFactors.back(),
				    _intermediateFactors[_graph.getEdge(m).second], 
				    _oldForwardMessages[m]));
	_computations.push_back(new svlFactorMaximizeOp(_backwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorLogNormalizeOp(_backwardMessages[m]));
    }
}

void svlMessagePassingInference::buildAsyncSumProdComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());

    // all intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_forwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_forwardMessages[*k]);
        }
	
	// forwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingFwdMsgs));
	_computations.push_back(new svlFactorMarginalizeOp(_forwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_forwardMessages[m]));

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingBckMsgs));
	_computations.push_back(new svlFactorMarginalizeOp(_backwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_backwardMessages[m]));
    }
}

void svlMessagePassingInference::buildAsyncMaxProdComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());

    // all intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_forwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_forwardMessages[*k]);
        }
	
	// forwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingFwdMsgs));
	_computations.push_back(new svlFactorMaximizeOp(_forwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_forwardMessages[m]));

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorProductOp(_intermediateFactors.back(),
				    incomingBckMsgs));
	_computations.push_back(new svlFactorMaximizeOp(_backwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorNormalizeOp(_backwardMessages[m]));
    }
}

void svlMessagePassingInference::buildAsyncLogMaxProdComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());

    // all intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_forwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_forwardMessages[*k]);
        }
	
	// forwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorAdditionOp(_intermediateFactors.back(),
				    incomingFwdMsgs));
	_computations.push_back(new svlFactorMaximizeOp(_forwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorLogNormalizeOp(_forwardMessages[m]));

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	_computations.push_back(new svlFactorAdditionOp(_intermediateFactors.back(),
				    incomingBckMsgs));
	_computations.push_back(new svlFactorMaximizeOp(_backwardMessages[m],
				    _intermediateFactors.back()));
	_computations.push_back(new svlFactorLogNormalizeOp(_backwardMessages[m]));
    }
}

void svlMessagePassingInference::buildAsyncLogMaxProdLazyComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());

    // all intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // reserve space in lazy sets
    _forwardLazySet.resize(_graph.numEdges());
    _backwardLazySet.resize(_graph.numEdges());
    _computations.reserve(_graph.numEdges());
    _fwdMessagesModified.reserve(_graph.numEdges());
    _bckMessagesModified.reserve(_graph.numEdges());

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k == m) continue;
            incomingFwdMsgs.push_back(_backwardMessages[*k]);
            _backwardLazySet[*k].insert(_computations.size());
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k == m) continue;
            incomingFwdMsgs.push_back(_forwardMessages[*k]);
            _forwardLazySet[*k].insert(_computations.size());
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k == m) continue;
            incomingBckMsgs.push_back(_backwardMessages[*k]);
            _backwardLazySet[*k].insert(_computations.size() + 1);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k == m) continue;
            incomingBckMsgs.push_back(_forwardMessages[*k]);
            _forwardLazySet[*k].insert(_computations.size() + 1);
        }
	
	// forwards
	vector<svlFactorOperation *> atom;
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	atom.push_back(new svlFactorAdditionOp(_intermediateFactors.back(),
                           incomingFwdMsgs));
	atom.push_back(new svlFactorMaximizeOp(_forwardMessages[m],
                           _intermediateFactors.back()));
	atom.push_back(new svlFactorLogNormalizeOp(_forwardMessages[m]));
	_computations.push_back(new svlFactorAtomicOp(atom));
	atom.clear();

        // message modified by this operation
        _fwdMessagesModified.push_back(m);
        _bckMessagesModified.push_back(-1);

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	atom.push_back(new svlFactorAdditionOp(_intermediateFactors.back(),
                           incomingBckMsgs));
	atom.push_back(new svlFactorMaximizeOp(_backwardMessages[m],
                           _intermediateFactors.back()));
	atom.push_back(new svlFactorLogNormalizeOp(_backwardMessages[m]));
	_computations.push_back(new svlFactorAtomicOp(atom));
	atom.clear();

        // message modified by this operation
        _fwdMessagesModified.push_back(-1);
        _bckMessagesModified.push_back(m);
    }
}

void svlMessagePassingInference::buildAsyncSumProdDivComputationGraph()
{
    _intermediateFactors.reserve(_graph.numCliques() + 2 * _graph.numEdges());

    // node factors and intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // since we're implementing sum-product-divide, we need to
    // send all messages orginating from the same node at once
    for (int n = 0; n < _graph.numCliques(); n++) {
	vector<const svlFactor *> incomingMessages;
	incomingMessages.push_back(_cliquePotentials[n]);
        for (vector<int>::const_iterator k = fwdIncidentEdges[n].begin();
             k != fwdIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[n].begin();
             k != bckIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_forwardMessages[*k]);
        }

	// intermediate node factor
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	svlFactor *nodeFactor = _intermediateFactors.back();
	_computations.push_back(new svlFactorProductOp(nodeFactor,
				    incomingMessages));
	// outgoing forward messages
        for (vector<int>::const_iterator k = fwdIncidentEdges[n].begin();
             k != fwdIncidentEdges[n].end(); ++k) {
            _intermediateFactors.push_back(new svlFactor(_sharedStorage[1]));
            _computations.push_back(new svlFactorDivideOp(_intermediateFactors.back(),
                                        nodeFactor,
                                        _backwardMessages[*k]));
            _computations.push_back(new svlFactorMarginalizeOp(_forwardMessages[*k],
                    _intermediateFactors.back()));
            _computations.push_back(new svlFactorNormalizeOp(_forwardMessages[*k]));
        }

	// outgoing backward messages
        for (vector<int>::const_iterator k = bckIncidentEdges[n].begin();
             k != bckIncidentEdges[n].end(); ++k) {
            _intermediateFactors.push_back(new svlFactor(_sharedStorage[1]));
            _computations.push_back(new svlFactorDivideOp(_intermediateFactors.back(),
                                        nodeFactor,
                                        _forwardMessages[*k]));
            _computations.push_back(new svlFactorMarginalizeOp(_backwardMessages[*k],
                    _intermediateFactors.back()));
            _computations.push_back(new svlFactorNormalizeOp(_backwardMessages[*k]));
        }
    }
}

void svlMessagePassingInference::buildAsyncMaxProdDivComputationGraph()
{
    _intermediateFactors.reserve(_graph.numCliques() + 2 * _graph.numEdges());

    // node factors and intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // since we're implementing max-product-divide, we need to
    // send all messages orginating from the same node at once
    for (int n = 0; n < _graph.numCliques(); n++) {
	vector<const svlFactor *> incomingMessages;
	incomingMessages.push_back(_cliquePotentials[n]);
        for (vector<int>::const_iterator k = fwdIncidentEdges[n].begin();
             k != fwdIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[n].begin();
             k != bckIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_forwardMessages[*k]);
        }

	// intermediate node factor
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	svlFactor *nodeFactor = _intermediateFactors.back();
	_computations.push_back(new svlFactorProductOp(nodeFactor,
				    incomingMessages));
	// outgoing forward messages
        for (vector<int>::const_iterator k = fwdIncidentEdges[n].begin();
             k != fwdIncidentEdges[n].end(); ++k) {
            _intermediateFactors.push_back(new svlFactor(_sharedStorage[1]));
            _computations.push_back(new svlFactorDivideOp(_intermediateFactors.back(),
                                        nodeFactor,
                                        _backwardMessages[*k]));
            _computations.push_back(new svlFactorMaximizeOp(_forwardMessages[*k],
                    _intermediateFactors.back()));
            _computations.push_back(new svlFactorNormalizeOp(_forwardMessages[*k]));
        }

	// outgoing backward messages
        for (vector<int>::const_iterator k = bckIncidentEdges[n].begin();
             k != bckIncidentEdges[n].end(); ++k) {
            _intermediateFactors.push_back(new svlFactor(_sharedStorage[1]));
            _computations.push_back(new svlFactorDivideOp(_intermediateFactors.back(),
                                        nodeFactor,
                                        _forwardMessages[*k]));
            _computations.push_back(new svlFactorMaximizeOp(_backwardMessages[*k],
                    _intermediateFactors.back()));
            _computations.push_back(new svlFactorNormalizeOp(_backwardMessages[*k]));
        }
    }
}

void svlMessagePassingInference::buildAsyncLogMaxProdDivComputationGraph()
{
    _intermediateFactors.reserve(_graph.numCliques() + 2 * _graph.numEdges());

    // node factors and intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // since we're implementing log-max-product-divide, we need to
    // send all messages orginating from the same node at once
    for (int n = 0; n < _graph.numCliques(); n++) {
	vector<const svlFactor *> incomingMessages;
	incomingMessages.push_back(_cliquePotentials[n]);
        for (vector<int>::const_iterator k = fwdIncidentEdges[n].begin();
             k != fwdIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[n].begin();
             k != bckIncidentEdges[n].end(); ++k) {
            incomingMessages.push_back(_forwardMessages[*k]);
        }

	// intermediate node factor
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	svlFactor *nodeFactor = _intermediateFactors.back();
	_computations.push_back(new svlFactorAdditionOp(nodeFactor,
				    incomingMessages));
	// outgoing forward messages
        for (vector<int>::const_iterator k = fwdIncidentEdges[n].begin();
             k != fwdIncidentEdges[n].end(); ++k) {
            _intermediateFactors.push_back(new svlFactor(_sharedStorage[1]));
            _computations.push_back(new svlFactorSubtractOp(_intermediateFactors.back(),
                                        nodeFactor,
                                        _backwardMessages[*k]));
            _computations.push_back(new svlFactorMaximizeOp(_forwardMessages[*k],
                    _intermediateFactors.back()));
            _computations.push_back(new svlFactorLogNormalizeOp(_forwardMessages[*k]));
        }

	// outgoing backward messages
        for (vector<int>::const_iterator k = bckIncidentEdges[n].begin();
             k != bckIncidentEdges[n].end(); ++k) {
            _intermediateFactors.push_back(new svlFactor(_sharedStorage[1]));
            _computations.push_back(new svlFactorSubtractOp(_intermediateFactors.back(),
                                        nodeFactor,
                                        _forwardMessages[*k]));
            _computations.push_back(new svlFactorMaximizeOp(_backwardMessages[*k],
                    _intermediateFactors.back()));
            _computations.push_back(new svlFactorLogNormalizeOp(_backwardMessages[*k]));
        }
    }
}

void svlMessagePassingInference::buildResidualBPComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());

    // all intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_forwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_forwardMessages[*k]);
        }
	
	// forwards
	vector<svlFactorOperation *> atom;
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	atom.push_back(new svlFactorProductOp(_intermediateFactors.back(),
			   incomingFwdMsgs));
	atom.push_back(new svlFactorMarginalizeOp(_forwardMessages[m],
			   _intermediateFactors.back()));
	atom.push_back(new svlFactorNormalizeOp(_forwardMessages[m]));

	_computations.push_back(new svlFactorAtomicOp(atom));
	atom.clear();

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	atom.push_back(new svlFactorProductOp(_intermediateFactors.back(),
			   incomingBckMsgs));
	atom.push_back(new svlFactorMarginalizeOp(_backwardMessages[m],
			   _intermediateFactors.back()));
	atom.push_back(new svlFactorNormalizeOp(_backwardMessages[m]));
	
	_computations.push_back(new svlFactorAtomicOp(atom));
	atom.clear();
    }
}

void svlMessagePassingInference::buildResidualBPMaxProdComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());

    // all intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_forwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_forwardMessages[*k]);
        }
	
	// forwards
	vector<svlFactorOperation *> atom;
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	atom.push_back(new svlFactorProductOp(_intermediateFactors.back(),
			   incomingFwdMsgs));
	atom.push_back(new svlFactorMaximizeOp(_forwardMessages[m],
			   _intermediateFactors.back()));
	atom.push_back(new svlFactorNormalizeOp(_forwardMessages[m]));

	_computations.push_back(new svlFactorAtomicOp(atom));
	atom.clear();

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	atom.push_back(new svlFactorProductOp(_intermediateFactors.back(),
			   incomingBckMsgs));
	atom.push_back(new svlFactorMaximizeOp(_backwardMessages[m],
			   _intermediateFactors.back()));
	atom.push_back(new svlFactorNormalizeOp(_backwardMessages[m]));
	
	_computations.push_back(new svlFactorAtomicOp(atom));
	atom.clear();
    }
}

void svlMessagePassingInference::buildResidualBPLogMaxProdComputationGraph()
{
    _intermediateFactors.reserve(2 * _graph.numEdges());

    // all intermediate factors share storage
    _sharedStorage.push_back(new svlFactorStorage(0, true));

    // compute reverse edge-to-node indices
    vector<vector<int> > fwdIncidentEdges(_graph.numCliques());
    vector<vector<int> > bckIncidentEdges(_graph.numCliques());
    for (int m = 0; m < _graph.numEdges(); m++) {
        fwdIncidentEdges[_graph.getEdge(m).first].push_back(m);
        bckIncidentEdges[_graph.getEdge(m).second].push_back(m);
    }

    // build computation graph
    for (int m = 0; m < _graph.numEdges(); m++) {
	int fwdIndx = _graph.getEdge(m).first;
	int bckIndx = _graph.getEdge(m).second;
	vector<const svlFactor *> incomingFwdMsgs;
	vector<const svlFactor *> incomingBckMsgs;

	incomingFwdMsgs.push_back(_cliquePotentials[fwdIndx]);
	incomingBckMsgs.push_back(_cliquePotentials[bckIndx]);

        for (vector<int>::const_iterator k = fwdIncidentEdges[fwdIndx].begin(); 
             k != fwdIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[fwdIndx].begin(); 
             k != bckIncidentEdges[fwdIndx].end(); ++k) {
            if (*k != m) incomingFwdMsgs.push_back(_forwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = fwdIncidentEdges[bckIndx].begin(); 
             k != fwdIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_backwardMessages[*k]);
        }
        for (vector<int>::const_iterator k = bckIncidentEdges[bckIndx].begin(); 
             k != bckIncidentEdges[bckIndx].end(); ++k) {
            if (*k != m) incomingBckMsgs.push_back(_forwardMessages[*k]);
        }
	
	// forwards
	vector<svlFactorOperation *> atom;
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	atom.push_back(new svlFactorAdditionOp(_intermediateFactors.back(),
			   incomingFwdMsgs));
	atom.push_back(new svlFactorMaximizeOp(_forwardMessages[m],
			   _intermediateFactors.back()));
	atom.push_back(new svlFactorLogNormalizeOp(_forwardMessages[m]));

	_computations.push_back(new svlFactorAtomicOp(atom));
	atom.clear();

	// backwards
	_intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
	atom.push_back(new svlFactorAdditionOp(_intermediateFactors.back(),
			   incomingBckMsgs));
	atom.push_back(new svlFactorMaximizeOp(_backwardMessages[m],
			   _intermediateFactors.back()));
	atom.push_back(new svlFactorLogNormalizeOp(_backwardMessages[m]));
	
	_computations.push_back(new svlFactorAtomicOp(atom));
	atom.clear();
    }
}

void svlMessagePassingInference::buildGeneralizedMPLPGraph()
{
    // convergent message passing algorithm
    // Globerson and Jaakkola, NIPS 2007

    // intermediate factors for \lambda_{s \to c} and \lambda_{c \to s}
    _intermediateFactors.reserve(2 * _lpEdges.size());
    _sharedStorage.push_back(new svlFactorStorage(0, true));
    _sharedStorage.push_back(new svlFactorStorage(0, true));
        
    // since we're implementing coordinate ascent, we need to
    // send all messages orginating from the same node at once
    for (int c = 0; c < _graph.numCliques(); c++) {

        // subtract old forward messages from all \lambda_{s \to \hat{c}}
        for (int s = 0; s < (int)_lpCliqueEdges[c].size(); s++) {
            int messageId = _lpCliqueEdges[c][s];
            int separatorId = _lpEdges[messageId].second;
            for (int cHat = 0; cHat < (int)_lpSeparatorEdges[separatorId].size(); cHat++) {
                int backwardMessageId = _lpSeparatorEdges[separatorId][cHat];
                int cHatCliqueId = _lpEdges[backwardMessageId].first;
                if (cHatCliqueId == c)
                    continue;

                SVL_ASSERT((messageId < (int)_forwardMessages.size()) &&
                    (backwardMessageId < (int)_backwardMessages.size()));
                
                _computations.push_back(new svlFactorSubtractOp(_backwardMessages[backwardMessageId],
                                            _backwardMessages[backwardMessageId],
                                            _forwardMessages[messageId]));
            }
        } 

        // send all \lambda_{c \to s} messages
        for (int s = 0; s < (int)_lpCliqueEdges[c].size(); s++) {
            int messageId = _lpCliqueEdges[c][s];
            int separatorId = _lpEdges[messageId].second;
            SVL_ASSERT(_lpEdges[messageId].first == c);

#if 0
            SVL_LOG(SVL_LOG_MESSAGE, "*** generating messages from c_" << c << " " <<
                toString(_graph.getClique(c)) << " to s_" << separatorId << " " << 
                toString(_lpSeparators[separatorId]));
#endif

            vector<const svlFactor *> incomingMessages;
            incomingMessages.push_back(_cliquePotentials[c]);
            for (int sHat = 0; sHat < (int)_lpCliqueEdges[c].size(); sHat++) {
                if (sHat == s) continue;                
#if 1
                incomingMessages.push_back(_backwardMessages[_lpCliqueEdges[c][sHat]]);
#else
                pair<int, int> csPair = edges[_lpCliqueEdges[c][sHat]]; 
                for (int cHat = 0; cHat < (int)_lpSeparatorEdges[_lpEdges[_lpCliqueEdges[c][sHat]].second].size(); cHat++) {
                    pair<int, int> e = edges[_lpSeparatorEdges[csPair.second][cHat]];
                    if (e.first == c) continue;
                    incomingMessages.push_back(_forwardMessages[_lpSeparatorEdges[csPair.second][cHat]]);
                }
#endif
            }
            
            // sum_{hat{s}} \lambda_{\hat{s} \to c} + \theta_c
            _intermediateFactors.push_back(new svlFactor(_sharedStorage[0]));
            svlFactor *sumFactor = _intermediateFactors.back();
            if (_cliquePotentials[c]->empty()) {
                svlClique clique = _graph.getClique(c);
                for (svlClique::const_iterator it = clique.begin(); it != clique.end(); ++it) {
                    sumFactor->addVariable(*it, _graph.getCardinality(*it));
                }
            }
            _computations.push_back(new svlFactorAdditionOp(sumFactor,
                                        incomingMessages));
            // max (sum_{hat{s}} \lambda_{\hat{s} \to c} + \theta_c)
            svlClique ns;
            set_difference(_graph.getClique(c).begin(), _graph.getClique(c).end(),
                _lpSeparators[separatorId].begin(), _lpSeparators[separatorId].end(),
                insert_iterator<svlClique>(ns, ns.begin()));
            svlFactor *maxFactor = NULL;
            if (!ns.empty()) {
                _intermediateFactors.push_back(new svlFactor(_sharedStorage[1]));
                maxFactor = _intermediateFactors.back();            
                _computations.push_back(new svlFactorMaximizeOp(maxFactor,
                        sumFactor, ns));
            } else {
                maxFactor = sumFactor;
            }
            // weighted sum
            double w = 1.0 / (double)_lpCliqueEdges[c].size();
            _computations.push_back(new svlFactorWeightedSumOp(_forwardMessages[messageId],
                                        _backwardMessages[messageId], maxFactor,
                                        (w - 1.0), w));
        }

        // add new forward messages to all \lambda_{s \to \hat{c}}
        for (int s = 0; s < (int)_lpCliqueEdges[c].size(); s++) {
            int messageId = _lpCliqueEdges[c][s];
            int separatorId = _lpEdges[messageId].second;
            for (int cHat = 0; cHat < (int)_lpSeparatorEdges[separatorId].size(); cHat++) {
                int backwardMessageId = _lpSeparatorEdges[separatorId][cHat];
                int cHatCliqueId = _lpEdges[backwardMessageId].first;
                if (cHatCliqueId == c)
                    continue;

                SVL_ASSERT((messageId < (int)_forwardMessages.size()) &&
                    (backwardMessageId < (int)_backwardMessages.size()));

                _computations.push_back(new svlFactorAdditionOp(_backwardMessages[backwardMessageId],
                                            _backwardMessages[backwardMessageId],
                                            _forwardMessages[messageId]));
            }
        }    
    }
}

// message passing loops -----------------------------------------------------

bool svlMessagePassingInference::messagePassingLoop(int maxIterations)
{
    SVL_LOG(SVL_LOG_VERBOSE, "Starting message passing loop...");
    SVL_LOG(SVL_LOG_VERBOSE, "..." << _graph.numVariables() << " variables; "
        << _graph.numCliques() << " cliques; "
        << _graph.numEdges() << " edges");
    bool bConverged = false;
    int nIteration = 0;
    
    while (!bConverged) {
	int nConverged = 0;
        bConverged = true;
        nIteration += 1;

	for (int i = 0; i < (int)_computations.size(); i++) {
	    _computations[i]->execute();
	}	

        for (int i = 0; i < (int)_forwardMessages.size(); i++) {
            if (_oldForwardMessages[i]->dataCompareAndCopy(*_forwardMessages[i])) {
                nConverged += 1;
            } else {               
                bConverged = false;   
            }
            
            if (_oldBackwardMessages[i]->dataCompareAndCopy(*_backwardMessages[i])) {
                nConverged += 1;   
            } else {
                bConverged = false;   
            }
        }
        
        SVL_LOG(SVL_LOG_VERBOSE, "...iteration " << nIteration << " ("
            << nConverged << " of " << (2 * _forwardMessages.size()) 
            << " messages converged)");

        if ((nIteration >= maxIterations) && !bConverged) {
            SVL_LOG(SVL_LOG_WARNING, "message passing failed to converge after "
                << nIteration << " iterations (" << nConverged << " of "
                << (2 * _forwardMessages.size()) << " messages converged)"); 
            break;
        }
    }

    if (bConverged) {
	SVL_LOG(SVL_LOG_VERBOSE, "...converged in " << nIteration << " iterations");
    }

    return bConverged;
}

bool svlMessagePassingInference::lazyMessagePassingLoop(int maxIterations)
{
    SVL_ASSERT(_forwardLazySet.size() == _forwardMessages.size());
    SVL_ASSERT(_backwardLazySet.size() == _backwardMessages.size());
    SVL_ASSERT(_fwdMessagesModified.size() == _computations.size());
    SVL_ASSERT(_bckMessagesModified.size() == _computations.size());

    SVL_LOG(SVL_LOG_VERBOSE, "Starting lazy message passing loop...");
    SVL_LOG(SVL_LOG_VERBOSE, "..." << _graph.numVariables() << " variables; "
        << _graph.numCliques() << " cliques; "
        << _graph.numEdges() << " edges");
    bool bConverged = false;
    int nIteration = 0;

    vector<bool> fwdMsgChanged(_forwardMessages.size(), true);
    vector<bool> bckMsgChanged(_backwardMessages.size(), true);
    vector<bool> executionList(_computations.size(), false);

    for (int i = 0; i < (int)_oldForwardMessages.size(); i++) {
        *_oldForwardMessages[i] = *_forwardMessages[i];
        *_oldBackwardMessages[i] = *_backwardMessages[i];
    }

    while (!bConverged) {
	int nConverged = 0;
        bConverged = true;
        nIteration += 1;

        // determine execution list
        fill(executionList.begin(), executionList.end(), false);
        for (int i = 0; i < (int)fwdMsgChanged.size(); i++) {
            if (fwdMsgChanged[i]) {
                for (set<int>::const_iterator it = _forwardLazySet[i].begin();
                     it != _forwardLazySet[i].end(); ++it) {
                    executionList[*it] = true;
                }
            }
            if (bckMsgChanged[i]) {
                for (set<int>::const_iterator it = _backwardLazySet[i].begin();
                     it != _backwardLazySet[i].end(); ++it) {
                    executionList[*it] = true;
                }
            }
        }

        // execute atomic operations
	for (int i = 0; i < (int)_computations.size(); i++) {
            if (executionList[i]) {
                _computations[i]->execute();
            }
	}	

        // find which messages may have changes
#if 0
        fill(fwdMsgChanged.begin(), fwdMsgChanged.end(), false);
        fill(bckMsgChanged.begin(), bckMsgChanged.end(), false);
        for (int i = 0; i < (int)executionList.size(); i++) {
            if (executionList[i]) {
                if (_fwdMessagesModified[i] >= 0)
                    fwdMsgChanged[_fwdMessagesModified[i]] = true;
                if (_bckMessagesModified[i] >= 0)
                    bckMsgChanged[_bckMessagesModified[i]] = true;
            }
        }

        // check for convergence
        for (int i = 0; i < (int)_forwardMessages.size(); i++) {
            if (fwdMsgChanged[i]) {
                if (_oldForwardMessages[i]->dataCompare(*_forwardMessages[i])) {
                    fwdMsgChanged[i] = false;
                    nConverged += 1;
                } else {               
                    *_oldForwardMessages[i] = *_forwardMessages[i];
                    bConverged = false;   
                }
            } else {
                nConverged += 1;
            }

            if (bckMsgChanged[i]) {
                if (_oldBackwardMessages[i]->dataCompare(*_backwardMessages[i])) {
                    bckMsgChanged[i] = false;
                    nConverged += 1;   
                } else {
                    *_oldBackwardMessages[i] = *_backwardMessages[i];
                    bConverged = false;   
                }	    
            } else {
                nConverged += 1;
            }                    
        }
#else
        // check for convergence
        for (int i = 0; i < (int)_forwardMessages.size(); i++) {
            if (_oldForwardMessages[i]->dataCompare(*_forwardMessages[i])) {
                fwdMsgChanged[i] = false;
                nConverged += 1;
            } else {               
                *_oldForwardMessages[i] = *_forwardMessages[i];
                fwdMsgChanged[i] = true;
                bConverged = false;   
            }

            if (_oldBackwardMessages[i]->dataCompare(*_backwardMessages[i])) {
                bckMsgChanged[i] = false;
                nConverged += 1;   
            } else {
                *_oldBackwardMessages[i] = *_backwardMessages[i];
                bckMsgChanged[i] = true;
                bConverged = false;                
            }	    
        }
#endif
        
        SVL_LOG(SVL_LOG_VERBOSE, "...iteration " << nIteration << " ("
            << nConverged << " of " << (2 * _forwardMessages.size()) 
            << " messages converged)");

        if ((nIteration >= maxIterations) && !bConverged) {
            SVL_LOG(SVL_LOG_WARNING, "message passing failed to converge after "
                << nIteration << " iterations (" << nConverged << " of "
                << (2 * _forwardMessages.size()) << " messages converged)"); 
            break;
        }
    }

    if (bConverged) {
	SVL_LOG(SVL_LOG_VERBOSE, "...converged in " << nIteration << " iterations");
    }

    return bConverged;
}

bool svlMessagePassingInference::gemplpMessagePassingLoop(int maxIterations)
{
    SVL_LOG(SVL_LOG_VERBOSE, "Starting message passing loop...");
    SVL_LOG(SVL_LOG_VERBOSE, "..." << _graph.numVariables() << " variables; "
        << _graph.numCliques() << " cliques; "
        << _graph.numEdges() << " edges");
    bool bConverged = false;
    int nIteration = 0;
    
    while (!bConverged) {
	int nConverged = 0;
        nIteration += 1;

	for (int i = 0; i < (int)_computations.size(); i++) {
	    _computations[i]->execute();
	}	

        double dualObjective = 0;
        for (int s = 0; s < (int)_lpSeparatorEdges.size(); s++) {           
            // TODO: add this to computation graph
            svlFactor phi;
            vector<const svlFactor *> incomingMessages;
            incomingMessages.reserve(_lpSeparatorEdges[s].size());
            for (int c = 0; c < (int)_lpSeparatorEdges[s].size(); c++) {
                incomingMessages.push_back(_forwardMessages[_lpSeparatorEdges[s][c]]);
            }
            svlFactorAdditionOp addOp(&phi, incomingMessages);
            addOp.execute();                                
            SVL_ASSERT(phi.numVars() == (int)_lpSeparators[s].size());
            dualObjective += phi[phi.indexOfMax()];
        }
        SVL_LOG(SVL_LOG_VERBOSE, "...iteration " << nIteration 
            << "; dual objective " << dualObjective);
        
        // check for convergence of the dual
        bConverged = (_lastDualObjective - dualObjective < SVL_EPSILON);
        _lastDualObjective = dualObjective;

        if ((nIteration >= maxIterations) && !bConverged) {
            SVL_LOG(SVL_LOG_WARNING, "message passing failed to converge after "
                << nIteration << " iterations (" << nConverged << " of "
                << (2 * _forwardMessages.size()) << " messages converged)"); 
            break;
        }
    }

    if (bConverged) {
	SVL_LOG(SVL_LOG_VERBOSE, "...converged in " << nIteration << " iterations");
    }

    return bConverged;
}

bool svlMessagePassingInference::residualBPMessagePassingLoop(int maxIterations)
{
    // run initial iteration
    for (int i = 0; i < (int)_computations.size(); i++) {
	_computations[i]->execute();
    }	

    // compute residuals
    vector<pair<double, int> > Q(2 * _graph.numEdges());
    for (int i = 0; i < _graph.numEdges(); i++) {
	Q[2 * i].first = 0.0;
	for (int k = 0; k < _forwardMessages[i]->size(); k++) {
	    Q[2 * i].first += fabs((*_forwardMessages[i])[k] - 
		(*_oldForwardMessages[i])[k]);
	}
	Q[2 * i].second = 2 * i;

	Q[2 * i + 1].first = 0.0;
	for (int k = 0; k < _backwardMessages[i]->size(); k++) {
	    Q[2 * i + 1].first += fabs((*_backwardMessages[i])[k] - 
		(*_oldBackwardMessages[i])[k]);
	}
	Q[2 * i + 1].second = 2 * i + 1;
    }
    sort(Q.begin(), Q.end());

    // loop until convergence
    SVL_LOG(SVL_LOG_VERBOSE, "Starting message passing loop...");
    int nIteration = 0;
    bool bConverged;

    while (!(bConverged = Q.back().first < 1.0e-9)) {
        nIteration += 1;
	if (nIteration > maxIterations) {
	    break;
	}

        //cerr << "..." << nIteration << "\r";

	// send message at front of queue
	int edgeIndx = Q.back().second;
	int nodeIndx = -1;
	if (edgeIndx % 2 == 0) {
	    edgeIndx /= 2;
	    *_oldForwardMessages[edgeIndx] = *_forwardMessages[edgeIndx];
	    Q.back().first = 0.0;
	    nodeIndx = _graph.getEdge(edgeIndx).second;
	} else {
	    edgeIndx = (edgeIndx - 1) / 2;
	    *_oldBackwardMessages[edgeIndx] = *_backwardMessages[edgeIndx];
	    Q.back().first = 0.0;
	    nodeIndx = _graph.getEdge(edgeIndx).first;
	}
	
	SVL_ASSERT(nodeIndx >= 0);
	
	// update neighbours
	for (int i = 0; i < (int)Q.size(); i++) {
	    edgeIndx = Q[i].second;
	    if (edgeIndx % 2 == 0) {
		edgeIndx /= 2;
		if (_graph.getEdge(edgeIndx).first == nodeIndx) {
		    _computations[Q[i].second]->execute();

		    Q[i].first = 0.0;
		    for (int k = 0; k < _forwardMessages[edgeIndx]->size(); k++) {
			Q[i].first += fabs((*_forwardMessages[edgeIndx])[k] - 
			    (*_oldForwardMessages[edgeIndx])[k]);
		    }
		}
	    } else {
		edgeIndx = (edgeIndx - 1) / 2;
		if (_graph.getEdge(edgeIndx).second == nodeIndx) {
		    _computations[Q[i].second]->execute();

		    Q[i].first = 0.0;
		    for (int k = 0; k < _backwardMessages[edgeIndx]->size(); k++) {
			Q[i].first += fabs((*_backwardMessages[edgeIndx])[k] - 
			    (*_oldBackwardMessages[edgeIndx])[k]);
		    }
		}
	    }
	}

	// resort Q
	sort(Q.begin(), Q.end());
    }

    if (bConverged) {
	SVL_LOG(SVL_LOG_VERBOSE, "...converged in " << nIteration << " iterations");
    } else {
	int nConverged = 0;
	for (int i = 0; i < _graph.numEdges(); i++) {
            if (_oldForwardMessages[i]->dataCompare(*_forwardMessages[i])) {
                nConverged += 1;   
            }
            if (_oldBackwardMessages[i]->dataCompare(*_backwardMessages[i])) {
                nConverged += 1;   
            }	    
	}

	SVL_LOG(SVL_LOG_WARNING, "message passing failed to converge after "
	     << nIteration << " iterations (" << nConverged << " of "
            << (int)(2 * _graph.numEdges()) << " messages converged)");
    }

    return bConverged;
}

bool svlMessagePassingInference::sontag08MessagePassingLoop(int maxIterations)
{
    const double DUALITY_EPS = 1.0e-4;
    const double WARMSTART_ITERATIONS = 0.1;
    const int MAX_CLIQUES_TO_ADD = 5;
    // TODO: clean this up and generalize

    // run initial inner loop
    _algorithm = SVL_MP_GEMPLP;
    gemplpMessagePassingLoop(maxIterations);

    // decode messages
    finalizeGeneralizedMPLP(); 
    vector<int> mapAssignment(_graph.numVariables(), -1);
    for (int i = 0; i < _graph.numCliques(); i++) {
        if (_cliquePotentials[i]->numVars() == 1) {
            mapAssignment[_cliquePotentials[i]->variableId(0)] = 
                _cliquePotentials[i]->indexOfMax();
        }
    }

    double energy = 0.0;
    for (int i = 0; i < _graph.numCliques(); i++) {
        if (_graph[i].empty())
            continue;
        vector<int> subAssignment = extractSubVector(mapAssignment, _graph[i].vars());
        //energy += log(_graph[i][_graph[i].indexOf(subAssignment)]);
        energy += _graph[i][_graph[i].indexOf(subAssignment)];
    }

    double dualityGap = _lastDualObjective - energy;
    SVL_LOG(SVL_LOG_VERBOSE, "duality gap " << dualityGap);
    if (dualityGap < DUALITY_EPS) {
        SVL_LOG(SVL_LOG_VERBOSE, "MAP solution found: " << toString(mapAssignment));
        _algorithm = SVL_MP_SONTAG08;
        return true;
    }

    int numInitialCliques = (int)_cliquePotentials.size();
    
    // generate clique set
    SVL_LOG(SVL_LOG_VERBOSE, "generating clique list...");
    map<svlClique, vector<int> > cliqueSet;
#if 0
    // triplets from merging existing cliques
    for (int e1 = 0; e1 < (int)_cliquePotentials.size(); e1++) {
        if (_cliquePotentials[e1]->numVars() != 2)
            continue;
        if ((_cliquePotentials[e1]->cards()[0] == 1) ||
            (_cliquePotentials[e1]->cards()[1] == 1))
            continue;
        for (int e2 = e1 + 1; e2 < (int)_cliquePotentials.size(); e2++) {
            if (_cliquePotentials[e2]->numVars() != 2)
                continue;
            if ((_cliquePotentials[e2]->cards()[0] == 1) ||
                (_cliquePotentials[e2]->cards()[1] == 1))
                continue;
            
            svlClique c;
            c.insert(_cliquePotentials[e1]->variableId(0));
            c.insert(_cliquePotentials[e1]->variableId(1));
            c.insert(_cliquePotentials[e2]->variableId(0));
            c.insert(_cliquePotentials[e2]->variableId(1));
            if (c.size() != 3) continue;
            if (cliqueSet.find(c) != cliqueSet.end())
                continue;

            vector<int> edgeIndices;
            edgeIndices.push_back(e1);
            edgeIndices.push_back(e2);
            for (int e = e2 + 1; e < (int)_cliquePotentials.size(); e++) {
                if (_cliquePotentials[e]->numVars() != 2)
                    continue;
                if (c.find(_cliquePotentials[e]->variableId(0)) == c.end())
                    continue;
                if (c.find(_cliquePotentials[e]->variableId(1)) == c.end())
                    continue;

                edgeIndices.push_back(e);
            }

            cliqueSet.insert(make_pair(c, edgeIndices));
        }    
    }
#elif 1
    // triplets from merging existing cliques
    for (int e1 = 0; e1 < (int)_cliquePotentials.size(); e1++) {
        if (_cliquePotentials[e1]->numVars() > 2)
            continue;
        if ((_cliquePotentials[e1]->cards()[0] == 1) ||
            ((_cliquePotentials[e1]->numVars() == 2) &&
                (_cliquePotentials[e1]->cards()[1] == 1)))
            continue;
        for (int e2 = e1 + 1; e2 < (int)_cliquePotentials.size(); e2++) {
            if (_cliquePotentials[e2]->numVars() > 2)
                continue;
            if ((_cliquePotentials[e2]->cards()[0] == 1) ||
                ((_cliquePotentials[e2]->numVars() == 2) &&
                    (_cliquePotentials[e2]->cards()[1] == 1)))
                continue;
            svlClique c;
            for (int v = 0; v < _cliquePotentials[e1]->numVars(); v++) {
                c.insert(_cliquePotentials[e1]->variableId(v));
            }
            for (int v = 0; v < _cliquePotentials[e2]->numVars(); v++) {
                c.insert(_cliquePotentials[e2]->variableId(v));
            }
            //if (c.size() != 3) continue;
            if (c.size() > 3) continue;
            //if (c.size() < 3) continue;
            if (cliqueSet.find(c) != cliqueSet.end())
                continue;

            vector<int> edgeIndices;
            edgeIndices.push_back(e1);
            edgeIndices.push_back(e2);
            for (int e = 0; e < (int)_cliquePotentials.size(); e++) {                
                if ((e == e1) || (e == e2)) continue;
                if (_cliquePotentials[e]->numVars() == 1) continue;
                svlClique c2(c);
                c2.insert(_cliquePotentials[e]->vars().begin(), _cliquePotentials[e]->vars().end());
                if (c2.size() != std::max(c.size(), (size_t)_cliquePotentials[e]->numVars()))
                    continue;

                edgeIndices.push_back(e);
            }

            if (edgeIndices.size() == 2) continue;
            cliqueSet.insert(make_pair(c, edgeIndices));
        }    
    }
#elif 0
    // all triplets
    for (int v1 = 0; v1 < _graph.numVariables(); v1++) {
        for (int v2 = v1 + 1; v2 < _graph.numVariables(); v2++) {
            for (int v3 = v2 + 1; v3 < _graph.numVariables(); v3++) {
                svlClique c;
                c.insert(v1);
                c.insert(v2);
                c.insert(v3);

                vector<int> edgeIndices;
                for (int e = 0; e < (int)_cliquePotentials.size(); e++) {
                    if (_cliquePotentials[e]->hasVariable(v1) ||
                        _cliquePotentials[e]->hasVariable(v2) ||
                        _cliquePotentials[e]->hasVariable(v3)) {
                        edgeIndices.push_back(e);
                    }
                }
                cliqueSet.insert(make_pair(c, edgeIndices));
            }
        }
    }
#else
    // all quadruplets
    for (int v1 = 0; v1 < _graph.numVariables(); v1++) {
        for (int v2 = v1 + 1; v2 < _graph.numVariables(); v2++) {
            for (int v3 = v2 + 1; v3 < _graph.numVariables(); v3++) {
                for (int v4 = v3 + 1; v4 < _graph.numVariables(); v4++) {
                    svlClique c;
                    c.insert(v1);
                    c.insert(v2);
                    c.insert(v3);
                    c.insert(v4);

                    vector<int> edgeIndices;
                    for (int e = 0; e < (int)_cliquePotentials.size(); e++) {
                        int varCount = 0;
                        varCount += _cliquePotentials[e]->hasVariable(v1) ? 1 : 0;
                        varCount += _cliquePotentials[e]->hasVariable(v2) ? 1 : 0;
                        varCount += _cliquePotentials[e]->hasVariable(v3) ? 1 : 0;                        
                        varCount += _cliquePotentials[e]->hasVariable(v4) ? 1 : 0;
                        if (varCount >= 2) {
                            edgeIndices.push_back(e);
                        }
                    }
                    cliqueSet.insert(make_pair(c, edgeIndices));
                }
            }
        }
    }
#endif
    SVL_LOG(SVL_LOG_VERBOSE, "..." << cliqueSet.size() << " candidates generated");

    // iteratively add cliques and re-solve
    while (1) {

        // score each clique in cliqueSet
        multimap<double, svlClique> scoredCliques;
        svlFactorStorage sumBeliefStorage(0, true);
        svlFactorStorage tmpFactorStorage(0, true);
        for (map<svlClique, vector<int> >::const_iterator c = cliqueSet.begin(); c != cliqueSet.end(); ++c) {
            double sumMaxBeliefs = 0.0;
            svlFactor sumBeliefs(&sumBeliefStorage);
            for (int j = 0; j < (int)c->second.size(); j++) {
                int e = c->second[j];
                sumMaxBeliefs += (*_cliquePotentials[e])[_cliquePotentials[e]->indexOfMax()];
                if (sumBeliefs.empty()) {
                    sumBeliefs = *(_cliquePotentials[e]);
                } else {
                    svlFactor phi(&tmpFactorStorage);
                    svlFactorAdditionOp addOp(&phi, &sumBeliefs, _cliquePotentials[e]);
                    addOp.execute();
                    sumBeliefs = phi;
                }
            }
            
            double score = sumMaxBeliefs - sumBeliefs[sumBeliefs.indexOfMax()];
            scoredCliques.insert(pair<double, svlClique>(-score, c->first));

            SVL_LOG(SVL_LOG_DEBUG, "score for clique " << toString(c->first) 
                << " is " << score << " (" << c->second.size() << " neighbours)");
        }

        // add best cliques
#if 1
        int numCliquesAdded = 0;
        vector<pair<svlClique, vector<int> > > cliquesAdded;
        for (multimap<double, svlClique>::const_iterator it = scoredCliques.begin(); 
             it != scoredCliques.end(); ++it) {
            if (numCliquesAdded++ >= MAX_CLIQUES_TO_ADD) break;
            SVL_LOG(SVL_LOG_VERBOSE, numCliquesAdded << "-th best clique is " 
                << toString(it->second) << " (" << - it->first << ")");

            _graph.addClique(it->second);
            cliquesAdded.push_back(make_pair(it->second, cliqueSet[it->second]));
            cliqueSet.erase(cliqueSet.find(it->second));
        }
        _graph.connectGraph();

        if (numCliquesAdded == 0) {
            SVL_LOG(SVL_LOG_ERROR, "failed to add additional cliques");
            break;
        }
#endif

#if 0
        // cold start: reset and run GEMPLP inference
        reset();
        inference(SVL_MP_GEMPLP, maxIterations);
#else
        // warm start: add new messages and operations
        // TO DO: clean up
        _lpCliqueEdges.resize(_graph.numCliques());
        //for (int i = 0; i < _graph.numCliques() - numCliquesAdded; i++) {
        for (int i = 0; i < numInitialCliques; i++) {
            svlClique c_i = _graph.getClique(i);
            for (int j = _graph.numCliques() - numCliquesAdded; 
                 j < _graph.numCliques(); j++) {
                svlClique c_j = _graph.getClique(j);
                svlClique s_ij;
                set_intersection(c_i.begin(), c_i.end(),
                    c_j.begin(), c_j.end(), 
                    insert_iterator<svlClique>(s_ij, s_ij.begin()));
                
                if (s_ij.empty())
                    continue;
                if (s_ij.size() < 2)
                    continue;
                
                int k = 0;
                for ( ; k < (int)_lpSeparators.size(); k++) {
                    if (_lpSeparators[k] == s_ij)
                        break;
                }
                
                if (k == (int)_lpSeparators.size()) {
                    _lpSeparators.push_back(s_ij);
                    _lpSeparatorEdges.push_back(vector<int>());
                }
                
                if (find(_lpEdges.begin(), _lpEdges.end(), make_pair(i, k)) == _lpEdges.end()) {
                    _lpCliqueEdges[i].push_back((int)_lpEdges.size());
                    _lpSeparatorEdges[k].push_back((int)_lpEdges.size());
                    _lpEdges.push_back(make_pair(i, k));
                }
                
                if (find(_lpEdges.begin(), _lpEdges.end(), make_pair(j, k)) == _lpEdges.end()) {
                    _lpCliqueEdges[j].push_back((int)_lpEdges.size());
                    _lpSeparatorEdges[k].push_back((int)_lpEdges.size());
                    _lpEdges.push_back(make_pair(j, k));
                }
            }
        }

        // re-assign initial clique potentials
        SVL_LOG(SVL_LOG_VERBOSE, "re-assigning clique potentials");
        _cliquePotentials.resize(_graph.numCliques(), NULL);
        for (int n = 0; n < _graph.numCliques(); n++) {
            if (_cliquePotentials[n] != NULL)
                delete _cliquePotentials[n];
            _cliquePotentials[n] = new svlFactor(_graph.getCliquePotential(n));
            //for (int i = 0; i < _cliquePotentials[n]->size(); i++) {
            //    (*_cliquePotentials[n])[i] = log((*_cliquePotentials[n])[i]);
            //}
        }

        // add new forward and backward messages
        SVL_LOG(SVL_LOG_VERBOSE, "adding new messages");
        _forwardMessages.reserve(_lpEdges.size());
        _backwardMessages.reserve(_forwardMessages.size());
        for (unsigned i = _forwardMessages.size(); i < _lpEdges.size(); i++) {
            _forwardMessages.push_back(new svlFactor());
            const svlClique s = _lpSeparators[_lpEdges[i].second];
            for (svlClique::const_iterator j = s.begin(); j != s.end(); ++j) {
                _forwardMessages.back()->addVariable(*j, _graph.getCardinality(*j));
            }
            _forwardMessages.back()->fill(0.0);

            //_oldForwardMessages.push_back(new svlFactor());
            //*_oldForwardMessages.back() = *_forwardMessages.back();
        }

        for (unsigned i = _backwardMessages.size(); i < _lpEdges.size(); i++) {
            int cliqueId = _lpEdges[i].first;
            int separatorId = _lpEdges[i].second;
            _backwardMessages.push_back(new svlFactor());
            _backwardMessages.back()->fill(0.0);
            for (unsigned c = 0; c < _lpSeparatorEdges[separatorId].size(); c++) {
                if (_lpEdges[_lpSeparatorEdges[separatorId][c]].first == cliqueId)
                    continue;
                svlFactorAdditionOp addOp(_backwardMessages.back(), _backwardMessages.back(),
                    _forwardMessages[_lpSeparatorEdges[separatorId][c]]);
                addOp.execute();
            }

            //_oldBackwardMessages.push_back(new svlFactor());
            //*_oldBackwardMessages.back() = *_backwardMessages.back();
        }

        // build graph
        SVL_LOG(SVL_LOG_VERBOSE, "clearing graph");
        for (unsigned i = 0; i < _computations.size(); i++) {
            delete _computations[i];
        }
        _computations.clear();
        
        for (unsigned i = 0; i < _intermediateFactors.size(); i++) {
            delete _intermediateFactors[i];
        }
        _intermediateFactors.clear();
        
        for (unsigned i = 0; i < _sharedStorage.size(); i++) {
            delete _sharedStorage[i];
        }
        _sharedStorage.clear();
        
        SVL_LOG(SVL_LOG_VERBOSE, "building graph");
        _algorithm = SVL_MP_GEMPLP;
        buildGeneralizedMPLPGraph();
#ifdef SVL_FACTOR_DEBUG_STATISTICS
        SVL_LOG(SVL_LOG_VERBOSE, "..." << (int)(8 * svlFactorStorage::_dbStatsCurrentMem / (1024 * 1024)) 
            << " MB allocated for factor storage");
#endif
        _lastDualObjective = numeric_limits<double>::max();
        gemplpMessagePassingLoop((int)(WARMSTART_ITERATIONS * maxIterations + 1));
        finalizeGeneralizedMPLP();    
#endif

        // decode messages
        vector<int> mapAssignment(_graph.numVariables(), -1);
        for (int i = 0; i < _graph.numCliques(); i++) {
            if (_cliquePotentials[i]->numVars() == 1) {
                mapAssignment[_cliquePotentials[i]->variableId(0)] = 
                    _cliquePotentials[i]->indexOfMax();
            }
        }
        
        double energy = 0.0;
        for (int i = 0; i < _graph.numCliques(); i++) {
            if (_graph[i].empty())
                continue;
            vector<int> subAssignment = extractSubVector(mapAssignment, _graph[i].vars());
            //energy += log(_graph[i][_graph[i].indexOf(subAssignment)]);
            energy += _graph[i][_graph[i].indexOf(subAssignment)];
        }
        
        double dualityGap = _lastDualObjective - energy;
        SVL_LOG(SVL_LOG_VERBOSE, "duality gap " << dualityGap);
        if (dualityGap < DUALITY_EPS) {
            SVL_LOG(SVL_LOG_VERBOSE, "MAP solution found: " << toString(mapAssignment));
            break;
        }    
    }

    _algorithm = SVL_MP_SONTAG08;
    return true;
}
