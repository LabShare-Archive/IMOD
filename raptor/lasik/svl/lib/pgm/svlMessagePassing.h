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
** FILENAME:    svlMessagePassing.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**   Message passing inference algorithms for Bayesian networks and Markov
**   random fields.
**
** TODO:
**  1. clean up code / separate out LP relaxation inference
**  2. return only singleton marginals / don't copy initial potentials
**  3. use ping-pong buffer for messages (saves copying)
**
*****************************************************************************/

#pragma once

#include <vector>

#include "svlFactor.h"
#include "svlFactorOperations.h"
#include "svlClusterGraph.h"

using namespace std;

// svlMessagePassingAlgorithms ----------------------------------------------

typedef enum _svlMessagePassingAlgorithms {
    SVL_MP_NONE,
    SVL_MP_SUMPROD,
    SVL_MP_MAXPROD,
    SVL_MP_LOGMAXPROD,
    SVL_MP_SUMPRODDIV,
    SVL_MP_MAXPRODDIV,
    SVL_MP_LOGMAXPRODDIV,
    SVL_MP_ASYNCSUMPROD,
    SVL_MP_ASYNCMAXPROD,
    SVL_MP_ASYNCLOGMAXPROD,
    SVL_MP_ASYNCLOGMAXPROD_LAZY, // don't propagate messages if unchanged
    SVL_MP_ASYNCSUMPRODDIV,
    SVL_MP_ASYNCMAXPRODDIV,
    SVL_MP_ASYNCLOGMAXPRODDIV,
    SVL_MP_RBP_SUMPROD,          // Elidan et al, UAI 2006 (Sum-Product)
    SVL_MP_RBP_MAXPROD,          // Elidan et al, UAI 2006 (Max-Product)
    SVL_MP_RBP_LOGMAXPROD,       // Elidan et al, UAI 2006 (log-space Max-Product)
    SVL_MP_GEMPLP,               // Globerson and Jaakkola, NIPS 2007
    SVL_MP_SONTAG08,             // Sontag et al, UAI 2008
} svlMessagePassingAlgorithms;

string toString(svlMessagePassingAlgorithms& mpa);
svlMessagePassingAlgorithms decodeMessagePassingAlgorithm(const char *s);

// svlMessagePassingInference Class -----------------------------------------

class svlMessagePassingInference {
 public:
    static bool SINGLETON_MARGINALS_ONLY;

 protected:
    // cluster graph for inference (includes initial clique potentials)
    svlClusterGraph _graph;
    
    // final clique potentials
    vector<svlFactor *> _cliquePotentials;

    // forward and backward messages during each iteration
    vector<svlFactor *> _forwardMessages;
    vector<svlFactor *> _backwardMessages;
    vector<svlFactor *> _oldForwardMessages;
    vector<svlFactor *> _oldBackwardMessages;
    
    // computation tree: intermediate factors, and (atomic) factor operations
    vector<svlFactor *> _intermediateFactors;
    vector<svlFactorOperation *> _computations;
    svlMessagePassingAlgorithms _algorithm;

    // shared storage for intermediate factors
    vector<svlFactorStorage *> _sharedStorage;

    // data for convergent message passing algorithms (GEMPLP, SONTAG08)
    vector<vector<int> > _lpCliqueEdges;
    vector<vector<int> > _lpSeparatorEdges;
    vector<svlClique> _lpSeparators;
    vector<pair<int, int> > _lpEdges;
    double _lastDualObjective;

    // data for lazy algorithms
    vector<set<int> > _forwardLazySet;
    vector<set<int> > _backwardLazySet;
    vector<int> _fwdMessagesModified;
    vector<int> _bckMessagesModified;
    
 public:
    // Constructors and destructors for all message passing algorithms. A
    // separate inference object needs to be constructed for each cluster
    // graph.
    svlMessagePassingInference(svlClusterGraph& graph);
    virtual ~svlMessagePassingInference();
    
    // Return reference to cluster graph to that factors can be changed.
    // This is useful during training iterations or if the graph structure
    // stays the same, but evidence (features) change.
    inline svlClusterGraph& graph() { return _graph; }

    // Clear all forward and backward messages and computation tree. This
    // method should be called if you plan to keep the inference object
    // around but want to conserve memory between calls. The messages will
    // start from uniform on the next call to inference().
    void reset();

    // Return whether factors are in log-space or normal-space.
    bool isLogSpace() const;

    // Run message passing algorithm to calibrate the cluster graph.
    // Messages and computation graph are not destoryed after runnning
    // inference. Call reset() if you want to free this memory. Calling
    // inference() again before calling reset() will resume with the
    // current messages.
    virtual bool inference(svlMessagePassingAlgorithms mpAlgorithm = SVL_MP_SUMPROD,
	int maxIterations = 100);

    // Access to final clique potentials.
    inline const svlFactor& operator[](unsigned index) const {
	return *_cliquePotentials[index];
    }

 protected:
    // Initialize potentials (normal or log space)
    void initializeMessagePassing();
    void initializeLogMessagePassing();
    void initializeGeneralizedMPLP();

    // Finalize potentials (normal or log space)
    void finalizeMessagePassing();
    void finalizeLogMessagePassing();
    void finalizeGeneralizedMPLP();

    // Constructs message passing computation graphs.
    void buildSumProdComputationGraph(); 
    void buildMaxProdComputationGraph(); 
    void buildLogMaxProdComputationGraph(); 
    void buildSumProdDivComputationGraph(); 
    void buildMaxProdDivComputationGraph(); 
    void buildLogMaxProdDivComputationGraph(); 
    void buildAsyncSumProdComputationGraph(); 
    void buildAsyncMaxProdComputationGraph(); 
    void buildAsyncLogMaxProdComputationGraph(); 
    void buildAsyncLogMaxProdLazyComputationGraph(); 
    void buildAsyncSumProdDivComputationGraph(); 
    void buildAsyncMaxProdDivComputationGraph();
    void buildAsyncLogMaxProdDivComputationGraph(); 
    void buildResidualBPComputationGraph();
    void buildResidualBPMaxProdComputationGraph();
    void buildResidualBPLogMaxProdComputationGraph();
    void buildGeneralizedMPLPGraph();

    // Message passing loops
    bool messagePassingLoop(int maxIterations);
    bool lazyMessagePassingLoop(int maxIterations);
    bool gemplpMessagePassingLoop(int maxIterations);
    bool residualBPMessagePassingLoop(int maxIterations);
    bool sontag08MessagePassingLoop(int maxIterations);
};


