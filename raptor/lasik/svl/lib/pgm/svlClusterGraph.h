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
** FILENAME:    svlClusterGraph.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**   Cluster graph for Bayesian networks and Markov random fields.
**
*****************************************************************************/

#pragma once

#include <set>
#include <vector>
#include <map>

#include "svlFactor.h"

using namespace std;

// svlClique class ---------------------------------------------------------

typedef set<int> svlClique;

// svlClusterGraph Class ---------------------------------------------------

class svlClusterGraph {
 protected:
    int _nVars;                  // number of variables
    vector<int> _varCards;       // size of domain of each variable

    vector<svlClique> _cliques;
    vector<pair<int, int> > _edges;
    vector<svlClique> _separators;
    
    vector<svlFactor> _initialPotentials;

 public:
    svlClusterGraph();
    svlClusterGraph(int nVars, int varCards = 2);
    svlClusterGraph(int nVars, const vector<int>& varCards);
    svlClusterGraph(int nVars, const vector<int>& varCards,
	const vector<svlClique>& cliques);
    //svlClusterGraph(const svlClusterGraph& g);
    virtual ~svlClusterGraph();

    int numVariables() const { return (int)_nVars; }
    int numCliques() const { return (int)_cliques.size(); }
    int numEdges() const { return (int)_edges.size(); }

    int getCardinality(int v) const { return _varCards[v]; }
    const vector<int>& getCardinalities() const { return _varCards; }
    pair<int, int> getEdge(int e) const { return _edges[e]; }

    void addClique(const svlClique& clique);
    void addClique(const svlClique& clique, const svlFactor& phi);
    void addClique(const svlFactor& phi);
    const svlClique& getClique(int indx) const;
    const svlClique& getSepSet(int indx) const;

    void setCliquePotential(int indx, const svlFactor& phi);
    const svlFactor& getCliquePotential(int indx) const;
    svlFactor& getCliquePotential(int indx);    
    svlFactor getPotential(const svlClique& clique) const;
    svlFactor getPotential(int var) const;

    // compute energy for a given assignment
    double getEnergy(const vector<int>& x, bool bLogPotentials = true) const;
    void decodeMAP(vector<int>& x, int startingClique = -1) const;

    // graph connectivity
    virtual bool connectGraph();
    virtual bool connectGraph(vector<pair<int, int> >& edges);
    bool betheApprox();
       
    // check running intersection property
    bool checkRunIntProp(); 

    ostream& write(ostream& os) const;
    bool write(const char *filename) const;
    bool read(const char *filename);

    svlFactor& operator[](unsigned index);
    svlFactor operator[](unsigned index) const;
    
    //bool operator==(const svlClusterGraph& g) const;

 protected:
    void computeSeparatorSets();
};

