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
** FILENAME:    svlClusterGraph.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**
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
#include <iterator>

#include "xmlParser/xmlParser.h"

#include "svlBase.h"
#include "svlGraphUtils.h"
#include "svlFactor.h"
#include "svlClusterGraph.h"

using namespace std;

// svlClusterGraph Class ---------------------------------------------------

svlClusterGraph::svlClusterGraph() :
    _nVars(0)
{
    // do nothing
}

svlClusterGraph::svlClusterGraph(int nVars, int varCards) :
    _nVars(nVars)
{
    SVL_ASSERT((nVars > 0) && (varCards > 1));
    _varCards.resize(nVars, varCards);
    _cliques.reserve(nVars);
    _initialPotentials.reserve(nVars);
}

svlClusterGraph::svlClusterGraph(int nVars, const vector<int>& varCards) :
    _nVars(nVars), _varCards(varCards)
{
    SVL_ASSERT((nVars > 0) && (varCards.size() == (unsigned)nVars));
    _cliques.reserve(nVars);
    _initialPotentials.reserve(nVars);
}

svlClusterGraph::svlClusterGraph(int nVars, const vector<int>& varCards,
    const vector<svlClique>& cliques) :
    _nVars(nVars), _varCards(varCards), _cliques(cliques)
{
    SVL_ASSERT((nVars > 0) && (varCards.size() == (unsigned)nVars));

    // resize data structues
    if (_cliques.size() > 0) {
        _initialPotentials.resize(_cliques.size());
	connectGraph();
    }
}

svlClusterGraph::~svlClusterGraph()
{
    // do nothing
}

void svlClusterGraph::addClique(const svlClique& c)
{
    _cliques.push_back(c);
    _initialPotentials.push_back(svlFactor());
}

void svlClusterGraph::addClique(const svlClique& c, const svlFactor& phi)
{
    _cliques.push_back(c);
    _initialPotentials.push_back(phi);
}

void svlClusterGraph::addClique(const svlFactor& phi)
{
    _cliques.push_back(svlClique(phi.vars().begin(), phi.vars().end()));
    _initialPotentials.push_back(phi);
}

const svlClique& svlClusterGraph::getClique(int indx) const
{
    SVL_ASSERT((indx >= 0) && (indx < (int)_cliques.size()));
    return _cliques[indx];
}

const svlClique& svlClusterGraph::getSepSet(int indx) const
{
    SVL_ASSERT((indx >= 0) && (indx < (int)_separators.size()));
    return _separators[indx];
}

void svlClusterGraph::setCliquePotential(int indx, const svlFactor& phi)
{
    // error checking
    SVL_ASSERT((indx >= 0) && (indx < (int)_cliques.size()));
    SVL_ASSERT(phi.numVars() == (int)_cliques[indx].size());
    for (svlClique::const_iterator it = _cliques[indx].begin();
	 it != _cliques[indx].end(); ++it) {
        SVL_ASSERT(phi.hasVariable(*it));
    }
    // set potential
    _initialPotentials[indx] = phi;
}

const svlFactor& svlClusterGraph::getCliquePotential(int indx) const
{
    SVL_ASSERT((indx >= 0) && (indx < (int)_cliques.size()));
    return _initialPotentials[indx];
}

svlFactor& svlClusterGraph::getCliquePotential(int indx)
{
    SVL_ASSERT((indx >= 0) && (indx < (int)_cliques.size()));
    return _initialPotentials[indx];
}

svlFactor svlClusterGraph::getPotential(const svlClique& clique) const
{
    SVL_ASSERT_MSG(false, "not implemented yet");
    return svlFactor();
}

svlFactor svlClusterGraph::getPotential(int var) const
{
    SVL_ASSERT_MSG(false, "not implemented yet");
    return svlFactor();
}

// compute energy for a given assignment
double svlClusterGraph::getEnergy(const vector<int>& x, bool bLogPotentials) const
{
    SVL_ASSERT(x.size() == (unsigned)_nVars);

    int k;
    double energy = 0.0;
    for (int i = 0; i < numCliques(); i++) {
        if (_initialPotentials[i].empty())
            continue;

        if (_initialPotentials[i].numVars() == 1) {
            k = x[_initialPotentials[i].variableId(0)];
#if 0
        } else if (_initialPotentials[i].numVars() == 2) {
            int u = _initialPotentials[i].variableId(0);
            int v = _initialPotentials[i].variableId(1);
            k = _initialPotentials[i].indexOf(v, x[v],
                _initialPotentials[i].indexOf(u, x[u]));
#endif
        } else {
            vector<int> v = extractSubVector(x, _initialPotentials[i].vars());
            k = _initialPotentials[i].indexOf(v);
            SVL_ASSERT(k < _initialPotentials[i].size());
        }

        if (bLogPotentials) {
            energy -= _initialPotentials[i][k];
        } else {
            energy -= log(_initialPotentials[i][k]);
        }
    }

    return energy;
}

void svlClusterGraph::decodeMAP(vector<int>& x, int startingClique) const
{
    // reset assignment
    x.resize(_nVars);
    fill(x.begin(), x.end(), -1);

    // find largest starting clique if not given
    if (startingClique == -1) {
        startingClique = 0;
        for (int i = 1; i < (int)_cliques.size(); i++) {
            if (_cliques[i].size() > _cliques[startingClique].size()) {
                startingClique = i;
            }
        }
    }

    // breadth-first-search over cliques
    vector<int> visited(_cliques.size(), false);
    while (startingClique >= 0) {
        deque<int> frontier(1, startingClique);
        while (!frontier.empty()) {
            int cliqueIndx = frontier.front();
            frontier.pop_front();

            if (visited[cliqueIndx]) continue;
            SVL_LOG(SVL_LOG_DEBUG, "...processing clique " << toString(_cliques[cliqueIndx]));

            // condition factor on existing assignments
            svlFactor phi = _initialPotentials[cliqueIndx];
            vector<int> vars = phi.vars();
            for (vector<int>::const_iterator v = vars.begin(); v != vars.end(); v++) {
                if (x[*v] != -1) {
                    phi.reduce(*v, x[*v]);
                }
            }

            // maximize over remaining variables
            vector<int> a;
            phi.assignmentOf(phi.indexOfMax(), a);
            for (int k = 0; k < phi.numVars(); k++) {
                x[phi.variableId(k)] = a[k];
            }

            // add unvisited neighbors to frontier
            for (vector<pair<int, int> >::const_iterator e = _edges.begin();
                 e != _edges.end(); e++) {
                if ((e->first == cliqueIndx) && (!visited[e->second])) {
                    frontier.push_back(e->second);
                }
                if ((e->second == cliqueIndx) && (!visited[e->first])) {
                    frontier.push_back(e->first);
                }
            }

            visited[cliqueIndx] = true;
        }

        // find any unvisited cliques
        startingClique = -1;
        for (int i = 0; i < (int)_cliques.size(); i++) {
            if (!visited[i]) {
                startingClique = i;
                break;
            }
        }
    }
}


// Check the running intersection property by performing depth first
// search for each variable in the network.
bool svlClusterGraph::checkRunIntProp()
{
    deque< pair<int, int> > frontier;  // frontier of search
    vector<bool> visited;              // true if node already visited

    // check running intersection property for each variable
    visited.resize(_cliques.size());
    for (int v = 0; v < _nVars; v++) {
#if 0
        cerr << "checking variable " << v << "..." << endl;
#endif
        // reset data for this variable
        fill(visited.begin(), visited.end(), false);
        frontier.clear();
        // find first clique containing v
        for (int i = 0; i < (int)_cliques.size(); i++) {
            if (_cliques[i].find(v) != _cliques[i].end()) {
                frontier.push_back(make_pair(i, -1));
                break;
            }
        }
	// if frontier is empty then variable doesn't exist in network
        if (frontier.empty())
	    continue;

        // perform depth first search to find loops
        while (!frontier.empty()) {
#if 0
            cerr << "...visiting node " << frontier.front().first << endl;
#endif
            // check for loop
            if (visited[frontier.front().first]) {
                cerr << "ERROR: variable " << v << " violates RunIntProp (loop)" << endl;
                return false;
            }
            // mark node as visited
            visited[frontier.front().first] = true;
            // add neighbours to frontier
            for (int i = 0; i < (int)_edges.size(); i++) {
#if 1
		if (_separators[i].find(v) == _separators[i].end())
		    continue;
#endif
                if (_edges[i].first == frontier.front().first) {
                    // make sure we're not backtracking along same edge
                    // and that neighbour contains variable v
                    if ((frontier.front().second != _edges[i].second) &&
                        (_cliques[_edges[i].second].find(v) !=
			    _cliques[_edges[i].second].end())) {
                        frontier.push_back(make_pair(_edges[i].second, _edges[i].first));
                    }
                } else if (_edges[i].second == frontier.front().first) {
                    // make sure we're not backtracking along same edge
                    // and that neighbour contains variable v
                    if ((frontier.front().second != _edges[i].first) &&
                        (_cliques[_edges[i].first].find(v) !=
			    _cliques[_edges[i].first].end())) {
                        frontier.push_back(make_pair(_edges[i].first, _edges[i].second));
                    }
                }
            }
            frontier.pop_front();
        }

        // check for disjoint clique
        for (int i = 0; i < (int)_cliques.size(); i++) {
            if ((_cliques[i].find(v) != _cliques[i].end()) && (visited[i] == false)) {
                cerr << "ERROR: variable " << v << " violates RunIntProp (disjoint cliques)" << endl;
                return false;
            }
        }
    }

    return true;
}

ostream& svlClusterGraph::write(std::ostream& os) const
{
    os << "<ClusterGraph vars=\"" << _nVars << "\" nodes=\""
       << _cliques.size() << "\" edges=\"" << _edges.size()
       << "\" version=\"1\">\n";

    // write domain cardinalities
    os << "  <VarCards>\n   ";
    for (int i = 0; i < _nVars; i++) {
	os << " " << _varCards[i];
    }
    os << "\n  </VarCards>\n";

    // write cliques
    for (int i = 0; i < (int)_cliques.size(); i++) {
	os << "  <Clique index=\"" << i << "\" size=\""
	   << _cliques[i].size() << "\">\n   ";
	for (svlClique::const_iterator it = _cliques[i].begin();
	     it != _cliques[i].end(); ++it) {
	    os << " " << *it;
	}
	os << "\n  </Clique>\n";
    }

    // write edges
    os << "\n  <Edges>\n";
    for (int i = 0; i < (int)_edges.size(); i++) {
	os << "    " << _edges[i].first << " " << _edges[i].second << "\n";
    }
    os << "  </Edges>\n\n";

    // write initial potentials
    os << "  <Potentials>\n";
    for (unsigned i = 0; i < _initialPotentials.size(); i++) {
	_initialPotentials[i].write(os, 4);
    }
    os << "  </Potentials>\n";

    os << "</ClusterGraph>\n" << endl;

    return os;
}

bool svlClusterGraph::write(const char *filename) const
{
    ofstream ofs(filename);
    this->write(ofs);
    ofs.close();

    return (!ofs.fail());
}

bool svlClusterGraph::read(const char *filename)
{
    _varCards.clear();
    _cliques.clear();
    _edges.clear();
    _separators.clear();
    _initialPotentials.clear();

    XMLNode root = XMLNode::parseFile(filename, "ClusterGraph");
    if (root.isEmpty()) {
	return false;
    }

    _nVars = atoi(root.getAttribute("vars"));
    int numNodes = atoi(root.getAttribute("nodes"));
    int numEdges = atoi(root.getAttribute("edges"));

    _cliques.resize(numNodes);
    _edges.resize(numEdges);
    _separators.resize(numEdges);
    _initialPotentials.resize(numNodes);

    XMLNode node = root.getChildNode("VarCards");
    parseString(string(node.getText()), _varCards);
    SVL_ASSERT(_varCards.size() == (unsigned)_nVars);

    vector<int> v;
    for (int i = 0; i < root.nChildNode("Clique"); i++) {
	node = root.getChildNode("Clique", i);
	int index = atoi(node.getAttribute("index"));
	v.clear();
	parseString(string(node.getText()), v);
	_cliques[index].insert(v.begin(), v.end());
    }

    node = root.getChildNode("Edges");
    if (!node.isEmpty()) {
        v.clear();
        parseString(string(node.getText()), v);
        SVL_ASSERT(2 * numEdges == (int)v.size());
        for (int i = 0; i < numEdges; i++) {
            _edges[i] = make_pair(v[2*i], v[2*i+1]);
        }
        computeSeparatorSets();
    } else {
        // connect graph if no edges found
        connectGraph();
    }

    // TO DO: fix me so that a factor can be assigned to any node
    node = root.getChildNode("Potentials");
    SVL_ASSERT(node.nChildNode("Factor") <= (int)_initialPotentials.size());
    for (int i = 0; i < node.nChildNode("Factor"); i++) {
	XMLNode phi = node.getChildNode("Factor", i);
	_initialPotentials[i] = svlFactor(phi);
    }

    return true;
}

bool svlClusterGraph::connectGraph()
{
#if 0
    return betheApprox();
#else
    _edges.clear();
    _separators.clear();

    // find cliques containing each variable
    vector<vector<int> > cliqueIndex(_nVars);
    for (int i = 0; i < (int)_cliques.size(); i++) {
	for (svlClique::const_iterator it = _cliques[i].begin();
	     it != _cliques[i].end(); ++it) {
	    SVL_ASSERT(*it < _nVars);
	    cliqueIndex[*it].push_back(i);
	}
    }

    // find max-spanning-tree for each variable
    vector<pair<int, int> > candidateEdges;
    vector<double> weights;
    for (int n = 0; n < _nVars; n++) {
	// create weighted graph
	candidateEdges.clear();
	weights.clear();
	for (int i = 0; i < (int)cliqueIndex[n].size() - 1; i++) {
	    for (int j = i + 1; j < (int)cliqueIndex[n].size(); j++) {
		candidateEdges.push_back(make_pair(cliqueIndex[n][i],
			cliqueIndex[n][j]));
		svlClique s;
		set_intersection(_cliques[cliqueIndex[n][i]].begin(),
		    _cliques[cliqueIndex[n][i]].end(),
		    _cliques[cliqueIndex[n][j]].begin(),
		    _cliques[cliqueIndex[n][j]].end(),
		    insert_iterator<svlClique>(s, s.begin()));
		weights.push_back(-(double)s.size());
	    }
	}
	// find spanning tree
	vector<pair<int, int> > spanningTree = minSpanningTree((int)_cliques.size(),
	    candidateEdges, weights);
#if 0
	cerr << "--- full graph for var " << n << " ---\n";
	for (unsigned i = 0; i < candidateEdges.size(); i++) {
	    cerr << " " << candidateEdges[i].first << " "
		 << candidateEdges[i].second << " (" << weights[i] << ")\n";
	}
	cerr << "--- spanning tree for var " << n << " ---\n";
	for (vector<pair<int, int> >::const_iterator it = spanningTree.begin();
	     it != spanningTree.end(); ++it) {
	    cerr << " " << it->first << " " << it->second << "\n";
	}
#endif
	// add edges and separators to cluster graph
	for (vector<pair<int, int> >::const_iterator it = spanningTree.begin();
	     it != spanningTree.end(); ++it) {
            SVL_ASSERT(it->first != it->second);
	    int existingIndx = 0;
	    while (existingIndx < (int)_edges.size()) {
		if ((_edges[existingIndx].first == it->first) &&
		    (_edges[existingIndx].second == it->second)) {
		    break;
		}
		existingIndx++;
	    }

	    if (existingIndx < (int)_edges.size()) {
		_separators[existingIndx].insert(n);
	    } else {
		_edges.push_back(*it);
		_separators.push_back(svlClique());
		_separators.back().insert(n);
	    }
	}
    }

    return true;
#endif
}

bool svlClusterGraph::connectGraph(vector<pair<int, int> >& edges)
{
    _edges = edges;
    computeSeparatorSets();
    return true;
}

// Connect graph using the bethe-approximation to the energy
// functional. All messages pass through marginals.
bool svlClusterGraph::betheApprox()
{
    _edges.clear();
    _separators.clear();

    // find singleton connecting nodes
    vector<int> singletonNodes(_nVars, -1);
    for (int i = 0; i < (int)_cliques.size(); i++) {
	if (_cliques[i].size() != 1)
	    continue;
	if (singletonNodes[*_cliques[i].begin()] == -1) {
	    singletonNodes[*_cliques[i].begin()] = i;
	}
    }

#if 0
    // insert any missing nodes
    for (int i = 0; i < _nVars; i++) {
	if (singletonNodes[i] == -1) {
	    svlClique clique;
	    clique.insert(i);
	    _cliques.push_back(clique);
	    singletonNodes[i] = _cliques.size() - 1;
	}
    }
#else
    // require singleton nodes
    for (int i = 0; i < _nVars; i++) {
	if (singletonNodes[i] == -1) {
	    SVL_LOG(SVL_LOG_FATAL, "missing node for variable " << i);
	    return false;
	}
    }
#endif

    // connect clique to singleton nodes
    for (int i = 0; i < (int)_cliques.size(); i++) {
	if ((_cliques[i].size() == 1) &&
	    (singletonNodes[*_cliques[i].begin()] == i))
	    continue;

	for (svlClique::const_iterator it = _cliques[i].begin();
	     it != _cliques[i].end(); ++it) {
	    SVL_ASSERT(singletonNodes[*it] != -1);
	    _edges.push_back(make_pair(i, singletonNodes[*it]));
	    _separators.push_back(_cliques[singletonNodes[*it]]);
	}
    }

    return true;
}

svlFactor& svlClusterGraph::operator[](unsigned index)
{
    return _initialPotentials[index];
}

svlFactor svlClusterGraph::operator[](unsigned index) const
{
    return _initialPotentials[index];
}

//bool operator==(const svlClusterGraph& g) const;

void svlClusterGraph::computeSeparatorSets()
{
    _separators.resize(_edges.size());
    for (unsigned i = 0; i < _edges.size(); i++) {
	svlClique s;
	set_intersection(_cliques[_edges[i].first].begin(),
	    _cliques[_edges[i].first].end(),
	    _cliques[_edges[i].second].begin(),
	    _cliques[_edges[i].second].end(),
	    insert_iterator<svlClique>(s, s.begin()));
	_separators[i] = s;
    }
}

