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
** FILENAME:    svlGraphUtils.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**
*****************************************************************************/

#include <cstdlib>
#include <cassert>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <limits>
#include <queue>

#include "svlBase.h"
#include "svlGraphUtils.h"

using namespace std;

// TO DO: speed up using union-find data structure.
vector<svlEdge> minSpanningTree(int numNodes, const vector<svlEdge>& edges,
    const vector<double>& weights)
{
    SVL_ASSERT(numNodes > 0);
    SVL_ASSERT(edges.size() == weights.size());
    vector<svlEdge> spanningTree;

    // sort edges by weight
    multimap<double, svlEdge> sortedEdges;
    for (unsigned i = 0; i < edges.size(); i++) {
	//assert(weights[i] >= 0.0);
	sortedEdges.insert(make_pair(weights[i], edges[i]));
    }

    // create forest of connected components
    vector<int> forestIndx(numNodes);
    for (int i = 0; i < numNodes; i++) {
	forestIndx[i] = i;
    }

    // greedy search on edges
    for (multimap<double, svlEdge>::const_iterator it = sortedEdges.begin();
	 it != sortedEdges.end(); ++it) {
	int indxA = forestIndx[it->second.first];
	int indxB = forestIndx[it->second.second];
	// check if nodes adjacent to edge are already in the same forest
	if (indxA == indxB)
	    continue;
	// add edge
	spanningTree.push_back(it->second);
	// merge forests
	for (int i = 0; i < numNodes; i++) {
	    if (forestIndx[i] == indxB)
		forestIndx[i] = indxA;
	}
    }

    return spanningTree;
}

// Maximum cardinality search algorithm.
bool maxCardinalitySearch(int numNodes, const vector<svlEdge>& edges,
    vector<int>& prefectOrder, int startNode)
{
    SVL_ASSERT((numNodes > 0) && (startNode < numNodes));

    // generate neighbourhood lists
    vector<set<int> > nbrs(numNodes);
    for (vector<svlEdge>::const_iterator it = edges.begin(); it != edges.end(); ++it) {
	SVL_ASSERT((it->first < numNodes) && (it->second < numNodes));
	nbrs[it->first].insert(it->second);
	nbrs[it->second].insert(it->first);
    }

    // use starting node with most neighbours
    if (startNode < 0) {
	startNode = 0;
	for (int i = 1; i < numNodes; i++) {
	    if (nbrs[i].size() > nbrs[startNode].size()) {
		startNode = i;
	    }
	}
    }

    // keep track of node ordering
    vector<int> nodeOrdering(numNodes, -1);
    vector<int> weights(numNodes, 0);

    int nextNode = startNode;
    while (1) {
	// order nextNode
	nodeOrdering[nextNode] = --numNodes;
        if (numNodes == 0)
            break;

	// increase weight of neighbours
	for (set<int>::const_iterator it = nbrs[nextNode].begin();
	     it != nbrs[nextNode].end(); ++it) {
	    if (nodeOrdering[*it] < 0)
		weights[*it] += 1;
	}

	// find next node to label
	nextNode = 0;
	while (nodeOrdering[nextNode] >= 0) {
	    nextNode += 1;
	    SVL_ASSERT(nextNode < (int)nodeOrdering.size());
	}
	for (int i = nextNode + 1; i < (int)nodeOrdering.size(); i++) {
	    if ((weights[i] > weights[nextNode]) && (nodeOrdering[i] >= 0))
		nextNode = i;
	}

	// check labeled neighbors (it and jt) of next node are adjacent
	for (set<int>::const_iterator it = nbrs[nextNode].begin();
	     it != nbrs[nextNode].end(); ++it) {
	    if (nodeOrdering[*it] < 0)
		continue;
	    set<int>::const_iterator jt = it;
	    for (++jt; jt != nbrs[nextNode].end(); ++jt) {
		if (nodeOrdering[*jt] < 0)
		    continue;
		if (nbrs[*it].find(*jt) == nbrs[*it].end()) {
		    // graph is not chordal --- return offending nodes
		    prefectOrder.resize(3, 0);
		    prefectOrder[0] = nextNode;
		    prefectOrder[1] = *it;
		    prefectOrder[2] = *jt;
		    return false;
		}
	    }
	}
    }

    // construct prefect ordering
    prefectOrder.resize(nodeOrdering.size(), 0);
    for (unsigned i = 0; i < nodeOrdering.size(); i++) {
	prefectOrder[nodeOrdering[i]] = i;
    }

    return true;
}

// Graph triangulation.
void triangulateGraph(int numNodes, vector<svlEdge>& edges,
    svlTriangulationHeuristic method)
{
    switch (method) {
    case SVL_MAXCARDSEARCH:
        {
            vector<int> v;
            while (!maxCardinalitySearch(numNodes, edges, v)) {
                // add chord
                edges.push_back(make_pair(v[1], v[2]));
            }
        }
        break;

    case SVL_MINFILLIN:
        {
            // After the elimination its neighbours are united in clique.
            // Cost of a vertex is the number of edges that will be added
            // after its elimination. Minimum cost vertex is eliminated.

            // generate neighbourhood lists
            vector<set<int> > nbrs(numNodes);
            for (vector<svlEdge>::const_iterator it = edges.begin(); it != edges.end(); ++it) {
                SVL_ASSERT((it->first < numNodes) && (it->second < numNodes));
                nbrs[it->first].insert(it->second);
                nbrs[it->second].insert(it->first);
            }

            // loop though adding one node at a time
            vector<bool> nodeAdded(numNodes, false);
            for (int i = 0; i < numNodes; i++) {
                int minIndx = -1;
                int minWeight = numeric_limits<int>::max();
                for (int j = 0; j < numNodes; j++) {
                    if (nodeAdded[j]) continue;
                    // count number of edges that need for neighbour clique
                    int w = 0;
                    for (set<int>::const_iterator it = nbrs[j].begin();
                         it != nbrs[j].end(); ++it) {
                        if (nodeAdded[*it]) continue;
                        set<int>::const_iterator jt = it;
                        for (++jt; jt != nbrs[j].end(); ++jt) {
                            if (nodeAdded[*jt]) continue;

                            if (nbrs[*it].find(*jt) == nbrs[*it].end()) {
                                w += 1;
                            }
                        }
                    }

                    if (w < minWeight) {
                        minWeight = w;
                        minIndx = j;
                        if (minWeight == 0) break;
                    }
                }

                // add node with minimumWeight
                SVL_ASSERT(minIndx != -1);
                nodeAdded[minIndx] = true;
                for (set<int>::const_iterator it = nbrs[minIndx].begin();
                     it != nbrs[minIndx].end(); ++it) {
                    if (nodeAdded[*it]) continue;
                    set<int>::const_iterator jt = it;
                    for (++jt; jt != nbrs[minIndx].end(); ++jt) {
                        if (nodeAdded[*jt]) continue;
                        if (nbrs[*it].find(*jt) == nbrs[*it].end()) {
                            edges.push_back(make_pair(*it, *jt));
                            nbrs[*it].insert(*jt);
                            nbrs[*jt].insert(*it);
                        }
                    }
                }
            }
        }
        break;

    default:
        SVL_LOG(SVL_LOG_FATAL, "unknown triangulation method");
    }
}

// Finds cliques obtained by variable elimination.
vector<set<int> > variableEliminationCliques(const vector<svlEdge>& edges,
    const vector<int>& nodeOrder)
{
    vector<set<int> > cliques;

    vector<svlEdge> remainingEdges(edges);
    for (vector<int>::const_iterator it = nodeOrder.begin();
         it != nodeOrder.end(); ++it) {
        // create a clique with current node and its neighbours
        set<int> c;
        c.insert(*it);
        for (vector<svlEdge>::iterator e = remainingEdges.begin();
             e != remainingEdges.end(); ++e) {
            if (e->first == *it) {
                c.insert(e->second);
                remainingEdges.erase(e);
            } else if (e->second == *it) {
                c.insert(e->first);
                remainingEdges.erase(e);
            }
        }
        // check if subset of another clique
        bool bMaximalClique = true;
        //FernandoAmat:modification to avoid error with iterators in gcc 4.0.1
        const vector<set<int> >::const_reverse_iterator auxIter = cliques.rend();
        for (vector<set<int> >::const_reverse_iterator jt = cliques.rbegin();
             jt != auxIter; ++jt) {
            vector<int> d;
            set_difference(c.begin(), c.end(), jt->begin(), jt->end(), d.end());
            if (d.empty()) {
                bMaximalClique = false;
                break;
            }
        }
        // add clique
        if (bMaximalClique) {
            cliques.push_back(c);
        }
    }

    return cliques;
}

// Floyd-Warshall algorithm for all paths shortest path
vector<vector<double> > allShortestPaths(int numNodes, const vector<svlEdge>& edges,
    const vector<double>& weights)
{
    SVL_ASSERT(edges.size() == weights.size());

    // populate edge cost matrix
    vector<vector<double> > edgeCosts(numNodes,
        vector<double>(numNodes, numeric_limits<double>::infinity()));
    for (unsigned e = 0; e < edges.size(); e++) {
        SVL_ASSERT(edges[e].first < numNodes);
        SVL_ASSERT(edges[e].second < numNodes);
        edgeCosts[edges[e].first][edges[e].second] = weights[e];
        edgeCosts[edges[e].second][edges[e].first] = weights[e];
    }
    for (int i = 0; i < numNodes; i++) {
        edgeCosts[i][i] = 0.0;
    }

    // initialize shortest path matrix
    vector<vector<double> > shortestPaths(edgeCosts);

    // run Floyd-Warshall
    for (int k = 0; k < numNodes; k++) {
        for (int i = 0; i < numNodes; i++) {
            for (int j = 0; j < numNodes; j++) {
                shortestPaths[i][j] = std::min(shortestPaths[i][j],
                    shortestPaths[i][k] + shortestPaths[k][j]);
            }
        }
    }

    return shortestPaths;
}

// Dijkstra's shortest path algorithm
vector<int> shortestPath(int numNodes, const vector<svlEdge>& edges,
    const vector<double>& weights, int source, int sink)
{
    SVL_ASSERT(edges.size() == weights.size());

    // initialize queue
    priority_queue<pair<double, int> > Q;
    vector<double> dist(numNodes);
    vector<int> previous(numNodes, -1);
    for (int i = 0; i < numNodes; i++) {
        dist[i] = (i == source) ? 0.0 : numeric_limits<double>::infinity();
        Q.push(make_pair(dist[i], i));
    }

    // initalize neighbors
    vector<set<pair<int, double> > > nbrs(numNodes);
    for (unsigned e = 0; e < edges.size(); e++) {
        SVL_ASSERT(edges[e].first < numNodes);
        SVL_ASSERT(edges[e].second < numNodes);

        nbrs[edges[e].first].insert(make_pair(edges[e].second, weights[e]));
        nbrs[edges[e].second].insert(make_pair(edges[e].first, weights[e]));
    }

    // loop until we reach sink
    while (!Q.empty()) {
        int u = Q.top().second;
        if (u == sink) break;

        double c = Q.top().first;
        Q.pop();

        for (set<pair<int, double> >::const_iterator v = nbrs[u].begin();
             v != nbrs[u].end(); v++) {
            double alt = c + v->second;
            if (alt < dist[v->first]) {
                dist[v->first] = alt;
                previous[v->first] = u;
            }
        }
    }

    // find path
    vector<int> path;
    int u = previous[sink];
    while (u != source) {
        SVL_ASSERT(previous[u] != -1);
        path.push_back(u);
        u = previous[u];
    }
    path.push_back(u);
    reverse(path.begin(), path.end());

    return path;
}
