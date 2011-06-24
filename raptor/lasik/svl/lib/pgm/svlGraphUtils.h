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
** FILENAME:    svlGraphUtils.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**  Generic graph utilities.
**
*****************************************************************************/

#pragma once

#include <cassert>
#include <vector>
#include <map>
#include <set>

using namespace std;

// basic data types ---------------------------------------------------------

typedef pair<int, int> svlEdge;

class svlWeightedEdge {
 public:
    int nodeA;    // source node
    int nodeB;    // target node
    double wAB;   // weight from source to target
    double wBA;   // weight from target to source

 public:
    svlWeightedEdge() : 
        nodeA(-1), nodeB(-1), wAB(0.0), wBA(0.0) { /* do nothing */ };
    svlWeightedEdge(int a, int b, double w = 0.0, double v = 0.0) :
        nodeA(a), nodeB(b), wAB(w), wBA(v) { /* do nothing */ };
    ~svlWeightedEdge() { /* do nothing */ };
};

typedef enum _svlTriangulationHeuristic {
    SVL_MAXCARDSEARCH, SVL_MINFILLIN
} svlTriangulationHeuristic;

// graph utilities ----------------------------------------------------------

// Finds the minimum weight spanning tree (forest) given a graph structure
// and weights for each edge (missing edges are assumed to have infinite
// weight). Implementation is based on Kruskal's algorithm.
vector<svlEdge> minSpanningTree(int numNodes, const vector<svlEdge>& edges,
    const vector<double>& weights);

// Maximum cardinality search. Returns an elimination ordering for a chordal
// graph (true) or three nodes that need to be triangulated for non-chordal
// graphs (return value false). The user can supply the starting node for the
// search.
bool maxCardinalitySearch(int numNodes, const vector<svlEdge>& edges,
    vector<int>& prefectOrder, int startNode = -1);

// Triangulate an undirected graph. Modifies the adjacency list inline. The
// resulting graph contains no cycles of length four or more without a chord.
// Different triangulation methods are supported.
void triangulateGraph(int numNodes, vector<svlEdge>& edges,
    svlTriangulationHeuristic method = SVL_MINFILLIN);

// Finds cliques obtained by variable elimination.
vector<set<int> > variableEliminationCliques(const vector<svlEdge>& edges,
    const vector<int>& nodeOrder);

// Floyd-Warshall algorithm for all paths shortest path
vector<vector<double> > allShortestPaths(int numNodes, const vector<svlEdge>& edges,
    const vector<double>& weights);

// Dijkstra's shortest path algorithm
vector<int> shortestPath(int numNodes, const vector<svlEdge>& edges,
    const vector<double>& weights, int source, int sink);
