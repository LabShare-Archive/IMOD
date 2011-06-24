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
** FILENAME:    svlStatsUtils.cpp
** AUTHOR(S):   Ian Goodfellow <ia3n@stanford.edu>
**              Stephen Gould <sgould@stanford.edu>
**
*****************************************************************************/

#include <cstdlib>
#include <cmath>
#include <vector>

#include "svlLogger.h"
#include "svlCompatibility.h"
#include "svlStatsUtils.h"

using namespace std;

#ifndef M_LN2
#define M_LN2 0.69314718055994530942
#endif

bool containsInvalidEntries(const vector<double> & v)
{
    SVL_ASSERT(v.size() > 0);

    for (unsigned i = 0; i < v.size(); i++) {
        if (isnan(v[i]) || isinf(v[i]))
            return true;
    }   

    return false;
}

double logistic(const vector<double>& theta, const vector<double>& data)
{
    SVL_ASSERT(theta.size() == data.size());

    double sigma;
    sigma = 0.0;
    for (unsigned i = 0; i < theta.size(); i++) {
        sigma -= theta[i] * data[i];
    }

    return 1.0 / (1.0 + exp(sigma));
}

double logistic(const double *theta, const double *data, int n)
{
    double sigma;
    sigma = 0.0;
    for (int i = 0; i < n; i++) {
        sigma -= theta[i] * data[i];
    }

    return 1.0 / (1.0 + exp(sigma));
}

// computes the entropy of a possibly unnormalized distribution
double entropy(const std::vector<double>& p)
{
    double Z = 0.0;
    double H = 0.0;

    for (unsigned i = 0; i < p.size(); i++) {
        if (p[i] > 0.0)
            H += p[i] * log(p[i]);
        Z += p[i];
    }

    return (log(Z) - H / Z) / M_LN2;
}

// exponentiates and normalizes a vector
void expAndNormalize(VectorXd& v)
{
    if (v.size() == 0) return;

    double maxValue = v[0];
    for (int i = 1; i < v.size(); i++) {
        if (v[i] > maxValue)
	    maxValue = v[i];
    }

    v = (v.cwise() - maxValue).cwise().exp();
    double Z = v.sum();
    v /= Z;
}

void expAndNormalize(std::vector<double>& v)
{
    if (v.empty()) return;

    double maxValue = v[0];
    for (unsigned i = 1; i < v.size(); i++) {
        if (v[i] > maxValue) {
            maxValue = v[i];
        }
    }

    double Z = 0.0;
    for (unsigned i = 0; i < v.size(); i++) {
        v[i] = exp(v[i] - maxValue);
        Z += v[i];
    }

    double *v_ptr = &v[0];
    for (int i = (int)v.size() / 2; i != 0; i--) {
        v_ptr[0] /= Z;
        v_ptr[1] /= Z;
        v_ptr += 2;
    }
    if (v.size() % 2 != 0) {
        *v_ptr /= Z;
    }
}

// computes a uniform random permutation of of integers 0..(n-1)
vector<int> randomPermutation(int n)
{
    SVL_ASSERT(n > 0);
    vector<int> v;
    int i, j, k;

    // fill vector with 0 to (n-1)
    v.resize(n);
    for (i = 0; i < n; i++) {
        v[i] = i;
    }

    // randomly swap entries
    for (i = 0; i < n - 1; i++) {
        j = rand() % (n - i);
        k = v[i]; v[i] = v[i + j]; v[i + j] = k;    // swap
    }

    return v;
}

// vector successor and predecessor functions
void predecessor(std::vector<int>& array, int limit)
{
    SVL_ASSERT(limit > 0);

    for (unsigned i = 0; i < array.size(); i++) {
        array[i] -= 1;
        if (array[i] < 0) {
            array[i] = limit - 1;
        } else {
            break;
        }
    }
}


void successor(std::vector<int>& array, int limit)
{
    for (unsigned i = 0; i < array.size(); i++) {
        array[i] += 1;
        if (array[i] >= limit) {
            array[i] = 0;
        } else {
            break;
        }
    }
}

void predecessor(std::vector<int>& array, const std::vector<int>& limits)
{
    SVL_ASSERT(array.size() == limits.size());

    for (unsigned i = 0; i < array.size(); i++) {
        array[i] -= 1;
        if (array[i] < 0) {
            SVL_ASSERT(limits[i] > 0);
            array[i] = limits[i] - 1;
        } else {
            break;
        }
    }
}

void successor(std::vector<int>& array, const std::vector<int>& limits)
{
    SVL_ASSERT(array.size() == limits.size());

    for (unsigned i = 0; i < array.size(); i++) {
        array[i] += 1;
        if (array[i] >= limits[i]) {
            array[i] = 0;
        } else {
            break;
        }
    }
}

// distance metrics
double bhattacharyyaDistance(std::vector<double>& p, std::vector<double>& q)
{
    SVL_ASSERT(p.size() == q.size());

    double d = 0.0;
    double Zp = 0.0; // normalization constant for p
    double Zq = 0.0; // normalization constant for q
    for (unsigned i = 0; i < p.size(); i++) {
	d += sqrt(p[i] * q[i]);
	Zp += p[i];
	Zq += q[i];
    }
    
    SVL_ASSERT((Zp > 0.0) && (Zq > 0.0));

    return -log(d / sqrt(Zp * Zq));
}

double euclideanDistanceSq(std::vector<double>& p, std::vector<double>& q)
{
    SVL_ASSERT(p.size() == q.size());

    double dist = 0.0;
    for (unsigned i = 0; i < p.size(); i++)
        dist += (p[i] - q[i]) * (p[i] - q[i]);
    return dist;
}

double sum(const std::vector<double> &d)
{
    double s = 0.0;
    for (unsigned i = 0; i < d.size(); i++)
        s += d[i];
    return s;
}

int argmin(const VectorXd &v)
{
    if (v.size() == 0)
        return -1;
    
    int minInd = 0;
    for (int i = 1; i < (int)v.size(); i++) {
        if (v[i] < v[minInd])
            minInd = i;
    }
    
    return minInd;
}

int argmax(const VectorXd &v)
{
    if (v.size() == 0)
        return -1;
    
    int maxInd = 0;
    for (int i = 1; i < (int)v.size(); i++) {
        if (v[i] > v[maxInd])
            maxInd = i;
    }
    
    return maxInd;
}

int argrand(const vector<double>& v)
{
    return argrand(Eigen::Map<VectorXd>(&v[0], v.size()));
}

int argrand(const VectorXd &v)
{
    double cutoff = v.sum() * rand() / (double)RAND_MAX;
    double cumSum = 0.0;
    for (int i = 0; i < v.size(); i++) {
        cumSum += v[i];
        if (cumSum >= cutoff) {
            return i;
        }
    }

    SVL_LOG(SVL_LOG_FATAL, "bug");
    return -1;
}

