/******************************************************************************
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
** FILENAME:    svlOptimizer.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**   Interface for a solving large scale unconstrained optimization problems.
**   Implements optimization using L-BFGS code is based on an implementation
**   by Jorge Nocedal:
**
**    * J. Nocedal. Updating  Quasi-Newton  Matrices  with  Limited  Storage
**      (1980), Mathematics of Computation 35, pp. 773-782.
**    * D.C. Liu and J. Nocedal. On the  Limited  Memory  Method  for  Large
**      Scale  Optimization  (1989),  Mathematical  Programming  B,  45,  3,
**      pp. 503-528.
**
**   The derived class must override functions objective() and gradient(). It
**   should also implement the objectiveAndGradient() function for efficiency,
**   and may override the monitor() function. The monitor function can assume
**   that _x contains the current estimate for the solution. Other functions
**   should use the input argument.
**
**   This class tries to MINIMIZE the objective function.
**
*****************************************************************************/

#pragma once

#include "Eigen/Core"

// comment out to use internal lbfgs routines
//#define USE_EXTERNAL_LBFGS

class svlOptimizer {
 public:
    unsigned _n;
    double *_x;
    double *_df;

 public:
    svlOptimizer();
    svlOptimizer(unsigned n);
    svlOptimizer(const svlOptimizer& o);
    virtual ~svlOptimizer();

    // initialization and optimization
    void initialize(unsigned n, const double *x = NULL);
    void initialize(const double *x = NULL);
    double solve(unsigned maxiter, double tol = 1.0e-3, bool bMonitor = false);

    // definition of objective function and gradient
    virtual double objective(const double *x) = 0;
    virtual void gradient(const double *x, double *df) = 0;
    virtual double objectiveAndGradient(const double *x, double *df) {
	gradient(x, df); return objective(x);
    }

    // access functions
    inline unsigned size() const { return _n; }
    inline double operator[](unsigned i) const { return _x[i]; }
    inline double& operator[](unsigned i) { return _x[i]; }

    // callback for each iteration during optimization (if bVerbose is true)
    virtual void monitor(unsigned iter, double objValue);

 protected:
#ifndef USE_EXTERNAL_LBFGS
    enum  svlLBFGSResult {
        SVL_LBFGS_ERROR, SVL_LBFGS_MAX_ITERS, SVL_LBFGS_CONVERGED_F,
        SVL_LBFGS_CONVERGED_G, SVL_LBFGS_CONVERGED_X
    };

    svlLBFGSResult lbfgsMinimize(int m, unsigned maxiter,
        double epsg, double epsf, double epsx, bool bMonitor);
    bool lbfgsSearch(double &f, const Eigen::VectorXd &s,
        double& stp, Eigen::VectorXd& diag);
    bool lbfgsStep(double& stx, double& fx, double& dx,
        double& sty, double& fy, double& dy,
        double& stp, const double& fp, const double& dp,
        bool& brackt, const double& stmin, const double& stmax);
#endif
};


