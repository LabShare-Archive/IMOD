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
** FILENAME:    svlOptimizer.cpp
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**
*****************************************************************************/

// C++ standard library
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cmath>
#include <string.h>
#include <assert.h>
#include <limits>
#include <iomanip>

// Eigen library
using std::ptrdiff_t;
#include "Eigen/Core"

#define USE_EXTERNAL_LBFGS
#ifdef USE_EXTERNAL_LBFGS
// LBFGS library
#include "lbfgs/ap.h"
#include "lbfgs/lbfgs.h"
#endif

// SVL headers
#include "svlCompatibility.h"
#include "svlLogger.h"
#include "svlOptimizer.h"

using namespace std;
USING_PART_OF_NAMESPACE_EIGEN;

// Private Functions --------------------------------------------------------

#ifdef USE_EXTERNAL_LBFGS
static void fdf(ap::real_1d_array x, double& f, ap::real_1d_array& g, void *params)
{
    for (int i = 0; i < (int)((svlOptimizer *)params)->_n; i++) {
        ((svlOptimizer *)params)->_x[i] = x(i + 1);
    }
    f = ((svlOptimizer *)params)->objectiveAndGradient(((svlOptimizer *)params)->_x,
        ((svlOptimizer *)params)->_df);
    for (int i = 0; i < (int)((svlOptimizer *)params)->_n; i++) {
        g(i + 1) = ((svlOptimizer *)params)->_df[i];
    }
}

static void newiter(int iter, const ap::real_1d_array& x, double f,
    const ap::real_1d_array& g, void *params)
{
    for (int i = 0; i < (int)((svlOptimizer *)params)->_n; i++) {
	((svlOptimizer *)params)->_x[i] = x(i + 1);
    }
    ((svlOptimizer *)params)->monitor(iter, f);
}
#endif

// Constructors/Destructors -------------------------------------------------

svlOptimizer::svlOptimizer() :
    _n(0), _x(NULL), _df(NULL)
{
    // do nothing
}

svlOptimizer::svlOptimizer(unsigned n) :
    _n(n)
{
    _x = new double[_n];
    memset(_x, 0, _n * sizeof(double));
    _df = new double[_n];
}

svlOptimizer::svlOptimizer(const svlOptimizer& o) :
    _n(o._n)
{
    if (_n == 0) {
	_x = NULL;
	_df = NULL;
	return;
    }

    _x = new double[_n];
    memcpy(_x, o._x, _n * sizeof(double));
    _df = new double[_n];
    memcpy(_df, o._df, _n * sizeof(double));
}

svlOptimizer::~svlOptimizer()
{
    if (_x != NULL) {
	delete[] _x;
	delete[] _df;
    }
}

// Public Member Functions ---------------------------------------------------

void svlOptimizer::initialize(unsigned n, const double *x)
{
    SVL_ASSERT(n != 0);
    if (_x != NULL) {
	delete[] _x;
	delete[] _df;
    }

    _x = new double[_n = n];
    _df = new double[_n];

    initialize(x);
}

void svlOptimizer::initialize(const double *x)
{
    SVL_ASSERT(_n != 0);
    if (x == NULL) {
	memset(_x, 0, _n * sizeof(double));
    } else {
	memcpy(_x, x, _n * sizeof(double));
    }
}

double svlOptimizer::solve(unsigned maxiter, double tol, bool bMonitor)
{
    SVL_ASSERT(_x != NULL);

    // constants
    const int m = (_n < 7 ? _n : 7);
    const double eps = 1.0e-6;

#ifdef USE_EXTERNAL_LBFGS
    // starting point
    ap::real_1d_array x;
    x.setbounds(1, (int)_n);
    for (int i = 0; i < (int)_n; i++) {
        x(i + 1) = _x[i];
    }

    // optimize
    int info;
    lbfgsminimize(_n, m, x, fdf, (bMonitor ? newiter : NULL),
	(void *)this, tol, eps, eps, maxiter, info);

    switch (info) {
    case 1:
        SVL_LOG(SVL_LOG_MESSAGE, "Optimization converged (relative function decrease <= tol).");
        break;
    case 2:
        SVL_LOG(SVL_LOG_MESSAGE, "Optimization converged (step size <= eps)");
        break;
    case 4:
        SVL_LOG(SVL_LOG_MESSAGE, "Optimization converged (gradient norm <= eps)");
        break;
    case 5:
        SVL_LOG(SVL_LOG_MESSAGE, "Maximum number of iterations reached.");
        break;
    default:
      //SVL_LOG(SVL_LOG_WARNING, "could not complete optimization.");
	SVL_LOG(SVL_LOG_ERROR, "Parameters were: _n = " << _n << ", m = " << m
            << ", bMonitor = " << (bMonitor ? 1 : 0)
            << ", tol = " << tol << ", eps = "  << eps
            << ", maxiter = " << maxiter << ", info = " << info);
        //assert(false);
    }

    // copy out solution
    for (unsigned i = 0; i < _n; i++) {
	_x[i] = x(i + 1);
    }
#else
    svlLBFGSResult info = lbfgsMinimize(m, maxiter, eps, eps, eps, bMonitor);
    switch (info) {
    case SVL_LBFGS_CONVERGED_F:
        SVL_LOG(SVL_LOG_MESSAGE, "Optimization converged (relative function decrease <= tol).");
        break;
    case SVL_LBFGS_CONVERGED_G:
        SVL_LOG(SVL_LOG_MESSAGE, "Optimization converged (step size <= eps)");
        break;
    case SVL_LBFGS_CONVERGED_X:
        SVL_LOG(SVL_LOG_MESSAGE, "Optimization converged (gradient norm <= eps)");
        break;
    case SVL_LBFGS_MAX_ITERS:
        SVL_LOG(SVL_LOG_MESSAGE, "Maximum number of iterations reached.");
        break;
    default:
	SVL_LOG(SVL_LOG_ERROR, "Parameters were: _n = " << _n << ", m = " << m
            << ", bMonitor = " << (bMonitor ? 1 : 0)
            << ", tol = " << tol << ", eps = "  << eps
            << ", maxiter = " << maxiter << ", info = " << info);
    }
#endif

    return objective(_x);
}

void svlOptimizer::monitor(unsigned iter, double objValue)
{
#if 1
    char buffer[32];
    sprintf(buffer, "%5d %10.5f", iter, objValue);
    SVL_LOG(SVL_LOG_MESSAGE, buffer);
    cout.flush();
#else
    fprintf(stdout, "%5d %10.5f\n", iter, objValue);
#endif
}

// Protected Member Functions ------------------------------------------------

#ifndef USE_EXTERNAL_LBFGS
svlOptimizer::svlLBFGSResult svlOptimizer::lbfgsMinimize(int m, unsigned maxiter,
    double epsg, double epsf, double epsx, bool bMonitor)
{
    SVL_ASSERT((m > 0) && (m <= (int)_n));
    SVL_ASSERT((epsg >= 0.0) && (epsf >= 0.0) && (epsx >= 0.0));

    Eigen::Map<VectorXd> x(_x, _n);

    VectorXd w(_n*(2*m+1) + 2*m);
    Eigen::Map<VectorXd> g(_df, _n);
    VectorXd xold(_n);
    VectorXd diag(VectorXd::Ones(_n));

    double f = objectiveAndGradient(_x, _df);
    double fold;

    int point = 0;
    int ispt = _n + 2*m;
    int iypt = ispt + _n*m;
    int npt = 0;

    w.setZero();
    w.segment(ispt, _n) = -g;
    double stp1 = 1.0 / g.norm();

    for (int iter = 0; iter < (int)maxiter; iter++) {
        xold = x;
        fold = f;

        int bound = std::min(iter, m);
        if (iter != 0) {
            double ys = w.segment(iypt + npt, _n).dot(w.segment(ispt + npt, _n));
            double yy = w.segment(iypt + npt, _n).squaredNorm();
            diag.setConstant(ys / yy);
            w[_n + ((point == 0) ? m - 1 : point - 1)] = 1.0 / ys;
            w.start(_n) = -g;

            int cp = point;
            for (int i = 0; i < bound; i++) {
                cp -= 1;
                if (cp == -1) {
                    cp = m - 1;
                }
                double sq = w.segment(ispt + cp * _n, _n).dot(w.start(_n));
                int inmc = _n + m + cp;
                int iycn = iypt + cp * _n;
                w[inmc] = sq * w[_n + cp];

                w.start(_n) -= w[inmc] * w.segment(iycn, _n);
            }

            w.start(_n).cwise() *= diag;

            for (int i = 0; i < bound; i++) {
                double yr = w.segment(iypt + cp * _n, _n).dot(w.start(_n));
                int inmc = _n + m + cp;
                int iscn = ispt + cp * _n;
                double beta = w[inmc] - w[_n + cp] * yr;

                w.start(_n) += beta * w.segment(iscn, _n);
                cp += 1;
                if (cp == m) {
                    cp = 0;
                }
            }

            w.segment(ispt + point * _n, _n) = w.start(_n);
        }
        
        double stp = (iter == 0) ? stp1 : 1.0;
        w.start(_n) = g;

        bool success = lbfgsSearch(f, w.segment(ispt + point * _n, _n), stp, diag);
        if (!success) {
            return SVL_LBFGS_ERROR;
        }

        npt = point * _n;
        w.segment(ispt + npt, _n) *= stp;
        w.segment(iypt + npt, _n) = g - w.start(_n);
        point += 1;
        if (point == m) {
            point = 0;
        }

        if (bMonitor) {
            this->monitor(iter + 1, f);
        }

        if (g.norm() <= epsg) {
            return SVL_LBFGS_CONVERGED_G;
        }

        double tf = std::max(fabs(fold), std::max(fabs(f), 1.0));
        if (fold - f <= epsf * tf) {
            return SVL_LBFGS_CONVERGED_F;
        }
        if ((x - xold).norm() <= epsx) {
            return SVL_LBFGS_CONVERGED_X;
        }
    }

    return SVL_LBFGS_MAX_ITERS;
}

bool svlOptimizer::lbfgsSearch(double &f, const VectorXd &s, double& stp, VectorXd& diag)
{
    static const int MAXFEV = 20;
    static const double STPMIN = pow(10.0, -20.0);
    static const double STPMAX = pow(10.0, 20.0);
    static const double XTOL = 100.0 * numeric_limits<double>::min();
    static const double GTOL = 0.9;
    static const double FTOL = 0.0001;
    static const double XTRAPF = 4.0;

    f = objectiveAndGradient(_x, _df);
    if (stp <= 0) {
        return false;
    }

    Eigen::Map<VectorXd> x(_x, _n);
    Eigen::Map<VectorXd> g(_df, _n);

    double dginit = g.dot(s);
    if (dginit >= 0.0) {
        return false;
    }

    bool brackt = false;
    bool stage1 = true;
    double finit = f;
    double dgtest = FTOL * dginit;
    double width = STPMAX - STPMIN;
    double width1 = 2.0 * width;

    diag = x;

    double stx = 0.0;
    double fx = finit;
    double dgx = dginit;
    double sty = 0.0;
    double fy = finit;
    double dgy = dginit;
    bool infoc = true;


    double fm, fxm, fym;
    double dgm, dgxm, dgym;
    double stmin, stmax;

    for (int nfev = 0; nfev < MAXFEV; nfev++) {
        if (brackt) {
            if (stx < sty) {
                stmin = stx;
                stmax = sty;
            } else {
                stmin = sty;
                stmax = stx;
            }
        } else {
            stmin = stx;
            stmax = stp + XTRAPF * (stp - stx);
        }

        if (stp > STPMAX) {
            stp = STPMAX;
        }
        if (stp < STPMIN) {
            stp = STPMIN;
        }
        if (brackt && ((stp <= stmin) || (stp >= stmax)) || (nfev == MAXFEV - 1) ||
            (!infoc) || (brackt && ((stmax - stmin) <= XTOL * stmax))) {
            stp = stx;
        }

        x = diag + stp * s;
        f = objectiveAndGradient(_x, _df);

        double dg = g.dot(s);
        double ftest1 = finit + stp * dgtest;

        if (brackt && ((stp <= stmin) || (stp >= stmax)) || (!infoc)) {
            return true;
        }
        if ((stp == STPMAX) && (f <= ftest1) && (dg <= dgtest)) {
            return true;
        }
        if ((stp == STPMIN) && ((f >= ftest1) || (dg >= dgtest))) {
            return true;
        }
        if (brackt && (stmax - stmin <= XTOL * stmax)) {
            return true;
        }
        if ((f <= ftest1) && (fabs(dg) <= -GTOL * dginit)) {
            return true;
        }

        stage1 = stage1 && ((f > ftest1) || (dg < std::min(FTOL, GTOL) * dginit));

        if (stage1) {
            fm = f - stp * dgtest;
            fxm = fx - stx * dgtest;
            fym = fy - sty * dgtest;
            dgm = dg - dgtest;
            dgxm = dgx - dgtest;
            dgym = dgy - dgtest;
            infoc = lbfgsStep(stx, fxm, dgxm, sty, fym, dgym, stp, fm, dgm, brackt, stmin, stmax);
            fx = fxm + stx * dgtest;
            fy = fym + sty * dgtest;
            dgx = dgxm + dgtest;
            dgy = dgym + dgtest;
        } else {
            infoc = lbfgsStep(stx, fx, dgx, sty, fy, dgy, stp, f, dg, brackt, stmin, stmax);
        }

        if (brackt) {
            if (fabs(sty - stx) >= 0.66 * width1) {
                stp = stx + 0.5 * (sty - stx);
            }
            width1 = width;
            width = fabs(sty - stx);
        }
    }

    return true;
}

bool svlOptimizer::lbfgsStep(double& stx, double& fx, double& dx, 
    double& sty, double& fy, double& dy,  
    double& stp, const double& fp, const double& dp,
    bool& brackt, const double& stmin, const double& stmax)
{
    bool bound;
    double gamma;
    double p;
    double q;
    double r;
    double s;
    double stpc;
    double stpf;
    double stpq;
    double theta;

    if (brackt && ((stp <= std::min(stx, sty)) || (stp >= std::max(stx, sty))) ||
        (dx * (stp - stx) >= 0) || (stmax < stmin)) {
        return false;
    }

    double sgnd = dp*(dx/fabs(dx));
    if (fp > fx) {
        bound = true;
        theta = 3.0 * (fx - fp) / (stp - stx) + dx + dp;
        s = std::max(fabs(theta), std::max(fabs(dx), fabs(dp)));
        gamma = s * sqrt((theta / s) * (theta / s) - (dx / s) * (dp / s));
        if (stp < stx) {
            gamma = -gamma;
        }
        p = gamma - dx + theta;
        q = gamma - dx + gamma + dp;
        r = p / q;
        stpc = stx + r * (stp - stx);
        stpq = stx + dx/((fx - fp)/(stp - stx) + dx)/2 * (stp - stx);
        if (fabs(stpc - stx) < fabs(stpq - stx)) {
            stpf = stpc;
        } else {
            stpf = stpc + (stpq - stpc)/2;
        }
        brackt = true;

    } else {

        if (sgnd < 0.0) {
            bound = false;
            theta = 3.0 * (fx - fp) / (stp - stx) + dx + dp;
            s = std::max(fabs(theta), std::max(fabs(dx), fabs(dp)));
            gamma = s * sqrt((theta / s) * (theta / s) - (dx / s) * (dp / s));
            if (stp > stx) {
                gamma = -gamma;
            }
            p = gamma - dp + theta;
            q = gamma - dp + gamma + dx;
            r = p / q;
            stpc = stp + r * (stx - stp);
            stpq = stp + dp / (dp - dx) * (stx - stp);
            stpf = (fabs(stpc - stp) > fabs(stpq - stp)) ? stpc : stpq;
            brackt = true;

        } else {

            if (fabs(dp) < fabs(dx)) {
                bound = true;
                theta = 3.0 * (fx - fp) / (stp - stx) + dx + dp;
                s = std::max(fabs(theta), std::max(fabs(dx), fabs(dp)));
                gamma = s * sqrt(std::max(0.0, (theta/s)*(theta/s) - (dx/s)*(dp/s)));
                if (stp > stx) {
                    gamma = -gamma;
                }
                p = gamma - dp + theta;
                q = gamma + (dx - dp) + gamma;
                r = p / q;
                if ((r < 0.0) && (gamma != 0.0)) {
                    stpc = stp + r * (stx - stp);
                } else {
                    stpc = (stp > stx) ? stmax : stmin;
                }

                stpq = stp + dp / (dp - dx) * (stx - stp);
                if (brackt) {
                    stpf = (fabs(stp - stpc) < fabs(stp - stpq)) ? stpc : stpq;
                } else {
                    stpf = (fabs(stp - stpc) > fabs(stp - stpq)) ? stpc : stpq;
                }

            } else {
                bound = false;
                if (brackt) {
                    theta = 3.0 * (fp - fy) / (sty - stp) + dy + dp;
                    s = std::max(fabs(theta), std::max(fabs(dy), fabs(dp)));
                    gamma = s * sqrt((theta/s)*(theta/s) - (dy/s)*(dp/s));
                    if (stp > sty) {
                        gamma = -gamma;
                    }
                    p = gamma - dp + theta;
                    q = gamma - dp + gamma + dy;
                    r = p / q;
                    stpc = stp + r * (sty - stp);
                    stpf = stpc;
                } else {
                    stpf = (stp > stx) ? stmax : stmin;
                }
            }
        }
    }

    if (fp > fx) {
        sty = stp;
        fy = fp;
        dy = dp;
    } else {
        if (sgnd < 0.0) {
            sty = stx;
            fy = fx;
            dy = dx;
        }
        stx = stp;
        fx = fp;
        dx = dp;
    }

    stp = std::max(stmin, std::min(stmax, stpf));
    if (brackt && bound) {
        if (sty > stx) {
            stp = std::min(stx + 0.66*(sty - stx), stp);
        } else {
            stp = std::max(stx + 0.66*(sty - stx), stp);
        }
    }

    return true;
}
#endif
