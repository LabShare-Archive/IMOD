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
** FILENAME:    svlML.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
** DESCRIPTION:
**  Include file for SVL machine learning library.
*****************************************************************************/

#pragma once

#include "../svl/lib/ml/svlBinaryClassifier.h"
#include "../svl/lib/ml/svlBoostedClassifier.h"
#include "../svl/lib/ml/svlBoostedClassifierSet.h"
#include "../svl/lib/ml/svlBoxMuller.h"
#include "../svl/lib/ml/svlClassifier.h"
#include "../svl/lib/ml/svlConfusionMatrix.h"
#include "../svl/lib/ml/svlCodebook.h"
#include "../svl/lib/ml/svlDecisionTree.h"
#include "../svl/lib/ml/svlDecisionTreeSet.h"
#include "../svl/lib/ml/svlDisjointSets.h"
#include "../svl/lib/ml/svlFeatureSelector.h"
#include "../svl/lib/ml/svlFeatureWhitener.h"
#include "../svl/lib/ml/svlGaussian.h"
#include "../svl/lib/ml/svlHistogram.h"
#include "../svl/lib/ml/svlHistogramOperators.h"
#include "../svl/lib/ml/svlLinearRegressor.h"
#include "../svl/lib/ml/svlLogistic.h"
#include "../svl/lib/ml/svlMIFeatureSelector.h"
#include "../svl/lib/ml/svlMLUtils.h"
#include "../svl/lib/ml/svlNativeBoostedClassifier.h"
#include "../svl/lib/ml/svlOpenCVBoostingUtils.h"
#include "../svl/lib/ml/svlPRcurve.h"
#include "../svl/lib/ml/svlQPSolver.h"
#include "../svl/lib/ml/svlSufficientStats.h"
#include "../svl/lib/ml/svlSVM.h"
