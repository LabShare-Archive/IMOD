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
** FILENAME:    svlVision.h
** AUTHOR(S):   Stephen Gould <sgould@stanford.edu>
**              Ian Goodfellow <ia3n@cs.stanford.edu>
**              Olga Russakovsky <olga@cs.stanford.edu>
** DESCRIPTION:
**  Include file for vision library.
*****************************************************************************/

#pragma once

#include "../svl/lib/vision/svlCacheOutputUtils.h"
#include "../svl/lib/vision/svlCameraExtrinsics.h"
#include "../svl/lib/vision/svlCameraIntrinsics.h"
#include "../svl/lib/vision/svlCompositeFeatureExtractor.h"
#include "../svl/lib/vision/svlConvolution.h"
#include "../svl/lib/vision/svlDataFrame.h"
#include "../svl/lib/vision/svlDepthSegImage.h"
#include "../svl/lib/vision/svlImageBufferManager.h"
#include "../svl/lib/vision/svlFeatureExtractor.h"
#include "../svl/lib/vision/svlFeatureExtractorFileChecker.h"
#include "../svl/lib/vision/svlFilterBankResponse.h"
#include "../svl/lib/vision/svlExtensionTree.h"
#include "../svl/lib/vision/svlFeatureVectors.h"
#include "../svl/lib/vision/svlHarrisCornerDetector.h"
#include "../svl/lib/vision/svlHarrisLaplaceDetector.h"
#include "../svl/lib/vision/svlImageLoader.h"
#include "../svl/lib/vision/svlImageProjector.h"
#include "../svl/lib/vision/svlImageSequence.h"
#include "../svl/lib/vision/svlImageWindowExtractor.h"
#include "../svl/lib/vision/svlInterestPointDetector.h"
#include "../svl/lib/vision/svlLabeledSequence.h"
#include "../svl/lib/vision/svlLabelVisualizer.h"
#include "../svl/lib/vision/svlMultiAspectAngleSWD.h"
#include "../svl/lib/vision/svlMultiSeg.h"
#include "../svl/lib/vision/svlObject2dExtensions.h"
#include "../svl/lib/vision/svlObjectDetectionAnalyzer.h"
#include "../svl/lib/vision/svlObjectList.h"
#include "../svl/lib/vision/svlSoftEdgeMap.h"
#include "../svl/lib/vision/svlPatchDefinition.h"
#include "../svl/lib/vision/svlPatchDictionary.h"
#include "../svl/lib/vision/svlPatchFeatureSelector.h"
#include "../svl/lib/vision/svlPixelDictionary.h"
#include "../svl/lib/vision/svlPixelFeatureExtractor.h"
#include "../svl/lib/vision/svlPoint2d.h"
#include "../svl/lib/vision/svlPoint3d.h"
#include "../svl/lib/vision/svlPointCloudData.h"
#include "../svl/lib/vision/svlRegionFeatures.h"
#include "../svl/lib/vision/svlRotatableIpl.h"
#include "../svl/lib/vision/svlRotInvariantExtractor.h"
#include "../svl/lib/vision/svlSegImage.h"
#include "../svl/lib/vision/svlSlidingWindowDetector.h"
#include "../svl/lib/vision/svlSpinImages.h"
#include "../svl/lib/vision/svlSuperResolution.h"
#include "../svl/lib/vision/svlTextonExtractor.h"
#include "../svl/lib/vision/svlTextonFilterBank.h"
#include "../svl/lib/vision/svlTrainingDatasetBuilder.h"
#include "../svl/lib/vision/svlVisionUtils.h"
#include "../svl/lib/vision/svlWindowDiscarder.h"



