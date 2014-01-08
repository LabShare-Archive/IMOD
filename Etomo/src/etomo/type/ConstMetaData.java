package etomo.type;

import etomo.comscript.CombineParams;
import etomo.comscript.FortranInputString;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.65  2011/05/31 21:06:13  sueh
 * <p> Bug# 1460 Added finalStackExpandCircleIterationsA and B.
 * <p>
 * <p> Revision 3.64  2011/05/03 02:49:47  sueh
 * <p> bug# 1416 Removed isGenResumeEnabled because the resume radio button state should be set based on
 * <p> the checkpoint value, not saved and reloaded.
 * <p>
 * <p> Revision 3.63  2011/04/04 17:02:04  sueh
 * <p> bug# 1416 Added getGenRadialRadius, getGenRadialSigma, getGenSubareaSize, getGenYOffsetOfSubarea,
 * <p> isGenBackProjection, isGenResumeEnabled, isGenSubarea.
 * <p>
 * <p> Revision 3.60  2010/12/05 04:43:50  sueh
 * <p> bug# 1416 Added getGenCurTab.
 * <p>
 * <p> Revision 3.59  2010/05/27 16:49:34  sueh
 * <p> bug# 1378 Added isLambdaForSmoothingListEmpty.
 * <p>
 * <p> Revision 3.58  2010/03/27 04:48:59  sueh
 * <p> bug# 1333 Added defaultGpuProcessing.  Added a separate tiltParallel for
 * <p> 3dfindbeads.
 * <p>
 * <p> Revision 3.57  2010/03/05 04:00:24  sueh
 * <p> bug# 1319 Added genExists, genLog, and gen scale parameters.
 * <p>
 * <p> Revision 3.56  2010/03/03 04:56:49  sueh
 * <p> bug# 1311 Added functions for patch tracking.
 * <p>
 * <p> Revision 3.55  2009/12/19 01:09:54  sueh
 * <p> bug# 1294 Added lambdaForSmoothing and lambdaForSmoothingList.
 * <p>
 * <p> Revision 3.54  2009/10/16 21:12:49  sueh
 * <p> bug# 1230 Added getPostCurTab.
 * <p>
 * <p> Revision 3.53  2009/09/01 02:38:06  sueh
 * <p> bug# 1222 Added 3d find binning.
 * <p>
 * <p> Revision 3.52  2009/06/05 02:03:13  sueh
 * <p> bug# 1219 Added getPostFlattenWarpSpacingInX,
 * <p> getPostFlattenWarpSpacingInY, isPostFlattenWarpContoursOnOneSurface,
 * <p> isPostFlattenWarpInputTrimVol, and isPostSqueezeVolInputTrimVol.
 * <p>
 * <p> Revision 3.51  2009/05/02 01:25:05  sueh
 * <p> bug# 1216 Removed B axis raptor data.
 * <p>
 * <p> Revision 3.50  2009/05/02 01:09:37  sueh
 * <p> bug# 1216 Added getTrackRaptorDiam, getTrackRaptorMark,
 * <p> getTrackRaptorUseRawStack, and GetTrackUseRaptor.
 * <p>
 * <p> Revision 3.49  2009/02/05 23:43:46  sueh
 * <p> bug# 1148 Added getTomoGenTrialTomogramNameList.
 * <p>
 * <p> Revision 3.48  2008/12/02 21:20:03  sueh
 * <p> bug# 1157 Added finalStackFiducialDiameterA and B.
 * <p>
 * <p> Revision 3.47  2008/11/20 01:35:04  sueh
 * <p> bug# 1147, bug# 1149 Added getFinalStackBetterRadius,
 * <p> getFinalStackPolynomialOrder, and isFinalStackBetterRadiusEmpty.
 * <p>
 * <p> Revision 3.46  2008/11/11 23:48:15  sueh
 * <p> bug# 1149 Changed tomoGenBinning to finalStackBinning.  Fixed the
 * <p> names of the CtfCorrectionParallel variables (should start with
 * <p> "finalStack", not "stack").
 * <p>
 * <p> Revision 3.45  2008/10/27 18:36:40  sueh
 * <p> bug# 1141 Added getStackCtfCorrectionParallel.
 * <p>
 * <p> Revision 3.44  2007/12/10 22:35:02  sueh
 * <p> bug# 1041 Made class an interface so inheritance can come from BaseMetaData.
 * <p>
 * <p> Revision 3.43  2007/08/16 16:55:50  sueh
 * <p> bug# 1035 Setting sizeToOutputInXandYA and B to NaN so it won't output
 * <p> bad values when empty.
 * <p>
 * <p> Revision 3.42  2007/08/16 16:33:32  sueh
 * <p> bug# 1035 Added sizeToOutputInXandYA and B.
 * <p>
 * <p> Revision 3.41  2007/03/21 19:43:08  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 3.40  2007/03/07 21:09:04  sueh
 * <p> bug# 981 Added noBeamTiltSelected, fixedBeamTiltSelected, and fixedBeamTilt.
 * <p>
 * <p> Revision 3.39  2007/03/03 01:00:46  sueh
 * <p> bug# 973 Added targetPatchSizeXandY and numberOfLocalPatchesXandY.
 * <p>
 * <p> Revision 3.38  2007/02/05 23:24:53  sueh
 * <p> bug# 962 Added Model and Rejoin fields.
 * <p>
 * <p> Revision 3.37  2006/09/19 22:32:39  sueh
 * <p> bug# 920 Added first and second axisPrepends for storing axis-level values.
 * <p> Added TiltParam.Storables for A and B.
 * <p>
 * <p> Revision 3.36  2006/05/11 19:57:02  sueh
 * <p> bug# 838 Added sample thickness.
 * <p>
 * <p> Revision 3.35  2006/03/23 19:45:54  sueh
 * <p> bug# 609 Improving the error message when a dual axis image stack does
 * <p> not end in a.st.
 * <p>
 * <p> Revision 3.34  2006/03/16 01:53:41  sueh
 * <p> bug# 828 Added getCombineParams().
 * <p>
 * <p> Revision 3.33  2005/12/13 02:16:25  sueh
 * <p> bug# 773 Added defaultParallel
 * <p>
 * <p> Revision 3.32  2005/10/27 00:32:22  sueh
 * <p> bug# 725 Added bStackProcessed.
 * <p>
 * <p> Revision 3.31  2005/10/12 21:24:16  sueh
 * <p> bug# 532 Changed the parallel booleans to EtomoBoolean2 so that they
 * <p> can remember whether they where set or not.  The default for the parallel
 * <p> checkboxes is based on the existance and validity of cpu.adoc.
 * <p>
 * <p> Revision 3.30  2005/09/29 18:45:29  sueh
 * <p> bug# 532 Saving the state of the parallel checkbox states.
 * <p>
 * <p> Revision 3.29  2005/09/16 17:50:23  sueh
 * <p> bug# 532 Added combineParallelProcess.
 * <p>
 * <p> Revision 3.28  2005/09/02 18:58:55  sueh
 * <p> bug# 720 Pass the manager to TrimvolParam instead of propertyUserDir
 * <p> because TrimvolParam is constructed by MetaData before
 * <p> propertyUserDir is set.
 * <p>
 * <p> Revision 3.27  2005/08/22 17:10:27  sueh
 * <p> bug# 532 Added a member variable to save the setting of the gen tomo
 * <p> parallel process checkbox.
 * <p>
 * <p> Revision 3.26  2005/07/29 00:53:15  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.25  2005/06/10 23:24:38  sueh
 * <p> bug# 583, bug# 677, bug# 584, bug# 679, bug# 683  Storing
 * <p> screen binning for Tomo Pos and Tomo Gen in MetaData separately.
 * <p> Added member variables:  tomoGenBinningA, tomoGenBinningB,
 * <p> tomoPosBinningA, tomoPosBinningB.
 * <p>
 * <p> Revision 3.24  2005/04/07 21:58:02  sueh
 * <p> bug# 626 Added isDistortionCorrection(), which returns true if magGradient
 * <p> or image distortion files are set.
 * <p>
 * <p> Revision 3.23  2005/03/02 20:25:03  sueh
 * <p> bug# 533 Added adjustedFocus.  Only used with montaging and mag
 * <p> gradient correction.
 * <p>
 * <p> Revision 3.22  2005/03/02 00:12:01  sueh
 * <p> bug# 611 Added mag gradients correction file.
 * <p>
 * <p> Revision 3.21  2005/02/19 00:09:01  sueh
 * <p> bug# 606 Removed MetaData (Setup) zfactors, fiducialess, wholetomogram,
 * <p> and localalignments.  Add them for A and B.
 * <p>
 * <p> Revision 3.20  2005/02/15 21:05:30  sueh
 * <p> bug# 603 Removed SectionType (single or serial sections).
 * <p>
 * <p> Revision 3.19  2005/01/25 22:07:48  sueh
 * <p> Converting useZFactors to EtomoBoolean2.
 * <p>
 * <p> Revision 3.18  2005/01/21 23:06:48  sueh
 * <p> bug# 509 bug# 591  Removed transferfidNumberViews.  Added
 * <p> transferfidParamA and transferfidParamB to hold the user-modifiable
 * <p> transferfid values.  Removed initializeTransferfidParam() and added
 * <p> getTransferfidParamAFields() and getTransferfidParamBFields() to get/set
 * <p> storable fields in TransferfidParam.
 * <p>
 * <p> Revision 3.17  2005/01/12 00:42:45  sueh
 * <p> bug# 579 Make the reset value on useZFactors true.
 * <p>
 * <p> Revision 3.16  2005/01/11 18:07:27  sueh
 * <p> bug# 578 Added useZFactors.
 * <p>
 * <p> Revision 3.15  2004/12/14 21:45:05  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  All state variables saved after a process is run belong in
 * <p> the state object.
 * <p>
 * <p> Revision 3.14  2004/12/08 21:30:21  sueh
 * <p> bug# 564 Added access to TomogramState member variable.
 * <p>
 * <p> Revision 3.13  2004/12/07 22:47:38  sueh
 * <p> bug# 564 Added TomogramState member variable.
 * <p>
 * <p> Revision 3.12  2004/12/02 18:29:16  sueh
 * <p> bug# 557 Added a SqueezevolParam instance to be stored in the .edf file.
 * <p>
 * <p> Revision 3.11  2004/11/19 23:33:52  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.10.4.4  2004/11/19 00:15:59  sueh
 * <p> bug# 520 Changed the file extension to contain the period.
 * <p>
 * <p> Revision 3.10.4.3  2004/10/11 02:07:25  sueh
 * <p> bug# 520 Fixed a bug in ConstMetaData where the open edf file menu
 * <p> item wasn't working because it was validating the propertyUserDir of the
 * <p> current manager, not the parent of the edf file being opened.  Now able
 * <p> to pass in the edf file to get the parent from to use in validation.
 * <p>
 * <p> Revision 3.10.4.2  2004/10/01 19:47:26  sueh
 * <p> bug# 520 provide a standard way to get the identifier of a meta data file
 * <p> (getName).  Define a new join string that will go in the menu.  Set a file
 * <p> extension value.
 * <p>
 * <p> Revision 3.10.4.1  2004/09/29 19:23:26  sueh
 * <p> bug# 520 Added base class BaseMetaData.  Made
 * <p> latestRevisionNumber static.  Moved revision functionality to base class.
 * <p> Moved axisType and invalid reason to base class.  Moved store()
 * <p> functions to this class.  Implemented Storable with abstract load
 * <p> functions.
 * <p>
 * <p> Revision 3.10  2004/06/22 02:01:52  sueh
 * <p> bug# 441 added TrimvolParam, updated equals().
 * <p>
 * <p> Revision 3.9  2004/06/01 18:54:49  rickg
 * <p> Bug #391 whole tomogram sampling state implementation
 * <p>
 * <p> Revision 3.8  2004/05/25 23:57:49  sueh
 * <p> bug# 355 Change isValid() so it can be used with setup dialog
 * <p> or a .edf file.  Tell the user to check the .edf file, when necessary.
 * <p>
 * <p> Revision 3.7  2004/05/25 23:23:40  rickg
 * <p> Bug #391 method refactor
 * <p>
 * <p> Revision 3.6  2004/04/06 03:00:40  rickg
 * <p> Updated imageRotation to store axis separately
 * <p>
 * <p> Revision 3.5  2004/02/24 18:52:22  sueh
 * <p> bug# 385 initialized binning to null
 * <p>
 * <p> Revision 3.4  2004/02/20 23:44:45  sueh
 * <p> bug# 386 added distortionFile and binning
 * <p>
 * <p> Revision 3.3  2004/02/07 03:10:10  sueh
 * <p> bug# 169 Created dataset validation function that returns
 * <p> the valid directory
 * <p>
 * <p> Revision 3.2  2003/12/23 21:53:16  sueh
 * <p> bug# 371 Remove validation test for b stack.
 * <p>
 * <p> Revision 3.1  2003/12/08 22:31:16  sueh
 * <p> bug# 169 adding a new function isDatasetNameValid.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.11  2003/10/28 02:23:59  sueh
 * <p> bug267
 * <p>
 * <p> Revision 2.10  2003/10/23 23:04:59  sueh
 * <p> bug267 removing prints
 * <p>
 * <p> Revision 2.9  2003/10/23 22:34:35  sueh
 * <p> bug267 removing prints
 * <p>
 * <p> Revision 2.8  2003/10/23 19:11:16  sueh
 * <p> bug267 look for stack in the working directory and the
 * <p> backup directory when validating directories.
 * <p>
 * <p> Revision 2.7  2003/10/06 22:35:18  sueh
 * <p> transferfidNumberViews needs a default because there is
 * <p> no conscript
 * <p>
 * <p> Revision 2.6  2003/09/26 19:46:16  sueh
 * <p> bug223 removed task marks
 * <p>
 * <p> Revision 2.5  2003/09/26 19:43:48  sueh
 * <p> bug223 no field should be persistant.  Changed MetaData.
 * <p> Added TransferfidNumberViews.
 * <p> Changed the done fine allignment and open fine allignment functions
 * <p> to work with MetaData
 * <p>
 * <p> Revision 2.4  2003/05/12 01:24:24  rickg
 * <p> Return invalid working directory in reason
 * <p>
 * <p> Revision 2.3  2003/05/07 17:54:08  rickg
 * <p> Working direcotry is no longer stored in the metadata
 * <p> System property user.dir now defines the working directory
 * <p>
 * <p> Revision 2.2  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.1  2003/03/18 23:47:20  rickg
 * <p> Changed method name to get CombineParams reference
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2002/10/08 23:53:42  rickg
 * <p> getCombineParams now returns a ConstCombineParam object
 * <p>
 * <p> Revision 1.2  2002/09/30 23:48:32  rickg
 * <p> Reformatted after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public interface ConstMetaData {
  public static final String rcsid = "$Id$";

  public ConstEtomoNumber getPostCurTab();

  public ConstEtomoNumber getGenCurTab();

  public AxisType getAxisType();

  public String getDatasetName();

  public boolean getComScriptCreated();

  public ConstEtomoNumber getAdjustedFocusA();

  public ConstEtomoNumber getAdjustedFocusB();

  public ConstEtomoNumber getImageRotation(AxisID axisID);

  public String getBackupDirectory();

  public int getBinning();

  public ViewType getViewType();

  public double getPixelSize();

  public DataSource getDataSource();

  public double getFiducialDiameter();

  public TiltAngleSpec getTiltAngleSpecA();

  public String getExcludeProjectionsA();

  public TiltAngleSpec getTiltAngleSpecB();

  public String getDistortionFile();

  public String getMagGradientFile();

  public String getExcludeProjectionsB();

  public CombineParams getCombineParams();

  public ConstEtomoNumber getCombineVolcombineParallel();

  public boolean isDefaultParallel();

  public boolean isDefaultGpuProcessing();

  public String getFirstAxisPrepend();

  public String getSecondAxisPrepend();

  public String getTargetPatchSizeXandY();

  public ConstEtomoNumber getFixedBeamTiltSelected(AxisID axisID);

  public String getNumberOfLocalPatchesXandY();

  public ConstEtomoNumber getFixedBeamTilt(AxisID axisID);

  public ConstEtomoNumber getNoBeamTiltSelected(AxisID axisID);

  public EtomoNumber getSampleThickness(AxisID axisID);

  public FortranInputString getSizeToOutputInXandY(AxisID axisID);

  public int getPosBinning(AxisID axisID);

  public int getStackBinning(AxisID axisID);

  public boolean isStack3dFindBinningSet(AxisID axisID);

  public int getStack3dFindBinning(AxisID axisID);

  public ConstEtomoNumber getTiltParallel(AxisID axisID, PanelId panelId);

  public ConstEtomoNumber getFinalStackCtfCorrectionParallel(AxisID axisID);

  public boolean isDistortionCorrection();

  public boolean isFinalStackBetterRadiusEmpty(AxisID axisID);

  public String getFinalStackBetterRadius(AxisID axisID);

  public boolean isFinalStackFiducialDiameterNull(AxisID axisID);

  public String getFinalStackFiducialDiameter(AxisID axisID);

  public int getFinalStackExpandCircleIterations(AxisID axisID);

  public boolean isFinalStackExpandCircleIterationsSet(final AxisID axisID);

  public int getFinalStackPolynomialOrder(AxisID axisID);

  public IntKeyList getTomoGenTrialTomogramNameList(AxisID axisID);

  public boolean getTrackRaptorUseRawStack();

  public String getTrackRaptorMark();

  public ConstEtomoNumber getTrackRaptorDiam();

  public boolean getEraseGoldModelUseFid(final AxisID axisID);

  public boolean isPostFlattenWarpInputTrimVol();

  public boolean isPostFlattenWarpContoursOnOneSurface();

  public String getPostFlattenWarpSpacingInX();

  public String getPostFlattenWarpSpacingInY();

  public boolean isPostSqueezeVolInputTrimVol();

  public boolean isPostTrimvolConvertToBytes();

  public boolean isPostTrimvolFixedScaling();

  public boolean isPostTrimvolRotateX();

  public boolean isFiducialessAlignment(AxisID axisID);

  public String getLambdaForSmoothing();

  public String getLambdaForSmoothingList();

  public boolean isLambdaForSmoothingListEmpty();

  public String getTrackOverlapOfPatchesXandY(AxisID axisID);

  public String getTrackNumberOfPatchesXandY(AxisID axisID);

  public String getTrackLengthAndOverlap(AxisID axisID);

  public boolean isTrackOverlapOfPatchesXandYSet(AxisID axisID);

  public boolean isTrackLengthAndOverlapSet(AxisID axisID);

  public String getTrackMethod(AxisID axisID);

  public String getGenLog(AxisID axisID);

  public String getGenScaleFactorLog(AxisID axisID);

  public String getGenScaleOffsetLog(AxisID axisID);

  public String getGenScaleFactorLinear(AxisID axisID);

  public String getGenScaleOffsetLinear(AxisID axisID);

  public boolean isGenScaleFactorLinearSet(AxisID axisID);

  public boolean isGenScaleOffsetLinearSet(AxisID axisID);

  public boolean isGenBackProjection(AxisID axisID);

  public String getGenSubareaSize(AxisID axisID);

  public String getGenYOffsetOfSubarea(AxisID axisID);

  public boolean isGenSubarea(AxisID axisID);

  public String getGenRadialRadius(AxisID axisID);

  public String getGenRadialSigma(AxisID axisID);

  public boolean isUseFinalStackExpandCircleIterations(AxisID axisID);

  public boolean isPostTrimvolSwapYZ();

  public String getPostTrimvolFixedScaleMax();

  public String getPostTrimvolFixedScaleMin();

  public String getPostTrimvolScaleXMax();

  public String getPostTrimvolScaleXMin();

  public String getPostTrimvolScaleYMin();

  public String getPostTrimvolScaleYMax();

  public String getPostTrimvolSectionScaleMax();

  public String getPostTrimvolSectionScaleMin();

  public String getPostTrimvolXMax();

  public String getPostTrimvolXMin();

  public String getPostTrimvolYMin();

  public String getPostTrimvolYMax();

  public String getPostTrimvolZMin();

  public String getPostTrimvolZMax();

  public boolean isEraseBeadsInitialized();

  public boolean isTrackSeedModelManual(AxisID axisID);

  public boolean isTrackSeedModelAuto(AxisID axisID);

  public boolean isTrackSeedModelTransfer(AxisID axisID);

  public boolean isTrackExcludeInsideAreas(AxisID axisID);

  public String getTrackTargetNumberOfBeads(AxisID axisID);

  public String getTrackTargetDensityOfBeads(AxisID axisID);

  public boolean isTrackClusteredPointsAllowedElongated(AxisID axisID);

  public int getTrackClusteredPointsAllowedElongatedValue(AxisID axisID);

  public boolean isTrackAdvanced(AxisID axisID);

  public boolean isStack3dFindThicknessSet(AxisID axisID);

  public String getStack3dFindThickness(AxisID axisID);

  public boolean isSetFEIPixelSize();

  public boolean isTwodir(AxisID axisID);

  public String getTwodir(AxisID axisID);

  public int getRaptorTab(AxisID axisID);

  public int getSeedAndTrackTab(AxisID axisID);

  public ConstEtomoNumber getAntialiasFilter(DialogType dialogType, AxisID axisID);

  public boolean isAntialiasFilterNull(DialogType dialogType, AxisID axisID);

  public boolean isTrackElongatedPointsAllowedNull(AxisID axisID);

  public ConstEtomoNumber getTrackElongatedPointsAllowed(AxisID axisID);

  public boolean getWeightWholeTracks(AxisID axisID);

  public String getLengthOfPieces(AxisID axisID);

  public String getMinimumOverlap(AxisID axisID);
}
