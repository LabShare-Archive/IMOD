package etomo.comscript;

import etomo.type.TiltAngleSpec;

/**
* <p>Description: Was ConstBeadtrack.  Do not modify this file except to
* deprecate functions that use member variables that are no longer in use.</p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class OldConstBeadtrackParam {
  public static  final String  rcsid =  "$Id$";
  protected static final int nondefaultGroupSize = 3;
  protected static final boolean[] nondefaultGroupIntegerType = { true, true, true };
  
  //corresponds to ImageFile
  protected String inputFile;
  //corresponds to PieceListFile
  protected String pieceListFile;
  //corresponds to InputSeedModel
  protected String seedModelFile;
  //corresponds to OutputModel
  protected String outputModelFile;
  //changed to SkipViews
  protected String viewSkipList;
  //changed to RotationAngle
  protected double imageRotation;
  //not in use
  protected int nAdditionalViewSets;
  //corresponds to SeparateGroup
  protected StringList additionalViewGroups;
  //corresponds to FirstTiltAngle,TiltIncrement,TiltFile,TiltAngles
  protected TiltAngleSpec tiltAngleSpec;
  //changed to TiltDefaultGrouping
  protected FortranInputString tiltAngleGroupParams;
  //corresponds to TiltNondefaultGroup
  protected FortranInputString[] tiltAngleGroups;
  //changed to MagDefaultGrouping
  protected FortranInputString magnificationGroupParams;
  //corresponds to MagNondefaultGroup
  protected FortranInputString[] magnificationGroups;
  //changed to MinViewsForTiltalign
  protected int nMinViews;
  //changed to CentroidRadius,LightBeads
  protected FortranInputString fiducialParams;
  //corresponds to FillGaps
  protected boolean fillGaps;
  //changed to MaxGapSize
  protected int maxGap;

  //changed to MinTiltRangeToFindAxis,MinTiltRangeToFindAngles
  protected FortranInputString tiltAngleMinRange;
  //corresponds to BoxSizeXandY
  protected FortranInputString searchBoxPixels;
  //changed to MaxBeadsToAverage
  protected int maxFiducialsAvg;
  //corresponds to PointsToFitMaxAndMin
  protected FortranInputString fiducialExtrapolationParams;
  //corresponds to DensityRescueFractionAndSD
  protected FortranInputString rescueAttemptParams;
  //changed to DistanceRescueCriterion
  protected int minRescueDistance;
  //corresponds to RescueRelaxationDensityAndDistance
  protected FortranInputString rescueRelaxationParams;
  //changed to PostFitRescueResidual
  protected double residualDistanceLimit;
  //changed to DensityRelaxationPostFit,MaxRescueDistance
  protected FortranInputString secondPassParams;
  //corresponds to ResidualsToAnalyzeMaxAndMin
  protected FortranInputString meanResidChangeLimits;
  //corresponds to DeletionCriterionMinAndSD
  protected FortranInputString deletionParams;

  public OldConstBeadtrackParam() {
    // The attributes of this class are initialized in the constructor because
    // many of them required additional calls besides construction for
    // appropriate initialization  
    additionalViewGroups = new StringList(0);

    tiltAngleSpec = new TiltAngleSpec();

    tiltAngleGroupParams = new FortranInputString(2);
    tiltAngleGroupParams.setIntegerType(0, true);
    tiltAngleGroupParams.setIntegerType(1, true);

    magnificationGroupParams = new FortranInputString(2);
    magnificationGroupParams.setIntegerType(0, true);
    magnificationGroupParams.setIntegerType(1, true);

    fiducialParams = new FortranInputString(2);
    fiducialParams.setIntegerType(1, true);
    tiltAngleMinRange = new FortranInputString(2);

    searchBoxPixels = new FortranInputString(2);
    searchBoxPixels.setIntegerType(0, true);
    searchBoxPixels.setIntegerType(1, true);

    fiducialExtrapolationParams = new FortranInputString(2);
    fiducialExtrapolationParams.setIntegerType(0, true);
    fiducialExtrapolationParams.setIntegerType(1, true);

    rescueAttemptParams = new FortranInputString(2);
    rescueAttemptParams.setIntegerType(1, true);

    rescueRelaxationParams = new FortranInputString(2);

    secondPassParams = new FortranInputString(2);

    meanResidChangeLimits = new FortranInputString(2);
    meanResidChangeLimits.setIntegerType(0, true);
    meanResidChangeLimits.setIntegerType(1, true);

    deletionParams = new FortranInputString(2);
    deletionParams.setIntegerType(0, false);
    deletionParams.setIntegerType(1, true);

  }

  /**
   * Validate the parameters stored in the BeadtrackObject
   */
  public String validate() {
    StringBuffer errors = null;
    //  Compare the number of additional view sets and the number of entries in
    //  in additionalViewGroups
    if (nAdditionalViewSets != additionalViewGroups.getNElements()) {
      errors = new StringBuffer(
          "The number of additional view groups does not equal the number specified");
      errors.append("\nnumber of additional views sets: "
          + String.valueOf(nAdditionalViewSets));
      errors.append("\nnumber of list entries: "
          + String.valueOf(additionalViewGroups.getNElements()));
    }
    if (errors == null) {
      return null;
    }
    return errors.toString();
  }

  public String getInputFile() {
    return inputFile;
  }

  public String getPieceListFile() {
    return pieceListFile;
  }

  public String getSeedModelFile() {
    return seedModelFile;
  }

  public String getOutputModelFile() {
    return outputModelFile;
  }

  /**
   * @deprecated
   * @return
   */
  public String getViewSkipList() {
    return viewSkipList;
  }

  /**
   * @deprecated
   * @return
   */
  public double getImageRotation() {
    return imageRotation;
  }

  /**
   * @deprecated
   * @return
   */
  public int getNAdditionalViewSets() {
    return nAdditionalViewSets;
  }

  public String getAdditionalViewGroups() {
    return additionalViewGroups.toString();
  }

  public TiltAngleSpec getTiltAngleSpec() {
    return new TiltAngleSpec(tiltAngleSpec);
  }

  /**
   * @deprecated
   * @return
   */
  public String getTiltAngleGroupParams() {
    return tiltAngleGroupParams.toString();
  }

  /**
   * @deprecated
   * @return
   */
  public int getTiltAngleGroupSize() {
    return tiltAngleGroupParams.getInt(0);
  }

  public String getTiltAngleGroups() {
    return ParamUtilities.valueOf(tiltAngleGroups);
  }

  /**
   * @deprecated
   * @return
   */
  public String getMagnificationGroupParams() {
    return magnificationGroupParams.toString();
  }

  /**
   * @deprecated
   * @return
   */
  public int getMagnificationGroupSize() {
    return magnificationGroupParams.getInt(0);
  }

  public String getMagnificationGroups() {
    return ParamUtilities.valueOf(magnificationGroups);
  }

  /**
   * @deprecated
   * @return
   */
  public int getNMinViews() {
    return nMinViews;
  }

  /**
   * @deprecated
   * @return
   */
  public String getFiducialParams() {
    return fiducialParams.toString();
  }

  public boolean getFillGaps() {
    return fillGaps;
  }

  /**
   * @deprecated
   * @return
   */
  public int getMaxGap() {
    return maxGap;
  }

  /**
   * @deprecated
   * @return
   */
  public String getTiltAngleMinRange() {
    return tiltAngleMinRange.toString();
  }

  public String getSearchBoxPixels() {
    return searchBoxPixels.toString();
  }

  /**
   * @deprecated
   * @return
   */
  public int getMaxFiducialsAvg() {
    return maxFiducialsAvg;
  }

  public String getFiducialExtrapolationParams() {
    return fiducialExtrapolationParams.toString();
  }

  public String getRescueAttemptParams() {
    return rescueAttemptParams.toString();
  }

  /**
   * @deprecated
   * @return
   */
  public int getMinRescueDistance() {
    return minRescueDistance;
  }

  public String getRescueRelaxationParams() {
    return rescueRelaxationParams.toString();
  }

  /**
   * @deprecated
   * @return
   */
  public double getResidualDistanceLimit() {
    return residualDistanceLimit;
  }

  /**
   * @deprecated
   * @return
   */
  public String getSecondPassParams() {
    return secondPassParams.toString();
  }

  public String getMeanResidChangeLimits() {
    return meanResidChangeLimits.toString();
  }

  public String getDeletionParams() {
    return deletionParams.toString();
  }


}
/**
* <p> $Log$
* <p> Revision 1.1  2005/05/09 23:07:41  sueh
* <p> bug# 658 This class used to be ConstBeadtrackParam before the the PIP
* <p> upgrade.  Set functions that get member variables that are not used by
* <p> the new BeadtrackParam are deprecated.  Other then this sort of change,
* <p> this class should not be changed.
* <p> </p>
*/