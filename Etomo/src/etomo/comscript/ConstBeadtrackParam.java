package etomo.comscript;

import java.util.ArrayList;

import etomo.type.TiltAngleSpec;

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
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ConstBeadtrackParam {
  public static final String rcsid =
    "$Id$";

  protected static final int nondefaultGroupSize = 3;
  protected static final boolean[] nondefaultGroupIntegerType = { true, true, true };
  
  protected String inputFile;
  protected String pieceListFile;
  protected String seedModelFile;
  protected String outputModelFile;
  protected String viewSkipList;
  protected double imageRotation;
  protected int nAdditionalViewSets;
  protected StringList additionalViewGroups;
  protected TiltAngleSpec tiltAngleSpec;
  protected FortranInputString tiltAngleGroupParams;
  protected FortranInputString[] tiltAngleGroups;
  protected FortranInputString magnificationGroupParams;
  protected FortranInputString[] magnificationGroups;
  protected int nMinViews;
  protected FortranInputString fiducialParams;
  protected boolean fillGaps;
  protected int maxGap;

  protected FortranInputString tiltAngleMinRange;
  protected FortranInputString searchBoxPixels;
  protected int maxFiducialsAvg;
  protected FortranInputString fiducialExtrapolationParams;
  protected FortranInputString rescueAttemptParams;
  protected int minRescueDistance;
  protected FortranInputString rescueRelaxationParams;
  protected double residualDistanceLimit;
  protected FortranInputString secondPassParams;
  protected FortranInputString meanResidChangeLimits;
  protected FortranInputString deletionParams;

  public ConstBeadtrackParam() {
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
  public boolean isValid() {
    ArrayList errors = new ArrayList();
    //  Compare the number of additional view sets and the number of entries in
    //  in additionalViewGroups
    if (nAdditionalViewSets != additionalViewGroups.getNElements()) {
      errors.add(
        "The number of additional view groups does not equal the number specified");
      errors.add(
        "  number of additional views sets: "
          + String.valueOf(nAdditionalViewSets));
      errors.add(
        "  number of list entries: "
          + String.valueOf(additionalViewGroups.getNElements()));

    }
    return true;
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

  public String getViewSkipList() {
    return viewSkipList;
  }

  public double getImageRotation() {
    return imageRotation;
  }

  public int getNAdditionalViewSets() {
    return nAdditionalViewSets;
  }

  public String getAdditionalViewGroups() {
    return additionalViewGroups.toString();
  }

  public TiltAngleSpec getTiltAngleSpec() {
    return new TiltAngleSpec(tiltAngleSpec);
  };

  public String getTiltAngleGroupParams() {
    return tiltAngleGroupParams.toString();
  }

  public int getTiltAngleGroupSize() {
    return tiltAngleGroupParams.getInt(0);
  }

  public String getTiltAngleGroups() {
    return ParamUtilities.valueOf(tiltAngleGroups);
  }

  public String getMagnificationGroupParams() {
    return magnificationGroupParams.toString();
  }

  public int getMagnificationGroupSize() {
    return magnificationGroupParams.getInt(0);
  }

  public String getMagnificationGroups() {
    return ParamUtilities.valueOf(magnificationGroups);
  }

  public int getNMinViews() {
    return nMinViews;
  }

  public String getFiducialParams() {
    return fiducialParams.toString();
  }

  public boolean getFillGaps() {
    return fillGaps;
  }

  public int getMaxGap() {
    return maxGap;
  }

  public String getTiltAngleMinRange() {
    return tiltAngleMinRange.toString();
  }

  public String getSearchBoxPixels() {
    return searchBoxPixels.toString();
  }

  public int getMaxFiducialsAvg() {
    return maxFiducialsAvg;
  }

  public String getFiducialExtrapolationParams() {
    return fiducialExtrapolationParams.toString();
  }

  public String getRescueAttemptParams() {
    return rescueAttemptParams.toString();
  }

  public int getMinRescueDistance() {
    return minRescueDistance;
  }

  public String getRescueRelaxationParams() {
    return rescueRelaxationParams.toString();
  }

  public double getResidualDistanceLimit() {
    return residualDistanceLimit;
  }

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
