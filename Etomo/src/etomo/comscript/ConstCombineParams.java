package etomo.comscript;

import etomo.type.CombinePatchSize;
import etomo.type.FiducialMatch;

/**
 * <p>Description: A read only model of the parameter interface for the
 * setupcombine script</p>
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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ConstCombineParams {
  public static final String rcsid = "$Id$";

  protected boolean matchBtoA = true;
  protected FiducialMatch fiducialMatch = FiducialMatch.BOTH_SIDES;
  protected StringList fiducialMatchListA = new StringList(0);
  protected StringList fiducialMatchListB = new StringList(0);
  protected CombinePatchSize patchSize = CombinePatchSize.MEDIUM;
  protected int patchXMin = 0;
  protected int patchXMax = 0;
  protected int patchYMin = 0;
  protected int patchYMax = 0;
  protected int patchZMin = 0;
  protected int patchZMax = 0;
  
  protected String patchRegionModel = "";
  protected String tempDirectory = "";
  protected boolean manualCleanup = false;

  public ConstCombineParams() {

  }

  public boolean getMatchBtoA() {
    return matchBtoA;
  }

  public FiducialMatch getFiducialMatch() {
    return fiducialMatch;
  }

  public String getFiducialMatchListA() {
    return fiducialMatchListA.toString();
  }

  public String getFiducialMatchListB() {
    return fiducialMatchListB.toString();
  }

  public CombinePatchSize getPatchSize() {
    return patchSize;
  }

  public String getPatchRegionModel() {
    return patchRegionModel;
  }

  public String getTempDirectory() {
    return tempDirectory;
  }

  public boolean getManualCleanup() {
    return manualCleanup;
  }
  /**
   * Returns the patchXMax.
   * @return int
   */
  public int getPatchXMax() {
    return patchXMax;
  }

  /**
   * Returns the patchXMin.
   * @return int
   */
  public int getPatchXMin() {
    return patchXMin;
  }

  /**
   * Returns the patchYMax.
   * @return int
   */
  public int getPatchYMax() {
    return patchYMax;
  }

  /**
   * Returns the patchYMin.
   * @return int
   */
  public int getPatchYMin() {
    return patchYMin;
  }

  /**
   * Returns the patchZMax.
   * @return int
   */
  public int getPatchZMax() {
    return patchZMax;
  }

  /**
   * Returns the patchZMin.
   * @return int
   */
  public int getPatchZMin() {
    return patchZMin;
  }

}
