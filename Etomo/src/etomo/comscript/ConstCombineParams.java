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
 * <p> $Log$ </p>
 */

public class ConstCombineParams {
  public static final String rcsid = "$Id$";

  protected boolean matchBtoA = true;
  protected FiducialMatch fiducialMatch = FiducialMatch.BOTH_SIDES;
  protected StringList fiducialMatchListA = new StringList(0);
  protected StringList fiducialMatchListB = new StringList(0);
  protected CombinePatchSize patchSize = CombinePatchSize.MEDIUM;
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
}
