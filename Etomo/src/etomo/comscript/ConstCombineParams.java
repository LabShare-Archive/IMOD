package etomo.comscript;

import java.util.ArrayList;

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
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/10/20 16:51:29  rickg
 * <p> Removed scriptsCreated flag, use existence of combine com scripts instead
 * <p>
 * <p> Revision 2.4  2003/07/28 22:44:56  rickg
 * <p> Added an equals method
 * <p>
 * <p> Revision 2.3  2003/03/18 23:49:40  rickg
 * <p> Added scripts created state variable
 * <p>
 * <p> Revision 2.2  2003/03/18 16:38:18  rickg
 * <p> Added model based boolean
 * <p>
 * <p> Revision 2.1  2003/02/24 23:29:54  rickg
 * <p> Added use patch region model method
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.4.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.4  2002/10/09 17:28:37  rickg
 * <p> Fixed javadoc tag
 * <p>
 * <p> Revision 1.3  2002/10/08 23:59:22  rickg
 * <p> Added isPatchBoundarySet method
 * <p> Added basic isValid method
 * <p>
 * <p> Revision 1.2  2002/10/03 03:59:31  rickg
 * <p> Added path X,Y,Z min and max attributes
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ConstCombineParams {
  public static final String rcsid =
    "$Id$";

  protected String revisionNumber = "1.1";

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
  protected int maxPatchZMax = 0;

  protected String patchRegionModel = "";
  protected String tempDirectory = "";
  protected boolean manualCleanup = false;
  protected boolean modelBased = false;

  protected ArrayList invalidReasons = new ArrayList();

  public ConstCombineParams() {

  }

  public boolean equals(ConstCombineParams cmp) {
    if (!(matchBtoA == cmp.getMatchBtoA())) {
      return false;
    }
    if (!fiducialMatch.equals(cmp.getFiducialMatch())) {
      return false;
    }
    if (!fiducialMatchListA
      .toString()
      .equals(cmp.getFiducialMatchListA().toString())) {
      return false;
    }
    if (!fiducialMatchListB
      .toString()
      .equals(cmp.getFiducialMatchListB().toString())) {
      return false;
    }
    if (!patchSize.equals(cmp.getPatchSize())) {
      return false;
    }
    if (!(patchXMin == cmp.getPatchXMin())) {
      return false;
    }
    if (!(patchXMax == cmp.getPatchXMax())) {
      return false;
    }
    if (!(patchYMin == cmp.getPatchYMin())) {
      return false;
    }
    if (!(patchYMax == cmp.getPatchYMax())) {
      return false;
    }
    if (!(patchZMin == cmp.getPatchZMin())) {
      return false;
    }
    if (!(patchZMax == cmp.getPatchZMax())) {
      return false;
    }
    if (!(patchRegionModel.equals(cmp.getPatchRegionModel()))) {
      return false;
    }
    if (!(tempDirectory.equals(cmp.getTempDirectory()))) {
      return false;
    }
    if (!(manualCleanup == cmp.getManualCleanup())) {
      return false;
    }
    if (!(modelBased == cmp.isModelBased())) {
      return false;
    }
    return true;
  }

  /**
   * Returns true if the patch boundary values have been modified
   */
  public boolean isPatchBoundarySet() {
    if (patchXMin == 0
      && patchXMax == 0
      && patchYMin == 0
      && patchYMax == 0
      && patchZMin == 0
      && patchZMax == 0) {
      return false;
    }
    return true;
  }

  /**
   * Checks the validity of the attribute values.
   * @return true if all entries are valid, otherwise the reasons are 
   * available through the method getInvalidReasons.
   */
  public boolean isValid() {
    boolean valid = true;
    //  Clear any previous reasons from the list
    invalidReasons.clear();

    if (patchXMin < 1) {
      valid = false;
      invalidReasons.add("X min value is less than 1");
    }
    if (patchXMax < 1) {
      valid = false;
      invalidReasons.add("X max value is less than 1");
    }
    if (patchXMin > patchXMax) {
      valid = false;
      invalidReasons.add("X min value is greater than the X max value");
    }

    if (patchYMin < 1) {
      valid = false;
      invalidReasons.add("Y min value is less than 1");
    }
    if (patchYMax < 1) {
      valid = false;
      invalidReasons.add("Y max value is less than 1");
    }
    if (patchYMin > patchYMax) {
      valid = false;
      invalidReasons.add("Y min value is greater than the Y max value");
    }

    if (patchZMin < 1) {
      valid = false;
      invalidReasons.add("Z min value is less than 1");
    }
    if (patchZMax < 1) {
      valid = false;
      invalidReasons.add("ZX max value is less than 1");
    }
    if (maxPatchZMax > 0 && patchZMax > maxPatchZMax) {
      valid = false;
      invalidReasons.add(
        "Z max value is greater than the maximum Z max value ("
          + maxPatchZMax
          + ")");
    }
    if (patchZMin > patchZMax) {
      valid = false;
      invalidReasons.add("Z min value is greater than the Z max value");
    }
    return valid;
  }

  /**
   * Returns the reasons the attribute values are invalid as a string array.
   */
  public String[] getInvalidReasons() {
    return (String[]) invalidReasons.toArray(new String[invalidReasons.size()]);
  }

  public String getRevisionNumber() {
    return revisionNumber;
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
  
  public int getMaxPatchZMax() {
    return maxPatchZMax;
  }

  /**
   * Returns true if a patch region model has been specified.
   * @return boolean
   */
  public boolean usePatchRegionModel() {

    return !patchRegionModel.matches("^\\s*$");
  }

  /**
   * @return boolean
   */
  public boolean isModelBased() {
    return modelBased;
  }
}
