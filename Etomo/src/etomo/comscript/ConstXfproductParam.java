/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */

package etomo.comscript;

public class ConstXfproductParam {
  public static final String rcsid = "$Id$";

  protected String inputFile1 = "";
  protected String inputFile2 = "";
  protected String outputFile = "";
  protected FortranInputString scaleShifts = new FortranInputString(2);

  /**
   * @return Returns the inputFile1.
   */
  public String getInputFile1() {
    return inputFile1;
  }

  /**
   * @return Returns the inputFile2.
   */
  public String getInputFile2() {
    return inputFile2;
  }

  /**
   * @return Returns the outputFile.
   */
  public String getOutputFile() {
    return outputFile;
  }

  /**
   * @return Returns the scaleShifts.
   */
  public String getScaleShifts() {
    return scaleShifts.toString();
  }
}
