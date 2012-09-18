package etomo.comscript;

import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.type.ScriptParameter;

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
 * <p> Revision 3.5  2007/02/05 21:39:49  sueh
 * <p> bug# 962  Put EtomoNumber type info into an inner class.
 * <p>
 * <p> Revision 3.4  2006/09/13 23:08:56  sueh
 * <p> bug# 921 Added initialShiftXYZ.
 * <p>
 * <p> Revision 3.3  2006/08/29 20:02:59  sueh
 * <p> bug# 924 Added kernelSigma.
 * <p>
 * <p> Revision 3.2  2006/08/25 22:49:43  sueh
 * <p> bug# 918 Convert to PIP
 * <p>
 * <p> Revision 3.1  2004/03/02 21:48:40  sueh
 * <p> bug# 250 added borders, moved reset() to const
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */

public class ConstPatchcrawl3DParam {
  public static final String rcsid = "$Id$";

  protected static final String REFERENCE_FILE_KEY = "ReferenceFile";
  protected static final String FILE_TO_ALIGN_KEY = "FileToAlign";
  protected static final String OUTPUT_FILE_KEY = "OutputFile";
  protected static final String B_SOURCE_TRANSFORM_KEY = "BSourceTransform";
  protected static final String B_SOURCE_OR_SIZE_XYZ_KEY = "BSourceOrSizeXYZ";
  protected static final String REGION_MODEL_KEY = "RegionModel";
  public static final String KERNEL_SIGMA_KEY = "KernelSigma";
  public static final String INITIAL_SHIFT_XYZ_KEY = "InitialShiftXYZ";
  protected static final int X_INDEX = 0;
  protected static final int Y_INDEX = 1;
  protected static final int Z_INDEX = 2;

  protected final FortranInputString patchSizeXYZ = new FortranInputString(
      "PatchSizeXYZ", 3);
  protected final FortranInputString numberOfPatchesXYZ = new FortranInputString(
      "NumberOfPatchesXYZ", 3);
  protected final FortranInputString xMinAndMax = new FortranInputString("XMinAndMax", 2);
  protected final FortranInputString yMinAndMax = new FortranInputString("YMinAndMax", 2);
  protected final FortranInputString zMinAndMax = new FortranInputString("ZMinAndMax", 2);
  protected final FortranInputString bSourceBorderXLoHi = new FortranInputString(
      "BSourceBorderXLoHi", 2);
  protected final FortranInputString bSourceBorderYZLoHi = new FortranInputString(
      "BSourceBorderYZLoHi", 2);
  protected final FortranInputString initialShiftXYZ = new FortranInputString(
      INITIAL_SHIFT_XYZ_KEY, 3);
  protected final ScriptParameter kernelSigma = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, KERNEL_SIGMA_KEY);

  protected String referenceFile = null;
  protected String fileToAlign = null;
  protected String outputFile = null;
  protected String bSourceTransform = null;
  protected String bSourceOrSizeXYZ = null;
  protected String regionModel = null;

  public ConstPatchcrawl3DParam() {
    patchSizeXYZ.setIntegerType(true);
    numberOfPatchesXYZ.setIntegerType(true);
    xMinAndMax.setIntegerType(true);
    yMinAndMax.setIntegerType(true);
    zMinAndMax.setIntegerType(true);
    bSourceBorderXLoHi.setIntegerType(true);
    bSourceBorderYZLoHi.setIntegerType(true);
    kernelSigma.setDisplayValue(1);
    reset();
  }

  protected void reset() {
    patchSizeXYZ.reset();
    numberOfPatchesXYZ.reset();
    xMinAndMax.reset();
    yMinAndMax.reset();
    zMinAndMax.reset();
    referenceFile = null;
    fileToAlign = null;
    outputFile = null;
    bSourceTransform = null;
    bSourceOrSizeXYZ = null;
    bSourceBorderXLoHi.reset();
    regionModel = null;
    initialShiftXYZ.setDefault();//optional parameter
    kernelSigma.reset();
  }

  public boolean isUseBoundaryModel() {
    return regionModel != null;
  }

  /**
   * @return int
   */
  public int getXPatchSize() {
    return patchSizeXYZ.getInt(X_INDEX);
  }

  /**
   * @return int
   */
  public int getYPatchSize() {
    return patchSizeXYZ.getInt(Y_INDEX);
  }

  /**
   * @return int
   */
  public int getZPatchSize() {
    return patchSizeXYZ.getInt(Z_INDEX);
  }

  /**
   * @return int
   */
  public int getNX() {
    return numberOfPatchesXYZ.getInt(X_INDEX);
  }

  /**
   * @return int
   */
  public int getNY() {
    return numberOfPatchesXYZ.getInt(Y_INDEX);
  }

  /**
   * @return int
   */
  public int getNZ() {
    return numberOfPatchesXYZ.getInt(Z_INDEX);
  }

  /**
   * @return int
   */
  public int getXLow() {
    return xMinAndMax.getInt(0);
  }

  /**
   * @return int
   */
  public int getXHigh() {
    return xMinAndMax.getInt(1);
  }

  /**
   * @return int
   */
  public int getYLow() {
    return yMinAndMax.getInt(0);
  }

  /**
   * @return int
   */
  public int getYHigh() {
    return yMinAndMax.getInt(1);
  }

  /**
   * @return int
   */
  public int getZLow() {
    return zMinAndMax.getInt(0);
  }

  /**
   * @return int
   */
  public int getZHigh() {
    return zMinAndMax.getInt(1);
  }

  public String getInitialShiftX() {
    return initialShiftXYZ.toString(X_INDEX);
  }

  public String getInitialShiftY() {
    return initialShiftXYZ.toString(Y_INDEX);
  }

  public String getInitialShiftZ() {
    return initialShiftXYZ.toString(Z_INDEX);
  }

  public boolean isKernelSigmaActive() {
    return kernelSigma.isActive();
  }

  public ConstEtomoNumber getKernelSigma() {
    return kernelSigma;
  }
}
