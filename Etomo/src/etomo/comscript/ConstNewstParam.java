package etomo.comscript;

import java.io.File;
import java.util.Iterator;
import java.util.Vector;

import etomo.type.AxisID;
import etomo.type.EtomoNumber;

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
 * <p> Revision 3.5  2005/01/08 01:32:14  sueh
 * <p> bug# 578 Create 2 modes - whole tomo sample and full aligned stack.
 * <p> Implement Command.  Add fiducialessAlignment variable and make it
 * <p> available through the Command interface.
 * <p>
 * <p> Revision 3.4  2004/06/25 18:04:33  sueh
 * <p> bug# 484 returning default when binByFactor is not set.
 * <p>
 * <p> Revision 3.3  2004/02/18 00:50:32  rickg
 * <p> Check buffer length when deleting trailing comma in getOffsetsInXandY
 * <p>
 * <p> Revision 3.2  2004/02/14 00:16:12  rickg
 * <p> Updated for PIP based newstack, fixed return values where
 * <p> internal objects were returned.
 * <p>
 * <p> Revision 3.1  2004/02/13 01:04:08  rickg
 * <p> Updated for PIP based newstack
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.2  2003/10/02 18:57:47  sueh
 * <p> bug236 added testing:
 * <p> NewstParamTest
 * <p> ComScriptTest
 * <p>
 * <p> Removed marks
 * <p>
 * <p> Revision 2.1  2003/09/29 23:34:57  sueh
 * <p> bug236 Added UseLinearInterpolation to
 * <p> TomogramGenerationDialog.
 * <p>
 * <p> UseLinearInterpolation:
 * <p> check box
 * <p> Advanced
 * <p> newst -linear
 * <p>
 * <p> Files:
 * <p> ComScriptManager.java
 * <p> ConstNewstParam.java
 * <p> NewstParam.java
 * <p> TomogramGenerationDialog.java
 * <p> ApplicationManager.java
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

public class ConstNewstParam implements Command {
  public static final String rcsid = 
  "$Id$";
  
  //generic gets
  public static final int GET_FIDUCIALESS_ALIGNMENT = -1;
  
  //command modes
  public static final int WHOLE_TOMOGRAM_SAMPLE_MODE = -1;
  public static final int FULL_ALIGNED_STACK_MODE = -2;
  
  private static final String commandFileName = "newst";
  private static final String commandFileExtension = ".com";
  
  protected Vector inputFile;
  protected Vector outputFile;
  protected String fileOfInputs;
  protected String fileOfOutputs;
  protected Vector sectionsToRead;
  protected Vector numberToOutput;
  protected FortranInputString sizeToOutputInXandY;
  protected int modeToOutput;
  protected Vector offsetsInXandY;
  protected boolean applyOffsetsFirst;
  protected String transformFile;
  protected String useTransformLines;
  protected float rotateByAngle;
  protected float expandByFactor;
  protected int binByFactor;
  protected boolean linearInterpolation;
  protected int floatDensities;
  protected FortranInputString contrastBlackWhite;
  protected FortranInputString scaleMinAndMax;
  protected String distortionField;
  protected int imagesAreBinned;
  protected FortranInputString testLimits;
  protected String parameterFile;
  protected boolean fiducialessAlignment;
  protected int commandMode;
  
  private AxisID axisID;
  
  //defaults
  public static final int BIN_BY_FACTOR_DEFAULT = 1;

  public ConstNewstParam(AxisID axisID) {
    this.axisID = axisID;
    initializeEmpty();
  }
  
  public AxisID getAxisID() {
    return axisID;
  }
  /**
   * @return Returns the applyOffsetsFirst.
   */
  public boolean isApplyOffsetsFirst() {
    return applyOffsetsFirst;
  }
  /**
   * @return Returns the binByFactor.
   */
  public int getBinByFactor() {
    return ParamUtilities.get(binByFactor, BIN_BY_FACTOR_DEFAULT);
  }
  /**
   * @return Returns the contrastBlackWhite.
   */
  public String getContrastBlackWhite() {
    return contrastBlackWhite.toString();
  }
  /**
   * @return Returns the distortionField.
   */
  public String getDistortionField() {
    return distortionField;
  }
  /**
   * @return Returns the expandByFactor.
   */
  public float getExpandByFactor() {
    return expandByFactor;
  }
  /**
   * @return Returns the fileOfInputs.
   */
  public String getFileOfInputs() {
    return fileOfInputs;
  }
  /**
   * @return Returns the fileOfOutputs.
   */
  public String getFileOfOutputs() {
    return fileOfOutputs;
  }
  /**
   * @return Returns the floatDensities.
   */
  public int getFloatDensities() {
    return floatDensities;
  }
  /**
   * @return Returns the imagesAreBinned.
   */
  public int getImagesAreBinned() {
    return imagesAreBinned;
  }
  
  /**
   * Backward compatibility with pre PIP structure, just return the first input
   * file
   * @return Returns the inputFile.
   */
  public String getInputFile() {
    return (String) inputFile.get(0);
  }
  
  /**
   * Create a defensive copy of the internal object inputFile
   * @return
   */
  public Vector getInputFiles() {
    Vector copy = new Vector(inputFile);
    return copy;
  }
  
  /**
   * @return Returns the linearInterpolation.
   */
  public boolean isLinearInterpolation() {
    return linearInterpolation;
  }
  /**
   * @return Returns the modeToOutput.
   */
  public int getModeToOutput() {
    return modeToOutput;
  }
  
  /**
   * @return Returns the numberToOutput.
   */
  public Vector getNumberToOutput() {
    return numberToOutput;
  }
  
  /**
   * @return Returns the offsetsInXandY.
   */
  public String getOffsetsInXandY() {
    StringBuffer buffer = new StringBuffer();
    for(Iterator i = offsetsInXandY.iterator(); i.hasNext();) {
      buffer.append((String) i.next());
      buffer.append(",");
    }
    // Remove the trailing comma
    if(buffer.length() > 0) {
      buffer.deleteCharAt(buffer.length()-1);
    }
    return buffer.toString();
  }
  
  /**
   * Backward compatibility with pre PIP structure, just return the first ouput
   * file
   * @return Returns the inputFile.
   */
  public String getOutputFile() {
    return (String) outputFile.get(0);
  }
  
  /**
   * Create a defensive copy of the internal object inputFile
   * @return
   */
  public Vector getOutputFiles() {
    Vector copy = new Vector(outputFile);
    return copy;
  }
  
  /**
   * @return Returns the parameterFile.
   */
  public String getParameterFile() {
    return parameterFile;
  }
  /**
   * @return Returns the rotateByAngle.
   */
  public float getRotateByAngle() {
    return rotateByAngle;
  }
  /**
   * @return Returns the scaleMinAndMax.
   */
  public FortranInputString getScaleMinAndMax() {
    return scaleMinAndMax;
  }
  /**
   * @return Returns the sectionsToRead.
   */
  public Vector getSectionsToRead() {
    Vector copy = new Vector(sectionsToRead);
    return copy;
  }
 
  /**
   * @return Returns the sizeToOutputInXandY.
   */
  public String getSizeToOutputInXandY() {
    return sizeToOutputInXandY.toString();
  }
  /**
   * @return Returns the testLimits.
   */
  public String getTestLimits() {
    return testLimits.toString();
  }
  /**
   * @return Returns the transformFile.
   */
  public String getTransformFile() {
    return transformFile;
  }
  /**
   * @return Returns the useTransformLines.
   */
  public String getUseTransformLines() {
    return useTransformLines;
  }
  
  /**
   * Initialize all of the attributes of this class to their empty
   * (unspecified) values.
   */
  protected void initializeEmpty() {
    inputFile = new Vector();
    outputFile = new Vector();
    fileOfInputs = "";
    fileOfOutputs = "";
    sectionsToRead = new Vector();
    numberToOutput = new Vector();
    sizeToOutputInXandY = new FortranInputString(2);
    boolean[] bothTrue = {true, true};
    sizeToOutputInXandY.setIntegerType(bothTrue);
    modeToOutput = Integer.MIN_VALUE;
    offsetsInXandY = new Vector();
    applyOffsetsFirst = false;
    transformFile = "";
    useTransformLines = "";
    rotateByAngle = Float.NaN;
    expandByFactor = Float.NaN;
    binByFactor = Integer.MIN_VALUE;
    linearInterpolation = false;
    floatDensities = Integer.MIN_VALUE;
    contrastBlackWhite = new FortranInputString(2);
    contrastBlackWhite.setIntegerType(bothTrue);
    scaleMinAndMax = new FortranInputString(2);
    distortionField = "";
    imagesAreBinned = Integer.MIN_VALUE;
    testLimits = new FortranInputString(2);
    testLimits.setIntegerType(bothTrue);
    parameterFile = "";
    fiducialessAlignment = false;
  }
  
  public static String getCommandFileName(AxisID axisID) {
    return commandFileName + axisID.getExtension() + commandFileExtension;
  }
  
  public String getCommandLine() {
    return getCommandFileName(axisID);
  }
  
  public String getCommandName() {
    return commandFileName;
  }
  public String[] getCommandArray() {
    String[] array = { getCommandLine() };
    return array;
  }
  public int getCommandMode() {
    return commandMode;
  }
  public File getCommandOutputFile() {
    return null;
  }

  public int getIntegerValue(int name) {
    return EtomoNumber.INTEGER_NULL_VALUE;
  }
  public boolean getBooleanValue(int name) {
    switch (name) {
    case GET_FIDUCIALESS_ALIGNMENT:
      return fiducialessAlignment;
    }
    return false;
  }
}
