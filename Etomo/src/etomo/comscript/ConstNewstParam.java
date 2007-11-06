package etomo.comscript;

import java.io.File;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.20  2007/09/11 21:16:21  sueh
 * <p> bug# 1035 Added getSizeToOutputInX and Y and isSizeToOutputInXandYSet.
 * <p>
 * <p> Revision 3.19  2007/05/11 15:06:47  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 3.18  2007/02/05 21:39:17  sueh
 * <p> bug# 962 Put mode info into an inner class.
 * <p>
 * <p> Revision 3.17  2006/05/22 22:35:44  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 3.16  2006/05/11 19:39:07  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 3.15  2006/04/06 18:49:04  sueh
 * <p> bug# 808 Implementing ProcessDetails.  Added Fields to pass requests to
 * <p> the generic gets.
 * <p>
 * <p> Revision 3.14  2006/03/22 21:28:21  sueh
 * <p> bug# 803 Added DATA_MODE_OPTION and
 * <p> FLOAT_DENSITIES_OPTION.
 * <p>
 * <p> Revision 3.13  2006/03/22 18:37:02  sueh
 * <p> bug# 803 Added statics FLOAT_DENSITIES_MEAN and
 * <p> FLOAT_DENSITIES_DEFAULT, which are used with floatDensities.
 * <p>
 * <p> Revision 3.12  2006/03/22 17:52:34  sueh
 * <p> bug# 803 Added statics DATA_MODE_BYTE and
 * <p> DATA_MODE_DEFAULT, which used with modeToOutput.
 * <p>
 * <p> Revision 3.11  2006/01/20 20:45:43  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 3.10  2005/11/19 01:51:43  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 3.9  2005/09/02 18:55:55  sueh
 * <p> bug# 721 Adding magGradientFile.
 * <p>
 * <p> Revision 3.8  2005/08/25 01:47:18  sueh
 * <p> bug# 716 preventing index error in getOutputFile()
 * <p>
 * <p> Revision 3.7  2005/06/10 22:45:08  sueh
 * <p> Added GET_BINNING
 * <p>
 * <p> Revision 3.6  2005/04/25 20:38:26  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
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

public class ConstNewstParam implements CommandDetails {
  public static final String rcsid = "$Id$";
  //data mode
  public static final String DATA_MODE_OPTION = "-mo";
  public static final int DATA_MODE_DEFAULT = Integer.MIN_VALUE;
  public static final int DATA_MODE_BYTE = 0;

  //float densities
  public static final String FLOAT_DENSITIES_OPTION = "-fl";
  public static final int FLOAT_DENSITIES_DEFAULT = Integer.MIN_VALUE;
  public static final int FLOAT_DENSITIES_MEAN = 2;

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
  protected Mode commandMode;
  protected String magGradientFile;

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

  public CommandDetails getSubcommandDetails() {
    return null;
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
    for (Iterator i = offsetsInXandY.iterator(); i.hasNext();) {
      buffer.append((String) i.next());
      buffer.append(",");
    }
    // Remove the trailing comma
    if (buffer.length() > 0) {
      buffer.deleteCharAt(buffer.length() - 1);
    }
    return buffer.toString();
  }

  /**
   * Backward compatibility with pre PIP structure, just return the first ouput
   * file
   * @return Returns the inputFile.
   */
  public String getOutputFile() {
    if (outputFile.size() == 0) {
      return "";
    }
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

  public int getSizeToOutputInX() {
    return sizeToOutputInXandY.getInt(0);
  }

  public int getSizeToOutputInY() {
    return sizeToOutputInXandY.getInt(1);
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

  public boolean isSizeToOutputInXandYSet() {
    return sizeToOutputInXandY.valuesSet()
        && (!sizeToOutputInXandY.isDefault());
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
    boolean[] bothTrue = { true, true };
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
    magGradientFile = null;
  }

  public String getCommand() {
    return getCommandFileName(axisID);
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

  public CommandMode getCommandMode() {
    return commandMode;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public int getIntValue(etomo.comscript.Fields field) {
    if (field == Fields.BINNING) {
      return getBinByFactor();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(etomo.comscript.Fields field) {
    if (field == Fields.FIDUCIALESS_ALIGNMENT) {
      return fiducialessAlignment;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public float getFloatValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public static final class Fields implements etomo.comscript.Fields {
    private Fields() {
    }

    public static final Fields FIDUCIALESS_ALIGNMENT = new Fields();
    public static final Fields BINNING = new Fields();
  }

  public static final class Mode implements CommandMode {
    public static final Mode WHOLE_TOMOGRAM_SAMPLE = new Mode();
    public static final Mode FULL_ALIGNED_STACK = new Mode();
  }
}