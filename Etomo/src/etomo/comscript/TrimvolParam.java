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
 * <p> Revision 3.4  2004/12/08 21:22:34  sueh
 * <p> bug# 564 Implemented Command.  Provided access to swapYZ.
 * <p>
 * <p> Revision 3.3  2004/12/02 18:27:49  sueh
 * <p> bug# 557 Added a static getOutputFile(String datasetName) to put the
 * <p> responsibility of knowing how to build the trimvol output file in
 * <p> TrimvolParam.
 * <p>
 * <p> Revision 3.2  2004/06/22 01:53:33  sueh
 * <p> bug# 441 Added store(), load(), and equals().  Prevented
 * <p> setDefaultRange from overriding non-default values.  Moved
 * <p> the logic for creating the inputFile and outputFile names into
 * <p> this class
 * <p>
 * <p> Revision 3.1  2004/04/22 23:27:28  rickg
 * <p> Switched getIMODBinPath method
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.12  2003/11/06 21:28:51  sueh
 * <p> bug307 setDefaultRange(String): Set sectionScaleMin to 1/3
 * <p> y or z max and sectionScaleMax to 2/3 y or z max.
 * <p>
 * <p> Revision 1.11  2003/11/06 16:50:27  rickg
 * <p> Removed -e flag for tcsh execution for all but the com scripts
 * <p>
 * <p> Revision 1.10  2003/11/04 20:56:11  rickg
 * <p> Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 * <p>
 * <p> Revision 1.9  2003/10/20 23:25:26  rickg
 * <p> Bug# 253 Added convert to bytes checkbox
 * <p>
 * <p> Revision 1.8  2003/05/23 22:03:37  rickg
 * <p> Added -P to command string to get shell PID output
 * <p>
 * <p> Revision 1.7  2003/05/21 21:23:46  rickg
 * <p> Added e flag to tcsh execution
 * <p>
 * <p> Revision 1.6  2003/05/14 21:45:59  rickg
 * <p> Added full path to trimvol script for windows
 * <p>
 * <p> Revision 1.5  2003/04/16 22:19:30  rickg
 * <p> Initial revision
 * <p>
 * <p> Revision 1.4  2003/04/16 00:14:12  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 1.3  2003/04/14 23:56:59  rickg
 * <p> Default state of YZ swap changed to true
 * <p>
 * <p> Revision 1.2  2003/04/10 23:40:40  rickg
 * <p> In progress
 * <p>
 * <p> Revision 1.1  2003/04/09 23:36:57  rickg
 * <p> In progress
 * <p> </p>
 */
package etomo.comscript;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Properties;

import etomo.BaseManager;
import etomo.type.AxisType;
import etomo.util.MRCHeader;
import etomo.util.InvalidParameterException;


public class TrimvolParam implements Command {
  public static final String rcsid = "$Id$";

  public static final int SWAPYZ = -1;

  public static final String PARAM_ID = "Trimvol";
  public static final String XMIN = "XMin";
  public static final String XMAX = "XMax";
  public static final String YMIN = "YMin";
  public static final String YMAX = "YMax";
  public static final String ZMIN = "ZMin";
  public static final String ZMAX = "ZMax";
  public static final String CONVERT_TO_BYTES = "ConvertToBytes";
  public static final String FIXED_SCALING = "FixedScaling";
  public static final String SECTION_SCALE_MIN = "SectionScaleMin";
  public static final String SECTION_SCALE_MAX = "SectionScaleMax";
  public static final String FIXED_SCALE_MIN = "FixedScaleMin";
  public static final String FIXED_SCALE_MAX = "FixedScaleMax";
  private static final String swapYZString = "SwapYZ";
  public static final String INPUT_FILE = "InputFile";
  public static final String OUTPUT_FILE = "OutputFile";
  
  private static final int commandSize = 3;
  private static final String commandName = "trimvol";
  
  private int xMin = Integer.MIN_VALUE;
  private int xMax = Integer.MIN_VALUE;
  private int yMin = Integer.MIN_VALUE;
  private int yMax = Integer.MIN_VALUE;
  private int zMin = Integer.MIN_VALUE;
  private int zMax = Integer.MIN_VALUE;
  private boolean convertToBytes = true;
  private boolean fixedScaling = false;
  private int sectionScaleMin = Integer.MIN_VALUE;
  private int sectionScaleMax = Integer.MIN_VALUE;
  private int fixedScaleMin = Integer.MIN_VALUE;
  private int fixedScaleMax = Integer.MIN_VALUE;
  private boolean swapYZ = true;
  private String inputFile = "";
  private String outputFile = "";
  private String[] commandArray;
  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props) {
    store(props, "");
  }
  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = PARAM_ID + ".";
    }
    else {
      group = prepend + PARAM_ID + ".";
    }
    props.setProperty(group + XMIN, String.valueOf(xMin));
    props.setProperty(group + XMAX, String.valueOf(xMax));
    props.setProperty(group + YMIN, String.valueOf(yMin));
    props.setProperty(group + YMAX, String.valueOf(yMax));
    props.setProperty(group + ZMIN, String.valueOf(zMin));
    props.setProperty(group + ZMAX, String.valueOf(zMax));
    props.setProperty(group + CONVERT_TO_BYTES, String.valueOf(convertToBytes));
    props.setProperty(group + FIXED_SCALING, String.valueOf(fixedScaling));
    props.setProperty(
      group + SECTION_SCALE_MIN,
      String.valueOf(sectionScaleMin));
    props.setProperty(
      group + SECTION_SCALE_MAX,
      String.valueOf(sectionScaleMax));
    props.setProperty(group + FIXED_SCALE_MIN, String.valueOf(fixedScaleMin));
    props.setProperty(group + FIXED_SCALE_MAX, String.valueOf(fixedScaleMax));
    props.setProperty(group + swapYZString, String.valueOf(swapYZ)  );
    props.setProperty(group + INPUT_FILE, inputFile);
    props.setProperty(group + OUTPUT_FILE, outputFile);
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }
  public void load(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      group = PARAM_ID + ".";
    }
    else {
      group = prepend + PARAM_ID + ".";
    }

    // Load the trimvol values if they are present, don't change the
    // current value if the property is not present
    xMin =
      Integer
        .valueOf(props.getProperty(group + XMIN, Integer.toString(xMin)))
        .intValue();
        
    xMax =
      Integer
        .valueOf(props.getProperty(group + XMAX, Integer.toString(xMax)))
        .intValue();
        
    yMin =
      Integer
        .valueOf(props.getProperty(group + YMIN, Integer.toString(yMin)))
        .intValue();

    yMax =
      Integer
        .valueOf(props.getProperty(group + YMAX, Integer.toString(yMax)))
        .intValue();
        
    zMin =
      Integer
        .valueOf(props.getProperty(group + ZMIN, Integer.toString(zMin)))
        .intValue();

    zMax =
      Integer
        .valueOf(props.getProperty(group + ZMAX, Integer.toString(zMax)))
        .intValue();

    convertToBytes =
      Boolean
        .valueOf(
          props.getProperty(
            group + CONVERT_TO_BYTES,
            Boolean.toString(convertToBytes)))
        .booleanValue();

    fixedScaling =
      Boolean
        .valueOf(
          props.getProperty(
            group + FIXED_SCALING,
            Boolean.toString(fixedScaling)))
        .booleanValue();

    sectionScaleMin =
      Integer
        .valueOf(
          props.getProperty(
            group + SECTION_SCALE_MIN,
            Integer.toString(sectionScaleMin)))
        .intValue();

    sectionScaleMax =
      Integer
        .valueOf(
          props.getProperty(
            group + SECTION_SCALE_MAX,
            Integer.toString(sectionScaleMax)))
        .intValue();

    fixedScaleMin =
      Integer
        .valueOf(
          props.getProperty(
            group + FIXED_SCALE_MIN,
            Integer.toString(fixedScaleMin)))
        .intValue();

    fixedScaleMax =
      Integer
        .valueOf(
          props.getProperty(
            group + FIXED_SCALE_MAX,
            Integer.toString(fixedScaleMax)))
        .intValue();
        
    swapYZ =
      Boolean
        .valueOf(props.getProperty(group + swapYZString, Boolean.toString(swapYZ)))
        .booleanValue();
        
    inputFile = props.getProperty(group + INPUT_FILE, inputFile);
      
    outputFile = props.getProperty(group + OUTPUT_FILE, outputFile);
  }

  /**
   * @return
   */
  public boolean isConvertToBytes() {
    return convertToBytes;
  }

  /**
   * @param convertToBytes
   */
  public void setConvertToBytes(boolean convertToBytes) {
    this.convertToBytes = convertToBytes;
  }

  public TrimvolParam() {
  }
  
  private void createCommand() {
    ArrayList options = genOptions();
    commandArray = new String[options.size() + commandSize];
    // Do not use the -e flag for tcsh since David's scripts handle the failure 
    // of commands and then report appropriately.  The exception to this is the
    // com scripts which require the -e flag.  RJG: 2003-11-06  
    commandArray[0] = "tcsh";
    commandArray[1] = "-f";
    commandArray[2] = BaseManager.getIMODBinPath() + commandName;          
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
  }

  /**
   * Get the command string specified by the current state
   */
  public ArrayList genOptions() {
    ArrayList options = new ArrayList();
    options.add("-P");

    // TODO add error checking and throw an exception if the parameters have not
    // been set
    if (xMin >= 0 && xMax >= 0) {
      options.add("-x");
      options.add(String.valueOf(xMin) + "," + String.valueOf(xMax));
    }
    if (yMin >= 0 && yMax >= 0) {
      options.add("-y");
      options.add(String.valueOf(yMin) + "," + String.valueOf(yMax));
    }
    if (zMin >= 0 && zMax >= 0) {
      options.add("-z");
      options.add(String.valueOf(zMin) + "," + String.valueOf(zMax));
    }
    if (convertToBytes) {
      if (fixedScaling) {
        options.add("-c");
        options.add(String.valueOf(fixedScaleMin) + "," + String.valueOf(fixedScaleMax));

      }
      else {
        options.add("-s");
        options.add(String.valueOf(sectionScaleMin) + "," + String.valueOf(sectionScaleMax));
      }
    }

    if (swapYZ) {
      options.add("-yz");
    }
    // TODO check to see that filenames are apropriate
    options.add(inputFile);
    options.add(outputFile);

    return options;
  }

  /**
   * @return int
   */
  public int getFixedScaleMax() {
    return fixedScaleMax;
  }

  /**
   * @return int
   */
  public int getFixedScaleMin() {
    return fixedScaleMin;
  }

  /**
   * @return boolean
   */
  public boolean isFixedScaling() {
    return fixedScaling;
  }

  /**
   * @return int
   */
  public int getSectionScaleMax() {
    return sectionScaleMax;
  }

  /**
   * @return int
   */
  public int getSectionScaleMin() {
    return sectionScaleMin;
  }

  /**
   * @return boolean
   */
  public boolean isSwapYZ() {
    return swapYZ;
  }

  /**
   * @return int
   */
  public int getXMax() {
    return xMax;
  }

  /**
   * @return int
   */
  public int getXMin() {
    return xMin;
  }

  /**
   * @return int
   */
  public int getYMax() {
    return yMax;
  }

  /**
   * @return int
   */
  public int getYMin() {
    return yMin;
  }

  /**
   * @return int
   */
  public int getZMax() {
    return zMax;
  }

  /**
   * @return int
   */
  public int getZMin() {
    return zMin;
  }

  /**
   * Sets the fixedScaleMax.
   * @param fixedScaleMax The fixedScaleMax to set
   */
  public void setFixedScaleMax(int fixedScaleMax) {
    this.fixedScaleMax = fixedScaleMax;
  }

  /**
   * Sets the fixedScaleMin.
   * @param fixedScaleMin The fixedScaleMin to set
   */
  public void setFixedScaleMin(int fixedScaleMin) {
    this.fixedScaleMin = fixedScaleMin;
  }

  /**
   * Sets the fixedScaling.
   * @param fixedScaling The fixedScaling to set
   */
  public void setFixedScaling(boolean fixedScaling) {
    this.fixedScaling = fixedScaling;
  }

  /**
   * Sets the scaleSectionMax.
   * @param scaleSectionMax The scaleSectionMax to set
   */
  public void setSectionScaleMax(int scaleSectionMax) {
    this.sectionScaleMax = scaleSectionMax;
  }

  /**
   * Sets the scaleSectionMin.
   * @param scaleSectionMin The scaleSectionMin to set
   */
  public void setSectionScaleMin(int scaleSectionMin) {
    this.sectionScaleMin = scaleSectionMin;
  }

  /**
   * Sets the swapYZ.
   * @param swapYZ The swapYZ to set
   */
  public void setSwapYZ(boolean swapYZ) {
    this.swapYZ = swapYZ;
  }

  /**
   * Sets the xMax.
   * @param xMax The xMax to set
   */
  public void setXMax(int xMax) {
    this.xMax = xMax;
  }

  /**
   * Sets the xMin.
   * @param xMin The xMin to set
   */
  public void setXMin(int xMin) {
    this.xMin = xMin;
  }

  /**
   * Sets the yMax.
   * @param yMax The yMax to set
   */
  public void setYMax(int yMax) {
    this.yMax = yMax;
  }

  /**
   * Sets the yMin.
   * @param yMin The yMin to set
   */
  public void setYMin(int yMin) {
    this.yMin = yMin;
  }

  /**
   * Sets the zMax.
   * @param zMax The zMax to set
   */
  public void setZMax(int zMax) {
    this.zMax = zMax;
  }

  /**
   * Sets the zMin.
   * @param zMin The zMin to set
   */
  public void setZMin(int zMin) {
    this.zMin = zMin;
  }

  /**
   * Sets the range to match the full volume
   * @param fileName The MRC iamge stack file name used to set the range
   */
  public void setDefaultRange() throws InvalidParameterException, IOException {
    setDefaultRange(inputFile);
  }
  
  public void setDefaultRange(String fileName)
      throws InvalidParameterException, IOException {
    //Don't override existing values
    if (xMin != Integer.MIN_VALUE) {
      return;
    }
    // Get the data size limits from the image stack
    MRCHeader mrcHeader = new MRCHeader(fileName);
    mrcHeader.read();

    xMin = 1;
    xMax = mrcHeader.getNColumns();
    yMin = 1;
    yMax = mrcHeader.getNRows();
    zMin = 1;
    zMax = mrcHeader.getNSections();

    // Check the swapped YZ state to decide which dimension to use for the 
    // section range
    if (swapYZ) {
      sectionScaleMin = yMax / 3;
      sectionScaleMax = yMax * 2 / 3;
    }
    else {
      sectionScaleMin = zMax / 3;
      sectionScaleMax = zMax * 2 / 3;
    }
  }

  /**
   * 
   * @return
   */
  public String getInputFileName() {
    return inputFile;
  }
  /**
   * 
   * @param axisType
   * @param datasetName
   * @return
   */
  public static String getInputFileName(AxisType axisType, String datasetName) {
    if (axisType == AxisType.SINGLE_AXIS) {
      return datasetName + "_full.rec";
    }
    return "sum.rec";
  }
  /**
   * 
   * @param axisType
   * @param datasetName
   */
  public void setInputFileName(AxisType axisType, String datasetName) {
    inputFile = getInputFileName(axisType, datasetName);
  }

  /**
   * 
   * @return
   */
  public String getOutputFileName() {
    return outputFile;
  }
  
  public static String getOutputFileName(String datasetName) {
    return datasetName + ".rec";
  }
  /**
   * 
   * @param datasetName
   */
  public void setOutputFileName(String datasetName) {
    outputFile = datasetName + ".rec";
  }
  
  public int getBinning() {
    return 1;
  }
  
  public boolean getBooleanValue(int name) {
    switch (name) {
    case SWAPYZ:
      return swapYZ;
    }
    return false;
  }
  
  public String[] getCommandArray() {
    createCommand();
    return commandArray;
  }
  
  public String getCommandLine() {
    if (commandArray == null) {
      return "";
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }
  
  public String getCommandName() {
    return commandName;
  }
  
  public int getIntegerValue(int name) {
    return Integer.MIN_VALUE;
  }
  
  public int getMode() {
    return 0;
  }
  
  public File getOutputFile() {
    return new File(outputFile);
  }
  
  public static String getName() {
    return commandName;
  }
  
  /**
   * 
   * @param trim
   * @return
   */
  public boolean equals(TrimvolParam trim) {
    if (xMin != trim.getXMin()) {
      return false;
    }
    if (xMax != trim.getXMax()) {
      return false;
    }
    if (yMin != trim.getYMin()) {
      return false;
    }
    if (yMax != trim.getYMax()) {
      return false;
    }
    if (zMin != trim.getZMin()) {
      return false;
    }
    if (zMax != trim.getZMax()) {
      return false;
    }
    if (convertToBytes != trim.isConvertToBytes()) {
      return false;
    }
    if (fixedScaling != trim.isFixedScaling()) {
      return false;
    }
    if (sectionScaleMin != trim.getSectionScaleMin()) {
      return false;
    }
    if (sectionScaleMax != trim.getSectionScaleMax()) {
      return false;
    }
    if (fixedScaleMin != trim.getFixedScaleMin()) {
      return false;
    }
    if (fixedScaleMax != trim.getFixedScaleMax()) {
      return false;
    }
    if (swapYZ != trim.isSwapYZ()) {
      return false;
    }
    if (!inputFile.equals(trim.getInputFileName())
      && (inputFile.equals("\\S+") || trim.getInputFileName().equals("\\S+"))) {
      return false;
    }
    if (!outputFile.equals(trim.getOutputFile())
      && (outputFile.equals("\\S+") || trim.getOutputFile().equals("\\S+"))) {
      return false;
    }   
    return true;
  }

}