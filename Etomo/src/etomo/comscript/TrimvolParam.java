package etomo.comscript;

import java.io.IOException;

import etomo.util.MRCHeader;
import etomo.util.InvalidParameterException;

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
public class TrimvolParam {
  public static final String rcsid =
    "$Id$";

  private int xMin = -1;
  private int xMax = -1;
  private int yMin = -1;
  private int yMax = -1;
  private int zMin = -1;
  private int zMax = -1;
  private boolean fixedScaling = false;
  private int sectionScaleMin = -1;
  private int sectionScaleMax = -1;
  private int fixedScaleMin = -1;
  private int fixedScaleMax = -1;
  private boolean swapYZ = true;
  private String inputFile = "";
  private String outputFile = "";

  public TrimvolParam() {
  }

  /**
   * Get the command string specified by the current state
   */
  public String getCommandString() {
    StringBuffer commandLine = new StringBuffer("trimvol");

    // TODO add error checking and throw an exception if the parameters have not
    // been set
    if (xMin >= 0 && xMax >= 0) {
      commandLine.append(" -x ");
      commandLine.append(String.valueOf(xMin));
      commandLine.append(",");
      commandLine.append(String.valueOf(xMax));
    }
    if (yMin >= 0 && yMax >= 0) {
      commandLine.append(" -y ");
      commandLine.append(String.valueOf(yMin));
      commandLine.append(",");
      commandLine.append(String.valueOf(yMax));
    }
    if (zMin >= 0 && zMax >= 0) {
      commandLine.append(" -z ");
      commandLine.append(String.valueOf(zMin));
      commandLine.append(",");
      commandLine.append(String.valueOf(zMax));
    }
    if (fixedScaling) {
      commandLine.append(" -c ");
      commandLine.append(String.valueOf(fixedScaleMin));
      commandLine.append(",");
      commandLine.append(String.valueOf(fixedScaleMax));

    }
    else {
      commandLine.append(" -s ");
      commandLine.append(String.valueOf(sectionScaleMin));
      commandLine.append(",");
      commandLine.append(String.valueOf(sectionScaleMax));
    }

    if(swapYZ) {
      commandLine.append(" -yz ");
    }
    // TODO check to see that filenames are apropriate
    commandLine.append(" ");
    commandLine.append(inputFile);
    commandLine.append(" ");
    commandLine.append(outputFile);

    return commandLine.toString();
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
   * @return String
   */
  public String getInputFile() {
    return inputFile;
  }

  /**
   * @return String
   */
  public String getOutputFile() {
    return outputFile;
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
   * Sets the inputFile.
   * @param inputFile The inputFile to set
   */
  public void setInputFile(String inputFile) {
    this.inputFile = inputFile;
  }

  /**
   * Sets the outputFile.
   * @param outputFile The outputFile to set
   */
  public void setOutputFile(String outputFile) {
    this.outputFile = outputFile;
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
  public void setDefaultRange(String fileName)
    throws InvalidParameterException, IOException {

    // Get the data size limits from the image stack
    MRCHeader mrcHeader = new MRCHeader(fileName);
    mrcHeader.read();

    xMin = 1;
    xMax = mrcHeader.getNColumns();
    yMin = 1;
    yMax = mrcHeader.getNRows();
    zMin = 1;
    zMax = mrcHeader.getNSections();

    
  }

}
