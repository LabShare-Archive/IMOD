package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.SystemProgram;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstSectionTableRowData;
import etomo.type.EtomoNumber;
import etomo.type.SectionTableRowData;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.2  2004/11/19 22:56:59  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.10  2004/11/16 02:20:21  sueh
* <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
* <p> EtomoLong with EtomoNumber.
* <p>
* <p> Revision 1.1.2.9  2004/11/12 22:47:53  sueh
* <p> bug# 520 Storing binning, size, and shift information.  Added getBinning().
* <p> Added getIntegerValue().
* <p>
* <p> Revision 1.1.2.8  2004/11/11 01:35:32  sueh
* <p> bug# 520 Adding trial mode:  using -t with sampling rate in Z and binning.
* <p>
* <p> Revision 1.1.2.7  2004/11/08 22:10:50  sueh
* <p> bug# 520 Add modes, implement MAX_SIZE_MODE.  Implement
* <p> Command.  Add function to query current mode.  This way the information
* <p> return by the -m option (max size) can be retrieved with generic code.
* <p>
* <p> Revision 1.1.2.6  2004/10/30 01:28:16  sueh
* <p> bug# 520 Added comments.
* <p>
* <p> Revision 1.1.2.5  2004/10/29 01:17:06  sueh
* <p> bug# 520 Removed working directory from meta data.  Getting working
* <p> directory from propertyUserDir.
* <p>
* <p> Revision 1.1.2.4  2004/10/25 22:58:24  sueh
* <p> bug# 520 Use the negative of shift in X, Y when passing to finish join.
* <p>
* <p> Revision 1.1.2.3  2004/10/22 20:55:34  sueh
* <p> bug# 520 Using EtomoSimpleType where possible.  Changed offsetInX, Y
* <p> to shiftInX, Y.
* <p>
* <p> Revision 1.1.2.2  2004/10/22 03:20:38  sueh
* <p> bug# 520 Reducing the number of ConstJoinMetaData functions by
* <p> passing EtomoInteger, EtomoFloat, etc and using its get() and getString()
* <p> functions.
* <p>
* <p> Revision 1.1.2.1  2004/10/21 02:32:20  sueh
* <p> bug# 520 Param for finishjoin.
* <p> </p>
*/
public class FinishjoinParam implements Command {
  public static final String  rcsid =  "$Id$";
  
  public static final int FINISH_JOIN_MODE = -1;
  public static final int MAX_SIZE_MODE = -2;
  public static final int TRIAL_MODE = -3;
  public static final String SIZE_TAG = "Maximum size required:";
  public static final String OFFSET_TAG = "Offset needed to center:";
  public static final int SIZE_IN_X_INDEX = 3;
  public static final int SIZE_IN_Y_INDEX = 4;
  public static final int OFFSET_IN_X_INDEX = 4;
  public static final int OFFSET_IN_Y_INDEX = 5;
  public static final int SIZE_IN_X_VALUE_NAME = -1;
  public static final int SIZE_IN_Y_VALUE_NAME = -2;
  public static final int SHIFT_IN_X_VALUE_NAME = -3;
  public static final int SHIFT_IN_Y_VALUE_NAME = -4;
  
  private static final String commandName = "finishjoin";
  private ConstJoinMetaData metaData;
  private String[] commandArray;
  private SystemProgram program;
  private String rootName;
  private File outputFile;
  private int mode;
  private int binning = 1;
  private int sizeInX = Integer.MIN_VALUE;
  private int sizeInY = Integer.MIN_VALUE;
  private int shiftInX = Integer.MIN_VALUE;
  private int shiftInY = Integer.MIN_VALUE;
  
  public FinishjoinParam(ConstJoinMetaData metaData, int mode) {
    this.metaData = metaData;
    this.mode = mode;
    rootName = metaData.getRootName();
    outputFile = new File(EtomoDirector.getInstance().getCurrentPropertyUserDir(), rootName + ".join");
    ArrayList options = genOptions();
    commandArray = new String[options.size() + 3];
    commandArray[0] = "tcsh";
    commandArray[1] = "-f";
    commandArray[2] = BaseManager.getIMODBinPath() + commandName;          
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + 3] = (String) options.get(i);
    }
    program = new SystemProgram(commandArray);
    program.setWorkingDirectory(new File(EtomoDirector.getInstance().getCurrentPropertyUserDir()));
  }
  
  public int getIntegerValue(int name) {
    switch (name) {
    case SIZE_IN_X_VALUE_NAME:
      return sizeInX;
    case SIZE_IN_Y_VALUE_NAME:
      return sizeInY;
    case SHIFT_IN_X_VALUE_NAME:
      return shiftInX;
    case SHIFT_IN_Y_VALUE_NAME:
      return shiftInY;
    default:
      return Integer.MIN_VALUE;
    }
  }
  
  public int getBinning() {
    return binning;
  }
  
  public String[] getCommandArray() {
    return commandArray;
  }
  
  public String getCommandLine() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < commandArray.length; i++) {
      buffer.append(commandArray[i] + " ");
    }
    return buffer.toString();
  }
  
  public String getCommandName() {
    return commandName;
  }
  
  public static String getName() {
    return commandName;
  }
  
  public static ConstEtomoNumber getShift(String offset) {
    return new EtomoNumber(EtomoNumber.INTEGER_TYPE).set(offset).getNegation();
  }
  
  public File getOutputFile() {
    return outputFile;
  }
  
  public int getMode() {
    return mode;
  }
  
  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    if (metaData.getUseAlignmentRefSection()) {
      options.add("-r");
      options.add(metaData.getAlignmentRefSection().toString());
    }
    //Add optional size
    ConstEtomoNumber sizeInX = metaData.getSizeInX();
    ConstEtomoNumber sizeInY = metaData.getSizeInY();
    this.sizeInX = sizeInX.getInteger(true);
    this.sizeInY = sizeInY.getInteger(true);
    if (sizeInX.isSetAndNotDefault() || sizeInY.isSetAndNotDefault()) {
      options.add("-s");
      //both numbers must exist
      options.add(sizeInX.toString(true) + "," + sizeInY.toString(true));
    }
    //Add optional offset
    ConstEtomoNumber shiftInX = metaData.getShiftInX();
    ConstEtomoNumber shiftInY = metaData.getShiftInY();
    this.shiftInX = shiftInX.getInteger(true);
    this.shiftInY = shiftInY.getInteger(true);
    if (shiftInX.isSetAndNotDefault() || shiftInY.isSetAndNotDefault()) {
      options.add("-o");
      //both numbers must exist
      //offset is a negative shift
      options.add(shiftInX.getNegation().toString(true) + "," + shiftInY.getNegation().toString(true));
    }
    if (mode == MAX_SIZE_MODE) {
      options.add("-m");
    }
    if (mode == TRIAL_MODE) {
      options.add("-t");
      options.add(metaData.getUseEveryNSlices().toString());
      ConstEtomoNumber binning = metaData.getTrialBinning();
      this.binning = binning.getInteger(true);
      if (binning.isSetAndNotDefault()) {
        options.add("-b");
        options.add(binning.toString());
      }
    }
    options.add(rootName);
    ArrayList sectionData = metaData.getSectionTableData();
    int sectionDataSize = sectionData.size();
    for (int i = 0; i < sectionDataSize; i++) {
      ConstSectionTableRowData data = (SectionTableRowData) sectionData.get(i);
      //both numbers must exist
      options.add(data.getFinalStart().toString() + "," + data.getFinalEnd().toString());
    }
    return options;
  }

}
