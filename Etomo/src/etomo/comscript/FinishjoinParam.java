package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstSectionTableRowData;
import etomo.type.EtomoNumber;
import etomo.type.ScriptParameter;
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
* <p> Revision 1.10  2005/01/25 21:40:41  sueh
* <p> Converting EtomoNumbers to ScriptParameters.
* <p>
* <p> Revision 1.9  2005/01/21 22:41:57  sueh
* <p> bug# 509 bug# 591  Added isUpdateCommand() in place of
* <p> isSetAndNotDefault() as a standard why to decide if a parameter should
* <p> be placed in a comscript.
* <p>
* <p> Revision 1.8  2005/01/08 01:39:09  sueh
* <p> bug# 578 Changed the names of the statics used to make variables
* <p> available in the Command interface.  Add GET_.  Updated Command
* <p> interface.
* <p>
* <p> Revision 1.7  2004/12/08 21:21:06  sueh
* <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
* <p> Changed statics SHIFT_IN_X_VALUE_NAME, etc to SHIFT_IN_X.
* <p>
* <p> Revision 1.6  2004/12/02 18:24:48  sueh
* <p> bug 520 Remove unnecessary import.
* <p>
* <p> Revision 1.5  2004/12/01 03:45:15  sueh
* <p> bug# 520 Removed unnecessary member variable SystemProgram
* <p> program.
* <p>
* <p> Revision 1.4  2004/11/24 23:03:47  sueh
* <p> bug# 520 Add -P option to get the PID from finishjoin.
* <p>
* <p> Revision 1.3  2004/11/23 22:29:29  sueh
* <p> bug# 520 Converted finalStart and end to EtomoNumbers.
* <p>
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
  public static final int GET_SIZE_IN_X = -1;
  public static final int GET_SIZE_IN_Y = -2;
  public static final int GET_SHIFT_IN_X = -3;
  public static final int GET_SHIFT_IN_Y = -4;
  public static final int GET_BINNING = -5;
  
  private static final String commandName = "finishjoin";
  private ConstJoinMetaData metaData;
  private String[] commandArray;
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
  }
  
  public AxisID getAxisID() {
    return AxisID.ONLY;
  }
  
  public int getIntegerValue(int name) {
    switch (name) {
    case GET_SIZE_IN_X:
      return sizeInX;
    case GET_SIZE_IN_Y:
      return sizeInY;
    case GET_SHIFT_IN_X:
      return shiftInX;
    case GET_SHIFT_IN_Y:
      return shiftInY;
    case GET_BINNING:
      return binning;
    default:
      return Integer.MIN_VALUE;
    }
  }
  
  public boolean getBooleanValue(int name) {
    return false;
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
  
  public static int getShift(String offset) {
    EtomoNumber offsetNumber = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    if (offsetNumber.set(offset).isValid()) {
      return offsetNumber.getInteger() * -1;
    }
    throw new IllegalArgumentException(offsetNumber.getInvalidReason());
  }
  
  public File getCommandOutputFile() {
    return outputFile;
  }
  
  public int getCommandMode() {
    return mode;
  }
  
  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    options.add("-P");
    if (metaData.getUseAlignmentRefSection()) {
      options.add("-r");
      options.add(metaData.getAlignmentRefSection().toString());
    }
    //Add optional size
    ScriptParameter sizeInX = metaData.getSizeInXParameter();
    ScriptParameter sizeInY = metaData.getSizeInYParameter();
    this.sizeInX = sizeInX.getInteger();
    this.sizeInY = sizeInY.getInteger();
    if (sizeInX.isUseInScript() || sizeInY.isUseInScript()) {
      options.add("-s");
      //both numbers must exist
      options.add(sizeInX.toString() + "," + sizeInY.toString());
    }
    //Add optional offset
    ScriptParameter shiftInX = metaData.getShiftInXParameter();
    ScriptParameter shiftInY = metaData.getShiftInYParameter();
    this.shiftInX = shiftInX.getInteger();
    this.shiftInY = shiftInY.getInteger();
    if (shiftInX.isUseInScript() || shiftInY.isUseInScript()) {
      options.add("-o");
      //both numbers must exist
      //offset is a negative shift
      options.add(Integer.toString(shiftInX.getInteger() * -1) + "," + Integer.toString(shiftInY.getInteger() * -1));
    }
    if (mode == MAX_SIZE_MODE) {
      options.add("-m");
    }
    if (mode == TRIAL_MODE) {
      options.add("-t");
      options.add(metaData.getUseEveryNSlices().toString());
      ScriptParameter binning = metaData.getTrialBinningParameter();
      this.binning = binning.getInteger();
      if (binning.isUseInScript()) {
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
