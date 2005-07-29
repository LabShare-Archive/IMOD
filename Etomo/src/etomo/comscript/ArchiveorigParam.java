package etomo.comscript;

import java.io.File;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.util.Utilities;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class ArchiveorigParam implements Command {
  public static  final String  rcsid =  "$Id$";
  
  public static final String COMMAND_NAME = "archiveorig";
  public static final int AXIS_A_MODE = -1;
  public static final int AXIS_B_MODE = -2;
  public static final int AXIS_ONLY_MODE = -3;
  
  private String[] commandArray;
  private int mode = AXIS_ONLY_MODE;
  private File outputFile;
  private final BaseManager manager;
  
  public ArchiveorigParam(BaseManager manager, AxisID axisID) {
    this.manager = manager;
    if (axisID == AxisID.FIRST) {
      mode = AXIS_A_MODE;
    }
    else if (axisID == AxisID.SECOND) {
      mode = AXIS_B_MODE;
    }
    File stack = Utilities.getFile(manager, false, axisID, ".st", "");
    commandArray = new String[] { COMMAND_NAME, "-P", stack.getName() };
    outputFile = Utilities.getFile(manager, false, axisID, "_xray.st.gz", "");
  }
  
  public String[] getCommandArray() {
    return commandArray;
  }
  
  public String getCommandName() {
    return COMMAND_NAME;
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
  
  public int getCommandMode() {
    return mode;
  }
  
  public File getCommandOutputFile() {
    return outputFile;
  }
  
  public int getIntegerValue(int name) {
    return EtomoNumber.INTEGER_NULL_VALUE;
  }
  
  public boolean getBooleanValue(int name) {
    return false;
  }
  
  public AxisID getAxisID() {
    return AxisID.ONLY;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/07/26 17:08:27  sueh
* <p> bug# 701 Get the PID from archiveorig
* <p>
* <p> Revision 1.1  2005/05/18 22:31:38  sueh
* <p> bug# 662 A param object for archiveorig.
* <p> </p>
*/