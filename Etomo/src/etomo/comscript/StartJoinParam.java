package etomo.comscript;

import java.io.File;
import java.util.Hashtable;

import etomo.type.AxisID;
import etomo.type.ProcessName;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class StartJoinParam implements ProcessDetails {
  public static  final String  rcsid =  "$Id$";
  
  private static final ProcessName PROCESS_NAME = ProcessName.STARTJOIN;
  
  private final AxisID axisID;
  
  private Hashtable rotationAnglesList = null;
  private boolean rotate = false;
  private int totalRows = 0;
  
  public StartJoinParam(AxisID axisID) {
    this.axisID = axisID;
  }
  
  public void setRotate(boolean rotate) {
    this.rotate = rotate;
  }
  
  public void setRotationAnglesList(Hashtable rotationAnglesList) {
    this.rotationAnglesList = rotationAnglesList;
  }
  
  public void setTotalRows(int totalRows) {
    this.totalRows = totalRows;
  }
  
  public String getCommandLine() {
    return PROCESS_NAME.getCommand(axisID);
  }
  
  public AxisID getAxisID() {
    return axisID;
  }
  
  
  public Hashtable getHashtable(etomo.comscript.Fields field) {
    if (field == Fields.ROTATION_ANGLES_LIST) {
      return rotationAnglesList;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(etomo.comscript.Fields field) {
    if (field == Fields.ROTATE) {
      return rotate;
    }
    throw new IllegalArgumentException("field=" + field);
  }
  
  public double getDoubleValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }
  
  public int getIntValue(etomo.comscript.Fields field) {
    if (field == Fields.TOTAL_ROWS) {
      return totalRows;
    }
    throw new IllegalArgumentException("field=" + field);
  }
  
  public String[] getCommandArray() {
    return null;
  }
  
  public String getCommandName() {
    return null;
  }
  
  public int getCommandMode() {
    return 0;
  }
  
  public File getCommandOutputFile() {
    return null;
  }
  
  public static final class Fields implements etomo.comscript.Fields {
    private Fields() {
    }
    
    public static final Fields ROTATION_ANGLES_LIST = new Fields();
    public static final Fields TOTAL_ROWS = new Fields();
    public static final Fields ROTATE = new Fields();
  }
}
/**
* <p> $Log$ </p>
*/