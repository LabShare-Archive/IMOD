package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.util.DatasetFiles;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class ExtractmagradParam {
  public static  final String  rcsid =  "$Id$";
  
  public static final String COMMAND_NAME = "extractmagrad";
  
  private final AxisID axisID;
  private final BaseManager manager;
  
  private final EtomoNumber rotationAngle = new EtomoNumber(EtomoNumber.FLOAT_TYPE);

  private String gradientTable = null;
  private String[] commandArray = null;
  
  public ExtractmagradParam(BaseManager manager, AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
  }
  
  public final String[] getCommand() {
    if (commandArray == null) {
      buildCommand();
    }
    return commandArray;
  }
  
  private final void buildCommand() {
    ArrayList command = new ArrayList();
    command.add(BaseManager.getIMODBinPath() + COMMAND_NAME);
    command.add("-rot");
    command.add(rotationAngle.toString());
    command.add("-grad");
    command.add(gradientTable);
    String dataset = manager.getBaseMetaData().getName();
    command.add(DatasetFiles.getStackName(manager, axisID));
    command.add(DatasetFiles.getMagGradientName(manager, axisID));
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }

  public final void setGradientTable(String gradientTable) {
    this.gradientTable = gradientTable;
  }
  
  public final void setRotationAngle(float rotationAngle) {
    this.rotationAngle.set(rotationAngle);
  }
}
/**
* <p> $Log$ </p>
*/