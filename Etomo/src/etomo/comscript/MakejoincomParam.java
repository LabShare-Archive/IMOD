package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstSectionTableRowData;
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
* <p> Old log: Makejoincom.java,v
* <p> Revision 1.1.2.1  2004/09/29 17:46:50  sueh
* <p> bug# 520 Class to run the makejoincom script.  Gets its options from
* <p> ConstJoinMetaData.
* <p> </p>
* 
* <p> $Log$
* <p> Revision 1.9  2005/04/25 20:40:08  sueh
* <p> bug# 615 Passing the axis where a command originates to the message
* <p> functions so that the message will be popped up in the correct window.
* <p> This requires adding AxisID to many objects.
* <p>
* <p> Revision 1.8  2005/01/25 22:47:02  sueh
* <p> Adding boolean force parameter to ScriptParameter.addToScript() to tell
* <p> the function to avoid checking isUseInScript()
* <p>
* <p> Revision 1.7  2005/01/25 21:42:02  sueh
* <p> Converting EtomoNumbers parameters to ScriptParameters.
* <p>
* <p> Revision 1.6  2005/01/21 22:42:19  sueh
* <p> bug# 509 bug# 591  Added isUpdateCommand() in place of
* <p> isSetAndNotDefault() as a standard why to decide if a parameter should
* <p> be placed in a comscript.
* <p>
* <p> Revision 1.5  2005/01/08 01:39:40  sueh
* <p> bug# 578 Updated Command interface.
* <p>
* <p> Revision 1.4  2004/12/08 21:21:27  sueh
* <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
* <p>
* <p> Revision 1.3  2004/12/01 03:45:47  sueh
* <p> bug# 520 Removed unnecessary member variable SystemProgram
* <p> program.
* <p>
* <p> Revision 1.2  2004/11/19 23:04:18  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.13  2004/11/19 00:01:10  sueh
* <p> bug# 520 Removed unnecessary pass-through functions from
* <p> ConstSectionTableRowData.
* <p>
* <p> Revision 1.1.2.12  2004/11/16 23:27:56  sueh
* <p> bug# 520 Bug fix:  In genOptions(), checking for sectionData == null
* <p> before using.
* <p>
* <p> Revision 1.1.2.11  2004/11/16 02:20:46  sueh
* <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
* <p> EtomoLong with EtomoNumber.
* <p>
* <p> Revision 1.1.2.10  2004/11/15 22:17:10  sueh
* <p> bug# 520 Implementing Command.
* <p>
* <p> Revision 1.1.2.9  2004/10/30 01:31:40  sueh
* <p> bug# 520 bug fix: A missing rotationAngleX caused the param to not use
* <p> the -rot options.  Now the option will be used if any rotation angle is set and
* <p> the unset ones will be defaulted.
* <p>
* <p> Revision 1.1.2.8  2004/10/29 22:07:58  sueh
* <p> bug# 520 Use -tmpext to set the temp file extension to .rot.
* <p>
* <p> Revision 1.1.2.7  2004/10/29 01:17:19  sueh
* <p> bug# 520 Removed working directory from meta data.  Getting working
* <p> directory from propertyUserDir.
* <p>
* <p> Revision 1.1.2.6  2004/10/22 20:57:50  sueh
* <p> bug# 520 Simplifying ConstSectionTableRowData by passing
* <p> EtomoSimpleType instead of String and int.
* <p>
* <p> Revision 1.1.2.5  2004/10/22 03:20:52  sueh
* <p> bug# 520 Reducing the number of ConstJoinMetaData functions by
* <p> passing EtomoInteger, EtomoFloat, etc and using its get() and getString()
* <p> functions.
* <p>
* <p> Revision 1.1.2.4  2004/10/21 02:34:59  sueh
* <p> bug# 520 Removed unnecessary function run().
* <p>
* <p> Revision 1.1.2.3  2004/10/18 17:42:02  sueh
* <p> bug# 520 Added -reference to the command string.
* <p>
* <p> Revision 1.1.2.2  2004/10/14 02:27:53  sueh
* <p> bug# 520 Setting working directory in SystemProgram.
* <p>
* <p> Revision 1.1.2.1  2004/10/08 15:50:06  sueh
* <p> bug# 520 Renamed Makejoincom to MakejoincomParam.  Switched from
* <p> a command line to a command array because of the possibility of spaces
* <p> within parameters.
* <p> </p>
*/
public class MakejoincomParam implements Command {
  public static  final String  rcsid =  "$Id$";
  
  private static final int commandSize = 3;
  private static final String commandName = "makejoincom";
  
  private ConstJoinMetaData metaData;
  private String[] commandArray;
  private SystemProgram program;
  
  public MakejoincomParam(ConstJoinMetaData metaData) {
    this.metaData = metaData;
    ArrayList options = genOptions();
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = "tcsh";
    commandArray[1] = "-f";
    commandArray[2] = BaseManager.getIMODBinPath() + commandName;          
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
  }
  
  public AxisID getAxisID() {
    return AxisID.ONLY;
  }

  
  public String[] getCommandArray() {
    return commandArray;
  }
  
  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    ArrayList sectionData = metaData.getSectionTableData();
    if (sectionData != null) {
      int sectionDataSize = sectionData.size();
      for (int i = 0; i < sectionDataSize; i++) {
        ConstSectionTableRowData data = (SectionTableRowData) sectionData
            .get(i);
        if (i < sectionDataSize - 1) {
          options.add("-top");
          //both numbers must exist
          options.add(data.getSampleTopStart().toString() + ","
              + data.getSampleTopEnd().toString());
        }
        if (i != 0) {
          options.add("-bot");
          //both numbers must exist
          options.add(data.getSampleBottomStart().toString() + ","
              + data.getSampleBottomEnd().toString());
        }
        //Add optional rotation angles
        ScriptParameter rotationAngleX = data.getRotationAngleXParameter();
        ScriptParameter rotationAngleY = data.getRotationAngleYParameter();
        ScriptParameter rotationAngleZ = data.getRotationAngleZParameter();
        if (rotationAngleX.isUseInScript() || rotationAngleY.isUseInScript()
            || rotationAngleZ.isUseInScript()) {
          options.add("-rot");
          //all three numbers must exist
          StringBuffer buffer = new StringBuffer();
          if (rotationAngleX.isNull()) {
            buffer.append("0");
          }
          else {
            buffer.append(rotationAngleX.getDouble());
          }
          buffer.append(",");
          if (rotationAngleY.isNull()) {
            buffer.append("0");
          }
          else {
            buffer.append(rotationAngleY.getDouble());
          }
          buffer.append(",");
          if (rotationAngleZ.isNull()) {
            buffer.append("0");
          }
          else {
            buffer.append(rotationAngleZ.getDouble());
          }
          options.add(buffer.toString());
        }
        options.add(data.getSection().getAbsolutePath());
      }
    }
    options.add("-tmpext");
    options.add("rot");
    ScriptParameter densityRefSection = metaData.getDensityRefSectionParameter();
    if (densityRefSection.isUseInScript()) {
      options.add("-ref");
      options.add(densityRefSection.toString());
    }
    options.add(metaData.getRootName());
    return options;
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
  
  public int getIntegerValue(int name) {
    return Integer.MIN_VALUE;
  }
  
  public boolean getBooleanValue(int name) {
    return false;
  }
  
  public int getCommandMode() {
    return 0;
  }
  
  public File getCommandOutputFile() {
    return null;
  }
  
  public static String getName() {
    return commandName;
  }

}
