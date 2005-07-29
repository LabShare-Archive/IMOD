package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.type.AxisID;
import etomo.type.ConstJoinMetaData;
import etomo.type.ScriptParameter;

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
* <p> Revision 1.9  2005/04/25 20:41:52  sueh
* <p> bug# 615 Passing the axis where a command originates to the message
* <p> functions so that the message will be popped up in the correct window.
* <p> This requires adding AxisID to many objects.
* <p>
* <p> Revision 1.8  2005/01/25 21:51:47  sueh
* <p> Converting EtomoNumbers parameters to ScriptParameters.
* <p>
* <p> Revision 1.7  2005/01/21 22:54:15  sueh
* <p> bug# 509 bug# 591  Added isUpdateCommand() in place of
* <p> isSetAndNotDefault() as a standard why to decide if a parameter should
* <p> be placed in a comscript.
* <p>
* <p> Revision 1.6  2005/01/08 01:46:38  sueh
* <p> bug# 578 Updated Command interface.
* <p>
* <p> Revision 1.5  2004/12/08 21:22:43  sueh
* <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
* <p>
* <p> Revision 1.4  2004/12/02 18:28:00  sueh
* <p> bug 520 Remove unnecessary import.
* <p>
* <p> Revision 1.3  2004/12/01 03:46:22  sueh
* <p> bug# 520 Removed unnecessary member variable SystemProgram
* <p> program.
* <p>
* <p> Revision 1.2  2004/11/19 23:15:33  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.10  2004/11/16 02:21:42  sueh
* <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
* <p> EtomoLong with EtomoNumber.
* <p>
* <p> Revision 1.1.2.9  2004/11/12 22:49:00  sueh
* <p> bug# 520 Added empty getIntegerValue and getBinning.
* <p>
* <p> Revision 1.1.2.8  2004/11/08 22:12:41  sueh
* <p> bug# 520 Add getMode to conform to Command.
* <p>
* <p> Revision 1.1.2.7  2004/10/30 01:32:09  sueh
* <p> bug# 520 Added comments.
* <p>
* <p> Revision 1.1.2.6  2004/10/29 01:17:46  sueh
* <p> bug# 520 Removed working directory from meta data.  Getting working
* <p> directory from propertyUserDir.
* <p>
* <p> Revision 1.1.2.5  2004/10/28 16:57:04  sueh
* <p> bug# 520 Specifying output file: -o rootname_auto.xf.
* <p>
* <p> Revision 1.1.2.4  2004/10/22 20:59:25  sueh
* <p> bug# 520 Using EtomoSimpleType where possible.  Changed offsetInX, Y
* <p> to shiftInX, Y.
* <p>
* <p> Revision 1.1.2.3  2004/10/22 03:21:16  sueh
* <p> bug# 520 Reducing the number of ConstJoinMetaData functions by
* <p> passing EtomoInteger, EtomoFloat, etc and using their get() and
* <p> getString() functions.
* <p>
* <p> Revision 1.1.2.2  2004/10/21 02:37:22  sueh
* <p> bug# 520 Adding modes (initial and refine) that can change how the
* <p> options are set.  Removed unnecessary function run().  Implementing
* <p> Command interface.
* <p>
* <p> Revision 1.1.2.1  2004/10/18 17:45:04  sueh
* <p> bug# 520 Added a param to create the xfalign command.
* <p> </p>
*/
public class XfalignParam implements Command {
  public static final String  rcsid =  "$Id$";
  
  private static final int commandSize = 3;
  private static final String commandName = "xfalign";
  private static final String outputFileExtension = "_auto.xf";
  
  public static final int INITIAL_MODE = -1;
  public static final int REFINE_MODE = -2;
  
  private ConstJoinMetaData metaData;
  private String[] commandArray;
  private String workingDir = null;
  private String rootName = null;
  private String outputFileName = null;
  private File outputFile = null;
  private int mode;
  private final BaseManager manager;
  
  public XfalignParam(JoinManager manager, int mode) {
    this.manager = manager;
    metaData = manager.getConstMetaData();
    this.mode = mode;
    workingDir = manager.getPropertyUserDir();
    rootName = metaData.getRootName();
    outputFileName = rootName + outputFileExtension;
    outputFile = new File(workingDir, outputFileName);
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
  
  public static String getOutputFileExtension() {
    return outputFileExtension;
  }
  
  public File getCommandOutputFile() {
    return outputFile;
  }
  
  public int getIntegerValue(int name) {
    return Integer.MIN_VALUE;
  }
  
  public boolean getBooleanValue(int name) {
    return false;
  }
  
  public int getCommandMode() {
    return mode;
  }
  
  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    options.add("-tomo");
    switch (mode) {
    case INITIAL_MODE:
      genInitialOptions(options);
      break;
    case REFINE_MODE:
      genRefineOptions(options);
      break;
    default:
      throw new IllegalArgumentException("Unknown mode " + mode + ".");
    }
    return options;
  }
  
  private void genInitialOptions(ArrayList options) {
    options.add("-pre");
    genFilterOptions(options);
    genParamsOptions(options);
    options.add(rootName + ".sampavg");
    options.add(outputFileName);
  }
  
  private void genRefineOptions(ArrayList options) {
    options.add("-ini");
    options.add(rootName + ".xf");
    genFilterOptions(options);
    genParamsOptions(options);
    options.add(rootName + ".sampavg");
    options.add(outputFileName);
  }
  
  private void genFilterOptions(ArrayList options) {
    ScriptParameter sigmaLowFrequency = metaData.getSigmaLowFrequencyParameter();
    ScriptParameter cutoffHighFrequency = metaData
        .getCutoffHighFrequencyParameter();
    ScriptParameter sigmaHighFrequency = metaData.getSigmaHighFrequencyParameter();
    //optional
    if (sigmaLowFrequency.isUseInScript() || cutoffHighFrequency.isUseInScript()
        || sigmaHighFrequency.isUseInScript()) {
      options.add("-fil");
      //all three numbers must exist
      options.add(sigmaLowFrequency.toString() + "," + sigmaHighFrequency.toString() + ",0,"
          + cutoffHighFrequency.toString());
    }
  }
  
  private void genParamsOptions(ArrayList options) {
    if (!metaData.isFullLinearTransformation()) {
      if (metaData.isRotationTranslationMagnification()) {
        options.add("-par");
        options.add("4");
      }
      else if (metaData.isRotationTranslation()) {
        options.add("-par");
        options.add("3");
      }
    }
  }

}
