package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.JoinManager;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstSectionTableRowData;
import etomo.type.ProcessName;
import etomo.type.SectionTableRowData;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
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
 * <p> Revision 1.14  2009/03/17 00:32:12  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.13  2007/11/06 19:11:40  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.12  2007/02/05 22:37:24  sueh
 * <p> bug# 962 Using BaseMetaData.getName instead of JoinMetaData.getRootName.
 * <p>
 * <p> Revision 1.11  2006/05/22 22:39:27  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.10  2006/05/11 19:44:50  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.9  2006/04/06 19:33:50  sueh
 * <p> bug# 808 Implementing ProcessDetails.
 * <p>
 * <p> Revision 1.8  2006/01/20 20:47:31  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.7  2005/11/19 01:53:07  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.6  2005/07/29 00:49:27  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.5  2005/04/25 20:40:31  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.4  2005/01/08 01:39:53  sueh
 * <p> bug# 578 Updated Command interface.
 * <p>
 * <p> Revision 1.3  2004/12/08 21:21:40  sueh
 * <p> bug# 564 Added getBooleanValue() to get a misc boolean value.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:05:04  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.8  2004/11/16 02:21:23  sueh
 * <p> bug# 520 EtomoNumber uses toString() instead of getString().
 * <p>
 * <p> Revision 1.1.2.7  2004/11/12 22:48:45  sueh
 * <p> bug# 520 Added empty getIntegerValue and getBinning.
 * <p>
 * <p> Revision 1.1.2.6  2004/11/08 22:12:00  sueh
 * <p> bug# 520 Add getMode to conform to Command.  Returns 0, since there
 * <p> is no more for this Param.
 * <p>
 * <p> Revision 1.1.2.5  2004/10/29 01:17:33  sueh
 * <p> bug# 520 Removed working directory from meta data.  Getting working
 * <p> directory from propertyUserDir.
 * <p>
 * <p> Revision 1.1.2.4  2004/10/28 16:55:07  sueh
 * <p> bug# 520 Specifying output file: -o rootname_midas.xf.
 * <p>
 * <p> Revision 1.1.2.3  2004/10/25 22:59:49  sueh
 * <p> bug# 520 Fix chunk size by passing the number of rows to
 * <p> ConstJoinMetaData.getChunkSize.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/22 20:58:58  sueh
 * <p> bug# 520 Getting chunk size from ConstSectionTableRowData.
 * <p>
 * <p> Revision 1.1.2.1  2004/10/21 02:35:21  sueh
 * <p> bug# 520 Param for running Midas.
 * <p> </p>
 */
public class MidasParam implements Command {
  public static final String rcsid = "$Id$";

  private static final int commandSize = 1;
  private static final ProcessName PROCESS_NAME = ProcessName.MIDAS;
  private static final String commandName = "midas";
  private static final String outputFileExtension = "_midas.xf";

  private ConstJoinMetaData metaData;
  private String[] commandArray;
  private SystemProgram program;
  private String workingDir;
  private File outputFile = null;
  private String rootName = null;
  private String outputFileName = null;
  private AxisID axisID;
  private final BaseManager manager;

  public MidasParam(JoinManager manager, AxisID axisID) {
    this.manager = manager;
    metaData = manager.getConstMetaData();
    this.axisID = axisID;
    workingDir = manager.getPropertyUserDir();
    rootName = metaData.getName();
    outputFileName = rootName + outputFileExtension;
    outputFile = new File(workingDir, outputFileName);
    ArrayList options = genOptions();
    commandArray = new String[options.size() + commandSize];
    commandArray[0] = BaseManager.getIMODBinPath() + commandName;
    for (int i = 0; i < options.size(); i++) {
      commandArray[i + commandSize] = (String) options.get(i);
    }
    program = new SystemProgram(manager.getPropertyUserDir(), commandArray,
        axisID, manager.getManagerKey());
    program.setWorkingDirectory(new File(workingDir));
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public String[] getCommandArray() {
    return commandArray;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }
  
  public ProcessName getSubcommandProcessName() {
    return null;
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
  
  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public String getCommand() {
    return commandName;
  }

  private ArrayList genOptions() {
    ArrayList options = new ArrayList();
    ArrayList sectionData = metaData.getSectionTableData();
    int sectionDataSize = sectionData.size();
    StringBuffer chunkSize = new StringBuffer();
    options.add("-c");
    for (int i = 0; i < sectionDataSize; i++) {
      ConstSectionTableRowData data = (SectionTableRowData) sectionData.get(i);
      chunkSize.append(data.getChunkSize(sectionDataSize).toString());
      if (i < sectionDataSize - 1) {
        chunkSize.append(",");
      }
    }
    options.add(chunkSize.toString());
    options.add("-b");
    options.add("0");
    options.add("-D");
    options.add("-o");
    options.add(outputFileName);
    options.add(rootName + ".sample");
    options.add(rootName + ".xf");
    return options;
  }

  public File getCommandOutputFile() {
    return outputFile;
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public static String getName() {
    return commandName;
  }

  public static String getOutputFileExtension() {
    return outputFileExtension;
  }
}
