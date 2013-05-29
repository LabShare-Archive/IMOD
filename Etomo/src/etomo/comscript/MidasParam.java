package etomo.comscript;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ConstSectionTableRowData;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
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
 * <p> Revision 1.20  2011/02/22 03:17:27  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.19  2010/04/28 16:00:59  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.18  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.17  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.16  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.15  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
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
public final class MidasParam implements Command {
  public static final String rcsid = "$Id$";

  private static final int commandSize = 1;
  private static final ProcessName PROCESS_NAME = ProcessName.MIDAS;
  private static final String commandName = "midas";
  private static final String outputFileExtension = "_midas.xf";

  private final EtomoNumber binning = new EtomoNumber();

  private final String workingDir;
  private final File outputFile;
  private final String rootName;
  private final String outputFileName;
  private final AxisID axisID;
  private final Mode mode;
  private final BaseManager manager;

  private ArrayList sectionTableRowData = null;
  private String inputFileName = null;
  private String[] commandArray = null;

  public MidasParam(final BaseManager manager, final AxisID axisID, final Mode mode) {
    this.manager = manager;
    this.axisID = axisID;
    this.mode = mode;
    workingDir = manager.getPropertyUserDir();
    rootName = manager.getName();
    outputFileName = rootName + outputFileExtension;
    outputFile = new File(workingDir, outputFileName);
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public String[] getCommandArray() {
    if (commandArray == null) {
      ArrayList options = genOptions();
      commandArray = new String[options.size() + commandSize];
      commandArray[0] = BaseManager.getIMODBinPath() + commandName;
      for (int i = 0; i < options.size(); i++) {
        commandArray[i + commandSize] = (String) options.get(i);
      }
    }
    return commandArray;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public String getCommandLine() {
    getCommandArray();
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
    if (mode == Mode.SAMPLE) {
      // If the section table is not available, don't use the chunks option.
      if (sectionTableRowData != null) {
        int sectionDataSize = sectionTableRowData.size();
        StringBuffer chunkSize = new StringBuffer();
        options.add("-cs");
        int nSlices;
        for (int i = 0; i < sectionDataSize; i++) {
          ConstSectionTableRowData data = (SectionTableRowData) sectionTableRowData
              .get(i);
          // Order: section 1 - top, section 2 - bottom, section 2 - top, section 3 -
          // bottom.
          nSlices = data.getSampleBottomNumberSlices(sectionDataSize);
          if (nSlices != -1) {
            chunkSize.append(nSlices);
            if (i < sectionDataSize - 1) {
              chunkSize.append(",");
            }
          }
          nSlices = data.getSampleTopNumberSlices(sectionDataSize);
          if (nSlices != -1) {
            chunkSize.append(nSlices + ",");
          }
        }
        options.add(chunkSize.toString());
      }
    }
    if (!binning.isNull() && binning.gt(1)) {
      options.add("-B");
      options.add(binning.toString());
    }
    if (mode == Mode.SERIAL_SECTIONS_FIX_EDGES) {
      options.add("-p");
      options.add(FileType.PIECE_LIST.getFileName(manager, axisID));
    }
    options.add("-b");
    options.add("0");
    if (mode == Mode.SAMPLE) {
      options.add("-D");
      options.add("-o");
      options.add(outputFileName);
      options.add(inputFileName);
      options.add(rootName + ".xf");
    }
    else if (mode == Mode.SERIAL_SECTIONS_FIX_EDGES) {
      options.add("-q");
      options.add(inputFileName);
      options.add(FileType.PIECE_SHIFTS.getFileName(manager, axisID));
    }
    return options;
  }

  public void setSectionTableRowData(final ArrayList input) {
    sectionTableRowData = input;
  }

  public void setInputFileName(final String input) {
    inputFileName = input;
  }

  public void setBinning(final Number input) {
    binning.set(input);
  }

  public File getCommandOutputFile() {
    return outputFile;
  }

  public FileType getOutputImageFileType() {
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public static String getName() {
    return commandName;
  }

  public static String getOutputFileExtension() {
    return outputFileExtension;
  }

  public static final class Mode {
    public static final Mode SAMPLE = new Mode();
    public static final Mode SERIAL_SECTIONS_FIX_EDGES = new Mode();

    private Mode() {
    }
  }
}
