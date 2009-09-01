package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;

import etomo.BaseManager;
import etomo.storage.CpuAdoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;
import etomo.ui.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.RemotePath;
import etomo.util.RemotePath.InvalidMountRuleException;

/**
 * <p>Description: Command line for processchunks.  Assumes that it will be run
 * once per instance (no reset function).</p>
 * 
 * <p>Copyright: Copyright (c) 2005 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ProcesschunksParam implements DetachedCommandDetails, ParallelParam {
  public static final String rcsid = "$Id$";

  private static final ProcessName PROCESS_NAME = ProcessName.PROCESSCHUNKS;
  public static final int NICE_CEILING = 19;
  public static final int DROP_VALUE = 5;
  public static final String WORKING_DIR_OPTION = "-w";

  private final EtomoBoolean2 resume = new EtomoBoolean2();
  private final EtomoNumber nice = new EtomoNumber();
  private final List machineNames = new ArrayList();
  private final Map computerMap = new HashMap();
  private final EtomoNumber cpuNumber = new EtomoNumber();

  private final BaseManager manager;
  private final AxisID axisID;
  private final String rootName;

  private CommandDetails subcommandDetails = null;
  private String[] commandArray = null;
  private StringBuffer machineList = null;
  private boolean valid = true;
  private boolean debug = true;
  private String queueCommand = null;
  private String queue = null;
  private String subdirName = "";
  private boolean test = false;
  private CommandMode subcommandMode = null;
  private ProcessName subcommandProcessName=null;

  public ProcesschunksParam(final BaseManager manager, final AxisID axisID,
      final String rootName) {
    this.manager = manager;
    this.axisID = axisID;
    this.rootName = rootName;
    init();
  }

  /**
   * Sets rootName to ProcessName + AxisID
   * @param manager
   * @param axisID
   * @param processName
   */
  public ProcesschunksParam(final BaseManager manager, final AxisID axisID,
      final ProcessName processName) {
    this.manager = manager;
    this.axisID = axisID;
    this.rootName = processName.toString() + axisID.getExtension();
    subcommandProcessName=processName;
    init();
  }

  private void init() {
    nice.set(manager.getParallelProcessingDefaultNice());
    nice.setFloor(CpuAdoc.getInstance(axisID, manager.getPropertyUserDir(),
        manager.getManagerKey()).getMinNice());
    nice.setCeiling(NICE_CEILING);
  }

  public String getCommand() {
    return PROCESS_NAME.toString();
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public void setSubcommandMode(CommandMode input) {
    subcommandMode = input;
  }

  public CommandMode getSubcommandMode() {
    return subcommandMode;
  }

  /**
   * Set resume.  This value can be set after the command is built because it
   * comes from the parallel panel and can be changed for a resume.
   * 
   * Causes commandArray to be set to null.
   * @param resume
   */
  public void setResume(final boolean resume) {
    if (this.resume.equals(resume)) {
      return;
    }
    commandArray = null;
    this.resume.set(resume);
  }

  /**
   * Set nice.  This value can be set after the command is built because it
   * comes from the parallel panel and can be changed for a resume.
   * 
   * Causes commandArray to be set to null.
   * @param nice
   */
  public void setNice(final Number nice) {
    if (this.nice.equals(nice)) {
      return;
    }
    commandArray = null;
    this.nice.set(nice);
  }

  public void setSubdirName(final String input) {
    subdirName = input;
  }

  public void setDebug(final boolean input) {
    debug = input;
  }

  public void setSubcommandDetails(CommandDetails input) {
    subcommandDetails = input;
  }

  public CommandDetails getSubcommandDetails() {
    return subcommandDetails;
  }
  
  public ProcessName getSubcommandProcessName() {
    return subcommandProcessName;
  }

  public void setQueue(final String queue) {
    this.queue = queue;
  }

  public void setQueueCommand(final String command) {
    queueCommand = command;
  }

  public void setCPUNumber(final String input) {
    cpuNumber.set(input);
  }

  public boolean equalsRootName(ProcessName processName, AxisID axisID) {
    if ((rootName == null || rootName.matches("\\s*")) && processName == null) {
      return true;
    }
    if (axisID == null) {
      return rootName.equals(processName.toString());
    }
    return rootName.equals(processName.toString() + axisID.getExtension());
  }

  public String getRootName() {
    return rootName;
  }

  public ConstEtomoNumber getResume() {
    return resume;
  }

  public boolean isSubdirNameEmpty() {
    return subdirName == null || subdirName.matches("\\s*");
  }

  public String getSubdirName() {
    return subdirName;
  }

  public Map getComputerMap() {
    return computerMap;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  /**
   * Clears machinesNames.  This value can be set after the command is built because it
   * comes from the parallel panel and can be changed for a resume.
   * 
   * Causes commandArray to be set to null.
   */
  public void resetMachineName() {
    if (machineNames.size() == 0) {
      return;
    }
    commandArray = null;
    machineNames.clear();
  }

  /**
   * Build machineNames and computerMap.
   * @param machineName
   * @param cpus
   */
  public void addMachineName(final String machineName, final int cpus) {
    if (commandArray != null) {
      throw new IllegalStateException(
          "can't change parameter values after command is built");
    }
    for (int i = 0; i < cpus; i++) {
      machineNames.add(machineName);
    }
    if (cpus > 0) {
      computerMap.put(machineName, String.valueOf(cpus));
    }
  }

  public String getCommandName() {
    return PROCESS_NAME.toString();
  }
  
  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public String getShortCommandName() {
    return "pc";
  }

  public String getCommandLine() {
    String[] commandArray = getCommandArray();
    if (commandArray == null || commandArray.length == 0) {
      return null;
    }
    StringBuffer buffer = new StringBuffer(commandArray[0]);
    for (int i = 1; i < commandArray.length; i++) {
      buffer.append(' ' + commandArray[i]);
    }
    return buffer.toString();
  }

  public String[] getCommandArray() {
    if (commandArray == null) {
      buildCommand();
    }
    return commandArray;
  }

  public boolean isValid() {
    return valid;
  }

  /**
   * Gets a string version of the command array which can be used safely, even
   * if there are embedded spaces in the directory paths, because the directory
   * path spaces have been back-slashed.
   * The command file is already quoted and doesn't need a backslash this is
   * because in Windows the command contains backslashes for the path and has to
   * be quoted.
   */
  public String getCommandString() {
    getCommandArray();
    if (commandArray == null) {
      return null;
    }
    StringBuffer buffer = new StringBuffer();
    boolean foundDir = false;
    for (int i = 0; i < commandArray.length; i++) {
      String command = commandArray[i];
      //add back slashes to the spaces in any directory path
      boolean foundDirOption = false;
      if (command.equals(WORKING_DIR_OPTION)) {
        //found an option which takes a directory path
        foundDirOption = true;
        foundDir = true;
      }
      if (!foundDirOption && foundDir) {
        //add back slashes to the spaces in this directory path
        foundDir = false;
        command = backSlashSpaces(command);
      }
      //add each option to the buffer
      if (i == 0) {
        buffer.append(command);
      }
      else {
        buffer.append(" " + command);
      }
    }
    return buffer.toString();
  }

  public String validate() {
    if ((queueCommand == null && (machineNames == null || machineNames.size() == 0))
        || (queueCommand != null && cpuNumber.lt(0))) {
      return "No CPUs where selected.";
    }
    return null;
  }

  public boolean isSecondCommandLine() {
    return queueCommand != null;
  }

  /**
   * Use the second command line to  nice processchunks when it runs on a queue.
   */
  public String getSecondCommandLine() {
    if (queueCommand == null) {
      return "";
    }
    return "nice +18";
  }

  private void buildCommand() {
    valid = true;
    ArrayList command = new ArrayList();
    command.add("tcsh");
    command.add("-f");
    command.add("'" + BaseManager.getIMODBinPath()
        + PROCESS_NAME.toString() + "'");
    if (resume.is()) {
      command.add("-r");
    }
    if (queueCommand == null) {
      command.add("-g");
      command.add("-n");
      command.add(nice.toString());
    }
    String remoteUserDir = null;
    try {
      remoteUserDir = RemotePath.INSTANCE.getRemotePath(manager, manager
          .getPropertyUserDir(), axisID);
    }
    catch (InvalidMountRuleException e) {
      UIHarness.INSTANCE.openMessageDialog("ERROR:  Remote path error.  "
          + "Unabled to run " + PROCESS_NAME + ".\n\n"
          + e.getMessage(), "Processchunks Error", axisID, manager
          .getManagerKey());
      valid = false;
    }
    if (remoteUserDir != null) {
      command.add(WORKING_DIR_OPTION);
      if (!isSubdirNameEmpty()) {
        remoteUserDir += File.separator + subdirName;
      }
      command.add(remoteUserDir.toString());
    }
    command.add("-d");
    command.add(String.valueOf(DROP_VALUE));
    command.add("-c");
    StringBuffer commandsFileName = new StringBuffer();
    if (!isSubdirNameEmpty()) {
      commandsFileName.append("\"../");
    }
    commandsFileName.append(new StringBuffer(DatasetFiles.getCommandsFileName(
        subdirName, rootName)));
    if (!isSubdirNameEmpty()) {
      commandsFileName.append("\"");
    }
    command.add(commandsFileName.toString());
    command.add("-P");
    if (queueCommand == null) {
      //add machine names
      buildMachineList();
      if (machineList != null) {
        command.add(machineList.toString());
      }
    }
    else {
      command.add("-Q");
      command.add(queue);
      command.add("-q");
      command.add(cpuNumber.toString());
      command.add("\"" + queueCommand + "\"");
    }
    command.add(rootName);
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
    if (debug) {
      for (int i = 0; i < commandArray.length; i++) {
        if (i > 0) {
          System.err.print(" ");
        }
        System.err.print(commandArray[i]);
      }
      System.err.println();
    }
  }

  private void buildMachineList() {
    int size = machineNames.size();
    if (size > 0) {
      machineList = new StringBuffer((String) machineNames.get(0));
      for (int i = 1; i < size; i++) {
        machineList.append(',');
        machineList.append(machineNames.get(i));
      }
    }
  }

  /**
   * Put a back slash in front of each space in directoryPath
   * @param directoryPath
   * @return
   */
  private String backSlashSpaces(String directoryPath) {
    if (directoryPath == null) {
      return null;
    }
    //see if directory path has any spaces
    int spaceIndex = directoryPath.indexOf(' ');
    while (spaceIndex != -1) {
      //find each space and add a backslash to it
      directoryPath = directoryPath.substring(0, spaceIndex) + "\\ "
          + directoryPath.substring(spaceIndex + 1);
      int startingIndex = spaceIndex + 2;
      if (startingIndex >= directoryPath.length()) {
        break;
      }
      spaceIndex = directoryPath.indexOf(' ', startingIndex);
      return directoryPath;
    }
    return directoryPath;
  }

  public int getIntValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public boolean getBooleanValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public float getFloatValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String[] getStringArray(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String getString(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public Hashtable getHashtable(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public double getDoubleValue(FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.37  2009/04/20 19:19:36  sueh
 * <p> bug# 1192 Added computerMap to hold selected computers and the number of CPUs selected.  Using computerMap instead of machinesNames to add the computer names to the command.
 * <p>
 * <p> Revision 1.36  2009/03/17 00:32:39  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.35  2008/07/19 00:24:22  sueh
 * <p> bug# 1125 Making it easier to access CpuAdoc by not passing the
 * <p> manager to it; all it needs is the current directory.
 * <p>
 * <p> Revision 1.34  2008/02/23 00:57:05  sueh
 * <p> bug# 1086 Quoting the -c parameter when the subdir is not empty.
 * <p>
 * <p> Revision 1.33  2008/01/14 20:25:22  sueh
 * <p> bug# 1067 Don't backslash the command because its already being quoted
 * <p> (required in Windows).
 * <p>
 * <p> Revision 1.31  2007/12/17 18:35:36  sueh
 * <p> bug# 1061 Added isSecondCommandLine and getSecondCommandLine to nice
 * <p> processchunks when its running on a queue.
 * <p>
 * <p> Revision 1.30  2007/12/10 21:59:33  sueh
 * <p> bug# 1041 Added setting the root name to the constructor since its required.
 * <p> Root name can be set either with a root name name string, which is taken
 * <p> literally; or by a ProcessName, which is used to construct the root name.
 * <p>
 * <p> Revision 1.29  2007/11/12 22:12:22  sueh
 * <p> bug# 1047 Fixed a bug in buildCommand(); was adding the -w option when the
 * <p> remote directory was null.
 * <p>
 * <p> Revision 1.28  2007/11/12 14:53:59  sueh
 * <p> bug# 1047 Added the sub-directory to the remote path when the sub-directory is
 * <p> set.
 * <p>
 * <p> Revision 1.27  2007/11/06 19:15:50  sueh
 * <p> bug# 1047 Added access to the command that processchunks is running.
 * <p>
 * <p> Revision 1.26  2007/09/27 19:22:56  sueh
 * <p> bug# 1044 Added support for submitting chunks to a queue.
 * <p>
 * <p> Revision 1.25  2007/07/17 20:56:40  sueh
 * <p> bug# 1018 Moved nice floor default to CpuAdoc.
 * <p>
 * <p> Revision 1.24  2007/05/18 23:52:13  sueh
 * <p> bug# 987 Made CpuAdoc thread-safe.  Added minNice.
 * <p>
 * <p> Revision 1.23  2007/05/11 15:30:40  sueh
 * <p> bug# 964 Reformat.
 * <p>
 * <p> Revision 1.22  2007/05/03 00:46:05  sueh
 * <p> bug# 964 Placing the nice default in the manager so it can be changed.
 * <p>
 * <p> Revision 1.21  2007/04/27 23:38:51  sueh
 * <p> bug# 964 Turned on debug temporarily.
 * <p>
 * <p> Revision 1.20  2007/02/05 22:39:36  sueh
 * <p> bug# 962 Changed getCommandMode to return CommandMode.
 * <p>
 * <p> Revision 1.19  2006/12/02 04:32:45  sueh
 * <p> bug# 944 Added get/setProcessName
 * <p>
 * <p> Revision 1.18  2006/07/20 23:17:24  sueh
 * <p> bug# 885 Added isValid().  BuildCommand():  handling
 * <p> InvalidMountRuleException.
 * <p>
 * <p> Revision 1.17  2006/05/22 22:40:17  sueh
 * <p> bug# 577 Moved the call to buildCommand to getCommandArray().  Made
 * <p> getCommand() conform to the Command interface.
 * <p>
 * <p> Revision 1.16  2006/05/11 19:47:34  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.15  2006/02/15 18:49:36  sueh
 * <p> bug# 796 Windows fix:  put the path of the command file into single
 * <p> quotes because it will be run from a file and it needs be translated into a
 * <p> unix-style path by Cygwin.
 * <p>
 * <p> Revision 1.14  2006/01/20 20:47:48  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.13  2006/01/11 21:40:57  sueh
 * <p> Removing unnecessary print.
 * <p>
 * <p> Revision 1.12  2006/01/06 02:37:35  sueh
 * <p> bug# 792 Implementing DetachedCommand.  Added getCommandString
 * <p> and backSlashSpaces to create a safe command string that can go into
 * <p> a run file.
 * <p>
 * <p> Revision 1.11  2005/11/21 22:00:17  sueh
 * <p> bug# 761 Added validate() to check that at least one machine name was
 * <p> added.
 * <p>
 * <p> Revision 1.10  2005/11/19 01:54:24  sueh
 * <p> bug# 744 Implementing Command.
 * <p>
 * <p> Revision 1.9  2005/11/10 18:03:53  sueh
 * <p> bug# 733 Added a -w option to processchunks.  The -w option sends the
 * <p> remote version of the propertyUserDir.
 * <p>
 * <p> Revision 1.8  2005/09/21 16:07:09  sueh
 * <p> bug# 532 Allowing a used param to be updated by the parallel processing
 * <p> panel and reused.  When resume, nice, or the machine list are changed,
 * <p> the commandArray is set to null so it can be recreated.
 * <p>
 * <p> Revision 1.7  2005/09/16 20:52:37  sueh
 * <p> bug# 532 Added getResume().
 * <p>
 * <p> Revision 1.6  2005/09/16 17:19:29  sueh
 * <p> bug# 532 Implementing ParallelParam.
 * <p>
 * <p> Revision 1.5  2005/09/07 20:31:15  sueh
 * <p> bug# 532 Adding commands file option.
 * <p>
 * <p> Revision 1.4  2005/09/01 17:46:56  sueh
 * <p> bug# 532 Make the drop value available publicly.  Change
 * <p> getComputerList() to getMachineList and fix it.
 * <p>
 * <p> Revision 1.3  2005/08/30 18:29:09  sueh
 * <p> bug# 532 Added getComputerList() so ProcesschunksProcessMonitor
 * <p> can figure out which computers it is monitoring.
 * <p>
 * <p> Revision 1.2  2005/08/22 16:05:56  sueh
 * <p> bug# 532 Added the extension to the rootName and getRootName().
 * <p>
 * <p> Revision 1.1  2005/08/01 17:58:22  sueh
 * <p> Class to create a processchunks command line.
 * <p> </p>
 */
