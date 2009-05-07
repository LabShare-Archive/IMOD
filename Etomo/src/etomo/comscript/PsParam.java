package etomo.comscript;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.OSType;
import etomo.type.Time;
import etomo.util.RemotePath;

/**
 * <p>Description:
 * Class which represents the ps command.  Can build a ps command string and
 * parse ps command output.</p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class PsParam {
  public static final String rcsid = "$Id$";

  private static final String PID_HEADER = "PID";
  private static final String PARENT_PID_HEADER = "PPID";
  private static final String GROUP_PID_HEADER = "PGID";
  private static final String WINDOW_PID_HEADER = "WINPID";
  private static final String TERMINAL_HEADER = "TTY";
  private static final String USER_ID_HEADER = "UID";

  private final String startTimeHeader;
  private final String startTimeCommand;

  private final ArrayList command = new ArrayList();
  private final ArrayList valuesArray = new ArrayList();

  private String[] output = null;

  private int pidStartIndex = -1;
  private int pidEndIndex = -1;
  private int parentPidStartIndex = -1;
  private int parentPidEndIndex = -1;
  private int groupPidStartIndex = -1;
  private int groupPidEndIndex = -1;
  private int windowPidStartIndex = -1;
  private int windowPidEndIndex = -1;
  private int terminalStartIndex = -1;
  private int terminalEndIndex = -1;
  private int userIdStartIndex = -1;
  private int userIdEndIndex = -1;
  private int startTimeStartIndex = -1;
  private int startTimeEndIndex = -1;

  private boolean pidColumn = true;
  private boolean parentPidColumn = false;
  private boolean groupPidColumn = true;
  private boolean windowPidColumn = false;
  private boolean terminalColumn = false;
  private boolean userIdColumn = false;
  private boolean startTimeColumn = true;

  /**
   * Builds the ps commmand.  Uses the pid to limit the output to one process.
   * @param pid
   * @param osType
   * @param hostName
   */
  public PsParam(BaseManager manager, AxisID axisID, String pid, OSType osType,
      String hostName, boolean willRunOnWorkerThread) {
    startTimeHeader = osType == OSType.WINDOWS ? "STIME" : "STARTED";
    startTimeCommand = osType == OSType.MAC ? "start" : "lstart";
    if (hostName != null && !hostName.matches("\\*")
        && !hostName.equals(RemotePath.getHostName(manager, axisID))) {
      //If the timeout option cannot be added to ssh then only ssh if the caller
      //of this constructor promises to run the command in a worker thread.  An
      //ssh on the main thread can lock up the user interface.
      if (willRunOnWorkerThread || SshParam.INSTANCE.isTimeoutAvailable()) {
        List sshCommand = SshParam.INSTANCE.getCommand(true, hostName);
        if (sshCommand != null) {
          Iterator iterator = sshCommand.iterator();
          while (iterator.hasNext()) {
            Object element = iterator.next();
            if (element != null && !element.equals("")) {
              command.add(element);
            }
          }
        }
      }
    }
    //The calling function expects at least one line be returned even when the
    //pid is not found.
    command.add("ps");
    if (osType == OSType.WINDOWS) {
      parentPidColumn = true;
      windowPidColumn = true;
      terminalColumn = true;
      userIdColumn = true;
    }
    else if (osType == OSType.MAC) {
      command.add("-A");
    }
    else if (pid != null) {
      command.add("-p");
      command.add(pid);
    }
    if (osType != OSType.WINDOWS) {
      command.add("-o");
    }
    command.add("pid,pgid," + startTimeCommand);
  }

  public Row getRow() {
    return new Row(this);
  }

  public ArrayList getCommandArray() {
    return command;
  }

  /**
   * Sets the output of the ps command.  Sets the indices of fields that can be
   * parsed.
   * @param output
   */
  public void setOutput(String[] output) {
    this.output = output;
    valuesArray.clear();
    if (output == null || output.length < 2) {
      return;
    }
    //set the indexes based on the header
    int endIndex = -1;
    if (pidColumn) {
      pidStartIndex = endIndex + 1;
      endIndex = pidEndIndex = output[0].indexOf(PID_HEADER)
          + PID_HEADER.length();
    }
    if (parentPidColumn) {
      parentPidStartIndex = endIndex + 1;
      endIndex = parentPidEndIndex = output[0].indexOf(PARENT_PID_HEADER)
          + PARENT_PID_HEADER.length();
    }
    if (groupPidColumn) {
      groupPidStartIndex = endIndex + 1;
      endIndex = groupPidEndIndex = output[0].indexOf(GROUP_PID_HEADER)
          + GROUP_PID_HEADER.length();
    }
    if (windowPidColumn) {
      windowPidStartIndex = endIndex + 1;
      endIndex = windowPidEndIndex = output[0].indexOf(WINDOW_PID_HEADER)
          + WINDOW_PID_HEADER.length();
    }
    if (terminalColumn) {
      terminalStartIndex = endIndex + 1;
      endIndex = terminalEndIndex = output[0].indexOf(TERMINAL_HEADER)
          + TERMINAL_HEADER.length();
    }
    if (userIdColumn) {
      userIdStartIndex = endIndex + 1;
      endIndex = userIdEndIndex = output[0].indexOf(USER_ID_HEADER)
          + USER_ID_HEADER.length();
    }
    if (startTimeColumn) {
      startTimeStartIndex = endIndex + 1;
      startTimeEndIndex = output[0].indexOf(startTimeHeader)
          + startTimeHeader.length();
    }
    loadValues();
  }

  /**
   * Loads the output of the ps command into valuesArray.
   */
  private void loadValues() {
    if (valuesArray.size() > 0 || output == null) {
      return;
    }
    for (int i = 1; i < output.length; i++) {
      if (output[i] != null)
        if (output[i] != null && output[i].length() >= startTimeEndIndex) {
          valuesArray.add(new Values(output[i]));
        }
    }
  }

  /**
   * @return the index of the row in the ps output where the pid is found.
   * @param pid
   */
  int findRow(String pid) {
    for (int i = 0; i < valuesArray.size(); i++) {
      Values values = (Values) valuesArray.get(i);
      if (values.getPid().equals(pid)) {
        return i;
      }
    }
    return -1;
  }

  /**
   * @return true if the pid, groupPid, and startTime are found on a row.
   * @param pid
   * @param groupPid
   * @param startTime
   */
  public boolean findRow(String pid, String groupPid, Time startTime) {
    for (int i = 0; i < valuesArray.size(); i++) {
      Values values = (Values) valuesArray.get(i);
      if (values.getPid().equals(pid) && values.getGroupPid().equals(groupPid)
          && values.getStartTime().almostEquals(startTime)) {
        return true;
      }
    }
    return false;
  }

  String getGroupPid(int rowIndex) {
    if (rowIndex == -1 || valuesArray.size() <= rowIndex) {
      return null;
    }
    return ((Values) valuesArray.get(rowIndex)).getGroupPid();
  }

  Time getStartTime(int rowIndex) {
    if (rowIndex == -1 || valuesArray.size() <= rowIndex) {
      return null;
    }
    return ((Values) valuesArray.get(rowIndex)).getStartTime();
  }

  int getPidStartIndex() {
    return pidStartIndex;
  }

  int getPidEndIndex() {
    return pidEndIndex;
  }

  int getGroupPidStartIndex() {
    return groupPidStartIndex;
  }

  int getGroupPidEndIndex() {
    return groupPidEndIndex;
  }

  int getStartTimeStartIndex() {
    return startTimeStartIndex;
  }

  int getStartTimeEndIndex() {
    return startTimeEndIndex;
  }

  private final class Values {
    private final String row;

    private String pid = null;
    private String groupPid = null;
    private Time startTime = null;

    private Values(String row) {
      this.row = row;
    }

    public String toString() {
      return "[pid=" + getPid() + ",groupPid=" + getGroupPid() + ",startTime="
          + getStartTime() + ",row=" + row + "]";
    }

    String getPid() {
      if (pid != null) {
        return pid;
      }
      pid = row.substring(getPidStartIndex(), getPidEndIndex()).trim();
      return pid;
    }

    String getGroupPid() {
      if (groupPid != null) {
        return groupPid;
      }
      groupPid = row.substring(getGroupPidStartIndex(), getGroupPidEndIndex())
          .trim();
      return groupPid;
    }

    Time getStartTime() {
      if (startTime != null) {
        return startTime;
      }
      startTime = new Time(row.substring(getStartTimeStartIndex(),
          getStartTimeEndIndex()).trim());
      return startTime;
    }
  }

  public class Row {
    private final PsParam psParam;

    private int index = -1;

    Row(PsParam psParam) {
      this.psParam = psParam;
    }

    /**
     * Find the process which matches the pid.
     * @param command
     * @param pid
     * @return
     */
    public boolean find(String pid) {
      index = psParam.findRow(pid);
      if (index != -1) {
        return true;
      }
      return false;
    }

    public String getGroupPid() {
      return psParam.getGroupPid(index);
    }

    public Time getStartTime() {
      return psParam.getStartTime(index);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.6  2009/05/05 00:14:07  sueh
 * <p> bug# 1207 In findRow adding prints to get more information on Mac.
 * <p>
 * <p> Revision 1.5  2009/04/15 16:52:05  sueh
 * <p> bug# 1207 Removed annoying print statement.
 * <p>
 * <p> Revision 1.4  2009/04/13 22:24:23  sueh
 * <p> bug# 1207 Placing ssh command before ps command when necessary.
 * <p>
 * <p> Revision 1.3  2006/06/07 20:37:28  sueh
 * <p> bug# 766 Added ps command for mac
 * <p>
 * <p> Revision 1.2  2006/06/06 17:16:28  sueh
 * <p> bug# 766 Implementing ps for windows.  Add a boolean for each column, so that column reading
 * <p> works with any OS.  When the OS is windows change the command and
 * <p> change column booleans.
 * <p>
 * <p> Revision 1.1  2006/06/05 16:16:02  sueh
 * <p> bug# 766 Class to create ps commands and interprete their output.
 * <p> </p>
 */
