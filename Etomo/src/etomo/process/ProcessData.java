package etomo.process;

import java.util.Properties;

import etomo.BaseManager;
import etomo.comscript.PsParam;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstStringProperty;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.StringProperty;
import etomo.type.Time;

/**
 * <p>Description:
 * Process data to allow identification of processes that Etomo is no longer
 * managing because it exited after they started.  Allows Etomo to prevent two
 * processes from running on an axis, even when the running process is
 * unmanaged.  Saved in the data file.
 * 
 * Future plans:  Use this data to reconnect the monitor to the process.  It
 * should be possible to reconnect any monitor that relies on a file rather
 * then standard out.</p>
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
public final class ProcessData implements Storable {
  public static final String rcsid = "$Id$";

  private static final String PID_KEY = "PID";
  private static final String GROUP_PID_KEY = "GroupPID";
  private static final String START_TIME_KEY = "StartTime";
  private static final String PROCESS_NAME_KEY = "ProcessName";

  private final EtomoNumber displayKey = new EtomoNumber("DisplayKey");
  private final StringProperty subProcessName = new StringProperty(
      "SubProcessName");
  private final StringProperty subDirName = new StringProperty("SubDirName");

  private final AxisID axisID;
  private final String processDataPrepend;
  private final BaseManager manager;

  private String pid = null;
  private String groupPid = null;
  private Time startTime = null;
  private ProcessName processName = null;
  private boolean doNotLoad = false;

  /**
   * Get an instance of ProcessData which is associated with a process managed
   * by Etomo.  It cannot be loaded from the param file.  This instance will
   * have a fixed process name.
   * @param axisID
   * @param manager
   * @param processName
   * @return
   */
  static ProcessData getManagedInstance(AxisID axisID, BaseManager manager,
      ProcessName processName) {
    ProcessData processData = new ProcessData(axisID, manager);
    processData.processName = processName;
    processData.doNotLoad = true;
    return processData;
  }

  /**
   * Create an instance of ProcessData designed to be loaded from the param
   * file.
   * @param axisID
   * @param manager
   */
  ProcessData(AxisID axisID, BaseManager manager) {
    if (axisID == AxisID.ONLY) {
      axisID = AxisID.FIRST;
    }
    this.axisID = axisID;
    processDataPrepend = "ProcessData" + "." + axisID.getExtension();
    this.manager = manager;
  }

  public void setDisplayKey(ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay != null) {
      displayKey.set(processResultDisplay.getDependencyIndex());
    }
  }

  public void setSubProcessName(String input) {
    subProcessName.set(input);
  }

  public void setSubDirName(String input) {
    subDirName.set(input);
  }

  /**
   * @return true if the pid, groupPid, or startTime is empty.
   */
  public boolean isEmpty() {
    return pid == null || groupPid == null || startTime == null;
  }

  void reset() {
    pid = null;
    groupPid = null;
    startTime = null;
  }

  /**
   * Look for the process data in the ps output.
   * @return true if the process data is found in the ps output, false if the
   * process data is not found or this instance is empty.
   */
  boolean isRunning() {
    if (isEmpty()) {
      return false;
    }
    PsParam param = runPs(pid);
    return param.findRow(pid, groupPid, startTime);
  }

  /**
   * Use the pid to get the process data from the ps output.
   * @param pid
   */
  void setPid(String pid) {
    reset();
    if (pid == null || pid.matches("\\s*+")) {
      return;
    }
    PsParam param = runPs(pid);
    PsParam.Row row = param.getRow();
    if (row.find(pid)) {
      this.pid = pid;
      groupPid = row.getGroupPid();
      startTime = row.getStartTime();
    }
  }

  /**
   * Run ps.  Use the -p pid option.
   * @param pid
   * @return
   */
  private PsParam runPs(String pid) {
    PsParam param = new PsParam(pid);
    SystemProgram ps = new SystemProgram(manager.getPropertyUserDir(), param
        .getCommandArray(), axisID, manager.getManagerKey());
    ps.run();
    param.setOutput(ps.getStdOutput());
    return param;
  }

  public void store(Properties props) {
    store(props, "");
  }

  public String getPid() {
    return pid;
  }

  public ProcessName getProcessName() {
    if (processName == null) {
      return null;
    }
    return processName;
  }

  public String getSubProcessName() {
    return subProcessName.toString();
  }

  public ConstStringProperty getSubDirName() {
    return subDirName;
  }

  public ConstEtomoNumber getDisplayKey() {
    return displayKey;
  }

  private String getPrepend(String prepend) {
    if (prepend == "") {
      prepend = processDataPrepend;
    }
    else {
      prepend += "." + prepend;
    }
    return prepend;
  }

  public String toString() {
    String group = getPrepend("") + ".";
    return "[" + group + PID_KEY + "=" + pid + "," + group + GROUP_PID_KEY
        + "=" + groupPid + "," + group + START_TIME_KEY + "=" + startTime + ","
        + group + PROCESS_NAME_KEY + "=" + processName + "]";
  }

  private void store(Properties props, String prepend) {
    prepend = getPrepend(prepend);
    String group = prepend + ".";
    if (isEmpty()) {
      props.remove(group + PID_KEY);
      props.remove(group + GROUP_PID_KEY);
      props.remove(group + START_TIME_KEY);
      props.remove(group + PROCESS_NAME_KEY);
      displayKey.remove(props, prepend);
      subProcessName.remove(props, prepend);
      subDirName.remove(props, prepend);
    }
    else {
      props.setProperty(group + PID_KEY, pid);
      props.setProperty(group + GROUP_PID_KEY, groupPid);
      props.setProperty(group + START_TIME_KEY, startTime.toString());
      if (processName == null) {
        props.remove(group + PROCESS_NAME_KEY);
      }
      else {
        props.setProperty(group + PROCESS_NAME_KEY, processName.toString());
      }
      displayKey.store(props, prepend);
      subProcessName.store(props, prepend);
      subDirName.store(props, prepend);
    }
  }

  public void load(Properties props) {
    load(props, "");
  }

  private void load(Properties props, String prepend) {
    if (doNotLoad) {
      throw new IllegalStateException(
          "Trying to load into into thread data that belongs to a managed process.");
    }
    reset();
    processName = null;
    prepend = getPrepend(prepend);
    String group = prepend + ".";
    pid = props.getProperty(group + PID_KEY);
    groupPid = props.getProperty(group + GROUP_PID_KEY);
    //load startTime
    String sStartTime = props.getProperty(group + START_TIME_KEY);
    if (sStartTime != null) {
      startTime = new Time(sStartTime);
    }
    //load processName
    String sProcessName = props.getProperty(group + PROCESS_NAME_KEY);
    if (sProcessName != null) {
      processName = ProcessName.getInstance(sProcessName, axisID);
    }
    subProcessName.load(props, prepend);
    subDirName.load(props, prepend);
    displayKey.load(props, prepend);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.7  2008/01/14 21:45:54  sueh
 * <p> bug# 1050 Changed readOnly to doNotLoad because that is the only limitation is
 * <p> on loading.  Added displayKey to get the processResultDisplay generically.  Add
 * <p> subDirNAme and subProcessName for processchunks.
 * <p>
 * <p> Revision 1.6  2007/12/10 22:30:48  sueh
 * <p> bug# 1041 working with the changes in ProcessName.
 * <p>
 * <p> Revision 1.5  2006/10/24 21:39:45  sueh
 * <p> bug# 947 Changed ProcessName.fromString() to getInstance().
 * <p>
 * <p> Revision 1.4  2006/10/10 05:11:44  sueh
 * <p> Added comment
 * <p>
 * <p> Revision 1.3  2006/07/19 20:09:29  sueh
 * <p> bug# 902 GetProcessName():  return ProcessName instead of string.
 * <p>
 * <p> Revision 1.2  2006/06/06 17:19:16  sueh
 * <p> bug# 766 change threadData to processData.
 * <p>
 * <p> Revision 1.1  2006/06/05 16:31:34  sueh
 * <p> bug# 766 A class that can get process data from the ps command and store and
 * <p> load it.
 * <p> </p>
 */
