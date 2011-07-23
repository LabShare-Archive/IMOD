package etomo.process;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.ui.swing.LoadDisplay;
import etomo.util.Utilities;

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

public class LoadAverageMonitor extends LoadMonitor {
  public static final String rcsid = "$Id$";

  private static final String OUTPUT_KEY_PHRASE = "load average";
  private static final String OUTPUT_KEY_PHRASE_WINDOWS = "Percent CPU usage";

  private final ConstEtomoNumber numberOfProcessorsWindows = EtomoDirector.INSTANCE
      .getNumberOfProcessorsWindows();

  public LoadAverageMonitor(LoadDisplay display, AxisID axisID, BaseManager manager) {
    super(display, axisID, manager);
  }

  /**
   * Processes the output of programState.program.  Sets the results in display.
   * This function is meant to be called over and over while
   * programState.program is running.
   * @param programState
   */
  void processData(ProgramState programState) {
    //System.out.println("processData");
    //process standard out
    String[] stdout = programState.getStdOutput(this);
    if (stdout == null || stdout.length == 0) {
      return;
    }
    programState.msgReceivedData();
    double cpuUsage = -1;
    double load1 = -1;
    double load5 = -1;
    programState.clearUsers();
    int users = 0;
    boolean headerLineFound = false;
    for (int i = 0; i < stdout.length; i++) {
      //System.out.println(stdout[i]);
      if (Utilities.isWindowsOS()) {
        if (stdout[i].indexOf(OUTPUT_KEY_PHRASE_WINDOWS) != -1) {
          programState.setWaitForCommand(0);
          String[] array = stdout[i].trim().split("\\s+");
          cpuUsage = getLoad(array[array.length - 1]);
        }
      }
      else if (stdout[i].indexOf(OUTPUT_KEY_PHRASE) != -1) {
        programState.setWaitForCommand(0);
        String[] array = stdout[i].trim().split("\\s+");
        load1 = getLoad(array[array.length - 3]);
        load5 = getLoad(array[array.length - 2]);
      }
      //no need to total users when the usersColumn is not being displayed
      else if (usersColumn) {
        if (!headerLineFound) {
          //ignore the header line
          headerLineFound = true;
        }
        else {
          //count users
          String[] array = stdout[i].trim().split("\\s+");
          if (!array[0].equals("root") && !programState.containsUser(array[0])) {
            programState.addUser(array[0]);
            users++;
          }
        }
      }
    }
    if (Utilities.isWindowsOS()) {
      if (cpuUsage == -1) {
        return;
      }
      display.setCPUUsage(programState.getCommand().getComputer(), cpuUsage,
          numberOfProcessorsWindows);
    }
    else {
      if (load1 == -1) {
        return;
      }
      display.setLoad(programState.getCommand().getComputer(), load1, load5, users,
          programState.getUserList());
    }
  }

  public final String getOutputKeyPhrase() {
    if (Utilities.isWindowsOS()) {
      return OUTPUT_KEY_PHRASE_WINDOWS;
    }
    //need to get users for linux systems, so don't use the output key phrase to limit process output
    return null;
  }

  private double getLoad(String load) {
    load = load.trim();
    if (load.charAt(load.length() - 1) == ',') {
      return Double.parseDouble(load.substring(0, load.length() - 1));
    }
    return Double.parseDouble(load);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.29  2011/07/23 02:26:57  sueh
 * <p> Bug# 1517 Added getWindowsLoad.
 * <p>
 * <p> Revision 1.28  2011/02/22 04:03:42  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.27  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.26  2009/04/13 22:31:31  sueh
 * <p> bug# 1207 Made FailureReason available to other classes.
 * <p>
 * <p> Revision 1.25  2007/09/27 20:24:56  sueh
 * <p> bug# 1044 Added QueuechunkLoadMonitor, which has mostly the same functionality as LoadAverageMonitor, except for the output and how it is used.  Factoring out common functionality into a new parent class, LoadMonitor.
 * <p>
 * <p> Revision 1.24  2007/07/17 21:06:54  sueh
 * <p> bug# 1018 Getting usersColumn from CpuAdoc.
 * <p>
 * <p> Revision 1.23  2007/05/26 00:28:19  sueh
 * <p> bug# 964 Removed allowRestart.  Added restart() to do restarts with a
 * <p> button press instead of automatically.
 * <p>
 * <p> Revision 1.22  2007/05/25 00:24:19  sueh
 * <p> bug# 994 In ProgramState, add dataReceived and getFailureReason().
 * <p>
 * <p> Revision 1.21  2007/05/21 18:10:13  sueh
 * <p> bug# 992 Added usersColumn.  In processData(), not calculating users
 * <p> when usersColumn is false.
 * <p>
 * <p> Revision 1.20  2007/02/22 20:36:12  sueh
 * <p> bug# 964 Added allowRestarts to make it possible prevent all restarts.  Setting
 * <p> allowRestarts to false for now.
 * <p>
 * <p> Revision 1.19  2006/11/29 00:02:10  sueh
 * <p> bug# 934 Added stopped and functions isMonitoring, stop, and stopMonitoring.
 * <p>
 * <p> Revision 1.18  2006/11/18 00:49:02  sueh
 * <p> bug# 936 Parallel Processing:  added user list tooltip to user column.
 * <p>
 * <p> Revision 1.17  2006/11/08 21:05:53  sueh
 * <p> bug# 936  Linux and Mac:  getOutputKey:  send null as the outputKeyPhrase, so
 * <p> that all output lines are returned.  processData:  process the detail lines of the
 * <p> "w" command and total all the different users on each computer.
 * <p>
 * <p> Revision 1.16  2006/10/18 15:41:15  sueh
 * <p> bug# 929 Changed failure reason to "no connection"
 * <p>
 * <p> Revision 1.15  2006/02/15 22:06:44  sueh
 * <p> bug# 796 Added a key phrase for windows
 * <p>
 * <p> Revision 1.14  2006/02/15 18:51:23  sueh
 * <p> bug# 796 Set wait for command back to 0 when a imodwincpu returns a
 * <p> string
 * <p>
 * <p> Revision 1.13  2006/02/09 23:04:34  sueh
 * <p> bug# 796 Handling load averages in windows
 * <p>
 * <p> Revision 1.12  2006/02/08 03:35:01  sueh
 * <p> bug# 796 Use imodwindcpu instead of w for windows.
 * <p>
 * <p> Revision 1.11  2005/12/01 00:24:54  sueh
 * <p> bug# 775   The command interface is also about distributing commands
 * <p> across multiple computers.  Remove getKey and added getComputer.
 * <p>
 * <p> Revision 1.10  2005/11/19 02:32:02  sueh
 * <p> bug# 744 Changed parallel processing display clearFailureReason
 * <p> function to msgStartingProcess.  This hides the implementation.
 * <p>
 * <p> Revision 1.9  2005/11/14 21:26:49  sueh
 * <p> bug# 762 The internal class is accessing protected functions instead of
 * <p> private variables.
 * <p>
 * <p> Revision 1.8  2005/09/10 01:50:59  sueh
 * <p> bug# 532 Changed IntermittentSystemProgram to
 * <p> IntermittentBackgroundProcess.  Made intermittentSystemProgram a child
 * <p> of SystemProgram.  Made OutputBufferManager in independent class
 * <p> instead of being inside SystemProgram.  IntermittentSystemProgram can
 * <p> use OutputBufferManager to do things only necessary for intermittent
 * <p> programs, such as deleting standard output after it is processed,
 * <p> keeping separate lists of standard output for separate monitors, and
 * <p> setting a key phrase in OutputBufferManager so that only useful lines from
 * <p> standard output will be saved.
 * <p>
 * <p> Revision 1.7  2005/09/09 21:40:56  sueh
 * <p> bug# 532 Checked program.isStopped() before running processData().
 * <p> Set waitForCommand back to 0 everytime it succeeds and when it restarts.
 * <p> This prevents timeouts.  Send a reason to the display when the load
 * <p> average times out.
 * <p>
 * <p> Revision 1.6  2005/09/07 20:38:09  sueh
 * <p> bug# 532 ProcessData(): Looking at the subset of the stdoutput that hasn't
 * <p> been processed.  Handling the first time connection question by sending
 * <p> "yes".
 * <p>
 * <p> Revision 1.5  2005/09/01 17:52:43  sueh
 * <p> bug# 532 Set waitForCommand to 0 after the problem is found.  Clear the
 * <p> load averages on the display when the connection is cut.  Handle first time
 * <p> connections between computers.
 * <p>
 * <p> Revision 1.4  2005/08/31 17:18:24  sueh
 * <p> bug# 532 Handle an unresponsive computer by dropping from
 * <p> processchunks after 12 unresponses.
 * <p>
 * <p> Revision 1.3  2005/08/30 18:44:36  sueh
 * <p> bug# 532 Added intermittentCommandFailed() to handle a failed w
 * <p> command.
 * <p>
 * <p> Revision 1.2  2005/08/24 00:21:58  sueh
 * <p> bug# 532  In processData() changed string used to find the load average
 * <p> line so it would both for Linux and Mac.
 * <p>
 * <p> Revision 1.1  2005/08/22 16:35:18  sueh
 * <p> bug# 532 Monitors a group of load average commands running in
 * <p> IntermittentSystemProgram instances.  Gets results from standard
 * <p> output.  Places results in a LoadAverageDisplay instance.   Runs until it
 * <p> receives a stop call associated with each of the
 * <p> IntermittentSystemProgram instances.
 * <p> </p>
 */
