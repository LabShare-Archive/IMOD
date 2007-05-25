package etomo.process;

import java.util.ArrayList;
import java.util.HashMap;

import etomo.comscript.IntermittentCommand;
import etomo.storage.CpuAdoc;
import etomo.type.AxisID;
import etomo.type.FailureReasonInterface;
import etomo.ui.LoadAverageDisplay;
import etomo.util.HashedArray;
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

public class LoadAverageMonitor implements IntermittentProcessMonitor, Runnable {
  public static final String rcsid = "$Id$";

  private static final String OUTPUT_KEY_PHRASE = "load average";
  private static final String OUTPUT_KEY_PHRASE_WINDOWS = "Percent CPU usage";

  private final LoadAverageDisplay display;
  private final boolean usersColumn;

  private HashedArray programs = new HashedArray();
  //stopped:  true when the run() is not executing.  Set at the end of the run
  //program.  Also set externally to stop the run() program.
  private boolean stopped = true;
  private boolean allowRestarts = false;

  public LoadAverageMonitor(LoadAverageDisplay display, AxisID axisID) {
    this.display = display;
    usersColumn = CpuAdoc.getInstance(axisID).isUsersColumn();
  }

  public void run() {
    try {
      while (!stopped) {
        boolean programsStopped = true;
        //update the output on the display from each of the running programs.
        for (int i = 0; i < programs.size(); i++) {
          ProgramState programState = (ProgramState) programs.get(i);
          if (!stopped && !programState.isStopped()) {
            programsStopped = false;
            processData(programState);
            if (programState.getWaitForCommand() > 12) {
              programState.setWaitForCommand(0);
              msgIntermittentCommandFailed(programState.getCommand());
            }
          }
        }
        if (programsStopped) {
          stopped = true;
        }
        Thread.sleep(1000);
      }
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
    stopped = true;
  }

  /**
   * set stopped to true
   */
  public void stop() {
    this.stopped = true;
  }

  /**
   * Sets the stopping member variable in the program state.  This just sets a flag in ProgramState
   * and doesn't affect the program.  Check all the program states.  If they are
   * all stopping or the program is stopped, then set stopped to true.
   * @param program
   */
  public void stopMonitoring(IntermittentBackgroundProcess program) {
    ProgramState programState = (ProgramState) programs.get(program
        .getCommand().getComputer());
    programState.setStopMonitoring(true);
    boolean programsStopped = true;
    for (int i = 0; i < programs.size(); i++) {
      programState = (ProgramState) programs.get(i);
      if (!programState.isStopMonitoring() && !programState.isStopped()) {
        programsStopped = false;
        break;
      }
    }
    if (programsStopped) {
      stopped = true;
    }
  }

  /**
   * returns true if the monitor is stopped, the program is unknown, or if it
   * has received a stop monitoring command from the program.
   * @param program
   * @return
   */
  public boolean isMonitoring(IntermittentBackgroundProcess program) {
    if (stopped) {
      return false;
    }
    String key = program.getCommand().getComputer();
    if (key == null) {
      return false;
    }
    return !((ProgramState) programs.get(key)).isStopMonitoring();
  }

  public synchronized void setProcess(IntermittentBackgroundProcess program) {
    String key = program.getCommand().getComputer();
    ProgramState programState = (ProgramState) programs.get(key);
    if (programState == null) {
      programs.add(key, new ProgramState(program));
    }
    else {
      programState.setStopMonitoring(false);
      programState.setWaitForCommand(0);
    }
    if (stopped) {
      stopped = false;
      new Thread(this).start();
    }
    display.msgStartingProcess(key, FailureReason.COMPUTER_DOWN.getReason(),
        FailureReason.LOGIN_FAILED.getReason());
  }

  /**
   * Processes the output of programState.program.  Sets the results in display.
   * This function is meant to be called over and over while
   * programState.program is running.
   * @param programState
   */
  private void processData(ProgramState programState) {
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
      display.setCPUUsage(programState.getCommand().getComputer(), cpuUsage);
    }
    else {
      if (load1 == -1) {
        return;
      }
      display.setLoadAverage(programState.getCommand().getComputer(), load1,
          load5, users, programState.getUserList());
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

  public void msgIntermittentCommandFailed(IntermittentCommand command) {
    String key = command.getComputer();
    if (programs.containsKey(key)) {
      ProgramState program = (ProgramState) programs.get(key);
      FailureReasonInterface failureReason =program.getFailureReason();
      display.msgLoadAverageFailed(key, failureReason.getReason(),failureReason.getTooltip());
      program.fail();
      if (allowRestarts) {
        program.restart();
      }
    }
  }

  public void msgSentIntermittentCommand(IntermittentCommand command) {
    ProgramState programState = (ProgramState) programs.get(command
        .getComputer());
    if (programState == null) {
      return;
    }
    programState.incrementWaitForCommand();
  }

  private static final class ProgramState {
    private final IntermittentBackgroundProcess program;
    //userMap:  convenience variable for counting the number of different users logged into a computer.
    private final HashMap userMap = new HashMap();
    private final ArrayList userList = new ArrayList();

    private int waitForCommand = 0;
    private boolean stopMonitoring = false;
    private boolean receivedData = false;

    private ProgramState(IntermittentBackgroundProcess program) {
      this.program = program;
    }

    public String toString() {
      return "[program=" + program + ",\nwaitForCommand=" + waitForCommand
          + "," + super.toString() + "]";
    }

    private String[] getStdError() {
      return program.getStdError();
    }

    boolean isStopped() {
      return program.isStopped();
    }

    void setStopMonitoring(boolean stopMonitoring) {
      this.stopMonitoring = stopMonitoring;
    }

    boolean isStopMonitoring() {
      return stopMonitoring;
    }

    void fail() {
      program.fail();
    }

    int getWaitForCommand() {
      return waitForCommand;
    }

    void incrementWaitForCommand() {
      waitForCommand++;
    }

    void setWaitForCommand(int waitForCommand) {
      this.waitForCommand = waitForCommand;
    }

    IntermittentCommand getCommand() {
      return program.getCommand();
    }

    void restart() {
      program.restart();
    }

    String[] getStdOutput(IntermittentProcessMonitor monitor) {
      return program.getStdOutput(monitor);
    }

    /**
     * sets receivedData to true, resets failureReason, and clears stderr.
     */
    void msgReceivedData() {
      if (receivedData) {
        return;
      }
      receivedData = true;
      clearStdError();
    }

    void clearStdError() {
      program.clearStdError();
    }

    /**
     * Provide a failure reason if the state of stderr shows that the computer
     * being ssh'ed to is down or the authentication failed.
     * Sets the failure reason to non-null
     * @param program
     * @return
     */
    private synchronized FailureReasonInterface getFailureReason() {
      //There was a failure, so failureReason must not be null
      FailureReasonInterface failureReason = program.getFailureReason();
      if (failureReason == null) {
        program.setFailureReason(FailureReason.UNKOWN);
      }
      //If data has already been received, this is an unknown error
      if (receivedData) {
        program.setFailureReason(FailureReason.UNKOWN);
        return FailureReason.UNKOWN;
      }
      //If stderr is empty but receivedData is false, return the existing failure reason
      String[] stderr = getStdError();
      if (stderr == null || stderr.length == 0) {
        return failureReason;
      }
      //Try to set a failure reason from the information in stderr
      boolean connectionSucceeded = false;
      failureReason = FailureReason.UNKOWN;
      for (int i = 0; i < stderr.length; i++) {
        if (!connectionSucceeded) {
          String line = stderr[i].toLowerCase();
          if (line.indexOf("connecting to") != -1) {
            failureReason = FailureReason.COMPUTER_DOWN;
          }
          else if (line.indexOf("next authentication") != -1) {
            failureReason = FailureReason.LOGIN_FAILED;
          }
          else if (line.indexOf("authentication succeeded") != -1) {
            //Not a connection failure.  Don't know why this failed.
            connectionSucceeded = true;
            failureReason = FailureReason.UNKOWN;
          }
        }
      }
      program.setFailureReason(failureReason);
      return failureReason;
    }

    void clearUsers() {
      userMap.clear();
      userList.clear();
    }

    boolean containsUser(String user) {
      return userMap.containsKey(user);
    }

    void addUser(String user) {
      userMap.put(user, null);
      userList.add(user);
    }

    String getUserList() {
      if (userList.isEmpty()) {
        return null;
      }
      StringBuffer list = new StringBuffer((String) userList.get(0));
      for (int i = 1; i < userList.size(); i++) {
        list.append(',' + (String) userList.get(i));
      }
      return list.toString();
    }
  }

  static final class FailureReason implements FailureReasonInterface{
    static final FailureReason UNKOWN = new FailureReason("",
        "Unable to get the load averages for this computer.");
    static final FailureReason COMPUTER_DOWN = new FailureReason("down",
        "This computer in not running.");
    static final FailureReason LOGIN_FAILED = new FailureReason("no login",
        "You must have an account on this computer.  "
            + "You must also have a passwordless login on this computer.");

    private final String reason;
    private final String tooltip;

    private FailureReason(String reason, String tooltip) {
      this.reason = reason;
      this.tooltip = tooltip;
    }

    public String getReason() {
      return reason;
    }

    public String getTooltip() {
      return tooltip;
    }
  }
}
/**
 * <p> $Log$
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
