package etomo.process;

import java.util.ArrayList;
import java.util.HashMap;

import etomo.BaseManager;
import etomo.comscript.IntermittentCommand;
import etomo.storage.CpuAdoc;
import etomo.type.AxisID;
import etomo.ui.swing.LoadDisplay;
import etomo.util.HashedArray;

/**
 * <p>Description: </p>
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
 * 
 * <p> $Log$
 * <p> Revision 1.7  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.6  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.5  2010/01/11 23:55:49  sueh
 * <p> bug# 1299 Made CpuAdoc a singleton without knowledge of manager or
 * <p> axis.
 * <p>
 * <p> Revision 1.4  2009/04/13 22:32:48  sueh
 * <p> bug# 1207 Using FailureReason instead of FailureReasonInterface.
 * <p>
 * <p> Revision 1.3  2009/03/17 00:41:48  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.2  2008/07/19 00:25:06  sueh
 * <p> bug# 1125 Making it easier to access CpuAdoc by not passing the
 * <p> manager to it; all it needs is the current directory.
 * <p>
 * <p> Revision 1.1  2007/09/27 20:25:10  sueh
 * <p> bug# 1044 Added QueuechunkLoadMonitor, which has mostly the same functionality as LoadAverageMonitor, except for the output and how it is used.  Factoring out common functionality into a new parent class, LoadMonitor.
 * <p> </p>
 */
public abstract class LoadMonitor implements IntermittentProcessMonitor, Runnable {
  public static final String rcsid = "$Id$";

  final LoadDisplay display;
  final boolean usersColumn;

  private HashedArray programs = new HashedArray();
  //stopped:  true when the run() is not executing.  Set at the end of the run
  //program.  Also set externally to stop the run() program.
  private boolean stopped = true;

  abstract void processData(ProgramState programState);

  public LoadMonitor(LoadDisplay display, AxisID axisID, BaseManager manager) {
    this.display = display;
    usersColumn = CpuAdoc.INSTANCE.isUsersColumn(manager, axisID, manager
        .getPropertyUserDir());
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
    ProgramState programState = (ProgramState) programs.get(program.getCommand()
        .getComputer());
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
   * returns false if the monitor is stopped, the program is unknown, or if it
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

  public void msgIntermittentCommandFailed(IntermittentCommand command) {
    String key = command.getComputer();
    if (programs.containsKey(key)) {
      ProgramState program = (ProgramState) programs.get(key);
      FailureReason failureReason = program.getFailureReason();
      display.msgLoadFailed(key, failureReason.getReason(), failureReason.getTooltip());
      program.fail();
      program.addToRestarter();
    }
  }

  public void restart() {
    ProcessRestarter.INSTANCE.restart();
  }

  public void msgSentIntermittentCommand(IntermittentCommand command) {
    ProgramState programState = (ProgramState) programs.get(command.getComputer());
    if (programState == null) {
      return;
    }
    programState.incrementWaitForCommand();
  }

  static final class ProgramState {
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
      return "[program=" + program + ",\nwaitForCommand=" + waitForCommand + ","
          + super.toString() + "]";
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

    public void addToRestarter() {
      ProcessRestarter.INSTANCE.addProcess(program);
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
    private synchronized FailureReason getFailureReason() {
      //There was a failure, so failureReason must not be null
      FailureReason failureReason = program.getFailureReason();
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
        return program.getFailureReason();
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
}
