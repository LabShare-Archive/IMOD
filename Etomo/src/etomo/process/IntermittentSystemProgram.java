package etomo.process;

import java.io.BufferedReader;
import java.io.IOException;

import etomo.BaseManager;
import etomo.type.AxisID;

/**
 * <p>Description:  Runs a command using SystemProgram.  Keeps standard input
 * open.  Sends a string through standard input at intervals.
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
public class IntermittentSystemProgram {
  public static final String rcsid = "$Id$";

  private final String outputKeyPhrase;
  private final SystemProgram program;
  private final boolean useStartCommand;
  private final AxisID axisID;

  private boolean debug = false;

  private IntermittentSystemProgram(BaseManager manager, String propertyUserDir,
      String[] cmdArray, AxisID axisID, String outputKeyPhrase, boolean useStartCommand) {
    program = new SystemProgram(manager, propertyUserDir, cmdArray, axisID);
    program.setCollectOutput(false);
    this.outputKeyPhrase = outputKeyPhrase;
    this.useStartCommand = useStartCommand;
    this.axisID = axisID;
  }

  public static IntermittentSystemProgram getStartInstance(BaseManager manager,
      String propertyUserDir, String[] startCmdArray, AxisID axisID,
      String outputKeyPhrase) {
    return new IntermittentSystemProgram(manager, propertyUserDir, startCmdArray, axisID,
        outputKeyPhrase, true);
  }

  /**
   * IntermittentCommand will be split on whitespace.  It must not contain any
   * whitespace in the directory path.
   * @param propertyUserDir
   * @param intermittentCommand
   * @param axisID
   * @param outputKeyPhrase
   * @return
   */
  public static IntermittentSystemProgram getIntermittentInstance(BaseManager manager,
      String propertyUserDir, String intermittentCommand, AxisID axisID,
      String outputKeyPhrase) {

    return new IntermittentSystemProgram(manager, propertyUserDir,
        intermittentCommand.split("\\s+"), axisID, outputKeyPhrase, false);
  }

  boolean useStartCommand() {
    return useStartCommand;
  }

  OutputBufferManager newOutputBufferManager(BufferedReader cmdBuffer) {
    OutputBufferManager bufferManager = new OutputBufferManager(cmdBuffer,
        outputKeyPhrase);
    bufferManager.setCollectOutput(false);
    return bufferManager;
  }

  OutputBufferManager newErrorBufferManager(BufferedReader cmdBuffer) {
    OutputBufferManager bufferManager = new OutputBufferManager(cmdBuffer);
    bufferManager.setCollectOutput(false);
    return bufferManager;
  }

  /**
   * Clear stderr.  Because stderr.collectionOutput is false, this can be done
   * by running stderr.get();
   */
  void clearStdError() {
    if (program != null) {
      program.clearStdError();
    }
  }

  public boolean isDone() {
    return program.isDone();
  }

  public boolean isStarted() {
    return program.isStarted();
  }

  void destroy() {
    program.destroy();
  }

  public String[] getStdError() {
    return program.getStdError();
  }

  void setAcceptInputWhileRunning(boolean acceptInputWhileRunning) {
    program.setAcceptInputWhileRunning(acceptInputWhileRunning);
  }

  public void setCurrentStdInput(String input) throws IOException {
    program.setCurrentStdInput(input);
  }

  void start() {
    new Thread(program).start();
  }

  /**
   * Get the standard output from the execution of the program.
   * @return String[] An array of strings containing the standard output from
   * the program.  Each line of standard out is stored in a String.
   */
  public String[] getStdOutput(IntermittentProcessMonitor monitor) {
    return program.getStdOutput(monitor);
  }

  /**
   * Get the standard error from the execution of the program.
   * @return String[] An array of strings containing the standard error from
   * the program.  Each line of standard err is stored in a String.
   */
  public String[] getStdError(IntermittentProcessMonitor monitor) {
    return program.getStdError(monitor);
  }

  public void msgDroppedMonitor(IntermittentProcessMonitor monitor) {
    program.dropStdOutputListener(monitor);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.23  2010/04/29 01:35:05  sueh
 * <p> Fixed a null pointer exception in clearStdError.
 * <p>
 * <p> Revision 1.22  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.21  2010/01/11 23:54:58  sueh
 * <p> bug# 1299 Passing in axisID and managerKey.
 * <p>
 * <p> Revision 1.20  2009/04/13 22:30:53  sueh
 * <p> bug# 1207 Added documentation.
 * <p>
 * <p> Revision 1.19  2009/03/17 00:41:41  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.18  2008/02/16 01:47:18  sueh
 * <p> bug# 1080 In clearStdError calling OutputBufferManager.clear.
 * <p>
 * <p> Revision 1.17  2007/09/27 20:19:54  sueh
 * <p> bug# 1044 Changed IntermittentSystemProgram to have the option of not
 * <p> running a start command.  It this case, this intermittent command is set that the
 * <p> instance is run multiple times.  No longer extending SystemProgram as this did
 * <p> not seem flexible enough.
 * <p>
 * <p> Revision 1.16  2007/05/26 00:25:58  sueh
 * <p> bug# 964 Removed setDebug call.  It was causing a compile error on
 * <p> ashtray.
 * <p>
 * <p> Revision 1.15  2007/05/25 00:22:47  sueh
 * <p> bug# 994 Added functions clearStdError and getStdError.
 * <p>
 * <p> Revision 1.14  2006/06/30 19:59:31  sueh
 * <p> bug# 877 Fixed null pointer exception in msgDroppedMonitor.
 * <p>
 * <p> Revision 1.13  2005/09/14 20:26:10  sueh
 * <p> bug# 532 Added drop() to remove a monitor from the listener list.  It is
 * <p> important for the called to prevent any last-minute gets after the drop() is
 * <p> called; the get() will add the monitor back to the listener list, if it is sent
 * <p> after the drop().
 * <p>
 * <p> Revision 1.12  2005/09/10 02:12:40  sueh
 * <p> bug# 532 Handling stderr differently from stdout.  Stderr does not filter on
 * <p> key phrase.
 * <p>
 * <p> Revision 1.11  2005/09/10 01:49:18  sueh
 * <p> bug# 532 Changed IntermittentSystemProgram to
 * <p> IntermittentBackgroundProcess.  Made intermittentSystemProgram a child
 * <p> of SystemProgram.  Made OutputBufferManager in independent class
 * <p> instead of being inside SystemProgram.  IntermittentSystemProgram can
 * <p> use OutputBufferManager to do things only necessary for intermittent
 * <p> programs, such as deleting standard output after it is processed and
 * <p> keeping separate lists of standard output for separate monitors.
 * <p>
 * <p> Revision 1.10  2005/09/09 21:25:16  sueh
 * <p> bug# 532 Made LoadAverageParam an n'ton (one for each computer) so
 * <p> that there aren't IntermittentSystemPrograms then computers.  This allows
 * <p> IntermittentSystemProgram to be used for other things and conforms to
 * <p> it definition of having one instance per IntermittentCommand, instead of
 * <p> one instance per computer.  Synchronizing start() and stop().  In run(),
 * <p> created a local SystemProgram. Saving the local SystemProgram to the
 * <p> member SystemProgram each time a new local one is created.
 * <p> Exit from the intermittent command when the local SystemProgram isn't
 * <p> equal to the member SystemProgram.  This makes the program exit
 * <p> quickly when there is alot of starting and stopping of commands.
 * <p>
 * <p> Revision 1.9  2005/09/07 20:34:24  sueh
 * <p> bug# 532 In getStdOutput() return the entire stdOutput structure.
 * <p>
 * <p> Revision 1.8  2005/09/02 18:58:42  sueh
 * <p> bug# 532 removed a null pointer exception problem from run().
 * <p>
 * <p> Revision 1.7  2005/09/01 17:50:10  sueh
 * <p> bug# 532 Added setCurrentStdInput() to allow load average monitor to
 * <p> confirm the connect, the first time a connect between computers is made.
 * <p>
 * <p> Revision 1.6  2005/08/31 17:16:33  sueh
 * <p> bug# 532 Allow monitor to call the stop(SystemProgramMonitor).
 * <p>
 * <p> Revision 1.5  2005/08/30 23:33:50  sueh
 * <p> bug# 532 Telling monitors that intermittent command was sent.
 * <p>
 * <p> Revision 1.4  2005/08/30 18:43:20  sueh
 * <p> bug# 532 When the intermitent command throughs a IOException.  Call
 * <p> intermittentCommandFailed in all monitors.
 * <p>
 * <p> Revision 1.3  2005/08/27 22:29:31  sueh
 * <p> bug# 532 Handle IOException from SystemProgram.setCurrentStdInput()
 * <p> so that failed intermittent commands can halted.
 * <p>
 * <p> Revision 1.2  2005/08/24 00:20:47  sueh
 * <p> bug# 532 removed the running member variable.  Only needed the stopped
 * <p> member variable.
 * <p>
 * <p> Revision 1.1  2005/08/22 16:31:15  sueh
 * <p> bug# 532 Runs a command using SystemProgram.  Keeps standard
 * <p> input open.   Sends a string through standard input at intervals.
 * <p> </p>
 */
