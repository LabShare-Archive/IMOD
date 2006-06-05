package etomo.process;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.ui.ParallelProgressDisplay;
import etomo.util.DatasetFiles;
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
public class ProcesschunksProcessMonitor implements OutfileProcessMonitor,
    ParallelProcessMonitor {
  public static final String rcsid = "$Id$";

  private final static String TITLE = "Processchunks";

  private static boolean debug = false;

  private final EtomoNumber nChunks = new EtomoNumber();
  private final EtomoNumber chunksFinished = new EtomoNumber();

  private final BaseManager manager;
  private final AxisID axisID;
  private final ParallelProgressDisplay parallelProgressDisplay;
  private final String rootName;
  private final String computerList;
  private final ProcessMessages messages = ProcessMessages
      .getInstanceForParallelProcessing();

  private boolean setProgressBarTitle = false;//turn on to changed the progress bar title
  private boolean reassembling = false;
  private ProcessEndState endState = null;
  private File commandsPipe = null;
  private BufferedWriter commandsWriter = null;
  private boolean useCommandsPipe = true;
  private BufferedReader bufferedReader = null;
  private boolean processRunning = true;
  private boolean pausing = false;
  private boolean killing = false;
  private File processOutputFile = null;
  private String pid = null;

  /**
   * Default constructor
   * @param manager
   * @param axisID
   * @param process
   */
  public ProcesschunksProcessMonitor(BaseManager manager, AxisID axisID,
      ParallelProgressDisplay parallelProgressDisplay, String rootName,
      String computerList) {
    this.manager = manager;
    this.axisID = axisID;
    this.parallelProgressDisplay = parallelProgressDisplay;
    this.rootName = rootName;
    this.computerList = computerList;
    debug = EtomoDirector.getInstance().isDebug();
    parallelProgressDisplay.setParallelProcessMonitor(this);
  }

  public void setProcess(SystemProcessInterface process) {
  }

  public void run() {
    //make sure commmandsPipe is deleted and enable its use
    deleteCommandsPipe(true);
    parallelProgressDisplay.msgStartingProcessOnSelectedComputers();
    nChunks.set(0);
    chunksFinished.set(0);
    initializeProgressBar();
    try {
      while (processRunning) {
        Thread.sleep(2000);
        if (updateState() || setProgressBarTitle) {
          updateProgressBar();
        }
      }
    }
    catch (InterruptedException e) {
      endMonitor(ProcessEndState.DONE);
    }
    catch (IOException e) {
      endMonitor(ProcessEndState.FAILED);
      e.printStackTrace();
    }
    parallelProgressDisplay.setParallelProcessMonitor(null);
    //make sure commmandsPipe is deleted and disable its use
    deleteCommandsPipe(false);
  }

  private void endMonitor(ProcessEndState endState) {
    setProcessEndState(endState);
    processRunning = false;//the only place that this should be changed
  }

  public String getPid() {
    return pid;
  }

  protected final boolean updateState() throws IOException {
    if (bufferedReader == null) {
      bufferedReader = getProcessOutputBufferedReader();
    }
    boolean returnValue = false;
    if (bufferedReader == null) {
      return returnValue;
    }
    String line;
    boolean failed = false;
    while ((line = bufferedReader.readLine()) != null) {
      line = line.trim();
      //get the first pid
      if (pid == null && line.startsWith("Shell PID:")) {
        String[] array = line.split("\\s+");
        if (array.length == 3) {
          pid = array[2].trim();
        }
      }
      //System.out.println(line);
      messages.addProcessOutput(line);
      if (messages.isError()) {
        //Set failure boolean but continue to add all the output lines to
        //messages.
        failed = true;
      }
      //If it got an error message, then it seems like the best thing to do is
      //stop processing.
      if (failed) {
        continue;
      }
      if (line.endsWith("to reassemble")) {
        //handle all chunks finished, starting the reassemble
        Utilities.timestamp(TITLE, rootName, "reassembling");
        //all chunks finished, turn off pause
        if (endState == ProcessEndState.PAUSED) {
          endState = null;
        }
        reassembling = true;
        setProgressBarTitle = true;
        returnValue = true;
      }
      else if (line.indexOf("BAD COMMAND IGNORED") != -1) {
        throw new IllegalStateException("Bad command sent to processchunks\n"
            + line);
      }
      else if (line.equals("Finished reassembling")) {
        endMonitor(ProcessEndState.DONE);
      }
      else if (line
          .equals("When you rerun with a different set of machines, be sure to use")) {
        endMonitor(ProcessEndState.KILLED);
      }
      else if (line
          .equals("All previously running chunks are done - exiting as requested")) {
        endMonitor(ProcessEndState.PAUSED);
      }
      else {
        String[] strings = line.split("\\s+");
        //set nChunks and chunksFinished
        if (strings.length > 2 && line.endsWith("DONE SO FAR")) {
          if (!nChunks.equals(strings[2])) {
            nChunks.set(strings[2]);
            setProgressBarTitle = true;
          }
          chunksFinished.set(strings[0]);
          returnValue = true;
        }
        else if (strings.length > 1) {
          if (line.startsWith("Dropping")) {
            //handle a dropped CPU
            parallelProgressDisplay.msgDropped(strings[1], "not responding");
          }
          //handle a finished chunk
          else if (strings[1].equals("finished")) {
            parallelProgressDisplay.addSuccess(strings[3]);
          }
          //handle a failed chunk
          else if (strings[1].equals("failed")) {
            parallelProgressDisplay.addRestart(strings[3]);
          }
        }
      }
    }
    if (failed) {
      endMonitor(ProcessEndState.FAILED);
      returnValue = false;
    }
    return returnValue;
  }

  /**
   * set end state
   * @param endState
   */
  public synchronized final void setProcessEndState(ProcessEndState endState) {
    this.endState = ProcessEndState.precedence(this.endState, endState);
  }

  public final ProcessEndState getProcessEndState() {
    return endState;
  }

  public final ProcessMessages getProcessMessages() {
    return messages;
  }

  private void initializeProgressBar() {
    setProgressBarTitle();
    manager.getMainPanel().setProgressBarValue(chunksFinished.getInt(),
        "Starting...", axisID);
  }

  private void updateProgressBar() {
    if (setProgressBarTitle) {
      setProgressBarTitle = false;
      setProgressBarTitle();
    }
    manager.getMainPanel().setProgressBarValue(chunksFinished.getInt(),
        getStatusString(), axisID);
  }

  private void setProgressBarTitle() {
    StringBuffer title = new StringBuffer(TITLE);
    if (rootName != null) {
      title.append(" " + rootName);
    }
    if (reassembling) {
      title.append(":  reassembling");
    }
    else if (killing) {
      title.append(" - killing:  exiting current chunks");
    }
    else if (pausing) {
      title.append(" - pausing:  finishing current chunks");
    }
    manager.getMainPanel().setProgressBar(title.toString(), nChunks.getInt(),
        axisID, !reassembling && !killing);
  }

  public void kill(SystemProcessInterface process, AxisID axisID) {
    try {
      writeCommand("Q");
      parallelProgressDisplay.msgKillingProcess();
      killing = true;
      setProgressBarTitle = true;
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  public void pause(SystemProcessInterface process, AxisID axisID) {
    try {
      writeCommand("P");
      parallelProgressDisplay.msgPausingProcess();
      pausing = true;
      setProgressBarTitle = true;
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

  public String getStatusString() {
    return chunksFinished + " of " + nChunks + " completed";
  }

  public final void drop(String computer) {
    if (computerList.indexOf(computer) != -1) {
      if (EtomoDirector.getInstance().isDebug()) {
        System.err.println("try to drop " + computer);
      }
      try {
        writeCommand("D " + computer);
        setProgressBarTitle = true;
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  /**
   * make sure the commandsPipe file is deleted and enable/disable its use
   * synchronized with createCommandsWriter
   * synchronization is for useCommandsPipe and commmandsPipe
   * @param startup
   */
  private synchronized final void deleteCommandsPipe(boolean enable) {
    if (!enable) {
      //turn off useCommandsPipe to prevent use of commandsPipe
      useCommandsPipe = false;
    }
    //delete the commands pipe even if it was never created (just to be sure)
    if (commandsPipe == null) {
      commandsPipe = DatasetFiles.getCommandsFile(manager, rootName);
    }
    commandsPipe.delete();
    if (enable) {
      //turn on useCommandsPipe to enable use of commandsPipe
      useCommandsPipe = true;
    }
  }

  /**
   * creates commandsWriter and, if necessary, commandsPipe and the commandsPipe
   * file.
   * synchronized with deleteCommandsPipe
   * synchronization is for useCommandsPipe and commmandsPipe
   * @return true if commandsWriter can be used
   * @throws IOException
   */
  private synchronized final boolean createCommandsWriter() throws IOException {
    if (!useCommandsPipe) {
      commandsWriter = null;
      return false;
    }
    if (commandsWriter != null) {
      return true;
    }
    if (commandsPipe == null) {
      commandsPipe = DatasetFiles.getCommandsFile(manager, rootName);
    }
    commandsPipe.createNewFile();
    commandsWriter = new BufferedWriter(new FileWriter(commandsPipe));
    return true;
  }

  /**
   * create commandsWriter if necessary, write command, add newline, flush
   * @param command
   * @throws IOException
   */
  private final void writeCommand(String command) throws IOException {
    if (!createCommandsWriter()) {
      return;
    }
    commandsWriter.write(command);
    commandsWriter.newLine();
    commandsWriter.flush();
  }

  public final AxisID getAxisID() {
    return axisID;
  }

  public final boolean isProcessRunning() {
    return processRunning;
  }

  /**
   * make sure process output file is new and set processOutputFile.  This
   * function should be first run before the process starts.
   */
  private synchronized final void makeProcessOutputFile() {
    if (processOutputFile == null) {
      processOutputFile = DatasetFiles.getOutFile(manager,
          ProcessName.PROCESSCHUNKS.toString(), axisID);
      //delete it the first time to avoid looking at a file from a previous run
      processOutputFile.delete();
    }
  }

  public final String getProcessOutputFileName() {
    makeProcessOutputFile();
    return processOutputFile.getName();
  }

  private final BufferedReader getProcessOutputBufferedReader() {
    makeProcessOutputFile();
    if (!processOutputFile.exists()) {
      return null;
    }
    try {
      return new BufferedReader(new FileReader(processOutputFile));
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      return null;
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.17  2006/01/31 20:45:04  sueh
 * <p> bug# 521 Added the process to combine monitor.  This allows the last
 * <p> ProcessResultDisplay used by the monitor to be assigned to the process.
 * <p>
 * <p> Revision 1.16  2006/01/06 23:17:08  sueh
 * <p> bug# 795 Fixed a bug in updateState where it ends the monitor as soon as
 * <p> it sees an error message.  This means that the error messages popped
 * <p> up to the user where incomplete.  When an error is found, collect all the
 * <p> messages and then end the monitor.
 * <p>
 * <p> Revision 1.15  2005/11/29 22:24:10  sueh
 * <p> bug# 744 Stop putting processchunks output into the error log, since its
 * <p> going into the .out file.
 * <p>
 * <p> Revision 1.14  2005/11/19 02:36:13  sueh
 * <p> bug# 744 Converted to a detached program.  Managing the process
 * <p> output file.  No longer using the process.  Reading the process output file
 * <p> to update the program display.  Handling kills and pauses in
 * <p> updateState().  Handling errors and chunk errors with ProcessMessages.
 * <p>
 * <p> Revision 1.13  2005/11/04 00:53:00  sueh
 * <p> fixed pausing message
 * <p>
 * <p> Revision 1.12  2005/10/21 19:55:54  sueh
 * <p> bug# 742 Sending text from standard error to the error log.
 * <p>
 * <p> Revision 1.11  2005/09/27 21:16:18  sueh
 * <p> bug# 532 Temporarily printing the processchunks output to the error log
 * <p> without the debug setting.
 * <p>
 * <p> Revision 1.10  2005/09/10 01:52:41  sueh
 * <p> bug# 532 Setting the reason for all dropped computers to "not responding"
 * <p> to distinguish it from load average timeouts.
 * <p>
 * <p> Revision 1.9  2005/09/09 21:42:22  sueh
 * <p> bug# 532 Throwing an exception if a command is sent that processchunks
 * <p> doesn't recognize.
 * <p>
 * <p> Revision 1.8  2005/09/07 20:51:06  sueh
 * <p> bug# 532 Added commandsPipe and commandsWriter to send commands
 * <p> for processchunks to a file.
 * <p>
 * <p> Revision 1.7  2005/09/01 17:54:58  sueh
 * <p> bug# 532 handle multiple drop reasons.  Fix drop function:  use interrupt
 * <p> signal only when the computer is recognized.
 * <p>
 * <p> Revision 1.6  2005/08/30 22:41:45  sueh
 * <p> bug# 532 Added error log print statement to drop().
 * <p>
 * <p> Revision 1.5  2005/08/30 18:50:23  sueh
 * <p> bug# 532 Added drop(String computer).  Added dropComputer which is set
 * <p> while waiting for the interrupt to happen.  Added computerList so that
 * <p> the monitor can avoid interrupting for computers that processchunks isn't
 * <p> using.  Added code to updateState() to send "D computer_name" to
 * <p> processchunks.
 * <p>
 * <p> Revision 1.4  2005/08/27 22:31:47  sueh
 * <p> bug# 532 In updateState() look for CHUNK ERROR: and save the most
 * <p> recent one found.  Return the last chunk error message with
 * <p> getErrorMessage().
 * <p>
 * <p> Revision 1.3  2005/08/22 17:03:43  sueh
 * <p> bug# 532 Added getStatusString() to implement ProcessMonitor.  The
 * <p> status string is used to add more information to the progress bar when
 * <p> the process ends.  It is currently being used only for pausing
 * <p> processchunks.  Added "pausing" string to progress title as soon as a
 * <p> pause is detected since pausing takes a lot of time.
 * <p>
 * <p> Revision 1.2  2005/08/15 18:23:54  sueh
 * <p> bug# 532  Added kill and pause functions to implement ProcessMonitor.
 * <p> Both kill and pause signal interrupt.  Change updateState to handle the
 * <p> interrupt message and send the correct string, based on whether a kill or
 * <p> a pause was requested.
 * <p>
 * <p> Revision 1.1  2005/08/04 19:46:15  sueh
 * <p> bug# 532 Class to monitor processchunks.  Monitors the standard output.
 * <p> Sends updates to the progress panel and to a parallel process display.
 * <p> </p>
 */
