package etomo.process;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.ProcessEndState;
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
public class ProcesschunksProcessMonitor implements ParallelProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  private final static String TITLE = "Processchunks";
  private final BaseManager manager;
  private final AxisID axisID;
  private final ParallelProgressDisplay parallelProgressDisplay;
  
  private SystemProcessInterface process = null;
  private EtomoNumber nChunks = new EtomoNumber();
  private EtomoNumber chunksFinished = new EtomoNumber();
  private int lastOutputLine = -1;
  private boolean setProgressBarTitle = false;//turn on to changed the progress bar title
  private boolean reassembling = false;
  private ProcessEndState endState = null;
  private final String rootName;
  private final String computerList;
  private String lastChunkError = null;
  private File commandsPipe = null;
  private BufferedWriter commandsWriter = null;
  private boolean useCommandsPipe = true;

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
    parallelProgressDisplay.setParallelProcessMonitor(this);
  }
  
  public final void setProcess(SystemProcessInterface process) {
    this.process = process;
  }
  
  public void run() {
    //make sure commmandsPipe is deleted and enable its use
    deleteCommandsPipe(true);
    nChunks.set(0);
    chunksFinished.set(0);
    initializeProgressBar();
    try {
      while (process == null || !process.isDone()) {
        Thread.sleep(2000);
        if (updateState() || setProgressBarTitle) {
          updateProgressBar();
        }
      }
    }
    catch (InterruptedException e) {
    }
    catch (Exception e) {
      e.printStackTrace();
    }
    parallelProgressDisplay.setParallelProcessMonitor(null);
    setProcessEndState(ProcessEndState.DONE);
    //make sure commmandsPipe is deleted and disable its use
    deleteCommandsPipe(false);
  }
  
  protected final boolean updateState() {
    String stdOutput[] = process.getCurrentStdOutput();
    boolean returnValue = false;
    if (stdOutput == null || lastOutputLine >= stdOutput.length) {
      return returnValue;
    }
    for (int i = lastOutputLine + 1; i < stdOutput.length; i++) {
      lastOutputLine = i;
      String line = stdOutput[i].trim();
      if (EtomoDirector.getInstance().isDebug()) {
        System.err.println(line);
      }
      if (line.startsWith(BaseProcessManager.CHUNK_ERROR_TAG)) {
        lastChunkError = line;
      }
      else if (line.endsWith("to reassemble")) {
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
        throw new IllegalStateException("Bad command sent to processchunks\n" + line);
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

  private void initializeProgressBar() {
    setProgressBarTitle();
    manager.getMainPanel().setProgressBarValue(chunksFinished.getInt(), "Starting...", axisID);
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
    if (endState == ProcessEndState.PAUSED) {
      title.append(" - finishing current chunks");
    }
    manager.getMainPanel().setProgressBar(title.toString(), nChunks.getInt(), axisID, !reassembling);
  }
  
  public void kill(SystemProcessInterface process, AxisID axisID) {
    //will write to commands pipe, so must check done
    endState = ProcessEndState.KILLED;
    //process.signalInterrupt(axisID);
    try {
      writeCommand("Q");
      parallelProgressDisplay.msgInterruptingProcess();
      setProgressBarTitle = true;
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }
  
  public void pause(SystemProcessInterface process, AxisID axisID) {
    endState = ProcessEndState.PAUSED;
    //process.signalInterrupt(axisID);
    try {
      writeCommand("P");
      parallelProgressDisplay.msgInterruptingProcess();
      setProgressBarTitle = true;
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }
  
  public String getStatusString() {
    return chunksFinished + " of " + nChunks + " completed";
  }
  
  public final String getErrorMessage() {
    return "Last chunk error received:\n" + lastChunkError;
  }
  
  public final void drop(String computer) {
    if (computerList.indexOf(computer) != -1) {
      if (EtomoDirector.getInstance().isDebug()) {
        System.err.println("try to drop " + computer);
      }
      //dropComputer = computer;
      //process.signalInterrupt(axisID);
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
}
/**
* <p> $Log$
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