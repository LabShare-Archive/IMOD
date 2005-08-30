package etomo.process;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.ProcessEndState;
import etomo.ui.ParallelProgressDisplay;
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
  private String dropComputer = null;

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
    nChunks.set(0);
    chunksFinished.set(0);
    initializeProgressBar();
    try {
      while (process == null || !process.isDone()) {
        Thread.sleep(500);
        if (updateState()) {
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
  }
  
  protected boolean updateState() {
    String stdOutput[] = process.getCurrentStdOutput();
    boolean returnValue = false;
    if (stdOutput == null || lastOutputLine >= stdOutput.length) {
      return returnValue;
    }
    for (int i = lastOutputLine + 1; i < stdOutput.length; i++) {
      lastOutputLine = i;
      String line = stdOutput[i].trim();
      //if (EtomoDirector.getInstance().isDebug()) {
        System.err.println(line);
      //}
      if (line.startsWith("Q to kill all jobs and quit")) {
        if (dropComputer != null) {
          //
          String computer = dropComputer;
          dropComputer = null;
          process.setCurrentStdInput("D " + computer);
        }
        //handle pause and kill
        if (endState == ProcessEndState.KILLED) {
          process.setCurrentStdInput("Q");
        }
        else if (endState == ProcessEndState.PAUSED) {
          process.setCurrentStdInput("P");
        }
        setProgressBarTitle = true;
        returnValue = true;
      }
      else if (line.startsWith(BaseProcessManager.CHUNK_ERROR_TAG)) {
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
        //handle a dropped CPU
        else if (strings.length > 1) {
          if (line.startsWith("Dropping")) {
            parallelProgressDisplay.drop(strings[1]);
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
      title.append(" - pausing");
    }
    manager.getMainPanel().setProgressBar(title.toString(), nChunks.getInt(), axisID, !reassembling);
  }
  
  public void kill(SystemProcessInterface process, AxisID axisID) {
    endState = ProcessEndState.KILLED;
    process.signalInterrupt(axisID);
    parallelProgressDisplay.msgInterruptingProcess();
  }
  
  public void pause(SystemProcessInterface process, AxisID axisID) {
    endState = ProcessEndState.PAUSED;
    process.signalInterrupt(axisID);
    parallelProgressDisplay.msgInterruptingProcess();
  }
  
  public String getStatusString() {
    return chunksFinished + " of " + nChunks + " completed";
  }
  
  public final String getErrorMessage() {
    return "Last chunk error received:\n" + lastChunkError;
  }
  
  public final void drop(String computer) {
    if (EtomoDirector.getInstance().isDebug()) {
      System.err.println("drop " + computer);
    }
    if (computerList.indexOf(computer) != -1) {
      dropComputer = computer;
    }
    process.signalInterrupt(axisID);
  }
}
/**
* <p> $Log$
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