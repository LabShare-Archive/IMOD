package etomo.process;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.ProcessEndState;
import etomo.ui.ParallelProgressDisplay;

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
public class ProcesschunksProcessMonitor implements ProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  private final static String TITLE = "Processchunks";
  private final BaseManager manager;
  private final AxisID axisID;
  private final ParallelProgressDisplay parallelProgressDisplay;
  
  private SystemProcessInterface process = null;
  private EtomoNumber nChunks = new EtomoNumber();
  private EtomoNumber lastChunkFinished = new EtomoNumber();
  private int lastOutputLine = -1;
  private boolean setProgressBarTitle = false;
  private boolean reassembling = false;
  private ProcessEndState endState = null;
  private String rootName = null;

  /**
   * Default constructor
   * @param manager
   * @param axisID
   * @param process
   */
  public ProcesschunksProcessMonitor(BaseManager manager, AxisID axisID,
      ParallelProgressDisplay parallelProgressDisplay) {
    this.manager = manager;
    this.axisID = axisID;
    this.parallelProgressDisplay = parallelProgressDisplay;
  }
  
  public final void setProcess(SystemProcessInterface process) {
    this.process = process;
  }
  
  public void run() {
    nChunks.set(0);
    lastChunkFinished.set(0);
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
      System.out.println(line);
      if (line
          .equals("Q to kill all jobs and quit, P to finish running jobs then exit,")) {
        if (endState == ProcessEndState.KILLED) {
          process.setCurrentStdInput("Q");
        }
        else if (endState == ProcessEndState.PAUSED) {
          process.setCurrentStdInput("P");
        }
      }
      else if (line.endsWith("to reassemble")) {
        reassembling = true;
        setProgressBarTitle = true;
        returnValue = true;
      }
      else {
        String[] strings = line.split("\\s+");
        if (strings.length > 2 && line.endsWith("DONE SO FAR")) {
          if (!nChunks.equals(strings[2])) {
            nChunks.set(strings[2]);
            setProgressBarTitle = true;
          }
          lastChunkFinished.set(strings[0]);
          returnValue = true;
        }
        else if (strings.length > 1) {
          if (line.startsWith("Dropping")) {
            parallelProgressDisplay.drop(strings[1]);
          }
          else if (strings[1].equals("finished")) {
            parallelProgressDisplay.addSuccess(strings[3]);
          }
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
    manager.getMainPanel().setProgressBarValue(lastChunkFinished.getInt(), "Starting...", axisID);
  }
  
  private void updateProgressBar() {
    if (setProgressBarTitle) {
      setProgressBarTitle = false;
      setProgressBarTitle();
    }
    manager.getMainPanel().setProgressBarValue(lastChunkFinished.getInt(),
        lastChunkFinished + " of " + nChunks + " completed", axisID);
 }
  
  private void setProgressBarTitle() {
    StringBuffer title = new StringBuffer(TITLE);
    if (rootName != null) {
      title.append(" " + rootName);
    }
    if (reassembling) {
      title.append(": reassembling");
    }
    manager.getMainPanel().setProgressBar(title.toString(), nChunks.getInt(), axisID);
  }
  
  public void kill(SystemProcessInterface process, AxisID axisID) {
    endState = ProcessEndState.KILLED;
    process.signalInterrupt(axisID);
  }
  
  public void pause(SystemProcessInterface process, AxisID axisID) {
    endState = ProcessEndState.PAUSED;
    process.signalInterrupt(axisID);
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/08/04 19:46:15  sueh
* <p> bug# 532 Class to monitor processchunks.  Monitors the standard output.
* <p> Sends updates to the progress panel and to a parallel process display.
* <p> </p>
*/