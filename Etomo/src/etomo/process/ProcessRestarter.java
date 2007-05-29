package etomo.process;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>Description: Collects IntermittentBackgroundProcesses that failed and restarts them on command.</p>
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
 * @singleton
 * 
 * @threadsafe
 * 
 * <p> $Log$
 * <p> Revision 1.1  2007/05/26 00:30:04  sueh
 * <p> bug# 994 Class to restart Intermittent Background Processes.
 * <p> </p>
 */
public final class ProcessRestarter implements Runnable {
  public static final String rcsid = "$Id$";

  static final ProcessRestarter INSTANCE = new ProcessRestarter();

  private final List processList = new ArrayList();

  private boolean stop = false;
  private boolean running = false;

  private ProcessRestarter() {
  }

  /**
   * Adds a process to processList if the process isn't already on the processList.
   * @param process
   */
  synchronized void addProcess(IntermittentBackgroundProcess process) {
    if (!processList.contains(process)) {
      processList.add(process);
    }
  }

  void restart() {
    if (stop) {
      return;
    }
    Thread thread = new Thread(INSTANCE);
    thread.start();
  }

  /**
   * Try to restart all the processes in processList.
   */
  public void run() {
    if (stop) {
      return;
    }
    //Only one thread can at a time can set running to true and continue.
    synchronized (this) {
      if (running) {
        return;
      }
      running = true;
    }
    //Work with the existing part of the list and ignore new entries created by
    //other failed processes.
    int size;
    synchronized (this) {
      size = processList.size();
    }
    for (int i = 0; i < size; i++) {
      if (stop) {
        break;
      }
      IntermittentBackgroundProcess process;
      synchronized (this) {
        //Not checking for out-of-bounds exception because no other thread should
        //be able to remove entries from the list.
        process = (IntermittentBackgroundProcess) processList.get(i);
      }
      //This should not be synchronized, since it is calling a function that is
      //synchronized on another mutex.
      if (process != null && !stop) {
        process.restartAll();
      }
    }
    if (!stop) {
      //After the processes are restarted, remove them.  Do this separately to
      //prevent the processes that fail fast from being re-added to processList
      //while the thread is still doing restarts.
      for (int i = size - 1; i >= 0; i--) {
        if (stop) {
          break;
        }
        synchronized (this) {
          //This must be the only place that a process can be removed from the
          //processList.
          processList.remove(i);
        }
      }
    }
    synchronized (this) {
      running = false;
    }
  }

  /**
   * Stops run().
   */
  public static void stop() {
    if (INSTANCE.stop == true) {
      return;
    }
    INSTANCE.stop = true;
    try {
      Thread.sleep(1000);
    }
    catch (InterruptedException e) {
    }
  }
}
