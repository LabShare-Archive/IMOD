package etomo;

import etomo.process.ParallelProcessMonitor;
import etomo.process.ReconnectProcess;
import etomo.type.ProcessingMethod;
import etomo.ui.swing.AxisProcessPanel;
import etomo.ui.swing.ParallelPanel;
import etomo.ui.swing.ParallelProgressDisplay;
import etomo.ui.swing.ProcessInterface;

/**
* <p>Description: Coordinates the actions involving the AxisProcessPanel, the
* ParallelPanel, the current dialog, and the reconnect process, if it is
* running.  The only state this class has is know what has registered with it.
* It relies on its registered class to remember their state when necessary.</p>
* 
* <p>Copyright: Copyright 2010</p>
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
* <p> Revision 1.2  2011/02/10 03:33:08  sueh
* <p> bug# 1437 Reformatting.
* <p>
* <p> Revision 1.1  2011/02/03 05:54:15  sueh
* <p> bug# 1422 Class that coordinates information about the current parallel
* <p> processing method.
* <p> </p>
*/
public final class ProcessingMethodMediator {
  public static final String rcsid = "$Id$";

  /**
   * reconnectProcess: Only one.  May or may not register before a dialog is
   * created.  Has priority over everything else.  Must deregister when its
   * process ends.
   */
  private ReconnectProcess reconnectProcess = null;
  /**
   * processInterface: Dialogs, tab panels, panels, or ancestors of panels.
   * One may register at a time.  Must deregister when not displayed, unless
   * it is immediately replaces by another interface.
   */
  private ProcessInterface processInterface = null;

  /**
   * parallelProcessMonitor: Parallel process monitor instances.  Registered
   * while running.  Inhibits axisProcessPanel.  Overriden by reconnectProcess.
   */
  private ParallelProcessMonitor parallelProcessMonitor = null;

  /**
   * axisProcessPanel: Only one.  Manages the parallel panel.  First thing to
   * register.  Does not have to deregister because it is not destroyed until
   * the program exits.  Takes direction from reconnectProcess and
   * processInterface.
   */
  private AxisProcessPanel axisProcessPanel = null;
  /**
   * parallelPanel:  Parallel table panel.  Only one.  Does not have to
   * deregister because it is not destroyed until the program exits  Takes
   * direction from reconnectProcess and processInterface.  Tells
   * processInterface if the QUEUE ProcessingMethod has been selected.
   */
  private ParallelPanel parallelPanel = null;

  /**
   * ReconnectProcess prevents processInterface and parallelPanel from changing
   * processing method.
   * @param reconnectProcess
   */
  public synchronized void register(final ReconnectProcess reconnectProcess) {
    if (reconnectProcess == null) {
      // can't use this function to deregister a process
      return;
    }
    if (this.reconnectProcess != null) {
      // must deregister old process to register new one
      return;
    }
    this.reconnectProcess = reconnectProcess;
    // Show the reconnect process method
    ProcessingMethod method = reconnectProcess.getProcessingMethod();
    if (axisProcessPanel != null) {
      if (!method.isLocal()) {
        // Does the monitor start before the process? Maybe, so force the
        // axisProcessPanel to show the parallel panel.
        axisProcessPanel.forceShowParallelPanel(true);
      }
      else {
        axisProcessPanel.showParallelPanel(false);
      }
    }
    // lock everything while the reconnect process is running.
    if (parallelPanel != null) {
      parallelPanel.setProcessingMethod(method);
      parallelPanel.lockProcessingMethod(true);
    }
    if (processInterface != null) {
      processInterface.lockProcessingMethod(true);
    }
  }

  /**
   * Releases control of reconnectProcess and reinstates the dialog method.
   * @param origin
   */
  public synchronized void deregister(final ReconnectProcess origin) {
    if (origin != reconnectProcess) {
      return;
    }
    reconnectProcess = null;
    // Going back to the processInterface method
    ProcessingMethod method = null;
    if (processInterface != null) {
      method = processInterface.getProcessingMethod();
    }
    if (method == null) {
      method = ProcessingMethod.DEFAULT;
    }
    // Unlock after the reconnect process is done and have the processInterface
    // take over.
    if (axisProcessPanel != null) {
      axisProcessPanel.showParallelPanel(!method.isLocal());
    }
    boolean queueMethod = false;
    if (parallelPanel != null) {
      parallelPanel.lockProcessingMethod(false);
      parallelPanel.setProcessingMethod(method);
      queueMethod = parallelPanel.getProcessingMethod() == ProcessingMethod.QUEUE;
    }
    if (processInterface != null) {
      processInterface.lockProcessingMethod(false);
      processInterface.disableGpu(queueMethod);
    }
  }

  /**
   * Sets the interface processing method.  When another processInterface is
   * register, the new one replaces it.
   * @param processInterface
   */
  public synchronized void register(final ProcessInterface processInterface) {
    if (processInterface == null) {
      // can't use this function to deregister an interface
      return;
    }
    this.processInterface = processInterface;
    if (reconnectProcess != null) {
      // nothing to do - reconnectProcess locks everything
      return;
    }
    // Set interface method - needs to be called twice because of the interdependency of
    // process interface and the parallel panel.
    setInterfaceMethod(processInterface.getProcessingMethod());
    setInterfaceMethod(processInterface.getProcessingMethod());
  }

  /**
   * Sets the default interface processing method.  To prevent the parallel
   * panel from blinking on and off, only call this function when another
   * interface isn't immediately available to replace it (helps with tabbing).
   * @param origin
   */
  public synchronized void deregister(final ProcessInterface origin) {
    if (origin != processInterface) {
      return;
    }
    processInterface = null;
    // hide parallel panel
    if (axisProcessPanel != null) {
      axisProcessPanel.showParallelPanel(false);
    }
    if (parallelPanel != null) {
      parallelPanel.setProcessingMethod(ProcessingMethod.DEFAULT);
    }
  }

  /**
   * Prevents axisProcessPanel from hiding the parallel panel while a parallel
   * process is running.  When a parallel process is done, reinstates the
   * normal display.
   * @param parallelPanel
   * @param running
   */
  public synchronized void register(final ParallelProcessMonitor parallelProcessMonitor) {
    if (parallelProcessMonitor == null) {
      // can't use this function to deregister an interface
      return;
    }
    if (this.parallelProcessMonitor != null) {
      // must deregister old interface to register new one
      return;
    }
    this.parallelProcessMonitor = parallelProcessMonitor;
    if (axisProcessPanel != null) {
      axisProcessPanel.lockProcessingMethod(true);
    }
  }

  public synchronized void deregister(final ParallelProcessMonitor origin) {
    if (origin != parallelProcessMonitor) {
      return;
    }
    parallelProcessMonitor = null;
    if (axisProcessPanel != null) {
      axisProcessPanel.lockProcessingMethod(false);
    }
    // do nothing if reconnectProcess is running - its deregistration will take
    // care of any changes.
    if (reconnectProcess == null) {
      ProcessingMethod method = ProcessingMethod.DEFAULT;
      if (processInterface != null) {
        method = processInterface.getProcessingMethod();
      }
      setInterfaceMethod(method);
    }
  }

  public synchronized void register(final AxisProcessPanel axisProcessPanel) {
    if (axisProcessPanel == null) {
      // can't use this function to deregister a panel
      return;
    }
    if (this.axisProcessPanel != null) {
      // must deregister old panel to register new one
      return;
    }
    this.axisProcessPanel = axisProcessPanel;
    // The axisProcessPanel should be created very early, so there wouldn't be
    // anything available for it to get information from.
  }

  public synchronized void register(final ParallelPanel parallelPanel) {
    if (parallelPanel == null) {
      // can't use this function to deregister a panel
      return;
    }
    if (this.parallelPanel != null) {
      // must deregister old panel to register new one
      return;
    }
    this.parallelPanel = parallelPanel;
    // Parallel panel is create in response to the existance of a reconnect
    // process or an interface. Setting its method should be taken care of
    // by the process or interface.
  }

  /**
   * The the method from processInterface in axisProcessPanel and
   * parallelPanel.  Also disable GPU in processInterface if the queue check
   * box is checked.
   * @param method - method from processInterface.
   */
  private void setInterfaceMethod(ProcessingMethod method) {
    if (method == null) {
      method = ProcessingMethod.DEFAULT;
    }
    if (axisProcessPanel != null) {
      axisProcessPanel.showParallelPanel(!method.isLocal());
    }
    boolean queueMethod = false;
    if (parallelPanel != null) {
      parallelPanel.setProcessingMethod(method);
      queueMethod = parallelPanel.getProcessingMethod() == ProcessingMethod.QUEUE;
    }
    if (processInterface != null) {
      processInterface.disableGpu(queueMethod);
    }
  }

  /**
   * Tell the parallel panel about the change in ProcessInterface method.
   * @param origin
   * @param method
   */
  public void setMethod(final ProcessInterface origin, final ProcessingMethod method) {
    // Ignore an unregistered process interface
    // Don't change processing method while reconnect process exists
    if (origin != processInterface || reconnectProcess != null) {
      return;
    }
    setInterfaceMethod(method);
  }

  /**
   * Tell the processInterface when to disable the GPU check box.
   * @param origin
   * @param method
   */
  public void setMethod(final ParallelPanel origin, final ProcessingMethod method) {
    // Ignore an unregistered parallel panel
    if (origin != parallelPanel) {
      return;
    }
    if (processInterface != null) {
      processInterface.disableGpu(method == ProcessingMethod.QUEUE);
    }
  }

  /**
   * Get the processing method for resume and for turning off queue check box.
   * @param parallelPanelMethod - may be null
   * @return
   */
  public ProcessingMethod getRunMethodForParallelPanel(
      final ProcessingMethod parallelPanelMethod) {
    if (processInterface != null) {
      ProcessingMethod processInterfaceMethod = processInterface.getProcessingMethod();
      if (!processInterfaceMethod.isLocal()
          && parallelPanelMethod == ProcessingMethod.QUEUE) {
        return parallelPanelMethod;
      }
      return processInterfaceMethod;
    }
    // OK to return the correct method during a reconnect because the resume button has no
    // effect and the parallel panel method is locked.
    return parallelPanelMethod;
  }

  /**
   * Get the processing method for running a process from the interface.
   * @param processInterfaceMethod
   * @return
   */
  public ProcessingMethod getRunMethodForProcessInterface(
      final ProcessingMethod processInterfaceMethod) {
    if (parallelPanel != null) {
      ProcessingMethod parallelPanelMethod = parallelPanel.getProcessingMethod();
      if (!processInterfaceMethod.isLocal()
          && parallelPanelMethod == ProcessingMethod.QUEUE) {
        return parallelPanelMethod;
      }
    }
    return processInterfaceMethod;
  }

  /**
   * Tells parallelPanel to stop its thread for exit.
   */
  public void msgExiting() {
    if (parallelPanel != null) {
      parallelPanel.endTable();
    }
  }

  /**
   * Provides a simple way to request the ParallelProgressDisplay from
   * parallelPanel if it has already been displayed.  Displaying parallePanel
   * should already have been taken care of by the register classes.  Does not
   * change anything.
   * @return ParallelProgressDisplay or null if not available
   */
  public ParallelProgressDisplay getParallelProgressDisplay() {
    if (parallelPanel == null) {
      return null;
    }
    return parallelPanel.getParallelProgressDisplay();
  }
}
