package etomo.process;

import java.util.Hashtable;

import etomo.type.AxisTypeException;
import etomo.util.Utilities;

/**
 * <p>Description: Watches 3dmod stderr for requests.  Processes requests.</p>
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
 */
class ImodRequestHandler implements Runnable {
  public static final String rcsid = "$Id$";

  //one instance per ImodManager
  public final static Hashtable instances = new Hashtable();

  private final ImodManager imodManager;
  private boolean stop = false;
  private boolean running = true;

  private ImodRequestHandler(ImodManager imodManager) {
    this.imodManager = imodManager;
  }

  static ImodRequestHandler getInstance(ImodManager imodManager) {
    if (!Utilities.isWindowsOS()) {
      return null;
    }
    if (instances.containsKey(imodManager)) {
      return (ImodRequestHandler) instances.get(imodManager);
    }
    ImodRequestHandler handler = new ImodRequestHandler(imodManager);
    instances.put(imodManager, handler);
    new Thread(handler).start();
    return handler;
  }

  public void run() {
    //System.out.println("run");
    while (!stop) {
      //System.out.println("!stop");
      try {
        Thread.sleep(100);
      }
      catch (InterruptedException e) {
      }
      try {
        imodManager.processRequest();
      }
      catch (AxisTypeException e) {
        e.printStackTrace();
      }
    }
    running = false;
    //System.out.println("end run");
  }

  /**
   * stop the worker thread
   * this request times out after 1 seconds
   */
  public void stop() {
    //System.out.println("stop");
    stop = true;
    //give the worker thread 1 second to clean up
    for (int i = 0;i< 20;i++) {
      //System.out.println("i="+i);
      try {
        Thread.sleep(50);
      }
      catch (InterruptedException e) {
      }
      if (!running) {
        break;
      }
    }
    //System.out.println("end stop");
  }
}
/**
 * <p> $Log$ </p>
 */
