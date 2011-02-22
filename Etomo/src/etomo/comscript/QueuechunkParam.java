package etomo.comscript;

import etomo.BaseManager;
import etomo.storage.Network;
import etomo.storage.Node;
import etomo.type.AxisID;

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
 * <p> Revision 1.5  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.4  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.3  2009/03/17 00:32:48  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.2  2008/07/19 00:24:43  sueh
 * <p> bug# 1125 Making it easier to access CpuAdoc by not passing the
 * <p> manager to it; all it needs is the current directory.
 * <p>
 * <p> Revision 1.1  2007/09/27 19:27:00  sueh
 * <p> bug# 1044 Class to run queuechunk.  Queuechunk is a site-level script with
 * <p> some standard features.  The basic command for running queuechunk can be
 * <p> added to cpu.adoc.  Only the parameter to retrieve the queue "load' needs to be
 * <p> implemented at this time.  The load option is used as an intermittent process, so
 * <p> QueuechunkParam implements IntermittentCommand, but queuechunk could be
 * <p> use to other things as regular process.
 * <p> </p>
 */
public final class QueuechunkParam implements IntermittentCommand {
  public static final String rcsid = "$Id$";

  private final String queue;

  private String intermittentCommand = null;

  private QueuechunkParam(String queue, AxisID axisID, BaseManager manager) {
    this.queue = queue;
  }

  public static QueuechunkParam getLoadInstance(String queue, AxisID axisID,
      BaseManager manager) {
    QueuechunkParam instance = new QueuechunkParam(queue, axisID, manager);
    instance.setIntermittentCommand(queue, axisID, manager);
    return instance;
  }

  private void setIntermittentCommand(String queue, AxisID axisID, BaseManager manager) {
    Node cluster = Network.getQueue(manager, queue, axisID, manager.getPropertyUserDir());
    if (cluster != null) {
      intermittentCommand = "bash " + cluster.getCommand() + " -a L";
    }
  }

  public int getInterval() {
    return 20000;
  }

  public String getComputer() {
    return queue;
  }

  public String getEndCommand() {
    return null;
  }

  /**
   * Returns intermittent command string or null if the queue name was not found
   * by Network.
   */
  public String getIntermittentCommand() {
    return intermittentCommand;
  }

  public String[] getLocalStartCommand() {
    return null;
  }

  public String[] getRemoteStartCommand() {
    return null;
  }

  public boolean notifySentIntermittentCommand() {
    return true;
  }
}
