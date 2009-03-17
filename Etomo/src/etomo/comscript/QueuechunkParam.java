package etomo.comscript;

import etomo.BaseManager;
import etomo.storage.CpuAdoc;
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
  private final String intermittentCommand;

  private QueuechunkParam(String queue, AxisID axisID, BaseManager manager) {
    this.queue = queue;
    intermittentCommand = "bash "
        + CpuAdoc.getInstance(axisID, manager.getPropertyUserDir(),
            manager.getManagerKey()).getQueue(queue).getCommand() + " -a L";
  }

  public static QueuechunkParam getLoadInstance(String queue, AxisID axisID,
      BaseManager manager) {
    QueuechunkParam instance = new QueuechunkParam(queue, axisID, manager);
    return instance;
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
