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
* <p> $Log$ </p>
*/
public final class QueuechunkParam implements IntermittentCommand {
  public static  final String  rcsid =  "$Id$";
  
  private final String queue;
  private final String intermittentCommand;
  
  private QueuechunkParam(String queue,AxisID axisID, BaseManager manager) {
    this.queue=queue;
    intermittentCommand="bash "+CpuAdoc.getInstance(axisID,manager).getQueue(queue).getCommand()+ " -a L";
  }
  
  public static QueuechunkParam getLoadInstance(String queue,AxisID axisID, BaseManager manager) {
    QueuechunkParam instance = new QueuechunkParam(queue, axisID,  manager);
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
  
  public String getIntermittentCommand(){
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
