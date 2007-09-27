package etomo.process;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.ui.LoadDisplay;

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
public final class QueuechunkLoadMonitor extends LoadMonitor {
  public static  final String  rcsid =  "$Id$";
  
  public QueuechunkLoadMonitor(LoadDisplay display, AxisID axisID,
      BaseManager manager) {
    super(display, axisID, manager);
  }
  
  public final String getOutputKeyPhrase() {
    return null;
  }
  
  void processData(ProgramState programState) {
    //process standard out
    String[] stdout = programState.getStdOutput(this);
    if (stdout == null || stdout.length == 0) {
      return;
    }
    programState.msgReceivedData();
    programState.clearUsers();
    String[] loadArray = stdout[0].split("\\s*\\,\\s*");
    if (loadArray == null || loadArray.length == 0) {
      return;
    }
    display.setLoad(programState.getCommand().getComputer(), loadArray);
  }
}
