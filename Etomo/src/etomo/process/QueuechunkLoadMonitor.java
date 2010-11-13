package etomo.process;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.ui.swing.LoadDisplay;

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
* <p> Revision 1.1  2007/09/27 20:27:25  sueh
* <p> bug# 1044 Added QueuechunkLoadMonitor, which has mostly the same functionality as LoadAverageMonitor, except for the output and how it is used.  Factoring out common functionality into a new parent class, LoadMonitor.
* <p> </p>
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
