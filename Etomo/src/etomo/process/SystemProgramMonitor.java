package etomo.process;

import etomo.comscript.IntermittentCommand;

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
public interface SystemProgramMonitor extends Runnable {
  public static  final String  rcsid =  "$Id$";
  
  public void setIntermittentSystemProgram(IntermittentSystemProgram intermittentSystemProgram);
  public void msgIntermittentCommandFailed(IntermittentCommand command);
  public void msgSentIntermittentCommand(IntermittentCommand command);
}
/**
* <p> $Log$
* <p> Revision 1.3  2005/08/31 17:18:45  sueh
* <p> bug# 532 Handle an unresponsive computer by dropping from
* <p> processchunks after 12 unresponses.
* <p>
* <p> Revision 1.2  2005/08/30 18:52:53  sueh
* <p> bug# 532 Added intermittentCommandFailed() to handle a failed w
* <p> command.
* <p>
* <p> Revision 1.1  2005/08/22 17:08:27  sueh
* <p> bug# 532 Interface for a monitor which can use
* <p> IntermittentSystemProgram and can receive a stop message.
* <p> </p>
*/