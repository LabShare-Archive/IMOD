package etomo.process;

import etomo.comscript.IntermittentCommand;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public interface IntermittentProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  public void setProcess(IntermittentBackgroundProcess intermittentBackgroundProcess);
  public void msgIntermittentCommandFailed(IntermittentCommand command);
  public void msgSentIntermittentCommand(IntermittentCommand command);
  public String getOutputKeyPhrase();
  public void stop();
  public boolean isMonitoring(IntermittentBackgroundProcess program);
  public void stopMonitoring(IntermittentBackgroundProcess program);
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/11/19 02:28:22  sueh
* <p> bug# 744 The base monitor interface extends Runnable.
* <p> IntermittentProcessMonitor doesn't need to.
* <p>
* <p> Revision 1.1  2005/09/10 01:49:08  sueh
* <p> bug# 532 Changed IntermittentSystemProgram to
* <p> IntermittentBackgroundProcess.  Made intermittentSystemProgram a child
* <p> of SystemProgram.  Made OutputBufferManager in independent class
* <p> instead of being inside SystemProgram.  IntermittentSystemProgram can
* <p> use OutputBufferManager to do things only necessary for intermittent
* <p> programs, such as deleting standard output after it is processed and
* <p> keeping separate lists of standard output for separate monitors.
* <p> </p>
*/
/**
* Old Log:
* <p> Revision 1.4  2005/09/09 21:46:07  sueh
* <p> bug# 532 Made LoadAverageParam an n'ton (one for each computer) so
* <p> that there aren't IntermittentSystemPrograms then computers.  This allows
* <p> IntermittentSystemProgram to be used for other things and conforms to
* <p> it definition of having one instance per IntermittentCommand, instead of
* <p> one instance per computer.
* <p>
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