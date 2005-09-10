package etomo.process;

import java.io.BufferedReader;

import etomo.type.AxisID;

/**
* <p>Description:
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
public class IntermittentSystemProgram extends SystemProgram {
  public static  final String  rcsid =  "$Id$";
  
  private final String outputKeyPhrase;
  
  public IntermittentSystemProgram(String propertyUserDir, String[] cmdArray, AxisID axisID, String outputKeyPhrase) {
    super(propertyUserDir, cmdArray, axisID);
    this.outputKeyPhrase = outputKeyPhrase;
  }
  
  protected OutputBufferManager createOutputBufferManager(BufferedReader cmdBuffer) {
    OutputBufferManager bufferManager = new OutputBufferManager(cmdBuffer, outputKeyPhrase);
    bufferManager.setCollectOutput(false);
    return bufferManager;
  }
  
  /**
   * Get the standard output from the execution of the program.
   * @return String[] An array of strings containing the standard output from
   * the program.  Each line of standard out is stored in a String.
   */
  public String[] getStdOutput(IntermittentProcessMonitor monitor) {
    if (stdout == null) {
      return null;
    }
    String[] stdOutputArray  = stdout.get(monitor);
    return stdOutputArray;
  }
}
/**
* <p> $Log$
* <p> Revision 1.10  2005/09/09 21:25:16  sueh
* <p> bug# 532 Made LoadAverageParam an n'ton (one for each computer) so
* <p> that there aren't IntermittentSystemPrograms then computers.  This allows
* <p> IntermittentSystemProgram to be used for other things and conforms to
* <p> it definition of having one instance per IntermittentCommand, instead of
* <p> one instance per computer.  Synchronizing start() and stop().  In run(),
* <p> created a local SystemProgram. Saving the local SystemProgram to the
* <p> member SystemProgram each time a new local one is created.
* <p> Exit from the intermittent command when the local SystemProgram isn't
* <p> equal to the member SystemProgram.  This makes the program exit
* <p> quickly when there is alot of starting and stopping of commands.
* <p>
* <p> Revision 1.9  2005/09/07 20:34:24  sueh
* <p> bug# 532 In getStdOutput() return the entire stdOutput structure.
* <p>
* <p> Revision 1.8  2005/09/02 18:58:42  sueh
* <p> bug# 532 removed a null pointer exception problem from run().
* <p>
* <p> Revision 1.7  2005/09/01 17:50:10  sueh
* <p> bug# 532 Added setCurrentStdInput() to allow load average monitor to
* <p> confirm the connect, the first time a connect between computers is made.
* <p>
* <p> Revision 1.6  2005/08/31 17:16:33  sueh
* <p> bug# 532 Allow monitor to call the stop(SystemProgramMonitor).
* <p>
* <p> Revision 1.5  2005/08/30 23:33:50  sueh
* <p> bug# 532 Telling monitors that intermittent command was sent.
* <p>
* <p> Revision 1.4  2005/08/30 18:43:20  sueh
* <p> bug# 532 When the intermitent command throughs a IOException.  Call
* <p> intermittentCommandFailed in all monitors.
* <p>
* <p> Revision 1.3  2005/08/27 22:29:31  sueh
* <p> bug# 532 Handle IOException from SystemProgram.setCurrentStdInput()
* <p> so that failed intermittent commands can halted.
* <p>
* <p> Revision 1.2  2005/08/24 00:20:47  sueh
* <p> bug# 532 removed the running member variable.  Only needed the stopped
* <p> member variable.
* <p>
* <p> Revision 1.1  2005/08/22 16:31:15  sueh
* <p> bug# 532 Runs a command using SystemProgram.  Keeps standard
* <p> input open.   Sends a string through standard input at intervals.
* <p> </p>
*/