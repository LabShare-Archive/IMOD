package etomo.process;

/**
 * <p>Description: ParsePID will parse the process ID from a (csh) process
 * that writes it out to standard error.  The process ID is stored in a string
 * buffer that is created by the invoking object.  This is implemented as
 * runnable class with the expectation that it will be run in its own thread.</p>
 * 
 * <p>Copyright: Copyright (c) 2002</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.4  2006/06/05 16:29:38  sueh
 * <p> bug# 766 Setting the pid in ProcessData.
 * <p>
 * <p> Revision 3.3  2005/09/09 21:41:14  sueh
 * <p> bug# 532 Handling null from stderr and stdout.
 * <p>
 * <p> Revision 3.2  2004/08/24 23:08:56  sueh
 * <p> bug# 508 place the pid found in the err log
 * <p>
 * <p> Revision 3.1  2004/08/23 23:39:08  sueh
 * <p> bug#  508 allow ParseBackgroundPID to override parsePIDString
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.1  2003/05/21 22:55:31  rickg
 * <p> Intitial revision
 * <p> </p>
 */
/**
 * @author rickg
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code Template
 */
public class ParsePID implements Runnable {
  public static final String rcsid = "$Id$";
  private final SystemProgram csh;
  private final StringBuffer PID;
  private final ProcessData processData;

  public ParsePID(SystemProgram cshProcess, StringBuffer bufPID,
      ProcessData processData) {
    csh = cshProcess;
    PID = bufPID;
    this.processData = processData;
  }

  public final void run() {
    //  Wait for the csh thread to start
    while (!csh.isStarted()) {
      try {
        Thread.sleep(100);
      }
      catch (InterruptedException except) {
        return;
      } 
    }

    // Once it is started scan the stderr output for the appropriate string
    while (PID.length() == 0 && !csh.isDone()) {
      try {
        parsePIDString();
        Thread.sleep(100);
      }
      catch (InterruptedException except) {
        return;
      }
    }
    processData.setPid(PID.toString());
    System.err.println("PID:" + PID);
  }

  protected final void appendPID(String PID) {
    this.PID.append(PID);
  }

  /**
   * Walk the standard error output to parse the PID string
   */
  protected void parsePIDString() {
    String[] stderr = csh.getStdError();
    if (stderr == null) {
      return;
    }
    for (int i = 0; i < stderr.length; i++) {
      if (stderr[i].startsWith("Shell PID:")) {
        String[] tokens = stderr[i].split("\\s+");
        if (tokens.length > 2) {
          PID.append(tokens[2]);
        }
      }
    }
  }
}
