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
  SystemProgram csh;
  StringBuffer PID;
  
  public ParsePID(SystemProgram cshProcess, StringBuffer bufPID) {
    csh = cshProcess;
    PID = bufPID;
  }


  public void run() {
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
    System.err.println("PID:" + PID);
  }

  /**
   * Walk the standard error output to parse the PID string
   */
  protected void parsePIDString() {
    String[] stderr = csh.getStdError();
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

