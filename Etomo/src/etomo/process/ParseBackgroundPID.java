package etomo.process;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 * <p>Description: ParseBackgroundPID will parse the process ID from a file.
 * The process ID is stored in a string buffer that is created by the invoking
 * object.  This is implemented as runnable class with the expectation that it
 * will be run in its own thread.</p>
 * 
 * <p>Copyright: Copyright (c) 2004</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$$ </p>
 */

public class ParseBackgroundPID implements Runnable {
  public static final String rcsid = "$$Id$$";
  SystemProgram csh;
  StringBuffer PID;
  private File outFile;
  boolean error = false;
  private static long sleep = 100;
  private static double timeoutMinutes = 1;
  private static double timeoutCount = timeoutMinutes * 60 * 1000 / sleep;
  private long sleepCount = 0;
  
  /**
   * @param cshProcess
   * @param bufPID
   */
  public ParseBackgroundPID(
    SystemProgram cshProcess,
    StringBuffer bufPID,
    File outFile) {
    csh = cshProcess;
    PID = bufPID;
    this.outFile = outFile;
  }
  
  public void run() {
    //  Wait for the csh thread to start
    while (!csh.isStarted() && sleepCount <= timeoutCount) {
      try {
        Thread.sleep(sleep);
        sleepCount++;
      }
      catch (InterruptedException except) {
      }
    }

    // Once it is started scan the stderr output for the appropriate string
    while (PID.length() == 0 && !error && sleepCount <= timeoutCount) {
      try {
        parsePIDString();
        Thread.sleep(sleep);
        sleepCount++;
      }
      catch (InterruptedException except) {
      }
    }
  }

  private void parsePIDString() {
    BufferedReader bufferedReader = null;
    try {
      bufferedReader = new BufferedReader(new FileReader(outFile));
    }
    catch (FileNotFoundException e) {
      return;
    }
    String line;
    try {
      if ((line = bufferedReader.readLine()) != null) {
        if (line.startsWith("Shell PID:")) {
          String[] tokens = line.split("\\s+");
          if (tokens.length > 2) {
            PID.append(tokens[2]);
          }
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      error = true;
    }
  }

}
