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
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.1  2004/08/06 23:02:52  sueh
 * <p> $bug# 508 get PID from the first line in a file.
 * <p> $Timeout if can't retrieve PID
 * <p> $$ </p>
 */

public class ParseBackgroundPID implements Runnable {
  public static final String rcsid = "$$Id$$";
  private static final long sleep = 100;
  private static final double timeoutMinutes = 1;
  private static final double timeoutCount = timeoutMinutes * 60 * 1000 / sleep;
  SystemProgram csh;
  StringBuffer PID;
  private File outFile;
  boolean error = false;

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
