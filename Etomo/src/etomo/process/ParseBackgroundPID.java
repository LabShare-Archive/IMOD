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
 * <p> $Revision 1.3  2004/08/23 23:38:08  sueh
 * <p> $bug# 508 inheriting ParsePID
 * <p> $
 * <p> $Revision 1.2  2004/08/19 02:28:07  sueh
 * <p> $bug# 508 changed comments, moved things around
 * <p> $
 * <p> $Revision 1.1  2004/08/06 23:02:52  sueh
 * <p> $bug# 508 get PID from the first line in a file.
 * <p> $Timeout if can't retrieve PID
 * <p> $$ </p>
 */

public class ParseBackgroundPID extends ParsePID {
  public static final String rcsid = "$$Id$$";
  File outFile = null;
  public ParseBackgroundPID(SystemProgram cshProcess, StringBuffer bufPID, File outFile) {
    super(cshProcess, bufPID);
    this.outFile = outFile;
  }
  
  protected void parsePIDString() {
    BufferedReader bufferedReader = null;
    try {
      bufferedReader = new BufferedReader(new FileReader(outFile));
    }
    catch (FileNotFoundException e) {
      closeFile(bufferedReader);
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
    }
    closeFile(bufferedReader);
  }
  
  private void closeFile(BufferedReader bufferedReader) {
    try {
      if (bufferedReader != null) {
        bufferedReader.close();
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }

}
