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
 * <p> $Revision 1.6  2006/06/06 17:18:49  sueh
 * <p> $bug# change threadData to processData.
 * <p> $
 * <p> $Revision 1.5  2006/06/05 16:29:28  sueh
 * <p> $bug# 766 Setting the pid in ProcessData.
 * <p> $
 * <p> $Revision 1.4  2004/08/25 23:02:27  sueh
 * <p> $bug# 508 closing buffered reader on combine.out
 * <p> $
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

public final class ParseBackgroundPID extends ParsePID {
  public static final String rcsid = "$$Id$$";

  private final File outFile;

  public ParseBackgroundPID(SystemProgram process, StringBuffer bufPID, File outFile,
      ProcessData processData) {
    super(process, bufPID, processData);
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
        if (line.startsWith("Shell PID:") || line.indexOf("Python PID:") != -1
            || line.startsWith("Windows PID:") || line.startsWith("Cygwin PID:")) {
          String[] tokens = line.split("\\s+");
          if (tokens.length > 2) {
            boolean found = false;
            for (int index = 0; index < tokens.length; index++) {
              if (found) {
                appendPID(tokens[index]);
                break;
              }
              else if (tokens[index].endsWith("PID:")) {
                found = true;
              }
            }
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
