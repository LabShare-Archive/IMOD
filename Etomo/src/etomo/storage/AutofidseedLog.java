package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.type.AxisID;
import etomo.type.ProcessName;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class AutofidseedLog implements Loggable {
  public static final String rcsid = "$Id:$";

  private final List lineList = new ArrayList();

  private final String userDir;
  private final AxisID axisID;

  private AutofidseedLog(String userDir, AxisID axisID) {
    this.userDir = userDir;
    this.axisID = axisID;
  }

  /**
   * @param userDir
   * @param axisID
   * @param processName
   * @return
   */
  public static AutofidseedLog getInstance(String userDir, AxisID axisID) {
    return new AutofidseedLog(userDir, axisID);
  }

  public String getName() {
    return ProcessName.AUTOFIDSEED.toString();
  }

  /**
   * Get a message to be logged in the LogPanel.
   */
  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    lineList.clear();
    // refresh the log file
    LogFile log = LogFile.getInstance(userDir, axisID, ProcessName.AUTOFIDSEED);
    if (log.exists()) {
      LogFile.ReaderId readerId = log.openReader();
      if (readerId != null && !readerId.isEmpty()) {
        String line = log.readLine(readerId);
        while (line != null) {
          if (line.indexOf("candidate points")!=-1) {
            lineList.add(line);
          }
          else if (line.trim().startsWith("Final:")) {
            lineList.add(line);
          }
          line = log.readLine(readerId);
        }
        log.closeRead(readerId);
        return lineList;
      }
    }
    return lineList;
  }
}
