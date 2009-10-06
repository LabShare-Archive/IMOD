package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.ManagerKey;
import etomo.type.ProcessName;

/**
 * <p>Description: Represents the flattenwarp log.</p>
 * 
 * <p>Copyright: Copyright 2009</p>
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
public final class FlattenWarpLog implements Loggable {
  public static final String rcsid = "$Id$";

  public static final FlattenWarpLog INSTANCE = new FlattenWarpLog();

  private final List lineList = new ArrayList();

  private String[] log = null;

  /**
   * Set the log and clear lineList.
   * @param log
   */
  public void setLog(String[] log) {
    this.log = log;
    lineList.clear();
  }

  public String getName() {
    return ProcessName.FLATTEN_WARP.toString();
  }

  /**
   * Get a message to be logged in the LogPanel.  Only refreshes lineList if it
   * is empty.  It should be emptied when this.log is set.  If log changes
   * without setLog being called (because it is a pointer), it would not
   * necessarily be correct anymore.
   */
  public List getLogMessage(ManagerKey managerKey)
      throws LogFile.LockException, FileNotFoundException, IOException {
    if (log == null || !lineList.isEmpty()) {
      return lineList;
    }
    for (int i = 0; i < log.length; i++) {
      if (log[i].trim().startsWith("Minimum spacing between contours is")) {
        lineList.add(log[i]);
      }
      else if (log[i].trim()
          .startsWith("Setting target spacings in X and Y to")) {
        lineList.add(log[i]);
      }
      else if (log[i].trim().startsWith("Mean Z height is")) {
        lineList.add(log[i]);
      }
      else if (log[i].trim().indexOf("warping transformations written") != -1) {
        lineList.add(log[i]);
      }
    }
    return lineList;
  }
}
