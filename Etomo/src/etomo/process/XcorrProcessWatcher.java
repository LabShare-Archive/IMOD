package etomo.process;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 * 
  * <p>Organization: Boulder Laboratory for 3D Electron Microscopy (BL3dEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.4.4.1  2004/09/29 19:12:32  sueh
 * <p> bug# 520 Removing pass-through function calls.
 * <p>
 * <p> Revision 3.4  2004/04/23 20:03:08  sueh
 * <p> bug# 83 allowing initializeProgressBar() to be called before
 * <p> nSections is set
 * <p>
 * <p> Revision 3.3  2004/03/16 21:53:40  sueh
 * <p> bug# 413  when last line in the log file found, starting incrementing waitingForExit
 * <p> counter
 * <p>
 * <p> Revision 3.2  2004/03/13 01:57:35  sueh
 * <p> bug# 413 fixed backward monitor by counting lines
 * <p> possible solution for LogFileProcessMonitor.run() infinite loop in comments.
 * <p>
 * <p> Revision 3.1  2003/11/26 23:38:03  rickg
 * <p> Changed name of logFileReader
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.7  2003/08/05 21:16:26  rickg
 * <p> Correctly set nSections.
 * <p> SystemProcessInterface object is no longer necessary
 * <p>
 * <p> Revision 1.6  2003/08/04 22:23:50  rickg
 * <p> Now derived from LogFileProcessMonitor
 * <p>
 * <p> Revision 1.5  2003/06/27 20:17:51  rickg
 * <p> Fixed javadoc header
 * <p> </p>
 */

import java.io.IOException;
import etomo.ApplicationManager;
import etomo.type.AxisID;

public class XcorrProcessWatcher extends LogFileProcessMonitor {
  String lastLineRead = null;
  /**
   * Construct a xcorr process watcher
   * @param appMgr
   * @param id
   */
  public XcorrProcessWatcher(ApplicationManager appMgr, AxisID id) {

    super(appMgr, id);
    logFileBasename = "xcorr";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#intializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar("Cross-correlating stack", 1, axisID);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar(
      "Cross-correlating stack",
      nSections,
      axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection()
    throws NumberFormatException, IOException {
    String line;
    while ((line = logFileReader.readLine()) != null) {
      if (line.startsWith("View")) {
        currentSection++;
      }
      lastLineRead = line;
    }

    if (lastLineRead != null
      && lastLineRead.trim().startsWith("PROGRAM EXECUTED TO END.")) {
      waitingForExit++;
    }
  }
}