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
//  String lastLineRead = null;
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
    applicationManager.setProgressBar(
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
//TODO
//      lastLineRead = line;
    }
//TODO
//    if (lastLineRead != null
//      && lastLineRead.trim().startsWith("PROGRAM EXECUTED TO END.")) {
//      waitingForExit++;
//    }
  }
}