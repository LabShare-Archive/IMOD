package etomo.process;

import java.io.IOException;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Electron Microscopy (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.8  2010/03/03 04:55:35  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 3.7  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.6  2009/02/04 23:23:54  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 3.5  2006/10/24 21:18:01  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 3.4  2006/10/10 05:07:40  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 3.3  2004/11/19 23:18:54  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.2.4.1  2004/09/29 17:48:45  sueh
 * <p> bug# 520 Removed MainPanel pass-through functions.
 * <p>
 * <p> Revision 3.2  2004/04/23 20:02:27  sueh
 * <p> bug# 83 allowing initializeProgressBar() to be called before
 * <p> nSections is set
 * <p>
 * <p> Revision 3.1  2003/11/26 23:37:38  rickg
 * <p> Changed name of logFileReader
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.1  2003/08/05 21:18:32  rickg
 * <p> Initial revision
 * <p> </p>
 */

public class CCDEraserProcessMonitor extends LogFileProcessMonitor {

  public static final String rcsid = "$Id$";

  /**
   * Default constructor
   * @param appMgr
   * @param id
   */
  public CCDEraserProcessMonitor(BaseManager manager, AxisID id) {

    super(manager, id);
    logFileBasename = "eraser";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#initializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      manager.getMainPanel().setProgressBar("CCD Eraser", 1, axisID);
      manager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    manager.getMainPanel().setProgressBar("CCD Eraser", nSections, axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection() throws NumberFormatException, LogFile.LockException,
      IOException {
    String line;
    while ((line = readLogFileLine()) != null) {
      if (line.startsWith("Section")) {
        String[] fields = line.split("\\s+");
        if (fields.length > 1) {
          String number = fields[1];
          currentSection = Integer.parseInt(number);
        }
      }
    }
  }
}
