package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
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
  public CCDEraserProcessMonitor(
    ApplicationManager appMgr,
    AxisID id) {

    super(appMgr, id);
    logFileBasename = "eraser";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#initializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar("CCD Eraser", 1, axisID);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar("CCD Eraser", nSections, axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection()
    throws NumberFormatException, IOException {
    String line;
    while ((line = logFileReader.readLine()) != null) {
      if (line.startsWith("Section")) {
        String[] fields = line.split("\\s+");
        if (fields.length > 1) {
          String number = fields[1];
          currentSection =
            Integer.parseInt(number);
        }
      }
    }
  }
}
