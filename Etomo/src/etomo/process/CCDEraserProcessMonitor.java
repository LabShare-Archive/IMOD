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
 * <p> $Log$ </p>
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
    applicationManager.setProgressBar("CCD Eraser", nSections, axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection()
    throws NumberFormatException, IOException {
    String line;
    while ((line = logFileBuffer.readLine()) != null) {
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
