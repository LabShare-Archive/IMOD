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
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.1  2003/07/01 19:26:56  rickg
 * <p> Initial revision
 * <p> </p>
 */
package etomo.process;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.comscript.ComScriptManager;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;


public class PrenewstProcessMonitor extends FileSizeProcessMonitor {

  public static final String rcsid = "$Id$";

  public PrenewstProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
  }

  /**
   * Calculate the expected files size in kBytes from the size of the current
   * stack and newstack binBy parameter.  The assumption for prenewst.com is
   * that the mode is always 0 (1 byte per pixel).
   */
  void calcFileSize() throws InvalidParameterException, IOException {
    int nX;
    int nY;
    int nZ;
    int modeBytes = 1;

    // Get the header from the raw stack to calculate the aligned stack stize
    String dataSetPath =
      System.getProperty("user.dir")
        + "/"
        + applicationManager.getDatasetName()
        + axisID.getExtension();

    MRCHeader rawStack = new MRCHeader(dataSetPath + ".st");
    rawStack.read();

    nX = rawStack.getNRows();
    nY = rawStack.getNColumns();
    nZ = rawStack.getNSections();

    // Get the binByFactor from prenewst.com script
    ComScriptManager comScriptManager = 
    applicationManager.getComScriptManager();
    comScriptManager.loadPrenewst(axisID);
    NewstParam prenewstParam = comScriptManager.getPrenewstParam(axisID);
    int binBy = prenewstParam.getBinByFactor();
    // If the bin by factor is unspecified it defaults to 1
    if(binBy > 1) {
      nX = nX / binBy;
      nY = nY / binBy;
    }
    long fileSize = 1024 + nX * nY * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);

    applicationManager.setProgressBar("Creating coarse stack", nKBytes, axisID);

    // Create a file object describing the file to be monitored
    watchedFile =
      new File(dataSetPath + ".preali");
  }
}
