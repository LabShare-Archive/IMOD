package etomo.process;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

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
 * <p> Revision 1.1  2003/07/01 19:26:56  rickg
 * <p> Initial revision
 * <p> </p>
 */

public class PrenewstProcessMonitor extends FileSizeProcessMonitor {

  public static final String rcsid = "$Id$";

  public PrenewstProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
  }

  /* (non-Javadoc)
   * @see etomo.process.FileSizeProcessMonitor#calcFileSize()
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

    long fileSize = 1024 + nX * nY * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);

    applicationManager.setProgressBar("Creating coarse stack", nKBytes, axisID);

    // Create a file object describing the file to be monitored
    watchedFile =
      new File(dataSetPath + ".preali");
  }
}
