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
 * <p> Revision 3.3.4.3  2004/10/11 02:03:50  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.3.4.2  2004/09/29 19:09:42  sueh
 * <p> bug# 520 Removing pass-through function calls.
 * <p>
 * <p> Revision 3.3.4.1  2004/09/07 17:55:50  sueh
 * <p> bug# 520 getting dataset name from metadata
 * <p>
 * <p> Revision 3.3  2004/06/17 23:55:51  rickg
 * <p> Bug #460 moved getting of current time into FileSizeProcessMonitor on
 * <p> instantiation
 * <p>
 * <p> Revision 3.2  2004/06/17 23:34:17  rickg
 * <p> Bug #460 added script starting time to differentiate old data files
 * <p>
 * <p> Revision 3.1  2004/04/08 16:59:27  rickg
 * <p> Account for binning in newstack command
 * <p>
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
    String dataSetPath = applicationManager.getPropertyUserDir() + "/"
      + applicationManager.getMetaData().getDatasetName() + axisID.getExtension();

    MRCHeader rawStack = new MRCHeader(dataSetPath + ".st");
    rawStack.read();

    nX = rawStack.getNRows();
    nY = rawStack.getNColumns();
    nZ = rawStack.getNSections();

    // Get the binByFactor from prenewst.com script
    ComScriptManager comScriptManager = applicationManager
      .getComScriptManager();
    comScriptManager.loadPrenewst(axisID);
    NewstParam prenewstParam = comScriptManager.getPrenewstParam(axisID);
    int binBy = prenewstParam.getBinByFactor();
    // If the bin by factor is unspecified it defaults to 1
    if (binBy > 1) {
      nX = nX / binBy;
      nY = nY / binBy;
    }
    long fileSize = 1024 + nX * nY * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);

    applicationManager.getMainPanel().setProgressBar("Creating coarse stack", nKBytes, axisID);

    // Create a file object describing the file to be monitored
    watchedFile = new File(dataSetPath + ".preali");
  }
}