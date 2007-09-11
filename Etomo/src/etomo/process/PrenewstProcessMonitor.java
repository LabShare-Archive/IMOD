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
 * <p> Revision 3.10  2006/10/24 21:37:46  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 3.9  2006/09/22 18:18:58  sueh
 * <p> bug# 931 Passing the process name to super().
 * <p>
 * <p> Revision 3.8  2006/08/11 00:17:41  sueh
 * <p> bug# 739 Added reloadWatchedFile() and loadDatasetPath().
 * <p>
 * <p> Revision 3.7  2005/07/29 00:52:18  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.6  2005/06/20 16:47:22  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in calcFileSize().
 * <p>
 * <p> Revision 3.5  2005/04/25 20:48:47  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.4  2004/11/19 23:24:10  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
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
import etomo.type.ProcessName;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

public class PrenewstProcessMonitor extends FileSizeProcessMonitor {
  public static final String rcsid = "$Id$";

  private String dataSetPath = null;
  
  public PrenewstProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id, ProcessName.PRENEWST);
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

    // Get the header from the raw stack to calculate the aligned stack size
    loadDataSetPath();
    MRCHeader rawStack = MRCHeader.getInstance(applicationManager
        .getPropertyUserDir(), dataSetPath + ".st", axisID);
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
    long fileSize = 1024 + ((long) nX * nY) * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);
    applicationManager.getMainPanel().setProgressBar("Creating coarse stack", nKBytes, axisID,ProcessName.PRENEWST);
  }
  
  protected void reloadWatchedFile() {
    loadDataSetPath();
    // Create a file object describing the file to be monitored
    watchedFile = new File(dataSetPath + ".preali");
  }
  
  private void loadDataSetPath() {
    if (dataSetPath != null) {
      return;
    }
    dataSetPath = applicationManager.getPropertyUserDir() + "/"
    + applicationManager.getMetaData().getDatasetName() + axisID.getExtension();
  }
}