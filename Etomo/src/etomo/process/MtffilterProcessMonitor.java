package etomo.process;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.type.ViewType;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.17  2010/03/03 04:55:35  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 1.16  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.15  2009/09/17 19:15:42  sueh
 * <p> bug# 1257 Added FileSizeProcessMonitor.getModeBytes to handle getting the right number of bytes based on the mode in a single location.
 * <p>
 * <p> Revision 1.14  2009/03/17 00:42:04  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.13  2009/02/13 02:14:38  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.  Gave calcFileSize
 * <p> a return value.
 * <p>
 * <p> Revision 1.12  2006/10/24 21:27:24  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 1.11  2006/09/22 18:18:35  sueh
 * <p> bug# 931 Passing the process name to super().
 * <p>
 * <p> Revision 1.10  2006/08/11 00:16:54  sueh
 * <p> bug# 739 Added reloadWatchedFile(), loadComScriptManager(), and
 * <p> loadMtfFilterParam().
 * <p>
 * <p> Revision 1.9  2006/03/27 19:55:05  sueh
 * <p> bug# 836 Removed print statements
 * <p>
 * <p> Revision 1.8  2006/03/24 21:01:19  sueh
 * <p> bug# 836 In calcFileSize(), making file size a double to avoid overflow.
 * <p>
 * <p> Revision 1.7  2006/03/24 20:42:27  sueh
 * <p> bug# 836 In calcFileSize(), making file size a double to avoid overflow.
 * <p>
 * <p> Revision 1.6  2006/03/22 00:35:56  sueh
 * <p> bug# 836 Added temporary diagnostics to calcFileSize().
 * <p>
 * <p> Revision 1.5  2005/11/03 00:51:23  sueh
 * <p> bug# 740 In calcFileSize():  Getting file to open from Blendmont when
 * <p> montage is true.  Taking starting and ending Z into account.
 * <p>
 * <p> Revision 1.4  2005/07/29 00:52:06  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.3  2005/06/20 16:46:56  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in calcFileSize().
 * <p>
 * <p> Revision 1.2  2005/04/25 20:48:19  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.1  2005/01/26 04:27:15  sueh
 * <p> bug# 83 File size process monitor for mtffilter.
 * <p> </p>
 */
final class MtffilterProcessMonitor extends FileSizeProcessMonitor {
  public static final String rcsid = "$Id$";

  private final ApplicationManager applicationManager;

  private MTFFilterParam mtfFilterParam = null;
  private ComScriptManager comScriptManager = null;

  public MtffilterProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id, ProcessName.MTFFILTER);
    this.applicationManager = appMgr;
  }

  /* (non-Javadoc)
   * @see etomo.process.FileSizeProcessMonitor#calcFileSize() */
  boolean calcFileSize() throws InvalidParameterException, IOException {
    double nX;
    double nY;
    double nZ;
    double modeBytes = 1.0d;

    // Get the depth, mode, any mods to the X and Y size from the tilt
    // command script and the input and output filenames.
    loadComScriptManager();
    String outputFilename;
    if (manager.getViewType() == ViewType.MONTAGE) {
      comScriptManager.loadBlend(axisID);
      BlendmontParam blendmontParam = comScriptManager.getBlendParam(axisID);

      // Get the header from the raw stack to calculate the aligned stack stize
      outputFilename = manager.getPropertyUserDir() + "/"
          + blendmontParam.getImageOutputFile();
    }
    else {
      comScriptManager.loadNewst(axisID);
      NewstParam newstParam = comScriptManager.getNewstComNewstParam(axisID);

      // Get the header from the raw stack to calculate the aligned stack stize
      outputFilename = manager.getPropertyUserDir() + "/" + newstParam.getOutputFile();
    }
    MRCHeader outputHeader = MRCHeader.getInstance(manager.getPropertyUserDir(),
        outputFilename, axisID);
    if (!outputHeader.read(manager)) {
      return false;
    }
    nX = (double) outputHeader.getNRows();
    nY = (double) outputHeader.getNColumns();
    nZ = (double) outputHeader.getNSections();
    modeBytes = (double) getModeBytes(outputHeader.getMode());

    loadMtfFilterParam();
    // take starting and ending Z into account
    if (mtfFilterParam.isStartingZSet()) {
      if (mtfFilterParam.isEndingZSet()) {
        nZ = (double) mtfFilterParam.getEndingZ()
            - (double) mtfFilterParam.getStartingZ() + 1.0d;
      }
      else {
        nZ = nZ - (double) mtfFilterParam.getStartingZ() + 1.0d;
      }
    }
    else if (mtfFilterParam.isEndingZSet()) {
      nZ = (double) mtfFilterParam.getEndingZ();
    }

    // Assumption: newst will write the output file with the same mode as the
    // the input file
    double fileSize = 1024 + nX * (double) nY * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);
    manager.getMainPanel().setProgressBar("Running MTF filter", nKBytes, axisID);
    return true;
  }

  protected void reloadWatchedFile() {
    loadMtfFilterParam();
    watchedFile = new File(manager.getPropertyUserDir(), mtfFilterParam.getOutputFile());
  }

  private void loadComScriptManager() {
    if (comScriptManager != null) {
      return;
    }
    comScriptManager = applicationManager.getComScriptManager();
  }

  private void loadMtfFilterParam() {
    if (mtfFilterParam != null) {
      return;
    }
    loadComScriptManager();
    comScriptManager.loadMTFFilter(axisID);
    mtfFilterParam = comScriptManager.getMTFFilterParam(axisID);
  }
}
