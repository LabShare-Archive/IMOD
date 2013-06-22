package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.type.ViewType;
import etomo.util.DatasetFiles;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.7  2010/03/03 04:55:35  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 1.6  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.5  2009/09/17 19:15:31  sueh
 * <p> bug# 1257 Added FileSizeProcessMonitor.getModeBytes to handle getting the right number of bytes based on the mode in a single location.
 * <p>
 * <p> Revision 1.4  2009/03/17 00:35:38  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.3  2009/02/13 02:14:09  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.  Gave calcFileSize
 * <p> a return value.
 * <p>
 * <p> Revision 1.2  2008/10/27 23:19:30  sueh
 * <p> bug# 1141 Fixed monitor - log file doesn't show the watched file's name, so turn off findWatchedFileName to avoid looking for it.
 * <p>
 * <p> Revision 1.1  2008/10/27 17:53:11  sueh
 * <p> bug# 1141 Monitor for ctfcorrection.com.
 * <p> </p>
 */
public final class CtfCorrectionMonitor extends FileSizeProcessMonitor {
  public static final String rcsid = "$Id$";

  private final ApplicationManager applicationManager;

  private ComScriptManager comScriptManager = null;

  public CtfCorrectionMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id, ProcessName.CTF_CORRECTION);
    this.applicationManager = appMgr;
    setFindWatchedFileName(false);
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
    // Assumption: newst will write the output file with the same mode as the
    // the input file
    double fileSize = 1024.0d + nX * (double) nY * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);
    manager.getMainPanel().setProgressBar("Running CTF Correction", nKBytes, axisID);
    return true;
  }

  void reloadWatchedFile() {
    watchedFile = DatasetFiles.getCtfCorrectionFile(applicationManager, axisID);
  }

  private void loadComScriptManager() {
    if (comScriptManager != null) {
      return;
    }
    comScriptManager = applicationManager.getComScriptManager();
  }
}
