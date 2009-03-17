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

  private ComScriptManager comScriptManager = null;

  public CtfCorrectionMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id, ProcessName.CTF_CORRECTION);
    setFindWatchedFileName(false);
  }

  /* (non-Javadoc)
   * @see etomo.process.FileSizeProcessMonitor#calcFileSize()
   */
  boolean calcFileSize() throws InvalidParameterException, IOException {
    double nX;
    double nY;
    double nZ;
    double modeBytes = 1.0d;

    // Get the depth, mode, any mods to the X and Y size from the tilt 
    // command script and the input and output filenames. 
    loadComScriptManager();
    String outputFilename;
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      comScriptManager.loadBlend(axisID);
      BlendmontParam blendmontParam = comScriptManager.getBlendParam(axisID);

      // Get the header from the raw stack to calculate the aligned stack stize
      outputFilename = applicationManager.getPropertyUserDir() + "/"
          + blendmontParam.getImageOutputFile();
    }
    else {
      comScriptManager.loadNewst(axisID);
      NewstParam newstParam = comScriptManager.getNewstComNewstParam(axisID);

      // Get the header from the raw stack to calculate the aligned stack stize
      outputFilename = applicationManager.getPropertyUserDir() + "/"
          + newstParam.getOutputFile();
    }
    MRCHeader outputHeader = MRCHeader.getInstance(applicationManager
        .getPropertyUserDir(), outputFilename, axisID, applicationManager
        .getManagerKey());
    if (!outputHeader.read()) {
      return false;
    }
    nX = (double) outputHeader.getNRows();
    nY = (double) outputHeader.getNColumns();
    nZ = (double) outputHeader.getNSections();
    switch (outputHeader.getMode()) {
    case 0:
      modeBytes = 1.0d;
      break;
    case 1:
      modeBytes = 2.0d;
      break;
    case 2:
      modeBytes = 4.0d;
      break;
    case 3:
      modeBytes = 4.0d;
      break;
    case 4:
      modeBytes = 8.0d;
      break;
    case 16:
      modeBytes = 3.0d;
      break;
    default:
      throw new InvalidParameterException("Unknown mode parameter");
    }
    // Assumption: newst will write the output file with the same mode as the
    // the input file 
    double fileSize = 1024.0d + nX * nY * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);
    applicationManager.getMainPanel().setProgressBar("Running CTF Correction",
        nKBytes, axisID, ProcessName.CTF_CORRECTION);
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
