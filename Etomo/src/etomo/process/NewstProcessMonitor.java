package etomo.process;

import java.io.File;
import java.io.IOException;
import java.util.Vector;
import etomo.ApplicationManager;
import etomo.comscript.ComScriptManager;
import etomo.comscript.NewstParam;
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
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.1  2003/07/01 19:26:56  rickg
 * <p> Initial revision
 * <p> </p>
 */

public class NewstProcessMonitor extends FileSizeProcessMonitor {
  public static final String rcsid = "$Id$";

  public NewstProcessMonitor(ApplicationManager appMgr, AxisID id) {
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

    // Get the depth, mode, any mods to the X and Y size from the tilt 
    // command script and the input and output filenames. 
    ComScriptManager comScriptManager = 
    applicationManager.getComScriptManager();
    comScriptManager.loadNewst(axisID);
    NewstParam newstParam = comScriptManager.getNewstComNewstParam(axisID);

    // Get the header from the raw stack to calculate the aligned stack stize
    String rawStackFilename = 
    System.getProperty("user.dir") + "/" + newstParam.getInputFile();
    MRCHeader rawStack = new MRCHeader(rawStackFilename);
    rawStack.read();
    nX = rawStack.getNRows();
    nY = rawStack.getNColumns();
    nZ = rawStack.getNSections();
    switch (rawStack.getMode()) {
      case 0 :
        modeBytes = 1;
        break;
      case 1 :
        modeBytes = 2;
        break;
      case 2 :
        modeBytes = 4;
        break;
      case 3 :
        modeBytes = 4;
        break;
      case 4 :
        modeBytes = 8;
        break;
      case 16 :
        modeBytes = 3;
        break;
      default :
        throw new InvalidParameterException("Unknown mode parameter");
    }

    // Assumption: newst will write the output file with the same mode as the
    // the input file 
    long fileSize = 1024 + nX * nY * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);
    applicationManager.setProgressBar("Creating final stack", nKBytes, axisID);

    // Create a file object describing the file to be monitored
    Vector outputFileList = newstParam.getOutputFile();
    if (outputFileList.size() != 1) {
      throw new InvalidParameterException(
        "Unable to watch multiple output files for newstack");
    }
    watchedFile = 
    new File(System.getProperty("user.dir"), (String) outputFileList.get(0));
  }
}
