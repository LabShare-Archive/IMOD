package etomo.process;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.comscript.ComScriptManager;
import etomo.comscript.TiltParam;
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
 * <p> Revision 1.3  2003/07/31 22:54:17  rickg
 * <p> 2GB fix.  An intermediate setp in the fileSize calculation over
 * <p> flowed the temporay int that was created.
 * <p>
 * <p> Revision 1.2  2003/07/01 19:30:06  rickg
 * <p> Added mode bytes handling
 * <p> Get input and output filenames from tilt?.com
 * <p>
 * <p> Revision 1.1  2003/06/27 20:29:25  rickg
 * <p> Initial rev (in progress)
 * <p> </p>
 */

public class TiltProcessMonitor extends FileSizeProcessMonitor {
  public static final String rcsid =
    "$Id$";

  public TiltProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
  }

  /* (non-Javadoc)
   * @see etomo.process.FileSizeProcessMonitor#calcFileSize()
   */
  void calcFileSize() throws InvalidParameterException, IOException {
    int nX;
    int nY;
    int nZ;
    int modeBytes = 4;

    // Get the depth, mode, any mods to the X and Y size from the tilt 
    // command script and the input and output filenames. 
    ComScriptManager comScriptManager =
      applicationManager.getComScriptManager();
    comScriptManager.loadTilt(axisID);
    TiltParam tiltParam = comScriptManager.getTiltParam(axisID);

    // Get the header from the aligned stack to use as default nX and
    // nY parameters
    String alignedFilename =
      System.getProperty("user.dir") + "/" + tiltParam.getInputFile();

    MRCHeader alignedStack = new MRCHeader(alignedFilename);
    alignedStack.read();

    nX = alignedStack.getNRows();
    nY = alignedStack.getNColumns();

    nZ = tiltParam.getThickness();
    if (tiltParam.hasWidth()) {
      nX = tiltParam.getWidth();
    }
    if (tiltParam.hasSlice()) {
      nY =
        (tiltParam.getIdxSliceStop() - tiltParam.getIdxSliceStart())
          / tiltParam.getIdxSliceIncr();
    }
    if (tiltParam.hasMode()) {
      switch (tiltParam.getMode()) {
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
    }
    long fileSize = 1024 + (long) nX * nY * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);

    applicationManager.setProgressBar("Calculating tomogram", nKBytes, axisID);

    // Create a file object describing the file to be monitored
    watchedFile =
      new File(
        System.getProperty("user.dir"),
        tiltParam.getOutputFile());
  }
}
