package etomo.process;

import etomo.ApplicationManager;
import etomo.comscript.ComScriptManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.TiltParam;
import etomo.comscript.TiltalignParam;
import etomo.type.AxisID;
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
 * <p> $Log$ </p>
 */

public class TiltProcessMonitor extends FileSizeProcessMonitor {
  public static final String rcsid = "$Id$";

  public TiltProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
  }

  /* (non-Javadoc)
   * @see etomo.process.FileSizeProcessMonitor#calcFileSize()
   */
  long calcFileSize() {
    int nX;
    int nY;
    int nZ;
    int modeBytes = 4;

    // Get the header from the aligned aligned stack to use as default nX and
    // nY parameters
    String alignedFilename =
      System.getProperty("user.dir")
        + applicationManager.getDatasetName()
        + axisID.getExtension()
        + "ali";

    MRCHeader alignedStack = new MRCHeader(alignedFilename);
    nX = alignedStack.getNRows();
    nY = alignedStack.getNColumns();

    // Get the depth, mode and any mods to the X and Y size from the tilt 
    // command script
    ComScriptManager comScriptManager =
      applicationManager.getComScriptManager();

    //  FIXME: can we assume that the latest tilt param is loaded?
    TiltParam tiltParam = comScriptManager.getTiltParam(axisID);
    nZ = tiltParam.getThickness();
    if(tiltParam.has)
    return 1024 + nX * nY * modeBytes;
  }
}
