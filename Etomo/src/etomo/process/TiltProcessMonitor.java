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
 * <p> Revision 3.7  2004/11/19 23:26:09  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.6.4.2  2004/10/11 02:04:52  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.6.4.1  2004/09/29 19:11:46  sueh
 * <p> bug# 520 Removing pass-through function calls.
 * <p>
 * <p> Revision 3.6  2004/06/18 05:36:59  rickg
 * <p> Handle y slice when step size not specified
 * <p>
 * <p> Revision 3.5  2004/06/17 23:55:51  rickg
 * <p> Bug #460 moved getting of current time into FileSizeProcessMonitor on
 * <p> instantiation
 * <p>
 * <p> Revision 3.4  2004/06/17 23:34:32  rickg
 * <p> Bug #460 added script starting time to differentiate old data files
 * <p>
 * <p> Revision 3.3  2004/06/17 23:06:21  rickg
 * <p> Bug #460 Using nio FileChannle.size() method to monitor file since it seems 
 * <p> to be much more reliable than the File.length() method
 * <p>
 * <p> Revision 3.2  2004/06/14 17:26:47  sueh
 * <p> bug# 460 set startTime in the constructor
 * <p>
 * <p> Revision 3.1  2004/03/24 03:09:28  rickg
 * <p> Tilt getter name change for incrSlice
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
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
      applicationManager.getPropertyUserDir() + "/" + tiltParam.getInputFile();

    MRCHeader alignedStack = new MRCHeader(alignedFilename, axisID);
    alignedStack.read();

    nX = alignedStack.getNRows();
    nY = alignedStack.getNColumns();

    nZ = tiltParam.getThickness();
    if (tiltParam.hasWidth()) {
      nX = tiltParam.getWidth();
    }
    if (tiltParam.hasSlice()) {
      int sliceRange = tiltParam.getIdxSliceStop() - tiltParam.getIdxSliceStart();
      // Divide by the step size if present
      if(tiltParam.getIncrSlice() == Integer.MIN_VALUE) {
        nY = sliceRange;
      }
      else {
        nY = sliceRange / tiltParam.getIncrSlice();
      }
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

    applicationManager.getMainPanel().setProgressBar("Calculating tomogram", nKBytes, axisID);

    // Create a file object describing the file to be monitored
    watchedFile =
      new File(
        applicationManager.getPropertyUserDir(),
        tiltParam.getOutputFile());
  }
}
