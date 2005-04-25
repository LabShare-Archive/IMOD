package etomo.process;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.comscript.ComScriptManager;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
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
* <p> Revision 1.1  2005/01/26 04:27:15  sueh
* <p> bug# 83 File size process monitor for mtffilter.
* <p> </p>
*/
public class MtffilterProcessMonitor extends FileSizeProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  public MtffilterProcessMonitor(ApplicationManager appMgr, AxisID id) {
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
    ComScriptManager comScriptManager = applicationManager.getComScriptManager();
    comScriptManager.loadNewst(axisID);
    NewstParam newstParam = comScriptManager.getNewstComNewstParam(axisID);

    // Get the header from the raw stack to calculate the aligned stack stize
    String newstOutputFilename = applicationManager.getPropertyUserDir() + "/"
      + newstParam.getOutputFile();
    MRCHeader newstOutputHeader = new MRCHeader(newstOutputFilename, axisID);
    newstOutputHeader.read();
    nX = newstOutputHeader.getNRows();
    nY = newstOutputHeader.getNColumns();
    nZ = newstOutputHeader.getNSections();
    switch (newstOutputHeader.getMode()) {
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
    applicationManager.getMainPanel()
      .setProgressBar("Running MTF filter", nKBytes, axisID);

    // Create a file object describing the file to be monitored
    comScriptManager.loadMTFFilter(axisID);
    MTFFilterParam mtffilterParam = comScriptManager.getMTFFilterParam(axisID);
    watchedFile = new File(applicationManager.getPropertyUserDir(), mtffilterParam
      .getOutputFile());
  }

}
