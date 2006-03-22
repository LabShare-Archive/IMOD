package etomo.process;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
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
public class MtffilterProcessMonitor extends FileSizeProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  public MtffilterProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
  }

  /* (non-Javadoc)
   * @see etomo.process.FileSizeProcessMonitor#calcFileSize()
   */
  void calcFileSize() throws InvalidParameterException, IOException {
    //TEMP bug# 839
    System.err.println("MtffilterProcessMonitor.calcFileSize:");
    int nX;
    int nY;
    int nZ;
    int modeBytes = 1;

    // Get the depth, mode, any mods to the X and Y size from the tilt 
    // command script and the input and output filenames. 
    ComScriptManager comScriptManager = applicationManager.getComScriptManager();
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
        .getPropertyUserDir(), outputFilename, axisID);
    outputHeader.read();
    nX = outputHeader.getNRows();
    nY = outputHeader.getNColumns();
    nZ = outputHeader.getNSections();
    switch (outputHeader.getMode()) {
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
    //take starting and ending Z into account
    comScriptManager.loadMTFFilter(axisID);
    MTFFilterParam mtfFilterParam = comScriptManager.getMTFFilterParam(axisID);
    if (mtfFilterParam.isStartingZSet()) {
      if (mtfFilterParam.isEndingZSet()) {
        nZ = mtfFilterParam.getEndingZ() - mtfFilterParam.getStartingZ() + 1;
      }
      else {
        nZ = nZ - mtfFilterParam.getStartingZ() + 1;
      }
    }
    else if (mtfFilterParam.isEndingZSet()) {
      nZ = mtfFilterParam.getEndingZ();
    }

    // Assumption: newst will write the output file with the same mode as the
    // the input file 
    long fileSize = 1024 + nX * nY * nZ * modeBytes;
    //TEMP bug# 839
    System.err.println("fileSize="+fileSize);
    nKBytes = (int) (fileSize / 1024);
    //TEMP bug# 839
    System.err.println("nKBytes="+nKBytes);
    applicationManager.getMainPanel().setProgressBar("Running MTF filter",
        nKBytes, axisID);

    // Create a file object describing the file to be monitored
    comScriptManager.loadMTFFilter(axisID);
    MTFFilterParam mtffilterParam = comScriptManager.getMTFFilterParam(axisID);
    watchedFile = new File(applicationManager.getPropertyUserDir(), mtffilterParam
      .getOutputFile());
    //TEMP bug# 839
    System.err.println("watchedFile="+watchedFile.getAbsolutePath());
  }
}
