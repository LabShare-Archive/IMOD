package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.type.AxisID;
import etomo.util.InvalidParameterException;
import etomo.util.Montagesize;

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
*/
public class BlendmontProcessMonitor extends LogFileProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  private String title;
  
  public BlendmontProcessMonitor(ApplicationManager appMgr, AxisID id, int mode) {
    super(appMgr, id);
    logFileBasename = BlendmontParam.getCommandFileName(mode);
    switch (mode) {
    case BlendmontParam.XCORR_MODE:
      title = "Cross-correlation";
      break;
    case BlendmontParam.PREBLEND_MODE:
      title = "Coarse alignment";
      break;
    case BlendmontParam.BLEND_MODE:
      title = "Full alignment";
      break;

    }
  }
  
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar(title + ": blendmont", 1, axisID);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar(title + ": blendmont", nSections, axisID);
  }
  
  protected void getCurrentSection() throws NumberFormatException, IOException {
    String line;
    while ((line = logFileReader.readLine()) != null) {
      line = line.trim();
      if (line.startsWith("working on section #")) {
        String[] strings = line.split("\\s+");
        //set currentSection - section in log starts from 0
        currentSection = Integer.parseInt(strings[4]) + 1;
      }
    }
    //Start timeout on last section
    if (currentSection >= nSections) {
      waitingForExit++;
    }
  }
  
  protected void findNSections() throws InterruptedException,
      NumberFormatException, IOException {
    Montagesize montagesize = null;
    try {
    montagesize = Montagesize.getInstance(applicationManager
        .getPropertyUserDir(), applicationManager.getMetaData()
        .getDatasetName(), axisID);
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      throw new IOException(e.getMessage());
    }
    nSections = montagesize.getZ().getInteger();
  }
}
/**
* <p> $Log$
* <p> Revision 1.3  2005/03/09 22:28:07  sueh
* <p> bug# 533 Documenting how timeout starts.  Timeout is necessary when
* <p> blendmont is run from xcorr since it finishes before tiltxcorr and no
* <p> interrupt is sent until xcorr.com is complete.
* <p>
* <p> Revision 1.2  2005/03/09 18:01:36  sueh
* <p> bug# 533 Passing the mode (xcorr, blend, preblend) to the blendmont
* <p> process monitor.
* <p>
* <p> Revision 1.1  2005/03/08 01:56:09  sueh
* <p> bug# 533 Process monitor for blendmont.
* <p> </p>
*/