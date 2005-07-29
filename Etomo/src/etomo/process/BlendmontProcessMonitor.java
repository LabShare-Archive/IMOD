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
  private boolean lastLineFound = false;
  private boolean doingMrctaper = false;
  private int mode;
  
  public BlendmontProcessMonitor(ApplicationManager appMgr, AxisID id, int mode) {
    super(appMgr, id);
    this.mode = mode;
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
    case BlendmontParam.UNDISTORT_MODE:
      title = "Distortion correction";
      break;
    case BlendmontParam.WHOLE_TOMOGRAM_SAMPLE_MODE:
      title = "Whole tomogram";
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
      else if (mode == BlendmontParam.BLEND_MODE || mode == BlendmontParam.WHOLE_TOMOGRAM_SAMPLE_MODE) {
        if (line.startsWith("Doing section #") && !doingMrctaper) {
          doingMrctaper = true;
          applicationManager.getMainPanel().setProgressBar(title + ": mrctaper", 1, axisID);
        }
        else if (currentSection >= nSections && line.startsWith("Done!")) {
          lastLineFound = true;
        }
      }
    }
    //Start timeout on last section
    if (currentSection >= nSections
        && ((mode != BlendmontParam.BLEND_MODE && mode != BlendmontParam.WHOLE_TOMOGRAM_SAMPLE_MODE) || lastLineFound)) {
      waitingForExit++;
    }
  }
  
  protected void findNSections() throws InterruptedException,
      NumberFormatException, IOException {
    Montagesize montagesize = null;
    montagesize = Montagesize.getInstance(applicationManager, axisID);
    try {
      montagesize.read();
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      throw new IOException(e.getMessage());
    }
    nSections = montagesize.getZ().getInt();
  }
}
/**
* <p> $Log$
* <p> Revision 1.10  2005/07/29 00:51:22  sueh
* <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
* <p> because the current manager changes when the user changes the tab.
* <p> Passing the manager where its needed.
* <p>
* <p> Revision 1.9  2005/07/14 21:59:41  sueh
* <p> bug# 626 Handling WHOLE_TOMOGRAM_SAMPLE_MODE.
* <p>
* <p> Revision 1.8  2005/06/21 00:44:42  sueh
* <p> bug# 522 findNSections():  simplifying Montagesize.getInstance().
* <p> Directory and dataset information can be retrieved when the stack is
* <p> constructed.
* <p>
* <p> Revision 1.7  2005/06/20 16:46:32  sueh
* <p> bug# 522 Changed  Montagesize so that read() is not called
* <p> automatically.
* <p>
* <p> Revision 1.6  2005/04/12 19:30:59  sueh
* <p> bug# 630 Handling mrctaper in full alignment.  End when mrctaper ends
* <p> and put "mrctaper" into the title when it begins.
* <p>
* <p> Revision 1.5  2005/04/07 21:53:15  sueh
* <p> bug# 626 Changed the title in undistort mode.
* <p>
* <p> Revision 1.4  2005/03/11 01:33:25  sueh
* <p> bug# 533 Setting a title in BlendmontParam.
* <p>
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