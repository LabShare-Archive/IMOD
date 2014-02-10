package etomo.process;

import java.io.IOException;

import etomo.BaseManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.BlendmontParam.Mode;
import etomo.logic.DatasetTool;
import etomo.storage.LogFile;
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
  public static final String rcsid = "$Id$";

  private String title;
  private boolean lastLineFound = false;
  private boolean doingMrctaper = false;
  private Mode mode;

  public BlendmontProcessMonitor(BaseManager manager, AxisID id, Mode mode) {
    super(manager, id);
    this.mode = mode;
    logFileBasename = BlendmontParam.getProcessName(mode).toString();
    if (mode == BlendmontParam.Mode.XCORR) {
      title = "Cross-correlation";
    }
    else if (mode == BlendmontParam.Mode.PREBLEND) {
      title = "Coarse alignment";
    }
    else if (mode == BlendmontParam.Mode.BLEND
        || mode == BlendmontParam.Mode.BLEND_3DFIND) {
      title = "Full alignment";
    }
    else if (mode == BlendmontParam.Mode.UNDISTORT) {
      title = "Distortion correction";
    }
    else if (mode == BlendmontParam.Mode.WHOLE_TOMOGRAM_SAMPLE) {
      title = "Whole tomogram";
    }
    else if (mode == BlendmontParam.Mode.SERIAL_SECTION_PREBLEND) {
      title = "Initial blend";
    }
    else if (mode == BlendmontParam.Mode.SERIAL_SECTION_BLEND) {
      title = "Blend serial sections";
    }
  }

  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      manager.getMainPanel().setProgressBar(title + ": blendmont", 1, axisID);
      manager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    manager.getMainPanel().setProgressBar(title + ": blendmont", nSections, axisID);
  }

  protected void getCurrentSection() throws NumberFormatException, LogFile.LockException,
      IOException {
    String line;
    while ((line = readLogFileLine()) != null) {
      line = line.trim();
      if (line.startsWith("working on section #")) {
        String[] strings = line.split("\\s+");
        // set currentSection - section in log starts from 0
        currentSection = Integer.parseInt(strings[4]) + 1;
      }
      else if (mode == BlendmontParam.Mode.BLEND
          || mode == BlendmontParam.Mode.BLEND_3DFIND
          || mode == BlendmontParam.Mode.WHOLE_TOMOGRAM_SAMPLE) {
        if (line.startsWith("Doing section #") && !doingMrctaper) {
          doingMrctaper = true;
          manager.getMainPanel().setProgressBar(title + ": mrctaper", 1, axisID);
        }
        else if (currentSection >= nSections && line.startsWith("Done!")) {
          lastLineFound = true;
        }
      }
    }
    // Set ending on the last section
    if (currentSection >= nSections
        && ((mode != BlendmontParam.Mode.BLEND
            && mode != BlendmontParam.Mode.BLEND_3DFIND && mode != BlendmontParam.Mode.WHOLE_TOMOGRAM_SAMPLE) || lastLineFound)) {
      ending = true;
    }
  }

  protected void findNSections() throws InterruptedException, NumberFormatException,
      LogFile.LockException, InvalidParameterException, IOException {
    Montagesize montagesize = null;
    montagesize = Montagesize.getInstance(manager, axisID,
        DatasetTool.STANDARD_DATASET_EXT);
    montagesize.read(manager);
    nSections = montagesize.getZ().getInt();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.21  2011/02/22 03:58:51  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.20  2010/03/03 04:55:35  sueh
 * <p> bug# 1311 Removed unnecessary ProcessName references.
 * <p>
 * <p> Revision 1.19  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.18  2009/09/21 17:47:23  sueh
 * <p> bug# 1267 Handling the BLEND_3DFIND mode.
 * <p>
 * <p> Revision 1.17  2009/03/17 00:34:39  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.16  2009/02/04 23:23:29  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.15  2007/02/05 22:53:29  sueh
 * <p> bug# 962 Move comscript mode info to inner class.
 * <p>
 * <p> Revision 1.14  2006/10/24 21:17:41  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 1.13  2006/10/10 05:07:23  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 1.12  2005/08/24 22:34:58  sueh
 * <p> bug# 715 Changed BlendmontParam.getCommandFileName() to
 * <p> getProcessName() to make use of ProcessName.
 * <p>
 * <p> Revision 1.11  2005/07/29 19:46:04  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
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
