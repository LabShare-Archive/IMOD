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
 * <p> Revision 3.20  2009/03/17 00:42:47  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.19  2009/02/13 02:14:49  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.  Gave calcFileSize
 * <p> a return value.
 * <p>
 * <p> Revision 3.18  2009/02/04 23:26:32  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 3.17  2008/01/31 20:18:55  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 3.16  2007/09/11 21:27:01  sueh
 * <p> bug# 1035 In calcFileSize prevent integer overflow when calculating fileSize by
 * <p> casting nX * xY to long.  Getting nX and nY from
 * <p> NewstParam.sizeToOutputInXandY when set.
 * <p>
 * <p> Revision 3.15  2006/10/24 21:27:44  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 3.14  2006/10/10 05:10:53  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 3.13  2006/09/22 18:18:47  sueh
 * <p> bug# 931 Passing the process name to super().
 * <p>
 * <p> Revision 3.12  2006/08/11 00:17:18  sueh
 * <p> bug# 739 Added reloadWatchedFile() and loadNewstParam().
 * <p>
 * <p> Revision 3.11  2006/08/09 20:14:34  sueh
 * <p> bug# 631 Adding usingLog().
 * <p>
 * <p> Revision 3.10  2005/07/29 00:52:12  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.9  2005/06/20 16:47:14  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in calcFileSize().
 * <p>
 * <p> Revision 3.8  2005/04/25 20:48:32  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.7  2004/11/19 23:22:41  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.6.4.2  2004/10/11 02:03:40  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.6.4.1  2004/09/29 19:09:05  sueh
 * <p> bug# 520 Removing pass-through function calls.
 * <p>
 * <p> Revision 3.6  2004/06/17 23:55:51  rickg
 * <p> Bug #460 moved getting of current time into FileSizeProcessMonitor on
 * <p> instantiation
 * <p>
 * <p> Revision 3.5  2004/06/17 23:34:17  rickg
 * <p> Bug #460 added script starting time to differentiate old data files
 * <p>
 * <p> Revision 3.4  2004/04/26 00:22:32  rickg
 * <p> bug# 426 Changed progress bar text to reflect that it is not
 * <p> always the final alignment
 * <p>
 * <p> Revision 3.3  2004/04/08 16:59:27  rickg
 * <p> Account for binning in newstack command
 * <p>
 * <p> Revision 3.2  2004/02/13 01:05:13  rickg
 * <p> Simplified ouputFile for newstack
 * <p>
 * <p> Revision 3.1  2004/02/13 00:09:51  rickg
 * <p> Updated for PIP based newstack
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.1  2003/07/01 19:26:56  rickg
 * <p> Initial revision
 * <p> </p>
 */
package etomo.process;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import etomo.ApplicationManager;
import etomo.comscript.ComScriptManager;
import etomo.comscript.NewstParam;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

final class NewstProcessMonitor extends FileSizeProcessMonitor {
  public static final String rcsid = "$Id$";

  //private BufferedReader logReader = null;
  private boolean gotStatusFromLog = false;
  private NewstParam newstParam = null;

  public NewstProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id, ProcessName.NEWST);
  }

  /**
   * Set gettingStatusFromLog to true and return true if mrctaper is being run
   */
  protected boolean gotStatusFromLog() {
    //log already in use, return true
    if (gotStatusFromLog) {
      return true;
    }
    //read the lines available in the log and look for a line shows that
    //mrctaper started
    String line;
    LogFile.ReaderId logReaderId = null;
    try {
      logReaderId = getLogFile().openReader();
    }
    catch (LogFile.LockException e) {
      return false;
    }
    catch (FileNotFoundException e) {
      return false;
    }
    try {
      while ((line = getLogFile().readLine(logReaderId)) != null) {
        if (line.startsWith("Doing section")) {
          //mrctaper started
          applicationManager.getMainPanel().setProgressBarValue(0, "mrctaper",
              axisID);
          gotStatusFromLog = true;
          getLogFile().closeReader(logReaderId);
          logReaderId = null;
          return true;
        }
      }
    }
    //there is a problem with the log
    catch (LogFile.LockException e) {
      return false;
    }
    catch (IOException e) {
      return false;
    }
    if (logReaderId != null && !logReaderId.isEmpty()) {
      getLogFile().closeReader(logReaderId);
      logReaderId = null;//added this
    }
    //did not find a line shows that mrctaper started
    return false;
  }

  /* (non-Javadoc)
   * @see etomo.process.FileSizeProcessMonitor#calcFileSize()
   */
  boolean calcFileSize() throws InvalidParameterException, IOException {
    int nX;
    int nY;
    int nZ;
    int modeBytes = 1;

    // Get the depth, mode, any mods to the X and Y size from the tilt 
    // command script and the input and output filenames. 
    loadNewstParam();
    // Get the header from the raw stack to calculate the aligned stack stize
    String rawStackFilename = applicationManager.getPropertyUserDir() + "/"
        + newstParam.getInputFile();
    MRCHeader rawStack = MRCHeader.getInstance(applicationManager
        .getPropertyUserDir(), rawStackFilename, axisID, applicationManager
        .getManagerKey());
    if (!rawStack.read()) {
      return false;
    }
    if (newstParam.isSizeToOutputInXandYSet()) {
      nX = newstParam.getSizeToOutputInX();
      nY = newstParam.getSizeToOutputInY();
    }
    else {
      nX = rawStack.getNRows();
      nY = rawStack.getNColumns();
    }
    nZ = rawStack.getNSections();
    System.err.println("NewstProcessMonitor.calcFileSize:rawStack.getMode()="+rawStack.getMode());
    switch (rawStack.getMode()) {
    case 0:
      modeBytes = 1;
      break;
    case 1:
      modeBytes = 2;
      break;
    case 2:
      modeBytes = 4;
      break;
    case 3:
      modeBytes = 4;
      break;
    case 4:
      modeBytes = 8;
      break;
    case 16:
      modeBytes = 3;
      break;
    default:
      throw new InvalidParameterException("Unknown mode parameter");
    }

    // Get the binByFactor from newst.com script
    int binBy = newstParam.getBinByFactor();
    System.err.println("NewstProcessMonitor.calcFileSize:binBy="+binBy);
    // If the bin by factor is unspecified it defaults to 1
    if (binBy > 1) {
      nX = nX / binBy;
      nY = nY / binBy;
    }

    // Assumption: newst will write the output file with the same mode as the
    // the input file 
    long fileSize = 1024 + ((long) nX * nY) * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);
    applicationManager.getMainPanel().setProgressBar("Creating aligned stack",
        nKBytes, axisID, processName);
    return true;
  }

  private void loadNewstParam() {
    if (newstParam != null) {
      return;
    }
    ComScriptManager comScriptManager = applicationManager
        .getComScriptManager();
    comScriptManager.loadNewst(axisID);
    newstParam = comScriptManager.getNewstComNewstParam(axisID);
  }

  protected void reloadWatchedFile() {
    loadNewstParam();
    // Create a file object describing the file to be monitored
    watchedFile = new File(applicationManager.getPropertyUserDir(), newstParam
        .getOutputFile());
  }
}