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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import etomo.ApplicationManager;
import etomo.comscript.ComScriptManager;
import etomo.comscript.NewstParam;
import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.util.DatasetFiles;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

final class NewstProcessMonitor extends FileSizeProcessMonitor {
  public static final String rcsid = "$Id$";

  private BufferedReader logReader = null;
  private boolean logAvailable = true;
  private boolean usingLog = false;
  private NewstParam newstParam = null;

  public NewstProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
  }

  /**
   * Set usingLog to true and return true if mrctaper is being run
   * If there is a problem accessing the log, set logAvailable to false and stop
   * checking.
   */
  protected boolean usingLog() {
    //log already in use, return true
    if (usingLog) {
      return true;
    }
    //log could not be opened, return false
    if (!logAvailable) {
      return false;
    }
    //create logReader
    if (logReader == null) {
      try {
        logReader = new BufferedReader(new FileReader(DatasetFiles.getLogFile(
            applicationManager, axisID, ProcessName.NEWST)));
      }
      //set logAvailable to false if there is a problem with the log
      catch (IOException e) {
        logAvailable = false;
        return false;
      }
      if (logReader == null) {
        logAvailable = false;
        return false;
      }
    }
    //read the lines available in the log and look for a line shows that
    //mrctaper started
    String line;
    try {
      while ((line = logReader.readLine()) != null) {
        if (line.startsWith("Doing section")) {
          //mrctaper started
          applicationManager.getMainPanel().setProgressBarValue(0, "mrctaper",
              axisID);
          usingLog = true;
          try {
            logReader.close();
          }
          catch (IOException e) {
          }
          return true;
        }
      }
    }
    //set logAvailable to false if there is a problem with the log
    catch (IOException e) {
      logAvailable = false;
      return false;
    }
    //did not find a line shows that mrctaper started
    return false;
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
    loadNewstParam();
    // Get the header from the raw stack to calculate the aligned stack stize
    String rawStackFilename = applicationManager.getPropertyUserDir() + "/"
        + newstParam.getInputFile();
    MRCHeader rawStack = MRCHeader.getInstance(applicationManager
        .getPropertyUserDir(), rawStackFilename, axisID);
    rawStack.read();
    nX = rawStack.getNRows();
    nY = rawStack.getNColumns();
    nZ = rawStack.getNSections();
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
    // If the bin by factor is unspecified it defaults to 1
    if (binBy > 1) {
      nX = nX / binBy;
      nY = nY / binBy;
    }

    // Assumption: newst will write the output file with the same mode as the
    // the input file 
    long fileSize = 1024 + nX * nY * nZ * modeBytes;
    nKBytes = (int) (fileSize / 1024);
    applicationManager.getMainPanel().setProgressBar("Creating aligned stack",
        nKBytes, axisID);
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