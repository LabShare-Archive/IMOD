package etomo.process;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Map;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.10  2011/02/03 06:03:18  sueh
 * <p> bug# 1422 No need parallel progress display passed to the constructor.
 * <p>
 * <p> Revision 1.9  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.8  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.7  2009/04/20 20:01:34  sueh
 * <p> bug# 1192  Constructing with Map computerMap instead of String
 * <p> computerList.
 * <p>
 * <p> Revision 1.6  2009/03/17 00:43:11  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.5  2009/02/04 23:26:53  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.4  2008/01/14 21:33:23  sueh
 * <p> big# 1050 Removed protected modifiers since classes in this familly aren't being
 * <p> inherited by classes outside their package.
 * <p>
 * <p> Revision 1.3  2007/12/10 22:29:58  sueh
 * <p> bug# 1041 Removed subdirName from the super constructor because it is optional and never used in this class.
 * <p>
 * <p> Revision 1.2  2007/11/06 19:26:15  sueh
 * <p> bug# 1047 Change super() call to adapt to parent constructor change.
 * <p>
 * <p> Revision 1.1  2006/12/02 04:39:05  sueh
 * <p> bug# 944 A Processchunks monitor which does specialized process for
 * <p> volcombine.  Watches volcombine-start.log and volcombine-finish.log
 * <p> </p>
 */
final class ProcesschunksVolcombineMonitor extends ProcesschunksProcessMonitor {
  public static final String rcsid = "$Id$";

  private LogFile startLog = null;
  private LogFile finishLog = null;
  private final VolcombineProcessMonitor.Subprocess subprocess = new VolcombineProcessMonitor.Subprocess();
  private LogFile.ReaderId readerIdStart = null;
  private LogFile.ReaderId readerIdFinish = null;

  public ProcesschunksVolcombineMonitor(BaseManager manager, AxisID axisID,
      String rootName, Map<String,String> computerMap) {
    super(manager, axisID, rootName, computerMap, false);
  }

  boolean updateState() throws LogFile.LockException, FileNotFoundException, IOException {
    String line = null;
    if (super.updateState()) {
      return true;
    }
    if (isStarting()) {
      if (startLog == null) {
        startLog = LogFile.getInstance(manager.getPropertyUserDir(),
            DatasetFiles.VOLCOMBINE_START_LOG);
      }
      if (readerIdStart == null || readerIdStart.isEmpty()) {
        try {
          readerIdStart = startLog.openReader();
        }
        catch (LogFile.LockException e) {
          return false;
        }
      }
      if (readerIdStart != null && !readerIdStart.isEmpty()) {
        while ((line = startLog.readLine(readerIdStart)) != null) {
          if (VolcombineProcessMonitor.setSubprocess(line, subprocess)) {
            return true;
          }
        }
      }
    }
    else if (isFinishing()) {
      if (finishLog == null) {
        finishLog = LogFile.getInstance(manager.getPropertyUserDir(),
            DatasetFiles.VOLCOMBINE_FINISH_LOG);
      }
      if (readerIdFinish == null || readerIdFinish.isEmpty()) {
        try {
          readerIdFinish = finishLog.openReader();
        }
        catch (LogFile.LockException e) {
          return false;
        }
      }
      if (readerIdFinish != null && !readerIdFinish.isEmpty()) {
        while ((line = finishLog.readLine(readerIdFinish)) != null) {
          if (VolcombineProcessMonitor.setSubprocess(line, subprocess)) {
            return true;
          }
        }
      }
    }
    return false;
  }

  void updateProgressBar() {
    if (subprocess.isFilltomo()) {
      manager.getMainPanel().setProgressBarValue(0, "Filltomo", axisID);
    }
    else if (subprocess.isReassembling()) {
      manager.getMainPanel().setProgressBarValue(0, "Reassembling", axisID);
    }
    else if (subprocess.isDensmatch()) {
      manager.getMainPanel().setProgressBarValue(0, "Densmatch", axisID);
    }
    else {
      super.updateProgressBar();
    }
  }

  void closeProcessOutput() {
    super.closeProcessOutput();
    if (startLog != null && readerIdStart != null && !readerIdStart.isEmpty()) {
      startLog.closeRead(readerIdStart);
      startLog = null;
      readerIdStart = null;
    }
    if (finishLog != null && readerIdFinish != null && !readerIdFinish.isEmpty()) {
      finishLog.closeRead(readerIdFinish);
      finishLog = null;
      readerIdFinish = null;
    }
  }
}
