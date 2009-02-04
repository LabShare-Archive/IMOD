package etomo.process;

import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.ui.ParallelProgressDisplay;
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
      ParallelProgressDisplay parallelProgressDisplay, String rootName,
      String computerList) {
    super(manager, axisID, parallelProgressDisplay, rootName, computerList);
  }

  boolean updateState() throws LogFile.LockException,FileNotFoundException,IOException {
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
      startLog.closeReader(readerIdStart);
      startLog = null;
      readerIdStart = null;
    }
    if (finishLog != null && readerIdFinish != null
        && !readerIdFinish.isEmpty()) {
      finishLog.closeReader(readerIdFinish);
      finishLog = null;
      readerIdFinish = null;
    }
  }
}
