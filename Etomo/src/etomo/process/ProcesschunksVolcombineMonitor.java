package etomo.process;

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
 * <p> Revision 1.1  2006/12/02 04:39:05  sueh
 * <p> bug# 944 A Processchunks monitor which does specialized process for
 * <p> volcombine.  Watches volcombine-start.log and volcombine-finish.log
 * <p> </p>
 */
public final class ProcesschunksVolcombineMonitor extends
    ProcesschunksProcessMonitor {
  public static final String rcsid = "$Id$";

  private LogFile startLog = null;
  private LogFile finishLog = null;
  private final VolcombineProcessMonitor.Subprocess subprocess = new VolcombineProcessMonitor.Subprocess();
  private long readIdStart = LogFile.NO_ID;
  private long readIdFinish = LogFile.NO_ID;

  public ProcesschunksVolcombineMonitor(BaseManager manager, AxisID axisID,
      ParallelProgressDisplay parallelProgressDisplay, String rootName,
      String computerList) {
    super(manager, axisID, parallelProgressDisplay, rootName, computerList,null);
  }

  protected boolean updateState() throws LogFile.ReadException,
      LogFile.FileException {
    String line = null;
    if (super.updateState()) {
      return true;
    }
    if (isStarting()) {
      if (startLog == null) {
        startLog = LogFile.getInstance(manager.getPropertyUserDir(),
            DatasetFiles.VOLCOMBINE_START_LOG);
      }
      if (readIdStart == LogFile.NO_ID) {
        try {
          readIdStart = startLog.openReader();
        }
        catch (LogFile.ReadException e) {
          return false;
        }
      }
      if (readIdStart != LogFile.NO_ID) {
        while ((line = startLog.readLine(readIdStart)) != null) {
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
      if (readIdFinish == LogFile.NO_ID) {
        try {
          readIdFinish = finishLog.openReader();
        }
        catch (LogFile.ReadException e) {
          return false;
        }
      }
      if (readIdFinish != LogFile.NO_ID) {
        while ((line = finishLog.readLine(readIdFinish)) != null) {
          if (VolcombineProcessMonitor.setSubprocess(line, subprocess)) {
            return true;
          }
        }
      }
    }
    return false;
  }

  protected void updateProgressBar() {
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

  protected void closeProcessOutput() {
    super.closeProcessOutput();
    if (startLog != null && readIdStart != LogFile.NO_ID) {
      startLog.closeReader(readIdStart);
      startLog = null;
      readIdStart = LogFile.NO_ID;
    }
    if (finishLog != null && readIdFinish != LogFile.NO_ID) {
      finishLog.closeReader(readIdFinish);
      finishLog = null;
      readIdFinish = LogFile.NO_ID;
    }
  }
}
