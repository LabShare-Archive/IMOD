package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessName;

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
public final class TiltxcorrProcessWatcher extends LogFileProcessMonitor {
  public static final String rcsid = "$Id$";

  private String lastLineRead = null;
  private boolean blendmontRan = false;

  /**
   * Construct an xcorr process watcher
   * @param appMgr
   * @param id
   */
  public TiltxcorrProcessWatcher(final BaseManager manager,
      final AxisID id) {
    super(manager, id, ProcessName.XCORR);
    logFileBasename = "xcorr";
  }

  /**
   * Construct an xcorr process watcher
   * @param appMgr
   * @param id
   * @param blendmontRan - True if blendmont output is in the log file prior to
   *        tiltxcorr output.
   */
  public TiltxcorrProcessWatcher(final ApplicationManager appMgr,
      final AxisID id, boolean blendmontRan) {
    super(appMgr, id, ProcessName.XCORR);
    logFileBasename = "xcorr";
    this.blendmontRan = blendmontRan;
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#intializeProgressBar()
   */
  void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      manager.getMainPanel().setProgressBar("Cross-correlating stack", 1,
          axisID, processName);
      manager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    manager.getMainPanel().setProgressBar("Cross-correlating stack", nSections,
        axisID, processName);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  void getCurrentSection() throws NumberFormatException, LogFile.LockException,
      IOException {
    String line;
    while ((line = readLogFileLine()) != null) {
      if (line.startsWith("View")) {
        currentSection++;
      }
      lastLineRead = line;
    }

    if (lastLineRead != null
        && lastLineRead.trim().startsWith("PROGRAM EXECUTED TO END.")) {
      waitingForExit++;
    }
  }

  /**
   * Search the log file for the header section and extract the number of
   * sections
   */
  void findNSections() throws InterruptedException, NumberFormatException,
      LogFile.LockException, IOException {
    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;

    nSections = -1;
    while (!foundNSections) {
      Thread.sleep(UPDATE_PERIOD);
      String line;
      while ((line = readLogFileLine()) != null) {
        line = line.trim();
        if (line.startsWith(nSectionsHeader)) {
          String[] fields = line.split("\\s+");
          if (fields.length > nSectionsIndex) {
            //Take the second header output if there is blendmont output in the
            //log file
            if (blendmontRan) {
              blendmontRan = false;
            }
            else {
              nSections = Integer.parseInt(fields[nSectionsIndex]);
              foundNSections = true;
            }
            break;
          }
          else {
            throw new NumberFormatException("Incomplete size line in header");
          }
        }
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2009/09/25 22:22:27  sueh
 * <p> bug# 1272 In findNSections handle line == null.
 * <p>
 * <p> Revision 1.7  2009/09/24 19:07:28  sueh
 * <p> bug# 1258 In findNSections trimmed line before splitting it.
 * <p>
 * <p> Revision 1.6  2009/06/10 17:25:08  sueh
 * <p> bug# 1202 Parse the mrc header based on
 * <p> EtomoDirector.ImodBriefHeader.
 * <p>
 * <p> Revision 1.5  2009/06/05 01:59:55  sueh
 * <p> bug# 1219 Kept up with changes in parent class.
 * <p>
 * <p> Revision 1.4  2009/02/04 23:26:53  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.3  2006/10/24 21:41:01  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 1.2  2006/10/10 05:14:39  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 1.1  2005/03/09 18:08:59  sueh
 * <p> bug# 533 This class used to be the XcorrProcessWatcher.  It watches
 * <p> tiltxcorr in the xcorr script.
 * <p> </p>
 */
