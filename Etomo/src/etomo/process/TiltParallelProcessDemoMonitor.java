package etomo.process;

import java.io.IOException;
import java.util.Date;
import java.util.Random;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.ui.ParallelProgressDisplay;
import etomo.util.InvalidParameterException;

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
public class TiltParallelProcessDemoMonitor extends TiltProcessMonitor {
  public static final String rcsid = "$Id$";

  private long chunks = 0;
  private long chunksDone = 0;
  private long lastChunksDone = 0;
  private int currentLength = 0;
  private int lastCurrentLength = 0;
  private ParallelProgressDisplay progressDisplay;
  private static TiltParallelProcessDemoMonitor monitorA = null;
  private static TiltParallelProcessDemoMonitor monitorB = null;
  private Random random = new Random(new Date().getTime());

  public static TiltParallelProcessDemoMonitor getNewInstance(
      ApplicationManager appMgr, AxisID axisID,
      ParallelProgressDisplay progressDisplay) {
    if (axisID == AxisID.SECOND) {
      monitorB = new TiltParallelProcessDemoMonitor(appMgr, axisID,
          progressDisplay);
      return monitorB;
    }
    monitorA = new TiltParallelProcessDemoMonitor(appMgr, axisID,
        progressDisplay);
    return monitorA;
  }

  public static TiltParallelProcessDemoMonitor getExistingInstance(
      ApplicationManager appMgr, AxisID axisID,
      ParallelProgressDisplay progressDisplay) {
    if (axisID == AxisID.SECOND) {
      if (monitorB != null) {
        return monitorB;
      }
    }
    if (monitorA != null) {
      return monitorA;
    }
    return getNewInstance(appMgr, axisID, progressDisplay);
  }

  private TiltParallelProcessDemoMonitor(ApplicationManager appMgr,
      AxisID axisId, ParallelProgressDisplay progressDisplay) {
    super(appMgr, axisId);
    this.progressDisplay = progressDisplay;
    progressDisplay.signalStartProgress();
  }

  void calcFileSize() throws InvalidParameterException, IOException {
    super.calcFileSize();
    applicationManager.getMainPanel().setProgressBar("Calculating tomogram",
        nKBytes, axisID);
    if (chunks == 0) {
      //new demo process
      long chunkFactor = Math.round(nKBytes / 20000);
      if (chunkFactor < 2) {
        chunkFactor = 2;
      }
      int cpusSelected = progressDisplay.getCpusSelected();
      chunks = cpusSelected * chunkFactor;
    }
    else {
      //existing demo process (but a new tilt process)
      lastCurrentLength = currentLength;
      try {
        Thread.sleep(updatePeriod);
      }
      catch (InterruptedException exception) {
      }
    }
  }

  void updateProgressBar() {
    boolean fileWriting = true;
    double partiallyDoneFactor = 1;
    //pretending a new tilt process is a resumed demo process.  Calculate
    //a factor to reduce current length and chunks done.
    if (lastCurrentLength != 0) {
      partiallyDoneFactor = (double) (nKBytes - lastCurrentLength) / nKBytes;
    }
    while (fileWriting) {
      currentLength = 0;
      try {
        currentLength = (int) (watchedChannel.size() / 1024);
        if (lastCurrentLength != 0) {
          currentLength = (int) Math.round((double) lastCurrentLength
              + currentLength * partiallyDoneFactor);
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      double fractionDone = (double) currentLength / nKBytes;
      int percentage = (int) Math.round(fractionDone * 100);
      //  Catch any wierd values before they get displayed
      if (percentage < 0) {
        percentage = 0;
      }
      if (percentage > 99) {
        percentage = 99;
      }
      long curChunksDone = Math.round(fractionDone * chunks);
      if (curChunksDone > chunksDone) {
        if (.80 < random.nextDouble()) {
          progressDisplay.addRandomRestart();
        }
        for (int i = 0; i < curChunksDone - chunksDone; i++) {
          progressDisplay.signalRandomSuccess();
        }
        chunksDone = curChunksDone;
      }
      long elapsedTime = System.currentTimeMillis() - processStartTime;
      double remainingTime = elapsedTime / fractionDone - elapsedTime;
      String message = chunksDone + " of " + chunks;
      applicationManager.getMainPanel().setProgressBarValue(currentLength,
          message, axisID);

      try {
        Thread.sleep(updatePeriod);
      }
      catch (InterruptedException exception) {
        fileWriting = false;
        closeChannel();
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2005/07/21 21:41:53  sueh
 * <p> bug# removed "kill / pause" label from kill process button.  Pause button
 * <p> with be separate.
 * <p>
 * <p> Revision 1.1  2005/07/11 22:50:41  sueh
 * <p> bug# 619 Monitor which watches tilt.com and pretends that it is watching
 * <p> processchunks tilt.
 * <p> </p>
 */