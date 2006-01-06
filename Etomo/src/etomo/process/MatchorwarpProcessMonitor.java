package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.type.AxisID;

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
* 
* <p> $Log$
* <p> Revision 1.1  2005/01/26 23:41:40  sueh
* <p> bug# 83 Matchorwarp process monitor.
* <p> </p>
*/
public class MatchorwarpProcessMonitor extends LogFileProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  String lastLineRead = null;
  /**
   * Construct a matchvol1 process watcher
   * @param appMgr
   * @param id
   */
  public MatchorwarpProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
    logFileBasename = "matchorwarp";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#intializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar("Combine: matchorwarp", 1, axisID);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar(
      "Combine: matchorwarp",
      nSections,
      axisID);
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#getCurrentSection()
   */
  protected void getCurrentSection()
    throws NumberFormatException, IOException {
    String line;
    while ((line = logFileReader.readLine()) != null) {
      line = line.trim();
      if (line.startsWith("Finished")) {
        String[] strings = line.split("\\s+");
        currentSection = Integer.parseInt(strings[1]);
      }
      lastLineRead = line;
    }
    if (currentSection >= nSections) {
      waitingForExit++;
    }
  }

  /**
   * Search matchvol1.log.out file for the number of positions
   */
  protected void findNSections()
    throws InterruptedException, NumberFormatException, IOException {
    //  Search for the number of sections, we should see a header ouput first
    boolean foundNSections = false;
    nSections = -1;
    Thread.sleep(updatePeriod);
    while (!foundNSections) {
      String line = logFileReader.readLine();
      if (line == null) {
        Thread.sleep(updatePeriod);
      }
      if (line != null && line.trim().startsWith("Finished")) {
        line = line.trim();
        String[] strings = line.split("\\s+");
        nSections = Integer.parseInt(strings[3]);
        foundNSections = true;
      }
    }
    Thread.sleep(updatePeriod);
  }



}
