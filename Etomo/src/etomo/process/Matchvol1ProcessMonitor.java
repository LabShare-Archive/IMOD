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
* <p> $Log$ </p>
*/
public class Matchvol1ProcessMonitor extends LogFileProcessMonitor {
  public static  final String  rcsid =  "$Id$";
  
  String lastLineRead = null;
  /**
   * Construct a matchvol1 process watcher
   * @param appMgr
   * @param id
   */
  public Matchvol1ProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
    logFileBasename = "matchvol1";
  }

  /* (non-Javadoc)
   * @see etomo.process.LogFileProcessMonitor#intializeProgressBar()
   */
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar("Combine: matchvol1", 1, axisID);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar(
      "Combine: matchvol1",
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
    while (!foundNSections) {
      Thread.sleep(updatePeriod);
      String line = logFileReader.readLine();
      if (line != null && line.trim().startsWith("Finished")) {
        line = line.trim();
        String[] strings = line.split("\\s+");
        nSections = Integer.parseInt(strings[3]);
        foundNSections = true;
      }
    }
  }

}
