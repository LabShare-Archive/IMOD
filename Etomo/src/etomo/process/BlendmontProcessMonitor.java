package etomo.process;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
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
  public static  final String  rcsid =  "$Id$";
  
  public BlendmontProcessMonitor(ApplicationManager appMgr, AxisID id) {
    super(appMgr, id);
    logFileBasename = BlendmontParam.getCommandFileName(BlendmontParam.PREBLEND_MODE);
  }
  
  protected void initializeProgressBar() {
    if (nSections == Integer.MIN_VALUE) {
      applicationManager.getMainPanel().setProgressBar("blendmont", 1, axisID);
      applicationManager.getMainPanel().setProgressBarValue(0, "Starting...", axisID);
      return;
    }
    applicationManager.getMainPanel().setProgressBar("blendmont", nSections, axisID);
  }
  
  protected void getCurrentSection() throws NumberFormatException, IOException {
    String line;
    while ((line = logFileReader.readLine()) != null) {
      line = line.trim();
      if (line.startsWith("working on section #")) {
        String[] strings = line.split("\\s+");
        currentSection = Integer.parseInt(strings[4]) + 1;
      }
    }
    if (currentSection >= nSections) {
      waitingForExit++;
    }
  }
  
  protected void findNSections() throws InterruptedException,
      NumberFormatException, IOException {
    Montagesize montagesize = null;
    try {
    montagesize = Montagesize.getInstance(applicationManager
        .getPropertyUserDir(), applicationManager.getMetaData()
        .getDatasetName(), axisID);
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      throw new IOException(e.getMessage());
    }
    nSections = montagesize.getZ().getInteger();
  }
}
/**
* <p> $Log$ </p>
*/