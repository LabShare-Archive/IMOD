package etomo.logic;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.ApplicationManager;
import etomo.BaseManager;
import etomo.process.SystemProgram;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.EtomoVersion;
import etomo.type.ImodVersion;
import etomo.ui.swing.UIHarness;
import etomo.util.EnvironmentVariable;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/

public final class VersionControl {
  public final String rcsid = "$$Id$$";

  private static final String DEPENDANT_IMOD_VERSION = "4.4.4";
  private static final String DEPENDANT_PEET_VERSION = "1.8.0";

  /**

   * Returns false if IMOD and PEET are not compatible.
   * @param axisID
   * @return
   */
  public static final boolean isCompatiblePeet(final AxisID axisID) {
    EtomoVersion peetVersion = EtomoVersion.getDefaultInstance(getPeetVersion());
    EtomoVersion imodVersion = null;
    List<String> imodInfo = getImodInfo(axisID);
    if (imodInfo != null && imodInfo.size() > 0) {
      imodVersion = EtomoVersion.getDefaultInstance(imodInfo.get(0));
    }
    boolean retVal = true;
    if (imodVersion.lt(DEPENDANT_IMOD_VERSION)) {
      retVal = peetVersion.lt(DEPENDANT_PEET_VERSION);
    }
    else if (imodVersion.ge(DEPENDANT_IMOD_VERSION)) {
      retVal = peetVersion.ge(DEPENDANT_PEET_VERSION);
    }
    if (!retVal) {
      UIHarness.INSTANCE.openMessageDialog((BaseManager) null,
          "The PEET version is incompatible with this IMOD version.  " + "IMOD "
              + DEPENDANT_IMOD_VERSION + " or later requires PEET "
              + DEPENDANT_PEET_VERSION + " or later.  IMOD versions prior to "
              + DEPENDANT_IMOD_VERSION + " require a PEET version prior to "
              + DEPENDANT_PEET_VERSION + ".  The currently installed versions are IMOD: "
              + imodVersion.toString() + ", and PEET: " + peetVersion.toString() + ".",
          "Wrong Version of PEET");

    }
    return retVal;
  }

  /**
   * Returns the version of PEET.
   * @return
   */
  public static final String getPeetVersion() {
    LogFile peetVersionFile = null;
    LogFile.ReaderId id = null;
    try {
      peetVersionFile = LogFile.getInstance(
          new File(EnvironmentVariable.INSTANCE.getValue(null, null,
              EnvironmentVariable.PARTICLE_DIR, AxisID.ONLY)), "PEETVersion.txt");
      id = peetVersionFile.openReader();
      String version = peetVersionFile.readLine(id);
      if (version != null && !version.matches("\\s*")) {
        return version;
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (FileNotFoundException e) {
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    if (peetVersionFile != null && id != null) {
      peetVersionFile.closeRead(id);
    }
    return null;
  }

  public static String getEtomoVersion() {
    // updated by a script
    return ImodVersion.CURRENT_VERSION + " " + "10/25/2013 21:08";
  }

  /**
   * Run 3dmod -h to get version and copyright information in the form of a List
   * containing { version, copyright line 1, copyright line 2 }.  If unable to find the
   * information, returns null.  If the copyright information is missing, the list will
   * contain one element.
   * @return
   */
  public static List<String> getImodInfo(AxisID axisID) {
    String[] command = new String[] { ApplicationManager.getIMODBinPath() + "imodinfo" };
    SystemProgram threeDmod_h = new SystemProgram(null, null, command, axisID);
    threeDmod_h.run();
    String[] stdout = threeDmod_h.getStdOutput();
    List<String> imodInfo = null;
    if (stdout != null && stdout.length >= 1) {
      imodInfo = new ArrayList<String>();
      // Get version info
      int idxVersion = stdout[0].indexOf("Version");
      if (idxVersion > 0) {
        String noPath = stdout[0].substring(idxVersion);
        String[] tokens = noPath.split(" ");
        if (tokens.length > 1) {
          imodInfo.add(tokens[1]);
        }
      }
      // Get copyright info
      if (stdout.length > 3) {
        imodInfo.add(stdout[1]);
        imodInfo.add(stdout[2]);
      }
    }
    return imodInfo;
  }
}
