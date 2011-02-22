package etomo.util;

import etomo.BaseManager;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.FileType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
 * <p> Revision 1.1  2010/03/03 05:11:59  sueh
 * <p> bug# 1311 Class to run imodinfo.
 * <p> </p>
 */
public final class Imodinfo {
  public static final String rcsid = "$Id$";

  private final FileType fileType;

  private boolean patchTracking = false;

  public Imodinfo(final FileType fileType) {
    this.fileType = fileType;
  }

  public boolean isPatchTracking(final BaseManager manager, final AxisID axisID) {
    run(manager, axisID);
    return patchTracking;
  }

  private void run(final BaseManager manager, final AxisID axisID) {
    SystemProgram systemProgram = new SystemProgram(manager,
        manager.getPropertyUserDir(), new String[] { "imodinfo", "-h",
            fileType.getFileName(manager, axisID) }, axisID);
    systemProgram.run();
    String[] stdout = systemProgram.getStdOutput();
    if (stdout != null) {
      for (int i = 0; i < stdout.length; i++) {
        if (stdout[i].trim().startsWith("# NAME")) {
          patchTracking = stdout[i].indexOf("Patch Tracking Model") != -1;
        }
      }
    }
  }
}
