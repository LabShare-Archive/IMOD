package etomo.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.ui.swing.UIHarness;

/**
 * <p>Description: Represents a file with a list of file names.  The last file
 * name may be duplicated.</p>
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
 * <p> Revision 1.3  2010/11/13 16:05:03  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.2  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.1  2010/01/13 21:53:36  sueh
 * <p> bug# 1298 Reads averagedFilenames.txt.
 * <p> </p>
 */
public final class AveragedFileNames {
  public static final String rcsid = "$Id$";

  /**
   * Build a list from the lines in averagedFilenames.txt.  Duplicates are only
   * at the end of the file, so stop building the list when a duplicate is
   * found.
   * @param manager
   * @param axisID
   * @param errorMessage
   * @param errorTitle
   * @return the list of lines
   */
  public List getList(BaseManager manager, AxisID axisID, String errorMessage,
      String errorTitle) {
    LogFile file = null;
    LogFile.ReaderId id = null;
    try {
      file = LogFile.getInstance(new File(manager.getPropertyUserDir(),
          "averagedFilenames.txt"));
      id = file.openReader();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return null;
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "File not found. " + errorMessage,
          errorTitle);
    }
    List list = new ArrayList();
    String line;
    String prevLine = null;
    try {
      while ((line = file.readLine(id)) != null) {
        if (line.equals(prevLine)) {
          //Duplicate are all of the last file name so that rest of the file
          //should be nothing but duplicates.
          break;
        }
        list.add(line);
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    return list;
  }
}
