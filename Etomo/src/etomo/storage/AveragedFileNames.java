package etomo.storage;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.ui.UIHarness;

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
 * <p> $Log$ </p>
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
          "averagedFilenames.txt"), manager.getManagerKey());
      id = file.openReader();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return null;
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog("File not found. " + errorMessage,
          errorTitle, manager.getManagerKey());
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
