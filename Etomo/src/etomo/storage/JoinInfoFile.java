package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean2;
import etomo.ui.swing.UIHarness;
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
 */
public class JoinInfoFile {
  public static final String rcsid = "$Id$";

  private static final String ERROR_TITLE = "Etomo Error";
  private static final String ERROR_MESSAGE = "WARNING:  Unable to find out if sections will be inverted.";

  //Set by constructor or getInstance.
  private LogFile joinInfo;

  private boolean loaded = false;
  private ArrayList invertedArray = null;

  private JoinInfoFile() {
  }

  private JoinInfoFile(LogFile logFile) {
    joinInfo = logFile;
  }

  public static JoinInfoFile getInstance(BaseManager manager)
      throws LogFile.LockException {
    JoinInfoFile instance = new JoinInfoFile();
    instance.joinInfo = LogFile.getInstance(manager.getPropertyUserDir(), DatasetFiles
        .getJoinInfoName(manager));
    return instance;
  }

  static JoinInfoFile getTestInstance(LogFile logFile) {
    return new JoinInfoFile(logFile);
  }

  public ConstEtomoNumber getInverted(BaseManager manager, int index) {
    if (!loaded) {
      if (!load(manager)) {
        return null;
      }
    }
    try {
      return (EtomoBoolean2) invertedArray.get(index);
    }
    catch (IndexOutOfBoundsException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, ERROR_MESSAGE + "\n" + e.toString(),
          ERROR_TITLE);
      return null;
    }
  }

  private boolean load(BaseManager manager) {
    reset();
    try {
      LogFile.ReaderId readerId = joinInfo.openReader();
      joinInfo.readLine(readerId);
      String line = joinInfo.readLine(readerId);
      if (line == null) {
        UIHarness.INSTANCE.openMessageDialog(manager, ERROR_MESSAGE, ERROR_TITLE);
        return false;
      }
      else {
        String[] array = line.trim().split(" +");
        for (int i = 0; i < array.length; i++) {
          EtomoBoolean2 inverted = new EtomoBoolean2();
          if (inverted.isValid()) {
            invertedArray.add(new EtomoBoolean2().set(array[i]));
          }
        }
      }
      joinInfo.closeRead(readerId);
      readerId = null;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Etomo Error", ERROR_MESSAGE + "\n"
          + e.toString());
      return false;
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Etomo Error", ERROR_MESSAGE + "\n"
          + e.toString());
      return false;
    }
    catch (IOException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, "Etomo Error", ERROR_MESSAGE + "\n"
          + e.toString());
      return false;
    }
    loaded = true;
    return true;
  }

  private void reset() {
    if (invertedArray == null) {
      invertedArray = new ArrayList();
    }
    else {
      invertedArray.clear();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.9  2010/11/13 16:05:03  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.8  2010/02/26 20:38:11  sueh
 * <p> Changing the complex popup titles are making it hard to complete the
 * <p> uitests.
 * <p>
 * <p> Revision 1.7  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.6  2009/03/17 00:44:33  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.5  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.4  2008/01/31 20:21:56  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.3  2006/10/16 22:45:03  sueh
 * <p> bug# 919  GetInverted():  return null on failure.
 * <p>
 * <p> Revision 1.2  2006/10/13 22:28:56  sueh
 * <p> bug# 919 Using LogFile to read the .info file.  Popping up a message when
 * <p> unable to read.
 * <p>
 * <p> Revision 1.1  2006/10/10 05:16:25  sueh
 * <p> bug# 919 File to read the join info file.
 * <p> </p>
 */
