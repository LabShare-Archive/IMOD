package etomo.storage;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean2;
import etomo.ui.UIHarness;
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

  private final BaseManager manager;

  //Set by constructor or getInstance.
  private LogFile joinInfo;

  private boolean loaded = false;
  private ArrayList invertedArray = null;

  private JoinInfoFile(BaseManager manager) {
    this.manager = manager;
  }

  private JoinInfoFile(BaseManager manager, LogFile logFile) {
    this.manager = manager;
    joinInfo = logFile;
  }

  public static JoinInfoFile getInstance(BaseManager manager)
      throws LogFile.LockException {
    JoinInfoFile instance = new JoinInfoFile(manager);
    instance.joinInfo = LogFile.getInstance(manager.getPropertyUserDir(),
        DatasetFiles.getJoinInfoName(manager), manager.getManagerKey());
    return instance;
  }

  static JoinInfoFile getTestInstance(BaseManager manager, LogFile logFile) {
    return new JoinInfoFile(manager, logFile);
  }

  public ConstEtomoNumber getInverted(int index) {
    if (!loaded) {
      if (!load()) {
        return null;
      }
    }
    try {
      return (EtomoBoolean2) invertedArray.get(index);
    }
    catch (IndexOutOfBoundsException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(ERROR_TITLE, ERROR_MESSAGE + "\n"
          + e.toString(), manager.getManagerKey());
      return null;
    }
  }

  private boolean load() {
    reset();
    try {
      LogFile.ReaderId readerId = joinInfo.openReader();
      joinInfo.readLine(readerId);
      String line = joinInfo.readLine(readerId);
      if (line == null) {
        UIHarness.INSTANCE.openMessageDialog(ERROR_TITLE, ERROR_MESSAGE,
            manager.getManagerKey());
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
      joinInfo.closeReader(readerId);
      readerId = null;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog("Etomo Error", ERROR_MESSAGE + "\n"
          + e.toString(), manager.getManagerKey());
      return false;
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog("Etomo Error", ERROR_MESSAGE + "\n"
          + e.toString(), manager.getManagerKey());
      return false;
    }
    catch (IOException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog("Etomo Error", ERROR_MESSAGE + "\n"
          + e.toString(), manager.getManagerKey());
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
