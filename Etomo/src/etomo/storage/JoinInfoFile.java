package etomo.storage;

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
  private final LogFile joinInfo;

  private boolean loaded = false;
  private ArrayList invertedArray = null;

  public JoinInfoFile(BaseManager manager) {
    joinInfo = LogFile.getInstance(manager.getPropertyUserDir(),
        DatasetFiles.getJoinInfoName(manager));
  }
  
  JoinInfoFile(LogFile logFile){
    joinInfo = logFile;
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
          + e.toString());
      return null;
    }
  }

  private boolean load() {
    reset();
    try {
      long readId = joinInfo.openReader();
      joinInfo.readLine(readId);
      String line = joinInfo.readLine(readId);
      if (line == null) {
        UIHarness.INSTANCE.openMessageDialog(ERROR_TITLE, ERROR_MESSAGE);
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
      joinInfo.closeReader(readId);
      readId = LogFile.NO_ID;
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog("Etomo Error", ERROR_MESSAGE + "\n"
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
 * <p> Revision 1.2  2006/10/13 22:28:56  sueh
 * <p> bug# 919 Using LogFile to read the .info file.  Popping up a message when
 * <p> unable to read.
 * <p>
 * <p> Revision 1.1  2006/10/10 05:16:25  sueh
 * <p> bug# 919 File to read the join info file.
 * <p> </p>
 */
