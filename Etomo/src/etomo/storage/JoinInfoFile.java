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

  private final BaseManager manager;

  private boolean loaded = false;
  private ArrayList invertedArray = null;

  public JoinInfoFile(BaseManager manager) {
    this.manager = manager;
  }

  public ConstEtomoNumber getInverted(int index) {
    if (!loaded) {
      load();
    }
    try {
      return (EtomoBoolean2) invertedArray.get(index);
    }
    catch (IndexOutOfBoundsException e) {
      //programmer error
      e.printStackTrace();
      return new EtomoBoolean2();
    }
  }

  private void load() {
    reset();
    try {
      LogFile joinInfo = LogFile.getInstance(manager.getPropertyUserDir(),
          DatasetFiles.getJoinInfoName(manager));
      long readId = joinInfo.openReader();
      joinInfo.readLine(readId);
      String line = joinInfo.readLine(readId);
      if (line == null) {
        UIHarness.INSTANCE.openMessageDialog("Join Info File Error","WARNING:  Unable to find out if sections with be inverted.");
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
      UIHarness.INSTANCE.openMessageDialog("Join Info File Error","WARNING:  Unable to find out if sections with be inverted.\n"+e.getMessage());
    }
    loaded = true;
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
 * <p> Revision 1.1  2006/10/10 05:16:25  sueh
 * <p> bug# 919 File to read the join info file.
 * <p> </p>
 */
