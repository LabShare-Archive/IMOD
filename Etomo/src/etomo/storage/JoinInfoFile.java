package etomo.storage;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean2;
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
      BufferedReader reader = new BufferedReader(new FileReader(DatasetFiles
          .getJoinInfo(manager)));
      reader.readLine();
      String line = reader.readLine();
      if (line != null) {
        String[] array = line.trim().split(" +");
        for (int i = 0; i < array.length; i++) {
          EtomoBoolean2 inverted = new EtomoBoolean2();
          if (inverted.isValid()) {
            invertedArray.add(new EtomoBoolean2().set(array[i]));
          }
        }
      }
      reader.close();
    }
    catch (FileNotFoundException e) {
      //programmer error
      e.printStackTrace();
    }
    catch (IOException e) {
      //programmer or system error
      e.printStackTrace();
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
 * <p> $Log$ </p>
 */
