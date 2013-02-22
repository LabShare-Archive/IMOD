package etomo.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.5  2005/08/27 22:43:51  sueh
* <p> bug# 532 In Utilities.timestamp() change the int status to String status,
* <p> since it doesn't have to be compared.
* <p>
* <p> Revision 1.4  2005/07/29 00:55:18  sueh
* <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
* <p> because the current manager changes when the user changes the tab.
* <p> Passing the manager where its needed.
* <p>
* <p> Revision 1.3  2005/06/17 20:04:00  sueh
* <p> bug# 685 Added timestamps to read().
* <p>
* <p> Revision 1.2  2004/11/20 00:11:39  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.1  2004/10/30 02:40:54  sueh
* <p> bug# 520 Reads the .info file created while making samples in the join
* <p> dialog.
* <p> </p>
*/
public class JoinInfoFile {
  public static final String rcsid = "$Id$";

  private String rootName;
  private ArrayList fileNameArray = null;
  private final String propertyUserDir;

  public JoinInfoFile(String propertyUserDir, String rootName) {
    this.propertyUserDir = propertyUserDir;
    this.rootName = rootName;
  }

  public boolean read(int numberFileNames) {
    String joinInfoFileName = rootName + ".info";
    Utilities.timestamp("read", joinInfoFileName, Utilities.STARTED_STATUS);
    fileNameArray = new ArrayList(numberFileNames);
    File joinInfoFile = new File(propertyUserDir, joinInfoFileName);
    if (!joinInfoFile.exists()) {
      Utilities.timestamp("read", joinInfoFileName, Utilities.FAILED_STATUS);
      return false;
    }
    BufferedReader reader = null;
    try {
      reader = new BufferedReader(new FileReader(joinInfoFile));
    }
    catch (IOException e) {
      e.printStackTrace();
      Utilities.timestamp("read", joinInfoFileName, Utilities.FAILED_STATUS);
      return false;
    }
    String line;
    int linesInFile = 0;
    ArrayList lineArray = new ArrayList(numberFileNames + 3);
    try {
      while ((line = reader.readLine()) != null) {
        lineArray.add(line);
      }
      reader.close();
    }
    catch (IOException e) {
      e.printStackTrace();
      Utilities.timestamp("read", joinInfoFileName, Utilities.FAILED_STATUS);
      try {
        reader.close();
      }
      catch (IOException e1) {
        e1.printStackTrace();
      }
      return false;
    }
    int lineArraySize = lineArray.size();
    int offset = lineArraySize - numberFileNames;
    for (int i = offset; i < lineArraySize; i++) {
      Object object = lineArray.get(i);
      if (object != null) {
        fileNameArray.add(object);
      }
    }
    Utilities.timestamp("read", joinInfoFileName, Utilities.FINISHED_STATUS);
    return true;
  }

  public String getFileName(int index) {
    if (fileNameArray == null) {
      return null;
    }
    return (String) fileNameArray.get(index);
  }
}
