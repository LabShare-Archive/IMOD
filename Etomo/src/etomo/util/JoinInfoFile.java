package etomo.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

import etomo.EtomoDirector;

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
* <p> Revision 1.1.2.1  2004/10/30 02:40:54  sueh
* <p> bug# 520 Reads the .info file created while making samples in the join
* <p> dialog.
* <p> </p>
*/
public class JoinInfoFile {
  public static  final String  rcsid =  "$Id$";
  
  private String rootName;
  private ArrayList fileNameArray = null;
  
  public JoinInfoFile(String rootName) {
    this.rootName = rootName;
  }
  
  public boolean read(int numberFileNames) {
    fileNameArray = new ArrayList(numberFileNames);
    File joinInfoFile = new File(EtomoDirector.getInstance().getCurrentPropertyUserDir(), rootName + ".info");
    if (!joinInfoFile.exists()) {
      return false;
    }
    BufferedReader reader;
    try {
      reader = new BufferedReader(new FileReader(joinInfoFile));
    }
    catch (IOException e) {
      e.printStackTrace();
      return false;
    }
    String line;
    int linesInFile = 0;
    ArrayList lineArray = new ArrayList(numberFileNames + 3);
    try {
      while ((line = reader.readLine()) != null) {
        lineArray.add(line);
      }
    }
    catch (IOException e) {
      e.printStackTrace();
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
    return true;
  }
  
  public String getFileName(int index) {
    if (fileNameArray == null) {
      return null;
    }
    return (String) fileNameArray.get(index);
  }
}
