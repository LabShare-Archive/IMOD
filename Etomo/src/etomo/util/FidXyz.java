package etomo.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

/**
 * <p>Description: An interface to the fid.xyz file.</p>
 *
* <p>Copyright: Copyright © 2002, 2003, 2004</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
 *
 * <p> $$Log$$ </p>
 */

public class FidXyz {
  public static final String rcsid = "$$Id$$";

  private String filename;
  private boolean exists = false;
  private double pixelSize = Double.NaN;

  public FidXyz(String name) {
    filename = new String(name);
  }

  public void read() throws IOException {
    if (filename == null || filename.length() == 0) {
      throw new IOException("No filename specified");
    }
    
    File fidXyzFile = new File(System.getProperty("user.dir"), filename);
    if (!fidXyzFile.exists()) {
      return;
    }
    exists = true;
    pixelSize = 1;
    BufferedReader fileReader = new BufferedReader(new FileReader(fidXyzFile));
    String line = fileReader.readLine();
    fileReader.close();

    // The first line contains the pixel size
    if (line.toLowerCase().indexOf("pixel size:") != -1) {
      String[] tokens = line.split("\\s+");
      if (tokens.length < 10) {
        return;
      }
      pixelSize = Double.parseDouble(tokens[9]);
    }
  }
  
  /**
   * 
   * @return
   */
  public boolean exists() {
    return exists;
  }
  
  public double getPixelSize() {
    return pixelSize;
  }
}
