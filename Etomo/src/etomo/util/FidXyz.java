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
 * <p> $$Log$
 * <p> $Revision 1.6  2005/06/22 23:37:15  sueh
 * <p> $bug# 694 Changed FidXyz to work with old and new fid.xyz formats.
 * <p> $Added setPixelSize().
 * <p> $
 * <p> $Revision 1.5  2005/06/17 20:03:27  sueh
 * <p> $bug# 685 Added timestamps to read().
 * <p> $
 * <p> $Revision 1.4  2004/11/20 00:11:05  sueh
 * <p> $bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p> $
 * <p> $Revision 1.3.2.1  2004/10/11 02:28:40  sueh
 * <p> $bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> $property.  This property would need a different value for each manager.
 * <p> $This variable can be retrieved from the manager if the object knows its
 * <p> $manager.  Otherwise it can retrieve it from the current manager using the
 * <p> $EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> $gets the value from the "user.dir" property.
 * <p> $
 * <p> $Revision 1.3  2004/08/31 21:53:16  sueh
 * <p> $bug# 545 Read(): Handle the case where fix.xyz exists but is empty by
 * <p> $setting a member variable.
 * <p> $
 * <p> $Revision 1.2  2004/07/02 00:41:45  sueh
 * <p> $bug# 487 adding a function that checks whether pixel size was
 * <p> $set successfully
 * <p> $
 * <p> $Revision 1.1  2004/06/29 23:52:41  sueh
 * <p> $bug# 487 extracting pixel size from the fid.xyz file
 * <p> $$ </p>
 */

public class FidXyz {
  public static final String rcsid = "$$Id$$";

  private static final String OLD_PIXEL_SIZE_LABEL = "pixel size:";
  private static final String NEW_PIXEL_SIZE_LABEL = "pix:";
  private static final int OLD_PIXEL_SIZE_INDEX = 9;
  private static final int NEW_PIXEL_SIZE_INDEX = 7;
  
  private String filename;
  private boolean exists = false;
  private boolean empty = false;
  private double pixelSize = Double.NaN;
  private final String propertyUserDir;

  public FidXyz(String propertyUserDir, String name) {
    this.propertyUserDir = propertyUserDir;
    filename = new String(name);
  }

  public void read() throws IOException {
    if (filename == null || filename.length() == 0) {
      throw new IOException("No filename specified");
    }
    Utilities.timestamp("read", filename, 0);
    
    File fidXyzFile = new File(propertyUserDir, filename);
    if (!fidXyzFile.exists() || fidXyzFile.isDirectory()) {
      Utilities.timestamp("read", filename, -1);
      return;
    }
    exists = true;
    if (fidXyzFile.length() == 0) {
      empty = true;
      Utilities.timestamp("read", filename, -1);
      return;
    }

    BufferedReader fileReader = new BufferedReader(new FileReader(fidXyzFile));
    String line = fileReader.readLine();
    fileReader.close();

    // The first line contains the pixel size
    if (!setPixelSize(line, NEW_PIXEL_SIZE_LABEL, NEW_PIXEL_SIZE_INDEX)) {
      if (!setPixelSize(line, OLD_PIXEL_SIZE_LABEL, OLD_PIXEL_SIZE_INDEX)) {
        Utilities.timestamp("read", filename, -1);
      }
    }
    Utilities.timestamp("read", filename, 1);
  }
  
  /**
   * Handle format change in fid.xyz
   * @param line
   * @param pixelSizeLabel
   * @param pixelSizeIndex
   * @return
   */
  private boolean setPixelSize(String line, String pixelSizeLabel,
      int pixelSizeIndex) {
    if (line == null) {
      return false;
    }
    line = line.toLowerCase();
    if (pixelSizeLabel.equals(NEW_PIXEL_SIZE_LABEL)) {
      line = line.trim();
    }
    if (line.indexOf(pixelSizeLabel) != -1) {
      String[] tokens = line.split("\\s+");
      if (tokens.length < pixelSizeIndex + 1
          || !tokens[pixelSizeIndex - 1].equals(pixelSizeLabel)) {
        Utilities.timestamp("read", filename, -1);
        throw new IllegalStateException("bad fid.xyz format: " + ",\ntokens["
            + pixelSizeIndex + "]=" + tokens[pixelSizeIndex]
            + ",pixelSizeLabel=" + pixelSizeLabel + ",line=" + line);
      }
      pixelSize = Double.parseDouble(tokens[pixelSizeIndex]);
      return true;
    }
    return false;
  }

  /**
   * 
   * @return
   */
  public boolean exists() {
    return exists;
  }
  
  /**
   * 
   * @return true if zero length file
   */
  public boolean isEmpty() {
    return empty;
  }
  
  public boolean isPixelSizeSet() {
    return !Double.isNaN(pixelSize);
  }

  public double getPixelSize() {
    return pixelSize;
  }
}
