package etomo.logic;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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

public final class LatestBuild {
  public static final String rcsid = "$$Id$$";

  public static String get() {
    // Check in this file each time etomo is changed
    if (rcsid.indexOf("LatestBuild") == -1) {
      return "";
    }
    String[] array = rcsid.split("\\s+");
    if (array == null || array.length < 5) {
      return "";
    }
    int i = rcsid.indexOf(".java");
    return "Build: " + array[3] + " "+ array[4];
  }
}
