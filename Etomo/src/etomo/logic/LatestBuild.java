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

  public static final String TIME_STAMP = "Mon Feb 20 15:45:40 MST 2012";

  public static String get() {
    // Check in this file each time etomo is changed
    String[] array = rcsid.split("\\s+");
    if (array == null || array.length < 5) {
      return "";
    }
    return array[3] + " " + array[4] + " GMT";
  }
}
