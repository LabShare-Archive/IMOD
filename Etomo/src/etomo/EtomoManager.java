package etomo;

/**
 * <p> Description: Manages ApplicationManager. </p>
 * 
 * <p>Copyright: Copyright (c) 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */

public class EtomoManager {
  public static final String rcsid = "$$Id$$";
  private static EtomoManager theEtomoManager = null;
  String[] args = null;

  public static void main(String[] args) {
    createInstance(args);
  }

  private synchronized static EtomoManager createInstance(String[] args) {
    if (theEtomoManager == null) {
      theEtomoManager = new EtomoManager(args);
    }
    return theEtomoManager;
  }
  
  public static EtomoManager getInstance() {
    if (theEtomoManager == null) {
      throw new IllegalStateException();
    }
    return theEtomoManager;
  }
  
  private EtomoManager(String[] args) {
    theEtomoManager = this;
  }
  
}
