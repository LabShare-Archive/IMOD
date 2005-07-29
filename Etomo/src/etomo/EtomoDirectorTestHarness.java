package etomo;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class EtomoDirectorTestHarness {
  public static  final String  rcsid =  "$Id$";
  
  public final static BaseManager getCurrentManager() {
    EtomoDirector director = EtomoDirector.getInstance();
    if (!director.isTest()) {
      throw new IllegalStateException("Using test harness outside of test mode");
    }
    return director.getCurrentManager();
  }
  
  public final static String setCurrentPropertyUserDir(String propertyUserDir) {
    EtomoDirector director = EtomoDirector.getInstance();
    if (!director.isTest()) {
      throw new IllegalStateException("Using test harness outside of test mode");
    }
    return director.setCurrentPropertyUserDir(propertyUserDir);
  }
}
/**
* <p> $Log$ </p>
*/