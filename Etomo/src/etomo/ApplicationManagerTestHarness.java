package etomo;

import etomo.comscript.CombineComscriptState;
import etomo.ui.TomogramCombinationDialog;

/**
* <p>Description: Used for testing only.  It is used to run protected functions 
* in ApplicationManager.  It inherits ApplicationManager and can only be 
* constructed with an args[] parameter which contains the "--test" option.
* This option prevents ApplicationManager from displaying a window.</p>
*
* <p>Copyright: Copyright 2004 </p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $Author$
*
* @version $Revision$
*
* <p> $Log$ </p>
*/
public class ApplicationManagerTestHarness extends ApplicationManager {
  public static final String rcsid = "$$Id$$";

  public static ApplicationManagerTestHarness getApplicationManagerTestHarness(
      String[] args) {
    for (int i = 0; i < args.length; i++) {
      if (args[i].equals("--test")) {
        return new ApplicationManagerTestHarness(args);
      }
    }
    return null;
  }
  
  /**
   * @param args
   */
  private ApplicationManagerTestHarness(String[] args) {
    super(args);
  }
  
  public CombineComscriptState runUpdateCombineComscriptState(
      int startCommand) {
    return updateCombineComscriptState(startCommand);
  }
  
  public TomogramCombinationDialog getTomogramCombinationDialog() {
    return tomogramCombinationDialog;
  }
  
}
