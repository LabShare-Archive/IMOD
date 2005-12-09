package etomo;

import etomo.comscript.CombineComscriptState;
import etomo.type.AxisID;
import etomo.ui.TomogramCombinationDialog;

/**
* <p>Description: Used for testing only.  It is used to run protected functions 
* in ApplicationManager.  It inherits ApplicationManager and can only be 
* constructed with an args[] parameter which contains the "--test" option.
* This option prevents ApplicationManager from displaying a window.</p>
*
* <p>Copyright: Copyright 2004 - 2005 </p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
*
* @author $Author$
*
* @version $Revision$
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
    super("", AxisID.ONLY);
  }

  public CombineComscriptState runUpdateCombineComscriptState(int startCommand) {
    return updateCombineComscriptState(startCommand);
  }

  public TomogramCombinationDialog getTomogramCombinationDialog() {
    return tomogramCombinationDialog;
  }
}
/**
* <p> $Log$
* <p> Revision 1.6  2005/08/11 23:31:03  sueh
* <p> Reformatting
* <p>
* <p> Revision 1.5  2005/06/01 21:22:22  sueh
* <p> bug# 667 Removed controller classes.
* <p>
* <p> Revision 1.4  2005/04/25 20:29:24  sueh
* <p> bug# 615 Passing the axis where the command originated to the message
* <p> functions so that the message will be popped up in the correct window.
* <p> This requires adding AxisID to many objects.
* <p>
* <p> Revision 1.3  2005/01/21 22:07:09  sueh
* <p> bug# 509 bug# 591  Moved the management of MetaData to the Controller
* <p> class.
* <p>
* <p> Revision 1.2  2004/11/19 22:33:38  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.1  2004/09/03 20:34:48  sueh
* <p> bug# 520 adapting to changes in AppMgr constructor
* <p>
* <p> Revision 1.1  2004/08/20 21:52:48  sueh
* <p> bug# 508 tests loading, updateing , and saving combine.com
* <p> </p>
*/
