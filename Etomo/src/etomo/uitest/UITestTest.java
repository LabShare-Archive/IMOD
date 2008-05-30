package etomo.uitest;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */

public class UITestTest {
  public static final String rcsid = "$Id$";

  private static final String ENABLED_STRING = "enabled";
  private static final String EXISTS_STRING = "exists";

  public static final UITestTest ENABLED = new UITestTest(ENABLED_STRING);
  public static final UITestTest EXISTS = new UITestTest(EXISTS_STRING);

  private final String action;

  private UITestTest(String action) {
    this.action = action;
  }

  public static UITestTest getInstance(String action) {
    if (action == null) {
      return null;
    }
    if (action.equals(ENABLED_STRING)) {
      return ENABLED;
    }
    if (action.equals(EXISTS_STRING)) {
      return EXISTS;
    }
    return null;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2006/10/10 05:23:34  sueh
 * <p> bug# 931 Enum for tests (ENABLED and EXISTS).
 * <p> </p>
 */
