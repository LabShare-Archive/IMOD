package etomo.type;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2009/09/01 03:17:08  sueh
 * <p> bug# 1222 Added COMBO_BOX.
 * <p>
 * <p> Revision 1.1  2009/01/20 19:42:26  sueh
 * <p> bug# 1102 Was UITestField.
 * <p> </p>
 */

public final class UITestFieldType {
  public static final String rcsid = "$Id$";
  public static final UITestFieldType BUTTON = new UITestFieldType("bn");
  public static final UITestFieldType CHECK_BOX = new UITestFieldType("cb");
  public static final UITestFieldType CHECK_BOX_MENU_ITEM = new UITestFieldType("cbmn");
  public static final UITestFieldType COMBO_BOX = new UITestFieldType("cbb");
  public static final UITestFieldType MENU_ITEM = new UITestFieldType("mn");
  public static final UITestFieldType MINI_BUTTON = new UITestFieldType("mb");
  public static final UITestFieldType PANEL = new UITestFieldType("pnl");
  public static final UITestFieldType RADIO_BUTTON = new UITestFieldType("rb");
  public static final UITestFieldType SPINNER = new UITestFieldType("sp");
  public static final UITestFieldType TAB = new UITestFieldType("tb");
  public static final UITestFieldType TEXT_FIELD = new UITestFieldType("tf");

  private final String string;

  private UITestFieldType(String string) {
    this.string = string;
  }

  public static UITestFieldType getInstance(String string) {
    if (string == null) {
      return null;
    }
    if (string.equals(BUTTON.toString())) {
      return BUTTON;
    }
    if (string.equals(CHECK_BOX.toString())) {
      return CHECK_BOX;
    }
    if (string.equals(COMBO_BOX.toString())) {
      return COMBO_BOX;
    }
    if (string.equals(MENU_ITEM.toString())) {
      return MENU_ITEM;
    }
    if (string.equals(MINI_BUTTON.toString())) {
      return MINI_BUTTON;
    }
    if (string.equals(PANEL.toString())) {
      return PANEL;
    }
    if (string.equals(RADIO_BUTTON.toString())) {
      return RADIO_BUTTON;
    }
    if (string.equals(SPINNER.toString())) {
      return SPINNER;
    }
    if (string.equals(TAB.toString())) {
      return TAB;
    }
    if (string.equals(TEXT_FIELD.toString())) {
      return TEXT_FIELD;
    }
    return null;
  }

  public String toString() {
    return string;
  }

}
