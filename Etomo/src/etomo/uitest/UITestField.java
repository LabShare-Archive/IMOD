package etomo.uitest;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class UITestField {
  public static final String rcsid = "$Id$";

  private static final String BUTTON_STRING = "bn";
  private static final String CHECK_BOX_STRING = "cb";
  private static final String FILE_STRING = "file";
  private static final String MINI_BUTTON_STRING = "mb";
  private static final String POPUP_STRING = "popup";
  private static final String PROCESS_STRING = "process";
  private static final String RADIO_BUTTON_STRING = "rb";
  private static final String TEXT_FIELD_STRING = "tf";
  private static final String TABBED_PANE_STRING = "tp";
  private static final String SPINNER_STRING = "sp";

  public static final UITestField BUTTON = new UITestField(BUTTON_STRING);
  public static final UITestField CHECK_BOX = new UITestField(CHECK_BOX_STRING);
  public static final UITestField FILE = new UITestField(FILE_STRING);
  public static final UITestField MINI_BUTTON = new UITestField(
      MINI_BUTTON_STRING);
  public static final UITestField POPUP = new UITestField(POPUP_STRING);
  public static final UITestField PROCESS = new UITestField(PROCESS_STRING);
  public static final UITestField RADIO_BUTTON = new UITestField(
      RADIO_BUTTON_STRING);
  public static final UITestField TEXT_FIELD = new UITestField(
      TEXT_FIELD_STRING);
  public static final UITestField TABBED_PANE = new UITestField(
      TABBED_PANE_STRING);
  public static final UITestField SPINNER = new UITestField(SPINNER_STRING);
  static final UITestField DIALOG = new UITestField(
      DialogSectionCommand.SECTION_TYPE);

  private final String field;

  private UITestField(String field) {
    this.field = field;
  }

  public String toString() {
    return field;
  }

  public static UITestField getInstance(String field) {
    if (field == null) {
      return null;
    }
    if (field.equals(BUTTON_STRING)) {
      return BUTTON;
    }
    if (field.equals(CHECK_BOX_STRING)) {
      return CHECK_BOX;
    }
    if (field.equals(FILE_STRING)) {
      return FILE;
    }
    if (field.equals(MINI_BUTTON_STRING)) {
      return MINI_BUTTON;
    }
    if (field.equals(POPUP_STRING)) {
      return POPUP;
    }
    if (field.equals(PROCESS_STRING)) {
      return PROCESS;
    }
    if (field.equals(RADIO_BUTTON_STRING)) {
      return RADIO_BUTTON;
    }
    if (field.equals(TEXT_FIELD_STRING)) {
      return TEXT_FIELD;
    }
    if (field.equals(TABBED_PANE_STRING)) {
      return TABBED_PANE;
    }
    if (field.equals(SPINNER_STRING)) {
      return SPINNER;
    }
    if (field.equals(DIALOG.field)) {
      return DIALOG;
    }
    return null;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2006/10/10 05:23:04  sueh
 * <p> bug# 931 Adding FILE.
 * <p>
 * <p> Revision 1.2  2006/04/28 20:57:32  sueh
 * <p> bug# 787 Added mini-button.
 * <p>
 * <p> Revision 1.1  2006/04/25 18:59:16  sueh
 * <p> bug# 787 An enum style class which contains the fields found in
 * <p> uitestaxis.adoc files.
 * <p> </p>
 */
