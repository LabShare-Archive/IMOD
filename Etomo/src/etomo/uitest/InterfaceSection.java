package etomo.uitest;

import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.UITestAction;
import etomo.type.UITestField;

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
 * <p> Revision 1.1  2008/05/30 21:36:33  sueh
 * <p> bug# 1102 Class representing the Interface sections in uitest.adoc.
 * <p> </p>
 */
final class InterfaceSection {
  public static final String rcsid = "$Id$";

  private static final String DEFAULT_STRING = "default";

  private final ReadOnlySection section;
  private final ReadOnlyAttribute dialogAttribute;

  private InterfaceSection(ReadOnlySection section,
      ReadOnlyAttribute dialogAttribute) {
    this.section = section;
    this.dialogAttribute = dialogAttribute;
  }

  static InterfaceSection getInstance(ReadOnlyAutodoc autodoc,
      UITestCommand command) {
    ReadOnlySection section = null;
    ReadOnlyAttribute attribute = null;
    try {
      section = autodoc.getSection("Interface", command.getValue());
      attribute = section.getAttribute(UITestAction.DIALOG.toString());
    }
    catch (NullPointerException e) {
    }
    return new InterfaceSection(section, attribute);
  }

  /**
   * Get the navigation UITestField from the value side of a Dialog attribute.
   * @return the UITestField of the value of an attribute of the form:  navigation = value
   */
  UITestField getNavigationField() {
    try {
      return UITestField.getInstance(section.getAttribute("navigation")
          .getValue());
    }
    catch (NullPointerException e) {
      //Default navigation is bn
      return UITestField.BUTTON;
    }
  }

  /**
   * Get the default dialog from the section if it is not already set.  Compare
   * it to dialog.
   * @param dialog
   * @return true if there is an attribute:  Dialog.dialogName = default
   */
  boolean isDefaultDialog(String dialogName) {
    try {
      return dialogAttribute.getAttribute(dialogName).getValue().equals(
          DEFAULT_STRING);
    }
    catch (NullPointerException e) {
      return false;
    }
  }

  /**
   * Get a index of a dialog tab.
   * @param dialogName
   * @return the value of a name pair of the form:  Dialog.dialogName.index = value
   */
  int getIndex(String dialogName) {

    try {
      return Integer.parseInt(dialogAttribute.getAttribute(dialogName)
          .getAttribute("index").getValue());
    }
    catch (NullPointerException e) {
      return 0;
    }
  }
}
