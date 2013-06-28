package etomo.ui.swing;

import java.awt.Container;

import javax.swing.JCheckBox;
import javax.swing.text.Document;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoBoolean2;
import etomo.type.UITestFieldType;
import etomo.util.Utilities;

/**
 * <p>Description: A self-naming check box.  Implements StateChangeSource with its state
 * equal to whether it has changed since it was checkpointed.</p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.4  2011/04/25 23:30:53  sueh
 * <p> bug# 1416 Implemented StateChangeActionSource.  Added equals(Object) and equals(Document) so
 * <p> StateChangedReporter can find instances of this class.
 * <p>
 * <p> Revision 1.3  2011/04/04 17:17:19  sueh
 * <p> bug# 1416 Added savedValue, checkpoint, isChanged.
 * <p>
 * <p> Revision 1.2  2011/02/22 18:04:56  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.19  2010/03/05 04:02:01  sueh
 * <p> bug# 1319 Added a constructor with no label string.
 * <p>
 * <p> Revision 1.18  2009/11/20 17:02:08  sueh
 * <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
 * <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
 * <p> "bn." field command.
 * <p>
 * <p> Revision 1.17  2009/04/13 22:55:46  sueh
 * <p> Removed newstuff.
 * <p>
 * <p> Revision 1.16  2009/02/27 03:50:23  sueh
 * <p> bug# 1172 Added experimental automation recording background color
 * <p> (newstuff only).
 * <p>
 * <p> Revision 1.15  2009/01/20 19:49:28  sueh
 * <p> bug# 1102 Changed this UITestField to UITestFieldType.
 * <p>
 * <p> Revision 1.14  2008/10/27 20:30:26  sueh
 * <p> bug# 1141 Added printinfo (debugging tool).
 * <p>
 * <p> Revision 1.13  2008/05/30 22:31:35  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 1.12  2008/05/30 21:28:02  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.11  2007/12/26 22:22:42  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.10  2007/09/07 00:26:17  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.9  2007/02/09 00:47:40  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.8  2007/02/05 23:34:30  sueh
 * <p> bug# 962 Removed commented out functions.
 * <p>
 * <p> Revision 1.7  2006/05/16 21:35:17  sueh
 * <p> bug# 856 Changing the name whenever the label is changed so that its easy to
 * <p> see what the name is.
 * <p>
 * <p> Revision 1.6  2006/04/25 19:12:23  sueh
 * <p> bug# 787 Added UITestField, an enum style class which contains the
 * <p> fields found in uitestaxis.adoc files.
 * <p>
 * <p> Revision 1.5  2006/04/06 20:15:51  sueh
 * <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> util.Utilities.
 * <p>
 * <p> Revision 1.4  2006/01/12 17:08:14  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.3  2006/01/11 21:58:46  sueh
 * <p> bug# 675 corrected print name functionality
 * <p>
 * <p> Revision 1.2  2006/01/04 20:23:29  sueh
 * <p> bug# 675 For printing the name:  putting the type first and making the type
 * <p> as constant.
 * <p>
 * <p> Revision 1.1  2006/01/03 23:30:46  sueh
 * <p> bug# 675 Extends JCheckBox.  Names the check box using the label.
 * <p> </p>
 */
final class CheckBox extends JCheckBox {
  public static final String rcsid = "$Id$";

  private EtomoBoolean2 checkpointValue = null;
  private boolean debug = false;

  public CheckBox() {
    super();
  }

  public CheckBox(String text) {
    super(text);
    setName(text);
  }

  public boolean equals(final Object object) {
    return object == this;
  }

  public boolean equals(final Document document) {
    return false;
  }

  public String toString() {
    return "[text:" + getText() + "]";
  }

  public void setText(String text) {
    super.setText(text);
    setName(text);
  }

  public void setName(String text) {
    String name = Utilities.convertLabelToName(text);
    super.setName(UITestFieldType.CHECK_BOX.toString() + AutodocTokenizer.SEPARATOR_CHAR
        + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
  }

  /**
   * Constructs savedValue (if it doesn't exist).  Saves the current setting.
   */
  void checkpoint() {
    if (checkpointValue == null) {
      checkpointValue = new EtomoBoolean2();
    }
    checkpointValue.set(isSelected());
  }

  /**
   * Constructs savedValue (if it doesn't exist).  Saves the value parameter.
   */
  void checkpoint(final boolean value) {
    if (checkpointValue == null) {
      checkpointValue = new EtomoBoolean2();
    }
    checkpointValue.set(value);
  }

  /**
   * Resets to checkpointValue if checkpointValue has been set.  Otherwise has no effect.
   */
  void resetToCheckpoint() {
    if (checkpointValue == null) {
      return;
    }
    setSelected(checkpointValue.is());
  }

  void setDebug(final boolean input) {
    debug = input;
  }

  /**
   * If the field is disabled or not visible then return false because its value doesn't
   * matter.  It returns true if the checkpoint has not been done; the checkpoint value is
   * from an outside value, so the current value must be different from a non-existant
   * checkpoint value.  After eliminating this possibility, it returns a boolean based on
   * the difference between the selected state of the checkbox and the checkpointed value.
   * @return
   */
  boolean isDifferentFromCheckpoint() {
    return isDifferentFromCheckpoint(false);
  }

  /**
   * 
   * @param alwaysCheck - check for difference even when the field is disables or invisible
   * @return
   */
  boolean isDifferentFromCheckpoint(final boolean alwaysCheck) {
    if (!alwaysCheck && (!isEnabled() || !isVisible())) {
      return false;
    }
    return checkpointValue == null || !checkpointValue.equals(isSelected());
  }

  public void setToolTipText(String text) {
    super.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  public void setToolTipText(final ReadOnlySection section, final String enumValue) {
    setToolTipText(EtomoAutodoc.getTooltip(section, enumValue));
  }

  void printInfo() {
    System.out.println(getName());
    printInfo(getParent());
  }

  private void printInfo(Container parent) {
    System.out.println(parent);
    if (parent != null) {
      String parentName = parent.getName();
      if (parentName != null) {
        System.out.println(parent.getName());
      }
      else {
        printInfo(parent.getParent());
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2011/04/25 23:30:53  sueh
 * <p> bug# 1416 Implemented StateChangeActionSource.  Added equals(Object) and equals(Document) so
 * <p> StateChangedReporter can find instances of this class.
 * <p>
 * <p> Revision 1.3  2011/04/04 17:17:19  sueh
 * <p> bug# 1416 Added savedValue, checkpoint, isChanged.
 * <p>
 * <p> Revision 1.2  2011/02/22 18:04:56  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.19  2010/03/05 04:02:01  sueh
 * <p> bug# 1319 Added a constructor with no label string.
 * <p>
 * <p> Revision 1.18  2009/11/20 17:02:08  sueh
 * <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
 * <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
 * <p> "bn." field command.
 * <p>
 * <p> Revision 1.17  2009/04/13 22:55:46  sueh
 * <p> Removed newstuff.
 * <p>
 * <p> Revision 1.16  2009/02/27 03:50:23  sueh
 * <p> bug# 1172 Added experimental automation recording background color
 * <p> (newstuff only).
 * <p>
 * <p> Revision 1.15  2009/01/20 19:49:28  sueh
 * <p> bug# 1102 Changed this UITestField to UITestFieldType.
 * <p>
 * <p> Revision 1.14  2008/10/27 20:30:26  sueh
 * <p> bug# 1141 Added printinfo (debugging tool).
 * <p>
 * <p> Revision 1.13  2008/05/30 22:31:35  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 1.12  2008/05/30 21:28:02  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.11  2007/12/26 22:22:42  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.10  2007/09/07 00:26:17  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.9  2007/02/09 00:47:40  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.8  2007/02/05 23:34:30  sueh
 * <p> bug# 962 Removed commented out functions.
 * <p>
 * <p> Revision 1.7  2006/05/16 21:35:17  sueh
 * <p> bug# 856 Changing the name whenever the label is changed so that its easy to
 * <p> see what the name is.
 * <p>
 * <p> Revision 1.6  2006/04/25 19:12:23  sueh
 * <p> bug# 787 Added UITestField, an enum style class which contains the
 * <p> fields found in uitestaxis.adoc files.
 * <p>
 * <p> Revision 1.5  2006/04/06 20:15:51  sueh
 * <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> util.Utilities.
 * <p>
 * <p> Revision 1.4  2006/01/12 17:08:14  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.3  2006/01/11 21:58:46  sueh
 * <p> bug# 675 corrected print name functionality
 * <p>
 * <p> Revision 1.2  2006/01/04 20:23:29  sueh
 * <p> bug# 675 For printing the name:  putting the type first and making the type
 * <p> as constant.
 * <p>
 * <p> Revision 1.1  2006/01/03 23:30:46  sueh
 * <p> bug# 675 Extends JCheckBox.  Names the check box using the label.
 * <p> </p>
 */
