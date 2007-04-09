package etomo.storage;

import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.type.DialogType;
import etomo.type.UITestAction;

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
public final class UITestAxisCommand implements AdocCommand {
  public static final String rcsid = "$Id$";

  private UITestAction action = null;
  private String value = null;
  private String string = "";
  private DialogType dialogType = null;
  private boolean empty = true;
  private boolean known = false;

  public void set(ReadOnlyStatement statement) {
    reset();
    if (statement == null) {
      return;
    }
    empty = false;
    string = statement.getString();
    if (statement.sizeLeftSide() == 0) {
      return;
    }
    //set the action
    action = UITestAction.getInstance(statement.getLeftSide(0));
    //get the value
    value = statement.getRightSide();
    if (action == UITestAction.TEST_FROM || action == UITestAction.WAIT_FOR) {
      dialogType = DialogType.getInstance(value);
    }
    //ignore unknown commands
    if (action == UITestAction.SLEEP || action == UITestAction.TEST_FROM
        || action == UITestAction.WAIT_FOR || action == UITestAction.VERBOSE) {
      known = true;
    }
    else {
      action = null;
    }
  }

  public void reset() {
    empty = true;
    known = false;
    action = null;
    value = null;
    string = "";
    dialogType = null;
  }

  public UITestAction getAction() {
    return action;
  }

  public String getValue() {
    return value;
  }

  public String toString() {
    return string;
  }

  public DialogType getDialogType() {
    return dialogType;
  }

  public boolean isEmpty() {
    return empty;
  }

  public boolean isKnown() {
    return known;
  }

  public boolean isFunctionLocation() {
    return false;
  }
  
  public boolean isFunction() {
    return false;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2007/03/21 18:12:35  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Creating Autodoc using a factory.  Moved AdocCommand classes out of the
 * <p> autodoc package because they not part of the autodoc.
 * <p>
 * <p> Revision 1.3  2007/03/08 22:02:42  sueh
 * <p> bug# 964 In NameValuePair, change the wording:  a name is made of 1 or more
 * <p> attributes.
 * <p>
 * <p> Revision 1.2  2006/08/08 18:11:35  sueh
 * <p> bug# 852 Adding isFunctionLocation() and isFunction().  Removing
 * <p> isSecondaryAutodoc().
 * <p>
 * <p> Revision 1.1  2006/06/14 00:34:12  sueh
 * <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
 * <p> attributes specific to a type of autdoc.
 * <p>
 * <p> Revision 1.1  2006/04/28 21:08:34  sueh
 * <p> bug# 787 Parses the uitest axis autodoc global attributes.
 * <p> </p>
 */