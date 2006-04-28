package etomo.ui;

import etomo.storage.autodoc.AdocCommand;
import etomo.storage.autodoc.NameValuePair;
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
final class UITestAxisCommand implements AdocCommand {
  public static final String rcsid = "$Id$";

  private UITestAction action = null;
  private String value = null;
  private String string = "";
  private DialogType dialogType = null;
  private boolean empty = true;
  private boolean known = false;

  public void set(NameValuePair pair) {
    reset();
    if (pair == null) {
      return;
    }
    empty = false;
    string = pair.getString();
    if (pair.levels() == 0) {
      return;
    }
    //set the action
    action = UITestAction.getInstance(pair.getName(0));
    //get the value
    value = pair.getValue();
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

  DialogType getDialogType() {
    return dialogType;
  }

  boolean isEmpty() {
    return empty;
  }

  boolean isKnown() {
    return known;
  }

  public boolean isSecondaryAutodoc() {
    return false;
  }
}
/**
 * <p> $Log$ </p>
 */