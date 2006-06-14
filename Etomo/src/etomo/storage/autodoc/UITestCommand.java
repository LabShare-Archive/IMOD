package etomo.storage.autodoc;

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
public final class UITestCommand implements AdocCommand {
  public static final String rcsid = "$Id$";
  
  private UITestAction action = null;
  private String value = null;
  private String string = "";
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
    //ignore unknown commands
    if (action == UITestAction.SLEEP || action == UITestAction.SOURCE
        || action == UITestAction.TEST_DIR) {
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

  public boolean isEmpty() {
    return empty;
  }

  public boolean isKnown() {
    return known;
  }

  public boolean isSecondaryAutodoc() {
    return false;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/04/28 21:10:09  sueh
* <p> bug# 787 Used to be UITestAxisDialogCommand.  Parses the uitest
* <p> autodoc global name/value pairs.
* <p> </p>
*/