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
    if (action == UITestAction.SLEEP || action == UITestAction.FILE_DIR
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

  public boolean isFunctionLocation() {
    return false;
  }
  
  public boolean isFunction() {
    return false;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/08/08 18:17:20  sueh
* <p> bug# 852 Adding isFunctionLocation() and isFunction().  Removing
* <p> isSecondaryAutodoc().
* <p>
* <p> Revision 1.1  2006/06/14 00:34:42  sueh
* <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
* <p> attributes specific to a type of autdoc.
* <p>
* <p> Revision 1.2  2006/04/28 21:10:09  sueh
* <p> bug# 787 Used to be UITestAxisDialogCommand.  Parses the uitest
* <p> autodoc global name/value pairs.
* <p> </p>
*/