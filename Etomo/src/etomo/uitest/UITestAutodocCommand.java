package etomo.uitest;

import etomo.storage.autodoc.ReadOnlyStatement;
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
public final class UITestAutodocCommand implements UITestCommand {
  public static final String rcsid = "$Id$";

  private UITestAction action = null;
  private String value = null;
  private String string = "";
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
    //ignore unknown commands
    if (action == UITestAction.SLEEP) {
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
 * <p> Revision 1.1  2008/05/30 21:44:13  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.2  2007/04/09 20:03:08  sueh
 * <p> bug# 964 Change NameValuePair to an abstract class called Statement and
 * <p> child classes representing name/value pair, comment, empty line, and
 * <p> subsection.  Made delimiter change an attribute of the name/value pair class.
 * <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
 * <p>
 * <p> Revision 1.1  2007/03/21 18:12:59  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Creating Autodoc using a factory.  Moved AdocCommand classes out of the
 * <p> autodoc package because they not part of the autodoc.
 * <p>
 * <p> Revision 1.5  2007/03/08 22:03:14  sueh
 * <p> bug# 964 In NameValuePair, change the wording:  a name is made of 1 or more
 * <p> attributes.
 * <p>
 * <p> Revision 1.4  2006/10/24 21:45:15  sueh
 * <p> bug# 948 Removed file_dir and test_dir from global section.
 * <p>
 * <p> Revision 1.3  2006/08/28 18:24:27  sueh
 * <p> bug# 923 Changed the source attribute to filedir.  Global filedir is an absolute file
 * <p> path.
 * <p>
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
