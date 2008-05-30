package etomo.uitest;

import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.type.AxisID;
import etomo.type.UITestAction;
import etomo.type.UITestField;

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
public final class TestSectionCommand implements UITestCommand {
  public static final String rcsid = "$Id$";

  public static final String SECTION_TYPE = "Test";
  private static final String KEEP_STRING = "keep";

  private UITestAction action = null;
  private UITestField field = null;
  private String value = null;
  private String string = "";
  private boolean empty = true;
  private boolean known = false;
  private AxisID axisID = null;
  private boolean keep = false;
  private Variable variable = null;

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
    //get the action
    action = UITestAction.getInstance(statement.getLeftSide(0));
    //get the value
    value = statement.getRightSide();
    if (action == UITestAction.ADOC) {
      axisID = AxisID.getInstance(statement.getLeftSide(1));
      if (axisID == null) {
        axisID = AxisID.ONLY;
      }
      variable = new Variable("axis", axisID.getExtension());
    }
    else if (action == UITestAction.DATASET_DIR) {
      String keepString = statement.getLeftSide(1);
      if (keepString != null && keepString.equals(KEEP_STRING)) {
        keep = true;
      }
    }
    else if (action == UITestAction.SET) {
      String name = statement.getLeftSide(1);
      axisID = AxisID.getInstance(name);
      if (axisID == null) {
        variable = new Variable(name, value);
      }
      else {
        variable = new Variable(statement.getLeftSide(2), value);
      }
    }
    else if (action == UITestAction.DATASET) {
      variable = new Variable(action.toString(), value);
    }
    else if (action == UITestAction.COPY) {
      field = UITestField.getInstance(statement.getLeftSide(1));
    }
    //ignore unknown commands
    if (action == UITestAction.ADOC
        || (action == UITestAction.COPY && field == UITestField.FILE)
        || action == UITestAction.DATA_FILE || action == UITestAction.DATASET
        || action == UITestAction.DATASET_DIR
        || action == UITestAction.DURATION || action == UITestAction.SET
        || action == UITestAction.DATA_DIR || action == UITestAction.INTERFACE) {
      known = true;
    }
    else {
      action = null;
      System.err.println("Unknown command:  " + string);
    }
  }

  public void reset() {
    empty = true;
    known = false;
    action = null;
    value = null;
    string = "";
    axisID = null;
    keep = false;
    variable = null;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public boolean isKeep() {
    return keep;
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

  public Variable getVariable() {
    return variable;
  }

  static final class Variable {
    private final String name;
    private final String value;

    Variable(String name, String value) {
      this.name = name;
      if (value == null) {
        value = "";
      }
      this.value = value;
    }

    String getName() {
      return name;
    }

    String getValue() {
      return value;
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2008/05/30 21:37:17  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.2  2007/04/09 20:03:17  sueh
 * <p> bug# 964 Change NameValuePair to an abstract class called Statement and
 * <p> child classes representing name/value pair, comment, empty line, and
 * <p> subsection.  Made delimiter change an attribute of the name/value pair class.
 * <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
 * <p>
 * <p> Revision 1.1  2007/03/21 18:13:14  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Creating Autodoc using a factory.  Moved AdocCommand classes out of the
 * <p> autodoc package because they not part of the autodoc.
 * <p>
 * <p> Revision 1.6  2007/03/08 22:03:24  sueh
 * <p> bug# 964 In NameValuePair, change the wording:  a name is made of 1 or more
 * <p> attributes.
 * <p>
 * <p> Revision 1.5  2006/10/24 21:47:04  sueh
 * <p> bug# 948 Changed filedir to datadir.
 * <p>
 * <p> Revision 1.4  2006/10/10 05:21:23  sueh
 * <p> bug# 931 Fixed dataset dir functionality.
 * <p>
 * <p> Revision 1.3  2006/08/28 18:25:31  sueh
 * <p> bug# 923 Changed the uitest source attribute to filedir.  Global filedir is an
 * <p> absolute file path.
 * <p>
 * <p> Revision 1.2  2006/08/08 18:17:38  sueh
 * <p> bug# 852 Adding isFunctionLocation() and isFunction().  Removing
 * <p> isSecondaryAutodoc().
 * <p>
 * <p> Revision 1.1  2006/06/14 00:34:53  sueh
 * <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
 * <p> attributes specific to a type of autdoc.
 * <p>
 * <p> Revision 1.2  2006/05/01 21:21:37  sueh
 * <p> bug# 787 Removed fiducial diameter, added set.
 * <p>
 * <p> Revision 1.1  2006/04/28 21:11:31  sueh
 * <p> bug# 787 Parses the uitest autodoc global name/value pairs.
 * <p> </p>
 */
