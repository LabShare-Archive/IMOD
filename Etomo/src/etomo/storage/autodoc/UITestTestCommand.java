package etomo.storage.autodoc;

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
public final class UITestTestCommand implements AdocCommand {
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

  public void set(NameValuePair pair) {
    reset();
    if (pair == null) {
      return;
    }
    empty = false;
    string = pair.getString();
    if (pair.numAttributes() == 0) {
      return;
    }
    //get the action
    action = UITestAction.getInstance(pair.getAttribute(0));
    //get the value
    value = pair.getValue();
    if (action == UITestAction.ADOC) {
      axisID = AxisID.getInstance(pair.getAttribute(1));
      if (axisID == null) {
        axisID = AxisID.ONLY;
      }
      variable = new Variable("axis", axisID.getExtension());
    }
    else if (action == UITestAction.DATASET_DIR) {
      String keepString = pair.getAttribute(1);
      if (keepString != null && keepString.equals(KEEP_STRING)) {
        keep = true;
      }
    }
    else if (action == UITestAction.SET) {
      String name = pair.getAttribute(1);
      axisID = AxisID.getInstance(name);
      if (axisID == null) {
        variable = new Variable(name, value);
      }
      else {
        variable = new Variable(pair.getAttribute(2), value);
      }
    }
    else if (action == UITestAction.DATASET) {
      variable = new Variable(action.toString(), value);
    }
    else if (action == UITestAction.COPY) {
      field = UITestField.getInstance(pair.getAttribute(1));
    }
    //ignore unknown commands
    if (action == UITestAction.ADOC
        || (action == UITestAction.COPY && field == UITestField.FILE)
        || action == UITestAction.DATA_FILE || action == UITestAction.DATASET
        || action == UITestAction.DATASET_DIR
        || action == UITestAction.DURATION || action == UITestAction.SET
        || action == UITestAction.DATA_DIR) {
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
