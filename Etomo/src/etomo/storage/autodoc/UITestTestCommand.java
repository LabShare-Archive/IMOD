package etomo.storage.autodoc;

import etomo.type.AxisID;
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
public final class UITestTestCommand implements AdocCommand {
  public static final String rcsid = "$Id$";

  public static final String SECTION_TYPE = "Test";
  private static final String KEEP_STRING = "keep";

  private UITestAction action = null;
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
    if (pair.levels() == 0) {
      return;
    }
    //get the action
    action = UITestAction.getInstance(pair.getName(0));
    //get the value
    value = pair.getValue();
    if (action == UITestAction.ADOC) {
      axisID = AxisID.getInstance(pair.getName(1));
      if (axisID == null) {
        axisID = AxisID.ONLY;
      }
      variable = new Variable("axis", axisID.getExtension());
    }
    else if (action == UITestAction.DATASET_DIR) {
      String keepString = pair.getName(1);
      if (keepString != null && keepString.equals(KEEP_STRING)) {
        keep = true;
        return;
      }
    }
    else if (action == UITestAction.SET) {
      String name = pair.getName(1);
      axisID = AxisID.getInstance(name);
      if (axisID == null) {
        variable = new Variable(name, value);
      }
      else {
        variable = new Variable(pair.getName(2), value);
      }
    }
    else if (action == UITestAction.DATASET) {
      variable = new Variable(action.toString(), value);
    }
    //ignore unknown commands
    if (action == UITestAction.ADOC || action == UITestAction.COPY
        || action == UITestAction.DATA_FILE || action == UITestAction.DATASET
        || action == UITestAction.DATASET_DIR
        || action == UITestAction.DURATION || action == UITestAction.SET
        || action == UITestAction.SOURCE) {
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

  public boolean isSecondaryAutodoc() {
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
 * <p> Revision 1.2  2006/05/01 21:21:37  sueh
 * <p> bug# 787 Removed fiducial diameter, added set.
 * <p>
 * <p> Revision 1.1  2006/04/28 21:11:31  sueh
 * <p> bug# 787 Parses the uitest autodoc global name/value pairs.
 * <p> </p>
 */