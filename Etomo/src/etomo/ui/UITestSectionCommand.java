package etomo.ui;

import etomo.storage.autodoc.AdocCommand;
import etomo.storage.autodoc.NameValuePair;
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
final class UITestSectionCommand implements AdocCommand {
  public static final String rcsid = "$Id$";

  static final String SECTION_TYPE = "Test";
  private static final String KEEP_STRING = "keep";

  private UITestAction action = null;
  private String value = null;
  private String string = "";
  private boolean empty = true;
  private boolean known = false;
  private AxisID axisID = null;
  private boolean keep = false;

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
    }
    else if (action == UITestAction.DATASET_DIR) {
      String name = pair.getName(1);
      if (name != null && name.equals(KEEP_STRING)) {
        keep = true;
        return;
      }
    }
    //ignore unknown commands
    if (action == UITestAction.ADOC || action == UITestAction.COPY
        || action == UITestAction.DATA_FILE || action == UITestAction.DATASET
        || action == UITestAction.DATASET_DIR
        || action == UITestAction.DURATION
        || action == UITestAction.FIDUCIAL_DIAMETER
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
  }

  AxisID getAxisID() {
    return axisID;
  }
  
  boolean isKeep() {
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