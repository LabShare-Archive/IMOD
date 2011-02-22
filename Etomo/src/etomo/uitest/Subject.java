package etomo.uitest;

import junit.framework.Assert;
import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.type.AxisID;
import etomo.type.UITestActionType;
import etomo.type.UITestFieldType;
import etomo.type.UITestSubjectType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.3  2009/11/20 17:46:44  sueh
 * <p> bug# 1282 Limited the types of subcommand actions.
 * <p>
 * <p> Revision 1.2  2009/09/01 03:18:33  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.1  2009/01/20 20:48:59  sueh
 * <p> bug# 1102 Class that holds the subject portion of a Command.
 * <p> </p>
 */
class Subject extends Assert {
  public static final String rcsid = "$Id$";

  private final AxisID testAxisID;

  private boolean empty = true;
  private UITestSubjectType subjectType = null;
  private String name = null;
  private AxisID axisID = null;
  String string;

  Subject(AxisID testAxisID) {
    this.testAxisID = testAxisID;
  }

  void reset() {
    //Using empty in Command to avoid constructing subject every time a
    //statement is read.
    empty = true;
    subjectType = null;
    name = null;
    axisID = null;
    string = null;
  }

  private void assertValid() {
    //Grammer assertions
    assertNotNull("subjectType is required (" + string + ")", subjectType);
    assertNotNull("axisID should be assumed to be AxisID.ONLY when it is not included ("
        + string + ")", axisID);
    assertNotNull("must create string representation of command", string);
  }

  int set(ReadOnlyStatement statement, int startAt, VariableList variableList) {
    reset();
    assertNotNull("statement is null", statement);
    //Is there a subject at startAt?
    subjectType = UITestSubjectType.getInstance(statement.getLeftSide(startAt));
    if (subjectType == null) {
      //not subject type, so there is no subject
      return startAt;
    }
    //build the rest of the subject
    int i = startAt + 1;
    empty = false;
    string = statement.getString();
    //Check for axis without name
    String leftSide = statement.getLeftSide(i);
    axisID = AxisID.getInstance(leftSide);
    if (axisID != null) {
      i++;
    }
    else {
      axisID = AxisID.ONLY;
      //name can't be axis, or a subcommand action, field type
      UITestActionType actionType = UITestActionType.getSubcommandInstance(leftSide);
      UITestFieldType fieldType = UITestFieldType.getInstance(leftSide);
      if (actionType == null && fieldType == null) {
        //Set name
        name = Command.replaceVariables(leftSide, variableList, testAxisID);
        i++;
        //set axis
        axisID = AxisID.getInstance(statement.getLeftSide(i));
        if (axisID != null) {
          i++;
        }
        else {
          axisID = AxisID.ONLY;
        }
      }
    }
    assertValid();
    return i;
  }

  boolean isEmpty() {
    return empty;
  }

  UITestSubjectType getSubjectType() {
    return subjectType;
  }

  AxisID getAxisID() {
    return axisID;
  }

  String getName() {
    return name;
  }

  public String toString() {
    return string;
  }
}
