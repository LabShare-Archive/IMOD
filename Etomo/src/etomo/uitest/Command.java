package etomo.uitest;

import junit.framework.Assert;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.storage.autodoc.Statement;
import etomo.type.AxisID;
import etomo.type.UITestActionType;
import etomo.type.UITestSubjectType;

/**
 * <p>Description: An action command as described in the uitest man page.
 * Because action commands and field commands are mixed together, this class
 * can also read a field command.</p>
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
 * <p> Revision 1.3  2009/02/13 02:38:47  sueh
 * <p> bug# 1102 Added optional modifier to subcommand.
 * <p>
 * <p> Revision 1.2  2009/01/28 00:58:20  sueh
 * <p> bug# 1102 Allowing a subcommand after a field.
 * <p>
 * <p> Revision 1.1  2009/01/20 20:46:19  sueh
 * <p> bug# 1102 Class that holds all uitest commands.
 * <p> </p>
 */
final class Command extends Assert {
  public static final String rcsid = "$Id$";

  private final Subject subject;
  private final Field field;
  //current axis being tested
  private final AxisID testAxisID;

  private boolean empty = true;
  private UITestActionType actionType = null;
  private UITestModifierType modifierType = null;
  private Command subcommand = null;
  private String value = null;
  private ReadOnlySection subsection = null;
  private String string = null;
  private boolean known = false;

  Command(AxisID testAxisID) {
    this.testAxisID = testAxisID;
    subject = new Subject(testAxisID);
    field = new Field(testAxisID);
  }

  private void reset() {
    empty = true;
    subsection = null;
    actionType = null;
    subject.reset();
    modifierType = null;
    field.reset();
    if (subcommand != null) {
      subcommand.reset();
    }
    value = null;
    string = null;
    known = false;
  }

  private void assertValid() {
    if (actionType == null) {
      assertFalse("must have a field if there is not actionType(" + string
          + ")", field.isEmpty());
    }
    if (modifierType != null) {
      assertFalse("modifierType can't exist without a subject or a field ("
          + string + ")", subject.isEmpty() && field.isEmpty());
    }
    assertNotNull("must create string representation of command", string);
    assertTrue("processed command must be known (" + string + ")", isKnown());
  }

  /**
   * Set a command from a statement.
   * @param statement
   */
  public void set(ReadOnlyStatement statement, VariableList variableList) {
    reset();
    if (statement == null) {
      return;
    }
    string = statement.getString();
    Statement.Type type = statement.getType();
    if (type == Statement.Type.EMPTY_LINE || type == Statement.Type.COMMENT) {
      return;
    }
    if (type == Statement.Type.SUBSECTION) {
      subsection = statement.getSubsection();
    }
    int i = 0;
    actionType = UITestActionType.getInstance(statement.getLeftSide(i));
    if (actionType == null) {
      i = field.set(statement, i, variableList);
    }
    else {
      i++;
      modifierType = UITestModifierType.getInstance(statement.getLeftSide(i));
      if (modifierType != null) {
        i++;
      }
      i = subject.set(statement, i, variableList);
      i = field.set(statement, i, variableList);
      if (subcommand == null) {
        subcommand = new Command(testAxisID);
      }
      i = subcommand.set(statement, i, variableList);
    }
    String leftSide = statement.getLeftSide(i);
    assertNull("unknown attributes at the end of the command - " + leftSide
        + " (" + string + ")", leftSide);
    value = replaceVariables(statement.getRightSide(), variableList, testAxisID);
    known = true;
    //validate
    assertValid();
  }

  /**
   * Used to set a subcommand.
   * @param statement
   * @param startAt
   * @param variableList
   * @return
   */
  public int set(ReadOnlyStatement statement, int startAt,
      VariableList variableList) {
    reset();
    assertNotNull("statement is null", statement);
    string = statement.getString();
    Statement.Type type = statement.getType();
    if (type == Statement.Type.EMPTY_LINE || type == Statement.Type.COMMENT) {
      return startAt;
    }
    actionType = UITestActionType.getInstance(statement.getLeftSide(startAt));
    if (actionType == null) {
      return startAt;
    }
    int i = startAt + 1;
    empty = false;
    modifierType = UITestModifierType.getInstance(statement.getLeftSide(i));
    if (modifierType != null) {
      i++;
    }
    i = subject.set(statement, i, variableList);
    String leftSide = statement.getLeftSide(i);
    value = replaceVariables(statement.getRightSide(), variableList, testAxisID);
    known = true;
    //validate
    assertValid();
    return i;
  }

  static String replaceVariables(String input, VariableList variableList,
      AxisID axisID) {
    if (variableList == null || input == null
        || !input.matches(".*%\\{.+\\}.*")) {
      return input;
    }
    StringBuffer buffer = new StringBuffer();
    buffer.append(input);
    boolean done = false;
    int start;
    int end;
    while (!done) {
      start = buffer.indexOf("%{");
      if (start == -1) {
        done = true;
        continue;
      }
      end = buffer.indexOf("}", start);
      if (end == -1) {
        done = true;
        continue;
      }
      String variableName = buffer.substring(start + 2, end);
      String variableValue = variableList
          .getVariableValue(variableName, axisID);
      assertNotNull("Unknown variable " + variableName + " (" + input + ")",
          variableValue);
      buffer.replace(start, end + 1, variableValue);
    }
    return buffer.toString();
  }

  boolean isEmpty() {
    return empty;
  }

  boolean isKnown() {
    return known;
  }

  UITestActionType getActionType() {
    return actionType;
  }

  Subject getSubject() {
    if (subject.isEmpty()) {
      return null;
    }
    return subject;
  }

  UITestSubjectType getSubjectType() {
    if (subject.isEmpty()) {
      return null;
    }
    return subject.getSubjectType();
  }

  UITestModifierType getModifierType() {
    return modifierType;
  }

  Field getField() {
    if (field.isEmpty()) {
      return null;
    }
    return field;
  }

  Command getSubcommand() {
    if (subcommand == null) {
      return null;
    }
    if (subcommand.isEmpty()) {
      return null;
    }
    return subcommand;
  }

  String getValue() {
    return value;
  }

  boolean isSubsection() {
    return subsection != null;
  }

  ReadOnlySection getSubsection() {
    return subsection;
  }

  public String toString() {
    return string;
  }
}
