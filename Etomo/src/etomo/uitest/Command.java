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
 * <p> Revision 1.7  2009/11/20 17:45:55  sueh
 * <p> bug# 1282 Added quotes and list separators.  Rewrote replaceVariables to handle them.
 * <p>
 * <p> Revision 1.6  2009/09/22 21:05:27  sueh
 * <p> bug# 1259 In replaceVariables handling empty variable.
 * <p>
 * <p> Revision 1.5  2009/09/02 22:46:08  sueh
 * <p> bug# 1254 Adding field to subcommand.
 * <p>
 * <p> Revision 1.4  2009/09/01 03:18:33  sueh
 * <p> bug# 1222
 * <p>
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

  private static final String SEPARATOR_OPERATOR = "|";
  private static final char IGNORE_VARIABLE_OPERATOR = '"';

  private final Subject subject;
  private final Field field;
  //current axis being tested
  private final AxisID testAxisID;

  private boolean empty = true;
  private UITestActionType actionType = null;
  private UITestModifierType modifierType = null;
  private Command subcommand = null;
  private String value = null;
  private String[] valueArray = null;
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
    valueArray = null;
    string = null;
    known = false;
  }

  private void assertValid() {
    if (actionType == null) {
      assertFalse("must have a field if there is not actionType(" + string + ")", field
          .isEmpty());
    }
    if (modifierType != null) {
      assertFalse("modifierType can't exist without a subject or a field (" + string
          + ")", subject.isEmpty() && field.isEmpty());
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
      i = subcommand.setSubcommand(statement, i, variableList);
    }
    String leftSide = statement.getLeftSide(i);
    assertNull("unknown attributes at the end of the command - " + leftSide + " ("
        + string + ")", leftSide);
    setValue(statement.getRightSide(), variableList);
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
  public int setSubcommand(ReadOnlyStatement statement, int startAt,
      VariableList variableList) {
    reset();
    assertNotNull("statement is null", statement);
    string = statement.getString();
    actionType = UITestActionType.getSubcommandInstance(statement.getLeftSide(startAt));
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
    i = field.set(statement, i, variableList);
    String leftSide = statement.getLeftSide(i);
    setValue(statement.getRightSide(), variableList);
    known = true;
    //validate
    assertValid();
    return i;
  }

  private void setValue(String rightSide, VariableList variableList) {
    if (rightSide != null) {
      valueArray = rightSide.trim().split("\\s*\\" + SEPARATOR_OPERATOR + "\\s*");
    }
    if (valueArray != null) {
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < valueArray.length; i++) {
        valueArray[i] = replaceVariables(valueArray[i], variableList, testAxisID);
        buffer.append(valueArray[i]
            + (i < valueArray.length - 1 ? SEPARATOR_OPERATOR : ""));
      }
      if (buffer.length() > 0) {
        value = buffer.toString();
      }
    }
  }

  /**
   * Replace variables references.  They have the syntax %{variablie_name}.  Do
   * not replace variable references inside of matching double quotes.  Strip
   * matching double quotes.
   * @param input
   * @param variableList
   * @param axisID
   * @return
   */
  static String replaceVariables(String input, VariableList variableList, AxisID axisID) {
    //Return input as is if it is empty.
    if (variableList == null || input == null) {
      return input;
    }
    StringBuffer buffer = new StringBuffer();
    boolean ignoringVariables = false;
    int variableRefStart = -1;
    int bufferVariableRefStart = -1;
    int index = 0;
    //Walk through input looking for quotes and variable references.  Put each
    //character into the buffer, replacing variable references.
    while (index < input.length()) {
      char curChar = input.charAt(index);
      if (curChar == IGNORE_VARIABLE_OPERATOR) {
        //Found a quote.  Decide whether this is a start quote, an end quote, or
        //an unmatched quote.
        if (ignoringVariables) {
          //Found end quote.
          ignoringVariables = false;
        }
        //This is a start quote if a matching end quote can be found, otherwise
        //ignore it.
        else if (index + 1 < input.length()
            && input.indexOf(IGNORE_VARIABLE_OPERATOR, index + 1) != -1) {
          //Found start quote.
          ignoringVariables = true;
          //Quotes take precedence over variable references, so a matched quote
          //in the middle of a variable reference invalidates the variable
          //reference.
          variableRefStart = -1;
          bufferVariableRefStart = -1;
        }
        else {
          //Not a start or end quote so don't strip it.
          buffer.append(curChar);
        }
      }
      else {
        //Only matching quotes are stripped.
        buffer.append(curChar);
        //If not ignoring variables, check for a variable reference.
        if (!ignoringVariables) {
          //If not already in a variable reference, check for the start of a
          //variable reference %{.
          if (variableRefStart == -1 && curChar == '%' && input.length() > index + 1
              && input.charAt(index + 1) == '{') {
            //May be the start of a variable reference.
            variableRefStart = index;
            bufferVariableRefStart = buffer.length() - 1;
            //Move the index forward to the {.
            index++;
            buffer.append(input.charAt(index));
          }
          //If currently inside a variables reference, check for the end of the
          //variable reference.  Ignore empty variable references.
          else if (variableRefStart != -1 && curChar == '}'
              && bufferVariableRefStart + 2 < buffer.length() - 1) {
            //Found the end of the variable reference - substitute.
            String variableName = buffer.substring(bufferVariableRefStart + 2, buffer
                .length() - 1);
            assertTrue("Unknown variable " + variableName + " (" + input + ")",
                variableList.isVariableSet(variableName, axisID));
            String variableValue = variableList.getVariableValue(variableName, axisID);
            if (variableValue == null) {
              variableValue = "";
            }
            //Replace the variable reference, including the %{} characters, in the
            //buffer.
            buffer.replace(bufferVariableRefStart, buffer.length(), variableValue);
            variableRefStart = -1;
            bufferVariableRefStart = -1;
          }
        }
      }
      index++;
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

  /**
   * Return the valueArray[index].  ValueArray is value, broken up by dividers
   * ("|").  Whitespace around the "|" is considered to be part of the
   * divider.
   * @param index
   * @return valueArray[index] or null for an invalid request
   */
  String getValue(int index) {
    if (valueArray == null || valueArray.length == 0) {
      if (index == 0) {
        return value;
      }
      else
        return null;
    }
    if (index >= valueArray.length) {
      return null;
    }
    return valueArray[index];
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
