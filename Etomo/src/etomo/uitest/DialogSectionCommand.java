package etomo.uitest;

import java.util.ArrayList;

import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.type.AxisID;
import etomo.type.CallbackClassEnum;
import etomo.type.EtomoAutodoc;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;

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
public class DialogSectionCommand implements UITestCommand {
  public static final String rcsid = "$Id$";

  public static final String SECTION_TYPE = "Dialog";
  private static final String ENABLED_STRING = "enabled";

  private final ArrayList variables;

  private UITestAction action = null;
  private UITestField field = null;
  private UITestTest test = null;
  private String fieldName = null;
  private String formattedFieldName = null;
  private int fieldIndex = 0;
  private String value = null;
  private String formattedValue = null;
  private String string = "";
  private ProcessEndState processEndState = null;
  private boolean empty = true;
  private boolean known = false;
  private boolean autodoc = false;
  private boolean function = false;
  private CallbackClassEnum callbackClassEnum = null;
  private ProcessName processName = null;

  public DialogSectionCommand(ArrayList variables) {
    this.variables = variables;
  }

  public final void set(ReadOnlyStatement statement) {
    reset();
    if (statement == null) {
      return;
    }
    else {
      empty = false;
      string = statement.getString();
      if (statement.sizeLeftSide() == 0) {
        return;
      }
      int index = 0;
      //set the action
      action = UITestAction.getInstance(statement.getLeftSide(0));
      if (action == UITestAction.ADOC) {
        autodoc = true;
      }
      else if (action == UITestAction.FUNCTION) {
        function = true;
      }
      //set the field
      else if (action != UITestAction.SLEEP && action != UITestAction.STOP) {
        if (action == UITestAction.WAIT_FOR || action == UITestAction.ASSERT
            || action == UITestAction.COPY) {
          index++;
        }
        setField(statement, index);
      }
      //get the value
      value = statement.getRightSide();
      formattedValue = replaceVariables(value, formattedValue);
      if (action == UITestAction.WAIT_FOR) {
        //handle waitfor =
        //Defaults to dialog
        if (field == null) {
          field = UITestField.DIALOG;
        }
        //handle waitfor.process = 
        if (field == UITestField.PROCESS) {
          processEndState = ProcessEndState.getInstance(value);
        }
      }
    }
    //this ignores the Version name/value pair
    if (action == UITestAction.ADOC
        || (action == UITestAction.COPY && field == UITestField.FILE)
        || action == UITestAction.SLEEP || action == UITestAction.STOP
        || action == UITestAction.WAIT_FOR || action == UITestAction.FUNCTION
        || field != null) {
      known = true;
      return;
    }
    //check for valid assert
    if (action == UITestAction.ASSERT && field != null) {
      if (test == null) {
        known = true;
        return;
      }
      if ((field != UITestField.FILE && test == UITestTest.ENABLED)
          || (field == UITestField.FILE && test == UITestTest.EXISTS)) {
        known = true;
        return;
      }
    }
    action = null;
    System.err.println("Unknown command:  " + string);
  }

  private void setField(ReadOnlyStatement statement, int index) {
    int levels = statement.sizeLeftSide();
    if (index >= levels) {
      return;
    }
    field = UITestField.getInstance(statement.getLeftSide(index++));
    if (index >= levels) {
      return;
    }
    if (action == UITestAction.ASSERT && field == UITestField.FILE) {
      test = UITestTest.getInstance(statement.getLeftSide(index++));
    }
    else if (action == UITestAction.WAIT_FOR && field == UITestField.PROCESS) {
      processName = ProcessName.getInstance(statement.getLeftSide(index++),
          AxisID.ONLY);
    }
    else {
      fieldName = statement.getLeftSide(index++);
      formattedFieldName = replaceVariables(fieldName, formattedFieldName);
      if (index >= levels) {
        return;
      }
      //may end in ".enabled"
      String name = statement.getLeftSide(index++);
      test = UITestTest.getInstance(name);
      if (test == UITestTest.ENABLED) {
        return;
      }
      try {
        //may contain an index
        fieldIndex = Integer.parseInt(name);
      }
      catch (NumberFormatException e) {
      }
      if (index >= levels) {
        return;
      }
      //may end in ".enabled"
      test = UITestTest.getInstance(statement.getLeftSide(index));
    }
  }

  /**
   * Replaces %{variable} with variableValue in formattedString.
   * @param formattedString
   * @param variable
   * @param variableValue
   * @return formattedString
   */
  private String replaceVariable(String formattedString, String variable,
      String variableValue) {
    if (formattedString == null) {
      return formattedString;
    }
    formattedString = formattedString.replaceAll(EtomoAutodoc.VAR_TAG + "\\"
        + "{" + variable + "}", variableValue);
    return formattedString;
  }

  /**
   * Builds formattedString out of string.  Replaces the variables found string
   * with variable values until there is nothing left to replace.
   * @param string
   * @param formattedString
   * @return formattedString.
   */
  private String replaceVariables(String string, String formattedString) {
    formattedString = string;
    if (variables == null || formattedString == null
        || formattedString.indexOf(EtomoAutodoc.VAR_TAG) == -1) {
      //no replacement needed
      return formattedString;
    }
    for (int i = 0; i < variables.size(); i++) {
      //If there are no more variable tags in the string ("%"), the replace is done
      if (formattedString != null
          && formattedString.indexOf(EtomoAutodoc.VAR_TAG) == -1) {
        return formattedString;
      }
      TestSectionCommand.Variable variable = (TestSectionCommand.Variable) variables
          .get(i);
      formattedString = replaceVariable(formattedString, variable.getName(),
          variable.getValue());
    }
    return formattedString;
  }

  public final void reset() {
    empty = true;
    known = false;
    action = null;
    field = null;
    test = null;
    fieldName = null;
    formattedFieldName = null;
    fieldIndex = 0;
    value = null;
    formattedValue = null;
    string = "";
    processEndState = null;
    autodoc = false;
    function = false;
    processName = null;
  }

  public final UITestAction getAction() {
    return action;
  }

  public final UITestField getType() {
    return field;
  }

  public final ProcessName getProcessName() {
    return processName;
  }

  public final String getName() {
    if (formattedFieldName != null) {
      return formattedFieldName;
    }
    return fieldName;
  }

  public final int getIndex() {
    return fieldIndex;
  }

  public final String getValue() {
    if (formattedValue != null) {
      return formattedValue;
    }
    return value;
  }

  public final String toString() {
    return string;
  }

  public final CallbackClassEnum getCallbackClassEnum() {
    return callbackClassEnum;
  }

  public final ProcessEndState getProcessEndState() {
    return processEndState;
  }

  public UITestTest getTest() {
    return test;
  }

  public boolean isEmpty() {
    return empty;
  }

  public boolean isKnown() {
    return known;
  }

  public boolean isFunctionLocation() {
    return autodoc;
  }

  public boolean isFunction() {
    return function;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2007/12/10 22:31:20  sueh
 * <p> bug# 1041 working with the changes in ProcessName.
 * <p>
 * <p> Revision 1.2  2007/04/09 20:02:57  sueh
 * <p> bug# 964 Change NameValuePair to an abstract class called Statement and
 * <p> child classes representing name/value pair, comment, empty line, and
 * <p> subsection.  Made delimiter change an attribute of the name/value pair class.
 * <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
 * <p>
 * <p> Revision 1.1  2007/03/21 18:12:47  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Creating Autodoc using a factory.  Moved AdocCommand classes out of the
 * <p> autodoc package because they not part of the autodoc.
 * <p>
 * <p> Revision 1.8  2007/03/08 22:03:02  sueh
 * <p> bug# 964 In NameValuePair, change the wording:  a name is made of 1 or more
 * <p> attributes.
 * <p>
 * <p> Revision 1.7  2006/10/24 21:41:56  sueh
 * <p> bug# 947 Added waitfor.process.process_name =
 * <p>
 * <p> Revision 1.6  2006/10/10 05:20:11  sueh
 * <p> bug# 931 Added file exists functionality to UITest.
 * <p>
 * <p> Revision 1.5  2006/09/07 17:24:00  sueh
 * <p> bug# 852 Added functionality to replace variables in field names.
 * <p>
 * <p> Revision 1.4  2006/08/08 18:12:55  sueh
 * <p> bug# 852 Adding isFunctionLocation() and isFunction().  Removing
 * <p> isSecondaryAutodoc().  Changing the adoc command to mean the function
 * <p> location.  Added the function command, to run a function section.
 * <p>
 * <p> Revision 1.3  2006/06/30 23:09:09  sueh
 * <p> bug# 852 Using the {}s in variables in the Dialog sections all the time to simplify
 * <p> parsing them.
 * <p>
 * <p> Revision 1.2  2006/06/27 22:33:00  sueh
 * <p> bug# 582 Treating all subsections as CallbackClassEnum's.
 * <p>
 * <p> Revision 1.1  2006/06/14 00:34:23  sueh
 * <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
 * <p> attributes specific to a type of autdoc.
 * <p>
 * <p> Revision 1.3  2006/06/06 17:23:13  sueh
 * <p> bug# 766 replaceVariable():  Since autodoc test files are shared between OSs, always use
 * <p> the "/" path character.
 * <p>
 * <p> Revision 1.2  2006/05/01 21:21:07  sueh
 * <p> bug# 787 Handling all variables in UITestAxisDialogCommand.
 * <p>
 * <p> Revision 1.1  2006/04/28 21:09:22  sueh
 * <p> bug# 787 Was UITestCommand.  Parses the uitest axis autodoc section-
 * <p> level name/value pairs.
 * <p>
 * <p> Revision 1.1  2006/04/25 19:39:10  sueh
 * <p> bug# 787 Parses the name/value pair.  Was UIField.
 * <p> </p>
 */
