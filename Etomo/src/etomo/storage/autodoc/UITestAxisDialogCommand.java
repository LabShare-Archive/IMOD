package etomo.storage.autodoc;

import java.util.ArrayList;

import etomo.type.CallbackClassEnum;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.ProcessEndState;
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
public class UITestAxisDialogCommand implements AdocCommand {
  public static final String rcsid = "$Id$";

  private static final String ENABLED_STRING = "enabled";
  public static final String SECTION_TYPE = "Dialog";

  private final ArrayList variables;

  private UITestAction action = null;
  private UITestField field = null;
  private String fieldName = null;
  private int fieldIndex = 0;
  private String value = null;
  private String formattedValue = null;
  private String string = "";
  private DialogType dialogType = null;
  private ProcessEndState processEndState = null;
  private boolean enabled = false;
  private boolean empty = true;
  private boolean known = false;
  private boolean functionLocation = false;
  private boolean function = false;
  private CallbackClassEnum callbackClassEnum = null;

  public UITestAxisDialogCommand(ArrayList variables) {
    this.variables = variables;
  }

  public final void set(NameValuePair pair) {
    reset();
    if (pair == null) {
      return;
    }
    else {
      empty = false;
      string = pair.getString();
      if (pair.levels() == 0) {
        return;
      }
      int index = 0;
      //set the action
      action = UITestAction.getInstance(pair.getName(0));
      if (action == UITestAction.ADOC) {
        functionLocation = true;
      }
      else if (action == UITestAction.FUNCTION) {
        function = true;
      }
      //set the field
      else if (action != UITestAction.SLEEP
          && action != UITestAction.STOP && action != UITestAction.COPY) {
        if (action == UITestAction.WAIT_FOR) {
          index++;
        }
        else if (action == UITestAction.ASSERT) {
          index++;
        }
        setField(pair, index);
      }
      //get the value
      value = pair.getValue();
      replaceVariables();
      if (action == UITestAction.WAIT_FOR) {
        //handle waitfor = 
        if (field == null) {
          dialogType = DialogType.getInstance(value);
        }
        //handle waitfor.process = 
        else if (field == UITestField.PROCESS) {
          processEndState = ProcessEndState.getInstance(value);
        }
      }
    }
    //this ignores the Version name/value pair
    if (action == UITestAction.ADOC || action == UITestAction.ASSERT
        || action == UITestAction.COPY || action == UITestAction.SLEEP
        || action == UITestAction.STOP || action == UITestAction.WAIT_FOR
        || action == UITestAction.FUNCTION || field != null) {
      known = true;
    }
    else {
      action = null;
    }
  }

  private void setField(NameValuePair pair, int index) {
    int levels = pair.levels();
    if (index >= levels) {
      return;
    }
    field = UITestField.getInstance(pair.getName(index++));
    if (index >= levels) {
      return;
    }
    fieldName = pair.getName(index++);
    if (index >= levels) {
      return;
    }
    String name = pair.getName(index++);
    if (name.equals(ENABLED_STRING)) {
      enabled = true;
      return;
    }
    try {
      fieldIndex = Integer.parseInt(name);
    }
    catch (NumberFormatException e) {
    }
    if (index >= levels) {
      return;
    }
    name = pair.getName(index++);
    if (name.equals(ENABLED_STRING)) {
      enabled = true;
      return;
    }
  }

  private void replaceVariable(String variable, String assignment) {
    if (value == null) {
      return;
    }
    if (formattedValue == null) {
      formattedValue = value;
    }
    formattedValue = formattedValue.replaceAll(EtomoAutodoc.VAR_TAG + "\\"
        + "{" + variable + "}", assignment);
  }

  private void replaceVariables() {
    if (variables == null || value == null
        || value.indexOf(EtomoAutodoc.VAR_TAG) == -1) {
      return;
    }
    for (int i = 0; i < variables.size(); i++) {
      //If there are no more variable tags in the string ("%"), the replace is done
      if (formattedValue != null
          && formattedValue.indexOf(EtomoAutodoc.VAR_TAG) == -1) {
        return;
      }
      UITestTestCommand.Variable variable = (UITestTestCommand.Variable) variables
          .get(i);
      replaceVariable(variable.getName(), variable.getValue());
    }
  }

  public final void reset() {
    empty = true;
    known = false;
    action = null;
    field = null;
    fieldName = null;
    fieldIndex = 0;
    value = null;
    formattedValue = null;
    string = "";
    dialogType = null;
    processEndState = null;
    enabled = false;
    functionLocation = false;
    function = false;
  }

  public final UITestAction getAction() {
    return action;
  }

  public final UITestField getType() {
    return field;
  }

  public final String getName() {
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

  public final DialogType getDialogType() {
    return dialogType;
  }

  public final CallbackClassEnum getCallbackClassEnum() {
    return callbackClassEnum;
  }

  public final ProcessEndState getProcessEndState() {
    return processEndState;
  }

  public boolean isEnabled() {
    return enabled;
  }

  public boolean isEmpty() {
    return empty;
  }

  public boolean isKnown() {
    return known;
  }

  public boolean isFunctionLocation() {
    return functionLocation;
  }
  
  public boolean isFunction() {
    return function;
  }
}
/**
 * <p> $Log$
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
