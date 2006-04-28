package etomo.ui;

import java.io.File;

import etomo.storage.autodoc.AdocCommand;
import etomo.storage.autodoc.NameValuePair;
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
final class UITestAxisSectionCommand implements AdocCommand {
  public static final String rcsid = "$Id$";

  private static final String ENABLED_STRING = "enabled";
  static final String SECTION_TYPE = "Dialog";

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
  private boolean secondaryAutodoc = false;

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
    int index = 0;
    //set the action
    action = UITestAction.getInstance(pair.getName(0));
    if (action != UITestAction.SLEEP && action != UITestAction.COPY) {
      //set the field
      if (action == UITestAction.WAIT_FOR) {
        index++;
      }
      else if (action == UITestAction.ASSERT) {
        index++;
      }
      else if (action == UITestAction.ADOC) {
        secondaryAutodoc = true;
      }
      setField(pair, index);
    }
    //get the value
    value = pair.getValue();
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
    //this ignores the Version name/value pair
    if (action == UITestAction.ADOC || action == UITestAction.ASSERT
        || action == UITestAction.COPY || action == UITestAction.SLEEP
        || action == UITestAction.STOP || action == UITestAction.WAIT_FOR
        || field != null) {
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

  void setVariable(String variable, String assignment) {
    if (formattedValue == null) {
      formattedValue = value;
    }
    if (formattedValue == null) {
      return;
    }
    formattedValue = formattedValue.replaceAll(EtomoAutodoc.VAR_TAG + "\\"
        + "{" + variable + "}", assignment);
    formattedValue = formattedValue.replaceAll(EtomoAutodoc.VAR_TAG + variable
        + File.separatorChar, assignment + File.separatorChar);
  }

  void unsetVariables() {
    formattedValue = null;
  }

  public void reset() {
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
    secondaryAutodoc = false;
  }

  public UITestAction getAction() {
    return action;
  }

  UITestField getField() {
    return field;
  }

  String getFieldName() {
    return fieldName;
  }

  int getFieldIndex() {
    return fieldIndex;
  }

  public String getValue() {
    if (formattedValue != null) {
      return formattedValue;
    }
    return value;
  }

  public String toString() {
    return string;
  }

  DialogType getDialogType() {
    return dialogType;
  }

  ProcessEndState getProcessEndState() {
    return processEndState;
  }

  boolean isEnabled() {
    return enabled;
  }

  boolean isEmpty() {
    return empty;
  }

  boolean isKnown() {
    return known;
  }
  
  public boolean isSecondaryAutodoc() {
    return secondaryAutodoc;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2006/04/25 19:39:10  sueh
 * <p> bug# 787 Parses the name/value pair.  Was UIField.
 * <p> </p>
 */
