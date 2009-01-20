package etomo.uitest;

import junit.framework.Assert;
import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.type.EtomoNumber;
import etomo.type.UITestFieldType;

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
 * <p> $Log$ </p>
 */

final class Field extends Assert {
  public static final String rcsid = "$Id$";

  private boolean empty = true;
  private UITestFieldType fieldType = null;
  private String name = null;
  private EtomoNumber index = new EtomoNumber();
  String string;

  void reset() {
    empty = true;
    fieldType = null;
    name = null;
    index.reset();
    string = null;
  }

  private void assertValid() {
    //Grammer assertions
    assertNotNull("fieldType is required (" + string + ")", fieldType);
    assertTrue("index must be integer - " + index.getInvalidReason() + " ("
        + string + ")", index.isValid());
    assertFalse("index must be set (default 0) (" + string + ")", index
        .isNull());
    assertNotNull("must create string representation of command", string);
    //Logic assertions
    if (fieldType == UITestFieldType.PANEL) {
      assertTrue("panel find does not use the index (" + string + ")", index
          .equals(0));
    }
  }

  int set(ReadOnlyStatement statement, int startAt, VariableList variableList) {
    reset();
    assertNotNull("statement is null", statement);
    //Is there a field at startAt?
    fieldType = UITestFieldType.getInstance(statement.getLeftSide(startAt));
    if (fieldType == null) {
      //not field type, so there is no field
      return startAt;
    }
    //build the rest of the field
    int i = startAt + 1;
    empty = false;
    string = statement.getString();
    //Set name
    name = Command.replaceVariables(statement.getLeftSide(i), variableList);
    i++;
    //Set index
    String leftSide = statement.getLeftSide(i);
    if (leftSide != null) {
      index.set(leftSide);
      i++;
    }
    else {
      index.set(0);
    }
    assertValid();
    return i;
  }

  boolean isEmpty() {
    return empty;
  }

  UITestFieldType getFieldType() {
    return fieldType;
  }

  String getName() {
    return name;
  }

  int getIndex() {
    return index.getInt();
  }

  public String toString() {
    return string;
  }

}
