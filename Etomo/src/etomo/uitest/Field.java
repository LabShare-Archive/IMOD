package etomo.uitest;

import junit.framework.Assert;
import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.type.AxisID;
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
 * <p> $Log$
 * <p> Revision 1.3  2009/09/01 03:18:33  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.2  2009/01/28 00:58:29  sueh
 * <p> bug# 1102 Allowing a subcommand after a field.
 * <p>
 * <p> Revision 1.1  2009/01/20 20:47:15  sueh
 * <p> bug# 1102 Class that holds the field portion of a Command.
 * <p> </p>
 */

final class Field extends Assert {
  public static final String rcsid = "$Id$";

  private final AxisID testAxisID;

  private boolean empty = true;
  private UITestFieldType fieldType = null;
  private String name = null;
  private EtomoNumber index = new EtomoNumber();
  String string;

  Field(AxisID testAxisID) {
    this.testAxisID = testAxisID;
  }

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
    assertFalse("index must be set (default 0) (" + string + ")", index.isNull());
    assertNotNull("must create string representation of command", string);
    //Logic assertions
    if (fieldType == UITestFieldType.PANEL) {
      assertTrue("panel find does not use the index (" + string + ")", index.equals(0));
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
    name = Command.replaceVariables(statement.getLeftSide(i), variableList, testAxisID);
    i++;
    //Set index - must be an integer
    String leftSide = statement.getLeftSide(i);
    if (leftSide != null) {
      index.set(leftSide);
      if (index.isValid()) {
        i++;
      }
      else {
        index.set(0);
      }
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
