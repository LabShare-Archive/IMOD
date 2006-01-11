package etomo.ui;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class UIField {
  public static final String rcsid = "$Id$";

  private static final String ENABLED_ATTRIB = "enabled";
  private boolean assertCommand = false;
  private boolean assertEnabled = false;
  private boolean sleepCommand = false;
  private String fieldType = null;
  private String fieldName = null;
  private int fieldIndex = 0;
  private String value = null;
  private String string = null;

  final void set(NameValuePair pair) {
    reset();
    if (pair == null) {
      return;
    }
    string = pair.getString();
    if (pair.levels() == 0) {
      return;
    }
    int index = 0;
    String name = pair.getName(0);
    if (name.equals(UITest.SLEEP_ATTRIB)) {
      sleepCommand = true;
    }
    else {
      if (name.equals("assert")) {
        assertCommand = true;
        index++;
      }
      set(pair, index);
    }
    value = pair.getValue();
  }

  private void set(NameValuePair pair, int index) {
    int levels = pair.levels();
    if (index >= levels) {
      return;
    }
    fieldType = pair.getName(index++);
    if (index >= levels) {
      return;
    }
    fieldName = pair.getName(index++);
    if (index >= levels) {
      return;
    }
    String name = pair.getName(index++);
    if (name.equals(ENABLED_ATTRIB)) {
      assertEnabled = true;
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
    if (name.equals(ENABLED_ATTRIB)) {
      assertEnabled = true;
      return;
    }
  }

  private final void reset() {
    assertCommand = false;
    assertEnabled = false;
    sleepCommand = false;
    fieldType = null;
    fieldName = null;
    fieldIndex = 0;
    value = null;
    string = null;
  }

  final boolean isSleepCommand() {
    return sleepCommand;
  }
  
  final boolean isAssertCommand() {
    return assertCommand;
  }
  
  final boolean isAssertEnabled() {
    return assertEnabled;
  }
  
  final String getFieldType() {
    return fieldType;
  }
  
  final String getFieldName() {
    return fieldName;
  }
  
  final int getFieldIndex() {
    return fieldIndex;
  }
  
  final String getValue() {
    return value;
  }
  
  final String getString() {
    return string;
  }
}
/**
 * <p> $Log$ </p>
 */