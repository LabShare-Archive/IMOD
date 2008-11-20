package etomo.type;

import java.io.File;

import etomo.comscript.ComScriptCommand;
import etomo.comscript.InvalidParameterException;

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
 * <p> Revision 1.1  2008/10/27 20:28:51  sueh
 * <p> bug# 1141 Class to update and read strings in a comscript.
 * <p> </p>
 */
public final class StringParameter implements ConstStringParameter {
  public static final String rcsid = "$Id$";

  private final String name;
  private String value = null;
  private boolean debug = false;

  public StringParameter(String name) {
    this.name = name;
  }

  public void reset() {
    value = null;
  }

  public boolean equals(String comparee) {
    if (value == null) {
      return isEmpty(comparee);
    }
    return value.equals(comparee);
  }

  public boolean endsWith(String comparee) {
    if (value == null) {
      return isEmpty(comparee);
    }
    return value.endsWith(comparee);
  }

  public void parse(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    if (!scriptCommand.hasKeyword(name)) {
      reset();
    }
    else {
      value = scriptCommand.getValue(name);
    }
  }

  public boolean isEmpty() {
    return value == null;
  }

  private boolean isEmpty(String string) {
    return string == null || string.matches("\\s*");
  }

  public void updateComScript(ComScriptCommand scriptCommand) {
    if (isEmpty()) {
      scriptCommand.deleteKey(name);
    }
    else {
      scriptCommand.setValue(name, value);
    }
  }

  public void set(String input) {
    if (isEmpty(input)) {
      reset();
    }
    else {
      value = input;
    }
  }

  public void set(File input) {
    if (input == null) {
      reset();
    }
    else {
      value = input.getAbsolutePath();
    }
  }

  public String getName() {
    return name;
  }

  public String toString() {
    if (value == null) {
      return "";
    }
    return value;
  }
  
  public void setDebug() {
    debug = true;
  }
}
