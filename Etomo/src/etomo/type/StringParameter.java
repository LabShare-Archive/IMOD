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
 * <p> $Log$ </p>
 */
public final class StringParameter implements ConstStringParameter {
  public static final String rcsid = "$Id$";

  private final String name;
  private String value = null;

  public StringParameter(String name) {
    this.name = name;
  }

  public void reset() {
    value = null;
  }

  public boolean equals(String input) {
    if (value == null) {
      return input == null;
    }
    return value.equals(input);
  }

  public boolean endsWith(String input) {
    if (value == null) {
      return input == null;
    }
    return value.endsWith(input);
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

  public void updateComScript(ComScriptCommand scriptCommand) {
    if (value != null && !value.matches("\\s*")) {
      scriptCommand.setValue(name, value);
    }
    else {
      scriptCommand.deleteKey(name);
    }
  }

  public void set(String input) {
    value = input;
  }

  public void set(File input) {
    if (input == null) {
      value = null;
    }
    else {
      value = input.getAbsolutePath();
    }
  }

  public String toString() {
    if (value == null) {
      return "";
    }
    return value;
  }
}
