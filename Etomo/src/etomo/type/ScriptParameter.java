package etomo.type;

import java.util.HashMap;

import etomo.comscript.ComScriptCommand;
import etomo.comscript.InvalidParameterException;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.14  2009/09/22 21:02:35  sueh
 * <p> bug# 1259 No longer unnecessarily returning ConstEtomoNumber from updateComScript.
 * <p>
 * <p> Revision 1.13  2008/10/27 20:24:34  sueh
 * <p> bug# 1141 Added debug only print statements.
 * <p>
 * <p> Revision 1.12  2007/03/07 21:10:19  sueh
 * <p> bug# 981 Changed ScriptParameter.isUseInScript to isNotNullAndNotDefault for
 * <p> clarity.  Added a second updateComScript function to choose whether to include
 * <p> when the instance is defaulted (always in comscript).
 * <p>
 * <p> Revision 1.11  2007/02/05 23:31:06  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.10  2006/10/17 20:17:42  sueh
 * <p> bug# 939  Moving defaultValue and associated functions to ConstEtomoNumber.
 * <p>
 * <p> Revision 1.9  2006/08/29 20:08:47  sueh
 * <p> bug# 924 Added active - an optional member variable that causes the value to be
 * <p> removed from ComScriptCommand (in updateComScript) when it is false.
 * <p>
 * <p> Revision 1.8  2005/11/29 22:40:49  sueh
 * <p> bug# 757 Added getDouble(booleahn defaultIfNull).
 * <p>
 * <p> Revision 1.7  2005/06/16 20:08:23  sueh
 * <p> bug# 692 Make integer the default type.
 * <p>
 * <p> Revision 1.6  2005/06/14 22:04:41  sueh
 * <p> bug# 681 Removed parse(ComScriptCommand, String).  It was not being
 * <p> used.  Made shortName protected to EtomoBoolean2 could handle it.
 * <p>
 * <p> Revision 1.5  2005/05/17 19:21:01  sueh
 * <p> bug# 658 Passing a HashMap of required values from the autodoc to
 * <p> ScriptParameter constructors.  In setRequired(HashMap) requiredMap set
 * <p> nullIsValid according to a requiredMap element, if there is one with a key
 * <p> that matches ScriptParameter.name.
 * <p>
 * <p> Revision 1.4  2005/05/12 01:30:16  sueh
 * <p> bug# 658 Removed setDefault(boolean) because it is not in use.  Added
 * <p> getDefaultDouble().
 * <p>
 * <p> Revision 1.3  2005/05/10 03:08:34  sueh
 * <p> bug# 658 Added shortName, which is used as an alternative to the
 * <p> regular name when searching for the value in a comscript.  Added
 * <p> useDefaultAsDisplayValue() because this situation comes up a lot.
 * <p> Changed setInScript() to updateComScript().  Removed addToScript() and
 * <p> setUseScreenDisplayValue() because these functions where confusing
 * <p> and where only used in one situation.  Removed getValueForScript()
 * <p> because it is now identical to ConstEtomoNumber.getValue().
 * <p> To create a field with a default:
 * <p> Call setDefault()
 * <p> Call useDefaultAsDisplayValue(), if you want the default to be displayed
 * <p> when the field is empty.
 * <p>
 * <p> Revision 1.2  2005/02/11 22:32:34  sueh
 * <p> Fixed problem in ScriptParameter:  it was letting defaulted parameters
 * <p> into scripts.
 * <p>
 * <p> Revision 1.1  2005/01/25 23:59:34  sueh
 * <p> An EtomoNumber which can read from and write to scripts.  Has a
 * <p> defaultValue member variable which is compared to currentValue in
 * <p> order to prevent unnecessary script entries.  Has it own getValue function
 * <p> called getValueForScript() which ignores useDisplayValue.
 * <p> Can set useDisplayValue to prevent the screen from showing the display
 * <p> value, but still allow the script to show it.  To force entries to appear in
 * <p> a script when they are defaulted, set the force parameter to true.  Can
 * <p> also use EtomoNumber.get functions, but these do not ignore
 * <p> useDisplayValue.
 * <p> </p>
 */
public class ScriptParameter extends EtomoNumber {
  public static final String rcsid = "$Id$";

  protected String shortName = null;
  //An inactive value is not placed in the comscript
  private EtomoBoolean2 active = null;

  /**
   * Construct a ScriptParameter with type = INTEGER_TYPE
   * @param name the name of the instance
   */
  public ScriptParameter(String name) {
    super(name);
  }

  public ScriptParameter(Type type) {
    super(type);
  }

  public ScriptParameter(Type type, String name) {
    super(type, name);
  }

  public ScriptParameter(Type type, String name, HashMap requiredMap) {
    super(type, name);
    setRequired(requiredMap);
  }

  public ScriptParameter(Type type, String name, String shortName) {
    super(type, name);
    this.shortName = shortName;
  }

  public ScriptParameter(ConstEtomoNumber that) {
    super(that);
  }

  protected String paramString() {
    return super.paramString() + ",\nactive=" + active;
  }

  /**
   * Sets nullIsValid based on requiredMap.  It would be ok to make this
   * public or override it.
   * @param requiredMap
   * @return
   */
  private ConstEtomoNumber setRequired(HashMap requiredMap) {
    if (requiredMap == null) {
      return this;
    }
    EtomoNumber required = new EtomoNumber();
    required.set((String) requiredMap.get(name));
    if (required.equals(EtomoAutodoc.REQUIRED_TRUE_VALUE)) {
      nullIsValid = false;
    }
    return this;
  }

  public void updateComScript(ComScriptCommand scriptCommand) {
    updateComScript(scriptCommand, false);
  }

  /**
   * Remove currentValue from or place currentValue in the comscript command
   * depending on whether this instance is active, null, or defaulted.  An
   * inactive value is always removed.  If includeWhenDefaulted is false, then
   * the value is removed when it is null or defaulted.  If includeWhenDefaulted
   * is true, then the value is remove when it is null.
   * @param scriptCommand
   * @param includeWhenDefaulted
   * @return
   */
  public void updateComScript(ComScriptCommand scriptCommand, boolean includeWhenDefaulted) {
    if (isActive()
        && ((includeWhenDefaulted && !isNull()) || (!includeWhenDefaulted && isNotNullAndNotDefault()))) {
      scriptCommand.setValue(name, toString(getValue()));
    }
    else {
      scriptCommand.deleteKey(name);
    }
  }

  /**
   * Returns true if value is null or default.
   * @return
   */
  public boolean isNotNullAndNotDefault() {
    Number value = getValue();
    if (!isNull(value) && !isDefault(value)) {
      return true;
    }
    return false;
  }

  public ConstEtomoNumber parse(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    return parse(scriptCommand, false);
  }

  /**
   * Parse scriptCommand for name and shortName.  If keyword is not found, call reset().
   * If name or shortName is found, call set with the string value found in scriptCommand.
   * @param scriptCommand
   * @return
   * @throws InvalidParameterException
   */
  public ConstEtomoNumber parse(ComScriptCommand scriptCommand, boolean setActive)
      throws InvalidParameterException {
    boolean found = false;
    if (isDebug()) {
      System.out.println("name=" + name + ",scriptCommand.hasKeyword(name)="
          + scriptCommand.hasKeyword(name));
    }
    if (!scriptCommand.hasKeyword(name)) {
      if (shortName == null || !scriptCommand.hasKeyword(shortName)) {
        reset();
      }
      else {
        found = true;
        set(scriptCommand.getValue(shortName));
      }
    }
    else {
      found = true;
      if (isDebug()) {
        System.out
            .println("scriptCommand.getValue(name)=" + scriptCommand.getValue(name));
      }
      set(scriptCommand.getValue(name));
      if (isDebug()) {
        System.out.println("toString()=" + toString());
      }
    }
    if (setActive) {
      setActive(found);
    }
    return this;
  }

  public void setActive(boolean active) {
    if (this.active == null) {
      this.active = new EtomoBoolean2();
    }
    this.active.set(active);
  }

  public boolean isActive() {
    return active == null || active.is();
  }
}