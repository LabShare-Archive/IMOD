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
  public static  final String  rcsid =  "$Id$";

  protected Number defaultValue;
  protected String shortName = null;
  
  /**
   * Construct a ScriptParameter with type = INTEGER_TYPE
   * @param name the name of the instance
   */
  public ScriptParameter(String name) {
    super(name);
    defaultValue = newNumber();
  }
  
  public ScriptParameter(int type) {
    super(type);
    defaultValue = newNumber();
  }
   
  public ScriptParameter(int type, String name) {
    super(type, name);
    defaultValue = newNumber();
  }
   
  public ScriptParameter(int type, String name, HashMap requiredMap) {
    super(type, name);
    defaultValue = newNumber();
    setRequired(requiredMap);
  }
   
   public ScriptParameter(int type, String name, String shortName) {
     super(type, name);
     this.shortName = shortName;
     defaultValue = newNumber();
   }
   
   public ScriptParameter(ScriptParameter that) {
     super(that);
     defaultValue = newNumber(that.defaultValue);
   }
   
   public ScriptParameter(ConstEtomoNumber that) {
     super(that);
     defaultValue = newNumber();
   }
   
   protected String paramString() {
     return super.paramString() + ",\ndefaultValue=" + defaultValue;
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
   

   /**
    * Sets defaultValue.  When defaultValue is not null and currentValue is
    * equal to defaultValue, isUseInScript() will return null.  Also
    * updateComScript will remove the entry.  Most of the time comscripts should
    * show the value, even if it is defaulted, so this is rarely used.
    * @param defaultValue
    * @return
    */
   public ScriptParameter setDefault(int defaultValue) {
     this.defaultValue = newNumber(defaultValue);
     return this;
   }

   public ConstEtomoNumber useDefaultAsDisplayValue() {
     return setDisplayValue(defaultValue);
   }
   
   public double getDefaultDouble() {
     return defaultValue.doubleValue();
   }
   
   /**
    * Returns true if defaultValue is not null and getValue() is equal to
    * defaultValue.
    * @return
    */
   protected boolean isDefault(Number value) {
     if (isNull(value) || isNull(defaultValue)) {
       return false;
     }
     return equals(value, defaultValue);
   }
   
   public ConstEtomoNumber updateComScript(ComScriptCommand scriptCommand) {
     if (isUseInScript()) {
       scriptCommand.setValue(name, toString(getValue()));
     }
     else {
       scriptCommand.deleteKey(name);
     }
     return this;
   }
   
   /**
    * Returns true if value would be placed in a script (not null and not
    * default).
    * @return
    */
   public boolean isUseInScript() {
     Number value = getValue();
     if (!isNull(value) && !isDefault(value)) {
       return true;
     }
     return false;
   }
   
   /**
    * Parse scriptCommand for name and shortName.  If keyword is not found, call reset().
    * If name or shortName is found, call set with the string value found in scriptCommand.
    * @param scriptCommand
    * @return
    * @throws InvalidParameterException
    */   
   public ConstEtomoNumber parse(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
     if (!scriptCommand.hasKeyword(name)) {
       if (shortName == null || !scriptCommand.hasKeyword(shortName)) {
         return reset();
       }
       return set(scriptCommand.getValue(shortName));
     }
     return set(scriptCommand.getValue(name));
  }

}
